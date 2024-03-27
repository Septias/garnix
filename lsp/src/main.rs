use async_lsp::{
    client_monitor::ClientProcessMonitorLayer, concurrency::ConcurrencyLayer,
    panic::CatchUnwindLayer, router::Router, server::LifecycleLayer, tracing::TracingLayer,
    ClientSocket,
};
use lsp_types::{
    notification,
    request::{self, DocumentDiagnosticRequest, InlayHintRequest},
    Diagnostic, DocumentDiagnosticReport, FullDocumentDiagnosticReport, Hover, HoverContents,
    HoverProviderCapability, InitializeResult, InlayHintLabel, MarkedString,
    RelatedFullDocumentDiagnosticReport, ServerCapabilities, Url,
};
use std::{collections::HashMap, fs, ops::ControlFlow};
use tower::ServiceBuilder;
use tracing::Level;
use utils::*;

use infer::infer::infer;
use parser::parse;

mod utils;

struct ServerState {
    _client: ClientSocket,
    files: HashMap<String, (infer::Ast, String)>,
    errors: HashMap<String, Vec<Diagnostic>>,
}

impl ServerState {
    pub fn load_file(&mut self, uri: &Url) -> anyhow::Result<()> {
        eprintln!("load file: {:?}", uri);
        let content = fs::read_to_string(uri.path()).unwrap();
        self.load_text(&content, uri)?;
        Ok(())
    }

    pub fn load_text(&mut self, content: &str, uri: &Url) -> anyhow::Result<()> {
        eprintln!("load text: {:?}", content);
        match parse(&content) {
            Ok(ast) => {
                let ast = infer::Ast::from_parser_ast(ast, &content);
                self.errors.remove(&uri.to_string());
                if let Err(e) = infer(&ast) {
                    self.errors.insert(
                        uri.to_string(),
                        vec![Diagnostic {
                            range: to_lsp_range(&e.span, &content),
                            message: format!("{:?}", e),
                            severity: Some(lsp_types::DiagnosticSeverity::WARNING),
                            ..Default::default()
                        }],
                    );
                };
                self.files
                    .insert(uri.as_str().to_string(), (ast, content.to_string()));
            }
            Err(err) => {
                self.errors.insert(uri.to_string(), vec![]);
                let error_store = self.errors.get_mut(uri.as_str()).unwrap();
                match err {
                    parser::ParseError::MultipleErrors { errors } => {
                        for (kind, span) in errors {
                            error_store.push(Diagnostic {
                                range: to_lsp_range(&span, &content),
                                message: format!("{:?}", kind),
                                severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                                ..Default::default()
                            });
                        }
                    }
                }
            }
        };

        Ok(())
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        let mut router = Router::new(ServerState {
            _client: client.clone(),
            files: HashMap::new(),
            errors: HashMap::new(),
        });

        router
            .request::<request::Initialize, _>(|_, params| async move {
                eprintln!("Initialize with {params:?}");
                Ok(InitializeResult {
                    capabilities: ServerCapabilities {
                        hover_provider: Some(HoverProviderCapability::Simple(true)),
                        inlay_hint_provider: Some(lsp_types::OneOf::Left(true)),
                        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
                            lsp_types::TextDocumentSyncOptions {
                                open_close: Some(true),
                                change: Some(lsp_types::TextDocumentSyncKind::FULL),
                                will_save: None,
                                will_save_wait_until: None,
                                save: Some(lsp_types::TextDocumentSyncSaveOptions::SaveOptions(
                                    lsp_types::SaveOptions {
                                        include_text: Some(true),
                                    },
                                )),
                            },
                        )),
                        diagnostic_provider: Some(
                            lsp_types::DiagnosticServerCapabilities::Options(
                                lsp_types::DiagnosticOptions {
                                    ..Default::default()
                                },
                            ),
                        ),
                        ..ServerCapabilities::default()
                    },
                    server_info: None,
                })
            })
            .request::<InlayHintRequest, _>(|st, params| {
                let ret = st
                    .files
                    .get(params.text_document.uri.as_str())
                    .map(|(ast, source)| {
                        let idents = ast.collect_identifiers();
                        idents
                            .into_iter()
                            .map(|ident| lsp_types::InlayHint {
                                position: {
                                    let mut pos = to_lsp_range(&ident.span, source).end;
                                    pos.character += 1;
                                    pos
                                },
                                label: InlayHintLabel::String(format!(
                                    "{}",
                                    ident.var.get().map(|var| var.show()).unwrap_or_default()
                                )),
                                kind: None,
                                text_edits: None,
                                tooltip: None,
                                padding_left: None,
                                padding_right: None,
                                data: None,
                            })
                            .collect()
                    });
                async move { Ok(ret) }
            })
            .request::<request::HoverRequest, _>(|st, a| {
                let ast = st
                    .files
                    .get(a.text_document_position_params.text_document.uri.as_str());

                let ret = if let Some((ast, source)) = ast {
                    let info: Option<(String, &std::ops::Range<usize>)> = (move || {
                        let position =
                            from_lsp_position(a.text_document_position_params.position, source);
                        let node = ast.get_ident_at(position)?;

                        let constraints = node.var.get().map(|var| var.show());

                        let mut ret = format!("Node: `{:?}`\n", node.name);
                        if let Some(constraints) = constraints {
                            ret.push_str(&format!("Constraints: `{:?}`\n", constraints));
                        }

                        Some((ret, ast.get_span()))
                    })();

                    Ok(info.map(|(info, span)| Hover {
                        contents: HoverContents::Scalar(MarkedString::String(info)),
                        range: Some(to_lsp_range(span, source)),
                    }))
                } else {
                    st.load_file(&a.text_document_position_params.text_document.uri)
                        .ok();
                    Ok(Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::String(format!(
                            "file not yet read {}",
                            a.text_document_position_params.text_document.uri
                        ))),
                        range: None,
                    }))
                };
                async move { ret }
            })
            .request::<DocumentDiagnosticRequest, _>(|st, params| {
                let ret = st
                    .errors
                    .remove(params.text_document.uri.as_str())
                    .unwrap_or_default();
                async move {
                    Ok(lsp_types::DocumentDiagnosticReportResult::Report(
                        DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                            related_documents: None,
                            full_document_diagnostic_report: FullDocumentDiagnosticReport {
                                result_id: None,
                                items: ret,
                            },
                        }),
                    ))
                }
            })
            .notification::<notification::Initialized>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidChangeConfiguration>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidOpenTextDocument>(|st, file| {
                st.load_text(&file.text_document.text, &file.text_document.uri)
                    .ok();
                ControlFlow::Continue(())
            })
            .notification::<notification::DidChangeTextDocument>(|_, _| {
                ControlFlow::Continue(())
            })
            .notification::<notification::DidCloseTextDocument>(|st, file| {
                st.files.remove(file.text_document.uri.as_str());
                ControlFlow::Continue(())
            })
            .notification::<notification::DidSaveTextDocument>(|st, file| {
                st.load_text(&file.text.unwrap(), &file.text_document.uri)
                    .ok();
                ControlFlow::Continue(())
            })
            .notification::<notification::DidChangeWatchedFiles>(|_, _| ControlFlow::Continue(()));

        ServiceBuilder::new()
            .layer(TracingLayer::default())
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client))
            .service(router)
    });

    tracing_subscriber::fmt()
        .with_max_level(Level::INFO)
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    // Prefer truly asynchronous piped stdin/stdout without blocking tasks.
    #[cfg(unix)]
    let (stdin, stdout) = (
        async_lsp::stdio::PipeStdin::lock_tokio().unwrap(),
        async_lsp::stdio::PipeStdout::lock_tokio().unwrap(),
    );
    // Fallback to spawn blocking read/write otherwise.
    #[cfg(not(unix))]
    let (stdin, stdout) = (
        tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
        tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
    );

    server.run_buffered(stdin, stdout).await.unwrap();
}
