use async_lsp::{
    client_monitor::ClientProcessMonitorLayer, concurrency::ConcurrencyLayer,
    panic::CatchUnwindLayer, router::Router, server::LifecycleLayer, tracing::TracingLayer,
    ClientSocket,
};
use lsp_types::{
    notification,
    request::{self, DocumentDiagnosticRequest, InlayHintRequest},
    Diagnostic, DocumentDiagnosticReport, FullDocumentDiagnosticReport, HoverProviderCapability,
    InitializeResult, InlayHintLabel, RelatedFullDocumentDiagnosticReport, ServerCapabilities, Url,
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
        let content = fs::read_to_string(uri.path()).unwrap();
        match parse(&content) {
            Ok(ast) => {
                let ast = infer::Ast::from_parser_ast(ast, &content);
                if let Err(e) = infer(&ast) {
                    if !self.errors.contains_key(uri.as_str()) {
                        let errors = self.errors.get_mut(uri.as_str()).unwrap();
                        errors.push(Diagnostic {
                            range: to_lsp_range(&e.span, &content),
                            message: format!("{:?}", e),
                            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                            ..Default::default()
                        });
                    } else {
                        self.errors.insert(
                            uri.to_string(),
                            vec![Diagnostic {
                                range: to_lsp_range(&e.span, &content),
                                message: format!("{:?}", e),
                                severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                                ..Default::default()
                            }],
                        );
                    }
                };
                self.files.insert(uri.as_str().to_string(), (ast, content));
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
                                position: to_lsp_range(ast.get_span(), source).start,
                                label: InlayHintLabel::String(format!(
                                    "{}",
                                    ident.var.get().unwrap().show()
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
                st.load_file(&file.text_document.uri).ok();
                ControlFlow::Continue(())
            })
            .notification::<notification::DidChangeTextDocument>(|st, file| {
                st.load_file(&file.text_document.uri).ok();
                ControlFlow::Continue(())
            })
            .notification::<notification::DidCloseTextDocument>(|st, file| {
                st.files.remove(file.text_document.uri.as_str());
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
