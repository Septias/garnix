use async_lsp::{
    client_monitor::ClientProcessMonitorLayer, concurrency::ConcurrencyLayer,
    panic::CatchUnwindLayer, router::Router, server::LifecycleLayer, tracing::TracingLayer,
    ClientSocket,
};
use lsp_types::{
    notification,
    request::{self, DocumentDiagnosticRequest, InlayHintRequest},
    Diagnostic, Hover, HoverContents, HoverProviderCapability, InitializeResult, InlayHintLabel,
    InlayHintServerCapabilities, MarkedString, ServerCapabilities, Url,
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
                    if !self.errors.contains_key(&uri.as_str()) {
                        let errors = self.errors.get_mut(uri.as_str()).unwrap();
                        errors.push(Diagnostic {
                            range: to_lsp_range(&e.span, &content),
                            message: format!("{:?}", e),
                            severity: Some(lsp_types::DiagnosticSeverity::Error),
                            ..Default::default()
                        });
                    } else {
                        self.errors.insert(
                            uri.to_string(),
                            vec![Diagnostic {
                                range: to_lsp_range(&e.span, &content),
                                message: format!("{:?}", e),
                                severity: Some(lsp_types::DiagnosticSeverity::Error),
                                ..Default::default()
                            }],
                        );
                    }
                };
            }
            Err(err) => {
                self.errors.insert(uri.to_string(), vec![]);
                let errors = self.errors.get_mut(uri.as_str()).unwrap();
                match err {
                    parser::ParseError::MultipleErrors { errors } => {
                        for (kind, span) in errors {
                            let diagnostic = Diagnostic {
                                range: to_lsp_range(&span, &content),
                                message: format!("{:?}", kind),
                                severity: Some(lsp_types::DiagnosticSeverity::Error),
                                ..Default::default()
                            };
                            errors.push(diagnostic);
                        }
                    }
                }
                errors.extend(err)
            }
        };

        self.files.insert(uri.as_str().to_string(), (ast, content));
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
                        diagnostic_provider: Some(true),
                        ..ServerCapabilities::default()
                    },
                    server_info: None,
                })
            })
            .request::<request::HoverRequest, _>(|st, a| {
                let ast = st
                    .files
                    .get(a.text_document_position_params.text_document.uri.as_str());

                let ret = if let Some((ast, source)) = ast {
                    let info: Option<(String, &std::ops::Range<usize>)> = (move || {
                        let position =
                            from_lsp_position(a.text_document_position_params.position, source);
                        let node = ast.get_node_at(position)?;

                        let name = node.as_ref();
                        let constraints = node
                            .get_identifier()
                            .ok()
                            .map(|ident| ident.get_constraints());
                        let ty = node.get_identifier().ok().map(|ident| ident.get_type());

                        let mut ret = format!("Node: `{:?}`\n", name);
                        if let Some(constraints) = constraints {
                            ret.push_str(&format!("Constraints: `{:?}`\n", constraints));
                        }
                        if let Some(ty) = ty {
                            ret.push_str(&format!("Type: `{:?}`\n", ty));
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
                    eprintln!("loaded file: {:?}", st.files);
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
            .request::<InlayHintRequest, _>(|st, params| {
                let ret = st
                    .files
                    .get(&params.text_document.uri)
                    .map(|(ast, source)| {
                        let idents = ast.collect_identifiers();
                        idents.into_iter().map(|ident| lsp_types::InlayHint {
                            position: to_lsp_range(ast.get_span(), source),
                            label: InlayHintLabel::String(ident.ty),
                            ..Default::default()
                        });

                        hints
                    });
                async move { ret }
            })
            .request::<DocumentDiagnosticRequest>(|st, params| {
                let ret = st
                    .files
                    .get(&params.text_document.uri)
                    .map(|(ast, source)| {
                        let mut diagnostics = vec![];
                        for error in ast.errors.iter() {
                            let range = to_lsp_range(&error.span, source);
                            let diagnostic = Diagnostic {
                                range,
                                message: error.message.clone(),
                                severity: Some(lsp_types::DiagnosticSeverity::Error),
                                ..Default::default()
                            };
                            diagnostics.push(diagnostic);
                        }
                        diagnostics
                    });
                async move { ret }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_lsp_range() {
        let source = "aaaaaa\nbeeeeeee\nceeeeee";
        assert_eq!(
            to_lsp_range(&parser::Span { start: 11, end: 13 }, source),
            lsp_types::Range {
                start: Position {
                    line: 2,
                    character: 5
                },
                end: Position {
                    line: 2,
                    character: 7
                }
            }
        );
    }

    #[test]
    fn test_from_lsp_position() {
        let source = "aaaaaa\nbeeeeeee\nceeeeee";
        assert_eq!(
            from_lsp_position(
                Position {
                    line: 1,
                    character: 5
                },
                source
            ),
            12
        );
    }
}
