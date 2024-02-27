use std::collections::HashMap;
use std::ops::Range;
use std::{fs, ops::ControlFlow};

use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use async_lsp::tracing::TracingLayer;
use async_lsp::ClientSocket;
use lsp_types::{
    notification, request, Hover, HoverContents, HoverProviderCapability, InitializeResult,
    MarkedString, ServerCapabilities, Url,
};
use parser::ast::Ast;
use parser::parse;
use tower::ServiceBuilder;
use tracing::Level;

fn map_idents(input: &Ast, source: &str, map: &mut Vec<(Range<usize>, String)>) {
    match input {
        Ast::AttrSet { attrs, .. } => {
            for (_, v) in attrs {
                map_idents(v, source, map);
            }
            if let Some((range, _)) = attrs.first() {
                map.push((
                    range.clone(),
                    format!(
                        "{{{}}}",
                        attrs
                            .iter()
                            .map(|(k, _)| source[k.clone()].to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                ));
            }
        }
        Ast::LetBinding { bindings, body, .. } => {
            for (k, v) in bindings {
                map_idents(v, source, map);
                map.push((k.clone(), v.as_ref().to_string()));
            }
            map_idents(body, source, map);
        }

        e @ Ast::NixPath(span)
        | e @ Ast::Comment(span)
        | e @ Ast::DocComment(span)
        | e @ Ast::LineComment(span)
        | e @ Ast::Identifier(span)
        | e @ Ast::NixString(span) => {
            map.push((span.clone(), source[e.as_span()].to_string()));
        }

        _ => {}
    };
}

fn load_file(st: &mut ServerState, uri: &Url) -> anyhow::Result<()> {
    let content = fs::read_to_string(uri.path()).unwrap();
    let ast = parse(content)?;
    let mut map = Vec::new();
    eprintln!("Opened file: {}", uri);
    map_idents(&ast.ast, &ast.source, &mut map);
    st.files.insert(uri.as_str().to_string(), map);
    Ok(())
}

struct ServerState {
    _client: ClientSocket,
    files: HashMap<String, Vec<(Range<usize>, String)>>,
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        let mut router = Router::new(ServerState {
            _client: client.clone(),
            files: HashMap::new(),
        });

        router
            .request::<request::Initialize, _>(|_, params| async move {
                eprintln!("Initialize with {params:?}");
                Ok(InitializeResult {
                    capabilities: ServerCapabilities {
                        hover_provider: Some(HoverProviderCapability::Simple(true)),
                        ..ServerCapabilities::default()
                    },
                    server_info: None,
                })
            })
            .request::<request::HoverRequest, _>(|st, a| {
                let map = st
                    .files
                    .get(a.text_document_position_params.text_document.uri.as_str());

                let ret = if let Some(map) = map {
                    let t = map
                        .iter()
                        .find(|(range, _)| {
                            range.contains(
                                &(a.text_document_position_params.position.character as usize),
                            )
                        })
                        .map(|(_, str)| str)
                        .cloned();

                    Ok(Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::String(format!(
                            "Hovering: {:?} which is: {}",
                            a.text_document_position_params.position,
                            t.unwrap_or("None".to_string())
                        ))),
                        range: None,
                    }))
                } else {
                    load_file(st, &a.text_document_position_params.text_document.uri).ok();
                    eprintln!(
                        "file was not loaded: {}",
                        a.text_document_position_params.text_document.uri.as_str()
                    );
                    eprintln!("loaded: {:?}", st.files);
                    Ok(Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::String(format!(
                            "file not loaded {}",
                            a.text_document_position_params.text_document.uri
                        ))),
                        range: None,
                    }))
                };
                async move { ret }
            })
            .notification::<notification::Initialized>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidChangeConfiguration>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidOpenTextDocument>(|st, file| {
                load_file(st, &file.text_document.uri).ok();
                ControlFlow::Continue(())
            })
            .notification::<notification::DidChangeTextDocument>(|st, file| {
                load_file(st, &file.text_document.uri).ok();
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
