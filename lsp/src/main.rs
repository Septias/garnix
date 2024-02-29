use async_lsp::{
    client_monitor::ClientProcessMonitorLayer, concurrency::ConcurrencyLayer,
    panic::CatchUnwindLayer, router::Router, server::LifecycleLayer, tracing::TracingLayer,
    ClientSocket,
};
use lsp_types::{
    notification, request, Hover, HoverContents, HoverProviderCapability, InitializeResult,
    MarkedString, Position, ServerCapabilities, Url,
};
use std::{collections::HashMap, fs, ops::ControlFlow};
use tower::ServiceBuilder;
use tracing::Level;

use infer::hm::infer;
use parser::parse;

fn load_file(st: &mut ServerState, uri: &Url) -> anyhow::Result<()> {
    let content = fs::read_to_string(uri.path()).unwrap();
    let ast = parse(&content)?;
    let ast = infer::Ast::from_parser_ast(ast, &content);
    if let Err(e) = infer(&ast) {
        eprintln!("[Inference] Error: {:?}", e);
    };
    st.files.insert(uri.as_str().to_string(), (ast, content));
    Ok(())
}

fn to_lsp_range(span: &parser::Span, source: &str) -> lsp_types::Range {
    let mut start = 0;
    let mut line = 0;
    source
        .lines()
        .take_while(|source_line| {
            let new = start + source_line.len();
            if new < span.start {
                start = new;
                line += 1;
                true
            } else {
                false
            }
        })
        .for_each(|_| ());

    lsp_types::Range {
        start: Position {
            line,
            character: (span.start - start) as u32,
        },
        end: Position {
            line,
            character: (span.end - start) as u32,
        },
    }
}

fn _from_lsp_range(range: &lsp_types::Range, source: &str) -> parser::Span {
    let start_offset = source
        .lines()
        .take(range.start.line as usize)
        .fold(0, |acc, b| acc + b.len());

    let end_offset = source
        .lines()
        .take(range.end.line as usize)
        .fold(0, |acc, b| acc + b.len());

    parser::Span {
        start: start_offset + range.start.character as usize,
        end: end_offset + range.end.character as usize,
    }
}

fn from_lsp_position(pos: Position, source: &str) -> usize {
    let start_offset = source
        .lines()
        .take(pos.line as usize)
        .fold(0, |acc, b| acc + b.len());

    start_offset + pos.character as usize
}

struct ServerState {
    _client: ClientSocket,
    files: HashMap<String, (infer::Ast, String)>,
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
                let ast = st
                    .files
                    .get(a.text_document_position_params.text_document.uri.as_str());

                let ret = if let Some((ast, source)) = ast {
                    let info = (move || {
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
                    load_file(st, &a.text_document_position_params.text_document.uri).ok();
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
