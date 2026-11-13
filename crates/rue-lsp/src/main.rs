use rue_lsp::{Backend, encode_uris};
use tower::ServiceBuilder;
use tower_lsp_server::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    let service = ServiceBuilder::new()
        .map_request(encode_uris)
        .service(service);

    Server::new(stdin, stdout, socket).serve(service).await;
}
