//! Rumoca Language Server Protocol (LSP) binary.

#[tokio::main]
async fn main() {
    rumoca_tool_lsp::run_server().await;
}
