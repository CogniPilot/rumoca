//! GALEC (`.alg`) Language Server binary — speaks LSP over stdio.
//!
//! Editors may pass `--stdio`; there are no other options, and unrecognized
//! arguments are ignored (stdio is the only transport).

#[tokio::main]
async fn main() {
    rumoca_tool_galec_lsp::run_server().await;
}
