//! Native stdio GALEC language server (tower-lsp), behind the `server` feature.
//!
//! Deliberately lean: a document store keyed by URI plus diagnostics on
//! open/change. Text sync is FULL — each change carries the whole buffer, so no
//! range patching. Unlike the Modelica server there are no solver/session
//! dependencies; the analysis is `parse` + `validate` only.

use std::collections::HashMap;
use std::sync::Arc;

use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, MessageType, OneOf, ServerCapabilities,
    ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::diagnostics::compute_diagnostics;
use crate::navigation;

/// The GALEC `.alg` language server: a document store plus diagnostics on open
/// and change.
#[derive(Clone)]
pub struct GalecLanguageServer {
    client: Client,
    /// URI key → current full source text (FULL text sync).
    documents: Arc<RwLock<HashMap<String, String>>>,
}

impl GalecLanguageServer {
    /// Create a server bound to a tower-lsp [`Client`] (for pushing
    /// diagnostics and log messages).
    #[must_use]
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// The capabilities advertised at `initialize`: FULL text sync (diagnostics
    /// are pushed, so no additional providers are needed for slice 1).
    fn server_capabilities() -> ServerCapabilities {
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            definition_provider: Some(OneOf::Left(true)),
            ..Default::default()
        }
    }

    /// The current text of a document by URI, if open.
    async fn document_text(&self, uri: &Url) -> Option<String> {
        self.documents.read().await.get(&document_key(uri)).cloned()
    }

    /// Store a document's text and publish its diagnostics.
    async fn analyze(&self, uri: Url, text: String) {
        let key = document_key(&uri);
        let diagnostics = compute_diagnostics(&text, &key);
        self.documents.write().await.insert(key, text);
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for GalecLanguageServer {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: Self::server_capabilities(),
            server_info: Some(ServerInfo {
                name: "rumoca-galec-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "rumoca-galec-lsp ready")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.analyze(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        // FULL sync: the final content change is the entire new buffer.
        if let Some(change) = params.content_changes.into_iter().last() {
            self.analyze(params.text_document.uri, change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let key = document_key(&params.text_document.uri);
        self.documents.write().await.remove(&key);
        // Clear the closed document's diagnostics from the editor.
        self.client
            .publish_diagnostics(params.text_document.uri, Vec::new(), None)
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(text) = self.document_text(&uri).await else {
            return Ok(None);
        };
        Ok(navigation::hover(&text, &document_key(&uri), position))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(text) = self.document_text(&uri).await else {
            return Ok(None);
        };
        Ok(navigation::goto_definition(
            &text,
            &document_key(&uri),
            uri,
            position,
        ))
    }
}

/// Normalize an LSP URL to the document-store key: the decoded filesystem path
/// for a `file:` URL (so `%20` etc. are decoded), else the raw URL path.
fn document_key(uri: &Url) -> String {
    uri.to_file_path()
        .map(|path| path.to_string_lossy().into_owned())
        .unwrap_or_else(|()| uri.path().to_string())
}

/// Serve the GALEC language server over stdio until the client disconnects.
pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(GalecLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_util::StreamExt;
    use tower_lsp::lsp_types::{TextDocumentIdentifier, TextDocumentItem};

    /// Run an async test body on a current-thread runtime, draining the client
    /// socket so `publish_diagnostics` never blocks (mirrors the Modelica LSP
    /// harness). Server state is asserted directly — no socket-timing races.
    fn drive<F>(body: impl FnOnce(GalecLanguageServer) -> F)
    where
        F: std::future::Future<Output = ()>,
    {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("runtime");
        runtime.block_on(serve_then(body));
    }

    /// Stand up a service, drain its socket, and hand the server to `body`.
    async fn serve_then<F>(body: impl FnOnce(GalecLanguageServer) -> F)
    where
        F: std::future::Future<Output = ()>,
    {
        let (service, mut socket) = LspService::new(GalecLanguageServer::new);
        tokio::spawn(async move { while socket.next().await.is_some() {} });
        body(service.inner().clone()).await;
    }

    fn open(uri: &Url, text: &str) -> DidOpenTextDocumentParams {
        DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "galec".to_string(),
                version: 1,
                text: text.to_string(),
            },
        }
    }

    fn close(uri: Url) -> DidCloseTextDocumentParams {
        DidCloseTextDocumentParams {
            text_document: TextDocumentIdentifier { uri },
        }
    }

    #[test]
    fn initialize_advertises_full_text_sync() {
        drive(|server| async move {
            let result = server
                .initialize(InitializeParams::default())
                .await
                .expect("initialize");
            assert!(matches!(
                result.capabilities.text_document_sync,
                Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL))
            ));
            assert_eq!(
                result.server_info.expect("server info").name,
                "rumoca-galec-lsp"
            );
        });
    }

    #[test]
    fn did_open_then_close_tracks_the_document() {
        // A syntactically bad document exercises the parse-error path end to end
        // through the server (analyze -> compute_diagnostics -> publish).
        let text = "block Bad\nprotected\npublic\n\
                    method Startup\nalgorithm\nend Startup;\n\
                    method Recalibrate\nalgorithm\nend Recalibrate;\n\
                    method DoStep\nalgorithm\n1 := 2;\nend DoStep;\n\
                    end Bad;\n";
        drive(|server| async move {
            let uri = Url::parse("file:///ctrl.alg").expect("uri");
            server.did_open(open(&uri, text)).await;
            assert_eq!(server.documents.read().await.len(), 1, "document stored");
            server.did_close(close(uri)).await;
            assert!(
                server.documents.read().await.is_empty(),
                "document removed on close"
            );
        });
    }
}
