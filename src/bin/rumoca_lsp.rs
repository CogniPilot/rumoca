//! Rumoca Language Server Protocol (LSP) binary.
//!
//! This binary provides LSP support for Modelica files, including:
//! - Real-time diagnostics
//! - Code completion with workspace symbols
//! - Signature help
//! - Hover information
//! - Go to definition (local and cross-file)
//! - Go to type definition
//! - Find all references
//! - Document symbols (file outline)
//! - Workspace symbols
//! - Semantic tokens (rich syntax highlighting)
//! - Rename symbol (local and workspace-wide)
//! - Code folding
//! - Code actions (quick fixes)
//! - Inlay hints
//! - Multi-file workspace support
//! - Code formatting
//! - Code lenses
//! - Call hierarchy
//! - Document links

// Allow mutable key type warning - Uri has interior mutability but we use it correctly
#![allow(clippy::mutable_key_type)]

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::Notification as NotificationTrait;
use lsp_types::{
    CallHierarchyServerCapability, CodeActionProviderCapability, CodeLensOptions,
    CompletionOptions, Diagnostic, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DocumentLinkOptions, HoverProviderCapability, InitializeParams,
    InlayHintOptions, InlayHintServerCapabilities, RenameOptions, ServerCapabilities,
    SemanticTokensFullOptions, SemanticTokensOptions, SemanticTokensServerCapabilities,
    SignatureHelpOptions, TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Initialized},
    request::{
        CallHierarchyIncomingCalls, CallHierarchyOutgoingCalls, CallHierarchyPrepare,
        CodeActionRequest, CodeLensRequest, Completion, DocumentLinkRequest, DocumentSymbolRequest,
        Formatting, FoldingRangeRequest, GotoDefinition, GotoTypeDefinition, HoverRequest,
        InlayHintRequest, PrepareRenameRequest, References, Rename, SemanticTokensFullRequest,
        SignatureHelpRequest, WorkspaceSymbolRequest,
    },
};
use rumoca::lsp::{
    WorkspaceState, compute_diagnostics, get_semantic_token_legend, handle_code_action,
    handle_code_lens, handle_completion_workspace, handle_document_links, handle_document_symbols,
    handle_folding_range, handle_formatting, handle_goto_definition_workspace, handle_hover,
    handle_incoming_calls, handle_inlay_hints, handle_outgoing_calls, handle_prepare_call_hierarchy,
    handle_prepare_rename, handle_references, handle_rename_workspace, handle_semantic_tokens,
    handle_signature_help, handle_type_definition, handle_workspace_symbol,
};
use std::error::Error;
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("Starting rumoca-lsp server");

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string(), "(".to_string()]),
            resolve_provider: Some(false),
            ..Default::default()
        }),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: Some(vec![",".to_string()]),
            ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(lsp_types::OneOf::Left(true)),
        type_definition_provider: Some(lsp_types::TypeDefinitionProviderCapability::Simple(true)),
        references_provider: Some(lsp_types::OneOf::Left(true)),
        document_symbol_provider: Some(lsp_types::OneOf::Left(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: get_semantic_token_legend(),
                full: Some(SemanticTokensFullOptions::Bool(true)),
                range: None,
                ..Default::default()
            },
        )),
        workspace_symbol_provider: Some(lsp_types::OneOf::Left(true)),
        rename_provider: Some(lsp_types::OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: Default::default(),
        })),
        folding_range_provider: Some(lsp_types::FoldingRangeProviderCapability::Simple(true)),
        code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
        inlay_hint_provider: Some(lsp_types::OneOf::Right(InlayHintServerCapabilities::Options(
            InlayHintOptions {
                work_done_progress_options: Default::default(),
                resolve_provider: Some(false),
            },
        ))),
        document_formatting_provider: Some(lsp_types::OneOf::Left(true)),
        code_lens_provider: Some(CodeLensOptions {
            resolve_provider: Some(false),
        }),
        call_hierarchy_provider: Some(CallHierarchyServerCapability::Simple(true)),
        document_link_provider: Some(DocumentLinkOptions {
            resolve_provider: Some(false),
            work_done_progress_options: Default::default(),
        }),
        ..Default::default()
    })?;

    let init_params = match connection.initialize(server_capabilities) {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };

    let init_params: InitializeParams = serde_json::from_value(init_params)?;
    eprintln!("Server initialized");

    // Extract workspace folders for multi-file support
    let workspace_folders: Vec<PathBuf> = init_params
        .workspace_folders
        .unwrap_or_default()
        .into_iter()
        .filter_map(|folder| {
            // Uri in lsp_types uses fluent_uri
            // Extract the file path from the URI
            let uri_str = folder.uri.as_str();
            if uri_str.starts_with("file://") {
                Some(PathBuf::from(folder.uri.path().as_str()))
            } else {
                None
            }
        })
        .collect();

    eprintln!("Workspace folders: {:?}", workspace_folders);

    main_loop(connection, workspace_folders)?;
    io_threads.join()?;

    eprintln!("Shutting down rumoca-lsp server");
    Ok(())
}

fn main_loop(
    connection: Connection,
    workspace_folders: Vec<PathBuf>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    // Create workspace state for multi-file support
    let mut workspace = WorkspaceState::new();
    workspace.initialize(workspace_folders);

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                let req = match cast_request::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        // Use workspace-aware go to definition
                        let result = handle_goto_definition_workspace(&workspace, params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<GotoTypeDefinition>(req) {
                    Ok((id, params)) => {
                        let result =
                            handle_type_definition(workspace.documents(), params.text_document_position_params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<Completion>(req) {
                    Ok((id, params)) => {
                        // Use workspace-aware completion
                        let result = handle_completion_workspace(&workspace, params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<SignatureHelpRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_signature_help(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<HoverRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_hover(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<DocumentSymbolRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_document_symbols(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<References>(req) {
                    Ok((id, params)) => {
                        let result = handle_references(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<SemanticTokensFullRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_semantic_tokens(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<WorkspaceSymbolRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_workspace_symbol(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<PrepareRenameRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_prepare_rename(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<Rename>(req) {
                    Ok((id, params)) => {
                        // Use workspace-aware rename
                        let result = handle_rename_workspace(&workspace, params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<FoldingRangeRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_folding_range(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<CodeActionRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_code_action(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<InlayHintRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_inlay_hints(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<Formatting>(req) {
                    Ok((id, params)) => {
                        let result = handle_formatting(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<CodeLensRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_code_lens(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<CallHierarchyPrepare>(req) {
                    Ok((id, params)) => {
                        let result = handle_prepare_call_hierarchy(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<CallHierarchyIncomingCalls>(req) {
                    Ok((id, params)) => {
                        let result = handle_incoming_calls(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<CallHierarchyOutgoingCalls>(req) {
                    Ok((id, params)) => {
                        let result = handle_outgoing_calls(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                match cast_request::<DocumentLinkRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_document_links(workspace.documents(), params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(_req)) => {
                        // Unknown request, ignore
                    }
                };
            }
            Message::Response(_resp) => {
                // We don't send requests, so we don't expect responses
            }
            Message::Notification(notif) => {
                // Handle initialized notification
                let notif = match cast_notification::<Initialized>(notif) {
                    Ok(_params) => {
                        eprintln!("Client initialized");
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(notif)) => notif,
                };

                let notif = match cast_notification::<DidOpenTextDocument>(notif) {
                    Ok(params) => {
                        handle_did_open(&connection, &mut workspace, params)?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(notif)) => notif,
                };

                let notif = match cast_notification::<DidChangeTextDocument>(notif) {
                    Ok(params) => {
                        handle_did_change(&connection, &mut workspace, params)?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(notif)) => notif,
                };

                match cast_notification::<DidCloseTextDocument>(notif) {
                    Ok(params) => {
                        handle_did_close(&mut workspace, params);
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(_notif)) => {
                        // Unknown notification, ignore
                    }
                };
            }
        }
    }

    Ok(())
}

fn cast_request<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_notification<N>(notif: Notification) -> Result<N::Params, ExtractError<Notification>>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    notif.extract(N::METHOD)
}

fn handle_did_open(
    connection: &Connection,
    workspace: &mut WorkspaceState,
    params: DidOpenTextDocumentParams,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let uri = params.text_document.uri.clone();
    let text = params.text_document.text;

    // Open document in workspace (this also parses and indexes the file)
    workspace.open_document(uri.clone(), text.clone());

    let diagnostics = compute_diagnostics(&uri, &text);
    publish_diagnostics(connection, uri, diagnostics)?;

    Ok(())
}

fn handle_did_change(
    connection: &Connection,
    workspace: &mut WorkspaceState,
    params: DidChangeTextDocumentParams,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let uri = params.text_document.uri.clone();

    if let Some(change) = params.content_changes.into_iter().next() {
        let text = change.text;

        // Update document in workspace (this also re-parses and re-indexes the file)
        workspace.update_document(uri.clone(), text.clone());

        let diagnostics = compute_diagnostics(&uri, &text);
        publish_diagnostics(connection, uri, diagnostics)?;
    }

    Ok(())
}

fn handle_did_close(workspace: &mut WorkspaceState, params: DidCloseTextDocumentParams) {
    workspace.close_document(&params.text_document.uri);
}

fn publish_diagnostics(
    connection: &Connection,
    uri: Uri,
    diagnostics: Vec<Diagnostic>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let params = lsp_types::PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };
    let notif = Notification::new(
        <lsp_types::notification::PublishDiagnostics as NotificationTrait>::METHOD.to_string(),
        params,
    );
    connection.sender.send(Message::Notification(notif))?;
    Ok(())
}
