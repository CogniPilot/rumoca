// Allow mutable_key_type for HashMap<Uri, _> - Uri's hash is based on its immutable string
// representation, making it safe to use as a key despite interior mutability in auth data.
#![allow(clippy::mutable_key_type)]

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

use crossbeam_channel::{Select, unbounded};
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::Notification as NotificationTrait;
use lsp_types::{
    CallHierarchyServerCapability, CodeActionProviderCapability, CodeLensOptions,
    CompletionOptions, Diagnostic, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DocumentLinkOptions, HoverProviderCapability, InitializeParams,
    RenameOptions, SemanticTokensFullOptions, SemanticTokensOptions,
    SemanticTokensServerCapabilities, ServerCapabilities, SignatureHelpOptions,
    TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Initialized},
    request::{
        CallHierarchyIncomingCalls, CallHierarchyOutgoingCalls, CallHierarchyPrepare,
        CodeActionRequest, CodeLensRequest, Completion, DocumentLinkRequest, DocumentSymbolRequest,
        FoldingRangeRequest, Formatting, GotoDefinition, GotoTypeDefinition, HoverRequest,
        PrepareRenameRequest, References, Rename, SemanticTokensFullRequest, SignatureHelpRequest,
        WorkspaceSymbolRequest,
    },
};
use notify::{Config, RecommendedWatcher, RecursiveMode, Watcher, event::EventKind};
use rumoca::lsp::{
    WorkspaceState, compute_diagnostics, get_semantic_token_legend, handle_code_action,
    handle_code_lens, handle_completion_workspace, handle_document_links, handle_document_symbols,
    handle_folding_range, handle_formatting, handle_goto_definition_workspace,
    handle_hover_workspace, handle_incoming_calls, handle_outgoing_calls,
    handle_prepare_call_hierarchy, handle_prepare_rename, handle_references,
    handle_rename_workspace, handle_semantic_tokens, handle_signature_help, handle_type_definition,
    handle_workspace_symbol,
};
use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};

/// Debounce delay for diagnostics (milliseconds)
const DIAGNOSTIC_DEBOUNCE_MS: u64 = 300;

/// Pending diagnostic request
struct PendingDiagnostic {
    text: String,
    changed_at: Instant,
}

/// Global debug flag, set from initialization options
static DEBUG_MODE: AtomicBool = AtomicBool::new(false);

/// Check if debug mode is enabled
fn is_debug() -> bool {
    DEBUG_MODE.load(Ordering::Relaxed)
}

/// Log a debug message (only if debug mode is enabled)
macro_rules! debug_log {
    ($($arg:tt)*) => {
        if is_debug() {
            eprintln!($($arg)*);
        }
    };
}

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Configure rayon's global thread pool to use num_cpus - 1 threads
    // This leaves one core free for the system and avoids oversubscription
    let thread_count = std::cmp::max(1, num_cpus::get().saturating_sub(1));
    rayon::ThreadPoolBuilder::new()
        .num_threads(thread_count)
        .build_global()
        .ok(); // Ignore error if pool already initialized

    // Print startup message with binary path for debugging
    let exe_path = std::env::current_exe()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|_| "unknown".to_string());
    eprintln!("Starting rumoca-lsp server");
    eprintln!("  Binary: {}", exe_path);
    eprintln!("  Version: {}", env!("CARGO_PKG_VERSION"));
    eprintln!("  Threads: {}", thread_count);

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string(), "(".to_string(), ",".to_string()]),
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
        // Inlay hints disabled - can be distracting
        inlay_hint_provider: None,
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

    // Check for debug flag in initialization options
    let mut extra_library_paths: Vec<PathBuf> = Vec::new();
    let mut debug_enabled = false;
    if let Some(options) = &init_params.initialization_options {
        if let Some(debug) = options.get("debug").and_then(|v| v.as_bool()) {
            debug_enabled = debug;
            DEBUG_MODE.store(debug, Ordering::Relaxed);
        }
        // Extract modelicaPath from initialization options
        if let Some(paths) = options.get("modelicaPath").and_then(|v| v.as_array()) {
            for path in paths {
                if let Some(path_str) = path.as_str() {
                    extra_library_paths.push(PathBuf::from(path_str));
                }
            }
        }
    }

    // Extract workspace folders for multi-file support
    let workspace_folders: Vec<PathBuf> = init_params
        .workspace_folders
        .unwrap_or_default()
        .into_iter()
        .filter_map(|folder| {
            let uri_str = folder.uri.as_str();
            if uri_str.starts_with("file://") {
                Some(PathBuf::from(folder.uri.path().as_str()))
            } else {
                None
            }
        })
        .collect();

    // Echo configuration at startup (always visible in Output panel)
    eprintln!("Configuration:");
    eprintln!("  Debug mode: {}", debug_enabled);
    if extra_library_paths.is_empty() {
        eprintln!("  Modelica paths: (none configured)");
    } else {
        eprintln!("  Modelica paths:");
        for path in &extra_library_paths {
            eprintln!("    - {}", path.display());
        }
    }
    if workspace_folders.is_empty() {
        eprintln!("  Workspace folders: (none)");
    } else {
        eprintln!("  Workspace folders:");
        for folder in &workspace_folders {
            eprintln!("    - {}", folder.display());
        }
    }

    // Also check MODELICAPATH environment variable
    if let Ok(modelica_path) = std::env::var("MODELICAPATH") {
        eprintln!("  MODELICAPATH env: {}", modelica_path);
    }

    debug_log!("[rumoca-lsp] Server initialized (debug mode enabled)");
    debug_log!("[rumoca-lsp] Starting main_loop (will initialize workspace)...");

    main_loop(connection, workspace_folders, extra_library_paths)?;
    io_threads.join()?;

    eprintln!("Shutting down rumoca-lsp server");
    Ok(())
}

fn main_loop(
    connection: Connection,
    workspace_folders: Vec<PathBuf>,
    extra_library_paths: Vec<PathBuf>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    // Create workspace state for multi-file support
    debug_log!("[rumoca-lsp] Creating WorkspaceState...");
    let mut workspace = WorkspaceState::new();
    workspace.set_debug(is_debug());
    debug_log!("[rumoca-lsp] Calling workspace.initialize() - this scans for Modelica packages...");
    let init_start = std::time::Instant::now();
    workspace.initialize(workspace_folders.clone(), extra_library_paths.clone());
    let init_elapsed = init_start.elapsed();

    // Report what was discovered (always visible)
    eprintln!(
        "Initialized in {:?} - {} files, {} packages indexed",
        init_elapsed,
        workspace.discovered_files().len(),
        workspace.symbol_count()
    );

    debug_log!(
        "[rumoca-lsp] workspace.initialize() completed in {:?}",
        init_elapsed
    );
    debug_log!(
        "[rumoca-lsp] Discovered {} files, {} package roots",
        workspace.discovered_files().len(),
        workspace.package_roots().len()
    );

    // Set up file watcher for automatic indexing of new files
    let (fs_tx, fs_rx) = unbounded();
    let _watcher = setup_file_watcher(&workspace_folders, &extra_library_paths, fs_tx);

    debug_log!("[rumoca-lsp] File watcher started, ready to process messages");

    // Pending diagnostics (debounced)
    let mut pending_diagnostics: HashMap<Uri, PendingDiagnostic> = HashMap::new();
    let debounce_duration = Duration::from_millis(DIAGNOSTIC_DEBOUNCE_MS);

    // Main event loop using select to handle both LSP messages and file events
    loop {
        // Calculate timeout for next pending diagnostic
        let timeout = pending_diagnostics
            .values()
            .map(|p| {
                let elapsed = p.changed_at.elapsed();
                if elapsed >= debounce_duration {
                    Duration::ZERO
                } else {
                    debounce_duration - elapsed
                }
            })
            .min()
            .unwrap_or(Duration::from_secs(60)); // Long timeout if no pending

        let mut sel = Select::new();
        sel.recv(&connection.receiver);
        sel.recv(&fs_rx);

        let oper = sel.select_timeout(timeout);
        match oper {
            Ok(oper) => match oper.index() {
                // LSP message received
                0 => {
                    let msg = match oper.recv(&connection.receiver) {
                        Ok(msg) => msg,
                        Err(_) => return Ok(()), // Channel closed, shutdown
                    };
                    if handle_lsp_message_debounced(
                        &connection,
                        &mut workspace,
                        msg,
                        &mut pending_diagnostics,
                    )? {
                        return Ok(()); // Shutdown requested
                    }
                }
                // File system event received
                1 => {
                    if let Ok(event) = oper.recv(&fs_rx) {
                        handle_file_event(&mut workspace, event);
                    }
                }
                _ => {}
            },
            Err(_) => {
                // Timeout - check for ready diagnostics
            }
        }

        // Process any ready diagnostics
        let now = Instant::now();
        let ready_uris: Vec<Uri> = pending_diagnostics
            .iter()
            .filter(|(_, p)| now.duration_since(p.changed_at) >= debounce_duration)
            .map(|(uri, _)| uri.clone())
            .collect();

        for uri in ready_uris {
            if let Some(pending) = pending_diagnostics.remove(&uri) {
                let diagnostics = compute_diagnostics(&uri, &pending.text, &mut workspace);
                if let Err(e) = publish_diagnostics(&connection, uri, diagnostics) {
                    debug_log!("[rumoca-lsp] Failed to publish diagnostics: {}", e);
                }
            }
        }
    }
}

/// Set up file watcher for workspace directories
fn setup_file_watcher(
    workspace_folders: &[PathBuf],
    extra_library_paths: &[PathBuf],
    tx: crossbeam_channel::Sender<notify::Event>,
) -> Option<RecommendedWatcher> {
    let tx_clone = tx.clone();
    let mut watcher = RecommendedWatcher::new(
        move |res: Result<notify::Event, notify::Error>| {
            if let Ok(event) = res {
                let _ = tx_clone.send(event);
            }
        },
        Config::default(),
    )
    .ok()?;

    // Watch workspace folders
    for folder in workspace_folders {
        if let Err(e) = watcher.watch(folder, RecursiveMode::Recursive) {
            eprintln!("Warning: Failed to watch {}: {}", folder.display(), e);
        } else {
            debug_log!("[file-watcher] Watching: {}", folder.display());
        }
    }

    // Watch library paths
    for path in extra_library_paths {
        if let Err(e) = watcher.watch(path, RecursiveMode::Recursive) {
            eprintln!("Warning: Failed to watch {}: {}", path.display(), e);
        } else {
            debug_log!("[file-watcher] Watching library: {}", path.display());
        }
    }

    eprintln!(
        "File watcher active for {} directories",
        workspace_folders.len() + extra_library_paths.len()
    );
    Some(watcher)
}

/// Handle file system events (new files, modifications, deletions)
fn handle_file_event(workspace: &mut WorkspaceState, event: notify::Event) {
    for path in event.paths {
        // Only process .mo files
        if path.extension().is_some_and(|ext| ext == "mo") {
            match event.kind {
                EventKind::Create(_) => {
                    debug_log!("[file-watcher] New file: {}", path.display());
                    if workspace.ensure_file_loaded(&path).is_some() {
                        eprintln!("Indexed new file: {}", path.display());
                    }
                }
                EventKind::Modify(_) => {
                    // File modified on disk - only update if not currently open in editor
                    // (editor changes are handled via didChange notifications)
                    debug_log!("[file-watcher] Modified: {}", path.display());
                }
                EventKind::Remove(_) => {
                    debug_log!("[file-watcher] Removed: {}", path.display());
                    // File deleted - could remove from index, but for now just log
                    eprintln!("File removed: {}", path.display());
                }
                _ => {}
            }
        }
    }
}

/// Handle a single LSP message, returns true if shutdown was requested
fn handle_lsp_message(
    connection: &Connection,
    workspace: &mut WorkspaceState,
    msg: Message,
) -> Result<bool, Box<dyn Error + Sync + Send>> {
    match msg {
        Message::Request(req) => {
            if connection.handle_shutdown(&req)? {
                return Ok(true); // Shutdown requested
            }

            let req = match cast_request::<GotoDefinition>(req) {
                Ok((id, params)) => {
                    let result = handle_goto_definition_workspace(workspace, params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<GotoTypeDefinition>(req) {
                Ok((id, params)) => {
                    let result = handle_type_definition(
                        workspace.documents(),
                        params.text_document_position_params,
                    );
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<Completion>(req) {
                Ok((id, params)) => {
                    let result = handle_completion_workspace(workspace, params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<SignatureHelpRequest>(req) {
                Ok((id, params)) => {
                    let result = handle_signature_help(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<HoverRequest>(req) {
                Ok((id, params)) => {
                    let result = handle_hover_workspace(workspace, params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<DocumentSymbolRequest>(req) {
                Ok((id, params)) => {
                    let result = handle_document_symbols(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<References>(req) {
                Ok((id, params)) => {
                    let result = handle_references(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<SemanticTokensFullRequest>(req) {
                Ok((id, params)) => {
                    let result = handle_semantic_tokens(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<WorkspaceSymbolRequest>(req) {
                Ok((id, params)) => {
                    let result = handle_workspace_symbol(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<PrepareRenameRequest>(req) {
                Ok((id, params)) => {
                    let result = handle_prepare_rename(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<Rename>(req) {
                Ok((id, params)) => {
                    let result = handle_rename_workspace(workspace, params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<FoldingRangeRequest>(req) {
                Ok((id, params)) => {
                    let result = handle_folding_range(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<CodeActionRequest>(req) {
                Ok((id, params)) => {
                    let result = handle_code_action(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<Formatting>(req) {
                Ok((id, params)) => {
                    let result = handle_formatting(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<CodeLensRequest>(req) {
                Ok((id, params)) => {
                    let result = handle_code_lens(workspace, params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<CallHierarchyPrepare>(req) {
                Ok((id, params)) => {
                    let result = handle_prepare_call_hierarchy(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<CallHierarchyIncomingCalls>(req) {
                Ok((id, params)) => {
                    let result = handle_incoming_calls(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            let req = match cast_request::<CallHierarchyOutgoingCalls>(req) {
                Ok((id, params)) => {
                    let result = handle_outgoing_calls(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(req)) => req,
            };

            match cast_request::<DocumentLinkRequest>(req) {
                Ok((id, params)) => {
                    let result = handle_document_links(workspace.documents(), params);
                    let resp = Response::new_ok(id, result);
                    connection.sender.send(Message::Response(resp))?;
                }
                Err(ExtractError::JsonError { .. }) => {}
                Err(ExtractError::MethodMismatch(_req)) => {
                    // Unknown request, ignore
                }
            };
        }
        Message::Response(_resp) => {
            // We don't send requests to the client, so we don't expect responses
        }
        Message::Notification(notif) => {
            let notif = match cast_notification::<Initialized>(notif) {
                Ok(_params) => {
                    eprintln!("Client initialized");
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(notif)) => notif,
            };

            let notif = match cast_notification::<DidOpenTextDocument>(notif) {
                Ok(params) => {
                    handle_did_open(connection, workspace, params)?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(notif)) => notif,
            };

            let notif = match cast_notification::<DidChangeTextDocument>(notif) {
                Ok(params) => {
                    handle_did_change(connection, workspace, params)?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(notif)) => notif,
            };

            match cast_notification::<DidCloseTextDocument>(notif) {
                Ok(params) => {
                    handle_did_close(workspace, params);
                }
                Err(ExtractError::JsonError { .. }) => {}
                Err(ExtractError::MethodMismatch(_notif)) => {
                    // Unknown notification, ignore
                }
            };
        }
    }

    Ok(false)
}

/// Handle a single LSP message with debounced diagnostics
fn handle_lsp_message_debounced(
    connection: &Connection,
    workspace: &mut WorkspaceState,
    msg: Message,
    pending_diagnostics: &mut HashMap<Uri, PendingDiagnostic>,
) -> Result<bool, Box<dyn Error + Sync + Send>> {
    match msg {
        Message::Notification(notif) => {
            // Handle DidOpen - compute diagnostics immediately (first open)
            let notif = match cast_notification::<DidOpenTextDocument>(notif) {
                Ok(params) => {
                    let uri = params.text_document.uri.clone();
                    let text = params.text_document.text.clone();
                    workspace.open_document(uri.clone(), text.clone());
                    let diagnostics = compute_diagnostics(&uri, &text, workspace);
                    publish_diagnostics(connection, uri, diagnostics)?;
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(notif)) => notif,
            };

            // Handle DidChange - defer diagnostics (debounced)
            let notif = match cast_notification::<DidChangeTextDocument>(notif) {
                Ok(params) => {
                    let uri = params.text_document.uri.clone();
                    if let Some(change) = params.content_changes.into_iter().next() {
                        let text = change.text;
                        workspace.update_document(uri.clone(), text.clone());
                        // Queue for debounced diagnostics
                        pending_diagnostics.insert(
                            uri,
                            PendingDiagnostic {
                                text,
                                changed_at: Instant::now(),
                            },
                        );
                    }
                    return Ok(false);
                }
                Err(ExtractError::JsonError { .. }) => return Ok(false),
                Err(ExtractError::MethodMismatch(notif)) => notif,
            };

            // Handle DidClose
            match cast_notification::<DidCloseTextDocument>(notif) {
                Ok(params) => {
                    pending_diagnostics.remove(&params.text_document.uri);
                    workspace.close_document(&params.text_document.uri);
                }
                Err(ExtractError::JsonError { .. }) => {}
                Err(ExtractError::MethodMismatch(notif)) => {
                    // Pass other notifications to the regular handler
                    return handle_lsp_message(connection, workspace, Message::Notification(notif));
                }
            };
            Ok(false)
        }
        // Pass other message types to the regular handler
        other => handle_lsp_message(connection, workspace, other),
    }
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

    let diagnostics = compute_diagnostics(&uri, &text, workspace);
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

        let diagnostics = compute_diagnostics(&uri, &text, workspace);
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
