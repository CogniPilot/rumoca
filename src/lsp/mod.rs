// Allow mutable_key_type for HashMap<Uri, _> - Uri's hash is based on its immutable string
// representation, making it safe to use as a key despite interior mutability in auth data.
#![allow(clippy::mutable_key_type)]

//! Language Server Protocol implementation for Rumoca Modelica compiler.
//!
//! This module provides LSP support for Modelica files, including:
//! - Real-time diagnostics
//! - Code completion
//! - Signature help
//! - Hover information
//! - Go to definition
//! - Go to type definition
//! - Find all references
//! - Document symbols (file outline)
//! - Semantic tokens (rich syntax highlighting)
//! - Rename symbol
//! - Code folding
//! - Code actions (quick fixes)
//! - Inlay hints
//! - Multi-file workspace support
//! - Code formatting
//! - Code lenses
//! - Call hierarchy
//! - Document links

pub mod analyze;
pub mod data;
pub mod features;
pub mod handlers;
pub mod utils;
pub mod workspace;

// Re-export public API
pub use data::{BuiltinFunction, get_builtin_functions};
pub use features::{
    compute_diagnostics, handle_code_action, handle_code_lens, handle_document_links,
    handle_folding_range, handle_inlay_hints,
};
pub use handlers::{
    get_semantic_token_legend, handle_completion_workspace, handle_document_symbols,
    handle_formatting, handle_goto_definition, handle_goto_definition_workspace, handle_hover,
    handle_hover_workspace, handle_incoming_calls, handle_outgoing_calls,
    handle_prepare_call_hierarchy, handle_prepare_rename, handle_references, handle_rename,
    handle_rename_workspace, handle_semantic_tokens, handle_signature_help, handle_type_definition,
    handle_workspace_symbol,
};
pub use utils::parse_document;
pub use workspace::{WorkspaceState, collect_import_roots_from_def};

use lsp_types::Uri;
use std::collections::HashMap;

/// Create a document map for LSP tests (helper to keep mutable_key_type allow in lsp module)
pub fn create_documents(uri: &Uri, content: &str) -> HashMap<Uri, String> {
    let mut docs = HashMap::new();
    docs.insert(uri.clone(), content.to_string());
    docs
}
