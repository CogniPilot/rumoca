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
pub use analyze::{AnalyzeResult, analyze_class};
pub use data::get_builtin_functions;
pub use features::{
    ANALYZE_CLASS_COMMAND, compute_diagnostics, handle_code_action, handle_code_lens,
    handle_document_links, handle_folding_range, handle_inlay_hints,
};
pub use handlers::{
    get_semantic_token_legend, handle_completion, handle_completion_workspace,
    handle_document_symbols, handle_formatting, handle_goto_definition,
    handle_goto_definition_workspace, handle_hover, handle_incoming_calls, handle_outgoing_calls,
    handle_prepare_call_hierarchy, handle_prepare_rename, handle_references, handle_rename,
    handle_rename_workspace, handle_semantic_tokens, handle_signature_help, handle_type_definition,
    handle_workspace_symbol,
};
pub use utils::parse_document;
pub use workspace::WorkspaceState;
