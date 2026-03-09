//! Language Server Protocol implementation for Modelica.
//!
//! This crate provides LSP support for Modelica files, including:
//! - Real-time diagnostics (syntax errors, lint warnings)
//! - Document symbols (file outline)
//! - Semantic tokens (rich syntax highlighting)
//! - Code completion (scope-aware with dot-completion)
//! - Hover information (keywords, components, classes, builtins)
//! - Go-to-definition (local + cross-file via DefId)
//! - Find references (occurrence-based)
//! - Rename (all occurrences + end-name)
//! - Workspace symbols (fuzzy search)
//! - Signature help (function parameter hints)
//! - Folding ranges (classes, sections, comments)
//! - Document formatting (via rumoca-tool-fmt)
//! - Code lens (balance status for models)
//! - Code actions (quick fixes from diagnostics)
//!
//! # Architecture
//!
//! The handler code uses `lsp_types` directly and compiles to WASM.
//! The `tower-lsp` server is behind the `server` feature flag (native only).

pub mod handlers;
pub mod helpers;
mod traversal_adapter;

#[cfg(feature = "server")]
mod server;

#[cfg(feature = "server")]
pub use server::{ModelicaLanguageServer, run_server};

// Re-export key handler functions
pub use handlers::{
    DocSymbols, compute_diagnostics, get_semantic_token_legend, handle_code_actions,
    handle_code_lens, handle_completion, handle_document_symbols, handle_folding_ranges,
    handle_formatting, handle_goto_definition, handle_hover, handle_prepare_rename,
    handle_references, handle_rename, handle_semantic_tokens, handle_signature_help,
    handle_workspace_symbols,
};
