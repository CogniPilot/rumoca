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

pub mod builtin_functions;
pub mod call_hierarchy;
pub mod code_actions;
pub mod code_lens;
pub mod completion;
pub mod diagnostics;
pub mod document_links;
pub mod document_symbols;
pub mod folding;
pub mod formatting;
pub mod goto_definition;
pub mod hover;
pub mod inlay_hints;
pub mod references;
pub mod rename;
pub mod semantic_tokens;
pub mod signature_help;
pub mod type_definition;
pub mod utils;
pub mod workspace;
pub mod workspace_symbols;

pub use builtin_functions::get_builtin_functions;
pub use call_hierarchy::{
    handle_incoming_calls, handle_outgoing_calls, handle_prepare_call_hierarchy,
};
pub use code_actions::handle_code_action;
pub use code_lens::handle_code_lens;
pub use completion::{handle_completion, handle_completion_workspace};
pub use diagnostics::compute_diagnostics;
pub use document_links::handle_document_links;
pub use document_symbols::handle_document_symbols;
pub use folding::handle_folding_range;
pub use formatting::handle_formatting;
pub use goto_definition::{handle_goto_definition, handle_goto_definition_workspace};
pub use hover::handle_hover;
pub use inlay_hints::handle_inlay_hints;
pub use references::handle_references;
pub use rename::{handle_prepare_rename, handle_rename, handle_rename_workspace};
pub use semantic_tokens::{get_semantic_token_legend, handle_semantic_tokens};
pub use signature_help::handle_signature_help;
pub use type_definition::handle_type_definition;
pub use utils::parse_document;
pub use workspace::WorkspaceState;
pub use workspace_symbols::handle_workspace_symbol;
