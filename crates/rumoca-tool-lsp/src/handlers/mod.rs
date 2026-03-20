//! LSP request handlers.
//!
//! These handlers use `lsp_types` directly and compile to WASM.
//! They accept parsed AST from `rumoca-session` and return LSP responses.

mod diagnostics;
mod document_symbols;
mod semantic_tokens;

// Phase 1: Foundation
mod completion;
mod folding;
mod formatting;
pub mod hover;
pub mod workspace_symbols;

// Phase 2: Navigation
mod goto_definition;

// Phase 3: Rich Intelligence
mod code_actions;
mod code_lens;
mod document_links;
mod inlay_hints;
mod signature_help;

pub use diagnostics::compute_diagnostics;
#[cfg(feature = "server")]
pub(crate) use diagnostics::compute_diagnostics_with_mode;
pub use document_symbols::handle_document_symbols;
pub use semantic_tokens::{get_semantic_token_legend, handle_semantic_tokens};

pub use completion::handle_completion;
#[cfg(feature = "server")]
pub(crate) use completion::handle_completion_with_snapshot_and_provenance;
pub use hover::handle_hover;

pub use folding::handle_folding_ranges;
pub use formatting::handle_formatting;
pub use workspace_symbols::handle_workspace_symbols;

pub use goto_definition::handle_goto_definition;

pub use code_actions::handle_code_actions;
pub use code_lens::handle_code_lens;
pub use document_links::handle_document_links;
pub use inlay_hints::handle_inlay_hints;
pub use signature_help::handle_signature_help;

#[cfg(feature = "server")]
pub(crate) use diagnostics::common_diagnostics_for_file;
