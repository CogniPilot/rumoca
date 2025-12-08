//! LSP request handlers.

pub mod call_hierarchy;
pub mod completion;
pub mod document_symbols;
pub mod formatting;
pub mod goto_definition;
pub mod hover;
pub mod references;
pub mod rename;
pub mod semantic_tokens;
pub mod signature_help;
pub mod type_definition;
pub mod workspace_symbols;

pub use call_hierarchy::{
    handle_incoming_calls, handle_outgoing_calls, handle_prepare_call_hierarchy,
};
pub use completion::handle_completion_workspace;
pub use document_symbols::handle_document_symbols;
pub use formatting::handle_formatting;
pub use goto_definition::{handle_goto_definition, handle_goto_definition_workspace};
pub use hover::{handle_hover, handle_hover_workspace};
pub use references::handle_references;
pub use rename::{handle_prepare_rename, handle_rename, handle_rename_workspace};
pub use semantic_tokens::{get_semantic_token_legend, handle_semantic_tokens};
pub use signature_help::handle_signature_help;
pub use type_definition::handle_type_definition;
pub use workspace_symbols::handle_workspace_symbol;
