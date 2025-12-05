//! LSP feature implementations.

pub mod code_actions;
pub mod code_lens;
pub mod diagnostics;
pub mod document_links;
pub mod folding;
pub mod inlay_hints;

pub use code_actions::handle_code_action;
pub use code_lens::handle_code_lens;
pub use diagnostics::compute_diagnostics;
pub use document_links::handle_document_links;
pub use folding::handle_folding_range;
pub use inlay_hints::handle_inlay_hints;
