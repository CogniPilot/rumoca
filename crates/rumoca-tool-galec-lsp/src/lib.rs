//! GALEC (`.alg`) Language Server — the eFMI Algorithm Code language.
//!
//! Two layers, mirroring `rumoca-tool-lsp`:
//! - a **WASM-safe** core ([`position`] + [`diagnostics`]) over `lsp-types` and
//!   the GALEC language module, usable from a future in-browser `.alg` editor;
//! - a native stdio [`tower_lsp`] server behind the default `server` feature.
//!
//! It answers `textDocument/publishDiagnostics` (positioned parse and validator
//! diagnostics on open/change), `textDocument/hover`, and
//! `textDocument/definition` (resolving the reference under the cursor to its
//! declaration). Completion and find-references follow.

pub mod diagnostics;
pub mod navigation;
pub mod position;

pub use diagnostics::compute_diagnostics;

#[cfg(feature = "server")]
mod server;
#[cfg(feature = "server")]
pub use server::{GalecLanguageServer, run_server};
