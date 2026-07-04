//! GALEC (`.alg`) Language Server — the eFMI Algorithm Code language.
//!
//! Two layers, mirroring `rumoca-tool-lsp`:
//! - a **WASM-safe** core ([`position`] + [`diagnostics`]) over `lsp-types` and
//!   the GALEC language module, usable from a future in-browser `.alg` editor;
//! - a native stdio [`tower_lsp`] server behind the default `server` feature.
//!
//! Slice 1 answers `textDocument/publishDiagnostics`: it parses on open/change
//! and reports positioned parse and validator diagnostics. Hover / go-to /
//! completion follow once expression-level spans land.

pub mod diagnostics;
pub mod position;

pub use diagnostics::compute_diagnostics;

#[cfg(feature = "server")]
mod server;
#[cfg(feature = "server")]
pub use server::{GalecLanguageServer, run_server};
