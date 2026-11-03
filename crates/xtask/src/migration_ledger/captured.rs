//! Enforce the unchanged freeze over one retained complete source capture.
//!
//! This cannot admit a changed region. Source stability through actual builds
//! and checked AST/API/output products remain separate joint-gate obligations.

#[cfg(test)]
mod tests;

use super::{BACKEND, LEDGER_PATH, REFERENCE, REFERENCE_SHA256};
use crate::print_boundary_check::source_closure::SourceClosure;
use anyhow::{Result, ensure};
use std::path::Path;

/// Return the unresolved count only if the entire captured backend is unchanged.
/// A caller cannot replace full directory coverage with the expected file list.
/// This reads only retained bytes/identities; it never reopens the live checkout.
pub fn check_unchanged_capture(capture: &SourceClosure) -> Result<usize> {
    ensure!(
        super::digest(REFERENCE.as_bytes()) == REFERENCE_SHA256,
        "immutable audit baseline changed"
    );
    check_with_reference(capture, REFERENCE)
}

// Only the parent gate supplies the pinned reference; synthetic references are
// used by its private unit fixtures. There is no public reference override.
pub(super) fn check_with_reference(capture: &SourceClosure, reference: &str) -> Result<usize> {
    let ledger = super::parse_ledger(reference)?;
    ensure!(
        capture.file(Path::new(LEDGER_PATH))?.bytes() == reference.as_bytes(),
        "migration ledger changed; rebaselining or status-only retirement is forbidden"
    );
    let directory = capture.tree(Path::new(BACKEND))?;
    let actual = directory
        .files(capture)?
        .map(|(path, file)| (path.to_owned(), file.sha256().to_owned()))
        .collect();
    super::check_inventory(&ledger, &actual)
}
