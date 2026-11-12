//! Enforce the freeze over one retained complete source capture.
//!
//! This admits no changed region beyond the pinned owner-directed revisions.
//! Source stability through actual builds and checked AST/API/output products
//! remain separate joint-gate obligations.

#[cfg(test)]
mod tests;

use super::{BACKEND, LEDGER_PATH, REFERENCE, REVISIONS, REVISIONS_PATH};
use crate::print_boundary_check::source_closure::SourceClosure;
use anyhow::{Result, ensure};
use std::path::Path;

/// Return the unresolved count only if the entire captured backend matches the
/// ledger as revised by the pinned revision record, and both records are
/// captured byte for byte. A caller cannot replace full directory coverage with
/// the expected file list. This reads only retained bytes/identities; it never
/// reopens the live checkout.
pub fn check_unchanged_capture(capture: &SourceClosure) -> Result<usize> {
    super::check_pins()?;
    check_with_reference(capture, REFERENCE, REVISIONS)
}

// Only the parent gate supplies the pinned references; synthetic references are
// used by its private unit fixtures. There is no public reference override.
pub(super) fn check_with_reference(
    capture: &SourceClosure,
    reference: &str,
    revisions: &str,
) -> Result<usize> {
    let ledger = super::parse_ledger(reference)?;
    ensure!(
        capture.file(Path::new(LEDGER_PATH))?.bytes() == reference.as_bytes(),
        "migration ledger changed; rebaselining or status-only retirement is forbidden"
    );
    ensure!(
        capture.file(Path::new(REVISIONS_PATH))?.bytes() == revisions.as_bytes(),
        "backend revision record changed; only the pinned owner-directed record applies"
    );
    let directory = capture.tree(Path::new(BACKEND))?;
    let actual = directory
        .files(capture)?
        .map(|(path, file)| (path.to_owned(), file.sha256().to_owned()))
        .collect();
    super::check_inventory(&ledger, revisions, &actual)
}
