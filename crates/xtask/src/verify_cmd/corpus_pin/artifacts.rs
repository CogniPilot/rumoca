//! Reading a compile row's declared output artifacts back off disk.
//!
//! # The hole this closes
//!
//! A compile row judged on the exit status alone certifies that the compiler
//! returned zero, and nothing about what it wrote. The navigation estimator row
//! emits close to a megabyte of C and an eFMU container; a target that started
//! writing an empty file, or stopped writing one of them altogether, would keep
//! the row green. So each compile row names the files its target is pinned to
//! emit and the gate reads them back.
//!
//! # How deep the check goes
//!
//! Deliberately shallow, and shallow on purpose rather than by omission: the
//! deep checks on eFMU structure, schema validity and the checksum web are the
//! packaging suite's, run against a small model where a failure names one
//! thing. What this gate adds is that the *real* models still produce those
//! artifacts at all. Every declared artifact must be a regular file at or above
//! the size floor the row pins, and an artifact declared as an eFMU container
//! must additionally open as a zip carrying a non-empty `__content.xml` at its
//! root, which is the same marker the packaging step uses to recognize one of
//! its own products.

use std::fs;
use std::path::Path;

use super::manifest::{ArtifactKind, ExpectedArtifact};

/// The marker entry an eFMU container carries at its root, in both the
/// directory and the zip form.
const EFMU_CONTENT_MARKER: &str = "__content.xml";

/// Findings for one compile row's declared artifacts. Empty when every
/// declared artifact is there and holds what its kind requires.
pub(crate) fn judge(output_dir: &Path, artifacts: &[ExpectedArtifact]) -> Vec<String> {
    if artifacts.is_empty() {
        return vec![format!(
            "pinned to compile but declares no output artifact, so this row proves only that \
             the process exited zero. List the files the target must emit under \
             `expect.artifacts`; they are read back from {}.",
            output_dir.display()
        )];
    }
    artifacts
        .iter()
        .filter_map(|artifact| inspect(output_dir, artifact))
        .collect()
}

/// `None` when the artifact is present and admissible.
fn inspect(output_dir: &Path, artifact: &ExpectedArtifact) -> Option<String> {
    let path = output_dir.join(&artifact.path);
    let complaint = |what: String| {
        Some(format!(
            "declared {} `{}` {what}. The target is pinned to emit it under {}; a target that \
             stops emitting a declared artifact, or emits a different one, is a behavior change.",
            artifact.kind.label(),
            artifact.path,
            output_dir.display()
        ))
    };
    let Ok(metadata) = fs::metadata(&path) else {
        return complaint("is missing".to_string());
    };
    if !metadata.is_file() {
        return complaint("is not a regular file".to_string());
    }
    let size = metadata.len();
    if size < artifact.min_bytes {
        return complaint(format!(
            "is {size} bytes, under its pinned floor of {} bytes",
            artifact.min_bytes
        ));
    }
    match artifact.kind {
        ArtifactKind::File => None,
        ArtifactKind::EfmuContainer => match container_defect(&path) {
            Some(defect) => complaint(defect),
            None => None,
        },
    }
}

/// The cheapest structural statement an eFMU zip makes about itself: it opens
/// as a zip and carries a non-empty `__content.xml` at the archive root.
///
/// `None` when the container passes.
fn container_defect(path: &Path) -> Option<String> {
    let Ok(file) = fs::File::open(path) else {
        return Some("could not be opened".to_string());
    };
    let mut archive = match zip::ZipArchive::new(file) {
        Ok(archive) => archive,
        Err(error) => return Some(format!("does not open as a zip container ({error})")),
    };
    let Ok(marker) = archive.by_name(EFMU_CONTENT_MARKER) else {
        return Some(format!(
            "opens as a zip but carries no `{EFMU_CONTENT_MARKER}` at its root, so it is not an \
             eFMU container"
        ));
    };
    if !marker.is_file() {
        return Some(format!("has a non-file `{EFMU_CONTENT_MARKER}` entry"));
    }
    if marker.size() == 0 {
        return Some(format!("carries an empty `{EFMU_CONTENT_MARKER}`"));
    }
    None
}
