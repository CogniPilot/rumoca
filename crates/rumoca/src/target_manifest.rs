//! Thin target-loading facade over the compiler-owned artifact operation.
//!
//! Target refinement, semantic rendering, checksums, package assembly, and
//! transactional publication belong to `rumoca-compile`. This module owns
//! only target loading and delegates one checked target to one retained
//! [`CompilationResult`].

#[cfg(feature = "fmu-packaging")]
use std::path::Path;

use anyhow::Result;
#[cfg(feature = "fmu-packaging")]
use rumoca_compile::codegen::targets::PublishedTargetArtifact;
use rumoca_compile::codegen::targets::{
    ArtifactSessionInput, CheckedTargetBundle, CompletedRenderedFile, CompletedTargetArtifact,
    TargetBundle,
};

use crate::CompilationResult;

/// Load and close one built-in or directory target.
fn resolve_manifest_target(target: &str) -> Result<CheckedTargetBundle> {
    TargetBundle::load(target)?.check()
}

/// Consume one checked target through the compiler's sole semantic-to-byte
/// operation.
fn render_target_artifact(
    result: &CompilationResult,
    target: &str,
    artifact_input: ArtifactSessionInput,
) -> Result<CompletedTargetArtifact> {
    let target = resolve_manifest_target(target)?;
    result.strict().render_target(target, artifact_input)
}

/// Render a target in memory. Packaged assets remain package-only; the return
/// value is the exact rendered-file subsequence issued by the completed
/// artifact.
pub fn render_target_files(
    result: &CompilationResult,
    target: &str,
    artifact_input: ArtifactSessionInput,
) -> Result<Vec<CompletedRenderedFile>> {
    Ok(render_target_artifact(result, target, artifact_input)?.into_rendered_files())
}

/// Render and transactionally publish one manifest target beneath an explicit
/// host output root.
///
/// The completed artifact is a closed sum. Unpackaged publication is always
/// available; package publication requires the feature that supplies the ZIP
/// writer.
#[cfg(feature = "fmu-packaging")]
pub fn compile_target(
    result: &CompilationResult,
    target: &str,
    output_root: &Path,
    artifact_input: ArtifactSessionInput,
) -> Result<PublishedTargetArtifact> {
    let artifact = render_target_artifact(result, target, artifact_input)?;
    eprintln!(
        "Compiling target '{}' for {}",
        artifact.label(),
        result.model_name()
    );
    if let Some(description) = artifact.description() {
        eprintln!("  {description}");
    }

    artifact.publish(output_root)
}
