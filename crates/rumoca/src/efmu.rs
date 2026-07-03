//! eFMU packaging for `build = "efmu"` targets (SPEC_0034 GAL-021/022/023).
//!
//! Drives [`rumoca_efmi::write_efmu_container`] with the ALREADY-RENDERED
//! bytes of the current target invocation — the same single projection run
//! that produced each representation's `manifest.xml` and code files.
//! Nothing is re-projected or re-serialized here: every manifest's
//! `manifestRefId` is parsed back out of the exact bytes being packaged and
//! the SHA-1s recorded in `__content.xml` are computed from them at write
//! time, so the recorded checksums are always digests of the files actually
//! written (GAL-021: no placeholder checksums, ever).
//!
//! # Representation grouping (contract with the target's `[[files]]` paths)
//!
//! Rendered paths whose top-level directory segment is a recognized
//! representation container name (`AlgorithmCode/…`, `ProductionCode/…`)
//! are grouped into one model representation per container: the group's
//! root-level `manifest.xml` is its manifest, everything else a
//! representation file at its remaining relative path. A flat layout (no
//! recognized container prefix on any path) keeps its original meaning —
//! a single Algorithm Code representation whose manifest is the root-level
//! `manifest.xml` (the `galec` target). Mixing the two layouts is ambiguous
//! and refused.
//!
//! # Output layout (UX decision, mirroring `build = "fmu"`)
//!
//! Like `fmu.rs`, everything lands inside the chosen out dir and the final
//! zip artifact is named `<model>.<ext>` within it. One deliberate
//! difference: FMI defines only the zip package form, so `fmu.rs` can stage
//! the unzipped layout at the out-dir root; eFMI ch. 2 defines the directory
//! form as a package format in its own right whose root must hold exactly
//! `__content.xml`, `schemas/`, and representation containers — so the
//! directory form lives in its own pristine `<model>/` subdirectory:
//!
//! ```text
//! <out_dir>/
//!   <model>/            eFMU directory form (package format 1)
//!     __content.xml
//!     schemas/…         vendored Beta-1 XSDs + BSD-3 LICENSE (GAL-023)
//!     AlgorithmCode/    the one Algorithm Code representation
//!       manifest.xml
//!       <model>.alg
//!     ProductionCode/   Production Code representation (galec-production)
//!       manifest.xml
//!       <model>.h
//!       <model>.c
//!   <model>.efmu        eFMU zip form (package format 2), same content
//! ```
//!
//! Re-running the same command replaces both package forms, matching every
//! other target's overwrite-on-re-run UX (including `build = "fmu"`): the
//! container writer itself never overwrites, but this driver owns
//! `<out_dir>/<model>/` as its build product, so a root it recognizes as a
//! previous eFMU (by its `__content.xml`) is cleared before repackaging.
//! Anything else non-empty at that path is foreign and is refused with the
//! remedy, never deleted.

use std::path::Path;

use anyhow::{Context, Result, bail};
use rumoca_compile::codegen::targets::RenderedTargetFile;
use rumoca_efmi::content::ModelRepresentationKind;
use rumoca_efmi::{
    CONTENT_XML_NAME as EFMU_CONTENT_XML, EfmuMeta, FilePath, ModelRepresentationFiles,
    NameWithoutSlashes, RepresentationFile, manifest_root_id, write_efmu_container, write_efmu_zip,
};

/// Name of the manifest file every eFMI model representation carries at its
/// container root; the rendered file with exactly this path is the manifest.
const MANIFEST_FILE_NAME: &str = "manifest.xml";

/// Tool-chosen name of the Algorithm Code representation container
/// directory (eFMI ch. 2: representation containers are tool-named; the
/// kind is registered separately in `__content.xml`).
const ALGORITHM_CODE_CONTAINER: &str = "AlgorithmCode";

/// Tool-chosen name of the Production Code representation container
/// directory (SPEC_0034 GAL-024 conformant track).
const PRODUCTION_CODE_CONTAINER: &str = "ProductionCode";

/// Container-directory names this driver recognizes as top-level path
/// segments of rendered files, with the representation kind each one
/// registers in `__content.xml` (module docs on representation grouping).
const CONTAINER_KINDS: &[(&str, ModelRepresentationKind)] = &[
    (
        ALGORITHM_CODE_CONTAINER,
        ModelRepresentationKind::AlgorithmCode,
    ),
    (
        PRODUCTION_CODE_CONTAINER,
        ModelRepresentationKind::ProductionCode,
    ),
];

/// Assemble both eFMU package forms from this invocation's rendered files.
///
/// `files` are sorted into model representations by their top-level
/// container-directory segment (module docs on representation grouping);
/// each representation must contain a `manifest.xml` at its container root.
///
/// `model` is the source model name as given on the command line (dotted
/// for hierarchical models, e.g. `Pkg.Model`) and becomes the eFMU's
/// `Content/@name` — eFMI ch. 2.3.1 wants the block name as in the source
/// modeling environment. `model_identifier` is its file-system-safe form
/// (dots replaced by underscores), naming the container directory and the
/// `.efmu` archive. The Algorithm Code manifest's own `name` stays the
/// projection's GALEC block identifier (dots are not valid in GALEC block
/// names), which this driver consumes as-is.
pub(crate) fn build_efmu(
    files: &[RenderedTargetFile],
    model: &str,
    model_identifier: &str,
    out_dir: &Path,
) -> Result<()> {
    let representations = model_representations(files)?;
    let meta = EfmuMeta::generated(model, concat!("rumoca ", env!("CARGO_PKG_VERSION")))
        .context("Build eFMU container metadata")?;

    let container_root = out_dir.join(model_identifier);
    clear_previous_container(&container_root)?;
    let layout = write_efmu_container(&representations, &meta, &container_root)
        .context("Write eFMU container (directory form)")?;
    eprintln!("  wrote {}", layout.content_xml.display());
    for written in &layout.representations {
        eprintln!("  wrote {}", written.directory.display());
    }

    let efmu_zip = out_dir.join(format!("{model_identifier}.efmu"));
    write_efmu_zip(&container_root, &efmu_zip).context("Write eFMU container (zip form)")?;
    eprintln!("  wrote {}", efmu_zip.display());

    Ok(())
}

/// Make way for this run's container directory form (module docs on the
/// re-run UX): a directory at `container_root` holding a `__content.xml`
/// is a previous run's eFMU — this driver's own build product — and is
/// removed so the no-clobber container writer can repackage. An empty
/// directory is left for the writer (it accepts one); anything else
/// non-empty is NOT ours to delete and fails with the remedy, keeping
/// `rumoca-efmi`'s stale-files-never-mixed invariant intact for the
/// foreign-content case.
fn clear_previous_container(container_root: &Path) -> Result<()> {
    if !container_root.exists() {
        return Ok(());
    }
    if container_root.is_dir() && container_root.join(EFMU_CONTENT_XML).is_file() {
        std::fs::remove_dir_all(container_root).with_context(|| {
            format!(
                "Remove the previous run's eFMU container `{}`",
                container_root.display()
            )
        })?;
        return Ok(());
    }
    let empty_directory = container_root.is_dir()
        && std::fs::read_dir(container_root)
            .with_context(|| format!("Read eFMU output directory `{}`", container_root.display()))?
            .next()
            .is_none();
    if empty_directory {
        return Ok(());
    }
    bail!(
        "eFMU output path `{}` exists but is not an eFMU container from a previous run \
         (no {EFMU_CONTENT_XML}); refusing to remove it. Delete it or choose a \
         different --output directory.",
        container_root.display()
    );
}

/// Sort this invocation's rendered files into model representations
/// (module docs on representation grouping): every path is keyed by its
/// recognized top-level container-directory segment; a layout with no
/// recognized prefix anywhere keeps today's meaning — one Algorithm Code
/// representation rooted at the rendered paths themselves. Cardinality
/// rules (exactly one Algorithm Code representation per eFMU) stay owned
/// by `rumoca_efmi::Content`.
fn model_representations(files: &[RenderedTargetFile]) -> Result<Vec<ModelRepresentationFiles>> {
    let mut flat: Vec<(&str, &RenderedTargetFile)> = Vec::new();
    let mut grouped: Vec<Vec<(&str, &RenderedTargetFile)>> =
        vec![Vec::new(); CONTAINER_KINDS.len()];
    for file in files {
        match split_container_prefix(&file.path) {
            Some((group, remainder)) => grouped[group].push((remainder, file)),
            None => flat.push((file.path.as_str(), file)),
        }
    }
    if grouped.iter().all(Vec::is_empty) {
        let representation = container_representation(
            ALGORITHM_CODE_CONTAINER,
            ModelRepresentationKind::AlgorithmCode,
            &flat,
        )?;
        return Ok(vec![representation]);
    }
    if let Some((path, _)) = flat.first() {
        bail!(
            "build = \"efmu\" targets must not mix representation-container paths \
             (`AlgorithmCode/...`, `ProductionCode/...`) with unprefixed paths: \
             rendered file '{path}' carries no recognized container directory"
        );
    }
    CONTAINER_KINDS
        .iter()
        .zip(&grouped)
        .filter(|(_, group)| !group.is_empty())
        .map(|(&(container, kind), group)| container_representation(container, kind, group))
        .collect()
}

/// Split a rendered path into its recognized container group index and the
/// path remainder relative to that container; `None` for unprefixed paths.
fn split_container_prefix(path: &str) -> Option<(usize, &str)> {
    let (first_segment, remainder) = path.split_once('/')?;
    let group = CONTAINER_KINDS
        .iter()
        .position(|&(container, _)| container == first_segment)?;
    Some((group, remainder))
}

/// Build one model representation from `(path relative to the container,
/// rendered file)` pairs: the container-root `manifest.xml` is the manifest
/// (its own root `id` becomes the registered `manifestRefId`); everything
/// else is a representation file at its relative path.
fn container_representation(
    container: &'static str,
    kind: ModelRepresentationKind,
    files: &[(&str, &RenderedTargetFile)],
) -> Result<ModelRepresentationFiles> {
    let mut manifest_bytes: Option<Vec<u8>> = None;
    let mut representation_files = Vec::new();
    for (relative_path, file) in files {
        if *relative_path == MANIFEST_FILE_NAME {
            if manifest_bytes.is_some() {
                bail!(
                    "build = \"efmu\" targets must render exactly one manifest.xml \
                     at the {container} representation root"
                );
            }
            manifest_bytes = Some(file.content.clone().into_bytes());
            continue;
        }
        representation_files.push(representation_file(relative_path, file)?);
    }
    let Some(manifest_bytes) = manifest_bytes else {
        bail!(
            "build = \"efmu\" targets must render a manifest.xml at the {container} \
             representation root (the model representation manifest); none was produced"
        );
    };
    let manifest_ref_id = manifest_root_id(&manifest_bytes).with_context(|| {
        format!("Read the manifest's own id from the rendered {container} manifest.xml")
    })?;
    Ok(ModelRepresentationFiles {
        name: NameWithoutSlashes::new(container).expect("constant container name is slash-free"),
        kind,
        manifest_name: NameWithoutSlashes::new(MANIFEST_FILE_NAME)
            .expect("constant manifest name is slash-free"),
        manifest_ref_id,
        manifest_bytes,
        files: representation_files,
    })
}

/// Split one container-relative path into the eFMI `(path, name)` pair and
/// take the rendered file's exact bytes.
fn representation_file(
    relative_path: &str,
    file: &RenderedTargetFile,
) -> Result<RepresentationFile> {
    let (directory, name) = match relative_path.rsplit_once('/') {
        Some((directory, name)) => (
            FilePath::new(format!("./{directory}/")).with_context(|| {
                format!("Container directory for rendered file '{}'", file.path)
            })?,
            name,
        ),
        None => (FilePath::root(), relative_path),
    };
    Ok(RepresentationFile {
        directory,
        name: NameWithoutSlashes::new(name)
            .with_context(|| format!("Container file name for rendered file '{}'", file.path))?,
        bytes: file.content.clone().into_bytes(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Minimal well-formed manifest bytes carrying a root `id` the
    /// grouping can register as `manifestRefId`.
    fn manifest_xml(uuid: &str) -> String {
        format!("<Manifest id=\"{{{uuid}}}\"></Manifest>")
    }

    fn rendered(path: &str, content: &str) -> RenderedTargetFile {
        RenderedTargetFile {
            path: path.to_string(),
            content: content.to_string(),
        }
    }

    const AC_UUID: &str = "2f1a03de-9f14-4a3d-8b1e-73c60d0a1c11";
    const PC_UUID: &str = "7b9c2a41-5d0e-4f6a-9c3d-1e8f0a2b4c5d";

    /// The flat layout (no container prefix) keeps meaning a single
    /// Algorithm Code representation — the existing `galec` contract.
    #[test]
    fn flat_layout_stays_a_single_algorithm_code_representation() {
        let files = [
            rendered("Model.alg", "block Model"),
            rendered("manifest.xml", &manifest_xml(AC_UUID)),
        ];
        let representations =
            model_representations(&files).expect("flat layout should group as one AC container");
        assert_eq!(representations.len(), 1);
        let representation = &representations[0];
        assert_eq!(representation.name.as_str(), ALGORITHM_CODE_CONTAINER);
        assert_eq!(representation.kind, ModelRepresentationKind::AlgorithmCode);
        assert_eq!(representation.files.len(), 1);
        assert_eq!(representation.files[0].name.as_str(), "Model.alg");
    }

    /// Container-prefixed paths group into one representation per
    /// container directory, each with its own root manifest, and the
    /// kind is mapped from the directory constant.
    #[test]
    fn container_prefixed_paths_group_into_one_representation_per_container() {
        let files = [
            rendered("AlgorithmCode/Model.alg", "block Model"),
            rendered("AlgorithmCode/manifest.xml", &manifest_xml(AC_UUID)),
            rendered("ProductionCode/Model.h", "/* header */"),
            rendered("ProductionCode/Model.c", "/* source */"),
            rendered("ProductionCode/manifest.xml", &manifest_xml(PC_UUID)),
        ];
        let representations = model_representations(&files)
            .expect("two-container layout should group into two representations");
        assert_eq!(representations.len(), 2);

        let algorithm_code = &representations[0];
        assert_eq!(algorithm_code.name.as_str(), ALGORITHM_CODE_CONTAINER);
        assert_eq!(algorithm_code.kind, ModelRepresentationKind::AlgorithmCode);
        assert_eq!(
            algorithm_code.manifest_ref_id.to_string(),
            format!("{{{AC_UUID}}}")
        );
        assert_eq!(algorithm_code.files.len(), 1);
        assert_eq!(algorithm_code.files[0].name.as_str(), "Model.alg");

        let production_code = &representations[1];
        assert_eq!(production_code.name.as_str(), PRODUCTION_CODE_CONTAINER);
        assert_eq!(
            production_code.kind,
            ModelRepresentationKind::ProductionCode
        );
        assert_eq!(
            production_code.manifest_ref_id.to_string(),
            format!("{{{PC_UUID}}}")
        );
        let names: Vec<&str> = production_code
            .files
            .iter()
            .map(|file| file.name.as_str())
            .collect();
        assert_eq!(names, ["Model.h", "Model.c"]);
    }

    /// Mixing prefixed and unprefixed paths is ambiguous and refused
    /// (SPEC_0008 fail-early), naming the offending file.
    #[test]
    fn mixed_flat_and_container_paths_are_refused() {
        let files = [
            rendered("AlgorithmCode/manifest.xml", &manifest_xml(AC_UUID)),
            rendered("manifest.xml", &manifest_xml(PC_UUID)),
        ];
        let error =
            model_representations(&files).expect_err("mixed layout must be refused as ambiguous");
        let message = format!("{error:#}");
        assert!(
            message.contains("must not mix") && message.contains("'manifest.xml'"),
            "expected the mixed-layout diagnostic naming the file, got: {message}"
        );
    }

    /// Every grouped container must carry its own root manifest.xml.
    #[test]
    fn container_without_root_manifest_is_refused() {
        let files = [
            rendered("AlgorithmCode/Model.alg", "block Model"),
            rendered("AlgorithmCode/manifest.xml", &manifest_xml(AC_UUID)),
            rendered("ProductionCode/Model.c", "/* source */"),
        ];
        let error =
            model_representations(&files).expect_err("a manifest-less container must be refused");
        let message = format!("{error:#}");
        assert!(
            message.contains("manifest.xml at the ProductionCode representation root"),
            "expected the missing-manifest diagnostic to name the container, got: {message}"
        );
    }

    /// A second manifest.xml at the same container root is refused.
    #[test]
    fn duplicate_container_root_manifest_is_refused() {
        let files = [
            rendered("manifest.xml", &manifest_xml(AC_UUID)),
            rendered("manifest.xml", &manifest_xml(PC_UUID)),
        ];
        let error = model_representations(&files).expect_err("two root manifests must be refused");
        let message = format!("{error:#}");
        assert!(
            message.contains("exactly one manifest.xml"),
            "expected the duplicate-manifest diagnostic, got: {message}"
        );
    }
}
