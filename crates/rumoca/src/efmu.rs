//! eFMU packaging for `build = "efmu"` targets (SPEC_0034 GAL-021/022/023).
//!
//! Drives [`rumoca_efmi::write_efmu_container`] with the ALREADY-RENDERED
//! bytes of the current target invocation — the same single projection run
//! that produced `manifest.xml` and the `.alg` file. Nothing is re-projected
//! or re-serialized here: the manifest's `manifestRefId` is parsed back out
//! of the exact bytes being packaged and the SHA-1 recorded in
//! `__content.xml` is computed from them at write time, so the recorded
//! checksums are always digests of the files actually written (GAL-021: no
//! placeholder checksums, ever).
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

/// Assemble both eFMU package forms from this invocation's rendered files.
///
/// `files` must contain a root-level `manifest.xml`; every other rendered
/// file becomes a representation file of the single Algorithm Code
/// container, at its rendered relative path.
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
    let representation = algorithm_code_representation(files)?;
    let meta = EfmuMeta::generated(model, concat!("rumoca ", env!("CARGO_PKG_VERSION")))
        .context("Build eFMU container metadata")?;

    let container_root = out_dir.join(model_identifier);
    clear_previous_container(&container_root)?;
    let layout = write_efmu_container(&[representation], &meta, &container_root)
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

/// Sort this invocation's rendered files into the one Algorithm Code
/// representation: the root-level `manifest.xml` is the manifest (its own
/// root `id` becomes the registered `manifestRefId`); everything else is a
/// representation file at its rendered relative path.
fn algorithm_code_representation(files: &[RenderedTargetFile]) -> Result<ModelRepresentationFiles> {
    let mut manifest_bytes: Option<Vec<u8>> = None;
    let mut representation_files = Vec::new();
    for file in files {
        if file.path == MANIFEST_FILE_NAME {
            if manifest_bytes.is_some() {
                bail!("build = \"efmu\" targets must render exactly one root-level manifest.xml");
            }
            manifest_bytes = Some(file.content.clone().into_bytes());
            continue;
        }
        representation_files.push(representation_file(file)?);
    }
    let Some(manifest_bytes) = manifest_bytes else {
        bail!(
            "build = \"efmu\" targets must render a root-level manifest.xml \
             (the model representation manifest); none was produced"
        );
    };
    let manifest_ref_id = manifest_root_id(&manifest_bytes)
        .context("Read the manifest's own id from the rendered manifest.xml")?;
    Ok(ModelRepresentationFiles {
        name: NameWithoutSlashes::new(ALGORITHM_CODE_CONTAINER)
            .expect("constant container name is slash-free"),
        kind: ModelRepresentationKind::AlgorithmCode,
        manifest_name: NameWithoutSlashes::new(MANIFEST_FILE_NAME)
            .expect("constant manifest name is slash-free"),
        manifest_ref_id,
        manifest_bytes,
        files: representation_files,
    })
}

/// Split one rendered file's relative path into the eFMI `(path, name)`
/// pair and take its exact rendered bytes.
fn representation_file(file: &RenderedTargetFile) -> Result<RepresentationFile> {
    let (directory, name) = match file.path.rsplit_once('/') {
        Some((directory, name)) => (
            FilePath::new(format!("./{directory}/")).with_context(|| {
                format!("Container directory for rendered file '{}'", file.path)
            })?,
            name,
        ),
        None => (FilePath::root(), file.path.as_str()),
    };
    Ok(RepresentationFile {
        directory,
        name: NameWithoutSlashes::new(name)
            .with_context(|| format!("Container file name for rendered file '{}'", file.path))?,
        bytes: file.content.clone().into_bytes(),
    })
}
