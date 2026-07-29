//! Generic declarative checksum/packaging build step (contract §4).
//!
//! This product-agnostic build step is driven entirely by `target.toml`:
//! `[[files]]` carry logical identities and checksum edges, `[package]`
//! declares product layout, and `[[assets]]` names target-relative trees to
//! copy verbatim. It renders every file **in dependency order**, hashes the
//! EXACT bytes it is about to write, and threads each producer's SHA-1 into the
//! downstream files' context under the declared `as` key. Product-specific
//! filenames, extensions, and formats live only in the target directory.
//!
//! # The no-placeholder proof (contract §4c), by construction
//!
//! Each `[[files.checksums]] of = P` on file `C` is the directed edge
//! `P -> C` ("P rendered + hashed before C"). [`topo_sort`] refuses a self
//! edge (a file can never embed its own hash) and any cycle, rendering
//! **nothing** in either case (fail-early, no placeholder ever touches disk).
//! On the resulting total order, when file `C` is rendered every `sha1[of]`
//! was inserted right after its producer's bytes were produced — so the value
//! threaded into `C` is always the real SHA-1 of the producer's final bytes.
//! No placeholder is representable: the digest is computed only after the
//! producer bytes exist, and the bytes hashed are the exact bytes written.

use std::collections::{BTreeMap, HashMap};
#[cfg(feature = "scheduled-sim")]
use std::fs;
#[cfg(feature = "scheduled-sim")]
use std::io::Write as _;
#[cfg(feature = "scheduled-sim")]
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
#[cfg(feature = "scheduled-sim")]
use rumoca_compile::codegen::targets::{AssetBundle, TargetAssetFile, safe_target_join};
use rumoca_compile::codegen::targets::{
    ChecksumAlgorithm, RenderedTargetFile, TargetFile,
};
use sha1::{Digest, Sha1};
use serde::Serialize;
use time::OffsetDateTime;
use time::macros::format_description;
use uuid::Uuid;

/// How the rendered files + assets are finalized on disk (contract §4b).
#[cfg(feature = "scheduled-sim")]
pub struct PackageSpec {
    /// Files whose presence recognizes a directory as a previous product.
    pub required_files: Vec<String>,
    /// The zip package form, if the product has one. `None` = directory-only.
    pub zip: Option<ZipPackage>,
}

/// The zip form of a package (contract §4b `Zip { ext }`).
#[cfg(feature = "scheduled-sim")]
pub struct ZipPackage {
    /// Absolute path of the archive declared by the target.
    pub archive_path: PathBuf,
}

/// Immutable facts shared by every artifact rendered in one invocation.
///
/// The generic artifact layer owns identity/time minting. Concrete target
/// templates decide which identities they use and how they spell them.
#[derive(Debug, Clone, Serialize)]
pub struct ArtifactSession {
    pub generated_at: String,
    pub generation_tool: String,
    pub identities: BTreeMap<String, String>,
}

impl ArtifactSession {
    pub fn new(files: &[TargetFile]) -> Result<Self> {
        let generated_at = OffsetDateTime::now_utc()
            .format(format_description!(
                "[year]-[month]-[day]T[hour]:[minute]:[second]Z"
            ))
            .context("Format artifact generation timestamp")?;
        let identities = files
            .iter()
            .filter_map(|file| file.id.as_ref())
            .map(|id| (id.clone(), Uuid::new_v4().hyphenated().to_string()))
            .collect();
        Ok(Self {
            generated_at,
            generation_tool: format!("rumoca {}", env!("CARGO_PKG_VERSION")),
            identities,
        })
    }
}

/// Per-file immutable render context: one shared session plus the checksums
/// made available by the declared incoming DAG edges.
#[derive(Debug, Clone, Copy, Serialize)]
pub struct ArtifactRenderContext<'a> {
    #[serde(flatten)]
    pub session: &'a ArtifactSession,
    pub checksums: &'a BTreeMap<String, String>,
}

/// Order the target's files so every producer precedes every consumer of its
/// checksum (Kahn over the `of -> this` edges; contract §4b/§4c).
///
/// Returns indices into `files`. A declared cycle — or a file that checksums
/// itself — is a `target.toml` error; the caller renders nothing (the
/// no-placeholder guarantee). Ties are broken by original declaration order so
/// the render order is deterministic. Reference resolution / self-edge /
/// duplicate-id are validated at parse time
/// (`codegen_target::validate_checksum_web`); this re-resolves defensively and
/// is the sole home of cycle detection.
pub fn topo_sort(files: &[TargetFile]) -> Result<Vec<usize>> {
    let mut id_to_index: HashMap<&str, usize> = HashMap::new();
    for (index, file) in files.iter().enumerate() {
        if let Some(id) = &file.id
            && id_to_index.insert(id.as_str(), index).is_some()
        {
            bail!("duplicate [[files]] id '{id}' (ids must be unique per target)");
        }
    }

    // Edge `producer -> consumer`; indegree[consumer] = number of edges in.
    let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); files.len()];
    let mut indegree: Vec<usize> = vec![0; files.len()];
    for (consumer, file) in files.iter().enumerate() {
        for need in &file.checksums {
            let producer = *id_to_index.get(need.of.as_str()).with_context(|| {
                format!(
                    "[[files.checksums]] of = '{}' on file '{}' names no [[files]] id",
                    need.of, file.path
                )
            })?;
            if producer == consumer {
                bail!(
                    "[[files.checksums]] of = '{}' on file '{}' checksums itself; a file \
                     can never embed its own hash",
                    need.of,
                    file.path
                );
            }
            dependents[producer].push(consumer);
            indegree[consumer] += 1;
        }
    }

    // Ready set kept sorted descending so `pop` always yields the smallest
    // ready index — ties broken by original declaration order, deterministically.
    let mut order = Vec::with_capacity(files.len());
    let mut ready: Vec<usize> = (0..files.len()).filter(|&i| indegree[i] == 0).collect();
    ready.sort_unstable_by(|a, b| b.cmp(a));
    while let Some(node) = ready.pop() {
        order.push(node);
        for &consumer in &dependents[node] {
            indegree[consumer] -= 1;
            if indegree[consumer] == 0 {
                ready.push(consumer);
                ready.sort_unstable_by(|a, b| b.cmp(a));
            }
        }
    }

    if order.len() != files.len() {
        bail!(
            "checksum web has a cycle: a file transitively checksums itself, which is \
             impossible to render (no placeholder checksum is ever emitted). Fix the \
             [[files.checksums]] edges in the target.toml."
        );
    }
    Ok(order)
}

/// Render every declared file in dependency order, hashing the exact bytes to
/// be written and injecting each producer's SHA-1 into downstream contexts,
/// then finalize the product on disk (contract §4b).
///
/// `render(template, checksums)` renders one template string (a `[[files]]`
/// `path` or its content `template`) against the target's base context plus
/// the injected checksum keys — keeping this step ignorant of how the base
/// context is built. `asset_source(source)` resolves a target-relative tree.
/// Nothing is written until every byte is
/// rendered and hashed, so a render or topo failure leaves the product path
/// untouched.
#[cfg(feature = "scheduled-sim")]
pub fn render_and_package(
    files: &[TargetFile],
    render: impl Fn(&str, &ArtifactRenderContext<'_>) -> Result<String>,
    assets: &[AssetBundle],
    asset_source: impl Fn(&str) -> Result<Vec<TargetAssetFile>>,
    package: &PackageSpec,
    out_dir: &Path,
) -> Result<()> {
    let rendered = render_web(files, render)?;
    let resolved_assets = assets
        .iter()
        .map(|asset| {
            Ok((
                asset,
                asset_source(&asset.source).with_context(|| {
                    format!("Resolve target asset source '{}'", asset.source)
                })?,
            ))
        })
        .collect::<Result<Vec<_>>>()?;
    validate_required_files(&rendered, &resolved_assets, &package.required_files)?;
    validate_existing_root(out_dir, &package.required_files)?;

    let parent = out_dir
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    fs::create_dir_all(parent)
        .with_context(|| format!("Create package parent '{}'", parent.display()))?;
    let staging = tempfile::Builder::new()
        .prefix(".rumoca-package-")
        .tempdir_in(parent)
        .with_context(|| format!("Create package staging directory in '{}'", parent.display()))?;
    let staged_root = staging.path().join("product");
    fs::create_dir(&staged_root)
        .with_context(|| format!("Create staged product '{}'", staged_root.display()))?;
    write_rendered_files(&staged_root, &rendered)?;
    for (asset, files) in &resolved_assets {
        copy_asset_tree(&staged_root, asset, files)?;
    }
    let staged_archive = package
        .zip
        .as_ref()
        .map(|_| staging.path().join("archive.zip"));
    if let Some(archive_path) = &staged_archive {
        write_zip_package(&staged_root, archive_path).context("Build staged package archive")?;
    }
    install_staged_package(
        &staged_root,
        out_dir,
        staged_archive.as_deref(),
        package.zip.as_ref().map(|zip| zip.archive_path.as_path()),
        staging.path(),
    )?;
    Ok(())
}

/// Render every declared file in dependency order, hashing the exact bytes
/// and injecting each producer's SHA-1 into downstream contexts, and return
/// the rendered `(path, bytes)` pairs **without touching the filesystem**
/// (contract §4b render half).
///
/// This is the shared core of [`render_and_package`] (which appends the
/// on-disk write + asset copy + zip) and the in-memory `render_target_files`
/// path (which returns the same bytes as `RenderedTargetFile`s), so both drive
/// the identical topological render + checksum web. Nothing is written here, so
/// a render or topo failure leaves the product path untouched.
///
/// # Errors
///
/// Propagates a `target.toml` cycle/dangling-edge error from [`topo_sort`] or
/// any per-file render failure from `render`.
pub fn render_web(
    files: &[TargetFile],
    render: impl Fn(&str, &ArtifactRenderContext<'_>) -> Result<String>,
) -> Result<Vec<(String, Vec<u8>)>> {
    let order = topo_sort(files)?;
    let session = ArtifactSession::new(files)?;

    let mut digests: HashMap<(String, ChecksumAlgorithm), String> = HashMap::new();
    // Render into `order` positions but return in declaration order, so the
    // in-memory caller sees files in the same order the `target.toml` declares
    // them (`render_target_files` asserts a 1:1 file-count match).
    let mut rendered: Vec<Option<(String, Vec<u8>)>> = (0..files.len()).map(|_| None).collect();
    for &index in &order {
        let file = &files[index];
        let mut checksums = BTreeMap::new();
        for need in &file.checksums {
            // Guaranteed present: the producer precedes this file in `order`.
            let digest = digests
                .get(&(need.of.clone(), need.algorithm))
                .with_context(|| {
                format!(
                    "internal: producer '{}' hash missing while rendering '{}'",
                    need.of, file.path
                )
            })?;
            checksums.insert(need.as_key.clone(), digest.clone());
        }
        let context = ArtifactRenderContext {
            session: &session,
            checksums: &checksums,
        };
        let path = render(&file.path, &context)
            .with_context(|| format!("Render target output path '{}'", file.path))?
            .trim()
            .to_string();
        let bytes = render(&file.template, &context)
            .with_context(|| format!("Render target template '{}'", file.template))?
            .into_bytes();
        // Hash the exact bytes that will be written; hashing and writing share
        // this one buffer with no intervening reformat.
        if let Some(id) = &file.id {
            for algorithm in files.iter().flat_map(|consumer| {
                consumer
                    .checksums
                    .iter()
                    .filter(|need| need.of == *id)
                    .map(|need| need.algorithm)
            }) {
                digests
                    .entry((id.clone(), algorithm))
                    .or_insert_with(|| checksum_hex(algorithm, &bytes));
            }
        }
        rendered[index] = Some((path, bytes));
    }

    Ok(rendered
        .into_iter()
        .map(|entry| entry.expect("every file rendered exactly once in topological order"))
        .collect())
}

/// SHA-1 digest of exact artifact bytes as lowercase hexadecimal.
#[must_use]
pub fn sha1_hex(bytes: &[u8]) -> String {
    format!("{:x}", Sha1::digest(bytes))
}

fn checksum_hex(algorithm: ChecksumAlgorithm, bytes: &[u8]) -> String {
    match algorithm {
        ChecksumAlgorithm::Sha1 => sha1_hex(bytes),
    }
}

/// In-memory twin of [`render_and_package`]'s render half (contract §9 WI-5):
/// drive the declarative checksum web and return the rendered files as
/// `RenderedTargetFile`s (UTF-8 `content`) in `target.toml` declaration order,
/// for the `render_target_files` CI path. Assets/zip are packaging-only and
/// are not included.
///
/// # Errors
///
/// Those of [`render_web`], plus a non-UTF-8 rendered file (templates always
/// emit UTF-8, so this is an internal invariant break).
pub fn render_web_files(
    files: &[TargetFile],
    render: impl Fn(&str, &ArtifactRenderContext<'_>) -> Result<String>,
) -> Result<Vec<RenderedTargetFile>> {
    render_web(files, render)?
        .into_iter()
        .map(|(path, bytes)| {
            Ok(RenderedTargetFile {
                path,
                content: String::from_utf8(bytes)
                    .context("rendered target file is not valid UTF-8")?,
            })
        })
        .collect()
}

/// Make way for this run's product directory. A directory holding all declared
/// marker files is a previous build and can be replaced; any other non-empty
/// directory is treated as foreign.
#[cfg(feature = "scheduled-sim")]
fn validate_existing_root(out_dir: &Path, required_files: &[String]) -> Result<()> {
    if !out_dir.exists() {
        return Ok(());
    }
    let previous_product = !required_files.is_empty()
        && out_dir.is_dir()
        && required_files
            .iter()
            .all(|required| out_dir.join(required).is_file());
    if previous_product {
        return Ok(());
    }
    let empty = out_dir.is_dir()
        && std::fs::read_dir(out_dir)
            .with_context(|| format!("Read product directory `{}`", out_dir.display()))?
            .next()
            .is_none();
    if empty {
        return Ok(());
    }
    bail!(
        "product output path `{}` exists but is not a build product from a previous run \
         (required marker files are missing); refusing to remove it. Delete it or choose a different `--output` \
         directory.",
        out_dir.display()
    );
}

#[cfg(feature = "scheduled-sim")]
fn validate_required_files(
    rendered: &[(String, Vec<u8>)],
    assets: &[(&AssetBundle, Vec<TargetAssetFile>)],
    required_files: &[String],
) -> Result<()> {
    for required in required_files {
        safe_target_join(Path::new("product"), required)?;
        let rendered_match = rendered.iter().any(|(path, _)| path == required);
        let asset_match = assets.iter().any(|(asset, files)| {
            files.iter().any(|file| {
                let dest = asset.dest.trim_end_matches('/');
                format!("{dest}/{}", file.relative_path) == *required
            })
        });
        if !rendered_match && !asset_match {
            bail!(
                "[package] required file '{required}' is not produced by [[files]] or [[assets]]"
            );
        }
    }
    Ok(())
}

/// Write the exact rendered bytes that were hashed (contract §4c): the tuple
/// list is the same `(path, bytes)` produced by the render loop.
#[cfg(feature = "scheduled-sim")]
fn write_rendered_files(out_dir: &Path, rendered: &[(String, Vec<u8>)]) -> Result<()> {
    for (path, bytes) in rendered {
        let output_path = safe_target_join(out_dir, path)?;
        if let Some(parent) = output_path.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("Create directory for `{}`", output_path.display()))?;
        }
        std::fs::write(&output_path, bytes)
            .with_context(|| format!("Write `{}`", output_path.display()))?;
    }
    Ok(())
}

/// Copy one target-relative asset tree into `out_dir/<dest>` verbatim.
#[cfg(feature = "scheduled-sim")]
fn copy_asset_tree(
    out_dir: &Path,
    asset: &AssetBundle,
    files: &[TargetAssetFile],
) -> Result<()> {
    let dest_root = safe_target_join(out_dir, &asset.dest)?;
    for file in files {
        let output_path = safe_target_join(&dest_root, &file.relative_path)?;
        if let Some(parent) = output_path.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("Create directory for `{}`", output_path.display()))?;
        }
        std::fs::write(&output_path, &file.bytes)
            .with_context(|| format!("Write asset `{}`", output_path.display()))?;
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn install_staged_package(
    staged_root: &Path,
    final_root: &Path,
    staged_archive: Option<&Path>,
    final_archive: Option<&Path>,
    transaction_dir: &Path,
) -> Result<()> {
    let previous_root = transaction_dir.join("previous-product");
    let had_root = final_root.exists();
    if had_root {
        fs::rename(final_root, &previous_root).with_context(|| {
            format!(
                "Move previous product '{}' into transaction",
                final_root.display()
            )
        })?;
    }
    if let Err(error) = fs::rename(staged_root, final_root) {
        if had_root {
            let _ = fs::rename(&previous_root, final_root);
        }
        return Err(error).with_context(|| {
            format!("Install staged product '{}'", final_root.display())
        });
    }

    let archive_result = install_staged_archive(
        staged_archive,
        final_archive,
        transaction_dir.join("previous-archive"),
    );
    if let Err(error) = archive_result {
        let _ = fs::remove_dir_all(final_root);
        if had_root {
            let _ = fs::rename(&previous_root, final_root);
        }
        return Err(error);
    }
    if had_root {
        fs::remove_dir_all(&previous_root).with_context(|| {
            format!("Remove previous product '{}'", previous_root.display())
        })?;
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn install_staged_archive(
    staged_archive: Option<&Path>,
    final_archive: Option<&Path>,
    previous_archive: PathBuf,
) -> Result<()> {
    let (Some(staged), Some(final_path)) = (staged_archive, final_archive) else {
        return Ok(());
    };
    if let Some(parent) = final_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Create archive parent '{}'", parent.display()))?;
    }
    let had_archive = final_path.exists();
    if had_archive {
        fs::rename(final_path, &previous_archive).with_context(|| {
            format!("Move previous archive '{}' into transaction", final_path.display())
        })?;
    }
    if let Err(error) = fs::rename(staged, final_path) {
        if had_archive {
            let _ = fs::rename(&previous_archive, final_path);
        }
        return Err(error)
            .with_context(|| format!("Install package archive '{}'", final_path.display()));
    }
    if had_archive {
        fs::remove_file(&previous_archive).with_context(|| {
            format!(
                "Remove previous package archive '{}'",
                previous_archive.display()
            )
        })?;
    }
    Ok(())
}

/// Write a deterministic flat zip of `package_root` to a staging file and
/// replace `archive_path` only after the archive is complete.
#[cfg(feature = "scheduled-sim")]
fn write_zip_package(package_root: &Path, archive_path: &Path) -> Result<()> {
    let mut relative_paths = Vec::new();
    collect_package_files(package_root, package_root, &mut relative_paths)?;
    relative_paths.sort();

    if let Some(parent) = archive_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Create archive directory '{}'", parent.display()))?;
    }
    let mut staging_name = archive_path
        .file_name()
        .context("Package archive path must have a file name")?
        .to_os_string();
    staging_name.push(".part");
    let staging_path = archive_path.with_file_name(staging_name);
    write_zip_entries(package_root, archive_path, &staging_path, &relative_paths).inspect_err(
        |_| {
            let _ = fs::remove_file(&staging_path);
        },
    )?;
    if archive_path.exists() {
        fs::remove_file(archive_path).with_context(|| {
            format!("Remove previous archive '{}'", archive_path.display())
        })?;
    }
    fs::rename(&staging_path, archive_path).with_context(|| {
        format!(
            "Rename staged archive '{}' to '{}'",
            staging_path.display(),
            archive_path.display()
        )
    })
}

#[cfg(feature = "scheduled-sim")]
fn collect_package_files(root: &Path, dir: &Path, out: &mut Vec<String>) -> Result<()> {
    for entry in
        fs::read_dir(dir).with_context(|| format!("Read package directory '{}'", dir.display()))?
    {
        let entry =
            entry.with_context(|| format!("Read package entry in '{}'", dir.display()))?;
        let path = entry.path();
        let file_type = entry
            .file_type()
            .with_context(|| format!("Stat package entry '{}'", path.display()))?;
        if file_type.is_symlink() {
            bail!("Package trees may not contain symlinks: '{}'", path.display());
        }
        if file_type.is_dir() {
            collect_package_files(root, &path, out)?;
        } else if file_type.is_file() {
            out.push(relative_package_path(root, &path)?);
        }
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn relative_package_path(root: &Path, path: &Path) -> Result<String> {
    path.strip_prefix(root)
        .expect("package entry is beneath package root")
        .components()
        .map(|component| {
            component
                .as_os_str()
                .to_str()
                .context("Package paths must be valid UTF-8")
        })
        .collect::<Result<Vec<_>>>()
        .map(|segments| segments.join("/"))
}

#[cfg(feature = "scheduled-sim")]
fn write_zip_entries(
    package_root: &Path,
    archive_path: &Path,
    staging_path: &Path,
    relative_paths: &[String],
) -> Result<()> {
    let file = fs::File::create(staging_path)
        .with_context(|| format!("Create staged archive '{}'", staging_path.display()))?;
    let mut archive = zip::ZipWriter::new(file);
    let options = zip::write::SimpleFileOptions::default()
        .compression_method(zip::CompressionMethod::Deflated)
        .last_modified_time(zip::DateTime::default())
        .unix_permissions(0o644);
    for relative in relative_paths {
        let source = package_root.join(relative.replace('/', std::path::MAIN_SEPARATOR_STR));
        let bytes = fs::read(&source)
            .with_context(|| format!("Read package file '{}'", source.display()))?;
        archive.start_file(relative, options).with_context(|| {
            format!(
                "Start zip entry '{relative}' in '{}'",
                archive_path.display()
            )
        })?;
        archive.write_all(&bytes).with_context(|| {
            format!(
                "Write zip entry '{relative}' in '{}'",
                archive_path.display()
            )
        })?;
    }
    archive
        .finish()
        .with_context(|| format!("Finish archive '{}'", archive_path.display()))?;
    Ok(())
}

#[cfg(all(test, feature = "scheduled-sim"))]
mod tests {
    use super::*;
    use rumoca_compile::codegen::targets::parse_target_manifest;

    fn manifest_files(toml: &str) -> Vec<TargetFile> {
        parse_target_manifest(toml)
            .expect("manifest should parse")
            .files
    }

    /// The eFMU checksum web (`alg -> ac -> pc`, `{h,c} -> pc`, `ac -> content`,
    /// `pc -> content`) is a DAG; topo_sort orders every producer before its
    /// consumers, with `content` a sink.
    #[test]
    fn topo_sort_orders_producers_before_consumers() {
        let files = manifest_files(
            r#"
version = 1
ir = "dae"
name = "web"
readiness_level = 2

[capabilities]

[[files]]
id = "alg"
path = "AlgorithmCode/M.alg"
template = "a.jinja"

[[files]]
id = "c_header"
path = "ProductionCode/M.h"
template = "h.jinja"

[[files]]
id = "c_source"
path = "ProductionCode/M.c"
template = "c.jinja"

[[files]]
id = "ac_manifest"
path = "AlgorithmCode/manifest.xml"
template = "ac.jinja"
  [[files.checksums]]
  of = "alg"
  algorithm = "sha1"
  as = "alg_sha1"

[[files]]
id = "pc_manifest"
path = "ProductionCode/manifest.xml"
template = "pc.jinja"
  [[files.checksums]]
  of = "ac_manifest"
  algorithm = "sha1"
  as = "ac_manifest_sha1"
  [[files.checksums]]
  of = "c_header"
  algorithm = "sha1"
  as = "c_header_sha1"
  [[files.checksums]]
  of = "c_source"
  algorithm = "sha1"
  as = "c_source_sha1"

[[files]]
id = "content"
path = "__content.xml"
template = "content.jinja"
  [[files.checksums]]
  of = "ac_manifest"
  algorithm = "sha1"
  as = "ac_manifest_sha1"
  [[files.checksums]]
  of = "pc_manifest"
  algorithm = "sha1"
  as = "pc_manifest_sha1"
"#,
        );
        let order = topo_sort(&files).expect("DAG should topo-sort");
        let position: HashMap<&str, usize> = order
            .iter()
            .enumerate()
            .map(|(rank, &index)| (files[index].id.as_deref().unwrap(), rank))
            .collect();
        assert!(position["alg"] < position["ac_manifest"]);
        assert!(position["ac_manifest"] < position["pc_manifest"]);
        assert!(position["c_header"] < position["pc_manifest"]);
        assert!(position["c_source"] < position["pc_manifest"]);
        assert!(position["ac_manifest"] < position["content"]);
        assert!(position["pc_manifest"] < position["content"]);
        // `content` is a sink.
        assert_eq!(position["content"], files.len() - 1);
    }

    /// A mutual A<->B cycle renders nothing (parse allows it — both ids exist,
    /// no self edge — so the topo sort is the guard).
    #[test]
    fn topo_sort_rejects_a_cycle() {
        let files = manifest_files(
            r#"
version = 1
ir = "dae"
name = "cycle"
readiness_level = 2

[capabilities]

[[files]]
id = "a"
path = "a.xml"
template = "a.jinja"
  [[files.checksums]]
  of = "b"
  algorithm = "sha1"
  as = "b_sha1"

[[files]]
id = "b"
path = "b.xml"
template = "b.jinja"
  [[files.checksums]]
  of = "a"
  algorithm = "sha1"
  as = "a_sha1"
"#,
        );
        let err = topo_sort(&files).expect_err("a mutual cycle must render nothing");
        assert!(err.to_string().contains("cycle"), "{err}");
    }

    /// A self-hash edge is rejected at parse time (no file can embed its own
    /// hash — the DAG-by-construction invariant).
    #[test]
    fn self_hash_edge_is_rejected_at_parse() {
        let err = parse_target_manifest(
            r#"
version = 1
ir = "dae"
name = "self"
readiness_level = 2

[capabilities]

[[files]]
id = "m"
path = "m.xml"
template = "m.jinja"
  [[files.checksums]]
  of = "m"
  algorithm = "sha1"
  as = "m_sha1"
"#,
        )
        .expect_err("a self-hash edge must be refused");
        assert!(err.to_string().contains("checksums itself"), "{err}");
    }

    /// A checksum `of` naming no declared id is rejected at parse time.
    #[test]
    fn dangling_checksum_of_is_rejected_at_parse() {
        let err = parse_target_manifest(
            r#"
version = 1
ir = "dae"
name = "dangling"
readiness_level = 2

[capabilities]

[[files]]
id = "c"
path = "c.xml"
template = "c.jinja"
  [[files.checksums]]
  of = "missing"
  algorithm = "sha1"
  as = "missing_sha1"
"#,
        )
        .expect_err("a dangling checksum `of` must be refused");
        assert!(err.to_string().contains("names no [[files]] id"), "{err}");
    }

    /// End-to-end: render in dependency order, inject each producer's real
    /// SHA-1 downstream under its `as` key, and write the exact hashed bytes.
    /// The consumer's rendered content carries the SHA-1 of the producer's
    /// on-disk bytes — the no-placeholder guarantee, black-box.
    #[test]
    fn render_and_package_threads_real_producer_hashes() {
        let files = manifest_files(
            r#"
version = 1
ir = "dae"
name = "e2e"
readiness_level = 2

[capabilities]

[[assets]]
source = "fake-assets"
dest = "schemas/"

[[files]]
id = "leaf"
path = "leaf.txt"
template = "leaf-template"

[[files]]
id = "root"
path = "root.txt"
template = "root-template"
  [[files.checksums]]
  of = "leaf"
  algorithm = "sha1"
  as = "leaf_sha1"
"#,
        );
        // Fake renderer: `leaf-template` -> fixed content; `root-template` ->
        // text embedding the injected `leaf_sha1`; path templates -> the path.
        let render = |template: &str, artifact: &ArtifactRenderContext<'_>| -> Result<String> {
            Ok(match template {
                "leaf-template" => "LEAF-BODY".to_string(),
                "root-template" => format!(
                    "root sees leaf={}",
                    artifact
                        .checksums
                        .get("leaf_sha1")
                        .expect("leaf_sha1 injected")
                ),
                other => other.to_string(), // path templates render to themselves
            })
        };
        let asset_source = |source: &str| -> Result<Vec<TargetAssetFile>> {
            assert_eq!(source, "fake-assets");
            Ok(vec![TargetAssetFile {
                relative_path: "LICENSE".to_string(),
                bytes: b"license bytes".to_vec(),
            }])
        };
        let dir = tempfile::tempdir().expect("temp dir");
        let out_dir = dir.path().join("product");
        let package = PackageSpec {
            required_files: vec!["root.txt".to_string()],
            zip: None,
        };
        let manifest = parse_target_manifest(
            r#"
version = 1
ir = "dae"
name = "e2e"
readiness_level = 2

[capabilities]

[[assets]]
source = "fake-assets"
dest = "schemas/"

[[files]]
id = "leaf"
path = "leaf.txt"
template = "leaf-template"

[[files]]
id = "root"
path = "root.txt"
template = "root-template"
  [[files.checksums]]
  of = "leaf"
  algorithm = "sha1"
  as = "leaf_sha1"
"#,
        )
        .expect("manifest parses");

        render_and_package(
            &files,
            render,
            &manifest.assets,
            asset_source,
            &package,
            &out_dir,
        )
        .expect("declarative package build should succeed");

        let leaf_bytes = std::fs::read(out_dir.join("leaf.txt")).expect("leaf written");
        let expected = sha1_hex(&leaf_bytes);
        let root = std::fs::read_to_string(out_dir.join("root.txt")).expect("root written");
        assert_eq!(root, format!("root sees leaf={expected}"));
        let license =
            std::fs::read_to_string(out_dir.join("schemas/LICENSE")).expect("asset copied");
        assert_eq!(license, "license bytes");
    }
}
