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
use rumoca_compile::codegen::targets::{ChecksumAlgorithm, RenderedTargetFile, TargetFile};
use serde::Serialize;
use sha1::{Digest, Sha1};
use time::OffsetDateTime;
use time::macros::format_description;
use uuid::Uuid;

#[cfg(all(test, feature = "scheduled-sim"))]
mod tests;

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

#[cfg(feature = "scheduled-sim")]
struct PreparedProduct {
    files: BTreeMap<PathBuf, Vec<u8>>,
    required_files: Vec<PathBuf>,
}

#[cfg(feature = "scheduled-sim")]
struct StagedArchive {
    directory: tempfile::TempDir,
    file: PathBuf,
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
    let out_dir = resolve_product_root(out_dir)?;
    let archive_path = package
        .zip
        .as_ref()
        .map(|zip| resolve_archive_path(&zip.archive_path, &out_dir))
        .transpose()?;
    let required_files = resolve_required_files(&package.required_files)?;
    let rendered = render_web(files, render)?;
    let resolved_assets = assets
        .iter()
        .map(|asset| {
            Ok((
                asset.dest.clone(),
                asset_source(&asset.source)
                    .with_context(|| format!("Resolve target asset source '{}'", asset.source))?,
            ))
        })
        .collect::<Result<Vec<_>>>()?;
    let prepared = prepare_product(rendered, resolved_assets, required_files)?;
    validate_existing_root(&out_dir, &prepared.required_files)?;
    if let Some(archive_path) = &archive_path {
        validate_existing_archive(archive_path, &prepared.required_files)?;
    }

    let parent = out_dir
        .parent()
        .expect("validated product root has a parent");
    fs::create_dir_all(parent)
        .with_context(|| format!("Create package parent '{}'", parent.display()))?;
    let staged_root = tempfile::Builder::new()
        .prefix(".rumoca-package-")
        .tempdir_in(parent)
        .with_context(|| format!("Create package staging directory in '{}'", parent.display()))?;
    write_prepared_product(staged_root.path(), &prepared)?;
    validate_staged_product(staged_root.path(), &prepared.required_files)?;
    let staged_archive = archive_path
        .as_ref()
        .map(|archive_path| stage_archive(staged_root.path(), archive_path))
        .transpose()?;
    install_staged_package(
        staged_root,
        &out_dir,
        staged_archive,
        archive_path.as_deref(),
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

#[cfg(feature = "scheduled-sim")]
fn resolve_product_root(root: &Path) -> Result<PathBuf> {
    validate_destination_path(root, "Package root")?;
    std::path::absolute(root).with_context(|| format!("Resolve package root '{}'", root.display()))
}

#[cfg(feature = "scheduled-sim")]
fn resolve_archive_path(archive: &Path, root: &Path) -> Result<PathBuf> {
    validate_destination_path(archive, "Package archive path")?;
    let archive = std::path::absolute(archive)
        .with_context(|| format!("Resolve package archive '{}'", archive.display()))?;
    if archive == root || archive.starts_with(root) || root.starts_with(&archive) {
        bail!(
            "Package archive '{}' must not overlap package root '{}'",
            archive.display(),
            root.display()
        );
    }
    Ok(archive)
}

#[cfg(feature = "scheduled-sim")]
fn validate_destination_path(path: &Path, label: &str) -> Result<()> {
    if path.as_os_str().is_empty() {
        bail!("{label} must not be empty");
    }
    let mut names_child = false;
    for component in path.components() {
        match component {
            std::path::Component::Normal(_) => names_child = true,
            std::path::Component::ParentDir => {
                bail!("{label} '{}' must not contain traversal", path.display());
            }
            std::path::Component::CurDir
            | std::path::Component::RootDir
            | std::path::Component::Prefix(_) => {}
        }
    }
    if !names_child {
        bail!(
            "{label} '{}' must name a child path, not the filesystem or current directory",
            path.display()
        );
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn resolve_required_files(required_files: &[String]) -> Result<Vec<PathBuf>> {
    if required_files.is_empty() {
        bail!("[package] required_files must declare at least one product marker");
    }
    required_files
        .iter()
        .map(|required| resolve_relative_path(required, "[package] required file"))
        .collect()
}

#[cfg(feature = "scheduled-sim")]
fn resolve_relative_path(path: impl AsRef<Path>, label: &str) -> Result<PathBuf> {
    let path = path.as_ref();
    let joined = safe_target_join(Path::new("product"), path)
        .with_context(|| format!("Resolve {label} '{}'", path.display()))?;
    let relative = joined
        .strip_prefix("product")
        .expect("safe target join preserves its root");
    let normalized: PathBuf = relative
        .components()
        .filter_map(|component| match component {
            std::path::Component::Normal(segment) => Some(segment),
            std::path::Component::CurDir => None,
            _ => unreachable!("safe target join rejected non-relative components"),
        })
        .collect();
    if normalized.as_os_str().is_empty() {
        bail!("{label} '{}' must name a file or directory", path.display());
    }
    Ok(normalized)
}

#[cfg(feature = "scheduled-sim")]
fn prepare_product(
    rendered: Vec<(String, Vec<u8>)>,
    assets: Vec<(String, Vec<TargetAssetFile>)>,
    required_files: Vec<PathBuf>,
) -> Result<PreparedProduct> {
    let mut files = BTreeMap::new();
    for (path, bytes) in rendered {
        let path = resolve_relative_path(&path, "rendered file path")?;
        insert_prepared_file(&mut files, path, bytes)?;
    }
    for (dest, asset_files) in assets {
        let dest = resolve_relative_path(&dest, "asset destination")?;
        for asset_file in asset_files {
            let relative = resolve_relative_path(&asset_file.relative_path, "asset file path")?;
            insert_prepared_file(&mut files, dest.join(relative), asset_file.bytes)?;
        }
    }
    validate_file_tree(&files)?;
    for required in &required_files {
        if !files.contains_key(required) {
            bail!(
                "[package] required file '{}' is not produced by [[files]] or [[assets]]",
                required.display()
            );
        }
    }
    Ok(PreparedProduct {
        files,
        required_files,
    })
}

#[cfg(feature = "scheduled-sim")]
fn insert_prepared_file(
    files: &mut BTreeMap<PathBuf, Vec<u8>>,
    path: PathBuf,
    bytes: Vec<u8>,
) -> Result<()> {
    if files.insert(path.clone(), bytes).is_some() {
        bail!(
            "Package path '{}' is produced more than once",
            path.display()
        );
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn validate_file_tree(files: &BTreeMap<PathBuf, Vec<u8>>) -> Result<()> {
    for path in files.keys() {
        let mut ancestor = path.parent();
        while let Some(parent) = ancestor.filter(|parent| !parent.as_os_str().is_empty()) {
            if files.contains_key(parent) {
                bail!(
                    "Package path '{}' is both a file and a parent directory",
                    parent.display()
                );
            }
            ancestor = parent.parent();
        }
    }
    Ok(())
}

/// Make way for this run's product directory. A directory holding all declared
/// marker files is a previous build and can be replaced; any other non-empty
/// directory is treated as foreign.
#[cfg(feature = "scheduled-sim")]
fn validate_existing_root(out_dir: &Path, required_files: &[PathBuf]) -> Result<()> {
    if !out_dir.exists() {
        return Ok(());
    }
    let previous_product = out_dir.is_dir()
        && required_files
            .iter()
            .all(|required| out_dir.join(required).is_file());
    if previous_product {
        return Ok(());
    }
    let empty = out_dir.is_dir()
        && fs::read_dir(out_dir)
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
fn validate_existing_archive(archive_path: &Path, required_files: &[PathBuf]) -> Result<()> {
    if !archive_path.exists() {
        return Ok(());
    }
    let metadata = fs::symlink_metadata(archive_path)
        .with_context(|| format!("Inspect package archive '{}'", archive_path.display()))?;
    if !metadata.file_type().is_file() {
        bail!(
            "package archive path '{}' exists but is not a regular file",
            archive_path.display()
        );
    }
    let file = fs::File::open(archive_path)
        .with_context(|| format!("Open previous package archive '{}'", archive_path.display()))?;
    let mut archive = zip::ZipArchive::new(file).with_context(|| {
        format!(
            "package archive '{}' is not a recognized previous product",
            archive_path.display()
        )
    })?;
    for required in required_files {
        let marker = zip_entry_name(required)?;
        let entry = archive.by_name(&marker).with_context(|| {
            format!(
                "package archive '{}' is not a recognized previous product: marker '{}' is missing",
                archive_path.display(),
                marker
            )
        })?;
        if !entry.is_file() {
            bail!(
                "package archive '{}' has non-file marker '{}'",
                archive_path.display(),
                marker
            );
        }
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn zip_entry_name(path: &Path) -> Result<String> {
    path.components()
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
fn write_prepared_product(root: &Path, prepared: &PreparedProduct) -> Result<()> {
    for (path, bytes) in &prepared.files {
        let output_path = root.join(path);
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Create directory for '{}'", output_path.display()))?;
        }
        fs::write(&output_path, bytes)
            .with_context(|| format!("Write '{}'", output_path.display()))?;
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn validate_staged_product(root: &Path, required_files: &[PathBuf]) -> Result<()> {
    for required in required_files {
        let marker = root.join(required);
        if !marker.is_file() {
            bail!(
                "Staged package is incomplete: required marker '{}' is missing",
                required.display()
            );
        }
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn stage_archive(package_root: &Path, final_path: &Path) -> Result<StagedArchive> {
    let parent = final_path
        .parent()
        .expect("validated archive path has a parent");
    fs::create_dir_all(parent)
        .with_context(|| format!("Create archive parent '{}'", parent.display()))?;
    let directory = tempfile::Builder::new()
        .prefix(".rumoca-archive-")
        .tempdir_in(parent)
        .with_context(|| format!("Create archive staging directory in '{}'", parent.display()))?;
    let file = directory.path().join("package.zip");
    write_zip_package(package_root, &file).context("Build staged package archive")?;
    Ok(StagedArchive { directory, file })
}

#[cfg(feature = "scheduled-sim")]
fn install_staged_package(
    staged_root: tempfile::TempDir,
    final_root: &Path,
    staged_archive: Option<StagedArchive>,
    final_archive: Option<&Path>,
) -> Result<()> {
    let parent = final_root
        .parent()
        .expect("validated product root has a parent");
    let transaction = tempfile::Builder::new()
        .prefix(".rumoca-replace-")
        .tempdir_in(parent)
        .with_context(|| format!("Create replacement transaction in '{}'", parent.display()))?;
    let previous_root = transaction.path().join("previous-product");
    let had_root = final_root.exists();
    if had_root {
        fs::rename(final_root, &previous_root)
            .with_context(|| format!("Stage previous product '{}'", final_root.display()))?;
    }

    let previous_archive = staged_archive
        .as_ref()
        .map(|staged| staged.directory.path().join("previous"));
    let had_archive = final_archive.is_some_and(Path::exists);
    if let (Some(final_path), Some(previous)) = (final_archive, previous_archive.as_deref())
        && had_archive
        && let Err(error) = fs::rename(final_path, previous)
    {
        let primary = anyhow::Error::new(error)
            .context(format!("Stage previous archive '{}'", final_path.display()));
        let rollback =
            collect_rollback_errors([restore_previous_root(had_root, &previous_root, final_root)]);
        return Err(abort_install(
            primary,
            rollback,
            staged_root,
            transaction,
            staged_archive,
        ));
    }

    if let Err(error) = fs::rename(staged_root.path(), final_root) {
        let primary = anyhow::Error::new(error)
            .context(format!("Install staged product '{}'", final_root.display()));
        let rollback = collect_rollback_errors([
            restore_previous_archive(had_archive, previous_archive.as_deref(), final_archive),
            restore_previous_root(had_root, &previous_root, final_root),
        ]);
        return Err(abort_install(
            primary,
            rollback,
            staged_root,
            transaction,
            staged_archive,
        ));
    }

    if let (Some(staged), Some(final_path)) = (staged_archive.as_ref(), final_archive)
        && let Err(error) = fs::rename(&staged.file, final_path)
    {
        let primary = anyhow::Error::new(error).context(format!(
            "Install package archive '{}'",
            final_path.display()
        ));
        let rollback = collect_rollback_errors([
            restore_staged_root(staged_root.path(), final_root),
            restore_previous_archive(had_archive, previous_archive.as_deref(), final_archive),
            restore_previous_root(had_root, &previous_root, final_root),
        ]);
        return Err(abort_install(
            primary,
            rollback,
            staged_root,
            transaction,
            staged_archive,
        ));
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn collect_rollback_errors<const N: usize>(results: [Result<()>; N]) -> Vec<anyhow::Error> {
    results.into_iter().filter_map(Result::err).collect()
}

#[cfg(feature = "scheduled-sim")]
fn abort_install(
    primary: anyhow::Error,
    rollback_errors: Vec<anyhow::Error>,
    staged_root: tempfile::TempDir,
    transaction: tempfile::TempDir,
    staged_archive: Option<StagedArchive>,
) -> anyhow::Error {
    if rollback_errors.is_empty() {
        return primary;
    }
    let mut recovery = vec![staged_root.keep(), transaction.keep()];
    if let Some(staged_archive) = staged_archive {
        recovery.push(staged_archive.directory.keep());
    }
    let recovery = recovery
        .iter()
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>()
        .join(", ");
    let rollback = rollback_errors
        .iter()
        .map(|error| format!("{error:#}"))
        .collect::<Vec<_>>()
        .join("; ");
    primary.context(format!(
        "replacement rollback was incomplete ({rollback}); recovery directories retained: {recovery}"
    ))
}

#[cfg(feature = "scheduled-sim")]
fn restore_staged_root(staged_root: &Path, final_root: &Path) -> Result<()> {
    fs::rename(final_root, staged_root).with_context(|| {
        format!(
            "Rollback newly installed product '{}'",
            final_root.display()
        )
    })
}

#[cfg(feature = "scheduled-sim")]
fn restore_previous_root(had_root: bool, previous_root: &Path, final_root: &Path) -> Result<()> {
    if had_root {
        fs::rename(previous_root, final_root)
            .with_context(|| format!("Restore previous product '{}'", final_root.display()))?;
    }
    Ok(())
}

#[cfg(feature = "scheduled-sim")]
fn restore_previous_archive(
    had_archive: bool,
    previous_archive: Option<&Path>,
    final_archive: Option<&Path>,
) -> Result<()> {
    if had_archive {
        fs::rename(
            previous_archive.expect("previous archive path exists"),
            final_archive.expect("final archive path exists"),
        )
        .with_context(|| {
            format!(
                "Restore previous archive '{}'",
                final_archive.expect("final archive path exists").display()
            )
        })?;
    }
    Ok(())
}

/// Write a deterministic flat zip of `package_root` to a new staging file.
#[cfg(feature = "scheduled-sim")]
fn write_zip_package(package_root: &Path, archive_path: &Path) -> Result<()> {
    let mut relative_paths = Vec::new();
    collect_package_files(package_root, package_root, &mut relative_paths)?;
    relative_paths.sort();
    write_zip_entries(package_root, archive_path, &relative_paths)
}

#[cfg(feature = "scheduled-sim")]
fn collect_package_files(root: &Path, dir: &Path, out: &mut Vec<String>) -> Result<()> {
    for entry in
        fs::read_dir(dir).with_context(|| format!("Read package directory '{}'", dir.display()))?
    {
        let entry = entry.with_context(|| format!("Read package entry in '{}'", dir.display()))?;
        let path = entry.path();
        let file_type = entry
            .file_type()
            .with_context(|| format!("Stat package entry '{}'", path.display()))?;
        if file_type.is_symlink() {
            bail!(
                "Package trees may not contain symlinks: '{}'",
                path.display()
            );
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
    relative_paths: &[String],
) -> Result<()> {
    let file = fs::File::create(archive_path)
        .with_context(|| format!("Create staged archive '{}'", archive_path.display()))?;
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
