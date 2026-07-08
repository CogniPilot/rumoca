//! Generic declarative checksum/packaging build step (contract §4).
//!
//! This is the product-agnostic packaging path for the `galec`/`galec-production`
//! eFMU targets (contract §9 WI-5). Rather than baking eFMI representation
//! grouping and `__content.xml` synthesis into Rust, this build step is driven
//! entirely by `target.toml` declarations: `[[files]]` carry a logical `id` and
//! `[[files.checksums]]` edges (`of -> as`), and `[[assets]]` name bundles to
//! copy verbatim. It renders every file **in dependency order**, hashes the
//! EXACT bytes it is about to write, and threads each producer's SHA-1 into the
//! downstream files' context under the declared `as` key. No XML shape, no
//! eFMI knowledge, lives here — the product lives in the templates and the
//! `target.toml`.
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
//! No placeholder is representable ([`Sha1Hex`] is constructible only from
//! real bytes or a strict parse), and the bytes hashed are the exact bytes
//! written (one `bytes` buffer feeds both `Sha1Hex::of_bytes` and the write).

use std::collections::{BTreeMap, HashMap};
#[cfg(feature = "scheduled-sim")]
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
#[cfg(feature = "scheduled-sim")]
use rumoca_compile::codegen::targets::{AssetBundle, safe_target_join};
use rumoca_compile::codegen::targets::{RenderedTargetFile, TargetFile};
use rumoca_compile::galec::Sha1Hex;

#[cfg(feature = "scheduled-sim")]
use crate::container;

/// How the rendered files + assets are finalized on disk (contract §4b).
///
/// The `.efmu`/flat-zip specifics are fields, not constants, so `build_fmu`
/// can fold into this one step later with different values (out of scope here).
#[cfg(feature = "scheduled-sim")]
pub struct PackageSpec {
    /// The product's root index file (e.g. `__content.xml`). A directory that
    /// already holds it is recognized as a prior build of THIS product and is
    /// replaced on re-run; anything else non-empty is foreign and refused.
    pub index: String,
    /// The zip package form, if the product has one. `None` = directory-only.
    pub zip: Option<ZipPackage>,
}

/// The zip form of a package (contract §4b `Zip { ext }`).
#[cfg(feature = "scheduled-sim")]
pub struct ZipPackage {
    /// Absolute path of the archive to write (e.g. `<out>/<model>.efmu`).
    pub archive_path: PathBuf,
}

/// One resolved asset file: a bundle-relative `/`-separated path and its exact
/// bytes, copied verbatim into the product (contract §4d).
#[cfg(feature = "scheduled-sim")]
pub struct AssetFile {
    /// Path relative to the bundle's declared `dest`, `/`-separated.
    pub relative_path: String,
    /// The exact bytes to write.
    pub bytes: Vec<u8>,
}

/// Resolve a declared `[[assets]]` bundle name to the vendored eFMI XSD tree
/// (contract §4d): the generic build step's real asset source. Fails early on
/// an unknown bundle so a `target.toml` typo cannot silently ship an empty
/// `schemas/`.
#[cfg(feature = "scheduled-sim")]
pub fn efmi_asset_source(bundle: &str) -> Result<Vec<AssetFile>> {
    let files = container::asset_bundle_files(bundle)
        .with_context(|| format!("Unknown asset bundle '{bundle}'"))?;
    Ok(files
        .into_iter()
        .map(|(relative_path, bytes)| AssetFile {
            relative_path: relative_path.to_string(),
            bytes: bytes.to_vec(),
        })
        .collect())
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
/// context is built (typed export vs IR JSON). `asset_source(bundle)` resolves
/// a declared bundle name to its files. Nothing is written until every byte is
/// rendered and hashed, so a render or topo failure leaves the product path
/// untouched.
#[cfg(feature = "scheduled-sim")]
pub fn render_and_package(
    files: &[TargetFile],
    render: impl Fn(&str, &BTreeMap<String, String>) -> Result<String>,
    assets: &[AssetBundle],
    asset_source: impl Fn(&str) -> Result<Vec<AssetFile>>,
    package: &PackageSpec,
    out_dir: &Path,
) -> Result<()> {
    let rendered = render_web(files, render)?;

    prepare_root(out_dir, &package.index)?;
    write_rendered_files(out_dir, &rendered)?;
    for asset in assets {
        copy_asset_bundle(out_dir, asset, &asset_source)?;
    }
    if let Some(zip) = &package.zip {
        container::write_efmu_zip(out_dir, &zip.archive_path)
            .context("Write package (zip form)")?;
    }
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
    render: impl Fn(&str, &BTreeMap<String, String>) -> Result<String>,
) -> Result<Vec<(String, Vec<u8>)>> {
    let order = topo_sort(files)?;

    let mut sha1: HashMap<String, Sha1Hex> = HashMap::new();
    // Render into `order` positions but return in declaration order, so the
    // in-memory caller sees files in the same order the `target.toml` declares
    // them (`render_target_files` asserts a 1:1 file-count match).
    let mut rendered: Vec<Option<(String, Vec<u8>)>> = (0..files.len()).map(|_| None).collect();
    for &index in &order {
        let file = &files[index];
        let mut checksums = BTreeMap::new();
        for need in &file.checksums {
            // Guaranteed present: the producer precedes this file in `order`.
            let digest = sha1.get(&need.of).with_context(|| {
                format!(
                    "internal: producer '{}' hash missing while rendering '{}'",
                    need.of, file.path
                )
            })?;
            checksums.insert(need.as_key.clone(), digest.as_str().to_string());
        }
        let path = render(&file.path, &checksums)
            .with_context(|| format!("Render target output path '{}'", file.path))?
            .trim()
            .to_string();
        let bytes = render(&file.template, &checksums)
            .with_context(|| format!("Render target template '{}'", file.template))?
            .into_bytes();
        // Hash the EXACT bytes that will be written (§4c): `Sha1Hex::of_bytes`
        // and the write below share this one buffer with no intervening reformat.
        if let Some(id) = &file.id {
            sha1.insert(id.clone(), Sha1Hex::of_bytes(&bytes));
        }
        rendered[index] = Some((path, bytes));
    }

    Ok(rendered
        .into_iter()
        .map(|entry| entry.expect("every file rendered exactly once in topological order"))
        .collect())
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
    render: impl Fn(&str, &BTreeMap<String, String>) -> Result<String>,
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

/// Make way for this run's product directory (contract §4b): a directory
/// holding the package `index` file is a previous build of this product and is
/// removed so the writer starts clean; an empty directory is accepted; anything
/// else non-empty is foreign and refused with a remedy, never deleted.
#[cfg(feature = "scheduled-sim")]
fn prepare_root(out_dir: &Path, index: &str) -> Result<()> {
    if !out_dir.exists() {
        std::fs::create_dir_all(out_dir)
            .with_context(|| format!("Create product directory `{}`", out_dir.display()))?;
        return Ok(());
    }
    if out_dir.is_dir() && out_dir.join(index).is_file() {
        std::fs::remove_dir_all(out_dir).with_context(|| {
            format!("Remove the previous build product `{}`", out_dir.display())
        })?;
        std::fs::create_dir_all(out_dir)
            .with_context(|| format!("Recreate product directory `{}`", out_dir.display()))?;
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
         (no `{index}`); refusing to remove it. Delete it or choose a different `--output` \
         directory.",
        out_dir.display()
    );
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

/// Copy one declared asset bundle into `out_dir/<dest>` verbatim (contract
/// §4d): assets are outside the render/hash DAG, so this is a pure file copy.
#[cfg(feature = "scheduled-sim")]
fn copy_asset_bundle(
    out_dir: &Path,
    asset: &AssetBundle,
    asset_source: &impl Fn(&str) -> Result<Vec<AssetFile>>,
) -> Result<()> {
    let dest_root = safe_target_join(out_dir, &asset.dest)?;
    for file in asset_source(&asset.bundle)
        .with_context(|| format!("Resolve asset bundle '{}'", asset.bundle))?
    {
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
  as = "alg_sha1"

[[files]]
id = "pc_manifest"
path = "ProductionCode/manifest.xml"
template = "pc.jinja"
  [[files.checksums]]
  of = "ac_manifest"
  as = "ac_manifest_sha1"
  [[files.checksums]]
  of = "c_header"
  as = "c_header_sha1"
  [[files.checksums]]
  of = "c_source"
  as = "c_source_sha1"

[[files]]
id = "content"
path = "__content.xml"
template = "content.jinja"
  [[files.checksums]]
  of = "ac_manifest"
  as = "ac_manifest_sha1"
  [[files.checksums]]
  of = "pc_manifest"
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

[[files]]
id = "a"
path = "a.xml"
template = "a.jinja"
  [[files.checksums]]
  of = "b"
  as = "b_sha1"

[[files]]
id = "b"
path = "b.xml"
template = "b.jinja"
  [[files.checksums]]
  of = "a"
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

[[files]]
id = "m"
path = "m.xml"
template = "m.jinja"
  [[files.checksums]]
  of = "m"
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

[[files]]
id = "c"
path = "c.xml"
template = "c.jinja"
  [[files.checksums]]
  of = "missing"
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

[[assets]]
bundle = "fake-bundle"
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
  as = "leaf_sha1"
"#,
        );
        // Fake renderer: `leaf-template` -> fixed content; `root-template` ->
        // text embedding the injected `leaf_sha1`; path templates -> the path.
        let render = |template: &str, checksums: &BTreeMap<String, String>| -> Result<String> {
            Ok(match template {
                "leaf-template" => "LEAF-BODY".to_string(),
                "root-template" => format!(
                    "root sees leaf={}",
                    checksums.get("leaf_sha1").expect("leaf_sha1 injected")
                ),
                other => other.to_string(), // path templates render to themselves
            })
        };
        let asset_source = |bundle: &str| -> Result<Vec<AssetFile>> {
            assert_eq!(bundle, "fake-bundle");
            Ok(vec![AssetFile {
                relative_path: "LICENSE".to_string(),
                bytes: b"license bytes".to_vec(),
            }])
        };
        let dir = tempfile::tempdir().expect("temp dir");
        let out_dir = dir.path().join("product");
        let package = PackageSpec {
            index: "root.txt".to_string(),
            zip: None,
        };
        let manifest = parse_target_manifest(
            r#"
version = 1
ir = "dae"
name = "e2e"
readiness_level = 2

[[assets]]
bundle = "fake-bundle"
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
        let expected = Sha1Hex::of_bytes(&leaf_bytes);
        let root = std::fs::read_to_string(out_dir.join("root.txt")).expect("root written");
        assert_eq!(root, format!("root sees leaf={}", expected.as_str()));
        let license =
            std::fs::read_to_string(out_dir.join("schemas/LICENSE")).expect("asset copied");
        assert_eq!(license, "license bytes");
    }
}
