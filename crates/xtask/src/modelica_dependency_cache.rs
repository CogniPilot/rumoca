use anyhow::{Context, Result, anyhow, ensure};
use clap::{Args, Subcommand};
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fs;
use std::io::{Cursor, Read};
use std::path::{Component, Path, PathBuf};
use zip::ZipArchive;

const MANIFEST_FILE: &str = "examples/modelica_dependencies.toml";
const MSL_KEY: &str = "msl";
const CMM_KEY: &str = "cmm";
const REVISION_FILE: &str = ".rumoca-source-revision";

#[derive(Debug, Args, Clone)]
pub(crate) struct CmmArgs {
    #[command(subcommand)]
    pub(crate) command: CmmCommand,
}

#[derive(Debug, Subcommand, Clone)]
pub(crate) enum CmmCommand {
    /// Download and validate cached CogniPilot Modelica Models (CMM)
    Ensure(CmmEnsureArgs),
    /// Validate cached CogniPilot Modelica Models (CMM) without downloading
    Check,
    /// Print the CMM source-root path
    SourceRoot,
}

#[derive(Debug, Args, Clone)]
pub(crate) struct CmmEnsureArgs {
    /// Re-download CMM even when the local cache already validates
    #[arg(long)]
    pub(crate) force: bool,
}

#[derive(Debug, Args, Clone)]
pub(crate) struct ModelicaDepsArgs {
    #[command(subcommand)]
    pub(crate) command: ModelicaDepsCommand,
}

#[derive(Debug, Subcommand, Clone)]
pub(crate) enum ModelicaDepsCommand {
    /// Download and validate cached Modelica dependencies used by examples
    Ensure(ModelicaDepsEnsureArgs),
    /// Validate cached Modelica dependencies without downloading
    Check,
    /// Print source-root paths for cached Modelica dependencies
    SourceRoots,
}

#[derive(Debug, Args, Clone)]
pub(crate) struct ModelicaDepsEnsureArgs {
    /// Re-download dependencies even when local caches already validate
    #[arg(long)]
    pub(crate) force: bool,
}

#[derive(Debug, Deserialize)]
struct DependencyManifest {
    version: u32,
    libraries: BTreeMap<String, LibrarySpec>,
}

#[derive(Debug, Deserialize)]
struct LibrarySpec {
    display_name: String,
    version: String,
    url: String,
    sha256: String,
    cache_dir: PathBuf,
    zip_layout: ZipLayout,
    required_files: Vec<PathBuf>,
    #[serde(default)]
    write_revision_marker: bool,
}

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
enum ZipLayout {
    Direct,
    AutoStripCommonRoot,
}

pub(crate) fn run_cmm_command(args: CmmArgs, root: &Path) -> Result<()> {
    let manifest = load_manifest(root)?;
    let cmm = required_library(&manifest, CMM_KEY)?;
    match args.command {
        CmmCommand::Ensure(args) => {
            ensure_library(root, cmm, args.force)?;
            println!("{} {} is ready.", cmm.display_name, cmm.version);
        }
        CmmCommand::Check => {
            check_library(root, cmm)?;
            println!("{} {} cache is valid.", cmm.display_name, cmm.version);
        }
        CmmCommand::SourceRoot => {
            println!("{}", library_dir(root, cmm).display());
        }
    }
    Ok(())
}

pub(crate) fn run_modelica_deps_command(args: ModelicaDepsArgs, root: &Path) -> Result<()> {
    let manifest = load_manifest(root)?;
    match args.command {
        ModelicaDepsCommand::Ensure(args) => {
            for key in [MSL_KEY, CMM_KEY] {
                ensure_library(root, required_library(&manifest, key)?, args.force)?;
            }
            println!("Modelica dependency caches are ready.");
        }
        ModelicaDepsCommand::Check => {
            for key in [MSL_KEY, CMM_KEY] {
                check_library(root, required_library(&manifest, key)?)?;
            }
            println!("Modelica dependency caches are valid.");
        }
        ModelicaDepsCommand::SourceRoots => {
            for key in [MSL_KEY, CMM_KEY] {
                let spec = required_library(&manifest, key)?;
                println!("{}", library_dir(root, spec).display());
            }
        }
    }
    Ok(())
}

pub(crate) fn ensure_example_libraries(root: &Path, force: bool) -> Result<()> {
    let manifest = load_manifest(root)?;
    for key in [MSL_KEY, CMM_KEY] {
        ensure_library(root, required_library(&manifest, key)?, force)?;
    }
    Ok(())
}

fn load_manifest(root: &Path) -> Result<DependencyManifest> {
    let path = root.join(MANIFEST_FILE);
    let raw =
        fs::read_to_string(&path).with_context(|| format!("failed to read {}", path.display()))?;
    let manifest: DependencyManifest =
        toml::from_str(&raw).with_context(|| format!("failed to parse {}", path.display()))?;
    ensure!(
        manifest.version == 1,
        "{} has unsupported version {}; expected version 1",
        path.display(),
        manifest.version
    );
    Ok(manifest)
}

fn required_library<'a>(manifest: &'a DependencyManifest, key: &str) -> Result<&'a LibrarySpec> {
    manifest
        .libraries
        .get(key)
        .ok_or_else(|| anyhow!("{MANIFEST_FILE} is missing [libraries.{key}]"))
}

fn ensure_library(root: &Path, spec: &LibrarySpec, force: bool) -> Result<()> {
    let dir = library_dir(root, spec);
    if !force && cache_layout_valid(&dir, spec) {
        println!(
            "{} {} already cached at {}",
            spec.display_name,
            spec.version,
            dir.display()
        );
        return Ok(());
    }

    println!(
        "Downloading {} {} from {}",
        spec.display_name, spec.version, spec.url
    );
    let data = download_to_memory(&spec.url)?;
    verify_sha256(&data, &spec.sha256, &spec.url)?;
    reset_dir(&dir)?;
    extract_zip(&data, &dir, spec.zip_layout)?;
    if spec.write_revision_marker {
        fs::write(dir.join(REVISION_FILE), revision_marker(spec))
            .with_context(|| format!("failed to write revision marker in {}", dir.display()))?;
    }
    ensure!(
        cache_layout_valid(&dir, spec),
        "downloaded {} cache is invalid at {}",
        spec.display_name,
        dir.display()
    );
    println!(
        "Cached {} {} at {}",
        spec.display_name,
        spec.version,
        dir.display()
    );
    Ok(())
}

fn check_library(root: &Path, spec: &LibrarySpec) -> Result<()> {
    let dir = library_dir(root, spec);
    ensure!(
        cache_layout_valid(&dir, spec),
        "{} cache is missing or invalid at {}",
        spec.display_name,
        dir.display()
    );
    Ok(())
}

fn library_dir(root: &Path, spec: &LibrarySpec) -> PathBuf {
    root.join(&spec.cache_dir)
}

fn cache_layout_valid(dir: &Path, spec: &LibrarySpec) -> bool {
    if spec.write_revision_marker {
        let marker_matches = fs::read_to_string(dir.join(REVISION_FILE))
            .map(|text| text == revision_marker(spec))
            .unwrap_or(false);
        if !marker_matches {
            return false;
        }
    }
    spec.required_files
        .iter()
        .all(|relative| dir.join(relative).is_file())
}

fn revision_marker(spec: &LibrarySpec) -> String {
    format!("{}\nsha256:{}\n", spec.version, spec.sha256)
}

fn download_to_memory(url: &str) -> Result<Vec<u8>> {
    let response = ureq::get(url)
        .call()
        .map_err(|error| anyhow!("download failed for {url}: {error}"))?;
    let size = response
        .header("content-length")
        .and_then(|value| value.parse::<usize>().ok())
        .unwrap_or(0);
    let mut data = Vec::with_capacity(size);
    response
        .into_reader()
        .read_to_end(&mut data)
        .with_context(|| format!("failed to read response body from {url}"))?;
    Ok(data)
}

fn verify_sha256(data: &[u8], expected: &str, label: &str) -> Result<()> {
    let digest = Sha256::digest(data);
    let actual = digest
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect::<String>();
    ensure!(
        actual == expected,
        "SHA-256 mismatch for {label}: expected {expected}, got {actual}"
    );
    Ok(())
}

fn reset_dir(dir: &Path) -> Result<()> {
    if dir.exists() {
        fs::remove_dir_all(dir).with_context(|| format!("failed to remove {}", dir.display()))?;
    }
    fs::create_dir_all(dir).with_context(|| format!("failed to create {}", dir.display()))
}

fn extract_zip(data: &[u8], output_dir: &Path, layout: ZipLayout) -> Result<()> {
    let cursor = Cursor::new(data);
    let mut archive = ZipArchive::new(cursor).context("failed to open zip archive")?;
    let strip_common_root =
        matches!(layout, ZipLayout::AutoStripCommonRoot) && archive_has_common_root(&mut archive)?;

    for index in 0..archive.len() {
        let mut entry = archive
            .by_index(index)
            .with_context(|| format!("failed to read zip entry {index}"))?;
        let enclosed = entry
            .enclosed_name()
            .ok_or_else(|| anyhow!("zip entry has unsafe path: {}", entry.name()))?;
        let Some(relative_path) = zip_output_relative_path(&enclosed, strip_common_root) else {
            continue;
        };
        let output_path = output_dir.join(relative_path);

        if entry.is_dir() {
            fs::create_dir_all(&output_path)
                .with_context(|| format!("failed to create {}", output_path.display()))?;
            continue;
        }

        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("failed to create {}", parent.display()))?;
        }
        let mut file = fs::File::create(&output_path)
            .with_context(|| format!("failed to create {}", output_path.display()))?;
        std::io::copy(&mut entry, &mut file)
            .with_context(|| format!("failed to extract {}", output_path.display()))?;
    }

    Ok(())
}

fn archive_has_common_root(archive: &mut ZipArchive<Cursor<&[u8]>>) -> Result<bool> {
    let mut common_root: Option<PathBuf> = None;
    for index in 0..archive.len() {
        let entry = archive
            .by_index(index)
            .with_context(|| format!("failed to inspect zip entry {index}"))?;
        let Some(path) = entry.enclosed_name() else {
            continue;
        };
        let Some(Component::Normal(first)) = path.components().next() else {
            return Ok(false);
        };
        let first = PathBuf::from(first);
        match &common_root {
            Some(root) if root != &first => return Ok(false),
            Some(_) => {}
            None => common_root = Some(first),
        }
    }
    Ok(common_root.is_some())
}

fn zip_output_relative_path(path: &Path, strip_common_root: bool) -> Option<PathBuf> {
    if !strip_common_root {
        return Some(path.to_path_buf());
    }
    let mut components = path.components();
    components.next()?;
    let mut stripped = PathBuf::new();
    for component in components {
        if let Component::Normal(part) = component {
            stripped.push(part);
        }
    }
    (!stripped.as_os_str().is_empty()).then_some(stripped)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cmm_spec(root: PathBuf) -> LibrarySpec {
        LibrarySpec {
            display_name: "CMM".to_string(),
            version: "v0.0.1".to_string(),
            url: "https://example.invalid/CMM.zip".to_string(),
            sha256: "abc123".to_string(),
            cache_dir: root,
            zip_layout: ZipLayout::AutoStripCommonRoot,
            required_files: vec![
                PathBuf::from("LieGroup/package.mo"),
                PathBuf::from("RigidBody/package.mo"),
            ],
            write_revision_marker: true,
        }
    }

    #[test]
    fn cmm_cache_requires_revision_marker_and_packages() {
        let temp = tempfile::tempdir().expect("tempdir");
        let spec = cmm_spec(PathBuf::from("unused"));
        let root = temp.path();
        fs::create_dir_all(root.join("LieGroup")).expect("LieGroup dir");
        fs::create_dir_all(root.join("RigidBody")).expect("RigidBody dir");
        fs::write(
            root.join("LieGroup/package.mo"),
            "package LieGroup\nend LieGroup;\n",
        )
        .expect("LieGroup package");
        fs::write(
            root.join("RigidBody/package.mo"),
            "package RigidBody\nend RigidBody;\n",
        )
        .expect("RigidBody package");
        assert!(!cache_layout_valid(root, &spec));

        fs::write(root.join(REVISION_FILE), revision_marker(&spec)).expect("revision");
        assert!(cache_layout_valid(root, &spec));
    }

    #[test]
    fn strip_archive_root_removes_single_zip_prefix() {
        let path = Path::new("CogniPilotModelicaModels-sha/RigidBody/package.mo");
        assert_eq!(
            zip_output_relative_path(path, true).as_deref(),
            Some(Path::new("RigidBody/package.mo"))
        );
        assert_eq!(
            zip_output_relative_path(Path::new("CogniPilotModelicaModels-sha"), true),
            None
        );
        assert_eq!(
            zip_output_relative_path(Path::new("RigidBody/package.mo"), false).as_deref(),
            Some(Path::new("RigidBody/package.mo"))
        );
    }

    #[test]
    fn verifies_sha256_before_extracting_dependency() {
        let data = b"cmm";
        let expected = Sha256::digest(data)
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect::<String>();
        verify_sha256(data, &expected, "test").expect("matching hash");
        assert!(verify_sha256(data, "0", "test").is_err());
    }
}
