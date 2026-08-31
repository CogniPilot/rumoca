//! Frozen registry dependency closure for the compiler-under-test build.

use super::artifact_bundle::FileEvidence;
use super::process;
use super::typed_path::{CargoExecutable, RolePath, RustcExecutable};
use anyhow::{Context, Result, anyhow, ensure};
use flate2::read::GzDecoder;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File, OpenOptions};
use std::io::{Cursor, Read, Write};
use std::path::{Component, Path, PathBuf};

const MAX_ARCHIVE_BYTES: u64 = 64 * 1024 * 1024;
const MAX_EXPANDED_PACKAGE_BYTES: u64 = 256 * 1024 * 1024;
const MAX_VENDOR_BYTES: u64 = 512 * 1024 * 1024;
const MAX_TOOLCHAIN_BYTES: u64 = 1024 * 1024 * 1024;

pub(super) struct ResolutionInputs<'a> {
    pub(super) cargo: &'a RolePath<CargoExecutable>,
    pub(super) rustc: &'a RolePath<RustcExecutable>,
    pub(super) source_root: &'a Path,
    pub(super) destination: &'a Path,
    pub(super) artifact_root: &'a Path,
    pub(super) temporary_dir: &'a Path,
    pub(super) source_cargo_home: &'a Path,
    pub(super) nix_path: &'a std::ffi::OsStr,
    pub(super) expected_sha256: &'a str,
}

pub(super) struct DependencySnapshot {
    root: PathBuf,
    captured: CapturedVendor,
    evidence: DependencyEvidence,
    commands: Vec<process::CommandReceipt>,
}

#[derive(Serialize)]
pub(super) struct DependencyEvidence {
    pub(super) closure_sha256: String,
    pub(super) registry_packages: usize,
    pub(super) full_registry_packages: usize,
    pub(super) resolution_manifest_packages: usize,
    pub(super) local_packages: usize,
    pub(super) build_dependency_edges: usize,
    pub(super) proc_macro_packages: usize,
    pub(super) manifest_artifact: FileEvidence,
}

#[derive(Deserialize)]
struct Metadata {
    packages: Vec<MetadataPackage>,
    resolve: MetadataResolve,
}

#[derive(Deserialize)]
struct MetadataPackage {
    name: String,
    version: String,
    id: String,
    source: Option<String>,
    manifest_path: PathBuf,
    targets: Vec<MetadataTarget>,
}

#[derive(Deserialize)]
struct MetadataTarget {
    kind: Vec<String>,
    src_path: PathBuf,
}

#[derive(Deserialize)]
struct MetadataResolve {
    nodes: Vec<MetadataNode>,
}

#[derive(Deserialize)]
struct MetadataNode {
    id: String,
    deps: Vec<MetadataDependency>,
}

#[derive(Deserialize)]
struct MetadataDependency {
    pkg: String,
    dep_kinds: Vec<DependencyKind>,
}

#[derive(Deserialize)]
struct DependencyKind {
    kind: Option<String>,
}

struct SelectedClosure {
    registry: Vec<SelectedRegistryPackage>,
    local_packages: usize,
    build_dependency_edges: usize,
    proc_macro_packages: usize,
}

struct ResolutionCatalog {
    registry: Vec<SelectedRegistryPackage>,
}

#[derive(Clone, Eq, Ord, PartialEq, PartialOrd)]
struct SelectedRegistryPackage {
    name: String,
    version: String,
    source: String,
    checksum: String,
    resolution_files: BTreeSet<PathBuf>,
}

struct CapturedVendor {
    packages: Vec<CapturedPackage>,
}

struct CapturedPackage {
    directory: String,
    archive_sha256: String,
    materialization: PackageMaterialization,
    files: Vec<CapturedFile>,
}

#[derive(Clone, Copy, Serialize)]
#[serde(rename_all = "kebab-case")]
enum PackageMaterialization {
    Full,
    ResolutionManifest,
}

struct CapturedFile {
    relative: PathBuf,
    mode: u32,
    source_sha256: Option<String>,
    bytes: Vec<u8>,
}

const RESOLUTION_TARGET_SENTINEL: &[u8] =
    b"compile_error!(\"resolution-only dependency target cannot be compiled\");\n";

#[derive(Serialize)]
struct DependencyManifest<'a> {
    closure_sha256: &'a str,
    packages: Vec<DependencyManifestPackage<'a>>,
}

#[derive(Serialize)]
struct DependencyManifestPackage<'a> {
    directory: &'a str,
    archive_sha256: &'a str,
    materialization: PackageMaterialization,
    files: usize,
}

pub(super) fn stage(inputs: ResolutionInputs<'_>) -> Result<DependencySnapshot> {
    ensure!(inputs.source_root.is_absolute());
    ensure!(inputs.destination.is_absolute() && !inputs.destination.exists());
    ensure!(inputs.temporary_dir.is_absolute());
    ensure!(inputs.source_cargo_home.is_absolute());
    fs::create_dir_all(inputs.temporary_dir)?;
    let (all_target_output, all_target_receipt) = metadata_output(&inputs, None)?;
    let (host_output, host_receipt) = metadata_output(&inputs, Some(process::COMPILER_HOST_TARGET))
        .map_err(|error| process::attach_prior_receipts(error, vec![all_target_receipt.clone()]))?;
    let receipts = vec![all_target_receipt, host_receipt];
    let result = (|| {
        let metadata: Metadata = serde_json::from_slice(&all_target_output.stdout)
            .context("Cargo metadata output is not the expected JSON contract")?;
        let host_metadata: Metadata = serde_json::from_slice(&host_output.stdout)
            .context("host-filtered Cargo metadata output is not the expected JSON contract")?;
        let lock = lock_checksums(&inputs.source_root.join("Cargo.lock"))?;
        let selected = select_resolution_catalog(&metadata, inputs.source_root, &lock)?;
        let host_selected = select_closure(&host_metadata, inputs.source_root, &lock)?;
        // A Cargo directory source must expose every manifest used during
        // resolution, including target-inactive packages. Only the explicitly
        // pinned host closure may carry buildable source bytes: if the host
        // selector omits a package Cargo actually compiles, that build fails
        // on the deliberately absent source instead of widening authority.
        let captured = capture_registry_packages(
            &selected.registry,
            &host_selected.registry,
            inputs.source_cargo_home,
        )?;
        let full_registry_packages = captured
            .packages
            .iter()
            .filter(|package| matches!(package.materialization, PackageMaterialization::Full))
            .count();
        let resolution_manifest_packages = captured.packages.len() - full_registry_packages;
        let closure_sha256 = captured.sha256();
        ensure!(
            closure_sha256 == inputs.expected_sha256,
            "compiler dependency closure changed: observed {closure_sha256}, expected {}",
            inputs.expected_sha256
        );
        captured.publish(inputs.destination)?;
        captured.verify(inputs.destination, &closure_sha256)?;
        let manifest_artifact = publish_manifest(inputs.artifact_root, &closure_sha256, &captured)?;
        Ok(DependencySnapshot {
            root: inputs.destination.to_path_buf(),
            evidence: DependencyEvidence {
                closure_sha256,
                registry_packages: captured.packages.len(),
                full_registry_packages,
                resolution_manifest_packages,
                local_packages: host_selected.local_packages,
                build_dependency_edges: host_selected.build_dependency_edges,
                proc_macro_packages: host_selected.proc_macro_packages,
                manifest_artifact,
            },
            captured,
            commands: receipts.clone(),
        })
    })();
    result.map_err(|error| process::attach_prior_receipts(error, receipts))
}

fn metadata_output(
    inputs: &ResolutionInputs<'_>,
    filter_platform: Option<&str>,
) -> Result<(std::process::Output, process::CommandReceipt)> {
    let mut command = process::CompilerMetadataCommand::rumoca(process::CompilerMetadataInputs {
        cargo: inputs.cargo,
        rustc: inputs.rustc,
        source_root: inputs.source_root,
        temporary_dir: inputs.temporary_dir,
        cargo_home: inputs.source_cargo_home,
        nix_path: inputs.nix_path,
        filter_platform,
    })?;
    let (output, receipt) = process::compiler_metadata_output(&mut command)?;
    if output.status.success() {
        return Ok((output, receipt));
    }
    Err(process::attach_attempted_receipt(
        anyhow!(
            "pinned Cargo dependency resolution failed ({})\n{}",
            output.status,
            process::tail(&process::combined(&output))
        ),
        receipt,
    ))
}

pub(super) fn toolchain_tree_sha256(root: &Path) -> Result<String> {
    ensure!(
        root.is_absolute() && root.is_dir(),
        "Rust sysroot must be an absolute directory"
    );
    let mut paths = walkdir::WalkDir::new(root)
        .follow_links(false)
        .into_iter()
        .collect::<std::result::Result<Vec<_>, _>>()?;
    paths.sort_by_key(|entry| entry.path().to_path_buf());
    let mut digest = Sha256::new();
    digest.update(b"rumoca-rust-toolchain-tree-v1\0");
    let mut total = 0_u64;
    for entry in paths.into_iter().skip(1) {
        let relative = entry.path().strip_prefix(root)?;
        update(&mut digest, relative.to_string_lossy().as_bytes());
        let metadata = fs::symlink_metadata(entry.path())?;
        if metadata.is_dir() {
            digest.update(b"dir\0");
        } else if metadata.is_file() {
            digest.update(b"file\0");
            total = total
                .checked_add(metadata.len())
                .context("toolchain size overflow")?;
            ensure!(
                total <= MAX_TOOLCHAIN_BYTES,
                "Rust toolchain tree exceeds byte bound"
            );
            let bytes = read_bounded(entry.path(), MAX_TOOLCHAIN_BYTES)?;
            update(&mut digest, &bytes);
        } else if metadata.file_type().is_symlink() {
            digest.update(b"symlink\0");
            update(
                &mut digest,
                fs::read_link(entry.path())?.to_string_lossy().as_bytes(),
            );
        } else {
            ensure!(false, "Rust toolchain tree contains a special file");
        }
    }
    Ok(format!("{:x}", digest.finalize()))
}

impl DependencySnapshot {
    pub(super) fn root(&self) -> &Path {
        &self.root
    }

    pub(super) fn commands(&self) -> &[process::CommandReceipt] {
        &self.commands
    }

    pub(super) fn verify_after_build(&self) -> Result<()> {
        self.captured
            .verify(&self.root, &self.evidence.closure_sha256)
    }

    pub(super) fn into_evidence(self) -> DependencyEvidence {
        self.evidence
    }
}

fn select_closure(
    metadata: &Metadata,
    source_root: &Path,
    lock: &BTreeMap<(String, String, String), String>,
) -> Result<SelectedClosure> {
    let (visited, build_dependency_edges) = reachable_package_ids(metadata, source_root)?;
    let packages = metadata
        .packages
        .iter()
        .map(|package| (package.id.as_str(), package))
        .collect::<BTreeMap<_, _>>();
    let mut registry = Vec::new();
    let mut local_packages = 0_usize;
    let mut proc_macro_packages = 0_usize;
    // The metadata command deliberately resolves every target platform, so
    // this root-reachable graph includes target-inactive manifests Cargo still
    // consults during a frozen host build without pulling unrelated workspace
    // members into the compiler dependency closure.
    for id in visited {
        let package = packages
            .get(id)
            .context("Cargo metadata package for resolve node is missing")?;
        if package
            .targets
            .iter()
            .any(|target| target.kind.iter().any(|kind| kind == "proc-macro"))
        {
            proc_macro_packages += 1;
        }
        let Some(source) = &package.source else {
            ensure!(
                package.manifest_path.starts_with(source_root),
                "path dependency escaped frozen compiler source: {}",
                package.manifest_path.display()
            );
            local_packages += 1;
            continue;
        };
        registry.push(select_registry_package(package, source, lock)?);
    }
    sort_and_validate_registry(&mut registry)?;
    ensure!(
        build_dependency_edges > 0,
        "compiler closure has no host build dependencies"
    );
    ensure!(
        proc_macro_packages > 0,
        "compiler closure has no proc-macro packages"
    );
    Ok(SelectedClosure {
        registry,
        local_packages,
        build_dependency_edges,
        proc_macro_packages,
    })
}

fn select_resolution_catalog(
    metadata: &Metadata,
    source_root: &Path,
    lock: &BTreeMap<(String, String, String), String>,
) -> Result<ResolutionCatalog> {
    let mut registry = lock
        .iter()
        .map(|((name, version, source), checksum)| {
            (
                (name.clone(), version.clone(), source.clone()),
                SelectedRegistryPackage {
                    name: name.clone(),
                    version: version.clone(),
                    source: source.clone(),
                    checksum: checksum.clone(),
                    resolution_files: BTreeSet::from([PathBuf::from("Cargo.toml")]),
                },
            )
        })
        .collect::<BTreeMap<_, _>>();
    for package in &metadata.packages {
        let Some(source) = &package.source else {
            ensure!(
                package.manifest_path.starts_with(source_root),
                "path dependency escaped frozen compiler source: {}",
                package.manifest_path.display()
            );
            continue;
        };
        let selected = select_registry_package(package, source, lock)?;
        let key = (
            selected.name.clone(),
            selected.version.clone(),
            selected.source.clone(),
        );
        ensure!(
            registry.insert(key, selected).is_some(),
            "Cargo metadata registry package escaped Cargo.lock"
        );
    }
    let mut registry = registry.into_values().collect::<Vec<_>>();
    sort_and_validate_registry(&mut registry)?;
    Ok(ResolutionCatalog { registry })
}

fn sort_and_validate_registry(registry: &mut [SelectedRegistryPackage]) -> Result<()> {
    registry.sort_by(|left, right| {
        (&left.name, &left.version, &left.source).cmp(&(&right.name, &right.version, &right.source))
    });
    let mut vendor_directories = BTreeSet::new();
    for package in registry {
        ensure!(
            vendor_directories.insert(format!("{}-{}", package.name, package.version)),
            "registry sources collide at one Cargo vendor directory for {} {}",
            package.name,
            package.version
        );
    }
    Ok(())
}

fn select_registry_package(
    package: &MetadataPackage,
    source: &str,
    lock: &BTreeMap<(String, String, String), String>,
) -> Result<SelectedRegistryPackage> {
    ensure!(
        source == "registry+https://github.com/rust-lang/crates.io-index",
        "compiler closure contains unsupported registry dependency {} {source}",
        package.name
    );
    let key = (
        package.name.clone(),
        package.version.clone(),
        source.to_owned(),
    );
    let checksum = lock
        .get(&key)
        .with_context(|| format!("Cargo.lock lacks exact checksum for {}", package.name))?;
    let package_root = package
        .manifest_path
        .parent()
        .context("registry package manifest has no parent")?;
    let mut resolution_files = BTreeSet::from([PathBuf::from("Cargo.toml")]);
    for target in &package.targets {
        let relative = target
            .src_path
            .strip_prefix(package_root)
            .with_context(|| {
                format!(
                    "registry target escaped its package: {}",
                    target.src_path.display()
                )
            })?;
        validate_relative(relative)?;
        ensure!(
            relative != Path::new("Cargo.toml") && relative != Path::new(".cargo-checksum.json"),
            "registry target collides with dependency control file"
        );
        resolution_files.insert(relative.to_path_buf());
    }
    Ok(SelectedRegistryPackage {
        name: package.name.clone(),
        version: package.version.clone(),
        source: source.to_owned(),
        checksum: checksum.clone(),
        resolution_files,
    })
}

fn reachable_package_ids<'a>(
    metadata: &'a Metadata,
    source_root: &Path,
) -> Result<(BTreeSet<&'a str>, usize)> {
    let nodes = metadata
        .resolve
        .nodes
        .iter()
        .map(|node| (node.id.as_str(), node))
        .collect::<BTreeMap<_, _>>();
    let expected_manifest = source_root.join("crates/rumoca/Cargo.toml");
    let roots = metadata
        .packages
        .iter()
        .filter(|package| package.name == "rumoca" && package.source.is_none())
        .filter(|package| package.manifest_path == expected_manifest)
        .map(|package| package.id.as_str())
        .collect::<Vec<_>>();
    ensure!(
        roots.len() == 1,
        "Cargo metadata selected no unique Rumoca compiler root"
    );
    let mut pending = vec![roots[0]];
    let mut visited = BTreeSet::new();
    let mut build_dependency_edges = 0_usize;
    while let Some(id) = pending.pop() {
        if !visited.insert(id) {
            continue;
        }
        let node = nodes
            .get(id)
            .context("Cargo metadata resolve node is missing")?;
        for dependency in &node.deps {
            if dependency
                .dep_kinds
                .iter()
                .any(|kind| kind.kind.as_deref() == Some("build"))
            {
                build_dependency_edges += 1;
            }
            pending.push(dependency.pkg.as_str());
        }
    }
    Ok((visited, build_dependency_edges))
}

fn lock_checksums(path: &Path) -> Result<BTreeMap<(String, String, String), String>> {
    let text = fs::read_to_string(path)?;
    let value: toml::Value = toml::from_str(&text)?;
    let packages = value
        .get("package")
        .and_then(toml::Value::as_array)
        .context("Cargo.lock has no package array")?;
    let mut checksums = BTreeMap::new();
    for package in packages {
        let Some(source) = package.get("source").and_then(toml::Value::as_str) else {
            continue;
        };
        ensure!(
            source == "registry+https://github.com/rust-lang/crates.io-index",
            "Cargo.lock contains unsupported registry source {source}"
        );
        let name = package
            .get("name")
            .and_then(toml::Value::as_str)
            .context("Cargo.lock package lacks name")?;
        let version = package
            .get("version")
            .and_then(toml::Value::as_str)
            .context("Cargo.lock package lacks version")?;
        let checksum = package
            .get("checksum")
            .and_then(toml::Value::as_str)
            .context("registry Cargo.lock package lacks checksum")?;
        ensure!(is_sha256(checksum), "Cargo.lock checksum is malformed");
        ensure!(
            checksums
                .insert(
                    (name.to_string(), version.to_string(), source.to_string()),
                    checksum.to_string(),
                )
                .is_none(),
            "Cargo.lock package identity is duplicated"
        );
    }
    Ok(checksums)
}

fn capture_registry_packages(
    selected: &[SelectedRegistryPackage],
    host_selected: &[SelectedRegistryPackage],
    cargo_home: &Path,
) -> Result<CapturedVendor> {
    let host_selected = host_selected.iter().cloned().collect::<BTreeSet<_>>();
    ensure!(
        host_selected
            .iter()
            .all(|package| selected.contains(package)),
        "host compiler dependency closure escaped all-target resolution closure"
    );
    let mut packages = Vec::with_capacity(selected.len());
    let mut total = 0_u64;
    for package in selected {
        let archive = find_archive(cargo_home, &package.name, &package.version)?;
        let bytes = read_bounded(&archive, MAX_ARCHIVE_BYTES)?;
        let observed = format!("{:x}", Sha256::digest(&bytes));
        ensure!(
            observed == package.checksum,
            "registry archive checksum differs for {} {}",
            package.name,
            package.version
        );
        let directory = format!("{}-{}", package.name, package.version);
        let materialization = materialization_for(package, &host_selected);
        let files = extract_archive(
            &bytes,
            &directory,
            materialization,
            &package.resolution_files,
        )?;
        total = total
            .checked_add(
                files
                    .iter()
                    .map(|file| file.bytes.len() as u64)
                    .sum::<u64>(),
            )
            .context("compiler vendor size overflow")?;
        ensure!(
            total <= MAX_VENDOR_BYTES,
            "compiler vendor exceeds {MAX_VENDOR_BYTES} bytes"
        );
        packages.push(CapturedPackage {
            directory,
            archive_sha256: observed,
            materialization,
            files,
        });
    }
    Ok(CapturedVendor { packages })
}

fn materialization_for(
    package: &SelectedRegistryPackage,
    host_selected: &BTreeSet<SelectedRegistryPackage>,
) -> PackageMaterialization {
    if host_selected.contains(package) {
        PackageMaterialization::Full
    } else {
        PackageMaterialization::ResolutionManifest
    }
}

fn find_archive(cargo_home: &Path, name: &str, version: &str) -> Result<PathBuf> {
    let cache = cargo_home.join("registry/cache");
    let file_name = format!("{name}-{version}.crate");
    let mut matches = Vec::new();
    for registry in fs::read_dir(&cache)
        .with_context(|| format!("failed to read Cargo registry cache {}", cache.display()))?
    {
        let candidate = registry?.path().join(&file_name);
        if candidate.is_file() {
            matches.push(candidate);
        }
    }
    ensure!(
        matches.len() == 1,
        "expected one cached registry archive {file_name}, found {matches:?}"
    );
    Ok(matches.remove(0))
}

fn extract_archive(
    bytes: &[u8],
    directory: &str,
    materialization: PackageMaterialization,
    resolution_files: &BTreeSet<PathBuf>,
) -> Result<Vec<CapturedFile>> {
    let decoder = GzDecoder::new(Cursor::new(bytes));
    let mut archive = tar::Archive::new(decoder);
    let mut files = Vec::new();
    let mut seen = BTreeSet::new();
    let mut expanded_bytes = 0_u64;
    for entry in archive.entries()? {
        let mut entry = entry?;
        let path = entry.path()?.into_owned();
        let relative = path.strip_prefix(directory).with_context(|| {
            format!(
                "crate archive entry escaped {directory}: {}",
                path.display()
            )
        })?;
        if relative.as_os_str().is_empty() || entry.header().entry_type().is_dir() {
            continue;
        }
        validate_relative(relative)?;
        ensure!(
            entry.header().entry_type().is_file(),
            "crate archive contains non-file entry"
        );
        let size = entry.size();
        ensure!(
            size <= MAX_ARCHIVE_BYTES,
            "crate archive member exceeds bound"
        );
        expanded_bytes = expanded_bytes
            .checked_add(size)
            .context("expanded crate archive size overflow")?;
        ensure!(
            expanded_bytes <= MAX_EXPANDED_PACKAGE_BYTES,
            "expanded crate archive exceeds package bound"
        );
        let resolution_source = relative
            .extension()
            .is_some_and(|extension| extension == "rs");
        if matches!(materialization, PackageMaterialization::ResolutionManifest)
            && !resolution_files.contains(relative)
            && !resolution_source
        {
            continue;
        }
        ensure!(
            seen.insert(relative.to_path_buf()),
            "crate archive path is duplicated"
        );
        let mut file_bytes = Vec::with_capacity(size as usize);
        Read::by_ref(&mut entry)
            .take(MAX_ARCHIVE_BYTES + 1)
            .read_to_end(&mut file_bytes)?;
        ensure!(
            file_bytes.len() as u64 == size,
            "crate archive member changed size"
        );
        let source_sha256 = Some(format!("{:x}", Sha256::digest(&file_bytes)));
        let retained_bytes = retained_bytes(materialization, relative, file_bytes);
        files.push(CapturedFile {
            relative: relative.to_path_buf(),
            mode: entry.header().mode()? & 0o777,
            source_sha256,
            bytes: retained_bytes,
        });
    }
    if matches!(materialization, PackageMaterialization::ResolutionManifest) {
        let observed = files
            .iter()
            .map(|file| file.relative.clone())
            .collect::<BTreeSet<_>>();
        ensure!(
            observed.contains(Path::new("Cargo.toml")),
            "resolution package {directory} lacks its authenticated manifest"
        );
        for relative in resolution_files.difference(&observed) {
            files.push(CapturedFile {
                relative: relative.clone(),
                mode: 0o644,
                source_sha256: None,
                bytes: RESOLUTION_TARGET_SENTINEL.to_vec(),
            });
        }
    }
    files.sort_by(|left, right| left.relative.cmp(&right.relative));
    ensure!(!files.is_empty(), "crate archive contains no files");
    Ok(files)
}

fn retained_bytes(
    materialization: PackageMaterialization,
    relative: &Path,
    source_bytes: Vec<u8>,
) -> Vec<u8> {
    if matches!(materialization, PackageMaterialization::ResolutionManifest)
        && relative != Path::new("Cargo.toml")
    {
        RESOLUTION_TARGET_SENTINEL.to_vec()
    } else {
        source_bytes
    }
}

impl CapturedVendor {
    fn sha256(&self) -> String {
        let mut digest = Sha256::new();
        digest.update(b"rumoca-compiler-registry-vendor-v1\0");
        for package in &self.packages {
            update(&mut digest, package.directory.as_bytes());
            update(&mut digest, package.archive_sha256.as_bytes());
            digest.update([match package.materialization {
                PackageMaterialization::Full => 0,
                PackageMaterialization::ResolutionManifest => 1,
            }]);
            for file in &package.files {
                update(&mut digest, file.relative.to_string_lossy().as_bytes());
                digest.update(file.mode.to_le_bytes());
                digest.update([u8::from(file.source_sha256.is_some())]);
                update(
                    &mut digest,
                    file.source_sha256.as_deref().unwrap_or_default().as_bytes(),
                );
                update(&mut digest, &file.bytes);
            }
        }
        format!("{:x}", digest.finalize())
    }

    fn publish(&self, destination: &Path) -> Result<()> {
        fs::create_dir_all(destination)?;
        for package in &self.packages {
            let root = destination.join(&package.directory);
            fs::create_dir_all(&root)?;
            for file in &package.files {
                let output = root.join(&file.relative);
                fs::create_dir_all(output.parent().context("vendored file has no parent")?)?;
                let mut handle = OpenOptions::new()
                    .write(true)
                    .create_new(true)
                    .open(&output)?;
                handle.write_all(&file.bytes)?;
                set_mode(&output, file.mode & !0o222)?;
            }
            let checksum_bytes = package.checksum_bytes()?;
            let checksum_path = root.join(".cargo-checksum.json");
            fs::write(&checksum_path, &checksum_bytes)?;
            set_mode(&checksum_path, 0o444)?;
        }
        freeze_directories(destination)?;
        Ok(())
    }

    fn verify(&self, destination: &Path, expected_sha256: &str) -> Result<()> {
        ensure!(
            self.sha256() == expected_sha256,
            "retained vendor authority changed"
        );
        let mut expected_files = BTreeSet::new();
        let mut expected_directories = BTreeSet::from([PathBuf::new()]);
        for package in &self.packages {
            let package_root = PathBuf::from(&package.directory);
            expected_directories.insert(package_root.clone());
            for file in &package.files {
                verify_captured_file(
                    destination,
                    &package_root,
                    file,
                    &mut expected_files,
                    &mut expected_directories,
                )?;
            }
            let checksum_relative = package_root.join(".cargo-checksum.json");
            expected_files.insert(checksum_relative.clone());
            let checksum_path = destination.join(checksum_relative);
            ensure!(
                read_bounded(&checksum_path, MAX_ARCHIVE_BYTES)? == package.checksum_bytes()?,
                "vendored Cargo checksum manifest changed"
            );
            ensure_mode(&checksum_path, 0o444)?;
        }
        let mut observed_files = BTreeSet::new();
        let mut observed_directories = BTreeSet::new();
        collect_roster(
            destination,
            destination,
            &mut observed_files,
            &mut observed_directories,
        )?;
        ensure!(
            observed_files == expected_files && observed_directories == expected_directories,
            "vendor roster changed"
        );
        ensure_tree_read_only(destination)
    }
}

fn verify_captured_file(
    destination: &Path,
    package_root: &Path,
    file: &CapturedFile,
    expected_files: &mut BTreeSet<PathBuf>,
    expected_directories: &mut BTreeSet<PathBuf>,
) -> Result<()> {
    let relative = package_root.join(&file.relative);
    expected_files.insert(relative.clone());
    expected_directories.extend(relative.ancestors().skip(1).map(Path::to_path_buf));
    let path = destination.join(&relative);
    let observed = read_bounded(&path, MAX_ARCHIVE_BYTES)?;
    ensure!(observed == file.bytes, "vendored package file changed");
    ensure_mode(&path, file.mode & !0o222)
}

impl CapturedPackage {
    fn checksum_bytes(&self) -> Result<Vec<u8>> {
        let checksums = self
            .files
            .iter()
            .map(|file| {
                (
                    file.relative.to_string_lossy().replace('\\', "/"),
                    format!("{:x}", Sha256::digest(&file.bytes)),
                )
            })
            .collect::<BTreeMap<_, _>>();
        Ok(serde_json::to_vec(&serde_json::json!({
            "files": checksums,
            "package": self.archive_sha256,
        }))?)
    }
}

fn publish_manifest(
    artifact_root: &Path,
    closure_sha256: &str,
    captured: &CapturedVendor,
) -> Result<FileEvidence> {
    let relative = "evidence-bundle/setup/compiler-dependencies.json";
    let path = artifact_root.join(relative);
    fs::create_dir_all(path.parent().context("dependency manifest has no parent")?)?;
    let bytes = serde_json::to_vec_pretty(&DependencyManifest {
        closure_sha256,
        packages: captured
            .packages
            .iter()
            .map(|package| DependencyManifestPackage {
                directory: &package.directory,
                archive_sha256: &package.archive_sha256,
                materialization: package.materialization,
                files: package.files.len(),
            })
            .collect(),
    })?;
    fs::write(&path, &bytes)?;
    set_mode(&path, 0o444)?;
    Ok(FileEvidence {
        relative_path: relative.to_string(),
        sha256: format!("{:x}", Sha256::digest(&bytes)),
        bytes: bytes.len() as u64,
    })
}

fn read_bounded(path: &Path, limit: u64) -> Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let metadata = file.metadata()?;
    ensure!(metadata.is_file() && metadata.len() <= limit);
    let mut bytes = Vec::with_capacity(metadata.len() as usize);
    Read::by_ref(&mut file)
        .take(limit + 1)
        .read_to_end(&mut bytes)?;
    ensure!(bytes.len() as u64 == metadata.len());
    Ok(bytes)
}

fn validate_relative(path: &Path) -> Result<()> {
    ensure!(
        !path.as_os_str().is_empty()
            && path
                .components()
                .all(|component| matches!(component, Component::Normal(_))),
        "crate path is not normalized relative: {}",
        path.display()
    );
    Ok(())
}

fn update(digest: &mut Sha256, bytes: &[u8]) {
    digest.update((bytes.len() as u64).to_le_bytes());
    digest.update(bytes);
}

fn is_sha256(value: &str) -> bool {
    value.len() == 64
        && value
            .bytes()
            .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase())
}

fn collect_roster(
    root: &Path,
    directory: &Path,
    files: &mut BTreeSet<PathBuf>,
    directories: &mut BTreeSet<PathBuf>,
) -> Result<()> {
    directories.insert(directory.strip_prefix(root)?.to_path_buf());
    for entry in fs::read_dir(directory)? {
        let path = entry?.path();
        let metadata = fs::symlink_metadata(&path)?;
        if metadata.is_dir() {
            collect_roster(root, &path, files, directories)?;
        } else {
            ensure!(metadata.is_file(), "vendor contains a special file");
            files.insert(path.strip_prefix(root)?.to_path_buf());
        }
    }
    Ok(())
}

fn ensure_mode(path: &Path, expected: u32) -> Result<()> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        ensure!(
            fs::symlink_metadata(path)?.permissions().mode() & 0o777 == expected,
            "vendored file mode changed: {}",
            path.display()
        );
    }
    #[cfg(not(unix))]
    let _ = (path, expected);
    Ok(())
}

fn freeze_directories(path: &Path) -> Result<()> {
    for entry in fs::read_dir(path)? {
        let path = entry?.path();
        if fs::symlink_metadata(&path)?.is_dir() {
            freeze_directories(&path)?;
        }
    }
    set_mode(path, 0o555)
}

fn ensure_tree_read_only(path: &Path) -> Result<()> {
    let metadata = fs::symlink_metadata(path)?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        ensure!(
            metadata.permissions().mode() & 0o222 == 0,
            "vendor became writable"
        );
    }
    if metadata.is_dir() {
        for entry in fs::read_dir(path)? {
            ensure_tree_read_only(&entry?.path())?;
        }
    }
    Ok(())
}

fn set_mode(path: &Path, mode: u32) -> Result<()> {
    let mut permissions = fs::metadata(path)?.permissions();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        permissions.set_mode(mode);
    }
    #[cfg(not(unix))]
    permissions.set_readonly(mode & 0o222 == 0);
    fs::set_permissions(path, permissions)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        CapturedFile, CapturedPackage, CapturedVendor, DependencyKind, Metadata,
        MetadataDependency, MetadataNode, MetadataPackage, MetadataResolve, MetadataTarget,
        PackageMaterialization, RESOLUTION_TARGET_SENTINEL, SelectedRegistryPackage,
        extract_archive, freeze_directories, materialization_for, retained_bytes, select_closure,
        select_resolution_catalog, set_mode,
    };
    use std::collections::{BTreeMap, BTreeSet};
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::process::{Command, Output};

    #[test]
    fn graph_selection_includes_build_and_proc_macro_dependencies() {
        let root = Path::new("/frozen");
        let metadata = Metadata {
            packages: vec![
                package(
                    "root",
                    "rumoca",
                    None,
                    "/frozen/crates/rumoca/Cargo.toml",
                    vec!["bin"],
                ),
                package(
                    "build",
                    "build-helper",
                    Some("registry+https://github.com/rust-lang/crates.io-index"),
                    "/cache/build/Cargo.toml",
                    vec!["lib"],
                ),
                package(
                    "macro",
                    "macro-helper",
                    Some("registry+https://github.com/rust-lang/crates.io-index"),
                    "/cache/macro/Cargo.toml",
                    vec!["proc-macro"],
                ),
                package(
                    "optional",
                    "locked-optional",
                    Some("registry+https://github.com/rust-lang/crates.io-index"),
                    "/cache/optional/Cargo.toml",
                    vec!["lib"],
                ),
            ],
            resolve: MetadataResolve {
                nodes: vec![
                    node(
                        "root",
                        vec![
                            ("build", Some("build")),
                            ("macro", None),
                            ("optional", None),
                        ],
                    ),
                    node("build", vec![]),
                    node("macro", vec![]),
                    node("optional", vec![]),
                ],
            },
        };
        let lock = BTreeMap::from([
            (
                (
                    "build-helper".into(),
                    "1.0.0".into(),
                    "registry+https://github.com/rust-lang/crates.io-index".into(),
                ),
                "0".repeat(64),
            ),
            (
                (
                    "macro-helper".into(),
                    "1.0.0".into(),
                    "registry+https://github.com/rust-lang/crates.io-index".into(),
                ),
                "1".repeat(64),
            ),
            (
                (
                    "locked-optional".into(),
                    "1.0.0".into(),
                    "registry+https://github.com/rust-lang/crates.io-index".into(),
                ),
                "2".repeat(64),
            ),
            (
                (
                    "locked-unused".into(),
                    "1.0.0".into(),
                    "registry+https://github.com/rust-lang/crates.io-index".into(),
                ),
                "3".repeat(64),
            ),
        ]);
        let selected = select_closure(&metadata, root, &lock).unwrap();
        assert_eq!(selected.registry.len(), 3);
        assert_eq!(selected.build_dependency_edges, 1);
        assert_eq!(selected.proc_macro_packages, 1);
        assert_resolution_catalog_includes_unused(&metadata, root, &lock);
    }

    fn assert_resolution_catalog_includes_unused(
        metadata: &Metadata,
        root: &Path,
        lock: &BTreeMap<(String, String, String), String>,
    ) {
        let catalog = select_resolution_catalog(metadata, root, lock).unwrap();
        assert_eq!(catalog.registry.len(), 4);
        assert!(
            catalog
                .registry
                .iter()
                .any(|package| package.name == "locked-unused")
        );
    }

    #[test]
    fn graph_selection_rejects_omitted_host_build_or_proc_macro_closure() {
        let root = Path::new("/frozen");
        for (dependency_id, dependency_kind, target_kind) in [
            ("macro", None, "proc-macro"),
            ("build", Some("build"), "lib"),
        ] {
            let metadata = Metadata {
                packages: vec![
                    package(
                        "root",
                        "rumoca",
                        None,
                        "/frozen/crates/rumoca/Cargo.toml",
                        vec!["bin"],
                    ),
                    package(
                        dependency_id,
                        "dependency",
                        Some("registry+https://github.com/rust-lang/crates.io-index"),
                        "/cache/dependency/Cargo.toml",
                        vec![target_kind],
                    ),
                ],
                resolve: MetadataResolve {
                    nodes: vec![
                        node("root", vec![(dependency_id, dependency_kind)]),
                        node(dependency_id, vec![]),
                    ],
                },
            };
            let lock = BTreeMap::from([(
                (
                    "dependency".into(),
                    "1.0.0".into(),
                    "registry+https://github.com/rust-lang/crates.io-index".into(),
                ),
                "0".repeat(64),
            )]);
            let error = select_closure(&metadata, root, &lock)
                .err()
                .expect("incomplete host closure must fail");
            let message = error.to_string();
            if dependency_kind.is_none() {
                assert!(message.contains("host build dependencies"));
            } else {
                assert!(message.contains("proc-macro packages"));
            }
        }
    }

    #[test]
    fn target_inactive_packages_retain_only_resolution_manifests() {
        let active = registry_package("active");
        let inactive = registry_package("inactive");
        let host = BTreeSet::from([active.clone()]);
        assert!(matches!(
            materialization_for(&active, &host),
            PackageMaterialization::Full
        ));
        assert!(matches!(
            materialization_for(&inactive, &host),
            PackageMaterialization::ResolutionManifest
        ));
    }

    #[test]
    fn resolution_target_entries_are_non_buildable_sentinels() {
        assert_eq!(
            retained_bytes(
                PackageMaterialization::ResolutionManifest,
                Path::new("Cargo.toml"),
                b"[package]".to_vec(),
            ),
            b"[package]"
        );
        assert_eq!(
            retained_bytes(
                PackageMaterialization::ResolutionManifest,
                Path::new("build.rs"),
                b"fn main() { panic!() }".to_vec(),
            ),
            RESOLUTION_TARGET_SENTINEL
        );
    }

    #[test]
    fn absent_published_target_entry_is_synthesized_as_a_sentinel() {
        use flate2::Compression;
        use flate2::write::GzEncoder;
        use std::io::Cursor;

        let encoder = GzEncoder::new(Vec::new(), Compression::default());
        let mut archive = tar::Builder::new(encoder);
        let manifest = b"[package]\nname = \"demo\"\nversion = \"1.0.0\"\n";
        let mut header = tar::Header::new_gnu();
        header.set_size(manifest.len() as u64);
        header.set_mode(0o644);
        header.set_cksum();
        archive
            .append_data(&mut header, "demo-1.0.0/Cargo.toml", Cursor::new(manifest))
            .unwrap();
        let archive = archive.into_inner().unwrap().finish().unwrap();
        let required = BTreeSet::from([
            PathBuf::from("Cargo.toml"),
            PathBuf::from("benches/omitted.rs"),
        ]);
        let captured = extract_archive(
            &archive,
            "demo-1.0.0",
            PackageMaterialization::ResolutionManifest,
            &required,
        )
        .unwrap();
        let synthesized = captured
            .iter()
            .find(|file| file.relative == Path::new("benches/omitted.rs"))
            .unwrap();
        assert_eq!(synthesized.bytes, RESOLUTION_TARGET_SENTINEL);
        assert!(synthesized.source_sha256.is_none());
    }

    #[test]
    fn cargo_resolves_inactive_skeleton_and_rejects_active_sentinels() {
        let inactive = run_sentinel_probe(SentinelProbe::Library, false);
        assert!(
            inactive.status.success(),
            "inactive resolution skeleton failed: {}",
            String::from_utf8_lossy(&inactive.stderr)
        );
        for kind in [
            SentinelProbe::Library,
            SentinelProbe::BuildScript,
            SentinelProbe::ProcMacro,
        ] {
            let active = run_sentinel_probe(kind, true);
            assert!(
                !active.status.success(),
                "active sentinel unexpectedly built"
            );
            assert!(
                String::from_utf8_lossy(&active.stderr)
                    .contains("resolution-only dependency target cannot be compiled")
            );
        }
    }

    #[test]
    fn registry_target_cannot_collide_with_control_files() {
        let root = Path::new("/frozen");
        for collision in ["Cargo.toml", ".cargo-checksum.json"] {
            let mut dependency = package(
                "dependency",
                "dependency",
                Some("registry+https://github.com/rust-lang/crates.io-index"),
                "/cache/dependency/Cargo.toml",
                vec!["lib"],
            );
            dependency.targets[0].src_path = Path::new("/cache/dependency").join(collision);
            let metadata = Metadata {
                packages: vec![
                    package(
                        "root",
                        "rumoca",
                        None,
                        "/frozen/crates/rumoca/Cargo.toml",
                        vec!["bin"],
                    ),
                    dependency,
                ],
                resolve: MetadataResolve {
                    nodes: vec![
                        node("root", vec![("dependency", Some("build"))]),
                        node("dependency", vec![]),
                    ],
                },
            };
            let lock = BTreeMap::from([(
                (
                    "dependency".into(),
                    "1.0.0".into(),
                    "registry+https://github.com/rust-lang/crates.io-index".into(),
                ),
                "0".repeat(64),
            )]);
            assert!(select_closure(&metadata, root, &lock).is_err());
        }
    }

    #[cfg(unix)]
    #[test]
    fn frozen_vendor_rejects_missing_extra_and_modified_package_files() {
        let captured = CapturedVendor {
            packages: vec![CapturedPackage {
                directory: "demo-1.0.0".into(),
                archive_sha256: "2".repeat(64),
                materialization: PackageMaterialization::Full,
                files: vec![CapturedFile {
                    relative: PathBuf::from("src/lib.rs"),
                    mode: 0o644,
                    source_sha256: Some("0".repeat(64)),
                    bytes: b"source".to_vec(),
                }],
            }],
        };
        for mutation in [
            "missing",
            "extra",
            "empty-directory",
            "modified",
            "checksum",
            "mode",
        ] {
            let temporary = tempfile::tempdir().unwrap();
            let vendor = temporary.path().join("vendor");
            captured.publish(&vendor).unwrap();
            make_directories_writable(&vendor);
            let source = vendor.join("demo-1.0.0/src/lib.rs");
            let checksum = vendor.join("demo-1.0.0/.cargo-checksum.json");
            match mutation {
                "missing" => fs::remove_file(&source).unwrap(),
                "extra" => {
                    let extra = vendor.join("demo-1.0.0/extra");
                    fs::write(&extra, "extra").unwrap();
                    set_mode(&extra, 0o444).unwrap();
                }
                "empty-directory" => {
                    fs::create_dir(vendor.join("demo-1.0.0/extra-directory")).unwrap();
                }
                "modified" => {
                    set_mode(&source, 0o644).unwrap();
                    fs::write(&source, "changed").unwrap();
                    set_mode(&source, 0o444).unwrap();
                }
                "checksum" => {
                    set_mode(&checksum, 0o644).unwrap();
                    fs::write(&checksum, "{}").unwrap();
                    set_mode(&checksum, 0o444).unwrap();
                }
                "mode" => set_mode(&source, 0o400).unwrap(),
                _ => unreachable!(),
            }
            freeze_directories(&vendor).unwrap();
            assert!(captured.verify(&vendor, &captured.sha256()).is_err());
        }
    }

    fn package(
        id: &str,
        name: &str,
        source: Option<&str>,
        manifest: &str,
        kinds: Vec<&str>,
    ) -> MetadataPackage {
        MetadataPackage {
            name: name.into(),
            version: "1.0.0".into(),
            id: id.into(),
            source: source.map(str::to_string),
            manifest_path: manifest.into(),
            targets: vec![MetadataTarget {
                kind: kinds.into_iter().map(str::to_string).collect(),
                src_path: Path::new(manifest).parent().unwrap().join("src/lib.rs"),
            }],
        }
    }

    fn node(id: &str, deps: Vec<(&str, Option<&str>)>) -> MetadataNode {
        MetadataNode {
            id: id.into(),
            deps: deps
                .into_iter()
                .map(|(pkg, kind)| MetadataDependency {
                    pkg: pkg.into(),
                    dep_kinds: vec![DependencyKind {
                        kind: kind.map(str::to_string),
                    }],
                })
                .collect(),
        }
    }

    fn registry_package(name: &str) -> SelectedRegistryPackage {
        SelectedRegistryPackage {
            name: name.into(),
            version: "1.0.0".into(),
            source: "registry+https://github.com/rust-lang/crates.io-index".into(),
            checksum: "0".repeat(64),
            resolution_files: BTreeSet::from([
                PathBuf::from("Cargo.toml"),
                PathBuf::from("src/lib.rs"),
            ]),
        }
    }

    #[derive(Clone, Copy, Debug)]
    enum SentinelProbe {
        Library,
        BuildScript,
        ProcMacro,
    }

    fn run_sentinel_probe(kind: SentinelProbe, active: bool) -> Output {
        let temporary = tempfile::tempdir().unwrap();
        let root = temporary.path();
        fs::create_dir_all(root.join("src")).unwrap();
        fs::write(root.join("src/main.rs"), "fn main() {}\n").unwrap();
        let target_condition = if active {
            "cfg(target_os = \"linux\")"
        } else {
            "cfg(target_os = \"windows\")"
        };
        fs::write(
            root.join("Cargo.toml"),
            format!(
                "[package]\nname = \"root-probe\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[target.'{target_condition}'.dependencies]\nprobe = \"1.0.0\"\n"
            ),
        )
        .unwrap();
        write_probe_lock(root);
        let vendor = root.join("vendor/probe-1.0.0");
        fs::create_dir_all(vendor.join("src")).unwrap();
        let (manifest_tail, files) = sentinel_probe_files(kind);
        fs::write(
            vendor.join("Cargo.toml"),
            format!(
                "[package]\nname = \"probe\"\nversion = \"1.0.0\"\nedition = \"2024\"\n{manifest_tail}"
            ),
        )
        .unwrap();
        for (relative, bytes) in files {
            let path = vendor.join(relative);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, bytes).unwrap();
        }
        write_probe_checksum(&vendor);
        let cargo = std::env::var_os("CARGO").expect("Cargo test runner did not provide CARGO");
        let output = Command::new(cargo)
            .current_dir(root)
            .args(["build", "--frozen", "--offline"])
            .args(["--target", super::process::COMPILER_HOST_TARGET])
            .args([
                "--config",
                "source.crates-io.replace-with=\"vendored-sources\"",
            ])
            .arg("--config")
            .arg(format!(
                "source.vendored-sources.directory=\"{}\"",
                root.join("vendor").display()
            ))
            .output()
            .unwrap();
        if !output.status.success() {
            assert!(
                !root
                    .join("target")
                    .join(super::process::COMPILER_HOST_TARGET)
                    .join("debug/root-probe")
                    .exists(),
                "failed sentinel build exposed a compiler capability"
            );
        }
        output
    }

    fn sentinel_probe_files(
        kind: SentinelProbe,
    ) -> (&'static str, Vec<(&'static str, &'static [u8])>) {
        match kind {
            SentinelProbe::Library => ("", vec![("src/lib.rs", RESOLUTION_TARGET_SENTINEL)]),
            SentinelProbe::BuildScript => (
                "build = \"build.rs\"\n",
                vec![
                    ("src/lib.rs", b"pub fn harmless() {}\n"),
                    ("build.rs", RESOLUTION_TARGET_SENTINEL),
                ],
            ),
            SentinelProbe::ProcMacro => (
                "\n[lib]\nproc-macro = true\n",
                vec![("src/lib.rs", RESOLUTION_TARGET_SENTINEL)],
            ),
        }
    }

    fn write_probe_lock(root: &Path) {
        fs::write(
            root.join("Cargo.lock"),
            format!(
                "version = 4\n\n[[package]]\nname = \"probe\"\nversion = \"1.0.0\"\nsource = \"registry+https://github.com/rust-lang/crates.io-index\"\nchecksum = \"{}\"\n\n[[package]]\nname = \"root-probe\"\nversion = \"0.1.0\"\ndependencies = [\"probe\"]\n",
                "0".repeat(64)
            ),
        )
        .unwrap();
    }

    fn write_probe_checksum(vendor: &Path) {
        use sha2::{Digest as _, Sha256};
        let mut files = BTreeMap::new();
        for relative in ["Cargo.toml", "src/lib.rs", "build.rs"] {
            let path = vendor.join(relative);
            if path.is_file() {
                files.insert(
                    relative,
                    format!("{:x}", Sha256::digest(fs::read(path).unwrap())),
                );
            }
        }
        fs::write(
            vendor.join(".cargo-checksum.json"),
            serde_json::to_vec(&serde_json::json!({
                "files": files,
                "package": "0".repeat(64),
            }))
            .unwrap(),
        )
        .unwrap();
    }

    #[cfg(unix)]
    fn make_directories_writable(path: &Path) {
        use std::os::unix::fs::PermissionsExt;
        let metadata = fs::symlink_metadata(path).unwrap();
        if metadata.is_dir() {
            let mut permissions = metadata.permissions();
            permissions.set_mode(0o755);
            fs::set_permissions(path, permissions).unwrap();
            for entry in fs::read_dir(path).unwrap() {
                make_directories_writable(&entry.unwrap().path());
            }
        }
    }
}
