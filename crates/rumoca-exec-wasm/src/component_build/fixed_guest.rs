//! One source-built PIC guest core; graph admission remains a separate owner.

mod cache;
mod contract;

use std::{
    path::{Path, PathBuf},
    process::Command,
};

include!(concat!(env!("OUT_DIR"), "/fixed_guest_sources.rs"));

/// Core emitted from this tool's captured fixed-source inventory.
/// No raw-byte, artifact-path or caller-inventory constructor exists.
/// This grants neither static execution nor final component publication.
pub struct FixedNativeGuestCore {
    bytes: Vec<u8>,
    contract: contract::FixedGuestContract,
    bindings: wit_component::metadata::Bindgen,
    messages: String,
    source_directory: PathBuf,
    target_directory: PathBuf,
}

impl FixedNativeGuestCore {
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Actual Cargo issuer output, retained with the source-built product.
    pub fn build_messages(&self) -> &str {
        &self.messages
    }

    /// Owned scratch paths for raw issuer/.d inspection, never constructors.
    pub fn build_directories(&self) -> (&Path, &Path) {
        (&self.source_directory, &self.target_directory)
    }

    pub fn source_files(&self) -> &'static [(&'static str, &'static [u8])] {
        FIXED_SOURCES
    }

    /// Actual imports of this source-built core, never a wildcard WASI allowance.
    pub fn imports(&self) -> impl Iterator<Item = (&str, &str)> {
        self.contract
            .imports
            .keys()
            .map(|(module, name)| (module.as_str(), name.as_str()))
    }

    /// Actual dylink static memory and table requirements; alignments are exponents.
    pub fn allocation(&self) -> (u32, u32, u32, u32) {
        let memory = &self.contract.allocation;
        (
            memory.memory_size,
            memory.memory_alignment,
            memory.table_size,
            memory.table_alignment,
        )
    }

    /// Validate against this build's original canonical FMI WIT and actual
    /// platform import inventory. Structural checking alone cannot issue a
    /// linked model or publication product.
    pub fn check_component_interface(&self, bytes: &[u8]) -> Result<(), String> {
        let directory = "crates/rumoca-phase-codegen/src/templates/fmi-ls-wasm/wit/";
        let mut sources = wit_parser::SourceMap::new();
        for (path, bytes) in FIXED_SOURCES {
            if path.starts_with(directory) && path.ends_with(".wit") {
                sources.push(
                    Path::new(path),
                    std::str::from_utf8(bytes).map_err(|error| error.to_string())?,
                );
            }
        }
        super::interface::ExpectedInterface::from_sources(
            sources,
            "co-simulation-fmu",
            Default::default(),
        )?
        .with_source_imports(&self.bindings)?
        .check(bytes)
    }
}

/// Build the fixed source once, without accepting any external source or artifact.
/// The two directories are owned scratch storage, not source authority.
pub fn build_fixed_native_guest_core(
    staging_parent: &Path,
    target_parent: &Path,
) -> Result<FixedNativeGuestCore, String> {
    build(staging_parent, target_parent, None)
}

/// As [`build_fixed_native_guest_core`], reusing the core a previous build
/// stored under `cache_root` for exactly the same staged bytes and compiler.
pub fn build_fixed_native_guest_core_cached(
    staging_parent: &Path,
    target_parent: &Path,
    cache_root: &Path,
) -> Result<FixedNativeGuestCore, String> {
    build(staging_parent, target_parent, Some(cache_root))
}

fn build(
    staging_parent: &Path,
    target_parent: &Path,
    cache_root: Option<&Path>,
) -> Result<FixedNativeGuestCore, String> {
    let staging = tempfile::Builder::new()
        .prefix("fixed-native-source-")
        .tempdir_in(staging_parent)
        .map_err(|error| error.to_string())?
        .keep();
    materialize(&staging)?;
    let _lease = super::cache_lease::acquire(target_parent)?;
    // Empty first-party output avoids the demonstrated cross-worktree stale
    // artifact reuse. Registry inputs may still use the caller's owned cache.
    let target = tempfile::Builder::new()
        .prefix("fixed-native-target-")
        .tempdir_in(target_parent)
        .map_err(|error| error.to_string())?
        .keep();
    let rustc = Path::new(ISSUING_RUSTC);
    if output(Command::new(rustc).arg("-vV"), "issuing compiler identity")? != ISSUING_VERSION {
        return Err("fixed guest compiler differs from the issuing source tool".into());
    }
    let flags = rustflags(rustc)?;
    let key = cache::key(
        FIXED_SOURCES,
        ISSUING_RUSTC,
        ISSUING_VERSION,
        &recipe(&flags),
    );
    let cache_lease = cache_root
        .map(|root| super::cache_lease::acquire(root).map(|lease| (root, lease)))
        .transpose()?;
    let stored = cache_lease
        .as_ref()
        .and_then(|(root, _)| cache::load(root, &key));
    let (bytes, messages) = match stored {
        Some(stored) => (stored.bytes, stored.messages),
        None => {
            let (bytes, messages) = cargo_build(rustc, &flags, &staging, &target)?;
            if let Some((root, _)) = &cache_lease {
                cache::store(root, &key, &bytes, &messages)?;
            }
            (bytes, messages)
        }
    };
    let contract = contract::check(&bytes)?;
    let (stripped, bindings) = wit_component::metadata::decode(&bytes)
        .map_err(|error| format!("decode fixed source interface metadata: {error:#}"))?;
    if stripped.is_none() {
        return Err("fixed guest has no source-issued interface metadata".into());
    }
    Ok(FixedNativeGuestCore {
        bytes,
        contract,
        bindings,
        messages,
        source_directory: staging,
        target_directory: target,
    })
}

/// Every build input besides the staged bytes and the compiler identity.
fn recipe(flags: &str) -> String {
    format!("{flags}\n{CARGO_ARGUMENTS:?}\nartifact=rumoca_runtime_fmi_ls")
}

const CARGO_ARGUMENTS: [&str; 9] = [
    "build",
    "--locked",
    "--offline",
    "--release",
    "-Zbuild-std=std,panic_abort",
    "--target",
    "wasm32-wasip2",
    "--message-format=json-render-diagnostics",
    "--manifest-path",
];

fn rustflags(rustc: &Path) -> Result<String, String> {
    let sysroot = output(
        Command::new(rustc).args(["--print", "sysroot"]),
        "compiler sysroot",
    )?;
    let host = ISSUING_VERSION
        .lines()
        .find_map(|line| line.strip_prefix("host: "))
        .ok_or("issuing compiler has no host triple")?;
    let linker = Path::new(sysroot.trim())
        .join("lib/rustlib")
        .join(host)
        .join("bin/rust-lld");
    if !linker.is_file() {
        return Err("issuing compiler has no core rust-lld".into());
    }
    Ok([
        "-Dwarnings".to_owned(),
        "-Crelocation-model=pic".into(),
        "-Cpanic=abort".into(),
        format!("-Clinker={}", linker.display()),
        "-Clink-arg=--pie".into(),
        "-Clink-arg=--experimental-pic".into(),
        "-Clink-arg=--import-memory=env,memory".into(),
        "-Clink-arg=--import-table".into(),
        "-Clink-arg=--fatal-warnings".into(),
        "-Clink-arg=--threads=4".into(),
    ]
    .join("\u{1f}"))
}

fn cargo_build(
    rustc: &Path,
    flags: &str,
    staging: &Path,
    target: &Path,
) -> Result<(Vec<u8>, String), String> {
    let manifest = staging.join("crates/rumoca-runtime-fmi-ls/Cargo.toml");
    let messages = output(
        Command::new("cargo")
            .current_dir(staging)
            .args(CARGO_ARGUMENTS)
            .arg(&manifest)
            .env("RUSTC", rustc)
            .env_remove("RUSTC_WRAPPER")
            .env_remove("RUSTC_WORKSPACE_WRAPPER")
            .env_remove("RUSTFLAGS")
            .env("CARGO_ENCODED_RUSTFLAGS", flags)
            .env("CARGO_TARGET_DIR", target)
            .env("CARGO_BUILD_JOBS", "4")
            .env("RUST_TEST_THREADS", "4")
            .env("RAYON_NUM_THREADS", "4")
            .env("CARGO_INCREMENTAL", "0")
            .env("CARGO_PROFILE_RELEASE_DEBUG", "0"),
        "fixed native guest build",
    )?;
    let artifact = target.join("wasm32-wasip2/release/rumoca_runtime_fmi_ls.wasm");
    super::cargo_artifact::require_exact_artifact(
        &messages,
        &manifest,
        "rumoca_runtime_fmi_ls",
        &artifact,
    )?;
    let bytes = std::fs::read(&artifact).map_err(|error| error.to_string())?;
    Ok((bytes, messages))
}

fn materialize(root: &Path) -> Result<(), String> {
    for (relative, bytes) in FIXED_SOURCES {
        let relative = Path::new(relative);
        rumoca_core::artifact_build::validate_artifact_relative_path(relative)?;
        let destination = root.join(relative);
        if let Some(parent) = destination.parent() {
            std::fs::create_dir_all(parent).map_err(|error| error.to_string())?;
        }
        std::fs::write(destination, bytes).map_err(|error| error.to_string())?;
    }
    Ok(())
}

fn output(command: &mut Command, name: &str) -> Result<String, String> {
    let result = command
        .output()
        .map_err(|error| format!("{name}: {error}"))?;
    if !result.status.success() {
        return Err(format!(
            "{name}: {}\n{}\n{}",
            result.status,
            String::from_utf8_lossy(&result.stdout),
            String::from_utf8_lossy(&result.stderr)
        ));
    }
    String::from_utf8(result.stdout).map_err(|error| format!("{name}: {error}"))
}
