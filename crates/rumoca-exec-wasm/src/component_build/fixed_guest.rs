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
    profile: GuestProfile,
    bytes: Vec<u8>,
    contract: contract::FixedGuestContract,
    bindings: wit_component::metadata::Bindgen,
    messages: String,
    source_directory: PathBuf,
    target_directory: PathBuf,
}

/// The product kind a fixed guest core is built for, fixed at build time: the
/// stateless native product, a stateful scalar product
/// (`rumoca-runtime-fmi-ls/scalar-product`), or a stateful scalar product that
/// executes linked compiled entries through `rumoca_dispatch`
/// (`rumoca-runtime-fmi-ls/scalar-linked-product`). The linked product never
/// enables `scalar-product`, whose bootstrap admits only the interpreter.
/// `ScalarLinkedCertify` is the build-only certification core of the linked
/// product (`certify-linked` added): the packager runs it once and never
/// publishes it. Part of the cache recipe.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum GuestProfile {
    Native,
    Scalar,
    ScalarLinked,
    ScalarLinkedCertify,
}

impl GuestProfile {
    fn cargo_features(self) -> &'static [&'static str] {
        match self {
            Self::Native => &[],
            Self::Scalar => &["--features", "scalar-product"],
            Self::ScalarLinked => &["--features", "scalar-linked-product"],
            Self::ScalarLinkedCertify => &["--features", "scalar-linked-product,certify-linked"],
        }
    }

    /// Whether this profile's core compiles reference (interpreted) execution:
    /// only the interpreted scalar product opts in (`scalar-product`). The
    /// linked product and its certification core build without it, so no
    /// interpreter body is compiled into them. Part of the cache recipe.
    fn reference_execution(self) -> bool {
        self == Self::Scalar
    }

    fn tag(self) -> &'static str {
        match self {
            Self::Native => "native",
            Self::Scalar => "scalar",
            Self::ScalarLinked => "scalar-linked",
            Self::ScalarLinkedCertify => "scalar-linked-certify",
        }
    }
}

impl FixedNativeGuestCore {
    /// Whether this core was built for the stateless native product.
    pub fn is_native_product(&self) -> bool {
        self.profile == GuestProfile::Native
    }

    /// Whether this core was built for the stateful scalar product without
    /// linked compiled entries.
    pub fn is_scalar_product(&self) -> bool {
        self.profile == GuestProfile::Scalar
    }

    /// Whether this core was built for the stateful scalar product that imports
    /// `rumoca_dispatch` and the saved product and linked artifact slots.
    pub fn is_scalar_linked_product(&self) -> bool {
        self.profile == GuestProfile::ScalarLinked
    }

    /// Whether this is the build-only certification core of the linked
    /// product: never published, run once by the packager before publication.
    pub fn is_scalar_linked_certify(&self) -> bool {
        self.profile == GuestProfile::ScalarLinkedCertify
    }

    /// Every export name of this core, as its contract read them.
    pub fn exports(&self) -> impl Iterator<Item = &str> {
        self.contract.exports.iter().map(String::as_str)
    }

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

/// Refuse a component that does not embed each of `modules` byte for byte as
/// a core module of its own, one embedded module per entry. Whatever the
/// linker generates around them, the linked libraries' bodies are then exactly
/// the ones the caller supplied.
pub fn check_embedded_core_modules(component: &[u8], modules: &[&[u8]]) -> Result<(), String> {
    let mut embedded = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(component) {
        if let wasmparser::Payload::ModuleSection {
            unchecked_range, ..
        } = payload.map_err(|error| error.to_string())?
        {
            embedded.push(
                component
                    .get(unchecked_range)
                    .ok_or("component core module is out of range")?,
            );
        }
    }
    for (index, module) in modules.iter().enumerate() {
        let position = embedded
            .iter()
            .position(|body| body == module)
            .ok_or_else(|| format!("linked library {index} is not embedded unchanged"))?;
        embedded.swap_remove(position);
    }
    Ok(())
}

/// Build the fixed source once, without accepting any external source or artifact.
/// The two directories are owned scratch storage, not source authority.
///
/// The caller must prepare its Cargo registry and pinned Git sources with
/// `cargo fetch --locked` from the issuing checkout before offline operation,
/// using the same `CARGO_HOME`. An installed or relocated tool does not carry
/// that cache; the captured compiler path and version must also remain usable.
pub fn build_fixed_native_guest_core(
    staging_parent: &Path,
    target_parent: &Path,
) -> Result<FixedNativeGuestCore, String> {
    build(staging_parent, target_parent, None, GuestProfile::Native)
}

/// As [`build_fixed_native_guest_core`], reusing the core a previous build
/// stored under `cache_root` for exactly the same staged bytes and compiler.
pub fn build_fixed_native_guest_core_cached(
    staging_parent: &Path,
    target_parent: &Path,
    cache_root: &Path,
) -> Result<FixedNativeGuestCore, String> {
    build(
        staging_parent,
        target_parent,
        Some(cache_root),
        GuestProfile::Native,
    )
}

/// As [`build_fixed_native_guest_core_cached`] for the stateful scalar product:
/// the same fixed source built with `rumoca-runtime-fmi-ls/scalar-product`,
/// under its own cache key.
pub fn build_fixed_scalar_guest_core_cached(
    staging_parent: &Path,
    target_parent: &Path,
    cache_root: &Path,
) -> Result<FixedNativeGuestCore, String> {
    build(
        staging_parent,
        target_parent,
        Some(cache_root),
        GuestProfile::Scalar,
    )
}

/// As [`build_fixed_scalar_guest_core_cached`] for the scalar product that
/// executes linked compiled entries: the same fixed source built with
/// `rumoca-runtime-fmi-ls/scalar-linked-product`, under its own cache key.
pub fn build_fixed_scalar_linked_guest_core_cached(
    staging_parent: &Path,
    target_parent: &Path,
    cache_root: &Path,
) -> Result<FixedNativeGuestCore, String> {
    build(
        staging_parent,
        target_parent,
        Some(cache_root),
        GuestProfile::ScalarLinked,
    )
}

/// As [`build_fixed_scalar_linked_guest_core_cached`] with the build-only
/// `rumoca-runtime-fmi-ls/certify-linked` export added, under its own cache
/// key. Its component is certified once at build and never published.
pub fn build_fixed_scalar_linked_certify_guest_core_cached(
    staging_parent: &Path,
    target_parent: &Path,
    cache_root: &Path,
) -> Result<FixedNativeGuestCore, String> {
    build(
        staging_parent,
        target_parent,
        Some(cache_root),
        GuestProfile::ScalarLinkedCertify,
    )
}

fn build(
    staging_parent: &Path,
    target_parent: &Path,
    cache_root: Option<&Path>,
    profile: GuestProfile,
) -> Result<FixedNativeGuestCore, String> {
    let staging = tempfile::Builder::new()
        .prefix("fixed-native-source-")
        .tempdir_in(staging_parent)
        .map_err(|error| error.to_string())?
        .keep();
    materialize(&staging)?;
    let _lease = super::cache_lease::acquire(target_parent)?;
    // Empty first-party output avoids the demonstrated cross-worktree stale
    // artifact reuse. Registry and pinned Git inputs use the caller's owned cache.
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
        &recipe(&flags, profile),
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
            let (bytes, messages) = cargo_build(rustc, &flags, &staging, &target, profile)?;
            if let Some((root, _)) = &cache_lease {
                cache::store(root, &key, &bytes, &messages)?;
            }
            (bytes, messages)
        }
    };
    let contract = contract::check(&bytes, profile)?;
    let (stripped, bindings) = wit_component::metadata::decode(&bytes)
        .map_err(|error| format!("decode fixed source interface metadata: {error:#}"))?;
    if stripped.is_none() {
        return Err("fixed guest has no source-issued interface metadata".into());
    }
    Ok(FixedNativeGuestCore {
        profile,
        bytes,
        contract,
        bindings,
        messages,
        source_directory: staging,
        target_directory: target,
    })
}

/// Every build input besides the staged bytes and the compiler identity.
fn recipe(flags: &str, profile: GuestProfile) -> String {
    format!(
        "{flags}\n{CARGO_ARGUMENTS:?}\n{:?}\nprofile={}\nreference-execution={}\n\
         artifact=rumoca_runtime_fmi_ls",
        profile.cargo_features(),
        profile.tag(),
        profile.reference_execution()
    )
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
    profile: GuestProfile,
) -> Result<(Vec<u8>, String), String> {
    let manifest = staging.join("crates/rumoca-runtime-fmi-ls/Cargo.toml");
    let messages = output(
        Command::new("cargo")
            .current_dir(staging)
            .args(CARGO_ARGUMENTS)
            .arg(&manifest)
            .args(profile.cargo_features())
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

#[cfg(test)]
mod embedded_tests {
    use super::check_embedded_core_modules;
    use wasm_encoder::{Component, CustomSection, Module, ModuleSection};

    fn module(name: &str) -> Module {
        let mut module = Module::new();
        module.section(&CustomSection {
            name: name.into(),
            data: std::borrow::Cow::Borrowed(&[]),
        });
        module
    }

    /// Each module must be embedded unchanged, once per entry: an absent, a
    /// changed or a second copy of a once-embedded module is refused.
    #[test]
    fn a_component_embeds_each_linked_module_unchanged() {
        let (first, second) = (module("first"), module("second"));
        let mut component = Component::new();
        component.section(&ModuleSection(&first));
        component.section(&ModuleSection(&second));
        let component = component.finish();
        let (first, second) = (first.finish(), second.finish());
        let check = |modules: &[&Vec<u8>]| {
            let modules: Vec<&[u8]> = modules.iter().map(|module| module.as_slice()).collect();
            check_embedded_core_modules(&component, &modules)
        };
        assert!(check(&[&second, &first]).is_ok());
        assert_eq!(
            check(&[&first, &first]).unwrap_err(),
            "linked library 1 is not embedded unchanged"
        );
        let mut changed = first.clone();
        *changed.last_mut().unwrap() ^= 1;
        assert!(check(&[&changed]).is_err());
        let third = module("third").finish();
        assert!(check(&[&first, &third]).is_err());
    }
}

#[cfg(test)]
mod profile_tests {
    use super::{GuestProfile, recipe};

    /// The two product kinds never share a cached core: the scalar profile's
    /// feature and tag are in its recipe, and the native recipe has neither.
    #[test]
    fn the_scalar_profile_has_its_own_feature_and_cache_recipe() {
        assert!(GuestProfile::Native.cargo_features().is_empty());
        assert_eq!(
            GuestProfile::Scalar.cargo_features(),
            ["--features", "scalar-product"]
        );
        let (native, scalar) = (
            recipe("flags", GuestProfile::Native),
            recipe("flags", GuestProfile::Scalar),
        );
        assert_ne!(native, scalar);
        assert!(scalar.contains("scalar-product") && scalar.contains("profile=scalar"));
        assert!(!native.contains("scalar-product") && native.contains("profile=native"));
    }

    /// The linked scalar product has its own feature, never the interpreted
    /// scalar product's, and never shares that product's cached core.
    #[test]
    fn the_scalar_linked_profile_has_its_own_feature_and_cache_recipe() {
        let features = GuestProfile::ScalarLinked.cargo_features();
        assert_eq!(features, ["--features", "scalar-linked-product"]);
        assert!(
            features
                .iter()
                .flat_map(|list| list.split(','))
                .all(|feature| feature != "scalar-product")
        );
        let (scalar, linked) = (
            recipe("flags", GuestProfile::Scalar),
            recipe("flags", GuestProfile::ScalarLinked),
        );
        assert_ne!(scalar, linked);
        assert!(
            linked.contains("scalar-linked-product") && linked.contains("profile=scalar-linked")
        );
        assert!(!scalar.contains("scalar-linked-product"));
    }

    /// The build-only certification core never shares the published linked
    /// core's cached build: its feature and tag are in its own recipe.
    #[test]
    fn the_certification_profile_has_its_own_feature_and_cache_recipe() {
        assert_eq!(
            GuestProfile::ScalarLinkedCertify.cargo_features(),
            ["--features", "scalar-linked-product,certify-linked"]
        );
        let (linked, certify) = (
            recipe("flags", GuestProfile::ScalarLinked),
            recipe("flags", GuestProfile::ScalarLinkedCertify),
        );
        assert_ne!(linked, certify);
        assert!(
            certify.contains("certify-linked") && certify.contains("profile=scalar-linked-certify")
        );
        assert!(!linked.contains("certify-linked"));
    }

    /// Only the interpreted scalar product compiles reference execution; the
    /// published linked core and its certification core never do, and the
    /// cache recipe records which, so their cores never share a key with a
    /// core built before the exclusion.
    #[test]
    fn only_the_interpreted_product_compiles_reference_execution() {
        let manifest = include_str!("../../../rumoca-runtime-fmi-ls/Cargo.toml");
        let feature = |name: &str| {
            let prefix = format!("{name} = [");
            manifest
                .lines()
                .find(|line| line.starts_with(&prefix))
                .unwrap_or_else(|| panic!("the {name} feature"))
                .to_string()
        };
        for profile in [
            GuestProfile::Native,
            GuestProfile::Scalar,
            GuestProfile::ScalarLinked,
            GuestProfile::ScalarLinkedCertify,
        ] {
            let expected = format!("reference-execution={}", profile.reference_execution());
            assert!(recipe("flags", profile).contains(&expected), "{profile:?}");
        }
        assert!(GuestProfile::Scalar.reference_execution());
        assert!(feature("scalar-product").contains("rumoca-solver/reference-execution"));
        for profile in [
            GuestProfile::ScalarLinked,
            GuestProfile::ScalarLinkedCertify,
        ] {
            assert!(!profile.reference_execution(), "{profile:?}");
        }
        assert!(!feature("scalar-linked-product").contains("reference"));
        assert!(
            !manifest.contains("certify-linked = [\""),
            "certify-linked stays empty"
        );
    }
}
