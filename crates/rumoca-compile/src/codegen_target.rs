use std::borrow::Cow;
use std::collections::BTreeMap;
use std::path::{Component, Path, PathBuf};

use anyhow::{Context, Result, bail};
use rumoca_core::ExpressionVisitor;
use rumoca_core::{BuiltinFunction, Expression, Subscript};
use rumoca_ir_dae as dae;
use rumoca_phase_codegen::templates;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TargetTemplateIr {
    Dae,
    Solve,
    Flat,
    Ast,
}

/// Post-render packaging step selected by a target's `build` field. The
/// build kind is the packaging mechanism — there is no CLI flag: `fmu`
/// compiles + zips an FMU, `efmu` assembles a schema-valid eFMU container
/// (directory + `.efmu` zip forms) from the invocation's rendered files.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TargetBuildKind {
    Fmu,
    Efmu,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TargetManifest {
    pub version: u32,
    pub ir: TargetTemplateIr,
    pub name: Option<String>,
    pub description: Option<String>,
    pub execution_mode: Option<String>,
    pub deployment_class: Option<String>,
    pub readiness_level: Option<u8>,
    pub build: Option<TargetBuildKind>,
    pub completion_message: Option<String>,
    #[serde(alias = "requirements", alias = "requires")]
    pub capabilities: Option<TargetCapabilities>,
    #[serde(default)]
    pub files: Vec<TargetFile>,
    /// Declared asset bundles the packaging build step copies verbatim into
    /// the product (contract §4d): e.g. the vendored eFMI XSD tree. Assets
    /// are NOT graph nodes — nothing checksums them, so they sit outside the
    /// render/hash DAG. A product needing no bundled assets declares none.
    #[serde(default)]
    pub assets: Vec<AssetBundle>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TargetCapabilities {
    pub continuous_states: Option<bool>,
    pub residual_equations: Option<bool>,
    pub scalar_fallback: Option<bool>,
    pub external_functions: Option<bool>,
    pub external_tables: Option<bool>,
    pub random: Option<bool>,
    pub initialization: Option<bool>,
    pub events: Option<bool>,
    pub runtime_events: Option<bool>,
    pub forward_ad: Option<bool>,
    pub reverse_ad: Option<bool>,
    pub dynamic_control_flow: Option<bool>,
    pub host_callbacks: Option<bool>,
    pub clocks: Option<bool>,
    pub dynamic_ranges: Option<bool>,
    pub dynamic_derivative_subscripts: Option<bool>,
    pub tensor: Option<TensorCapabilities>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TensorCapabilities {
    pub matmul: Option<TensorCapability>,
    pub linsolve: Option<TensorCapability>,
    pub elementwise: Option<TensorCapability>,
    pub stencil: Option<TensorCapability>,
    pub reductions: Option<TensorCapability>,
    pub layout: Option<TensorLayoutCapability>,
    pub supports_dynamic_shapes: Option<bool>,
    pub sparse: Option<bool>,
    pub dtypes: Option<Vec<String>>,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TensorCapability {
    Native,
    Scalar,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TensorLayoutCapability {
    RowMajor,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TargetFile {
    pub path: String,
    pub template: String,
    pub mode: Option<String>,
    /// Stable logical identity of this rendered file within the target
    /// (contract §4a). Only files a checksum edge points at (`of = <id>`)
    /// need one; the identity is keyed off `id`, never the templated `path`,
    /// so it is stable across path interpolation (`{{ model_name }}`).
    pub id: Option<String>,
    /// Checksum edges this file consumes: for each entry, the SHA-1 of the
    /// producer file `of` is exposed to this file's templates under the
    /// context key `as` (contract §4a). The declaration is co-located with
    /// the template that interpolates the key, so under strict-undefined
    /// minijinja a template referencing `{{ <as> }}` without a matching
    /// entry fails loudly at render — declaration and use cannot drift.
    #[serde(default)]
    pub checksums: Vec<ChecksumNeed>,
}

/// One consumer-declared checksum edge: "embed the producer `of`'s SHA-1
/// under my context key `as`" (contract §4a). Modeled as the directed edge
/// `of -> this` ("`of` rendered + hashed before this file") by the packaging
/// topo sort; a manifest can never checksum itself (no self edge) so the
/// edge set is a DAG by construction (contract §4c).
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ChecksumNeed {
    /// The producer file's `id` whose exact rendered bytes are hashed.
    pub of: String,
    /// The context key this file's templates read the producer's SHA-1 from.
    /// `as` is a Rust keyword, so the field is renamed for the struct.
    #[serde(rename = "as")]
    pub as_key: String,
}

/// A declared asset bundle: copy the named embedded/vendored `bundle` into
/// the product under `dest` (contract §4d). The bundle payload is declared
/// here; the copy mechanism is coded once, generically, in the build step.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AssetBundle {
    /// Logical name of the embedded/vendored bundle (e.g. `efmi-schemas`).
    pub bundle: String,
    /// Destination directory (product-root-relative) the bundle is copied
    /// into, e.g. `schemas/`.
    pub dest: String,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BuiltinTargetDescriptor {
    pub id: String,
    pub label: String,
    pub manifest: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum TargetFeatureSupport {
    Native,
    Scalar,
    Unsupported,
    Unknown,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TargetCompatibilityEntry {
    pub id: String,
    pub label: String,
    pub ir: TargetTemplateIr,
    pub execution_mode: Option<String>,
    pub deployment_class: Option<String>,
    pub readiness_level: Option<u8>,
    pub scalar_programs: TargetFeatureSupport,
    pub matmul: TargetFeatureSupport,
    pub linsolve: TargetFeatureSupport,
    pub elementwise: TargetFeatureSupport,
    pub stencil: TargetFeatureSupport,
    pub reductions: TargetFeatureSupport,
    pub supports_dynamic_shapes: Option<bool>,
    pub sparse: TargetFeatureSupport,
    pub dtypes: Vec<String>,
    pub events: TargetFeatureSupport,
    pub runtime_events: TargetFeatureSupport,
    pub forward_ad: TargetFeatureSupport,
    pub reverse_ad: TargetFeatureSupport,
    pub dynamic_control_flow: TargetFeatureSupport,
    pub host_callbacks: TargetFeatureSupport,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RenderedTargetFile {
    pub path: String,
    pub content: String,
}

pub enum TargetBundle {
    Builtin {
        target: &'static templates::BuiltinTarget,
    },
    Directory {
        dir: PathBuf,
        manifest: String,
    },
}

impl TargetBundle {
    pub fn load(target: &str) -> Result<Self> {
        if let Some(bundle) = Self::builtin(target) {
            return Ok(bundle);
        }

        let dir = PathBuf::from(target);
        let manifest_path = dir.join("target.toml");
        let manifest = std::fs::read_to_string(&manifest_path).with_context(|| {
            format!(
                "Read target manifest for '{}' at {}",
                target,
                manifest_path.display()
            )
        })?;
        Ok(Self::Directory { dir, manifest })
    }

    pub fn builtin(target: &str) -> Option<Self> {
        templates::builtin_target(target).map(|target| Self::Builtin { target })
    }

    pub fn parse_manifest(&self) -> Result<TargetManifest> {
        match self {
            Self::Builtin { target } => parse_target_manifest(target.manifest),
            Self::Directory { manifest, .. } => parse_target_manifest(manifest),
        }
    }

    pub fn label<'a>(&'a self, manifest: &'a TargetManifest) -> &'a str {
        manifest.name.as_deref().unwrap_or(match self {
            Self::Builtin { target } => target.name,
            Self::Directory { dir, .. } => dir.to_str().unwrap_or("custom"),
        })
    }
}

impl TargetTemplateSource for TargetBundle {
    fn template_source<'a>(&'a self, template: &str) -> Result<Cow<'a, str>> {
        match self {
            Self::Builtin { target } => target
                .template_source(template)
                .map(Cow::Borrowed)
                .ok_or_else(|| {
                    anyhow::anyhow!("Built-in target references unknown template '{template}'")
                }),
            Self::Directory { dir, .. } => {
                let path = safe_target_join(dir, template)?;
                std::fs::read_to_string(&path)
                    .map(Cow::Owned)
                    .with_context(|| format!("Read target template {}", path.display()))
            }
        }
    }
}

pub trait TargetTemplateSource {
    fn template_source<'a>(&'a self, template: &str) -> Result<Cow<'a, str>>;
}

impl TargetTemplateSource for BTreeMap<String, String> {
    fn template_source<'a>(&'a self, template: &str) -> Result<Cow<'a, str>> {
        self.get(template)
            .map(|source| Cow::Borrowed(source.as_str()))
            .ok_or_else(|| anyhow::anyhow!("Target template not found: {template}"))
    }
}

pub fn builtin_target_descriptors_for_ir(ir: TargetTemplateIr) -> Vec<BuiltinTargetDescriptor> {
    templates::builtin_targets()
        .iter()
        .filter(|target| target_manifest_ir(target.manifest) == Some(ir))
        .map(|target| BuiltinTargetDescriptor {
            id: target.name.to_string(),
            label: target.name.to_string(),
            manifest: target.manifest,
        })
        .collect()
}

pub fn builtin_target_compatibility_matrix() -> Result<Vec<TargetCompatibilityEntry>> {
    templates::builtin_targets()
        .iter()
        .map(|target| {
            let manifest = parse_target_manifest(target.manifest)
                .with_context(|| format!("Parse built-in target '{}'", target.name))?;
            Ok(target_compatibility_entry(target.name, &manifest))
        })
        .collect()
}

fn target_compatibility_entry(id: &str, manifest: &TargetManifest) -> TargetCompatibilityEntry {
    let capabilities = manifest.capabilities.as_ref();
    let tensor = capabilities.and_then(|capabilities| capabilities.tensor.as_ref());
    let scalar_fallback = capabilities
        .and_then(|capabilities| capabilities.scalar_fallback)
        .unwrap_or(true);
    TargetCompatibilityEntry {
        id: id.to_string(),
        label: manifest.name.clone().unwrap_or_else(|| id.to_string()),
        ir: manifest.ir,
        execution_mode: manifest.execution_mode.clone(),
        deployment_class: manifest.deployment_class.clone(),
        readiness_level: manifest.readiness_level,
        scalar_programs: scalar_program_support(manifest.ir),
        matmul: tensor_feature_support(
            manifest.ir,
            scalar_fallback,
            tensor.and_then(|tensor| tensor.matmul),
        ),
        linsolve: tensor_feature_support(
            manifest.ir,
            scalar_fallback,
            tensor.and_then(|tensor| tensor.linsolve),
        ),
        elementwise: tensor_feature_support(
            manifest.ir,
            scalar_fallback,
            tensor.and_then(|tensor| tensor.elementwise),
        ),
        stencil: tensor_feature_support(
            manifest.ir,
            scalar_fallback,
            tensor.and_then(|tensor| tensor.stencil),
        ),
        reductions: tensor_feature_support(
            manifest.ir,
            scalar_fallback,
            tensor.and_then(|tensor| tensor.reductions),
        ),
        supports_dynamic_shapes: tensor.and_then(|tensor| tensor.supports_dynamic_shapes),
        sparse: feature_support(tensor.and_then(|tensor| tensor.sparse)),
        dtypes: tensor_dtypes(tensor),
        events: feature_support(capabilities.and_then(|capabilities| capabilities.events)),
        runtime_events: feature_support(
            capabilities.and_then(|capabilities| capabilities.runtime_events),
        ),
        forward_ad: feature_support(capabilities.and_then(|capabilities| capabilities.forward_ad)),
        reverse_ad: feature_support(capabilities.and_then(|capabilities| capabilities.reverse_ad)),
        dynamic_control_flow: feature_support(
            capabilities.and_then(|capabilities| capabilities.dynamic_control_flow),
        ),
        host_callbacks: feature_support(
            capabilities.and_then(|capabilities| capabilities.host_callbacks),
        ),
    }
}

fn tensor_dtypes(tensor: Option<&TensorCapabilities>) -> Vec<String> {
    match tensor.and_then(|tensor| tensor.dtypes.as_ref()) {
        Some(dtypes) => dtypes.clone(),
        None => Vec::new(),
    }
}

fn scalar_program_support(ir: TargetTemplateIr) -> TargetFeatureSupport {
    match ir {
        TargetTemplateIr::Solve => TargetFeatureSupport::Native,
        TargetTemplateIr::Dae | TargetTemplateIr::Flat | TargetTemplateIr::Ast => {
            TargetFeatureSupport::Unsupported
        }
    }
}

fn tensor_feature_support(
    ir: TargetTemplateIr,
    scalar_fallback: bool,
    capability: Option<TensorCapability>,
) -> TargetFeatureSupport {
    if ir != TargetTemplateIr::Solve {
        return TargetFeatureSupport::Unsupported;
    }
    match capability {
        Some(TensorCapability::Native) => TargetFeatureSupport::Native,
        Some(TensorCapability::Scalar) if scalar_fallback => TargetFeatureSupport::Scalar,
        Some(TensorCapability::Scalar) => TargetFeatureSupport::Unsupported,
        None => TargetFeatureSupport::Unknown,
    }
}

fn feature_support(value: Option<bool>) -> TargetFeatureSupport {
    match value {
        Some(true) => TargetFeatureSupport::Native,
        Some(false) => TargetFeatureSupport::Unsupported,
        None => TargetFeatureSupport::Unknown,
    }
}

pub fn parse_target_manifest(source: &str) -> Result<TargetManifest> {
    let manifest: TargetManifest = toml::from_str(source).context("Parse target.toml")?;
    validate_target_manifest(&manifest)?;
    Ok(manifest)
}

pub fn target_manifest_ir(source: &str) -> Option<TargetTemplateIr> {
    parse_target_manifest(source)
        .ok()
        .map(|manifest| manifest.ir)
}

pub fn target_ir_is_dae_renderable(ir: TargetTemplateIr) -> bool {
    matches!(ir, TargetTemplateIr::Dae)
}

pub fn render_dae_target_files(
    source: &impl TargetTemplateSource,
    manifest: &TargetManifest,
    dae: &dae::Dae,
    model_name: &str,
) -> Result<Vec<RenderedTargetFile>> {
    ensure_target_has_rendered_files(manifest)?;
    if !target_ir_is_dae_renderable(manifest.ir) {
        bail!(
            "{:?} IR is not available from DAE-only target render state",
            manifest.ir
        );
    }

    let mut files = Vec::with_capacity(manifest.files.len());
    for file in &manifest.files {
        let path = render_dae_target_str(dae, &file.path, model_name)
            .with_context(|| format!("Render target output path '{}'", file.path))?;
        let template = source.template_source(&file.template)?;
        let content = render_dae_target_str(dae, template.as_ref(), model_name)
            .with_context(|| format!("Render target template '{}'", file.template))?;
        files.push(RenderedTargetFile {
            path: path.trim().to_string(),
            content,
        });
    }
    Ok(files)
}

pub fn validate_dae_target_capabilities(
    dae: &dae::Dae,
    manifest: &TargetManifest,
    capabilities: &TargetCapabilities,
) -> Result<()> {
    if capabilities.continuous_states == Some(false)
        && (!dae.variables.states.is_empty() || !dae.continuous.equations.is_empty())
    {
        unsupported_feature(
            manifest,
            "continuous_states",
            format!(
                "{} state(s), {} residual derivative equation(s)",
                dae.variables.states.len(),
                dae.continuous.equations.len()
            ),
        )?;
    }
    if capabilities.residual_equations == Some(false) && !dae.continuous.equations.is_empty() {
        unsupported_feature(
            manifest,
            "residual_equations",
            format!("{} equation(s)", dae.continuous.equations.len()),
        )?;
    }
    if capabilities.external_functions == Some(false) && dae_has_external_functions(dae) {
        unsupported_feature(
            manifest,
            "external_functions",
            "external declarations present",
        )?;
    }
    if capabilities.external_tables == Some(false) && dae_uses_external_tables(dae) {
        unsupported_feature(manifest, "external_tables", "table runtime calls present")?;
    }
    if capabilities.random == Some(false) && dae_uses_random(dae) {
        unsupported_feature(manifest, "random", "random runtime calls present")?;
    }
    if capabilities.initialization == Some(false) && dae_has_initialization(dae) {
        unsupported_feature(manifest, "initialization", "initial equations present")?;
    }
    if capabilities.events == Some(false) && dae_has_events(dae) {
        unsupported_feature(manifest, "events", "event or condition partitions present")?;
    }
    if capabilities.clocks == Some(false) && dae_has_clocks(dae) {
        unsupported_feature(manifest, "clocks", "clock partition entries present")?;
    }
    if capabilities.dynamic_ranges == Some(false) && dae_has_dynamic_ranges(dae) {
        unsupported_feature(
            manifest,
            "dynamic_ranges",
            "non-literal range expressions present",
        )?;
    }
    if capabilities.dynamic_derivative_subscripts == Some(false)
        && dae_has_dynamic_derivative_subscripts(dae)
    {
        unsupported_feature(
            manifest,
            "dynamic_derivative_subscripts",
            "derivative references with dynamic subscripts present",
        )?;
    }
    Ok(())
}

pub fn safe_target_join(root: &Path, relative: impl AsRef<Path>) -> Result<PathBuf> {
    let relative = relative.as_ref();
    if relative.as_os_str().is_empty() {
        bail!("Target manifest path must not be empty");
    }
    if relative.is_absolute() {
        bail!(
            "Target manifest path '{}' must be relative",
            relative.display()
        );
    }
    for component in relative.components() {
        match component {
            Component::Normal(_) | Component::CurDir => {}
            Component::ParentDir | Component::RootDir | Component::Prefix(_) => {
                bail!(
                    "Target manifest path '{}' must not escape the target root",
                    relative.display()
                );
            }
        }
    }
    Ok(root.join(relative))
}

fn validate_target_manifest(manifest: &TargetManifest) -> Result<()> {
    if manifest.version != 1 {
        bail!(
            "Unsupported target manifest version {}; expected version 1",
            manifest.version
        );
    }
    if manifest.name.as_deref().is_some_and(str::is_empty) {
        bail!("target name must not be empty when present");
    }
    if manifest.description.as_deref().is_some_and(str::is_empty) {
        bail!("target description must not be empty when present");
    }
    if manifest
        .completion_message
        .as_deref()
        .is_some_and(str::is_empty)
    {
        bail!("target completion_message must not be empty when present");
    }
    if manifest
        .readiness_level
        .is_some_and(|readiness_level| readiness_level > 5)
    {
        bail!("target readiness_level must be between 0 and 5");
    }
    if manifest.files.is_empty() && manifest.readiness_level != Some(0) {
        bail!("target.toml must contain at least one file entry");
    }
    if let Some(capabilities) = &manifest.capabilities {
        validate_target_capabilities(manifest, capabilities)?;
    }
    for file in &manifest.files {
        if let Some(mode) = file.mode.as_deref() {
            u32::from_str_radix(mode.trim_start_matches("0o"), 8)
                .with_context(|| format!("Parse target file mode '{mode}'"))?;
        }
    }
    validate_checksum_web(&manifest.files)?;
    validate_asset_bundles(&manifest.assets)?;
    Ok(())
}

/// Fail-early structural checks on the declared checksum web (contract §4a/
/// §4c), before any rendering: file `id`s are unique, every `[[files.checksums]]`
/// `of` resolves to a declared `id`, no file checksums itself (the no-self-hash
/// invariant that keeps the edge set a DAG), and every `as` key is non-empty and
/// unique per file. Cycle detection is the packaging topo sort's job (it renders
/// nothing on a cycle); this rejects the malformed declarations that can be seen
/// without ordering.
fn validate_checksum_web(files: &[TargetFile]) -> Result<()> {
    let mut ids = std::collections::BTreeSet::new();
    for file in files {
        if let Some(id) = &file.id {
            if id.trim().is_empty() {
                bail!("[[files]] id must not be empty (path '{}')", file.path);
            }
            if !ids.insert(id.as_str()) {
                bail!("duplicate [[files]] id '{id}' (ids must be unique per target)");
            }
        }
    }
    for file in files {
        let mut as_keys = std::collections::BTreeSet::new();
        for need in &file.checksums {
            if need.as_key.trim().is_empty() {
                bail!(
                    "[[files.checksums]] `as` must not be empty (file '{}', of = '{}')",
                    file.path,
                    need.of
                );
            }
            if !as_keys.insert(need.as_key.as_str()) {
                bail!(
                    "[[files.checksums]] `as` = '{}' is declared twice on file '{}'; each `as` \
                     key names one distinct injected checksum, so a duplicate would silently \
                     overwrite one producer's real SHA-1 with another's",
                    need.as_key,
                    file.path
                );
            }
            if !ids.contains(need.of.as_str()) {
                bail!(
                    "[[files.checksums]] of = '{}' on file '{}' names no [[files]] id \
                     (declare `id = \"{}\"` on the producer file)",
                    need.of,
                    file.path,
                    need.of
                );
            }
            if file.id.as_deref() == Some(need.of.as_str()) {
                bail!(
                    "[[files.checksums]] of = '{}' on file '{}' checksums itself; a file \
                     can never embed its own hash (contract §4c no-self-hash invariant)",
                    need.of,
                    file.path
                );
            }
        }
    }
    Ok(())
}

/// Fail-early checks on declared `[[assets]]` bundles: bundle name and dest
/// must be non-empty (the copy mechanism resolves the bundle payload by name).
fn validate_asset_bundles(assets: &[AssetBundle]) -> Result<()> {
    for asset in assets {
        if asset.bundle.trim().is_empty() {
            bail!("[[assets]] bundle name must not be empty");
        }
        if asset.dest.trim().is_empty() {
            bail!(
                "[[assets]] dest must not be empty (bundle '{}')",
                asset.bundle
            );
        }
    }
    Ok(())
}

pub fn ensure_target_has_rendered_files(manifest: &TargetManifest) -> Result<()> {
    if manifest.files.is_empty() {
        bail!(
            "target '{}' is manifest-only and does not define generated files yet",
            manifest.name.as_deref().unwrap_or("custom")
        );
    }
    Ok(())
}

fn validate_target_capabilities(
    manifest: &TargetManifest,
    capabilities: &TargetCapabilities,
) -> Result<()> {
    if capabilities.tensor.is_some() && manifest.ir != TargetTemplateIr::Solve {
        bail!("tensor capabilities are only valid for ir = \"solve\" targets");
    }
    if capabilities.scalar_fallback == Some(false) {
        let Some(tensor) = &capabilities.tensor else {
            return Ok(());
        };
        let scalar_tensor_ops = [
            ("tensor.matmul", tensor.matmul),
            ("tensor.linsolve", tensor.linsolve),
            ("tensor.elementwise", tensor.elementwise),
            ("tensor.stencil", tensor.stencil),
            ("tensor.reductions", tensor.reductions),
        ]
        .into_iter()
        .filter_map(|(name, mode)| (mode == Some(TensorCapability::Scalar)).then_some(name))
        .collect::<Vec<_>>();
        if !scalar_tensor_ops.is_empty() {
            bail!(
                "target.toml sets scalar_fallback = false but marks {} as scalar",
                scalar_tensor_ops.join(", ")
            );
        }
    }
    if let Some(tensor) = &capabilities.tensor
        && tensor
            .dtypes
            .as_ref()
            .is_some_and(|dtypes| dtypes.iter().any(|dtype| dtype.trim().is_empty()))
    {
        bail!("target tensor dtypes must not contain empty entries");
    }
    Ok(())
}

fn render_dae_target_str(dae: &dae::Dae, template: &str, model_name: &str) -> Result<String> {
    crate::codegen_api::render_dae_template_with_name(dae, template, model_name)
        .map_err(anyhow::Error::from)
}

fn unsupported_feature(
    manifest: &TargetManifest,
    feature: &str,
    detail: impl std::fmt::Display,
) -> Result<()> {
    bail!(
        "unsupported-feature:{}: Target '{}' does not support feature '{}': {} \
         (see `rumoca targets` for a target supporting the '{}' column)",
        feature,
        manifest.name.as_deref().unwrap_or("custom"),
        feature,
        detail,
        feature
    )
}

fn dae_has_external_functions(dae: &dae::Dae) -> bool {
    dae.symbols
        .functions
        .values()
        .any(|function| function.external.is_some())
}

fn dae_uses_external_tables(dae: &dae::Dae) -> bool {
    dae_expressions(dae).any(|expr| expression_has_named_call(expr, is_external_table_call))
}

fn is_external_table_call(name: &str) -> bool {
    matches!(
        rumoca_core::top_level_last_segment(name),
        "ExternalCombiTimeTable"
            | "ExternalCombiTable1D"
            | "ExternalCombiTable2D"
            | "getTimeTableTmax"
            | "getTimeTableTmin"
            | "getTimeTableValueNoDer"
            | "getTimeTableValueNoDer2"
            | "getTimeTableValue"
            | "getTable1DAbscissaUmax"
            | "getTable1DAbscissaUmin"
            | "getTable1DValueNoDer"
            | "getTable1DValueNoDer2"
            | "getTable1DValue"
            | "getNextTimeEvent"
            | "isValidTable"
    )
}

fn dae_uses_random(dae: &dae::Dae) -> bool {
    dae_expressions(dae).any(|expr| expression_has_named_call(expr, is_random_call))
}

fn is_random_call(name: &str) -> bool {
    let short = rumoca_core::top_level_last_segment(name);
    short.contains("Xorshift")
        || matches!(
            short,
            "initialState"
                | "random"
                | "impureRandom"
                | "impureRandomInteger"
                | "initializeImpureRandom"
        )
}

fn dae_has_initialization(dae: &dae::Dae) -> bool {
    !dae.initialization.equations.is_empty() || !dae.initialization.structured_equations.is_empty()
}

fn dae_has_events(dae: &dae::Dae) -> bool {
    !dae.conditions.equations.is_empty()
        || !dae.conditions.relations.is_empty()
        || !dae.events.synthetic_root_conditions.is_empty()
        || !dae.events.scheduled_time_events.is_empty()
        || !dae.discrete.real_updates.is_empty()
        || !dae.discrete.valued_updates.is_empty()
}

fn dae_has_clocks(dae: &dae::Dae) -> bool {
    !dae.clocks.constructor_exprs.is_empty()
        || !dae.clocks.schedules.is_empty()
        || !dae.clocks.triggered_conditions.is_empty()
        || !dae.clocks.intervals.is_empty()
        || !dae.clocks.timings.is_empty()
}

fn dae_has_dynamic_ranges(dae: &dae::Dae) -> bool {
    dae_expressions(dae).any(expression_has_dynamic_range)
}

fn dae_has_dynamic_derivative_subscripts(dae: &dae::Dae) -> bool {
    dae_expressions(dae).any(expression_has_dynamic_derivative_subscripts)
}

fn dae_expressions(dae: &dae::Dae) -> impl Iterator<Item = &Expression> {
    dae.continuous
        .equations
        .iter()
        .map(|equation| &equation.rhs)
        .chain(
            dae.initialization
                .equations
                .iter()
                .map(|equation| &equation.rhs),
        )
        .chain(
            dae.discrete
                .real_updates
                .iter()
                .map(|equation| &equation.rhs),
        )
        .chain(
            dae.discrete
                .valued_updates
                .iter()
                .map(|equation| &equation.rhs),
        )
        .chain(
            dae.conditions
                .equations
                .iter()
                .map(|equation| &equation.rhs),
        )
        .chain(dae.conditions.relations.iter())
        .chain(dae.events.synthetic_root_conditions.iter())
        .chain(dae.clocks.constructor_exprs.iter())
        .chain(dae.clocks.triggered_conditions.iter())
        .chain(dae.metadata.variable_starts.values())
}

fn expression_has_named_call(expr: &Expression, predicate: fn(&str) -> bool) -> bool {
    struct Checker {
        predicate: fn(&str) -> bool,
        found: bool,
    }

    impl ExpressionVisitor for Checker {
        fn visit_expression(&mut self, expr: &Expression) {
            if !self.found {
                self.walk_expression(expr);
            }
        }

        fn visit_function_call(
            &mut self,
            name: &rumoca_core::Reference,
            args: &[Expression],
            _: bool,
        ) {
            if (self.predicate)(name.as_str()) {
                self.found = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }
    }

    let mut checker = Checker {
        predicate,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

fn expression_has_dynamic_range(expr: &Expression) -> bool {
    struct Checker {
        found: bool,
    }

    impl ExpressionVisitor for Checker {
        fn visit_expression(&mut self, expr: &Expression) {
            if !self.found {
                self.walk_expression(expr);
            }
        }

        fn visit_range(&mut self, start: &Expression, step: Option<&Expression>, end: &Expression) {
            if !is_integer_literal(start)
                || step.is_some_and(|step| !is_integer_literal(step))
                || !is_integer_literal(end)
            {
                self.found = true;
                return;
            }
            self.visit_expression(start);
            if let Some(step) = step {
                self.visit_expression(step);
            }
            self.visit_expression(end);
        }
    }

    let mut checker = Checker { found: false };
    checker.visit_expression(expr);
    checker.found
}

fn expression_has_dynamic_derivative_subscripts(expr: &Expression) -> bool {
    struct Checker {
        found: bool,
    }

    impl ExpressionVisitor for Checker {
        fn visit_expression(&mut self, expr: &Expression) {
            if !self.found {
                self.walk_expression(expr);
            }
        }

        fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
            if *function == BuiltinFunction::Der
                && args.iter().any(expression_target_has_dynamic_subscript)
            {
                self.found = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }
    }

    let mut checker = Checker { found: false };
    checker.visit_expression(expr);
    checker.found
}

fn expression_target_has_dynamic_subscript(expr: &Expression) -> bool {
    match expr {
        Expression::VarRef { subscripts, .. } | Expression::Index { subscripts, .. } => {
            subscripts.iter().any(|subscript| match subscript {
                Subscript::Expr { expr, .. } => !is_integer_literal(expr),
                Subscript::Colon { .. } => true,
                Subscript::Index { .. } => false,
            })
        }
        Expression::FieldAccess { base, .. } => expression_target_has_dynamic_subscript(base),
        _ => false,
    }
}

fn is_integer_literal(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Literal {
            value: rumoca_core::Literal::Integer(_),
            ..
        }
    )
}

#[cfg(test)]
mod tests {
    use super::{
        TargetFeatureSupport, TargetManifest, TargetTemplateIr, TensorCapability,
        TensorLayoutCapability, builtin_target_compatibility_matrix,
        ensure_target_has_rendered_files, parse_target_manifest, safe_target_join, templates,
        validate_dae_target_capabilities, validate_target_manifest,
    };
    use rumoca_core::{
        BuiltinFunction, Expression, ExternalFunction, Function, Literal, Reference, Span,
        Subscript, VarName,
    };
    use rumoca_ir_dae::{Dae, Equation};
    use std::path::Path;

    fn function_call(name: &str) -> Expression {
        Expression::FunctionCall {
            name: Reference::new(name),
            args: Vec::new(),
            is_constructor: false,
            span: Span::DUMMY,
        }
    }

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::new(name),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        }
    }

    fn int(value: i64) -> Expression {
        Expression::Literal {
            value: Literal::Integer(value),
            span: Span::DUMMY,
        }
    }

    fn residual(rhs: Expression, origin: &str) -> Equation {
        Equation::residual(rhs, Span::DUMMY, origin)
    }

    fn manifest_with_capabilities(capabilities: &str) -> TargetManifest {
        toml::from_str(&format!(
            r#"
version = 1
ir = "dae"
name = "custom"
readiness_level = 3

{capabilities}

[[files]]
path = "model.out"
template = "model.out.jinja"
"#
        ))
        .expect("parse target manifest")
    }

    fn parse_manifest_with_ir_capabilities(ir: &str, capabilities: &str) -> TargetManifest {
        super::parse_target_manifest(&format!(
            r#"
version = 1
ir = "{ir}"
name = "custom"
readiness_level = 1

{capabilities}

[[files]]
path = "model.out"
template = "model.out.jinja"
"#
        ))
        .expect("parse and validate target manifest")
    }

    #[test]
    fn target_manifest_rejects_escaping_paths() {
        let root = Path::new("out");
        assert!(safe_target_join(root, "../escape").is_err());
        assert!(safe_target_join(root, "/absolute").is_err());
        assert_eq!(
            safe_target_join(root, "nested/file.c").unwrap(),
            root.join("nested/file.c")
        );
    }

    #[test]
    fn target_manifest_parses_capabilities_table() {
        let manifest = manifest_with_capabilities(
            r#"
[capabilities]
external_functions = false
events = true
runtime_events = false
forward_ad = true
reverse_ad = false
dynamic_control_flow = true
host_callbacks = false
"#,
        );
        let capabilities = manifest.capabilities.expect("capabilities table");

        assert_eq!(manifest.readiness_level, Some(3));
        assert_eq!(capabilities.external_functions, Some(false));
        assert_eq!(capabilities.external_tables, None);
        assert_eq!(capabilities.events, Some(true));
        assert_eq!(capabilities.runtime_events, Some(false));
        assert_eq!(capabilities.forward_ad, Some(true));
        assert_eq!(capabilities.reverse_ad, Some(false));
        assert_eq!(capabilities.dynamic_control_flow, Some(true));
        assert_eq!(capabilities.host_callbacks, Some(false));
    }

    #[test]
    fn all_builtin_target_manifests_parse() {
        for target in templates::builtin_targets() {
            parse_target_manifest(target.manifest).unwrap_or_else(|err| {
                panic!("built-in target '{}' failed to parse: {err}", target.name)
            });
        }
    }

    #[test]
    fn all_builtin_target_manifests_describe_matrix_axes() {
        for target in templates::builtin_targets() {
            let manifest = parse_target_manifest(target.manifest).unwrap_or_else(|err| {
                panic!("built-in target '{}' failed to parse: {err}", target.name)
            });
            assert!(
                manifest.execution_mode.is_some(),
                "built-in target '{}' must declare execution_mode",
                target.name
            );
            assert!(
                manifest.deployment_class.is_some(),
                "built-in target '{}' must declare deployment_class",
                target.name
            );
        }
    }

    #[test]
    fn builtin_target_compatibility_matrix_reports_solve_tensor_fallback() {
        let matrix = builtin_target_compatibility_matrix()
            .expect("built-in target compatibility matrix should build");
        let c_solve = matrix
            .iter()
            .find(|entry| entry.id == "c-solve")
            .expect("c-solve target should be listed");
        assert_eq!(c_solve.ir, TargetTemplateIr::Solve);
        assert_eq!(c_solve.readiness_level, Some(2));
        assert_eq!(c_solve.scalar_programs, TargetFeatureSupport::Native);
        assert_eq!(c_solve.matmul, TargetFeatureSupport::Scalar);
        assert_eq!(c_solve.linsolve, TargetFeatureSupport::Scalar);
        assert_eq!(c_solve.elementwise, TargetFeatureSupport::Unknown);
        assert_eq!(c_solve.sparse, TargetFeatureSupport::Unsupported);
        assert_eq!(c_solve.dtypes, vec!["f64"]);
        assert_eq!(c_solve.events, TargetFeatureSupport::Unsupported);
        assert_eq!(c_solve.runtime_events, TargetFeatureSupport::Unsupported);
        assert_eq!(c_solve.forward_ad, TargetFeatureSupport::Unsupported);
        assert_eq!(c_solve.reverse_ad, TargetFeatureSupport::Unsupported);
        assert_eq!(
            c_solve.dynamic_control_flow,
            TargetFeatureSupport::Unsupported
        );
        assert_eq!(c_solve.host_callbacks, TargetFeatureSupport::Unsupported);

        let mlir = matrix
            .iter()
            .find(|entry| entry.id == "mlir")
            .expect("mlir target should be listed");
        assert_eq!(mlir.readiness_level, Some(1));
        assert_eq!(mlir.forward_ad, TargetFeatureSupport::Native);
        assert_eq!(mlir.reverse_ad, TargetFeatureSupport::Unsupported);

        let rust_solve = matrix
            .iter()
            .find(|entry| entry.id == "rust-solve")
            .expect("rust-solve target should be listed");
        assert_eq!(rust_solve.readiness_level, Some(2));
        assert_eq!(rust_solve.matmul, TargetFeatureSupport::Scalar);

        let rust_fixed_solve = matrix
            .iter()
            .find(|entry| entry.id == "rust-fixed-solve")
            .expect("rust-fixed-solve target should be listed");
        assert_eq!(rust_fixed_solve.readiness_level, Some(2));
        assert_eq!(rust_fixed_solve.deployment_class.as_deref(), Some("cpu"));
        assert_eq!(rust_fixed_solve.execution_mode.as_deref(), Some("compiled"));
        assert_eq!(rust_fixed_solve.sparse, TargetFeatureSupport::Unsupported);
        assert_eq!(rust_fixed_solve.dtypes, vec!["f64"]);

        let cuda_c = matrix
            .iter()
            .find(|entry| entry.id == "cuda-c")
            .expect("cuda-c target should be listed");
        assert_eq!(cuda_c.readiness_level, Some(1));
        assert_eq!(cuda_c.deployment_class.as_deref(), Some("gpu"));
        assert_eq!(cuda_c.matmul, TargetFeatureSupport::Scalar);
        assert_eq!(cuda_c.linsolve, TargetFeatureSupport::Scalar);
        assert_eq!(cuda_c.sparse, TargetFeatureSupport::Unsupported);
        assert_eq!(cuda_c.dtypes, vec!["f64"]);

        let cranelift = matrix
            .iter()
            .find(|entry| entry.id == "cranelift-solve-jit")
            .expect("cranelift-solve-jit target should be listed");
        assert_eq!(cranelift.readiness_level, Some(0));
        assert_eq!(cranelift.execution_mode.as_deref(), Some("jit"));
        assert_eq!(cranelift.matmul, TargetFeatureSupport::Scalar);

        let cuda_nvrtc = matrix
            .iter()
            .find(|entry| entry.id == "cuda-nvrtc-solve-jit")
            .expect("cuda-nvrtc-solve-jit target should be listed");
        assert_eq!(cuda_nvrtc.readiness_level, Some(0));
        assert_eq!(cuda_nvrtc.deployment_class.as_deref(), Some("gpu"));
        assert_eq!(cuda_nvrtc.matmul, TargetFeatureSupport::Native);

        let wgsl_solve = matrix
            .iter()
            .find(|entry| entry.id == "wgsl-solve")
            .expect("wgsl-solve target should be listed");
        assert_eq!(wgsl_solve.readiness_level, Some(0));
        assert_eq!(wgsl_solve.deployment_class.as_deref(), Some("gpu"));
        assert_eq!(wgsl_solve.matmul, TargetFeatureSupport::Scalar);
        assert_eq!(wgsl_solve.elementwise, TargetFeatureSupport::Native);
        assert_eq!(wgsl_solve.stencil, TargetFeatureSupport::Native);

        let sympy = matrix
            .iter()
            .find(|entry| entry.id == "sympy")
            .expect("sympy target should be listed");
        assert_eq!(sympy.ir, TargetTemplateIr::Dae);
        assert_eq!(sympy.scalar_programs, TargetFeatureSupport::Unsupported);
        assert_eq!(sympy.matmul, TargetFeatureSupport::Unsupported);
    }

    #[test]
    fn target_manifest_parses_solve_tensor_capabilities() {
        let manifest = parse_manifest_with_ir_capabilities(
            "solve",
            r#"
[capabilities]
scalar_fallback = true

[capabilities.tensor]
matmul = "native"
linsolve = "scalar"
stencil = "native"
layout = "row-major"
supports_dynamic_shapes = false
sparse = false
dtypes = ["f32", "f64"]
"#,
        );
        let capabilities = manifest.capabilities.expect("capabilities table");
        let tensor = capabilities.tensor.expect("tensor capabilities");

        assert_eq!(manifest.ir, TargetTemplateIr::Solve);
        assert_eq!(capabilities.scalar_fallback, Some(true));
        assert_eq!(tensor.matmul, Some(TensorCapability::Native));
        assert_eq!(tensor.linsolve, Some(TensorCapability::Scalar));
        assert_eq!(tensor.stencil, Some(TensorCapability::Native));
        assert_eq!(tensor.layout, Some(TensorLayoutCapability::RowMajor));
        assert_eq!(tensor.supports_dynamic_shapes, Some(false));
        assert_eq!(tensor.sparse, Some(false));
        assert_eq!(
            tensor.dtypes,
            Some(vec!["f32".to_string(), "f64".to_string()])
        );
    }

    #[test]
    fn target_manifest_rejects_tensor_capabilities_for_non_solve_ir() {
        let err = super::parse_target_manifest(
            r#"
version = 1
ir = "dae"
name = "custom"

[capabilities.tensor]
matmul = "native"

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
        )
        .expect_err("tensor capabilities should require solve IR");

        assert!(
            err.to_string()
                .contains("tensor capabilities are only valid")
        );
    }

    #[test]
    fn target_manifest_rejects_scalar_tensor_ops_without_scalar_fallback() {
        let manifest = parse_manifest_with_ir_capabilities(
            "solve",
            r#"
[capabilities]
scalar_fallback = false

[capabilities.tensor]
matmul = "native"
linsolve = "native"
"#,
        );
        let capabilities = manifest.capabilities.as_ref().expect("capabilities");
        validate_target_manifest(&manifest).expect("native tensor ops need no scalar fallback");
        assert_eq!(capabilities.scalar_fallback, Some(false));

        let err = super::parse_target_manifest(
            r#"
version = 1
ir = "solve"
name = "custom"

[capabilities]
scalar_fallback = false

[capabilities.tensor]
matmul = "scalar"

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
        )
        .expect_err("scalar tensor op should require scalar fallback");

        assert!(err.to_string().contains("scalar_fallback = false"));
    }

    #[test]
    fn target_manifest_rejects_invalid_readiness_level() {
        let err = super::parse_target_manifest(
            r#"
version = 1
ir = "solve"
name = "invalid"
readiness_level = 6

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
        )
        .expect_err("readiness level above 5 should fail");

        assert!(err.to_string().contains("readiness_level"), "{err}");
    }

    #[test]
    fn target_manifest_accepts_manifest_only_readiness_zero() {
        let manifest = super::parse_target_manifest(
            r#"
version = 1
ir = "solve"
name = "future-target"
readiness_level = 0
"#,
        )
        .expect("readiness level 0 target may be manifest-only");

        assert!(manifest.files.is_empty());
        let err = ensure_target_has_rendered_files(&manifest)
            .expect_err("manifest-only targets should not render files");
        assert!(err.to_string().contains("manifest-only"), "{err}");
    }

    #[test]
    fn target_manifest_rejects_missing_files_after_readiness_zero() {
        let err = super::parse_target_manifest(
            r#"
version = 1
ir = "solve"
name = "unfinished"
readiness_level = 1
"#,
        )
        .expect_err("readiness level above 0 requires generated files");

        assert!(err.to_string().contains("file entry"), "{err}");
    }

    #[test]
    fn target_manifest_rejects_empty_tensor_dtype() {
        let err = super::parse_target_manifest(
            r#"
version = 1
ir = "solve"
name = "invalid-dtypes"

[capabilities.tensor]
dtypes = ["f64", ""]

[[files]]
path = "model.out"
template = "model.out.jinja"
"#,
        )
        .expect_err("empty tensor dtype should fail");

        assert!(err.to_string().contains("dtypes"), "{err}");
    }

    #[test]
    fn target_manifest_accepts_requirements_as_capabilities_alias() {
        let manifest = manifest_with_capabilities(
            r#"
[requirements]
continuous_states = false
residual_equations = false
"#,
        );
        let capabilities = manifest.capabilities.expect("requirements alias");

        assert_eq!(capabilities.continuous_states, Some(false));
        assert_eq!(capabilities.residual_equations, Some(false));
    }

    #[test]
    fn target_capabilities_reject_external_functions_generically() {
        let manifest = manifest_with_capabilities(
            r#"
[capabilities]
external_functions = false
"#,
        );
        let capabilities = manifest.capabilities.as_ref().expect("capabilities");
        let mut dae = Dae::new();
        let mut function = Function::new("ExternalUser", rumoca_core::Span::DUMMY);
        function.external = Some(ExternalFunction::default());
        dae.symbols
            .functions
            .insert(VarName::new("ExternalUser"), function);

        let err = validate_dae_target_capabilities(&dae, &manifest, capabilities)
            .expect_err("external function should be rejected");

        let message = err.to_string();
        assert!(message.contains("custom"));
        assert!(message.contains("external_functions"));
    }

    #[test]
    fn target_capabilities_reject_events_generically() {
        let manifest = manifest_with_capabilities(
            r#"
[capabilities]
events = false
"#,
        );
        let capabilities = manifest.capabilities.as_ref().expect("capabilities");
        let mut dae = Dae::new();
        dae.events.scheduled_time_events.push(0.1);

        let err = validate_dae_target_capabilities(&dae, &manifest, capabilities)
            .expect_err("events should be rejected");

        assert!(err.to_string().contains("events"));
    }

    #[test]
    fn target_capabilities_reject_external_tables_generically() {
        let manifest = manifest_with_capabilities(
            r#"
[capabilities]
external_tables = false
"#,
        );
        let capabilities = manifest.capabilities.as_ref().expect("capabilities");
        let mut dae = Dae::new();
        dae.continuous.equations.push(residual(
            function_call("ModelicaStandardTables.CombiTable1D.getTable1DValue"),
            "external table call",
        ));

        let err = validate_dae_target_capabilities(&dae, &manifest, capabilities)
            .expect_err("external table calls should be rejected");

        assert!(err.to_string().contains("external_tables"));
    }

    #[test]
    fn target_capabilities_reject_random_generically() {
        let manifest = manifest_with_capabilities(
            r#"
[capabilities]
random = false
"#,
        );
        let capabilities = manifest.capabilities.as_ref().expect("capabilities");
        let mut dae = Dae::new();
        dae.discrete.valued_updates.push(residual(
            function_call("Modelica.Math.Random.Utilities.initializeImpureRandom"),
            "random call",
        ));

        let err = validate_dae_target_capabilities(&dae, &manifest, capabilities)
            .expect_err("random calls should be rejected");

        assert!(err.to_string().contains("random"));
    }

    #[test]
    fn target_capabilities_reject_dynamic_ranges_generically() {
        let manifest = manifest_with_capabilities(
            r#"
[capabilities]
dynamic_ranges = false
"#,
        );
        let capabilities = manifest.capabilities.as_ref().expect("capabilities");
        let mut dae = Dae::new();
        dae.continuous.equations.push(residual(
            Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(var("n")),
                span: Span::DUMMY,
            },
            "dynamic range",
        ));

        let err = validate_dae_target_capabilities(&dae, &manifest, capabilities)
            .expect_err("dynamic ranges should be rejected");

        assert!(err.to_string().contains("dynamic_ranges"));
    }

    #[test]
    fn target_capabilities_reject_dynamic_derivative_subscripts_generically() {
        let manifest = manifest_with_capabilities(
            r#"
[capabilities]
dynamic_derivative_subscripts = false
"#,
        );
        let capabilities = manifest.capabilities.as_ref().expect("capabilities");
        let mut dae = Dae::new();
        dae.continuous.equations.push(residual(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: Reference::new("x"),
                    subscripts: vec![Subscript::generated_expr(
                        Box::new(var("i")),
                        rumoca_core::Span::DUMMY,
                    )],
                    span: Span::DUMMY,
                }],
                span: Span::DUMMY,
            },
            "dynamic derivative subscript",
        ));

        let err = validate_dae_target_capabilities(&dae, &manifest, capabilities)
            .expect_err("dynamic derivative subscripts should be rejected");

        assert!(err.to_string().contains("dynamic_derivative_subscripts"));
    }

    // --- checksum-web / asset-bundle validators (each fail-early branch) ---

    /// A well-formed checksum web (one producer, one consumer edge) parses and
    /// validates — the positive control for the rejection tests below.
    #[test]
    fn checksum_web_accepts_a_wellformed_declaration() {
        super::parse_target_manifest(
            r#"
version = 1
ir = "solve"
name = "checksum-web"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "a"
as = "a_sha1"
"#,
        )
        .expect("a well-formed checksum web validates");
    }

    fn expect_target_error(source: &str, needle: &str) {
        let err = super::parse_target_manifest(source)
            .expect_err("malformed target.toml must be rejected");
        assert!(
            err.to_string().contains(needle),
            "error `{err}` should mention `{needle}`"
        );
    }

    #[test]
    fn checksum_web_rejects_duplicate_file_ids() {
        expect_target_error(
            r#"
version = 1
ir = "solve"
name = "dup-id"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "x"
[[files]]
path = "b.txt"
template = "b.jinja"
id = "x"
"#,
            "duplicate [[files]] id",
        );
    }

    #[test]
    fn checksum_web_rejects_dangling_of() {
        expect_target_error(
            r#"
version = 1
ir = "solve"
name = "dangling"
[[files]]
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "ghost"
as = "ghost_sha1"
"#,
            "names no [[files]] id",
        );
    }

    #[test]
    fn checksum_web_rejects_self_hash() {
        expect_target_error(
            r#"
version = 1
ir = "solve"
name = "self-hash"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files.checksums]]
of = "a"
as = "a_sha1"
"#,
            "checksums itself",
        );
    }

    #[test]
    fn checksum_web_rejects_empty_as_key() {
        expect_target_error(
            r#"
version = 1
ir = "solve"
name = "empty-as"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "a"
as = ""
"#,
            "`as` must not be empty",
        );
    }

    #[test]
    fn checksum_web_rejects_duplicate_as_key_on_one_file() {
        expect_target_error(
            r#"
version = 1
ir = "solve"
name = "dup-as"
[[files]]
path = "a.txt"
template = "a.jinja"
id = "a"
[[files]]
path = "c.txt"
template = "c.jinja"
id = "c"
[[files]]
path = "b.txt"
template = "b.jinja"
[[files.checksums]]
of = "a"
as = "sha1"
[[files.checksums]]
of = "c"
as = "sha1"
"#,
            "declared twice",
        );
    }

    #[test]
    fn asset_bundle_rejects_empty_bundle_and_dest() {
        expect_target_error(
            r#"
version = 1
ir = "solve"
name = "empty-bundle"
[[files]]
path = "a.txt"
template = "a.jinja"
[[assets]]
bundle = ""
dest = "schemas/"
"#,
            "bundle name must not be empty",
        );
        expect_target_error(
            r#"
version = 1
ir = "solve"
name = "empty-dest"
[[files]]
path = "a.txt"
template = "a.jinja"
[[assets]]
bundle = "efmi-schemas"
dest = ""
"#,
            "dest",
        );
    }
}
