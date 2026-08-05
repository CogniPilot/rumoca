use std::borrow::Cow;
use std::collections::BTreeMap;
use std::path::{Component, Path, PathBuf};

use anyhow::{Context, Result, bail};
use rumoca_ir_dae as dae;
use rumoca_phase_codegen::templates;
use serde::{Deserialize, Serialize};

mod feature_analysis;

use feature_analysis::{
    dae_has_clocks, dae_has_dynamic_derivative_subscripts, dae_has_dynamic_ranges, dae_has_events,
    dae_has_external_functions, dae_has_initialization, dae_has_runtime_events,
    dae_has_unlowered_source_temporal_operators, dae_uses_external_tables, dae_uses_random,
};

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TargetTemplateIr {
    Dae,
    Solve,
    Flat,
    Ast,
    AlgorithmCode,
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
    pub package: Option<TargetPackage>,
    pub integer: Option<TargetIntegerDomain>,
    pub completion_message: Option<String>,
    #[serde(alias = "requirements", alias = "requires")]
    pub capabilities: Option<TargetCapabilities>,
    #[serde(default)]
    pub files: Vec<TargetFile>,
    /// Declared target-relative asset trees copied verbatim into the product.
    #[serde(default)]
    pub assets: Vec<AssetBundle>,
}

/// Target-declared product layout. All paths are target templates rendered
/// against the same semantic context as `[[files]]`.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TargetPackage {
    pub root: String,
    #[serde(default)]
    pub required_files: Vec<String>,
    pub archive: Option<TargetArchive>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TargetArchive {
    pub path: String,
    pub format: TargetArchiveFormat,
    pub root: TargetArchiveRoot,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TargetArchiveFormat {
    Zip,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TargetArchiveRoot {
    Flat,
}

/// Integer range whose operations a target promises to represent.
#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TargetIntegerDomain {
    pub minimum: i64,
    pub maximum: i64,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TargetCapabilities {
    pub continuous_states: Option<bool>,
    pub residual_equations: Option<bool>,
    /// The target consumes compact DAE `structured_equations` as the
    /// authoritative body instead of blindly iterating placeholder scalar rows.
    pub structured_equation_families: Option<bool>,
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
    Unsupported,
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
    /// Hash algorithm selected by the target format.
    pub algorithm: ChecksumAlgorithm,
    /// The context key this file's templates read the producer's SHA-1 from.
    /// `as` is a Rust keyword, so the field is renamed for the struct.
    #[serde(rename = "as")]
    pub as_key: String,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "kebab-case")]
pub enum ChecksumAlgorithm {
    Sha1,
}

/// A declared asset tree copied from `source` under the target directory to
/// `dest` under the package root.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AssetBundle {
    pub source: String,
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

    pub fn asset_files(&self, source: &str) -> Result<Vec<TargetAssetFile>> {
        match self {
            Self::Builtin { target } => target
                .asset_files(source)
                .map(|files| {
                    files
                        .into_iter()
                        .map(|(relative_path, bytes)| TargetAssetFile {
                            relative_path: relative_path.to_owned(),
                            bytes: bytes.to_vec(),
                        })
                        .collect()
                })
                .with_context(|| {
                    format!(
                        "Built-in target '{}' contains no asset source '{source}'",
                        target.name
                    )
                }),
            Self::Directory { dir, .. } => {
                let root = safe_target_join(dir, source)?;
                collect_target_assets(&root)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TargetAssetFile {
    pub relative_path: String,
    pub bytes: Vec<u8>,
}

fn collect_target_assets(root: &Path) -> Result<Vec<TargetAssetFile>> {
    if !root.is_dir() {
        bail!(
            "Target asset source '{}' is not a directory",
            root.display()
        );
    }
    let mut paths = Vec::new();
    collect_target_asset_paths(root, root, &mut paths)?;
    paths.sort_by(|left, right| left.0.cmp(&right.0));
    paths
        .into_iter()
        .map(|(relative_path, path)| {
            Ok(TargetAssetFile {
                relative_path,
                bytes: std::fs::read(&path)
                    .with_context(|| format!("Read target asset '{}'", path.display()))?,
            })
        })
        .collect()
}

fn collect_target_asset_paths(
    root: &Path,
    dir: &Path,
    out: &mut Vec<(String, PathBuf)>,
) -> Result<()> {
    for entry in std::fs::read_dir(dir)
        .with_context(|| format!("Read target asset directory '{}'", dir.display()))?
    {
        let entry =
            entry.with_context(|| format!("Read target asset entry in '{}'", dir.display()))?;
        let file_type = entry
            .file_type()
            .with_context(|| format!("Stat target asset '{}'", entry.path().display()))?;
        if file_type.is_symlink() {
            bail!(
                "Target asset source may not contain symlinks: '{}'",
                entry.path().display()
            );
        }
        if file_type.is_dir() {
            collect_target_asset_paths(root, &entry.path(), out)?;
        } else if file_type.is_file() {
            let relative_path = target_asset_relative_path(root, &entry.path())?;
            out.push((relative_path, entry.path()));
        }
    }
    Ok(())
}

fn target_asset_relative_path(root: &Path, path: &Path) -> Result<String> {
    let relative = path.strip_prefix(root).with_context(|| {
        format!(
            "Target asset '{}' is not beneath source root '{}'",
            path.display(),
            root.display()
        )
    })?;
    let mut parts = Vec::new();
    for component in relative.components() {
        let Component::Normal(part) = component else {
            bail!(
                "Target asset path must contain only normal components: '{}'",
                path.display()
            );
        };
        let part = part
            .to_str()
            .with_context(|| format!("Target asset path must be UTF-8: '{}'", path.display()))?;
        parts.push(part);
    }
    if parts.is_empty() {
        bail!(
            "Target asset path must identify a file beneath source root: '{}'",
            path.display()
        );
    }
    Ok(parts.join("/"))
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
        TargetTemplateIr::Dae
        | TargetTemplateIr::Flat
        | TargetTemplateIr::Ast
        | TargetTemplateIr::AlgorithmCode => TargetFeatureSupport::Unsupported,
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
        Some(TensorCapability::Unsupported) => TargetFeatureSupport::Unsupported,
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
    let capabilities = manifest
        .capabilities
        .as_ref()
        .context("DAE target manifest must declare a [capabilities] table")?;
    validate_dae_target_capabilities(dae, manifest, capabilities)?;
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
    if dae_has_unlowered_source_temporal_operators(dae) {
        bail!(
            "invalid canonical DAE for target '{}': a source temporal or synchronous operator survived the Phase-DAE boundary",
            manifest.name.as_deref().unwrap_or("<unnamed>")
        );
    }
    let (state_count, continuous_equation_count, continuous_family_count) = dae.inspect(|view| {
        let state_count = view
            .variables()
            .filter(|(_, variable)| variable.role() == dae::VariableRole::State)
            .count();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        (
            state_count,
            definitions.remaining_owner_count(),
            view.continuous_family_count(),
        )
    });
    // DAE templates must explicitly consume compact families. Algorithm Code
    // targets own a stronger, target-specific projection proof and re-check
    // every family before lowering; the generic renderer must not reject that
    // canonical input before the projection can inspect it.
    if manifest.ir == TargetTemplateIr::Dae
        && capabilities.structured_equation_families != Some(true)
        && continuous_family_count != 0
    {
        unsupported_feature(
            manifest,
            "structured_equation_families",
            format!("{continuous_family_count} compact equation family owner(s)"),
        )?;
    }
    if capabilities.continuous_states == Some(false)
        && (state_count != 0 || continuous_equation_count != 0)
    {
        unsupported_feature(
            manifest,
            "continuous_states",
            format!(
                "{} state(s), {} residual derivative equation(s)",
                state_count, continuous_equation_count
            ),
        )?;
    }
    if capabilities.residual_equations == Some(false) && continuous_equation_count != 0 {
        unsupported_feature(
            manifest,
            "residual_equations",
            format!("{continuous_equation_count} equation(s)"),
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
    if capabilities.events != Some(true) && dae_has_events(dae) {
        unsupported_feature(manifest, "events", "event or condition partitions present")?;
    }
    if capabilities.runtime_events == Some(false) && dae_has_runtime_events(dae) {
        unsupported_feature(
            manifest,
            "runtime_events",
            "delay-history or terminal-event runtime support is required",
        )?;
    }
    if capabilities.clocks != Some(true) && dae_has_clocks(dae) {
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

pub fn validate_solve_target_capabilities(
    solve: &rumoca_ir_solve::SolveProblem,
    manifest: &TargetManifest,
    capabilities: &TargetCapabilities,
) -> Result<()> {
    let mut inventory = solve.compute_node_counts();
    inventory.add_assign(solve.initialization.residual.compute_node_counts());
    let uses_linear_solve_component = solve.uses_linear_solve_component()
        || solve.initialization.residual.uses_linear_solve_component();
    validate_solve_tensor_inventory(
        manifest,
        capabilities,
        inventory,
        uses_linear_solve_component,
    )
}

pub fn validate_solve_tensor_inventory(
    manifest: &TargetManifest,
    capabilities: &TargetCapabilities,
    inventory: rumoca_ir_solve::ComputeNodeCounts,
    uses_linear_solve_component: bool,
) -> Result<()> {
    let scalar_fallback = capabilities.scalar_fallback.unwrap_or(true);
    let tensor = capabilities.tensor.as_ref();

    validate_solve_tensor_feature(
        manifest,
        "tensor.matmul",
        "MatMul",
        inventory.matmul,
        tensor.and_then(|tensor| tensor.matmul),
        scalar_fallback,
    )?;
    validate_solve_tensor_feature(
        manifest,
        "tensor.linsolve",
        "LinSolve",
        inventory
            .linsolve
            .saturating_add(usize::from(uses_linear_solve_component)),
        tensor.and_then(|tensor| tensor.linsolve),
        scalar_fallback,
    )?;
    validate_solve_tensor_feature(
        manifest,
        "tensor.elementwise",
        "Map",
        inventory.map,
        tensor.and_then(|tensor| tensor.elementwise),
        scalar_fallback,
    )?;
    validate_solve_tensor_feature(
        manifest,
        "tensor.stencil",
        "AffineStencil",
        inventory.affine_stencil,
        tensor.and_then(|tensor| tensor.stencil),
        scalar_fallback,
    )
}

fn validate_solve_tensor_feature(
    manifest: &TargetManifest,
    feature: &str,
    display_name: &str,
    count: usize,
    capability: Option<TensorCapability>,
    scalar_fallback: bool,
) -> Result<()> {
    if count == 0 {
        return Ok(());
    }
    match capability {
        Some(TensorCapability::Native) => Ok(()),
        Some(TensorCapability::Scalar) if scalar_fallback => Ok(()),
        Some(TensorCapability::Scalar) => unsupported_tensor_feature(
            manifest,
            feature,
            format!(
                "{display_name} is configured for scalar fallback but scalar fallback is disabled"
            ),
        ),
        Some(TensorCapability::Unsupported) => unsupported_tensor_feature(
            manifest,
            feature,
            format!(
                "{display_name} nodes are present but the target declares {feature} unsupported"
            ),
        ),
        None if scalar_fallback => Ok(()),
        None => unsupported_tensor_feature(
            manifest,
            feature,
            format!(
                "{display_name} nodes are present but the target does not declare native \
                 {display_name} support and scalar fallback is disabled"
            ),
        ),
    }
}

fn unsupported_tensor_feature(
    manifest: &TargetManifest,
    feature: &str,
    detail: impl std::fmt::Display,
) -> Result<()> {
    bail!(
        "unsupported-feature:{feature}: Target '{}' does not support feature '{feature}': {detail}",
        manifest.name.as_deref().unwrap_or("custom")
    )
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
    if matches!(
        manifest.ir,
        TargetTemplateIr::Dae | TargetTemplateIr::AlgorithmCode
    ) && manifest.capabilities.is_none()
    {
        bail!("DAE-derived target manifest must declare a [capabilities] table");
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
    validate_package(manifest.package.as_ref())?;
    if let Some(integer) = manifest.integer
        && integer.minimum > integer.maximum
    {
        bail!(
            "[integer] minimum ({}) must not exceed maximum ({})",
            integer.minimum,
            integer.maximum
        );
    }
    Ok(())
}

fn validate_package(package: Option<&TargetPackage>) -> Result<()> {
    let Some(package) = package else {
        return Ok(());
    };
    if package.root.trim().is_empty() {
        bail!("[package] root must not be empty");
    }
    for required in &package.required_files {
        if required.trim().is_empty() {
            bail!("[package] required_files entries must not be empty");
        }
    }
    if let Some(archive) = &package.archive
        && archive.path.trim().is_empty()
    {
        bail!("[package.archive] path must not be empty");
    }
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

/// Fail-early checks on declared target-relative asset trees.
fn validate_asset_bundles(assets: &[AssetBundle]) -> Result<()> {
    for asset in assets {
        if asset.source.trim().is_empty() {
            bail!("[[assets]] source must not be empty");
        }
        if asset.dest.trim().is_empty() {
            bail!(
                "[[assets]] dest must not be empty (source '{}')",
                asset.source
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
    if capabilities.structured_equation_families.is_some() && manifest.ir != TargetTemplateIr::Dae {
        bail!("structured_equation_families capability is only valid for ir = \"dae\" targets");
    }
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

#[cfg(test)]
mod tests {
    use super::{
        TargetFeatureSupport, TargetManifest, TargetTemplateIr, TensorCapability,
        TensorLayoutCapability, builtin_target_compatibility_matrix,
        ensure_target_has_rendered_files, parse_target_manifest, safe_target_join,
        target_asset_relative_path, templates, validate_dae_target_capabilities,
        validate_target_manifest,
    };
    use rumoca_core::{SourceMap, StructuredIndexBinder, StructuredIndexDomain};
    use rumoca_ir_dae::{Dae, DaeLiteral, DaeProvenance};
    use std::path::Path;

    fn dae_with_placeholder_family() -> Dae {
        let source_text = "for i in 1:4 loop 0.0 = 0.0; end for;";
        let mut source_map = SourceMap::new();
        let source_id = source_map.add("target-capability.mo", source_text);
        let owner = DaeProvenance::source(rumoca_core::Span::from_offsets(
            source_id,
            0,
            source_text.len(),
        ))
        .expect("fixture source span is exact");
        Dae::construct(source_map, |dae| {
            let domain = dae.domains(|domains| {
                domains.structured(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            id: 0,
                            display_name: "i".to_string(),
                            lower: 1,
                            upper: 4,
                            step: 1,
                        }],
                    },
                    owner,
                )
            })?;
            let residual = dae
                .expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Real(0.0)))?;
            dae.continuous(|equations| {
                equations.structured_family(
                    owner,
                    domain,
                    rumoca_core::ComprehensionScalarView::BinderSubstitution,
                    |family| family.body(residual),
                )
            })?;
            Ok(())
        })
        .expect("checked structured-family fixture is valid")
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
    fn builtin_structured_dae_capability_matches_template_consumption() {
        for target in templates::builtin_targets() {
            let manifest = parse_target_manifest(target.manifest).unwrap_or_else(|err| {
                panic!("built-in target '{}' failed to parse: {err}", target.name)
            });
            let family_aware = manifest
                .capabilities
                .as_ref()
                .and_then(|capabilities| capabilities.structured_equation_families)
                == Some(true);
            if !family_aware {
                continue;
            }
            for owner_path in [
                "dae.systems.continuous.owners",
                "dae.systems.initialization.owners",
            ] {
                assert!(
                    target
                        .templates
                        .iter()
                        .any(|template| template.source.contains(owner_path)),
                    "built-in target '{}' declares structured family ownership but no template \
                     consumes checked owner projection `{owner_path}`",
                    target.name
                );
            }
            assert!(
                target
                    .templates
                    .iter()
                    .all(|template| !template.source.contains("dae.f_x")),
                "built-in target '{}' declares structured family ownership but reads the removed \
                 scalar residual field",
                target.name
            );
        }
    }

    #[test]
    fn builtin_dae_consumers_use_only_the_checked_template_schema() {
        fn dae_root_fields(source: &str) -> impl Iterator<Item = &str> {
            source.match_indices("dae.").filter_map(|(start, _)| {
                let field = &source[start + "dae.".len()..];
                let end = field
                    .find(|character: char| !character.is_ascii_alphanumeric() && character != '_')
                    .unwrap_or(field.len());
                (end != 0).then_some(&field[..end])
            })
        }

        const CHECKED_ROOT_FIELDS: &[&str] = &[
            "schema",
            "value_types",
            "variables",
            "functions",
            "domains",
            "expressions",
            "modelica",
            "systems",
        ];
        let mut offenders = Vec::new();
        for target in templates::builtin_targets() {
            let manifest = parse_target_manifest(target.manifest).unwrap_or_else(|error| {
                panic!("built-in target '{}' failed to parse: {error}", target.name)
            });
            if !matches!(
                manifest.ir,
                TargetTemplateIr::Dae | TargetTemplateIr::Solve | TargetTemplateIr::AlgorithmCode
            ) {
                continue;
            }
            for template in target.templates {
                offenders.extend(
                    dae_root_fields(template.source)
                        .filter(|field| !CHECKED_ROOT_FIELDS.contains(field))
                        .map(|field| format!("{}:{}:dae.{field}", target.name, template.path)),
                );
            }
        }
        assert!(
            offenders.is_empty(),
            "built-in templates still consume fields outside the checked DAE schema: \
             {offenders:#?}"
        );
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
        assert_eq!(rust_fixed_solve.matmul, TargetFeatureSupport::Scalar);
        assert_eq!(rust_fixed_solve.linsolve, TargetFeatureSupport::Unsupported);
        assert_eq!(rust_fixed_solve.sparse, TargetFeatureSupport::Unsupported);
        assert_eq!(rust_fixed_solve.dtypes, vec!["f64"]);

        let cuda_c = matrix
            .iter()
            .find(|entry| entry.id == "cuda-c")
            .expect("cuda-c target should be listed");
        assert_eq!(cuda_c.readiness_level, Some(1));
        assert_eq!(cuda_c.deployment_class.as_deref(), Some("gpu"));
        assert_eq!(cuda_c.matmul, TargetFeatureSupport::Scalar);
        assert_eq!(cuda_c.linsolve, TargetFeatureSupport::Unsupported);
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

        for removed in [
            "casadi-mx",
            "casadi-sx",
            "fmi2",
            "fmi3",
            "jax",
            "julia-mtk",
            "onnx",
            "symforce",
            "sympy",
        ] {
            assert!(
                matrix.iter().all(|entry| entry.id != removed),
                "target `{removed}` consumed the removed DAE template schema"
            );
        }
    }

    #[test]
    fn removed_file_render_context_is_rejected() {
        parse_target_manifest(
            r#"
version = 1
ir = "solve"
name = "removed-context"

[[files]]
path = "modelDescription.xml"
template = "modelDescription.xml.jinja"
render_context = "fmi-model-description"
"#,
        )
        .expect_err("removed per-file render contexts must not parse");
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
    fn dae_target_must_declare_structured_family_consumption() {
        let dae = dae_with_placeholder_family();
        let manifest = manifest_with_capabilities(
            r#"
[capabilities]
residual_equations = true
"#,
        );
        let capabilities = manifest.capabilities.as_ref().expect("capabilities");

        let error = validate_dae_target_capabilities(&dae, &manifest, capabilities)
            .expect_err("scalar-only DAE target must reject placeholder rows");

        assert!(
            error
                .to_string()
                .contains("unsupported-feature:structured_equation_families")
        );
    }

    #[test]
    fn family_aware_dae_target_accepts_canonical_structured_owner() {
        let dae = dae_with_placeholder_family();
        let manifest = manifest_with_capabilities(
            r#"
[capabilities]
residual_equations = true
structured_equation_families = true
"#,
        );
        let capabilities = manifest.capabilities.as_ref().expect("capabilities");

        validate_dae_target_capabilities(&dae, &manifest, capabilities)
            .expect("declared family-aware target may consume the compact owner");
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
    fn shared_dae_renderer_blocks_placeholder_scalar_residuals() {
        let manifest = manifest_with_capabilities(
            r#"
[capabilities]
residual_equations = true
"#,
        );
        let templates =
            std::collections::BTreeMap::from([("model.out.jinja".to_string(), String::new())]);

        let error = super::render_dae_target_files(
            &templates,
            &manifest,
            &dae_with_placeholder_family(),
            "M",
        )
        .expect_err("shared DAE rendering must not expose placeholder residuals");

        assert!(
            error
                .to_string()
                .contains("unsupported-feature:structured_equation_families"),
            "{error}"
        );
    }

    #[test]
    fn shared_dae_renderer_allows_declared_structured_owner_consumer() {
        let manifest = manifest_with_capabilities(
            r#"
[capabilities]
residual_equations = true
structured_equation_families = true
"#,
        );
        let templates = std::collections::BTreeMap::from([(
            "model.out.jinja".to_string(),
            "{{ dae.systems.continuous.owners | length }}".to_string(),
        )]);

        let files = super::render_dae_target_files(
            &templates,
            &manifest,
            &dae_with_placeholder_family(),
            "M",
        )
        .expect("declared family-aware consumer may render the canonical owner");

        assert_eq!(files[0].content, "1");
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
  algorithm = "sha1"
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
  algorithm = "sha1"
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
  algorithm = "sha1"
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
  algorithm = "sha1"
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
  algorithm = "sha1"
as = "sha1"
[[files.checksums]]
of = "c"
  algorithm = "sha1"
as = "sha1"
"#,
            "declared twice",
        );
    }

    #[test]
    fn asset_tree_rejects_empty_source_and_dest() {
        expect_target_error(
            r#"
version = 1
ir = "solve"
name = "empty-bundle"
[[files]]
path = "a.txt"
template = "a.jinja"
[[assets]]
source = ""
dest = "schemas/"
"#,
            "source must not be empty",
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
source = "schemas"
dest = ""
"#,
            "dest",
        );
    }

    #[test]
    fn target_asset_relative_path_is_portable_and_nested() {
        let root = Path::new("target/assets");
        let path = root.join("schémas").join("nested").join("model.xsd");

        assert_eq!(
            target_asset_relative_path(root, &path).expect("nested UTF-8 asset path"),
            "schémas/nested/model.xsd"
        );
    }

    #[test]
    fn target_asset_relative_path_rejects_paths_outside_root() {
        let error = target_asset_relative_path(
            Path::new("target/assets"),
            Path::new("target/templates/model.jinja"),
        )
        .expect_err("asset outside its declared root must fail");

        assert!(
            error.to_string().contains("is not beneath source root"),
            "unexpected error: {error:#}"
        );
    }
}
