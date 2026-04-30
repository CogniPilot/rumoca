#![allow(clippy::excessive_nesting)]

use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use crate::merge::collect_model_names;
use crate::parse::parse_files_parallel_lenient;

const PROJECT_CONFIG_VERSION: u32 = 1;
const MODEL_CONFIG_DIR: &str = "models";
const MODEL_BY_ID_DIR: &str = "by-id";
const MODEL_IDENTITY_FILE: &str = "identity.toml";
const MODEL_SIMULATION_FILE: &str = "simulation.toml";
const MODEL_VIEWS_FILE: &str = "views.toml";

#[derive(Debug, Clone, Default)]
pub struct ProjectConfig {
    pub workspace_root: PathBuf,
    pub rumoca_dir: PathBuf,
    pub config_path: PathBuf,
    pub data: ProjectConfigFile,
    pub model_identities: BTreeMap<String, ModelIdentityRecord>,
    pub diagnostics: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct ProjectConfigFile {
    pub version: u32,
    pub project: ProjectMeta,
    pub source_roots: SourceRootsConfig,
    pub simulation: SimulationConfig,
    pub plot: PlotConfig,
}

impl Default for ProjectConfigFile {
    fn default() -> Self {
        Self {
            version: PROJECT_CONFIG_VERSION,
            project: ProjectMeta::default(),
            source_roots: SourceRootsConfig::default(),
            simulation: SimulationConfig::default(),
            plot: PlotConfig::default(),
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ProjectMeta {
    pub name: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct SourceRootsConfig {
    pub paths: Vec<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct SimulationConfig {
    pub defaults: SimulationDefaults,
    pub models: BTreeMap<String, SimulationModelOverride>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct SimulationDefaults {
    pub solver: Option<String>,
    pub t_end: Option<f64>,
    pub dt: Option<f64>,
    pub output_dir: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct SimulationModelOverride {
    pub solver: Option<String>,
    pub t_end: Option<f64>,
    pub dt: Option<f64>,
    pub output_dir: Option<String>,
    pub source_root_overrides: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct EffectiveSimulationConfig {
    pub solver: String,
    pub t_end: f64,
    pub dt: Option<f64>,
    pub output_dir: String,
    pub source_root_paths: Vec<String>,
}

impl Default for EffectiveSimulationConfig {
    fn default() -> Self {
        Self {
            solver: "auto".to_string(),
            t_end: 10.0,
            dt: None,
            output_dir: String::new(),
            source_root_paths: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectSimulationSnapshot {
    pub preset: Option<EffectiveSimulationPreset>,
    pub defaults: EffectiveSimulationConfig,
    pub effective: EffectiveSimulationConfig,
    pub diagnostics: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EffectiveSimulationPreset {
    pub solver: String,
    pub t_end: f64,
    pub dt: Option<f64>,
    pub output_dir: String,
    pub source_root_overrides: Vec<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct PlotConfig {
    pub include: Vec<String>,
    pub defaults: PlotDefaults,
    pub models: BTreeMap<String, PlotModelConfig>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct PlotDefaults {
    pub initial_selection: Option<String>,
    pub show_sidebar: Option<bool>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct PlotModelConfig {
    pub views: Vec<PlotViewConfig>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct PlotViewConfig {
    pub id: String,
    pub title: String,
    #[serde(rename = "type")]
    pub view_type: String,
    pub x: Option<String>,
    pub y: Vec<String>,
    pub script: Option<String>,
    pub script_path: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ModelIdentityRecord {
    pub version: u32,
    pub uuid: String,
    pub qualified_name: String,
    pub class_name: String,
    pub source_file: Option<String>,
    pub class_type: Option<String>,
    pub signature: Option<String>,
    pub aliases: Vec<String>,
    pub previous_source_files: Vec<String>,
    pub last_seen_unix_ms: u64,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ProjectGcCandidate {
    pub uuid: String,
    pub qualified_name: String,
    pub path: String,
    pub reason: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ProjectGcReport {
    pub dry_run: bool,
    pub scanned_sidecars: usize,
    pub parsed_model_files: usize,
    pub parse_failures: usize,
    pub discovered_models: usize,
    pub removed_sidecars: usize,
    pub candidates: Vec<ProjectGcCandidate>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ProjectResyncRemap {
    pub from_model: String,
    pub to_model: String,
    pub reason: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ProjectResyncReport {
    pub dry_run: bool,
    pub prune_orphans: bool,
    pub parsed_model_files: usize,
    pub parse_failures: usize,
    pub discovered_models: usize,
    pub remapped_models: usize,
    pub applied_move_hints: usize,
    pub removed_orphans: usize,
    pub remaps: Vec<ProjectResyncRemap>,
    pub orphans: Vec<ProjectGcCandidate>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ProjectFileMoveHint {
    pub old_path: String,
    pub new_path: String,
}

impl ProjectConfig {
    pub fn discover(workspace_root: &Path) -> Result<Option<Self>> {
        let config_path = workspace_root.join(".rumoca").join("project.toml");
        if !config_path.is_file() {
            return Ok(None);
        }
        Self::load_from_path(workspace_root, &config_path).map(Some)
    }

    pub fn load_or_default(workspace_root: &Path) -> Result<Self> {
        match Self::discover(workspace_root)? {
            Some(config) => Ok(config),
            None => Ok(Self {
                workspace_root: workspace_root.to_path_buf(),
                rumoca_dir: workspace_root.join(".rumoca"),
                config_path: workspace_root.join(".rumoca").join("project.toml"),
                data: ProjectConfigFile::default(),
                model_identities: BTreeMap::new(),
                diagnostics: Vec::new(),
            }),
        }
    }

    pub fn save(&self) -> Result<()> {
        if let Some(parent) = self.config_path.parent() {
            fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
        }
        self.write_model_scoped_configs()?;

        // Keep project.toml as a compact workspace-level config.
        // Per-model settings are stored under `.rumoca/models/by-id/<uuid>/`.
        let mut root_data = self.data.clone();
        root_data.simulation.models.clear();
        root_data.plot.models.clear();
        let mut root_value =
            toml::Value::try_from(&root_data).context("serialize .rumoca/project.toml value")?;
        strip_model_tables_from_root_value(&mut root_value);
        let text = toml::to_string_pretty(&root_value).context("serialize .rumoca/project.toml")?;
        fs::write(&self.config_path, text)
            .with_context(|| format!("write {}", self.config_path.display()))
    }

    pub fn model_override(&self, model: &str) -> Option<&SimulationModelOverride> {
        self.data.simulation.models.get(model)
    }

    pub fn set_model_override(&mut self, model: &str, model_override: SimulationModelOverride) {
        self.data
            .simulation
            .models
            .insert(model.to_string(), model_override);
    }

    pub fn remove_model_override(&mut self, model: &str) -> bool {
        self.data.simulation.models.remove(model).is_some()
    }

    pub fn defaults_with_fallback(
        &self,
        fallback: &EffectiveSimulationConfig,
    ) -> EffectiveSimulationConfig {
        let mut effective = fallback.clone();
        self.apply_defaults(&mut effective);
        let project_source_roots = self.resolve_project_source_root_paths();
        if !project_source_roots.is_empty() {
            effective.source_root_paths = project_source_roots;
        }
        effective
    }

    pub fn effective_for_model(
        &self,
        model: &str,
        fallback: &EffectiveSimulationConfig,
    ) -> EffectiveSimulationConfig {
        let mut effective = self.defaults_with_fallback(fallback);
        if let Some(model_override) = self.data.simulation.models.get(model) {
            self.apply_model_override(&mut effective, model_override);
        }
        effective
    }

    pub fn resolve_project_source_root_paths(&self) -> Vec<String> {
        resolve_and_dedup_paths(&self.workspace_root, &self.data.source_roots.paths)
    }

    pub fn resolve_all_source_root_paths(&self) -> Vec<String> {
        let mut merged = self.resolve_project_source_root_paths();
        let mut seen: HashSet<String> = merged
            .iter()
            .map(|path| {
                fs::canonicalize(path)
                    .map(|value| value.to_string_lossy().to_string())
                    .unwrap_or_else(|_| path.clone())
            })
            .collect();

        for model_override in self.data.simulation.models.values() {
            for path in
                resolve_and_dedup_paths(&self.workspace_root, &model_override.source_root_overrides)
            {
                let key = fs::canonicalize(&path)
                    .map(|value| value.to_string_lossy().to_string())
                    .unwrap_or_else(|_| path.clone());
                if seen.insert(key) {
                    merged.push(path);
                }
            }
        }

        merged
    }

    pub fn simulation_snapshot_for_model(
        &self,
        model: &str,
        fallback: &EffectiveSimulationConfig,
    ) -> ProjectSimulationSnapshot {
        let defaults = self.defaults_with_fallback(fallback);
        let effective = self.effective_for_model(model, fallback);
        let preset = self
            .model_override(model)
            .map(|model_override| self.preset_from_override(model_override, &defaults));
        ProjectSimulationSnapshot {
            preset,
            defaults,
            effective,
            diagnostics: self.diagnostics.clone(),
        }
    }

    fn apply_defaults(&self, effective: &mut EffectiveSimulationConfig) {
        let defaults = &self.data.simulation.defaults;
        if let Some(solver) = normalize_solver(defaults.solver.as_deref()) {
            effective.solver = solver.to_string();
        }
        if let Some(t_end) = defaults.t_end.filter(|v| v.is_finite() && *v > 0.0) {
            effective.t_end = t_end;
        }
        effective.dt = normalize_dt(defaults.dt).or(effective.dt);
        if let Some(output_dir) = defaults.output_dir.as_deref() {
            effective.output_dir = output_dir.to_string();
        }
    }

    fn apply_model_override(
        &self,
        effective: &mut EffectiveSimulationConfig,
        model_override: &SimulationModelOverride,
    ) {
        if let Some(solver) = normalize_solver(model_override.solver.as_deref()) {
            effective.solver = solver.to_string();
        }
        if let Some(t_end) = model_override.t_end.filter(|v| v.is_finite() && *v > 0.0) {
            effective.t_end = t_end;
        }
        if let Some(dt) = normalize_dt(model_override.dt) {
            effective.dt = Some(dt);
        }
        if let Some(output_dir) = model_override.output_dir.as_deref() {
            effective.output_dir = output_dir.to_string();
        }
        let additional_source_root_paths =
            resolve_and_dedup_paths(&self.workspace_root, &model_override.source_root_overrides);
        if !additional_source_root_paths.is_empty() {
            let mut merged = effective.source_root_paths.clone();
            let mut seen: HashSet<String> = merged
                .iter()
                .map(|path| {
                    fs::canonicalize(path)
                        .map(|value| value.to_string_lossy().to_string())
                        .unwrap_or_else(|_| path.clone())
                })
                .collect();
            for path in additional_source_root_paths {
                let key = fs::canonicalize(&path)
                    .map(|value| value.to_string_lossy().to_string())
                    .unwrap_or_else(|_| path.clone());
                if seen.insert(key) {
                    merged.push(path);
                }
            }
            effective.source_root_paths = merged;
        }
    }

    fn preset_from_override(
        &self,
        model_override: &SimulationModelOverride,
        defaults: &EffectiveSimulationConfig,
    ) -> EffectiveSimulationPreset {
        let mut effective = defaults.clone();
        let mut override_copy = model_override.clone();
        override_copy.solver = normalize_solver_opt(override_copy.solver);
        override_copy.dt = normalize_dt_opt(override_copy.dt);
        if let Some(solver) = override_copy.solver.as_deref() {
            effective.solver = solver.to_string();
        }
        if let Some(t_end) = override_copy.t_end.filter(|v| v.is_finite() && *v > 0.0) {
            effective.t_end = t_end;
        }
        if let Some(dt) = override_copy.dt {
            effective.dt = Some(dt);
        }
        if let Some(output_dir) = override_copy.output_dir.as_deref() {
            effective.output_dir = output_dir.to_string();
        }
        let source_root_overrides = if override_copy.source_root_overrides.is_empty() {
            effective.source_root_paths
        } else {
            override_copy.source_root_overrides
        };
        EffectiveSimulationPreset {
            solver: effective.solver,
            t_end: effective.t_end,
            dt: effective.dt,
            output_dir: effective.output_dir,
            source_root_overrides,
        }
    }

    fn load_from_path(workspace_root: &Path, config_path: &Path) -> Result<Self> {
        let text = fs::read_to_string(config_path)
            .with_context(|| format!("read {}", config_path.display()))?;
        let value: toml::Value = toml::from_str(&text)
            .with_context(|| format!("parse TOML {}", config_path.display()))?;
        let mut diagnostics = validate_top_level_keys(&value);

        let mut data: ProjectConfigFile =
            toml::from_str(&text).with_context(|| format!("decode {}", config_path.display()))?;
        if data.version == 0 {
            data.version = PROJECT_CONFIG_VERSION;
        }

        let rumoca_dir = config_path
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| workspace_root.join(".rumoca"));

        let merged_plot = merge_plot_includes(&rumoca_dir, &data.plot, &mut diagnostics);
        // Only UUID sidecars under `.rumoca/models/by-id` are authoritative.
        data.simulation.models.clear();
        data.plot.models.clear();
        data.plot = merged_plot;
        let model_identities = load_model_scoped_configs(&rumoca_dir, &mut data, &mut diagnostics)?;

        Ok(Self {
            workspace_root: workspace_root.to_path_buf(),
            rumoca_dir,
            config_path: config_path.to_path_buf(),
            data,
            model_identities,
            diagnostics,
        })
    }

    #[allow(clippy::too_many_lines)]
    fn write_model_scoped_configs(&self) -> Result<()> {
        let models_root = self.rumoca_dir.join(MODEL_CONFIG_DIR);
        let by_id_root = models_root.join(MODEL_BY_ID_DIR);
        fs::create_dir_all(&models_root)
            .with_context(|| format!("create {}", models_root.display()))?;
        fs::create_dir_all(&by_id_root)
            .with_context(|| format!("create {}", by_id_root.display()))?;

        let now = current_unix_ms();
        let mut identities = self.model_identities.clone();
        let mut used_uuids = HashSet::<String>::new();
        let mut desired_uuids = HashSet::<String>::new();

        for model in collect_configured_model_names(&self.data) {
            let write_sim = self
                .data
                .simulation
                .models
                .get(&model)
                .map(|value| !simulation_model_override_is_empty(value))
                .unwrap_or(false);
            let write_plot = self
                .data
                .plot
                .models
                .get(&model)
                .map(|value| !plot_model_config_is_empty(value))
                .unwrap_or(false);
            if !write_sim && !write_plot {
                continue;
            }

            let model_uuid = find_identity_uuid_for_model(&identities, &model, &used_uuids)
                .unwrap_or_else(|| generate_unique_model_uuid(&identities, &used_uuids, &model));
            used_uuids.insert(model_uuid.clone());
            desired_uuids.insert(model_uuid.clone());

            let model_dir = by_id_root.join(&model_uuid);
            fs::create_dir_all(&model_dir)
                .with_context(|| format!("create {}", model_dir.display()))?;

            let mut identity =
                identities
                    .remove(&model_uuid)
                    .unwrap_or_else(|| ModelIdentityRecord {
                        version: 1,
                        uuid: model_uuid.clone(),
                        qualified_name: model.clone(),
                        class_name: class_name_from_qualified_name(&model),
                        source_file: None,
                        class_type: None,
                        signature: None,
                        aliases: Vec::new(),
                        previous_source_files: Vec::new(),
                        last_seen_unix_ms: now,
                    });

            if identity.qualified_name != model {
                if !identity.qualified_name.trim().is_empty() && identity.qualified_name != model {
                    push_unique_string(&mut identity.aliases, identity.qualified_name.clone());
                }
                identity.qualified_name = model.clone();
            }
            identity.version = 1;
            identity.uuid = model_uuid.clone();
            identity.class_name = class_name_from_qualified_name(&model);
            identity.last_seen_unix_ms = now;

            let identity_path = model_dir.join(MODEL_IDENTITY_FILE);
            let identity_text = toml::to_string_pretty(&identity)
                .with_context(|| format!("serialize {}", identity_path.display()))?;
            fs::write(&identity_path, identity_text)
                .with_context(|| format!("write {}", identity_path.display()))?;

            let sim_path = model_dir.join(MODEL_SIMULATION_FILE);
            if write_sim {
                if let Some(sim_override) = self.data.simulation.models.get(&model) {
                    let text = toml::to_string_pretty(sim_override)
                        .with_context(|| format!("serialize {}", sim_path.display()))?;
                    fs::write(&sim_path, text)
                        .with_context(|| format!("write {}", sim_path.display()))?;
                }
            } else if sim_path.is_file() {
                fs::remove_file(&sim_path)
                    .with_context(|| format!("remove {}", sim_path.display()))?;
            }

            let views_path = model_dir.join(MODEL_VIEWS_FILE);
            if write_plot {
                if let Some(plot_cfg) = self.data.plot.models.get(&model) {
                    let text = toml::to_string_pretty(plot_cfg)
                        .with_context(|| format!("serialize {}", views_path.display()))?;
                    fs::write(&views_path, text)
                        .with_context(|| format!("write {}", views_path.display()))?;
                }
            } else if views_path.is_file() {
                fs::remove_file(&views_path)
                    .with_context(|| format!("remove {}", views_path.display()))?;
            }
        }

        for uuid in collect_uuid_dirs(&by_id_root)? {
            if desired_uuids.contains(&uuid) {
                continue;
            }
            let stale_dir = by_id_root.join(&uuid);
            remove_if_exists(&stale_dir.join(MODEL_SIMULATION_FILE))?;
            remove_if_exists(&stale_dir.join(MODEL_VIEWS_FILE))?;
            remove_if_exists(&stale_dir.join(MODEL_IDENTITY_FILE))?;
        }

        prune_empty_model_dirs(&models_root, Some(&by_id_root))?;
        Ok(())
    }
}

pub fn load_simulation_snapshot_for_model(
    workspace_root: &Path,
    model: &str,
    fallback: &EffectiveSimulationConfig,
) -> Result<ProjectSimulationSnapshot> {
    let config = ProjectConfig::load_or_default(workspace_root)?;
    Ok(config.simulation_snapshot_for_model(model, fallback))
}

pub fn write_model_simulation_preset(
    workspace_root: &Path,
    model: &str,
    mut model_override: SimulationModelOverride,
) -> Result<()> {
    model_override.solver = normalize_solver_opt(model_override.solver);
    model_override.dt = normalize_dt_opt(model_override.dt);
    if !model_override
        .t_end
        .map(|v| v.is_finite() && v > 0.0)
        .unwrap_or(true)
    {
        model_override.t_end = None;
    }
    let mut config = ProjectConfig::load_or_default(workspace_root)?;
    config.set_model_override(model, model_override);
    config.save()
}

pub fn clear_model_simulation_preset(workspace_root: &Path, model: &str) -> Result<()> {
    let mut config = ProjectConfig::load_or_default(workspace_root)?;
    config.remove_model_override(model);
    config.save()
}

pub fn load_plot_views_for_model(
    workspace_root: &Path,
    model: &str,
) -> Result<Vec<PlotViewConfig>> {
    let config = ProjectConfig::load_or_default(workspace_root)?;
    Ok(config
        .data
        .plot
        .models
        .get(model)
        .map(|cfg| cfg.views.clone())
        .unwrap_or_default())
}

pub fn write_plot_views_for_model(
    workspace_root: &Path,
    model: &str,
    mut views: Vec<PlotViewConfig>,
) -> Result<()> {
    for view in &mut views {
        view.id = view.id.trim().to_string();
        view.title = view.title.trim().to_string();
        view.view_type = view.view_type.trim().to_ascii_lowercase();
        if view.id.is_empty() {
            view.id = format!("view_{}", sanitize_identifier(&view.title));
        }
        if view.title.is_empty() {
            view.title = view.id.clone();
        }
        if !matches!(view.view_type.as_str(), "timeseries" | "scatter" | "3d") {
            view.view_type = "timeseries".to_string();
        }
        view.y = view
            .y
            .iter()
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect();
        if let Some(x) = &view.x {
            let trimmed = x.trim();
            view.x = if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            };
        }
        if let Some(script) = &view.script {
            let trimmed = script.trim();
            view.script = if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            };
        }
        if let Some(script_path) = &view.script_path {
            let trimmed = script_path.trim();
            view.script_path = if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            };
        }
    }

    let mut config = ProjectConfig::load_or_default(workspace_root)?;
    if views.is_empty() {
        config.data.plot.models.remove(model);
    } else {
        let entry = config
            .data
            .plot
            .models
            .entry(model.to_string())
            .or_default();
        entry.views = views;
    }
    config.save()
}

fn simulation_result_file_path(workspace_root: &Path, model: &str) -> PathBuf {
    workspace_root
        .join(".rumoca")
        .join("results")
        .join(format!("{}.json", sanitize_identifier(model)))
}

fn simulation_runs_dir(workspace_root: &Path) -> PathBuf {
    workspace_root.join(".rumoca").join("results").join("runs")
}

fn is_safe_result_id(id: &str) -> bool {
    !id.is_empty()
        && id
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-' | '.'))
}

pub fn load_last_simulation_result_for_model(
    workspace_root: &Path,
    model: &str,
) -> Result<Option<Value>> {
    let result_path = simulation_result_file_path(workspace_root, model);
    if !result_path.is_file() {
        return Ok(None);
    }
    let text = fs::read_to_string(&result_path)
        .with_context(|| format!("read {}", result_path.display()))?;
    let value: Value =
        serde_json::from_str(&text).with_context(|| format!("decode {}", result_path.display()))?;
    Ok(Some(value))
}

pub fn write_last_simulation_result_for_model(
    workspace_root: &Path,
    model: &str,
    payload: &Value,
    metrics: Option<&Value>,
) -> Result<()> {
    let result_path = simulation_result_file_path(workspace_root, model);
    if let Some(parent) = result_path.parent() {
        fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
    }
    let saved_at_unix_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0);
    let mut result_doc = json!({
        "version": 1,
        "model": model,
        "savedAtUnixMs": saved_at_unix_ms,
        "payload": payload,
    });
    if let Some(metrics) = metrics {
        result_doc["metrics"] = metrics.clone();
    }
    let text = serde_json::to_string_pretty(&result_doc)
        .context("serialize .rumoca/results simulation result")?;
    fs::write(&result_path, text).with_context(|| format!("write {}", result_path.display()))
}

pub fn write_simulation_run(
    workspace_root: &Path,
    model: &str,
    payload: &Value,
    metrics: Option<&Value>,
    views: Option<&Value>,
) -> Result<String> {
    let runs_dir = simulation_runs_dir(workspace_root);
    fs::create_dir_all(&runs_dir).with_context(|| format!("create {}", runs_dir.display()))?;
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0);
    let model_slug = sanitize_identifier(model);
    let mut run_id = format!("{now}_{model_slug}");
    let mut run_path = runs_dir.join(format!("{run_id}.json"));
    let mut suffix = 1usize;
    while run_path.is_file() {
        run_id = format!("{now}_{model_slug}_{suffix}");
        run_path = runs_dir.join(format!("{run_id}.json"));
        suffix += 1;
    }

    let mut run_doc = json!({
        "version": 1,
        "runId": run_id,
        "model": model,
        "savedAtUnixMs": now,
        "payload": payload,
    });
    if let Some(metrics) = metrics {
        run_doc["metrics"] = metrics.clone();
    }
    if let Some(views) = views {
        run_doc["views"] = views.clone();
    }

    let text = serde_json::to_string_pretty(&run_doc)
        .context("serialize .rumoca/results/runs simulation run")?;
    fs::write(&run_path, text).with_context(|| format!("write {}", run_path.display()))?;
    Ok(run_id)
}

pub fn load_simulation_run(workspace_root: &Path, run_id: &str) -> Result<Option<Value>> {
    if !is_safe_result_id(run_id) {
        return Ok(None);
    }
    let run_path = simulation_runs_dir(workspace_root).join(format!("{run_id}.json"));
    if !run_path.is_file() {
        return Ok(None);
    }
    let text =
        fs::read_to_string(&run_path).with_context(|| format!("read {}", run_path.display()))?;
    let value: Value =
        serde_json::from_str(&text).with_context(|| format!("decode {}", run_path.display()))?;
    Ok(Some(value))
}

pub fn gc_orphan_model_sidecars(workspace_root: &Path, dry_run: bool) -> Result<ProjectGcReport> {
    let mut config = ProjectConfig::load_or_default(workspace_root)?;
    let models_root = config.rumoca_dir.join(MODEL_CONFIG_DIR);
    let by_id_root = models_root.join(MODEL_BY_ID_DIR);

    let mut report = ProjectGcReport {
        dry_run,
        ..ProjectGcReport::default()
    };
    if !by_id_root.is_dir() {
        return Ok(report);
    }

    let (discovered_models, _, parsed_model_files, parse_failures) =
        discover_workspace_model_names(workspace_root)?;
    report.discovered_models = discovered_models.len();
    report.parsed_model_files = parsed_model_files;
    report.parse_failures = parse_failures;

    let (candidates, removals) = collect_orphan_sidecars(&by_id_root, &discovered_models)?;
    report.scanned_sidecars = report.scanned_sidecars.saturating_add(candidates.len());
    report.candidates = candidates;

    if dry_run {
        return Ok(report);
    }

    for (uuid, model_name, model_dir) in &removals {
        if model_dir.is_dir() {
            fs::remove_dir_all(model_dir)
                .with_context(|| format!("remove {}", model_dir.display()))?;
            report.removed_sidecars += 1;
        }
        config.model_identities.remove(uuid);
        config.data.simulation.models.remove(model_name);
        config.data.plot.models.remove(model_name);
    }
    prune_empty_model_dirs(&models_root, Some(&by_id_root))?;
    config.save()?;
    Ok(report)
}

pub fn resync_model_sidecars(
    workspace_root: &Path,
    dry_run: bool,
    prune_orphans: bool,
) -> Result<ProjectResyncReport> {
    resync_model_sidecars_with_known_models(workspace_root, &[], dry_run, prune_orphans)
}

pub fn resync_model_sidecars_with_known_models(
    workspace_root: &Path,
    known_models: &[String],
    dry_run: bool,
    prune_orphans: bool,
) -> Result<ProjectResyncReport> {
    resync_model_sidecars_with_move_hints(workspace_root, known_models, &[], dry_run, prune_orphans)
}

#[allow(clippy::too_many_lines)]
pub fn resync_model_sidecars_with_move_hints(
    workspace_root: &Path,
    known_models: &[String],
    moved_files: &[ProjectFileMoveHint],
    dry_run: bool,
    prune_orphans: bool,
) -> Result<ProjectResyncReport> {
    let mut config = ProjectConfig::load_or_default(workspace_root)?;
    let (discovered_models, source_by_model, parsed_model_files, parse_failures) =
        discover_workspace_model_names(workspace_root)?;
    let mut discovered_models = discovered_models;
    discovered_models.extend(known_models.iter().cloned());

    let mut report = ProjectResyncReport {
        dry_run,
        prune_orphans,
        parsed_model_files,
        parse_failures,
        discovered_models: discovered_models.len(),
        ..ProjectResyncReport::default()
    };
    if discovered_models.is_empty() {
        return Ok(report);
    }

    let mut class_to_models = HashMap::<String, Vec<String>>::new();
    for model in &discovered_models {
        class_to_models
            .entry(class_name_from_qualified_name(model))
            .or_default()
            .push(model.clone());
    }
    for entries in class_to_models.values_mut() {
        entries.sort();
    }

    let configured_models = collect_configured_model_names(&config.data);
    let mut remap = HashMap::<String, String>::new();
    let mut consumed_dest = HashSet::<String>::new();

    for model in configured_models {
        if discovered_models.contains(&model) {
            continue;
        }
        let class_name = class_name_from_qualified_name(&model);
        let candidates = class_to_models
            .get(&class_name)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .filter(|candidate| !consumed_dest.contains(candidate))
            .collect::<Vec<_>>();
        if candidates.len() != 1 {
            continue;
        }
        let destination = candidates[0].clone();
        consumed_dest.insert(destination.clone());
        remap.insert(model.clone(), destination.clone());
        report.remaps.push(ProjectResyncRemap {
            from_model: model,
            to_model: destination,
            reason: "unique class-name match".to_string(),
        });
    }

    let move_hint_remaps =
        remaps_from_move_hints(workspace_root, &config, moved_files, &source_by_model);
    report.applied_move_hints = move_hint_remaps.len();
    for (from, to, reason) in move_hint_remaps {
        if from == to || remap.contains_key(&from) {
            continue;
        }
        remap.insert(from.clone(), to.clone());
        report.remaps.push(ProjectResyncRemap {
            from_model: from,
            to_model: to,
            reason,
        });
    }
    report.remapped_models = remap.len();

    let mut changed = false;
    if !dry_run && !remap.is_empty() {
        remap_simulation_and_plot_keys(&mut config, &remap);
        remap_identity_records(&mut config, &remap);
        changed = true;
    }

    if !dry_run
        && update_identity_source_files(workspace_root, &mut config, &source_by_model, moved_files)
    {
        changed = true;
    }

    let by_id_root = config
        .rumoca_dir
        .join(MODEL_CONFIG_DIR)
        .join(MODEL_BY_ID_DIR);
    if by_id_root.is_dir() {
        let (orphans, removals) = collect_orphan_sidecars(&by_id_root, &discovered_models)?;
        report.orphans = orphans;

        if prune_orphans && !dry_run {
            for (uuid, model_name, model_dir) in &removals {
                if model_dir.is_dir() {
                    fs::remove_dir_all(model_dir)
                        .with_context(|| format!("remove {}", model_dir.display()))?;
                    report.removed_orphans += 1;
                }
                config.model_identities.remove(uuid);
                config.data.simulation.models.remove(model_name);
                config.data.plot.models.remove(model_name);
                changed = true;
            }
            prune_empty_model_dirs(&config.rumoca_dir.join(MODEL_CONFIG_DIR), Some(&by_id_root))?;
        }
    }

    if !dry_run && changed {
        config.save()?;
    }
    Ok(report)
}

fn remap_simulation_and_plot_keys(config: &mut ProjectConfig, remap: &HashMap<String, String>) {
    for (from, to) in remap {
        if let Some(sim_override) = config.data.simulation.models.remove(from) {
            config
                .data
                .simulation
                .models
                .entry(to.clone())
                .or_insert(sim_override);
        }
        if let Some(plot_cfg) = config.data.plot.models.remove(from) {
            config
                .data
                .plot
                .models
                .entry(to.clone())
                .or_insert(plot_cfg);
        }
    }
}

fn remap_identity_records(config: &mut ProjectConfig, remap: &HashMap<String, String>) {
    let now = current_unix_ms();
    for identity in config.model_identities.values_mut() {
        let from = identity.qualified_name.clone();
        let Some(to) = remap.get(&from).cloned() else {
            continue;
        };
        if from != to {
            push_unique_string(&mut identity.aliases, from);
            identity.qualified_name = to.clone();
            identity.class_name = class_name_from_qualified_name(&to);
            identity.last_seen_unix_ms = now;
        }
    }
}

#[allow(clippy::type_complexity)]
fn collect_orphan_sidecars(
    by_id_root: &Path,
    discovered_models: &HashSet<String>,
) -> Result<(Vec<ProjectGcCandidate>, Vec<(String, String, PathBuf)>)> {
    let mut candidates = Vec::<ProjectGcCandidate>::new();
    let mut removals = Vec::<(String, String, PathBuf)>::new();
    for uuid in collect_uuid_dirs(by_id_root)? {
        let model_dir = by_id_root.join(&uuid);
        let identity_path = model_dir.join(MODEL_IDENTITY_FILE);
        let identity = load_identity_record_from_path(&identity_path, &uuid)?;
        let mut qualified_name = identity.qualified_name.trim().to_string();
        if qualified_name.is_empty() {
            qualified_name = identity.class_name.trim().to_string();
        }

        if qualified_name.is_empty() {
            candidates.push(ProjectGcCandidate {
                uuid: uuid.clone(),
                qualified_name: String::new(),
                path: model_dir.to_string_lossy().to_string(),
                reason: "missing qualified model identity".to_string(),
            });
            removals.push((uuid, String::new(), model_dir));
            continue;
        }

        if !discovered_models.contains(&qualified_name) {
            candidates.push(ProjectGcCandidate {
                uuid: uuid.clone(),
                qualified_name: qualified_name.clone(),
                path: model_dir.to_string_lossy().to_string(),
                reason: "model not found in current workspace".to_string(),
            });
            removals.push((uuid, qualified_name, model_dir));
        }
    }
    Ok((candidates, removals))
}

fn load_identity_record_from_path(path: &Path, fallback_uuid: &str) -> Result<ModelIdentityRecord> {
    let mut identity = if path.is_file() {
        let text = fs::read_to_string(path).with_context(|| format!("read {}", path.display()))?;
        toml::from_str::<ModelIdentityRecord>(&text)
            .with_context(|| format!("parse {}", path.display()))?
    } else {
        ModelIdentityRecord::default()
    };

    if identity.version == 0 {
        identity.version = 1;
    }
    if identity.uuid.trim().is_empty() {
        identity.uuid = fallback_uuid.to_string();
    }
    if identity.qualified_name.trim().is_empty() && !identity.class_name.trim().is_empty() {
        identity.qualified_name = identity.class_name.clone();
    }
    if identity.class_name.trim().is_empty() && !identity.qualified_name.trim().is_empty() {
        identity.class_name = class_name_from_qualified_name(&identity.qualified_name);
    }
    Ok(identity)
}

#[allow(clippy::type_complexity)]
fn discover_workspace_model_names(
    workspace_root: &Path,
) -> Result<(HashSet<String>, HashMap<String, String>, usize, usize)> {
    let files = collect_workspace_modelica_files(workspace_root)?;
    let file_count = files.len();
    if files.is_empty() {
        return Ok((HashSet::new(), HashMap::new(), 0, 0));
    }
    let (successes, failures) = parse_files_parallel_lenient(&files);
    if successes.is_empty() {
        return Ok((HashSet::new(), HashMap::new(), file_count, failures.len()));
    }
    let mut models = HashSet::<String>::new();
    let mut source_by_model = HashMap::<String, String>::new();
    for (source_path, definition) in successes {
        for model in collect_model_names(&definition) {
            models.insert(model.clone());
            source_by_model
                .entry(model)
                .or_insert_with(|| normalize_path_text(workspace_root, &source_path));
        }
    }
    Ok((models, source_by_model, file_count, failures.len()))
}

fn normalize_path_text(workspace_root: &Path, path_text: &str) -> String {
    let path = PathBuf::from(path_text);
    normalize_path(workspace_root, &path)
}

fn normalize_path(workspace_root: &Path, path: &Path) -> String {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        workspace_root.join(path)
    };
    let stable = fs::canonicalize(&absolute).unwrap_or(absolute);
    stable.to_string_lossy().to_string()
}

fn remaps_from_move_hints(
    workspace_root: &Path,
    config: &ProjectConfig,
    moved_files: &[ProjectFileMoveHint],
    source_by_model: &HashMap<String, String>,
) -> Vec<(String, String, String)> {
    if moved_files.is_empty() {
        return Vec::new();
    }

    let mut discovered_by_source = HashMap::<String, Vec<String>>::new();
    for (model, source) in source_by_model {
        discovered_by_source
            .entry(source.clone())
            .or_default()
            .push(model.clone());
    }
    for models in discovered_by_source.values_mut() {
        models.sort();
        models.dedup();
    }

    let mut identities_by_source = HashMap::<String, Vec<String>>::new();
    for identity in config.model_identities.values() {
        let qualified = identity.qualified_name.trim();
        if qualified.is_empty() {
            continue;
        }
        if let Some(source) = identity.source_file.as_ref() {
            identities_by_source
                .entry(normalize_path_text(workspace_root, source))
                .or_default()
                .push(qualified.to_string());
        }
        for source in &identity.previous_source_files {
            identities_by_source
                .entry(normalize_path_text(workspace_root, source))
                .or_default()
                .push(qualified.to_string());
        }
    }
    for models in identities_by_source.values_mut() {
        models.sort();
        models.dedup();
    }

    let mut remaps = Vec::<(String, String, String)>::new();
    for hint in moved_files {
        if hint.old_path.trim().is_empty() || hint.new_path.trim().is_empty() {
            continue;
        }
        let old_path = normalize_path_text(workspace_root, &hint.old_path);
        let new_path = normalize_path_text(workspace_root, &hint.new_path);
        let Some(old_models) = identities_by_source.get(&old_path) else {
            continue;
        };
        let Some(new_models) = discovered_by_source.get(&new_path) else {
            continue;
        };
        if old_models.len() != 1 || new_models.len() != 1 {
            continue;
        }
        let from = old_models[0].clone();
        let to = new_models[0].clone();
        if from == to {
            continue;
        }
        remaps.push((
            from,
            to,
            format!("file move hint: {} -> {}", hint.old_path, hint.new_path),
        ));
    }
    remaps
}

fn update_identity_source_files(
    workspace_root: &Path,
    config: &mut ProjectConfig,
    source_by_model: &HashMap<String, String>,
    moved_files: &[ProjectFileMoveHint],
) -> bool {
    let mut changed = false;

    let mut move_new_by_old = HashMap::<String, String>::new();
    for hint in moved_files {
        if hint.old_path.trim().is_empty() || hint.new_path.trim().is_empty() {
            continue;
        }
        let old_norm = normalize_path_text(workspace_root, &hint.old_path);
        let new_norm = normalize_path_text(workspace_root, &hint.new_path);
        move_new_by_old.insert(old_norm, new_norm);
    }

    for identity in config.model_identities.values_mut() {
        let model = identity.qualified_name.trim();
        if model.is_empty() {
            continue;
        }

        let mut desired_source = source_by_model.get(model).cloned();
        if desired_source.is_none()
            && let Some(current) = identity.source_file.as_ref()
        {
            let current_norm = normalize_path_text(workspace_root, current);
            if let Some(new_source) = move_new_by_old.get(&current_norm) {
                desired_source = Some(new_source.clone());
            }
        }

        let Some(desired_source) = desired_source else {
            continue;
        };

        let current_norm = identity
            .source_file
            .as_ref()
            .map(|source| normalize_path_text(workspace_root, source));
        if current_norm.as_ref() == Some(&desired_source) {
            continue;
        }

        if let Some(old_source) = identity.source_file.clone() {
            push_unique_string(&mut identity.previous_source_files, old_source);
        }
        identity.source_file = Some(desired_source);
        identity.last_seen_unix_ms = current_unix_ms();
        changed = true;
    }

    changed
}

fn collect_workspace_modelica_files(workspace_root: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::<PathBuf>::new();
    let mut stack = vec![workspace_root.to_path_buf()];

    while let Some(dir) = stack.pop() {
        for entry in fs::read_dir(&dir).with_context(|| format!("read {}", dir.display()))? {
            let entry = entry.with_context(|| format!("read {}", dir.display()))?;
            let path = entry.path();
            let file_type = entry
                .file_type()
                .with_context(|| format!("stat {}", path.display()))?;
            if file_type.is_dir() {
                let name = entry.file_name().to_string_lossy().to_string();
                if name == ".git"
                    || name == "target"
                    || name == "node_modules"
                    || name == ".rumoca"
                    || name == ".venv"
                {
                    continue;
                }
                stack.push(path);
                continue;
            }
            if file_type.is_file()
                && path
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("mo"))
            {
                files.push(path);
            }
        }
    }

    files.sort();
    Ok(files)
}

fn class_name_from_qualified_name(model: &str) -> String {
    model
        .rsplit('.')
        .next()
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .unwrap_or("Model")
        .to_string()
}

fn push_unique_string(values: &mut Vec<String>, value: String) {
    if value.trim().is_empty() {
        return;
    }
    if !values.iter().any(|existing| existing == &value) {
        values.push(value);
    }
}

fn current_unix_ms() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_millis() as u64)
        .unwrap_or(0)
}

fn collect_configured_model_names(data: &ProjectConfigFile) -> Vec<String> {
    let mut names = HashSet::<String>::new();
    names.extend(data.simulation.models.keys().cloned());
    names.extend(data.plot.models.keys().cloned());
    let mut ordered = names.into_iter().collect::<Vec<_>>();
    ordered.sort();
    ordered
}

fn find_identity_uuid_for_model(
    identities: &BTreeMap<String, ModelIdentityRecord>,
    model_name: &str,
    used: &HashSet<String>,
) -> Option<String> {
    for (uuid, identity) in identities {
        if used.contains(uuid) {
            continue;
        }
        if identity.qualified_name == model_name {
            return Some(uuid.clone());
        }
    }

    for (uuid, identity) in identities {
        if used.contains(uuid) {
            continue;
        }
        if identity.aliases.iter().any(|alias| alias == model_name) {
            return Some(uuid.clone());
        }
    }

    let class_name = class_name_from_qualified_name(model_name);
    let class_matches = identities
        .iter()
        .filter(|(uuid, identity)| !used.contains(*uuid) && identity.class_name == class_name)
        .map(|(uuid, _)| uuid.clone())
        .collect::<Vec<_>>();
    if class_matches.len() == 1 {
        return class_matches.into_iter().next();
    }

    None
}

fn generate_unique_model_uuid(
    identities: &BTreeMap<String, ModelIdentityRecord>,
    used: &HashSet<String>,
    model_name: &str,
) -> String {
    let mut salt = 0u64;
    loop {
        let seed = format!("{}:{}:{}", current_unix_ms(), model_name, salt);
        let hash = blake3::hash(seed.as_bytes()).to_hex().to_string();
        let candidate = format!(
            "{}-{}-{}-{}-{}",
            &hash[0..8],
            &hash[8..12],
            &hash[12..16],
            &hash[16..20],
            &hash[20..32]
        );
        if !identities.contains_key(&candidate) && !used.contains(&candidate) {
            return candidate;
        }
        salt = salt.saturating_add(1);
    }
}

fn simulation_model_override_is_empty(value: &SimulationModelOverride) -> bool {
    value.solver.is_none()
        && value.t_end.is_none()
        && value.dt.is_none()
        && value.output_dir.is_none()
        && value.source_root_overrides.is_empty()
}

fn plot_model_config_is_empty(value: &PlotModelConfig) -> bool {
    value.views.is_empty()
}

fn collect_uuid_dirs(by_id_root: &Path) -> Result<Vec<String>> {
    if !by_id_root.is_dir() {
        return Ok(Vec::new());
    }
    let mut out = Vec::<String>::new();
    for entry in
        fs::read_dir(by_id_root).with_context(|| format!("read {}", by_id_root.display()))?
    {
        let entry = entry.with_context(|| format!("read {}", by_id_root.display()))?;
        let file_type = entry
            .file_type()
            .with_context(|| format!("stat {}", entry.path().display()))?;
        if !file_type.is_dir() {
            continue;
        }
        let name = entry.file_name().to_string_lossy().trim().to_string();
        if !name.is_empty() {
            out.push(name);
        }
    }
    out.sort();
    Ok(out)
}

fn remove_if_exists(path: &Path) -> Result<()> {
    if path.is_file() {
        fs::remove_file(path).with_context(|| format!("remove {}", path.display()))?;
    }
    Ok(())
}

fn prune_empty_model_dirs(models_root: &Path, keep_root: Option<&Path>) -> Result<()> {
    if !models_root.is_dir() {
        return Ok(());
    }

    let mut dirs = Vec::<PathBuf>::new();
    let mut stack = vec![models_root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        dirs.push(dir.clone());
        for entry in fs::read_dir(&dir).with_context(|| format!("read {}", dir.display()))? {
            let entry = entry.with_context(|| format!("read {}", dir.display()))?;
            let file_type = entry
                .file_type()
                .with_context(|| format!("stat {}", entry.path().display()))?;
            if file_type.is_dir() {
                stack.push(entry.path());
            }
        }
    }

    dirs.sort_by_key(|path| std::cmp::Reverse(path.components().count()));
    for dir in dirs {
        if dir == models_root {
            continue;
        }
        if let Some(keep_root) = keep_root
            && dir == keep_root
        {
            continue;
        }
        if fs::read_dir(&dir)
            .with_context(|| format!("read {}", dir.display()))?
            .next()
            .is_none()
        {
            let _ = fs::remove_dir(&dir);
        }
    }
    Ok(())
}

#[allow(clippy::too_many_lines)]
fn load_model_scoped_configs(
    rumoca_dir: &Path,
    data: &mut ProjectConfigFile,
    diagnostics: &mut Vec<String>,
) -> Result<BTreeMap<String, ModelIdentityRecord>> {
    let models_root = rumoca_dir.join(MODEL_CONFIG_DIR);
    let by_id_root = models_root.join(MODEL_BY_ID_DIR);
    let mut identities = BTreeMap::<String, ModelIdentityRecord>::new();

    if !models_root.is_dir() {
        return Ok(identities);
    }

    for uuid in collect_uuid_dirs(&by_id_root)? {
        let model_dir = by_id_root.join(&uuid);
        let identity_path = model_dir.join(MODEL_IDENTITY_FILE);
        let mut identity = if identity_path.is_file() {
            match fs::read_to_string(&identity_path) {
                Ok(text) => match toml::from_str::<ModelIdentityRecord>(&text) {
                    Ok(mut value) => {
                        if value.version == 0 {
                            value.version = 1;
                        }
                        if value.uuid.trim().is_empty() {
                            value.uuid = uuid.clone();
                        }
                        value
                    }
                    Err(error) => {
                        diagnostics.push(format!(
                            "Failed to parse model identity '{}': {}",
                            identity_path.display(),
                            error
                        ));
                        ModelIdentityRecord::default()
                    }
                },
                Err(error) => {
                    diagnostics.push(format!(
                        "Failed to read model identity '{}': {}",
                        identity_path.display(),
                        error
                    ));
                    ModelIdentityRecord::default()
                }
            }
        } else {
            ModelIdentityRecord::default()
        };
        identity.uuid = uuid.clone();
        identity.version = 1;

        if identity.qualified_name.trim().is_empty() && !identity.class_name.trim().is_empty() {
            identity.qualified_name = identity.class_name.clone();
        }
        if identity.class_name.trim().is_empty() && !identity.qualified_name.trim().is_empty() {
            identity.class_name = class_name_from_qualified_name(&identity.qualified_name);
        }

        let model_name = identity.qualified_name.trim().to_string();
        if model_name.is_empty() {
            diagnostics.push(format!(
                "Ignoring model-scoped config '{}': missing qualified_name in {}",
                model_dir.display(),
                identity_path.display()
            ));
            continue;
        }

        let sim_path = model_dir.join(MODEL_SIMULATION_FILE);
        if sim_path.is_file() {
            match fs::read_to_string(&sim_path) {
                Ok(text) => match parse_simulation_model_file(&text) {
                    Ok(value) => {
                        if !simulation_model_override_is_empty(&value) {
                            data.simulation.models.insert(model_name.clone(), value);
                        }
                    }
                    Err(error) => diagnostics.push(format!(
                        "Failed to parse model simulation config '{}': {}",
                        sim_path.display(),
                        error
                    )),
                },
                Err(error) => diagnostics.push(format!(
                    "Failed to read model simulation config '{}': {}",
                    sim_path.display(),
                    error
                )),
            }
        }

        let views_path = model_dir.join(MODEL_VIEWS_FILE);
        if views_path.is_file() {
            match fs::read_to_string(&views_path) {
                Ok(text) => match parse_plot_model_file(&text) {
                    Ok(value) => {
                        if !plot_model_config_is_empty(&value) {
                            data.plot.models.insert(model_name.clone(), value);
                        }
                    }
                    Err(error) => diagnostics.push(format!(
                        "Failed to parse model plot config '{}': {}",
                        views_path.display(),
                        error
                    )),
                },
                Err(error) => diagnostics.push(format!(
                    "Failed to read model plot config '{}': {}",
                    views_path.display(),
                    error
                )),
            }
        }

        identities.insert(uuid, identity);
    }

    Ok(identities)
}

fn parse_simulation_model_file(text: &str) -> Result<SimulationModelOverride> {
    toml::from_str::<SimulationModelOverride>(text).map_err(Into::into)
}

fn parse_plot_model_file(text: &str) -> Result<PlotModelConfig> {
    toml::from_str::<PlotModelConfig>(text).map_err(Into::into)
}

fn sanitize_identifier(input: &str) -> String {
    let mut out = String::new();
    for c in input.chars() {
        if c.is_ascii_alphanumeric() || c == '_' {
            out.push(c.to_ascii_lowercase());
        } else if c.is_ascii_whitespace() || c == '-' {
            out.push('_');
        }
    }
    if out.is_empty() {
        "panel".to_string()
    } else {
        out
    }
}

fn merge_plot_includes(
    base_dir: &Path,
    root: &PlotConfig,
    diagnostics: &mut Vec<String>,
) -> PlotConfig {
    let mut merged = PlotConfig::default();
    let mut include_paths = root.include.clone();
    include_paths.sort();
    for include in include_paths {
        let include_path = base_dir.join(&include);
        match fs::read_to_string(&include_path) {
            Ok(text) => match toml::from_str::<PlotConfig>(&text) {
                Ok(fragment) => merge_plot_section(&mut merged, fragment),
                Err(error) => diagnostics.push(format!(
                    "Failed to parse plot include '{}': {}",
                    include_path.display(),
                    error
                )),
            },
            Err(error) => diagnostics.push(format!(
                "Failed to read plot include '{}': {}",
                include_path.display(),
                error
            )),
        }
    }
    merge_plot_section(&mut merged, root.clone());
    merged
}

fn merge_plot_section(base: &mut PlotConfig, fragment: PlotConfig) {
    if fragment.defaults.initial_selection.is_some() {
        base.defaults.initial_selection = fragment.defaults.initial_selection;
    }
    if fragment.defaults.show_sidebar.is_some() {
        base.defaults.show_sidebar = fragment.defaults.show_sidebar;
    }
    for (model, config) in fragment.models {
        base.models.insert(model, config);
    }
}

fn normalize_solver(raw: Option<&str>) -> Option<&'static str> {
    let lowered = raw?.trim().to_ascii_lowercase();
    match lowered.as_str() {
        "auto" => Some("auto"),
        "bdf" => Some("bdf"),
        "rk-like" => Some("rk-like"),
        _ => None,
    }
}

fn normalize_solver_opt(value: Option<String>) -> Option<String> {
    match value
        .as_deref()
        .map(str::trim)
        .map(str::to_ascii_lowercase)
        .as_deref()
    {
        Some("auto") => Some("auto".to_string()),
        Some("bdf") => Some("bdf".to_string()),
        Some("rk-like") => Some("rk-like".to_string()),
        _ => None,
    }
}

fn normalize_dt(raw: Option<f64>) -> Option<f64> {
    raw.filter(|v| v.is_finite() && *v > 0.0)
}

fn normalize_dt_opt(raw: Option<f64>) -> Option<f64> {
    normalize_dt(raw)
}

fn validate_top_level_keys(value: &toml::Value) -> Vec<String> {
    let allowed: HashSet<&'static str> =
        HashSet::from(["version", "project", "source_roots", "simulation", "plot"]);
    let mut diagnostics = Vec::new();
    let Some(table) = value.as_table() else {
        diagnostics.push("Expected TOML table at top-level in .rumoca/project.toml".to_string());
        return diagnostics;
    };
    for key in table.keys() {
        if !allowed.contains(key.as_str()) {
            diagnostics.push(format!(
                "Unknown top-level key in .rumoca/project.toml: '{key}'"
            ));
        }
    }
    diagnostics
}

fn resolve_and_dedup_paths(workspace_root: &Path, paths: &[String]) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut out = Vec::new();
    for path in paths {
        let trimmed = path.trim();
        if trimmed.is_empty() {
            continue;
        }
        let resolved = resolve_path(workspace_root, trimmed);
        let key = fs::canonicalize(&resolved)
            .map(|v| v.to_string_lossy().to_string())
            .unwrap_or_else(|_| resolved.to_string_lossy().to_string());
        if seen.insert(key) {
            out.push(resolved.to_string_lossy().to_string());
        }
    }
    out
}

fn resolve_path(workspace_root: &Path, raw: &str) -> PathBuf {
    let path = Path::new(raw);
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        workspace_root.join(path)
    }
}

fn strip_model_tables_from_root_value(value: &mut toml::Value) {
    let Some(root) = value.as_table_mut() else {
        return;
    };

    if let Some(simulation) = root
        .get_mut("simulation")
        .and_then(toml::Value::as_table_mut)
    {
        simulation.remove("models");
    }
    if let Some(plot) = root.get_mut("plot").and_then(toml::Value::as_table_mut) {
        plot.remove("models");
    }
}

#[cfg(test)]
#[path = "project_config_tests.rs"]
mod project_config_tests;
