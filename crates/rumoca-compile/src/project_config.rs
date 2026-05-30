use std::collections::{BTreeMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, anyhow};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use crate::merge::collect_model_names;
use crate::parse::parse_files_parallel_lenient;

const RUMOCA_TASK_FILE_VERSION: &str = "1";
const GENERATED_RESULTS_DIR: &str = ".rumoca/results";

#[derive(Debug, Clone, Default)]
pub struct ProjectConfig {
    workspace_root: PathBuf,
    configs: BTreeMap<String, ColocatedModelConfig>,
    pub diagnostics: Vec<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ProjectConfigFile {
    /// `[rumoca]` marker section: authoritative task-file declaration.
    pub rumoca: RumocaTaskMarker,
    pub source_roots: Vec<String>,
    pub model: ModelConfig,
    pub sim: SimulationModelOverride,
    pub codegen: CodegenConfig,
    pub plot: PlotModelConfig,
    pub viewer: ScenarioViewerConfig,
}

/// `[rumoca]` marker section identifying a Rumoca task file. The filename
/// convention (`rum.toml` / `rum.<profile>.toml`) is the discovery hook; this
/// section is the authoritative declaration.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct RumocaTaskMarker {
    /// Task-file schema version (currently `"1"`).
    pub version: String,
    /// Declared task kind.
    pub task: ProjectTask,
}

impl Default for RumocaTaskMarker {
    fn default() -> Self {
        Self {
            version: RUMOCA_TASK_FILE_VERSION.to_string(),
            task: ProjectTask::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ProjectTask {
    #[default]
    Simulate,
    Codegen,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ModelConfig {
    pub file: Option<String>,
    pub name: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct CodegenConfig {
    pub target: Option<String>,
    pub output_dir: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ScenarioViewerConfig {
    pub mode: Option<ScenarioViewerMode>,
    pub prefer_external: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ScenarioViewerMode {
    ResultsPanel,
    ExternalWeb,
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

#[derive(Debug, Clone)]
pub(crate) struct ColocatedModelConfig {
    path: PathBuf,
    data: ProjectConfigFile,
}

impl ProjectConfig {
    pub fn discover(workspace_root: &Path) -> Result<Option<Self>> {
        let config = Self::load_or_default(workspace_root)?;
        if config.configs.is_empty() {
            Ok(None)
        } else {
            Ok(Some(config))
        }
    }

    pub fn load_or_default(workspace_root: &Path) -> Result<Self> {
        let mut diagnostics = Vec::new();
        let mut configs = BTreeMap::<String, ColocatedModelConfig>::new();
        for path in collect_workspace_scenario_files(workspace_root)? {
            let text = match fs::read_to_string(&path) {
                Ok(text) => text,
                Err(error) => {
                    diagnostics.push(format!(
                        "Failed to read config '{}': {error}",
                        path.display()
                    ));
                    continue;
                }
            };
            if !text_looks_like_rumoca_scenario_config(&text) {
                continue;
            }
            let raw = match toml::from_str::<toml::Value>(&text) {
                Ok(raw) => raw,
                Err(error) => {
                    diagnostics.push(format!(
                        "Failed to parse TOML config '{}': {error}",
                        path.display()
                    ));
                    continue;
                }
            };
            if !looks_like_rumoca_scenario_config(&raw) {
                continue;
            }
            let data = match toml::from_str::<ProjectConfigFile>(&text) {
                Ok(data) => data,
                Err(error) => {
                    diagnostics.push(format!(
                        "Failed to parse config '{}': {error}",
                        path.display()
                    ));
                    continue;
                }
            };
            let Some(model_name) = configured_model_name(&data, &path) else {
                continue;
            };
            if data.rumoca.task != ProjectTask::Simulate {
                continue;
            }
            configs.insert(model_name, ColocatedModelConfig { path, data });
        }
        Ok(Self {
            workspace_root: workspace_root.to_path_buf(),
            configs,
            diagnostics,
        })
    }

    pub fn save(&self) -> Result<()> {
        for config in self.configs.values() {
            write_colocated_config_file(&config.path, &config.data)?;
        }
        Ok(())
    }

    pub fn model_override(&self, model: &str) -> Option<&SimulationModelOverride> {
        self.configs.get(model).map(|config| &config.data.sim)
    }

    pub fn set_model_override(&mut self, model: &str, model_override: SimulationModelOverride) {
        let path = self
            .configs
            .get(model)
            .map(|config| config.path.clone())
            .unwrap_or_else(|| default_config_path_for_model(&self.workspace_root, model));
        let entry = self
            .configs
            .entry(model.to_string())
            .or_insert_with(|| ColocatedModelConfig {
                path,
                data: ProjectConfigFile {
                    model: ModelConfig {
                        name: Some(model.to_string()),
                        file: None,
                    },
                    ..ProjectConfigFile::default()
                },
            });
        entry.data.model.name = Some(model.to_string());
        entry.data.sim = model_override;
    }

    pub fn remove_model_override(&mut self, model: &str) -> bool {
        let Some(config) = self.configs.get_mut(model) else {
            return false;
        };
        let changed = !simulation_model_override_is_empty(&config.data.sim);
        config.data.sim = SimulationModelOverride::default();
        changed
    }

    pub fn defaults_with_fallback(
        &self,
        fallback: &EffectiveSimulationConfig,
    ) -> EffectiveSimulationConfig {
        fallback.clone()
    }

    pub fn effective_for_model(
        &self,
        model: &str,
        fallback: &EffectiveSimulationConfig,
    ) -> EffectiveSimulationConfig {
        let mut effective = self.defaults_with_fallback(fallback);
        if let Some(config) = self.configs.get(model) {
            apply_model_config(&self.workspace_root, &mut effective, config);
        }
        effective
    }

    pub fn resolve_project_source_root_paths(&self) -> Vec<String> {
        Vec::new()
    }

    pub fn resolve_all_source_root_paths(&self) -> Vec<String> {
        let mut seen = HashSet::new();
        let mut paths = Vec::new();
        for config in self.configs.values() {
            let base = config.path.parent().unwrap_or(&self.workspace_root);
            for path in &config.data.source_roots {
                push_resolved_path(base, path, &mut seen, &mut paths);
            }
            for path in &config.data.sim.source_root_overrides {
                push_resolved_path(base, path, &mut seen, &mut paths);
            }
        }
        paths
    }

    pub fn simulation_snapshot_for_model(
        &self,
        model: &str,
        fallback: &EffectiveSimulationConfig,
    ) -> ProjectSimulationSnapshot {
        let defaults = self.defaults_with_fallback(fallback);
        let effective = self.effective_for_model(model, fallback);
        let preset = self.configs.get(model).and_then(|config| {
            if simulation_model_override_is_empty(&config.data.sim) {
                None
            } else {
                Some(preset_from_override(&config.data.sim, &defaults))
            }
        });
        ProjectSimulationSnapshot {
            preset,
            defaults,
            effective,
            diagnostics: self.diagnostics.clone(),
        }
    }
}

/// Rumoca task-file naming convention: `rum.toml` (default) or
/// `rum.<profile>.toml` (named profile). The filename is the discovery hook;
/// the `[rumoca]` marker section is the authoritative declaration.
pub fn is_rumoca_task_filename(name: &str) -> bool {
    let name = name.to_ascii_lowercase();
    name.starts_with("rum.") && name.ends_with(".toml")
}

fn text_looks_like_rumoca_scenario_config(text: &str) -> bool {
    text.contains("[rumoca]")
}

fn looks_like_rumoca_scenario_config(value: &toml::Value) -> bool {
    value
        .as_table()
        .is_some_and(|table| table.contains_key("rumoca"))
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
        .map(|value| value.is_finite() && value > 0.0)
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
        .configs
        .get(model)
        .map(|cfg| cfg.data.plot.views.clone())
        .unwrap_or_default())
}

pub fn write_plot_views_for_model(
    workspace_root: &Path,
    model: &str,
    mut views: Vec<PlotViewConfig>,
) -> Result<()> {
    normalize_plot_views(&mut views);
    let mut config = ProjectConfig::load_or_default(workspace_root)?;
    let path = config
        .configs
        .get(model)
        .map(|config| config.path.clone())
        .unwrap_or_else(|| default_config_path_for_model(workspace_root, model));
    let entry = config
        .configs
        .entry(model.to_string())
        .or_insert_with(|| ColocatedModelConfig {
            path,
            data: ProjectConfigFile {
                model: ModelConfig {
                    name: Some(model.to_string()),
                    file: None,
                },
                ..ProjectConfigFile::default()
            },
        });
    entry.data.model.name = Some(model.to_string());
    entry.data.plot.views = views;
    config.save()
}

fn simulation_result_file_path(workspace_root: &Path, model: &str) -> PathBuf {
    workspace_root
        .join(GENERATED_RESULTS_DIR)
        .join(format!("{}.json", sanitize_identifier(model)))
}

fn simulation_runs_dir(workspace_root: &Path) -> PathBuf {
    workspace_root.join(GENERATED_RESULTS_DIR).join("runs")
}

fn is_safe_result_id(id: &str) -> bool {
    !id.is_empty()
        && !id.starts_with('.')
        && !id.contains("..")
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
        .map(|duration| duration.as_millis() as u64)
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
        .context("serialize generated simulation result")?;
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
        .map(|duration| duration.as_millis() as u64)
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

    let text =
        serde_json::to_string_pretty(&run_doc).context("serialize generated simulation run")?;
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

fn configured_model_name(data: &ProjectConfigFile, path: &Path) -> Option<String> {
    data.model
        .name
        .as_deref()
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .map(str::to_string)
        .or_else(|| {
            data.model
                .file
                .as_deref()
                .and_then(|file| Path::new(file).file_stem())
                .and_then(|stem| stem.to_str())
                .map(str::to_string)
        })
        .or_else(|| {
            if simulation_model_override_is_empty(&data.sim)
                && plot_model_config_is_empty(&data.plot)
            {
                None
            } else {
                path.file_stem()?.to_str().map(str::to_string)
            }
        })
}

fn apply_model_config(
    workspace_root: &Path,
    effective: &mut EffectiveSimulationConfig,
    config: &ColocatedModelConfig,
) {
    let sim = &config.data.sim;
    if let Some(solver) = normalize_solver(sim.solver.as_deref()) {
        effective.solver = solver.to_string();
    }
    if let Some(t_end) = sim.t_end.filter(|value| value.is_finite() && *value > 0.0) {
        effective.t_end = t_end;
    }
    if let Some(dt) = normalize_dt(sim.dt) {
        effective.dt = Some(dt);
    }
    if let Some(output_dir) = sim.output_dir.as_deref() {
        effective.output_dir = output_dir.to_string();
    }

    let base = config.path.parent().unwrap_or(workspace_root);
    let mut seen = HashSet::new();
    let mut paths = Vec::new();
    for existing in &effective.source_root_paths {
        let key = fs::canonicalize(existing)
            .map(|value| value.to_string_lossy().to_string())
            .unwrap_or_else(|_| existing.clone());
        if seen.insert(key) {
            paths.push(existing.clone());
        }
    }
    for path in &config.data.source_roots {
        push_resolved_path(base, path, &mut seen, &mut paths);
    }
    for path in &sim.source_root_overrides {
        push_resolved_path(base, path, &mut seen, &mut paths);
    }
    effective.source_root_paths = paths;
}

fn preset_from_override(
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
    if let Some(t_end) = override_copy
        .t_end
        .filter(|value| value.is_finite() && *value > 0.0)
    {
        effective.t_end = t_end;
    }
    if let Some(dt) = override_copy.dt {
        effective.dt = Some(dt);
    }
    if let Some(output_dir) = override_copy.output_dir.as_deref() {
        effective.output_dir = output_dir.to_string();
    }
    EffectiveSimulationPreset {
        solver: effective.solver,
        t_end: effective.t_end,
        dt: effective.dt,
        output_dir: effective.output_dir,
        source_root_overrides: override_copy.source_root_overrides,
    }
}

fn collect_workspace_scenario_files(workspace_root: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
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
                push_workspace_dir_if_not_ignored(&mut stack, path, &name);
                continue;
            }
            if file_type.is_file()
                && path
                    .file_name()
                    .and_then(|name| name.to_str())
                    .is_some_and(is_rumoca_task_filename)
            {
                files.push(path);
            }
        }
    }
    files.sort();
    Ok(files)
}

fn write_colocated_config_file(path: &Path, data: &ProjectConfigFile) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
    }

    let mut value = if path.is_file() {
        let text = fs::read_to_string(path).with_context(|| format!("read {}", path.display()))?;
        toml::from_str::<toml::Value>(&text)
            .unwrap_or_else(|_| toml::Value::Table(Default::default()))
    } else {
        toml::Value::Table(Default::default())
    };
    merge_config_value(&mut value, data)?;
    let text =
        toml::to_string_pretty(&value).with_context(|| format!("serialize {}", path.display()))?;
    fs::write(path, text).with_context(|| format!("write {}", path.display()))
}

fn merge_config_value(value: &mut toml::Value, data: &ProjectConfigFile) -> Result<()> {
    let root = ensure_table(value)?;
    // Drop any stale top-level version/task keys; the `[rumoca]` marker is
    // authoritative.
    root.remove("version");
    root.remove("task");
    let mut marker = toml::value::Table::new();
    marker.insert(
        "version".to_string(),
        toml::Value::String(data.rumoca.version.clone()),
    );
    marker.insert("task".to_string(), toml::Value::try_from(data.rumoca.task)?);
    root.insert("rumoca".to_string(), toml::Value::Table(marker));
    if !data.source_roots.is_empty() {
        root.insert(
            "source_roots".to_string(),
            toml::Value::Array(
                data.source_roots
                    .iter()
                    .map(|value| toml::Value::String(value.clone()))
                    .collect(),
            ),
        );
    }
    root.insert("model".to_string(), toml::Value::try_from(&data.model)?);
    merge_sim_config_value(root, &data.sim)?;
    merge_codegen_config_value(root, &data.codegen)?;
    if plot_model_config_is_empty(&data.plot) {
        root.remove("plot");
    } else {
        root.insert("plot".to_string(), toml::Value::try_from(&data.plot)?);
    }
    Ok(())
}

fn merge_codegen_config_value(
    root: &mut toml::map::Map<String, toml::Value>,
    codegen: &CodegenConfig,
) -> Result<()> {
    if codegen_config_is_empty(codegen) {
        if let Some(table) = root.get_mut("codegen").and_then(toml::Value::as_table_mut) {
            table.remove("target");
            table.remove("output_dir");
            if table.is_empty() {
                root.remove("codegen");
            }
        } else {
            root.remove("codegen");
        }
        return Ok(());
    }

    let codegen_value = root
        .entry("codegen".to_string())
        .or_insert_with(|| toml::Value::Table(Default::default()));
    if !codegen_value.is_table() {
        *codegen_value = toml::Value::Table(Default::default());
    }
    let table = codegen_value
        .as_table_mut()
        .ok_or_else(|| anyhow!("codegen config value is not a TOML table"))?;
    merge_optional_string(table, "target", codegen.target.as_deref());
    merge_optional_string(table, "output_dir", codegen.output_dir.as_deref());
    Ok(())
}

fn ensure_table(value: &mut toml::Value) -> Result<&mut toml::map::Map<String, toml::Value>> {
    if !value.is_table() {
        *value = toml::Value::Table(Default::default());
    }
    value
        .as_table_mut()
        .ok_or_else(|| anyhow!("project config root is not a TOML table"))
}

fn merge_sim_config_value(
    root: &mut toml::map::Map<String, toml::Value>,
    sim: &SimulationModelOverride,
) -> Result<()> {
    const MANAGED_KEYS: &[&str] = &[
        "solver",
        "t_end",
        "dt",
        "output_dir",
        "source_root_overrides",
    ];

    if simulation_model_override_is_empty(sim) {
        if let Some(table) = root.get_mut("sim").and_then(toml::Value::as_table_mut) {
            for key in MANAGED_KEYS {
                table.remove(*key);
            }
            if table.is_empty() {
                root.remove("sim");
            }
        } else {
            root.remove("sim");
        }
        return Ok(());
    }

    let sim_value = root
        .entry("sim".to_string())
        .or_insert_with(|| toml::Value::Table(Default::default()));
    if !sim_value.is_table() {
        *sim_value = toml::Value::Table(Default::default());
    }
    let table = sim_value
        .as_table_mut()
        .ok_or_else(|| anyhow!("sim config value is not a TOML table"))?;
    merge_optional_string(table, "solver", sim.solver.as_deref());
    merge_optional_f64(table, "t_end", sim.t_end);
    merge_optional_f64(table, "dt", sim.dt);
    merge_optional_string(table, "output_dir", sim.output_dir.as_deref());
    if sim.source_root_overrides.is_empty() {
        table.remove("source_root_overrides");
    } else {
        table.insert(
            "source_root_overrides".to_string(),
            toml::Value::Array(
                sim.source_root_overrides
                    .iter()
                    .map(|path| toml::Value::String(path.clone()))
                    .collect(),
            ),
        );
    }
    Ok(())
}

fn merge_optional_string(
    table: &mut toml::map::Map<String, toml::Value>,
    key: &str,
    value: Option<&str>,
) {
    if let Some(value) = value {
        table.insert(key.to_string(), toml::Value::String(value.to_string()));
    } else {
        table.remove(key);
    }
}

fn merge_optional_f64(
    table: &mut toml::map::Map<String, toml::Value>,
    key: &str,
    value: Option<f64>,
) {
    if let Some(value) = value {
        table.insert(key.to_string(), toml::Value::Float(value));
    } else {
        table.remove(key);
    }
}

fn default_config_path_for_model(workspace_root: &Path, model: &str) -> PathBuf {
    if let Ok(Some(source_file)) = find_model_source_file(workspace_root, model)
        && let Some(parent) = source_file.parent()
    {
        let stem = source_file
            .file_stem()
            .and_then(|value| value.to_str())
            .filter(|value| !value.is_empty())
            .unwrap_or_else(|| class_name_from_qualified_name(model));
        return parent.join(format!("rum.{}.toml", sanitize_identifier(stem)));
    }
    workspace_root.join(format!(
        "rum.{}.toml",
        sanitize_identifier(class_name_from_qualified_name(model))
    ))
}

fn find_model_source_file(workspace_root: &Path, model: &str) -> Result<Option<PathBuf>> {
    let files = collect_workspace_modelica_files(workspace_root)?;
    if files.is_empty() {
        return Ok(None);
    }
    let (successes, _) = parse_files_parallel_lenient(&files);
    let mut matches = Vec::new();
    for (source_path, definition) in successes {
        if collect_model_names(&definition)
            .iter()
            .any(|name| name == model)
        {
            matches.push(PathBuf::from(source_path));
        }
    }
    matches.sort();
    Ok(matches.into_iter().next())
}

fn collect_workspace_modelica_files(workspace_root: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
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
                push_workspace_dir_if_not_ignored(&mut stack, path, &name);
                continue;
            }
            if file_type.is_file()
                && path
                    .extension()
                    .is_some_and(|extension| extension.eq_ignore_ascii_case("mo"))
            {
                files.push(path);
            }
        }
    }
    files.sort();
    Ok(files)
}

fn is_ignored_workspace_dir(name: &str) -> bool {
    matches!(
        name,
        ".git" | ".rumoca" | "target" | "node_modules" | ".venv"
    )
}

fn push_workspace_dir_if_not_ignored(stack: &mut Vec<PathBuf>, path: PathBuf, name: &str) {
    if !is_ignored_workspace_dir(name) {
        stack.push(path);
    }
}

fn normalize_plot_views(views: &mut [PlotViewConfig]) {
    for view in views {
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
            .map(|value| value.trim().to_string())
            .filter(|value| !value.is_empty())
            .collect();
        view.x = view
            .x
            .as_deref()
            .map(str::trim)
            .filter(|value| !value.is_empty())
            .map(str::to_string);
        view.script = view
            .script
            .as_deref()
            .map(str::trim)
            .filter(|value| !value.is_empty())
            .map(str::to_string);
        view.script_path = view
            .script_path
            .as_deref()
            .map(str::trim)
            .filter(|value| !value.is_empty())
            .map(str::to_string);
    }
}

fn push_resolved_path(
    base: &Path,
    raw: &str,
    seen: &mut HashSet<String>,
    output: &mut Vec<String>,
) {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return;
    }
    let resolved = resolve_path(base, trimmed);
    let key = fs::canonicalize(&resolved)
        .map(|value| value.to_string_lossy().to_string())
        .unwrap_or_else(|_| resolved.to_string_lossy().to_string());
    if seen.insert(key) {
        output.push(resolved.to_string_lossy().to_string());
    }
}

fn resolve_path(base: &Path, raw: &str) -> PathBuf {
    let path = Path::new(raw);
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        base.join(path)
    }
}

fn class_name_from_qualified_name(model: &str) -> &str {
    let name = rumoca_core::top_level_last_segment(model).trim();
    if name.is_empty() { "Model" } else { name }
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

fn codegen_config_is_empty(value: &CodegenConfig) -> bool {
    value.target.is_none() && value.output_dir.is_none()
}

fn sanitize_identifier(input: &str) -> String {
    let mut out = String::new();
    for c in input.chars() {
        if c.is_ascii_alphanumeric() || c == '_' {
            out.push(c.to_ascii_lowercase());
        } else if c.is_ascii_whitespace() || c == '-' || c == '.' {
            out.push('_');
        }
    }
    if out.is_empty() {
        "model".to_string()
    } else {
        out
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
    raw.filter(|value| value.is_finite() && *value > 0.0)
}

fn normalize_dt_opt(raw: Option<f64>) -> Option<f64> {
    normalize_dt(raw)
}

#[cfg(test)]
mod tests;
