use super::*;

// =============================================================================
// Focused simulation target selection and subset controls
// =============================================================================

/// Default global worker sizing for MSL test stages: leave headroom for the
/// host and use the rest.
pub(super) const MSL_PARALLELISM_RESERVED_CPUS: usize = 3;
pub(super) const MSL_PARALLELISM_ENV: &str = "RUMOCA_MSL_PARALLELISM";
/// Optional total memory budget for all concurrent sim workers (MB).
pub(super) const SIM_TOTAL_MEMORY_MB_ENV: &str = "RUMOCA_MSL_SIM_TOTAL_MEMORY_MB";
/// Default number of models in state-count-ranked short/long simulation sets.
pub(super) const SIM_SET_LIMIT_DEFAULT: usize = 180;
pub(super) const DEFAULT_SIM_TARGETS_FILE_REL: &str =
    "tests/msl_tests/msl_simulation_targets_180.json";
const USE_GENERATED_SIM_TARGETS_ENV: &str = "RUMOCA_MSL_USE_GENERATED_SIM_TARGETS";
const USE_PRIOR_COMPLEXITY_SCHEDULE_ENV: &str = "RUMOCA_MSL_USE_PRIOR_COMPLEXITY_SCHEDULE";
const GENERATED_SIM_TARGETS_FILE_REL: &str = "results/msl_simulation_targets.json";
const PRIOR_RESULTS_FILE_REL: &str = "results/msl_results.json";

pub(super) const SIM_SET_ENV: &str = "RUMOCA_MSL_SIM_SET";
pub(super) const SIM_SET_LIMIT_ENV: &str = "RUMOCA_MSL_SIM_SET_LIMIT";

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
struct PriorModelComplexity {
    num_states: usize,
    num_algebraics: usize,
    num_f_x: usize,
    scalar_equations: usize,
    scalar_unknowns: usize,
}

#[derive(Debug, Deserialize)]
struct PriorResultsSummary {
    #[serde(default)]
    model_results: Vec<PriorResultRecord>,
}

#[derive(Debug, Deserialize)]
struct PriorResultRecord {
    model_name: String,
    #[serde(default)]
    num_states: Option<usize>,
    #[serde(default)]
    num_algebraics: Option<usize>,
    #[serde(default)]
    num_f_x: Option<usize>,
    #[serde(default)]
    scalar_equations: Option<usize>,
    #[serde(default)]
    scalar_unknowns: Option<usize>,
}

fn default_stage_parallelism(auto_threads: usize) -> usize {
    if auto_threads <= 4 {
        auto_threads.max(1)
    } else {
        auto_threads
            .saturating_sub(MSL_PARALLELISM_RESERVED_CPUS)
            .max(1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum SimSetMode {
    /// Fast development set: lowest-state-count root examples.
    Short,
    /// Complementary stress set: highest-state-count root examples.
    Long,
    /// Full explicit example set.
    Full,
}

impl std::fmt::Display for SimSetMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SimSetMode::Short => write!(f, "short"),
            SimSetMode::Long => write!(f, "long"),
            SimSetMode::Full => write!(f, "full"),
        }
    }
}

pub(super) fn sim_set_mode() -> SimSetMode {
    let Some(raw) = std::env::var(SIM_SET_ENV).ok() else {
        return SimSetMode::Short;
    };
    match raw.trim().to_ascii_lowercase().as_str() {
        "" | "short" => SimSetMode::Short,
        "long" => SimSetMode::Long,
        "full" => SimSetMode::Full,
        other => {
            eprintln!(
                "WARNING: invalid {}='{}' (expected short|long|full), defaulting to short",
                SIM_SET_ENV, other
            );
            SimSetMode::Short
        }
    }
}

pub(super) fn sim_set_limit() -> usize {
    std::env::var(SIM_SET_LIMIT_ENV)
        .ok()
        .and_then(|raw| raw.trim().parse::<usize>().ok())
        .filter(|limit| *limit > 0)
        .unwrap_or(SIM_SET_LIMIT_DEFAULT)
}

pub(super) fn sim_targets_file_override() -> Option<PathBuf> {
    let raw = std::env::var("RUMOCA_MSL_SIM_TARGETS_FILE").ok()?;
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    Some(PathBuf::from(trimmed))
}

fn generated_sim_targets_file() -> Option<PathBuf> {
    let path = get_msl_cache_dir().join(GENERATED_SIM_TARGETS_FILE_REL);
    path.is_file().then_some(path)
}

fn env_var_bool(key: &str) -> bool {
    std::env::var(key)
        .ok()
        .map(|raw| {
            matches!(
                raw.trim().to_ascii_lowercase().as_str(),
                "1" | "true" | "yes" | "on"
            )
        })
        .unwrap_or(false)
}

fn committed_sim_targets_file() -> Option<PathBuf> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(DEFAULT_SIM_TARGETS_FILE_REL);
    path.is_file().then_some(path)
}

fn select_default_sim_targets_file(
    override_file: Option<PathBuf>,
    committed_file: Option<PathBuf>,
    generated_file: Option<PathBuf>,
    use_generated_targets: bool,
) -> Option<PathBuf> {
    override_file.or_else(|| {
        if use_generated_targets {
            generated_file.or(committed_file)
        } else {
            committed_file.or(generated_file)
        }
    })
}

pub(super) fn default_sim_targets_file() -> Option<PathBuf> {
    committed_sim_targets_file().or_else(generated_sim_targets_file)
}

fn generated_sim_targets_file_requested() -> bool {
    env_var_bool(USE_GENERATED_SIM_TARGETS_ENV)
}

fn prior_complexity_schedule_requested() -> bool {
    env_var_bool(USE_PRIOR_COMPLEXITY_SCHEDULE_ENV)
}

fn should_apply_prior_complexity_schedule(
    subset_requested: bool,
    schedule_requested: bool,
) -> bool {
    !subset_requested && schedule_requested
}

fn prior_results_file() -> Option<PathBuf> {
    let path = get_msl_cache_dir().join(PRIOR_RESULTS_FILE_REL);
    path.is_file().then_some(path)
}

fn dedup_model_names_preserve_order(names: Vec<String>) -> Vec<String> {
    let mut seen = HashSet::new();
    names
        .into_iter()
        .filter(|name| seen.insert(name.clone()))
        .collect()
}

pub(super) fn parse_target_model_names(value: &serde_json::Value) -> Result<Vec<String>, String> {
    fn parse_string_array(array: &[serde_json::Value]) -> Result<Vec<String>, String> {
        array
            .iter()
            .map(|entry| {
                entry
                    .as_str()
                    .map(ToOwned::to_owned)
                    .ok_or_else(|| "expected array of strings".to_string())
            })
            .collect()
    }

    if let Some(array) = value.as_array() {
        return parse_string_array(array);
    }
    let Some(object) = value.as_object() else {
        return Err("expected JSON array or object".to_string());
    };
    if let Some(model_names) = object
        .get("model_names")
        .and_then(serde_json::Value::as_array)
    {
        return parse_string_array(model_names);
    }
    if let Some(records) = object.get("records").and_then(serde_json::Value::as_array) {
        let mut names = Vec::new();
        for record in records {
            let Some(name) = record
                .get("model_name")
                .and_then(serde_json::Value::as_str)
                .map(ToOwned::to_owned)
            else {
                return Err("expected records[*].model_name to be a string".to_string());
            };
            names.push(name);
        }
        return Ok(names);
    }
    Err("object must include 'model_names' or 'records'".to_string())
}

pub(super) fn load_target_model_names(path: &Path) -> Result<Vec<String>, String> {
    let raw = fs::read_to_string(path).map_err(|e| format!("read failed: {e}"))?;
    let value: serde_json::Value =
        serde_json::from_str(&raw).map_err(|e| format!("parse failed: {e}"))?;
    Ok(dedup_model_names_preserve_order(parse_target_model_names(
        &value,
    )?))
}

fn retain_selected_model_order(names: &mut Vec<String>, selected: &[String]) {
    let available: HashSet<String> = names.iter().cloned().collect();
    *names = selected
        .iter()
        .filter(|name| available.contains(*name))
        .cloned()
        .collect();
}

fn load_prior_model_complexities(path: &Path) -> Option<HashMap<String, PriorModelComplexity>> {
    let raw = fs::read_to_string(path).ok()?;
    let summary: PriorResultsSummary = serde_json::from_str(&raw).ok()?;
    let mut complexities = HashMap::new();
    for result in summary.model_results {
        let complexity = PriorModelComplexity {
            num_states: result.num_states.unwrap_or_default(),
            num_algebraics: result.num_algebraics.unwrap_or_default(),
            num_f_x: result.num_f_x.unwrap_or_default(),
            scalar_equations: result.scalar_equations.unwrap_or_default(),
            scalar_unknowns: result.scalar_unknowns.unwrap_or_default(),
        };
        if complexity != PriorModelComplexity::default() {
            complexities.insert(result.model_name, complexity);
        }
    }
    Some(complexities)
}

fn apply_prior_complexity_schedule(names: &mut [String], label: &str) {
    let Some(path) = prior_results_file() else {
        return;
    };
    let Some(complexities) = load_prior_model_complexities(&path) else {
        eprintln!(
            "WARNING: failed to read prior MSL results from '{}'; keeping lexical target order",
            path.display()
        );
        return;
    };
    let ranked_models = names
        .iter()
        .filter(|name| complexities.contains_key(*name))
        .count();
    if ranked_models == 0 {
        return;
    }
    names.sort_by(|left, right| {
        let left_key = complexities.get(left).copied().unwrap_or_default();
        let right_key = complexities.get(right).copied().unwrap_or_default();
        right_key.cmp(&left_key).then_with(|| left.cmp(right))
    });
    println!(
        "{label} schedule: ranked {ranked_models}/{} models using prior compile metrics from {}",
        names.len(),
        path.display()
    );
}

pub(super) fn sim_subset_patterns() -> Vec<String> {
    std::env::var("RUMOCA_MSL_SIM_MATCH")
        .ok()
        .map(|raw| {
            raw.split(',')
                .map(str::trim)
                .filter(|s| !s.is_empty())
                .map(ToOwned::to_owned)
                .collect()
        })
        .unwrap_or_default()
}

pub(super) fn sim_subset_limit() -> Option<usize> {
    let raw = std::env::var("RUMOCA_MSL_SIM_LIMIT").ok()?;
    match raw.trim().parse::<usize>() {
        Ok(limit) => Some(limit),
        Err(_) => {
            eprintln!(
                "WARNING: invalid RUMOCA_MSL_SIM_LIMIT='{}' (expected usize), ignoring",
                raw
            );
            None
        }
    }
}

pub(super) fn sim_subset_exact_match_enabled() -> bool {
    std::env::var("RUMOCA_MSL_SIM_MATCH_EXACT")
        .ok()
        .map(|raw| {
            matches!(
                raw.trim().to_ascii_lowercase().as_str(),
                "1" | "true" | "yes" | "on"
            )
        })
        .unwrap_or(false)
}

pub(super) fn msl_stage_parallelism() -> usize {
    let auto_threads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    // Preserve throughput on small CI runners (2-4 vCPUs) by using all cores.
    // Reserve a fixed amount only on larger hosts where headroom matters.
    let default_threads = default_stage_parallelism(auto_threads);
    std::env::var(MSL_PARALLELISM_ENV)
        .ok()
        .and_then(|raw| raw.trim().parse::<usize>().ok())
        .filter(|threads| *threads > 0)
        .unwrap_or(default_threads)
}

pub(super) fn simulation_parallelism() -> usize {
    let requested_threads = std::env::var("RUMOCA_MSL_SIM_PARALLELISM")
        .ok()
        .and_then(|raw| raw.trim().parse::<usize>().ok())
        .filter(|threads| *threads > 0)
        .unwrap_or_else(msl_stage_parallelism);

    let per_worker_memory_mb = sim_worker_memory_limit_mb();
    let total_budget_mb = std::env::var(SIM_TOTAL_MEMORY_MB_ENV)
        .ok()
        .and_then(|raw| raw.trim().parse::<usize>().ok())
        .filter(|mb| *mb > 0);
    let capped_by_memory = match (per_worker_memory_mb, total_budget_mb) {
        (Some(per_worker_mb), Some(total_mb)) => (total_mb / per_worker_mb.max(1)).max(1),
        _ => requested_threads,
    };

    let effective_threads = requested_threads.min(capped_by_memory).max(1);
    if effective_threads < requested_threads {
        println!(
            "Simulation parallelism capped by memory budget: {} workers (requested {}, per-worker cap {} MB, total budget {} MB)",
            effective_threads,
            requested_threads,
            per_worker_memory_mb.unwrap_or(0),
            total_budget_mb.unwrap_or(0)
        );
    }

    effective_threads
}

pub(super) fn model_name_matches_any_pattern(
    name: &str,
    patterns: &[String],
    exact_match: bool,
) -> bool {
    if exact_match {
        patterns.iter().any(|pat| name == pat)
    } else {
        patterns.iter().any(|pat| name.contains(pat))
    }
}

pub(super) fn apply_sim_subset_filters(names: &mut Vec<String>, label: &str) -> bool {
    let baseline_count = names.len();
    let target_file_override = sim_targets_file_override();
    let target_file = select_default_sim_targets_file(
        target_file_override.clone(),
        committed_sim_targets_file(),
        generated_sim_targets_file(),
        generated_sim_targets_file_requested(),
    );
    let patterns = sim_subset_patterns();
    let exact_match = sim_subset_exact_match_enabled();
    let limit = sim_subset_limit();
    let mut subset_requested = false;

    if let Some(path) = target_file.as_ref() {
        let is_override = target_file_override.is_some();
        subset_requested |= is_override;
        let selected = load_target_model_names(path).unwrap_or_else(|err| {
            panic!(
                "invalid simulation targets file '{}': {}",
                path.display(),
                err
            )
        });
        retain_selected_model_order(names, &selected);
        if is_override {
            println!(
                "{} target file (RUMOCA_MSL_SIM_TARGETS_FILE={}) kept {}/{} root examples",
                label,
                path.display(),
                names.len(),
                baseline_count
            );
        } else {
            println!(
                "{} default target file ({}) kept {}/{} root examples",
                label,
                path.display(),
                names.len(),
                baseline_count
            );
        }
    }

    if !patterns.is_empty() {
        subset_requested = true;
        names.retain(|name| model_name_matches_any_pattern(name, &patterns, exact_match));
        println!(
            "{} subset filter (RUMOCA_MSL_SIM_MATCH{}) kept {}/{} root examples",
            label,
            if exact_match { ", exact" } else { "" },
            names.len(),
            baseline_count
        );
    }

    if let Some(limit) = limit {
        subset_requested = true;
        if names.len() > limit {
            names.truncate(limit);
        }
        println!(
            "{} subset limit (RUMOCA_MSL_SIM_LIMIT={}) -> {} root examples",
            label,
            limit,
            names.len()
        );
    }

    subset_requested
}

/// Pre-select compile targets for focused simulation runs.
///
/// By default, compile/balance/simulation run on the committed 180-model explicit
/// MSL example target list (`msl_simulation_targets_180.json`). Optional focused
/// controls (`RUMOCA_MSL_SIM_TARGETS_FILE`, `RUMOCA_MSL_SIM_MATCH`,
/// `RUMOCA_MSL_SIM_LIMIT`) can narrow this scope further for local debugging.
pub(super) fn select_compile_targets_for_focused_simulation(
    model_names: &[String],
    _run_simulation: bool,
) -> Option<Vec<String>> {
    let mut names: Vec<String> = model_names
        .iter()
        .filter(|name| is_root_msl_example_model_name(name))
        .cloned()
        .collect();
    names.sort();
    let baseline_count = names.len();
    let subset_requested = apply_sim_subset_filters(&mut names, "Compile");
    if should_apply_prior_complexity_schedule(
        subset_requested,
        prior_complexity_schedule_requested(),
    ) {
        apply_prior_complexity_schedule(&mut names, "Compile");
    }

    if names.is_empty() {
        None
    } else {
        println!(
            "Compile scope: {}/{} root examples",
            names.len(),
            baseline_count
        );
        Some(names)
    }
}

pub(super) fn is_selected_sim_target(name: &str, ctx: &RenderSimContext<'_>) -> bool {
    ctx.sim_target_names.is_none_or(|set| set.contains(name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_stage_parallelism_uses_all_cores_on_small_hosts() {
        assert_eq!(default_stage_parallelism(1), 1);
        assert_eq!(default_stage_parallelism(2), 2);
        assert_eq!(default_stage_parallelism(4), 4);
    }

    #[test]
    fn default_stage_parallelism_reserves_headroom_on_large_hosts() {
        assert_eq!(default_stage_parallelism(8), 5);
        assert_eq!(default_stage_parallelism(32), 29);
    }

    #[test]
    fn load_target_model_names_preserves_record_order() {
        let temp = tempfile::tempdir().expect("tempdir");
        let path = temp.path().join("targets.json");
        fs::write(
            &path,
            serde_json::to_string_pretty(&serde_json::json!({
                "records": [
                    {"model_name": "Modelica.Electrical.Digital.Examples.DFFREGSRL"},
                    {"model_name": "Modelica.Blocks.Examples.BooleanNetwork1"},
                    {"model_name": "Modelica.Electrical.Digital.Examples.DFFREGSRL"},
                    {"model_name": "Modelica.Electrical.Digital.Examples.DFFREG"}
                ]
            }))
            .expect("serialize targets"),
        )
        .expect("write targets");

        assert_eq!(
            load_target_model_names(&path).expect("load target names"),
            vec![
                "Modelica.Electrical.Digital.Examples.DFFREGSRL".to_string(),
                "Modelica.Blocks.Examples.BooleanNetwork1".to_string(),
                "Modelica.Electrical.Digital.Examples.DFFREG".to_string(),
            ]
        );
    }

    #[test]
    fn load_prior_model_complexities_reads_state_rankings() {
        let temp = tempfile::tempdir().expect("tempdir");
        let path = temp.path().join("msl_results.json");
        fs::write(
            &path,
            serde_json::to_string_pretty(&serde_json::json!({
                "model_results": [
                    {
                        "model_name": "Modelica.Electrical.Digital.Examples.DFFREG",
                        "num_states": 0,
                        "num_algebraics": 42,
                        "num_f_x": 0,
                        "scalar_equations": 84,
                        "scalar_unknowns": 84
                    },
                    {
                        "model_name": "Modelica.Blocks.Examples.BooleanNetwork1",
                        "num_states": 4,
                        "num_algebraics": 8,
                        "num_f_x": 18,
                        "scalar_equations": 18,
                        "scalar_unknowns": 18
                    }
                ]
            }))
            .expect("serialize prior results"),
        )
        .expect("write prior results");

        let complexities = load_prior_model_complexities(&path).expect("prior complexities");
        assert_eq!(
            complexities.get("Modelica.Blocks.Examples.BooleanNetwork1"),
            Some(&PriorModelComplexity {
                num_states: 4,
                num_algebraics: 8,
                num_f_x: 18,
                scalar_equations: 18,
                scalar_unknowns: 18,
            })
        );
        assert_eq!(
            complexities.get("Modelica.Electrical.Digital.Examples.DFFREG"),
            Some(&PriorModelComplexity {
                num_states: 0,
                num_algebraics: 42,
                num_f_x: 0,
                scalar_equations: 84,
                scalar_unknowns: 84,
            })
        );
    }

    #[test]
    fn default_sim_targets_file_prefers_committed_baseline_list() {
        let default_targets = default_sim_targets_file().expect("default target file");
        assert!(
            default_targets.ends_with(DEFAULT_SIM_TARGETS_FILE_REL),
            "default target file should be the committed baseline list, got {}",
            default_targets.display()
        );
    }

    #[test]
    fn select_default_sim_targets_file_prefers_committed_baseline_without_generated_opt_in() {
        let selected = select_default_sim_targets_file(
            None,
            Some(PathBuf::from("/repo/tests/msl_simulation_targets_180.json")),
            Some(PathBuf::from(
                "/repo/target/msl/results/msl_simulation_targets.json",
            )),
            false,
        )
        .expect("default target file");
        assert_eq!(
            selected,
            PathBuf::from("/repo/tests/msl_simulation_targets_180.json")
        );
    }

    #[test]
    fn select_default_sim_targets_file_can_use_generated_targets_with_explicit_opt_in() {
        let selected = select_default_sim_targets_file(
            None,
            Some(PathBuf::from("/repo/tests/msl_simulation_targets_180.json")),
            Some(PathBuf::from(
                "/repo/target/msl/results/msl_simulation_targets.json",
            )),
            true,
        )
        .expect("generated target file");
        assert_eq!(
            selected,
            PathBuf::from("/repo/target/msl/results/msl_simulation_targets.json")
        );
    }

    #[test]
    fn prior_complexity_schedule_is_opt_in_only() {
        assert!(!should_apply_prior_complexity_schedule(false, false));
        assert!(!should_apply_prior_complexity_schedule(true, true));
        assert!(should_apply_prior_complexity_schedule(false, true));
    }
}
