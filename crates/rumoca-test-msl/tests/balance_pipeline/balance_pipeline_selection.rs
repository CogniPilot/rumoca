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

pub(super) const SIM_SET_ENV: &str = "RUMOCA_MSL_SIM_SET";
pub(super) const SIM_SET_LIMIT_ENV: &str = "RUMOCA_MSL_SIM_SET_LIMIT";

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

pub(super) fn default_sim_targets_file() -> Option<PathBuf> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(DEFAULT_SIM_TARGETS_FILE_REL);
    path.is_file().then_some(path)
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
    let mut names = parse_target_model_names(&value)?;
    names.sort();
    names.dedup();
    Ok(names)
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
    let target_file = target_file_override
        .clone()
        .or_else(default_sim_targets_file);
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
        let selected: HashSet<String> = selected.into_iter().collect();
        names.retain(|name| selected.contains(name));
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
    let _subset_requested = apply_sim_subset_filters(&mut names, "Compile");

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
}
