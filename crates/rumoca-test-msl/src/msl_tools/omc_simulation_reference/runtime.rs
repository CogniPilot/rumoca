use super::*;

pub(super) fn select_omc_simulation_models(
    model_names: &[String],
    runtimes: &HashMap<String, RumocaRuntime>,
    rumoca_sim_ok_only: bool,
) -> Vec<String> {
    // Default: build the OMC baseline for every target so that models which
    // become rumoca `sim_ok` *later* already have an OMC trace to compare
    // against. CI opts into the faster `sim_ok`-only subset.
    if !rumoca_sim_ok_only || runtimes.is_empty() {
        return model_names.to_vec();
    }
    model_names
        .iter()
        .filter(|model_name| {
            runtimes
                .get(model_name.as_str())
                .is_some_and(rumoca_runtime_is_trace_candidate)
        })
        .cloned()
        .collect()
}

fn rumoca_runtime_is_trace_candidate(runtime: &RumocaRuntime) -> bool {
    runtime.status == "sim_ok" && runtime.trace_file.is_some()
}

pub(super) fn attach_rumoca_runtime(
    runtimes: &HashMap<String, RumocaRuntime>,
    all_results: &mut BTreeMap<String, SimModelResult>,
) {
    for (model_name, result) in all_results {
        let Some(runtime) = runtimes.get(model_name) else {
            continue;
        };
        attach_rumoca_runtime_to_result(result, runtime);
    }
}

pub(super) fn ensure_target_placeholders(
    target_models: &[String],
    runtimes: &HashMap<String, RumocaRuntime>,
    all_results: &mut BTreeMap<String, SimModelResult>,
) {
    for model_name in target_models {
        if all_results.contains_key(model_name) {
            continue;
        }
        let mut result = skipped_omc_result();
        if let Some(runtime) = runtimes.get(model_name) {
            attach_rumoca_runtime_to_result(&mut result, runtime);
        }
        all_results.insert(model_name.clone(), result);
    }
}

fn attach_rumoca_runtime_to_result(result: &mut SimModelResult, runtime: &RumocaRuntime) {
    result.rumoca_status = Some(runtime.status.clone());
    result.rumoca_ic_status = runtime.ic_status.clone();
    result.rumoca_ic_error = runtime.ic_error.clone();
    result.rumoca_ic_seconds = runtime.ic_seconds;
    result.rumoca_sim_seconds = runtime.sim_seconds;
    result.rumoca_sim_build_seconds = runtime.sim_build_seconds;
    result.rumoca_sim_run_seconds = runtime.sim_run_seconds;
    result.rumoca_sim_wall_seconds = runtime.sim_wall_seconds;
    result.rumoca_trace_file = runtime.trace_file.clone();
    result.rumoca_trace_error = runtime.trace_error.clone();
}

fn skipped_omc_result() -> SimModelResult {
    SimModelResult {
        status: "skipped".to_string(),
        error: Some("OMC simulation skipped because Rumoca did not produce a trace".to_string()),
        sim_system_seconds: None,
        total_system_seconds: None,
        omc_wall_seconds: None,
        result_file: None,
        trace_file: None,
        trace_error: None,
        rumoca_status: None,
        rumoca_ic_status: None,
        rumoca_ic_error: None,
        rumoca_ic_seconds: None,
        rumoca_sim_seconds: None,
        rumoca_sim_build_seconds: None,
        rumoca_sim_run_seconds: None,
        rumoca_sim_wall_seconds: None,
        rumoca_trace_file: None,
        rumoca_trace_error: None,
    }
}

pub(super) fn path_for_rumoca_results(paths: &MslPaths) -> PathBuf {
    paths.results_dir.join("msl_results.json")
}

pub(super) fn load_rumoca_runtime(path: PathBuf) -> Result<HashMap<String, RumocaRuntime>> {
    if !path.exists() {
        return Ok(HashMap::new());
    }
    let payload: Value = serde_json::from_str(
        &std::fs::read_to_string(&path)
            .with_context(|| format!("failed to read '{}'", path.display()))?,
    )
    .with_context(|| format!("failed to parse '{}'", path.display()))?;
    let model_results = payload
        .get("model_results")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    let mut runtimes = HashMap::new();
    for model in model_results {
        let Some(name) = model.get("model_name").and_then(Value::as_str) else {
            continue;
        };
        let Some(status) = model.get("sim_status").and_then(Value::as_str) else {
            continue;
        };
        runtimes.insert(
            name.to_string(),
            RumocaRuntime {
                status: status.to_string(),
                ic_status: model
                    .get("ic_status")
                    .and_then(Value::as_str)
                    .map(ToOwned::to_owned),
                ic_error: model
                    .get("ic_error")
                    .and_then(Value::as_str)
                    .map(ToOwned::to_owned),
                ic_seconds: parse_json_float(model.get("ic_seconds")),
                sim_seconds: parse_json_float(model.get("sim_seconds")),
                sim_build_seconds: parse_json_float(model.get("sim_build_seconds")),
                sim_run_seconds: parse_json_float(model.get("sim_run_seconds")),
                sim_wall_seconds: parse_json_float(model.get("sim_wall_seconds")),
                trace_file: model
                    .get("sim_trace_file")
                    .and_then(Value::as_str)
                    .map(ToOwned::to_owned),
                trace_error: model
                    .get("sim_trace_error")
                    .and_then(Value::as_str)
                    .map(ToOwned::to_owned),
                compile_seconds: parse_json_float(model.get("compile_seconds")),
                scalar_equations: parse_json_usize(model.get("scalar_equations")),
                num_states: parse_json_usize(model.get("num_states")),
            },
        );
    }
    Ok(runtimes)
}

fn parse_json_float(value: Option<&Value>) -> Option<f64> {
    value
        .and_then(Value::as_f64)
        .filter(|value| value.is_finite())
}

fn parse_json_usize(value: Option<&Value>) -> Option<usize> {
    value.and_then(Value::as_u64).map(|value| value as usize)
}
