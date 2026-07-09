use super::*;

pub(super) fn canonical_msl_version(version: &str) -> &str {
    version.trim().trim_start_matches('v')
}

pub(super) fn canonical_omc_version(version: &str) -> &str {
    version.trim()
}

pub(super) fn quality_gate_omc_version(version: &str) -> String {
    let trimmed = version.trim();
    let Some(version_suffix) = trimmed.strip_prefix("OpenModelica ") else {
        return trimmed.to_string();
    };
    let release_len = version_suffix
        .bytes()
        .take_while(|byte| byte.is_ascii_digit() || *byte == b'.')
        .count();
    if release_len == 0 {
        return trimmed.to_string();
    }
    format!("OpenModelica {}", &version_suffix[..release_len])
}

pub(super) fn fnv1a64_update(mut hash: u64, bytes: &[u8]) -> u64 {
    const OFFSET: u64 = 0xcbf29ce484222325;
    const PRIME: u64 = 0x00000100000001B3;
    if hash == 0 {
        hash = OFFSET;
    }
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(PRIME);
    }
    hash
}

pub(super) fn parity_target_set_cache_key(
    target_models: &[String],
    msl_version: &str,
    omc_version: &str,
) -> String {
    let normalized_models = normalize_model_names(target_models.to_vec());
    let mut hash = 0_u64;
    hash = fnv1a64_update(hash, canonical_msl_version(msl_version).as_bytes());
    hash = fnv1a64_update(hash, &[0xff]);
    hash = fnv1a64_update(hash, canonical_omc_version(omc_version).as_bytes());
    hash = fnv1a64_update(hash, &[0xfe]);
    hash = fnv1a64_update(hash, normalized_models.len().to_string().as_bytes());
    hash = fnv1a64_update(hash, &[0xfd]);
    for model in &normalized_models {
        hash = fnv1a64_update(hash, model.as_bytes());
        hash = fnv1a64_update(hash, &[0x00]);
    }
    format!("{hash:016x}")
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) struct SimulationParityCachePolicy {
    pub(super) batch_timeout_seconds: u64,
    pub(super) workers: usize,
    pub(super) omc_threads: usize,
    pub(super) use_experiment_stop_time: bool,
    pub(super) stop_time_override: Option<f64>,
}

pub(super) fn simulation_stop_time_override() -> Option<f64> {
    // No stop-time override; use the model's experiment annotation.
    None
}

pub(super) fn omc_sim_reference_batch_timeout_seconds() -> u64 {
    parity_config()
        .omc_sim_reference_batch_timeout_secs
        .filter(|value| *value > 0)
        .unwrap_or(OMC_SIM_REFERENCE_BATCH_TIMEOUT_SECONDS)
}

pub(super) fn current_simulation_parity_cache_policy(
    workers: usize,
    omc_threads: usize,
    batch_timeout_seconds: u64,
) -> SimulationParityCachePolicy {
    let stop_time_override = simulation_stop_time_override();
    SimulationParityCachePolicy {
        batch_timeout_seconds,
        workers,
        omc_threads,
        use_experiment_stop_time: stop_time_override.is_none(),
        stop_time_override,
    }
}

pub(super) fn simulation_parity_cache_key(
    target_models: &[String],
    msl_version: &str,
    omc_version: &str,
    policy: SimulationParityCachePolicy,
) -> String {
    let normalized_models = normalize_model_names(target_models.to_vec());
    let mut hash = 0_u64;
    hash = fnv1a64_update(hash, canonical_msl_version(msl_version).as_bytes());
    hash = fnv1a64_update(hash, &[0xff]);
    hash = fnv1a64_update(hash, canonical_omc_version(omc_version).as_bytes());
    hash = fnv1a64_update(hash, &[0xfe]);
    hash = fnv1a64_update(hash, normalized_models.len().to_string().as_bytes());
    hash = fnv1a64_update(hash, &[0xfd]);
    for model in &normalized_models {
        hash = fnv1a64_update(hash, model.as_bytes());
        hash = fnv1a64_update(hash, &[0x00]);
    }
    hash = fnv1a64_update(hash, &[0xfc]);
    hash = fnv1a64_update(hash, policy.batch_timeout_seconds.to_string().as_bytes());
    hash = fnv1a64_update(hash, &[0xfb]);
    hash = fnv1a64_update(hash, policy.workers.to_string().as_bytes());
    hash = fnv1a64_update(hash, &[0xf9]);
    hash = fnv1a64_update(hash, policy.omc_threads.to_string().as_bytes());
    hash = fnv1a64_update(hash, &[0xf8]);
    hash = fnv1a64_update(hash, &[u8::from(policy.use_experiment_stop_time)]);
    hash = fnv1a64_update(hash, &[0xfa]);
    if let Some(stop_time_override) = policy.stop_time_override {
        hash = fnv1a64_update(hash, stop_time_override.to_string().as_bytes());
    } else {
        hash = fnv1a64_update(hash, b"none");
    }
    format!("{hash:016x}")
}

pub(super) fn parity_cache_entry_path(kind: &str, cache_key: &str) -> PathBuf {
    omc_parity_cache_dir()
        .join(kind)
        .join(format!("{cache_key}.json"))
}

pub(super) fn materialize_simulation_parity_cache_entry(
    cache_path: &Path,
    active_path: &Path,
) -> io::Result<()> {
    if !cache_path.is_file() {
        return Err(io::Error::other(format!(
            "missing simulation parity cache entry '{}'",
            cache_path.display()
        )));
    }
    let payload: serde_json::Value =
        serde_json::from_reader(File::open(cache_path)?).map_err(|error| {
            io::Error::other(format!(
                "failed to parse simulation parity cache '{}' for materialization: {error}",
                cache_path.display()
            ))
        })?;
    if let Some(parent) = active_path.parent() {
        fs::create_dir_all(parent)?;
    }
    let sanitized = sanitize_simulation_parity_cache_payload(payload);
    fs::write(
        active_path,
        serde_json::to_vec_pretty(&sanitized).map_err(|error| {
            io::Error::other(format!(
                "failed to serialize sanitized simulation parity cache '{}': {error}",
                active_path.display()
            ))
        })?,
    )
    .map_err(|error| {
        io::Error::other(format!(
            "failed to materialize sanitized simulation parity cache '{}' -> '{}': {error}",
            cache_path.display(),
            active_path.display()
        ))
    })
}

pub(super) fn sanitize_simulation_parity_cache_payload(
    mut payload: serde_json::Value,
) -> serde_json::Value {
    let Some(root) = payload.as_object_mut() else {
        return payload;
    };
    root.remove("runtime_comparison");
    root.remove("trace_comparison");

    let Some(models) = root
        .get_mut("models")
        .and_then(serde_json::Value::as_object_mut)
    else {
        return payload;
    };

    for model in models.values_mut() {
        let Some(model) = model.as_object_mut() else {
            continue;
        };
        model.remove("rumoca_status");
        model.remove("rumoca_ic_status");
        model.remove("rumoca_ic_error");
        model.remove("rumoca_ic_seconds");
        model.remove("rumoca_sim_seconds");
        model.remove("rumoca_sim_wall_seconds");
        model.remove("rumoca_trace_file");
        model.remove("rumoca_trace_error");
    }
    payload
}

pub(super) fn persist_simulation_parity_cache_entry(
    active_path: &Path,
    cache_path: &Path,
) -> io::Result<()> {
    if !active_path.is_file() {
        return Ok(());
    }
    let payload: serde_json::Value =
        serde_json::from_reader(File::open(active_path)?).map_err(|error| {
            io::Error::other(format!(
                "failed to parse simulation parity reference '{}' for cache persistence: {error}",
                active_path.display()
            ))
        })?;
    if let Some(parent) = cache_path.parent() {
        fs::create_dir_all(parent)?;
    }
    let sanitized = sanitize_simulation_parity_cache_payload(payload);
    fs::write(
        cache_path,
        serde_json::to_vec_pretty(&sanitized).map_err(|error| {
            io::Error::other(format!(
                "failed to serialize sanitized simulation parity cache '{}': {error}",
                cache_path.display()
            ))
        })?,
    )
    .map_err(|error| {
        io::Error::other(format!(
            "failed to persist simulation parity cache '{}' -> '{}': {error}",
            active_path.display(),
            cache_path.display()
        ))
    })
}

pub(super) fn current_omc_version() -> io::Result<String> {
    let output = omc_version_command().output()?;
    if !output.status.success() {
        return Err(io::Error::other(format!(
            "failed to query OMC version (status={})",
            output.status
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let version = if stdout.is_empty() {
        String::from_utf8_lossy(&output.stderr).trim().to_string()
    } else {
        stdout
    };
    if version.is_empty() {
        return Err(io::Error::other("omc --version returned empty output"));
    }
    Ok(version)
}

fn omc_version_command() -> std::process::Command {
    if let Ok(image) = std::env::var("RUMOCA_OMC_DOCKER_IMAGE")
        && !image.trim().is_empty()
    {
        let mut command = std::process::Command::new("docker");
        command
            .arg("run")
            .arg("--rm")
            .arg(image.trim())
            .arg("omc")
            .arg("--version");
        return command;
    }
    let mut command = std::process::Command::new("omc");
    command.arg("--version");
    command
}

pub(super) fn parity_cache_matches_targets_and_msl(
    path: &Path,
    target_models: &[String],
    msl_version: &str,
    omc_version: &str,
) -> io::Result<bool> {
    if !path.is_file() {
        return Ok(false);
    }
    let file = File::open(path)?;
    let payload: serde_json::Value = serde_json::from_reader(file).map_err(|error| {
        io::Error::other(format!("invalid parity JSON ({}): {error}", path.display()))
    })?;
    let Some(cached_msl_version) = payload
        .get("msl_version")
        .and_then(serde_json::Value::as_str)
    else {
        return Ok(false);
    };
    if canonical_msl_version(cached_msl_version) != canonical_msl_version(msl_version) {
        return Ok(false);
    }
    let Some(cached_omc_version) = payload
        .get("omc_version")
        .and_then(serde_json::Value::as_str)
    else {
        return Ok(false);
    };
    if canonical_omc_version(cached_omc_version) != canonical_omc_version(omc_version) {
        return Ok(false);
    }
    let Some(cached_models) = model_names_from_omc_models_map(&payload) else {
        return Ok(false);
    };
    Ok(cached_models == normalize_model_names(target_models.to_vec()))
}

pub(super) fn simulation_parity_cache_matches(
    path: &Path,
    target_models: &[String],
    msl_version: &str,
    omc_version: &str,
    policy: SimulationParityCachePolicy,
) -> io::Result<bool> {
    if !parity_cache_matches_targets_and_msl(path, target_models, msl_version, omc_version)? {
        return Ok(false);
    }
    let payload: serde_json::Value =
        serde_json::from_reader(File::open(path)?).map_err(|error| {
            io::Error::other(format!(
                "invalid simulation parity JSON ({}): {error}",
                path.display()
            ))
        })?;
    let batch_timeout_seconds = payload
        .get("timing")
        .and_then(serde_json::Value::as_object)
        .and_then(|timing| timing.get("batch_timeout_seconds"))
        .and_then(serde_json::Value::as_u64);
    if batch_timeout_seconds != Some(policy.batch_timeout_seconds) {
        return Ok(false);
    }
    let workers_used = payload
        .get("timing")
        .and_then(serde_json::Value::as_object)
        .and_then(|timing| timing.get("workers_used"))
        .and_then(serde_json::Value::as_u64);
    if workers_used != Some(policy.workers as u64) {
        return Ok(false);
    }
    let omc_threads = payload
        .get("timing")
        .and_then(serde_json::Value::as_object)
        .and_then(|timing| timing.get("omc_threads"))
        .and_then(serde_json::Value::as_u64);
    if omc_threads != Some(policy.omc_threads as u64) {
        return Ok(false);
    }
    let use_experiment_stop_time = payload
        .get("use_experiment_stop_time")
        .and_then(serde_json::Value::as_bool);
    if use_experiment_stop_time != Some(policy.use_experiment_stop_time) {
        return Ok(false);
    }
    let Some(stop_time_override) = policy.stop_time_override else {
        return Ok(true);
    };
    let stop_time = payload.get("stop_time").and_then(serde_json::Value::as_f64);
    Ok(stop_time.is_some_and(|value| {
        (value - stop_time_override).abs() <= f64::EPSILON.max(stop_time_override.abs() * 1e-12)
    }))
}

pub(super) fn simulation_parity_cache_can_resume(
    path: &Path,
    target_models: &[String],
    msl_version: &str,
    omc_version: &str,
    policy: SimulationParityCachePolicy,
) -> io::Result<bool> {
    if !path.is_file() {
        return Ok(false);
    }
    let payload: serde_json::Value =
        serde_json::from_reader(File::open(path)?).map_err(|error| {
            io::Error::other(format!(
                "invalid simulation parity JSON ({}): {error}",
                path.display()
            ))
        })?;
    if payload
        .get("msl_version")
        .and_then(serde_json::Value::as_str)
        .is_none_or(|cached| canonical_msl_version(cached) != canonical_msl_version(msl_version))
    {
        return Ok(false);
    }
    if payload
        .get("omc_version")
        .and_then(serde_json::Value::as_str)
        .is_none_or(|cached| canonical_omc_version(cached) != canonical_omc_version(omc_version))
    {
        return Ok(false);
    }
    let Some(cached_models) = model_names_from_omc_models_map(&payload) else {
        return Ok(false);
    };
    if cached_models.is_empty() {
        return Ok(false);
    }
    if !omc_models_map_has_success(&payload) {
        return Ok(false);
    }
    let target_models = normalize_model_names(target_models.to_vec());
    let target_set = target_models
        .into_iter()
        .collect::<std::collections::BTreeSet<_>>();
    if cached_models
        .iter()
        .any(|model| !target_set.contains(model))
    {
        return Ok(false);
    }
    let batch_timeout_seconds = payload
        .get("timing")
        .and_then(serde_json::Value::as_object)
        .and_then(|timing| timing.get("batch_timeout_seconds"))
        .and_then(serde_json::Value::as_u64);
    if batch_timeout_seconds != Some(policy.batch_timeout_seconds) {
        return Ok(false);
    }
    let workers_used = payload
        .get("timing")
        .and_then(serde_json::Value::as_object)
        .and_then(|timing| timing.get("workers_used"))
        .and_then(serde_json::Value::as_u64);
    if workers_used != Some(policy.workers as u64) {
        return Ok(false);
    }
    let omc_threads = payload
        .get("timing")
        .and_then(serde_json::Value::as_object)
        .and_then(|timing| timing.get("omc_threads"))
        .and_then(serde_json::Value::as_u64);
    if omc_threads != Some(policy.omc_threads as u64) {
        return Ok(false);
    }
    let use_experiment_stop_time = payload
        .get("use_experiment_stop_time")
        .and_then(serde_json::Value::as_bool);
    if use_experiment_stop_time != Some(policy.use_experiment_stop_time) {
        return Ok(false);
    }
    let Some(stop_time_override) = policy.stop_time_override else {
        return Ok(true);
    };
    let stop_time = payload.get("stop_time").and_then(serde_json::Value::as_f64);
    Ok(stop_time.is_some_and(|value| {
        (value - stop_time_override).abs() <= f64::EPSILON.max(stop_time_override.abs() * 1e-12)
    }))
}

fn omc_models_map_has_success(payload: &serde_json::Value) -> bool {
    payload
        .get("models")
        .and_then(serde_json::Value::as_object)
        .is_some_and(|models| {
            models.values().any(|model| {
                model.get("status").and_then(serde_json::Value::as_str) == Some("success")
            })
        })
}

pub(super) fn run_msl_tool_command<I, S>(exe: &Path, args: I) -> io::Result<()>
where
    I: IntoIterator<Item = S>,
    S: AsRef<std::ffi::OsStr>,
{
    let args_vec: Vec<std::ffi::OsString> = args
        .into_iter()
        .map(|arg| arg.as_ref().to_os_string())
        .collect();
    let mut cmd = Command::new(exe);
    cmd.args(&args_vec);
    cmd.stdout(std::process::Stdio::inherit());
    cmd.stderr(std::process::Stdio::inherit());
    let rendered_args = args_vec
        .iter()
        .map(|arg| arg.to_string_lossy())
        .collect::<Vec<_>>()
        .join(" ");
    println!(
        "Running parity command: {} {}",
        exe.display(),
        rendered_args
    );
    let status = cmd.status()?;
    if status.success() {
        return Ok(());
    }
    Err(io::Error::other(format!(
        "command '{}' failed (status={})",
        exe.display(),
        status
    )))
}

pub(super) fn omc_parity_workers() -> usize {
    parity_config()
        .omc_parity_workers
        .filter(|value| *value > 0)
        .unwrap_or_else(|| msl_stage_parallelism().clamp(1, OMC_PARITY_WORKERS_DEFAULT_MAX))
}

pub(super) fn omc_parity_threads() -> usize {
    OMC_PARITY_THREADS_DEFAULT
}

pub(super) fn force_omc_parity_refresh_enabled() -> bool {
    parity_config().force_omc_parity_refresh.unwrap_or(false)
}
