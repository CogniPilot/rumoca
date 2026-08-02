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
    pub(super) use_experiment_stop_time: bool,
    pub(super) stop_time_override: Option<f64>,
}

pub(super) fn simulation_stop_time_override() -> Option<f64> {
    // No stop-time override; use the model's experiment annotation.
    None
}

pub(super) fn current_simulation_parity_cache_policy() -> SimulationParityCachePolicy {
    let stop_time_override = simulation_stop_time_override();
    SimulationParityCachePolicy {
        // Match the timeout actually passed to `rumoca-msl-tools`. The effective
        // value scales with the Rumoca simulation ceiling; keying/checking only
        // the 120-second floor makes every ordinary 10-second run miss its own
        // freshly persisted 300-second cache entry.
        batch_timeout_seconds: omc_sim_reference_timeout_secs(),
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

fn simulation_parity_cache_trace_dir(cache_path: &Path) -> PathBuf {
    cache_path.with_extension("traces")
}

const CACHE_TRACE_DIGESTS_FIELD: &str = "cache_trace_blake3";

fn safe_relative_cache_path(path: &Path) -> bool {
    path.is_relative()
        && path
            .components()
            .all(|component| matches!(component, std::path::Component::Normal(_)))
}

fn cached_omc_trace_paths(payload: &serde_json::Value) -> Option<Vec<PathBuf>> {
    let Some(models) = payload.get("models").and_then(serde_json::Value::as_object) else {
        return Some(Vec::new());
    };
    let mut paths = Vec::new();
    for model in models.values() {
        if model.get("status").and_then(serde_json::Value::as_str) != Some("success") {
            continue;
        }
        let trace = model
            .get("trace_file")
            .and_then(serde_json::Value::as_str)?;
        let path = PathBuf::from(trace);
        if !safe_relative_cache_path(&path) {
            return None;
        }
        paths.push(path);
    }
    Some(paths)
}

fn cached_trace_digest(path: &Path) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut hasher = blake3::Hasher::new();
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let count = file.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        hasher.update(&buffer[..count]);
    }
    Ok(hasher.finalize().to_hex().to_string())
}

fn attach_cached_trace_digests(
    payload: &mut serde_json::Value,
    source_root: &Path,
) -> io::Result<()> {
    let paths = cached_omc_trace_paths(payload)
        .ok_or_else(|| io::Error::other("OMC cache contains an unsafe or missing trace path"))?;
    let mut digests = serde_json::Map::new();
    for relative_path in paths {
        let source = source_root.join(&relative_path);
        digests.insert(
            relative_path.to_string_lossy().into_owned(),
            cached_trace_digest(&source)?.into(),
        );
    }
    payload[CACHE_TRACE_DIGESTS_FIELD] = digests.into();
    Ok(())
}

fn cached_traces_are_valid(cache_path: &Path, payload: &serde_json::Value) -> io::Result<bool> {
    let Some(paths) = cached_omc_trace_paths(payload) else {
        return Ok(false);
    };
    let Some(digests) = payload
        .get(CACHE_TRACE_DIGESTS_FIELD)
        .and_then(serde_json::Value::as_object)
    else {
        return Ok(false);
    };
    let trace_root = simulation_parity_cache_trace_dir(cache_path);
    for relative_path in paths {
        let key = relative_path.to_string_lossy();
        let Some(expected) = digests
            .get(key.as_ref())
            .and_then(serde_json::Value::as_str)
        else {
            return Ok(false);
        };
        let trace_path = trace_root.join(&relative_path);
        if !trace_path.is_file() || cached_trace_digest(&trace_path)? != expected {
            return Ok(false);
        }
    }
    Ok(true)
}

fn transfer_cached_omc_traces(
    payload: &serde_json::Value,
    source_root: &Path,
    destination_root: &Path,
) -> io::Result<()> {
    let paths = cached_omc_trace_paths(payload)
        .ok_or_else(|| io::Error::other("OMC cache contains an unsafe or missing trace path"))?;
    for relative_path in paths {
        let source = source_root.join(&relative_path);
        if !source.is_file() {
            continue;
        }
        let destination = destination_root.join(&relative_path);
        if let Some(parent) = destination.parent() {
            fs::create_dir_all(parent)?;
        }
        link_or_copy_cached_omc_trace(&source, &destination)?;
    }
    Ok(())
}

fn link_or_copy_cached_omc_trace(source: &Path, destination: &Path) -> io::Result<()> {
    match fs::symlink_metadata(destination) {
        Ok(metadata) if metadata.is_file() || metadata.file_type().is_symlink() => {
            fs::remove_file(destination)?;
        }
        Ok(_) => {
            return Err(io::Error::other(format!(
                "cached OMC trace destination '{}' is not a file",
                destination.display()
            )));
        }
        Err(error) if error.kind() == io::ErrorKind::NotFound => {}
        Err(error) => return Err(error),
    }
    // Full MSL trace sets are hundreds of MB. The workspace cache and results
    // normally share a filesystem, so a hard link keeps the keyed artifact
    // alive across cleanup without doubling disk use. A copy preserves
    // portability when the paths happen to cross filesystem boundaries.
    if fs::hard_link(source, destination).is_err() {
        fs::copy(source, destination).map_err(|error| {
            io::Error::other(format!(
                "failed to copy cached OMC trace '{}' -> '{}': {error}",
                source.display(),
                destination.display()
            ))
        })?;
    }
    Ok(())
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
    if !cached_traces_are_valid(cache_path, &payload)? {
        return Err(io::Error::other(format!(
            "simulation parity cache '{}' has missing or stale OMC traces",
            cache_path.display()
        )));
    }
    if let Some(parent) = active_path.parent() {
        fs::create_dir_all(parent)?;
    }
    let sanitized = sanitize_simulation_parity_cache_payload(payload);
    let active_root = active_path.parent().unwrap_or_else(|| Path::new("."));
    transfer_cached_omc_traces(
        &sanitized,
        &simulation_parity_cache_trace_dir(cache_path),
        active_root,
    )?;
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
    // These fields describe the disposable results directory that produced the
    // cache entry. Neither participates in OMC result reuse, and retaining them
    // would make a workspace-global cache carry machine-specific absolute paths.
    root.remove("target_selection");

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
        model.remove("result_file");
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
    let mut sanitized = sanitize_simulation_parity_cache_payload(payload);
    let active_root = active_path.parent().unwrap_or_else(|| Path::new("."));
    attach_cached_trace_digests(&mut sanitized, active_root)?;
    transfer_cached_omc_traces(
        &sanitized,
        active_root,
        &simulation_parity_cache_trace_dir(cache_path),
    )?;
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
    let output = std::process::Command::new("omc")
        .arg("--version")
        .output()?;
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
    let use_experiment_stop_time = payload
        .get("use_experiment_stop_time")
        .and_then(serde_json::Value::as_bool);
    if use_experiment_stop_time != Some(policy.use_experiment_stop_time) {
        return Ok(false);
    }
    if let Some(stop_time_override) = policy.stop_time_override {
        let stop_time = payload.get("stop_time").and_then(serde_json::Value::as_f64);
        if !stop_time.is_some_and(|value| {
            (value - stop_time_override).abs() <= f64::EPSILON.max(stop_time_override.abs() * 1e-12)
        }) {
            return Ok(false);
        }
    }
    cached_traces_are_valid(path, &payload)
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
    msl_stage_parallelism()
}

pub(super) fn omc_parity_threads() -> usize {
    OMC_PARITY_THREADS_DEFAULT
}

pub(super) fn force_omc_parity_refresh_enabled() -> bool {
    false
}
