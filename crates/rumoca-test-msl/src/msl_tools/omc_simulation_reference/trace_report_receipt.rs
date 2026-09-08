//! Run-bound validation of a trace-comparison report.
//!
//! JSON shape validation proves only that a channel partition is internally
//! consistent. This boundary additionally reconstructs every measured (and
//! measurable-but-empty) partition from the exact trace bytes named by the OMC
//! reference. The opaque result is the only value the orchestration layer may
//! turn into current-run evidence.

use super::*;

/// Opaque proof that one exact report was checked against one exact set of
/// source traces.
#[derive(Debug)]
pub struct ValidatedTraceReport {
    results_dir: PathBuf,
    payload: Value,
    reference_payload: Value,
    report_digest: String,
    reference_digest: String,
    source_evidence_digest: String,
}

impl ValidatedTraceReport {
    pub(crate) fn results_dir(&self) -> &Path {
        &self.results_dir
    }

    pub fn payload(&self) -> &Value {
        &self.payload
    }

    pub fn report_digest(&self) -> &str {
        &self.report_digest
    }

    pub fn reference_payload(&self) -> &Value {
        &self.reference_payload
    }

    pub fn reference_digest(&self) -> &str {
        &self.reference_digest
    }

    pub(crate) fn reference_omc_version(&self) -> Option<String> {
        self.reference_payload
            .get("omc_version")
            .and_then(Value::as_str)
            .map(str::trim)
            .filter(|version| !version.is_empty())
            .map(str::to_string)
    }

    pub fn source_evidence_digest(&self) -> &str {
        &self.source_evidence_digest
    }
}

/// Validate exact report bytes against exact source traces from the exact OMC
/// reference bytes used by this run.
pub fn validate_trace_report_against_sources(
    paths: &MslPaths,
    report_bytes: &[u8],
    omc_reference_bytes: &[u8],
) -> Result<ValidatedTraceReport> {
    let payload: Value =
        serde_json::from_slice(report_bytes).context("current trace comparison is invalid JSON")?;
    let reference: Value = serde_json::from_slice(omc_reference_bytes)
        .context("current OMC simulation reference is invalid JSON")?;
    let report_models = required_object(&payload, "models", "trace comparison")?;
    if report_models.is_empty() {
        bail!("current trace comparison is vacuous: it carries no measured models");
    }
    let reference_models = required_object(&reference, "models", "OMC simulation reference")?;

    let mut evidence = blake3::Hasher::new();
    hash_field(
        &mut evidence,
        b"results-dir",
        paths.results_dir.as_os_str().as_encoded_bytes(),
    );
    hash_field(&mut evidence, b"trace-report", report_bytes);
    hash_field(&mut evidence, b"omc-reference", omc_reference_bytes);

    for (model_name, report_metric) in report_models {
        let source = source_model(reference_models, model_name)?;
        let (rumoca, reference) = load_source_pair(paths, model_name, source, &mut evidence)?;
        let recorded = parse_trace_model_metric(report_metric.clone())
            .with_context(|| format!("invalid trace metric for '{model_name}'"))?;
        let recomputed = compare_model_traces(model_name, &rumoca, &reference)
            .with_context(|| format!("cannot reproduce trace metric for '{model_name}'"))?;
        if serde_json::to_value(&recorded)? != serde_json::to_value(&recomputed)? {
            bail!("trace metric for '{model_name}' does not match the exact current source traces");
        }
    }

    validate_empty_comparisons(paths, &payload, reference_models, &mut evidence)?;

    Ok(ValidatedTraceReport {
        results_dir: paths.results_dir.clone(),
        payload,
        reference_payload: reference,
        report_digest: blake3::hash(report_bytes).to_hex().to_string(),
        reference_digest: blake3::hash(omc_reference_bytes).to_hex().to_string(),
        source_evidence_digest: evidence.finalize().to_hex().to_string(),
    })
}

/// Bind an exact merged report to source-validated shard reports.
pub fn validate_merged_trace_report(
    paths: &MslPaths,
    report_bytes: &[u8],
    reference_bytes: &[u8],
    shards: &[ValidatedTraceReport],
) -> Result<ValidatedTraceReport> {
    if shards.is_empty() {
        bail!("merged trace comparison has no source-bound shard reports");
    }
    let payload: Value =
        serde_json::from_slice(report_bytes).context("merged trace comparison is invalid JSON")?;
    let reference_payload: Value = serde_json::from_slice(reference_bytes)
        .context("merged OMC simulation reference is invalid JSON")?;
    for key in [
        "models",
        "missing_trace",
        "skipped",
        "trace_nonidentifiable",
    ] {
        let merged = required_object(&payload, key, "merged trace comparison")?;
        let witnessed = merge_witnessed_records(shards, key)?;
        if &witnessed != merged {
            bail!("merged `{key}` records do not equal the source-bound shard records");
        }
    }
    let merged_reference_models = required_object(
        &reference_payload,
        "models",
        "merged OMC simulation reference",
    )?;
    let mut witnessed_reference_models = serde_json::Map::new();
    for shard in shards {
        for (model, value) in required_object(
            shard.reference_payload(),
            "models",
            "validated shard OMC reference",
        )? {
            if witnessed_reference_models
                .insert(model.clone(), value.clone())
                .is_some()
            {
                bail!("source-bound shards duplicate OMC reference model `{model}`");
            }
        }
    }
    if &witnessed_reference_models != merged_reference_models {
        bail!("merged OMC model records do not equal the source-bound shard records");
    }
    let mut evidence = blake3::Hasher::new();
    hash_field(
        &mut evidence,
        b"results-dir",
        paths.results_dir.as_os_str().as_encoded_bytes(),
    );
    hash_field(&mut evidence, b"merged-report", report_bytes);
    hash_field(&mut evidence, b"merged-reference", reference_bytes);
    for shard in shards {
        hash_field(
            &mut evidence,
            shard.report_digest().as_bytes(),
            shard.source_evidence_digest().as_bytes(),
        );
    }
    Ok(ValidatedTraceReport {
        results_dir: paths.results_dir.clone(),
        payload,
        reference_payload,
        report_digest: blake3::hash(report_bytes).to_hex().to_string(),
        reference_digest: blake3::hash(reference_bytes).to_hex().to_string(),
        source_evidence_digest: evidence.finalize().to_hex().to_string(),
    })
}

fn merge_witnessed_records(
    shards: &[ValidatedTraceReport],
    key: &str,
) -> Result<serde_json::Map<String, Value>> {
    let mut witnessed = serde_json::Map::new();
    for shard in shards {
        for (model, value) in required_object(shard.payload(), key, "validated shard")? {
            if witnessed.insert(model.clone(), value.clone()).is_some() {
                bail!("source-bound shards duplicate `{key}` model `{model}`");
            }
        }
    }
    Ok(witnessed)
}

fn required_object<'a>(
    payload: &'a Value,
    key: &str,
    artifact: &str,
) -> Result<&'a serde_json::Map<String, Value>> {
    payload
        .get(key)
        .and_then(Value::as_object)
        .with_context(|| format!("{artifact} is missing its '{key}' object"))
}

fn source_model(
    reference_models: &serde_json::Map<String, Value>,
    model_name: &str,
) -> Result<SimModelResult> {
    let payload = reference_models.get(model_name).with_context(|| {
        format!("OMC simulation reference has no source record for '{model_name}'")
    })?;
    serde_json::from_value(payload.clone())
        .with_context(|| format!("invalid OMC source record for '{model_name}'"))
}

fn load_source_pair(
    paths: &MslPaths,
    model_name: &str,
    source: SimModelResult,
    evidence: &mut blake3::Hasher,
) -> Result<(SimTrace, SimTrace)> {
    let rumoca_path = resolve_rumoca_trace_path(paths, model_name, &source)
        .with_context(|| format!("'{model_name}' declares no Rumoca source trace"))?;
    let omc_path = resolve_declared_omc_trace_path(paths, model_name, &source)
        .with_context(|| format!("'{model_name}' declares no OMC source trace"))?;
    let rumoca = read_bound_trace("Rumoca", model_name, &rumoca_path, evidence)?;
    let omc = read_bound_trace("OMC", model_name, &omc_path, evidence)?;
    Ok((rumoca, omc))
}

fn read_bound_trace(
    owner: &str,
    model_name: &str,
    path: &Path,
    evidence: &mut blake3::Hasher,
) -> Result<SimTrace> {
    let bytes = std::fs::read(path).with_context(|| {
        format!(
            "failed to read {owner} source trace '{}' for '{model_name}'",
            path.display()
        )
    })?;
    hash_field(evidence, model_name.as_bytes(), owner.as_bytes());
    hash_field(evidence, path.to_string_lossy().as_bytes(), &bytes);
    serde_json::from_slice(&bytes).with_context(|| {
        format!(
            "invalid {owner} source trace '{}' for '{model_name}'",
            path.display()
        )
    })
}

fn validate_empty_comparisons(
    paths: &MslPaths,
    report: &Value,
    reference_models: &serde_json::Map<String, Value>,
    evidence: &mut blake3::Hasher,
) -> Result<()> {
    let skipped = required_object(report, "skipped", "trace comparison")?;
    for (model_name, payload) in skipped {
        let record: TraceExitRecord = serde_json::from_value(payload.clone())
            .with_context(|| format!("invalid skipped record for '{model_name}'"))?;
        let expected_partition = match record {
            TraceExitRecord::NoCommonVariables {
                channel_partition, ..
            } => Some((channel_partition, true)),
            TraceExitRecord::NoComparableSamples {
                channel_partition, ..
            } => Some((channel_partition, false)),
            _ => None,
        };
        let Some((recorded, expect_no_common)) = expected_partition else {
            continue;
        };
        let source = source_model(reference_models, model_name)?;
        let (rumoca, reference) = load_source_pair(paths, model_name, source, evidence)?;
        let (recomputed, is_no_common) = match compare_model_traces(model_name, &rumoca, &reference)
        {
            Ok(_) => {
                bail!("empty comparison record for '{model_name}' unexpectedly produced a metric")
            }
            Err(TraceCompareError::NoCommonVariables { channel_partition }) => {
                (channel_partition, true)
            }
            Err(TraceCompareError::NoComparableSamples { channel_partition }) => {
                (channel_partition, false)
            }
            Err(other) => bail!("cannot reproduce empty comparison for '{model_name}': {other}"),
        };
        if expect_no_common != is_no_common || recorded != recomputed {
            bail!(
                "empty-comparison channel partition for '{model_name}' does not match the exact current source traces"
            );
        }
    }
    Ok(())
}

fn hash_field(hasher: &mut blake3::Hasher, label: &[u8], bytes: &[u8]) {
    hasher.update(&(label.len() as u64).to_le_bytes());
    hasher.update(label);
    hasher.update(&(bytes.len() as u64).to_le_bytes());
    hasher.update(bytes);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn trace(model: &str, channel: &str) -> SimTrace {
        SimTrace {
            model_name: Some(model.to_string()),
            times: vec![0.0, 1.0],
            names: vec![channel.to_string()],
            data: vec![vec![Some(0.0), Some(1.0)]],
            variable_meta: None,
            certification_profile: None,
        }
    }

    fn metric_record(model: &str, source: &SimTrace) -> Value {
        let metric = compare_model_traces(model, source, source).expect("fixture comparison");
        let mut record = serde_json::to_value(metric)
            .expect("serialize metric")
            .as_object()
            .cloned()
            .expect("metric object");
        for key in [
            "state_selection",
            "rumoca_sim_wall_seconds",
            "rumoca_sim_seconds",
            "rumoca_sim_build_seconds",
            "rumoca_sim_run_seconds",
            "omc_sim_system_seconds",
            "omc_total_system_seconds",
            "omc_wall_seconds",
        ] {
            record.insert(key.to_string(), Value::Null);
        }
        Value::Object(record)
    }

    fn reference(model: &str) -> Value {
        json!({
            "models": { model: {
                "status": "success",
                "error": null,
                "sim_system_seconds": null,
                "total_system_seconds": null,
                "omc_wall_seconds": null,
                "result_file": null,
                "trace_file": "sim_traces/omc/source.json",
                "trace_error": null,
                "rumoca_status": "sim_ok",
                "rumoca_ic_status": null,
                "rumoca_ic_error": null,
                "rumoca_ic_seconds": null,
                "rumoca_sim_seconds": null,
                "rumoca_sim_build_seconds": null,
                "rumoca_sim_run_seconds": null,
                "rumoca_sim_wall_seconds": null,
                "rumoca_trace_file": "sim_traces/rumoca/source.json",
                "rumoca_trace_error": null,
                "failed_attempts": 0
            }}
        })
    }

    #[test]
    fn transplanted_partition_cannot_mint_source_bound_witness() {
        let temp = tempfile::tempdir().expect("temp dir");
        let results = temp.path().join("results");
        std::fs::create_dir_all(results.join("sim_traces/omc")).expect("OMC trace dir");
        std::fs::create_dir_all(results.join("sim_traces/rumoca")).expect("Rumoca trace dir");
        let actual = trace("M", "actual");
        std::fs::write(
            results.join("sim_traces/omc/source.json"),
            serde_json::to_vec(&actual).expect("serialize trace"),
        )
        .expect("write OMC trace");
        std::fs::write(
            results.join("sim_traces/rumoca/source.json"),
            serde_json::to_vec(&actual).expect("serialize trace"),
        )
        .expect("write Rumoca trace");

        let foreign = trace("M", "foreign");
        let report = json!({
            "models": { "M": metric_record("M", &foreign) },
            "missing_trace": {},
            "skipped": {},
            "trace_nonidentifiable": {}
        });
        let paths = MslPaths::current().with_results_dir(&results);
        let error = validate_trace_report_against_sources(
            &paths,
            &serde_json::to_vec(&report).expect("serialize report"),
            &serde_json::to_vec(&reference("M")).expect("serialize reference"),
        )
        .expect_err("foreign self-consistent partition must be rejected");
        assert!(
            error
                .to_string()
                .contains("does not match the exact current source traces")
        );
    }
}
