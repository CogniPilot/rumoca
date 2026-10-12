use super::*;
use serde_json::Value;

const LEGACY_ASSET_SHA256: &str =
    "6c6d3a77fff81fc8f328321ca1b0d0e8cf7825900e4848cd07801bb141baa4eb";
const LEGACY_SOURCE: &str = "da9d7ca4708e6b5d694a0163d07d5007d61a86eb";
const EVIDENCE_WORKFLOW_SHA256: &str =
    "94b87dc2aeafc6e8dc5a9fe9563053dd6b4514a57ee7c7d4e633e5724eb5fed9";

#[derive(Debug, Clone, Deserialize)]
#[serde(try_from = "BudgetCorrectionWire")]
pub(super) struct BudgetMetadataCorrection {
    wire: BudgetCorrectionWire,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct BudgetCorrectionWire {
    legacy_asset_sha256: String,
    legacy_asset_git_commit: String,
    legacy_quality_gate_version: u64,
    legacy_sim_timeout_seconds: f64,
    evidence_ci_run: u64,
    evidence_workflow_sha256: String,
    model_attempt_timeout_seconds: f64,
    sim_timeout_seconds: f64,
}

impl TryFrom<BudgetCorrectionWire> for BudgetMetadataCorrection {
    type Error = anyhow::Error;

    fn try_from(wire: BudgetCorrectionWire) -> Result<Self> {
        ensure!(
            wire.legacy_asset_sha256 == LEGACY_ASSET_SHA256
                && wire.legacy_asset_git_commit == LEGACY_SOURCE
                && wire.legacy_quality_gate_version == 14,
            "MSL budget metadata correction differs from the reviewed legacy asset"
        );
        ensure!(
            wire.evidence_ci_run == 37_620_374_298
                && wire.evidence_workflow_sha256 == EVIDENCE_WORKFLOW_SHA256,
            "MSL budget metadata correction differs from the immutable measurement recipe"
        );
        ensure!(
            wire.legacy_sim_timeout_seconds == 12.0
                && wire.model_attempt_timeout_seconds == 20.0
                && wire.sim_timeout_seconds == 45.0,
            "MSL budget metadata correction differs from the measured envelope"
        );
        Ok(Self { wire })
    }
}

fn require_budget_fields(document: &Value) -> Result<()> {
    for field in ["model_attempt_timeout_seconds", "sim_timeout_seconds"] {
        ensure!(
            document[field]
                .as_f64()
                .is_some_and(|value| value.is_finite() && value > 0.0),
            "MSL quality baseline {field} must be present, finite and positive"
        );
    }
    Ok(())
}

fn corrected_document(
    raw: &[u8],
    correction: Option<&BudgetMetadataCorrection>,
) -> Result<Option<Value>> {
    let mut document: Value = serde_json::from_slice(raw)?;
    if document.get("model_attempt_timeout_seconds").is_some() {
        require_budget_fields(&document)?;
        return Ok(None);
    }
    let correction =
        correction.context("MSL legacy baseline has no reviewed budget metadata correction")?;
    let proof = &correction.wire;
    ensure!(
        format!("{:x}", Sha256::digest(raw)) == proof.legacy_asset_sha256
            && document["git_commit"].as_str() == Some(proof.legacy_asset_git_commit.as_str())
            && document["quality_gate_version"].as_u64() == Some(proof.legacy_quality_gate_version)
            && document["sim_timeout_seconds"].as_f64() == Some(proof.legacy_sim_timeout_seconds),
        "MSL legacy baseline differs from the exact reviewed budget metadata asset"
    );
    let object = document
        .as_object_mut()
        .context("MSL baseline must be an object")?;
    object.insert(
        "model_attempt_timeout_seconds".to_owned(),
        proof.model_attempt_timeout_seconds.into(),
    );
    object.insert(
        "sim_timeout_seconds".to_owned(),
        proof.sim_timeout_seconds.into(),
    );
    require_budget_fields(&document)?;
    Ok(Some(document))
}

pub(super) fn selected_promoted_baseline(
    root: &Path,
    raw_path: PathBuf,
    correction: Option<&BudgetMetadataCorrection>,
) -> Result<PathBuf> {
    let raw = fs::read(&raw_path).with_context(|| format!("read {}", raw_path.display()))?;
    let Some(document) = corrected_document(&raw, correction)? else {
        return Ok(raw_path);
    };
    let derived = root.join("target/msl/baselines/msl_quality_baseline-budget-corrected.json");
    fs::create_dir_all(derived.parent().context("derived baseline parent")?)?;
    fs::write(&derived, serde_json::to_vec_pretty(&document)?)?;
    println!(
        "MSL quality baseline: corrected only reviewed budget metadata from {}; preserved raw asset and all model/count values in {}",
        raw_path.display(),
        derived.display()
    );
    Ok(derived)
}

#[cfg(test)]
mod tests;
