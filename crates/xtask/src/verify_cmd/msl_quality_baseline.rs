use anyhow::{Context, Result, bail, ensure};
use serde::{Deserialize, Deserializer};
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};

use super::VerifyMslParityArgs;

const MSL_QUALITY_BASELINE_ASSET_URL: &str = "https://github.com/CogniPilot/rumoca/releases/download/msl-quality-baseline/msl_quality_baseline.json";
const MSL_QUALITY_BASELINE_FALLBACK_REL: &str =
    "crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json";
const MSL_QUALITY_GATE_VERSION: u64 = 1;
const MSL_QUALITY_RUN_SCOPE: &str = "full";

#[derive(Debug, Deserialize)]
struct MslQualityBaselineHeader {
    quality_gate_version: u64,
    run_scope: String,
    #[serde(deserialize_with = "deserialize_omc_version")]
    omc_version: String,
    sim_target_models: usize,
    #[serde(default)]
    omc_context_migration: Option<OmcContextMigration>,
}

#[derive(Debug, Deserialize)]
struct OmcContextMigration {
    #[serde(deserialize_with = "deserialize_omc_version")]
    from_omc_version: String,
    #[serde(deserialize_with = "deserialize_omc_version")]
    to_omc_version: String,
    sim_target_models: usize,
}

fn deserialize_omc_version<'de, D>(deserializer: D) -> std::result::Result<String, D::Error>
where
    D: Deserializer<'de>,
{
    let value = serde_json::Value::deserialize(deserializer)?;
    value
        .as_str()
        .map(str::trim)
        .filter(|version| !version.is_empty())
        .map(str::to_owned)
        .ok_or_else(|| serde::de::Error::custom("omc_version must be a non-empty string"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BaselineChoice {
    Promoted,
    CheckedInMigration,
}

pub(super) fn resolve_msl_quality_baseline(
    root: &Path,
    args: &VerifyMslParityArgs,
) -> Result<PathBuf> {
    if let Some(path) = args.quality_baseline.as_ref() {
        let resolved = resolve_workspace_path(root, path);
        ensure!(
            resolved.is_file(),
            "explicit MSL quality baseline not found: {}",
            resolved.display()
        );
        load_baseline_header(&resolved)?;
        println!(
            "MSL quality baseline: using explicit {}",
            resolved.display()
        );
        return Ok(resolved);
    }

    let checked_in = checked_in_msl_quality_baseline_path(root);
    ensure!(
        checked_in.is_file(),
        "checked-in MSL quality baseline not found: {}",
        checked_in.display()
    );
    let checked_in_header = load_baseline_header(&checked_in)?;
    if !args.no_remote_quality_baseline
        && let Some(promoted) = download_msl_quality_baseline_asset(root)?
    {
        let promoted_header = load_baseline_header(&promoted)?;
        match choose_baseline(&promoted_header, &checked_in_header)? {
            BaselineChoice::Promoted => return Ok(promoted),
            BaselineChoice::CheckedInMigration => {
                println!(
                    "MSL quality baseline: checked-in baseline declares an OMC context migration; using {}",
                    checked_in.display()
                );
                return Ok(checked_in);
            }
        }
    }

    if args.no_remote_quality_baseline {
        println!(
            "MSL quality baseline: using checked-in fallback because --no-remote-quality-baseline was set ({})",
            checked_in.display()
        );
    } else {
        println!(
            "MSL quality baseline: using checked-in fallback {}",
            checked_in.display()
        );
    }
    Ok(checked_in)
}

fn choose_baseline(
    promoted: &MslQualityBaselineHeader,
    checked_in: &MslQualityBaselineHeader,
) -> Result<BaselineChoice> {
    validate_context_migration(checked_in)?;
    if promoted.omc_version == checked_in.omc_version {
        return Ok(BaselineChoice::Promoted);
    }

    let Some(migration) = checked_in.omc_context_migration.as_ref() else {
        bail!(
            "MSL quality baseline OMC context differs without an explicit migration (promoted={}, checked-in={})",
            promoted.omc_version,
            checked_in.omc_version
        );
    };
    ensure!(
        migration.from_omc_version == promoted.omc_version,
        "MSL OMC context migration source differs (declared={}, promoted={})",
        migration.from_omc_version,
        promoted.omc_version
    );
    ensure!(
        promoted.sim_target_models == checked_in.sim_target_models,
        "MSL OMC context migration target set differs (promoted={}, checked-in={})",
        promoted.sim_target_models,
        checked_in.sim_target_models
    );
    Ok(BaselineChoice::CheckedInMigration)
}

fn validate_context_migration(baseline: &MslQualityBaselineHeader) -> Result<()> {
    let Some(migration) = baseline.omc_context_migration.as_ref() else {
        return Ok(());
    };
    ensure!(
        migration.from_omc_version != migration.to_omc_version,
        "MSL OMC context migration source and target must differ"
    );
    ensure!(
        migration.to_omc_version == baseline.omc_version,
        "MSL OMC context migration target differs (declared={}, baseline={})",
        migration.to_omc_version,
        baseline.omc_version
    );
    ensure!(
        migration.sim_target_models == baseline.sim_target_models,
        "MSL OMC context migration target set differs (declared={}, baseline={})",
        migration.sim_target_models,
        baseline.sim_target_models
    );
    Ok(())
}

fn load_baseline_header(path: &Path) -> Result<MslQualityBaselineHeader> {
    let data = fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
    let baseline: MslQualityBaselineHeader = serde_json::from_slice(&data).map_err(|error| {
        anyhow::anyhow!(
            "invalid MSL quality baseline JSON in {}: {error}",
            path.display()
        )
    })?;
    ensure!(
        baseline.quality_gate_version == MSL_QUALITY_GATE_VERSION,
        "unsupported MSL quality_gate_version={} in {}",
        baseline.quality_gate_version,
        path.display()
    );
    ensure!(
        baseline.run_scope == MSL_QUALITY_RUN_SCOPE,
        "MSL quality baseline run_scope must be '{}' in {}",
        MSL_QUALITY_RUN_SCOPE,
        path.display()
    );
    ensure!(
        baseline.sim_target_models > 0,
        "MSL quality baseline sim_target_models must be positive in {}",
        path.display()
    );
    validate_context_migration(&baseline)
        .with_context(|| format!("invalid OMC context migration in {}", path.display()))?;
    Ok(baseline)
}

fn downloaded_msl_quality_baseline_path(root: &Path) -> PathBuf {
    root.join("target/msl/baselines/msl_quality_baseline.json")
}

fn checked_in_msl_quality_baseline_path(root: &Path) -> PathBuf {
    root.join(MSL_QUALITY_BASELINE_FALLBACK_REL)
}

fn resolve_workspace_path(root: &Path, path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        root.join(path)
    }
}

fn download_msl_quality_baseline_asset(root: &Path) -> Result<Option<PathBuf>> {
    let output_path = downloaded_msl_quality_baseline_path(root);
    println!(
        "MSL quality baseline: downloading latest promoted asset from {}",
        MSL_QUALITY_BASELINE_ASSET_URL
    );
    let response = match ureq::get(MSL_QUALITY_BASELINE_ASSET_URL).call() {
        Ok(response) => response,
        Err(error) => {
            eprintln!(
                "MSL quality baseline: failed to download latest promoted asset ({error}); falling back to checked-in baseline."
            );
            return Ok(None);
        }
    };

    let content_len = response
        .header("content-length")
        .and_then(|value| value.parse::<usize>().ok())
        .unwrap_or(0);
    let mut data = Vec::with_capacity(content_len);
    if let Err(error) = response.into_reader().read_to_end(&mut data) {
        eprintln!(
            "MSL quality baseline: failed to read latest promoted asset ({error}); falling back to checked-in baseline."
        );
        return Ok(None);
    }

    serde_json::from_slice::<serde_json::Value>(&data).with_context(|| {
        format!(
            "downloaded promoted MSL quality baseline from {MSL_QUALITY_BASELINE_ASSET_URL} is not valid JSON"
        )
    })?;

    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(&output_path, data)
        .with_context(|| format!("failed to write {}", output_path.display()))?;
    println!(
        "MSL quality baseline: downloaded promoted asset {}",
        output_path.display()
    );
    Ok(Some(output_path))
}

#[cfg(test)]
mod tests {
    use super::{
        super::VerifyMslParityArgs, BaselineChoice, MslQualityBaselineHeader, OmcContextMigration,
        choose_baseline, load_baseline_header, validate_context_migration,
    };
    use serde_json::json;
    use std::{fs, path::PathBuf};

    fn header(omc_version: &str) -> MslQualityBaselineHeader {
        MslQualityBaselineHeader {
            quality_gate_version: 1,
            run_scope: "full".to_string(),
            omc_version: omc_version.to_string(),
            sim_target_models: 566,
            omc_context_migration: None,
        }
    }

    fn migration(from: &str, to: &str) -> OmcContextMigration {
        OmcContextMigration {
            from_omc_version: from.to_string(),
            to_omc_version: to.to_string(),
            sim_target_models: 566,
        }
    }

    #[test]
    fn msl_parity_config_forwards_resolved_quality_baseline_path() {
        let args = VerifyMslParityArgs {
            quality_baseline: Some(PathBuf::from(
                "target/msl/baselines/msl_quality_baseline.json",
            )),
            ..VerifyMslParityArgs::default()
        };
        let config = args.to_parity_config_json();

        assert_eq!(
            config
                .get("quality_baseline_file")
                .and_then(serde_json::Value::as_str),
            Some("target/msl/baselines/msl_quality_baseline.json")
        );
    }

    #[test]
    fn default_msl_parity_uses_baseline_relative_quality_gate() {
        assert!(VerifyMslParityArgs::default().uses_baseline_relative_quality_gate());
        let short_run = VerifyMslParityArgs {
            sim_set: Some("short".to_string()),
            ..VerifyMslParityArgs::default()
        };
        assert!(!short_run.uses_baseline_relative_quality_gate());
    }

    #[test]
    fn checked_in_baseline_declares_omc_context_migration() {
        let promoted = header("OpenModelica 1.27.0");
        let mut checked_in = header("a96aa1a-cmake");
        checked_in.omc_context_migration = Some(migration("OpenModelica 1.27.0", "a96aa1a-cmake"));

        assert_eq!(
            choose_baseline(&promoted, &checked_in).expect("declared migration should select"),
            BaselineChoice::CheckedInMigration
        );
    }

    #[test]
    fn same_omc_context_keeps_promoted_baseline() {
        assert_eq!(
            choose_baseline(&header("a96aa1a-cmake"), &header("a96aa1a-cmake"))
                .expect("same context should select"),
            BaselineChoice::Promoted
        );
    }

    #[test]
    fn changed_omc_context_requires_exact_migration_declaration() {
        let promoted = header("old");
        let mut checked_in = header("new");
        assert!(choose_baseline(&promoted, &checked_in).is_err());

        checked_in.omc_context_migration = Some(migration("new", "old"));
        assert!(choose_baseline(&promoted, &checked_in).is_err());

        checked_in.omc_context_migration = Some(migration("old", "new"));
        checked_in
            .omc_context_migration
            .as_mut()
            .unwrap()
            .sim_target_models = 565;
        assert!(choose_baseline(&promoted, &checked_in).is_err());
    }

    #[test]
    fn migration_must_be_internally_consistent_without_promoted_baseline() {
        let mut baseline = header("new");
        baseline.omc_context_migration = Some(migration("old", "other"));
        assert!(validate_context_migration(&baseline).is_err());

        baseline.omc_context_migration = Some(migration("new", "new"));
        assert!(validate_context_migration(&baseline).is_err());

        baseline.omc_context_migration = Some(migration("old", "new"));
        baseline
            .omc_context_migration
            .as_mut()
            .unwrap()
            .sim_target_models = 565;
        assert!(validate_context_migration(&baseline).is_err());
    }

    #[test]
    fn baseline_header_rejects_missing_or_invalid_omc_version() {
        let temp = tempfile::tempdir().expect("temporary directory should be available");
        let invalid_versions = [None, Some(json!(null)), Some(json!(7)), Some(json!(" "))];

        for (index, version) in invalid_versions.into_iter().enumerate() {
            let path = temp.path().join(format!("invalid-{index}.json"));
            let mut baseline = json!({
                "quality_gate_version": 1,
                "run_scope": "full",
                "sim_target_models": 566
            });
            if let Some(version) = version {
                baseline["omc_version"] = version;
            }
            fs::write(&path, baseline.to_string()).expect("fixture should be writable");
            let error = load_baseline_header(&path).expect_err("invalid context must fail");
            assert!(error.to_string().contains("omc_version"), "{error}");
        }
    }
}
