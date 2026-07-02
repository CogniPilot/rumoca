use anyhow::{Context, Result, ensure};
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};

use super::VerifyMslParityArgs;

const MSL_QUALITY_BASELINE_ASSET_URL: &str = "https://github.com/CogniPilot/rumoca/releases/download/msl-quality-baseline/msl_quality_baseline.json";
const MSL_QUALITY_BASELINE_FALLBACK_REL: &str =
    "crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json";

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
        println!(
            "MSL quality baseline: using explicit {}",
            resolved.display()
        );
        return Ok(resolved);
    }

    if !args.no_remote_quality_baseline
        && let Some(path) = download_msl_quality_baseline_asset(root)?
    {
        return Ok(path);
    }

    let fallback = checked_in_msl_quality_baseline_path(root);
    ensure!(
        fallback.is_file(),
        "checked-in MSL quality baseline not found: {}",
        fallback.display()
    );
    if args.no_remote_quality_baseline {
        println!(
            "MSL quality baseline: using checked-in fallback because --no-remote-quality-baseline was set ({})",
            fallback.display()
        );
    } else {
        println!(
            "MSL quality baseline: using checked-in fallback {}",
            fallback.display()
        );
    }
    Ok(fallback)
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

    if let Err(error) = serde_json::from_slice::<serde_json::Value>(&data) {
        eprintln!(
            "MSL quality baseline: downloaded promoted asset is not valid JSON ({error}); falling back to checked-in baseline."
        );
        return Ok(None);
    }

    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(&output_path, data)
        .with_context(|| format!("failed to write {}", output_path.display()))?;
    println!(
        "MSL quality baseline: using downloaded promoted asset {}",
        output_path.display()
    );
    Ok(Some(output_path))
}

#[cfg(test)]
mod tests {
    use super::super::VerifyMslParityArgs;
    use std::path::PathBuf;

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
}
