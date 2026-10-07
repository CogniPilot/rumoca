//! The `cargo` argument lists of `cargo xtask coverage run`: the instrumented
//! test run and its summary report.

use crate::{CoverageRunArgs, test_cmd};

/// The `cargo llvm-cov` run and `cargo llvm-cov report` argument lists for
/// `args`: the whole workspace when no package is selected, the feature-gated
/// host tests always, and the artifacts under `target/llvm-cov`.
pub(crate) fn cargo_args(args: &CoverageRunArgs) -> (Vec<String>, Vec<String>) {
    let package_args = package_args(&args.packages);
    let mut full_args = vec!["llvm-cov".to_string()];
    if args.packages.is_empty() {
        full_args.push("--workspace".to_string());
    }
    full_args.extend([
        "--tests".to_string(),
        "--json".to_string(),
        "--output-path".to_string(),
        "target/llvm-cov/workspace-full.json".to_string(),
    ]);
    full_args.extend(
        test_cmd::COVERAGE_TEST_FEATURES
            .iter()
            .map(ToString::to_string),
    );
    full_args.extend(package_args.clone());
    if args.no_clean {
        full_args.push("--no-clean".to_string());
    }
    let mut summary_args = vec![
        "llvm-cov".to_string(),
        "report".to_string(),
        "--json".to_string(),
        "--summary-only".to_string(),
        "--output-path".to_string(),
        "target/llvm-cov/workspace-summary.json".to_string(),
    ];
    summary_args.extend(package_args);
    (full_args, summary_args)
}

/// `--package` arguments for the selected workspace packages.
fn package_args(packages: &[String]) -> Vec<String> {
    packages
        .iter()
        .flat_map(|package| ["--package".to_string(), package.clone()])
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_workspace_run_enables_the_feature_gated_host_tests() {
        let (full, summary) = cargo_args(&CoverageRunArgs {
            no_clean: false,
            packages: Vec::new(),
        });
        assert_eq!(
            full,
            [
                "llvm-cov",
                "--workspace",
                "--tests",
                "--json",
                "--output-path",
                "target/llvm-cov/workspace-full.json",
                "--features",
                "rumoca-bind-wasm/native-assignments",
            ]
        );
        assert_eq!(
            summary.last().map(String::as_str),
            Some("target/llvm-cov/workspace-summary.json")
        );
    }

    #[test]
    fn a_package_run_selects_each_package_in_both_commands_and_keeps_artifacts() {
        let (full, summary) = cargo_args(&CoverageRunArgs {
            no_clean: true,
            packages: vec!["rumoca-core".to_string(), "xtask".to_string()],
        });
        assert!(!full.contains(&"--workspace".to_string()));
        assert_eq!(full.last().map(String::as_str), Some("--no-clean"));
        let packages = ["--package", "rumoca-core", "--package", "xtask"];
        assert!(full.windows(4).any(|window| window == packages));
        assert!(summary.windows(4).any(|window| window == packages));
    }
}
