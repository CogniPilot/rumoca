//! Wall-budget knobs for `cargo xtask verify msl-parity`.
//!
//! Canonical verification and CI use the same named measurement profile.
//! Unprofiled invocations retain the harness defaults; explicit budgets support
//! isolated diagnostics such as the nightly event cohort.
//!
//! Budgets are whole seconds — sub-second precision is meaningless for a wall
//! budget and `f64` would block the `Eq` derive the CLI arg structs rely on.

use clap::{Args, ValueEnum};
use serde_json::{Map, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum MslParityBudgetProfile {
    Ci,
}

impl MslParityBudgetProfile {
    fn as_str(self) -> &'static str {
        match self {
            Self::Ci => "ci",
        }
    }
}

#[derive(Debug, Args, Clone, PartialEq, Eq, Default)]
pub(crate) struct MslParityBudgetArgs {
    /// Shared measurement budgets for canonical verification and CI.
    #[arg(
        long,
        value_enum,
        conflicts_with_all = ["sim_timeout_secs", "ir_solve_timeout_secs", "model_attempt_timeout_secs"]
    )]
    budget_profile: Option<MslParityBudgetProfile>,
    /// Per-model solver wall budget in seconds (raise-only; clamped to at least
    /// the focused harness floor)
    #[arg(long, value_name = "SECS")]
    sim_timeout_secs: Option<u64>,
    /// Per-model Solve-IR lowering wall budget in seconds (raise-only)
    #[arg(long, value_name = "SECS")]
    ir_solve_timeout_secs: Option<u64>,
    /// Per-model, per-phase wall budget in seconds (raise-only diagnostic
    /// override; one attempt per model)
    #[arg(long, value_name = "SECS")]
    model_attempt_timeout_secs: Option<u64>,
    /// Build the OMC reference for every simulation target rather than only the
    /// models rumoca already simulates. Needed by diagnostic lanes that compare
    /// models which are not yet `sim_ok`.
    #[arg(long)]
    all_omc_targets: bool,
}

impl MslParityBudgetArgs {
    /// Set the solver and per-phase wall budgets of an isolated rerun.
    pub(crate) fn set_wall_budgets(&mut self, secs: u64) {
        self.budget_profile = None;
        self.sim_timeout_secs = Some(secs);
        self.model_attempt_timeout_secs = Some(secs);
    }

    /// Write the selected profile or explicit budgets into the harness config map.
    pub(crate) fn insert_into(&self, config: &mut Map<String, Value>) {
        if let Some(profile) = self.budget_profile {
            config.insert("budget_profile".into(), profile.as_str().into());
        }
        for (key, value) in [
            ("sim_timeout_secs", self.sim_timeout_secs),
            ("ir_solve_timeout_secs", self.ir_solve_timeout_secs),
            (
                "model_attempt_timeout_secs",
                self.model_attempt_timeout_secs,
            ),
        ] {
            if let Some(secs) = value {
                config.insert(key.into(), (secs as f64).into());
            }
        }
        if self.all_omc_targets {
            config.insert("all_omc_targets".into(), true.into());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Parser;

    #[derive(Debug, Parser)]
    struct Harness {
        #[command(flatten)]
        budgets: MslParityBudgetArgs,
    }

    fn config_for(argv: &[&str]) -> Map<String, Value> {
        let parsed = Harness::parse_from(argv);
        let mut config = Map::new();
        parsed.budgets.insert_into(&mut config);
        config
    }

    #[test]
    fn unset_budgets_write_nothing_so_harness_defaults_apply() {
        assert!(config_for(&["verify-msl-parity"]).is_empty());
    }

    #[test]
    fn budgets_are_written_as_seconds_for_the_harness_config() {
        let config = config_for(&[
            "verify-msl-parity",
            "--sim-timeout-secs",
            "300",
            "--ir-solve-timeout-secs",
            "60",
            "--model-attempt-timeout-secs",
            "420",
            "--all-omc-targets",
        ]);
        assert_eq!(config["sim_timeout_secs"], Value::from(300.0));
        assert_eq!(config["ir_solve_timeout_secs"], Value::from(60.0));
        assert_eq!(config["model_attempt_timeout_secs"], Value::from(420.0));
        assert_eq!(config["all_omc_targets"], Value::from(true));
    }

    #[test]
    fn ci_profile_writes_the_shared_measurement_profile() {
        let config = config_for(&["verify-msl-parity", "--budget-profile", "ci"]);
        assert_eq!(config["budget_profile"], Value::from("ci"));
        assert_eq!(config.len(), 1);
    }

    #[test]
    fn named_profile_rejects_every_explicit_timeout_before_execution() {
        for option in [
            "--sim-timeout-secs",
            "--ir-solve-timeout-secs",
            "--model-attempt-timeout-secs",
        ] {
            let error = Harness::try_parse_from([
                "verify-msl-parity",
                "--budget-profile",
                "ci",
                option,
                "60",
            ])
            .expect_err("profile and explicit timeout must conflict");
            assert_eq!(error.kind(), clap::error::ErrorKind::ArgumentConflict);
        }
    }

    #[test]
    fn isolated_rerun_replaces_the_named_profile_with_its_wall_budget() {
        let mut parsed = Harness::parse_from(["verify-msl-parity", "--budget-profile", "ci"]);
        parsed.budgets.set_wall_budgets(300);
        let mut config = Map::new();
        parsed.budgets.insert_into(&mut config);
        assert_eq!(config["sim_timeout_secs"], Value::from(300.0));
        assert_eq!(config["model_attempt_timeout_secs"], Value::from(300.0));
        assert!(parsed.budgets.budget_profile.is_none());
    }
}
