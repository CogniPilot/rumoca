//! Wall-budget knobs for `cargo xtask verify msl-parity`.
//!
//! These exist for the long-budget diagnostic lanes (the nightly event cohort),
//! where models that legitimately need minutes rather than seconds are being
//! investigated. Every budget is *raise-only* on the harness side: the committed
//! quality baseline was measured with the default budgets, so shortening them
//! would silently make the gate easier by turning real failures into timeouts.
//!
//! Budgets are whole seconds — sub-second precision is meaningless for a wall
//! budget and `f64` would block the `Eq` derive the CLI arg structs rely on.

use clap::Args;
use serde_json::{Map, Value};

#[derive(Debug, Args, Clone, PartialEq, Eq, Default)]
pub(crate) struct MslParityBudgetArgs {
    /// Per-model solver wall budget in seconds (raise-only; clamped to at least
    /// the harness default)
    #[arg(long, value_name = "SECS")]
    sim_timeout_secs: Option<u64>,
    /// Per-model Solve-IR lowering wall budget in seconds (raise-only)
    #[arg(long, value_name = "SECS")]
    ir_solve_timeout_secs: Option<u64>,
    /// Per-model, per-phase wall budget in seconds (raise-only)
    #[arg(long, value_name = "SECS")]
    model_attempt_timeout_secs: Option<u64>,
    /// Build the OMC reference for every simulation target rather than only the
    /// models rumoca already simulates. Needed by diagnostic lanes that compare
    /// models which are not yet `sim_ok`.
    #[arg(long)]
    all_omc_targets: bool,
}

impl MslParityBudgetArgs {
    /// Write the explicitly set budgets into the harness config map.
    pub(crate) fn insert_into(&self, config: &mut Map<String, Value>) {
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
}
