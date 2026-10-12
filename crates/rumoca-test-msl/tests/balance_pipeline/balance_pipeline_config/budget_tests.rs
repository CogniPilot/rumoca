use super::*;
use serde_json::json;

fn config(value: serde_json::Value) -> MslParityConfig {
    MslParityConfig::from_json(&value.to_string()).expect("valid parity config wire")
}

#[test]
fn bare_and_empty_full_config_select_the_ci_envelope() {
    let baseline: serde_json::Value =
        serde_json::from_str(include_str!("../../msl_tests/msl_quality_baseline.json"))
            .expect("checked measurement envelope");
    for config in [
        MslParityConfig::default(),
        config(json!({})),
        config(json!({"target_scope": "root-examples", "sim_set": "full", "sim_match": []})),
        config(json!({"shard_index": 0, "shard_count": 4})),
    ] {
        assert!(!config.is_partial_run());
        assert_eq!(config.wall_budgets(), (20.0, 45.0));
        assert_eq!(
            baseline["model_attempt_timeout_seconds"],
            config.wall_budgets().0
        );
        assert_eq!(baseline["sim_timeout_seconds"], config.wall_budgets().1);
        assert_eq!(config.simulation_timeout_override(), Some(45.0));
        config
            .validate_budget_options()
            .expect("unambiguous defaults");
    }
}

#[test]
fn every_partial_selection_retains_the_focused_defaults() {
    for value in [
        json!({"target_scope": "default-simulation-targets"}),
        json!({"sim_targets_file": "original-targets.json"}),
        json!({"sim_match": ["original"]}),
        json!({"sim_limit": 20}),
        json!({"sim_set": "short"}),
        json!({"sim_set": "long"}),
        json!({"shard_index": 1, "shard_count": 4}),
    ] {
        let config = config(value);
        assert!(config.is_partial_run());
        assert_eq!(config.wall_budgets(), (10.0, 12.0));
        assert_eq!(config.simulation_timeout_override(), None);
    }
}

#[test]
fn explicit_ci_profile_and_full_merge_use_one_envelope() {
    for value in [
        json!({"budget_profile": "ci", "sim_match": ["original"]}),
        json!({"budget_profile": "ci", "shard_index": 1, "shard_count": 4}),
        json!({"merge_shards_dir": "original-shards"}),
        json!({"budget_profile": "ci", "merge_shards_dir": "original-shards"}),
    ] {
        let config = config(value);
        config
            .validate_budget_options()
            .expect("unambiguous profile");
        assert_eq!(config.wall_budgets(), (20.0, 45.0));
        assert_eq!(config.simulation_timeout_override(), Some(45.0));
    }
}

#[test]
fn direct_wire_rejects_profile_and_each_explicit_timeout() {
    for field in [
        "sim_timeout_secs",
        "model_attempt_timeout_secs",
        "ir_solve_timeout_secs",
    ] {
        let mut value = json!({"budget_profile": "ci"});
        value[field] = json!(60.0);
        assert_eq!(
            MslParityConfig::from_json(&value.to_string()).expect_err("ambiguous direct config"),
            "budget_profile conflicts with explicit timeout fields"
        );
    }
    assert!(MslParityConfig::from_json(r#"{"budget_profile":"unknown"}"#).is_err());
}

#[test]
fn explicit_diagnostics_keep_the_focused_raise_only_floors() {
    for selection in [json!({}), json!({"sim_match": ["original"]})] {
        let mut config = config(selection);
        config.model_attempt_timeout_secs = Some(300.0);
        config.sim_timeout_secs = Some(420.0);
        assert_eq!(config.wall_budgets(), (300.0, 420.0));
        config.model_attempt_timeout_secs = Some(1.0);
        config.sim_timeout_secs = Some(2.0);
        assert_eq!(config.wall_budgets(), (10.0, 12.0));
    }
}
