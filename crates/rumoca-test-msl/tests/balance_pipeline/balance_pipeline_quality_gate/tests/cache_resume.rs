use super::*;

#[test]
fn simulation_parity_cache_can_resume_rejects_all_error_checkpoint() {
    let temp = tempdir().expect("tempdir");
    let path = temp.path().join("omc_simulation_reference.json");
    fs::write(
        &path,
        serde_json::to_vec_pretty(&json!({
            "msl_version": "4.1.0",
            "omc_version": "OpenModelica 1.26.1",
            "stop_time": 10.0,
            "use_experiment_stop_time": true,
            "timing": {
                "batch_timeout_seconds": 600,
                "workers_used": 2,
                "omc_threads": 1
            },
            "models": {
                "A": {
                    "status": "error",
                    "error": "omc session spawn failed: port file did not appear"
                }
            }
        }))
        .expect("serialize cache payload"),
    )
    .expect("write cache payload");

    assert!(
        !simulation_parity_cache_can_resume(
            &path,
            &["A".to_string(), "B".to_string()],
            "4.1.0",
            "OpenModelica 1.26.1",
            SimulationParityCachePolicy {
                batch_timeout_seconds: 600,
                workers: 2,
                omc_threads: 1,
                use_experiment_stop_time: true,
                stop_time_override: None,
            },
        )
        .expect("all-error checkpoint should parse"),
        "a checkpoint without any reusable successful OMC result must not be resumed"
    );
}
