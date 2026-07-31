use super::*;

// =============================================================================
// Per-model resource ceilings
// =============================================================================
//
// The ceilings themselves — the numbers, the raise-only clamp, the streaming
// measurement, and the written acceptance contract — live in
// `rumoca_test_msl::resource_budget`. This file is only the harness wiring:
// read the per-run overrides out of the parity config, and stamp a violation
// onto a model result as the typed `ResourceBudget` bucket.
//
// # What a violation does to a result
//
// It never rewrites `phase_reached`. A model that compiled *did* compile, and
// moving it out of the compile-success cohort to report a size overrun would
// corrupt the compile counts to make the performance report louder. Instead:
//
//   * every violation stamps the machine-readable classification
//     (`failure_bucket = ResourceBudget`, owner `Performance`, a stable
//     `EMSL_BUDGET_*` code) so the cohort is greppable and never lands in
//     `Unclassified`;
//   * a violation on an attempt that recorded a simulation downgrades
//     `sim_status` to `sim_solver_fail` — a model whose Solve IR does not fit
//     the budget is not a passing simulation;
//   * a violation on a compile-only attempt is recorded in `ir_solve_error`,
//     the field triage already reads for lowering-stage defects.
//
// A result that already carries a producer-minted `failure_bucket` is left
// alone: the worker's own typed classification is closer to the defect than a
// budget observation made after the fact.

use super::balance_pipeline_core::set_harness_failure_classification;
use rumoca_test_msl::resource_budget::{
    COMPILE_WALL_BUDGET_ERROR_CODE, MODEL_COMPILE_WALL_LIMIT_SECS_DEFAULT,
    SOLVE_IR_SIZE_BUDGET_ERROR_CODE, SOLVE_IR_SIZE_LIMIT_MB_DEFAULT, SolveIrSizeBudget,
    check_compile_wall_secs, raise_only_mb, raise_only_secs,
};

/// The per-model Solve-IR serialized-size ceiling for this run (raise-only).
pub(super) fn solve_ir_size_budget() -> SolveIrSizeBudget {
    SolveIrSizeBudget::from_mb(raise_only_mb(
        parity_config().solve_ir_size_limit_mb,
        SOLVE_IR_SIZE_LIMIT_MB_DEFAULT,
    ))
}

/// The per-model total compile wall ceiling for this run, in seconds
/// (raise-only).
pub(super) fn model_compile_wall_limit_secs() -> f64 {
    raise_only_secs(
        parity_config().model_compile_wall_limit_secs,
        MODEL_COMPILE_WALL_LIMIT_SECS_DEFAULT,
    )
}

/// A Solve-IR artifact's on-disk size, when the harness wrote one.
///
/// Relative paths are resolved against the results directory, matching how
/// every other `*_file` field in a result row is stored.
fn solve_ir_artifact_bytes(relative_or_absolute: &str) -> Option<u64> {
    let path = Path::new(relative_or_absolute);
    let path = if path.is_absolute() {
        path.to_path_buf()
    } else {
        msl_results_dir().join(path)
    };
    fs::metadata(path).ok().map(|metadata| metadata.len())
}

/// Stamp the typed `ResourceBudget` classification and the rendered message
/// onto a model result.
fn record_resource_budget_violation(
    result: &mut MslModelResult,
    error_code: &str,
    message: String,
) {
    set_harness_failure_classification(
        result,
        result
            .failure_phase
            .or(Some(rumoca_worker::WorkerProgressPhase::Solve)),
        rumoca_worker::ModelFailureBucket::ResourceBudget,
        error_code,
    );
    if result.sim_status.is_some() {
        result.sim_status = Some("sim_solver_fail".to_string());
        result.sim_error = Some(message);
        result.sim_error_code = Some(error_code.to_string());
    } else {
        result.ir_solve_error = Some(message);
        result.ir_solve_error_code = Some(error_code.to_string());
    }
}

/// Apply the per-model resource ceilings to one finished model result.
///
/// Called once per attempt on the way out of the worker lane, so the ceilings
/// are enforced for every model rather than only for the ones a debugging flag
/// happened to emit artifacts for.
pub(super) fn enforce_model_resource_budgets(result: &mut MslModelResult) {
    if result.failure_bucket.is_some() {
        // The producer already classified this attempt from typed knowledge.
        return;
    }
    if let Some(exceeded) =
        check_compile_wall_secs(result.compile_seconds, model_compile_wall_limit_secs())
    {
        record_resource_budget_violation(
            result,
            COMPILE_WALL_BUDGET_ERROR_CODE,
            format!("{} ({})", exceeded, result.model_name),
        );
        return;
    }
    let Some(solve_ir_file) = result.ir_solve_file.as_deref() else {
        return;
    };
    let Some(bytes) = solve_ir_artifact_bytes(solve_ir_file) else {
        return;
    };
    if let Err(exceeded) = solve_ir_size_budget().check_bytes(bytes) {
        record_resource_budget_violation(
            result,
            SOLVE_IR_SIZE_BUDGET_ERROR_CODE,
            format!("{} ({})", exceeded, result.model_name),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compiled_result(model_name: &str) -> MslModelResult {
        phase_error_result(model_name.to_string(), "Success", None, None)
    }

    #[test]
    fn a_model_inside_both_ceilings_is_left_untouched() {
        let mut result = compiled_result("Modelica.Fast");
        result.compile_seconds = Some(0.25);
        result.sim_status = Some("sim_ok".to_string());
        enforce_model_resource_budgets(&mut result);
        assert_eq!(result.failure_bucket, None);
        assert_eq!(result.sim_status.as_deref(), Some("sim_ok"));
        assert_eq!(result.phase_reached, "Success");
    }

    #[test]
    fn a_compile_wall_overrun_is_typed_attributed_and_downgrades_the_simulation() {
        let mut result = compiled_result("Modelica.Slow");
        result.compile_seconds = Some(model_compile_wall_limit_secs() + 1.0);
        result.sim_status = Some("sim_ok".to_string());
        enforce_model_resource_budgets(&mut result);
        assert_eq!(
            result.failure_bucket,
            Some(rumoca_worker::ModelFailureBucket::ResourceBudget)
        );
        assert_eq!(
            result.owner_category,
            Some(rumoca_worker::ModelFailureOwner::Performance)
        );
        assert_eq!(
            result.failure_error_code.as_deref(),
            Some(COMPILE_WALL_BUDGET_ERROR_CODE)
        );
        assert_eq!(result.sim_status.as_deref(), Some("sim_solver_fail"));
        assert!(
            result
                .sim_error
                .as_deref()
                .is_some_and(|error| error.contains("Modelica.Slow")),
            "the rendered failure must name the model: {:?}",
            result.sim_error
        );
        assert_eq!(
            result.phase_reached, "Success",
            "a budget overrun must not move a compiled model out of the compile-success cohort"
        );
    }

    #[test]
    fn a_compile_only_overrun_lands_in_the_lowering_error_field() {
        let mut result = compiled_result("Modelica.SlowCompileOnly");
        result.compile_seconds = Some(model_compile_wall_limit_secs() + 1.0);
        enforce_model_resource_budgets(&mut result);
        assert_eq!(
            result.failure_bucket,
            Some(rumoca_worker::ModelFailureBucket::ResourceBudget)
        );
        assert_eq!(result.sim_status, None);
        assert_eq!(
            result.ir_solve_error_code.as_deref(),
            Some(COMPILE_WALL_BUDGET_ERROR_CODE)
        );
    }

    #[test]
    fn an_oversized_solve_ir_artifact_is_rejected_by_its_on_disk_size() {
        let temp = tempfile::tempdir().expect("temp dir");
        let artifact = temp.path().join("Oversized.json");
        let oversized = solve_ir_size_budget().limit_bytes() + 1;
        let file = fs::File::create(&artifact).expect("create artifact");
        file.set_len(oversized).expect("grow artifact");
        drop(file);

        let mut result = compiled_result("Modelica.Oversized");
        result.compile_seconds = Some(0.5);
        result.sim_status = Some("sim_ok".to_string());
        result.ir_solve_file = Some(artifact.to_string_lossy().to_string());
        enforce_model_resource_budgets(&mut result);

        assert_eq!(
            result.failure_bucket,
            Some(rumoca_worker::ModelFailureBucket::ResourceBudget)
        );
        assert_eq!(
            result.failure_error_code.as_deref(),
            Some(SOLVE_IR_SIZE_BUDGET_ERROR_CODE)
        );
        assert_eq!(result.sim_status.as_deref(), Some("sim_solver_fail"));
    }

    #[test]
    fn a_solve_ir_artifact_inside_the_ceiling_is_accepted() {
        let temp = tempfile::tempdir().expect("temp dir");
        let artifact = temp.path().join("Small.json");
        fs::write(&artifact, b"{\"schema_version\":1}").expect("write artifact");

        let mut result = compiled_result("Modelica.Small");
        result.compile_seconds = Some(0.5);
        result.sim_status = Some("sim_ok".to_string());
        result.ir_solve_file = Some(artifact.to_string_lossy().to_string());
        enforce_model_resource_budgets(&mut result);

        assert_eq!(result.failure_bucket, None);
        assert_eq!(result.sim_status.as_deref(), Some("sim_ok"));
    }

    #[test]
    fn a_producer_minted_classification_is_never_overwritten() {
        let mut result = compiled_result("Modelica.AlreadyClassified");
        result.compile_seconds = Some(model_compile_wall_limit_secs() + 100.0);
        set_harness_failure_classification(
            &mut result,
            None,
            rumoca_worker::ModelFailureBucket::Timeout,
            "EMSL_TIMEOUT_MODEL_ATTEMPT",
        );
        enforce_model_resource_budgets(&mut result);
        assert_eq!(
            result.failure_bucket,
            Some(rumoca_worker::ModelFailureBucket::Timeout),
            "a budget observation must not relabel a typed producer failure"
        );
    }

    #[test]
    fn a_missing_artifact_or_measurement_is_accepted_rather_than_failed() {
        let mut result = compiled_result("Modelica.NoArtifact");
        result.compile_seconds = None;
        result.ir_solve_file = Some("ir_solve/does-not-exist.json".to_string());
        enforce_model_resource_budgets(&mut result);
        assert_eq!(
            result.failure_bucket, None,
            "an absent measurement is not a budget overrun"
        );
    }

    #[test]
    fn the_run_ceilings_are_the_committed_defaults_unless_raised() {
        assert_eq!(
            solve_ir_size_budget().limit_mb(),
            SOLVE_IR_SIZE_LIMIT_MB_DEFAULT
        );
        assert_eq!(
            model_compile_wall_limit_secs(),
            MODEL_COMPILE_WALL_LIMIT_SECS_DEFAULT
        );
        assert!(
            model_compile_wall_limit_secs() > MODEL_ATTEMPT_TIMEOUT_SECS,
            "the total compile wall ceiling must be looser than the per-phase budget, \
             or the per-phase watchdog would never get to fire"
        );
    }
}
