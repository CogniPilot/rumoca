//! Missing required hooks fail preparation; explicit reference execution stays available.

use super::*;
use crate::PrecompiledSolveBackendBuilder;

fn precompiled(model: &solve::SolveModel) -> Rc<dyn SolveExecutionBackend> {
    Rc::new(
        PrecompiledSolveBackendBuilder::new(model)
            .expect("the fixture context is admitted")
            .finish(),
    )
}

#[test]
fn incomplete_compiled_hooks_fail_runtime_construction() {
    let model = warm_start_test_model();
    assert!(
        SolveRuntime::new_fixture_with_backend(&model, precompiled(&model)).is_err(),
        "missing compiled hooks must not construct an executable runtime"
    );
}

#[test]
fn every_issued_assignment_schedule_is_interpreted() {
    let model = warm_start_test_model();
    let runtime =
        SolveRuntime::new_fixture(&model).expect("explicit reference schedules construct");
    let issued = runtime
        .model
        .problem
        .continuous
        .refresh_owners
        .exact_assignment_schedules()
        .len();
    assert!(
        issued > 0,
        "the fixture issues an exact assignment schedule"
    );
    assert_eq!(runtime.interpreted_schedule_count(), issued);
}

#[test]
fn event_transactions_keep_their_atomic_interpreted_execution() {
    let mut model = super::super::event_transactions::tests::transaction_model();
    model.problem.continuous.refresh_owners =
        solve_eval::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
            .expect("the transaction fixture owns its refresh plan");
    let runtime = SolveRuntime::new(&model).expect("an explicit reference transaction constructs");
    assert!(
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .compiled_event_transaction(0)
            .unwrap()
            .is_none()
    );
    let mut p = [4.5, 1.0, 9.0, 9.0];
    let committed = runtime
        .execute_event_transaction(0, &mut [], &mut p, 0.0)
        .unwrap();
    assert!(committed.changed);
    assert_eq!(p, [4.5, 1.0, 4.5, 9.0]);
}

#[test]
fn a_torn_block_without_a_compiled_form_still_prepares_its_interpreted_sweep() {
    let model = warm_start_test_model();
    let runtime =
        SolveRuntime::new_fixture(&model).expect("the explicit reference runtime constructs");
    let tearing = solve::BlockTearing {
        tear_y_indices: Vec::new(),
        residual_rows: Vec::new(),
        causal_steps: vec![solve::CausalStep {
            row: 1,
            y_index: 1,
            ..Default::default()
        }],
    };
    assert!(
        runtime
            .scalar_execution()
            .unwrap()
            .prepared_torn_sweep(&tearing)
            .is_some(),
        "the certified isolator batches through the interpreter"
    );
}
