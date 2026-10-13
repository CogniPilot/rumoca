//! A precompiled backend admits expression and Jacobian hooks only; every
//! other construction request declines to the interpreter.

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
fn every_issued_assignment_schedule_is_interpreted() {
    let model = warm_start_test_model();
    let runtime = SolveRuntime::new_fixture_with_backend(&model, precompiled(&model))
        .expect("declined schedules construct their interpreter copy");
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
    let runtime = SolveRuntime::new_with_execution_backend(&model, Some(precompiled(&model)))
        .expect("a declined transaction constructs");
    assert!(
        runtime
            .scalar_execution()
            .unwrap()
            .scalar
            .compiled_event_transactions[0]
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
    let runtime = SolveRuntime::new_fixture_with_backend(&model, precompiled(&model))
        .expect("the precompiled backend constructs");
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
