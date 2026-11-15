use super::*;
use rumoca_ir_solve::{
    SolveAlgorithmBlock, SolveArithmeticProfile, SolveBlockMethodKind, SolveIntegerDomain,
    SolveRealFormat,
};

#[test]
fn typed_entries_refuse_before_owner_lookup_with_a_backend_error() {
    let table = SolvePureCallTable::default();
    for evaluate in [
        eval_pure_call,
        eval_pure_call_directional,
        eval_pure_call_checked,
        eval_pure_call_directional_checked,
    ] {
        let error = evaluate(&table, SolvePureCallOwnerId::LIFECYCLE_METHOD, &[])
            .expect_err("an excluded evaluator must not become an unknown-owner lookup");
        assert!(matches!(
            error,
            TypedProgramEvalError::InterpreterExcluded { .. }
        ));
        assert_eq!(error.source_span(), None);
        let error: crate::EvalSolveError = error.into();
        assert!(matches!(
            error,
            crate::EvalSolveError::CompiledExecution { .. }
        ));
    }
}

#[test]
fn excluded_lifecycle_methods_leave_caller_state_unchanged_before_binding() {
    let arithmetic =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let block = SolveAlgorithmBlock::construct(
        arithmetic,
        vec![],
        |_, _| Ok(vec![]),
        |_, _| Ok(vec![]),
        |_, _| Ok(vec![]),
    )
    .expect("empty lifecycle fixture");
    let mut state = vec![None];
    for method in [
        SolveBlockMethodKind::Startup,
        SolveBlockMethodKind::Recalibrate,
        SolveBlockMethodKind::DoStep,
    ] {
        let error = eval_algorithm_method(&block, method, &mut state)
            .expect_err("excluded method must refuse before state-shape binding");
        assert!(matches!(
            error,
            TypedProgramEvalError::InterpreterExcluded { .. }
        ));
        assert_eq!(state, vec![None]);
    }
}
