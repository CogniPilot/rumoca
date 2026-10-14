use super::*;

#[test]
fn portable_context_receipt_keeps_engine_and_declines_unadmitted_inputs() {
    use rumoca_solver::SimExecutionPolicy::{Auto, Interpreter};
    let accepted = execution_receipt(Auto, 1, 0);
    assert!(accepted.is_compiled());
    assert_eq!(
        accepted.engine,
        rumoca_solver::SimExecutionEngine::WasmProgram
    );
    assert!(!execution_receipt(Auto, 1, 1).is_compiled());
    assert!(!execution_receipt(Auto, 0, 0).is_compiled());
    assert!(!execution_receipt(Interpreter, 1, 0).is_compiled());
}

#[test]
fn portable_inputs_never_zero_pad_or_ignore_external_tables() {
    let layout = VarLayout::from_parts(Default::default(), 2, 1);
    assert!(validate_inputs(&layout, 2, 1, 0).is_ok());
    for (y, p, tables) in [(1, 1, 0), (3, 1, 0), (2, 0, 0), (2, 2, 0), (2, 1, 1)] {
        assert!(validate_inputs(&layout, y, p, tables).is_err());
    }
}
