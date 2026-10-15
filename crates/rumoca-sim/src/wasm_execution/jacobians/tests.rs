mod indexed;
mod programs;
mod typed_calls;
use super::*;
use rumoca_ir_solve::{BinaryOp, LinearOp};
use rumoca_solver::CompiledSolveJacobianExpression;

#[test]
fn directional_kernel_executes_emitted_wasm_and_rejects_missing_seed() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("directional.mo"),
        1,
        2,
    );
    let source = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::LoadSeed { dst: 0, index: 0 },
            LinearOp::Const {
                dst: 1,
                value: -2.0,
            },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        vec![span],
    )
    .unwrap();
    let layout = VarLayout::from_parts(Default::default(), 1, 0);
    let code = WasmJacobian::compile(&source, &layout, &SolvePureCallTable::default()).unwrap();
    assert!(code.validate(&[1.5], &[], &[], &[]).is_err());
    assert!(code.validate(&[1.5], &[], &[3.0], &[]).is_ok());
    let mut out = [19.0];
    code.call(&[1.5], &[], 0.0, &[3.0], &[], &mut out).unwrap();
    assert_eq!(out[0], -6.0);
    let before = out.map(f64::to_bits);
    assert!(code.call(&[1.5], &[], 0.0, &[], &[], &mut out).is_err());
    assert_eq!(out.map(f64::to_bits), before);
}
