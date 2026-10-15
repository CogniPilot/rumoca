use super::*;
use crate::native_execution::CraneliftJacobianExpression;
use rumoca_solver::CompiledSolveJacobianExpression;

#[test]
fn selected_native_jvp_retains_its_source_seed_contract_and_lazy_original_fault() {
    let first = vec![
        solve::LinearOp::LoadSeed { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
        solve::LinearOp::Const {
            dst: 1,
            value: -0.0,
        },
        solve::LinearOp::StoreOutput { src: 1 },
    ];
    let second = vec![
        solve::LinearOp::Const { dst: 0, value: 7.0 },
        solve::LinearOp::LoadY { dst: 1, index: 0 },
        solve::LinearOp::LoadIndexedRegister {
            dst: 2,
            base: 0,
            stride: 1,
            dimensions: vec![1].into(),
            indices: vec![solve::TensorIndex::Runtime(1)].into(),
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ];
    let source =
        solve::ScalarProgramBlock::with_program_spans(vec![first, second], vec![span(); 2])
            .unwrap();
    let code = CraneliftJacobianExpression(
        rumoca_exec_cranelift::compile_jacobian_scalar_program_block(&source).unwrap(),
    );
    let foreign: solve::ScalarProgramBlock =
        serde_json::from_value(serde_json::to_value(&source).unwrap()).unwrap();
    assert!(code.bind_program(&foreign, 0).is_err());
    let first = code.bind_program(&source, 0).unwrap().unwrap();
    let second = code.bind_program(&source, 1).unwrap().unwrap();
    drop(code);
    drop(source);
    let mut tuple = [19.0; 2];
    for seed in [3.0_f64, -0.0] {
        first
            .call(&[2.0], &[], 0.0, &[seed], &[], &mut tuple)
            .unwrap();
        assert_eq!(
            tuple.map(f64::to_bits),
            [seed.to_bits(), (-0.0_f64).to_bits()]
        );
    }
    let before = tuple.map(f64::to_bits);
    assert!(first.call(&[2.0], &[], 0.0, &[], &[], &mut tuple).is_err());
    assert_eq!(tuple.map(f64::to_bits), before);
    let mut scalar = [19.0];
    let fault = second
        .call(&[2.0], &[], 0.0, &[1.0], &[], &mut scalar)
        .unwrap_err();
    assert_eq!(fault.source_span(), Some(span()));
    assert_eq!(scalar, [19.0]);
    second
        .call(&[1.0], &[], 0.0, &[1.0], &[], &mut scalar)
        .unwrap();
    assert_eq!(scalar, [7.0]);
}
