use super::*;
use rumoca_ir_solve::TensorIndex;

#[test]
fn dynamic_index_does_not_reuse_an_overwritten_constant() {
    let source = ScalarProgramBlock::with_output_indices(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const {
                dst: 1,
                value: 10.0,
            },
            LinearOp::Const {
                dst: 2,
                value: 20.0,
            },
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::LoadIndexedRegister {
                dst: 3,
                base: 1,
                stride: 1,
                dimensions: vec![2].into_boxed_slice(),
                indices: vec![TensorIndex::Runtime(0)].into_boxed_slice(),
            },
            LinearOp::StoreOutput { src: 3 },
        ]],
        vec![fixture_span()],
        vec![0],
    )
    .unwrap();
    let compiled = compile_expression_scalar_program_block(&source).unwrap();
    for (index, expected) in [(1.0, 10.0), (2.0, 20.0)] {
        let mut out = [0.0];
        compiled.call(&[], &[index], 0.0, &mut out).unwrap();
        assert_eq!(out, [expected]);
    }
}

#[test]
fn tensor_writes_invalidate_constants_across_the_whole_destination() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Const {
            dst: 2,
            value: 10.0,
        },
        LinearOp::Const {
            dst: 3,
            value: 20.0,
        },
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::P,
            input_start: 0,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::LoadIndexedRegister {
            dst: 4,
            base: 2,
            stride: 1,
            dimensions: vec![2].into_boxed_slice(),
            indices: vec![TensorIndex::Runtime(0)].into_boxed_slice(),
        },
        LinearOp::LoadIndexedRegister {
            dst: 5,
            base: 2,
            stride: 1,
            dimensions: vec![2].into_boxed_slice(),
            indices: vec![TensorIndex::Runtime(1)].into_boxed_slice(),
        },
        LinearOp::StoreOutputRange {
            start: 4,
            count: 2,
            stride: 1,
        },
    ];
    let source =
        ScalarProgramBlock::with_output_indices(vec![row], vec![fixture_span()], vec![0, 1])
            .unwrap();
    let compiled = compile_expression_scalar_program_block(&source).unwrap();
    for (indices, expected) in [([1.0, 2.0], [10.0, 20.0]), ([2.0, 1.0], [20.0, 10.0])] {
        let mut out = [0.0; 2];
        compiled.call(&[], &indices, 0.0, &mut out).unwrap();
        assert_eq!(out, expected);
    }
}

#[test]
fn in_place_arithmetic_uses_input_versions_before_replacing_constant_facts() {
    let source = ScalarProgramBlock::with_output_indices(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const {
                dst: 1,
                value: 10.0,
            },
            LinearOp::Const {
                dst: 2,
                value: 20.0,
            },
            LinearOp::Binary {
                dst: 0,
                op: rumoca_ir_solve::BinaryOp::Add,
                lhs: 0,
                rhs: 0,
            },
            LinearOp::LoadIndexedRegister {
                dst: 3,
                base: 1,
                stride: 1,
                dimensions: vec![2].into_boxed_slice(),
                indices: vec![TensorIndex::Runtime(0)].into_boxed_slice(),
            },
            LinearOp::StoreOutput { src: 3 },
        ]],
        vec![fixture_span()],
        vec![0],
    )
    .unwrap();
    let compiled = compile_expression_scalar_program_block(&source).unwrap();
    let mut out = [0.0];
    compiled.call(&[], &[], 0.0, &mut out).unwrap();
    assert_eq!(out, [20.0]);
}
