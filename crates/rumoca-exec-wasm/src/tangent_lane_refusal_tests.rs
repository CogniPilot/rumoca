//! A tangent-lane block whose tensor aggregates are wider than two lanes is
//! refused with an error, never compiled with the dual-lane layout.

use rumoca_ir_solve::{
    BinaryOp, LinearOp, ScalarProgramBlock, TangentLaneProgram, TensorInputKind,
};

/// The squared elements of a dual vector load, widened to three tangent lanes.
pub(crate) fn tangent_lane_block() -> ScalarProgramBlock {
    let dual = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: Some(0),
            lanes: 2,
        },
        LinearOp::TensorBinary {
            dst_start: 4,
            op: BinaryOp::Mul,
            lhs_start: 0,
            rhs_start: 0,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 2,
        },
        LinearOp::StoreOutputRange {
            start: 5,
            count: 2,
            stride: 2,
        },
    ];
    let lanes = TangentLaneProgram::replicate(&dual, 3).expect("the program widens");
    let span = rumoca_ir_solve::source_span_from_offsets(1, 0, 1);
    let block = ScalarProgramBlock::with_tangent_lane_programs(&[lanes], vec![span])
        .expect("a checked tangent-lane block");
    assert_eq!(block.max_tensor_lanes(), 4);
    block
}

fn refusal(result: Result<impl Sized, crate::WasmCompileError>) -> String {
    match result {
        Ok(_) => panic!("a four-lane tensor block must not compile"),
        Err(error) => error.to_string(),
    }
}

#[test]
fn every_block_kind_with_wider_tensor_lanes_is_refused() {
    let block = tangent_lane_block();
    let layout = rumoca_ir_solve::VarLayout::from_parts(Default::default(), 2, 0);
    let messages = [
        refusal(crate::compile_residual_scalar_program_block_wasm(
            &block, &layout,
        )),
        refusal(crate::compile_jacobian_scalar_program_block_wasm(
            &block, &layout,
        )),
        refusal(crate::compile_expression_scalar_program_block_wasm(
            &block, &layout,
        )),
    ];
    for message in messages {
        assert!(message.contains("tensor lanes up to 2"), "{message}");
    }
}
