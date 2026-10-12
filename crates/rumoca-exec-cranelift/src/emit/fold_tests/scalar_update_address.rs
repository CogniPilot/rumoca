use super::nested_guard::{call_native_with_parameters, domain};
use super::*;

fn update_row() -> Vec<LinearOp> {
    let program = rumoca_ir_solve::FunctionFoldProgram::checked(
        domain(0, "patch"),
        2,
        2,
        vec![
            LinearOp::LoadFoldCapture { dst: 0, index: 0 },
            LinearOp::LoadFoldCapture { dst: 1, index: 1 },
            LinearOp::Const {
                dst: 2,
                value: 99.0,
            },
            LinearOp::StoreOutputFoldTensorUpdate {
                source_base: 0,
                source_stride: 1,
                dimensions: Box::new([2]),
                updates: Box::new([rumoca_ir_solve::FoldTensorUpdate {
                    subscripts: Box::new([rumoca_ir_solve::TensorUpdateSubscript::Index(
                        rumoca_ir_solve::TensorIndex::Runtime(0),
                    )]),
                    condition: Some(1),
                    value_start: 2,
                    value_stride: 1,
                }]),
                nodes: Box::new([rumoca_ir_solve::FoldTensorNode::Update { base: 0, update: 0 }]),
                result: 1,
                lanes: 1,
            },
        ],
    )
    .expect("checked scalar patch retains a live coordinate and activation condition");
    vec![
        LinearOp::LoadP { dst: 0, index: 1 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::Const {
            dst: 2,
            value: 10.0,
        },
        LinearOp::Const {
            dst: 3,
            value: 20.0,
        },
        LinearOp::FunctionFold {
            dst_start: 4,
            initial_start: 2,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::StoreOutput { src: 5 },
    ]
}

#[test]
fn scalar_fold_patch_matching_keeps_invalid_coordinates_safe_and_unchanged() {
    let row = update_row();
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 0, 1);
    let block = ScalarProgramBlock::with_output_indices(vec![row.clone()], vec![span], vec![0, 1])
        .expect("checked two-output block retains the compact scalar patch");
    let native = compile_residual_rows(std::slice::from_ref(&row)).unwrap();
    let mut calls = 0;
    for (index, selected) in [
        (1.0, [99.0, 20.0]),
        (2.0, [10.0, 99.0]),
        (0.0, [10.0, 20.0]),
        (3.0, [10.0, 20.0]),
        (1.5, [10.0, 20.0]),
        (f64::INFINITY, [10.0, 20.0]),
        (f64::NEG_INFINITY, [10.0, 20.0]),
        (f64::NAN, [10.0, 20.0]),
    ] {
        for condition in [0.0, 1.0] {
            let parameters = [condition, index];
            let parameter_bits = parameters.map(f64::to_bits);
            let expected = if condition == 0.0 {
                [10.0, 20.0]
            } else {
                selected
            };
            let mut reference = [-123.0; 2];
            rumoca_eval_solve::eval_scalar_program_block(
                &block,
                &[],
                &parameters,
                0.0,
                None,
                &mut reference,
            )
            .unwrap();
            assert_eq!(reference, expected, "canonical patch: {condition}, {index}");
            let mut output = [-123.0; 2];
            call_native_with_parameters(&native, &parameters, &mut output).unwrap();
            assert_eq!(output, expected, "native patch: {condition}, {index}");
            assert_eq!(parameters.map(f64::to_bits), parameter_bits);
            calls += 1;
        }
    }
    assert_eq!(native.jit_call_count(), calls);
}
