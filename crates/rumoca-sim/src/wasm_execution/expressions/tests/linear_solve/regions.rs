use super::*;
use rumoca_ir_solve as solve;

fn region(nested: bool) -> std::sync::Arc<solve::FunctionConditionalProgram> {
    let mut result = vec![LinearOp::LoadFunctionConditionalCaptureRange {
        dst_start: 0,
        index_start: 0,
        count: 7,
    }];
    result.push(if nested {
        LinearOp::FunctionConditional {
            dst_start: 7,
            capture_start: 0,
            program: region(false),
        }
    } else {
        LinearOp::LinearSolveComponent {
            dst: 7,
            matrix_start: 1,
            rhs_start: 5,
            n: 2,
            component: 0,
        }
    });
    result.push(LinearOp::StoreOutput { src: 7 });
    std::sync::Arc::new(
        solve::FunctionConditionalProgram::checked(
            7,
            vec![1],
            [(
                vec![
                    LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
                result,
            )],
            vec![
                LinearOp::Const {
                    dst: 0,
                    value: -77.0,
                },
                LinearOp::StoreOutput { src: 0 },
            ],
        )
        .unwrap(),
    )
}

#[test]
fn nested_selected_dense_regions_share_bounded_workspace_without_frame_aliasing() {
    let mut operations = vec![
        LinearOp::Const {
            dst: 8,
            value: -0.0,
        },
        LinearOp::StoreOutput { src: 8 },
    ];
    operations.extend((0..7).map(|index| LinearOp::LoadY {
        dst: index as u32,
        index,
    }));
    operations.extend([
        LinearOp::FunctionConditional {
            dst_start: 7,
            capture_start: 0,
            program: region(true),
        },
        LinearOp::StoreOutput { src: 7 },
    ]);
    let source = ScalarProgramBlock::with_program_spans(vec![operations], vec![span()]).unwrap();
    let code = WasmExpression::compile(
        &source,
        &VarLayout::from_parts(Default::default(), 7, 0),
        &Default::default(),
    )
    .unwrap();
    let mut output = [19.0; 2];
    for y in [
        [1.0, 3.0, 1.7, -3.0, 2.3, 0.7, 9.1],
        [0.0, f64::NAN, 1.7, -3.0, 2.3, 0.7, 9.1],
        [1.0, 0.0, 3.0, 7.0, 2.0, 5.0, 11.0],
    ] {
        code.call(&y, &[], 0.0, &[], &mut output).unwrap();
        assert_eq!(bits(&output), bits(&reference(&source, &y, None)));
    }
    let before = bits(&output);
    let y = [1.0, f64::NAN, 1.7, -3.0, 2.3, 0.7, 9.1];
    let error = code.call(&y, &[], 0.0, &[], &mut output).unwrap_err();
    assert_eq!(error.source_span(), Some(span()));
    assert!(error.to_string().contains("singular matrix"));
    assert_eq!(bits(&output), before);
}
