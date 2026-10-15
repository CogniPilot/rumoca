use super::*;
use rumoca_ir_solve::{FunctionConditionalProgram, TensorIndex, TensorInputKind};
use rumoca_solver::CompiledSolveExpression;
use std::sync::Arc;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("conditional_ranges.mo"), 7, 29)
}

fn region(
    count: usize,
    stride: usize,
    nested: bool,
    fault: bool,
) -> Arc<FunctionConditionalProgram> {
    let captures = 2 * count + 2;
    let mut result = vec![LinearOp::LoadFunctionConditionalCaptureRange {
        dst_start: 0,
        index_start: 0,
        count: captures,
    }];
    if nested {
        result.extend([
            LinearOp::FunctionConditional {
                dst_start: captures as u32,
                capture_start: 0,
                program: region(count, stride, false, fault),
            },
            LinearOp::StoreOutputRange {
                start: captures as u32,
                count: count + 2,
                stride: 1,
            },
        ]);
    } else {
        result.extend([
            LinearOp::StoreOutput { src: 1 },
            LinearOp::StoreOutputRange {
                start: 1,
                count,
                stride,
            },
        ]);
        if fault {
            result.push(LinearOp::LoadIndexedRegister {
                dst: captures as u32,
                base: 1,
                stride: 1,
                dimensions: vec![1].into(),
                indices: vec![TensorIndex::Runtime((captures - 1) as u32)].into(),
            });
        }
        result.push(LinearOp::StoreOutput { src: 1 });
    }
    let fallback = vec![
        LinearOp::Const {
            dst: 0,
            value: -0.0,
        },
        LinearOp::TensorFill {
            dst_start: 1,
            value_start: 0,
            count: count + 2,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 1,
            count: count + 2,
            stride: 1,
        },
    ];
    Arc::new(
        FunctionConditionalProgram::checked(
            captures,
            vec![count + 2],
            [(
                vec![
                    LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                    LinearOp::StoreOutputRange {
                        start: 0,
                        count: 1,
                        stride: 1,
                    },
                ],
                result,
            )],
            fallback,
        )
        .unwrap(),
    )
}

fn source(count: usize, stride: usize, nested: bool, fault: bool) -> ScalarProgramBlock {
    let captures = 2 * count + 2;
    let sentinel = (captures + count + 2) as u32;
    ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const {
                dst: sentinel,
                value: -0.0,
            },
            LinearOp::StoreOutput { src: sentinel },
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: captures,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::FunctionConditional {
                dst_start: captures as u32,
                capture_start: 0,
                program: region(count, stride, nested, fault),
            },
            LinearOp::StoreOutputRange {
                start: captures as u32,
                count: count + 2,
                stride: 1,
            },
        ]],
        vec![span()],
    )
    .unwrap()
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

#[test]
fn conditional_ranges_keep_exact_order_ieee_values_and_compact_nested_code() {
    for stride in [1, 2] {
        let mut sizes = Vec::new();
        for count in [1, 4096] {
            let source = source(count, stride, true, false);
            let mut y = (0..2 * count + 2).map(|n| n as f64).collect::<Vec<_>>();
            y[1] = -0.0;
            if let Some(value) = y.get_mut(2) {
                *value = f64::from_bits(0x7ff8_4321_abcd_1234);
            }
            let code = WasmExpression::compile(
                &source,
                &VarLayout::from_parts(Default::default(), y.len(), 0),
                &Default::default(),
            )
            .unwrap();
            sizes.push(code.selected[0].0.module_bytes().len());
            for guard in [1.0, 0.0, 2.0] {
                y[0] = guard;
                let mut expected = vec![0.0; count + 3];
                rumoca_eval_solve::eval_scalar_program_block_with_context(
                    &source,
                    &y,
                    &[],
                    0.0,
                    Default::default(),
                    &mut expected,
                )
                .unwrap();
                let mut actual = vec![17.0; count + 3];
                code.call(&y, &[], 0.0, &[], &mut actual).unwrap();
                assert_eq!(
                    bits(&actual),
                    bits(&expected),
                    "count={count}, stride={stride}"
                );
            }
        }
        assert!(
            sizes[1].abs_diff(sizes[0]) < 512,
            "range code expanded: {sizes:?}"
        );
    }
}

#[test]
fn singleton_range_does_not_encode_its_unused_stride() {
    let source = source(1, usize::MAX, true, false);
    let y = [1.0, -0.0, 2.0, 3.0];
    let code = WasmExpression::compile(
        &source,
        &VarLayout::from_parts(Default::default(), y.len(), 0),
        &Default::default(),
    )
    .unwrap();
    let mut actual = [17.0; 4];
    code.call(&y, &[], 0.0, &[], &mut actual).unwrap();
    assert_eq!(bits(&actual), bits(&[-0.0; 4]));
}

#[test]
fn range_publication_before_a_nested_fault_stays_private_and_recovers() {
    let source = source(3, 2, true, true);
    let layout = VarLayout::from_parts(Default::default(), 8, 0);
    let code = WasmExpression::compile(&source, &layout, &Default::default()).unwrap();
    let mut y = [1.0, -0.0, 2.0, 3.0, 4.0, 5.0, 6.0, 2.0];
    let mut actual = vec![17.0; 6];
    let error = code.call(&y, &[], 0.0, &[], &mut actual).unwrap_err();
    assert_eq!(error.source_span(), Some(span()));
    assert_eq!(bits(&actual), bits(&[17.0; 6]));
    y[0] = 0.0;
    code.call(&y, &[], 0.0, &[], &mut actual).unwrap();
    assert_eq!(bits(&actual), bits(&[-0.0; 6]));
    y[0] = 1.0;
    y[7] = 1.0;
    code.call(&y, &[], 0.0, &[], &mut actual).unwrap();
    assert_eq!(bits(&actual), bits(&[-0.0, -0.0, -0.0, 3.0, 5.0, -0.0]));
}
