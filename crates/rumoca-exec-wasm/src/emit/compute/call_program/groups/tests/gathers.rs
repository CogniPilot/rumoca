//! Model gather status, lazy activation and source ownership through outlining.
use super::*;
use solve::TensorIndex;

fn conditional() -> LinearOp {
    let region = solve::FunctionConditionalProgram::checked(
        1,
        vec![1],
        [(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 0. },
                LinearOp::Compare {
                    dst: 2,
                    op: solve::CompareOp::Eq,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 7. },
                LinearOp::StoreOutput { src: 0 },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 42. },
            LinearOp::LoadFunctionConditionalCapture { dst: 1, index: 0 },
            LinearOp::LoadIndexedRegister {
                dst: 2,
                base: 0,
                stride: 1,
                dimensions: vec![1].into_boxed_slice(),
                indices: vec![TensorIndex::Runtime(1)].into_boxed_slice(),
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .unwrap();
    LinearOp::FunctionConditional {
        dst_start: 2,
        capture_start: 1,
        program: std::sync::Arc::new(region),
    }
}

fn fixture(
    guarded: bool,
    late: bool,
) -> (
    solve::NativeRefreshAssignmentSchedule,
    VarLayout,
    solve::SolvePureCallTable,
) {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let table = solve::SolvePureCallTable::builder(arithmetic).finish();
    let rows = (0..COUNT)
        .map(|target| {
            let mut operations = vec![
                LinearOp::LoadY {
                    dst: 0,
                    index: target,
                },
                if late && target + 1 != COUNT {
                    LinearOp::Const { dst: 1, value: 1. }
                } else {
                    LinearOp::LoadP { dst: 1, index: 0 }
                },
            ];
            let result = if guarded {
                operations.push(conditional());
                2
            } else {
                operations.extend([
                    LinearOp::Const { dst: 2, value: 42. },
                    LinearOp::Const {
                        dst: 3,
                        value: -999.,
                    },
                    LinearOp::Const { dst: 4, value: 64. },
                    LinearOp::LoadIndexedRegister {
                        dst: 5,
                        base: 2,
                        stride: 2,
                        dimensions: vec![1, 2].into_boxed_slice(),
                        indices: vec![TensorIndex::Constant(0), TensorIndex::Runtime(1)]
                            .into_boxed_slice(),
                    },
                ]);
                5
            };
            operations.extend([
                LinearOp::Binary {
                    dst: 6,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: result,
                },
                LinearOp::StoreOutput { src: 6 },
            ]);
            operations
        })
        .collect();
    let source = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_program_spans(
            rows,
            (0..COUNT).map(|i| span(50 + i)).collect(),
        )
        .unwrap(),
    );
    let layout = VarLayout::from_parts(Default::default(), COUNT, 1);
    let targets = (0..COUNT)
        .map(|index| Some(solve::scalar_slot_y(index)))
        .collect::<Vec<_>>();
    let owners =
        solve::NativeRefreshAssignmentSchedule::from_continuous_block(&source, &targets, &layout)
            .unwrap();
    (owners, layout, table)
}

#[test]
fn checked_gather_only_programs_keep_integer_bounds_status_transaction_and_recovery() {
    let (schedule, layout, table) = fixture(false, false);
    let whole = emit_with_budget(&schedule, &layout, &table, usize::MAX).unwrap();
    let groups = emit_with_budget(&schedule, &layout, &table, BUDGET).unwrap();
    assert_eq!(whole.gather_faults(), groups.gather_faults());
    assert!(groups.faults().is_empty());
    assert_eq!(groups.gather_faults()[0].provenance, span(50));
    let mut a = Runner::new(&whole, &layout);
    let mut b = Runner::new(&groups, &layout);
    for index in [
        1.,
        2.,
        0.,
        -1.,
        3.,
        1.5,
        f64::NAN,
        f64::INFINITY,
        f64::NEG_INFINITY,
        9223372036854775808.,
        -9223372036854775808.,
        1.,
    ] {
        let actual = b.run(index);
        assert_eq!(actual, a.run(index));
        if index == 1. || index == 2. {
            assert_eq!(actual.0, 0);
            let value = if index == 1. { 42f64 } else { 64f64 };
            assert_eq!(actual.1, vec![value.to_bits(); COUNT]);
        } else {
            let fault = groups
                .gather_faults()
                .iter()
                .find(|fault| fault.status == actual.0 as u32)
                .unwrap();
            let conversion =
                !index.is_finite() || index.fract() != 0. || index >= 9223372036854775808.;
            assert_eq!(
                fault.kind,
                if conversion {
                    crate::TypedCallFaultKind::IntegerConversion
                } else {
                    crate::TypedCallFaultKind::IndexBounds
                }
            );
        }
    }
}

#[test]
fn checked_region_gather_faults_execute_only_in_selected_region() {
    let (schedule, layout, table) = fixture(true, false);
    let artifact = emit_with_budget(&schedule, &layout, &table, BUDGET).unwrap();
    assert_eq!(artifact.gather_faults()[0].region_path, vec![(2, 2)]);
    let mut runner = Runner::new(&artifact, &layout);
    assert_eq!(runner.run(0.).1, vec![7f64.to_bits(); COUNT]);
    assert_eq!(runner.run(1.).1, vec![42f64.to_bits(); COUNT]);
    for index in [2., 1.5, f64::NAN, f64::INFINITY] {
        assert_ne!(runner.run(index).0, 0);
    }
    assert_eq!(runner.run(0.).0, 0);
}

#[test]
fn late_outlined_gather_fault_keeps_complete_y_and_absolute_source_status() {
    let (schedule, layout, table) = fixture(false, true);
    let whole = emit_with_budget(&schedule, &layout, &table, usize::MAX).unwrap();
    let groups = emit_with_budget(&schedule, &layout, &table, BUDGET).unwrap();
    assert!(body_sizes(groups.module_bytes()).len() > body_sizes(whole.module_bytes()).len());
    assert_eq!(whole.gather_faults(), groups.gather_faults());
    let mut a = Runner::new(&whole, &layout);
    let mut b = Runner::new(&groups, &layout);
    for index in [3., f64::NAN, f64::INFINITY] {
        let actual = b.run(index);
        assert_eq!(actual, a.run(index));
        let fault = groups
            .gather_faults()
            .iter()
            .find(|fault| fault.status == actual.0 as u32)
            .unwrap();
        assert_eq!(fault.kernel, COUNT - 1);
        assert_eq!(fault.provenance, span(50 + COUNT - 1));
        assert_eq!(
            fault.kind,
            if index == 3. {
                crate::TypedCallFaultKind::IndexBounds
            } else {
                crate::TypedCallFaultKind::IntegerConversion
            }
        );
        // Runner proves P byte preservation and unchanged complete sentinel Y.
    }
    assert_eq!(b.run(1.).0, 0);
}
