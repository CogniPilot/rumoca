//! Checked lazy regions execute through the actual native schedule API.

use super::*;
use std::sync::Arc;

fn call_table() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let ty = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut builder = solve::SolvePureCallTable::builder(p);
    let owner = builder
        .add_owner(
            identity(801),
            vec![ty.clone()],
            vec![solve::SolvePureCallOutput::result(ty)],
            span(800),
            |body, inputs, outputs| {
                let input = body.load(inputs[0], span(801))?;
                let integer = body.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    input,
                    span(802),
                )?;
                let value = body.convert(
                    solve::SolveConversionOperator::IntegerToReal,
                    integer,
                    span(803),
                )?;
                body.store(outputs[0], value, span(804))
            },
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    (builder.finish(), site)
}

fn capture(dst: u32, index: usize) -> solve::LinearOp {
    solve::LinearOp::LoadFunctionConditionalCapture { dst, index }
}

fn conditional(site: &solve::SolvePureCallSite, nested: bool) -> solve::LinearOp {
    let result = if nested {
        vec![
            solve::LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start: 0,
                index_start: 0,
                count: 2,
            },
            conditional(site, false),
            solve::LinearOp::StoreOutput { src: 2 },
        ]
    } else {
        vec![
            capture(0, 1),
            solve::LinearOp::PureCall {
                dst_start: 1,
                input_starts: vec![0].into(),
                site: site.clone(),
            },
            solve::LinearOp::StoreOutput { src: 1 },
        ]
    };
    solve::LinearOp::FunctionConditional {
        dst_start: 2,
        capture_start: 0,
        program: Arc::new(
            solve::FunctionConditionalProgram::checked(
                2,
                vec![1],
                [(
                    vec![capture(0, 0), solve::LinearOp::StoreOutput { src: 0 }],
                    result,
                )],
                vec![capture(0, 1), solve::LinearOp::StoreOutput { src: 0 }],
            )
            .unwrap(),
        ),
    }
}

fn fixture(
    nested: bool,
) -> (
    solve::NativeRefreshAssignmentSchedule,
    solve::VarLayout,
    solve::SolvePureCallTable,
) {
    let (table, site) = call_table();
    let source = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_program_spans(
            vec![
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 0 },
                    solve::LinearOp::LoadP { dst: 1, index: 2 },
                    solve::LinearOp::Binary {
                        dst: 2,
                        op: solve::BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    solve::LinearOp::StoreOutput { src: 2 },
                ],
                vec![
                    solve::LinearOp::TensorLoad {
                        dst_start: 0,
                        input: solve::TensorInputKind::P,
                        input_start: 0,
                        count: 2,
                        seed_start: None,
                        lanes: 1,
                    },
                    conditional(&site, nested),
                    solve::LinearOp::LoadY { dst: 3, index: 1 },
                    solve::LinearOp::Binary {
                        dst: 4,
                        op: solve::BinaryOp::Sub,
                        lhs: 3,
                        rhs: 2,
                    },
                    solve::LinearOp::StoreOutput { src: 4 },
                ],
            ],
            vec![span(820); 2],
        )
        .unwrap(),
    );
    let layout = solve::VarLayout::from_parts(Default::default(), 2, 3);
    let owners = solve::NativeRefreshAssignmentSchedule::from_continuous_block(
        &source,
        &[Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))],
        &layout,
    )
    .unwrap();
    (owners, layout, table)
}

#[test]
fn conditional_native_regions_preserve_ieee_captures_lazy_faults_and_recovery() {
    for nested in [false, true] {
        let (schedule, layout, table) = fixture(nested);
        let compiled =
            compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
        let mut runner = ProgramRunner::new(&compiled, &layout);
        for bits in [
            0u64,
            (-0.0_f64).to_bits(),
            1,
            f64::INFINITY.to_bits(),
            0x7ff8_4321_abcd_1234,
        ] {
            let value = f64::from_bits(bits);
            let (status, actual) = runner.run(&[0., value, -0.]);
            assert_eq!(status, 0);
            assert_eq!(actual[0].to_bits(), (-0.0_f64).to_bits());
            assert_eq!(actual[1].to_bits(), bits);
        }
        for value in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
            let (status, _) = runner.run(&[1., value, 13.]);
            let fault = compiled
                .faults()
                .iter()
                .find(|f| f.status == status as u32)
                .unwrap();
            assert_eq!(fault.kind, TypedCallFaultKind::IntegerConversion);
            assert_eq!(fault.provenance, span(802));
            let (status, actual) = runner.run(&[1., -8.75, 13.]);
            assert_eq!(status, 0);
            assert_eq!(actual, [13., -8.]);
        }
    }
}

#[test]
fn conditional_native_invalid_buffer_is_atomic_then_instance_recovers() {
    let (schedule, layout, table) = fixture(true);
    let compiled =
        compile_native_assignment_schedule_with_calls_wasm(&schedule, &layout, &table).unwrap();
    let mut runner = ProgramRunner::new(&compiled, &layout);
    runner.run(&[0., 7., 13.]);
    let before = runner.memory.data(&runner.store).to_vec();
    let status = runner
        .call
        .call(&mut runner.store, (0, 0, 0., runner.scratch as i32, 0))
        .unwrap();
    assert_eq!(status, 1);
    assert_eq!(runner.memory.data(&runner.store), before);
    assert_eq!(runner.run(&[1., 7., 13.]), (0, vec![13., 7.]));
}

#[test]
fn conditional_complete_scalar_tuple_keeps_earlier_private_result_on_late_fault() {
    let (table, site) = call_table();
    let conditional = solve::LinearOp::FunctionConditional {
        dst_start: 4,
        capture_start: 2,
        program: Arc::new(
            solve::FunctionConditionalProgram::checked(
                2,
                vec![1, 1],
                [(
                    vec![capture(0, 0), solve::LinearOp::StoreOutput { src: 0 }],
                    vec![
                        capture(0, 1),
                        solve::LinearOp::StoreOutput { src: 0 },
                        solve::LinearOp::PureCall {
                            dst_start: 1,
                            input_starts: vec![0].into(),
                            site,
                        },
                        solve::LinearOp::StoreOutput { src: 1 },
                    ],
                )],
                vec![
                    capture(0, 1),
                    solve::LinearOp::StoreOutput { src: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            )
            .unwrap(),
        ),
    };
    let source = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_program_spans(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::LoadY { dst: 1, index: 1 },
                solve::LinearOp::TensorLoad {
                    dst_start: 2,
                    input: solve::TensorInputKind::P,
                    input_start: 0,
                    count: 2,
                    seed_start: None,
                    lanes: 1,
                },
                conditional,
                solve::LinearOp::Binary {
                    dst: 6,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 4,
                },
                solve::LinearOp::Binary {
                    dst: 7,
                    op: solve::BinaryOp::Sub,
                    lhs: 1,
                    rhs: 5,
                },
                solve::LinearOp::Move { dst: 8, src: 6 },
                solve::LinearOp::Move { dst: 9, src: 7 },
                solve::LinearOp::StoreOutputRange {
                    start: 8,
                    count: 2,
                    stride: 1,
                },
            ]],
            vec![span(820)],
        )
        .unwrap(),
    );
    let layout = solve::VarLayout::from_parts(Default::default(), 2, 2);
    let owners = solve::NativeRefreshAssignmentSchedule::from_continuous_block(
        &source,
        &[Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))],
        &layout,
    )
    .unwrap();
    let compiled =
        compile_native_assignment_schedule_with_calls_wasm(&owners, &layout, &table).unwrap();
    let mut runner = ProgramRunner::new(&compiled, &layout);
    for value in [f64::from_bits(0x7ff8_4321_abcd_1234), f64::INFINITY] {
        let (status, actual) = runner.run(&[0., value]);
        assert_eq!(status, 0);
        assert_eq!(
            actual.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            vec![value.to_bits(); 2]
        );
        let (status, _) = runner.run(&[1., value]);
        assert!(compiled.faults().iter().any(|f| f.status == status as u32
            && f.kind == TypedCallFaultKind::IntegerConversion
            && f.provenance == span(802)));
        assert_eq!(runner.run(&[1., -8.75]), (0, vec![-8.75, -8.]));
    }
}

#[test]
fn conditional_region_range_publication_refuses_before_module_publication() {
    let (table, site) = call_table();
    let conditional = solve::LinearOp::FunctionConditional {
        dst_start: 2,
        capture_start: 0,
        program: Arc::new(
            solve::FunctionConditionalProgram::checked(
                2,
                vec![1],
                [(
                    vec![capture(0, 0), solve::LinearOp::StoreOutput { src: 0 }],
                    vec![
                        capture(0, 1),
                        solve::LinearOp::PureCall {
                            dst_start: 1,
                            input_starts: vec![0].into(),
                            site,
                        },
                        solve::LinearOp::StoreOutputRange {
                            start: 1,
                            count: 1,
                            stride: 1,
                        },
                    ],
                )],
                vec![capture(0, 1), solve::LinearOp::StoreOutput { src: 0 }],
            )
            .unwrap(),
        ),
    };
    let block = solve::ScalarProgramBlock::with_program_spans(
        vec![vec![
            solve::LinearOp::TensorLoad {
                dst_start: 0,
                input: solve::TensorInputKind::P,
                input_start: 0,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            conditional,
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        vec![span(820)],
    )
    .unwrap();
    let error = rumoca_exec_wasm::compile_private_program_wasm(
        &block,
        &solve::VarLayout::from_parts(Default::default(), 1, 2),
        &table,
    )
    .err()
    .expect("conditional range publication must be refused");
    assert!(
        error
            .to_string()
            .contains("native conditional range publication is unsupported"),
        "{error}"
    );
}
