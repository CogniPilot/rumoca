//! Checked fixtures shared with standalone template diagnostics through an explicit JSON export.
use super::super::*;
use solve::LinearOp as Op;

fn domain(lower: i64, upper: i64, step: i64) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_owned(),
            lower,
            upper,
            step,
        }],
    }
}

fn sum_program(lower: i64, upper: i64, step: i64) -> Arc<solve::FunctionFoldProgram> {
    Arc::new(
        solve::FunctionFoldProgram::checked(
            domain(lower, upper, step),
            2,
            1,
            vec![
                Op::LoadFoldCarried { dst: 0, index: 0 },
                Op::LoadFoldCarried { dst: 1, index: 1 },
                Op::LoadFoldCapture { dst: 2, index: 0 },
                Op::LoadFoldIndex {
                    dst: 3,
                    dimension: 0,
                },
                Op::Binary {
                    dst: 4,
                    op: solve::BinaryOp::Add,
                    lhs: 0,
                    rhs: 3,
                },
                Op::Binary {
                    dst: 5,
                    op: solve::BinaryOp::Mul,
                    lhs: 2,
                    rhs: 3,
                },
                Op::Binary {
                    dst: 6,
                    op: solve::BinaryOp::Add,
                    lhs: 1,
                    rhs: 5,
                },
                Op::StoreOutput { src: 4 },
                Op::StoreOutput { src: 6 },
            ],
        )
        .unwrap(),
    )
}

fn continued_sum_program() -> Arc<solve::FunctionFoldProgram> {
    Arc::new(
        sum_program(1, 100, 1)
            .as_ref()
            .clone()
            .with_continuation(vec![
                Op::LoadFoldCarried { dst: 0, index: 0 },
                Op::Const { dst: 1, value: 4.0 },
                Op::Compare {
                    dst: 2,
                    op: solve::CompareOp::Lt,
                    lhs: 0,
                    rhs: 1,
                },
                Op::StoreOutput { src: 2 },
            ])
            .unwrap(),
    )
}

fn fold_row(program: Arc<solve::FunctionFoldProgram>, active: Option<f64>) -> Vec<Op> {
    let mut row = vec![
        Op::Const { dst: 0, value: 1.0 },
        Op::Const { dst: 1, value: 2.0 },
        Op::Const {
            dst: 2,
            value: 10.0,
        },
    ];
    if let Some(value) = active {
        row.push(Op::Const { dst: 3, value });
        row.push(Op::GuardedFunctionFold {
            dst_start: 4,
            initial_start: 0,
            capture_start: 2,
            activation: 3,
            program,
        });
    } else {
        row.push(Op::FunctionFold {
            dst_start: 4,
            initial_start: 0,
            capture_start: 2,
            program,
        });
    }
    row.push(Op::StoreOutputRange {
        start: 4,
        count: 2,
        stride: 1,
    });
    row
}

fn nested_program() -> Arc<solve::FunctionFoldProgram> {
    let inner = Arc::new(
        solve::FunctionFoldProgram::checked(
            domain(1, 2, 1),
            2,
            1,
            vec![
                Op::LoadFoldCarried { dst: 0, index: 0 },
                Op::LoadFoldCarried { dst: 1, index: 1 },
                Op::LoadFoldCapture { dst: 2, index: 0 },
                Op::LoadFoldIndex {
                    dst: 3,
                    dimension: 0,
                },
                Op::Binary {
                    dst: 4,
                    op: solve::BinaryOp::Add,
                    lhs: 2,
                    rhs: 3,
                },
                Op::Binary {
                    dst: 5,
                    op: solve::BinaryOp::Add,
                    lhs: 0,
                    rhs: 4,
                },
                Op::Const { dst: 6, value: 1.0 },
                Op::Binary {
                    dst: 7,
                    op: solve::BinaryOp::Add,
                    lhs: 1,
                    rhs: 6,
                },
                Op::StoreOutput { src: 5 },
                Op::StoreOutput { src: 7 },
            ],
        )
        .unwrap(),
    );
    Arc::new(
        solve::FunctionFoldProgram::checked(
            domain(1, 3, 1),
            2,
            0,
            vec![
                Op::LoadFoldIndex {
                    dst: 0,
                    dimension: 0,
                },
                Op::Const { dst: 1, value: 2.0 },
                Op::Compare {
                    dst: 2,
                    op: solve::CompareOp::Ne,
                    lhs: 0,
                    rhs: 1,
                },
                Op::StoreOutputFunctionFold {
                    initial: vec![solve::FoldInitialSource::ParentCarried { base: 0, count: 2 }]
                        .into(),
                    capture_start: 0,
                    program: inner,
                    result_base: 0,
                    count: 2,
                    condition: Some(2),
                    nested_when_true: true,
                },
            ],
        )
        .unwrap(),
    )
}

fn tensor_program() -> Arc<solve::FunctionFoldProgram> {
    Arc::new(
        solve::FunctionFoldProgram::checked(
            domain(1, 2, 1),
            8,
            0,
            vec![
                Op::LoadFoldIndex {
                    dst: 0,
                    dimension: 0,
                },
                Op::Const {
                    dst: 1,
                    value: 10.0,
                },
                Op::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                Op::Move { dst: 3, src: 0 },
                Op::Const { dst: 4, value: 1.0 },
                Op::Move { dst: 5, src: 2 },
                Op::Const {
                    dst: 6,
                    value: 10.0,
                },
                Op::Const { dst: 7, value: 2.0 },
                Op::Compare {
                    dst: 8,
                    op: solve::CompareOp::Lt,
                    lhs: 0,
                    rhs: 7,
                },
                Op::StoreOutputFoldTensorUpdate {
                    source_base: 0,
                    source_stride: 2,
                    dimensions: vec![2, 2].into(),
                    lanes: 2,
                    updates: vec![solve::FoldTensorUpdate {
                        subscripts: vec![
                            solve::TensorUpdateSubscript::Index(solve::TensorIndex::Runtime(0)),
                            solve::TensorUpdateSubscript::Whole,
                        ]
                        .into(),
                        condition: None,
                        value_start: 3,
                        value_stride: 2,
                    }]
                    .into(),
                    nodes: vec![
                        solve::FoldTensorNode::Update { base: 0, update: 0 },
                        solve::FoldTensorNode::Select {
                            condition: 8,
                            if_true: 1,
                            if_false: 0,
                        },
                    ]
                    .into(),
                    result: 2,
                },
            ],
        )
        .unwrap(),
    )
}

fn fixture(
    name: &str,
    row: Vec<Op>,
    expected: Option<&[f64]>,
    tables: &[rumoca_core::ExternalTableData],
) -> Value {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("checked_solve_fixture.mo"),
        0,
        1,
    )
    .require_provenance("checked Solve fixture")
    .unwrap();
    let block = Arc::new(solve::ScalarProgramBlock::with_source_span(vec![row], span).unwrap());
    let runtime = rumoca_eval_solve::SimulationRuntimeState::new();
    let mut output = vec![0.0; block.len()];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &block,
        &[],
        &[],
        0.0,
        rumoca_eval_solve::RowEvalContext {
            external_tables: Some(tables),
            runtime_state: Some(&runtime),
            ..Default::default()
        },
        &mut output,
    )
    .unwrap();
    if let Some(expected) = expected {
        assert_eq!(output, expected, "{name}");
    }
    let plan = super::super::super::scalar_program_plan::ScalarProgramPlan::new(block).unwrap();
    minijinja::context! {name=>name,expected=>output,external_tables=>tables,plan=>Value::from_object(plan)}
}

#[test]
// SPEC_0021: Exception - one regression renders the complete checked scalar fixture table.
#[allow(clippy::too_many_lines)]
fn checked_solve_fixture_export_matches_native_evaluation() {
    let mut fixtures = vec![
        fixture(
            "continued_sum",
            fold_row(continued_sum_program(), None),
            Some(&[4.0, 32.0]),
            &[],
        ),
        fixture(
            "sum",
            fold_row(sum_program(1, 3, 1), None),
            Some(&[7.0, 62.0]),
            &[],
        ),
        fixture(
            "descending",
            fold_row(sum_program(3, 1, -1), None),
            Some(&[7.0, 62.0]),
            &[],
        ),
        fixture(
            "empty",
            fold_row(sum_program(1, 0, 1), None),
            Some(&[1.0, 2.0]),
            &[],
        ),
        fixture(
            "inactive",
            fold_row(sum_program(1, 3, 1), Some(0.0)),
            Some(&[1.0, 2.0]),
            &[],
        ),
        fixture(
            "active",
            fold_row(sum_program(1, 3, 1), Some(1.0)),
            Some(&[7.0, 62.0]),
            &[],
        ),
        fixture(
            "nested",
            fold_row(nested_program(), None),
            Some(&[15.0, 6.0]),
            &[],
        ),
    ];
    let mut tensor = (0..8)
        .map(|dst| Op::Const {
            dst,
            value: [1.0, 11.0, 2.0, 22.0, 3.0, 33.0, 4.0, 44.0][dst as usize],
        })
        .collect::<Vec<_>>();
    tensor.push(Op::FunctionFold {
        dst_start: 8,
        initial_start: 0,
        capture_start: 0,
        program: tensor_program(),
    });
    tensor.push(Op::StoreOutputRange {
        start: 8,
        count: 8,
        stride: 1,
    });
    fixtures.push(fixture(
        "tensor_lanes",
        tensor,
        Some(&[1.0, 1.0, 10.0, 10.0, 3.0, 33.0, 4.0, 44.0]),
        &[],
    ));
    for generator in [
        solve::RandomGenerator::Xorshift64Star,
        solve::RandomGenerator::Xorshift128Plus,
        solve::RandomGenerator::Xorshift1024Star,
    ] {
        let mut row = vec![
            Op::Const {
                dst: 0,
                value: -12.5,
            },
            Op::Const {
                dst: 1,
                value: 42.0,
            },
        ];
        let len = match generator {
            solve::RandomGenerator::Xorshift64Star => 2,
            solve::RandomGenerator::Xorshift128Plus => 4,
            solve::RandomGenerator::Xorshift1024Star => 33,
        };
        for index in 0..len {
            row.push(Op::RandomInitialState {
                dst: 2 + index as u32,
                generator,
                local_seed: 0,
                global_seed: 1,
                state_len: len,
                state_index: index,
            });
        }
        row.push(Op::RandomResult {
            dst: 2 + len as u32,
            generator,
            state_start: 2,
            state_len: len,
        });
        for index in 0..len {
            row.push(Op::RandomState {
                dst: 3 + len as u32 + index as u32,
                generator,
                state_start: 2,
                state_len: len,
                state_index: index,
            });
        }
        row.push(Op::ImpureRandomInit {
            dst: 3 + 2 * len as u32,
            seed: 0,
        });
        row.push(Op::StoreOutputRange {
            start: 2,
            count: 2 * len + 2,
            stride: 1,
        });
        fixtures.push(fixture(&format!("random_{generator:?}"), row, None, &[]));
    }
    fixtures.push(fixture(
        "impure_event_calls",
        vec![
            Op::Const {
                dst: 0,
                value: 17.0,
            },
            Op::ImpureRandomInit { dst: 1, seed: 0 },
            Op::ImpureRandom {
                dst: 2,
                id: 1,
                call_site: u64::MAX,
            },
            Op::ImpureRandom {
                dst: 3,
                id: 1,
                call_site: u64::MAX,
            },
            Op::ImpureRandom {
                dst: 4,
                id: 1,
                call_site: u64::MAX - 1,
            },
            Op::Const { dst: 5, value: 7.0 },
            Op::Const {
                dst: 6,
                value: -3.0,
            },
            Op::ImpureRandomInteger {
                dst: 7,
                id: 1,
                imin: 5,
                imax: 6,
                call_site: 42,
            },
            Op::StoreOutputRange {
                start: 1,
                count: 7,
                stride: 1,
            },
        ],
        None,
        &[],
    ));
    let mut table_id = 100;
    for smoothness in [1, 2, 3] {
        for extrapolation in [1, 2, 3, 4] {
            for input in [-0.5, 0.0, 0.25, 1.0, 1.5, 3.0, 3.5] {
                table_id += 1;
                let table = rumoca_core::ExternalTableData {
                    id: table_id,
                    data: vec![
                        vec![0.0, 0.0, 2.0],
                        vec![1.0, 2.0, 5.0],
                        vec![2.0, 1.0, 3.0],
                        vec![3.0, 4.0, 8.0],
                    ],
                    columns: vec![3],
                    smoothness,
                    extrapolation,
                };
                let row = vec![
                    Op::Const {
                        dst: 0,
                        value: table_id as f64,
                    },
                    Op::Const { dst: 1, value: 1.0 },
                    Op::Const {
                        dst: 2,
                        value: input,
                    },
                    Op::TableBounds {
                        dst: 3,
                        table_id: 0,
                        max: false,
                    },
                    Op::TableBounds {
                        dst: 4,
                        table_id: 0,
                        max: true,
                    },
                    Op::TableLookup {
                        dst: 5,
                        table_id: 0,
                        column: 1,
                        input: 2,
                    },
                    Op::TableLookupSlope {
                        dst: 6,
                        table_id: 0,
                        column: 1,
                        input: 2,
                    },
                    Op::TableNextEvent {
                        dst: 7,
                        table_id: 0,
                        time: 2,
                    },
                    Op::StoreOutputRange {
                        start: 3,
                        count: 5,
                        stride: 1,
                    },
                ];
                fixtures.push(fixture(
                    &format!("table_{smoothness}_{extrapolation}_{input}"),
                    row,
                    None,
                    &[table],
                ));
            }
        }
    }
    let mut env = minijinja::Environment::new();
    env.add_template("fixture", "{{ fixtures|tojson }}")
        .unwrap();
    let json = env
        .get_template("fixture")
        .unwrap()
        .render(minijinja::context! {fixtures=>fixtures})
        .unwrap();
    let decoded: serde_json::Value = serde_json::from_str(&json).unwrap();
    assert_eq!(decoded.as_array().unwrap().len(), 96);
}
