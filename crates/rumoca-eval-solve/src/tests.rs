use super::*;
use crate::prepared::assignment_shape_reads_y_index;
use crate::random_runtime::checked_random_reg_offset;
use rumoca_ir_solve::RandomGenerator;
use rumoca_ir_solve::TargetAssignmentShape;

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("eval_solve_tests_source_45.mo"),
        0,
        1,
    )
}

#[test]
fn parameter_static_gradient_certificate_rejects_y_and_time_varying_coefficients() {
    let prepare = |row| {
        let block = ScalarProgramBlock::with_program_spans(vec![row], vec![fixture_span()])
            .expect("gradient-certificate fixture is source-backed");
        PreparedScalarProgramBlock::new(block).expect("gradient-certificate fixture prepares")
    };
    let parameter_affine = prepare(vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::LoadTime { dst: 3 },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ]);
    let time_coefficient = prepare(vec![
        LinearOp::LoadTime { dst: 0 },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]);
    let nonlinear = prepare(vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Binary {
            dst: 1,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ]);

    assert!(parameter_affine.certifies_parameter_static_y_gradient(0));
    assert!(!time_coefficient.certifies_parameter_static_y_gradient(0));
    assert!(!nonlinear.certifies_parameter_static_y_gradient(0));
}

#[test]
fn prepared_parameter_dependencies_preserve_tensor_load_ranges() {
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: rumoca_ir_solve::TensorInputKind::P,
                input_start: 7,
                count: 4,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![fixture_span()],
    )
    .expect("tensor parameter dependency fixture is source-backed");
    let prepared = PreparedScalarProgramBlock::new(block).expect("tensor program prepares");

    assert_eq!(
        prepared.row_parameter_indices(0),
        Some([7, 8, 9, 10].as_slice())
    );
}

#[test]
fn prepared_parameter_dependencies_recurse_through_lazy_conditional_regions() {
    let condition = vec![
        LinearOp::LoadP { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let result = vec![
        LinearOp::LoadP { dst: 0, index: 2 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let fallback = vec![
        LinearOp::LoadP { dst: 0, index: 3 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let conditional = rumoca_ir_solve::FunctionConditionalProgram::checked(
        0,
        [1],
        [(condition, result)],
        fallback,
    )
    .expect("lazy conditional fixture has a checked region ABI");
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: std::sync::Arc::new(conditional),
            },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![fixture_span()],
    )
    .expect("lazy conditional parameter fixture is source-backed");
    let prepared = PreparedScalarProgramBlock::new(block).expect("conditional program prepares");

    assert_eq!(
        prepared.row_parameter_indices(0),
        Some([1, 2, 3].as_slice())
    );
}

#[test]
fn projected_random_value_reports_missing_state_lane() {
    let err = projected_random_value(&[10.0, 20.0], 2)
        .expect_err("missing random state lane must be reported");
    assert_eq!(
        err,
        EvalSolveError::RandomStateProjectionOutOfBounds { index: 2, len: 2 }
    );

    let err =
        projected_random_value(&[], 0).expect_err("empty random state projection must be reported");
    assert_eq!(
        err,
        EvalSolveError::RandomStateProjectionOutOfBounds { index: 0, len: 0 }
    );
}

#[test]
fn read_reg_range_rejects_register_offset_overflow() {
    let err = checked_random_reg_offset(u32::MAX, 1)
        .expect_err("overflowing random register range should fail");

    assert!(matches!(err, EvalSolveError::InvalidRow { .. }));
    assert!(
        err.to_string()
            .contains("random register range starting at 4294967295 overflows")
    );
}

#[test]
fn initial_state_values_rejects_impossible_capacity() {
    let err = initial_state_values(RandomGenerator::Xorshift64Star, 1, 2, usize::MAX)
        .expect_err("impossible random state capacity should fail");

    assert!(matches!(err, EvalSolveError::InvalidRow { .. }));
    assert!(
        err.to_string()
            .contains("random initial state value count exceeds host memory limits")
    );
}

#[test]
fn eval_solve_f64_values_rejects_impossible_capacity() {
    let err = eval_solve_f64_values(usize::MAX, 0.0, "event action condition values")
        .expect_err("impossible eval-solve value capacity should fail");

    assert!(matches!(err, EvalSolveError::InvalidRow { .. }));
    assert!(
        err.to_string()
            .contains("event action condition values exceeds host memory limits")
    );
}

#[test]
fn eval_row_compare_equality_is_exact_not_epsilon_based() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Const {
            dst: 1,
            value: f64::MIN_POSITIVE,
        },
        LinearOp::Compare {
            dst: 2,
            op: CompareOp::Eq,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Compare {
            dst: 3,
            op: CompareOp::Ne,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];

    let output = eval_row(&row, &[], &[], 0.0, None).expect("compare row evaluates");

    assert_eq!(output, 1.0);
}

#[test]
fn eval_event_action_message_concatenates_text_and_numeric_parts() {
    let events = rumoca_ir_solve::SolveEventPartition {
        action_conditions: ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span()
                .require_provenance("evaluator fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("event action condition fixture is computable"),
        actions: vec![rumoca_ir_solve::SolveEventAction {
            kind: SolveEventActionKind::Assert,
            message: rumoca_ir_solve::SolveEventMessage {
                parts: vec![
                    rumoca_ir_solve::SolveEventMessagePart::Text("value = ".to_string()),
                    rumoca_ir_solve::SolveEventMessagePart::Conversion {
                        value: vec![
                            LinearOp::LoadY { dst: 0, index: 0 },
                            LinearOp::StoreOutput { src: 0 },
                        ],
                        source: rumoca_ir_solve::SolveStringConversionSource::Real,
                        format: rumoca_ir_solve::SolveStringConversionFormat::Options {
                            minimum_length: None,
                            left_justified: None,
                            significant_digits: None,
                        },
                    },
                ],
            },
            span: fixture_span(),
            origin: "assert".to_string(),
            clock_owner: None,
        }],
        ..Default::default()
    };

    let request = eval_event_action_request(&events, &[3.5], &[], 0.0, RowEvalContext::default())
        .expect("event action should evaluate");

    assert_eq!(
        request,
        EventActionRequest::AssertionFailed {
            message: "value = 3.5".to_string()
        }
    );
}

#[test]
fn eval_event_action_message_applies_dynamic_string_options() {
    let events = event_message_fixture(
        rumoca_ir_solve::SolveStringConversionSource::Real,
        rumoca_ir_solve::SolveStringConversionFormat::Options {
            minimum_length: Some(constant_row(8.0)),
            left_justified: Some(constant_row(0.0)),
            significant_digits: Some(constant_row(3.0)),
        },
    );

    let request = eval_event_action_request(&events, &[3.5], &[], 0.0, RowEvalContext::default())
        .expect("well-typed dynamic String options should evaluate");

    assert_eq!(
        request,
        EventActionRequest::AssertionFailed {
            message: "value =      3.5".to_string()
        }
    );
}

#[test]
fn eval_event_action_message_rejects_oversized_width_with_source_span() {
    let events = event_message_fixture(
        rumoca_ir_solve::SolveStringConversionSource::Real,
        rumoca_ir_solve::SolveStringConversionFormat::Options {
            minimum_length: Some(constant_row((MAX_EVENT_MESSAGE_BYTES + 1) as f64)),
            left_justified: None,
            significant_digits: None,
        },
    );

    let error = eval_event_action_request(&events, &[3.5], &[], 0.0, RowEvalContext::default())
        .expect_err("unbounded runtime formatting must fail before allocating");

    assert!(matches!(
        error,
        EvalSolveError::InvalidRow {
            span: Some(span),
            ..
        } if span == fixture_span()
    ));
    assert!(error.to_string().contains("minimumLength exceeds"));
}

fn event_message_fixture(
    source: rumoca_ir_solve::SolveStringConversionSource,
    format: rumoca_ir_solve::SolveStringConversionFormat,
) -> rumoca_ir_solve::SolveEventPartition {
    rumoca_ir_solve::SolveEventPartition {
        action_conditions: ScalarProgramBlock::with_source_span(
            vec![constant_row(1.0)],
            fixture_span()
                .require_provenance("evaluator fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("event action condition fixture is computable"),
        actions: vec![rumoca_ir_solve::SolveEventAction {
            kind: SolveEventActionKind::Assert,
            message: rumoca_ir_solve::SolveEventMessage {
                parts: vec![
                    rumoca_ir_solve::SolveEventMessagePart::Text("value = ".to_string()),
                    rumoca_ir_solve::SolveEventMessagePart::Conversion {
                        value: vec![
                            LinearOp::LoadY { dst: 0, index: 0 },
                            LinearOp::StoreOutput { src: 0 },
                        ],
                        source,
                        format,
                    },
                ],
            },
            span: fixture_span(),
            origin: "assert".to_string(),
            clock_owner: None,
        }],
        ..Default::default()
    }
}

fn constant_row(value: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value },
        LinearOp::StoreOutput { src: 0 },
    ]
}

#[test]
fn eval_row_division_uses_ieee_semantics() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Div,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];

    let value = eval_row(&row, &[], &[], 0.0, None).expect("0/0 row should evaluate");

    assert!(value.is_nan());

    let negative_over_zero = vec![
        LinearOp::Const {
            dst: 0,
            value: -1.0,
        },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Div,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let value =
        eval_row(&negative_over_zero, &[], &[], 0.0, None).expect("-1/0 row should evaluate");
    assert_eq!(value, f64::NEG_INFINITY);
}

#[test]
fn eval_row_sign_is_zero_at_zero() {
    for input in [0.0, -0.0] {
        let row = vec![
            LinearOp::Const {
                dst: 0,
                value: input,
            },
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Sign,
                arg: 0,
            },
            LinearOp::StoreOutput { src: 1 },
        ];
        let value = eval_row(&row, &[], &[], 0.0, None).expect("sign row should evaluate");
        assert_eq!(value, 0.0);
    }
}

#[test]
fn eval_row_missing_y_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = eval_row(&row, &[5.0], &[], 0.0, None)
        .expect_err("undersized y vector should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::MissingInput {
            vector: "y",
            index: 1,
            len: 1,
            span: None,
        }
    );
}

#[test]
fn eval_row_missing_p_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("undersized p vector should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::MissingInput {
            vector: "p",
            index: 0,
            len: 0,
            span: None,
        }
    );
}

#[test]
fn eval_row_missing_seed_input_is_error_not_zero() {
    let row = vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("missing seed vector should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::MissingInput {
            vector: "seed",
            index: 0,
            len: 0,
            span: None,
        }
    );
}

#[test]
fn eval_row_missing_source_register_is_error_not_panic_or_zero() {
    let row = vec![
        LinearOp::Move { dst: 0, src: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("missing source register should report an evaluation error");

    assert!(
        matches!(err, EvalSolveError::InvalidRow { span: None, .. }),
        "expected invalid row error, got {err:?}"
    );
    assert!(
        err.to_string().contains("Move op 0") && err.to_string().contains("undefined register r1"),
        "error should identify the first invalid register read: {err}"
    );
}

#[test]
fn eval_row_uninitialized_source_register_is_error_not_zero() {
    let row = vec![
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];

    let err = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("uninitialized register read should report an evaluation error");

    assert!(
        matches!(err, EvalSolveError::InvalidRow { span: None, .. }),
        "expected invalid row error, got {err:?}"
    );
    assert!(
        err.to_string().contains("Binary op 0")
            && err.to_string().contains("undefined register r1"),
        "error should identify the first invalid register read: {err}"
    );
}

#[test]
fn function_conditional_evaluates_only_the_selected_correlated_region() {
    let condition = vec![
        LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let selected = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let fallback = vec![
        LinearOp::Const { dst: 0, value: 7.0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionConditionalProgram::checked(
            1,
            [1],
            [(condition, selected)],
            fallback,
        )
        .expect("checked lazy conditional"),
    );
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::FunctionConditional {
                dst_start: 1,
                capture_start: 0,
                program: program.clone(),
            },
            LinearOp::StoreOutput { src: 1 },
        ]],
        vec![fixture_span()],
    )
    .expect("inactive conditional row");
    let mut output = [0.0];
    eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut output)
        .expect("inactive region must not read its missing parameter");
    assert_eq!(output, [7.0]);

    let active = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::FunctionConditional {
                dst_start: 1,
                capture_start: 0,
                program,
            },
            LinearOp::StoreOutput { src: 1 },
        ]],
        vec![fixture_span()],
    )
    .expect("active conditional row");
    eval_scalar_program_block(&active, &[], &[11.0], 0.0, None, &mut output)
        .expect("active region reads its parameter");
    assert_eq!(output, [11.0]);
}

#[test]
fn aggregate_conditional_remains_a_lazy_interpreter_program() {
    let condition = vec![
        LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let selected = vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let fallback = vec![
        LinearOp::Const { dst: 0, value: 3.0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let conditional = std::sync::Arc::new(
        rumoca_ir_solve::FunctionConditionalProgram::checked(
            1,
            [1],
            [(condition, selected)],
            fallback,
        )
        .expect("checked aggregate conditional"),
    );
    let mut row = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::FunctionConditional {
            dst_start: 1,
            capture_start: 0,
            program: conditional,
        },
        LinearOp::Const {
            dst: 2,
            value: 11.0,
        },
        LinearOp::Const {
            dst: 3,
            value: 13.0,
        },
        LinearOp::Select {
            dst: 4,
            cond: 1,
            if_true: 2,
            if_false: 3,
        },
    ];
    row.extend((5..64).map(|dst| LinearOp::Const {
        dst,
        value: dst as f64,
    }));
    row.push(LinearOp::StoreOutput { src: 4 });
    let block = ScalarProgramBlock::with_program_spans(vec![row], vec![fixture_span()])
        .expect("aggregate lazy-row fixture is source-backed");
    let prepared = PreparedScalarProgramBlock::new(block).expect("aggregate lazy row prepares");

    assert!(prepared.has_lazy_row_plan(0));
    assert_eq!(
        prepared
            .eval_row_with_context(0, &[], &[], 0.0, RowEvalContext::default())
            .expect("reference evaluator retains lazy execution"),
        11.0
    );
}

#[test]
fn eval_row_linsolve_missing_matrix_register_is_error_not_panic_or_zero() {
    let row = vec![
        LinearOp::LinearSolveComponent {
            dst: 0,
            matrix_start: 1,
            rhs_start: 5,
            n: 2,
            component: 0,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("malformed linear solve row should report an evaluation error");

    assert!(
        matches!(err, EvalSolveError::InvalidRow { span: None, .. }),
        "expected invalid row error, got {err:?}"
    );
    assert!(
        err.to_string().contains("LinearSolveComponent op 0")
            && err.to_string().contains("undefined register r1"),
        "error should identify the first invalid matrix register: {err}"
    );
}

#[test]
fn eval_row_linsolve_singular_matrix_is_error_not_zero() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::Const { dst: 3, value: 4.0 },
        LinearOp::Const { dst: 4, value: 3.0 },
        LinearOp::Const { dst: 5, value: 6.0 },
        LinearOp::LinearSolveComponent {
            dst: 6,
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            component: 0,
        },
        LinearOp::StoreOutput { src: 6 },
    ];

    let err = eval_row(&row, &[], &[], 0.0, None)
        .expect_err("a singular linear system must not produce a zero solution");

    assert_eq!(
        err,
        EvalSolveError::LinearSolve {
            size: 2,
            component: Some(0),
            reason: "singular matrix",
            span: None,
        }
    );
}

#[test]
fn batched_linsolve_rejects_short_output_instead_of_truncating() {
    let regs = [1.0, 0.0, 0.0, 1.0, 2.0, 3.0];
    let mut out = [0.0];

    let err = crate::linear_solve::solve_all_unchecked(
        &regs,
        0,
        4,
        2,
        crate::tensor_policy::LinearSolveKernel::Dense,
        None,
        &mut out,
    )
    .expect_err("a short output buffer must not truncate a linear solution");

    assert_eq!(
        err,
        EvalSolveError::OutputTooSmall {
            required: 2,
            len: 1,
            span: None,
        }
    );
}

#[test]
fn eval_scalar_program_block_short_output_is_error_not_truncation() {
    let block = ScalarProgramBlock::with_source_span(
        vec![
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_span()
            .require_provenance("evaluator fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("evaluation fixture is computable");
    let mut out = [0.0];

    let err = eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut out)
        .expect_err("short output buffer should report an evaluation error");

    assert_eq!(
        err,
        EvalSolveError::OutputTooSmall {
            required: 2,
            len: 1,
            span: None,
        }
    );
    assert_eq!(out, [0.0]);
}

#[test]
fn eval_scalar_program_block_uses_explicit_output_indices() {
    let block = ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                LinearOp::Const { dst: 0, value: 3.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 5.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        vec![fixture_span(); 2],
        vec![2, 4],
    )
    .expect("sparse output fixture metadata should match row count");
    let mut out = [9.0; 5];

    eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut out)
        .expect("sparse scalar block should evaluate");

    assert_eq!(out, [0.0, 0.0, 3.0, 0.0, 5.0]);
}

#[test]
fn eval_scalar_program_block_sparse_output_prevalidates_output_len() {
    let block = ScalarProgramBlock::with_output_indices(
        vec![vec![
            LinearOp::Const { dst: 0, value: 3.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![fixture_span()],
        vec![2],
    )
    .expect("sparse output fixture metadata should match row count");
    let mut out = [9.0; 2];

    let err = eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut out)
        .expect_err("short sparse output buffer should fail before execution");

    assert_eq!(
        err,
        EvalSolveError::OutputTooSmall {
            required: 3,
            len: 2,
            span: None,
        }
    );
    assert_eq!(out, [9.0, 9.0]);
}

#[test]
fn eval_scalar_program_block_prevalidates_inputs_before_mutating_output() {
    let block = ScalarProgramBlock::with_source_span(
        vec![
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadY { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_span()
            .require_provenance("evaluator fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("evaluation fixture is computable");
    let mut out = [9.0, 9.0];

    let err = eval_scalar_program_block(&block, &[5.0], &[], 0.0, None, &mut out)
        .expect_err("block should validate all input lengths before execution");

    assert_eq!(
        err,
        EvalSolveError::MissingInput {
            vector: "y",
            index: 1,
            len: 1,
            span: None,
        }
    );
    assert_eq!(out, [9.0, 9.0]);
}

#[test]
fn row_input_requirements_report_required_vector_lengths() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 2 },
        LinearOp::LoadP { dst: 1, index: 3 },
        LinearOp::LoadSeed { dst: 2, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];

    assert_eq!(
        row_input_requirements(&row).expect("valid row requirements should be computed"),
        RowInputRequirements {
            y_len: 3,
            p_len: 4,
            seed_len: 2,
        }
    );
}

/// A dual tensor load is the only seed reader some AD rows have. Its width has
/// to reach the requirements or the caller sizes the seed buffer for no seed at
/// all and the row fails at evaluation instead of at construction.
#[test]
fn row_input_requirements_span_tensor_load_windows() {
    let row = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::P,
            input_start: 2,
            count: 4,
            seed_start: Some(1),
            lanes: 2,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    assert_eq!(
        row_input_requirements(&row).expect("valid row requirements should be computed"),
        RowInputRequirements {
            y_len: 0,
            p_len: 6,
            seed_len: 5,
        }
    );
}

/// A primal tensor load reads no seed, so it demands no seed vector.
#[test]
fn row_input_requirements_omit_seed_for_primal_tensor_loads() {
    let row = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 1,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    assert_eq!(
        row_input_requirements(&row).expect("valid row requirements should be computed"),
        RowInputRequirements {
            y_len: 4,
            p_len: 0,
            seed_len: 0,
        }
    );
}

#[test]
fn scalar_program_block_input_requirements_merge_all_rows() {
    let block = ScalarProgramBlock::with_source_span(
        vec![
            vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadP { dst: 0, index: 2 },
                LinearOp::LoadSeed { dst: 1, index: 4 },
                LinearOp::StoreOutput { src: 1 },
            ],
        ],
        fixture_span()
            .require_provenance("evaluator fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("input-requirement fixture is computable");

    assert_eq!(
        scalar_program_block_input_requirements(&block)
            .expect("valid block requirements should be computed"),
        RowInputRequirements {
            y_len: 1,
            p_len: 3,
            seed_len: 5,
        }
    );
}

#[test]
fn row_input_requirements_reject_index_overflow() {
    let row = vec![
        LinearOp::LoadY {
            dst: 0,
            index: usize::MAX,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = row_input_requirements(&row).expect_err("overflowing input index should fail");

    assert!(
        matches!(err, EvalSolveError::InvalidRow { .. }),
        "expected invalid row error, got {err:?}"
    );
    assert!(
        err.to_string().contains("y input requirement overflow"),
        "error should explain input requirement overflow: {err}"
    );
}

#[test]
fn tensor_assignment_uses_selected_output_dependency_not_whole_program() {
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: rumoca_ir_solve::TensorInputKind::Y,
                input_start: 10,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::Const { dst: 2, value: 7.0 },
            LinearOp::Unary {
                dst: 3,
                op: UnaryOp::Sin,
                arg: 0,
            },
            LinearOp::StoreOutput { src: 2 },
            LinearOp::StoreOutput { src: 3 },
        ]],
        vec![fixture_span()],
    )
    .expect("tensor assignment fixture is source-backed");
    let prepared = PreparedScalarProgramBlock::new(block).expect("tensor fixture prepares");

    assert!(prepared.row_reads_y(0, 10));
    assert!(prepared.can_evaluate_target_assignment_output(0, 0, 10));
    assert!(!prepared.can_evaluate_target_assignment_output(0, 1, 10));
}

#[test]
fn assignment_dependency_uses_the_certified_expression_prefix() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 10 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::Const { dst: 0, value: 7.0 },
    ];
    let shape = TargetAssignmentShape::Direct {
        target_y_index: 11,
        expr_reg: 0,
        target_scale: 1.0,
        expr_eval_len: 1,
    };

    assert!(assignment_shape_reads_y_index(&row, shape, 10));
    assert!(assignment_shape_reads_y_index(
        &row,
        TargetAssignmentShape::Direct {
            target_y_index: 11,
            expr_reg: 0,
            target_scale: 1.0,
            expr_eval_len: row.len() + 1,
        },
        10,
    ));
}

#[test]
fn required_registers_reject_random_state_range_overflow() {
    let row = vec![
        LinearOp::RandomResult {
            dst: 1,
            generator: RandomGenerator::Xorshift64Star,
            state_start: u32::MAX,
            state_len: 2,
        },
        LinearOp::StoreOutput { src: 1 },
    ];

    let err = required_registers(&row).expect_err("overflowing random state range should fail");

    assert!(
        matches!(err, EvalSolveError::InvalidRow { .. }),
        "expected invalid row error, got {err:?}"
    );
    assert!(
        err.to_string().contains("RandomResult op 0 register range")
            && err.to_string().contains("overflows"),
        "error should explain random state range overflow: {err}"
    );
}

#[test]
fn required_registers_reject_linear_solve_matrix_size_overflow() {
    let row = vec![
        LinearOp::LinearSolveComponent {
            dst: 0,
            matrix_start: 0,
            rhs_start: 0,
            n: usize::MAX,
            component: 0,
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    let err = required_registers(&row).expect_err("overflowing matrix size should fail");

    assert!(
        matches!(err, EvalSolveError::InvalidRow { .. }),
        "expected invalid row error, got {err:?}"
    );
    assert!(
        err.to_string()
            .contains("LinearSolveComponent op 0 register range")
            && err.to_string().contains("overflows"),
        "error should explain linear solve matrix overflow: {err}"
    );
}

#[test]
fn prepared_scalar_block_attaches_row_span_to_invalid_row() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_prepared_row.mo"),
        11,
        19,
    );
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::LoadY {
                dst: 0,
                index: usize::MAX,
            },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![span],
    )
    .expect("invalid-row fixture metadata should match row count");

    let err = match PreparedScalarProgramBlock::new(block) {
        Ok(_) => panic!("prepared scalar block should reject overflowing input requirement"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string().contains("y input requirement overflow"),
        "error should explain input requirement overflow: {err}"
    );
}

#[test]
fn prepared_scalar_block_indexes_sparse_single_output_rows() {
    let span = fixture_span();
    let block = ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 3.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        vec![span; 3],
        vec![4, 1, 7, 3],
    )
    .expect("sparse-output fixture should satisfy the block contract");
    let prepared = PreparedScalarProgramBlock::new(block).expect("fixture should prepare");

    assert_eq!(prepared.row_output_count(0), Some(2));
    assert_eq!(prepared.row_output_count(1), Some(1));
    assert_eq!(prepared.row_output_index(0, 1), Some(1));
    assert_eq!(prepared.row_output_index(0, 2), None);
    assert_eq!(prepared.single_output_row_for_output_index(4), None);
    assert_eq!(prepared.single_output_row_for_output_index(7), Some(1));
    assert_eq!(prepared.single_output_row_for_output_index(3), Some(2));
}

#[test]
fn scalar_program_construction_rejects_ambiguous_single_output_owners() {
    let span = fixture_span();
    let error = ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::StoreOutput { src: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 3.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        vec![span; 3],
        vec![2, 2, 3, 2],
    )
    .expect_err("one logical output cannot have ambiguous scalar owners");

    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(
        error,
        rumoca_ir_solve::SolveProblemShapeContractError::DuplicateIndex {
            context: "ScalarProgramBlock.output_indices",
            index: 2,
            span: Some(error_span),
        } if error_span == span
    ));
}

#[test]
fn scalar_program_construction_rejects_logical_output_count_overflow() {
    let span = fixture_span();
    let error = ScalarProgramBlock::with_output_indices(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![span],
        vec![usize::MAX],
    )
    .expect_err("logical output count overflow must fail block construction");

    assert_eq!(error.source_span(), Some(span));
    assert!(matches!(
        error,
        rumoca_ir_solve::SolveProblemShapeContractError::OutputIndexOverflow {
            context,
            node_index: 0,
            span: Some(error_span),
        } if context == "ScalarProgramBlock" && error_span == span
    ));
}

#[test]
fn prepared_scalar_block_rejects_unallocatable_sparse_output_index() {
    let span = fixture_span();
    let block = ScalarProgramBlock::with_output_indices(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![span],
        vec![usize::MAX / 2],
    )
    .expect("sparse output fixture satisfies scalar-program contracts");

    let error = match PreparedScalarProgramBlock::new(block) {
        Ok(_) => panic!("unallocatable sparse output metadata should fail preparation"),
        Err(error) => error,
    };
    assert!(error.to_string().contains("host memory limits"));
    assert_eq!(error.source_span(), Some(span));
}

#[test]
fn scalar_program_construction_attaches_span_to_register_error() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_register_row.mo"),
        7,
        15,
    );
    let err = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Move { dst: 1, src: 0 },
            LinearOp::StoreOutput { src: 1 },
        ]],
        vec![span],
    )
    .expect_err("undefined register reads must fail during scalar-program construction");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string()
            .contains("Move op 0 reads undefined register r0"),
        "error should explain the undefined register: {err}"
    );
}

#[test]
fn prepared_target_assignment_attaches_span_to_singular_row() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_target_row.mo"),
        3,
        12,
    );
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 0.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::Const { dst: 3, value: 1.0 },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Add,
                lhs: 2,
                rhs: 3,
            },
            LinearOp::StoreOutput { src: 4 },
        ]],
        vec![span],
    )
    .expect("singular-target fixture metadata should match row count");
    let prepared =
        PreparedScalarProgramBlock::new(block).expect("affine singular row should prepare");

    let err = prepared
        .eval_target_assignment_row_with_context(0, 0, &[2.0], &[], 0.0, RowEvalContext::default())
        .expect_err("singular target assignment should fail at row evaluation");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string().contains("singular coefficient 0"),
        "error should explain the singular target assignment: {err}"
    );
}

#[test]
fn eval_row_supports_solve_ir_random_ops() {
    let initial_state = vec![
        LinearOp::Const {
            dst: 0,
            value: 11.0,
        },
        LinearOp::Const {
            dst: 1,
            value: 17.0,
        },
        LinearOp::RandomInitialState {
            dst: 2,
            generator: RandomGenerator::Xorshift64Star,
            local_seed: 0,
            global_seed: 1,
            state_len: 2,
            state_index: 0,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let seed0 = eval_row(&initial_state, &[], &[], 0.0, None).expect("initial random state row");
    assert!(seed0.is_finite() && seed0 >= 1.0);

    let random_result = vec![
        LinearOp::Const {
            dst: 0,
            value: seed0,
        },
        LinearOp::Const {
            dst: 1,
            value: 23.0,
        },
        LinearOp::RandomResult {
            dst: 2,
            generator: RandomGenerator::Xorshift64Star,
            state_start: 0,
            state_len: 2,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let result = eval_row(&random_result, &[], &[], 0.0, None).expect("random result row");
    assert!(result.is_finite() && result > 0.0 && result <= 1.0);

    let random_state = vec![
        LinearOp::Const {
            dst: 0,
            value: seed0,
        },
        LinearOp::Const {
            dst: 1,
            value: 23.0,
        },
        LinearOp::RandomState {
            dst: 2,
            generator: RandomGenerator::Xorshift64Star,
            state_start: 0,
            state_len: 2,
            state_index: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let state1 = eval_row(&random_state, &[], &[], 0.0, None).expect("random state row");
    assert!(state1.is_finite() && state1 >= 1.0);
}

#[test]
fn eval_row_supports_event_stable_impure_random_ops() {
    clear_runtime_state();
    let init = vec![
        LinearOp::Const {
            dst: 0,
            value: 67867967.0,
        },
        LinearOp::ImpureRandomInit { dst: 1, seed: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];
    let runtime_state = SimulationRuntimeState::new();
    let id = eval_row(&init, &[], &[], 0.0, None).expect("impure random init row");
    assert!(id.is_finite() && id >= 1.0);

    let draw = vec![
        LinearOp::Const { dst: 0, value: id },
        LinearOp::ImpureRandom {
            dst: 1,
            id: 0,
            call_site: 42,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let draw_with_state = |t| {
        eval_row_with_context(
            &draw,
            &[],
            &[],
            t,
            RowEvalContext {
                runtime_state: Some(&runtime_state),
                ..Default::default()
            },
        )
        .expect("impure random draw row")
    };
    let first = draw_with_state(1.0);
    let repeated_same_event = draw_with_state(1.0);
    let next_event = draw_with_state(2.0);

    assert!(first > 0.0 && first <= 1.0);
    assert_eq!(first, repeated_same_event);
    assert_ne!(first, next_event);
}

#[test]
fn simulation_runtime_state_clear_resets_impure_random_streams() {
    let init = vec![
        LinearOp::Const {
            dst: 0,
            value: 67867967.0,
        },
        LinearOp::ImpureRandomInit { dst: 1, seed: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];
    let draw = |runtime_state: &SimulationRuntimeState, id| {
        eval_row_with_context(
            &[
                LinearOp::Const { dst: 0, value: id },
                LinearOp::ImpureRandom {
                    dst: 1,
                    id: 0,
                    call_site: 42,
                },
                LinearOp::StoreOutput { src: 1 },
            ],
            &[],
            &[],
            1.0,
            RowEvalContext {
                runtime_state: Some(runtime_state),
                ..Default::default()
            },
        )
        .expect("impure random draw row")
    };

    let runtime_state = SimulationRuntimeState::new();
    let first_id = eval_row(&init, &[], &[], 0.0, None).expect("first init row");
    let first_draw = draw(&runtime_state, first_id);
    let _advanced_draw = draw(&runtime_state, first_id);

    runtime_state.clear();
    let second_id = eval_row(&init, &[], &[], 0.0, None).expect("second init row");
    let second_draw = draw(&runtime_state, second_id);

    assert_eq!(first_id, second_id);
    assert_eq!(first_draw, second_draw);
}

#[test]
fn simulation_runtime_snapshot_restores_impure_random_continuation() {
    let runtime_state = SimulationRuntimeState::new();
    let id = impure_random_stream_id(17);
    let draw = |time| impure_random_sample(id, 9, time, &runtime_state.impure_random);
    let _first = draw(1.0);
    let saved = runtime_state.snapshot();
    let expected = draw(2.0);
    let _later = draw(3.0);

    runtime_state.restore(&saved);

    assert_eq!(draw(2.0).to_bits(), expected.to_bits());
}

#[test]
fn row_eval_context_keeps_impure_random_state_model_local() {
    let init = vec![
        LinearOp::Const {
            dst: 0,
            value: 67867967.0,
        },
        LinearOp::ImpureRandomInit { dst: 1, seed: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];
    let draw_row = |id| {
        vec![
            LinearOp::Const { dst: 0, value: id },
            LinearOp::ImpureRandom {
                dst: 1,
                id: 0,
                call_site: 42,
            },
            LinearOp::StoreOutput { src: 1 },
        ]
    };
    let draw_at = |runtime_state, id, t| {
        eval_row_with_context(
            &draw_row(id),
            &[],
            &[],
            t,
            RowEvalContext {
                runtime_state: Some(runtime_state),
                ..Default::default()
            },
        )
        .expect("impure random draw row")
    };

    let state_a = SimulationRuntimeState::new();
    let state_b = SimulationRuntimeState::new();
    let id = eval_row(&init, &[], &[], 0.0, None).expect("impure random init row");

    let a1 = draw_at(&state_a, id, 1.0);
    clear_runtime_state();
    let a2 = draw_at(&state_a, id, 2.0);

    let b1 = draw_at(&state_b, id, 1.0);
    let b2 = draw_at(&state_b, id, 2.0);

    assert_eq!(a1, b1);
    assert_eq!(a2, b2);
}

#[test]
fn random_opcode_helper_rejects_non_random_op() {
    let op = LinearOp::Const { dst: 0, value: 1.0 };
    let mut regs = vec![0.0];
    let mut initialized = vec![false];

    let err = apply_random_op(
        &mut regs,
        &mut initialized,
        &op,
        0.0,
        RowEvalContext::default(),
        None,
    )
    .expect_err("random helper should reject non-random op");

    assert!(matches!(
        err,
        EvalSolveError::InvalidLinearOp {
            helper: "random",
            op: "Const",
        }
    ));
}
