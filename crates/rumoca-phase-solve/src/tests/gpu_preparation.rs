use super::*;

fn gpu_indexed_var(name: &str, index: i64, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: source_ref(name),
        subscripts: vec![rumoca_core::Subscript::generated_index(index, span)],
        span,
    }
}

fn gpu_initial_family_fixture(values: &[i64], spans: &[rumoca_core::Span]) -> dae::Dae {
    assert_eq!(values.len(), spans.len());
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        array_var("x", &[values.len() as i64]),
    );
    for (ordinal, (&value, &span)) in values.iter().zip(spans).enumerate() {
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Sub,
                der(gpu_indexed_var("x", ordinal as i64 + 1, span)),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span,
                },
            ),
            span,
            "derivative row",
        ));
        dae_model
            .initialization
            .equations
            .push(dae::Equation::residual(
                binary(
                    rumoca_core::OpBinary::Sub,
                    gpu_indexed_var("x", ordinal as i64 + 1, span),
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(value),
                        span,
                    },
                ),
                span,
                "structured initial row",
            ));
        dae_model
            .initialization
            .equation_provenance
            .push(dae::InitializationEquationProvenance::User);
    }
    let template = dae_model.initialization.equations[0].rhs.clone();
    dae_model
        .initialization
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: values.len() as i64,
                    step: 1,
                }],
            },
            first_equation_index: 0,
            equation_counts: vec![1; values.len()],
            span: spans[0],
            origin: "structured initial fixture".to_string(),
            regular: Some(rumoca_core::RegularForFamily {
                binders: vec!["i".to_string()],
                accesses: Vec::new(),
            }),
            template: Some(rumoca_core::ComprehensionTemplate {
                body: vec![template],
            }),
            interiors_materialized: true,
        });
    dae_model
}

#[test]
fn gpu_preparation_inlines_input_driven_algebraic_in_derivative_rhs() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("mask"), scalar_var("mask"));
    dae_model
        .variables
        .inputs
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), var("mask")),
        span,
        "state derivative reads derived field",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            var("mask"),
            binary(rumoca_core::OpBinary::Add, var("u"), int_expr(1)),
        ),
        span,
        "input-derived explicit field",
    ));

    let runtime = lower_solve_problem(&dae_model).expect("runtime lowering should succeed");
    let runtime_mask_y = match runtime.layout.binding("mask") {
        Some(solve::ScalarSlot::Y { index, .. }) => index,
        other => panic!("mask should be a retained runtime algebraic Y slot: {other:?}"),
    };
    let runtime_rhs = scalar_program_block_fixture(&runtime.continuous.derivative_rhs);
    assert!(
        runtime_rhs.programs[0].iter().any(
            |op| matches!(op, solve::LinearOp::LoadY { index, .. } if *index == runtime_mask_y)
        ),
        "{:?}",
        runtime_rhs.programs[0]
    );

    let gpu = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        1,
        Some(span),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect("GPU-preparation lowering should succeed");
    let gpu_u_p = match gpu.layout.binding("u") {
        Some(solve::ScalarSlot::P { index, .. }) => index,
        other => panic!("input u should be a P slot: {other:?}"),
    };
    assert_eq!(gpu.layout.y_scalars(), 1);
    assert_eq!(gpu.layout.binding("mask"), None);
    assert!(gpu.continuous.residual.is_empty());
    assert!(gpu.continuous.algebraic_projection_plan.blocks.is_empty());
    let gpu_rhs = scalar_program_block_fixture(&gpu.continuous.derivative_rhs);
    assert!(
        gpu_rhs.programs[0]
            .iter()
            .any(|op| matches!(op, solve::LinearOp::LoadP { index, .. } if *index == gpu_u_p)),
        "{:?}",
        gpu_rhs.programs[0]
    );
    assert!(
        !gpu_rhs.programs[0]
            .iter()
            .any(|op| matches!(op, solve::LinearOp::LoadY { .. })),
        "{:?}",
        gpu_rhs.programs[0]
    );
}

#[test]
fn gpu_preparation_rejects_nonstructured_initial_assignment_shape() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "der(x) = 0",
    ));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            binary(rumoca_core::OpBinary::Sub, var("x"), int_expr(7)),
            span,
            "x = 7",
        ));

    let gpu = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        1,
        Some(span),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect_err("GPU preparation must fail closed instead of scalarizing initialization rows");
    assert!(matches!(
        &gpu,
        crate::lower::LowerError::UnsupportedAt { .. }
    ));
    assert_eq!(gpu.source_span(), Some(span));
}

#[test]
fn gpu_preparation_ignores_automatic_fixed_start_rows() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "der(x) = 0",
    ));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            binary(rumoca_core::OpBinary::Sub, var("x"), int_expr(7)),
            span,
            "fixed start initialization for x",
        ));
    dae_model
        .initialization
        .equation_provenance
        .push(dae::InitializationEquationProvenance::FixedStart);

    let gpu = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        1,
        Some(span),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect("GPU preparation should retain declared fixed starts without scalar initialization");
    assert!(gpu.initialization.residual.is_empty());
    assert!(gpu.initialization.direct_families.is_empty());
    assert!(gpu.initialization.row_targets.is_empty());
}

#[test]
fn gpu_preparation_rejects_partial_fixed_start_target_coverage() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    for name in ["x", "y"] {
        dae_model
            .variables
            .states
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(rumoca_core::OpBinary::Sub, der(var(name)), int_expr(0)),
            span,
            "derivative",
        ));
    }
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            binary(rumoca_core::OpBinary::Sub, var("x"), int_expr(7)),
            span,
            "fixed start initialization for x",
        ));
    dae_model
        .initialization
        .equation_provenance
        .push(dae::InitializationEquationProvenance::FixedStart);

    let error = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        2,
        Some(span),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect_err("partial GPU initialization coverage must fail closed");
    assert!(error.to_string().contains("cover every solver Y slot"));
}

#[test]
fn gpu_preparation_rejects_overlapping_fixed_start_targets_at_conflicting_span() {
    let first_span = solve_numbered_span(301, 10, 20);
    let conflicting_span = solve_numbered_span(301, 30, 40);
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        first_span,
        "derivative",
    ));
    for span in [first_span, conflicting_span] {
        dae_model
            .initialization
            .equations
            .push(dae::Equation::residual(
                binary(rumoca_core::OpBinary::Sub, var("x"), int_expr(7)),
                span,
                "fixed start initialization for x",
            ));
        dae_model
            .initialization
            .equation_provenance
            .push(dae::InitializationEquationProvenance::FixedStart);
    }

    let error = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        1,
        Some(first_span),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect_err("overlapping fixed-start ownership must fail closed");
    assert!(error.to_string().contains("overlap"));
    assert_eq!(error.source_span(), Some(conflicting_span));
}

#[test]
fn gpu_preparation_emits_one_compact_fixed_range_for_array_target() {
    let span = solve_numbered_span(302, 10, 20);
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), array_var("x", &[128]));
    let mut derivative = dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "array derivative",
    );
    derivative.scalar_count = 128;
    dae_model.continuous.equations.push(derivative);
    let mut fixed = dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, var("x"), int_expr(7)),
        span,
        "fixed array start",
    );
    fixed.scalar_count = 128;
    dae_model.initialization.equations.push(fixed);
    dae_model
        .initialization
        .equation_provenance
        .push(dae::InitializationEquationProvenance::FixedStart);

    let gpu = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        128,
        Some(span),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect("fixed array target should lower as one affine range");
    assert_eq!(gpu.initialization.fixed_target_ranges.len(), 1);
    assert_eq!(gpu.initialization.fixed_target_ranges[0].start, 0);
    assert_eq!(gpu.initialization.fixed_target_ranges[0].end, 128);
}

#[test]
fn gpu_phase_range_validation_rejects_direct_fixed_overlap_at_later_span() {
    let first_span = solve_numbered_span(303, 10, 20);
    let conflicting_span = solve_numbered_span(303, 30, 40);
    let error = crate::gpu_initialization::normalize_gpu_target_ranges(
        vec![
            solve::InitializationTargetRange {
                start: 0,
                end: 2,
                span: Some(first_span),
            },
            solve::InitializationTargetRange {
                start: 1,
                end: 3,
                span: Some(conflicting_span),
            },
        ],
        3,
    )
    .expect_err("phase validation must reject direct/fixed ownership overlap");
    assert!(error.to_string().contains("overlap"));
    assert_eq!(error.source_span(), Some(conflicting_span));
}

#[test]
fn gpu_phase_range_validation_merges_adjacency_only() {
    let span = solve_numbered_span(304, 10, 20);
    let normalized = crate::gpu_initialization::normalize_gpu_target_ranges(
        vec![
            solve::InitializationTargetRange {
                start: 0,
                end: 1,
                span: Some(span),
            },
            solve::InitializationTargetRange {
                start: 1,
                end: 3,
                span: Some(span),
            },
        ],
        3,
    )
    .expect("adjacent phase ranges should merge");
    assert_eq!(normalized.len(), 1);
    assert_eq!((normalized[0].start, normalized[0].end), (0, 3));
}

#[test]
fn gpu_initial_projection_rejects_degenerate_structured_binder() {
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 1,
            step: 1,
        }],
    };

    let error = gpu_corner_cell_index(&domain, 0, solve_test_span())
        .expect_err("direct GPU initial projection must fail closed for one-cell binders");
    assert!(matches!(
        error,
        crate::lower::LowerError::UnsupportedAt { .. }
    ));
    assert!(
        error
            .to_string()
            .contains("non-degenerate structured binder")
    );
}

#[test]
fn gpu_initial_projection_handles_negative_binder_steps() {
    // Source binder traversal may descend. Target maps remain canonical dense
    // positive-stride maps; Solve-IR validation rejects negative target strides.
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 3,
            upper: 1,
            step: -1,
        }],
    };

    assert_eq!(gpu_corner_cell_index(&domain, 0, solve_test_span()), Ok(1));
}

#[test]
fn gpu_initial_uniformity_checks_destinations_nonloads_and_load_p() {
    let span = solve_test_span();
    let base = vec![
        solve::LinearOp::LoadP { dst: 0, index: 4 },
        solve::LinearOp::Const { dst: 1, value: 2.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ];
    let mut corner = base.clone();
    corner[0] = solve::LinearOp::LoadP { dst: 0, index: 7 };
    let mut loads = Vec::new();
    let mut constants = Vec::new();
    append_gpu_corner_strides(&base, &corner, 0, &mut loads, &mut constants, span)
        .expect("LoadP may vary affinely");
    assert_eq!(loads[0].terms[0].stride, 3);

    corner[2] = solve::LinearOp::Binary {
        dst: 3,
        op: solve::BinaryOp::Add,
        lhs: 0,
        rhs: 1,
    };
    append_gpu_corner_strides(&base, &corner, 0, &mut Vec::new(), &mut Vec::new(), span)
        .expect_err("destination register drift must fail closed");
}

#[test]
fn gpu_initial_projection_rejects_nonaffine_three_cell_constants_at_first_bad_row() {
    let spans = [
        solve_numbered_span(305, 10, 20),
        solve_numbered_span(305, 30, 40),
        solve_numbered_span(305, 50, 60),
    ];
    let dae_model = gpu_initial_family_fixture(&[1, 4, 9], &spans);

    let error = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        3,
        Some(spans[0]),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect_err("non-affine constants must not be synthesized as 1, 4, 7");

    assert!(error.to_string().contains("affine"), "{error}");
    assert_eq!(error.source_span(), Some(spans[2]));
}

#[test]
fn gpu_initial_metadata_failures_preserve_family_and_equation_spans() {
    let spans = [
        solve_numbered_span(306, 10, 20),
        solve_numbered_span(306, 30, 40),
        solve_numbered_span(306, 50, 60),
    ];
    for missing_regular in [true, false] {
        let mut dae_model = gpu_initial_family_fixture(&[1, 2, 3], &spans);
        let family = &mut dae_model.initialization.structured_equations[0];
        if missing_regular {
            family.regular = None;
        } else {
            family.template = None;
        }
        let error = lower_solve_problem_with_solver_len_and_model_span_and_profile(
            &dae_model,
            3,
            Some(spans[0]),
            SolveProblemLoweringProfile::GpuPreparation,
        )
        .expect_err("missing structured metadata must fail closed");
        assert_eq!(error.source_span(), Some(spans[0]));
    }

    let mut dae_model = gpu_initial_family_fixture(&[1, 2, 3], &spans);
    dae_model.initialization.equation_provenance.pop();
    let error = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        3,
        Some(spans[0]),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect_err("missing equation provenance must fail closed");
    assert_eq!(error.source_span(), Some(spans[2]));
}

#[test]
fn gpu_initial_coverage_failure_points_to_first_uncovered_user_equation() {
    let spans = [
        solve_numbered_span(307, 10, 20),
        solve_numbered_span(307, 30, 40),
        solve_numbered_span(307, 50, 60),
    ];
    let uncovered_span = solve_numbered_span(307, 70, 80);
    let mut dae_model = gpu_initial_family_fixture(&[1, 2, 3], &spans);
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Sub,
                gpu_indexed_var("x", 3, uncovered_span),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(4),
                    span: uncovered_span,
                },
            ),
            uncovered_span,
            "uncovered initial row",
        ));
    dae_model
        .initialization
        .equation_provenance
        .push(dae::InitializationEquationProvenance::User);

    let error = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        3,
        Some(spans[0]),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect_err("mixed structured and scalar initial rows must fail closed");
    assert_eq!(error.source_span(), Some(uncovered_span));
}

#[test]
fn gpu_initial_direct_and_body_shape_failures_preserve_user_spans() {
    let spans = [
        solve_numbered_span(308, 10, 20),
        solve_numbered_span(308, 30, 40),
        solve_numbered_span(308, 50, 60),
    ];
    let mut nondirect = gpu_initial_family_fixture(&[1, 2, 3], &spans);
    nondirect.initialization.equations[0].rhs = binary(
        rumoca_core::OpBinary::Add,
        gpu_indexed_var("x", 1, spans[0]),
        int_expr(1),
    );
    let error = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &nondirect,
        3,
        Some(spans[0]),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect_err("non-direct base row must fail closed");
    assert_eq!(error.source_span(), Some(spans[0]));

    let mut mismatched = gpu_initial_family_fixture(&[1, 2, 3], &spans);
    mismatched.initialization.equations[2].rhs = binary(
        rumoca_core::OpBinary::Sub,
        gpu_indexed_var("x", 3, spans[2]),
        binary(rumoca_core::OpBinary::Add, int_expr(4), int_expr(5)),
    );
    let error = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &mismatched,
        3,
        Some(spans[0]),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect_err("body-shape mismatch must fail closed");
    assert_eq!(error.source_span(), Some(spans[2]));

    let mut corner = vec![solve::LinearOp::Const { dst: 0, value: 1.0 }];
    corner.push(solve::LinearOp::StoreOutput { src: 0 });
    let error = append_gpu_corner_strides(
        &[solve::LinearOp::Const { dst: 0, value: 1.0 }],
        &corner,
        0,
        &mut Vec::new(),
        &mut Vec::new(),
        spans[1],
    )
    .expect_err("operation-shape mismatch must fail closed");
    assert_eq!(error.source_span(), Some(spans[1]));
}

#[test]
fn gpu_initial_lowering_rejects_random_operations_with_source_span() {
    let span = solve_numbered_span(309, 10, 20);
    let error = reject_nondeterministic_gpu_initial_ops(
        &[solve::LinearOp::ImpureRandomInit { dst: 1, seed: 0 }],
        span,
    )
    .expect_err("GPU initialization lowering must reject non-replayable operations");
    assert!(error.to_string().contains("random or impure"));
    assert_eq!(error.source_span(), Some(span));
}
