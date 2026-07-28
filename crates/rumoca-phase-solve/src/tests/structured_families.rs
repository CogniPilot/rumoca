use super::*;

#[test]
fn solve_problem_lowers_structured_continuous_residual_to_map() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("z"), source_array_var("z", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("w"), source_array_var("w", &[3]));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "state derivative",
    ));
    for index in 1..=3 {
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Sub,
                source_indexed_var(
                    "z",
                    vec![rumoca_core::Subscript::generated_index(index, span)],
                ),
                source_indexed_var(
                    "w",
                    vec![rumoca_core::Subscript::generated_index(index, span)],
                ),
            ),
            span,
            "structured z=w residual",
        ));
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 1,
            equations_per_point: 1,
            span,
            origin: "structured z=w residual".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        });

    let problem = lower_solve_problem(&dae_model).expect("structured residual should lower");
    let report = tensor_preservation_report(&dae_model, &problem)
        .expect("tensor preservation report should inspect compact metadata");

    assert_eq!(problem.continuous.residual.len(), Ok(3));
    assert_eq!(report.compact_family_count, 1);
    assert_eq!(report.compact_domain_points, 3);
    assert_eq!(report.preserved_family_bodies, 1);
    assert_eq!(report.scalarized_family_rows, 0);
    assert!(report.fallbacks.is_empty());
    assert!(
        matches!(
            problem.continuous.residual.nodes.as_slice(),
            [solve::ComputeNode::Map { .. }]
        ),
        "unexpected residual nodes: {:?}",
        problem.continuous.residual.nodes
    );
    let residual_rows = scalar_program_block_fixture(&problem.continuous.residual);
    assert_eq!(residual_rows.programs.len(), 3);
    assert_eq!(residual_rows.output_indices, vec![0, 1, 2]);
    assert!(matches!(
        problem.continuous.implicit_rhs.nodes.as_slice(),
        [
            solve::ComputeNode::ScalarPrograms(_),
            solve::ComputeNode::Map {
                output_map: solve::TensorOutputMap { start: 1, .. },
                ..
            },
            solve::ComputeNode::ScalarPrograms(_)
        ]
    ));
    let implicit_rows = scalar_program_block_fixture(&problem.continuous.implicit_rhs);
    assert_eq!(implicit_rows.output_indices, vec![0, 1, 2, 3, 4, 5, 6]);
}

#[test]
fn solve_problem_lowers_direct_derivative_family_to_one_map() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_array_var("x", &[4]));
    for index in 1..=4 {
        let subscript = rumoca_core::Subscript::generated_index(index, span);
        let state = source_indexed_var("x", vec![subscript]);
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Sub,
                der(state.clone()),
                rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Minus,
                    rhs: Box::new(state),
                    span,
                },
            ),
            span,
            "structured state derivative",
        ));
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 4,
                    step: 1,
                }],
            },
            first_equation_index: 0,
            equations_per_point: 1,
            span,
            origin: "structured state derivative".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        });

    let problem = lower_solve_problem(&dae_model).expect("derivative family should lower");
    assert!(
        matches!(
            problem.continuous.derivative_rhs.nodes.as_slice(),
            [solve::ComputeNode::Map { .. }]
        ),
        "direct derivative family should lower to one compact node: {:?}",
        problem.continuous.derivative_rhs.nodes
    );
    let report = tensor_preservation_report(&dae_model, &problem)
        .expect("derivative family report should remain compact");
    assert_eq!(report.preserved_family_bodies, 1);
    assert_eq!(report.scalarized_family_rows, 0);
}

#[test]
fn derivative_system_row_order_removal_preserves_interleaved_structured_family() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_array_var("x", &[2]));
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("y"), source_array_var("y", &[2]));
    for index in 1..=2 {
        for base in ["x", "y"] {
            let state = source_indexed_var(
                base,
                vec![rumoca_core::Subscript::generated_index(index, span)],
            );
            dae_model.continuous.equations.push(dae::Equation::residual(
                binary(
                    rumoca_core::OpBinary::Sub,
                    der(state.clone()),
                    rumoca_core::Expression::Unary {
                        op: rumoca_core::OpUnary::Minus,
                        rhs: Box::new(state),
                        span,
                    },
                ),
                span,
                "interleaved structured derivative body",
            ));
        }
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 2,
                    step: 1,
                }],
            },
            first_equation_index: 0,
            equations_per_point: 2,
            span,
            origin: "interleaved structured derivative body".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        });

    let model = lower_dae_to_solve_model_owned(dae_model.clone())
        .expect("runtime lowering must preserve the original structured row family");
    assert!(
        matches!(
            model.problem.continuous.derivative_rhs.nodes.as_slice(),
            [
                solve::ComputeNode::Map { .. },
                solve::ComputeNode::Map { .. }
            ]
        ),
        "both structured derivative bodies should remain native: {:?}",
        model.problem.continuous.derivative_rhs.nodes
    );
    let report = tensor_preservation_report(&dae_model, &model.problem)
        .expect("structured derivative report should retain source family metadata");
    assert_eq!(report.preserved_family_bodies, 2);
    assert_eq!(report.scalarized_family_bodies, 0);
    assert_eq!(report.scalarized_family_rows, 0);
    assert!(report.fallbacks.is_empty());
}

#[test]
fn solve_problem_lowers_aggregate_array_template_directly_to_map() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("z"), source_array_var("z", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("w"), source_array_var("w", &[3]));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "state derivative",
    ));
    let aggregate_residual = binary(rumoca_core::OpBinary::Sub, source_var("z"), source_var("w"));
    let mut aggregate_equation =
        dae::Equation::residual(aggregate_residual.clone(), span, "aggregate z=w residual");
    aggregate_equation.scalar_count = 3;
    dae_model.continuous.equations.push(aggregate_equation);
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "__array_i1".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 1,
            equations_per_point: 1,
            span,
            origin: "aggregate z=w residual".to_string(),
            regular: None,
            template: Some(rumoca_core::ComprehensionTemplate {
                body: vec![aggregate_residual],
                scalar_view: rumoca_core::ComprehensionScalarView::RowMajorProjection,
            }),
            interiors_materialized: true,
        });

    let problem = lower_solve_problem(&dae_model).expect("aggregate array residual should lower");

    assert_eq!(problem.continuous.residual.len(), Ok(3));
    assert!(
        matches!(
            problem.continuous.residual.nodes.as_slice(),
            [solve::ComputeNode::Map { .. }]
        ),
        "unexpected aggregate residual nodes: {:?}",
        problem.continuous.residual.nodes
    );
}

#[test]
fn solve_problem_lowers_structured_continuous_residual_with_scalar_math_to_map() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("z"), source_array_var("z", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("w"), source_array_var("w", &[3]));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("theta"), scalar_var("theta"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "state derivative",
    ));
    for index in 1..=3 {
        let subscript = rumoca_core::Subscript::generated_index(index, span);
        let trig_scaled_source = binary(
            rumoca_core::OpBinary::Add,
            binary(
                rumoca_core::OpBinary::Mul,
                builtin_call(rumoca_core::BuiltinFunction::Sin, vec![var("theta")]),
                source_indexed_var("w", vec![subscript.clone()]),
            ),
            builtin_call(rumoca_core::BuiltinFunction::Cos, vec![var("theta")]),
        );
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Sub,
                source_indexed_var("z", vec![subscript]),
                trig_scaled_source,
            ),
            span,
            "structured trig residual",
        ));
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 1,
            equations_per_point: 1,
            span,
            origin: "structured trig residual".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        });

    let problem = lower_solve_problem(&dae_model).expect("structured trig residual should lower");

    assert_eq!(problem.continuous.residual.len(), Ok(3));
    assert!(matches!(
        problem.continuous.residual.nodes.as_slice(),
        [solve::ComputeNode::Map { .. }]
    ));
}

#[test]
fn solve_problem_lowers_structured_continuous_residual_with_guard_to_map() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("z"), source_array_var("z", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("w"), source_array_var("w", &[3]));
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("mask"),
        source_array_var("mask", &[3]),
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("fallback"),
        scalar_var("fallback"),
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "state derivative",
    ));
    for index in 1..=3 {
        let subscript = rumoca_core::Subscript::generated_index(index, span);
        let guarded_source = rumoca_core::Expression::If {
            branches: vec![(
                binary(
                    rumoca_core::OpBinary::Gt,
                    source_indexed_var("mask", vec![subscript.clone()]),
                    int_expr(0),
                ),
                source_indexed_var("w", vec![subscript.clone()]),
            )],
            else_branch: Box::new(var("fallback")),
            span,
        };
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Sub,
                source_indexed_var("z", vec![subscript]),
                guarded_source,
            ),
            span,
            "structured guarded residual",
        ));
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 1,
            equations_per_point: 1,
            span,
            origin: "structured guarded residual".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        });

    let problem =
        lower_solve_problem(&dae_model).expect("structured guarded residual should lower");

    assert_eq!(problem.continuous.residual.len(), Ok(3));
    assert!(matches!(
        problem.continuous.residual.nodes.as_slice(),
        [solve::ComputeNode::Map { .. }]
    ));
}

/// A three-point family whose middle body is a different expression shape, so
/// the stencil has no single kernel to lift.
fn non_uniform_body_dae() -> dae::Dae {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("z"), source_array_var("z", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("w"), source_array_var("w", &[3]));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "state derivative",
    ));
    for index in 1..=3 {
        let z = source_indexed_var(
            "z",
            vec![rumoca_core::Subscript::generated_index(index, span)],
        );
        let w = source_indexed_var(
            "w",
            vec![rumoca_core::Subscript::generated_index(index, span)],
        );
        // The middle cell reads `w[2] + 1` where its neighbours read `w[i]`.
        let rhs = if index == 2 {
            binary(rumoca_core::OpBinary::Add, w, int_expr(1))
        } else {
            w
        };
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(rumoca_core::OpBinary::Sub, z, rhs),
            span,
            "non-uniform structured residual",
        ));
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 1,
            equations_per_point: 1,
            span,
            origin: "non-uniform structured residual".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        });
    dae_model
}

#[test]
fn tensor_report_names_the_declining_branch_end_to_end() {
    let dae_model = non_uniform_body_dae();

    let (problem, declines) = crate::lower_solve_problem_with_tensor_declines(&dae_model)
        .expect("non-uniform structured residual should lower");
    let report = tensor_preservation_report_with_declines(&dae_model, &problem, &declines)
        .expect("attributed report should inspect compact metadata");

    assert_eq!(report.preserved_family_bodies, 0);
    assert_eq!(report.scalarized_family_bodies, 1);
    let [fallback] = report.fallbacks.as_slice() else {
        panic!("expected exactly one fallback: {:?}", report.fallbacks);
    };
    assert_eq!(fallback.reason.code(), "solve:mismatched-dae-body-shape");
    assert_eq!(
        fallback.reason.headroom(),
        crate::TensorHeadroom::CorrectlyScalar
    );
    assert_eq!(fallback.scalarized_bodies, 1);
    assert_eq!(fallback.scalarized_rows, 3);

    let counts = report.fallback_counts();
    let [count] = counts.as_slice() else {
        panic!("expected one per-reason count: {counts:?}");
    };
    assert_eq!(count.code(), "solve:mismatched-dae-body-shape");
    assert_eq!(count.families, 1);
    assert_eq!(count.scalarized_bodies, 1);
    assert_eq!(count.scalarized_rows, 3);
}

#[test]
fn tensor_report_without_a_journal_reports_the_unattributed_residue() {
    let dae_model = non_uniform_body_dae();

    let problem =
        lower_solve_problem(&dae_model).expect("non-uniform structured residual should lower");
    let report = tensor_preservation_report(&dae_model, &problem)
        .expect("unattributed report should inspect compact metadata");

    let [fallback] = report.fallbacks.as_slice() else {
        panic!("expected exactly one fallback: {:?}", report.fallbacks);
    };
    assert_eq!(fallback.reason.code(), "solve:incomplete-tensor-coverage");
    assert_eq!(
        fallback.reason.headroom(),
        crate::TensorHeadroom::Unknown,
        "an unattributed residue carries no evidence either way"
    );
    assert_eq!(
        report.provenance,
        crate::TensorReportProvenance::UnverifiedDae,
        "without a journal the report cannot prove which DAE it measured"
    );
}

#[test]
fn tensor_report_measures_the_dae_lowering_consumed() {
    let dae_model = non_uniform_body_dae();

    let (problem, declines) = crate::lower_solve_problem_with_tensor_declines(&dae_model)
        .expect("non-uniform structured residual should lower");

    // The journal carries the family list lowering consumed, so this entry
    // point cannot be handed a second, disagreeing DAE at all.
    let from_lowering = crate::tensor_preservation_report_from_lowering(&problem, &declines)
        .expect("the journal records the families lowering consumed");
    assert_eq!(
        from_lowering.provenance,
        crate::TensorReportProvenance::LoweredDae
    );

    // Here the caller's DAE IS the one lowering consumed, so the DAE-taking
    // entry point verifies it and produces the identical measurement.
    let with_dae = tensor_preservation_report_with_declines(&dae_model, &problem, &declines)
        .expect("attributed report should inspect compact metadata");
    assert_eq!(from_lowering, with_dae);

    // A DAE whose family list differs is rejected: family attribution is
    // positional, so measuring it would name the wrong family in every row.
    let mut other_dae = dae_model.clone();
    other_dae.continuous.structured_equations.clear();
    let error = tensor_preservation_report_with_declines(&other_dae, &problem, &declines)
        .expect_err("a DAE that is not the lowered one must not be measured");
    assert!(error.reason().contains("structured families"), "{error:?}");
}

/// A compact structured family in the INITIALIZATION residual is lowered to a
/// direct tensor node addressing absolute residual outputs. The scalar-program
/// node holding the remaining rows addresses absolute outputs too, but a block
/// whose stored indices happen to be `0..n` is indistinguishable from the
/// local-contiguous form `ComputeBlock` re-bases onto the running output cursor.
/// It must therefore stay the block's first node: assembling the tensor nodes
/// ahead of it shifted every scalar row past the end of the residual, and the
/// Solve-IR shape contract rejected the whole problem.
#[test]
fn initialization_residual_block_length_matches_its_row_targets() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("y"), source_scalar_var("y"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("z"), source_array_var("z", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("w"), source_array_var("w", &[3]));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "state derivative",
    ));
    // Residual row 0 of the initialization system: a plain scalar row, so its
    // scalar-program block stores exactly the index `0`.
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, source_var("y"), int_expr(1)),
        span,
        "scalar y residual",
    ));
    let aggregate_residual = binary(rumoca_core::OpBinary::Sub, source_var("z"), source_var("w"));
    let mut aggregate_equation =
        dae::Equation::residual(aggregate_residual.clone(), span, "aggregate z=w residual");
    aggregate_equation.scalar_count = 3;
    dae_model.continuous.equations.push(aggregate_equation);
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "__array_i1".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 2,
            equations_per_point: 1,
            span,
            origin: "aggregate z=w residual".to_string(),
            regular: None,
            template: Some(rumoca_core::ComprehensionTemplate {
                body: vec![aggregate_residual],
                scalar_view: rumoca_core::ComprehensionScalarView::RowMajorProjection,
            }),
            interiors_materialized: true,
        });

    let problem =
        lower_solve_problem(&dae_model).expect("mixed scalar/compact initialization should lower");

    assert_eq!(problem.initialization.row_targets.len(), 4);
    assert_eq!(problem.initialization.residual.len(), Ok(4));
    problem
        .validate_shape_contract()
        .expect("initialization residual must satisfy the Solve-IR shape contract");
}
