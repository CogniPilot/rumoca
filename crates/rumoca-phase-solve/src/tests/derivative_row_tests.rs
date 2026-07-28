use super::*;

#[test]
fn solve_problem_partitions_derivative_rows_by_ir_not_position() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("a"), scalar_var("a"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("b"), scalar_var("b"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            var("a"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span,
            },
        ),
        span,
        "algebraic row before derivative row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), var("a")),
        span,
        // MLS Appendix B B.1a: continuous equations are simultaneous; a
        // state derivative row is not required to be first in `f_x`.
        "state derivative row after algebraic row",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, var("b"), var("x")),
        span,
        "algebraic row after derivative row",
    ));

    let problem = lower_solve_problem(&dae_model).expect("unordered f_x should lower");

    let drhs = scalar_program_block_fixture(&problem.continuous.derivative_rhs);
    let rhs = scalar_program_block_fixture(&problem.continuous.implicit_rhs);
    assert_eq!(drhs.programs.len(), 1);
    assert_eq!(problem.continuous.residual.len(), Ok(2));
    assert_eq!(rhs.programs.len(), 3);
    assert_ne!(rhs.programs[0], zero_rhs_row());
    assert_ne!(rhs.programs[1], zero_rhs_row());
    assert_ne!(rhs.programs[2], zero_rhs_row());
    assert!(matches!(
        problem.continuous.implicit_row_targets[0],
        Some(solve::ScalarSlot::Y { index: 0, .. })
    ));
    assert!(matches!(
        problem.continuous.implicit_row_targets[1],
        Some(solve::ScalarSlot::Y { index: 1, .. })
    ));
    assert!(matches!(
        problem.continuous.implicit_row_targets[2],
        Some(solve::ScalarSlot::Y { index: 2, .. })
    ));
}

#[test]
fn solve_problem_expands_sliced_derivative_rows_with_structural_bounds() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_array_var("x", &[2]));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("nx"),
        dae::Variable {
            name: rumoca_core::VarName::new("nx"),
            start: Some(int_expr(2)),
            fixed: Some(true),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            der(source_indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_index(1, span)],
            )),
            int_expr(0),
        ),
        span,
        "scalar first derivative",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            der(source_indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_expr(
                    Box::new(range_expr(int_expr(2), var("nx"))),
                    span,
                )],
            )),
            source_indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_expr(
                    Box::new(range_expr(
                        int_expr(1),
                        binary(rumoca_core::OpBinary::Sub, var("nx"), int_expr(1)),
                    )),
                    span,
                )],
            ),
        ),
        span,
        // MLS §10.5 permits vector subscripts in expressions. A vectorized
        // derivative equation still defines scalar derivative equations for
        // the selected state components after structural parameters resolve.
        "sliced derivative row",
    ));

    let problem = lower_solve_problem(&dae_model)
        .expect("sliced derivative equations with structural bounds should lower");

    let drhs = scalar_program_block_fixture(&problem.continuous.derivative_rhs);
    assert_eq!(drhs.programs.len(), 2);
}

#[test]
fn solve_problem_expands_descending_sliced_derivative_rows() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), source_array_var("x", &[3]));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            der(source_indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_index(1, span)],
            )),
            int_expr(0),
        ),
        span,
        "scalar first derivative",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            der(source_indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_expr(
                    Box::new(stepped_range_expr(
                        int_expr(3),
                        rumoca_core::Expression::Unary {
                            op: rumoca_core::OpUnary::Minus,
                            rhs: Box::new(int_expr(1)),
                            span,
                        },
                        int_expr(2),
                    )),
                    span,
                )],
            )),
            source_indexed_var(
                "x",
                vec![rumoca_core::Subscript::generated_expr(
                    Box::new(stepped_range_expr(
                        int_expr(2),
                        rumoca_core::Expression::Unary {
                            op: rumoca_core::OpUnary::Minus,
                            rhs: Box::new(int_expr(1)),
                            span,
                        },
                        int_expr(1),
                    )),
                    span,
                )],
            ),
        ),
        span,
        // MLS §10.4 range expressions may use a negative step. Sliced
        // derivative extraction must preserve the selected component order.
        "descending sliced derivative row",
    ));

    let problem = lower_solve_problem(&dae_model)
        .expect("descending sliced derivative equations should lower");

    let drhs = scalar_program_block_fixture(&problem.continuous.derivative_rhs);
    assert_eq!(drhs.programs.len(), 3);
}

fn derivative_system_var(name: &str, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
            name,
        )),
        subscripts: Vec::new(),
        span,
    }
}

fn derivative_system_der(
    expr: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![expr],
        span,
    }
}

fn derivative_system_binary(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn derivative_system_real(value: f64, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    }
}

fn coupled_derivative_system(reverse_equations: bool) -> dae::Dae {
    let first_span = solve_numbered_span(201, 10, 30);
    let second_span = solve_numbered_span(201, 40, 60);
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));

    let first_lhs = derivative_system_binary(
        rumoca_core::OpBinary::Add,
        derivative_system_binary(
            rumoca_core::OpBinary::Mul,
            derivative_system_real(2.0, first_span),
            derivative_system_der(derivative_system_var("x", first_span), first_span),
            first_span,
        ),
        derivative_system_der(derivative_system_var("y", first_span), first_span),
        first_span,
    );
    let first = dae::Equation::residual(
        derivative_system_binary(
            rumoca_core::OpBinary::Sub,
            first_lhs,
            derivative_system_real(5.0, first_span),
            first_span,
        ),
        first_span,
        "2*der(x) + der(y) = 5",
    );
    let second_lhs = derivative_system_binary(
        rumoca_core::OpBinary::Sub,
        derivative_system_der(derivative_system_var("x", second_span), second_span),
        derivative_system_der(derivative_system_var("y", second_span), second_span),
        second_span,
    );
    let second = dae::Equation::residual(
        derivative_system_binary(
            rumoca_core::OpBinary::Sub,
            second_lhs,
            derivative_system_real(1.0, second_span),
            second_span,
        ),
        second_span,
        "der(x) - der(y) = 1",
    );
    dae_model.continuous.equations = if reverse_equations {
        vec![second, first]
    } else {
        vec![first, second]
    };
    dae_model
}

#[test]
fn derivative_system_coupled_rows_are_permutation_invariant_for_all_profiles() {
    for (profile, profile_name) in [(0, "Runtime"), (1, "RuntimeValueOnly"), (2, "GPU")] {
        let mut canonical_programs = None;
        for reverse_equations in [false, true] {
            let dae_model = coupled_derivative_system(reverse_equations);
            let metadata = dae_model.clone();
            let model = match profile {
                0 => lower_dae_to_solve_model_owned(dae_model),
                1 => {
                    lower_dae_to_solve_model_owned_value_only_with_visible_expressions_and_metadata(
                        dae_model,
                        Vec::new(),
                        &metadata,
                    )
                }
                2 => lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata(
                    dae_model, &metadata,
                ),
                _ => unreachable!(),
            }
            .unwrap_or_else(|error| {
                panic!("{profile_name} coupled derivative lowering failed: {error}")
            });
            let block = scalar_program_block_fixture(&model.problem.continuous.derivative_rhs);
            let y = vec![0.0; model.problem.layout.y_scalars()];
            let mut outputs = vec![0.0; 2];
            rumoca_eval_solve::eval_scalar_program_block(
                &block,
                &y,
                &model.parameters,
                0.0,
                None,
                &mut outputs,
            )
            .unwrap_or_else(|error| {
                panic!("{profile_name} coupled derivative evaluation failed: {error}")
            });
            assert!(
                (outputs[0] - 2.0).abs() < 1.0e-12 && (outputs[1] - 1.0).abs() < 1.0e-12,
                "{profile_name} produced {outputs:?}"
            );
            if let Some((programs, output_indices)) = &canonical_programs {
                assert_eq!(
                    &block.programs, programs,
                    "{profile_name} lowering changed when equations were permuted"
                );
                assert_eq!(&block.output_indices, output_indices);
            } else {
                canonical_programs = Some((block.programs, block.output_indices));
            }
        }
    }
}

fn mixed_derivative_algebraic_system(reverse_equations: bool) -> dae::Dae {
    let first_span = solve_numbered_span(202, 10, 20);
    let second_span = solve_numbered_span(202, 30, 50);
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("a"), scalar_var("a"));
    let first = dae::Equation::residual(
        derivative_system_binary(
            rumoca_core::OpBinary::Sub,
            derivative_system_der(derivative_system_var("x", first_span), first_span),
            derivative_system_real(0.0, first_span),
            first_span,
        ),
        first_span,
        "der(x) = 0",
    );
    let second = dae::Equation::residual(
        derivative_system_binary(
            rumoca_core::OpBinary::Sub,
            derivative_system_binary(
                rumoca_core::OpBinary::Add,
                derivative_system_der(derivative_system_var("x", second_span), second_span),
                derivative_system_var("a", second_span),
                second_span,
            ),
            derivative_system_real(1.0, second_span),
            second_span,
        ),
        second_span,
        "der(x) + a = 1",
    );
    dae_model.continuous.equations = if reverse_equations {
        vec![second, first]
    } else {
        vec![first, second]
    };
    dae_model
}

#[test]
fn derivative_system_rejects_extra_mixed_row_without_dropping_it() {
    let mut errors = Vec::new();
    for reverse_equations in [false, true] {
        let dae_model = mixed_derivative_algebraic_system(reverse_equations);
        let Err(error) = crate::lower::analyze_derivative_rhs(&dae_model) else {
            panic!("mixed derivative/algebraic system must not silently select one row");
        };
        assert!(
            matches!(&error, LowerError::UnsupportedAt { .. }),
            "expected typed span-bearing rejection, got {error:?}"
        );
        assert_eq!(error.source_span(), Some(solve_numbered_span(202, 10, 20)));
        assert!(
            error
                .reason()
                .contains("2 derivative-containing equations for 1 state derivatives"),
            "unexpected error: {error}"
        );
        errors.push(error);
    }
    assert_eq!(
        errors[0], errors[1],
        "typed rejection must not depend on equation encounter order"
    );
}

fn singular_derivative_system(zero_row: bool) -> dae::Dae {
    let first_span = solve_numbered_span(203, 10, 20);
    let second_span = solve_numbered_span(203, 30, 40);
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));

    if zero_row {
        dae_model.continuous.equations.push(dae::Equation::residual(
            derivative_system_binary(
                rumoca_core::OpBinary::Sub,
                derivative_system_der(derivative_system_var("x", first_span), first_span),
                derivative_system_real(1.0, first_span),
                first_span,
            ),
            first_span,
            "der(x) = 1",
        ));
        let zero_derivative = derivative_system_binary(
            rumoca_core::OpBinary::Mul,
            derivative_system_real(0.0, second_span),
            derivative_system_der(derivative_system_var("y", second_span), second_span),
            second_span,
        );
        dae_model.continuous.equations.push(dae::Equation::residual(
            derivative_system_binary(
                rumoca_core::OpBinary::Sub,
                zero_derivative,
                derivative_system_real(0.0, second_span),
                second_span,
            ),
            second_span,
            "0*der(y) = 0",
        ));
        return dae_model;
    }

    for (span, rhs_value) in [(first_span, 1.0), (second_span, 2.0)] {
        let derivative_sum = derivative_system_binary(
            rumoca_core::OpBinary::Add,
            derivative_system_binary(
                rumoca_core::OpBinary::Mul,
                derivative_system_real(1.0, span),
                derivative_system_der(derivative_system_var("x", span), span),
                span,
            ),
            derivative_system_binary(
                rumoca_core::OpBinary::Mul,
                derivative_system_real(1.0, span),
                derivative_system_der(derivative_system_var("y", span), span),
                span,
            ),
            span,
        );
        dae_model.continuous.equations.push(dae::Equation::residual(
            derivative_system_binary(
                rumoca_core::OpBinary::Sub,
                derivative_sum,
                derivative_system_real(rhs_value, span),
                span,
            ),
            span,
            "duplicate constant derivative row",
        ));
    }
    dae_model
}

#[test]
fn derivative_system_rejects_non_finite_constant_coefficient_with_span() {
    let span = solve_numbered_span(204, 10, 30);
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    let scaled_derivative = derivative_system_binary(
        rumoca_core::OpBinary::Mul,
        derivative_system_real(f64::INFINITY, span),
        derivative_system_der(derivative_system_var("x", span), span),
        span,
    );
    dae_model.continuous.equations.push(dae::Equation::residual(
        derivative_system_binary(
            rumoca_core::OpBinary::Sub,
            scaled_derivative,
            derivative_system_real(0.0, span),
            span,
        ),
        span,
        "non-finite derivative coefficient",
    ));

    let Err(error) = crate::lower::analyze_derivative_rhs(&dae_model) else {
        panic!("non-finite compile-time derivative coefficients must be rejected");
    };
    assert!(
        matches!(&error, LowerError::UnsupportedAt { .. }),
        "expected typed span-bearing rejection, got {error:?}"
    );
    assert_eq!(error.source_span(), Some(span));
    assert!(error.reason().contains("non-finite"));
    assert!(
        !error.reason().contains("singular"),
        "non-finite coefficients are invalid, not a singularity proof"
    );
}

#[test]
fn derivative_system_rejects_constant_duplicate_and_zero_rows_at_lowering() {
    for zero_row in [false, true] {
        let dae_model = singular_derivative_system(zero_row);
        let Err(error) = crate::lower::analyze_derivative_rhs(&dae_model) else {
            panic!("compile-time singular derivative matrix must be rejected");
        };
        assert!(
            matches!(&error, LowerError::UnsupportedAt { .. }),
            "expected span-bearing unsupported derivative system, got {error:?}"
        );
        let expected_span = if zero_row {
            solve_test_span()
        } else {
            solve_numbered_span(203, 10, 20)
        };
        assert_eq!(error.source_span(), Some(expected_span));
        if zero_row {
            assert!(
                error
                    .reason()
                    .contains("has 0 derivative-containing equations for 1 state derivatives"),
                "an exact zero coefficient row is algebraic, leaving an underdetermined derivative system: {error}"
            );
        } else {
            assert!(
                error.reason().contains("singular"),
                "unexpected singular-system error: {error}"
            );
        }
    }
}
