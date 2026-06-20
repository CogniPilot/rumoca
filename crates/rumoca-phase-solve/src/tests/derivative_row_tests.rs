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
