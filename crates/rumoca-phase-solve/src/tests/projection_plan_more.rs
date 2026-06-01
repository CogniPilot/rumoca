use super::*;

#[test]
fn solve_problem_preserves_duplicate_target_residual_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    for name in ["u", "v", "w"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), var("u")),
        Default::default(),
        "state derivative",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            var("u"),
            binary(
                rumoca_core::OpBinary::Sub,
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: rumoca_core::Span::DUMMY,
                },
                var("x"),
            ),
        ),
        Default::default(),
        "forcing equation for u",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, var("u"), var("v")),
        Default::default(),
        "alias with duplicate u target",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, var("w"), var("u")),
        Default::default(),
        "alias with separate target",
    ));

    let problem = lower_solve_problem(&dae_model).expect("duplicate targets should lower");

    let rhs = rumoca_eval_solve::to_scalar_program_block(&problem.continuous.implicit_rhs);
    assert_eq!(problem.continuous.residual.programs.len(), 3);
    assert_eq!(rhs.programs.len(), 4);
    assert!(
        rhs.programs[1].iter().any(|op| matches!(
            op,
            solve::LinearOp::Const { value, .. } if (*value - 1.0).abs() <= f64::EPSILON
        )),
        "first u-targeted forcing equation should not be overwritten by the u alias"
    );
    assert!(
        problem
            .continuous
            .implicit_row_targets
            .iter()
            .filter(|target| { matches!(target, Some(solve::ScalarSlot::Y { index: 1, .. })) })
            .count()
            <= 1,
        "duplicate target hints should not make multiple rows claim the same solver slot"
    );
}

#[test]
fn solve_problem_preserves_fallback_state_residual_targets() {
    let mut dae_model = dae::Dae::default();
    for name in ["state.p", "state.T"] {
        dae_model
            .variables
            .states
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    for name in ["source.p", "source.T"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            der(var("state.p")),
            var("source.p"),
        ),
        Default::default(),
        "state.p derivative",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            der(var("state.T")),
            var("source.T"),
        ),
        Default::default(),
        "state.T derivative",
    ));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: binary(rumoca_core::OpBinary::Sub, var("state"), var("source")),
        span: rumoca_core::Span::DUMMY,
        origin: "scalarized record consistency equation".to_string(),
        scalar_count: 2,
    });

    let problem = lower_solve_problem(&dae_model)
        .expect("scalarized record state residual targets should lower");

    assert_eq!(problem.continuous.implicit_row_targets.len(), 4);
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
        Some(solve::ScalarSlot::Y { index: 0, .. })
    ));
    assert!(matches!(
        problem.continuous.implicit_row_targets[3],
        Some(solve::ScalarSlot::Y { index: 1, .. })
    ));
}

#[test]
fn solve_problem_records_continuous_row_targets_for_projectors() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("force"), scalar_var("force"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("height"), scalar_var("height"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, var("force"), var("height")),
        // MLS Appendix B B.1a: continuous algebraic equations remain
        // simultaneous, but solve-IR preserves the explicit equation target
        // so runtime projectors do not infer pivots from expression strings.
        Default::default(),
        "force residual-form equation",
    ));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("height")),
        rhs: binary(
            rumoca_core::OpBinary::Sub,
            var("height"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            },
        ),
        span: Default::default(),
        origin: "height direct equation".to_string(),
        scalar_count: 1,
    });

    let problem = lower_solve_problem(&dae_model).expect("continuous targets should lower");

    assert!(matches!(
        problem.continuous.implicit_row_targets[0],
        Some(solve::ScalarSlot::Y { index: 0, .. })
    ));
    assert!(matches!(
        problem.continuous.implicit_row_targets[1],
        Some(solve::ScalarSlot::Y { index: 1, .. })
    ));
}

#[test]
fn solve_problem_records_scaled_algebraic_residual_target() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("a"), scalar_var("a"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("m"), scalar_var("m"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("f"), scalar_var("f"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Add,
            unary(rumoca_core::OpUnary::Minus, var("f")),
            binary(rumoca_core::OpBinary::Mul, var("m"), var("a")),
        ),
        rumoca_core::Span::DUMMY,
        "scaled force residual: -f + m*a = 0",
    ));

    let problem = lower_solve_problem(&dae_model).expect("scaled residual target should lower");

    assert_eq!(problem.continuous.implicit_row_targets.len(), 1);
    assert!(matches!(
        problem.continuous.implicit_row_targets[0],
        Some(solve::ScalarSlot::Y { index: 0, .. })
    ));
}

#[test]
fn solve_problem_expands_scalarized_record_residual_targets() {
    let mut dae_model = dae::Dae::default();
    for name in ["state.p", "state.T", "source.p", "source.T"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("state")),
        rhs: var("source"),
        span: Default::default(),
        origin: "scalarized record assignment".to_string(),
        scalar_count: 2,
    });

    let problem = lower_solve_problem(&dae_model).expect("record target rows should lower");

    assert_eq!(problem.continuous.residual.programs.len(), 2);
    assert!(matches!(
        problem.continuous.implicit_row_targets[0],
        Some(solve::ScalarSlot::Y { index: 0, .. })
    ));
    assert!(matches!(
        problem.continuous.implicit_row_targets[1],
        Some(solve::ScalarSlot::Y { index: 1, .. })
    ));
}

#[test]
fn scalarized_record_target_names_ignore_only_top_level_nested_suffixes() {
    let mut dae_model = dae::Dae::default();
    for name in [
        "state.p",
        "state.v[index.with.dot]",
        "state.nested.q",
        "other.p",
    ] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    let layout = layout::build_var_layout(&dae_model);

    let names = scalarized_record_target_names("state", &layout).expect("state targets");
    assert!(names.contains(&"state.p".to_string()));
    assert!(names.contains(&"state.v[index.with.dot]".to_string()));
    assert!(!names.contains(&"state.nested.q".to_string()));
}

#[test]
fn solve_problem_expands_scalarized_record_residual_form_targets() {
    let mut dae_model = dae::Dae::default();
    for name in ["state.p", "state.T", "source.p", "source.T"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, var("state"), var("source")),
        Default::default(),
        "scalarized record residual form",
    ));
    dae_model.continuous.equations[0].scalar_count = 2;

    let problem = lower_solve_problem(&dae_model).expect("record residual targets should lower");

    assert_eq!(problem.continuous.residual.programs.len(), 2);
    assert!(matches!(
        problem.continuous.implicit_row_targets[0],
        Some(solve::ScalarSlot::Y { index: 0, .. })
    ));
    assert!(matches!(
        problem.continuous.implicit_row_targets[1],
        Some(solve::ScalarSlot::Y { index: 1, .. })
    ));
}

#[test]
fn solve_problem_expands_array_explicit_residual_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("y"), array_var("y", &[2]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), array_var("u", &[2]));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: var("u"),
        span: rumoca_core::Span::DUMMY,
        origin: "array explicit equation".to_string(),
        scalar_count: 2,
    });

    let problem = lower_solve_problem(&dae_model).expect("array residual rows should lower");

    assert_eq!(problem.continuous.residual.programs.len(), 2);
    assert!(matches!(
        problem.continuous.implicit_row_targets[0],
        Some(solve::ScalarSlot::Y { index: 0, .. })
    ));
    assert!(matches!(
        problem.continuous.implicit_row_targets[1],
        Some(solve::ScalarSlot::Y { index: 1, .. })
    ));
}

#[test]
fn solve_problem_expands_array_residual_form_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("y"), array_var("y", &[2]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("u"), array_var("u", &[2]));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: binary(rumoca_core::OpBinary::Sub, var("y"), var("u")),
        span: rumoca_core::Span::DUMMY,
        origin: "array residual equation".to_string(),
        scalar_count: 2,
    });

    let problem = lower_solve_problem(&dae_model).expect("array residual form should lower");

    assert_eq!(problem.continuous.residual.programs.len(), 2);
    assert!(matches!(
        problem.continuous.implicit_row_targets[0],
        Some(solve::ScalarSlot::Y { index: 0, .. })
    ));
    assert!(matches!(
        problem.continuous.implicit_row_targets[1],
        Some(solve::ScalarSlot::Y { index: 1, .. })
    ));
}

#[test]
fn solve_problem_records_additive_flow_sum_row_targets() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("lineForce.frame_b.f"),
        array_var("lineForce.frame_b.f", &[3]),
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("frame_b.f"),
        array_var("frame_b.f", &[3]),
    );
    dae_model
        .continuous
        .equations
        .push(dae::Equation::residual_array(
            binary(
                rumoca_core::OpBinary::Add,
                var("lineForce.frame_b.f"),
                unary(rumoca_core::OpUnary::Minus, var("frame_b.f")),
            ),
            rumoca_core::Span::DUMMY,
            "flow sum equation: lineForce.frame_b.f + -frame_b.f = 0",
            3,
        ));

    let problem = lower_solve_problem(&dae_model).expect("flow-sum targets should lower");

    assert_eq!(problem.continuous.residual.programs.len(), 3);
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
fn solve_problem_skips_negated_additive_terms_when_recording_row_targets() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("left"), scalar_var("left"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("right"), scalar_var("right"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Add,
            unary(rumoca_core::OpUnary::Minus, var("left")),
            var("right"),
        ),
        rumoca_core::Span::DUMMY,
        "flow sum equation: -left + right = 0",
    ));

    let problem = lower_solve_problem(&dae_model).expect("signed flow-sum targets should lower");

    assert!(
        problem
            .continuous
            .implicit_row_targets
            .iter()
            .any(|target| matches!(target, Some(solve::ScalarSlot::Y { index: 1, .. })))
    );
    assert!(
        !problem
            .continuous
            .implicit_row_targets
            .iter()
            .any(|target| matches!(target, Some(solve::ScalarSlot::Y { index: 0, .. })))
    );
}

#[test]
fn solve_problem_records_slice_assignment_row_targets() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("leg_v_b"),
        array_var("leg_v_b", &[3, 4]),
    );
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("rhs"), array_var("rhs", &[3]));
    dae_model
        .continuous
        .equations
        .push(dae::Equation::residual_array(
            binary(
                rumoca_core::OpBinary::Sub,
                indexed_var(
                    "leg_v_b",
                    vec![
                        rumoca_core::Subscript::generated_colon(rumoca_core::Span::DUMMY),
                        rumoca_core::Subscript::generated_expr(Box::new(int_expr(2))),
                    ],
                ),
                var("rhs"),
            ),
            Default::default(),
            "slice assignment target",
            3,
        ));

    let problem = lower_solve_problem(&dae_model).expect("slice targets should lower");

    assert_eq!(problem.continuous.residual.programs.len(), 3);
    for expected_index in [1, 5, 9] {
        assert!(matches!(
            problem.continuous.implicit_row_targets[expected_index],
            Some(solve::ScalarSlot::Y { index, .. }) if index == expected_index
        ));
    }
}

#[test]
fn solve_problem_reserves_late_direct_array_targets_before_fallback_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("h"), array_var("h", &[4]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("aux"), array_var("aux", &[3]));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, var("h[1]"), int_expr(1)),
        Default::default(),
        "first direct array row",
    ));
    for idx in 1..=3 {
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Add,
                binary(
                    rumoca_core::OpBinary::Mul,
                    var(&format!("aux[{idx}]")),
                    int_expr(2),
                ),
                int_expr(1),
            ),
            Default::default(),
            "untargeted residual row",
        ));
    }
    for idx in 2..=4 {
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Sub,
                var(&format!("h[{idx}]")),
                int_expr(idx as i64),
            ),
            Default::default(),
            "late direct array row",
        ));
    }

    let problem = lower_solve_problem(&dae_model).expect("array targets should reserve slots");

    assert_eq!(problem.continuous.implicit_row_targets.len(), 7);
    for idx in 0..4 {
        assert!(matches!(
            problem.continuous.implicit_row_targets[idx],
            Some(solve::ScalarSlot::Y { index, .. }) if index == idx
        ));
    }
    for idx in 4..7 {
        assert!(matches!(
            problem.continuous.implicit_row_targets[idx],
            Some(solve::ScalarSlot::Y { index, .. }) if index == idx
        ));
    }
}

#[test]
fn solve_problem_lowers_runtime_tail_alias_assignments_from_continuous_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("table_y"), scalar_var("table_y"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("table_y")),
        rhs: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        origin: "table output equation".to_string(),
        scalar_count: 1,
    });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("table_y")),
        rhs: var("u"),
        span: Default::default(),
        // MLS §8.3: connection aliases are equations. A runtime-tail
        // connector input constrained by a solver-backed output must be
        // carried as an explicit solve-IR assignment, not left stale.
        origin: "connector alias table_y = u".to_string(),
        scalar_count: 1,
    });

    let problem =
        lower_solve_problem(&dae_model).expect("runtime-tail alias assignments should lower");

    assert_eq!(problem.discrete.runtime_assignment_rhs.programs.len(), 1);
    assert_eq!(problem.discrete.runtime_assignment_targets.len(), 1);
    assert!(matches!(
        problem.discrete.runtime_assignment_targets[0],
        solve::ScalarSlot::P { .. }
    ));
}

#[test]
fn solve_problem_excludes_runtime_tail_aliases_from_implicit_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("table_y"), scalar_var("table_y"));
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("clock_y"), scalar_var("clock_y"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));

    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("table_y")),
        rhs: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        origin: "solver-backed table output".to_string(),
        scalar_count: 1,
    });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("table_y")),
        rhs: var("u"),
        span: Default::default(),
        // MLS Appendix B B.1: runtime-tail aliases constrain discrete/input
        // values at event/runtime boundaries; they must not replace a
        // continuous solver row when building the implicit row set.
        origin: "runtime-tail alias table_y = u".to_string(),
        scalar_count: 1,
    });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("clock_y")),
        rhs: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(2.0),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        origin: "solver-backed clock output".to_string(),
        scalar_count: 1,
    });

    let problem = lower_solve_problem(&dae_model).expect("runtime-tail alias rows should lower");

    assert_eq!(problem.discrete.runtime_assignment_rhs.programs.len(), 1);
    assert_eq!(problem.discrete.runtime_assignment_targets.len(), 1);
    assert_eq!(problem.continuous.implicit_row_targets.len(), 2);
    assert!(matches!(
        problem.continuous.implicit_row_targets[0],
        Some(solve::ScalarSlot::Y { index: 0, .. })
    ));
    assert!(matches!(
        problem.continuous.implicit_row_targets[1],
        Some(solve::ScalarSlot::Y { index: 1, .. })
    ));
}

#[test]
fn solve_problem_keeps_discrete_update_aliases_in_implicit_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("clock_y"), scalar_var("clock_y"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("clock_c"), scalar_var("clock_c"));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("clock_y")),
        rhs: var("clock_c"),
        span: Default::default(),
        // MLS Appendix B B.1b: a discrete variable with its own event
        // assignment is known during the continuous solve. The continuous
        // alias must read it instead of overwriting the event assignment.
        origin: "clock output reads discrete clock condition".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("clock_c")),
        rhs: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        origin: "clock condition update".to_string(),
        scalar_count: 1,
    });

    let problem =
        lower_solve_problem(&dae_model).expect("discrete-to-continuous alias should lower");

    assert!(problem.discrete.runtime_assignment_rhs.programs.is_empty());
    assert!(problem.discrete.runtime_assignment_targets.is_empty());
    assert_eq!(problem.continuous.implicit_row_targets.len(), 1);
    assert!(matches!(
        problem.continuous.implicit_row_targets[0],
        Some(solve::ScalarSlot::Y { index: 0, .. })
    ));
}

#[test]
fn solve_problem_keeps_static_runtime_tail_color_aliases_out_of_refresh_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("world.axisColor_x"),
        array_var("world.axisColor_x", &[3]),
    );
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("world.axisColor_y"),
        array_var("world.axisColor_y", &[3]),
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("world.y_label.cylinders.color"),
        array_var("world.y_label.cylinders.color", &[2, 3]),
    );
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("world.axisColor_x")),
        rhs: rumoca_core::Expression::Array {
            elements: vec![int_expr(0), int_expr(0), int_expr(255)],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
        origin: "static x axis color binding".to_string(),
        scalar_count: 3,
    });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("world.axisColor_y")),
        rhs: var("world.axisColor_x"),
        span: rumoca_core::Span::DUMMY,
        origin: "static axis color binding".to_string(),
        scalar_count: 3,
    });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("world.y_label.cylinders.color")),
        rhs: var("world.axisColor_y"),
        span: rumoca_core::Span::DUMMY,
        origin: "component array color binding".to_string(),
        scalar_count: 6,
    });

    let problem = lower_solve_problem(&dae_model)
        .expect("static runtime-tail color aliases should lower through Solve IR");

    assert!(problem.discrete.runtime_assignment_rhs.is_empty());
    assert!(problem.discrete.runtime_assignment_targets.is_empty());
    assert_eq!(problem.continuous.implicit_row_targets.len(), 6);
}

#[test]
fn solve_problem_recovers_discrete_target_from_residual_update_row() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), scalar_var("z"));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            binary(
                rumoca_core::OpBinary::Sub,
                var("z"),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            Default::default(),
            // MLS §8.3.4: equation residual form `z - expr = 0` is
            // semantically equivalent to the explicit update `z = expr`.
            "residual discrete update",
        ));

    let problem = lower_solve_problem(&dae_model).expect("residual discrete update should lower");

    assert_eq!(problem.discrete.rhs.programs.len(), 1);
    assert_eq!(problem.discrete.update_targets.len(), 1);
    assert!(matches!(
        problem.discrete.update_targets[0],
        solve::ScalarSlot::P { index: 0, .. }
    ));
    assert_eq!(
        problem.discrete.pre_modes,
        vec![solve::DiscreteEventPreMode::FollowCurrent]
    );
}

#[test]
fn solve_problem_accepts_lowered_pre_parameter_rows() {
    let mut dae_model = dae::Dae::default();
    for name in ["aux", "y"] {
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    insert_pre_parameter(&mut dae_model, "aux");
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: pre_var("aux"),
        span: Default::default(),
        // DAE lowering rewrites pre(aux) to a __pre__.aux parameter before
        // Solve-IR lowering. The first event-iteration pass reads the event
        // entry value; later passes read the previous fixed-point pass.
        origin: "digital gate previous auxiliary".to_string(),
        scalar_count: 1,
    });

    let problem =
        lower_solve_problem(&dae_model).expect("plain pre update should lower to solve-IR");

    assert_eq!(
        problem.discrete.pre_modes,
        vec![solve::DiscreteEventPreMode::Fixed]
    );
}

#[test]
fn solve_problem_marks_clocked_target_pre_rows_as_event_entry() {
    let mut dae_model = dae::Dae::default();
    for name in ["u", "y"] {
        dae_model
            .variables
            .discrete_reals
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.clocks.intervals.insert("y".to_string(), 0.1);
    insert_pre_parameter(&mut dae_model, "u");
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y")),
        rhs: pre_var("u"),
        span: Default::default(),
        // Clocked rows read the previous clock tick value for the whole tick;
        // event iteration must not advance these pre slots within the tick.
        origin: "clocked sample hold".to_string(),
        scalar_count: 1,
    });

    let problem =
        lower_solve_problem(&dae_model).expect("clocked pre update should lower to solve-IR");

    assert_eq!(
        problem.discrete.pre_modes,
        vec![solve::DiscreteEventPreMode::EventEntry]
    );
}

#[test]
fn solve_problem_marks_condition_memory_pre_rows_as_fixed_pre() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("c"), array_var("c", &[1]));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("hit"), scalar_var("hit"));
    insert_pre_parameter(&mut dae_model, "c[1]");
    dae_model.conditions.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("c[1]")),
        rhs: var("u"),
        span: Default::default(),
        origin: "condition memory".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("hit")),
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                pre_var("c[1]"),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(true),
                    span: rumoca_core::Span::DUMMY,
                },
            )],
            else_branch: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // MLS §8.6 / Appendix B: pre(c[i]) is relation memory from the
        // previous event-iteration pass, with the event-entry value on the
        // first pass.
        origin: "when condition memory read".to_string(),
        scalar_count: 1,
    });
    assert!(expression_contains_event_entry_pre_operator(
        &dae_model,
        &pre_var("c[1]")
    ));

    let problem = lower_solve_problem(&dae_model)
        .expect("condition-memory pre update should lower to solve-IR");

    assert_eq!(
        problem.discrete.pre_modes.first(),
        Some(&solve::DiscreteEventPreMode::Fixed)
    );
}

#[test]
fn solve_problem_marks_sample_guarded_discrete_rows_as_fixed_pre() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("z"), scalar_var("z"));
    insert_pre_parameter(&mut dae_model, "z");
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("z")),
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                sample_event_indicator_expr(0.0, 0.1),
                binary(
                    rumoca_core::OpBinary::Add,
                    pre_var("z"),
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(1.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
            )],
            else_branch: Box::new(pre_var("z")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // MLS §16.5.1: sample-triggered when equations use the tick
        // left-limit value of pre(..), not an event-iteration feedback
        // value that would reapply the assignment indefinitely.
        origin: "sample guarded update".to_string(),
        scalar_count: 1,
    });

    let problem = lower_solve_problem(&dae_model)
        .expect("sample-guarded discrete update should lower to solve-IR");

    assert_eq!(
        problem.discrete.pre_modes,
        vec![solve::DiscreteEventPreMode::Fixed]
    );
}

#[test]
fn solve_problem_marks_change_guarded_discrete_rows_as_fixed_pre() {
    let mut dae_model = dae::Dae::default();
    for name in ["x", "latched"] {
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    insert_pre_parameter(&mut dae_model, "x");
    insert_pre_parameter(&mut dae_model, "latched");
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("latched")),
        rhs: rumoca_core::Expression::If {
            branches: vec![(change_lowered_expr(var("x"), pre_var("x")), var("x"))],
            else_branch: Box::new(pre_var("latched")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Default::default(),
        // MLS §3.7.5 / §8.6: change/edge/pre in an event-triggered
        // discrete update read the event left-limit while the event
        // iteration solves current values to a fixed point.
        origin: "change guarded update".to_string(),
        scalar_count: 1,
    });

    let problem = lower_solve_problem(&dae_model)
        .expect("change-guarded discrete update should lower to solve-IR");

    assert_eq!(
        problem.discrete.pre_modes,
        vec![solve::DiscreteEventPreMode::Fixed]
    );
}

#[test]
fn solve_problem_lowers_change_against_left_limit_parameter() {
    let mut dae_model = dae::Dae::default();
    for name in ["clock", "flag"] {
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    insert_pre_parameter(&mut dae_model, "clock");
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("flag")),
        rhs: change_lowered_expr(var("clock"), pre_var("clock")),
        span: Default::default(),
        // MLS §3.7.3: change(v) observes v <> pre(v), where pre(v) is the
        // event left-limit value, not the current discrete value reloaded
        // through the same slot.
        origin: "flag = change(clock)".to_string(),
        scalar_count: 1,
    });

    let problem = lower_solve_problem(&dae_model)
        .expect("change(clock) should lower against a pre-parameter");
    let mut p = vec![0.0; problem.layout.p_scalars()];
    let set_p = |p: &mut [f64], name: &str, value: f64| {
        let Some(solve::ScalarSlot::P { index, .. }) = problem.layout.binding(name) else {
            panic!("expected parameter slot for {name}");
        };
        p[index] = value;
    };

    set_p(&mut p, "__pre__.clock", 3.0);
    set_p(&mut p, "clock", 4.0);
    let changed =
        rumoca_eval_solve::eval_row(&problem.discrete.rhs.programs[0], &[], &p, 0.0, None)
            .expect("change row evaluates");

    set_p(&mut p, "__pre__.clock", 4.0);
    let unchanged =
        rumoca_eval_solve::eval_row(&problem.discrete.rhs.programs[0], &[], &p, 0.0, None)
            .expect("unchanged row evaluates");

    assert_eq!(changed, 1.0);
    assert_eq!(unchanged, 0.0);
}

#[test]
fn solve_problem_lowers_indexed_change_against_left_limit_parameter() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("clock"), array_var("clock", &[2]));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("__pre__.clock"), {
            let mut var = array_var("__pre__.clock", &[2]);
            var.fixed = Some(true);
            var
        });
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("flag"), scalar_var("flag"));
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("flag")),
        rhs: change_lowered_expr(
            rumoca_core::Expression::Index {
                base: Box::new(var("clock")),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    2,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            },
            pre_var("clock[2]"),
        ),
        span: Default::default(),
        origin: "flag = change(clock[2])".to_string(),
        scalar_count: 1,
    });

    let problem =
        lower_solve_problem(&dae_model).expect("change(clock[2]) should lower with pre slot");
    let mut p = vec![0.0; problem.layout.p_scalars()];
    let set_p = |p: &mut [f64], name: &str, value: f64| {
        let Some(solve::ScalarSlot::P { index, .. }) = problem.layout.binding(name) else {
            panic!("expected parameter slot for {name}");
        };
        p[index] = value;
    };

    set_p(&mut p, "__pre__.clock[2]", 3.0);
    set_p(&mut p, "clock[2]", 4.0);

    let changed =
        rumoca_eval_solve::eval_row(&problem.discrete.rhs.programs[0], &[], &p, 0.0, None)
            .expect("indexed change row evaluates");

    assert_eq!(changed, 1.0);
}

#[test]
fn solve_problem_preserves_runtime_event_actions() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        rumoca_core::Span::DUMMY,
        "state derivative",
    ));

    let true_expr = rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Boolean(true),
        span: rumoca_core::Span::DUMMY,
    };
    dae_model.events.event_actions.push(dae::DaeEventAction {
        condition: true_expr.clone(),
        kind: dae::DaeEventActionKind::Assert {
            message: "assert failed".to_string(),
        },
        span: rumoca_core::Span::DUMMY,
        origin: "assert action".to_string(),
    });
    dae_model.events.event_actions.push(dae::DaeEventAction {
        condition: true_expr,
        kind: dae::DaeEventActionKind::Terminate {
            message: "finished".to_string(),
        },
        span: rumoca_core::Span::DUMMY,
        origin: "terminate action".to_string(),
    });

    let problem = lower_solve_problem(&dae_model).expect("event actions should lower");

    assert_eq!(problem.events.action_conditions.len(), 2);
    assert_eq!(problem.events.actions.len(), 2);
    assert!(matches!(
        problem.events.actions[0].kind,
        solve::SolveEventActionKind::Assert
    ));
    assert!(matches!(
        problem.events.actions[1].kind,
        solve::SolveEventActionKind::Terminate
    ));
}

#[path = "observation_aliases.rs"]
mod observation_aliases;
