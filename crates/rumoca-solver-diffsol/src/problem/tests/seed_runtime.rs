use super::runtime_projection_seed::reduced_solver::{
    add_trapezoid_parameter_starts, trapezoid_drive_expr,
};
use super::*;

fn build_negative_period_count_trapezoid_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("vIn.v"),
        dae::Variable::new(dae::VarName::new("vIn.v")),
    );
    add_trapezoid_parameter_starts(&mut dae);
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("vIn.v"),
        trapezoid_drive_expr(),
    )));
    dae
}

#[test]
fn test_apply_initial_section_assignments_propagates_aliases_into_pre_equations() {
    rumoca_eval_dae::runtime::clear_pre_values();

    let mut dae = dae::Dae::new();
    dae.discrete_valued.insert(
        dae::VarName::new("active"),
        dae::Variable::new(dae::VarName::new("active")),
    );
    dae.discrete_valued.insert(
        dae::VarName::new("localActive"),
        dae::Variable::new(dae::VarName::new("localActive")),
    );
    dae.discrete_valued.insert(
        dae::VarName::new("newActive"),
        dae::Variable::new(dae::VarName::new("newActive")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z"),
        lit(0.0),
    )));
    dae.f_m.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("active"),
        var("localActive"),
    )));
    dae.initial_equations.push(dae::Equation::explicit(
        dae::VarName::new("active"),
        lit(1.0),
        Span::DUMMY,
        "initial active",
    ));
    let pre_new_active = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args: vec![var("newActive")],
    };
    let pre_local_active = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args: vec![var("localActive")],
    };
    dae.initial_equations.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        pre_new_active,
        pre_local_active,
    )));

    let p = default_params(&dae);
    let mut y = vec![0.0; dae.f_x.len()];
    initialize_state_vector(&dae, &mut y);
    apply_initial_section_assignments(&dae, &mut y, &p, 0.0);

    let pre_local = rumoca_eval_dae::runtime::get_pre_value("localActive")
        .expect("pre(localActive) should be seeded from initial alias closure");
    let pre_new = rumoca_eval_dae::runtime::get_pre_value("newActive")
        .expect("pre(newActive) should follow explicit initial pre equation");
    assert!(
        (pre_local - 1.0).abs() < 1e-12,
        "expected pre(localActive)=1 from active=1 + alias equation, got {pre_local}"
    );
    assert!(
        (pre_new - 1.0).abs() < 1e-12,
        "expected pre(newActive)=1 from pre(newActive)=pre(localActive), got {pre_new}"
    );
}

#[test]
fn test_persist_initial_section_discrete_starts_propagates_aliases() {
    let mut dae = dae::Dae::new();
    dae.discrete_valued.insert(
        dae::VarName::new("active"),
        dae::Variable::new(dae::VarName::new("active")),
    );
    dae.discrete_valued.insert(
        dae::VarName::new("localActive"),
        dae::Variable::new(dae::VarName::new("localActive")),
    );
    dae.f_m.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("active"),
        var("localActive"),
    )));
    dae.initial_equations.push(dae::Equation::explicit(
        dae::VarName::new("active"),
        lit(1.0),
        Span::DUMMY,
        "initial active",
    ));

    let p = default_params(&dae);
    let y = Vec::<f64>::new();
    let updates = super::init::persist_initial_section_discrete_starts(&mut dae, &y, &p, 0.0)
        .expect("initial-section discrete starts should persist");

    assert_eq!(updates, 2);
    let active_start = dae
        .discrete_valued
        .get(&dae::VarName::new("active"))
        .and_then(|var| var.start.as_ref());
    let local_start = dae
        .discrete_valued
        .get(&dae::VarName::new("localActive"))
        .and_then(|var| var.start.as_ref());
    assert!(matches!(
        active_start,
        Some(dae::Expression::Literal(dae::Literal::Real(value))) if (*value - 1.0).abs() < 1.0e-12
    ));
    assert!(matches!(
        local_start,
        Some(dae::Expression::Literal(dae::Literal::Real(value))) if (*value - 1.0).abs() < 1.0e-12
    ));
}

#[test]
fn test_seed_direct_assignment_handles_size1_indexed_lhs() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("aux"),
        dae::Variable::new(dae::VarName::new("aux")),
    );
    let mut p_var = dae::Variable::new(dae::VarName::new("p"));
    p_var.start = Some(lit(1.25));
    dae.parameters.insert(dae::VarName::new("p"), p_var);
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("aux"),
            subscripts: vec![dae::Subscript::Index(1)],
        }),
        rhs: Box::new(var("p")),
    }));

    let n_x = count_states(&dae);
    let p = default_params(&dae);
    let mut y = vec![0.0; dae.f_x.len()];
    initialize_state_vector(&dae, &mut y);

    let updates = seed_direct_assignment_initial_values(&dae, &mut y, &p, n_x, false, 0.0);
    assert!(updates > 0);
    assert!((y[0] - 1.25).abs() < 1e-12);
}

#[test]
fn test_seed_direct_assignment_updates_all_array_slots_from_base_target() {
    let mut dae = dae::Dae::new();
    let mut aw = dae::Variable::new(dae::VarName::new("aw"));
    aw.dims = vec![3];
    dae.algebraics.insert(dae::VarName::new("aw"), aw);

    let rhs = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("aw")),
        rhs: Box::new(dae::Expression::Array {
            elements: vec![lit(1.0), lit(2.0), lit(3.0)],
            is_matrix: false,
        }),
    };
    dae.f_x.push(eq_from(rhs));

    let n_x = count_states(&dae);
    let p = default_params(&dae);
    let n_unknowns = dae.algebraics.values().map(|var| var.size()).sum::<usize>();
    let mut y = vec![0.0; n_unknowns];
    initialize_state_vector(&dae, &mut y);

    let updates = seed_direct_assignment_initial_values(&dae, &mut y, &p, n_x, false, 0.0);
    assert!(
        updates >= 3,
        "expected array assignment seeding updates for all slots"
    );
    assert!((y[0] - 1.0).abs() < 1e-12);
    assert!((y[1] - 2.0).abs() < 1e-12);
    assert!((y[2] - 3.0).abs() < 1e-12);
}

#[test]
fn test_seed_direct_assignment_initial_values_use_initial_mode_branch() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("x"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Initial,
                    args: vec![],
                },
                lit(1.0),
            )],
            else_branch: Box::new(lit(2.0)),
        },
    )));

    let mut y = vec![0.0; dae.f_x.len()];
    let updates = seed_direct_assignment_initial_values(&dae, &mut y, &[], 0, true, 0.0);

    assert!(updates > 0, "initial-mode direct seeding should update x");
    assert!((y[0] - 1.0).abs() < 1.0e-12);
}

#[test]
fn test_seed_direct_assignment_initial_values_bootstrap_initial_section_discrete_values() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.discrete_reals.insert(
        dae::VarName::new("t_start"),
        dae::Variable::new(dae::VarName::new("t_start")),
    );
    dae.initial_equations.push(dae::Equation::explicit(
        dae::VarName::new("t_start"),
        lit(-1.0),
        Span::DUMMY,
        "init t_start=-1",
    ));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("x"),
        dae::Expression::If {
            branches: vec![(
                binop(
                    OpBinary::Gt(Default::default()),
                    var("time"),
                    var("t_start"),
                ),
                lit(5.0),
            )],
            else_branch: Box::new(lit(-5.0)),
        },
    )));

    let mut y = vec![0.0; 1];
    let updates = seed_direct_assignment_initial_values(&dae, &mut y, &[], 0, false, 0.0);

    assert!(updates > 0, "initial direct seeding should update x");
    assert!(
        (y[0] - 5.0).abs() < 1.0e-12,
        "MLS §8.6: initial direct seeding must see initialization-section discrete values"
    );
}

#[test]
fn test_seed_direct_assignment_initial_values_falls_back_for_change_builtin() {
    rumoca_eval_dae::runtime::clear_pre_values();

    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.parameters.insert(
        dae::VarName::new("flag"),
        dae::Variable::new(dae::VarName::new("flag")),
    );
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("x"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Change,
                    args: vec![var("flag")],
                },
                lit(7.0),
            )],
            else_branch: Box::new(lit(3.0)),
        },
    )));

    let mut pre_env = rumoca_eval_dae::runtime::VarEnv::<f64>::new();
    pre_env.set("flag", 0.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&pre_env);

    let mut y = vec![0.0; 1];
    let updates = seed_direct_assignment_initial_values(&dae, &mut y, &[1.0], 0, false, 0.0);

    assert!(
        updates > 0,
        "change(flag) direct-assignment rows should stay seedable on the runtime path"
    );
    assert!((y[0] - 7.0).abs() < 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn test_seed_direct_assignment_ignores_orphaned_variable_pin_equations() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    let mut p_var = dae::Variable::new(dae::VarName::new("p"));
    p_var.start = Some(lit(2.0));
    dae.parameters.insert(dae::VarName::new("p"), p_var);

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(var("p")),
        },
        span: rumoca_core::Span::DUMMY,
        origin: "equation from model".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(lit(0.0)),
        },
        span: rumoca_core::Span::DUMMY,
        origin: "orphaned_variable_pin".to_string(),
        scalar_count: 1,
    });

    let n_x = count_states(&dae);
    let p = default_params(&dae);
    let mut y = vec![0.0; dae.f_x.len()];
    initialize_state_vector(&dae, &mut y);

    let updates = seed_direct_assignment_initial_values(&dae, &mut y, &p, n_x, false, 0.0);
    assert!(updates > 0);
    assert!(
        (y[0] - 2.0).abs() < 1e-12,
        "seeding should use physical direct assignment, not orphaned-variable pin"
    );
}

#[test]
fn test_runtime_projection_not_required_for_unique_acyclic_direct_assignments() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("y")),
        rhs: Box::new(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(lit(2.0)),
        }),
    }));

    assert!(
        !runtime_projection_required(&dae, 0),
        "unique acyclic direct assignments should use fast direct seeding"
    );
}

#[test]
fn test_seed_runtime_direct_assignments_resolves_acyclic_unknown_chain() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("y")),
        rhs: Box::new(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(lit(2.0)),
        }),
    }));

    let mut y = vec![0.0; dae.f_x.len()];
    let params = default_params(&dae);
    let updates = seed_runtime_direct_assignment_values(&dae, &mut y, &params, 0, 0.0);
    let names = solver_vector_names(&dae, y.len());
    let idx_for = |needle: &str| {
        names
            .iter()
            .position(|name| name == needle)
            .unwrap_or_else(|| panic!("missing solver variable '{needle}' in {:?}", names))
    };
    let x_idx = idx_for("x");
    let y_idx = idx_for("y");
    assert!(
        updates > 0,
        "runtime direct-assignment seeding should update acyclic chain variables"
    );
    assert!(
        (y[x_idx] - 1.0).abs() < 1.0e-12,
        "expected x=1 from x:=1 (names={names:?}, y={y:?})"
    );
    assert!(
        (y[y_idx] - 3.0).abs() < 1.0e-12,
        "expected y=x+2 to resolve in the same seeding pass chain (names={names:?}, y={y:?})"
    );
}

#[test]
fn test_seed_runtime_direct_assignments_uses_runtime_tail_start_chain() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );

    let mut p_var = dae::Variable::new(dae::VarName::new("p"));
    p_var.start = Some(lit(3.0));
    dae.parameters.insert(dae::VarName::new("p"), p_var);

    let mut u_var = dae::Variable::new(dae::VarName::new("u"));
    u_var.start = Some(binop(OpBinary::Add(Default::default()), var("p"), lit(1.0)));
    dae.inputs.insert(dae::VarName::new("u"), u_var);

    let mut d_var = dae::Variable::new(dae::VarName::new("d"));
    d_var.start = Some(binop(OpBinary::Add(Default::default()), var("u"), lit(2.0)));
    dae.discrete_reals.insert(dae::VarName::new("d"), d_var);

    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("x"),
        binop(OpBinary::Add(Default::default()), var("d"), lit(0.0)),
        Span::DUMMY,
        "seed x=d+0",
    ));
    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        binop(OpBinary::Add(Default::default()), var("x"), lit(1.0)),
        Span::DUMMY,
        "seed y=x+1",
    ));

    let mut y = vec![0.0; dae.f_x.len()];
    let updates = seed_runtime_direct_assignment_values(&dae, &mut y, &[3.0], 0, 0.0);
    let names = solver_vector_names(&dae, y.len());
    let idx_for = |needle: &str| {
        names
            .iter()
            .position(|name| name == needle)
            .unwrap_or_else(|| panic!("missing solver variable '{needle}' in {:?}", names))
    };
    let x_idx = idx_for("x");
    let y_idx = idx_for("y");
    assert!(
        updates >= 2,
        "runtime direct-assignment seeding should propagate runtime-tail starts through the chain"
    );
    assert!((y[x_idx] - 6.0).abs() < 1.0e-12);
    assert!((y[y_idx] - 7.0).abs() < 1.0e-12);
}

#[test]
fn test_seed_direct_assignment_initial_values_falls_back_when_compiled_row_is_unsupported() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("x"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![lit(1.0), lit(1.0)],
        },
        Span::DUMMY,
        "seed x=sample(1,1)",
    ));

    let mut y = vec![0.0];
    let updates = seed_direct_assignment_initial_values(&dae, &mut y, &[], 0, false, 0.0);
    assert_eq!(updates, 0);
    assert_eq!(y, vec![0.0]);
}

#[test]
fn test_seed_runtime_direct_assignments_propagates_hidden_non_solver_intermediate() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );

    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("hidden.mid"),
        lit(1.0),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("x"),
        var("hidden.mid"),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("y"),
        binop(OpBinary::Add(Default::default()), var("x"), lit(2.0)),
    )));

    let mut y = vec![0.0; dae.f_x.len()];
    let updates = seed_runtime_direct_assignment_values(&dae, &mut y, &[], 0, 0.0);
    let names = solver_vector_names(&dae, y.len());
    let idx_for = |needle: &str| {
        names
            .iter()
            .position(|name| name == needle)
            .unwrap_or_else(|| panic!("missing solver variable '{needle}' in {:?}", names))
    };
    let x_idx = idx_for("x");
    let y_idx = idx_for("y");
    assert!(
        updates >= 2,
        "hidden non-solver intermediates should propagate through the main seed loop"
    );
    assert!((y[x_idx] - 1.0).abs() < 1.0e-12);
    assert!((y[y_idx] - 3.0).abs() < 1.0e-12);
}

#[test]
fn test_seed_runtime_direct_assignments_respect_negative_integer_period_count() {
    let dae = build_negative_period_count_trapezoid_dae();

    let params = default_params(&dae);
    let ctx = build_runtime_direct_seed_context(&dae, 1, 0);
    let mut y = vec![0.0];
    let updates =
        seed_runtime_direct_assignment_values_with_context(&ctx, &dae, &mut y, &params, 0.0);

    assert!(updates > 0, "runtime direct seed should update vIn.v");
    assert!(
        (y[0] - 5.0).abs() <= 1.0e-12,
        "runtime direct seed should keep trapezoid source active when nperiod = -1, got {}",
        y[0]
    );
}

#[test]
fn test_runtime_projection_required_for_duplicate_direct_assignment_targets() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(var("y")),
    }));

    assert!(
        runtime_projection_required(&dae, 0),
        "multiple equations assigning the same target require Newton projection"
    );
}

#[test]
fn test_runtime_projection_required_for_direct_assignment_cycles() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(var("y")),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("y")),
        rhs: Box::new(var("x")),
    }));

    assert!(
        runtime_projection_required(&dae, 0),
        "cyclic direct assignments require Newton projection"
    );
}

#[test]
fn test_runtime_projection_required_for_runtime_discrete_builtins() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![var("x")],
        }),
    }));

    assert!(
        runtime_projection_required(&dae, 0),
        "assignments that depend on runtime discrete/event builtins must use runtime projection"
    );
}

#[test]
fn test_no_state_runtime_projection_not_required_for_hidden_direct_assignment_chain() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.outputs.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.discrete_reals.insert(
        dae::VarName::new("hiddenClock.c"),
        dae::Variable::new(dae::VarName::new("hiddenClock.c")),
    );

    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("y")),
        rhs: Box::new(var("x")),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("hiddenClock.c")),
        rhs: Box::new(var("y")),
    }));

    assert!(
        runtime_projection_required(&dae, 0),
        "general runtime projection stays conservative when a direct assignment targets an env-only intermediate",
    );
    assert!(
        !no_state_runtime_projection_required(&dae, 0),
        "no-state sampling should use direct assignment settling for unique acyclic hidden-target chains",
    );
}

#[test]
fn test_no_state_runtime_projection_not_required_for_alias_resolved_solver_target() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("sampleClock"),
        dae::Variable::new(dae::VarName::new("sampleClock")),
    );
    dae.outputs.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.discrete_reals.insert(
        dae::VarName::new("periodicClock.c"),
        dae::Variable::new(dae::VarName::new("periodicClock.c")),
    );

    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("y")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("periodicClock.c")),
        rhs: Box::new(var("sampleClock")),
    }));
    dae.f_z.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.c"),
        lit(1.0),
        Span::DUMMY,
        "periodicClock.c := 1",
    ));

    assert!(
        runtime_projection_required(&dae, 0),
        "general runtime projection stays conservative when a solver target is only reachable through alias propagation",
    );
    assert!(
        !no_state_runtime_projection_required(&dae, 0),
        "no-state sampling should accept solver targets resolved through runtime alias anchors",
    );
}

#[test]
fn test_no_state_runtime_projection_not_required_for_shift_sample_style_alias_graph() {
    let mut dae = dae::Dae::new();
    for name in ["sample1.u", "sample1.clock", "sine.y"] {
        dae.algebraics.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }
    dae.discrete_reals.insert(
        dae::VarName::new("periodicClock.c"),
        dae::Variable::new(dae::VarName::new("periodicClock.c")),
    );

    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("sine.y")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("sine.y")),
        rhs: Box::new(var("sample1.u")),
    }));
    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("periodicClock.c")),
        rhs: Box::new(var("sample1.clock")),
    }));
    dae.f_z.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.c"),
        lit(1.0),
        Span::DUMMY,
        "periodicClock.c := 1",
    ));

    assert!(
        runtime_projection_required(&dae, 0),
        "general runtime projection stays conservative for duplicate alias targets plus hidden clock aliases",
    );
    assert!(
        !no_state_runtime_projection_required(&dae, 0),
        "no-state sampling should settle shift-sample style alias graphs without Newton projection",
    );
}

#[test]
fn test_no_state_runtime_projection_required_for_unsupported_function_assignment() {
    let mut dae = dae::Dae::new();
    dae.outputs.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );

    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("userFn"),
            args: vec![var("time")],
            is_constructor: false,
        },
        Span::DUMMY,
        "y := userFn(time)",
    ));

    assert!(
        no_state_runtime_projection_required(&dae, 0),
        "no-state sampling must keep projection enabled when an algebraic RHS depends on a non-fast function call",
    );
}

#[test]
fn test_no_state_runtime_projection_required_for_change_guarded_assignment() {
    let mut dae = dae::Dae::new();
    dae.outputs.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.discrete_valued.insert(
        dae::VarName::new("flag"),
        dae::Variable::new(dae::VarName::new("flag")),
    );

    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Change,
                    args: vec![var("flag")],
                },
                lit(1.0),
            )],
            else_branch: Box::new(lit(0.0)),
        },
        Span::DUMMY,
        "y := if change(flag) then 1 else 0",
    ));

    assert!(
        no_state_runtime_projection_required(&dae, 0),
        "no-state sampling must keep projection enabled for change()-guarded assignments",
    );
}

#[test]
fn test_no_state_runtime_projection_not_required_for_pure_complex_helper_assignments() {
    let mut dae = dae::Dae::new();
    for name in ["y_conj", "y_pow", "y_ctor"] {
        dae.outputs.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }
    dae.parameters.insert(
        dae::VarName::new("u.re"),
        dae::Variable::new(dae::VarName::new("u.re")),
    );
    dae.parameters.insert(
        dae::VarName::new("u.im"),
        dae::Variable::new(dae::VarName::new("u.im")),
    );
    dae.parameters.insert(
        dae::VarName::new("k"),
        dae::Variable::new(dae::VarName::new("k")),
    );

    let conj_call = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.ComplexMath.conj"),
        args: vec![var("u.re"), var("u.im")],
        is_constructor: false,
    };
    let power_of_j = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.ComplexBlocks.ComplexMath.TransferFunction.powerOfJ"),
        args: vec![var("k")],
        is_constructor: false,
    };

    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y_conj"),
        dae::Expression::FieldAccess {
            base: Box::new(conj_call),
            field: "re".to_string(),
        },
        Span::DUMMY,
        "y_conj := conj(u).re",
    ));
    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y_pow"),
        dae::Expression::FieldAccess {
            base: Box::new(power_of_j),
            field: "im".to_string(),
        },
        Span::DUMMY,
        "y_pow := powerOfJ(k).im",
    ));
    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y_ctor"),
        dae::Expression::FieldAccess {
            base: Box::new(dae::Expression::FunctionCall {
                name: dae::VarName::new("Complex"),
                args: vec![lit(2.0), lit(-3.0)],
                is_constructor: true,
            }),
            field: "re".to_string(),
        },
        Span::DUMMY,
        "y_ctor := Complex(2, -3).re",
    ));

    assert!(
        !no_state_runtime_projection_required(&dae, 0),
        "pure complex helper calls should stay on the no-state direct-assignment refresh path",
    );
}

#[test]
fn test_no_state_runtime_projection_not_required_for_pure_table_value_helper_assignment() {
    let mut dae = dae::Dae::new();
    dae.outputs.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    for name in [
        "tableID",
        "icol",
        "time",
        "nextTimeEvent",
        "preNextTimeEvent",
        "startTime",
    ] {
        dae.parameters.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }

    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(lit(0.25)),
            rhs: Box::new(dae::Expression::FunctionCall {
                name: dae::VarName::new("Modelica.Blocks.Tables.Internal.getTimeTableValueNoDer2"),
                args: vec![
                    var("tableID"),
                    var("icol"),
                    var("time"),
                    var("nextTimeEvent"),
                    var("preNextTimeEvent"),
                    var("startTime"),
                ],
                is_constructor: false,
            }),
        },
        Span::DUMMY,
        "y := 0.25 + getTimeTableValueNoDer2(...)",
    ));

    assert!(
        !no_state_runtime_projection_required(&dae, 0),
        "pure external table value getters should stay on the no-state direct-assignment refresh path",
    );
}

#[test]
fn test_no_state_runtime_projection_required_for_visible_non_solver_output_without_runtime_settle()
{
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.outputs.insert(
        dae::VarName::new("Enable.y"),
        dae::Variable::new(dae::VarName::new("Enable.y")),
    );

    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));

    assert!(
        no_state_runtime_projection_required(&dae, 0),
        "visible outputs that are neither solver-backed nor runtime-settled must keep no-state projection enabled",
    );
}

#[test]
fn test_no_state_runtime_projection_not_required_for_visible_non_solver_output_with_discrete_settle()
 {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.outputs.insert(
        dae::VarName::new("Enable.y"),
        dae::Variable::new(dae::VarName::new("Enable.y")),
    );

    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_z.push(dae::Equation::explicit(
        dae::VarName::new("Enable.y"),
        lit(3.0),
        Span::DUMMY,
        "Enable.y := 3",
    ));

    assert!(
        !no_state_runtime_projection_required(&dae, 0),
        "visible non-solver outputs may stay on the no-projection path when runtime discrete settle materializes them",
    );
}

#[test]
fn test_no_state_runtime_projection_required_for_visible_non_solver_discrete_without_runtime_settle()
 {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.discrete_valued.insert(
        dae::VarName::new("Enable.y"),
        dae::Variable::new(dae::VarName::new("Enable.y")),
    );

    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));

    assert!(
        no_state_runtime_projection_required(&dae, 0),
        "visible non-solver discrete channels must keep no-state projection enabled when runtime settle cannot materialize them",
    );
}

#[test]
fn test_no_state_runtime_projection_not_required_for_visible_non_solver_discrete_with_runtime_settle()
 {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.discrete_valued.insert(
        dae::VarName::new("Enable.y"),
        dae::Variable::new(dae::VarName::new("Enable.y")),
    );

    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("Enable.y"),
        lit(3.0),
        Span::DUMMY,
        "Enable.y := 3",
    ));

    assert!(
        !no_state_runtime_projection_required(&dae, 0),
        "visible non-solver discrete channels may stay on the no-projection path when runtime settle materializes them",
    );
}

#[test]
fn test_no_state_runtime_projection_not_required_for_solver_backed_runtime_discrete_target() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.outputs.insert(
        dae::VarName::new("clk"),
        dae::Variable::new(dae::VarName::new("clk")),
    );
    dae.discrete_reals.insert(
        dae::VarName::new("clk"),
        dae::Variable::new(dae::VarName::new("clk")),
    );

    dae.f_x.push(eq_from(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(1.0)),
    }));
    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("clk"),
        lit(0.0),
        Span::DUMMY,
        "orphaned_variable_pin",
    ));
    dae.f_z.push(dae::Equation::explicit(
        dae::VarName::new("clk"),
        lit(2.0),
        Span::DUMMY,
        "clk := 2",
    ));

    assert!(
        !no_state_runtime_projection_required(&dae, 0),
        "solver-backed discrete targets updated on the runtime z/m settle path should not force no-state projection",
    );
}

#[test]
fn test_no_state_runtime_projection_not_required_for_raw_indexed_solver_runtime_target() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("src"),
        dae::Variable::new(dae::VarName::new("src")),
    );
    dae.discrete_reals.insert(
        dae::VarName::new("u"),
        dae::Variable::new(dae::VarName::new("u")),
    );

    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("src[1]"),
        var("u"),
    )));

    assert!(
        !no_state_runtime_projection_required(&dae, 0),
        "raw indexed solver-to-runtime direct assignments should stay on the no-projection path",
    );
}
