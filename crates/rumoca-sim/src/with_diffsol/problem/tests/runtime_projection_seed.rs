use super::*;
use crate::with_diffsol::problem::init::project_algebraics_with_fixed_states_at_time_with_context;

#[test]
fn test_project_runtime_seeds_direct_assignments_before_newton() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));
    dae.algebraics
        .insert(VarName::new("w"), Variable::new(VarName::new("w")));

    // 0 = der(x) - w
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![var("x")],
        },
        var("w"),
    )));

    // 0 = z - (if time < 0.5 then 1 else 2)
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("z"),
        Expression::If {
            branches: vec![(
                binop(OpBinary::Lt(Default::default()), var("time"), lit(0.5)),
                lit(1.0),
            )],
            else_branch: Box::new(lit(2.0)),
        },
    )));

    // 0 = (z - 1) * w
    dae.f_x.push(eq_from(binop(
        OpBinary::Mul(Default::default()),
        binop(OpBinary::Sub(Default::default()), var("z"), lit(1.0)),
        var("w"),
    )));

    let timeout = crate::TimeoutBudget::new(None);
    let projected = project_algebraics_with_fixed_states_at_time(
        &dae,
        &[0.0, 1.0, 1.0], // stale pre-event algebraic seed
        1,
        1.0,
        1e-9,
        &timeout,
    )
    .expect("runtime projection should not error")
    .expect("runtime projection should converge with direct-assignment seeding");

    assert!((projected[1] - 2.0).abs() < 1e-9);
    assert!(projected[2].abs() < 1e-9);
}

#[test]
fn test_runtime_projection_leaves_alias_connected_unknowns_free() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics.insert(
        VarName::new("source.p.v"),
        Variable::new(VarName::new("source.p.v")),
    );
    dae.algebraics.insert(
        VarName::new("node.v"),
        Variable::new(VarName::new("node.v")),
    );

    // 0 = der(x)
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![var("x")],
        },
        lit(0.0),
    )));

    // 0 = 24 - source.p.v
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        lit(24.0),
        var("source.p.v"),
    )));

    // 0 = source.p.v - node.v
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("source.p.v"),
        var("node.v"),
    )));

    let timeout = crate::TimeoutBudget::new(None);
    let projected = project_algebraics_with_fixed_states_at_time(
        &dae,
        &[0.0, 0.0, 0.0],
        1,
        1.0,
        1e-9,
        &timeout,
    )
    .expect("runtime projection should not error")
    .expect("runtime projection should converge when only alias-connected unknowns remain free");

    assert!((projected[1] - 24.0).abs() < 1e-9);
    assert!((projected[2] - 24.0).abs() < 1e-9);
}

#[test]
fn test_runtime_projection_handles_hidden_step_source_intermediate_target() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics.insert(
        VarName::new("source.v"),
        Variable::new(VarName::new("source.v")),
    );
    dae.algebraics.insert(
        VarName::new("source.p.v"),
        Variable::new(VarName::new("source.p.v")),
    );
    dae.algebraics.insert(
        VarName::new("node.v"),
        Variable::new(VarName::new("node.v")),
    );

    // 0 = der(x)
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![var("x")],
        },
        lit(0.0),
    )));

    // Hidden intermediate target, matching the MSL StepVoltage lowering shape:
    // source.signalSource.y is not part of the solver vector.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("source.signalSource.y"),
        Expression::If {
            branches: vec![(
                binop(OpBinary::Lt(Default::default()), var("time"), lit(0.5)),
                lit(0.0),
            )],
            else_branch: Box::new(lit(24.0)),
        },
    )));

    // source.v = source.signalSource.y
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("source.v"),
        var("source.signalSource.y"),
    )));

    // source.v = source.p.v
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("source.v"),
        var("source.p.v"),
    )));

    // source.p.v = node.v
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("source.p.v"),
        var("node.v"),
    )));

    let timeout = crate::TimeoutBudget::new(None);
    let projected = project_algebraics_with_fixed_states_at_time(
        &dae,
        &[0.0, 0.0, 0.0, 0.0, 0.0], // stale pre-event seed
        1,
        1.0,
        1e-9,
        &timeout,
    )
    .expect("runtime projection should not error")
    .expect("runtime projection should converge with hidden direct-assignment targets");

    assert!((projected[1] - 24.0).abs() < 1e-9);
    assert!((projected[2] - 24.0).abs() < 1e-9);
    assert!((projected[3] - 24.0).abs() < 1e-9);
}

pub(super) mod reduced_solver;

#[test]
fn test_runtime_projection_ignores_fixed_differential_rows() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics.insert(
        VarName::new("source.p.v"),
        Variable::new(VarName::new("source.p.v")),
    );
    dae.algebraics.insert(
        VarName::new("node.v"),
        Variable::new(VarName::new("node.v")),
    );

    // Differential row that would conflict with the post-event algebraic solve
    // if we let it participate while x is fixed.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![var("x")],
        },
        var("source.p.v"),
    )));

    // 0 = 24 - source.p.v
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        lit(24.0),
        var("source.p.v"),
    )));

    // 0 = source.p.v - node.v
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("source.p.v"),
        var("node.v"),
    )));

    let timeout = crate::TimeoutBudget::new(None);
    let projected = project_algebraics_with_fixed_states_at_time(
        &dae,
        &[0.0, 0.0, 0.0],
        1,
        1.0,
        1e-9,
        &timeout,
    )
    .expect("runtime projection should not error")
    .expect("runtime projection should converge when fixed differential rows are masked");

    assert!((projected[1] - 24.0).abs() < 1e-9);
    assert!((projected[2] - 24.0).abs() < 1e-9);
}

#[test]
fn test_runtime_alias_propagation_uses_discrete_runtime_defined_anchor() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));
    dae.discrete_valued.insert(
        VarName::new("enable"),
        Variable::new(VarName::new("enable")),
    );
    // Explicit runtime target keeps alias anchor discovery in canonical
    // event partitions (no model-algorithm fallback path).
    dae.f_m.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("enable"),
        binop(OpBinary::Add(Default::default()), var("enable"), lit(0.0)),
    )));

    // Alias chain: a = enable, b = a
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("a"),
        var("enable"),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("b"),
        var("a"),
    )));

    let mut y = vec![0.0, 0.0];
    let mut env = build_env(&dae, &y, &[], 0.0);
    env.set("enable", 4.0);

    let adjacency = build_runtime_alias_adjacency(&dae, 0);
    assert!(
        adjacency.contains_key("a"),
        "expected alias graph to include a"
    );
    assert!(
        adjacency.contains_key("enable"),
        "expected alias graph to include enable"
    );
    let anchors = collect_runtime_alias_anchor_names(&dae, 0);
    assert!(
        anchors.contains("enable"),
        "expected runtime anchor set to include enable"
    );

    let updates = propagate_runtime_alias_components_from_env(&dae, &mut y, 0, &mut env);
    assert_eq!(updates, 2);
    assert!((y[0] - 4.0).abs() < 1e-12);
    assert!((y[1] - 4.0).abs() < 1e-12);
    assert!((env.vars.get("a").copied().unwrap_or(0.0) - 4.0).abs() < 1e-12);
    assert!((env.vars.get("b").copied().unwrap_or(0.0) - 4.0).abs() < 1e-12);
}

#[test]
fn test_runtime_alias_propagation_uses_discrete_partition_alias_chain() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));
    dae.discrete_valued.insert(
        VarName::new("enable"),
        Variable::new(VarName::new("enable")),
    );
    dae.f_m.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("enable"),
        binop(OpBinary::Add(Default::default()), var("enable"), lit(0.0)),
    )));

    // Alias chain appears only in f_m (discrete partition), not f_x.
    dae.f_m.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("a"),
        var("enable"),
    )));
    dae.f_m.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("b"),
        var("a"),
    )));

    let mut y = vec![0.0, 0.0];
    let mut env = build_env(&dae, &y, &[], 0.0);
    env.set("enable", 4.0);

    let updates = propagate_runtime_alias_components_from_env(&dae, &mut y, 0, &mut env);
    assert_eq!(updates, 2);
    assert!((y[0] - 4.0).abs() < 1e-12);
    assert!((y[1] - 4.0).abs() < 1e-12);
    assert!((env.vars.get("a").copied().unwrap_or(0.0) - 4.0).abs() < 1e-12);
    assert!((env.vars.get("b").copied().unwrap_or(0.0) - 4.0).abs() < 1e-12);
}

#[test]
fn test_discrete_alias_chain_does_not_back_propagate_unanchored_zero() {
    let mut dae = Dae::new();
    for name in ["source", "a", "b", "y2"] {
        dae.discrete_reals
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    // One anchored non-alias assignment and a pure alias chain.
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("source"),
        lit(5.0),
    )));
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("y2"),
        var("b"),
    )));
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("b"),
        var("a"),
    )));
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("a"),
        var("source"),
    )));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env = build_env(&dae, &[], &[], 0.0);
    env.set("source", 0.0);
    env.set("a", 0.0);
    env.set("b", 0.0);
    env.set("y2", 0.0);

    let changed = apply_discrete_partition_updates(&dae, &mut env);
    assert!(
        changed,
        "discrete alias chain should settle from anchored source"
    );
    assert_eq!(env.vars.get("source").copied().unwrap_or(-1.0), 5.0);
    assert_eq!(env.vars.get("a").copied().unwrap_or(-1.0), 5.0);
    assert_eq!(env.vars.get("b").copied().unwrap_or(-1.0), 5.0);
    assert_eq!(env.vars.get("y2").copied().unwrap_or(-1.0), 5.0);
}

#[test]
fn test_runtime_alias_anchors_include_conditional_discrete_targets() {
    let mut dae = Dae::new();
    dae.algebraics.insert(
        VarName::new("assignClock1.clock"),
        Variable::new(VarName::new("assignClock1.clock")),
    );
    for name in ["periodicClock.c", "assignClock1.y", "unitDelay1.u"] {
        dae.discrete_reals
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    // Alias between runtime discrete clock state and solver algebraic clock.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("periodicClock.c"),
        var("assignClock1.clock"),
    )));

    // Discrete assignment targets hidden under If-rewritten residual forms.
    dae.f_z.push(eq_from(Expression::If {
        branches: vec![(
            lit(1.0),
            binop(
                OpBinary::Sub(Default::default()),
                var("periodicClock.c"),
                lit(1.0),
            ),
        )],
        else_branch: Box::new(binop(
            OpBinary::Sub(Default::default()),
            var("periodicClock.c"),
            lit(0.0),
        )),
    }));
    dae.f_z.push(eq_from(Expression::If {
        branches: vec![(
            lit(1.0),
            binop(
                OpBinary::Sub(Default::default()),
                var("assignClock1.y"),
                lit(2.0),
            ),
        )],
        else_branch: Box::new(binop(
            OpBinary::Sub(Default::default()),
            var("assignClock1.y"),
            Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args: vec![var("assignClock1.y")],
            },
        )),
    }));

    // Alias chain from sampled assignment target into peer discrete unknown.
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("assignClock1.y"),
        var("unitDelay1.u"),
    )));

    let anchors = collect_runtime_alias_anchor_names(&dae, 0);
    assert!(
        anchors.contains("periodicClock.c"),
        "conditional discrete target should remain a runtime alias anchor"
    );
    assert!(
        anchors.contains("assignClock1.y"),
        "conditional sampled target should remain a runtime alias anchor"
    );

    let mut y = vec![0.0];
    let mut env = build_env(&dae, &y, &[], 0.0);
    env.set("periodicClock.c", 1.0);
    env.set("assignClock1.y", 2.0);
    let updates = propagate_runtime_alias_components_from_env(&dae, &mut y, 0, &mut env);
    assert!(updates >= 2);
    assert!((y[0] - 1.0).abs() < 1.0e-12);
    assert!((env.vars.get("unitDelay1.u").copied().unwrap_or(-1.0) - 2.0).abs() < 1.0e-12);
}

#[test]
fn test_discrete_partition_table_like_varref_is_not_treated_as_alias() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.f_m.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("x"),
        var("Tbl[i]"),
    )));

    let mut env = build_env(&dae, &[0.0], &[], 0.0);
    env.set("i", 3.0);
    env.set("Tbl[3]", 4.0);

    let changed = apply_discrete_partition_updates(&dae, &mut env);
    assert!(changed, "expected discrete partition update to apply");
    assert!((env.vars.get("x").copied().unwrap_or(0.0) - 4.0).abs() < 1e-12);
}

#[test]
fn test_discrete_partition_direct_array_assignment_updates_all_elements() {
    let mut dae = Dae::new();
    let mut y = Variable::new(VarName::new("y"));
    y.dims = vec![2];
    dae.discrete_reals.insert(VarName::new("y"), y);

    let mut u = Variable::new(VarName::new("u"));
    u.dims = vec![2];
    dae.discrete_reals.insert(VarName::new("u"), u);

    dae.f_m.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("y"),
        var("u"),
    )));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env = build_env(&dae, &[], &[], 0.0);
    rumoca_eval_dae::runtime::set_array_entries(&mut env, "y", &[2], &[0.0, 0.0]);
    rumoca_eval_dae::runtime::set_array_entries(&mut env, "u", &[2], &[1.0, 2.0]);

    let changed = apply_discrete_partition_updates(&dae, &mut env);
    assert!(changed, "expected array direct assignment to update y");
    assert!(
        (env.vars.get("y").copied().unwrap_or(-1.0) - 1.0).abs() < 1e-12,
        "unexpected y values: y={}, y[1]={}, y[2]={}",
        env.vars.get("y").copied().unwrap_or(-1.0),
        env.vars.get("y[1]").copied().unwrap_or(-1.0),
        env.vars.get("y[2]").copied().unwrap_or(-1.0)
    );
    assert!((env.vars.get("y[1]").copied().unwrap_or(-1.0) - 1.0).abs() < 1e-12);
    assert!((env.vars.get("y[2]").copied().unwrap_or(-1.0) - 2.0).abs() < 1e-12);
}

#[test]
fn test_discrete_tuple_function_assignment_updates_scalar_and_array_targets() {
    let mut dae = Dae::new();
    dae.discrete_reals
        .insert(VarName::new("noise"), Variable::new(VarName::new("noise")));
    let mut seed_state = Variable::new(VarName::new("seedState"));
    seed_state.dims = vec![3];
    dae.discrete_valued
        .insert(VarName::new("seedState"), seed_state);

    let mut random_fn = dae::Function::new("Pkg.random", Span::DUMMY);
    random_fn
        .inputs
        .push(dae::FunctionParam::new("seedIn", "Integer").with_dims(vec![3]));
    random_fn
        .outputs
        .push(dae::FunctionParam::new("noiseOut", "Real"));
    random_fn
        .outputs
        .push(dae::FunctionParam::new("seedOut", "Integer").with_dims(vec![3]));
    random_fn.body.push(dae::Statement::Assignment {
        comp: crate::with_diffsol::test_support::comp_ref("noiseOut"),
        value: lit(0.75),
    });
    random_fn.body.push(dae::Statement::Assignment {
        comp: crate::with_diffsol::test_support::comp_ref("seedOut"),
        value: Expression::Array {
            elements: vec![lit(11.0), lit(12.0), lit(13.0)],
            is_matrix: false,
        },
    });
    dae.functions.insert(random_fn.name.clone(), random_fn);

    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        Expression::Tuple {
            elements: vec![var("noise"), var("seedState")],
        },
        Expression::FunctionCall {
            name: VarName::new("Pkg.random"),
            args: vec![var("seedState")],
            is_constructor: false,
        },
    )));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env = build_env(&dae, &[], &[], 0.0);
    env.set("noise", 0.0);
    env.set("seedState", 0.0);
    env.set("seedState[1]", 0.0);
    env.set("seedState[2]", 0.0);
    env.set("seedState[3]", 0.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env);

    let changed = apply_discrete_partition_updates(&dae, &mut env);
    assert!(
        changed,
        "tuple function assignment should update discrete targets"
    );
    assert!((env.vars.get("noise").copied().unwrap_or(-1.0) - 0.75).abs() < 1e-12);
    assert!((env.vars.get("seedState").copied().unwrap_or(-1.0) - 11.0).abs() < 1e-12);
    assert!((env.vars.get("seedState[1]").copied().unwrap_or(-1.0) - 11.0).abs() < 1e-12);
    assert!((env.vars.get("seedState[2]").copied().unwrap_or(-1.0) - 12.0).abs() < 1e-12);
    assert!((env.vars.get("seedState[3]").copied().unwrap_or(-1.0) - 13.0).abs() < 1e-12);
}

#[test]
fn test_runtime_alias_propagation_uses_non_alias_direct_assignment_anchor() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));

    // a is anchored by a non-alias direct assignment, then b aliases a.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("a"),
        lit(2.0),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("b"),
        var("a"),
    )));

    let mut y = vec![0.0, 0.0];
    let mut env = build_env(&dae, &y, &[], 0.0);
    env.set("a", 2.0);

    let updates = propagate_runtime_alias_components_from_env(&dae, &mut y, 0, &mut env);
    assert!(updates >= 1);
    assert!((y[1] - 2.0).abs() < 1e-12);
    assert!((env.vars.get("b").copied().unwrap_or(0.0) - 2.0).abs() < 1e-12);
}

#[test]
fn test_runtime_alias_propagation_updates_env_members_outside_solver_vector() {
    let mut dae = Dae::new();
    // Insertion order controls solver-vector truncation. Keep d1/d2 first so
    // only those plus `a` are in the first 3 solver slots.
    for name in ["d1", "d2", "a", "b", "c"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    // n_total = f_x.len() = 3, but alias component contains b/c too.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("a"),
        lit(4.0),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("b"),
        var("a"),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("c"),
        var("b"),
    )));

    // y has only first 3 algebraics in insertion order: d1, d2, a
    let mut y = vec![0.0, 0.0, 4.0];
    let mut env = build_env(&dae, &y, &[], 0.0);
    env.set("a", 4.0);

    let updates = propagate_runtime_alias_components_from_env(&dae, &mut y, 0, &mut env);
    assert!(updates > 0);
    assert!((env.vars.get("b").copied().unwrap_or(0.0) - 4.0).abs() < 1e-12);
    assert!((env.vars.get("c").copied().unwrap_or(0.0) - 4.0).abs() < 1e-12);
}

#[test]
fn test_runtime_direct_assignments_update_env_targets_outside_solver_vector() {
    let mut dae = Dae::new();
    // n_total = f_x.len() = 1; solver vector keeps only d1.
    for name in ["d1", "a"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("a"),
        lit(4.0),
    )));

    let mut y = vec![0.0];
    let mut env = build_env(&dae, &y, &[], 0.0);
    let updates = propagate_runtime_direct_assignments_from_env(&dae, &mut y, 0, &mut env);

    assert!(updates > 0);
    assert!((env.vars.get("a").copied().unwrap_or(0.0) - 4.0).abs() < 1e-12);
}

#[test]
fn test_discrete_partition_clocked_sample_alias_chain_latches_left_limit() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));
    dae.discrete_valued
        .insert(VarName::new("c"), Variable::new(VarName::new("c")));
    for name in ["clk", "sampled", "offset"] {
        dae.discrete_reals
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clk"),
        Expression::FunctionCall {
            name: VarName::new("Clock"),
            args: vec![var("c")],
            is_constructor: false,
        },
    )));
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("sampled"),
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args: vec![var("u"), var("clk")],
        },
    )));
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("sampled"),
        var("offset"),
    )));
    dae.f_m.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("c"),
        binop(OpBinary::Gt(Default::default()), var("time"), lit(0.5)),
    )));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env_prev = build_env(&dae, &[0.5], &[], 0.5);
    env_prev.set("u", 0.5);
    env_prev.set("c", 0.0);
    env_prev.set("clk", 0.0);
    env_prev.set("sampled", 0.0);
    env_prev.set("offset", 0.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_prev);

    let mut env = build_env(&dae, &[0.6], &[], 0.6);
    env.set("u", 0.6);
    let changed = apply_discrete_partition_updates(&dae, &mut env);
    assert!(changed, "expected discrete updates at first clock edge");
    assert_eq!(env.vars.get("c").copied().unwrap_or(0.0), 1.0);
    assert_eq!(env.vars.get("clk").copied().unwrap_or(0.0), 1.0);
    assert!((env.vars.get("sampled").copied().unwrap_or(0.0) - 0.5).abs() < 1.0e-12);
    assert!((env.vars.get("offset").copied().unwrap_or(0.0) - 0.5).abs() < 1.0e-12);
}

#[test]
fn test_guarded_when_no_arg_clock_uses_implicit_clock_activity() {
    let mut dae = Dae::new();
    dae.parameters.insert(
        VarName::new("period"),
        Variable::new(VarName::new("period")),
    );
    for name in ["clk", "x"] {
        dae.discrete_reals
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    // Explicit periodic clock edge source used to derive implicit clock activity.
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clk"),
        Expression::FunctionCall {
            name: VarName::new("Clock"),
            args: vec![var("period")],
            is_constructor: false,
        },
    )));

    // Guarded when-style assignment lowered to f_z:
    // x = if Clock() then 1 else pre(x)
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("x"),
        Expression::If {
            branches: vec![(
                Expression::FunctionCall {
                    name: VarName::new("Clock"),
                    args: vec![],
                    is_constructor: false,
                },
                lit(1.0),
            )],
            else_branch: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args: vec![var("x")],
            }),
        },
    )));

    rumoca_eval_dae::runtime::clear_pre_values();

    let mut env_t0 = build_env(&dae, &[], &[0.02], 0.0);
    env_t0.set("period", 0.02);
    env_t0.set("clk", 0.0);
    env_t0.set("x", 0.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_t0);
    let changed_t0 = apply_discrete_partition_updates(&dae, &mut env_t0);
    assert!(changed_t0, "expected tick updates at t=0");
    assert_eq!(env_t0.vars.get("clk").copied().unwrap_or(-1.0), 1.0);
    assert_eq!(
        env_t0.vars.get("x").copied().unwrap_or(-1.0),
        1.0,
        "guarded Clock() branch should fire on implicit clock tick"
    );

    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_t0);
    let mut env_t01 = build_env(&dae, &[], &[0.02], 0.01);
    env_t01.set("period", 0.02);
    env_t01.set("clk", 1.0);
    env_t01.set("x", 1.0);
    let changed_t01 = apply_discrete_partition_updates(&dae, &mut env_t01);
    assert!(changed_t01, "clock output should fall low between ticks");
    assert_eq!(env_t01.vars.get("clk").copied().unwrap_or(-1.0), 0.0);
    assert_eq!(
        env_t01.vars.get("x").copied().unwrap_or(-1.0),
        1.0,
        "guarded Clock() branch should hold pre(x) between implicit ticks"
    );
}

#[test]
fn test_guarded_when_array_pre_holds_elementwise_between_ticks() {
    let mut dae = Dae::new();
    dae.parameters.insert(
        VarName::new("period"),
        Variable::new(VarName::new("period")),
    );
    dae.discrete_reals
        .insert(VarName::new("clk"), Variable::new(VarName::new("clk")));

    let mut x = Variable::new(VarName::new("x"));
    x.dims = vec![2];
    dae.discrete_reals.insert(VarName::new("x"), x);

    let mut u = Variable::new(VarName::new("u"));
    u.dims = vec![2];
    dae.discrete_reals.insert(VarName::new("u"), u);

    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clk"),
        Expression::FunctionCall {
            name: VarName::new("Clock"),
            args: vec![var("period")],
            is_constructor: false,
        },
    )));

    // x = if Clock() then u else pre(x)
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("x"),
        Expression::If {
            branches: vec![(
                Expression::FunctionCall {
                    name: VarName::new("Clock"),
                    args: vec![],
                    is_constructor: false,
                },
                var("u"),
            )],
            else_branch: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args: vec![var("x")],
            }),
        },
    )));

    rumoca_eval_dae::runtime::clear_pre_values();

    let mut env_t0 = build_env(&dae, &[], &[0.02], 0.0);
    env_t0.set("period", 0.02);
    env_t0.set("clk", 0.0);
    rumoca_eval_dae::runtime::set_array_entries(&mut env_t0, "x", &[2], &[0.0, 0.0]);
    rumoca_eval_dae::runtime::set_array_entries(&mut env_t0, "u", &[2], &[1.0, 2.0]);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_t0);
    let changed_t0 = apply_discrete_partition_updates(&dae, &mut env_t0);
    assert!(changed_t0, "expected tick updates at t=0");
    assert_eq!(env_t0.vars.get("clk").copied().unwrap_or(-1.0), 1.0);
    assert!((env_t0.vars.get("x[1]").copied().unwrap_or(-1.0) - 1.0).abs() < 1.0e-12);
    assert!((env_t0.vars.get("x[2]").copied().unwrap_or(-1.0) - 2.0).abs() < 1.0e-12);

    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_t0);
    let mut env_t01 = build_env(&dae, &[], &[0.02], 0.01);
    env_t01.set("period", 0.02);
    env_t01.set("clk", 1.0);
    rumoca_eval_dae::runtime::set_array_entries(&mut env_t01, "x", &[2], &[1.0, 2.0]);
    rumoca_eval_dae::runtime::set_array_entries(&mut env_t01, "u", &[2], &[99.0, 77.0]);
    let changed_t01 = apply_discrete_partition_updates(&dae, &mut env_t01);
    assert!(changed_t01, "clock output should fall low between ticks");
    assert_eq!(env_t01.vars.get("clk").copied().unwrap_or(-1.0), 0.0);
    assert!((env_t01.vars.get("x[1]").copied().unwrap_or(-1.0) - 1.0).abs() < 1.0e-12);
    assert!((env_t01.vars.get("x[2]").copied().unwrap_or(-1.0) - 2.0).abs() < 1.0e-12);
}

#[test]
fn test_discrete_partition_previous_updates_only_on_clock_ticks() {
    let mut dae = Dae::new();
    dae.parameters.insert(
        VarName::new("period"),
        Variable::new(VarName::new("period")),
    );
    dae.discrete_reals
        .insert(VarName::new("clk"), Variable::new(VarName::new("clk")));
    dae.discrete_valued.insert(
        VarName::new("counter"),
        Variable::new(VarName::new("counter")),
    );

    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clk"),
        Expression::FunctionCall {
            name: VarName::new("Clock"),
            args: vec![var("period")],
            is_constructor: false,
        },
    )));
    dae.f_m.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("counter"),
        Expression::If {
            branches: vec![(
                binop(
                    OpBinary::Lt(Default::default()),
                    Expression::FunctionCall {
                        name: VarName::new("previous"),
                        args: vec![var("counter")],
                        is_constructor: false,
                    },
                    lit(3.0),
                ),
                binop(
                    OpBinary::Add(Default::default()),
                    Expression::FunctionCall {
                        name: VarName::new("previous"),
                        args: vec![var("counter")],
                        is_constructor: false,
                    },
                    lit(1.0),
                ),
            )],
            else_branch: Box::new(Expression::FunctionCall {
                name: VarName::new("previous"),
                args: vec![var("counter")],
                is_constructor: false,
            }),
        },
    )));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env_prev = build_env(&dae, &[], &[0.1], 0.0);
    env_prev.set("period", 0.1);
    env_prev.set("clk", 1.0);
    env_prev.set("counter", 1.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_prev);

    let mut env_non_tick = build_env(&dae, &[], &[0.1], 0.002);
    env_non_tick.set("period", 0.1);
    env_non_tick.set("clk", 1.0);
    env_non_tick.set("counter", 1.0);
    let changed_non_tick = apply_discrete_partition_updates(&dae, &mut env_non_tick);
    assert!(
        changed_non_tick,
        "clock output should fall low between ticks"
    );
    assert_eq!(env_non_tick.vars.get("clk").copied().unwrap_or(-1.0), 0.0);
    assert_eq!(
        env_non_tick.vars.get("counter").copied().unwrap_or(-1.0),
        1.0,
        "previous(counter) assignment must hold between clock ticks"
    );

    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_non_tick);
    let mut env_tick = build_env(&dae, &[], &[0.1], 0.2);
    env_tick.set("period", 0.1);
    env_tick.set("clk", 0.0);
    env_tick.set("counter", 1.0);
    let changed_tick = apply_discrete_partition_updates(&dae, &mut env_tick);
    assert!(changed_tick, "clock tick should trigger discrete updates");
    assert_eq!(env_tick.vars.get("clk").copied().unwrap_or(-1.0), 1.0);
    assert_eq!(
        env_tick.vars.get("counter").copied().unwrap_or(-1.0),
        2.0,
        "counter should advance on the next active clock edge"
    );
}

#[test]
fn test_subsample_counter_clock_ticks_with_factor_over_resolution_period() {
    let mut dae = Dae::new();
    dae.parameters.insert(
        VarName::new("factor"),
        Variable::new(VarName::new("factor")),
    );
    dae.parameters.insert(
        VarName::new("resolutionFactor"),
        Variable::new(VarName::new("resolutionFactor")),
    );
    dae.discrete_reals
        .insert(VarName::new("clk"), Variable::new(VarName::new("clk")));

    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clk"),
        Expression::FunctionCall {
            name: VarName::new("subSample"),
            args: vec![
                Expression::FunctionCall {
                    name: VarName::new("Clock"),
                    args: vec![var("factor")],
                    is_constructor: false,
                },
                var("resolutionFactor"),
            ],
            is_constructor: false,
        },
    )));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env_t0 = build_env(&dae, &[], &[20.0, 1000.0], 0.0);
    env_t0.set("factor", 20.0);
    env_t0.set("resolutionFactor", 1000.0);
    env_t0.set("clk", 0.0);
    let changed_t0 = apply_discrete_partition_updates(&dae, &mut env_t0);
    assert!(changed_t0, "clock should tick at t=0");
    assert_eq!(env_t0.vars.get("clk").copied().unwrap_or(-1.0), 1.0);

    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_t0);
    let mut env_t01 = build_env(&dae, &[], &[20.0, 1000.0], 0.01);
    env_t01.set("factor", 20.0);
    env_t01.set("resolutionFactor", 1000.0);
    env_t01.set("clk", 1.0);
    let changed_t01 = apply_discrete_partition_updates(&dae, &mut env_t01);
    assert!(changed_t01, "clock should fall low between ticks");
    assert_eq!(env_t01.vars.get("clk").copied().unwrap_or(-1.0), 0.0);

    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_t01);
    let mut env_t02 = build_env(&dae, &[], &[20.0, 1000.0], 0.02);
    env_t02.set("factor", 20.0);
    env_t02.set("resolutionFactor", 1000.0);
    env_t02.set("clk", 0.0);
    let changed_t02 = apply_discrete_partition_updates(&dae, &mut env_t02);
    assert!(changed_t02, "clock should tick again at t=0.02");
    assert_eq!(env_t02.vars.get("clk").copied().unwrap_or(-1.0), 1.0);
}

#[test]
fn test_shift_sample_value_form_inherits_source_clock_activity() {
    let mut dae = Dae::new();
    for name in ["period", "u"] {
        dae.parameters
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }
    for name in ["clk", "sampled", "shifted"] {
        dae.discrete_reals
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clk"),
        Expression::FunctionCall {
            name: VarName::new("Clock"),
            args: vec![var("period")],
            is_constructor: false,
        },
    )));
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("sampled"),
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args: vec![var("u"), var("clk")],
        },
    )));
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("shifted"),
        Expression::FunctionCall {
            name: VarName::new("shiftSample"),
            args: vec![var("sampled"), lit(0.0), lit(1.0)],
            is_constructor: false,
        },
    )));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env_prev = build_env(&dae, &[], &[0.1, 2.0], 0.0);
    env_prev.set("period", 0.1);
    env_prev.set("u", 2.0);
    env_prev.set("clk", 1.0);
    env_prev.set("sampled", 2.0);
    env_prev.set("shifted", 2.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_prev);

    let mut env_non_tick = build_env(&dae, &[], &[0.1, 9.0], 0.05);
    env_non_tick.set("period", 0.1);
    env_non_tick.set("u", 9.0);
    env_non_tick.set("clk", 1.0);
    env_non_tick.set("sampled", 2.0);
    env_non_tick.set("shifted", 2.0);
    let changed_non_tick = apply_discrete_partition_updates(&dae, &mut env_non_tick);
    assert!(
        changed_non_tick,
        "clock output should fall low between ticks"
    );
    assert_eq!(env_non_tick.vars.get("clk").copied().unwrap_or(-1.0), 0.0);
    assert_eq!(
        env_non_tick.vars.get("sampled").copied().unwrap_or(-1.0),
        2.0
    );
    assert_eq!(
        env_non_tick.vars.get("shifted").copied().unwrap_or(-1.0),
        2.0
    );

    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_non_tick);
    let mut env_tick = build_env(&dae, &[], &[0.1, 7.0], 0.1);
    env_tick.set("period", 0.1);
    env_tick.set("u", 7.0);
    env_tick.set("clk", 0.0);
    env_tick.set("sampled", 2.0);
    env_tick.set("shifted", 2.0);
    let changed_tick = apply_discrete_partition_updates(&dae, &mut env_tick);
    assert!(
        changed_tick,
        "clock tick should trigger source and shifted updates"
    );
    assert_eq!(env_tick.vars.get("clk").copied().unwrap_or(-1.0), 1.0);
    assert_eq!(env_tick.vars.get("sampled").copied().unwrap_or(-1.0), 9.0);
    assert_eq!(env_tick.vars.get("shifted").copied().unwrap_or(-1.0), 9.0);
}

#[test]
fn test_implicit_sample_time_uses_tick_instant_not_previous_sample_time() {
    let mut dae = Dae::new();
    dae.parameters.insert(
        VarName::new("period"),
        Variable::new(VarName::new("period")),
    );
    for name in ["clk", "sim_time", "step_y"] {
        dae.discrete_reals
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clk"),
        Expression::FunctionCall {
            name: VarName::new("Clock"),
            args: vec![var("period")],
            is_constructor: false,
        },
    )));
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("sim_time"),
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args: vec![var("time")],
        },
    )));
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("step_y"),
        Expression::If {
            branches: vec![(
                binop(OpBinary::Lt(Default::default()), var("sim_time"), lit(0.2)),
                lit(0.0),
            )],
            else_branch: Box::new(lit(1.0)),
        },
    )));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env_prev = build_env(&dae, &[], &[0.1], 0.1);
    env_prev.set("period", 0.1);
    env_prev.set("clk", 1.0);
    env_prev.set("sim_time", 0.1);
    env_prev.set("step_y", 0.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_prev);

    let mut env_tick = build_env(&dae, &[], &[0.1], 0.2);
    env_tick.set("period", 0.1);
    env_tick.set("clk", 0.0);
    env_tick.set("sim_time", 0.1);
    env_tick.set("step_y", 0.0);

    let changed_tick = apply_discrete_partition_updates(&dae, &mut env_tick);
    assert!(
        changed_tick,
        "clock tick should trigger implicit sample updates"
    );
    assert_eq!(env_tick.vars.get("clk").copied().unwrap_or(-1.0), 1.0);
    assert!(
        (env_tick.vars.get("sim_time").copied().unwrap_or(-1.0) - 0.2).abs() < 1.0e-12,
        "sample(time) should return the tick instant at clock events"
    );
    assert_eq!(
        env_tick.vars.get("step_y").copied().unwrap_or(-1.0),
        1.0,
        "time-based step must switch at the configured start time tick"
    );
}

fn add_tick_based_counter_parameters(dae: &mut Dae) {
    for name in ["period", "periodTicks", "periodOffset", "startTick"] {
        dae.parameters
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }
}

fn add_tick_based_counter_discrete_vars(dae: &mut Dae) {
    for name in ["clk", "y"] {
        dae.discrete_reals
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }
    for name in ["counter", "startOutput"] {
        dae.discrete_valued
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }
}

fn tick_previous(name: &str) -> Expression {
    Expression::FunctionCall {
        name: VarName::new("previous"),
        args: vec![var(name)],
        is_constructor: false,
    }
}

fn tick_counter_activation_expr() -> Expression {
    Expression::If {
        branches: vec![(
            tick_previous("startOutput"),
            binop(
                OpBinary::Sub(Default::default()),
                var("counter"),
                Expression::If {
                    branches: vec![(
                        binop(
                            OpBinary::Eq(Default::default()),
                            tick_previous("counter"),
                            binop(
                                OpBinary::Sub(Default::default()),
                                var("periodTicks"),
                                lit(1.0),
                            ),
                        ),
                        lit(0.0),
                    )],
                    else_branch: Box::new(binop(
                        OpBinary::Add(Default::default()),
                        tick_previous("counter"),
                        lit(1.0),
                    )),
                },
            ),
        )],
        else_branch: Box::new(binop(
            OpBinary::Sub(Default::default()),
            var("startOutput"),
            binop(
                OpBinary::Ge(Default::default()),
                tick_previous("counter"),
                binop(
                    OpBinary::Sub(Default::default()),
                    var("startTick"),
                    lit(1.0),
                ),
            ),
        )),
    }
}

fn tick_counter_progress_expr() -> Expression {
    Expression::If {
        branches: vec![(
            tick_previous("startOutput"),
            binop(
                OpBinary::Sub(Default::default()),
                var("startOutput"),
                tick_previous("startOutput"),
            ),
        )],
        else_branch: Box::new(binop(
            OpBinary::Sub(Default::default()),
            var("counter"),
            Expression::If {
                branches: vec![(var("startOutput"), lit(0.0))],
                else_branch: Box::new(binop(
                    OpBinary::Add(Default::default()),
                    tick_previous("counter"),
                    lit(1.0),
                )),
            },
        )),
    }
}

fn tick_output_expr() -> Expression {
    Expression::If {
        branches: vec![(
            var("startOutput"),
            Expression::FunctionCall {
                name: VarName::new("sin"),
                args: vec![binop(
                    OpBinary::Mul(Default::default()),
                    binop(
                        OpBinary::Div(Default::default()),
                        lit(2.0 * std::f64::consts::PI),
                        var("periodTicks"),
                    ),
                    binop(
                        OpBinary::Add(Default::default()),
                        var("counter"),
                        var("periodOffset"),
                    ),
                )],
                is_constructor: false,
            },
        )],
        else_branch: Box::new(lit(0.0)),
    }
}

fn build_tick_based_counter_dae() -> Dae {
    let mut dae = Dae::new();
    add_tick_based_counter_parameters(&mut dae);
    add_tick_based_counter_discrete_vars(&mut dae);
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clk"),
        Expression::FunctionCall {
            name: VarName::new("Clock"),
            args: vec![var("period")],
            is_constructor: false,
        },
    )));
    dae.f_m.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        lit(0.0),
        tick_counter_activation_expr(),
    )));
    dae.f_m.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        lit(0.0),
        tick_counter_progress_expr(),
    )));
    dae.f_z.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("y"),
        tick_output_expr(),
    )));
    dae
}

fn seed_tick_based_counter_pre_values(dae: &Dae) {
    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env_prev = build_env(dae, &[], &[0.1, 10.0, 2.0, 4.0], 0.2);
    env_prev.set("period", 0.1);
    env_prev.set("periodTicks", 10.0);
    env_prev.set("periodOffset", 2.0);
    env_prev.set("startTick", 4.0);
    env_prev.set("counter", 3.0);
    env_prev.set("startOutput", 0.0);
    env_prev.set("clk", 0.0);
    env_prev.set("y", 0.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env_prev);
}

fn tick_based_counter_runtime_env(dae: &Dae) -> VarEnv<f64> {
    let mut env_tick = build_env(dae, &[], &[0.1, 10.0, 2.0, 4.0], 0.3);
    env_tick.set("period", 0.1);
    env_tick.set("periodTicks", 10.0);
    env_tick.set("periodOffset", 2.0);
    env_tick.set("startTick", 4.0);
    env_tick.set("counter", 3.0);
    env_tick.set("startOutput", 0.0);
    env_tick.set("clk", 0.0);
    env_tick.set("y", 0.0);
    env_tick
}

#[test]
fn test_discrete_if_residual_assignments_update_tick_based_counter_state() {
    let dae = build_tick_based_counter_dae();
    seed_tick_based_counter_pre_values(&dae);
    let mut env_tick = tick_based_counter_runtime_env(&dae);

    let changed_tick = apply_discrete_partition_updates(&dae, &mut env_tick);
    assert!(
        changed_tick,
        "clock tick should trigger discrete partition updates"
    );
    assert_eq!(
        env_tick.vars.get("startOutput").copied().unwrap_or(-1.0),
        1.0,
        "startOutput should activate once startTick is reached"
    );
    assert_eq!(
        env_tick.vars.get("counter").copied().unwrap_or(-1.0),
        0.0,
        "counter should reset to zero on activation tick"
    );
    assert!(
        (env_tick.vars.get("y").copied().unwrap_or(0.0) - 0.9510565162951535).abs() < 1.0e-12,
        "tick-based sine should emit first nonzero sample at activation tick"
    );
}

#[test]
fn test_discrete_if_residual_direct_shape_updates_active_branch_target() {
    let mut dae = Dae::new();
    dae.discrete_valued.insert(
        VarName::new("counter"),
        Variable::new(VarName::new("counter")),
    );

    // Direct flattened residual if-shape (no outer `0 - (...)` wrapper):
    // if cond then counter - 1 else counter - 0
    dae.f_m.push(eq_from(Expression::If {
        branches: vec![(
            lit(1.0),
            binop(OpBinary::Sub(Default::default()), var("counter"), lit(1.0)),
        )],
        else_branch: Box::new(binop(
            OpBinary::Sub(Default::default()),
            var("counter"),
            lit(0.0),
        )),
    }));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env = build_env(&dae, &[], &[], 0.0);
    env.set("counter", 0.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env);

    let changed = apply_discrete_partition_updates(&dae, &mut env);
    assert!(
        changed,
        "direct residual if-shape should produce a discrete update"
    );
    assert_eq!(
        env.vars.get("counter").copied().unwrap_or(-1.0),
        1.0,
        "active if-branch assignment should update discrete target"
    );
}

#[test]
fn test_runtime_projection_settles_discrete_branch_before_runtime_newton() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("s"), Variable::new(VarName::new("s")));
    dae.algebraics
        .insert(VarName::new("v"), Variable::new(VarName::new("v")));
    dae.discrete_valued
        .insert(VarName::new("off"), Variable::new(VarName::new("off")));

    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![var("x")],
        },
        lit(0.0),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Add(Default::default()),
        var("s"),
        lit(9.0),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("v"),
        var("s"),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("v"),
        Expression::If {
            branches: vec![(var("off"), var("s"))],
            else_branch: Box::new(lit(10.0)),
        },
    )));
    dae.f_m.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("off"),
        binop(OpBinary::Lt(Default::default()), var("s"), lit(0.0)),
    )));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut pre_env = build_env(&dae, &[0.0, -9.0, -9.0], &[], 0.0);
    pre_env.set("off", 0.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&pre_env);

    let mut runtime_seed_env = Some(pre_env);
    let timeout = crate::TimeoutBudget::new(None);
    let projected = project_algebraics_with_fixed_states_at_time_with_context(
        &dae,
        &[0.0, -9.0, -9.0],
        1,
        RuntimeProjectionContext {
            p: &[],
            compiled_runtime: None,
            fixed_cols: &[true, false, false, false],
            ignored_rows: &[true, false, false, false],
            branch_local_analog_cols: &[],
            direct_seed_ctx: None,
            direct_seed_env_cache: Some(&mut runtime_seed_env),
        },
        0.0,
        1.0e-9,
        &timeout,
    )
    .expect("runtime projection should not error")
    .expect("runtime projection should converge with settled discrete branch");

    // MLS §8.6: runtime projection at ordinary events must re-evaluate normal
    // equations against the updated discrete fixed point, not a stale pre(off).
    assert!((projected[1] + 9.0).abs() < 1.0e-12);
    assert!((projected[2] + 9.0).abs() < 1.0e-12);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn test_seed_runtime_direct_assignments_accepts_time_source() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("u"),
        var("time"),
    )));

    let mut y = vec![0.0];
    let updates = seed_runtime_direct_assignments(&dae, &mut y, &[], 0, 0.6);
    assert!(updates > 0, "runtime seeding should update u from time");
    assert!((y[0] - 0.6).abs() < 1.0e-12);
}

#[test]
fn test_runtime_direct_seed_keeps_time_driven_branch_candidate() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("u"),
        Expression::If {
            branches: vec![(
                binop(OpBinary::Lt(Default::default()), var("time"), lit(0.5)),
                lit(1.0),
            )],
            else_branch: Box::new(lit(2.0)),
        },
    )));

    let ctx = build_runtime_direct_seed_context(&dae, 1, 0);
    let mut y = vec![0.0];
    let updates = seed_runtime_direct_assignment_values_with_context(&ctx, &dae, &mut y, &[], 1.0);

    assert!(
        updates > 0,
        "time-driven branch should still be direct-seeded"
    );
    assert!(
        (y[0] - 2.0).abs() < 1.0e-12,
        "expected seeded branch value 2.0, got {}",
        y[0]
    );
}

#[test]
fn test_runtime_direct_seed_skips_solver_dependent_branch_candidate() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));

    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("a"),
        lit(1.0),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("b"),
        Expression::BuiltinCall {
            function: BuiltinFunction::NoEvent,
            args: vec![Expression::If {
                branches: vec![(
                    binop(OpBinary::Gt(Default::default()), var("a"), lit(0.0)),
                    lit(2.0),
                )],
                else_branch: Box::new(lit(-2.0)),
            }],
        },
    )));

    let ctx = build_runtime_direct_seed_context(&dae, 2, 0);
    let mut y = vec![0.0, 99.0];
    let updates = seed_runtime_direct_assignment_values_with_context(&ctx, &dae, &mut y, &[], 0.0);

    assert!(
        updates > 0,
        "branch-free defining equation should still seed"
    );
    assert!(
        (y[0] - 1.0).abs() < 1.0e-12,
        "expected a to seed from its literal definition, got {}",
        y[0]
    );
    assert!(
        (y[1] - 99.0).abs() < 1.0e-12,
        "solver-dependent branch target must stay unseeded, got {}",
        y[1]
    );
}

#[test]
fn test_runtime_direct_seed_skips_branch_through_env_only_intermediate() {
    let mut dae = Dae::new();
    for name in ["a", "b", "c"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("a"),
        var("c"),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("b"),
        Expression::BuiltinCall {
            function: BuiltinFunction::NoEvent,
            args: vec![Expression::If {
                branches: vec![(
                    binop(OpBinary::Gt(Default::default()), var("a"), lit(0.0)),
                    lit(2.0),
                )],
                else_branch: Box::new(lit(-2.0)),
            }],
        },
    )));

    let ctx = build_runtime_direct_seed_context(&dae, 3, 0);
    let mut y = vec![99.0, 77.0, -3.0];
    let updates = seed_runtime_direct_assignment_values_with_context(&ctx, &dae, &mut y, &[], 0.0);

    assert!(updates > 0, "the non-branch intermediate may still seed");
    assert!(
        (y[0] + 3.0).abs() < 1.0e-12,
        "expected env-only intermediate a to seed from c, got {}",
        y[0]
    );
    assert!(
        (y[1] - 77.0).abs() < 1.0e-12,
        "branch target must stay unseeded when its condition depends on an env-only intermediate sourced from an unsolved solver unknown, got {}",
        y[1]
    );
}

#[test]
fn test_runtime_direct_assignment_seed_skips_alias_equations() {
    let mut dae = Dae::new();
    for name in ["clock_expr", "clock_alias"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    // Defining clock equation.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clock_expr"),
        Expression::FunctionCall {
            name: VarName::new("Clock"),
            args: vec![lit(0.02)],
            is_constructor: false,
        },
    )));
    // Pure alias equation that must not directionally overwrite the defining
    // relation in runtime direct-assignment seeding.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clock_expr"),
        var("clock_alias"),
    )));

    // Keep stale alias value in the environment.
    let mut y = vec![0.0, 1.0];
    let mut env = build_env(&dae, &y, &[], 0.01);
    env.set("clock_alias", 1.0);

    let _ = propagate_runtime_direct_assignments_from_env(&dae, &mut y, 0, &mut env);
    assert!(
        (env.vars.get("clock_expr").copied().unwrap_or(-1.0) - 0.0).abs() < 1.0e-12,
        "clock expression should stay driven by defining equation at non-tick time"
    );
}

#[test]
fn test_ic_seed_skips_alias_equations_for_runtime_clock_assignments() {
    let mut dae = Dae::new();
    for name in ["clock_expr", "clock_alias"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clock_expr"),
        Expression::FunctionCall {
            name: VarName::new("Clock"),
            args: vec![lit(0.02)],
            is_constructor: false,
        },
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("clock_expr"),
        var("clock_alias"),
    )));

    let mut y = vec![0.0, 1.0];
    let updates = seed_runtime_direct_assignments(&dae, &mut y, &[], 0, 0.01);
    assert!(
        updates <= 1,
        "alias equations should not drive extra seed updates"
    );
    assert!(
        y[0].abs() < 1.0e-12,
        "IC runtime seed should preserve non-tick value from defining clock equation"
    );
}

#[test]
fn test_runtime_direct_assignment_seed_skips_multi_definition_targets() {
    let mut dae = Dae::new();
    for name in ["u", "v"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    // Ambiguous target: both equations define `u` directionally.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("u"),
        var("time"),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("u"),
        binop(OpBinary::Add(Default::default()), var("v"), lit(0.0)),
    )));

    let mut y = vec![0.0, 1.0];
    let updates = seed_runtime_direct_assignments(&dae, &mut y, &[], 0, 0.6);
    assert_eq!(
        updates, 0,
        "ambiguous direct-assignment targets must not be seeded directionally"
    );
    assert!(
        y[0].abs() < 1.0e-12,
        "ambiguous target should remain untouched by runtime seed"
    );
}

#[test]
fn test_runtime_direct_assignment_seed_allows_unique_non_alias_target_definition() {
    let mut dae = Dae::new();
    for name in ["u", "v"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    // One non-alias defining equation plus one alias equality for the same target.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("u"),
        var("time"),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("u"),
        var("v"),
    )));

    let mut y = vec![0.0, 3.0];
    let updates = seed_runtime_direct_assignments(&dae, &mut y, &[], 0, 0.6);
    assert!(
        updates > 0,
        "unique non-alias target definition should be used for runtime seed"
    );
    assert!(
        (y[0] - 0.6).abs() < 1.0e-12,
        "runtime seed should follow the unique non-alias defining equation"
    );
}

#[test]
fn test_runtime_direct_propagation_skips_multi_definition_targets() {
    let mut dae = Dae::new();
    for name in ["u", "v"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    // Ambiguous target: both equations define `u` directionally.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("u"),
        var("time"),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("u"),
        binop(OpBinary::Add(Default::default()), var("v"), lit(0.0)),
    )));

    let mut y = vec![123.0, 1.0];
    let mut env = build_env(&dae, &y, &[], 0.6);
    env.set("u", 123.0);
    env.set("v", 1.0);

    let updates = propagate_runtime_direct_assignments_from_env(&dae, &mut y, 0, &mut env);
    assert_eq!(
        updates, 0,
        "ambiguous direct-assignment targets must not be propagated directionally"
    );
    assert!(
        (y[0] - 123.0).abs() < 1.0e-12,
        "ambiguous target should remain untouched by runtime propagation"
    );
}

#[test]
fn test_runtime_direct_propagation_allows_unique_non_alias_target_definition() {
    let mut dae = Dae::new();
    for name in ["u", "v"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    // One non-alias defining equation plus one alias equality for the same target.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("u"),
        var("time"),
    )));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("u"),
        var("v"),
    )));

    let mut y = vec![123.0, 5.0];
    let mut env = build_env(&dae, &y, &[], 0.6);
    env.set("u", 123.0);
    env.set("v", 5.0);

    let updates = propagate_runtime_direct_assignments_from_env(&dae, &mut y, 0, &mut env);
    assert!(
        updates > 0,
        "unique non-alias target definition should be used for runtime propagation"
    );
    assert!(
        (y[0] - 0.6).abs() < 1.0e-12,
        "runtime propagation should follow the unique non-alias defining equation"
    );
}
