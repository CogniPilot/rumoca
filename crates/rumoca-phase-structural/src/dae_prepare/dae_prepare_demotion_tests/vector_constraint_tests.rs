use super::*;

#[test]
fn test_singular_holonomic_reduction_demotes_dependent_state_pair() {
    let mut dae = Dae::new();
    for name in ["q", "w"] {
        let mut state = test_variable(name);
        state.state_select = rumoca_core::StateSelect::Always;
        dae.variables.states.insert(VarName::new(name), state);
    }
    for name in ["s", "v"] {
        let mut state = test_variable(name);
        state.state_select = rumoca_core::StateSelect::Prefer;
        dae.variables.states.insert(VarName::new(name), state);
    }
    for name in ["aq", "av"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }
    dae.continuous.equations.push(eq(sub(der("q"), var("w"))));
    dae.continuous.equations.push(eq(sub(der("w"), var("aq"))));
    dae.continuous.equations.push(eq(sub(der("s"), var("v"))));
    dae.continuous.equations.push(eq(sub(der("v"), var("av"))));
    dae.continuous.equations.push(eq(sub(
        mul(var("s"), var("s")),
        Expression::BuiltinCall {
            function: BuiltinFunction::Sin,
            args: vec![var("q")],
            span: Span::DUMMY,
        },
    )));

    let candidates = crate::dae_prepare::singular_holonomic_state_candidates(&dae)
        .expect("the position/velocity state-selection chain should be constructible");
    let selected = candidates
        .into_iter()
        .find(|candidate| candidate.demoted_states.len() == 2)
        .expect("the complete second-order constraint chain should be present");
    dae = selected.dae;

    assert!(dae.variables.states.contains_key(&VarName::new("q")));
    assert!(dae.variables.states.contains_key(&VarName::new("w")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("s")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("v")));
    assert_eq!(
        dae.continuous
            .equations
            .iter()
            .filter(|equation| equation.origin.contains("d_dt_holonomic_constraint"))
            .count(),
        2
    );
}

#[test]
fn test_singular_holonomic_reduction_preserves_vector_constraint_rank() {
    let mut dae = Dae::new();
    let mut position = test_variable("position");
    position.dims = vec![3];
    position.state_select = rumoca_core::StateSelect::Avoid;
    dae.variables
        .states
        .insert(VarName::new("position"), position);
    let mut velocity = test_variable("velocity");
    velocity.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("velocity"), velocity);

    let mut ode = eq(sub(der("position"), var("velocity")));
    ode.scalar_count = 3;
    dae.continuous.equations.push(ode);
    for component in 1..=3 {
        dae.continuous
            .equations
            .push(eq(sub(var_idx("position", component), int(0))));
    }

    assert_eq!(
        build_der_value_map(&dae).get("position"),
        Some(&var("velocity")),
        "the aggregate ODE must provide the vector derivative value"
    );
    let structural_bindings = crate::static_eval::structural_scalar_bindings(&dae);
    let groups = crate::dae_prepare::dummy_derivative_group::holonomic_constraint_groups(
        &dae,
        &VarName::new("position"),
        &structural_bindings,
    )
    .expect("vector constraint grouping should succeed");
    assert_eq!(groups, vec![vec![1, 2, 3]]);
    let differentiated =
        crate::dae_prepare::dummy_derivative_group::differentiate_holonomic_constraint_group(
            &dae,
            &VarName::new("position"),
            &groups[0],
            &collect_residual_defining_expr_index(&dae),
            &structural_bindings,
            &HashMap::new(),
            &HashMap::new(),
        )
        .expect("vector constraint differentiation should succeed");
    assert!(
        differentiated.is_some(),
        "the complete vector constraint should have a closed derivative"
    );

    let candidates = crate::dae_prepare::singular_holonomic_state_candidates(&dae)
        .expect("the complete vector constraint should be differentiated as one rank-3 block");
    let selected = candidates
        .into_iter()
        .find(|candidate| {
            candidate
                .demoted_states
                .iter()
                .any(|(_, name)| name.as_str() == "position")
        })
        .expect("the vector position state should have a holonomic reduction candidate");

    assert!(
        selected
            .dae
            .variables
            .algebraics
            .contains_key(&VarName::new("position"))
    );
    let differentiated = selected
        .dae
        .continuous
        .equations
        .iter()
        .filter(|equation| equation.origin.contains("d_dt_holonomic_constraint"))
        .collect::<Vec<_>>();
    assert_eq!(differentiated.len(), 3);
    for component in 1..=3 {
        assert!(
            differentiated.iter().any(|equation| expr_contains_var(
                &equation.rhs,
                &VarName::new(format!("velocity[{component}]"))
            )),
            "the differentiated block must retain velocity component {component}"
        );
    }
    assert!(
        selected
            .dae
            .continuous
            .equations
            .iter()
            .all(|equation| !expr_contains_der_of(&equation.rhs, &VarName::new("position")))
    );
}

#[test]
fn test_index_reduction_normalizes_exact_derivative_alias_from_independent_constraint() {
    let mut dae = Dae::new();
    for name in ["x", "q"] {
        dae.variables
            .states
            .insert(VarName::new(name), test_variable(name));
    }
    for name in ["dx", "a", "b"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }

    dae.continuous.equations.push(eq(sub(var("dx"), der("x"))));
    dae.continuous.equations.push(eq(sub(der("q"), real(1.0))));
    dae.continuous
        .equations
        .push(eq(sub(var("a"), add(var("q"), var("x")))));
    dae.continuous.equations.push(eq(sub(var("a"), var("b"))));
    dae.continuous.equations.push(eq(sub(var("b"), var("q"))));

    let changed = index_reduce_missing_state_derivatives_once(&mut dae)
        .expect("index reduction should follow the exact derivative alias");

    assert_eq!(changed, 1);
    assert!(
        expr_contains_der_of(&dae.continuous.equations[2].rhs, &VarName::new("x")),
        "the differentiated constraint must expose the canonical state derivative"
    );
    assert_eq!(dae.initialization.equations.len(), 1);
    assert!(expr_contains_var(
        &dae.initialization.equations[0].rhs,
        &VarName::new("x")
    ));
}

#[test]
fn test_index_reduction_accepts_coupled_vector_constraint_rows() {
    let indexed_constraint_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("vector_constraint.mo"),
        120,
        131,
    );
    let mut dae = Dae::new();
    let mut x = test_variable("x");
    x.dims = vec![3];
    dae.variables.states.insert(VarName::new("x"), x);
    let mut y = test_variable("y");
    y.dims = vec![3];
    dae.variables.states.insert(VarName::new("y"), y);
    let mut distance = test_variable("distance");
    distance.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("distance"), distance);
    let mut vy = test_variable("vy");
    vy.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("vy"), vy);

    dae.continuous
        .equations
        .push(eq(sub(var("distance"), sub(var("y"), var("x")))));
    dae.continuous.equations.push(eq(sub(
        var_idx_with_span("distance", 3, indexed_constraint_span),
        int(0),
    )));
    dae.continuous
        .equations
        .push(eq(sub(var_idx("vy", 1), der_idx("y", 1))));
    dae.continuous
        .equations
        .push(eq(sub(var_idx("vy", 2), der_idx("y", 2))));
    dae.continuous
        .equations
        .push(eq(sub(var_idx("vy", 3), der_idx("y", 3))));

    let changed = index_reduce_missing_state_derivatives_once(&mut dae)
        .expect("index reduction should succeed");

    assert_eq!(changed, 1);
    let differentiated = &dae.continuous.equations[1].rhs;
    assert!(
        expr_contains_der_of(differentiated, &VarName::new("x[3]")),
        "indexed differentiated row should preserve the missing state derivative: {differentiated:?}"
    );
    assert!(
        !expr_contains_der_of(differentiated, &VarName::new("distance")),
        "indexed algebraic derivative should be projected from the defining array equation: {differentiated:?}"
    );
}

#[test]
fn test_index_reduction_recognizes_structured_derivative_target_width() {
    let mut dae = Dae::new();
    for name in ["x", "acceleration", "omega"] {
        let mut variable = test_variable(name);
        variable.dims = vec![3];
        if name == "x" {
            dae.variables.states.insert(VarName::new(name), variable);
        } else {
            dae.variables
                .algebraics
                .insert(VarName::new(name), variable);
        }
    }

    dae.continuous.equations.push(eq(sub(
        der("x"),
        sub(var("acceleration"), cross(var("omega"), var("x"))),
    )));
    dae.continuous
        .equations
        .push(eq(sub(mul(var("x"), var("x")), real(1.0))));

    let changed = index_reduce_missing_state_derivatives_once(&mut dae)
        .expect("index reduction should recognize the structured ODE");

    assert_eq!(changed, 0);
    assert!(dae.initialization.equations.is_empty());
    assert!(
        dae.continuous
            .equations
            .iter()
            .all(|equation| !equation.origin.contains("index_reduction"))
    );
}

#[test]
fn test_exact_alias_component_rewrites_derivative_of_non_state_alias_to_canonical_state() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("load.phi"), test_variable("load.phi"));
    dae.variables.algebraics.insert(
        VarName::new("load.flange_b.phi"),
        test_variable("load.flange_b.phi"),
    );
    dae.variables.algebraics.insert(
        VarName::new("speed.flange.phi"),
        test_variable("speed.flange.phi"),
    );
    dae.variables
        .outputs
        .insert(VarName::new("speed.w"), test_variable("speed.w"));

    // MLS §8 simple equality equations define one exact alias component:
    // load.phi = load.flange_b.phi = speed.flange.phi. Any derivative taken
    // through a non-state alias in that component must track the canonical
    // state trajectory before later derivative-alias cleanup runs.
    dae.continuous
        .equations
        .push(eq(sub(var("load.phi"), var("load.flange_b.phi"))));
    dae.continuous
        .equations
        .push(eq(sub(var("speed.flange.phi"), var("load.flange_b.phi"))));
    dae.continuous
        .equations
        .push(eq(sub(var("speed.w"), der("speed.flange.phi"))));

    let demoted =
        demote_exact_alias_component_states(&mut dae).expect("exact alias demotion should succeed");
    assert_eq!(demoted, 0, "single-state alias component should not demote");
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(var("speed.w"), der("load.phi"))),
        "derivative users of exact non-state aliases should be rewritten to the canonical state"
    );
    assert!(
        !dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(var("speed.w"), der("speed.flange.phi"))),
        "non-state derivative alias should not survive after exact alias rewrite"
    );
}

#[test]
fn test_exact_alias_component_propagates_start_to_canonical_state() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("c.v"), test_variable("c.v"));
    let mut alias = test_variable("v");
    alias.start = Some(int(0));
    dae.variables.algebraics.insert(VarName::new("v"), alias);

    // MLS §8 simple equalities define one exact alias component. If the chosen
    // canonical state lacks start/fixed metadata, it must inherit compatible
    // metadata from exact alias peers so initialization still observes the
    // declared start value on the shared trajectory.
    dae.continuous.equations.push(eq(sub(var("v"), var("c.v"))));

    let demoted =
        demote_exact_alias_component_states(&mut dae).expect("exact alias demotion should succeed");
    assert_eq!(demoted, 0, "single-state alias component should not demote");
    assert_eq!(
        dae.variables
            .states
            .get(&VarName::new("c.v"))
            .and_then(|var| var.start.clone()),
        Some(int(0))
    );
}

#[test]
fn test_constrained_dummy_state_names_reports_direct_state_constraint() {
    let mut dae = Dae::new();
    for name in ["x1", "x2", "x3"] {
        dae.variables
            .states
            .insert(VarName::new(name), test_variable(name));
    }

    dae.continuous
        .equations
        .push(eq(sub(var("x1"), sub(neg(var("x2")), var("x3")))));
    dae.continuous.equations.push(eq(sub(der("x1"), int(1))));
    dae.continuous.equations.push(eq(sub(der("x2"), int(1))));
    dae.continuous.equations.push(eq(sub(der("x3"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae).expect("analysis should succeed");

    assert_eq!(
        dummy_states,
        IndexSet::from(["x1".to_string()]),
        "direct algebraic state constraints should identify the dependent state"
    );
}

#[test]
fn test_constrained_dummy_state_names_follows_linear_alias_constraints() {
    let mut dae = Dae::new();
    for name in ["x1", "x2", "x3"] {
        dae.variables
            .states
            .insert(VarName::new(name), test_variable(name));
    }
    for name in ["i[1]", "i[2]", "i[3]"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }

    // Connector/current aliases expose the actual state relation only after
    // eliminating the non-state current variables:
    //   x1 + i[1] = 0
    //   x2 + i[2] = 0
    //   x3 + i[3] = 0
    //   i[1] + i[2] + i[3] = 0
    dae.continuous
        .equations
        .push(eq(add(var("x1"), var_idx("i", 1))));
    dae.continuous
        .equations
        .push(eq(add(var("x2"), var_idx("i", 2))));
    dae.continuous
        .equations
        .push(eq(add(var("x3"), var_idx("i", 3))));
    dae.continuous.equations.push(eq(add(
        add(var_idx("i", 1), var_idx("i", 2)),
        var_idx("i", 3),
    )));
    dae.continuous.equations.push(eq(sub(der("x1"), int(1))));
    dae.continuous.equations.push(eq(sub(der("x2"), int(1))));
    dae.continuous.equations.push(eq(sub(der("x3"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae).expect("analysis should succeed");

    assert_eq!(
        dummy_states,
        IndexSet::from(["x1".to_string()]),
        "linear alias constraints should identify one dependent state without model-specific names"
    );
}

#[test]
fn test_constrained_dummy_state_names_follows_constant_scaled_alias() {
    let mut dae = Dae::new();
    let mut preferred = test_variable("accelerate.v");
    preferred.state_select = rumoca_core::StateSelect::Prefer;
    dae.variables
        .states
        .insert(VarName::new("accelerate.v"), preferred);
    dae.variables
        .states
        .insert(VarName::new("mass.v"), test_variable("mass.v"));

    dae.continuous.equations.push(eq(sub(
        var("mass.v"),
        div(
            mul(neg(var("accelerate.v")), real(-1.0)),
            mul(real(-1.0), real(-1.0)),
        ),
    )));
    dae.continuous
        .equations
        .push(eq(sub(der("accelerate.v"), int(1))));
    dae.continuous
        .equations
        .push(eq(sub(der("mass.v"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae).expect("analysis should succeed");

    assert_eq!(
        dummy_states,
        IndexSet::from(["mass.v".to_string()]),
        "constant scale wrappers around an exact alias should not hide duplicate states"
    );
}

#[test]
fn test_constrained_dummy_derivative_reduction_reaches_fixed_point() {
    let mut dae = Dae::new();
    for name in ["accelerate.s", "accelerate.v"] {
        let mut state = test_variable(name);
        state.state_select = rumoca_core::StateSelect::Prefer;
        dae.variables.states.insert(VarName::new(name), state);
    }
    for name in ["mass.s", "mass.v"] {
        dae.variables
            .states
            .insert(VarName::new(name), test_variable(name));
    }
    dae.variables
        .parameters
        .insert(VarName::new("mass.L"), test_variable("mass.L"));

    dae.continuous
        .equations
        .push(eq(sub(der("accelerate.s"), var("accelerate.v"))));
    dae.continuous
        .equations
        .push(eq(sub(der("accelerate.v"), int(1))));
    dae.continuous
        .equations
        .push(eq(sub(der("mass.s"), var("mass.v"))));
    dae.continuous
        .equations
        .push(eq(sub(der("mass.v"), int(1))));
    dae.continuous.equations.push(eq(sub(
        var("accelerate.s"),
        sub(var("mass.s"), div(var("mass.L"), int(2))),
    )));
    dae.continuous.equations.push(eq(sub(
        var("mass.v"),
        div(
            mul(neg(var("accelerate.v")), real(-1.0)),
            mul(real(-1.0), real(-1.0)),
        ),
    )));

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("constrained dummy reduction should succeed");

    assert_eq!(
        demoted, 2,
        "position and velocity alias states should both be reduced before BLT"
    );
    assert!(!dae.variables.states.contains_key(&VarName::new("mass.s")));
    assert!(!dae.variables.states.contains_key(&VarName::new("mass.v")));
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("mass.s"))
    );
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("mass.v"))
    );
}

#[test]
fn test_constrained_dummy_derivative_reduction_handles_parameter_guarded_vector_constraint() {
    let mut dae = Dae::new();
    let mut constrained = test_variable("x");
    constrained.dims = vec![3];
    constrained.state_select = rumoca_core::StateSelect::Avoid;
    dae.variables.states.insert(VarName::new("x"), constrained);
    let mut independent = test_variable("y");
    independent.dims = vec![3];
    dae.variables.states.insert(VarName::new("y"), independent);
    let mut mass = test_variable("m");
    mass.start = Some(real(0.5));
    mass.is_tunable = true;
    dae.variables.parameters.insert(VarName::new("m"), mass);

    dae.continuous.equations.push(Equation {
        lhs: None,
        rhs: Expression::If {
            branches: vec![(gt(var("m"), int(0)), sub(var("x"), var("y")))],
            else_branch: Box::new(sub(var("x"), array(vec![int(0), int(0), int(0)]))),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "parameter-guarded vector constraint".to_string(),
        scalar_count: 3,
    });
    dae.continuous.equations.push(Equation {
        lhs: None,
        rhs: sub(der("x"), der("y")),
        span: Span::DUMMY,
        origin: "constrained vector derivative".to_string(),
        scalar_count: 3,
    });
    dae.continuous.equations.push(Equation {
        lhs: None,
        rhs: sub(der("y"), array(vec![int(1), int(2), int(3)])),
        span: Span::DUMMY,
        origin: "independent vector derivative".to_string(),
        scalar_count: 3,
    });

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("parameter-guarded vector constraint should be reducible");

    assert_eq!(demoted, 1);
    assert!(!dae.variables.states.contains_key(&VarName::new("x")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("x")));
    assert!(
        dae.continuous
            .equations
            .iter()
            .all(|equation| !expr_contains_der_of(&equation.rhs, &VarName::new("x")))
    );
}

#[test]
fn test_complete_dummy_derivative_group_preserves_position_and_adds_velocity_constraint() {
    let mut dae = Dae::new();
    for index in 1..=3 {
        let core_name = format!("q{index}");
        let mut core = test_variable(&core_name);
        core.state_select = rumoca_core::StateSelect::Prefer;
        dae.variables.states.insert(VarName::new(&core_name), core);

        let dummy_name = format!("dummy{index}");
        let mut dummy = test_variable(&dummy_name);
        dummy.state_select = rumoca_core::StateSelect::Never;
        dae.variables
            .states
            .insert(VarName::new(&dummy_name), dummy);

        let velocity_name = format!("velocity{index}");
        dae.variables
            .algebraics
            .insert(VarName::new(&velocity_name), test_variable(&velocity_name));
        dae.continuous
            .equations
            .push(eq(sub(var(&velocity_name), der(&dummy_name))));
        dae.continuous
            .equations
            .push(eq(sub(der(&core_name), int(index))));
        dae.continuous
            .equations
            .push(eq(sub(var_idx("position", index), var(&dummy_name))));
    }
    let mut position = test_variable("position");
    position.dims = vec![3];
    position.state_select = rumoca_core::StateSelect::Never;
    dae.variables
        .algebraics
        .insert(VarName::new("position"), position);
    let position_constraint = sub(
        var("position"),
        array((1..=3).map(|index| var(&format!("q{index}"))).collect()),
    );
    dae.continuous.equations.push(Equation {
        lhs: None,
        rhs: position_constraint.clone(),
        span: Span::DUMMY,
        origin: "holonomic position constraint".to_string(),
        scalar_count: 3,
    });
    let original_equation_count = dae.continuous.equations.len();

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("the complete vector dummy-derivative group should reduce");

    assert_eq!(demoted, 3);
    assert_eq!(dae.continuous.equations.len(), original_equation_count + 1);
    assert!(dae.continuous.equations.iter().any(|equation| {
        equation.origin == "holonomic position constraint" && equation.rhs == position_constraint
    }));
    let differentiated = dae
        .continuous
        .equations
        .iter()
        .find(|equation| {
            equation
                .origin
                .contains("d_dt_complete_dummy_derivative_group")
        })
        .expect("the differentiated velocity constraint should be appended");
    for index in 1..=3 {
        let dummy = VarName::new(format!("dummy{index}"));
        assert!(!dae.variables.states.contains_key(&dummy));
        assert!(dae.variables.algebraics.contains_key(&dummy));
        assert!(
            dae.continuous
                .equations
                .iter()
                .all(|equation| !expr_contains_der_of(&equation.rhs, &dummy))
        );
        assert!(expr_contains_var(
            &differentiated.rhs,
            &VarName::new(format!("velocity{index}"))
        ));
    }
}

#[test]
fn test_constrained_dummy_prefers_viable_duplicate_definition() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x"));
    dae.variables
        .states
        .insert(VarName::new("q"), test_variable("q"));
    for name in ["bad_alias", "good_alias"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }

    // Both rows constrain x to the same q trajectory. The first route passes
    // through a self-dependent algebraic definition, so differentiating it
    // would retain der(x); the later direct route has the closed derivative
    // der(q) and must win duplicate-definition selection.
    dae.continuous
        .equations
        .push(eq(sub(var("x"), var("bad_alias"))));
    dae.continuous.equations.push(eq(sub(
        var("bad_alias"),
        sub(add(var("x"), var("q")), var("q")),
    )));
    dae.continuous
        .equations
        .push(eq(sub(var("x"), var("good_alias"))));
    dae.continuous
        .equations
        .push(eq(sub(var("good_alias"), var("q"))));
    dae.continuous.equations.push(eq(sub(der("x"), der("q"))));
    dae.continuous.equations.push(eq(sub(der("q"), int(1))));

    let definitions = constrained_dummy_state_defining_exprs(&dae)
        .expect("duplicate constrained-state definitions should be analyzable");
    assert_eq!(
        definitions
            .get(&VarName::new("x"))
            .and_then(|definition| definition.aggregate_defining_expr.as_ref()),
        Some(&var("good_alias")),
        "a viable closed derivative plan should replace an earlier unusable definition"
    );

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("the viable duplicate definition should support reduction");
    assert_eq!(demoted, 1);
    assert!(dae.variables.algebraics.contains_key(&VarName::new("x")));
}

#[test]
fn test_constrained_dummy_derivative_reduction_rejects_time_guarded_constraint() {
    let mut dae = Dae::new();
    for name in ["x", "y"] {
        dae.variables
            .states
            .insert(VarName::new(name), test_variable(name));
    }
    dae.continuous.equations.push(eq(Expression::If {
        branches: vec![(gt(var("time"), int(0)), sub(var("x"), var("y")))],
        else_branch: Box::new(sub(var("x"), int(0))),
        span: Span::DUMMY,
    }));
    dae.continuous.equations.push(eq(sub(der("x"), der("y"))));
    dae.continuous.equations.push(eq(sub(der("y"), int(1))));

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("dynamic constraint analysis should remain well formed");

    assert_eq!(demoted, 0);
    assert!(dae.variables.states.contains_key(&VarName::new("x")));

    let directly_demoted = demote_direct_assigned_states(&mut dae)
        .expect("dynamic direct-assignment analysis should remain well formed");
    assert_eq!(directly_demoted, 0);
    assert!(dae.variables.states.contains_key(&VarName::new("x")));
}

#[test]
fn test_direct_demotion_reaches_semantic_fixed_point_beyond_eight_rounds() {
    let mut dae = Dae::new();
    for index in 0..12 {
        let state = format!("s{index}");
        dae.variables
            .states
            .insert(VarName::new(&state), test_variable(&state));
        dae.continuous.equations.push(eq(sub(der(&state), int(1))));
        if index == 11 {
            dae.continuous
                .equations
                .push(eq(sub(var(&state), var("time"))));
            continue;
        }
        dae.continuous.equations.push(eq(sub(
            var(&state),
            Expression::BuiltinCall {
                function: BuiltinFunction::Sin,
                args: vec![var(&format!("s{}", index + 1))],
                span: Span::DUMMY,
            },
        )));
    }

    let demoted = demote_direct_assigned_states(&mut dae)
        .expect("finite direct-demotion chain should reach closure");

    assert_eq!(demoted, 12);
    assert!(dae.variables.states.is_empty());
}

#[test]
fn test_constrained_dummy_state_names_maps_singleton_array_component_state() {
    let mut dae = Dae::new();
    let mut x = test_variable("x");
    x.dims = vec![1];
    dae.variables.states.insert(VarName::new("x"), x);
    dae.variables
        .states
        .insert(VarName::new("y"), test_variable("y"));

    dae.continuous
        .equations
        .push(eq(sub(var_idx("x", 1), var("y"))));
    dae.continuous
        .equations
        .push(eq(sub(der_idx("x", 1), int(1))));
    dae.continuous.equations.push(eq(sub(der("y"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae).expect("analysis should succeed");

    assert_eq!(
        dummy_states.iter().next().map(String::as_str),
        Some("x"),
        "singleton state array constraints should map the indexed component back to the selected parent state"
    );
}

#[test]
fn test_constrained_dummy_derivative_reduction_rewrites_indexed_singleton_derivative() {
    let mut dae = Dae::new();
    let mut x = test_variable("x");
    x.dims = vec![1];
    dae.variables.states.insert(VarName::new("x"), x);
    dae.variables
        .states
        .insert(VarName::new("y"), test_variable("y"));

    dae.continuous
        .equations
        .push(eq(sub(var_idx("x", 1), var("y"))));
    dae.continuous
        .equations
        .push(eq(sub(der_idx("x", 1), int(1))));
    dae.continuous.equations.push(eq(sub(der("y"), int(1))));

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("constrained dummy reduction should succeed");

    assert_eq!(demoted, 1);
    assert!(!dae.variables.states.contains_key(&VarName::new("x")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("x")));
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.rhs == sub(der("y"), int(1))),
        "der(x[1]) should be rewritten to der(y) after singleton dummy-state demotion"
    );
}

#[test]
fn test_constrained_dummy_state_names_follows_singleton_array_alias_chain() {
    let mut dae = Dae::new();
    let mut filtered = test_variable("criticalDamping.x");
    filtered.dims = vec![1];
    dae.variables
        .states
        .insert(VarName::new("criticalDamping.x"), filtered);
    dae.variables.states.insert(
        VarName::new("firstOrder1.y"),
        test_variable("firstOrder1.y"),
    );
    for name in [
        "criticalDamping.y",
        "inverseBlockConstraints.u1",
        "inverseBlockConstraints.u2",
    ] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }

    dae.continuous.equations.push(eq(sub(
        var("criticalDamping.y"),
        var_sub("criticalDamping.x", var("criticalDamping.n")),
    )));
    dae.continuous.equations.push(eq(sub(
        var("criticalDamping.y"),
        var("inverseBlockConstraints.u1"),
    )));
    dae.continuous.equations.push(eq(sub(
        var("inverseBlockConstraints.u1"),
        var("inverseBlockConstraints.u2"),
    )));
    dae.continuous.equations.push(eq(sub(
        var("firstOrder1.y"),
        var("inverseBlockConstraints.u2"),
    )));
    dae.continuous.equations.push(eq(sub(
        der_idx("criticalDamping.x", 1),
        var("criticalDamping.u"),
    )));
    dae.continuous
        .equations
        .push(eq(sub(der("firstOrder1.y"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae).expect("analysis should succeed");

    assert_eq!(
        dummy_states.iter().next().map(String::as_str),
        Some("criticalDamping.x"),
        "linear alias chains should expose singleton array state constraints"
    );
}

#[test]
fn test_constrained_dummy_state_names_keeps_always_state() {
    let mut dae = Dae::new();
    let mut x1 = test_variable("x1");
    x1.state_select = rumoca_core::StateSelect::Always;
    dae.variables.states.insert(VarName::new("x1"), x1);
    dae.variables
        .states
        .insert(VarName::new("x2"), test_variable("x2"));

    dae.continuous.equations.push(eq(sub(var("x1"), var("x2"))));
    dae.continuous.equations.push(eq(sub(der("x1"), int(1))));
    dae.continuous.equations.push(eq(sub(der("x2"), int(1))));

    let dummy_states = constrained_dummy_state_names(&dae).expect("analysis should succeed");

    assert!(
        dummy_states.is_empty(),
        "StateSelect.always must not be downgraded by metadata state selection"
    );
}
