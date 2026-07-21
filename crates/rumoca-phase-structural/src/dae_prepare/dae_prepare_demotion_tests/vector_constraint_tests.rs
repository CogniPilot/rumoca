use super::*;

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
