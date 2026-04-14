use super::*;

#[test]
fn refresh_env_solver_and_parameter_values_updates_runtime_slots_only() {
    let mut dae = dae::Dae::default();
    dae.states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.parameters
        .insert(VarName::new("p"), dae::Variable::new(VarName::new("p")));
    dae.discrete_reals
        .insert(VarName::new("d"), dae::Variable::new(VarName::new("d")));

    let mut env = build_env(&dae, &[1.0, 2.0], &[3.0], 0.25);
    env.set("d", 9.0);

    refresh_env_solver_and_parameter_values(&mut env, &dae, &[4.0, 5.0], &[6.0], 0.75);

    assert_eq!(env.get("time"), 0.75);
    assert_eq!(env.get("x"), 4.0);
    assert_eq!(env.get("y"), 5.0);
    assert_eq!(env.get("p"), 6.0);
    assert_eq!(env.get("d"), 9.0);
}

#[test]
fn build_runtime_parameter_tail_env_populates_inputs_and_discretes_without_solver_slots() {
    let mut dae = dae::Dae::default();
    dae.states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));

    let mut p = dae::Variable::new(VarName::new("p"));
    p.start = Some(dae::Expression::Literal(dae::Literal::Real(0.0)));
    dae.parameters.insert(VarName::new("p"), p);

    let mut u = dae::Variable::new(VarName::new("u"));
    u.start = Some(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Add(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: VarName::new("p"),
            subscripts: vec![],
        }),
        rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
    });
    dae.inputs.insert(VarName::new("u"), u);

    let mut d = dae::Variable::new(VarName::new("d"));
    d.start = Some(dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Add(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: VarName::new("u"),
            subscripts: vec![],
        }),
        rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
    });
    dae.discrete_reals.insert(VarName::new("d"), d);

    let env = build_runtime_parameter_tail_env(&dae, &[3.0], 0.25);

    assert_eq!(env.get("time"), 0.25);
    assert_eq!(env.get("p"), 3.0);
    assert_eq!(env.get("u"), 4.0);
    assert_eq!(env.get("d"), 6.0);
    assert!(!env.vars.contains_key("x"));
}

#[test]
fn build_runtime_parameter_tail_env_prefers_pre_store_for_lowered_pre_parameters() {
    clear_pre_values();

    let mut dae = dae::Dae::default();
    let mut pre = dae::Variable::new(VarName::new("__pre__.reset"));
    pre.start = Some(dae::Expression::Literal(dae::Literal::Real(0.0)));
    dae.parameters.insert(VarName::new("__pre__.reset"), pre);

    set_pre_value("reset", 1.0);
    let env = build_runtime_parameter_tail_env(&dae, &[0.0], 0.0);

    assert_eq!(env.get("__pre__.reset"), 1.0);

    clear_pre_values();
}

#[test]
fn refresh_env_solver_and_parameter_values_refreshes_lowered_pre_parameters_from_pre_store() {
    clear_pre_values();

    let mut dae = dae::Dae::default();
    dae.states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.parameters.insert(
        VarName::new("__pre__.reset"),
        dae::Variable::new(VarName::new("__pre__.reset")),
    );

    let mut env = build_env(&dae, &[0.0], &[0.0], 0.0);
    set_pre_value("reset", 2.0);
    refresh_env_solver_and_parameter_values(&mut env, &dae, &[1.0], &[0.0], 0.5);

    assert_eq!(env.get("x"), 1.0);
    assert_eq!(env.get("__pre__.reset"), 2.0);

    clear_pre_values();
}

#[test]
fn build_runtime_parameter_tail_env_skips_zero_sized_parameter_slots() {
    let mut dae = dae::Dae::default();

    let mut dyn_arr = dae::Variable::new(VarName::new("dyn_arr"));
    dyn_arr.dims = vec![0];
    dae.parameters.insert(VarName::new("dyn_arr"), dyn_arr);

    dae.parameters.insert(
        VarName::new("table_id"),
        dae::Variable::new(VarName::new("table_id")),
    );

    let env = build_runtime_parameter_tail_env(&dae, &[4.0], 0.0);

    assert_eq!(env.get("table_id"), 4.0);
    assert!(!env.vars.contains_key("dyn_arr"));
}

#[test]
fn build_runtime_parameter_tail_env_binds_enum_parameters_without_numeric_slots() {
    let mut dae = dae::Dae::default();
    dae.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'X'".to_string(),
        2,
    );
    dae.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
        3,
    );

    let mut before = dae::Variable::new(VarName::new("before"));
    before.start = Some(dae::Expression::VarRef {
        name: VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'X'"),
        subscripts: vec![],
    });
    dae.parameters.insert(VarName::new("before"), before);

    let mut after = dae::Variable::new(VarName::new("after"));
    after.start = Some(dae::Expression::VarRef {
        name: VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
        subscripts: vec![],
    });
    dae.parameters.insert(VarName::new("after"), after);

    let mut u = dae::Variable::new(VarName::new("u"));
    u.start = Some(dae::Expression::VarRef {
        name: VarName::new("before"),
        subscripts: vec![],
    });
    dae.inputs.insert(VarName::new("u"), u);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("before"), 2.0);
    assert_eq!(env.get("after"), 3.0);
    assert_eq!(env.get("u"), 2.0);
}

#[test]
fn build_runtime_parameter_tail_env_binds_qualified_enum_parameters_without_numeric_slots() {
    let mut dae = dae::Dae::default();
    dae.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
        3,
    );
    dae.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
        4,
    );

    let mut before = dae::Variable::new(VarName::new("Enable.before"));
    before.start = Some(dae::Expression::VarRef {
        name: VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
        subscripts: vec![],
    });
    dae.parameters.insert(VarName::new("Enable.before"), before);

    let mut after = dae::Variable::new(VarName::new("Enable.after"));
    after.start = Some(dae::Expression::VarRef {
        name: VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'1'"),
        subscripts: vec![],
    });
    dae.parameters.insert(VarName::new("Enable.after"), after);

    let mut step_time = dae::Variable::new(VarName::new("Enable.stepTime"));
    step_time.start = Some(dae::Expression::Literal(dae::Literal::Real(1.0)));
    dae.parameters
        .insert(VarName::new("Enable.stepTime"), step_time);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("Enable.before"), 3.0);
    assert_eq!(env.get("Enable.after"), 4.0);
    assert_eq!(env.get("Enable.stepTime"), 1.0);
}

#[test]
fn build_runtime_parameter_tail_env_binds_counter_like_enum_parameter_chain() {
    let mut dae = dae::Dae::default();
    dae.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
        3,
    );

    let mut q0 = dae::Variable::new(VarName::new("Counter.q0"));
    q0.start = Some(dae::Expression::VarRef {
        name: VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
        subscripts: vec![],
    });
    dae.parameters.insert(VarName::new("Counter.q0"), q0);

    let mut ff_q0 = dae::Variable::new(VarName::new("Counter.FF[1].q0"));
    ff_q0.start = Some(dae::Expression::VarRef {
        name: VarName::new("Counter.q0"),
        subscripts: vec![],
    });
    dae.parameters
        .insert(VarName::new("Counter.FF[1].q0"), ff_q0);

    let mut td_y0 = dae::Variable::new(VarName::new("Counter.FF[1].RS1.TD1.y0"));
    td_y0.start = Some(dae::Expression::VarRef {
        name: VarName::new("Counter.q0"),
        subscripts: vec![],
    });
    dae.parameters
        .insert(VarName::new("Counter.FF[1].RS1.TD1.y0"), td_y0);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("Counter.q0"), 3.0);
    assert_eq!(env.get("Counter.FF[1].q0"), 3.0);
    assert_eq!(env.get("Counter.FF[1].RS1.TD1.y0"), 3.0);
}

#[test]
fn build_runtime_parameter_tail_env_broadcasts_enum_literal_to_discrete_array_start() {
    let mut dae = dae::Dae::default();
    dae.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
        1,
    );

    let mut auxiliary = dae::Variable::new(VarName::new("auxiliary"));
    auxiliary.dims = vec![3];
    auxiliary.start = Some(dae::Expression::VarRef {
        name: VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'U'"),
        subscripts: vec![],
    });
    dae.discrete_valued
        .insert(VarName::new("auxiliary"), auxiliary);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("auxiliary[1]"), 1.0);
    assert_eq!(env.get("auxiliary[2]"), 1.0);
    assert_eq!(env.get("auxiliary[3]"), 1.0);
}

#[test]
fn build_runtime_parameter_tail_env_binds_singleton_parameter_array_index_entries() {
    let mut dae = dae::Dae::default();

    let mut t_param = dae::Variable::new(VarName::new("a.t"));
    t_param.dims = vec![1];
    t_param.start = Some(dae::Expression::Array {
        elements: vec![dae::Expression::Literal(dae::Literal::Real(1.0))],
        is_matrix: false,
    });
    dae.parameters.insert(VarName::new("a.t"), t_param);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0);

    assert_eq!(env.get("a.t"), 1.0);
    assert_eq!(env.get("a.t[1]"), 1.0);
}

#[test]
fn build_env_skips_zero_sized_solver_slots() {
    let mut dae = dae::Dae::default();

    let mut dyn_out = dae::Variable::new(VarName::new("dyn_out"));
    dyn_out.dims = vec![0];
    dae.outputs.insert(VarName::new("dyn_out"), dyn_out);
    dae.outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    let env = build_env(&dae, &[7.0], &[], 0.0);

    assert_eq!(env.get("y"), 7.0);
    assert!(!env.vars.contains_key("dyn_out"));
}
