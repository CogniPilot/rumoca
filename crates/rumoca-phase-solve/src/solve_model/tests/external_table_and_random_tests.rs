use super::*;

fn dynamic_external_time_table_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    let mut table = scalar_var("table");
    table.dims = vec![4];
    table.start = Some(array_expr(
        vec![
            real_expr(1.0),
            real_expr(3.0),
            real_expr(5.0),
            real_expr(7.0),
        ],
        false,
    ));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("table"), table);

    let mut n = scalar_var("n");
    n.start = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![var("table"), int_expr(1)],
        span: solve_model_test_span(),
    });
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("n"), n);

    let mut table_dyn = scalar_var("table_dyn");
    table_dyn.dims = vec![0, 2];
    table_dyn.start = Some(dynamic_external_time_table_matrix());
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("table_dyn"), table_dyn);

    let mut table_id = scalar_var("table_id");
    table_id.start = Some(call_expr(
        "ExternalCombiTimeTable",
        vec![
            real_expr(0.0),
            real_expr(0.0),
            var("table_dyn"),
            real_expr(0.0),
            array_expr(vec![int_expr(2)], false),
            int_expr(3),
            int_expr(1),
        ],
    ));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("table_id"), table_id);
    dae_model
}

fn dynamic_boolean_time_table_dae() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    let mut table = scalar_var("table1.table");
    table.dims = vec![2];
    table.start = Some(array_expr(vec![real_expr(0.05), real_expr(0.15)], false));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("table1.table"), table);

    let mut n = scalar_var("table1.n");
    n.start = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![var("table1.table"), int_expr(1)],
        span: solve_model_test_span(),
    });
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("table1.n"), n);

    let mut table_dyn = scalar_var("table1.combiTimeTable.table");
    table_dyn.dims = vec![0, 2];
    table_dyn.start = Some(dynamic_boolean_time_table_matrix());
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("table1.combiTimeTable.table"),
        table_dyn,
    );

    let mut table_id = scalar_var("table1.combiTimeTable.tableID");
    table_id.start = Some(call_expr(
        "ExternalCombiTimeTable",
        vec![
            real_expr(0.0),
            real_expr(0.0),
            var("table1.combiTimeTable.table"),
            real_expr(0.0),
            array_expr(vec![int_expr(2)], false),
            int_expr(3),
            int_expr(1),
        ],
    ));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("table1.combiTimeTable.tableID"),
        table_id,
    );
    dae_model
}

fn dynamic_boolean_time_table_matrix() -> rumoca_core::Expression {
    let first_row = array_expr(vec![indexed_var("table1.table", 1), real_expr(0.0)], true);
    rumoca_core::Expression::If {
        branches: vec![(
            rumoca_core::Expression::Binary {
                op: OpBinary::Gt,
                lhs: Box::new(var("table1.n")),
                rhs: Box::new(real_expr(0.0)),
                span: solve_model_test_span(),
            },
            array_expr(
                vec![
                    first_row,
                    array_expr(
                        vec![
                            var("table1.table"),
                            dynamic_external_time_table_toggles("table1.n"),
                        ],
                        true,
                    ),
                ],
                true,
            ),
        )],
        else_branch: Box::new(array_expr(
            vec![array_expr(vec![real_expr(0.0), real_expr(0.0)], false)],
            true,
        )),
        span: solve_model_test_span(),
    }
}

fn dynamic_external_time_table_matrix() -> rumoca_core::Expression {
    let first_row = array_expr(vec![indexed_var("table", 1), real_expr(0.0)], false);
    rumoca_core::Expression::If {
        branches: vec![(
            rumoca_core::Expression::Binary {
                op: OpBinary::Gt,
                lhs: Box::new(var("n")),
                rhs: Box::new(real_expr(0.0)),
                span: solve_model_test_span(),
            },
            array_expr(
                vec![
                    first_row,
                    array_expr(
                        vec![var("table"), dynamic_external_time_table_toggles("n")],
                        false,
                    ),
                ],
                true,
            ),
        )],
        else_branch: Box::new(array_expr(
            vec![array_expr(vec![real_expr(0.0), real_expr(0.0)], false)],
            true,
        )),
        span: solve_model_test_span(),
    }
}

fn dynamic_external_time_table_toggles(n_name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Mod,
            args: vec![var("i"), real_expr(2.0)],
            span: solve_model_test_span(),
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int_expr(1)),
                step: None,
                end: Box::new(var(n_name)),
                span: solve_model_test_span(),
            },
        }],
        filter: None,
        span: solve_model_test_span(),
    }
}

#[test]
fn default_parameters_build_external_time_table_from_dynamic_matrix_start() {
    let dae_model = dynamic_external_time_table_dae();
    let runtime = Arc::new(EvalRuntimeState::default());
    let params = default_parameter_values(
        &dae_model,
        None,
        runtime.clone(),
        &std::collections::HashMap::new(),
    )
    .expect("parameter starts lower");
    assert_eq!(
        params.len(),
        6,
        "dynamic matrix parameter must not consume a flattened slot"
    );
    let table_id = *params.last().expect("table_id parameter");
    let env = build_runtime_parameter_tail_env_with_runtime(&dae_model, &params, 0.0, runtime)
        .expect("test runtime tail env should build");
    let tables = external_table_data_for_parameter_values_in(&env, &params);
    let next = rumoca_eval_dae::eval::eval_time_table_next_event_value_in(table_id, 0.0, &tables);
    let high = rumoca_eval_dae::eval::eval_table_lookup_value_in(table_id, 1.0, 2.0, &tables)
        .expect("table lookup");

    assert!(table_id > 0.0);
    assert!((next - 1.0).abs() <= 1.0e-12, "next={next}");
    assert!((high - 1.0).abs() <= 1.0e-12, "high={high}");
}

#[test]
fn default_parameters_build_boolean_time_table_with_duplicate_first_knot() {
    let dae_model = dynamic_boolean_time_table_dae();
    let runtime = Arc::new(EvalRuntimeState::default());
    let params = default_parameter_values(
        &dae_model,
        None,
        runtime.clone(),
        &std::collections::HashMap::new(),
    )
    .expect("parameter starts lower");
    let table_id = *params.last().expect("table_id parameter");
    let env = build_runtime_parameter_tail_env_with_runtime(&dae_model, &params, 0.0, runtime)
        .expect("test runtime tail env should build");
    let tables = external_table_data_for_parameter_values_in(&env, &params);
    let before = rumoca_eval_dae::eval::eval_table_lookup_value_in(table_id, 1.0, 0.049, &tables)
        .expect("table lookup before duplicate knot");
    let after = rumoca_eval_dae::eval::eval_table_lookup_value_in(table_id, 1.0, 0.050001, &tables)
        .expect("table lookup after duplicate knot");

    assert!(table_id > 0.0);
    assert!((before - 0.0).abs() <= 1.0e-12, "before={before}");
    assert!((after - 1.0).abs() <= 1.0e-12, "after={after}");
}

#[test]
fn default_parameters_build_external_time_table_from_constructor_marked_call_with_strings() {
    let mut dae_model = dynamic_boolean_time_table_dae();
    let table_id = dae_model
        .variables
        .parameters
        .get_mut(&rumoca_core::VarName::new("table1.combiTimeTable.tableID"))
        .expect("table ID parameter");
    table_id.start = Some(constructor_call_expr(
        "Modelica.Blocks.Types.ExternalCombiTimeTable",
        vec![
            string_expr("NoName"),
            string_expr("NoName"),
            var("table1.combiTimeTable.table"),
            real_expr(0.0),
            array_expr(vec![int_expr(2)], false),
            int_expr(3),
            int_expr(1),
        ],
    ));

    let runtime = Arc::new(EvalRuntimeState::default());
    let params = default_parameter_values(
        &dae_model,
        None,
        runtime.clone(),
        &std::collections::HashMap::new(),
    )
    .expect("parameter starts lower");
    let table_id = *params.last().expect("table_id parameter");
    let env = build_runtime_parameter_tail_env_with_runtime(&dae_model, &params, 0.0, runtime)
        .expect("test runtime tail env should build");
    let tables = external_table_data_for_parameter_values_in(&env, &params);
    let after = rumoca_eval_dae::eval::eval_table_lookup_value_in(table_id, 1.0, 0.050001, &tables)
        .expect("table lookup after duplicate knot");

    assert!(table_id > 0.0);
    assert!((after - 1.0).abs() <= 1.0e-12, "after={after}");
}

#[test]
fn lower_applies_initial_random_distribution_chain_to_runtime_parameters() {
    let dae_model = initial_random_distribution_chain_model();
    let prepared = lower_dae_to_solve_model(&dae_model)
        .expect("Solve IR lowering should apply direct initial equations");
    let layout = &prepared.problem.solve_layout;
    let r_raw = prepared.parameters[layout
        .discrete_real_parameter_index("r_raw")
        .expect("r_raw runtime parameter")];
    let r = prepared.parameters[layout
        .discrete_real_parameter_index("r")
        .expect("r runtime parameter")];
    let state_1 = prepared.parameters[layout
        .discrete_valued_parameter_index("state[1]")
        .expect("state[1] runtime parameter")];

    assert!(r_raw > 0.0 && r_raw < 1.0, "r_raw={r_raw}");
    assert!(r.is_finite(), "r={r}");
    assert!(state_1 > 0.0, "state_1={state_1}");
}

fn initial_random_distribution_chain_model() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    for (name, value) in [
        ("localSeed", 0.0),
        ("actualGlobalSeed", 0.0),
        ("mu", 0.0),
        ("sigma", 2.0),
    ] {
        let mut parameter = scalar_var(name);
        parameter.start = Some(real_expr(value));
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), parameter);
    }
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("state"),
        dae::Variable {
            dims: vec![4],
            start: Some(int_expr(0)),
            ..scalar_var("state")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.state"),
        dae::Variable {
            dims: vec![4],
            start: Some(int_expr(0)),
            ..scalar_var("__pre__.state")
        },
    );
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("r_raw"), scalar_var("r_raw"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("r"), scalar_var("r"));

    insert_initial_random_distribution_chain_equations(&mut dae_model);
    dae_model
}

fn insert_initial_random_distribution_chain_equations(dae_model: &mut dae::Dae) {
    dae_model
        .initialization
        .equations
        .push(dae::Equation::explicit(
            rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
                "localSeed",
            )),
            call_expr(
                "Modelica.Math.Random.Utilities.automaticLocalSeed",
                vec![string_expr("initial_random_distribution_chain_model")],
            ),
            solve_model_test_span(),
            "localSeed = automaticLocalSeed(\"initial_random_distribution_chain_model\")",
        ));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            sub(
                var("__pre__.state"),
                call_expr(
                    "Modelica.Math.Random.Generators.Xorshift128plus.initialState",
                    vec![var("localSeed"), var("actualGlobalSeed")],
                ),
            ),
            solve_model_test_span(),
            // MLS Appendix B.2.2: initial equations may initialize discrete
            // random state through pre(state), already lowered to the
            // `__pre__.state` parameter array at the Solve boundary.
            "__pre__.state = initialState(localSeed, actualGlobalSeed)",
        ));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::explicit(
            rumoca_core::Reference::from_component_reference(source_component_ref_from_name(
                "r_raw",
            )),
            call_expr(
                "Modelica.Math.Random.Generators.Xorshift128plus.random",
                vec![
                    var("__pre__.state"),
                    rumoca_core::Expression::BuiltinCall {
                        function: rumoca_core::BuiltinFunction::Size,
                        args: vec![var("__pre__.state"), int_expr(1)],
                        span: solve_model_test_span(),
                    },
                ],
            ),
            solve_model_test_span(),
            "r_raw = random(__pre__.state, size(__pre__.state, 1))",
        ));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::explicit(
            rumoca_core::Reference::from_component_reference(source_component_ref_from_name("r")),
            call_expr(
                "Modelica.Math.Distributions.Normal.quantile",
                vec![var("r_raw"), var("mu"), var("sigma")],
            ),
            solve_model_test_span(),
            "r = Normal.quantile(r_raw, mu, sigma)",
        ));
}
