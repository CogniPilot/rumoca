use super::*;

fn record_pair_function(
    assign_second_field: bool,
) -> indexmap::IndexMap<String, rumoca_core::Function> {
    let record_def = rumoca_core::DefId::new(9_001);
    let mut constructor = rumoca_core::Function::new("Pkg.Pair", rumoca_core::Span::DUMMY);
    constructor.def_id = Some(record_def);
    constructor.is_constructor = true;
    constructor.add_input(rumoca_core::FunctionParam::new(
        "a",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    constructor.add_input(rumoca_core::FunctionParam::new(
        "b",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));

    let mut function = rumoca_core::Function::new("Pkg.makePair", rumoca_core::Span::DUMMY);
    function.add_output(
        rumoca_core::FunctionParam::new(
            "state",
            "Pkg.Pair",
            rumoca_core::Span::source_free_serde_default(),
        )
        .with_type_class(rumoca_core::ClassType::Record)
        .with_type_def_id(record_def),
    );
    function.add_output(rumoca_core::FunctionParam::new(
        "valid",
        "Boolean",
        rumoca_core::Span::source_free_serde_default(),
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: comp_ref(&["state", "a"]),
        value: real(1.0),
        span: rumoca_core::Span::DUMMY,
    });
    if assign_second_field {
        function.body.push(rumoca_core::Statement::Assignment {
            comp: comp_ref(&["state", "b"]),
            value: real(2.0),
            span: rumoca_core::Span::DUMMY,
        });
    }
    function.body.push(rumoca_core::Statement::Assignment {
        comp: comp_ref(&["valid"]),
        value: bool_lit(true),
        span: rumoca_core::Span::DUMMY,
    });
    indexmap::IndexMap::from([
        ("Pkg.Pair".to_string(), constructor),
        ("Pkg.makePair".to_string(), function),
    ])
}

#[test]
fn multi_output_record_materialization_assigns_every_field() {
    let mut env = VarEnv::<f64>::new();
    env.functions = std::sync::Arc::new(record_pair_function(true));

    eval_statements(
        &[rumoca_core::Statement::FunctionCall {
            comp: comp_ref(&["Pkg", "makePair"]),
            args: Vec::new(),
            outputs: vec![comp_ref(&["result"]), comp_ref(&["ok"])],
            span: rumoca_core::Span::DUMMY,
        }],
        &mut env,
    )
    .expect("complete record output should materialize");

    assert_eq!(env_value(&env, "result.a"), 1.0);
    assert_eq!(env_value(&env, "result.b"), 2.0);
    assert_eq!(env_value(&env, "ok"), 1.0);
}

#[test]
fn partial_record_output_does_not_read_colliding_caller_binding() {
    let mut env = VarEnv::<f64>::new();
    env.functions = std::sync::Arc::new(record_pair_function(false));
    env.set("state.b", 99.0);

    let error = eval_statements(
        &[rumoca_core::Statement::FunctionCall {
            comp: comp_ref(&["Pkg", "makePair"]),
            args: Vec::new(),
            outputs: vec![comp_ref(&["result"]), comp_ref(&["ok"])],
            span: rumoca_core::Span::DUMMY,
        }],
        &mut env,
    )
    .expect_err("an unassigned record field must be rejected");

    assert_eq!(
        error,
        EvalError::MissingBinding {
            name: "state.b".to_string()
        }
    );
    assert!(env.get_optional("result.b").is_none());
}
