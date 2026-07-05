use super::*;
use rumoca_core::{ClassType, Literal, Span, VarName};

fn test_span(start: usize) -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name(file!()),
        start,
        start + 5,
    )
}

fn var_ref(name: &str, span: Span) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![],
        span,
    }
}

fn structured_var_ref(name: &str, span: Span) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(
            rumoca_core::ComponentReference::from_flat_segments(name, span, None),
        ),
        subscripts: vec![],
        span,
    }
}

fn record_constructor() -> rumoca_core::Function {
    let mut constructor = rumoca_core::Function::new("Pkg.Record", test_span(1));
    constructor.is_constructor = true;
    constructor.add_input(rumoca_core::FunctionParam::new("a", "Real", test_span(1)));
    constructor.add_input(rumoca_core::FunctionParam::new("b", "Real", test_span(1)));
    constructor
}

fn function_with_record_input() -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("Pkg.f", test_span(1));
    function.add_input(
        rumoca_core::FunctionParam::new("r", "Pkg.Record", test_span(1))
            .with_type_class(ClassType::Record),
    );
    function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span(1)));
    function
}

fn function_with_array_input() -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("Pkg.g", test_span(1));
    function
        .add_input(rumoca_core::FunctionParam::new("u", "Real", test_span(1)).with_dims(vec![-1]));
    function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span(1)));
    function
}

fn block_like_constructor() -> rumoca_core::Function {
    let mut constructor = rumoca_core::Function::new("Pkg.Divide", test_span(1));
    constructor.is_constructor = true;
    constructor.add_input(
        rumoca_core::FunctionParam::new("u1", "RealInput", test_span(1))
            .with_type_class(ClassType::Connector),
    );
    constructor.add_input(
        rumoca_core::FunctionParam::new("u2", "RealInput", test_span(1))
            .with_type_class(ClassType::Connector),
    );
    constructor
}

fn assignment_to(name: &str, value: rumoca_core::Expression) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference {
            local: false,
            span: test_span(1),
            parts: vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span: test_span(1),
                subs: vec![],
            }],
            def_id: None,
        },
        value,
        span: test_span(1),
    }
}

#[test]
fn prepare_dae_for_codegen_unwraps_block_constructor_value_wrapper() {
    let mut dae = Dae::default();
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.Divide"), block_like_constructor());
    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.f").into(),
            args: vec![rumoca_core::Expression::FunctionCall {
                name: VarName::new("Pkg.Divide").into(),
                args: vec![var_ref("u", test_span(1))],
                is_constructor: true,
                span: test_span(1),
            }],
            is_constructor: false,
            span: test_span(1),
        },
        span: test_span(1),
        origin: "test".to_string(),
        scalar_count: 1,
    });

    let prepared = prepare_dae_for_codegen(&dae).expect("codegen preparation");

    let rumoca_core::Expression::FunctionCall { args, .. } =
        &prepared.as_dae().continuous.equations[0].rhs
    else {
        panic!("expected outer function call");
    };
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "u"
    ));
}

#[test]
fn prepare_dae_for_codegen_keeps_record_constructor_value() {
    let mut dae = Dae::default();
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.Record"), record_constructor());
    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.Record").into(),
            args: vec![var_ref("u", test_span(1))],
            is_constructor: true,
            span: test_span(1),
        },
        span: test_span(1),
        origin: "test".to_string(),
        scalar_count: 1,
    });

    let prepared = prepare_dae_for_codegen(&dae).expect("codegen preparation");

    assert!(matches!(
        &prepared.as_dae().continuous.equations[0].rhs,
        rumoca_core::Expression::FunctionCall { name, .. } if name.as_str() == "Pkg.Record"
    ));
}

#[test]
fn dae_record_param_lowering_uses_constructor_signature_metadata() {
    let span = test_span(1);
    let mut dae = Dae::default();
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.Record"), record_constructor());
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.f"), function_with_record_input());
    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.f").into(),
            args: vec![var_ref("rec", span)],
            is_constructor: false,
            span,
        },
        span,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    lower_record_function_params_dae(&mut dae).expect("record lowering should preserve spans");

    let function = dae
        .symbols
        .functions
        .get(&VarName::new("Pkg.f"))
        .expect("function remains");
    let input_names = function
        .inputs
        .iter()
        .map(|input| input.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(input_names, vec!["r_a", "r_b"]);
    let rumoca_core::Expression::FunctionCall { args, .. } = &dae.continuous.equations[0].rhs
    else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, span: arg_span, .. }
            if name.as_str() == "rec.a" && *arg_span == span
    ));
    assert!(matches!(
        &args[1],
        rumoca_core::Expression::VarRef { name, span: arg_span, .. }
            if name.as_str() == "rec.b" && *arg_span == span
    ));
}

#[test]
fn dae_record_param_lowering_rejects_unspanned_generated_fields() {
    let mut dae = Dae::default();
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.Record"), record_constructor());
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.f"), function_with_record_input());
    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.f").into(),
            args: vec![var_ref("rec", Span::DUMMY)],
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    let err = lower_record_function_params_dae(&mut dae)
        .expect_err("unspanned record argument expansion should fail");

    assert!(matches!(err, ToDaeError::RuntimeMetadataViolation { .. }));
    assert!(
        err.to_string()
            .contains("missing source provenance for DAE record argument expansion"),
        "error should explain missing record-argument provenance: {err}"
    );
}

#[test]
fn prepare_dae_for_codegen_does_not_mutate_simulation_dae() {
    let span = test_span(11);
    let mut dae = Dae::default();
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.Record"), record_constructor());
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.f"), function_with_record_input());
    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.f").into(),
            args: vec![var_ref("rec", span)],
            is_constructor: false,
            span,
        },
        span,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    let prepared =
        prepare_dae_for_codegen(&dae).expect("codegen preparation should preserve spans");

    let original_function = dae
        .symbols
        .functions
        .get(&VarName::new("Pkg.f"))
        .expect("original function remains");
    assert_eq!(original_function.inputs.len(), 1);

    let prepared_function = prepared
        .as_dae()
        .symbols
        .functions
        .get(&VarName::new("Pkg.f"))
        .expect("prepared function remains");
    let input_names = prepared_function
        .inputs
        .iter()
        .map(|input| input.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(input_names, vec!["r_a", "r_b"]);
}

#[test]
fn insert_array_size_args_rejects_unspanned_generated_size_call() {
    let mut dae = Dae::default();
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.g"), function_with_array_input());
    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.g").into(),
            args: vec![var_ref("u", Span::DUMMY)],
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    let err = insert_array_size_args_dae(&mut dae)
        .expect_err("unspanned array-size argument insertion should fail");

    assert!(matches!(err, ToDaeError::RuntimeMetadataViolation { .. }));
    assert!(
        err.to_string()
            .contains("missing source provenance for DAE array size argument"),
        "error should explain missing array-size provenance: {err}"
    );
}

#[test]
fn dae_record_param_lowering_leaves_unknown_record_metadata_unexpanded() {
    let mut dae = Dae::default();
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.f"), function_with_record_input());
    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.f").into(),
            args: vec![rumoca_core::Expression::Literal {
                value: Literal::Real(1.0),
                span: test_span(1),
            }],
            is_constructor: false,
            span: test_span(1),
        },
        span: test_span(1),
        origin: "test".to_string(),
        scalar_count: 1,
    });

    lower_record_function_params_dae(&mut dae).expect("unknown metadata should not rewrite calls");

    let function = dae
        .symbols
        .functions
        .get(&VarName::new("Pkg.f"))
        .expect("function remains");
    assert_eq!(function.inputs.len(), 1);
    let rumoca_core::Expression::FunctionCall { args, .. } = &dae.continuous.equations[0].rhs
    else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 1);
}

#[test]
fn dae_record_param_lowering_keeps_external_object_inputs_opaque() {
    let span = test_span(1);
    let mut dae = Dae::default();

    let mut external_constructor = rumoca_core::Function::new("Pkg.ExternalTable", span);
    external_constructor.is_constructor = true;
    external_constructor.add_input(rumoca_core::FunctionParam::new("table", "Real", span));
    external_constructor.add_input(rumoca_core::FunctionParam::new("fileName", "String", span));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.ExternalTable"), external_constructor);

    let mut record_typed_user = rumoca_core::Function::new("Pkg.recordUser", span);
    record_typed_user.add_input(
        rumoca_core::FunctionParam::new("recordish", "Pkg.ExternalTable", span)
            .with_type_class(ClassType::Record),
    );
    record_typed_user.add_output(rumoca_core::FunctionParam::new("y", "Real", span));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.recordUser"), record_typed_user);

    let mut external_user = rumoca_core::Function::new("Pkg.getTableValue", span);
    external_user.add_input(
        rumoca_core::FunctionParam::new("tableID", "Pkg.ExternalTable", span)
            .with_type_class(ClassType::Class),
    );
    external_user.add_input(rumoca_core::FunctionParam::new("column", "Integer", span));
    external_user.add_output(rumoca_core::FunctionParam::new("y", "Real", span));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.getTableValue"), external_user);

    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.getTableValue").into(),
            args: vec![
                var_ref("integerTable.combiTimeTable.tableID", span),
                rumoca_core::Expression::Literal {
                    value: Literal::Integer(1),
                    span,
                },
            ],
            is_constructor: false,
            span,
        },
        span,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    lower_record_function_params_dae(&mut dae).expect("record params lower");

    let external_user = dae
        .symbols
        .functions
        .get(&VarName::new("Pkg.getTableValue"))
        .expect("external-object function remains");
    let input_names = external_user
        .inputs
        .iter()
        .map(|input| input.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(input_names, vec!["tableID", "column"]);

    let rumoca_core::Expression::FunctionCall { args, .. } = &dae.continuous.equations[0].rhs
    else {
        panic!("expected external-object function call");
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, span: arg_span, .. }
            if name.as_str() == "integerTable.combiTimeTable.tableID" && *arg_span == span
    ));
}

#[test]
fn dae_record_param_lowering_infers_fields_from_already_lowered_body() {
    let mut dae = Dae::default();
    let mut function = function_with_record_input();
    function.body.push(assignment_to(
        "y",
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var_ref("r_T", test_span(1))),
            rhs: Box::new(rumoca_core::Expression::Index {
                base: Box::new(var_ref("r_X", test_span(1))),
                subscripts: vec![rumoca_core::Subscript::Expr {
                    expr: Box::new(rumoca_core::Expression::Literal {
                        value: Literal::Integer(1),
                        span: test_span(1),
                    }),
                    span: test_span(1),
                }],
                span: test_span(1),
            }),
            span: test_span(1),
        },
    ));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.f"), function);
    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.f").into(),
            args: vec![var_ref("rec", test_span(1))],
            is_constructor: false,
            span: test_span(1),
        },
        span: test_span(1),
        origin: "test".to_string(),
        scalar_count: 1,
    });

    lower_record_function_params_dae(&mut dae).expect("record params lower");

    let function = dae
        .symbols
        .functions
        .get(&VarName::new("Pkg.f"))
        .expect("function remains");
    let input_names = function
        .inputs
        .iter()
        .map(|input| input.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(input_names, vec!["r_T", "r_X"]);
    let rumoca_core::Expression::FunctionCall { args, .. } = &dae.continuous.equations[0].rhs
    else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.T"
    ));
    assert!(matches!(
        &args[1],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.X"
    ));
}

#[test]
fn dae_record_arg_expansion_projects_overexpanded_record_field_actual_to_base() {
    let mut out = Vec::new();
    expand_dae_record_arg(
        &structured_var_ref("pipe.flowModel.states.phase", test_span(1)),
        &[
            "phase".to_string(),
            "h".to_string(),
            "d".to_string(),
            "T".to_string(),
            "p".to_string(),
        ],
        &mut out,
        test_span(1),
    )
    .expect("record arg expansion should project to record base");

    let names = out
        .iter()
        .map(|expr| match expr {
            rumoca_core::Expression::VarRef { name, .. } => name.as_str().to_string(),
            other => panic!("expected VarRef, got {other:?}"),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        names,
        vec![
            "pipe.flowModel.states.phase",
            "pipe.flowModel.states.h",
            "pipe.flowModel.states.d",
            "pipe.flowModel.states.T",
            "pipe.flowModel.states.p",
        ]
    );
}

#[test]
fn dae_record_param_lowering_merges_metadata_and_body_inferred_fields() {
    let mut dae = Dae::default();
    let mut constructor = rumoca_core::Function::new("Pkg.Record", test_span(1));
    constructor.is_constructor = true;
    constructor.add_input(rumoca_core::FunctionParam::new("p", "Real", test_span(1)));
    constructor.add_input(rumoca_core::FunctionParam::new("T", "Real", test_span(1)));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.Record"), constructor);

    let mut function = function_with_record_input();
    function.body.push(assignment_to(
        "y",
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var_ref("r_T", test_span(1))),
            rhs: Box::new(var_ref("r_X", test_span(1))),
            span: test_span(1),
        },
    ));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.f"), function);

    lower_record_function_params_dae(&mut dae).expect("record params lower");

    let function = dae
        .symbols
        .functions
        .get(&VarName::new("Pkg.f"))
        .expect("function remains");
    let input_names = function
        .inputs
        .iter()
        .map(|input| input.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(input_names, vec!["r_p", "r_T", "r_X"]);
}

#[test]
fn dae_record_param_lowering_propagates_nested_callee_field_requirements() {
    let mut dae = Dae::default();
    let mut constructor = rumoca_core::Function::new("Pkg.Record", test_span(1));
    constructor.is_constructor = true;
    constructor.add_input(rumoca_core::FunctionParam::new("p", "Real", test_span(1)));
    constructor.add_input(rumoca_core::FunctionParam::new("T", "Real", test_span(1)));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.Record"), constructor);

    let mut callee = rumoca_core::Function::new("Pkg.g", test_span(1));
    callee.add_input(
        rumoca_core::FunctionParam::new("state", "Pkg.Record", test_span(1))
            .with_type_class(ClassType::Record),
    );
    callee.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span(1)));
    callee
        .body
        .push(assignment_to("y", var_ref("state_X", test_span(1))));
    dae.symbols.functions.insert(VarName::new("Pkg.g"), callee);

    let mut caller = rumoca_core::Function::new("Pkg.f", test_span(1));
    caller.add_input(
        rumoca_core::FunctionParam::new("state", "Pkg.Record", test_span(1))
            .with_type_class(ClassType::Record),
    );
    caller.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span(1)));
    caller.body.push(assignment_to(
        "y",
        rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.g").into(),
            args: vec![var_ref("state", test_span(1))],
            is_constructor: false,
            span: test_span(1),
        },
    ));
    dae.symbols.functions.insert(VarName::new("Pkg.f"), caller);

    lower_record_function_params_dae(&mut dae).expect("record params lower");

    let caller = dae
        .symbols
        .functions
        .get(&VarName::new("Pkg.f"))
        .expect("caller remains");
    let input_names = caller
        .inputs
        .iter()
        .map(|input| input.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(input_names, vec!["state_p", "state_T", "state_X"]);
    let rumoca_core::Statement::Assignment { value, .. } = &caller.body[0] else {
        panic!("expected assignment");
    };
    let rumoca_core::Expression::FunctionCall { args, .. } = value else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 3);
    assert!(
        matches!(
            &args[2],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "state.X"
        ),
        "unexpected propagated callee args: {args:#?}"
    );
}

#[test]
fn dae_record_param_lowering_infers_air_state_x_width_from_indexed_body_use() {
    let mut dae = Dae::default();

    let mut function =
        rumoca_core::Function::new("Buildings.Media.Air.specificHeatCapacityCp", test_span(1));
    function.add_input(rumoca_core::FunctionParam::new(
        "state_p",
        "AbsolutePressure",
        test_span(1),
    ));
    function.add_input(rumoca_core::FunctionParam::new(
        "state_T",
        "Temperature",
        test_span(1),
    ));
    function.add_output(rumoca_core::FunctionParam::new(
        "cp",
        "SpecificHeatCapacity",
        test_span(1),
    ));
    function.body.push(assignment_to(
        "cp",
        rumoca_core::Expression::Index {
            base: Box::new(var_ref("state_X", test_span(1))),
            subscripts: vec![rumoca_core::Subscript::Expr {
                expr: Box::new(rumoca_core::Expression::Literal {
                    value: Literal::Integer(1),
                    span: test_span(1),
                }),
                span: test_span(1),
            }],
            span: test_span(1),
        },
    ));
    dae.symbols.functions.insert(
        VarName::new("Buildings.Media.Air.specificHeatCapacityCp"),
        function,
    );

    lower_record_function_params_dae(&mut dae).expect("record params lower");

    let function = dae
        .symbols
        .functions
        .get(&VarName::new("Buildings.Media.Air.specificHeatCapacityCp"))
        .expect("function remains");
    let state_x = function
        .inputs
        .iter()
        .find(|input| input.name == "state_X")
        .expect("state_X input should be synthesized");
    assert_eq!(state_x.dims, vec![1]);
}
