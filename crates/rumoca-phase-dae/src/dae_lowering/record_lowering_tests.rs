use super::*;
use rumoca_core::{ClassType, Literal, Span, VarName};

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![],
        span: Span::DUMMY,
    }
}

fn record_constructor() -> rumoca_core::Function {
    let mut constructor = rumoca_core::Function::new("Pkg.Record", Span::DUMMY);
    constructor.is_constructor = true;
    constructor.add_input(rumoca_core::FunctionParam::new("a", "Real"));
    constructor.add_input(rumoca_core::FunctionParam::new("b", "Real"));
    constructor
}

fn function_with_record_input() -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("r", "Pkg.Record").with_type_class(ClassType::Record),
    );
    function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
    function
}

fn block_like_constructor() -> rumoca_core::Function {
    let mut constructor = rumoca_core::Function::new("Pkg.Divide", Span::DUMMY);
    constructor.is_constructor = true;
    constructor.add_input(
        rumoca_core::FunctionParam::new("u1", "RealInput").with_type_class(ClassType::Connector),
    );
    constructor.add_input(
        rumoca_core::FunctionParam::new("u2", "RealInput").with_type_class(ClassType::Connector),
    );
    constructor
}

fn assignment_to(name: &str, value: rumoca_core::Expression) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference {
            local: false,
            span: Span::DUMMY,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span: Span::DUMMY,
                subs: vec![],
            }],
            def_id: None,
        },
        value,
        span: Span::DUMMY,
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
                args: vec![var_ref("u")],
                is_constructor: true,
                span: Span::DUMMY,
            }],
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    let prepared = prepare_dae_for_codegen(&dae);

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
            args: vec![var_ref("u")],
            is_constructor: true,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    let prepared = prepare_dae_for_codegen(&dae);

    assert!(matches!(
        &prepared.as_dae().continuous.equations[0].rhs,
        rumoca_core::Expression::FunctionCall { name, .. } if name.as_str() == "Pkg.Record"
    ));
}

#[test]
fn dae_record_param_lowering_uses_constructor_signature_metadata() {
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
            args: vec![var_ref("rec")],
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    lower_record_function_params_dae(&mut dae);

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
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.a"
    ));
    assert!(matches!(
        &args[1],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.b"
    ));
}

#[test]
fn prepare_dae_for_codegen_does_not_mutate_simulation_dae() {
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
            args: vec![var_ref("rec")],
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    let prepared = prepare_dae_for_codegen(&dae);

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
                span: Span::DUMMY,
            }],
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    lower_record_function_params_dae(&mut dae);

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
fn dae_record_param_lowering_infers_fields_from_already_lowered_body() {
    let mut dae = Dae::default();
    let mut function = function_with_record_input();
    function.body.push(assignment_to(
        "y",
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var_ref("r_T")),
            rhs: Box::new(rumoca_core::Expression::Index {
                base: Box::new(var_ref("r_X")),
                subscripts: vec![rumoca_core::Subscript::Expr {
                    expr: Box::new(rumoca_core::Expression::Literal {
                        value: Literal::Integer(1),
                        span: Span::DUMMY,
                    }),
                    span: Span::DUMMY,
                }],
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        },
    ));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.f"), function);
    dae.continuous.equations.push(rumoca_ir_dae::Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.f").into(),
            args: vec![var_ref("rec")],
            is_constructor: false,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    lower_record_function_params_dae(&mut dae);

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
fn dae_record_param_lowering_merges_metadata_and_body_inferred_fields() {
    let mut dae = Dae::default();
    let mut constructor = rumoca_core::Function::new("Pkg.Record", Span::DUMMY);
    constructor.is_constructor = true;
    constructor.add_input(rumoca_core::FunctionParam::new("p", "Real"));
    constructor.add_input(rumoca_core::FunctionParam::new("T", "Real"));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.Record"), constructor);

    let mut function = function_with_record_input();
    function.body.push(assignment_to(
        "y",
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var_ref("r_T")),
            rhs: Box::new(var_ref("r_X")),
            span: Span::DUMMY,
        },
    ));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.f"), function);

    lower_record_function_params_dae(&mut dae);

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
    let mut constructor = rumoca_core::Function::new("Pkg.Record", Span::DUMMY);
    constructor.is_constructor = true;
    constructor.add_input(rumoca_core::FunctionParam::new("p", "Real"));
    constructor.add_input(rumoca_core::FunctionParam::new("T", "Real"));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.Record"), constructor);

    let mut callee = rumoca_core::Function::new("Pkg.g", Span::DUMMY);
    callee.add_input(
        rumoca_core::FunctionParam::new("state", "Pkg.Record").with_type_class(ClassType::Record),
    );
    callee.add_output(rumoca_core::FunctionParam::new("y", "Real"));
    callee.body.push(assignment_to("y", var_ref("state_X")));
    dae.symbols.functions.insert(VarName::new("Pkg.g"), callee);

    let mut caller = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    caller.add_input(
        rumoca_core::FunctionParam::new("state", "Pkg.Record").with_type_class(ClassType::Record),
    );
    caller.add_output(rumoca_core::FunctionParam::new("y", "Real"));
    caller.body.push(assignment_to(
        "y",
        rumoca_core::Expression::FunctionCall {
            name: VarName::new("Pkg.g").into(),
            args: vec![var_ref("state")],
            is_constructor: false,
            span: Span::DUMMY,
        },
    ));
    dae.symbols.functions.insert(VarName::new("Pkg.f"), caller);

    lower_record_function_params_dae(&mut dae);

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
    assert!(matches!(
        &args[2],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "state_X"
    ));
}

#[test]
fn dae_record_param_lowering_infers_air_state_x_width_from_indexed_body_use() {
    let mut dae = Dae::default();

    let mut function =
        rumoca_core::Function::new("Buildings.Media.Air.specificHeatCapacityCp", Span::DUMMY);
    function.add_input(rumoca_core::FunctionParam::new(
        "state_p",
        "AbsolutePressure",
    ));
    function.add_input(rumoca_core::FunctionParam::new("state_T", "Temperature"));
    function.add_output(rumoca_core::FunctionParam::new(
        "cp",
        "SpecificHeatCapacity",
    ));
    function.body.push(assignment_to(
        "cp",
        rumoca_core::Expression::Index {
            base: Box::new(var_ref("state_X")),
            subscripts: vec![rumoca_core::Subscript::Expr {
                expr: Box::new(rumoca_core::Expression::Literal {
                    value: Literal::Integer(1),
                    span: Span::DUMMY,
                }),
                span: Span::DUMMY,
            }],
            span: Span::DUMMY,
        },
    ));
    dae.symbols.functions.insert(
        VarName::new("Buildings.Media.Air.specificHeatCapacityCp"),
        function,
    );

    lower_record_function_params_dae(&mut dae);

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
