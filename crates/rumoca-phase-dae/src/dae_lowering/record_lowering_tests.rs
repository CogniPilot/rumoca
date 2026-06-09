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
