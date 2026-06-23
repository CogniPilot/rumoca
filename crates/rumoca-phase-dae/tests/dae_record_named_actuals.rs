use rumoca_core::{ClassType, Expression, Function, FunctionParam, Span, VarName};
use rumoca_ir_dae::{Dae, Equation};
use rumoca_phase_dae::lower_record_function_params_dae;

fn test_span(start: usize) -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name(file!()),
        start,
        start + 5,
    )
}

fn var_ref(name: &str, span: Span) -> Expression {
    Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![],
        span,
    }
}

fn named_arg(name: &str, value: Expression, span: Span) -> Expression {
    Expression::FunctionCall {
        name: VarName::new(format!("__rumoca_named_arg__.{name}")).into(),
        args: vec![value],
        is_constructor: true,
        span,
    }
}

fn record_constructor(span: Span) -> Function {
    let mut constructor = Function::new("Pkg.Record", span);
    constructor.is_constructor = true;
    constructor.add_input(FunctionParam::new("a", "Real", span));
    constructor.add_input(FunctionParam::new("b", "Real", span));
    constructor
}

fn function_with_record_input(span: Span) -> Function {
    let mut function = Function::new("Pkg.f", span);
    function
        .add_input(FunctionParam::new("r", "Pkg.Record", span).with_type_class(ClassType::Record));
    function.add_output(FunctionParam::new("y", "Real", span));
    function
}

fn dae_with_record_call(arg: Expression, span: Span) -> Dae {
    let mut dae = Dae::default();
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.Record"), record_constructor(span));
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.f"), function_with_record_input(span));
    dae.continuous.equations.push(Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: Expression::FunctionCall {
            name: VarName::new("Pkg.f").into(),
            args: vec![arg],
            is_constructor: false,
            span,
        },
        span,
        origin: "test".to_string(),
        scalar_count: 1,
    });
    dae
}

#[test]
fn dae_record_param_lowering_expands_named_record_actual_value() {
    let span = test_span(41);
    let mut dae = dae_with_record_call(named_arg("r", var_ref("rec", span), span), span);

    lower_record_function_params_dae(&mut dae).expect("named record argument should lower");

    let Expression::FunctionCall { args, .. } = &dae.continuous.equations[0].rhs else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[0],
        Expression::VarRef { name, .. } if name.as_str() == "rec.a"
    ));
    assert!(matches!(
        &args[1],
        Expression::VarRef { name, .. } if name.as_str() == "rec.b"
    ));
}

#[test]
fn dae_record_param_lowering_expands_named_record_field_actual_value() {
    let span = test_span(51);
    let field_actual = Expression::FieldAccess {
        base: Box::new(var_ref("plant.unit", span)),
        field: "recordValue".to_string(),
        span,
    };
    let mut dae = dae_with_record_call(named_arg("r", field_actual, span), span);

    lower_record_function_params_dae(&mut dae).expect("named field argument should lower");

    let Expression::FunctionCall { args, .. } = &dae.continuous.equations[0].rhs else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    assert!(matches!(
        &args[0],
        Expression::FieldAccess { field, .. } if field == "a"
    ));
    assert!(matches!(
        &args[1],
        Expression::FieldAccess { field, .. } if field == "b"
    ));
}
