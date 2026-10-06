use super::*;
use rumoca_core::{
    BuiltinFunction, ComponentRefPart, ComponentReference, DefId, Reference, SourceMap,
};

fn spans() -> (Span, Span) {
    let mut sources = SourceMap::new();
    let id = sources.add("guard.mo", "x[i] x[i] x[i+1]");
    (Span::from_offsets(id, 0, 4), Span::from_offsets(id, 5, 9))
}

fn reference(name: &str, index: &str, span: Span) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: vec![Subscript::Expr {
            expr: Box::new(Expression::VarRef {
                name: Reference::new(index),
                subscripts: Vec::new(),
                span,
            }),
            span,
        }],
        span,
    }
}

fn wrapped(function: BuiltinFunction, value: Expression) -> Expression {
    Expression::BuiltinCall {
        function,
        args: vec![value],
        span: Span::DUMMY,
    }
}

fn retained(then: Expression, otherwise: Expression) -> bool {
    let parameter = |name: &VarName| name.as_str() == "enabled";
    let unknown = |name: &VarName| matches!(name.as_str(), "x" | "y");
    let condition = Expression::VarRef {
        name: Reference::new("enabled"),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };
    retains_parameter_guard(
        &[(condition, then)],
        &otherwise,
        &GuardClasses {
            tunable_parameter: &parameter,
            unknown: &unknown,
        },
    )
}

#[test]
fn equal_index_incidence_ignores_source_locations() {
    let (a, b) = spans();
    assert_ne!(
        format!("{:?}", reference("x", "i", a)),
        format!("{:?}", reference("x", "i", b))
    );
    assert!(retained(reference("x", "i", a), reference("x", "i", b)));
}

#[test]
fn incidence_keeps_variable_index_and_operator_distinctions() {
    let (a, b) = spans();
    let x = reference("x", "i", a);
    assert!(!retained(x.clone(), reference("y", "i", b)));
    assert!(!retained(x.clone(), reference("x", "j", b)));
    let mut shifted = reference("x", "i", b);
    let Expression::VarRef { subscripts, .. } = &mut shifted else {
        unreachable!()
    };
    let Subscript::Expr { expr, .. } = &mut subscripts[0] else {
        unreachable!()
    };
    **expr = Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: expr.clone(),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(1),
            span: b,
        }),
        span: b,
    };
    assert!(!retained(x.clone(), shifted));
    assert!(!retained(
        x.clone(),
        wrapped(BuiltinFunction::Der, x.clone())
    ));
    assert!(!retained(
        x.clone(),
        wrapped(BuiltinFunction::Pre, x.clone())
    ));
    assert!(!retained(
        wrapped(BuiltinFunction::Der, x.clone()),
        wrapped(BuiltinFunction::Pre, x)
    ));
}

#[test]
fn incidence_is_a_set_without_source_order_or_duplicate_read_bias() {
    let (a, b) = spans();
    let sum = |lhs, rhs| Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: a,
    };
    let x = reference("x", "i", a);
    let y = reference("y", "i", b);
    assert!(retained(
        sum(x.clone(), y.clone()),
        sum(y.clone(), x.clone())
    ));
    assert!(retained(
        sum(sum(x.clone(), x.clone()), y.clone()),
        sum(x, y)
    ));
}

fn input_model(external: bool, connected: bool) -> flat::Model {
    let mut model = flat::Model::new();
    let mut parameter = flat::Variable::empty_with_span(Span::DUMMY);
    parameter.name = VarName::new("enabled");
    parameter.variability = Variability::Parameter(Default::default());
    model.add_variable(parameter.name.clone(), parameter);
    let mut input = flat::Variable::empty_with_span(Span::DUMMY);
    input.name = VarName::new("component.x");
    input.causality = Causality::Input(Default::default());
    input.connected = connected;
    input.component_ref = Some(
        ComponentReference::construct(
            false,
            Span::DUMMY,
            vec![
                ComponentRefPart {
                    ident: "component".into(),
                    span: Span::DUMMY,
                    subs: Vec::new(),
                    def_id: DefId::new(1),
                },
                ComponentRefPart {
                    ident: "x".into(),
                    span: Span::DUMMY,
                    subs: Vec::new(),
                    def_id: DefId::new(2),
                },
            ],
        )
        .unwrap(),
    );
    model.add_variable(input.name.clone(), input);
    if external {
        model.top_level_input_components.insert("component".into());
    }
    model
}

fn retained_input(model: &flat::Model) -> bool {
    let (a, b) = spans();
    retains_flat_guard(
        model,
        &Default::default(),
        &[(
            reference("enabled", "i", a),
            reference("component.x", "i", a),
        )],
        &Expression::Literal {
            value: Literal::Real(0.0),
            span: b,
        },
    )
}

#[test]
fn external_input_uses_the_exact_role_ownership() {
    assert!(retained_input(&input_model(true, false)));
    let mut connector = input_model(false, true);
    connector.top_level_connectors.insert("component".into());
    assert!(retained_input(&connector));
}

#[test]
fn connected_nested_input_keeps_unknown_incidence() {
    assert!(!retained_input(&input_model(false, true)));
    assert!(!retained_input(&input_model(false, false)));
}

#[test]
fn missing_input_root_cannot_grant_known_coordinate_authority() {
    let mut model = input_model(true, false);
    let name = VarName::new("component.x");
    model.variables.get_mut(&name).unwrap().component_ref = None;
    assert!(!retained_input(&model));
    assert!(
        super::super::super::analysis::is_external_input(&model, &name, &model.variables[&name])
            .is_err()
    );
}
