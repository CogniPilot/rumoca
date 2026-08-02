use rumoca_core::Reference;

use super::super::*;
use super::support::*;

#[test]
fn production_lowering_enters_only_through_construct() {
    let source = TestSource::new("model M Real x; equation 0 = x - 1.0; end M;");
    let model = scalar_real_model(&source);
    let dae = construct(&model, source.map).unwrap();

    dae.inspect(|view| {
        assert_eq!(view.variable_count(), 1);
        assert_eq!(view.continuous_equation_count(), 1);
        let variable = view.variable(view.variable_id(0).unwrap()).unwrap();
        assert_eq!(variable.role(), dae::VariableRole::Algebraic);
        let equation = view.continuous_equation(0).unwrap();
        assert_eq!(equation.provenance().span(), model.equations[0].span);
        assert_eq!(
            view.expression(equation.residual())
                .unwrap()
                .provenance()
                .origin(),
            dae::DaeProvenanceOrigin::Source
        );
    });
}

/// MLS §10.3.2 / ARR-015: a typed Flat `vector(A)` reaches the checked DAE
/// compact reshape owner. The source operand remains one rank-three expression;
/// neither Flat→DAE analysis nor construction scalarizes its payload.
#[test]
fn typed_flat_vector_call_reaches_the_checked_compact_dae_constructor() {
    let source =
        TestSource::new("model M parameter Real x[1,3,1]; parameter Real y[3] = vector(x); end M;");
    let call_span = source.span("vector(x)", 0);
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "x",
        "parameter Real x[1,3,1]",
        8,
        vec![1, 3, 1],
        false,
    );
    add_primitive_variable(
        &mut model,
        &source,
        "y",
        "parameter Real y[3]",
        7,
        vec![3],
        false,
    );
    model
        .variables
        .get_mut(&VarName::new("x"))
        .unwrap()
        .variability = Variability::Parameter(Default::default());
    let y = model.variables.get_mut(&VarName::new("y")).unwrap();
    y.variability = Variability::Parameter(Default::default());
    y.binding = Some(Expression::BuiltinCall {
        function: BuiltinFunction::Vector,
        args: vec![Expression::VarRef {
            name: test_reference("x"),
            subscripts: Vec::new(),
            span: source.span("x", 1),
        }],
        span: call_span,
    });

    let dae = construct(&model, source.map)
        .expect("typed Flat vector(A) has a checked compact DAE owner");
    dae.inspect(|view| {
        let y = view
            .variables()
            .map(|(_, variable)| variable)
            .find(|variable| variable.name().as_str() == "y")
            .expect("the bound parameter reaches DAE");
        assert_eq!(y.role(), dae::VariableRole::Parameter);
        let vector = view
            .expression(y.binding().expect("the parameter keeps its binding"))
            .expect("the vector expression resolves");
        let dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Vector,
            arguments,
        } = vector.operation()
        else {
            panic!("the source call remains one checked Vector node");
        };
        assert_eq!(vector.provenance().span(), call_span);
        assert_eq!(vector.value_type().scalar_type(), dae::ScalarType::Real);
        assert_eq!(vector.value_type().dimensions(), [3]);
        assert_eq!(vector.variability(), dae::ExpressionVariability::Parameter);
        assert_eq!(arguments.len(), 1, "Vector retains one compact operand");
        let operand = view
            .expression(arguments.get(0).expect("the checked operand exists"))
            .expect("the compact operand resolves");
        assert_eq!(operand.value_type().scalar_type(), dae::ScalarType::Real);
        assert_eq!(operand.value_type().dimensions(), [1, 3, 1]);
        assert_eq!(vector.binder_domain(), operand.binder_domain());
    });
}

/// MLS §10.3.5 / ARR-038: Flat proves the first-two-axis permutation before
/// lowering the source call into one compact checked DAE expression.
#[test]
fn typed_flat_rank_three_transpose_reaches_the_checked_dae_constructor() {
    let source = TestSource::new(
        "model M parameter Real x[2,3,4]; parameter Real y[3,2,4] = transpose(x); end M;",
    );
    let call_span = source.span("transpose(x)", 0);
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "x",
        "parameter Real x[2,3,4]",
        8,
        vec![2, 3, 4],
        false,
    );
    add_primitive_variable(
        &mut model,
        &source,
        "y",
        "parameter Real y[3,2,4]",
        7,
        vec![3, 2, 4],
        false,
    );
    model
        .variables
        .get_mut(&VarName::new("x"))
        .unwrap()
        .variability = Variability::Parameter(Default::default());
    let y = model.variables.get_mut(&VarName::new("y")).unwrap();
    y.variability = Variability::Parameter(Default::default());
    y.binding = Some(Expression::BuiltinCall {
        function: BuiltinFunction::Transpose,
        args: vec![Expression::VarRef {
            name: test_reference("x"),
            subscripts: Vec::new(),
            span: source.span("x", 1),
        }],
        span: call_span,
    });

    let dae = construct(&model, source.map)
        .expect("typed Flat transpose(A) has an exact checked compact DAE owner");
    dae.inspect(|view| {
        let y = view
            .variables()
            .map(|(_, variable)| variable)
            .find(|variable| variable.name().as_str() == "y")
            .expect("the bound parameter reaches DAE");
        let transpose = view
            .expression(y.binding().expect("the parameter keeps its binding"))
            .expect("the transpose expression resolves");
        let dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Transpose,
            arguments,
        } = transpose.operation()
        else {
            panic!("the source call remains one checked Transpose node");
        };
        assert_eq!(transpose.provenance().span(), call_span);
        assert_eq!(transpose.value_type().scalar_type(), dae::ScalarType::Real);
        assert_eq!(transpose.value_type().dimensions(), [3, 2, 4]);
        assert_eq!(arguments.len(), 1);
        let operand = view.expression(arguments.get(0).unwrap()).unwrap();
        assert_eq!(operand.value_type().dimensions(), [2, 3, 4]);
        assert_eq!(transpose.variability(), operand.variability());
        assert_eq!(transpose.binder_domain(), operand.binder_domain());
    });
}

/// MLS §3.7.3 / EQN-023: `initial()` is also an ordinary scalar Boolean
/// expression. It keeps the checked initial-condition owner even when nested
/// below another Boolean operator in an `if` condition.
#[test]
fn scalar_initial_expression_reaches_a_checked_condition_coordinate() {
    let source = TestSource::new(
        "model M Real x; equation x = if initial() and true then 1.0 else 2.0; end M;",
    );
    let call_span = source.span("initial()", 0);
    let model = initial_conditional_model(&source, Vec::new());

    let dae = construct(&model, source.map)
        .expect("zero-arity initial() has a checked scalar Boolean owner");
    dae.inspect(|view| {
        assert_eq!(view.condition_count(), 1);
        let condition_id = view.condition_id(0).expect("the initial condition exists");
        let condition = view
            .condition(condition_id)
            .expect("the initial condition resolves");
        assert!(matches!(
            condition.operation(),
            dae::ConditionOperation::Initial
        ));
        assert_eq!(condition.provenance().span(), call_span);

        let coordinate = (0..view.expression_count())
            .filter_map(|index| view.expression(view.expression_id(index)?))
            .find(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Coordinate(dae::CoordinateView::Condition(found))
                        if found == condition_id
                )
            })
            .expect("initial() becomes a coordinate into its condition owner");
        assert_eq!(coordinate.provenance().span(), call_span);
        assert_eq!(
            coordinate.value_type().scalar_type(),
            dae::ScalarType::Boolean
        );
        assert!(coordinate.value_type().dimensions().is_empty());
        assert_eq!(
            coordinate.variability(),
            dae::ExpressionVariability::Discrete
        );
        assert!(
            (0..view.expression_count())
                .filter_map(|index| view.expression(view.expression_id(index)?))
                .any(|expression| matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Conditional(_)
                )),
            "the Boolean coordinate remains nested in the source conditional"
        );
    });
}

#[test]
fn scalar_initial_expression_rejects_arguments_at_the_source_boundary() {
    let source =
        TestSource::new("model M Real x; equation x = if initial(true) then 1.0 else 2.0; end M;");
    let call_span = source.span("initial(true)", 0);
    let model = initial_conditional_model(
        &source,
        vec![Expression::Literal {
            value: Literal::Boolean(true),
            span: source.span("true", 0),
        }],
    );

    assert!(matches!(
        construct(&model, source.map),
        Err(ToDaeError::UnsupportedRuntimeOperator { operator, span, .. })
            if operator == "initial" && span == call_span
    ));
}

fn initial_conditional_model(source: &TestSource, initial_args: Vec<Expression>) -> flat::Model {
    let mut model = test_model();
    add_primitive_variable(&mut model, source, "x", "Real x", 7, Vec::new(), false);
    let zero_arity = initial_args.is_empty();
    let call_span = if zero_arity {
        source.span("initial()", 0)
    } else {
        source.span("initial(true)", 0)
    };
    let condition_span = if zero_arity {
        source.span("initial() and true", 0)
    } else {
        call_span
    };
    let initial = Expression::BuiltinCall {
        function: BuiltinFunction::Initial,
        args: initial_args,
        span: call_span,
    };
    let condition = if zero_arity {
        Expression::Binary {
            op: OpBinary::And,
            lhs: Box::new(initial),
            rhs: Box::new(Expression::Literal {
                value: Literal::Boolean(true),
                span: source.span("true", 0),
            }),
            span: condition_span,
        }
    } else {
        initial
    };
    let conditional_text = if zero_arity {
        "if initial() and true then 1.0 else 2.0"
    } else {
        "if initial(true) then 1.0 else 2.0"
    };
    let conditional_span = source.span(conditional_text, 0);
    let rhs = Expression::If {
        branches: vec![(
            condition,
            Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            },
        )],
        else_branch: Box::new(Expression::Literal {
            value: Literal::Real(2.0),
            span: source.span("2.0", 0),
        }),
        span: conditional_span,
    };
    let equation_span = source.span(&format!("x = {conditional_text}"), 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: test_reference("x"),
                subscripts: Vec::new(),
                span: source.span("x", 1),
            }),
            rhs: Box::new(rhs),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    model
}

#[test]
fn missing_predefined_type_identity_fails_before_dae_construction() {
    let source = TestSource::new("model M end M;");
    let model = flat::Model::new();
    let error = construct(&model, source.map)
        .expect_err("Flat without exact predefined type identities is not constructible");
    assert!(matches!(
        error,
        ToDaeError::MissingSemanticIdentity { identity }
            if identity == "predefined scalar canonical TypeIds"
    ));
}

#[test]
fn undefined_references_fail_before_construction() {
    let source = TestSource::new("model M Real x; equation 0 = x - 1.0; // missing\nend M;");
    let mut model = scalar_real_model(&source);
    let span = source.span("missing", 0);
    model.equations[0].residual = Expression::VarRef {
        name: Reference::new("missing"),
        subscripts: Vec::new(),
        span,
    };

    assert!(matches!(
        construct(&model, source.map),
        Err(ToDaeError::UnresolvedReference { name, span: found })
            if name == "missing" && found == span
    ));
}

#[test]
fn missing_expression_provenance_is_not_defaulted() {
    let source = TestSource::new("model M Real x; equation 0 = x - 1.0; end M;");
    let mut model = scalar_real_model(&source);
    model.equations[0].residual = Expression::VarRef {
        name: Reference::new("x"),
        subscripts: Vec::new(),
        span: Span::DUMMY,
    };

    assert!(matches!(
        construct(&model, source.map),
        Err(ToDaeError::MissingProvenance { .. })
    ));
}

#[test]
fn binding_lowering_does_not_fallback_to_declaration_provenance() {
    let source = TestSource::new("model M Real x = 1.0; end M;");
    let mut model = test_model();
    add_primitive_variable(&mut model, &source, "x", "Real x", 7, Vec::new(), false);
    model.variables.get_mut(&VarName::new("x")).unwrap().binding = Some(Expression::Literal {
        value: Literal::Real(1.0),
        span: source.span("1.0", 0),
    });
    model.is_partial = true;

    let analysis = analyze(&model).expect("valid binding must be accepted during analysis");
    let variable_plan =
        plan_variable_construction(&model, &analysis).expect("valid attributes must be planned");
    let Some(Expression::Literal { span, .. }) = model
        .variables
        .get_mut(&VarName::new("x"))
        .and_then(|variable| variable.binding.as_mut())
    else {
        panic!("fixture must retain its scalar binding");
    };
    *span = Span::DUMMY;

    let error = dae::Dae::construct(source.map, |construction| {
        build_checked(&model, &analysis, &variable_plan, construction)
    })
    .expect_err("lowering must recheck exact binding provenance");
    assert!(matches!(
        error,
        dae::DaeConstructionError::MissingProvenance {
            origin: dae::DaeProvenanceOrigin::Source,
            attempted_span: None,
        }
    ));
}
