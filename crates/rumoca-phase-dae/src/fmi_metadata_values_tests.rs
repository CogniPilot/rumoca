use crate::errors::ToDaeError;
use crate::fmi_metadata_values::{
    build_empty_fmi_array_expression_for_test, fold_fmi_model_description_values_to_literals,
};
use rumoca_core::{
    BuiltinFunction, ComponentRefPart, ComponentReference, DefId, Expression, Function,
    FunctionInstanceId, FunctionParam, Literal, OpBinary, Reference, ResolvedFunctionReference,
    SourceId, Span, Statement, Subscript, VarName,
};
use rumoca_ir_dae::{Dae, Variable, VariableOrigin};

fn test_span(start: usize, end: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("fmi_metadata_values_fixture.mo"),
        start,
        end,
    )
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![],
        span: test_span(1, 2),
    }
}

fn var_index(name: &str, index: i64) -> Expression {
    Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![Subscript::Index {
            value: index,
            span: test_span(1, 2),
        }],
        span: test_span(1, 2),
    }
}

fn structured_indexed_ref(rendered: &str, def_id: DefId, index: i64) -> Expression {
    Expression::VarRef {
        name: Reference::with_component_reference(
            rendered,
            ComponentReference {
                local: false,
                span: test_span(1, 2),
                parts: vec![ComponentRefPart {
                    ident: "p".to_string(),
                    span: test_span(1, 2),
                    subs: vec![Subscript::Index {
                        value: index,
                        span: test_span(1, 2),
                    }],
                }],
                def_id: Some(def_id),
            },
        ),
        subscripts: vec![],
        span: test_span(1, 2),
    }
}

fn array(elements: Vec<Expression>) -> Expression {
    Expression::Array {
        elements,
        is_matrix: false,
        span: test_span(9, 10),
    }
}

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: test_span(3, 4),
    }
}

fn integer(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: test_span(5, 6),
    }
}

fn parameter(name: &str, start: Expression) -> Variable {
    let mut var = Variable::new(VarName::new(name), test_span(1, 2));
    var.start = Some(start);
    var.is_tunable = false;
    var
}

fn state(name: &str, start: Expression) -> Variable {
    let mut var = Variable::new(VarName::new(name), test_span(1, 2));
    var.start = Some(start);
    var
}

fn assert_real(expr: &Expression, expected: f64) {
    match expr {
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } => assert!(
            (*value - expected).abs() <= 1.0e-12,
            "expected {expected}, got {value}"
        ),
        other => panic!("expected real literal {expected}, got {other:?}"),
    }
}

fn assert_real_array(expr: &Expression, expected: &[f64]) {
    let Expression::Array { elements, .. } = expr else {
        panic!("expected real array {expected:?}, got {expr:?}");
    };
    assert_eq!(elements.len(), expected.len());
    for (element, expected) in elements.iter().zip(expected) {
        assert_real(element, *expected);
    }
}

fn assert_array_extents(expr: &Expression, dimensions: &[usize]) {
    let Expression::Array { elements, .. } = expr else {
        panic!("expected array with dimensions {dimensions:?}, got {expr:?}");
    };
    assert_eq!(elements.len(), dimensions[0]);
    if dimensions.len() > 1 {
        for element in elements {
            assert_array_extents(element, &dimensions[1..]);
        }
    }
}

#[test]
fn fmi_model_description_folds_parameter_dependent_starts() {
    let mut dae = Dae::new();
    dae.variables
        .parameters
        .insert(VarName::new("p"), parameter("p", real(2.0)));
    dae.variables
        .parameters
        .insert(VarName::new("q"), parameter("q", real(3.0)));
    dae.variables.parameters.insert(
        VarName::new("r"),
        parameter(
            "r",
            Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(var("p")),
                rhs: Box::new(var("q")),
                span: test_span(21, 22),
            },
        ),
    );
    dae.variables.states.insert(
        VarName::new("x"),
        state(
            "x",
            Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(var("p")),
                rhs: Box::new(var("q")),
                span: test_span(23, 24),
            },
        ),
    );
    dae.variables
        .states
        .insert(VarName::new("y"), state("y", var("r")));

    fold_fmi_model_description_values_to_literals(&mut dae)
        .unwrap_or_else(|err| panic!("FMI start folding should succeed: {err}"));

    for name in ["r", "x", "y"] {
        let start = dae
            .variables
            .parameters
            .get(&VarName::new(name))
            .or_else(|| dae.variables.states.get(&VarName::new(name)))
            .and_then(|var| var.start.as_ref())
            .unwrap_or_else(|| panic!("{name} should have a folded start"));
        assert_real(start, 5.0);
    }
}

#[test]
fn fmi_model_description_rejects_unserializable_start_expression() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), state("x", var("unbound")));

    let err = fold_fmi_model_description_values_to_literals(&mut dae)
        .expect_err("FMI start folding should reject unresolved starts");

    assert!(
        matches!(err, ToDaeError::RuntimeMetadataViolationAt { detail, .. } if detail.contains("FMI modelDescription start for `x`"))
    );
}

#[test]
fn fmi_model_description_rejects_numeric_condition_coercion() {
    let mut dae = Dae::new();
    dae.variables.states.insert(
        VarName::new("x"),
        state(
            "x",
            Expression::If {
                branches: vec![(integer(1), real(2.0))],
                else_branch: Box::new(real(3.0)),
                span: test_span(25, 26),
            },
        ),
    );

    let err = fold_fmi_model_description_values_to_literals(&mut dae)
        .expect_err("FMI metadata folding should reject numeric conditions");

    assert!(
        matches!(err, ToDaeError::RuntimeMetadataViolationAt { detail, .. } if detail.contains("condition is not Boolean"))
    );
}

#[test]
fn fmi_model_description_rejects_source_variable_without_def_id() {
    let mut dae = Dae::new();
    let mut p = parameter("p", real(1.0));
    p.origin = VariableOrigin::Source;
    dae.variables.parameters.insert(VarName::new("p"), p);

    let err = fold_fmi_model_description_values_to_literals(&mut dae)
        .expect_err("source FMI metadata must carry DefId identity");

    assert!(
        matches!(err, ToDaeError::RuntimeContractViolation { detail, .. } if detail.contains("lost DefId metadata"))
    );
}

#[test]
fn fmi_model_description_rejects_delay_metadata_expression() {
    let mut dae = Dae::new();
    dae.variables.states.insert(
        VarName::new("x"),
        state(
            "x",
            Expression::BuiltinCall {
                function: BuiltinFunction::Delay,
                args: vec![real(1.0), real(0.1)],
                span: test_span(26, 27),
            },
        ),
    );

    let err = fold_fmi_model_description_values_to_literals(&mut dae)
        .expect_err("FMI metadata folding should reject delay");

    assert!(
        matches!(err, ToDaeError::RuntimeMetadataViolationAt { detail, .. } if detail.contains("unsupported expression: delay"))
    );
}

#[test]
fn fmi_model_description_smooth_does_not_fallback_on_unevaluable_expr() {
    let mut dae = Dae::new();
    dae.variables.states.insert(
        VarName::new("x"),
        state(
            "x",
            Expression::BuiltinCall {
                function: BuiltinFunction::Smooth,
                args: vec![real(1.0), var("unbound")],
                span: test_span(28, 29),
            },
        ),
    );

    let err = fold_fmi_model_description_values_to_literals(&mut dae)
        .expect_err("FMI metadata folding should reject unevaluable smooth expression");

    assert!(
        matches!(err, ToDaeError::RuntimeMetadataViolationAt { detail, .. } if detail.contains("referenced value is not available"))
    );
}

#[test]
fn fmi_model_description_folds_numeric_metadata_attributes() {
    let mut dae = Dae::new();
    dae.variables
        .parameters
        .insert(VarName::new("p"), parameter("p", real(2.0)));
    dae.variables
        .parameters
        .insert(VarName::new("q"), parameter("q", real(3.0)));
    dae.variables.parameters.insert(
        VarName::new("r"),
        parameter(
            "r",
            Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(var("p")),
                rhs: Box::new(var("q")),
                span: test_span(27, 28),
            },
        ),
    );
    let mut x = state("x", var("r"));
    x.min = Some(Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(var("p")),
        rhs: Box::new(real(1.0)),
        span: test_span(29, 30),
    });
    x.max = Some(Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(var("q")),
        rhs: Box::new(real(5.0)),
        span: test_span(31, 32),
    });
    x.nominal = Some(var("r"));
    dae.variables.states.insert(VarName::new("x"), x);

    fold_fmi_model_description_values_to_literals(&mut dae)
        .unwrap_or_else(|err| panic!("FMI metadata folding should succeed: {err}"));

    let x = &dae.variables.states[&VarName::new("x")];
    assert_real(x.start.as_ref().expect("start"), 5.0);
    assert_real(x.min.as_ref().expect("min"), 1.0);
    assert_real(x.max.as_ref().expect("max"), 8.0);
    assert_real(x.nominal.as_ref().expect("nominal"), 5.0);
}

#[test]
fn fmi_model_description_folds_array_alias_and_subscripted_starts() {
    let mut dae = Dae::new();
    let mut p = parameter("p", array(vec![real(1.0), real(2.0)]));
    p.dims = vec![2];
    dae.variables.parameters.insert(VarName::new("p"), p);
    let mut x = state("x", var("p"));
    x.dims = vec![2];
    dae.variables.states.insert(VarName::new("x"), x);
    dae.variables
        .states
        .insert(VarName::new("y"), state("y", var_index("p", 2)));

    fold_fmi_model_description_values_to_literals(&mut dae)
        .unwrap_or_else(|err| panic!("FMI array folding should succeed: {err}"));

    assert_real_array(
        dae.variables.states[&VarName::new("x")]
            .start
            .as_ref()
            .expect("x start"),
        &[1.0, 2.0],
    );
    assert_real(
        dae.variables.states[&VarName::new("y")]
            .start
            .as_ref()
            .expect("y start"),
        2.0,
    );
}

#[test]
fn fmi_model_description_preserves_nested_zero_array_extents() {
    for dimensions in [vec![0], vec![2, 0], vec![0, 2], vec![2, 0, 3]] {
        let expression = build_empty_fmi_array_expression_for_test(&dimensions, test_span(1, 2))
            .unwrap_or_else(|err| {
                panic!("FMI zero-extent array {dimensions:?} should rebuild: {err}")
            });
        assert_array_extents(&expression, &dimensions);
    }
}

#[test]
fn fmi_model_description_rejects_array_cardinality_overflow() {
    let error = build_empty_fmi_array_expression_for_test(&[usize::MAX, 2], test_span(1, 2))
        .expect_err("overflowing array cardinality should fail");

    assert!(error.contains("overflow"));
}

#[test]
fn fmi_model_description_uses_structured_reference_identity_and_subscripts() {
    let mut dae = Dae::new();
    let def_id = DefId::new(100);
    let mut p = parameter("p", array(vec![real(1.0), real(2.0)]));
    p.origin = VariableOrigin::Source;
    p.component_ref = Some(ComponentReference {
        local: false,
        span: test_span(1, 2),
        parts: vec![ComponentRefPart {
            ident: "p".to_string(),
            span: test_span(1, 2),
            subs: Vec::new(),
        }],
        def_id: Some(def_id),
    });
    p.dims = vec![2];
    dae.variables.parameters.insert(VarName::new("p"), p);
    dae.variables.states.insert(
        VarName::new("y"),
        state(
            "y",
            structured_indexed_ref("wrong_rendered_name[99]", def_id, 2),
        ),
    );

    fold_fmi_model_description_values_to_literals(&mut dae)
        .unwrap_or_else(|err| panic!("FMI structured folding should succeed: {err}"));

    assert_real(
        dae.variables.states[&VarName::new("y")]
            .start
            .as_ref()
            .expect("y start"),
        2.0,
    );
}

#[test]
fn fmi_model_description_folds_array_constructors_and_ranges() {
    let mut dae = Dae::new();
    dae.variables
        .parameters
        .insert(VarName::new("p"), parameter("p", real(4.0)));
    let mut fill_var = state(
        "fillVar",
        Expression::BuiltinCall {
            function: BuiltinFunction::Fill,
            args: vec![
                Expression::Binary {
                    op: OpBinary::Sub,
                    lhs: Box::new(var("p")),
                    rhs: Box::new(real(3.0)),
                    span: test_span(33, 34),
                },
                integer(3),
            ],
            span: test_span(35, 36),
        },
    );
    fill_var.dims = vec![3];
    dae.variables
        .states
        .insert(VarName::new("fillVar"), fill_var);
    let mut range_var = state(
        "rangeVar",
        Expression::Range {
            start: Box::new(integer(1)),
            step: None,
            end: Box::new(integer(3)),
            span: test_span(37, 38),
        },
    );
    range_var.dims = vec![3];
    dae.variables
        .states
        .insert(VarName::new("rangeVar"), range_var);

    fold_fmi_model_description_values_to_literals(&mut dae)
        .unwrap_or_else(|err| panic!("FMI constructor folding should succeed: {err}"));

    assert_real_array(
        dae.variables.states[&VarName::new("fillVar")]
            .start
            .as_ref()
            .expect("fillVar start"),
        &[1.0, 1.0, 1.0],
    );
    assert_real_array(
        dae.variables.states[&VarName::new("rangeVar")]
            .start
            .as_ref()
            .expect("rangeVar start"),
        &[1.0, 2.0, 3.0],
    );
}

#[test]
fn fmi_model_description_folds_identity_matrix_start() {
    let mut dae = Dae::new();
    let mut rotation = state(
        "rotation",
        Expression::BuiltinCall {
            function: BuiltinFunction::Identity,
            args: vec![integer(3)],
            span: test_span(39, 40),
        },
    );
    rotation.dims = vec![3, 3];
    dae.variables
        .states
        .insert(VarName::new("rotation"), rotation);

    fold_fmi_model_description_values_to_literals(&mut dae)
        .unwrap_or_else(|err| panic!("FMI identity folding should succeed: {err}"));

    assert_real_array(
        dae.variables.states[&VarName::new("rotation")]
            .start
            .as_ref()
            .expect("rotation start"),
        &[1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0],
    );
}

#[test]
fn fmi_model_description_folds_array_returning_user_function_start() {
    let mut dae = Dae::new();
    let mut vector = Function::new("Pkg.vector", test_span(41, 42));
    vector.instance_id = Some(FunctionInstanceId::new(1));
    vector.add_output(
        FunctionParam::new("v_out", "Real", test_span(41, 42))
            .with_dims(vec![2])
            .with_default(array(vec![real(2.0), real(3.0)])),
    );
    vector.body = vec![Statement::Empty {
        span: test_span(41, 42),
    }];
    dae.symbols
        .functions
        .insert(VarName::new("Pkg.vector"), vector);

    let mut output = state(
        "output",
        Expression::FunctionCall {
            name: Reference::from_component_reference(
                rumoca_core::component_reference_from_flat_name(
                    &VarName::new("Pkg.vector"),
                    test_span(43, 44),
                )
                .expect("structured function reference"),
            )
            .with_resolved_function(ResolvedFunctionReference {
                instance_id: FunctionInstanceId::new(1),
                base_part_count: 2,
            }),
            args: Vec::new(),
            is_constructor: false,
            span: test_span(43, 44),
        },
    );
    output.dims = vec![2];
    dae.variables.states.insert(VarName::new("output"), output);

    fold_fmi_model_description_values_to_literals(&mut dae)
        .unwrap_or_else(|err| panic!("FMI user-function array folding should succeed: {err}"));

    assert_real_array(
        dae.variables.states[&VarName::new("output")]
            .start
            .as_ref()
            .expect("output start"),
        &[2.0, 3.0],
    );
}

#[test]
fn fmi_model_description_encodes_boolean_start_for_numeric_runtime_slot() {
    let mut dae = Dae::new();
    dae.variables.discrete_valued.insert(
        VarName::new("valid"),
        state(
            "valid",
            Expression::Literal {
                value: Literal::Boolean(false),
                span: test_span(45, 46),
            },
        ),
    );

    fold_fmi_model_description_values_to_literals(&mut dae)
        .unwrap_or_else(|err| panic!("FMI Boolean start folding should succeed: {err}"));

    assert_real(
        dae.variables.discrete_valued[&VarName::new("valid")]
            .start
            .as_ref()
            .expect("valid start"),
        0.0,
    );
}
