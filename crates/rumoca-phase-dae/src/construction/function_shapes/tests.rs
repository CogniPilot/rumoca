use rumoca_core::{
    DefId, EffectiveType, FunctionInstanceId, Reference, ResolvedFunctionReference, SourceMap,
    Subscript, TypeId,
};

use super::*;

fn literal(value: f64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn array(extent: usize, span: Span) -> Expression {
    Expression::Array {
        elements: (0..extent)
            .map(|ordinal| literal(ordinal as f64, span))
            .collect(),
        is_matrix: false,
        span,
    }
}

fn real_param(name: &str, dimensions: Vec<i64>, span: Span) -> rumoca_core::FunctionParam {
    let value_type = EffectiveType::new(TypeId::new(1), TypeId::new(1), dimensions)
        .expect("fixture function type is resolved");
    rumoca_core::FunctionParam::new(name, "Real", value_type, span)
}

fn vectorization_target(name: &str, span: Span) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference::construct(
        false,
        span,
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: Vec::new(),
            def_id: DefId::new(900),
        }],
    )
    .expect("the synthetic function target has exact identity")
}

fn exact_function_reference(name: &str, instance_id: FunctionInstanceId) -> Reference {
    Reference::new(name).with_resolved_function(ResolvedFunctionReference {
        instance_id,
        base_part_count: 1,
        transitively_non_replaceable: true,
    })
}

fn pair_constructor_model(span: Span) -> (flat::Model, FunctionInstanceId, DefId) {
    let record = DefId::new(40);
    let left = DefId::new(41);
    let right = DefId::new(42);
    let mut constructor = rumoca_core::Function::new("Pair", span);
    constructor.def_id = Some(record);
    constructor.is_constructor = true;
    let mut left_parameter = real_param("left", Vec::new(), span);
    left_parameter.def_id = Some(left);
    let mut right_parameter = real_param("right", Vec::new(), span);
    right_parameter.def_id = Some(right);
    constructor.add_input(left_parameter);
    constructor.add_input(right_parameter);

    let mut model = flat::Model::new();
    model.record_types.insert(
        record,
        flat::RecordType {
            name: "Pair".to_string(),
            fields: vec![
                flat::RecordField {
                    name: "left".to_string(),
                    def_id: left,
                    dims: Vec::new(),
                },
                flat::RecordField {
                    name: "right".to_string(),
                    def_id: right,
                    dims: Vec::new(),
                },
            ],
        },
    );
    model.add_function(constructor);
    let instance_id = model.functions[&VarName::new("Pair")]
        .instance_id
        .expect("Flat assigns the synthetic constructor an exact instance");
    (model, instance_id, left)
}

fn pair_call(instance_id: FunctionInstanceId, span: Span) -> Expression {
    Expression::FunctionCall {
        name: exact_function_reference("Pair", instance_id),
        args: vec![literal(1.0, span), literal(2.0, span)],
        is_constructor: true,
        span,
    }
}

fn add_scalar_read(model: &mut flat::Model, span: Span) -> FunctionInstanceId {
    let mut read = rumoca_core::Function::new("read", span);
    read.add_input(real_param("value", Vec::new(), span));
    read.add_output(real_param("result", Vec::new(), span));
    model.add_function(read);
    model.functions[&VarName::new("read")]
        .instance_id
        .expect("Flat assigns the regular function an exact instance")
}

fn assert_constructor_identity_error(error: ToDaeError, expected: String, span: Span) {
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span: error_span,
        } if feature == "record constructor" && detail == expected && error_span == span
    ));
}

fn identity_function(span: Span, result_has_shape_equality: bool) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("identity", span);
    function
        .add_input(real_param("u", vec![0], span).with_shape_expr(vec![Subscript::colon(span)]));
    let mut output = real_param("y", vec![0], span);
    if result_has_shape_equality {
        output = output.with_shape_expr(vec![Subscript::expr(
            Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Size,
                args: vec![
                    Expression::VarRef {
                        name: Reference::new("u"),
                        subscripts: Vec::new(),
                        span,
                    },
                    Expression::Literal {
                        value: Literal::Integer(1),
                        span,
                    },
                ],
                span,
            }),
            span,
        )]);
    }
    function.add_output(output);
    function
}

fn enumeration_reference(name: &str, declaration: DefId, span: Span) -> Expression {
    let component_ref = rumoca_core::ComponentReference::construct(
        false,
        span,
        vec![rumoca_core::ComponentRefPart {
            ident: "Choice".to_string(),
            span,
            subs: Vec::new(),
            def_id: declaration,
        }],
    )
    .expect("fixture enumeration reference has exact identity");
    Expression::VarRef {
        name: Reference::with_component_reference(name, component_ref),
        subscripts: Vec::new(),
        span,
    }
}

#[test]
fn enum_literal_shape_requires_catalog_and_enumeration_type_identity() {
    let mut sources = SourceMap::new();
    let source = sources.add("enum_shape.mo", "Choice.active;");
    let span = Span::from_offsets(source, 0, 13);
    let enum_declaration = DefId::new(81);
    let other_declaration = DefId::new(82);
    let enum_type = TypeId::new(91);
    let literal_name = "Pkg.Choice.active";
    let mut model = flat::Model::new();
    model.type_ids_by_def_id.insert(enum_declaration, enum_type);
    model.enumeration_type_roots.insert(enum_type);
    model
        .enum_literal_ordinals
        .insert(literal_name.to_string(), 1);
    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new())
        .expect("the fixture model has a valid shape environment");
    let exact = enumeration_reference(literal_name, enum_declaration, span);
    assert_eq!(
        analysis
            .expression_shape(&exact, analysis.model_values())
            .expect("an exact cataloged enumeration literal is a scalar"),
        Vec::<u32>::new()
    );

    let forged = enumeration_reference(literal_name, other_declaration, span);
    let Err(error) = analysis.expression_shape(&forged, analysis.model_values()) else {
        panic!("catalog spelling without enumeration type identity must be rejected");
    };
    assert!(matches!(
        error,
        ToDaeError::UnresolvedReference {
            name,
            span: error_span,
        } if name == literal_name && error_span == span
    ));
}

fn call(extent: usize, span: Span) -> flat::Equation {
    flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("identity"),
            args: vec![array(extent, span)],
            is_constructor: false,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    )
}

#[test]
fn record_constructor_arity_remains_strict() {
    let mut sources = SourceMap::new();
    let source = sources.add("record_arity.mo", "Pair(1.0);");
    let span = Span::from_offsets(source, 0, 9);
    let mut constructor = rumoca_core::Function::new("Pair", span);
    constructor.is_constructor = true;
    constructor.add_input(real_param("left", Vec::new(), span));
    constructor.add_input(real_param("right", Vec::new(), span));

    let mut model = flat::Model::new();
    model.add_function(constructor);
    let constructor = model.functions[&VarName::new("Pair")]
        .instance_id
        .expect("Flat assigns the constructor an exact instance");
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: exact_function_reference("Pair", constructor),
            args: vec![literal(1.0, span)],
            is_constructor: true,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let Err(error) = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()) else {
        panic!("record constructor with one missing field must be rejected");
    };
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span: error_span,
        } if feature == "record constructor"
            && detail == "`Pair` expects 2 fields but receives 1"
            && error_span == span
    ));
}

#[test]
fn root_structural_constructor_keeps_its_aggregate_shape_proof() {
    let mut sources = SourceMap::new();
    let source = sources.add("root_constructor.mo", "Pair(1.0, 2.0);");
    let span = Span::from_offsets(source, 0, 15);
    let (mut model, constructor, _) = pair_constructor_model(span);
    model.add_equation(flat::Equation::new(
        pair_call(constructor, span),
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new())
        .expect("a root structural constructor owns no fabricated function result");
    assert!(analysis.certificates().is_empty());
    assert_eq!(
        analysis
            .constructor_field_shapes(
                &exact_function_reference("Pair", constructor),
                &[literal(1.0, span), literal(2.0, span)],
                analysis.model_values(),
            )
            .expect("constructor discovery retains exact field shapes"),
        [Vec::<u32>::new(), Vec::<u32>::new()]
    );
}

#[test]
fn nested_structural_constructor_proves_a_field_inside_a_regular_call() {
    let mut sources = SourceMap::new();
    let source = sources.add("nested_constructor.mo", "read(Pair(1.0, 2.0).left + 0.0);");
    let span = Span::from_offsets(source, 0, 32);
    let (mut model, constructor, left) = pair_constructor_model(span);
    let mut read = rumoca_core::Function::new("read", span);
    read.add_input(real_param("value", Vec::new(), span));
    read.add_output(real_param("result", Vec::new(), span));
    model.add_function(read);
    let read_instance = model.functions[&VarName::new("read")]
        .instance_id
        .expect("Flat assigns the regular function an exact instance");
    let field = Expression::FieldAccess {
        base: Box::new(pair_call(constructor, span)),
        field: "left".to_string(),
        field_def_id: left,
        span,
    };
    let argument = Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(field),
        rhs: Box::new(literal(0.0, span)),
        span,
    };
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: exact_function_reference("read", read_instance),
            args: vec![argument],
            is_constructor: false,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new())
        .expect("a nested structural constructor proves its aggregate and selected field shape");
    let [read] = analysis.certificates() else {
        panic!("only the ordinary outer call owns a function specialization")
    };
    assert_eq!(read.key.function, VarName::new("read"));
    assert_eq!(read.parameters, vec![Vec::<u32>::new()]);
    assert_eq!(read.results, vec![Vec::<u32>::new()]);
}

#[test]
fn nested_fix_does_not_fabricate_a_result_for_a_regular_empty_function() {
    let mut sources = SourceMap::new();
    let source = sources.add("empty_function.mo", "empty(1.0);");
    let span = Span::from_offsets(source, 0, 11);
    let mut empty = rumoca_core::Function::new("empty", span);
    empty.add_input(real_param("value", Vec::new(), span));
    let mut model = flat::Model::new();
    model.add_function(empty);
    let instance = model.functions[&VarName::new("empty")]
        .instance_id
        .expect("Flat assigns the regular function an exact instance");
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: exact_function_reference("empty", instance),
            args: vec![literal(1.0, span)],
            is_constructor: false,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let Err(error) = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()) else {
        panic!("a regular zero-output function must keep its named rejection")
    };
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span: error_span,
        } if feature == "function result shape"
            && detail == "`empty` has no first result"
            && error_span == span
    ));
}

#[test]
fn post_analysis_rejects_a_forged_constructor_occurrence() {
    let mut sources = SourceMap::new();
    let source = sources.add("forged_constructor.mo", "Pair(1.0, 2.0);");
    let span = Span::from_offsets(source, 0, 10);
    let (mut model, _, _) = pair_constructor_model(span);
    let regular_instance = add_scalar_read(&mut model, span);
    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new())
        .expect("an unused regular function needs no specialization");
    let forged = Expression::FunctionCall {
        name: exact_function_reference("Pair", regular_instance),
        args: vec![literal(1.0, span), literal(2.0, span)],
        is_constructor: true,
        span,
    };

    let Err(error) = analysis.expression_shape(&forged, analysis.model_values()) else {
        panic!("a forged constructor marker must not manufacture aggregate shape")
    };
    assert_constructor_identity_error(
        error,
        format!(
            "`Pair` resolves to function instance {}, which is not constructor metadata",
            regular_instance.index()
        ),
        span,
    );
}

#[test]
fn post_analysis_rejects_an_unresolved_constructor_marker() {
    let mut sources = SourceMap::new();
    let source = sources.add("unresolved_constructor.mo", "Pair(1.0, 2.0);");
    let span = Span::from_offsets(source, 0, 15);
    let (model, _, _) = pair_constructor_model(span);
    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new())
        .expect("an unused constructor needs no specialization");
    let unresolved = Expression::FunctionCall {
        name: Reference::new("Pair"),
        args: vec![literal(1.0, span), literal(2.0, span)],
        is_constructor: true,
        span,
    };

    let Err(error) = analysis.expression_shape(&unresolved, analysis.model_values()) else {
        panic!("an unresolved constructor marker must not manufacture aggregate shape")
    };
    assert_constructor_identity_error(
        error,
        "`Pair` is marked as a constructor without exact resolved function metadata".to_string(),
        span,
    );
}

#[test]
fn discovery_rejects_a_constructor_name_with_a_regular_exact_instance() {
    let mut sources = SourceMap::new();
    let source = sources.add("wrong_constructor_identity.mo", "Pair(1.0, 2.0);");
    let span = Span::from_offsets(source, 0, 15);
    let (mut model, _, _) = pair_constructor_model(span);
    let regular_instance = add_scalar_read(&mut model, span);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: exact_function_reference("Pair", regular_instance),
            args: vec![literal(1.0, span), literal(2.0, span)],
            is_constructor: true,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let Err(error) = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()) else {
        panic!("discovery must reject a constructor spelling resolved to a regular function")
    };
    assert_constructor_identity_error(
        error,
        format!(
            "`Pair` resolves to function instance {}, which is not constructor metadata",
            regular_instance.index()
        ),
        span,
    );
}

#[test]
fn reachable_calls_receive_distinct_concrete_shape_certificates() {
    let mut sources = SourceMap::new();
    let source = sources.add("shape.mo", "identity({1,2}); identity({1,2,3});");
    let first = Span::from_offsets(source, 0, 15);
    let second = Span::from_offsets(source, 17, 34);
    let mut model = flat::Model::new();
    model.add_function(identity_function(first, true));
    model.add_equation(call(2, first));
    model.add_equation(call(3, second));

    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()).unwrap();
    let certificates = analysis.certificates();
    assert_eq!(certificates.len(), 2);
    assert_eq!(certificates[0].parameters, vec![vec![2]]);
    assert_eq!(certificates[0].results, vec![vec![2]]);
    assert_eq!(certificates[1].parameters, vec![vec![3]]);
    assert_eq!(certificates[1].results, vec![vec![3]]);
}

#[test]
fn empty_array_call_has_a_zero_extent_shape_certificate() {
    let mut sources = SourceMap::new();
    let source = sources.add("empty_shape.mo", "identity({});");
    let span = Span::from_offsets(source, 0, 13);
    let mut model = flat::Model::new();
    model.add_function(identity_function(span, true));
    model.add_equation(call(0, span));

    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()).unwrap();
    let [certificate] = analysis.certificates() else {
        panic!("empty array call should have one shape certificate");
    };
    assert_eq!(certificate.parameters, vec![vec![0]]);
    assert_eq!(certificate.results, vec![vec![0]]);
}

#[test]
fn unresolved_result_axis_is_rejected_at_analysis() {
    let mut sources = SourceMap::new();
    let source = sources.add("shape_error.mo", "identity({1,2});");
    let span = Span::from_offsets(source, 0, 15);
    let mut model = flat::Model::new();
    model.add_function(identity_function(span, false));
    model.add_equation(call(2, span));

    let error = match FunctionShapeAnalysis::analyze(&model, &EvalContext::new()) {
        Ok(_) => panic!("an unresolved result axis must not produce a certificate"),
        Err(error) => error,
    };
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, span: error_span, .. }
            if feature == "function shape proof" && error_span == span
    ));
}

fn scalar_identity(span: Span) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("scalar_identity", span);
    function.transitively_non_replaceable = true;
    function.add_input(real_param("r", Vec::new(), span));
    function.add_output(real_param("result", Vec::new(), span));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: vectorization_target("result", span),
        value: Expression::VarRef {
            name: Reference::new("r"),
            subscripts: Vec::new(),
            span,
        },
        span,
    });
    function
}

fn exact_call(
    name: &str,
    instance: FunctionInstanceId,
    arguments: Vec<Expression>,
    span: Span,
) -> flat::Equation {
    flat::Equation::new(
        Expression::FunctionCall {
            name: exact_function_reference(name, instance),
            args: arguments,
            is_constructor: false,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    )
}

fn matrix(rows: usize, columns: usize, span: Span) -> Expression {
    Expression::Array {
        elements: (0..rows).map(|_| array(columns, span)).collect(),
        is_matrix: false,
        span,
    }
}

#[test]
fn scalar_function_vectorization_keeps_one_scalar_specialization() {
    let mut sources = SourceMap::new();
    let source = sources.add("vectorized_scalar.mo", "scalar_identity({1,2,3});");
    let span = Span::from_offsets(source, 0, 25);
    let mut model = flat::Model::new();
    model.add_function(scalar_identity(span));
    let instance = model.functions[&VarName::new("scalar_identity")]
        .instance_id
        .unwrap();
    model.add_equation(exact_call(
        "scalar_identity",
        instance,
        vec![array(3, span)],
        span,
    ));

    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()).unwrap();
    let [certificate] = analysis.certificates() else {
        panic!("one vectorized call reuses one scalar specialization")
    };
    assert_eq!(certificate.parameters, vec![Vec::<u32>::new()]);
    assert_eq!(certificate.results, vec![Vec::<u32>::new()]);
    assert_eq!(
        analysis
            .expression_shape(&model.equations[0].residual, analysis.model_values())
            .unwrap(),
        vec![3]
    );
}

#[test]
fn array_formal_vectorization_preserves_trailing_element_shape() {
    let mut sources = SourceMap::new();
    let source = sources.add("vectorized_array.mo", "f(A);");
    let span = Span::from_offsets(source, 0, 4);
    let mut function = rumoca_core::Function::new("f", span);
    function.transitively_non_replaceable = true;
    function.add_input(real_param("r", vec![3], span));
    function.add_output(real_param("result", vec![3], span));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: vectorization_target("result", span),
        value: Expression::VarRef {
            name: Reference::new("r"),
            subscripts: Vec::new(),
            span,
        },
        span,
    });
    let mut model = flat::Model::new();
    model.add_function(function);
    let instance = model.functions[&VarName::new("f")].instance_id.unwrap();
    model.add_equation(exact_call("f", instance, vec![matrix(2, 3, span)], span));

    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()).unwrap();
    assert_eq!(analysis.certificates()[0].parameters, vec![vec![3]]);
    assert_eq!(
        analysis
            .expression_shape(&model.equations[0].residual, analysis.model_values())
            .unwrap(),
        vec![2, 3]
    );
}

#[test]
fn multi_axis_vectorization_broadcasts_non_vectorized_inputs() {
    let mut sources = SourceMap::new();
    let source = sources.add("vectorized_broadcast.mo", "f(A, scale);");
    let span = Span::from_offsets(source, 0, 11);
    let mut function = scalar_identity(span);
    function.name = VarName::new("f");
    function.add_input(real_param("scale", Vec::new(), span));
    let mut model = flat::Model::new();
    model.add_function(function);
    let instance = model.functions[&VarName::new("f")].instance_id.unwrap();
    model.add_equation(exact_call(
        "f",
        instance,
        vec![matrix(2, 3, span), literal(2.0, span)],
        span,
    ));

    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()).unwrap();
    let Expression::FunctionCall { name, args, .. } = &model.equations[0].residual else {
        unreachable!()
    };
    let call = analysis
        .call_certificate(name, args, analysis.model_values(), span)
        .unwrap();
    assert_eq!(call.prefix, vec![2, 3]);
    assert_eq!(call.vectorized_inputs, vec![true, false]);
    assert_eq!(analysis.certificates()[0].parameters, vec![vec![], vec![]]);
    assert_eq!(
        analysis
            .expression_shape(&model.equations[0].residual, analysis.model_values())
            .unwrap(),
        vec![2, 3]
    );
}

#[test]
fn vectorized_inputs_require_one_common_prefix() {
    let mut sources = SourceMap::new();
    let source = sources.add("vectorized_mismatch.mo", "f(a,b);");
    let span = Span::from_offsets(source, 0, 6);
    let mut function = scalar_identity(span);
    function.name = VarName::new("f");
    function.add_input(real_param("other", Vec::new(), span));
    let mut model = flat::Model::new();
    model.add_function(function);
    let instance = model.functions[&VarName::new("f")].instance_id.unwrap();
    model.add_equation(exact_call(
        "f",
        instance,
        vec![array(2, span), array(3, span)],
        span,
    ));

    let error = match FunctionShapeAnalysis::analyze(&model, &EvalContext::new()) {
        Ok(_) => panic!("inconsistent vectorization prefixes must be rejected"),
        Err(error) => error,
    };
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, detail, .. }
            if feature == "function vectorization proof"
                && detail.contains("inconsistent automatic-vectorization prefixes")
    ));
}

#[test]
fn vectorization_requires_an_exact_non_replaceable_owner() {
    let mut sources = SourceMap::new();
    let source = sources.add("replaceable_vectorization.mo", "f({1,2});");
    let span = Span::from_offsets(source, 0, 9);
    let mut model = flat::Model::new();
    let mut function = scalar_identity(span);
    function.name = VarName::new("f");
    model.add_function(function);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("f"),
            args: vec![array(2, span)],
            is_constructor: false,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let error = match FunctionShapeAnalysis::analyze(&model, &EvalContext::new()) {
        Ok(_) => panic!("vectorization without an exact owner must be rejected"),
        Err(error) => error,
    };
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, detail, .. }
            if feature == "function vectorization proof"
                && detail.contains("transitively non-replaceable exact owner")
    ));
}

#[test]
fn vectorization_rejects_unknown_transitive_non_replaceability() {
    let mut sources = SourceMap::new();
    let source = sources.add("unknown_vectorization_owner.mo", "f({1,2});");
    let span = Span::from_offsets(source, 0, 9);
    let mut model = flat::Model::new();
    let mut function = scalar_identity(span);
    function.name = VarName::new("f");
    function.transitively_non_replaceable = false;
    model.add_function(function);
    let instance = model.functions[&VarName::new("f")].instance_id.unwrap();
    model.add_equation(exact_call("f", instance, vec![array(2, span)], span));

    let error = match FunctionShapeAnalysis::analyze(&model, &EvalContext::new()) {
        Ok(_) => panic!("unknown non-replaceability must not mint vectorization"),
        Err(error) => error,
    };
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, detail, .. }
            if feature == "function vectorization proof"
                && detail.contains("constructor-proven transitive non-replaceability")
    ));
}

#[test]
fn vectorization_rejects_an_exact_instance_without_an_occurrence_proof() {
    let mut sources = SourceMap::new();
    let source = sources.add("unproven_vectorization_occurrence.mo", "f({1,2});");
    let span = Span::from_offsets(source, 0, 9);
    let mut model = flat::Model::new();
    let mut function = scalar_identity(span);
    function.name = VarName::new("f");
    model.add_function(function);
    let instance = model.functions[&VarName::new("f")].instance_id.unwrap();
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("f").with_resolved_function(ResolvedFunctionReference {
                instance_id: instance,
                base_part_count: 1,
                transitively_non_replaceable: false,
            }),
            args: vec![array(2, span)],
            is_constructor: false,
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let error = match FunctionShapeAnalysis::analyze(&model, &EvalContext::new()) {
        Ok(_) => panic!("an exact instance must not substitute for its exposure-path proof"),
        Err(error) => error,
    };
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, detail, .. }
            if feature == "function vectorization proof"
                && detail.contains("occurrence-proven transitively non-replaceable")
    ));
}

#[test]
fn vectorized_element_shape_must_equal_the_declared_shape() {
    let mut sources = SourceMap::new();
    let source = sources.add("vectorized_element_mismatch.mo", "f(A);");
    let span = Span::from_offsets(source, 0, 4);
    let mut function = rumoca_core::Function::new("f", span);
    function.transitively_non_replaceable = true;
    function.add_input(real_param("r", vec![3], span));
    function.add_output(real_param("result", Vec::new(), span));
    let mut model = flat::Model::new();
    model.add_function(function);
    let instance = model.functions[&VarName::new("f")].instance_id.unwrap();
    model.add_equation(exact_call("f", instance, vec![matrix(2, 4, span)], span));

    let error = match FunctionShapeAnalysis::analyze(&model, &EvalContext::new()) {
        Ok(_) => panic!("a wrong vectorized element shape must be rejected"),
        Err(error) => error,
    };
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, detail, .. }
            if feature == "function shape proof"
                && detail.contains("axis 1 requires extent 3")
                && detail.contains("call site proves 4")
    ));
}
