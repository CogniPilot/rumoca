use super::*;

// Most tests in this module exercise successful/deferred value semantics. Keep
// those assertions compact while the dedicated fail-closed tests below inspect
// the public `Result<Option<_>>` boundary directly.
fn try_eval_integer_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<i64> {
    super::try_eval_integer_with_context(expr, ctx).expect("fixture evaluation is valid")
}

fn try_eval_real_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<f64> {
    super::try_eval_real_with_context(expr, ctx).expect("fixture evaluation is valid")
}

fn try_eval_flat_expr_boolean(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
) -> Option<bool> {
    super::try_eval_flat_expr_boolean(expr, known_ints, known_bools, known_enums)
        .expect("fixture evaluation is valid")
}

fn try_eval_flat_expr_enum(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
) -> Option<String> {
    super::try_eval_flat_expr_enum(expr, known_ints, known_bools, known_enums)
        .expect("fixture evaluation is valid")
}

fn infer_array_dimensions_full_with_conds(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    super::infer_array_dimensions_full_with_conds(
        expr,
        known_ints,
        known_bools,
        known_enums,
        array_dims,
    )
    .expect("fixture dimension inference is valid")
}

fn infer_array_dimensions_full_with_functions(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    super::infer_array_dimensions_full_with_functions(expr, ctx)
        .expect("fixture dimension inference is valid")
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("eval_flat_phase_constant_source_7.mo"),
        0,
        1,
    )
}

fn context_with_pending_function() -> crate::constant::EvalContext {
    let pending =
        rumoca_core::Function::new("Pkg.pending", rumoca_core::DefId::new(12_500), test_span());
    let mut functions = FxHashMap::default();
    functions.insert("first".to_string(), pending.clone());
    functions.insert("duplicate".to_string(), pending);
    build_structural_eval_context(
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
        &functions,
    )
    .expect("pending Flat callable identity must not poison context construction")
}

#[test]
fn unrelated_fold_succeeds_with_pending_flat_callable_identity() {
    let expression = binary(rumoca_core::OpBinary::Add, int(1), int(1));
    let context = context_with_pending_function();
    assert_eq!(context.pending_function_count(), Some(1));
    let value = crate::constant::eval_expr(&expression, &context)
        .expect("an unrelated literal fold remains evaluable");
    assert_eq!(value, crate::constant::Value::Integer(2));
}

#[test]
fn distinct_pending_flat_callable_definitions_are_both_retained() {
    let mut context = crate::constant::EvalContext::structural_preidentity();
    let first =
        rumoca_core::Function::new("Pkg.first", rumoca_core::DefId::new(12_501), test_span());
    let second =
        rumoca_core::Function::new("Pkg.second", rumoca_core::DefId::new(12_502), test_span());
    context.try_add_function(first.clone()).unwrap();
    context.try_add_function(first).unwrap();
    context.try_add_function(second).unwrap();
    assert_eq!(context.pending_function_count(), Some(2));
}

#[test]
fn encountered_pending_flat_callable_has_typed_exact_span_refusal() {
    let call_span = rumoca_core::Span::from_offsets(test_span().source, 7, 18);
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.pending"),
        args: Vec::new(),
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: call_span,
    };
    let error = crate::constant::eval_expr(&call, &context_with_pending_function())
        .expect_err("an encountered pending callable cannot be evaluated before Flat identity");
    assert!(matches!(
        &error,
        crate::constant::EvalError::PendingCallableIdentity { span, .. } if *span == call_span
    ));
    assert_eq!(error.runtime_dependent_reason(), None);
}

fn test_function(
    name: impl Into<String>,
    exposure: rumoca_core::DefId,
    span: rumoca_core::Span,
) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new(name, exposure, span);
    function.def_id = Some(exposure);
    function.instance_id = Some(rumoca_core::FunctionInstanceId::new(exposure.index()));
    function
}

#[derive(Clone, Copy)]
enum TestScalar {
    Real,
    Integer,
    Boolean,
}

fn function_param(
    name: &str,
    scalar: TestScalar,
    dimensions: Vec<i64>,
) -> rumoca_core::FunctionParam {
    let (type_name, type_id) = match scalar {
        TestScalar::Real => ("Real", rumoca_core::TypeId::new(1)),
        TestScalar::Integer => ("Integer", rumoca_core::TypeId::new(2)),
        TestScalar::Boolean => ("Boolean", rumoca_core::TypeId::new(3)),
    };
    let effective_type = rumoca_core::EffectiveType::new(type_id, type_id, dimensions)
        .expect("fixture function type is valid");
    rumoca_core::FunctionParam::new(name, type_name, effective_type, test_span())
        .with_def_id(fixture_def_id(name))
}

fn fixture_def_id(name: &str) -> rumoca_core::DefId {
    let def_id = name.bytes().fold(1_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    rumoca_core::DefId::new(def_id.max(1))
}

fn var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: test_span(),
    }
}

fn resolved_var(name: &str, def_id: u32) -> rumoca_core::Expression {
    let Ok(component_ref) = rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: test_span(),
            subs: Vec::new(),
            def_id: rumoca_core::DefId::new(def_id),
        }],
    ) else {
        panic!("fixture reference must be exact");
    };
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(component_ref),
        subscripts: Vec::new(),
        span: test_span(),
    }
}

fn resolved_occurrence_var(name: &str, def_id: u32, instance_id: u32) -> rumoca_core::Expression {
    let rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    } = resolved_var(name, def_id)
    else {
        panic!("resolved occurrence fixture must remain a variable reference");
    };
    rumoca_core::Expression::VarRef {
        name: name.with_instance_id(rumoca_core::InstanceId::new(instance_id)),
        subscripts,
        span,
    }
}

fn resolved_occurrence_path(parts: &[(&str, u32)], instance_id: u32) -> rumoca_core::Expression {
    let Ok(component_ref) = rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        parts
            .iter()
            .map(|(name, def_id)| rumoca_core::ComponentRefPart {
                ident: (*name).to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: rumoca_core::DefId::new(*def_id),
            })
            .collect(),
    ) else {
        panic!("fixture path must carry exact declaration identities");
    };
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(component_ref)
            .with_instance_id(rumoca_core::InstanceId::new(instance_id)),
        subscripts: Vec::new(),
        span: test_span(),
    }
}

fn formal_var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: formal_reference(name),
        subscripts: Vec::new(),
        span: test_span(),
    }
}

fn formal_reference(name: &str) -> rumoca_core::Reference {
    let Ok(component_ref) = rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: test_span(),
            subs: Vec::new(),
            def_id: fixture_def_id(name),
        }],
    ) else {
        panic!("fixture formal reference must be exact");
    };
    rumoca_core::Reference::from_component_reference(component_ref)
}

fn indexed_var(name: &str, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: formal_reference(name),
        subscripts: vec![rumoca_core::Subscript::index(index, test_span())],
        span: test_span(),
    }
}

#[test]
fn param_evaluator_and_checked_dimension_inference_preserve_hard_errors() {
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let context = ParamEvalContext::new_structural(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
        Some("model.p"),
    );

    let empty = rumoca_core::Expression::Empty { span: test_span() };
    assert!(matches!(
        super::try_eval_integer_with_context(&empty, &context),
        Err(crate::constant::EvalError::InvalidSemanticIr { span, .. }) if span == test_span()
    ));
    assert!(matches!(
        super::infer_array_dimensions_checked(&empty),
        Err(crate::constant::EvalError::InvalidSemanticIr { span, .. }) if span == test_span()
    ));

    let runtime = var("runtime_value");
    assert_eq!(
        super::try_eval_integer_with_context(&runtime, &context).unwrap(),
        None
    );
}

fn index_expr(base: rumoca_core::Expression, index: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Index {
        base: Box::new(base),
        subscripts: vec![rumoca_core::Subscript::index(index, test_span())],
        span: test_span(),
    }
}

fn comp_ref(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: test_span(),
            subs: Vec::new(),
            def_id: fixture_def_id(name),
        }],
    )
    .expect("fixture assignment target is exact")
}

fn field(base: rumoca_core::Expression, name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: name.to_string(),
        field_def_id: rumoca_core::DefId::new(1),
        span: test_span(),
    }
}

fn int(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: test_span(),
    }
}

fn real(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: test_span(),
    }
}

fn boolean(value: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Boolean(value),
        span: test_span(),
    }
}

fn array(elements: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix: false,
        span: test_span(),
    }
}

fn binary(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(),
    }
}

fn empty_param_context<'a>(
    known_ints: &'a FxHashMap<String, i64>,
    known_reals: &'a FxHashMap<String, f64>,
    known_bools: &'a FxHashMap<String, bool>,
    known_enums: &'a FxHashMap<String, String>,
    array_dims: &'a FxHashMap<String, Vec<i64>>,
    functions: &'a FxHashMap<String, rumoca_core::Function>,
) -> ParamEvalContext<'a> {
    ParamEvalContext::new_structural(
        known_ints,
        known_reals,
        known_bools,
        known_enums,
        array_dims,
        functions,
        None,
    )
}

fn call(
    function: rumoca_core::BuiltinFunction,
    args: Vec<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function,
        args,
        span: test_span(),
    }
}

fn function_call(
    function: &rumoca_core::Function,
    args: Vec<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    let name = function.name.as_str();
    let Ok(reference) = rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: test_span(),
            subs: Vec::new(),
            def_id: function.exposure_def_id,
        }],
    ) else {
        panic!("fixture call target must be exact");
    };
    let Some(instance_id) = function.instance_id else {
        panic!("fixture function must have an exact instance identity");
    };
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(reference).with_resolved_function(
            rumoca_core::ResolvedFunctionReference {
                instance_id,
                base_part_count: 1,
                transitively_non_replaceable: function.transitively_non_replaceable,
            },
        ),
        args,
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    }
}

fn resolved_function_call(
    name: &str,
    instance_id: rumoca_core::FunctionInstanceId,
    transitively_non_replaceable: bool,
    args: Vec<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(name).with_resolved_function(
            rumoca_core::ResolvedFunctionReference {
                instance_id,
                base_part_count: 1,
                transitively_non_replaceable,
            },
        ),
        args,
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    }
}

fn scalar_indexing_function(
    name: &str,
    exposure_def_id: rumoca_core::DefId,
    scalar: TestScalar,
) -> rumoca_core::Function {
    let mut function = test_function(name, exposure_def_id, test_span());
    function.add_input(function_param("x", scalar, Vec::new()));
    function.add_output(function_param("y", scalar, Vec::new()));
    function.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("y"),
        value: indexed_var("x", 1),
        span: test_span(),
    }];
    function
}

#[test]
fn eval_integer_div_operator_requires_exact_quotient() {
    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Div,
        lhs: Box::new(int(7)),
        rhs: Box::new(int(2)),
        span: test_span(),
    };
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &known_reals,
        known_bools: &known_bools,
        array_dims: &array_dims,
        functions: &functions,
        identity_inventory: ParamIdentityInventory::StructuralPreIdentity {
            known_enums: &known_enums,
        },
        var_context: None,
    };
    assert!(matches!(
        super::try_eval_integer_with_context(&expr, &ctx),
        Err(crate::constant::EvalError::TypeMismatch { span, .. }) if span == test_span()
    ));
}

#[test]
fn eval_integer_exponentiation_for_structural_dimensions() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("n_addr".to_string(), 2);
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    let expr = call(
        rumoca_core::BuiltinFunction::Integer,
        vec![binary(rumoca_core::OpBinary::Exp, int(2), var("n_addr"))],
    );

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(4));
}

#[test]
fn infer_user_function_output_dims_from_shape_expr() {
    let mut functions = FxHashMap::default();
    let mut function = test_function("Pkg.indices", rumoca_core::DefId::new(12_003), test_span());
    function.add_input(function_param("m", TestScalar::Integer, Vec::new()));
    function.add_output(
        function_param("ind", TestScalar::Integer, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(
                Box::new(binary(rumoca_core::OpBinary::Add, formal_var("m"), int(1))),
                test_span(),
            ),
        ]),
    );
    functions.insert("Pkg.indices".to_string(), function);

    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let expr = function_call(&functions["Pkg.indices"], vec![int(4)]);

    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &expr,
            &ParamEvalContext::new_structural(
                &known_ints,
                &known_reals,
                &known_bools,
                &known_enums,
                &array_dims,
                &functions,
                None,
            ),
        ),
        Some(vec![5])
    );
}

fn size_shaped_function(name: &str, input_dims: Vec<i64>, axis: i64) -> rumoca_core::Function {
    let mut function = test_function(name, fixture_def_id(name), test_span());
    function.add_input(function_param("x", TestScalar::Real, input_dims));
    function.add_output(
        function_param("y", TestScalar::Real, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(
                Box::new(call(
                    rumoca_core::BuiltinFunction::Size,
                    vec![formal_var("x"), int(axis)],
                )),
                test_span(),
            ),
        ]),
    );
    function
}

#[test]
fn function_shape_frame_binds_array_literal_extents_and_named_actuals() {
    let mut functions = FxHashMap::default();
    functions.insert(
        "extent".to_string(),
        size_shaped_function("extent", vec![0], 1),
    );
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    for len in [2_i64, 3] {
        let actual = array((0..len).map(|value| real(value as f64)).collect());
        for args in [vec![actual.clone()], vec![named_marker("x", actual)]] {
            assert_eq!(
                infer_array_dimensions_full_with_functions(
                    &function_call(&functions["extent"], args),
                    &ctx,
                ),
                Some(vec![len])
            );
        }
    }
}

#[test]
fn function_shape_frame_uses_checked_shape_without_requiring_array_value() {
    let mut functions = FxHashMap::default();
    functions.insert(
        "extent".to_string(),
        size_shaped_function("extent", vec![0], 1),
    );
    functions.insert(
        "columns".to_string(),
        size_shaped_function("columns", vec![0, 0], 2),
    );
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let mut array_dims = FxHashMap::default();
    array_dims.insert("runtime_vector".to_string(), vec![3]);
    array_dims.insert("runtime_matrix".to_string(), vec![2, 4]);
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(&functions["extent"], vec![var("runtime_vector")]),
            &ctx
        ),
        Some(vec![3])
    );
    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(&functions["columns"], vec![var("runtime_matrix")]),
            &ctx
        ),
        Some(vec![4])
    );
}

#[test]
fn function_shape_frame_rejects_rank_and_invalid_actuals() {
    let mut functions = FxHashMap::default();
    functions.insert(
        "extent".to_string(),
        size_shaped_function("extent", vec![0], 1),
    );
    let mut scalar = size_shaped_function("scalar_extent", Vec::new(), 1);
    scalar.inputs[0] = function_param("x", TestScalar::Integer, Vec::new());
    functions.insert("scalar_extent".to_string(), scalar);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    let matrix = array(vec![array(vec![real(1.0), real(2.0)])]);
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(
            &function_call(&functions["extent"], vec![matrix]),
            &ctx
        ),
        Err(crate::constant::EvalError::FunctionError { .. })
    ));
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(
            &function_call(
                &functions["extent"],
                vec![rumoca_core::Expression::Empty { span: test_span() }]
            ),
            &ctx
        ),
        Err(crate::constant::EvalError::InvalidSemanticIr { .. })
    ));
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(
            &function_call(
                &functions["scalar_extent"],
                vec![array(vec![int(1), int(2)])],
            ),
            &ctx
        ),
        Err(crate::constant::EvalError::FunctionError { .. })
    ));
}

#[test]
fn function_shape_defaults_follow_dependencies_and_shadow_outer_names() {
    let mut function = test_function("later", rumoca_core::DefId::new(12_104), test_span());
    function.add_input(
        function_param("a", TestScalar::Integer, Vec::new()).with_default(formal_var("b")),
    );
    function.add_input(function_param("b", TestScalar::Integer, Vec::new()));
    function.add_output(
        function_param("y", TestScalar::Real, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(Box::new(formal_var("a")), test_span()),
        ]),
    );
    let mut functions = FxHashMap::default();
    functions.insert("later".to_string(), function);
    let mut known_ints = FxHashMap::default();
    known_ints.insert("model.b".to_string(), 99);
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let mut ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    ctx.var_context = Some("model.p");
    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(&functions["later"], vec![named_marker("b", int(2))]),
            &ctx
        ),
        Some(vec![2])
    );

    let mut cycle = functions["later"].clone();
    cycle.name = rumoca_core::VarName::new("cycle");
    cycle.exposure_def_id = rumoca_core::DefId::new(12_119);
    cycle.inputs[1] =
        function_param("b", TestScalar::Integer, Vec::new()).with_default(formal_var("a"));
    functions.insert("cycle".to_string(), cycle);
    let cycle_ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(
            &function_call(&functions["cycle"], Vec::new()),
            &cycle_ctx
        ),
        Err(crate::constant::EvalError::CircularDependency { .. })
    ));
}

#[test]
fn function_shape_default_alias_preserves_shape_only_zero_leading_extent() {
    let mut function = test_function("alias", rumoca_core::DefId::new(12_105), test_span());
    function
        .add_input(function_param("a", TestScalar::Real, vec![0, 0]).with_default(formal_var("b")));
    function.add_input(function_param("b", TestScalar::Real, vec![0, 0]));
    function.add_output(
        function_param("y", TestScalar::Real, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(
                Box::new(call(
                    rumoca_core::BuiltinFunction::Size,
                    vec![formal_var("a"), int(2)],
                )),
                test_span(),
            ),
        ]),
    );
    let mut functions = FxHashMap::default();
    functions.insert("alias".to_string(), function);
    let mut alias_vector = size_shaped_function("alias_vector", vec![0], 1);
    alias_vector.inputs[0] =
        function_param("a", TestScalar::Real, vec![0]).with_default(formal_var("b"));
    alias_vector.add_input(function_param("b", TestScalar::Real, vec![0]));
    alias_vector.outputs[0].shape_expr = vec![rumoca_core::Subscript::expr(
        Box::new(call(
            rumoca_core::BuiltinFunction::Size,
            vec![formal_var("a"), int(1)],
        )),
        test_span(),
    )];
    functions.insert("alias_vector".to_string(), alias_vector);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let mut array_dims = FxHashMap::default();
    array_dims.insert("runtime".to_string(), vec![0, 4]);
    array_dims.insert("runtime_vector".to_string(), vec![3]);
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(&functions["alias"], vec![named_marker("b", var("runtime"))],),
            &ctx
        ),
        Some(vec![4])
    );
    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(
                &functions["alias_vector"],
                vec![named_marker("b", var("runtime_vector"))]
            ),
            &ctx
        ),
        Some(vec![3])
    );
}

#[test]
fn function_shape_validates_fixed_extents_vectorization_and_final_extents() {
    let mut fixed = test_function("fixed", rumoca_core::DefId::new(12_106), test_span());
    fixed.add_input(function_param("x", TestScalar::Real, vec![2]));
    fixed.add_output(function_param("y", TestScalar::Real, vec![4]));

    let mut vectorized = test_function("vectorized", rumoca_core::DefId::new(12_107), test_span());
    vectorized.add_input(function_param("a", TestScalar::Real, Vec::new()));
    vectorized.add_input(function_param("b", TestScalar::Real, Vec::new()));
    vectorized.add_output(function_param("y", TestScalar::Real, Vec::new()));

    let mut negative = test_function("negative", rumoca_core::DefId::new(12_108), test_span());
    negative.add_input(function_param("x", TestScalar::Integer, Vec::new()));
    negative.add_output(
        function_param("y", TestScalar::Real, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(Box::new(int(-1)), test_span()),
        ]),
    );

    let mut functions = FxHashMap::default();
    functions.insert("fixed".to_string(), fixed);
    functions.insert("vectorized".to_string(), vectorized);
    functions.insert("negative".to_string(), negative);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    for (name, args) in [
        ("fixed", vec![array(vec![real(1.0), real(2.0), real(3.0)])]),
        (
            "vectorized",
            vec![array(vec![real(1.0), real(2.0)]), array(vec![real(1.0); 3])],
        ),
        ("negative", vec![int(1)]),
    ] {
        assert!(matches!(
            infer_user_function_call_dimensions(&function_call(&functions[name], args), &ctx),
            Err(crate::constant::EvalError::FunctionError { .. })
        ));
    }
}

#[test]
fn infer_scalar_user_function_broadcasts_array_argument_dims() {
    let mut functions = FxHashMap::default();
    let mut function = test_function("Cv.from_deg", rumoca_core::DefId::new(12_004), test_span());
    let instance_id = rumoca_core::FunctionInstanceId::new(12_004);
    function.instance_id = Some(instance_id);
    function.transitively_non_replaceable = true;
    function.add_input(function_param("degree", TestScalar::Real, Vec::new()));
    function.add_output(function_param("radian", TestScalar::Real, Vec::new()));
    functions.insert("Cv.from_deg".to_string(), function);

    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let mut array_dims = FxHashMap::default();
    array_dims.insert("pathPlanning.angleBegDeg".to_string(), vec![6]);
    let expr = resolved_function_call("Cv.from_deg", instance_id, true, vec![var("angleBegDeg")]);

    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &expr,
            &ParamEvalContext::new_structural(
                &known_ints,
                &known_reals,
                &known_bools,
                &known_enums,
                &array_dims,
                &functions,
                Some("pathPlanning.angleBeg"),
            ),
        ),
        Some(vec![6])
    );
}

#[test]
fn function_vectorization_requires_the_exact_occurrence_certificate() {
    let instance_id = rumoca_core::FunctionInstanceId::new(12_109);
    let mut function = test_function("vector", rumoca_core::DefId::new(12_109), test_span());
    function.instance_id = Some(instance_id);
    function.transitively_non_replaceable = true;
    function.add_input(function_param("x", TestScalar::Real, Vec::new()));
    function.add_output(function_param("y", TestScalar::Real, Vec::new()));
    let mut functions = FxHashMap::default();
    functions.insert("vector".to_string(), function);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    let actual = array(vec![real(1.0), real(2.0)]);

    let identity_absent = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("vector"),
        args: vec![actual.clone()],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    };
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(&identity_absent, &ctx),
        Err(crate::constant::EvalError::InvalidSemanticIr { .. })
    ));

    assert_eq!(
        super::infer_array_dimensions_full_with_functions(
            &function_call(&functions["vector"], vec![actual.clone()]),
            &ctx,
        )
        .expect("an exact non-replaceable occurrence authorizes vectorization"),
        Some(vec![2])
    );
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(
            &resolved_function_call("vector", instance_id, false, vec![actual.clone()]),
            &ctx,
        ),
        Err(crate::constant::EvalError::FunctionError { .. })
    ));
    let wrong_instance = resolved_function_call(
        "vector",
        rumoca_core::FunctionInstanceId::new(12_110),
        true,
        vec![actual.clone()],
    );
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(&wrong_instance, &ctx),
        Err(crate::constant::EvalError::InvalidSemanticIr { .. })
    ));

    let mut replaceable = functions["vector"].clone();
    replaceable.transitively_non_replaceable = false;
    let mut replaceable_functions = FxHashMap::default();
    replaceable_functions.insert("vector".to_string(), replaceable);
    let replaceable_ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &replaceable_functions,
    );
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(
            &resolved_function_call("vector", instance_id, true, vec![actual]),
            &replaceable_ctx,
        ),
        Err(crate::constant::EvalError::FunctionError { .. })
    ));
}

#[test]
fn function_shape_selects_instance_identity_before_rendered_name() {
    let first_instance = rumoca_core::FunctionInstanceId::new(12_115);
    let second_instance = rumoca_core::FunctionInstanceId::new(12_116);
    let mut first = test_function("same", rumoca_core::DefId::new(115), test_span());
    first.instance_id = Some(first_instance);
    first.add_input(function_param("x", TestScalar::Real, Vec::new()));
    first.add_output(function_param("y", TestScalar::Real, vec![2]));
    let mut second = test_function("same", rumoca_core::DefId::new(116), test_span());
    second.instance_id = Some(second_instance);
    second.add_input(function_param("x", TestScalar::Real, Vec::new()));
    second.add_output(function_param("y", TestScalar::Real, vec![4]));
    let mut functions = FxHashMap::default();
    functions.insert("same".to_string(), first);
    functions.insert("same#second".to_string(), second);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &resolved_function_call("same", second_instance, false, vec![real(1.0)]),
            &ctx,
        ),
        Some(vec![4])
    );
    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(&functions["same#second"], vec![real(1.0)]),
            &ctx,
        ),
        Some(vec![4])
    );
    let Ok(foreign_reference) = rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: "same".to_string(),
            span: test_span(),
            subs: Vec::new(),
            def_id: rumoca_core::DefId::new(117),
        }],
    ) else {
        panic!("foreign fixture call target must be exact");
    };
    let foreign_same_spelling = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_component_reference(foreign_reference),
        args: vec![real(1.0)],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    };
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(&foreign_same_spelling, &ctx),
        Err(crate::constant::EvalError::InvalidSemanticIr { .. })
    ));
}

#[test]
fn function_symbolic_input_extent_is_an_exact_obligation() {
    let mut function = test_function("sized", rumoca_core::DefId::new(12_110), test_span());
    function.add_input(function_param("n", TestScalar::Integer, Vec::new()));
    function.add_input(
        function_param("x", TestScalar::Real, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(Box::new(formal_var("n")), test_span()),
        ]),
    );
    function.add_output(
        function_param("y", TestScalar::Real, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(Box::new(formal_var("n")), test_span()),
        ]),
    );
    let mut functions = FxHashMap::default();
    functions.insert("sized".to_string(), function);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    let three = array(vec![real(1.0), real(2.0), real(3.0)]);

    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(
            &function_call(&functions["sized"], vec![int(2), three.clone()]),
            &ctx,
        ),
        Err(crate::constant::EvalError::FunctionError { .. })
    ));
    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(&functions["sized"], vec![int(3), three]),
            &ctx,
        ),
        Some(vec![3])
    );
}

#[test]
fn resolved_outer_value_uses_exact_inventory_without_same_spelling_fallback() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("model.b".to_string(), 41);
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let exact_identity = crate::constant::ResolvedOccurrenceKey {
        instance_id: rumoca_core::InstanceId::new(990),
        root_def_id: rumoca_core::DefId::new(99),
    };
    let mut values_by_identity = FxHashMap::default();
    values_by_identity.insert(exact_identity, Value::Integer(99));
    let array_dimensions_by_identity = FxHashMap::default();
    let enum_catalog = crate::constant::ResolvedEnumCatalog::empty();
    let ctx = ParamEvalContext::new_resolved(
        &known_ints,
        &known_reals,
        &known_bools,
        &array_dims,
        &functions,
        ResolvedParamInventory::new(
            &values_by_identity,
            &array_dimensions_by_identity,
            &enum_catalog,
        ),
        Some("model.p"),
    );

    assert_eq!(
        try_eval_integer_with_context(&resolved_occurrence_var("b", 99, 990), &ctx),
        Some(99)
    );
    assert_eq!(
        try_eval_integer_with_context(&resolved_occurrence_var("b", 98, 991), &ctx),
        None
    );
}

#[test]
fn resolved_multi_part_occurrences_keep_root_identity_for_values_and_shapes() {
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let mut rendered_shapes = FxHashMap::default();
    rendered_shapes.insert("a.n".to_string(), vec![99]);
    rendered_shapes.insert("b.n".to_string(), vec![99]);
    let functions = FxHashMap::default();
    let a_identity = crate::constant::ResolvedOccurrenceKey {
        instance_id: rumoca_core::InstanceId::new(700),
        root_def_id: rumoca_core::DefId::new(10),
    };
    let b_identity = crate::constant::ResolvedOccurrenceKey {
        instance_id: rumoca_core::InstanceId::new(701),
        root_def_id: rumoca_core::DefId::new(20),
    };
    let c_first_identity = crate::constant::ResolvedOccurrenceKey {
        instance_id: rumoca_core::InstanceId::new(702),
        root_def_id: rumoca_core::DefId::new(40),
    };
    let c_second_identity = crate::constant::ResolvedOccurrenceKey {
        instance_id: rumoca_core::InstanceId::new(703),
        root_def_id: rumoca_core::DefId::new(40),
    };
    let mut values = FxHashMap::default();
    values.insert(a_identity, Value::Integer(2));
    values.insert(b_identity, Value::Integer(3));
    values.insert(c_first_identity, Value::Integer(4));
    values.insert(c_second_identity, Value::Integer(5));
    let mut shapes = FxHashMap::default();
    shapes.insert(a_identity, vec![2, 0, 3]);
    shapes.insert(b_identity, vec![0, 3]);
    shapes.insert(c_first_identity, vec![4]);
    shapes.insert(c_second_identity, vec![5]);
    let enum_catalog = crate::constant::ResolvedEnumCatalog::empty();
    let ctx = ParamEvalContext::new_resolved(
        &known_ints,
        &known_reals,
        &known_bools,
        &rendered_shapes,
        &functions,
        ResolvedParamInventory::new(&values, &shapes, &enum_catalog),
        Some("model.p"),
    );
    let a = resolved_occurrence_path(&[("a", 10), ("n", 77)], 700);
    let b = resolved_occurrence_path(&[("b", 20), ("n", 77)], 701);
    let foreign_same_tail = resolved_occurrence_path(&[("foreign", 30), ("n", 77)], 700);
    let c_first = resolved_occurrence_path(&[("C", 40), ("n", 77)], 702);
    let c_second = resolved_occurrence_path(&[("C", 40), ("n", 77)], 703);

    assert_eq!(try_eval_integer_with_context(&a, &ctx), Some(2));
    assert_eq!(try_eval_integer_with_context(&b, &ctx), Some(3));
    assert_eq!(
        try_eval_integer_with_context(&foreign_same_tail, &ctx),
        None
    );
    assert_eq!(try_eval_integer_with_context(&c_first, &ctx), Some(4));
    assert_eq!(try_eval_integer_with_context(&c_second, &ctx), Some(5));
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(&a, &ctx),
        Ok(Some(dimensions)) if dimensions == vec![2, 0, 3]
    ));
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(&b, &ctx),
        Ok(Some(dimensions)) if dimensions == vec![0, 3]
    ));
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(&foreign_same_tail, &ctx),
        Ok(None)
    ));
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(&c_first, &ctx),
        Ok(Some(dimensions)) if dimensions == vec![4]
    ));
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(&c_second, &ctx),
        Ok(Some(dimensions)) if dimensions == vec![5]
    ));
}

#[test]
fn foreign_same_spelling_default_keeps_outer_tensor_shape_identity() {
    let mut a = function_param("b", TestScalar::Real, vec![0]);
    a.name = "a".to_string();
    a.def_id = Some(rumoca_core::DefId::new(1));
    a.default = Some(resolved_occurrence_var("b", 99, 990));
    let mut b = function_param("b", TestScalar::Real, vec![0]);
    b.def_id = Some(rumoca_core::DefId::new(2));
    let mut function = test_function("foreign", rumoca_core::DefId::new(12_111), test_span());
    function.add_input(a);
    function.add_input(b);
    function.add_output(
        function_param("y", TestScalar::Real, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(
                Box::new(call(
                    rumoca_core::BuiltinFunction::Size,
                    vec![resolved_var("a", 1), int(1)],
                )),
                test_span(),
            ),
        ]),
    );
    let mut functions = FxHashMap::default();
    functions.insert("foreign".to_string(), function);
    let mut missing = functions["foreign"].clone();
    missing.name = rumoca_core::VarName::new("foreign_missing");
    missing.exposure_def_id = rumoca_core::DefId::new(12_118);
    missing.instance_id = Some(rumoca_core::FunctionInstanceId::new(12_118));
    missing.inputs[0].default = Some(resolved_occurrence_var("b", 98, 991));
    functions.insert("foreign_missing".to_string(), missing);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let mut array_dims = FxHashMap::default();
    array_dims.insert("model.b".to_string(), vec![7]);
    let values_by_identity = FxHashMap::default();
    let mut array_dimensions_by_identity = FxHashMap::default();
    array_dimensions_by_identity.insert(
        crate::constant::ResolvedOccurrenceKey {
            instance_id: rumoca_core::InstanceId::new(990),
            root_def_id: rumoca_core::DefId::new(99),
        },
        vec![4],
    );
    let enum_catalog = crate::constant::ResolvedEnumCatalog::empty();
    let ctx = ParamEvalContext::new_resolved(
        &known_ints,
        &known_reals,
        &known_bools,
        &array_dims,
        &functions,
        ResolvedParamInventory::new(
            &values_by_identity,
            &array_dimensions_by_identity,
            &enum_catalog,
        ),
        Some("model.p"),
    );

    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(
            &resolved_occurrence_var("b", 99, 990),
            &ctx,
        ),
        Ok(Some(shape)) if shape == vec![4]
    ));
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(
            &resolved_occurrence_var("b", 98, 991),
            &ctx,
        ),
        Ok(None)
    ));

    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(
                &functions["foreign"],
                vec![named_marker("b", array(vec![real(1.0), real(2.0)]))],
            ),
            &ctx,
        ),
        Some(vec![4])
    );
    assert!(matches!(
        super::infer_array_dimensions_full_with_functions(
            &function_call(
                &functions["foreign_missing"],
                vec![named_marker("b", array(vec![real(1.0), real(2.0)]))],
            ),
            &ctx,
        ),
        Err(crate::constant::EvalError::FunctionError { .. })
    ));
}

#[test]
fn function_array_actuals_are_rectangular_and_scalar_typed() {
    let mut function = test_function("matrix", rumoca_core::DefId::new(12_112), test_span());
    function.add_input(function_param("x", TestScalar::Real, vec![0, 0]));
    function.add_output(function_param("y", TestScalar::Real, Vec::new()));
    let mut boolean_function = test_function(
        "boolean_matrix",
        rumoca_core::DefId::new(12_117),
        test_span(),
    );
    boolean_function.add_input(function_param("x", TestScalar::Boolean, vec![0, 0]));
    boolean_function.add_output(function_param("y", TestScalar::Boolean, vec![1]));
    let mut functions = FxHashMap::default();
    functions.insert("matrix".to_string(), function);
    functions.insert("boolean_matrix".to_string(), boolean_function);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    let ragged = array(vec![
        array(vec![real(1.0), real(2.0)]),
        array(vec![real(3.0)]),
    ]);
    let booleans = array(vec![
        array(vec![boolean(true), boolean(false)]),
        array(vec![boolean(false), boolean(true)]),
    ]);

    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(&functions["boolean_matrix"], vec![booleans.clone()]),
            &ctx,
        ),
        Some(vec![1])
    );

    for actual in [ragged, booleans] {
        assert!(matches!(
            super::infer_array_dimensions_full_with_functions(
                &function_call(&functions["matrix"], vec![actual]),
                &ctx,
            ),
            Err(crate::constant::EvalError::FunctionError { .. }
                | crate::constant::EvalError::TypeMismatch { .. })
        ));
    }
}

#[test]
fn empty_array_actuals_require_complete_trailing_extent_metadata() {
    let mut matrix = test_function("empty_matrix", rumoca_core::DefId::new(12_113), test_span());
    matrix.add_input(function_param("x", TestScalar::Real, vec![0, 3]));
    matrix.add_output(function_param("y", TestScalar::Real, vec![1]));
    let mut tensor = test_function("empty_tensor", rumoca_core::DefId::new(12_114), test_span());
    tensor.add_input(function_param("x", TestScalar::Real, vec![2, 0, 3]));
    tensor.add_output(function_param("y", TestScalar::Real, vec![1]));
    let mut functions = FxHashMap::default();
    functions.insert("empty_matrix".to_string(), matrix);
    functions.insert("empty_tensor".to_string(), tensor);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let mut array_dims = FxHashMap::default();
    array_dims.insert("m".to_string(), vec![0, 3]);
    array_dims.insert("bad_m".to_string(), vec![0, 4]);
    array_dims.insert("t".to_string(), vec![2, 0, 3]);
    array_dims.insert("bad_t".to_string(), vec![2, 0, 4]);
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    for (function, actual) in [("empty_matrix", "m"), ("empty_tensor", "t")] {
        assert_eq!(
            infer_array_dimensions_full_with_functions(
                &function_call(&functions[function], vec![var(actual)]),
                &ctx,
            ),
            Some(vec![1])
        );
    }
    for (function, actual) in [("empty_matrix", "bad_m"), ("empty_tensor", "bad_t")] {
        assert!(matches!(
            super::infer_array_dimensions_full_with_functions(
                &function_call(&functions[function], vec![var(actual)]),
                &ctx,
            ),
            Err(crate::constant::EvalError::FunctionError { .. })
        ));
    }
    for call in [
        function_call(&functions["empty_matrix"], vec![array(Vec::new())]),
        function_call(
            &functions["empty_tensor"],
            vec![array(vec![array(Vec::new()), array(Vec::new())])],
        ),
    ] {
        assert!(matches!(
            super::infer_array_dimensions_full_with_functions(&call, &ctx),
            Err(crate::constant::EvalError::UnsupportedExpression { .. })
        ));
    }
}

#[test]
fn user_function_integer_eval_error_remains_a_typed_error() {
    let mut functions = FxHashMap::default();
    functions.insert(
        "Pkg.badInteger".to_string(),
        scalar_indexing_function(
            "Pkg.badInteger",
            rumoca_core::DefId::new(12_001),
            TestScalar::Integer,
        ),
    );
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    let expr = function_call(&functions["Pkg.badInteger"], vec![int(3)]);

    let error = super::try_eval_integer_with_context(&expr, &ctx)
        .expect_err("invalid function evaluation must remain a hard error");
    assert!(error.runtime_dependent_reason().is_none());
    assert_eq!(error.span(), Some(test_span()));
}

#[test]
fn user_function_real_eval_error_remains_a_typed_error() {
    let mut functions = FxHashMap::default();
    functions.insert(
        "Pkg.badReal".to_string(),
        scalar_indexing_function(
            "Pkg.badReal",
            rumoca_core::DefId::new(12_002),
            TestScalar::Real,
        ),
    );
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    let error = super::eval_user_func_real(
        &rumoca_core::Reference::new("Pkg.badReal"),
        &[real(3.0)],
        &ctx,
    )
    .expect_err("invalid function evaluation must remain a hard error");
    assert!(error.runtime_dependent_reason().is_none());
    assert_eq!(error.span(), Some(test_span()));
}

#[test]
fn an_overqualified_rendered_name_is_never_recovered_by_stripping_segments() {
    // A reference whose rendered path does not name a known value is not a
    // value: the evaluator has no licence to search shorter renderings of it
    // for something that happens to look similar. The unique case is rejected
    // for exactly the same reason as the ambiguous one — neither carries the
    // identity of the declaration being read.
    let mut unique = FxHashMap::default();
    unique.insert("outer.n".to_string(), 2);
    let mut ambiguous = unique.clone();
    ambiguous.insert("n".to_string(), 3);

    for known_ints in [&unique, &ambiguous] {
        let known_reals = FxHashMap::default();
        let known_bools = FxHashMap::default();
        let known_enums = FxHashMap::default();
        let array_dims = FxHashMap::default();
        let functions = FxHashMap::default();
        let ctx = empty_param_context(
            known_ints,
            &known_reals,
            &known_bools,
            &known_enums,
            &array_dims,
            &functions,
        );

        assert_eq!(
            try_eval_integer_with_context(&var("model.outer.n"), &ctx),
            None
        );
    }
}

#[test]
fn eval_integer_div_builtin_remains_truncating() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Div,
        args: vec![int(7), int(2)],
        span: test_span(),
    };
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &known_reals,
        known_bools: &known_bools,
        array_dims: &array_dims,
        functions: &functions,
        identity_inventory: ParamIdentityInventory::StructuralPreIdentity {
            known_enums: &known_enums,
        },
        var_context: None,
    };
    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(3));
}

#[test]
fn eval_real_with_context_evaluates_builtin_sqrt_chain() {
    let mut known_reals = FxHashMap::default();
    known_reals.insert("l".to_string(), 1.0e-6);
    known_reals.insert("c".to_string(), 15.0e-12);
    known_reals.insert("len".to_string(), 100.0e3);
    let known_ints = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    let c0 = binary(
        rumoca_core::OpBinary::Div,
        real(1.0),
        call(
            rumoca_core::BuiltinFunction::Sqrt,
            vec![binary(rumoca_core::OpBinary::Mul, var("l"), var("c"))],
        ),
    );
    let td = binary(rumoca_core::OpBinary::Div, var("len"), c0);

    let value = try_eval_real_with_context(&td, &ctx).expect("td should evaluate");
    let expected = 100.0e3 * (1.0e-6_f64 * 15.0e-12_f64).sqrt();
    assert!((value - expected).abs() < 1.0e-15);
}

#[test]
fn eval_real_with_context_evaluates_named_builtin_sqrt() {
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    let Some(value) = try_eval_real_with_context(
        &call(rumoca_core::BuiltinFunction::Sqrt, vec![real(4.0)]),
        &ctx,
    ) else {
        panic!("named builtin sqrt must evaluate");
    };
    assert!((value - 2.0).abs() < 1.0e-15);
}

#[test]
fn eval_integer_mod_builtin_uses_floor_semantics() {
    let expr = call(rumoca_core::BuiltinFunction::Mod, vec![int(-7), int(3)]);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(2));
}

#[test]
fn eval_integer_rem_builtin_remains_truncating() {
    let expr = call(rumoca_core::BuiltinFunction::Rem, vec![int(-7), int(3)]);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(-1));
}

#[test]
fn eval_max_min_integer_uses_full_structural_context() {
    let mut known_reals = FxHashMap::default();
    known_reals.insert("f_max".to_string(), 4.0);
    known_reals.insert("f_resolution".to_string(), 0.2);
    let mut known_ints = FxHashMap::default();
    known_ints.insert("nf".to_string(), 50);
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    let frequency_ratio = binary(
        rumoca_core::OpBinary::Div,
        var("f_max"),
        var("f_resolution"),
    );
    let ceil_ratio = call(rumoca_core::BuiltinFunction::Ceil, vec![frequency_ratio]);
    let integer_ceil_ratio = call(rumoca_core::BuiltinFunction::Integer, vec![ceil_ratio]);
    let selected = call(
        rumoca_core::BuiltinFunction::Min,
        vec![
            binary(rumoca_core::OpBinary::Add, integer_ceil_ratio, int(1)),
            var("nf"),
        ],
    );
    let expr = call(rumoca_core::BuiltinFunction::Max, vec![int(1), selected]);

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(21));
}

#[test]
fn eval_boolean_enum_eq_accepts_different_qualification_paths() {
    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "pipe.modelStructure".to_string(),
        "Modelica.Fluid.Types.ModelStructure.a_vb".to_string(),
    );

    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("pipe.modelStructure")),
        rhs: Box::new(var("pipe.Types.ModelStructure.a_vb")),
        span: test_span(),
    };

    let value = try_eval_flat_expr_boolean(
        &expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &known_enums,
    );

    assert_eq!(value, Some(true));
}

#[test]
fn enum_canonicalization_matches_complete_path_segments() {
    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "proper".to_string(),
        "Modelica.Fluid.Types.ModelStructure.a_vb".to_string(),
    );
    known_enums.insert(
        "unrelated".to_string(),
        "Modelica.Fluid.NotTypes.ModelStructure.a_vb".to_string(),
    );

    assert_eq!(
        canonicalize_enum_literal("pipe.Types.ModelStructure.a_vb", &known_enums),
        "Modelica.Fluid.Types.ModelStructure.a_vb",
        "a suffix must begin at a component-path segment boundary",
    );
}

#[test]
fn enum_eval_preserves_nested_enumeration_type_path() {
    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "initType".to_string(),
        "Modelica.Blocks.Types.Init.NoInit".to_string(),
    );

    // MLS §4.8.5: identity is the enumeration type plus the literal, so the
    // complete nested type path must survive evaluation of both a parameter
    // reference and a scope-qualified literal reached through a subscripted
    // component instance.
    assert_eq!(
        try_eval_flat_expr_enum(
            &var("initType"),
            &FxHashMap::default(),
            &FxHashMap::default(),
            &known_enums,
        ),
        Some("Modelica.Blocks.Types.Init.NoInit".to_string()),
    );
    assert_eq!(
        try_eval_flat_expr_enum(
            &var("controllers[bus.index].Types.Init.NoInit"),
            &FxHashMap::default(),
            &FxHashMap::default(),
            &known_enums,
        ),
        Some("Modelica.Blocks.Types.Init.NoInit".to_string()),
    );
}

#[test]
fn eval_boolean_enum_eq_accepts_shared_type_literal_tail() {
    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "frameResolve".to_string(),
        "sensor_frame_a2.MultiBody.Types.ResolveInFrameA.frame_resolve".to_string(),
    );

    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("frameResolve")),
        rhs: Box::new(var(
            "Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve",
        )),
        span: test_span(),
    };

    let value = try_eval_flat_expr_boolean(
        &expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &known_enums,
    );
    assert_eq!(value, Some(true));
}

#[test]
fn eval_boolean_enum_eq_rejects_different_enum_type() {
    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "mode".to_string(),
        "Modelica.Blocks.Types.Init.PI".to_string(),
    );

    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("mode")),
        rhs: Box::new(var("Modelica.Blocks.Types.SimpleController.PI")),
        span: test_span(),
    };

    let value = try_eval_flat_expr_boolean(
        &expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &known_enums,
    );
    assert_eq!(value, Some(false));
}

#[test]
fn eval_integer_if_uses_canonicalized_enum_condition() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("pipe.n".to_string(), 1);

    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "pipe.modelStructure".to_string(),
        "Modelica.Fluid.Types.ModelStructure.a_vb".to_string(),
    );

    let cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("pipe.modelStructure")),
        rhs: Box::new(var("pipe.Types.ModelStructure.a_vb")),
        span: test_span(),
    };

    let expr = rumoca_core::Expression::If {
        branches: vec![(
            cond,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                lhs: Box::new(var("pipe.n")),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: test_span(),
                }),
                span: test_span(),
            },
        )],
        else_branch: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: test_span(),
        }),
        span: test_span(),
    };

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        identity_inventory: ParamIdentityInventory::StructuralPreIdentity {
            known_enums: &known_enums,
        },
        var_context: Some("pipe.nFMDistributed"),
    };

    let value = try_eval_integer_with_context(&expr, &ctx);
    assert_eq!(value, Some(2));
}

#[test]
fn eval_integer_if_resolves_unqualified_enum_condition_with_var_context() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("Bessel.order".to_string(), 3);

    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "Bessel.filterType".to_string(),
        "Modelica.Blocks.Types.FilterType.LowPass".to_string(),
    );

    let cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Or,
        lhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandPass")),
            span: test_span(),
        }),
        rhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandStop")),
            span: test_span(),
        }),
        span: test_span(),
    };

    let expr = rumoca_core::Expression::If {
        branches: vec![(
            cond,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(int(2)),
                rhs: Box::new(var("order")),
                span: test_span(),
            },
        )],
        else_branch: Box::new(var("order")),
        span: test_span(),
    };

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        identity_inventory: ParamIdentityInventory::StructuralPreIdentity {
            known_enums: &known_enums,
        },
        var_context: Some("Bessel.na"),
    };

    let value = try_eval_integer_with_context(&expr, &ctx);
    assert_eq!(value, Some(3));
}

#[test]
fn eval_integer_prefers_scoped_unqualified_name_over_global_name() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("m".to_string(), 3);
    known_ints.insert("machine.rotor.converter.m".to_string(), 2);

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        identity_inventory: ParamIdentityInventory::StructuralPreIdentity {
            known_enums: &FxHashMap::default(),
        },
        var_context: Some("machine.rotor.converter.orientation"),
    };

    assert_eq!(try_eval_integer_with_context(&var("m"), &ctx), Some(2));
}

#[test]
fn eval_integer_if_handles_integer_builtin_with_scoped_enum_conditions() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("Bessel.order".to_string(), 3);

    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "Bessel.filterType".to_string(),
        "Modelica.Blocks.Types.FilterType.LowPass".to_string(),
    );
    known_enums.insert(
        "Bessel.analogFilter".to_string(),
        "Modelica.Blocks.Types.AnalogFilter.Bessel".to_string(),
    );

    let filter_is_band = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Or,
        lhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandPass")),
            span: test_span(),
        }),
        rhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Box::new(var("filterType")),
            rhs: Box::new(var("Modelica.Blocks.Types.FilterType.BandStop")),
            span: test_span(),
        }),
        span: test_span(),
    };

    let analog_is_cd = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("analogFilter")),
        rhs: Box::new(var("Modelica.Blocks.Types.AnalogFilter.CriticalDamping")),
        span: test_span(),
    };

    let expr = rumoca_core::Expression::If {
        branches: vec![(filter_is_band, var("order")), (analog_is_cd, int(0))],
        else_branch: Box::new(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Integer,
            args: vec![rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Div,
                lhs: Box::new(var("order")),
                rhs: Box::new(int(2)),
                span: test_span(),
            }],
            span: test_span(),
        }),
        span: test_span(),
    };

    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        identity_inventory: ParamIdentityInventory::StructuralPreIdentity {
            known_enums: &known_enums,
        },
        var_context: Some("Bessel.na"),
    };

    let value = try_eval_integer_with_context(&expr, &ctx);
    assert_eq!(value, Some(1));
}

#[test]
fn extract_enum_value_ignores_dotted_parameter_refs() {
    let extracted = try_extract_enum_value(&var("pipe1.system.energyDynamics"));
    assert_eq!(extracted, None);
}

#[test]
fn extract_enum_value_accepts_scoped_enum_literal_paths() {
    let extracted = try_extract_enum_value(&var("pipe.Types.ModelStructure.a_v_b"));
    assert_eq!(
        extracted,
        Some("pipe.Types.ModelStructure.a_v_b".to_string())
    );
}

#[test]
fn extract_enum_value_ignores_uppercase_name_with_dot_only_inside_subscript() {
    let extracted = try_extract_enum_value(&var("TypeAlias[data.medium]"));
    assert_eq!(extracted, None);
}

#[test]
fn eval_boolean_enum_eq_does_not_guess_dotted_parameter_ref_literal() {
    let mut known_enums = FxHashMap::default();
    known_enums.insert(
        "pipe.energyDynamics".to_string(),
        "Modelica.Fluid.Types.Dynamics.SteadyStateInitial".to_string(),
    );

    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("pipe.energyDynamics")),
        rhs: Box::new(var("pipe1.system.energyDynamics")),
        span: test_span(),
    };

    let value = try_eval_flat_expr_boolean(
        &expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &known_enums,
    );
    assert_eq!(value, None);
}

#[test]
fn eval_integer_field_access_does_not_recover_an_overqualified_record_path() {
    // The record path reaching the evaluator is over-qualified relative to the
    // key that holds the value. Recovering it would mean guessing which record
    // occurrence the reference meant from the shape of its rendering, so the
    // field access stays unevaluated and the caller keeps the binding.
    let mut known_ints = FxHashMap::default();
    known_ints.insert("stackData.cellData[1,1].nRC".to_string(), 2);

    let expr = field(var("stack.cell[1,1].cell.stackData.cellData[1,1]"), "nRC");
    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        identity_inventory: ParamIdentityInventory::StructuralPreIdentity {
            known_enums: &FxHashMap::default(),
        },
        var_context: Some("stack.cell[1,1].cell.cellData.nRC"),
    };

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), None);
}

#[test]
fn eval_integer_if_returns_common_value_when_condition_unknown() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("left".to_string(), 2);
    known_ints.insert("right".to_string(), 2);

    let expr = rumoca_core::Expression::If {
        branches: vec![(var("cond"), var("left"))],
        else_branch: Box::new(var("right")),
        span: test_span(),
    };
    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        identity_inventory: ParamIdentityInventory::StructuralPreIdentity {
            known_enums: &FxHashMap::default(),
        },
        var_context: None,
    };

    assert_eq!(try_eval_integer_with_context(&expr, &ctx), Some(2));
}

#[test]
fn component_path_parent_ignores_dot_inside_subscript_expression() {
    assert_eq!(
        rumoca_core::ComponentPath::from_flat_path("arr[data.medium]")
            .parent()
            .map(|path| path.to_flat_string()),
        Some(String::new())
    );
    assert_eq!(
        rumoca_core::ComponentPath::from_flat_path("pkg.arr[data.medium]")
            .parent()
            .map(|path| path.to_flat_string())
            .as_deref(),
        Some("pkg")
    );
    assert_eq!(
        rumoca_core::ComponentPath::from_flat_path("pkg.arr[data.medium].field")
            .parent()
            .map(|path| path.to_flat_string())
            .as_deref(),
        Some("pkg.arr[data.medium]")
    );
}

#[test]
fn scoped_lookup_keeps_a_dot_inside_a_subscript_within_one_path_segment() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("pkg.arr[data.medium].x".to_string(), 1);
    // A decoy that only a lookup splitting the path at the dot *inside* the
    // subscript could ever reach.
    known_ints.insert("medium].x".to_string(), 99);
    let ctx = ParamEvalContext {
        known_ints: &known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        identity_inventory: ParamIdentityInventory::StructuralPreIdentity {
            known_enums: &FxHashMap::default(),
        },
        var_context: Some("pkg.arr[data.medium].y"),
    };

    assert_eq!(
        try_eval_integer_with_context(&var("x"), &ctx),
        Some(1),
        "the enclosing scope of `pkg.arr[data.medium].y` is `pkg.arr[data.medium]`"
    );
}

#[test]
fn eval_enum_if_resolves_selected_branch_with_known_bool_condition() {
    let mut known_bools = FxHashMap::default();
    known_bools.insert("Medium.singleState".to_string(), true);

    let expr = rumoca_core::Expression::If {
        branches: vec![(var("Medium.singleState"), var("Dynamics.SteadyState"))],
        else_branch: Box::new(var("Dynamics.SteadyStateInitial")),
        span: test_span(),
    };

    let value = try_eval_flat_expr_enum(
        &expr,
        &FxHashMap::default(),
        &known_bools,
        &FxHashMap::default(),
    );
    assert_eq!(value, Some("Dynamics.SteadyState".to_string()));
}

#[test]
fn eval_enum_if_returns_common_value_when_condition_unknown() {
    let expr = rumoca_core::Expression::If {
        branches: vec![(var("cond"), var("Dynamics.SteadyState"))],
        else_branch: Box::new(var("Dynamics.SteadyState")),
        span: test_span(),
    };

    let value = try_eval_flat_expr_enum(
        &expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
    );
    assert_eq!(value, Some("Dynamics.SteadyState".to_string()));
}

#[test]
fn infer_array_dims_from_comprehension_range_and_body() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("n".to_string(), 4);

    let expr = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(var("n")),
                span: test_span(),
            },
        }],
        filter: None,
        span: test_span(),
    };

    let dims = infer_array_dimensions_full_with_conds(
        &expr,
        &known_ints,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
    );
    assert_eq!(dims, Some(vec![4]));
}

#[test]
fn infer_array_dims_with_context_resolves_scoped_if_matrix_columns() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("booleanTable.n".to_string(), 7);
    let mut array_dims = FxHashMap::default();
    array_dims.insert("booleanTable.table".to_string(), vec![7]);

    let table_column = var("table");
    let generated_column = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(var("n")),
                span: test_span(),
            },
        }],
        filter: None,
        span: test_span(),
    };
    let then_matrix = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Array {
                elements: vec![indexed_var("booleanTable.table", 1), int(0)],
                is_matrix: false,
                span: test_span(),
            },
            rumoca_core::Expression::Array {
                elements: vec![table_column, generated_column],
                is_matrix: false,
                span: test_span(),
            },
        ],
        is_matrix: true,
        span: test_span(),
    };
    let else_matrix = rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::Array {
            elements: vec![int(0), int(0)],
            is_matrix: false,
            span: test_span(),
        }],
        is_matrix: true,
        span: test_span(),
    };
    let expr = rumoca_core::Expression::If {
        branches: vec![(
            binary(rumoca_core::OpBinary::Gt, var("n"), int(0)),
            then_matrix,
        )],
        else_branch: Box::new(else_matrix),
        span: test_span(),
    };

    let dims = infer_array_dimensions_full_with_functions(
        &expr,
        &ParamEvalContext::new_structural(
            &known_ints,
            &FxHashMap::default(),
            &FxHashMap::default(),
            &FxHashMap::default(),
            &array_dims,
            &FxHashMap::default(),
            Some("booleanTable.combiTimeTable.table"),
        ),
    );

    assert_eq!(dims, Some(vec![8, 2]));
}

#[test]
fn infer_array_dims_with_context_preserves_vector_column_rows() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("booleanTable.n".to_string(), 7);
    let mut known_bools = FxHashMap::default();
    known_bools.insert("booleanTable.startValue".to_string(), false);
    let mut array_dims = FxHashMap::default();
    array_dims.insert("booleanTable.table".to_string(), vec![7]);

    let generated_column = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(var("n")),
                span: test_span(),
            },
        }],
        filter: None,
        span: test_span(),
    };
    let vector_column_matrix = rumoca_core::Expression::Array {
        elements: vec![var("table"), generated_column],
        is_matrix: true,
        span: test_span(),
    };
    let expr = rumoca_core::Expression::If {
        branches: vec![(var("startValue"), int(0))],
        else_branch: Box::new(vector_column_matrix),
        span: test_span(),
    };

    let dims = infer_array_dimensions_full_with_functions(
        &expr,
        &ParamEvalContext::new_structural(
            &known_ints,
            &FxHashMap::default(),
            &known_bools,
            &FxHashMap::default(),
            &array_dims,
            &FxHashMap::default(),
            Some("booleanTable.combiTimeTable.table"),
        ),
    );

    assert_eq!(dims, Some(vec![7, 2]));
}

#[test]
fn infer_array_dims_with_context_resolves_boolean_table_binding() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("booleanTable.n".to_string(), 7);
    let mut known_bools = FxHashMap::default();
    known_bools.insert("booleanTable.startValue".to_string(), false);
    let mut array_dims = FxHashMap::default();
    array_dims.insert("booleanTable.table".to_string(), vec![7]);

    let generated_column = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(call(
            rumoca_core::BuiltinFunction::Mod,
            vec![var("i"), real(2.0)],
        )),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(var("booleanTable.n")),
                span: test_span(),
            },
        }],
        filter: None,
        span: test_span(),
    };
    let false_branch = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Array {
                elements: vec![index_expr(var("booleanTable.table"), 1), real(0.0)],
                is_matrix: true,
                span: test_span(),
            },
            rumoca_core::Expression::Array {
                elements: vec![var("booleanTable.table"), generated_column],
                is_matrix: true,
                span: test_span(),
            },
        ],
        is_matrix: true,
        span: test_span(),
    };
    let expr = rumoca_core::Expression::If {
        branches: vec![(
            binary(rumoca_core::OpBinary::Gt, var("booleanTable.n"), int(0)),
            rumoca_core::Expression::If {
                branches: vec![(var("booleanTable.startValue"), int(0))],
                else_branch: Box::new(false_branch),
                span: test_span(),
            },
        )],
        else_branch: Box::new(int(0)),
        span: test_span(),
    };

    let dims = infer_array_dimensions_full_with_functions(
        &expr,
        &ParamEvalContext::new_structural(
            &known_ints,
            &FxHashMap::default(),
            &known_bools,
            &FxHashMap::default(),
            &array_dims,
            &FxHashMap::default(),
            Some("booleanTable.combiTimeTable.table"),
        ),
    );

    assert_eq!(dims, Some(vec![8, 2]));
}

#[test]
fn infer_array_dims_from_nested_comprehension_body_shape() {
    let expr = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(rumoca_core::Expression::Array {
            elements: vec![var("i"), var("i")],
            is_matrix: false,
            span: test_span(),
        }),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(int(3)),
                span: test_span(),
            },
        }],
        filter: None,
        span: test_span(),
    };

    let dims = infer_array_dimensions_checked(&expr);
    assert!(matches!(dims, Ok(Some(shape)) if shape == vec![3, 2]));
}

#[test]
fn infer_array_dims_vector_of_vertical_matrix_concat_uses_operand_shapes() {
    let mut known_ints = FxHashMap::default();
    known_ints.insert("na".to_string(), 3);
    known_ints.insert("nb".to_string(), 2);
    let mut array_dims = FxHashMap::default();
    array_dims.insert("b".to_string(), vec![2]);

    let zero_rows = call(
        rumoca_core::BuiltinFunction::Zeros,
        vec![
            call(
                rumoca_core::BuiltinFunction::Max,
                vec![
                    int(0),
                    rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Sub,
                        lhs: Box::new(var("na")),
                        rhs: Box::new(var("nb")),
                        span: test_span(),
                    },
                ],
            ),
            int(1),
        ],
    );
    let matrix = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Array {
                elements: vec![zero_rows],
                is_matrix: true,
                span: test_span(),
            },
            rumoca_core::Expression::Array {
                elements: vec![var("b")],
                is_matrix: true,
                span: test_span(),
            },
        ],
        is_matrix: true,
        span: test_span(),
    };
    let expr = call(rumoca_core::BuiltinFunction::Vector, vec![matrix]);

    assert_eq!(
        infer_array_dimensions_full_with_conds(
            &expr,
            &known_ints,
            &FxHashMap::default(),
            &FxHashMap::default(),
            &array_dims,
        ),
        Some(vec![3]),
        "MLS §10.4.2 matrix constructors concatenate array operands before vector() flattens them"
    );
}

#[test]
fn infer_array_dims_from_comprehension_returns_none_with_filter() {
    let expr = rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(int(3)),
                span: test_span(),
            },
        }],
        filter: Some(Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(var("i")),
            rhs: Box::new(int(1)),
            span: test_span(),
        })),
        span: test_span(),
    };

    let dims = infer_array_dimensions_checked(&expr);
    assert!(matches!(dims, Ok(None)));
}

fn marker_call(
    name: rumoca_core::Reference,
    args: Vec<rumoca_core::Expression>,
    is_constructor: bool,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    }
}

fn named_marker(name: &str, value: rumoca_core::Expression) -> rumoca_core::Expression {
    marker_call(
        rumoca_core::Reference::generated(format!(
            "{}{name}",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        vec![value],
        true,
    )
}

fn checked_fixture_call_plan<'a>(
    function: &'a rumoca_core::Function,
    call: &'a rumoca_core::Expression,
) -> Result<crate::constant::CheckedCallPlan<'a>, crate::constant::EvalError> {
    let rumoca_core::Expression::FunctionCall { name, .. } = call else {
        return Err(crate::constant::EvalError::InvalidSemanticIr {
            reason: "fixture call is not a function call".to_string(),
            span: test_span(),
        });
    };
    let target =
        crate::constant::resolve_function_occurrence(name, std::iter::once(function), test_span())?;
    checked_call_plan(target, call, test_span())
}

#[test]
fn resolved_function_occurrence_rejects_duplicate_instance_identity_in_any_order() {
    let first = test_function("first", rumoca_core::DefId::new(12_201), test_span());
    let mut second = test_function("second", rumoca_core::DefId::new(12_202), test_span());
    second.instance_id = first.instance_id;
    let call = function_call(&first, Vec::new());
    let rumoca_core::Expression::FunctionCall { name, .. } = &call else {
        panic!("fixture call remains a function call");
    };

    for functions in [[&first, &second], [&second, &first]] {
        assert!(matches!(
            crate::constant::resolve_function_occurrence(name, functions, test_span(),),
            Err(crate::constant::EvalError::InvalidSemanticIr { .. })
        ));
    }
}

#[test]
fn phase_constant_function_shape_binding_requires_exact_argument_layout() {
    let mut function = test_function("f", rumoca_core::DefId::new(12_005), test_span());
    function.add_input(function_param("a", TestScalar::Integer, Vec::new()));
    function.add_input(function_param("b", TestScalar::Integer, Vec::new()).with_default(int(2)));
    assert!(
        checked_fixture_call_plan(
            &function,
            &function_call(&function, vec![int(1), named_marker("b", int(3))]),
        )
        .is_ok()
    );
    assert!(
        checked_fixture_call_plan(
            &function,
            &function_call(&function, vec![named_marker("a", int(1))]),
        )
        .is_ok()
    );
    for malformed in [
        vec![named_marker("b", int(3))],
        vec![int(1), named_marker("unknown", int(3))],
        vec![int(1), named_marker("b", int(2)), named_marker("b", int(3))],
        vec![named_marker("b", int(2)), int(1)],
        vec![int(1), int(2), int(3)],
    ] {
        assert!(
            checked_fixture_call_plan(&function, &function_call(&function, malformed)).is_err()
        );
    }
    let malformed_marker = marker_call(
        rumoca_core::Reference::generated(format!("{}a", rumoca_core::NAMED_FUNCTION_ARG_PREFIX)),
        vec![int(1), int(2)],
        true,
    );
    assert!(
        checked_fixture_call_plan(&function, &function_call(&function, vec![malformed_marker]),)
            .is_err()
    );

    function.add_output(function_param("y", TestScalar::Integer, vec![4]));
    let mut functions = FxHashMap::default();
    functions.insert("f".to_string(), function);
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );
    assert!(
        matches!(
            infer_user_function_call_dimensions(
                &function_call(
                    &functions["f"],
                    vec![int(1), named_marker("unknown", int(3))],
                ),
                &ctx,
            ),
            Err(crate::constant::EvalError::FunctionError { .. })
        ),
        "a concrete declared output shape must not hide an invalid call layout"
    );
}

fn shape_from_formal_function(name: &str, formal: &str) -> rumoca_core::Function {
    let mut function = test_function(name, fixture_def_id(name), test_span());
    function.add_input(function_param(formal, TestScalar::Integer, Vec::new()));
    function.add_output(
        function_param("y", TestScalar::Real, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(Box::new(formal_var(formal)), test_span()),
        ]),
    );
    function
}

#[test]
fn declared_dimension_fold_reads_the_output_shape_through_the_exact_occurrence() {
    let function = shape_from_formal_function("Pkg.sym", "m");
    let call = function_call(&function, vec![int(3)]);
    let mut functions = FxHashMap::default();
    functions.insert("Pkg.sym".to_string(), function);

    assert_eq!(
        super::infer_array_dimensions_checked_with_functions(&call, &functions)
            .expect("an identified callee states its declared output shape"),
        Some(vec![3])
    );
    // The occurrence alone is not enough: the catalog must hold the instance
    // it names, and an absent catalog is a refusal, never a name lookup.
    assert!(super::infer_array_dimensions_checked(&call).is_err());
}

#[test]
fn unsettled_formal_extent_is_no_shape_rather_than_an_error() {
    let function = shape_from_formal_function("Pkg.sym", "m");
    // The actual is a reference the empty value context cannot settle, so the
    // declared `y[m]` has no extent yet; that is "unknown", not a defect.
    let call = function_call(&function, vec![formal_var("outer_m")]);
    let mut functions = FxHashMap::default();
    functions.insert("Pkg.sym".to_string(), function);

    assert_eq!(
        super::infer_array_dimensions_checked_with_functions(&call, &functions)
            .expect("an unsettled actual leaves the shape unknown"),
        None
    );
}

#[test]
fn unknown_array_actual_shape_is_no_shape_rather_than_an_error() {
    let mut function = test_function("Pkg.table", fixture_def_id("Pkg.table"), test_span());
    function.add_input(function_param("table", TestScalar::Real, vec![0, 0]));
    function.add_output(function_param("y", TestScalar::Real, Vec::new()));
    let call = function_call(&function, vec![formal_var("outer_table")]);
    let mut functions = FxHashMap::default();
    functions.insert("Pkg.table".to_string(), function);

    assert_eq!(
        super::infer_array_dimensions_checked_with_functions(&call, &functions)
            .expect("an array actual of unknown shape leaves the call shapeless"),
        None
    );
}

fn resolved_catalog_with(function: &rumoca_core::Function) -> crate::constant::EvalContext {
    let mut context = crate::constant::EvalContext::resolved_empty();
    context
        .try_add_function(function.clone())
        .expect("identified function issues into the catalog");
    context
}

#[test]
fn external_function_call_is_a_typed_refusal_not_an_evaluation() {
    let mut function = test_function("Pkg.ext", fixture_def_id("Pkg.ext"), test_span());
    function.add_input(function_param("x", TestScalar::Real, Vec::new()));
    function.add_output(function_param("y", TestScalar::Real, Vec::new()));
    function.external = Some(rumoca_core::ExternalFunction::default());
    let call = function_call(&function, vec![real(1.0)]);
    let context = resolved_catalog_with(&function);

    let error = crate::constant::eval_expr(&call, &context)
        .expect_err("an external body cannot be executed at translation time");
    assert!(
        matches!(&error, crate::constant::EvalError::NotConstant { reason, .. } if reason.contains("external function")),
        "unexpected refusal: {error:?}"
    );
    // The refusal defers the fold; it never yields the zero-valued outputs an
    // empty environment would produce.
    assert_eq!(
        crate::constant::eval_optional(&call, &context).expect("a deferral is not an error"),
        None
    );
}

#[test]
fn impure_function_call_is_a_typed_refusal_not_an_evaluation() {
    let mut function = test_function("Pkg.seed", fixture_def_id("Pkg.seed"), test_span());
    function.add_input(function_param("s", TestScalar::Integer, Vec::new()));
    function.add_output(function_param("r", TestScalar::Integer, Vec::new()));
    function.pure = false;
    let call = function_call(&function, vec![int(3)]);
    let context = resolved_catalog_with(&function);

    let error =
        crate::constant::eval_expr(&call, &context).expect_err("an impure body cannot be folded");
    assert!(
        matches!(&error, crate::constant::EvalError::NotConstant { reason, .. } if reason.contains("impure function")),
        "unexpected refusal: {error:?}"
    );
    assert_eq!(
        crate::constant::eval_optional(&call, &context).expect("a deferral is not an error"),
        None
    );
}

/// `input Real x[<shape>]` as Flat lowers a written shape: every subscript is
/// retained in `shape_expr` and the effective type carries the literal
/// extents with `0` standing in for `:`.
fn written_shape_param(name: &str, shape: &[Option<i64>]) -> rumoca_core::FunctionParam {
    let dimensions = shape.iter().map(|extent| extent.unwrap_or(0)).collect();
    let shape_expr = shape
        .iter()
        .map(|extent| match extent {
            Some(extent) => rumoca_core::Subscript::index(*extent, test_span()),
            None => rumoca_core::Subscript::Colon { span: test_span() },
        })
        .collect();
    function_param(name, TestScalar::Real, dimensions).with_shape_expr(shape_expr)
}

/// `function f input Real x[<shape>]; output Real y[size(x, axis)]; end f;`
fn written_shape_function(name: &str, shape: &[Option<i64>], axis: i64) -> rumoca_core::Function {
    let mut function = test_function(name, fixture_def_id(name), test_span());
    function.add_input(written_shape_param("x", shape));
    function.add_output(
        function_param("y", TestScalar::Real, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(
                Box::new(call(
                    rumoca_core::BuiltinFunction::Size,
                    vec![formal_var("x"), int(axis)],
                )),
                test_span(),
            ),
        ]),
    );
    function
}

fn written_shape_functions() -> FxHashMap<String, rumoca_core::Function> {
    let mut functions = FxHashMap::default();
    for (name, shape, axis) in [
        ("vector", &[None][..], 1),
        ("pair", &[Some(2)][..], 1),
        ("matrix", &[None, None][..], 2),
        ("three_rows", &[Some(3), None][..], 2),
    ] {
        functions.insert(name.to_string(), written_shape_function(name, shape, axis));
    }
    functions
}

fn real_vector(len: i64) -> rumoca_core::Expression {
    array((0..len).map(|value| real(value as f64)).collect())
}

fn real_matrix(rows: i64, columns: i64) -> rumoca_core::Expression {
    array((0..rows).map(|_| real_vector(columns)).collect())
}

/// MLS §12.4.5: a function input declared `x[:]` takes its extent from the
/// actual argument. `Modelica.Electrical.Polyphase.Functions.quasiRMS`,
/// `activePower`, and `Modelica.Math.BooleanVectors.anyTrue` all declare
/// their vector inputs this way and are called with three-phase vectors.
/// The written `:` survives lowering as `Subscript::Colon`, so the frame must
/// classify that dimension from the subscript and not from the `0` the
/// effective type carries in its place; the bound size `size(x, 1)` reads
/// is then the actual's.
#[test]
fn unspecified_formal_extent_binds_the_actual_extent() {
    let functions = written_shape_functions();
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    for args in [
        vec![real_vector(3)],
        vec![named_marker("x", real_vector(3))],
    ] {
        assert_eq!(
            infer_array_dimensions_full_with_functions(
                &function_call(&functions["vector"], args),
                &ctx,
            ),
            Some(vec![3]),
            "`x[:]` binds size(x, 1) to the actual's extent"
        );
    }
    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(&functions["matrix"], vec![real_matrix(2, 4)]),
            &ctx,
        ),
        Some(vec![4]),
        "`x[:, :]` binds both extents from the actual"
    );
    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(&functions["three_rows"], vec![real_matrix(3, 5)]),
            &ctx,
        ),
        Some(vec![5]),
        "`x[3, :]` binds the trailing extent from a conforming actual"
    );
}

/// The unspecified-extent rule must not blind the exact check: a literal
/// extent is an obligation the actual has to meet, in every dimension of a
/// mixed shape.
#[test]
fn fixed_formal_extent_still_refuses_a_different_actual() {
    let functions = written_shape_functions();
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    for (name, args, expected) in [
        ("pair", vec![real_vector(3)], "expected [2]"),
        ("three_rows", vec![real_matrix(2, 5)], "expected [3, :]"),
    ] {
        let error = super::infer_array_dimensions_full_with_functions(
            &function_call(&functions[name], args),
            &ctx,
        )
        .expect_err("a literal extent refuses a different actual extent");
        assert!(
            matches!(error, crate::constant::EvalError::FunctionError { .. }),
            "{name}: {error}"
        );
        assert!(error.to_string().contains(expected), "{name}: {error}");
    }
}

/// An unspecified extent still declares a dimension, so rank is always an
/// exact obligation: a scalar is not a vector and a vector is not a matrix.
#[test]
fn unspecified_formal_extent_still_refuses_a_rank_mismatch() {
    let functions = written_shape_functions();
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = empty_param_context(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        &functions,
    );

    for (name, args) in [
        ("vector", vec![real(1.0)]),
        ("matrix", vec![real_vector(3)]),
        ("three_rows", vec![real_vector(3)]),
    ] {
        let error = super::infer_array_dimensions_full_with_functions(
            &function_call(&functions[name], args),
            &ctx,
        )
        .expect_err("an unspecified extent does not relax the declared rank");
        assert!(
            matches!(error, crate::constant::EvalError::FunctionError { .. }),
            "{name}: {error}"
        );
    }
}

/// `Modelica.Electrical.Polyphase.Functions.activePower` shape:
/// `input Real v[:]; input Real i[size(v, 1)]`, here with
/// `output Real p[size(i, 1)]` so the bound sizes are observable.
fn active_power_function() -> rumoca_core::Function {
    let mut function = test_function("activePower", fixture_def_id("activePower"), test_span());
    function.add_input(written_shape_param("v", &[None]));
    function.add_input(
        function_param("i", TestScalar::Real, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(
                Box::new(call(
                    rumoca_core::BuiltinFunction::Size,
                    vec![formal_var("v"), int(1)],
                )),
                test_span(),
            ),
        ]),
    );
    function.add_output(
        function_param("p", TestScalar::Real, vec![0]).with_shape_expr(vec![
            rumoca_core::Subscript::expr(
                Box::new(call(
                    rumoca_core::BuiltinFunction::Size,
                    vec![formal_var("i"), int(1)],
                )),
                test_span(),
            ),
        ]),
    );
    function
}

/// Under resolved authority, a formal read inside a sibling formal's extent
/// (`i[size(v, 1)]`) names its declaration only. The actuals here are
/// runtime vectors the context knows by shape alone (the sensor outputs the
/// MSL machine examples pass), so the frame binds `v` by checked shape and
/// no value; that binding must vouch for the read. The extent then evaluates
/// to the actual's size and stays an exact obligation on `i`: a second
/// actual of a different length is refused with the evaluated extent.
#[test]
fn resolved_frame_reads_a_sibling_formal_shape_in_a_declared_extent() {
    let mut functions = FxHashMap::default();
    functions.insert("activePower".to_string(), active_power_function());
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let values_by_identity = FxHashMap::default();
    let mut array_dimensions_by_identity = FxHashMap::default();
    for (def_id, instance_id, extent) in [(91, 910, 3), (92, 920, 3), (93, 930, 2)] {
        array_dimensions_by_identity.insert(
            crate::constant::ResolvedOccurrenceKey {
                instance_id: rumoca_core::InstanceId::new(instance_id),
                root_def_id: rumoca_core::DefId::new(def_id),
            },
            vec![extent],
        );
    }
    let enum_catalog = crate::constant::ResolvedEnumCatalog::empty();
    let ctx = ParamEvalContext::new_resolved(
        &known_ints,
        &known_reals,
        &known_bools,
        &array_dims,
        &functions,
        ResolvedParamInventory::new(
            &values_by_identity,
            &array_dimensions_by_identity,
            &enum_catalog,
        ),
        Some("model.p"),
    );
    let voltages = resolved_occurrence_var("v", 91, 910);
    let currents = resolved_occurrence_var("i", 92, 920);
    let short_currents = resolved_occurrence_var("i2", 93, 930);

    assert_eq!(
        infer_array_dimensions_full_with_functions(
            &function_call(&functions["activePower"], vec![voltages.clone(), currents]),
            &ctx,
        ),
        Some(vec![3]),
        "`i[size(v, 1)]` reads the shape-bound `v` under resolved authority"
    );
    let error = super::infer_array_dimensions_full_with_functions(
        &function_call(&functions["activePower"], vec![voltages, short_currents]),
        &ctx,
    )
    .expect_err("an evaluated extent is an exact obligation");
    assert!(
        matches!(error, crate::constant::EvalError::FunctionError { .. }),
        "{error}"
    );
    assert!(
        error
            .to_string()
            .contains("argument `i` has shape [2], expected [3]"),
        "{error}"
    );
}
