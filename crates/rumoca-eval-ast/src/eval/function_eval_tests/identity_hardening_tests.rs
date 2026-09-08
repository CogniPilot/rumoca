use super::*;

fn call_with_identity(
    spelling: &str,
    identity: rumoca_core::DefId,
    args: Vec<Expression>,
) -> Expression {
    let mut reference = cref(spelling);
    reference.parts.last_mut().unwrap().def_id = Some(identity);
    Expression::FunctionCall {
        comp: reference,
        args,
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn shadow_function(name: &str, identity: rumoca_core::DefId, arity: usize) -> ClassDef {
    let mut function = ClassDef {
        def_id: Some(identity),
        name: token(name),
        class_type: ClassType::Function,
        pure: true,
        ..ClassDef::default()
    };
    for index in 0..arity {
        let formal = format!("x{index}");
        function
            .components
            .insert(formal.clone(), input_parameter(&formal, None));
    }
    function
        .components
        .insert("y".to_string(), output_parameter("y"));
    function.algorithms.push(vec![Statement::Assignment {
        comp: cref("y"),
        value: int_expr(77),
    }]);
    function
}

fn outer_function(name: &str, identity: rumoca_core::DefId, nested_call: Expression) -> ClassDef {
    let mut function = ClassDef {
        def_id: Some(identity),
        name: token(name),
        class_type: ClassType::Function,
        pure: true,
        ..ClassDef::default()
    };
    function
        .components
        .insert("y".to_string(), output_parameter("y"));
    function.algorithms.push(vec![Statement::Assignment {
        comp: cref("y"),
        value: nested_call,
    }]);
    function
}

#[test]
fn user_function_identity_preempts_builtin_spelling() {
    for (name, args) in [
        ("abs", vec![int_expr(-9)]),
        ("integer", vec![int_expr(9)]),
        ("size", vec![int_expr(9), int_expr(1)]),
    ] {
        let identity = rumoca_core::DefId::new(500 + args.len() as u32);
        let function = shadow_function(name, identity, args.len());
        let mut functions = FxHashMap::default();
        functions.insert(name.to_string(), function);
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        ctx.functions = Arc::new(functions);

        assert_eq!(eval_integer(&call(name, args.clone()), &ctx), Some(77));
        assert_eq!(
            eval_integer(&call_with_identity(name, identity, args), &ctx),
            Some(77)
        );
    }
}

#[test]
fn imported_alias_selects_user_identity_before_builtin_spelling() {
    let identity = rumoca_core::DefId::new(510);
    let function = shadow_function("abs", identity, 1);
    let mut functions = FxHashMap::default();
    functions.insert("Pkg.abs".to_string(), function);
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.functions = Arc::new(functions);

    assert_eq!(
        eval_integer(
            &call_with_identity("abs", identity, vec![int_expr(-9)]),
            &ctx,
        ),
        Some(77),
    );
}

#[test]
fn resolved_predefined_calls_require_registered_identity_and_exact_arity() {
    let abs = rumoca_core::DefId::new(520);
    let integer = rumoca_core::DefId::new(521);
    let size = rumoca_core::DefId::new(522);
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.set_predefined_functions([
        ("abs".to_string(), abs),
        ("integer".to_string(), integer),
        ("size".to_string(), size),
    ]);
    ctx.add_dimensions("a", vec![3]);

    assert_eq!(
        eval_integer(&call_with_identity("abs", abs, vec![int_expr(-9)]), &ctx),
        Some(9),
    );
    assert_eq!(
        eval_integer(
            &call_with_identity("integer", integer, vec![real_expr(-1.2)]),
            &ctx,
        ),
        Some(-2),
    );
    assert_eq!(
        eval_integer(
            &call_with_identity("size", size, vec![cref_expr("a"), int_expr(1)]),
            &ctx,
        ),
        Some(3),
    );

    assert_eq!(
        eval_integer(
            &call_with_identity("abs", rumoca_core::DefId::new(599), vec![int_expr(-9)]),
            &ctx,
        ),
        None,
    );
    assert_eq!(
        eval_integer(
            &call_with_identity("abs", abs, vec![int_expr(-9), int_expr(1)]),
            &ctx,
        ),
        None,
    );
    assert_eq!(
        eval_integer(
            &call_with_identity("abs", abs, vec![int_expr(i64::MIN)]),
            &ctx
        ),
        None,
    );
}

/// SPEC_0036: in a post-Resolve environment an identity-free call reference
/// selects nothing and folds nothing: neither a user function published
/// under the rendered spelling nor a builtin by bare spelling.
#[test]
fn resolved_identity_environment_refuses_identity_free_call_references() {
    let identity = rumoca_core::DefId::new(530);
    let function = shadow_function("shadowed", identity, 1);
    let mut functions = FxHashMap::default();
    functions.insert("shadowed".to_string(), function);
    let mut ctx = TypeCheckEvalContext::for_resolved_identities();
    ctx.functions = Arc::new(functions);
    ctx.set_predefined_functions([("abs".to_string(), rumoca_core::DefId::new(531))]);

    // Identity-free user-function spelling folds nothing.
    assert_eq!(
        eval_integer(&call("shadowed", vec![int_expr(1)]), &ctx),
        None
    );
    // Identity-free builtin spelling folds nothing, even though the builtin
    // is registered under that name.
    assert_eq!(eval_integer(&call("abs", vec![int_expr(-9)]), &ctx), None);
    // The same calls with their Resolve-issued identities still fold.
    assert_eq!(
        eval_integer(
            &call_with_identity("shadowed", identity, vec![int_expr(1)]),
            &ctx
        ),
        Some(77),
    );
    assert_eq!(
        eval_integer(
            &call_with_identity("abs", rumoca_core::DefId::new(531), vec![int_expr(-9)]),
            &ctx,
        ),
        Some(9),
    );
}

/// The delimited pre-identity structural category retains rendered-name and
/// builtin-spelling selection; the two categories differ only there.
#[test]
fn pre_identity_structural_category_retains_spelling_selection() {
    let identity = rumoca_core::DefId::new(540);
    let function = shadow_function("named", identity, 1);
    let mut functions = FxHashMap::default();
    functions.insert("named".to_string(), function);
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.functions = Arc::new(functions);

    assert_eq!(
        eval_integer(&call("named", vec![int_expr(1)]), &ctx),
        Some(77)
    );
}

/// M6/MC-3: an interpreted function body inherits the caller's exact
/// call-identity category. These witnesses force the nested path: the outer
/// function has no constant result of its own, so every successful `77`/`9`
/// came from evaluating its inner call.
#[test]
fn nested_function_evaluation_preserves_the_call_identity_policy() {
    let inner_id = rumoca_core::DefId::new(550);
    let exact_outer_id = rumoca_core::DefId::new(551);
    let identity_free_outer_id = rumoca_core::DefId::new(552);
    let builtin_outer_id = rumoca_core::DefId::new(553);
    let abs_id = rumoca_core::DefId::new(554);

    let inner = shadow_function("inner", inner_id, 1);
    let exact_outer = outer_function(
        "exactOuter",
        exact_outer_id,
        call_with_identity("inner", inner_id, vec![int_expr(1)]),
    );
    let identity_free_outer = outer_function(
        "identityFreeOuter",
        identity_free_outer_id,
        call("inner", vec![int_expr(1)]),
    );
    let builtin_outer = outer_function(
        "builtinOuter",
        builtin_outer_id,
        call("abs", vec![int_expr(-9)]),
    );
    let mut functions = FxHashMap::default();
    functions.insert("inner".to_string(), inner);
    functions.insert("exactOuter".to_string(), exact_outer);
    functions.insert("identityFreeOuter".to_string(), identity_free_outer);
    functions.insert("builtinOuter".to_string(), builtin_outer);

    let mut strict = TypeCheckEvalContext::for_resolved_identities();
    strict.functions = Arc::new(functions.clone());
    strict.set_predefined_functions([("abs".to_string(), abs_id)]);

    assert_eq!(
        eval_integer(
            &call_with_identity("exactOuter", exact_outer_id, Vec::new()),
            &strict,
        ),
        Some(77),
        "the positive witness must reach and fold the exact-identity inner call"
    );
    assert_eq!(
        eval_integer(
            &call_with_identity("identityFreeOuter", identity_free_outer_id, Vec::new(),),
            &strict,
        ),
        None,
        "a strict outer call must not widen its identity-free nested user call"
    );
    assert_eq!(
        eval_integer(
            &call_with_identity("builtinOuter", builtin_outer_id, Vec::new()),
            &strict,
        ),
        None,
        "a strict outer call must not widen its identity-free nested builtin call"
    );

    let mut structural = TypeCheckEvalContext::for_pre_identity_structural();
    structural.functions = Arc::new(functions);
    structural.set_predefined_functions([("abs".to_string(), abs_id)]);
    assert_eq!(
        eval_integer(
            &call_with_identity("identityFreeOuter", identity_free_outer_id, Vec::new(),),
            &structural,
        ),
        Some(77),
    );
    assert_eq!(
        eval_integer(
            &call_with_identity("builtinOuter", builtin_outer_id, Vec::new()),
            &structural,
        ),
        Some(9),
    );
}
