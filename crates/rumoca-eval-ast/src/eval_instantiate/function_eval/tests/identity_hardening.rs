use super::*;

fn call_with_identity(
    spelling: &str,
    identity: rumoca_core::DefId,
    args: Vec<ast::Expression>,
) -> ast::Expression {
    let mut reference = cref(spelling);
    reference.parts.last_mut().unwrap().def_id = Some(identity);
    ast::Expression::FunctionCall {
        comp: reference,
        args,
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn shadow_function(name: &str, identity: rumoca_core::DefId, arity: usize) -> ast::ClassDef {
    let mut function = ast::ClassDef {
        def_id: Some(identity),
        name: token(name),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    for index in 0..arity {
        let formal = format!("x{index}");
        function
            .components
            .insert(formal.clone(), input_int_component(&formal));
    }
    function
        .components
        .insert("y".to_string(), output_int_component("y"));
    function
        .algorithms
        .push(vec![assignment("y", int_expr(77))]);
    function
}

struct Fixture {
    tree: ast::ClassTree,
    mod_env: ast::ModificationEnvironment,
    components: IndexMap<String, ast::Component>,
}

impl Fixture {
    fn new(tree: ast::ClassTree) -> Self {
        Self {
            tree,
            mod_env: ast::ModificationEnvironment::new(),
            components: IndexMap::default(),
        }
    }

    fn context(&self) -> InstantiateEvalCtx<'_> {
        InstantiateEvalCtx {
            tree: &self.tree,
            mod_env: &self.mod_env,
            effective_components: &self.components,
            resolve_class_components: no_op_resolve_class_components,
        }
    }
}

#[test]
fn user_identity_and_unresolved_local_shadow_predefined_spelling() {
    for name in ["abs", "integer"] {
        let identity = rumoca_core::DefId::new(if name == "abs" { 700 } else { 701 });
        let fixture = Fixture::new(tree_with_function(name, shadow_function(name, identity, 1)));
        let ctx = fixture.context();
        assert_eq!(
            try_eval_integer_expr(&ctx, &func_call(name, vec![int_expr(-9)])),
            Some(77),
        );
        assert_eq!(
            try_eval_integer_expr(
                &ctx,
                &call_with_identity(name, identity, vec![int_expr(-9)]),
            ),
            Some(77),
        );
    }
}

#[test]
fn imported_alias_uses_resolved_user_function_identity() {
    let identity = rumoca_core::DefId::new(702);
    let fixture = Fixture::new(tree_with_function(
        "abs",
        shadow_function("abs", identity, 1),
    ));
    let ctx = fixture.context();
    assert_eq!(
        try_eval_integer_expr(
            &ctx,
            &call_with_identity("localAbs", identity, vec![int_expr(-9)]),
        ),
        Some(77),
    );
}

#[test]
fn resolved_predefined_integer_builtins_require_registry_identity_and_arity() {
    let abs = rumoca_core::DefId::new(710);
    let integer = rumoca_core::DefId::new(711);
    let mut tree = ast::ClassTree::new();
    tree.scope_tree
        .add_predefined_member(rumoca_core::ComponentPath::from_flat_path("abs"), abs);
    tree.scope_tree.add_predefined_member(
        rumoca_core::ComponentPath::from_flat_path("integer"),
        integer,
    );
    let fixture = Fixture::new(tree);
    let ctx = fixture.context();

    assert_eq!(
        try_eval_integer_expr(&ctx, &call_with_identity("abs", abs, vec![int_expr(-9)]),),
        Some(9),
    );
    assert_eq!(
        try_eval_integer_expr(
            &ctx,
            &call_with_identity("integer", integer, vec![real_expr("-1.2")]),
        ),
        Some(-2),
    );
    assert_eq!(
        try_eval_integer_expr(
            &ctx,
            &call_with_identity("abs", rumoca_core::DefId::new(799), vec![int_expr(-9)],),
        ),
        None,
    );
    assert_eq!(
        try_eval_integer_expr(
            &ctx,
            &call_with_identity("abs", abs, vec![int_expr(-9), int_expr(1)]),
        ),
        None,
    );
    assert_eq!(
        try_eval_integer_expr(
            &ctx,
            &call_with_identity("abs", abs, vec![int_expr(i64::MIN)]),
        ),
        None,
    );
}
