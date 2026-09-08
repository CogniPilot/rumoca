use std::sync::Arc;

use super::{
    build_structural_eval_context, collect_component_binding_values,
    collect_function_calls_from_equation, equation_targets_boolean_instance,
    inject_alias_component_package_constants, lookup_with_scope,
    try_eval_const_flat_expr_with_scope, try_eval_const_integer_with_scope,
    try_eval_structural_equation, try_extract_named_record_constructor_constant,
};
use crate::Context;
use crate::constant_extraction::extract_extends_modification_expr;
use rumoca_core::{ClassType, DefId, Literal, OpBinary, Token, TypeId, Variability};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::{
    ClassDef, ClassDefIndex, ClassTree, Component, ComponentRefPart, ComponentReference,
    InstanceData, InstanceOverlay, QualifiedName,
};

/// A fold canonicalizer over an empty callable catalog: every call in the
/// fixture keeps whatever occurrence it was built with.
fn empty_call_canonicalizer<'a>(
    tree: &ClassTree,
    class_index: &'a ClassDefIndex<'a>,
) -> crate::functions::StructuralFoldCallCanonicalizer<'a> {
    crate::functions::StructuralFoldCallCanonicalizer::new(std::iter::empty(), tree, class_index)
        .expect("an empty catalog canonicalizes")
}

#[test]
fn function_precollector_rejects_empty_recovery_node_at_owner_span() {
    let tree = ClassTree::new();
    let class_index = ClassDefIndex::from_tree(&tree);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_precollector_test.mo"),
        7,
        11,
    );
    let mut calls = crate::functions::FunctionRequests::default();

    let error = collect_function_calls_from_equation(
        &ast::Equation::Empty,
        span,
        &mut calls,
        &tree,
        &class_index,
    )
    .expect_err("function precollection must not discard Equation::Empty");

    assert!(matches!(
        error,
        crate::FlattenError::InvalidAstRecovery {
            span: error_span,
            ..
        } if error_span == span
    ));
    assert!(calls.into_entries().is_empty());
}

#[test]
fn subscript_dot_name_does_not_trigger_dotted_suffix_lookup() {
    let mut values: rustc_hash::FxHashMap<String, i64> = rustc_hash::FxHashMap::default();
    values.insert("sys.arr[data.medium]".to_string(), 7);

    assert_eq!(lookup_with_scope("arr[data.medium]", "", &values), None);
}

#[test]
fn subscript_dot_name_still_resolves_with_parent_scope() {
    let mut values: rustc_hash::FxHashMap<String, i64> = rustc_hash::FxHashMap::default();
    values.insert("sys.arr[data.medium]".to_string(), 7);

    assert_eq!(
        lookup_with_scope("arr[data.medium]", "sys", &values),
        Some(7)
    );
}

fn token(text: &str) -> Token {
    Token {
        text: Arc::from(text.to_string()),
        ..Token::default()
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("constant_injection_test.mo"),
        4,
        20,
    )
}

fn fixture_def_id(name: &str) -> DefId {
    let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    DefId::new(hash.max(1))
}

fn int_expr(value: i64) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        token: token(&value.to_string()),
        span: test_span(),
    }
}

fn real_expr(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
        token: token(value),
        span: test_span(),
    }
}

fn real_array_expr(values: &[&str]) -> ast::Expression {
    ast::Expression::Array {
        elements: values.iter().map(|value| real_expr(value)).collect(),
        is_matrix: false,
        span: test_span(),
    }
}

fn comp_ref(path: &str) -> ComponentReference {
    ComponentReference {
        local: false,
        parts: crate::path_utils::segments(path)
            .into_iter()
            .map(|part| ComponentRefPart {
                ident: token(part),
                subs: None,
                def_id: Some(fixture_def_id(part)),
            })
            .collect(),
        span: test_span(),
        qualified_display_name: None,
    }
}

fn call_expr(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: comp_ref(name),
        args,
        is_partial_application: false,
        span: test_span(),
    }
}

fn field_access_expr(base: &str, field: &str) -> ast::Expression {
    ast::Expression::FieldAccess {
        base: Arc::new(ast::Expression::ComponentReference(comp_ref(base))),
        field: field.to_string(),
        field_def_id: Some(fixture_def_id(field)),
        span: test_span(),
    }
}

fn simple_equation(lhs: &str, rhs: ast::Expression) -> ast::Equation {
    ast::Equation::Simple {
        lhs: ast::Expression::ComponentReference(comp_ref(lhs)),
        rhs,
    }
}

#[test]
fn component_record_constant_alias_is_visible_under_source_type_name() {
    let record_def = DefId::new(1);
    let mut constants = ClassDef {
        def_id: Some(record_def),
        name: token("SpiceConstants"),
        class_type: ClassType::Record,
        ..ClassDef::default()
    };
    constants.components.insert(
        "CKTgmin".to_string(),
        Component {
            name: "CKTgmin".to_string(),
            name_token: token("CKTgmin"),
            type_name: ast::Name::from_string("Real"),
            variability: Variability::Constant(Token::default()),
            binding: Some(real_expr("1e-12")),
            has_explicit_binding: true,
            ..Component::empty_with_span(test_span())
        },
    );

    let mut tree = ast::ClassTree::new();
    tree.definitions
        .classes
        .insert("SpiceConstants".to_string(), constants);
    tree.def_map
        .insert(record_def, "SpiceConstants".to_string());
    tree.name_map
        .insert("SpiceConstants".to_string(), record_def);
    let class_index = ClassDefIndex::from_tree(&tree);
    let alias_comp = Component {
        name: "C".to_string(),
        name_token: token("C"),
        type_name: ast::Name::from_string("SpiceConstants"),
        type_def_id: Some(record_def),
        variability: Variability::Constant(Token::default()),
        ..Component::empty_with_span(test_span())
    };
    let mut ctx = Context::new();

    inject_alias_component_package_constants(
        &tree,
        &class_index,
        "mp",
        "C",
        &alias_comp,
        "Device",
        &mut ctx,
    )
    .expect("sourced package fixture injects constants");

    assert_eq!(
        ctx.constant_values.get("mp.SpiceConstants.CKTgmin"),
        Some(&rumoca_core::Expression::Literal {
            value: Literal::Real(1e-12),
            span: test_span(),
        })
    );
    assert!(ctx.constant_values.contains_key("mp.C.CKTgmin"));
    assert!(!ctx.constant_values.contains_key("mp.CKTgmin"));
}

#[test]
fn integer_constant_never_enters_the_real_typed_cache() {
    let record_def = DefId::new(1);
    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    let integer_id = crate::test_support::predefined_type_def_id(&tree, "Integer");
    let mut constants = ClassDef {
        def_id: Some(record_def),
        name: token("Limits"),
        class_type: ClassType::Record,
        ..ClassDef::default()
    };
    constants.components.insert(
        "maxWaypoints".to_string(),
        Component {
            name: "maxWaypoints".to_string(),
            name_token: token("maxWaypoints"),
            type_name: ast::Name::from_string("Integer"),
            type_def_id: Some(integer_id),
            variability: Variability::Constant(Token::default()),
            binding: Some(int_expr(8)),
            has_explicit_binding: true,
            ..Component::empty_with_span(test_span())
        },
    );

    tree.definitions
        .classes
        .insert("Limits".to_string(), constants);
    tree.def_map.insert(record_def, "Limits".to_string());
    tree.name_map.insert("Limits".to_string(), record_def);
    let class_index = ClassDefIndex::from_tree(&tree);
    let alias_comp = Component {
        name: "LimitsAlias".to_string(),
        name_token: token("LimitsAlias"),
        type_name: ast::Name::from_string("Limits"),
        type_def_id: Some(record_def),
        variability: Variability::Constant(Token::default()),
        ..Component::empty_with_span(test_span())
    };
    let mut ctx = Context::new();

    inject_alias_component_package_constants(
        &tree,
        &class_index,
        "vehicle",
        "LimitsAlias",
        &alias_comp,
        "Vehicle",
        &mut ctx,
    )
    .expect("sourced package fixture injects constants");

    assert_eq!(
        ctx.parameter_values.get("vehicle.LimitsAlias.maxWaypoints"),
        Some(&8)
    );
    assert!(
        !ctx.real_parameter_values
            .contains_key("vehicle.LimitsAlias.maxWaypoints"),
        "typed structural caches must not represent one Integer as a Real"
    );
}

#[test]
fn derived_integer_and_boolean_constants_use_their_canonical_cache_owner() {
    let integer_type = DefId::new(10);
    let boolean_type = DefId::new(11);
    let owner_def = DefId::new(12);
    let mut tree = ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    let integer_id = crate::test_support::predefined_type_def_id(&tree, "Integer");
    let boolean_id = crate::test_support::predefined_type_def_id(&tree, "Boolean");
    let mut count = ClassDef {
        def_id: Some(integer_type),
        name: token("Count"),
        class_type: ClassType::Type,
        ..ClassDef::default()
    };
    count.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Integer"),
        base_def_id: Some(integer_id),
        ..ast::Extend::default()
    });
    let mut flag = ClassDef {
        def_id: Some(boolean_type),
        name: token("Flag"),
        class_type: ClassType::Type,
        ..ClassDef::default()
    };
    flag.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Boolean"),
        base_def_id: Some(boolean_id),
        ..ast::Extend::default()
    });
    let mut owner = ClassDef {
        def_id: Some(owner_def),
        name: token("Owner"),
        ..ClassDef::default()
    };
    owner.components.insert(
        "n".to_string(),
        Component {
            name: "n".to_string(),
            name_token: token("n"),
            type_name: ast::Name {
                name: vec![token("Count")],
                def_id: Some(integer_type),
            },
            type_def_id: Some(integer_type),
            variability: Variability::Constant(Token::default()),
            binding: Some(int_expr(8)),
            has_explicit_binding: true,
            ..Component::empty_with_span(test_span())
        },
    );
    owner.components.insert(
        "enabled".to_string(),
        Component {
            name: "enabled".to_string(),
            name_token: token("enabled"),
            type_name: ast::Name {
                name: vec![token("Flag")],
                def_id: Some(boolean_type),
            },
            type_def_id: Some(boolean_type),
            variability: Variability::Constant(Token::default()),
            binding: Some(ast::Expression::Terminal {
                terminal_type: ast::TerminalType::Bool,
                token: token("true"),
                span: test_span(),
            }),
            has_explicit_binding: true,
            ..Component::empty_with_span(test_span())
        },
    );
    tree.definitions.classes.insert("Count".to_string(), count);
    tree.definitions.classes.insert("Flag".to_string(), flag);
    tree.definitions.classes.insert("Owner".to_string(), owner);
    for (def_id, name) in [
        (integer_type, "Count"),
        (boolean_type, "Flag"),
        (owner_def, "Owner"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }
    let class_index = ClassDefIndex::from_tree(&tree);
    let owner = class_index.get(owner_def).expect("Owner is indexed");
    let mut ctx = Context::new();

    super::extract_constants_from_class(&class_index, owner, &mut ctx);

    assert_eq!(ctx.parameter_values.get("n"), Some(&8));
    assert_eq!(ctx.boolean_parameter_values.get("enabled"), Some(&true));
    assert!(!ctx.real_parameter_values.contains_key("n"));
    assert!(!ctx.real_parameter_values.contains_key("enabled"));
    assert!(!ctx.parameter_values.contains_key("enabled"));
}

#[test]
fn integer_extends_modifier_cannot_also_own_the_real_cache() {
    let base_def = DefId::new(20);
    let integer_type = DefId::new(21);
    let n_def = DefId::new(22);
    let derived_def = DefId::new(23);
    let mut tree = ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    let integer_id = crate::test_support::predefined_type_def_id(&tree, "Integer");
    let mut count = ClassDef {
        def_id: Some(integer_type),
        name: token("Count"),
        class_type: ClassType::Type,
        ..ClassDef::default()
    };
    count.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Integer"),
        base_def_id: Some(integer_id),
        ..ast::Extend::default()
    });
    let mut base = ClassDef {
        def_id: Some(base_def),
        name: token("Base"),
        ..ClassDef::default()
    };
    base.components.insert(
        "n".to_string(),
        Component {
            def_id: Some(n_def),
            name: "n".to_string(),
            name_token: token("n"),
            type_name: ast::Name {
                name: vec![token("Count")],
                def_id: Some(integer_type),
            },
            type_def_id: Some(integer_type),
            variability: Variability::Parameter(Token::default()),
            binding: Some(int_expr(1)),
            has_explicit_binding: true,
            ..Component::empty_with_span(test_span())
        },
    );
    let derived = ClassDef {
        def_id: Some(derived_def),
        name: token("Derived"),
        ..ClassDef::default()
    };
    tree.definitions.classes.insert("Count".to_string(), count);
    tree.definitions.classes.insert("Base".to_string(), base);
    tree.definitions
        .classes
        .insert("Derived".to_string(), derived);
    for (def_id, name) in [
        (integer_type, "Count"),
        (base_def, "Base"),
        (derived_def, "Derived"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }
    let class_index = ClassDefIndex::from_tree(&tree);
    let mut target = comp_ref("n");
    target.set_target_def_id(Some(n_def));
    let modification = ast::Expression::Modification {
        target,
        value: Some(Arc::new(int_expr(8))),
        span: test_span(),
    };
    let mut ctx = Context::new();

    extract_extends_modification_expr(
        &tree,
        &class_index,
        "Derived",
        &modification,
        "Derived",
        &mut ctx,
    );

    assert_eq!(ctx.parameter_values.get("Derived.n"), Some(&8));
    assert!(!ctx.real_parameter_values.contains_key("Derived.n"));
}

#[test]
fn named_record_fields_follow_declared_type_not_value_shape() {
    let record_def = DefId::new(30);
    let field_def = DefId::new(31);
    let mut tree = ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    let real_id = crate::test_support::predefined_type_def_id(&tree, "Real");
    let mut record = ClassDef {
        def_id: Some(record_def),
        name: token("R"),
        class_type: ClassType::Record,
        ..ClassDef::default()
    };
    record.components.insert(
        "k".to_string(),
        Component {
            def_id: Some(field_def),
            name: "k".to_string(),
            name_token: token("k"),
            type_name: ast::Name::from_string("Real"),
            type_def_id: Some(real_id),
            variability: Variability::Parameter(Token::default()),
            ..Component::empty_with_span(test_span())
        },
    );
    tree.definitions.classes.insert("R".to_string(), record);
    tree.def_map.insert(record_def, "R".to_string());
    tree.name_map.insert("R".to_string(), record_def);
    let class_index = ClassDefIndex::from_tree(&tree);
    let mut constructor = comp_ref("R");
    constructor.set_target_def_id(Some(record_def));
    let expression = ast::Expression::FunctionCall {
        comp: constructor,
        args: vec![ast::Expression::NamedArgument {
            name: token("k"),
            value: Arc::new(int_expr(2)),
            span: test_span(),
        }],
        is_partial_application: false,
        span: test_span(),
    };
    let mut ctx = Context::new();

    try_extract_named_record_constructor_constant(&expression, &class_index, &mut ctx, "", "r")
        .expect("named record constant evaluates");

    assert_eq!(ctx.real_parameter_values.get("r.k"), Some(&2.0));
    assert!(!ctx.parameter_values.contains_key("r.k"));
    assert!(!ctx.enum_parameter_values.contains_key("r.k"));
}

#[test]
fn record_field_without_type_evidence_never_guesses_enum_from_a_reference() {
    let mut ctx = Context::new();
    let value = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("E.a"),
        subscripts: Vec::new(),
        span: test_span(),
    };

    super::register_named_record_field_constant(&mut ctx, None, "r", "mode", "r.mode", &value);

    assert_eq!(ctx.constant_values.get("r.mode"), Some(&value));
    assert!(!ctx.parameter_values.contains_key("r.mode"));
    assert!(!ctx.real_parameter_values.contains_key("r.mode"));
    assert!(!ctx.boolean_parameter_values.contains_key("r.mode"));
    assert!(!ctx.enum_parameter_values.contains_key("r.mode"));
}

#[test]
fn structural_context_rejects_overlapping_typed_cache_ownership() {
    let mut ctx = Context::new();
    ctx.parameter_values.insert("n".to_string(), 2);
    ctx.real_parameter_values.insert("n".to_string(), 2.0);

    let tree = ClassTree::new();
    let class_index = ClassDefIndex::from_tree(&tree);
    let error = build_structural_eval_context(&ctx, &InstanceOverlay::new(), &tree, &class_index)
        .expect_err("typed cache overlap is invalid construction state");

    assert!(matches!(error, crate::FlattenError::Internal(message) if message.contains("n")));
}

#[test]
fn const_function_call_preserves_component_reference_scope() {
    let ctx = Context::default();
    let expr = call_expr(
        "Medium.specificEnthalpy_pTX",
        vec![int_expr(1), int_expr(2)],
    );

    let lowered = try_eval_const_flat_expr_with_scope(&expr, &ctx, "").unwrap();

    let rumoca_core::Expression::FunctionCall { name, .. } = lowered else {
        panic!("expected lowered function call");
    };
    let scope = name
        .component_scope()
        .expect("constant lowering must retain component reference scope");
    assert_eq!(
        scope
            .parts()
            .len()
            .checked_sub(2)
            .and_then(|index| scope.parts().get(index))
            .map(|part| part.ident.as_str()),
        Some("Medium")
    );
    assert_eq!(scope.leaf_ident(), Some("specificEnthalpy_pTX"));
}

#[test]
fn const_array_builtin_lowers_to_array_literal() {
    let expr = call_expr("array", vec![real_expr("1.0"), real_expr("2.0")]);

    let lowered = try_eval_const_flat_expr_with_scope(&expr, &Context::new(), "").unwrap();

    let rumoca_core::Expression::Array { elements, .. } = lowered else {
        panic!("array(...) should lower to an array literal");
    };
    assert_eq!(elements.len(), 2);
}

#[test]
fn const_integer_div_operator_requires_exact_quotient() {
    let expr = ast::Expression::Binary {
        op: OpBinary::Div,
        lhs: Arc::new(int_expr(7)),
        rhs: Arc::new(int_expr(2)),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        try_eval_const_integer_with_scope(&expr, &Context::new(), ""),
        None
    );
}

#[test]
fn const_integer_div_builtin_remains_truncating() {
    let expr = call_expr("div", vec![int_expr(7), int_expr(2)]);

    assert_eq!(
        try_eval_const_integer_with_scope(&expr, &Context::new(), ""),
        Some(3)
    );
}

#[test]
fn size_call_maps_qualified_class_member_to_instance_scope() {
    let mut ctx = Context::new();
    ctx.array_dimensions.insert(
        "adaptor.filter[1].transferFunction[1].a".to_string(),
        vec![2],
    );
    let expr = call_expr(
        "size",
        vec![
            ast::Expression::ComponentReference(comp_ref(
                "Modelica.Blocks.Continuous.TransferFunction.a",
            )),
            int_expr(1),
        ],
    );

    assert_eq!(
        try_eval_const_integer_with_scope(&expr, &ctx, "adaptor.filter[1].transferFunction[1]"),
        Some(2)
    );
}

#[test]
fn const_field_access_resolves_package_constant_value() {
    let mut ctx = Context::new();
    ctx.real_parameter_values
        .insert("Modelica.Constants.eps".to_string(), 2.0e-16);
    let expr = field_access_expr("Modelica.Constants", "eps");

    assert_eq!(
        try_eval_const_flat_expr_with_scope(&expr, &ctx, ""),
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(2.0e-16),
            span: test_span(),
        })
    );
}

#[test]
fn component_binding_collection_uses_only_structural_components() {
    let mut overlay = InstanceOverlay::new();
    let x_id = overlay.alloc_id();
    let p_id = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: x_id,
            qualified_name: QualifiedName::from_dotted("M.x"),
            variability: Variability::Empty,
            binding: Some(real_expr("1.0")),
            ..InstanceData::default()
        })
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(InstanceData {
            instance_id: p_id,
            qualified_name: QualifiedName::from_dotted("M.p"),
            variability: Variability::Parameter(Token::default()),
            binding: Some(real_expr("2.0")),
            ..InstanceData::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let mut eval_ctx = rumoca_eval_flat::constant::EvalContext::structural_preidentity();
    let tree = ClassTree::new();
    let class_index = ClassDefIndex::from_tree(&tree);
    let mut calls = empty_call_canonicalizer(&tree, &class_index);

    collect_component_binding_values(&Context::new(), &overlay, &mut eval_ctx, &mut calls).unwrap();

    assert!(eval_ctx.get("M.x").is_none());
    assert!(eval_ctx.get("M.p").is_some());
}

#[test]
fn structural_component_collection_never_guesses_a_start_value() {
    let mut overlay = InstanceOverlay::new();
    let with_binding_id = overlay.alloc_id();
    let fixed_false_id = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: with_binding_id,
            qualified_name: QualifiedName::from_dotted("M.with_binding"),
            variability: Variability::Parameter(Token::default()),
            binding: Some(ast::Expression::ComponentReference(comp_ref(
                "runtime_value",
            ))),
            start: Some(real_expr("7.0")),
            ..InstanceData::default()
        })
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(InstanceData {
            instance_id: fixed_false_id,
            qualified_name: QualifiedName::from_dotted("M.fixed_false"),
            variability: Variability::Parameter(Token::default()),
            fixed: Some(false),
            start: Some(real_expr("9.0")),
            ..InstanceData::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let mut ctx = Context::new();
    ctx.non_structural_params
        .insert("M.fixed_false".to_string());
    let mut eval_ctx = rumoca_eval_flat::constant::EvalContext::structural_preidentity();
    let tree = ClassTree::new();
    let class_index = ClassDefIndex::from_tree(&tree);
    let mut calls = empty_call_canonicalizer(&tree, &class_index);

    collect_component_binding_values(&ctx, &overlay, &mut eval_ctx, &mut calls).unwrap();

    assert_eq!(eval_ctx.get("M.with_binding"), None);
    assert_eq!(eval_ctx.get("M.fixed_false"), None);
}

#[test]
fn component_binding_collection_resolves_indexed_value_from_owner_scope() {
    let mut reference = comp_ref("f");
    reference.parts[0].subs = Some(vec![ast::Subscript::Expression(int_expr(2))]);
    let mut overlay = InstanceOverlay::new();
    let component_id = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: component_id,
            qualified_name: QualifiedName::from_dotted("M.child.f"),
            variability: Variability::Parameter(Token::default()),
            binding: Some(ast::Expression::ComponentReference(reference)),
            ..InstanceData::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let mut eval_ctx = rumoca_eval_flat::constant::EvalContext::structural_preidentity();
    eval_ctx.add_parameter("f", rumoca_eval_flat::constant::Value::Real(99.0));
    eval_ctx.add_parameter(
        "M.f",
        rumoca_eval_flat::constant::Value::Array(vec![
            rumoca_eval_flat::constant::Value::Real(10.0),
            rumoca_eval_flat::constant::Value::Real(20.0),
        ]),
    );

    let tree = ClassTree::new();
    let class_index = ClassDefIndex::from_tree(&tree);
    let mut calls = empty_call_canonicalizer(&tree, &class_index);

    collect_component_binding_values(&Context::new(), &overlay, &mut eval_ctx, &mut calls).unwrap();

    assert_eq!(
        eval_ctx.get("M.child.f"),
        Some(&rumoca_eval_flat::constant::Value::Real(20.0))
    );
}

#[test]
fn structural_context_never_registers_an_array_as_a_scalar_value() {
    let mut ctx = Context::new();
    ctx.real_parameter_values.insert("M.f".to_string(), 99.0);
    ctx.array_dimensions.insert("M.f".to_string(), vec![2]);

    let mut reference = comp_ref("f");
    reference.parts[0].subs = Some(vec![ast::Subscript::Expression(int_expr(2))]);
    let mut overlay = InstanceOverlay::new();
    let array_id = overlay.alloc_id();
    let selected_id = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: array_id,
            qualified_name: QualifiedName::from_dotted("M.f"),
            variability: Variability::Parameter(Token::default()),
            binding: Some(real_array_expr(&["10.0", "20.0"])),
            ..InstanceData::default()
        })
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(InstanceData {
            instance_id: selected_id,
            qualified_name: QualifiedName::from_dotted("M.child.f"),
            variability: Variability::Parameter(Token::default()),
            binding: Some(ast::Expression::ComponentReference(reference)),
            ..InstanceData::default()
        })
        .expect("fixture occurrence insertion must succeed");

    let tree = ClassTree::new();
    let class_index = ClassDefIndex::from_tree(&tree);
    let eval_ctx = build_structural_eval_context(&ctx, &overlay, &tree, &class_index).unwrap();

    assert_eq!(
        eval_ctx.get("M.f"),
        Some(&rumoca_eval_flat::constant::Value::Array(vec![
            rumoca_eval_flat::constant::Value::Real(10.0),
            rumoca_eval_flat::constant::Value::Real(20.0),
        ]))
    );
    assert_eq!(
        eval_ctx.get("M.child.f"),
        Some(&rumoca_eval_flat::constant::Value::Real(20.0))
    );
}

#[test]
fn structural_context_registers_enum_literals_from_resolved_tree_identity() {
    let mut tree = ClassTree::new();
    let enum_def_id = DefId::new(91);
    let mut color = ClassDef {
        def_id: Some(enum_def_id),
        name: token("Color"),
        class_type: ClassType::Type,
        ..ClassDef::default()
    };
    color.enum_literals.push(ast::EnumLiteral {
        ident: token("red"),
        description: Vec::new(),
    });
    color.enum_literals.push(ast::EnumLiteral {
        ident: token("blue"),
        description: Vec::new(),
    });
    tree.definitions.classes.insert("Color".to_string(), color);
    tree.def_map.insert(enum_def_id, "Color".to_string());
    tree.name_map.insert("Color".to_string(), enum_def_id);

    let class_index = ClassDefIndex::from_tree(&tree);
    let eval_ctx = build_structural_eval_context(
        &Context::new(),
        &InstanceOverlay::new(),
        &tree,
        &class_index,
    )
    .expect("resolved enum catalog builds a structural evaluator context");

    let resolved_literal = |literal: &str, literal_id: u32| {
        let reference = rumoca_core::ComponentReference::construct(
            false,
            rumoca_core::Span::DUMMY,
            vec![
                rumoca_core::ComponentRefPart {
                    ident: "Color".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: Vec::new(),
                    def_id: enum_def_id,
                },
                rumoca_core::ComponentRefPart {
                    ident: literal.to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: Vec::new(),
                    def_id: DefId::new(literal_id),
                },
            ],
        )
        .expect("fixture enum literal reference is exact");
        rumoca_core::Reference::from_component_reference(reference)
    };
    let red = eval_ctx
        .get_enum_reference(&resolved_literal("red", 92))
        .expect("resolved declaration selects the red literal");
    let blue = eval_ctx
        .get_enum_reference(&resolved_literal("blue", 93))
        .expect("resolved declaration selects the blue literal");
    assert_eq!(red.declaration(), enum_def_id);
    assert_eq!(red.ordinal(), 1);
    assert_eq!(blue.declaration(), enum_def_id);
    assert_eq!(blue.ordinal(), 2);
}

#[test]
fn const_flat_expr_accepts_enum_literal_component_ref() {
    let path = "Modelica.Electrical.Digital.Interfaces.Logic.'U'";
    let component_ref = comp_ref(path);
    let expected_reference = rumoca_core::Reference::with_component_reference(
        path,
        super::core_component_reference_from_ast(&component_ref)
            .expect("the fixture gives every path segment a declaration identity"),
    );
    let expr = ast::Expression::ComponentReference(component_ref.clone());

    let evaluated = try_eval_const_flat_expr_with_scope(&expr, &Context::new(), "");
    assert_eq!(
        evaluated,
        Some(rumoca_core::Expression::VarRef {
            name: expected_reference,
            subscripts: vec![],
            span: test_span(),
        })
    );
    let Some(rumoca_core::Expression::VarRef { name, .. }) = evaluated else {
        panic!("enum literal remains a source reference");
    };
    assert!(!name.is_generated());
    assert_eq!(
        name.component_ref()
            .expect("source enum literal retains its structured identity")
            .target_def_id(),
        component_ref
            .target_def_id()
            .expect("resolved fixture enum literal has an exact target")
    );
}

#[test]
fn unresolved_enum_literal_is_not_fabricated_during_constant_folding() {
    let path = "Modelica.Electrical.Digital.Interfaces.Logic.'U'";
    let mut component_ref = comp_ref(path);
    component_ref.parts[1].def_id = None;
    let expr = ast::Expression::ComponentReference(component_ref);

    assert_eq!(
        try_eval_const_flat_expr_with_scope(&expr, &Context::new(), ""),
        None,
        "constant folding must defer an unresolved reference to the semantic pipeline"
    );
}

#[test]
fn const_flat_expr_preserves_array_parameter_refs() {
    let mut ctx = Context::new();
    ctx.array_dimensions.insert("c0".to_string(), vec![0]);
    ctx.real_parameter_values.insert("c0".to_string(), 0.0);
    let expr = ast::Expression::ComponentReference(comp_ref("c0"));

    assert_eq!(try_eval_const_flat_expr_with_scope(&expr, &ctx, ""), None);
}

#[test]
fn structural_bool_pre_eval_keeps_sample_event_indicator_runtime() {
    let mut eval_ctx = rumoca_eval_flat::constant::EvalContext::structural_preidentity();
    eval_ctx.add_parameter(
        "sampleTrigger.startTime".to_string(),
        rumoca_eval_flat::constant::Value::Real(0.0),
    );
    eval_ctx.add_parameter(
        "sampleTrigger.period".to_string(),
        rumoca_eval_flat::constant::Value::Real(0.5),
    );
    let eq = simple_equation(
        "y",
        call_expr(
            "sample",
            vec![
                ast::Expression::ComponentReference(comp_ref("startTime")),
                ast::Expression::ComponentReference(comp_ref("period")),
            ],
        ),
    );

    // MLS §16.5.1 / Appendix B: sample(start, interval) is an event
    // indicator, not a structural Boolean constant.
    let tree = ClassTree::new();
    let class_index = ClassDefIndex::from_tree(&tree);
    let mut calls = empty_call_canonicalizer(&tree, &class_index);
    assert_eq!(
        try_eval_structural_equation(
            &eq,
            &QualifiedName::from_dotted("sampleTrigger"),
            &Context::new(),
            &eval_ctx,
            &mut calls,
        )
        .unwrap(),
        None
    );
}

#[test]
fn structural_boolean_target_uses_the_effective_type_root() {
    let effective_type = TypeId(0x00ff_0042);
    let tree = ClassTree::new();
    let boolean_type = tree.type_table.boolean();
    let real_type = tree.type_table.real();
    let equation = simple_equation(
        "flag",
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token: token("true"),
            span: test_span(),
        },
    );
    let source_ref = comp_ref("flag");
    let mut overlay = InstanceOverlay::new();
    let owner_id = overlay.alloc_id();
    let first_element_id = overlay.alloc_id();
    let second_element_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: owner_id,
            qualified_name: QualifiedName::from_dotted("M"),
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(InstanceData {
            instance_id: first_element_id,
            owner_class_id: Some(owner_id),
            component_ref: Some(
                super::core_component_reference_from_ast(&source_ref)
                    .expect("the resolved fixture has exact component identity"),
            ),
            qualified_name: QualifiedName::from_dotted("M.flag"),
            type_id: effective_type,
            is_primitive: true,
            ..InstanceData::default()
        })
        .expect("fixture occurrence insertion must succeed");
    overlay.type_roots.insert(effective_type, boolean_type);

    assert!(equation_targets_boolean_instance(
        &equation,
        owner_id,
        &overlay,
        boolean_type
    ));

    let mut first_element = QualifiedName::from_dotted("M");
    first_element.push("flag".to_string(), vec![1]);
    overlay
        .components
        .get_mut(&first_element_id)
        .expect("fixture component exists")
        .qualified_name = first_element;
    let mut second_element = QualifiedName::from_dotted("M");
    second_element.push("flag".to_string(), vec![2]);
    overlay
        .add_component(InstanceData {
            instance_id: second_element_id,
            owner_class_id: Some(owner_id),
            component_ref: Some(
                super::core_component_reference_from_ast(&source_ref)
                    .expect("the second scalar element retains declaration identity"),
            ),
            qualified_name: second_element,
            type_id: effective_type,
            is_primitive: true,
            ..InstanceData::default()
        })
        .expect("fixture occurrence insertion must succeed");
    overlay.array_parent_dims.insert(
        QualifiedName::from_dotted("M.flag").to_component_path(),
        vec![2],
    );
    assert!(
        !equation_targets_boolean_instance(&equation, owner_id, &overlay, boolean_type),
        "production-shape scalar elements must not make a whole-array equation scalar"
    );

    overlay.array_parent_dims.clear();
    overlay
        .components
        .get_mut(&first_element_id)
        .expect("fixture component exists")
        .qualified_name = QualifiedName::from_dotted("M.flag");

    overlay.type_roots.insert(effective_type, real_type);
    assert!(
        !equation_targets_boolean_instance(&equation, owner_id, &overlay, boolean_type),
        "a non-Boolean effective root must not enter Boolean structural evaluation"
    );
}
