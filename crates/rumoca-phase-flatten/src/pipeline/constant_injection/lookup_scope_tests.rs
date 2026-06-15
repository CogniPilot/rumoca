use std::sync::Arc;

use super::{
    collect_component_binding_values, inject_alias_component_package_constants, lookup_with_scope,
    try_eval_const_boolean_with_scope, try_eval_const_flat_expr_with_scope,
    try_eval_const_integer_with_scope, try_eval_structural_equation,
};
use crate::Context;
use rumoca_core::{ClassType, DefId, Literal, OpBinary, Token, Variability};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::{
    ClassDef, ClassDefIndex, Component, ComponentRefPart, ComponentReference, InstanceData,
    InstanceId, InstanceOverlay, QualifiedName,
};

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

fn int_expr(value: i64) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        token: token(&value.to_string()),
        span: rumoca_core::Span::DUMMY,
    }
}

fn real_expr(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
        token: token(value),
        span: rumoca_core::Span::DUMMY,
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
            })
            .collect(),
        def_id: None,
        span: rumoca_core::Span::DUMMY,
    }
}

fn call_expr(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: comp_ref(name),
        args,
        span: rumoca_core::Span::DUMMY,
    }
}

fn field_access_expr(base: &str, field: &str) -> ast::Expression {
    ast::Expression::FieldAccess {
        base: Arc::new(ast::Expression::ComponentReference(comp_ref(base))),
        field: field.to_string(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn eq_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
    ast::Expression::Binary {
        op: OpBinary::Eq,
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),
        span: rumoca_core::Span::DUMMY,
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
            ..Component::default()
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
        ..Component::default()
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
    );

    assert_eq!(
        ctx.constant_values.get("mp.SpiceConstants.CKTgmin"),
        Some(&rumoca_core::Expression::Literal {
            value: Literal::Real(1e-12),
            span: rumoca_core::Span::DUMMY,
        })
    );
    assert!(ctx.constant_values.contains_key("mp.C.CKTgmin"));
    assert!(!ctx.constant_values.contains_key("mp.CKTgmin"));
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
    assert_eq!(scope.parent_ident(), Some("Medium"));
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
            span: rumoca_core::Span::DUMMY,
        })
    );
}

#[test]
fn component_binding_collection_uses_only_structural_components() {
    let mut overlay = InstanceOverlay::new();
    overlay.add_component(InstanceData {
        instance_id: InstanceId::new(1),
        qualified_name: QualifiedName::from_dotted("M.x"),
        variability: Variability::Empty,
        binding: Some(real_expr("1.0")),
        ..InstanceData::default()
    });
    overlay.add_component(InstanceData {
        instance_id: InstanceId::new(2),
        qualified_name: QualifiedName::from_dotted("M.p"),
        variability: Variability::Parameter(Token::default()),
        binding: Some(real_expr("2.0")),
        ..InstanceData::default()
    });
    let mut eval_ctx = rumoca_eval_flat::constant::EvalContext::default();

    collect_component_binding_values(&overlay, &mut eval_ctx).unwrap();

    assert!(eval_ctx.get("M.x").is_none());
    assert!(eval_ctx.get("M.p").is_some());
}

#[test]
fn const_flat_expr_accepts_enum_literal_component_ref() {
    let expr = ast::Expression::ComponentReference(comp_ref(
        "Modelica.Electrical.Digital.Interfaces.Logic.'U'",
    ));

    assert_eq!(
        try_eval_const_flat_expr_with_scope(&expr, &Context::new(), ""),
        Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("Modelica.Electrical.Digital.Interfaces.Logic.'U'"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        })
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
    let mut eval_ctx = rumoca_eval_flat::constant::EvalContext::new();
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
    assert_eq!(
        try_eval_structural_equation(
            &eq,
            &QualifiedName::from_dotted("sampleTrigger"),
            &Context::new(),
            &eval_ctx
        )
        .unwrap(),
        None
    );
}

#[test]
fn const_boolean_enum_eq_accepts_suffix_qualification() {
    let mut ctx = Context::new();
    ctx.enum_parameter_values.insert(
        "controllerType".to_string(),
        "Modelica.Blocks.Types.SimpleController.PI".to_string(),
    );

    let expr = eq_expr(
        ast::Expression::ComponentReference(comp_ref("controllerType")),
        ast::Expression::ComponentReference(comp_ref("SimpleController.PI")),
    );
    assert_eq!(
        try_eval_const_boolean_with_scope(&expr, &ctx, ""),
        Some(true)
    );
}

#[test]
fn const_boolean_enum_eq_accepts_shared_type_literal_tail() {
    let mut ctx = Context::new();
    ctx.enum_parameter_values.insert(
        "frameResolve".to_string(),
        "sensor_frame_a2.MultiBody.Types.ResolveInFrameA.frame_resolve".to_string(),
    );

    let expr = eq_expr(
        ast::Expression::ComponentReference(comp_ref("frameResolve")),
        ast::Expression::ComponentReference(comp_ref(
            "Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve",
        )),
    );
    assert_eq!(
        try_eval_const_boolean_with_scope(&expr, &ctx, ""),
        Some(true)
    );
}

#[test]
fn const_boolean_enum_eq_rejects_different_enum_type() {
    let mut ctx = Context::new();
    ctx.enum_parameter_values.insert(
        "mode".to_string(),
        "Modelica.Blocks.Types.Init.PI".to_string(),
    );

    let expr = eq_expr(
        ast::Expression::ComponentReference(comp_ref("mode")),
        ast::Expression::ComponentReference(comp_ref("Modelica.Blocks.Types.SimpleController.PI")),
    );
    assert_eq!(
        try_eval_const_boolean_with_scope(&expr, &ctx, ""),
        Some(false)
    );
}
