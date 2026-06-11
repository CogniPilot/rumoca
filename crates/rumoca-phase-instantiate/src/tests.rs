use super::*;
use rumoca_ir_ast as ast;

/// Helper to create a token with text for testing.
fn make_token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: std::sync::Arc::from(text),
        location: rumoca_core::Location::default(),
        token_number: 0,
        token_type: 0,
    }
}

fn make_location(file_name: &str, start: u32, end: u32) -> rumoca_core::Location {
    rumoca_core::Location {
        file_name: file_name.to_string(),
        start,
        end,
        ..Default::default()
    }
}

fn make_token_at(text: &str, file_name: &str, start: u32, end: u32) -> rumoca_core::Token {
    rumoca_core::Token {
        text: std::sync::Arc::from(text),
        location: make_location(file_name, start, end),
        token_number: 0,
        token_type: 0,
    }
}

/// Helper to create a Name for testing.
fn make_name(text: &str) -> rumoca_ir_ast::Name {
    rumoca_ir_ast::Name {
        name: vec![make_token(text)],
        def_id: None,
    }
}

fn make_int_expr(value: i64) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: make_token(&value.to_string()),
        span: rumoca_core::Span::DUMMY,
    }
}

fn terminal_text(expr: &ast::Expression) -> &str {
    match expr {
        ast::Expression::Terminal { token, .. } => token.text.as_ref(),
        other => panic!("expected terminal expression, got {other:?}"),
    }
}

fn make_bool_expr(value: bool) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::Bool,
        token: make_token(if value { "true" } else { "false" }),
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_string_expr(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::String,
        token: make_token(&format!("\"{value}\"")),
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_comp_ref_expr(names: &[&str]) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: names
            .iter()
            .map(|name| ast::ComponentRefPart {
                ident: make_token(name),
                subs: None,
            })
            .collect(),
        def_id: None,
        span: rumoca_core::Span::DUMMY,
    })
}

fn make_eval_ctx<'a>(
    tree: &'a ast::ClassTree,
    mod_env: &'a ast::ModificationEnvironment,
    effective_components: &'a IndexMap<String, ast::Component>,
) -> InstantiateEvalCtx<'a> {
    InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    }
}

fn make_comp_ref_expr_at(names: &[&str], file_name: &str, start: u32, end: u32) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: names
            .iter()
            .enumerate()
            .map(|(index, name)| ast::ComponentRefPart {
                ident: make_token_at(
                    name,
                    file_name,
                    start + u32::try_from(index).unwrap(),
                    end + u32::try_from(index).unwrap(),
                ),
                subs: None,
            })
            .collect(),
        def_id: None,
        span: rumoca_core::Span::DUMMY,
    })
}

fn make_binary_expr(
    op: rumoca_core::OpBinary,
    lhs: ast::Expression,
    rhs: ast::Expression,
) -> ast::Expression {
    ast::Expression::Binary {
        op,
        lhs: std::sync::Arc::new(lhs),
        rhs: std::sync::Arc::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_component(name: &str, type_name: &str, type_def_id: Option<DefId>) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        name_token: make_token(name),
        type_name: make_name(type_name),
        type_def_id,
        ..Default::default()
    }
}

fn class_with_component(
    class_name: &str,
    class_def_id: DefId,
    class_range: (u32, u32),
    component: ast::Component,
) -> ast::ClassDef {
    let mut class = ast::ClassDef {
        def_id: Some(class_def_id),
        name: make_token_at(class_name, "scope.mo", class_range.0, class_range.0 + 1),
        location: make_location("scope.mo", class_range.0, class_range.1),
        ..Default::default()
    };
    class.components.insert(component.name.clone(), component);
    class
}

fn context_with_source_scope_tree(classes: Vec<ast::ClassDef>) -> InstantiateContext {
    let mut tree = ast::ClassTree::new();
    for class in classes {
        tree.definitions
            .classes
            .insert(class.name.text.to_string(), class);
    }
    let mut ctx = InstantiateContext::new();
    ctx.index_source_scopes(&tree);
    ctx
}

fn make_simple_equation(lhs: &str, rhs: i64) -> ast::Equation {
    ast::Equation::Simple {
        lhs: make_comp_ref_expr(&[lhs]),
        rhs: make_int_expr(rhs),
    }
}

#[test]
fn test_extract_attributes_preserves_local_fixed_with_local_start() {
    let mut comp = make_component("x", "Real", None);
    comp.modifications
        .insert("start".to_string(), make_int_expr(1));
    comp.modifications
        .insert("fixed".to_string(), make_bool_expr(true));

    let tree = ast::ClassTree::default();
    let mod_env = ast::ModificationEnvironment::new();
    let effective_components = IndexMap::default();
    let eval_ctx = make_eval_ctx(&tree, &mod_env, &effective_components);
    let attrs = extract_attributes(&comp, &mod_env, "x", &eval_ctx, &[])
        .expect("valid attributes should extract");

    assert!(attrs.start.is_some());
    assert_eq!(attrs.fixed, Some(true));
}

#[test]
fn test_extract_attributes_preserves_local_fixed_with_outer_start() {
    let mut comp = make_component("x", "Real", None);
    comp.modifications
        .insert("fixed".to_string(), make_bool_expr(true));

    let mut mod_env = ast::ModificationEnvironment::new();
    mod_env.add(
        ast::QualifiedName::from_dotted("x.start"),
        ast::ModificationValue::simple(make_int_expr(2)),
    );

    let tree = ast::ClassTree::default();
    let effective_components = IndexMap::default();
    let eval_ctx = make_eval_ctx(&tree, &mod_env, &effective_components);
    let attrs = extract_attributes(&comp, &mod_env, "x", &eval_ctx, &[])
        .expect("valid attributes should extract");

    assert!(attrs.start.is_some());
    assert_eq!(attrs.fixed, Some(true));
}

#[test]
fn test_extract_attributes_outer_state_select_overrides_local() {
    let mut comp = make_component("x", "Real", None);
    comp.modifications.insert(
        "stateSelect".to_string(),
        make_comp_ref_expr(&["StateSelect", "prefer"]),
    );

    let mut mod_env = ast::ModificationEnvironment::new();
    mod_env.add(
        ast::QualifiedName::from_dotted("x.stateSelect"),
        ast::ModificationValue::simple(make_comp_ref_expr(&["StateSelect", "never"])),
    );

    let tree = ast::ClassTree::default();
    let effective_components = IndexMap::default();
    let eval_ctx = make_eval_ctx(&tree, &mod_env, &effective_components);
    let attrs = extract_attributes(&comp, &mod_env, "x", &eval_ctx, &[])
        .expect("valid attributes should extract");

    assert_eq!(attrs.state_select, rumoca_core::StateSelect::Never);
}

#[test]
fn test_extract_attributes_evaluates_state_select_parameter() {
    let mut comp = make_component("x", "Real", None);
    comp.modifications.insert(
        "stateSelect".to_string(),
        make_comp_ref_expr(&["stateSelect"]),
    );

    let mut state_select = make_component("stateSelect", "StateSelect", None);
    state_select.binding = Some(make_comp_ref_expr(&["StateSelect", "prefer"]));
    let mut effective_components = IndexMap::default();
    effective_components.insert("stateSelect".to_string(), state_select);

    let tree = ast::ClassTree::default();
    let mod_env = ast::ModificationEnvironment::new();
    let eval_ctx = make_eval_ctx(&tree, &mod_env, &effective_components);
    let attrs = extract_attributes(&comp, &mod_env, "x", &eval_ctx, &[])
        .expect("valid attributes should extract");

    assert_eq!(attrs.state_select, rumoca_core::StateSelect::Prefer);
}

#[test]
fn test_lookup_type_info_accepts_predefined_state_select() {
    let tree = ast::ClassTree::new();
    let comp = make_component("stateSelect", "StateSelect", None);

    let info = lookup_type_info(&tree, &comp, "StateSelect")
        .expect("predefined StateSelect type should resolve through the type table");

    assert!(info.class_def.is_none());
    assert!(!info.is_primitive);
    assert!(info.is_discrete);
}

#[test]
fn test_extract_attributes_rejects_invalid_state_select() {
    let mut comp = make_component("x", "Real", None);
    comp.modifications.insert(
        "stateSelect".to_string(),
        make_comp_ref_expr(&["sometimes"]),
    );

    let tree = ast::ClassTree::default();
    let mod_env = ast::ModificationEnvironment::new();
    let effective_components = IndexMap::default();
    let eval_ctx = make_eval_ctx(&tree, &mod_env, &effective_components);
    let err = extract_attributes(&comp, &mod_env, "x", &eval_ctx, &[])
        .expect_err("invalid stateSelect literal should fail");

    assert!(err.to_string().contains("stateSelect"));
}

#[test]
fn test_validate_final_type_attribute_rejects_outer_override() {
    let real_def = DefId::new(1);
    let voltage_def = DefId::new(2);
    let mut tree = ast::ClassTree::default();
    let real = ast::ClassDef {
        name: make_token("Real"),
        def_id: Some(real_def),
        ..Default::default()
    };
    let voltage = ast::ClassDef {
        name: make_token("Voltage"),
        def_id: Some(voltage_def),
        extends: vec![ast::Extend {
            base_name: make_name("Real"),
            base_def_id: Some(real_def),
            modifications: vec![ast::ExtendModification {
                expr: ast::Expression::Modification {
                    target: match make_comp_ref_expr(&["unit"]) {
                        ast::Expression::ComponentReference(cref) => cref,
                        _ => unreachable!(),
                    },
                    value: std::sync::Arc::new(make_string_expr("V")),
                    span: rumoca_core::Span::DUMMY,
                },
                final_: true,
                ..Default::default()
            }],
            ..Default::default()
        }],
        ..Default::default()
    };
    tree.def_map.insert(real_def, "Real".to_string());
    tree.def_map.insert(voltage_def, "Voltage".to_string());
    tree.definitions.classes.insert("Real".to_string(), real);
    tree.definitions
        .classes
        .insert("Voltage".to_string(), voltage.clone());

    let comp = make_component("x", "Voltage", Some(voltage_def));
    let mut mod_env = ast::ModificationEnvironment::new();
    mod_env.add(
        ast::QualifiedName::from_dotted("x.unit"),
        ast::ModificationValue::simple(make_string_expr("kV")),
    );

    let err = validate_final_type_attribute_overrides(&tree, Some(&voltage), &comp, &mod_env)
        .expect_err("overriding final type attribute should fail");
    assert!(err.to_string().contains("final"));
}

#[test]
fn test_parameter_declaration_binding_promotes_builtin_default_start() {
    let mut comp = make_component("p", "Real", None);
    comp.variability = rumoca_core::Variability::Parameter(make_token("parameter"));
    comp.start = make_int_expr(0);
    comp.binding = Some(make_int_expr(5));

    let tree = ast::ClassTree::default();
    let mod_env = ast::ModificationEnvironment::new();
    let effective_components = IndexMap::default();
    let eval_ctx = make_eval_ctx(&tree, &mod_env, &effective_components);
    let result = extract_component_attrs_and_binding(&comp, &mod_env, &eval_ctx, &[])
        .expect("valid attributes should extract");

    assert_eq!(
        result.attrs.start.as_ref().map(terminal_text),
        Some("5"),
        "parameter declaration binding should provide start when no explicit start exists"
    );
}

#[test]
fn test_parameter_declaration_binding_does_not_override_explicit_start() {
    let mut comp = make_component("p", "Real", None);
    comp.variability = rumoca_core::Variability::Parameter(make_token("parameter"));
    comp.start = make_int_expr(0);
    comp.modifications
        .insert("start".to_string(), make_int_expr(2));
    comp.binding = Some(make_int_expr(5));

    let tree = ast::ClassTree::default();
    let mod_env = ast::ModificationEnvironment::new();
    let effective_components = IndexMap::default();
    let eval_ctx = make_eval_ctx(&tree, &mod_env, &effective_components);
    let result = extract_component_attrs_and_binding(&comp, &mod_env, &eval_ctx, &[])
        .expect("valid attributes should extract");

    assert_eq!(
        result.attrs.start.as_ref().map(terminal_text),
        Some("2"),
        "explicit start must win over declaration binding"
    );
}

#[test]
fn test_continuous_declaration_binding_preserves_runtime_expression() {
    let mut phi_rel = make_component("phi_rel", "Real", None);
    phi_rel.start = make_int_expr(0);

    let mut phi_rel0 = make_component("phi_rel0", "Real", None);
    phi_rel0.variability = rumoca_core::Variability::Parameter(make_token("parameter"));
    phi_rel0.binding = Some(make_int_expr(0));

    let binding = make_binary_expr(
        rumoca_core::OpBinary::Sub,
        make_comp_ref_expr(&["phi_rel"]),
        make_comp_ref_expr(&["phi_rel0"]),
    );
    let mut phi_diff = make_component("phi_diff", "Real", None);
    phi_diff.binding = Some(binding.clone());

    let mut effective_components = IndexMap::default();
    effective_components.insert("phi_rel".to_string(), phi_rel);
    effective_components.insert("phi_rel0".to_string(), phi_rel0);
    let tree = ast::ClassTree::default();
    let mut ctx = InstantiateContext::new();

    let info = prepare_component_binding_info(
        &tree,
        &phi_diff,
        &mut ctx,
        &effective_components,
        false,
        &[],
    )
    .expect("continuous binding should prepare");

    assert_eq!(
        info.binding.as_ref(),
        Some(&binding),
        "continuous declaration bindings must not be evaluated from sibling start values"
    );
}

#[test]
fn test_parameter_declaration_binding_still_resolves_structural_expression() {
    let mut n = make_component("n", "Integer", None);
    n.variability = rumoca_core::Variability::Parameter(make_token("parameter"));
    n.binding = Some(make_int_expr(5));

    let mut p = make_component("p", "Integer", None);
    p.variability = rumoca_core::Variability::Parameter(make_token("parameter"));
    p.binding = Some(make_comp_ref_expr(&["n"]));

    let mut effective_components = IndexMap::default();
    effective_components.insert("n".to_string(), n);
    let tree = ast::ClassTree::default();
    let mut ctx = InstantiateContext::new();

    let info =
        prepare_component_binding_info(&tree, &p, &mut ctx, &effective_components, true, &[])
            .expect("parameter binding should prepare");

    assert_eq!(
        info.binding.as_ref().map(terminal_text),
        Some("5"),
        "structural parameter declaration bindings should still resolve"
    );
}

#[test]
fn test_equations_to_instance_without_connections_filters_connect_equations() {
    let equations = vec![
        ast::Equation::Connect {
            lhs: ast::ComponentReference {
                local: false,
                parts: vec![ast::ComponentRefPart {
                    ident: make_token("a"),
                    subs: None,
                }],
                def_id: None,
                span: rumoca_core::Span::DUMMY,
            },
            rhs: ast::ComponentReference {
                local: false,
                parts: vec![ast::ComponentRefPart {
                    ident: make_token("b"),
                    subs: None,
                }],
                def_id: None,
                span: rumoca_core::Span::DUMMY,
            },
        },
        ast::Equation::Simple {
            lhs: make_comp_ref_expr(&["x"]),
            rhs: make_int_expr(1),
        },
    ];

    let source_map = rumoca_core::SourceMap::new();
    let origin = ast::QualifiedName::from_ident("M");
    let ctx = InstantiateContext::new();
    let converted =
        equations_to_instance_without_connections(&ctx, &equations, &origin, &source_map);

    assert_eq!(converted.len(), 1);
    assert!(matches!(
        converted[0].equation,
        ast::Equation::Simple { .. }
    ));
    assert_eq!(converted[0].origin, origin);
}

#[test]
fn test_context_path() {
    let mut ctx = InstantiateContext::new();
    assert!(ctx.current_path().is_empty());

    ctx.push_path("model");
    ctx.push_path_part("component", vec![2]);
    let path = ctx.current_path();
    assert_eq!(path.to_flat_string(), "model.component[2]");
    assert_eq!(path.parts[1].0, "component");
    assert_eq!(path.parts[1].1, vec![2]);

    ctx.pop_path();
    assert_eq!(ctx.current_path().to_flat_string(), "model");
}

#[test]
fn test_zero_sized_array_component_records_parent_dimensions() {
    let mut ctx = InstantiateContext::new();
    let mut overlay = ast::InstanceOverlay::default();

    ctx.push_path("tank1");
    register_zero_sized_array_component(&mut ctx, &mut overlay, "topPorts", &[0]);
    ctx.pop_path();

    assert_eq!(
        overlay.array_parent_dims.get("tank1.topPorts"),
        Some(&vec![0])
    );
}

#[test]
fn test_extract_int_params_record_alias_prefers_rebound_field_values() {
    // Reproduces CellRCStack pattern:
    // parameter Data cellData; parameter Data cellData2(nRC=2); ... cellData=cellData2
    // For-loop ranges over cellData.nRC must use 2, not cellData's default 1.
    let mut mod_env = ast::ModificationEnvironment::new();
    mod_env.add(
        ast::QualifiedName::from_dotted("cellData.nRC"),
        ast::ModificationValue::simple(make_int_expr(1)),
    );
    mod_env.add(
        ast::QualifiedName::from_dotted("cellData2.nRC"),
        ast::ModificationValue::simple(make_int_expr(2)),
    );
    mod_env.add(
        ast::QualifiedName::from_ident("cellData"),
        ast::ModificationValue::simple(make_comp_ref_expr(&["cellData2"])),
    );

    let effective_components = IndexMap::default();
    let tree = ast::ClassTree::default();
    let eval_ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &mod_env,
        effective_components: &effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    let int_params = extract_int_params_with_mods(&eval_ctx);

    assert_eq!(
        int_params.get("cellData2.nRC"),
        Some(&2),
        "target record field should be present"
    );
    assert_eq!(
        int_params.get("cellData.nRC"),
        Some(&2),
        "aliased record field should override stale/default value"
    );
}

// -------------------------------------------------------------------------
// Inner/Outer tests (MLS §5.4)
// -------------------------------------------------------------------------

#[test]
fn test_register_and_find_inner() {
    let mut ctx = InstantiateContext::new();

    // Register an inner declaration
    let qn = ast::QualifiedName::from_dotted("system.world");
    ctx.register_inner("world", qn, "World", None);

    // Should be able to find it
    let inner = ctx.find_inner("world");
    assert!(inner.is_some());
    assert_eq!(
        inner.unwrap().qualified_name.to_flat_string(),
        "system.world"
    );
    assert_eq!(inner.unwrap().type_name, "World");

    // Non-existent inner should not be found
    assert!(ctx.find_inner("nonexistent").is_none());
}

#[test]
fn test_inner_scope_visibility() {
    let mut ctx = InstantiateContext::new();

    // Register an inner in the root scope
    let qn = ast::QualifiedName::from_dotted("root.g");
    ctx.register_inner("g", qn, "Real", None);

    // Push a new scope (entering a nested component)
    ctx.push_inner_scope();

    // Inner from parent scope should still be visible
    assert!(ctx.find_inner("g").is_some());

    // Register a different inner in the nested scope
    let qn2 = ast::QualifiedName::from_dotted("root.nested.x");
    ctx.register_inner("x", qn2, "Real", None);
    assert!(ctx.find_inner("x").is_some());

    // Pop the scope
    ctx.pop_inner_scope();

    // Inner from root scope should still be visible
    assert!(ctx.find_inner("g").is_some());

    // Inner from nested scope should NOT be visible anymore
    assert!(ctx.find_inner("x").is_none());
}

#[test]
fn test_inner_shadowing() {
    // MLS §5.4: An outer element references the closest inner element
    let mut ctx = InstantiateContext::new();

    // Register "g" in root scope
    let qn_outer = ast::QualifiedName::from_dotted("root.g");
    ctx.register_inner("g", qn_outer, "Real", None);

    // Push a new scope and register another "g"
    ctx.push_inner_scope();
    let qn_inner = ast::QualifiedName::from_dotted("root.nested.g");
    ctx.register_inner("g", qn_inner, "Real", None);

    // Should find the inner (closer) "g", not the outer one
    let inner = ctx.find_inner("g").unwrap();
    assert_eq!(inner.qualified_name.to_flat_string(), "root.nested.g");

    // Pop the scope
    ctx.pop_inner_scope();

    // Now should find the outer "g"
    let inner = ctx.find_inner("g").unwrap();
    assert_eq!(inner.qualified_name.to_flat_string(), "root.g");
}

#[test]
fn test_late_inner_declaration_resolves_pending_outer_without_synthesis() {
    let state_id = DefId::new(100);
    let uses_outer_id = DefId::new(101);
    let root_id = DefId::new(102);

    let state = ast::ClassDef {
        def_id: Some(state_id),
        name: make_token("State"),
        components: [("x".to_string(), make_component("x", "Real", None))]
            .into_iter()
            .collect(),
        equations: vec![make_simple_equation("x", 1)],
        ..Default::default()
    };

    let mut outer_shared = make_component("shared", "State", Some(state_id));
    outer_shared.outer = true;
    let uses_outer = ast::ClassDef {
        def_id: Some(uses_outer_id),
        name: make_token("UsesOuter"),
        components: [("shared".to_string(), outer_shared)].into_iter().collect(),
        ..Default::default()
    };

    let child = make_component("child", "UsesOuter", Some(uses_outer_id));
    let mut inner_shared = make_component("shared", "State", Some(state_id));
    inner_shared.inner = true;
    let root = ast::ClassDef {
        def_id: Some(root_id),
        name: make_token("Root"),
        components: [
            ("child".to_string(), child),
            ("shared".to_string(), inner_shared),
        ]
        .into_iter()
        .collect(),
        ..Default::default()
    };

    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("State".to_string(), state);
    tree.definitions
        .classes
        .insert("UsesOuter".to_string(), uses_outer);
    tree.definitions.classes.insert("Root".to_string(), root);
    tree.def_map.insert(state_id, "State".to_string());
    tree.def_map.insert(uses_outer_id, "UsesOuter".to_string());
    tree.def_map.insert(root_id, "Root".to_string());

    let outcome = instantiate_model_with_outcome(&tree, "Root");
    let InstantiationOutcome::Success(overlay) = outcome else {
        panic!("late inner declaration should resolve pending outer reference");
    };

    assert!(
        overlay.synthesized_inners.is_empty(),
        "the real late inner declaration should avoid synthetic retry"
    );
    assert_eq!(
        overlay.outer_prefix_to_inner.get("child.shared"),
        Some(&"shared".to_string())
    );

    let shared_classes: Vec<_> = overlay
        .classes
        .values()
        .filter(|class| class.qualified_name.to_flat_string() == "shared")
        .collect();
    assert_eq!(
        shared_classes.len(),
        1,
        "late inner resolution should instantiate the shared inner once"
    );
    assert_eq!(shared_classes[0].equations.len(), 1);
}

// -------------------------------------------------------------------------
// Type compatibility tests (MLS §5.4)
// -------------------------------------------------------------------------

#[test]
fn test_type_compatible_exact_match() {
    // Exact type name match is always compatible
    let tree = ast::ClassTree::default();
    assert!(is_type_compatible(&tree, "Real", "Real"));
    assert!(is_type_compatible(&tree, "MyConnector", "MyConnector"));
}

#[test]
fn test_type_compatible_builtin_mismatch() {
    // Built-in types must match exactly
    let tree = ast::ClassTree::default();
    assert!(!is_type_compatible(&tree, "Real", "Integer"));
    assert!(!is_type_compatible(&tree, "Boolean", "String"));
    assert!(!is_type_compatible(&tree, "Real", "Boolean"));
}

#[test]
fn test_type_compatible_class_inheritance() {
    // Test that a derived class is compatible with its base
    // Create a simple class hierarchy: DerivedConnector extends BaseConnector
    let mut tree = ast::ClassTree::default();

    // Base class
    let base = ast::ClassDef {
        name: make_token("BaseConnector"),
        ..Default::default()
    };

    // Derived class that extends Base
    let derived = ast::ClassDef {
        name: make_token("DerivedConnector"),
        extends: vec![ast::Extend {
            base_name: make_name("BaseConnector"),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions
        .classes
        .insert("BaseConnector".to_string(), base);
    tree.definitions
        .classes
        .insert("DerivedConnector".to_string(), derived);

    // DerivedConnector should be compatible with BaseConnector (subtype)
    assert!(is_type_compatible(
        &tree,
        "BaseConnector",
        "DerivedConnector"
    ));

    // BaseConnector is NOT compatible with DerivedConnector (not a subtype)
    assert!(!is_type_compatible(
        &tree,
        "DerivedConnector",
        "BaseConnector"
    ));
}

#[test]
fn test_class_extends_direct() {
    let tree = ast::ClassTree::default();

    // Create a class that directly extends BaseConnector
    let derived = ast::ClassDef {
        name: make_token("Derived"),
        extends: vec![ast::Extend {
            base_name: make_name("BaseConnector"),
            ..Default::default()
        }],
        ..Default::default()
    };

    assert!(class_extends(&tree, &derived, "BaseConnector"));
    assert!(!class_extends(&tree, &derived, "OtherClass"));
}

#[test]
fn test_class_extends_transitive() {
    // Create hierarchy: C extends B extends A
    let mut tree = ast::ClassTree::default();

    let class_a = ast::ClassDef {
        name: make_token("A"),
        ..Default::default()
    };

    let class_b = ast::ClassDef {
        name: make_token("B"),
        extends: vec![ast::Extend {
            base_name: make_name("A"),
            ..Default::default()
        }],
        ..Default::default()
    };

    let class_c = ast::ClassDef {
        name: make_token("C"),
        extends: vec![ast::Extend {
            base_name: make_name("B"),
            ..Default::default()
        }],
        ..Default::default()
    };

    tree.definitions.classes.insert("A".to_string(), class_a);
    tree.definitions.classes.insert("B".to_string(), class_b);
    tree.definitions
        .classes
        .insert("C".to_string(), class_c.clone());

    // C extends B directly
    assert!(class_extends(&tree, &class_c, "B"));
    // C extends A transitively (through B)
    assert!(class_extends(&tree, &class_c, "A"));
    // C does not extend D
    assert!(!class_extends(&tree, &class_c, "D"));
}

#[test]
fn test_record_declaration_binding_expands_in_instance_owner_scope() {
    let scope = binding_scope_for_record_expansion(
        &ast::QualifiedName::from_dotted("reluctance_m.V_m"),
        false,
        None,
    );

    assert_eq!(scope, Some(ast::QualifiedName::from_dotted("reluctance_m")));
}

#[test]
fn test_record_modifier_binding_keeps_lexical_source_scope() {
    let source_scope = ast::QualifiedName::from_dotted("outer_model");
    let scope = binding_scope_for_record_expansion(
        &ast::QualifiedName::from_dotted("sub.V_m"),
        true,
        Some(&source_scope),
    );

    assert_eq!(scope, Some(source_scope));
}

#[test]
fn inherited_attribute_modification_keeps_written_source_scope() {
    let mut comp = make_component("x", "Logic", Some(DefId::new(20)));
    comp.def_id = Some(DefId::new(10));
    let start_expr = make_comp_ref_expr_at(&["L", "'U'"], "scope.mo", 150, 155);
    comp.modifications
        .insert("start".to_string(), start_expr.clone());

    let ctx = context_with_source_scope_tree(vec![
        class_with_component("Base", DefId::new(1), (0, 100), comp.clone()),
        class_with_component(
            "Derived",
            DefId::new(2),
            (120, 220),
            make_component("dummy", "Real", None),
        ),
    ]);

    let mod_env = ast::ModificationEnvironment::new();
    let tree = ast::ClassTree::default();
    let effective_components = IndexMap::default();
    let eval_ctx = make_eval_ctx(&tree, &mod_env, &effective_components);
    let mut attrs = extract_attributes(&comp, &mod_env, "x", &eval_ctx, &[])
        .expect("valid attributes should extract");
    infer_local_attribute_source_scopes(&ctx, &comp, &mut attrs);

    assert_eq!(
        attrs.source_scopes.get("start"),
        Some(&ast::QualifiedName::from_ident("Derived"))
    );
    assert_eq!(attrs.start, Some(start_expr));
}

#[test]
fn local_attribute_modification_keeps_instance_qualification() {
    let mut comp = make_component("nextstate", "Logic", Some(DefId::new(20)));
    comp.def_id = Some(DefId::new(10));
    let start_expr = make_comp_ref_expr_at(&["n"], "scope.mo", 50, 51);
    comp.modifications
        .insert("start".to_string(), start_expr.clone());

    let ctx = context_with_source_scope_tree(vec![class_with_component(
        "DFFR",
        DefId::new(1),
        (0, 100),
        comp.clone(),
    )]);

    let mod_env = ast::ModificationEnvironment::new();
    let tree = ast::ClassTree::default();
    let effective_components = IndexMap::default();
    let eval_ctx = make_eval_ctx(&tree, &mod_env, &effective_components);
    let mut attrs = extract_attributes(&comp, &mod_env, "nextstate", &eval_ctx, &[])
        .expect("valid attributes should extract");
    infer_local_attribute_source_scopes(&ctx, &comp, &mut attrs);

    assert!(
        !attrs.source_scopes.contains_key("start"),
        "local attributes use the instance parent prefix so sibling references like n resolve to the current instance"
    );
    assert_eq!(attrs.start, Some(start_expr));
}
