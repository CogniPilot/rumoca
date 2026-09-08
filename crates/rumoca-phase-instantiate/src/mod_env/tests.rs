use super::*;
use std::sync::Arc;

const TEST_FILE: &str = "mod_env.mo";

fn test_location() -> rumoca_core::Location {
    rumoca_core::Location {
        start_line: 1,
        start_column: 1,
        end_line: 1,
        end_column: 2,
        start: 0,
        end: 1,
        source: rumoca_core::SourceId::from_source_name(TEST_FILE),
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_instantiate_mod_env_source_7.mo"),
        0,
        1,
    )
}

fn make_token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: std::sync::Arc::from(text),
        location: rumoca_core::Location::default(),
        token_number: 0,
        token_type: 0,
    }
}

fn make_int_expr_with_span(value: i64, span: rumoca_core::Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: make_token(&value.to_string()),
        span,
    }
}

fn make_int_expr(value: i64) -> ast::Expression {
    make_int_expr_with_span(value, rumoca_core::Span::DUMMY)
}

fn make_comp_ref_expr(names: &[&str]) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: names
            .iter()
            .map(|name| ast::ComponentRefPart {
                ident: make_token(name),
                subs: None,
                def_id: None,
            })
            .collect(),
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    })
}

fn make_resolved_comp_ref_expr(parts: &[(&str, u32)]) -> ast::Expression {
    let mut expr = make_comp_ref_expr(&parts.iter().map(|(name, _)| *name).collect::<Vec<_>>());
    let ast::Expression::ComponentReference(comp_ref) = &mut expr else {
        unreachable!("component-reference helper must return a component reference");
    };
    for (part, (_, def_id)) in comp_ref.parts.iter_mut().zip(parts) {
        part.def_id = Some(rumoca_core::DefId::new(*def_id));
    }
    expr
}

fn resolved_component(def_id: u32) -> ast::Component {
    let mut component = ast::Component::empty_with_span(test_span());
    component.def_id = Some(rumoca_core::DefId::new(def_id));
    component
}

fn make_named_arg(name: &str, value: ast::Expression) -> ast::Expression {
    ast::Expression::NamedArgument {
        name: make_token(name),
        value: Arc::new(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn active_mod_env_keys(ctx: &InstantiateContext) -> Vec<String> {
    ctx.mod_env()
        .active
        .keys()
        .map(ToString::to_string)
        .collect()
}

fn assert_mod_env_matches(
    current: &ast::ModificationEnvironment,
    snapshot: &ast::ModificationEnvironment,
) {
    assert_eq!(current.active.len(), snapshot.active.len());
    for (key, before) in &snapshot.active {
        let after = current
            .active
            .get(key)
            .expect("snapshot key remains present");
        assert_eq!(after.value, before.value);
        assert_eq!(after.source, before.source);
        assert_eq!(after.source_scope, before.source_scope);
        assert_eq!(after.each, before.each);
        assert_eq!(after.final_, before.final_);
    }
}

#[test]
fn indexed_modifier_resolution_refuses_proven_malformed_defers_unknown() {
    let array = ast::Expression::Array {
        elements: vec![make_int_expr(1)],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    let subscript = |expr| ast::Subscript::Expression(expr);
    let signed = |op, value| ast::Expression::Unary {
        op,
        rhs: Arc::new(make_int_expr(value)),
        span: test_span(),
    };
    assert!(matches!(
        select_array_value(&array, &[subscript(make_int_expr(0))]),
        ArrayValueSelection::Invalid(reason) if reason.contains("zero")
    ));
    assert!(matches!(
        select_array_value(&array, &[subscript(make_int_expr(2))]),
        ArrayValueSelection::Invalid(reason) if reason.contains("out of bounds")
    ));
    assert_eq!(
        select_array_value(
            &array,
            &[subscript(make_resolved_comp_ref_expr(&[("i", 99_001)]))]
        ),
        ArrayValueSelection::NotStatic
    );
    // A scalar literal value carries no static evidence of malformation: it may
    // be a broadcast into an array component (`Real x[3] = 1`), which a later
    // phase resolves once the declared dimensionality is known. Defer, do not
    // refuse.
    assert_eq!(
        select_array_value(&make_int_expr(1), &[subscript(make_int_expr(1))]),
        ArrayValueSelection::NotStatic
    );
    // A component reference may be array-valued; its shape is invisible here.
    assert_eq!(
        select_array_value(
            &make_resolved_comp_ref_expr(&[("arrayParam", 99_002)]),
            &[subscript(make_int_expr(1))]
        ),
        ArrayValueSelection::NotStatic
    );
    // `fill(0.0, 3)` is array-valued but not a literal array. Defer rather than
    // treating the absence of a literal as proof of malformation.
    let fill_call = ast::Expression::FunctionCall {
        comp: ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: make_token("fill"),
                subs: None,
                def_id: None,
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        },
        args: vec![make_int_expr(0), make_int_expr(3)],
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(
        select_array_value(&fill_call, &[subscript(make_int_expr(1))]),
        ArrayValueSelection::NotStatic
    );
    assert_eq!(
        select_array_value(&array, &[subscript(make_int_expr(1))]),
        ArrayValueSelection::Selected(make_int_expr(1))
    );
    assert!(matches!(
        select_array_value(
            &array,
            &[subscript(signed(rumoca_core::OpUnary::Minus, 1))]
        ),
        ArrayValueSelection::Invalid(reason) if reason.contains("negative")
    ));
    assert_eq!(
        select_array_value(&array, &[subscript(signed(rumoca_core::OpUnary::Plus, 1))]),
        ArrayValueSelection::Selected(make_int_expr(1))
    );
}

#[test]
fn known_modifier_array_static_invalid_indices_are_typed_errors() {
    let component_id = rumoca_core::DefId::new(99_000);
    let mut component = resolved_component(component_id.index());
    component.name = "values".to_string();
    let mut components = IndexMap::default();
    components.insert("values".to_string(), component);
    let mut mod_env = ast::ModificationEnvironment::default();
    mod_env.add(
        ast::QualifiedName::from_ident("values"),
        ast::ModificationValue::simple(ast::Expression::Array {
            elements: vec![make_int_expr(7)],
            is_matrix: false,
            span: test_span(),
        }),
    );
    let indexed_ref = |selector| {
        let mut expression = make_resolved_comp_ref_expr(&[("values", component_id.index())]);
        let ast::Expression::ComponentReference(reference) = &mut expression else {
            unreachable!("reference helper");
        };
        reference.span = test_span();
        reference.parts[0].subs = Some(vec![ast::Subscript::Expression(selector)]);
        expression
    };
    let tree = ast::ClassTree::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);

    for selector in [
        make_int_expr(0),
        make_int_expr(2),
        ast::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Arc::new(make_int_expr(1)),
            span: test_span(),
        },
    ] {
        let expression = indexed_ref(selector);
        let error =
            resolve_single_part_ref_expr(&expression, &mod_env, &components, &tree, &class_index)
                .expect_err("a known invalid static modifier selection must not retain the input");
        assert!(matches!(*error, InstantiateError::InvalidModPath { .. }));
    }

    let dynamic = indexed_ref(make_resolved_comp_ref_expr(&[("i", 99_001)]));
    assert_eq!(
        resolve_single_part_ref_expr(&dynamic, &mod_env, &components, &tree, &class_index)
            .expect("non-static selector may defer"),
        None
    );

    // A literal array value with an in-range static index still resolves to the
    // selected element, confirming the deferral change did not weaken the
    // resolving path.
    let selected = resolve_single_part_ref_expr(
        &indexed_ref(make_int_expr(1)),
        &mod_env,
        &components,
        &tree,
        &class_index,
    )
    .expect("in-range static selection resolves")
    .expect("a resolved edge is produced");
    assert_eq!(selected.1, make_int_expr(7));
}

#[test]
fn subscripted_modifier_over_non_literal_array_value_defers() {
    // A modifier bound to an array-valued but non-literal expression (a
    // component reference, `fill`, or an arithmetic expression) has no visible
    // element extent at this phase. Selecting an element must defer with the
    // subscripted reference intact rather than refusing, since a later phase
    // knows the declared shape. Refusing here is the defect this guards against.
    let component_id = rumoca_core::DefId::new(98_000);
    let mut component = resolved_component(component_id.index());
    component.name = "values".to_string();
    let mut components = IndexMap::default();
    components.insert("values".to_string(), component);

    let tree = ast::ClassTree::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);

    let indexed_ref = || {
        let mut expression = make_resolved_comp_ref_expr(&[("values", component_id.index())]);
        let ast::Expression::ComponentReference(reference) = &mut expression else {
            unreachable!("reference helper");
        };
        reference.span = test_span();
        reference.parts[0].subs = Some(vec![ast::Subscript::Expression(make_int_expr(2))]);
        expression
    };

    let non_literal_values = [
        // A reference to another array parameter.
        make_resolved_comp_ref_expr(&[("arrayParam", 98_100)]),
        // `fill(0.0, 3)`: array-valued, not a literal array.
        ast::Expression::FunctionCall {
            comp: ast::ComponentReference {
                local: false,
                parts: vec![ast::ComponentRefPart {
                    ident: make_token("fill"),
                    subs: None,
                    def_id: None,
                }],
                span: rumoca_core::Span::DUMMY,
                qualified_display_name: None,
            },
            args: vec![make_int_expr(0), make_int_expr(3)],
            is_partial_application: false,
            span: rumoca_core::Span::DUMMY,
        },
        // A scalar literal broadcast into an array component (`Real x[3] = 1`).
        make_int_expr(1),
    ];

    for value in non_literal_values {
        let mut mod_env = ast::ModificationEnvironment::default();
        mod_env.add(
            ast::QualifiedName::from_ident("values"),
            ast::ModificationValue::simple(value),
        );
        assert_eq!(
            resolve_single_part_ref_expr(
                &indexed_ref(),
                &mod_env,
                &components,
                &tree,
                &class_index
            )
            .expect("a non-literal array value defers rather than failing"),
            None,
            "non-literal array-valued modifier must defer, preserving the reference"
        );
    }
}

fn make_name(name: &str) -> ast::Name {
    ast::Name {
        name: vec![make_token(name)],
        def_id: None,
    }
}

#[test]
fn string_modifier_type_check_requires_segment_boundary() {
    assert!(component_type_allows_string_modifier("String"));
    assert!(component_type_allows_string_modifier("Modelica.String"));
    assert!(component_type_allows_string_modifier("Pkg.Types.String"));
    assert!(!component_type_allows_string_modifier("MyString"));
    assert!(!component_type_allows_string_modifier("Pkg.StringAlias"));
}

#[test]
fn test_resolve_sibling_modification_keeps_class_modification_reference() {
    let mut effective_components: IndexMap<String, ast::Component> = IndexMap::default();
    let mut data = resolved_component(100);
    data.name = "aimcData".to_string();
    data.modifications.insert(
        "statorCoreParameters".to_string(),
        ast::Expression::ClassModification {
            target: ast::ComponentReference {
                local: false,
                parts: vec![
                    ast::ComponentRefPart {
                        ident: make_token("Modelica"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Electrical"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Machines"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Losses"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("CoreParameters"),
                        subs: None,
                        def_id: None,
                    },
                ],
                span: rumoca_core::Span::DUMMY,
                qualified_display_name: None,
            },
            modifications: vec![
                make_named_arg("PRef", make_int_expr(410)),
                make_named_arg("VRef", make_int_expr(388)),
            ],
            each_flags: vec![false, false],
            final_flags: vec![false, false],
            redeclare_flags: vec![false, false],
            span: rumoca_core::Span::DUMMY,
        },
    );
    effective_components.insert("aimcData".to_string(), data);

    let expr = make_resolved_comp_ref_expr(&[("aimcData", 100), ("statorCoreParameters", 101)]);
    let tree = ast::ClassTree::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let resolved = resolve_modification_expr(
        &expr,
        ModifierResolveScope {
            mod_env: &ast::ModificationEnvironment::default(),
            effective_components: &effective_components,
            tree: &tree,
            imports: crate::dims::ImportRewrite::without_imports(&class_index),
        },
        false,
    )
    .expect("resolution should succeed");

    assert_eq!(
        resolved, expr,
        "record bindings should stay as references so declaration defaults are preserved"
    );
}

#[test]
fn test_resolve_sibling_modification_still_resolves_scalar_field_override() {
    let mut effective_components: IndexMap<String, ast::Component> = IndexMap::default();
    let mut data = resolved_component(110);
    data.name = "stackData".to_string();
    data.modifications
        .insert("mSystems".to_string(), make_int_expr(2));
    effective_components.insert("stackData".to_string(), data);

    let expr = make_resolved_comp_ref_expr(&[("stackData", 110), ("mSystems", 111)]);
    let tree = ast::ClassTree::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let resolved = resolve_modification_expr(
        &expr,
        ModifierResolveScope {
            mod_env: &ast::ModificationEnvironment::default(),
            effective_components: &effective_components,
            tree: &tree,
            imports: crate::dims::ImportRewrite::without_imports(&class_index),
        },
        false,
    )
    .expect("resolution should succeed");

    assert_eq!(
        resolved,
        make_int_expr(2),
        "scalar sibling field overrides should keep existing behavior"
    );
}

#[test]
fn test_declaration_binding_preserves_component_reference_identity() {
    let mut mod_env = ast::ModificationEnvironment::default();
    mod_env.add(
        ast::QualifiedName::from_ident("pathLengths"),
        ast::ModificationValue::with_source_scope(
            make_comp_ref_expr(&["length"]),
            Some(make_comp_ref_expr(&["length"])),
            Some(ast::QualifiedName::from_ident("pipe")),
        ),
    );

    let expr = make_comp_ref_expr(&["pathLengths"]);
    let tree = ast::ClassTree::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let resolved = resolve_declaration_binding_expr(
        &expr,
        &mod_env,
        &IndexMap::default(),
        &tree,
        &class_index,
    )
    .expect("declaration binding resolution should succeed");

    assert_eq!(
        resolved, expr,
        "declaration bindings must keep sibling component references instead of inlining modifier values"
    );
}

#[test]
fn test_resolve_sibling_modification_keeps_function_call_record_like_binding() {
    let mut effective_components: IndexMap<String, ast::Component> = IndexMap::default();
    let mut data = resolved_component(120);
    data.name = "aimcData".to_string();
    data.modifications.insert(
        "statorCoreParameters".to_string(),
        ast::Expression::FunctionCall {
            comp: ast::ComponentReference {
                local: false,
                parts: vec![
                    ast::ComponentRefPart {
                        ident: make_token("Modelica"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Electrical"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Machines"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("Losses"),
                        subs: None,
                        def_id: None,
                    },
                    ast::ComponentRefPart {
                        ident: make_token("CoreParameters"),
                        subs: None,
                        def_id: None,
                    },
                ],
                span: rumoca_core::Span::DUMMY,
                qualified_display_name: None,
            },
            args: vec![make_int_expr(410), make_int_expr(388)],
            is_partial_application: false,
            span: rumoca_core::Span::DUMMY,
        },
    );
    effective_components.insert("aimcData".to_string(), data);

    let expr = make_resolved_comp_ref_expr(&[("aimcData", 120), ("statorCoreParameters", 121)]);
    let tree = ast::ClassTree::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let resolved = resolve_modification_expr(
        &expr,
        ModifierResolveScope {
            mod_env: &ast::ModificationEnvironment::default(),
            effective_components: &effective_components,
            tree: &tree,
            imports: crate::dims::ImportRewrite::without_imports(&class_index),
        },
        false,
    )
    .expect("resolution should succeed");

    assert_eq!(
        resolved, expr,
        "function-call record-like overrides should stay as references"
    );
}

#[test]
fn modifier_resolution_follows_more_than_the_old_depth_limit() {
    let mut mod_env = ast::ModificationEnvironment::default();
    let mut effective_components = IndexMap::default();
    for index in 0..24_u32 {
        let name = format!("p{index}");
        let def_id = 500 + index;
        let mut component = resolved_component(def_id);
        component.name = name.clone();
        effective_components.insert(name.clone(), component);
        let value = if index == 23 {
            ast::Expression::Empty { span: test_span() }
        } else {
            let target = format!("p{}", index + 1);
            make_resolved_comp_ref_expr(&[(target.as_str(), def_id + 1)])
        };
        mod_env.add(
            ast::QualifiedName::from_ident(&name),
            ast::ModificationValue::simple(value),
        );
    }

    let expression = make_resolved_comp_ref_expr(&[("p0", 500)]);
    let tree = ast::ClassTree::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let resolved = resolve_modification_expr(
        &expression,
        ModifierResolveScope {
            mod_env: &mod_env,
            effective_components: &effective_components,
            tree: &tree,
            imports: crate::dims::ImportRewrite::without_imports(&class_index),
        },
        false,
    )
    .expect("long acyclic modifier forwarding chain resolves");
    assert!(matches!(resolved, ast::Expression::Empty { .. }));
}

#[test]
fn modifier_resolution_rejects_cycle_without_mutating_catalogs() {
    let mut mod_env = ast::ModificationEnvironment::default();
    let mut effective_components = IndexMap::default();
    for (name, def_id, target, target_id) in [("a", 600, "b", 601), ("b", 601, "a", 600)] {
        let mut component = resolved_component(def_id);
        component.name = name.to_string();
        effective_components.insert(name.to_string(), component);
        mod_env.add(
            ast::QualifiedName::from_ident(name),
            ast::ModificationValue::simple(make_resolved_comp_ref_expr(&[(target, target_id)])),
        );
    }
    let mod_snapshot = mod_env.clone();
    let component_snapshot = effective_components.clone();
    let expression = make_resolved_comp_ref_expr(&[("a", 600)]);
    let tree = ast::ClassTree::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let error = resolve_modification_expr(
        &expression,
        ModifierResolveScope {
            mod_env: &mod_env,
            effective_components: &effective_components,
            tree: &tree,
            imports: crate::dims::ImportRewrite::without_imports(&class_index),
        },
        false,
    )
    .expect_err("modifier forwarding cycle is invalid");
    assert!(matches!(
        *error,
        InstantiateError::InstantiationCycle { .. }
    ));
    assert_mod_env_matches(&mod_env, &mod_snapshot);
    assert_eq!(effective_components, component_snapshot);
}

#[test]
fn modifier_resolution_rejects_same_spelling_with_different_identity() {
    let mut component = resolved_component(700);
    component.name = "p".to_string();
    let effective_components = [("p".to_string(), component)]
        .into_iter()
        .collect::<IndexMap<_, _>>();
    let expression = make_resolved_comp_ref_expr(&[("p", 701)]);
    let tree = ast::ClassTree::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let result = resolve_modification_expr(
        &expression,
        ModifierResolveScope {
            mod_env: &ast::ModificationEnvironment::default(),
            effective_components: &effective_components,
            tree: &tree,
            imports: crate::dims::ImportRewrite::without_imports(&class_index),
        },
        false,
    );
    assert!(matches!(
        result,
        Err(error) if matches!(*error, InstantiateError::MissingResolvedIdentity { .. })
    ));
}

#[test]
fn modifier_resolution_uses_the_exact_lexical_component_identity() {
    let active_id = rumoca_core::DefId::new(710);
    let lexical_id = rumoca_core::DefId::new(711);
    let owner_id = rumoca_core::DefId::new(712);
    let mut active = resolved_component(active_id.index());
    active.name = "p".to_string();
    let effective_components = [("p".to_string(), active)]
        .into_iter()
        .collect::<IndexMap<_, _>>();

    let mut lexical = resolved_component(lexical_id.index());
    lexical.name = "p".to_string();
    let mut owner = ast::ClassDef {
        name: make_token("Owner"),
        def_id: Some(owner_id),
        ..Default::default()
    };
    owner.components.insert("p".to_string(), lexical);
    let mut tree = ast::ClassTree::default();
    tree.definitions.classes.insert("Owner".to_string(), owner);

    let mut mod_env = ast::ModificationEnvironment::default();
    mod_env.add(
        ast::QualifiedName::from_ident("p"),
        ast::ModificationValue::simple(make_int_expr(5)),
    );
    let expression = make_resolved_comp_ref_expr(&[("p", lexical_id.index())]);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let resolved = resolve_modification_expr(
        &expression,
        ModifierResolveScope {
            mod_env: &mod_env,
            effective_components: &effective_components,
            tree: &tree,
            imports: crate::dims::ImportRewrite::without_imports(&class_index),
        },
        false,
    )
    .expect("lexically resolved modifier reference is valid");
    assert_eq!(resolved, make_int_expr(5));
}

#[test]
fn modifier_resolution_leaves_an_exact_class_root_to_its_owner() {
    let constants_id = rumoca_core::DefId::new(720);
    let value_id = rumoca_core::DefId::new(721);
    let constants = ast::ClassDef {
        name: make_token("Constants"),
        def_id: Some(constants_id),
        ..Default::default()
    };
    let mut tree = ast::ClassTree::default();
    tree.definitions
        .classes
        .insert("Constants".to_string(), constants);
    let expression = make_resolved_comp_ref_expr(&[
        ("Constants", constants_id.index()),
        ("value", value_id.index()),
    ]);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let resolved = resolve_modification_expr(
        &expression,
        ModifierResolveScope {
            mod_env: &ast::ModificationEnvironment::default(),
            effective_components: &IndexMap::default(),
            tree: &tree,
            imports: crate::dims::ImportRewrite::without_imports(&class_index),
        },
        false,
    )
    .expect("an exact class root is not a sibling component modifier");
    assert_eq!(resolved, expression);
}

#[test]
fn population_rolls_back_earlier_modifiers_when_override_evidence_is_malformed() {
    let missing_target_id = rumoca_core::DefId::new(751);
    let tree = crate::test_support::resolved_tree(
        "malformed_override_evidence.mo",
        r"
package BaseMedium end BaseMedium;
model Container
  parameter Integer good = 0;
  replaceable package Medium = BaseMedium constrainedby BaseMedium;
end Container;
model Use
  replaceable package Medium = BaseMedium constrainedby BaseMedium;
  Container c(good = 1, redeclare package Medium = Medium);
end Use;
",
    );
    let target_class = tree
        .get_class_by_qualified_name("Container")
        .expect("container identity");
    let forwarding_alias_id = tree
        .get_class_by_qualified_name("Use")
        .and_then(|class| class.classes.get("Medium"))
        .and_then(|class| class.def_id)
        .expect("forwarding alias identity");
    let component = tree
        .get_class_by_qualified_name("Use")
        .and_then(|class| class.components.get("c"))
        .expect("resolved component redeclare");
    let mut overrides = TypeOverrideMap::new();
    overrides.insert_alias(forwarding_alias_id, missing_target_id);
    let mut ctx = InstantiateContext::new();
    ctx.mod_env_mut().add(
        ast::QualifiedName::from_ident("sentinel"),
        ast::ModificationValue::simple(make_int_expr(9)),
    );
    let snapshot = ctx.mod_env().clone();
    let empty_values = IndexMap::default();
    let empty_keys = IndexMap::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);

    let error = populate_modification_environment(
        &mut ctx,
        &tree,
        PopulateModEnvInput {
            comp: component,
            effective_components: &IndexMap::default(),
            type_overrides: &overrides,
            target_class: Some(target_class),
            parent_snapshot: &empty_values,
            shifted_parent_keys: &empty_keys,
            modifier_imports: crate::dims::ImportRewrite::without_imports(&class_index),
        },
    )
    .expect_err("missing exact override target must abort publication");

    assert!(matches!(
        *error,
        InstantiateError::MissingResolvedIdentity { .. }
    ));
    assert_mod_env_matches(ctx.mod_env(), &snapshot);
}

#[test]
fn test_insert_scoped_modifier_binding_reorders_non_shifted_parent_key() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("k");
    let sibling = ast::QualifiedName::from_ident("a");

    ctx.mod_env_mut().add(
        key.clone(),
        ast::ModificationValue::simple(make_int_expr(1)),
    );
    ctx.mod_env_mut().add(
        sibling.clone(),
        ast::ModificationValue::simple(make_int_expr(2)),
    );

    let mut parent_snapshot = IndexMap::default();
    parent_snapshot.insert(
        key.clone(),
        ast::ModificationValue::simple(make_int_expr(1)),
    );
    parent_snapshot.insert(
        sibling.clone(),
        ast::ModificationValue::simple(make_int_expr(2)),
    );

    insert_scoped_modifier_binding(
        &mut ctx,
        ScopedModifierBinding {
            key: key.clone(),
            value: make_int_expr(9),
            source: None,
            source_scope: None,
            prefixes: ModifierPrefixes::default(),
        },
        &parent_snapshot,
        &IndexMap::default(),
    )
    .expect("non-final parent key can be replaced");

    assert_eq!(
        active_mod_env_keys(&ctx),
        vec!["a".to_string(), "k".to_string()],
        "non-shifted parent key should be replaced as a new local binding"
    );
    assert_eq!(
        ctx.mod_env().get(&key).map(|mv| mv.value.clone()),
        Some(make_int_expr(9))
    );
}

#[test]
fn test_insert_scoped_modifier_binding_keeps_shifted_parent_key_position() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("k");
    let sibling = ast::QualifiedName::from_ident("a");

    ctx.mod_env_mut().add(
        key.clone(),
        ast::ModificationValue::simple(make_int_expr(1)),
    );
    ctx.mod_env_mut().add(
        sibling.clone(),
        ast::ModificationValue::simple(make_int_expr(2)),
    );

    let mut parent_snapshot = IndexMap::default();
    parent_snapshot.insert(
        key.clone(),
        ast::ModificationValue::simple(make_int_expr(1)),
    );
    parent_snapshot.insert(
        sibling.clone(),
        ast::ModificationValue::simple(make_int_expr(2)),
    );

    let mut shifted_parent_keys = IndexMap::default();
    shifted_parent_keys.insert(key.clone(), ());

    insert_scoped_modifier_binding(
        &mut ctx,
        ScopedModifierBinding {
            key: key.clone(),
            value: make_int_expr(11),
            source: None,
            source_scope: None,
            prefixes: ModifierPrefixes::default(),
        },
        &parent_snapshot,
        &shifted_parent_keys,
    )
    .expect("shifted non-final parent key can be preserved");

    assert_eq!(
        active_mod_env_keys(&ctx),
        vec!["k".to_string(), "a".to_string()],
        "shifted parent key should remain in place"
    );
    assert_eq!(
        ctx.mod_env().get(&key).map(|mv| mv.value.clone()),
        Some(make_int_expr(1)),
        "outer/shifted parent modifier must keep precedence (MLS §7.2.4)"
    );
}

#[test]
fn test_insert_scoped_modifier_binding_keeps_shifted_final_parent_key() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("R");
    ctx.mod_env_mut().add(
        key.clone(),
        ast::ModificationValue::with_prefixes(make_int_expr(10), false, true),
    );

    let mut parent_snapshot = IndexMap::default();
    parent_snapshot.insert(
        key.clone(),
        ast::ModificationValue::with_prefixes(make_int_expr(10), false, true),
    );

    let mut shifted_parent_keys = IndexMap::default();
    shifted_parent_keys.insert(key.clone(), ());

    insert_scoped_modifier_binding(
        &mut ctx,
        ScopedModifierBinding {
            key: key.clone(),
            value: make_int_expr(20),
            source: None,
            source_scope: None,
            prefixes: ModifierPrefixes::default(),
        },
        &parent_snapshot,
        &shifted_parent_keys,
    )
    .expect("shifted final outer modifier keeps precedence over inner default");

    assert_eq!(
        ctx.mod_env().get(&key).map(|mv| mv.value.clone()),
        Some(make_int_expr(10))
    );
}

#[test]
fn test_apply_component_modifier_rejects_inherited_final_component() {
    let base_id = rumoca_core::DefId::new(10);
    let mut base = ast::ClassDef {
        def_id: Some(base_id),
        name: make_token("Base"),
        ..Default::default()
    };
    base.components.insert(
        "p".to_string(),
        ast::Component {
            name: "p".to_string(),
            type_name: make_name("Real"),
            is_final: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut derived = ast::ClassDef {
        name: make_token("Derived"),
        ..Default::default()
    };
    derived.extends.push(ast::Extend {
        base_name: make_name("Base"),
        base_def_id: Some(base_id),
        location: test_location(),
        ..Default::default()
    });

    let mut tree = ast::ClassTree::default();
    tree.source_map.add(TEST_FILE, "extends Base;");
    tree.definitions.classes.insert("Base".to_string(), base);
    tree.definitions
        .classes
        .insert("Derived".to_string(), derived.clone());
    tree.def_map.insert(base_id, "Base".to_string());
    tree.name_map.insert("Base".to_string(), base_id);

    let mut ctx = InstantiateContext::new();
    let parent_snapshot = IndexMap::default();
    let shifted_parent_keys = IndexMap::default();
    let type_overrides = TypeOverrideMap::default();
    let comp = ast::Component::empty_with_span(test_span());
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let eval_ctx = ModifierEvalContext {
        tree: &tree,
        comp: &comp,
        effective_components: &IndexMap::default(),
        type_overrides: &type_overrides,
        target_class: Some(&derived),
        insert_ctx: ScopedInsertContext {
            parent_snapshot: &parent_snapshot,
            shifted_parent_keys: &shifted_parent_keys,
            source_scope: None,
            imports: crate::dims::ImportRewrite::without_imports(&class_index),
        },
    };

    let err = apply_component_modifier(
        &mut ctx,
        "p",
        &make_int_expr_with_span(2, test_span()),
        ModifierPrefixes::default(),
        &eval_ctx,
    )
    .expect_err("inherited final components must reject modification");

    let message = err.to_string();
    assert!(message.contains("final"), "{message}");
    assert!(matches!(*err, InstantiateError::RedeclareFinal { .. }));
}

#[test]
fn test_apply_component_modifier_requires_span_for_final_modifier_error() {
    let mut class = ast::ClassDef {
        name: make_token("C"),
        ..Default::default()
    };
    class.components.insert(
        "p".to_string(),
        ast::Component {
            name: "p".to_string(),
            type_name: make_name("Real"),
            is_final: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut ctx = InstantiateContext::new();
    let parent_snapshot = IndexMap::default();
    let shifted_parent_keys = IndexMap::default();
    let type_overrides = TypeOverrideMap::default();
    let tree = ast::ClassTree::default();
    let comp = ast::Component::empty_with_span(test_span());
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let eval_ctx = ModifierEvalContext {
        tree: &tree,
        comp: &comp,
        effective_components: &IndexMap::default(),
        type_overrides: &type_overrides,
        target_class: Some(&class),
        insert_ctx: ScopedInsertContext {
            parent_snapshot: &parent_snapshot,
            shifted_parent_keys: &shifted_parent_keys,
            source_scope: None,
            imports: crate::dims::ImportRewrite::without_imports(&class_index),
        },
    };

    let err = apply_component_modifier(
        &mut ctx,
        "p",
        &make_int_expr(2),
        ModifierPrefixes::default(),
        &eval_ctx,
    )
    .expect_err("unspanned final modifier error should fail fast");

    assert!(matches!(
        *err,
        InstantiateError::MissingSourceContext { .. }
    ));
}

#[test]
fn test_insert_scoped_modifier_binding_reports_final_collision_at_source() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("k");
    ctx.mod_env_mut().add(
        key.clone(),
        ast::ModificationValue::with_prefixes(make_int_expr_with_span(1, test_span()), false, true),
    );

    let err = insert_scoped_modifier_binding(
        &mut ctx,
        ScopedModifierBinding {
            key,
            value: make_int_expr_with_span(2, test_span()),
            source: Some(make_int_expr_with_span(3, test_span())),
            source_scope: None,
            prefixes: ModifierPrefixes::default(),
        },
        &IndexMap::default(),
        &IndexMap::default(),
    )
    .expect_err("local modification must not override final binding");

    assert!(matches!(*err, InstantiateError::RedeclareFinal { .. }));
}

#[test]
fn test_insert_scoped_modifier_binding_requires_span_for_final_collision() {
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("k");
    ctx.mod_env_mut().add(
        key.clone(),
        ast::ModificationValue::with_prefixes(make_int_expr_with_span(1, test_span()), false, true),
    );

    let err = insert_scoped_modifier_binding(
        &mut ctx,
        ScopedModifierBinding {
            key,
            value: make_int_expr(2),
            source: None,
            source_scope: None,
            prefixes: ModifierPrefixes::default(),
        },
        &IndexMap::default(),
        &IndexMap::default(),
    )
    .expect_err("unspanned final binding error should fail fast");

    assert!(matches!(
        *err,
        InstantiateError::MissingSourceContext { .. }
    ));
}

#[test]
fn test_forwarded_modifier_keeps_forwarded_source_scope() {
    let tree = crate::test_support::resolved_tree(
        "forwarded_modifier_scope.mo",
        r"
record FrictionParameters end FrictionParameters;
record AimcData
  FrictionParameters frictionParameters;
end AimcData;
model ModifierScope
  AimcData aimcData;
  FrictionParameters frictionParameters = aimcData.frictionParameters;
  FrictionParameters forwarded = frictionParameters;
end ModifierScope;
",
    );
    let owner = tree
        .get_class_by_qualified_name("ModifierScope")
        .expect("resolved modifier scope");
    let forwarded_value = owner
        .components
        .get("frictionParameters")
        .and_then(|component| component.binding.clone())
        .expect("resolved forwarded parent value");
    let local_reference = owner
        .components
        .get("forwarded")
        .and_then(|component| component.binding.as_ref())
        .expect("resolved local forwarding reference");
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("frictionParameters");
    let forwarded_scope = Some(ast::QualifiedName::new());

    ctx.mod_env_mut().active.insert(
        key.clone(),
        ast::ModificationValue::with_source_scope(
            forwarded_value.clone(),
            Some(forwarded_value.clone()),
            forwarded_scope.clone(),
        ),
    );

    let parent_snapshot = ctx.mod_env().active.clone();
    let shifted_parent_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let insert_ctx = ScopedInsertContext {
        parent_snapshot: &parent_snapshot,
        shifted_parent_keys: &shifted_parent_keys,
        source_scope: Some(ast::QualifiedName::from_ident("aimc")),
        imports: crate::dims::ImportRewrite::without_imports(&class_index),
    };

    insert_modifier_value_with_structural_overrides(
        &mut ctx,
        "frictionParameters",
        local_reference,
        ModifierInsertOptions {
            allow_string_eval: false,
            prefixes: ModifierPrefixes::default(),
        },
        &owner.components,
        &tree,
        &insert_ctx,
    )
    .expect("resolved forwarded modifier insertion should succeed");

    let stored = ctx
        .mod_env()
        .get(&key)
        .expect("forwarded modifier binding should exist");
    assert_eq!(
        stored.value, forwarded_value,
        "forwarded binding should preserve resolved parent expression"
    );
    assert_eq!(
        stored.source_scope, forwarded_scope,
        "forwarded binding should preserve original lexical source scope"
    );
    assert_eq!(
        stored.source.as_ref(),
        Some(&forwarded_value),
        "forwarded binding should preserve symbolic source expression"
    );
}

#[test]
fn test_sibling_modifier_reference_keeps_local_source_scope() {
    let tree = crate::test_support::resolved_tree(
        "sibling_modifier_scope.mo",
        r"
model ModifierScope
  Real length;
  Real pathLengths = length;
  Real pathLengths_internal = pathLengths;
end ModifierScope;
",
    );
    let owner = tree
        .get_class_by_qualified_name("ModifierScope")
        .expect("resolved modifier scope");
    let resolved_length = owner
        .components
        .get("pathLengths")
        .and_then(|component| component.binding.clone())
        .expect("resolved sibling value");
    let sibling_reference = owner
        .components
        .get("pathLengths_internal")
        .and_then(|component| component.binding.as_ref())
        .expect("resolved sibling reference");
    let mut ctx = InstantiateContext::new();
    ctx.mod_env_mut().active.insert(
        ast::QualifiedName::from_ident("pathLengths"),
        ast::ModificationValue::with_source_scope(
            resolved_length.clone(),
            Some(resolved_length.clone()),
            Some(ast::QualifiedName::from_ident("pipe")),
        ),
    );

    let parent_snapshot = ctx.mod_env().active.clone();
    let shifted_parent_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    let local_scope = Some(ast::QualifiedName::from_ident("flowModel"));
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let insert_ctx = ScopedInsertContext {
        parent_snapshot: &parent_snapshot,
        shifted_parent_keys: &shifted_parent_keys,
        source_scope: local_scope.clone(),
        imports: crate::dims::ImportRewrite::without_imports(&class_index),
    };

    insert_modifier_value_with_structural_overrides(
        &mut ctx,
        "pathLengths_internal",
        sibling_reference,
        ModifierInsertOptions {
            allow_string_eval: false,
            prefixes: ModifierPrefixes::default(),
        },
        &owner.components,
        &tree,
        &insert_ctx,
    )
    .expect("resolved sibling modifier insertion should succeed");

    let stored = ctx
        .mod_env()
        .get(&ast::QualifiedName::from_ident("pathLengths_internal"))
        .expect("sibling modifier binding should exist");
    assert_eq!(stored.value, resolved_length);
    assert_eq!(stored.source.as_ref(), Some(sibling_reference));
    assert_eq!(stored.source_scope, local_scope);
}

#[test]
fn test_modifier_with_same_resolved_value_keeps_existing_source_scope() {
    let tree = crate::test_support::resolved_tree(
        "same_value_modifier_scope.mo",
        r"
record FrictionParameters end FrictionParameters;
record AimcData
  FrictionParameters frictionParameters;
end AimcData;
model ModifierScope
  AimcData aimcData;
  FrictionParameters frictionParameters = aimcData.frictionParameters;
end ModifierScope;
",
    );
    let owner = tree
        .get_class_by_qualified_name("ModifierScope")
        .expect("resolved modifier scope");
    let forwarded_value = owner
        .components
        .get("frictionParameters")
        .and_then(|component| component.binding.clone())
        .expect("resolved multi-part modifier value");
    let mut ctx = InstantiateContext::new();
    let key = ast::QualifiedName::from_ident("frictionParameters");
    let forwarded_scope = Some(ast::QualifiedName::new());

    ctx.mod_env_mut().active.insert(
        key.clone(),
        ast::ModificationValue::with_source_scope(
            forwarded_value.clone(),
            Some(forwarded_value.clone()),
            forwarded_scope.clone(),
        ),
    );

    let parent_snapshot = ctx.mod_env().active.clone();
    let shifted_parent_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let insert_ctx = ScopedInsertContext {
        parent_snapshot: &parent_snapshot,
        shifted_parent_keys: &shifted_parent_keys,
        source_scope: Some(ast::QualifiedName::from_ident("aimc")),
        imports: crate::dims::ImportRewrite::without_imports(&class_index),
    };

    insert_modifier_value_with_structural_overrides(
        &mut ctx,
        "frictionParameters",
        &forwarded_value,
        ModifierInsertOptions {
            allow_string_eval: false,
            prefixes: ModifierPrefixes::default(),
        },
        &owner.components,
        &tree,
        &insert_ctx,
    )
    .expect("same resolved value modifier insertion should succeed");

    let stored = ctx
        .mod_env()
        .get(&key)
        .expect("modifier binding should exist");
    assert_eq!(stored.value, forwarded_value);
    assert_eq!(stored.source.as_ref(), Some(&forwarded_value));
    assert_eq!(
        stored.source_scope, forwarded_scope,
        "resolved multi-part modifier should inherit source scope from existing parent binding"
    );
}

#[test]
fn test_propagate_record_binding_overrides_non_targeted_field_values() {
    let mut nested_record = ast::ClassDef {
        name: make_token("State"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(800)),
        ..Default::default()
    };
    nested_record
        .components
        .insert("phase".to_string(), resolved_component(801));
    nested_record
        .components
        .insert("p".to_string(), resolved_component(802));

    let mut ctx = InstantiateContext::new();
    ctx.mod_env_mut().add(
        ast::QualifiedName::from_ident("phase"),
        ast::ModificationValue::simple(make_int_expr(7)),
    );

    let binding_expr = make_resolved_comp_ref_expr(&[("state_in", 8_001)]);
    let targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            source_scope: None,
            each: false,
        },
        &nested_record,
        &targeted_keys,
    )
    .expect("record field projection should succeed");

    let phase_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("phase"))
        .expect("phase field binding should be present");
    match &phase_mod.value {
        ast::Expression::FieldAccess {
            base,
            field,
            field_def_id,
            ..
        } => {
            assert_eq!(field, "phase");
            assert_eq!(*field_def_id, Some(rumoca_core::DefId::new(801)));
            match base.as_ref() {
                ast::Expression::ComponentReference(cref) => {
                    assert_eq!(cref.parts.len(), 1);
                    assert_eq!(cref.parts[0].ident.text.as_ref(), "state_in");
                }
                _ => panic!("field binding should project from record binding expression"),
            }
        }
        _ => panic!("phase field should be rebound from record binding"),
    }
}

#[test]
fn test_record_projection_preserves_written_source_and_exact_field_identity() {
    let mut core_parameters = ast::ClassDef {
        name: make_token("CoreParameters"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(408)),
        ..Default::default()
    };
    core_parameters
        .components
        .insert("PRef".to_string(), resolved_component(414));

    let resolved = make_resolved_comp_ref_expr(&[("aimcData", 305), ("statorCoreParameters", 519)]);
    let written = make_resolved_comp_ref_expr(&[("statorCoreParameters", 519)]);
    let source_scope = ast::QualifiedName::from_ident("aimc");
    let mut ctx = InstantiateContext::new();

    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &resolved,
            source: Some(&written),
            source_scope: Some(source_scope.clone()),
            each: false,
        },
        &core_parameters,
        &IndexMap::default(),
    )
    .expect("record field projection should preserve value and source provenance");

    let projected = ctx
        .mod_env()
        .get(&ast::QualifiedName::from_ident("PRef"))
        .expect("projected field binding should exist");
    assert_eq!(projected.source_scope, Some(source_scope));
    assert_projected_ref(&projected.value, &[305, 519], 414);
    assert_projected_ref(
        projected
            .source
            .as_ref()
            .expect("projected binding should retain its written source"),
        &[519],
        414,
    );
}

fn assert_projected_ref(expr: &ast::Expression, base_def_ids: &[u32], field_def_id: u32) {
    let ast::Expression::FieldAccess {
        base,
        field,
        field_def_id: actual_field_def_id,
        ..
    } = expr
    else {
        panic!("expected projected field access, got {expr:?}");
    };
    let ast::Expression::ComponentReference(comp_ref) = base.as_ref() else {
        panic!("expected component-reference projection base, got {base:?}");
    };
    assert_eq!(field, "PRef");
    assert_eq!(
        *actual_field_def_id,
        Some(rumoca_core::DefId::new(field_def_id))
    );
    assert_eq!(
        comp_ref
            .parts
            .iter()
            .map(|part| part.def_id.expect("base segment must have exact identity"))
            .collect::<Vec<_>>(),
        base_def_ids
            .iter()
            .map(|def_id| rumoca_core::DefId::new(*def_id))
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_propagate_record_binding_preserves_each_prefix_for_fields() {
    let mut nested_record = ast::ClassDef {
        name: make_token("Orientation"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(810)),
        ..Default::default()
    };
    nested_record
        .components
        .insert("T".to_string(), resolved_component(811));

    let mut ctx = InstantiateContext::new();
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &make_resolved_comp_ref_expr(&[("R", 8_002)]),
            source: None,
            source_scope: None,
            each: true,
        },
        &nested_record,
        &IndexMap::default(),
    )
    .expect("record field projection should succeed");

    let field = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("T"))
        .expect("projected field binding should exist");
    assert!(field.each);
}

#[test]
fn test_propagate_record_binding_does_not_treat_start_as_field_default() {
    let mut nested_record = ast::ClassDef {
        name: make_token("Orientation"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(820)),
        ..Default::default()
    };
    nested_record.components.insert(
        "T".to_string(),
        ast::Component {
            def_id: Some(rumoca_core::DefId::new(821)),
            start: make_int_expr(0),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut ctx = InstantiateContext::new();
    let binding_expr = make_resolved_comp_ref_expr(&[("source", 8_003)]);
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            source_scope: None,
            each: false,
        },
        &nested_record,
        &IndexMap::default(),
    )
    .expect("record field projection should succeed");

    let field_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("T"))
        .expect("start-only fields must inherit the record binding");
    let ast::Expression::FieldAccess {
        base,
        field,
        field_def_id,
        ..
    } = &field_mod.value
    else {
        panic!("field binding should project from the record binding");
    };
    assert_eq!(field, "T");
    assert_eq!(*field_def_id, Some(rumoca_core::DefId::new(821)));
    assert_eq!(base.as_ref(), &binding_expr);
}

#[test]
fn test_propagate_record_binding_preserves_targeted_field_modifiers() {
    let mut nested_record = ast::ClassDef {
        name: make_token("State"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(830)),
        ..Default::default()
    };
    nested_record
        .components
        .insert("phase".to_string(), resolved_component(831));

    let mut ctx = InstantiateContext::new();
    let phase_qn = ast::QualifiedName::from_ident("phase");
    ctx.mod_env_mut().add(
        phase_qn.clone(),
        ast::ModificationValue::simple(make_int_expr(42)),
    );

    let mut targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    targeted_keys.insert(phase_qn.clone(), ());
    let binding_expr = make_resolved_comp_ref_expr(&[("state_in", 8_004)]);
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            source_scope: None,
            each: true,
        },
        &nested_record,
        &targeted_keys,
    )
    .expect("record field projection should succeed");

    let phase_mod = ctx
        .mod_env()
        .active
        .get(&phase_qn)
        .expect("targeted phase modifier should still be present");
    match &phase_mod.value {
        ast::Expression::Terminal { token, .. } => assert_eq!(token.text.as_ref(), "42"),
        _ => panic!("targeted field modifier should not be replaced"),
    }
    assert!(
        !phase_mod.each,
        "the winning field modifier must not inherit `each` from the record binding"
    );
}

#[test]
fn test_propagate_record_binding_projects_if_expression_branches_per_field() {
    let mut nested_record = ast::ClassDef {
        name: make_token("CellData"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(840)),
        ..Default::default()
    };
    nested_record
        .components
        .insert("OCV_SOC".to_string(), resolved_component(841));

    let mut ctx = InstantiateContext::new();
    let binding_expr = ast::Expression::If {
        branches: vec![(
            make_resolved_comp_ref_expr(&[("isDegraded", 8_400)]),
            make_resolved_comp_ref_expr(&[("cellDataDegraded", 8_401)]),
        )],
        else_branch: Arc::new(make_resolved_comp_ref_expr(&[("cellDataOriginal", 8_402)])),
        span: rumoca_core::Span::DUMMY,
    };
    let targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            source_scope: None,
            each: false,
        },
        &nested_record,
        &targeted_keys,
    )
    .expect("record field projection should succeed");

    let field_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("OCV_SOC"))
        .expect("OCV_SOC field binding should be present");
    let ast::Expression::If {
        branches,
        else_branch,
        ..
    } = &field_mod.value
    else {
        panic!("field projection should preserve if-expression structure");
    };
    assert_eq!(branches.len(), 1);
    let (_cond, then_expr) = &branches[0];
    let ast::Expression::FieldAccess {
        base,
        field,
        field_def_id,
        ..
    } = then_expr
    else {
        panic!("then-branch should project field access");
    };
    assert_eq!(field, "OCV_SOC");
    assert_eq!(*field_def_id, Some(rumoca_core::DefId::new(841)));
    assert_eq!(
        *base.as_ref(),
        make_resolved_comp_ref_expr(&[("cellDataDegraded", 8_401)])
    );

    let ast::Expression::FieldAccess {
        base: else_base,
        field: else_field,
        ..
    } = else_branch.as_ref()
    else {
        panic!("else-branch should project field access");
    };
    assert_eq!(else_field, "OCV_SOC");
    assert_eq!(
        *else_base.as_ref(),
        make_resolved_comp_ref_expr(&[("cellDataOriginal", 8_402)])
    );
}

#[test]
fn test_record_alias_from_outer_scope_projects_declared_default_field() {
    let mut nested_record = ast::ClassDef {
        name: make_token("CellData"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(1200)),
        ..Default::default()
    };
    nested_record.components.insert(
        "nRC".to_string(),
        ast::Component {
            def_id: Some(rumoca_core::DefId::new(1201)),
            binding: Some(make_int_expr(1)),
            start: make_int_expr(1),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut ctx = InstantiateContext::new();
    let binding_expr = make_resolved_comp_ref_expr(&[("cellDataOriginal", 1_202)]);
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            source_scope: Some(ast::QualifiedName::new()),
            each: false,
        },
        &nested_record,
        &IndexMap::default(),
    )
    .expect("outer record alias should project fields");

    let field_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("nRC"))
        .expect("the outer record's nRC must replace the target default");
    let ast::Expression::FieldAccess {
        base,
        field,
        field_def_id,
        ..
    } = &field_mod.value
    else {
        panic!("outer record field should be projected");
    };
    assert_eq!(field, "nRC");
    assert_eq!(*field_def_id, Some(rumoca_core::DefId::new(1201)));
    assert_eq!(base.as_ref(), &binding_expr);
}

#[test]
fn record_projection_refuses_unresolved_alias_without_partial_field_publication() {
    let mut nested_record = ast::ClassDef {
        name: make_token("State"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(1_210)),
        ..Default::default()
    };
    for (name, def_id) in [("x", 1_211), ("y", 1_212)] {
        nested_record.components.insert(
            name.to_string(),
            ast::Component {
                def_id: Some(rumoca_core::DefId::new(def_id)),
                ..ast::Component::empty_with_span(test_span())
            },
        );
    }
    let mut ctx = InstantiateContext::new();
    ctx.mod_env_mut().add(
        ast::QualifiedName::from_ident("sentinel"),
        ast::ModificationValue::simple(make_int_expr(9)),
    );
    let snapshot = ctx.mod_env().clone();
    let unresolved = make_comp_ref_expr(&["source"]);

    let error = propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &unresolved,
            source: None,
            source_scope: None,
            each: false,
        },
        &nested_record,
        &IndexMap::default(),
    )
    .expect_err("record alias projection requires exact source identity");

    assert!(matches!(
        *error,
        InstantiateError::MissingResolvedIdentity { .. }
    ));
    assert_mod_env_matches(ctx.mod_env(), &snapshot);
}

#[test]
fn record_projection_does_not_read_same_spelled_local_alias_with_other_identity() {
    let record_id = rumoca_core::DefId::new(1_300);
    let mut nested_record = ast::ClassDef {
        name: make_token("State"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(record_id),
        ..Default::default()
    };
    nested_record.components.insert(
        "x".to_string(),
        ast::Component {
            name: "x".to_string(),
            def_id: Some(rumoca_core::DefId::new(1_301)),
            binding: Some(make_int_expr(1)),
            ..ast::Component::empty_with_span(test_span())
        },
    );
    nested_record.components.insert(
        "source".to_string(),
        ast::Component {
            name: "source".to_string(),
            def_id: Some(rumoca_core::DefId::new(1_302)),
            type_def_id: Some(record_id),
            ..ast::Component::empty_with_span(test_span())
        },
    );
    let mut ctx = InstantiateContext::new();
    ctx.mod_env_mut().add(
        ast::QualifiedName::from_ident("source").child("x"),
        ast::ModificationValue::simple(make_int_expr(42)),
    );
    let binding = make_resolved_comp_ref_expr(&[("source", 1_399)]);

    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding,
            source: None,
            source_scope: None,
            each: false,
        },
        &nested_record,
        &IndexMap::default(),
    )
    .expect("outer exact alias projects without consulting the colliding local component");

    let x = ctx
        .mod_env()
        .get(&ast::QualifiedName::from_ident("x"))
        .expect("x projection");
    assert!(matches!(&x.value, ast::Expression::FieldAccess { .. }));
    assert_ne!(x.value, make_int_expr(42));
}

#[test]
fn test_propagate_record_binding_preserves_matching_default_record_constructor() {
    let mut nested_record = ast::ClassDef {
        name: make_token("BaseData"),
        class_type: rumoca_core::ClassType::Record,
        def_id: Some(rumoca_core::DefId::new(850)),
        ..Default::default()
    };
    nested_record.components.insert(
        "mu_i".to_string(),
        ast::Component {
            def_id: Some(rumoca_core::DefId::new(851)),
            binding: Some(make_int_expr(1)),
            start: make_int_expr(1),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut ctx = InstantiateContext::new();
    let binding_expr = ast::Expression::FunctionCall {
        comp: ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: make_token("BaseData"),
                subs: None,
                def_id: Some(rumoca_core::DefId::new(850)),
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        },
        args: Vec::new(),
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    };

    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            source_scope: None,
            each: false,
        },
        &nested_record,
        &IndexMap::default(),
    )
    .expect("record field projection should succeed");

    assert!(
        ctx.mod_env().active.is_empty(),
        "matching zero-argument record constructors should preserve declared defaults"
    );
}

#[test]
fn test_propagate_record_binding_projects_subtype_default_record_constructor_fields() {
    let tree = crate::test_support::resolved_tree(
        "record_subtype_projection.mo",
        r"
record BaseData
  Integer mu_i = 1;
end BaseData;
record M350_50A
  extends BaseData(mu_i = 2);
end M350_50A;
model Holder
  BaseData data = M350_50A();
end Holder;
",
    );
    let nested_record = tree
        .get_class_by_qualified_name("BaseData")
        .expect("resolved base record");
    let binding_expr = tree
        .get_class_by_qualified_name("Holder")
        .and_then(|class| class.components.get("data"))
        .and_then(|component| component.binding.as_ref())
        .expect("resolved record constructor binding");
    let mut ctx = InstantiateContext::new();

    propagate_record_binding_to_fields(
        &tree,
        &mut ctx,
        RecordBindingProjection {
            value: binding_expr,
            source: None,
            source_scope: None,
            each: false,
        },
        nested_record,
        &IndexMap::default(),
    )
    .expect("record field projection should succeed");

    let field_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("mu_i"))
        .expect("subtype default record constructor should project field binding");
    let ast::Expression::Terminal {
        terminal_type,
        token,
        ..
    } = &field_mod.value
    else {
        panic!("subtype constructor should publish its effective field default");
    };
    assert_eq!(*terminal_type, ast::TerminalType::UnsignedInteger);
    assert_eq!(token.text.as_ref(), "2");
    assert_eq!(field_mod.source.as_ref(), Some(&field_mod.value));
}

#[test]
fn test_propagate_record_binding_projects_through_unique_constructor_record_field() {
    let tree = crate::test_support::resolved_tree(
        "record_field_projection.mo",
        r"
package Pkg
  record Inner
    Real x;
  end Inner;
  record Outer
    Inner innerParams;
  end Outer;
end Pkg;
model Holder
  Pkg.Inner value = Pkg.Outer();
end Holder;
",
    );
    let inner_record = tree
        .get_class_by_qualified_name("Pkg.Inner")
        .expect("resolved inner record");
    let binding_expr = tree
        .get_class_by_qualified_name("Holder")
        .and_then(|class| class.components.get("value"))
        .and_then(|component| component.binding.as_ref())
        .expect("resolved outer constructor binding");
    let mut ctx = InstantiateContext::new();

    propagate_record_binding_to_fields(
        &tree,
        &mut ctx,
        RecordBindingProjection {
            value: binding_expr,
            source: None,
            source_scope: Some(ast::QualifiedName::from_ident("Pkg")),
            each: false,
        },
        inner_record,
        &IndexMap::default(),
    )
    .expect("record field projection should succeed");

    let field_mod = ctx
        .mod_env()
        .active
        .get(&ast::QualifiedName::from_ident("x"))
        .expect("inner field binding should be present");
    let ast::Expression::FieldAccess {
        base,
        field: inner_field,
        ..
    } = &field_mod.value
    else {
        panic!("inner field should be projected");
    };
    assert_eq!(inner_field, "x");
    let ast::Expression::FieldAccess {
        base: constructor,
        field: outer_field,
        ..
    } = base.as_ref()
    else {
        panic!("projection should first select the unique compatible record field");
    };
    assert_eq!(outer_field, "innerParams");
    assert_eq!(constructor.as_ref(), binding_expr);
}

#[test]
fn test_propagate_record_binding_skips_non_record_classes() {
    let mut nested_block = ast::ClassDef {
        name: make_token("UniformNoise"),
        class_type: rumoca_core::ClassType::Block,
        ..Default::default()
    };
    nested_block.components.insert(
        "y".to_string(),
        ast::Component::empty_with_span(test_span()),
    );
    nested_block.components.insert(
        "seedState".to_string(),
        ast::Component::empty_with_span(test_span()),
    );

    let mut ctx = InstantiateContext::new();
    let binding_expr = make_comp_ref_expr(&["noise"]);
    let targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::default();
    propagate_record_binding_to_fields(
        &ast::ClassTree::default(),
        &mut ctx,
        RecordBindingProjection {
            value: &binding_expr,
            source: None,
            source_scope: None,
            each: false,
        },
        &nested_block,
        &targeted_keys,
    )
    .expect("non-record projection should succeed without mutation");

    assert!(
        ctx.mod_env().active.is_empty(),
        "non-record class modifiers must not synthesize per-field record bindings"
    );
}
