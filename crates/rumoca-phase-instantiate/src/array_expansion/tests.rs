//! Unit tests for array component expansion.

use super::{
    ArrayExpansionScope, array_element_binding_modification,
    distribute_component_ref_mods_for_element, distribute_mods_for_element,
    index_array_expression_for_element, index_binding_for_element, pre_resolve_array_modifications,
    project_array_selection_for_element, resolve_mod_to_array,
};
use crate::type_overrides::TypeOverrideMap;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use std::sync::Arc;

mod homogeneity_tests;
mod homogeneous_family_tests;
mod vector_subscript_tests;

fn make_token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: Arc::from(text),
        location: rumoca_core::Location::default(),
        token_number: 0,
        token_type: 0,
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("array_expansion_test.mo"),
        1,
        2,
    )
}

fn make_int_expr(value: i64) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: make_token(&value.to_string()),
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_component_ref(names: &[&str]) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: names
            .iter()
            .map(|name| ast::ComponentRefPart {
                ident: make_token(name),
                subs: None,
            })
            .collect(),
        def_id: None,
        target_def_id: None,
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_comp_ref_expr(names: &[&str]) -> ast::Expression {
    ast::Expression::ComponentReference(make_component_ref(names))
}

fn make_indexed_comp_ref_expr(name: &str, index_name: &str) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: make_token(name),
            subs: Some(vec![ast::Subscript::Expression(make_comp_ref_expr(&[
                index_name,
            ]))]),
        }],
        def_id: None,
        target_def_id: None,
        span: rumoca_core::Span::DUMMY,
    })
}

fn make_function_call(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: make_token(name),
                subs: None,
            }],
            def_id: None,
            target_def_id: None,
            span: rumoca_core::Span::DUMMY,
        },
        args,
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_qualified_function_call(names: &[&str], args: Vec<ast::Expression>) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: ast::ComponentReference {
            local: false,
            parts: names
                .iter()
                .map(|name| ast::ComponentRefPart {
                    ident: make_token(name),
                    subs: None,
                })
                .collect(),
            def_id: None,
            target_def_id: None,
            span: rumoca_core::Span::DUMMY,
        },
        args,
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn test_array_element_binding_preserves_modifier_source_scope() {
    let tree = ast::ClassTree::new();
    let effective_components = IndexMap::default();
    let type_overrides = TypeOverrideMap::new();
    let imports = Vec::new();
    let scope = ArrayExpansionScope {
        tree: &tree,
        effective_components: &effective_components,
        type_overrides: &type_overrides,
        imports: crate::ComponentImports {
            qualification: &imports,
            attributes: &[],
        },
    };
    let source = make_comp_ref_expr(&["outer", "x"]);
    let parent_mod = ast::ModificationValue::with_source_scope(
        source.clone(),
        Some(source),
        Some(ast::QualifiedName::from_dotted("outerScope")),
    );

    let value = array_element_binding_modification(
        &scope,
        &make_comp_ref_expr(&["resolved", "x"]),
        &[2],
        Some(&parent_mod),
        Some(ast::QualifiedName::from_dotted("declarationScope")),
    )
    .expect("array element binding modification should succeed");

    assert_eq!(
        value.source_scope.map(|scope| scope.to_flat_string()),
        Some("outerScope".to_string())
    );
    let ast::Expression::ArrayIndex { base, .. } =
        value.source.expect("indexed source should be preserved")
    else {
        panic!("expected indexed source");
    };
    let ast::Expression::ComponentReference(source_ref) = base.as_ref() else {
        panic!("expected component reference base");
    };
    assert_eq!(source_ref.parts[0].ident.text.as_ref(), "outer");
}

#[test]
fn test_array_element_binding_preserves_declaration_source_scope() {
    let tree = ast::ClassTree::new();
    let effective_components = IndexMap::default();
    let type_overrides = TypeOverrideMap::new();
    let imports = Vec::new();
    let scope = ArrayExpansionScope {
        tree: &tree,
        effective_components: &effective_components,
        type_overrides: &type_overrides,
        imports: crate::ComponentImports {
            qualification: &imports,
            attributes: &[],
        },
    };
    let binding = make_comp_ref_expr(&["plug", "pin"]);

    let value = array_element_binding_modification(
        &scope,
        &binding,
        &[1],
        None,
        Some(ast::QualifiedName::from_dotted("Model.Source")),
    )
    .expect("array element binding modification should succeed");

    assert_eq!(
        value.source_scope.map(|scope| scope.to_flat_string()),
        Some("Model.Source".to_string())
    );
    assert_eq!(value.source, Some(binding));
}

fn real_lit_value(expr: &ast::Expression) -> f64 {
    let ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedReal,
        token,
        ..
    } = expr
    else {
        panic!("expected real literal");
    };
    token.text.parse().expect("real literal should parse")
}

fn make_range_expr(start: i64, end: i64) -> ast::Expression {
    ast::Expression::Range {
        start: Arc::new(make_int_expr(start)),
        step: None,
        end: Arc::new(make_int_expr(end)),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn test_resolve_mod_to_array_symmetric_orientation_with_unary_minus() {
    let call = make_qualified_function_call(
        &["Polyphase", "Functions", "symmetricOrientation"],
        vec![make_int_expr(3)],
    );
    let expr = ast::Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: Arc::new(call),
        span: rumoca_core::Span::DUMMY,
    };

    let resolved = resolve_mod_to_array(
        &expr,
        &rumoca_ir_ast::ModificationEnvironment::default(),
        &IndexMap::default(),
        &ast::ClassTree::default(),
    );

    let ast::Expression::Array { elements, .. } = resolved else {
        panic!("symmetricOrientation() should resolve to an array");
    };
    assert_eq!(elements.len(), 3);
    let values = elements.iter().map(real_lit_value).collect::<Vec<_>>();
    assert!(values[0].abs() <= 1e-14);
    assert!((values[1] + 2.0 * std::f64::consts::PI / 3.0).abs() <= 1e-14);
    assert!((values[2] + 4.0 * std::f64::consts::PI / 3.0).abs() <= 1e-14);
}

#[test]
fn test_resolve_mod_to_array_fill_constructor() {
    let expr = make_function_call("fill", vec![make_int_expr(7), make_int_expr(3)]);
    let resolved = resolve_mod_to_array(
        &expr,
        &rumoca_ir_ast::ModificationEnvironment::default(),
        &IndexMap::default(),
        &ast::ClassTree::default(),
    );

    let ast::Expression::Array { elements, .. } = resolved else {
        panic!("fill() should resolve to an array for modifier distribution");
    };
    assert_eq!(elements.len(), 3);
    for e in elements {
        match e {
            ast::Expression::Terminal { token, .. } => assert_eq!(token.text.as_ref(), "7"),
            _ => panic!("fill() element should be a scalar expression"),
        }
    }
}

#[test]
fn test_index_binding_for_element_indexes_proven_array_part() {
    let mut parent_components = IndexMap::default();
    let array_comp = ast::Component {
        name: "arr".to_string(),
        shape: vec![3],
        ..ast::Component::empty_with_span(test_span())
    };
    parent_components.insert("arr".to_string(), array_comp);

    let binding = make_comp_ref_expr(&["arr", "v"]);
    let indexed = index_binding_for_element(
        &ast::ClassTree::default(),
        &parent_components,
        &binding,
        &[2],
    )
    .expect("array element binding should index proven array part");

    let ast::Expression::ComponentReference(cref) = indexed else {
        panic!("expected indexed component reference");
    };
    assert_eq!(cref.parts.len(), 2);
    assert_eq!(cref.parts[0].ident.text.as_ref(), "arr");
    let Some(subs) = &cref.parts[0].subs else {
        panic!("array part should be subscripted");
    };
    assert_eq!(subs.len(), 1);
    let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subs[0] else {
        panic!("expected integer subscript expression");
    };
    assert_eq!(token.text.as_ref(), "2");
    assert!(
        cref.parts[1].subs.is_none(),
        "field part must remain unindexed"
    );
}

#[test]
fn test_index_binding_for_element_no_array_part_uses_array_index_fallback() {
    let binding = make_comp_ref_expr(&["a", "b", "c"]);
    let indexed = index_binding_for_element(
        &ast::ClassTree::default(),
        &IndexMap::default(),
        &binding,
        &[1],
    )
    .expect("array element binding should preserve unproven reference as ArrayIndex");

    let ast::Expression::ArrayIndex {
        base, subscripts, ..
    } = indexed
    else {
        panic!("unproven array part should use ArrayIndex fallback");
    };
    assert_eq!(subscripts.len(), 1);
    let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subscripts[0] else {
        panic!("expected integer subscript expression");
    };
    assert_eq!(token.text.as_ref(), "1");
    assert_eq!(*base, binding);
}

#[test]
fn test_index_binding_for_element_projects_multidim_array_comprehension() {
    let binding = ast::Expression::ArrayComprehension {
        expr: Arc::new(make_comp_ref_expr(&["ks"])),
        indices: vec![
            ast::ForIndex {
                ident: make_token("ks"),
                range: make_range_expr(1, 3),
            },
            ast::ForIndex {
                ident: make_token("kp"),
                range: make_range_expr(1, 2),
            },
        ],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };

    let indexed = index_binding_for_element(
        &ast::ClassTree::default(),
        &IndexMap::default(),
        &binding,
        &[2, 1],
    )
    .expect("array comprehension projection should succeed");
    let ast::Expression::Terminal { token, .. } = indexed else {
        panic!("multi-index comprehension should project to a concrete element expression");
    };
    assert_eq!(token.text.as_ref(), "2");
}

#[test]
fn test_index_binding_for_element_substitutes_comprehension_index_in_subscripts() {
    let binding = ast::Expression::ArrayComprehension {
        expr: Arc::new(ast::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Arc::new(make_comp_ref_expr(&["level"])),
            rhs: Arc::new(make_indexed_comp_ref_expr("top_heights", "i")),
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![ast::ForIndex {
            ident: make_token("i"),
            range: make_range_expr(1, 3),
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };

    let indexed = index_binding_for_element(
        &ast::ClassTree::default(),
        &IndexMap::default(),
        &binding,
        &[2],
    )
    .expect("array comprehension projection should succeed");

    let ast::Expression::Binary { rhs, .. } = indexed else {
        panic!("array comprehension should project to expression body");
    };
    let ast::Expression::ComponentReference(cref) = rhs.as_ref() else {
        panic!("expected indexed component reference");
    };
    let Some(subscripts) = cref.parts[0].subs.as_ref() else {
        panic!("expected subscript");
    };
    let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subscripts[0] else {
        panic!("expected literal subscript");
    };
    assert_eq!(token.text.as_ref(), "2");
}

#[test]
fn test_index_binding_for_element_substitutes_comprehension_index_in_class_modification() {
    let binding = ast::Expression::ArrayComprehension {
        expr: Arc::new(ast::Expression::ClassModification {
            target: make_component_ref(&["SalientPermeance"]),
            modifications: vec![ast::Expression::Modification {
                target: make_component_ref(&["d"]),
                value: Arc::new(make_indexed_comp_ref_expr("effectiveTurns", "k")),
                span: rumoca_core::Span::DUMMY,
            }],
            each_flags: vec![false],
            final_flags: vec![false],
            redeclare_flags: vec![false],
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![ast::ForIndex {
            ident: make_token("k"),
            range: make_range_expr(1, 3),
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };

    let indexed = index_binding_for_element(
        &ast::ClassTree::default(),
        &IndexMap::default(),
        &binding,
        &[2],
    )
    .expect("class modification comprehension projection should succeed");

    let ast::Expression::ClassModification { modifications, .. } = indexed else {
        panic!("array comprehension should project to class modification body");
    };
    let ast::Expression::Modification { value, .. } = &modifications[0] else {
        panic!("expected field modification");
    };
    let ast::Expression::ComponentReference(cref) = value.as_ref() else {
        panic!("expected indexed component reference");
    };
    let Some(subscripts) = cref.parts[0].subs.as_ref() else {
        panic!("expected substituted subscript");
    };
    let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subscripts[0] else {
        panic!("expected literal subscript");
    };
    assert_eq!(token.text.as_ref(), "2");
}

#[test]
fn test_resolve_mod_to_array_substitutes_comprehension_index_in_class_modification() {
    let modifier = ast::Expression::ArrayComprehension {
        expr: Arc::new(ast::Expression::ClassModification {
            target: make_component_ref(&["SalientPermeance"]),
            modifications: vec![ast::Expression::Modification {
                target: make_component_ref(&["d"]),
                value: Arc::new(make_indexed_comp_ref_expr("effectiveTurns", "k")),
                span: rumoca_core::Span::DUMMY,
            }],
            each_flags: vec![false],
            final_flags: vec![false],
            redeclare_flags: vec![false],
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![ast::ForIndex {
            ident: make_token("k"),
            range: make_range_expr(1, 3),
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };

    let resolved = resolve_mod_to_array(
        &modifier,
        &rumoca_ir_ast::ModificationEnvironment::default(),
        &IndexMap::default(),
        &ast::ClassTree::default(),
    );
    let ast::Expression::Array { elements, .. } = resolved else {
        panic!("class modification comprehension should resolve to an array");
    };
    let ast::Expression::ClassModification { modifications, .. } = &elements[1] else {
        panic!("second element should remain a class modification");
    };
    let ast::Expression::Modification { value, .. } = &modifications[0] else {
        panic!("expected field modification");
    };
    let ast::Expression::ComponentReference(cref) = value.as_ref() else {
        panic!("expected indexed component reference");
    };
    let Some(subscripts) = cref.parts[0].subs.as_ref() else {
        panic!("expected substituted subscript");
    };
    let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subscripts[0] else {
        panic!("expected literal subscript");
    };
    assert_eq!(token.text.as_ref(), "2");
}

#[test]
fn test_index_array_start_projects_vectorized_binary_comprehension() {
    let start = ast::Expression::Binary {
        op: rumoca_core::OpBinary::Div,
        lhs: Arc::new(ast::Expression::ArrayComprehension {
            expr: Arc::new(make_indexed_comp_ref_expr("m_flows", "i")),
            indices: vec![ast::ForIndex {
                ident: make_token("i"),
                range: make_range_expr(1, 3),
            }],
            filter: None,
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Arc::new(make_comp_ref_expr(&["nParallel"])),
        span: rumoca_core::Span::DUMMY,
    };

    let indexed = index_array_expression_for_element(
        &ast::ClassTree::default(),
        &IndexMap::default(),
        &start,
        &[2],
    )
    .expect("array-valued start projection should not fail")
    .expect("array-valued start should project to one scalar element");

    let ast::Expression::Binary { lhs, rhs, .. } = indexed else {
        panic!("expected vectorized binary expression to stay binary");
    };
    let ast::Expression::ComponentReference(cref) = lhs.as_ref() else {
        panic!("expected projected lhs component reference");
    };
    let Some(subscripts) = cref.parts[0].subs.as_ref() else {
        panic!("expected projected subscript");
    };
    let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subscripts[0] else {
        panic!("expected literal projected subscript");
    };
    assert_eq!(token.text.as_ref(), "2");
    let ast::Expression::ComponentReference(rhs_ref) = rhs.as_ref() else {
        panic!("scalar rhs should remain unchanged");
    };
    assert_eq!(rhs_ref.parts[0].ident.text.as_ref(), "nParallel");
}

#[test]
fn test_index_binding_for_element_projects_nested_array_comprehensions() {
    let inner = ast::Expression::ArrayComprehension {
        expr: Arc::new(make_comp_ref_expr(&["ks"])),
        indices: vec![ast::ForIndex {
            ident: make_token("kp"),
            range: make_range_expr(1, 2),
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };
    let binding = ast::Expression::ArrayComprehension {
        expr: Arc::new(inner),
        indices: vec![ast::ForIndex {
            ident: make_token("ks"),
            range: make_range_expr(1, 3),
        }],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };

    let indexed = index_binding_for_element(
        &ast::ClassTree::default(),
        &IndexMap::default(),
        &binding,
        &[2, 1],
    )
    .expect("nested array comprehension projection should succeed");
    let ast::Expression::Terminal { token, .. } = indexed else {
        panic!("nested comprehensions should project to a concrete element expression");
    };
    assert_eq!(token.text.as_ref(), "2");
}

#[test]
fn test_index_binding_for_element_indexes_nested_array_part_via_type_walk() {
    let stack_data_id = DefId::new(100);
    let mut tree = ast::ClassTree::default();

    let mut stack_data = ast::ClassDef {
        name: make_token("StackData"),
        def_id: Some(stack_data_id),
        ..Default::default()
    };
    stack_data.components.insert(
        "cellData".to_string(),
        ast::Component {
            name: "cellData".to_string(),
            shape: vec![3, 2],
            ..ast::Component::empty_with_span(test_span())
        },
    );
    tree.definitions
        .classes
        .insert("StackData".to_string(), stack_data);
    tree.def_map.insert(stack_data_id, "StackData".to_string());
    tree.name_map.insert("StackData".to_string(), stack_data_id);

    let mut parent_components = IndexMap::default();
    parent_components.insert(
        "stackData".to_string(),
        ast::Component {
            name: "stackData".to_string(),
            type_name: ast::Name {
                name: vec![make_token("StackData")],
                def_id: Some(stack_data_id),
            },
            type_def_id: Some(stack_data_id),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let binding = make_comp_ref_expr(&["stackData", "cellData"]);
    let indexed = index_binding_for_element(&tree, &parent_components, &binding, &[2, 1])
        .expect("nested array part indexing should succeed");

    let ast::Expression::ComponentReference(cref) = indexed else {
        panic!("expected indexed nested component reference");
    };
    assert_eq!(cref.parts.len(), 2);
    assert!(
        cref.parts[0].subs.is_none(),
        "root record part must remain unindexed"
    );
    let Some(subs) = &cref.parts[1].subs else {
        panic!("nested array field should be indexed");
    };
    assert_eq!(subs.len(), 2);
}

#[test]
fn test_distribute_mods_for_element_fill_modifier() {
    let mut comp = ast::Component::empty_with_span(test_span());
    comp.modifications.insert(
        "k".to_string(),
        make_function_call("fill", vec![make_int_expr(5), make_int_expr(2)]),
    );

    let resolved_mods = pre_resolve_array_modifications(
        &comp,
        &rumoca_ir_ast::ModificationEnvironment::default(),
        &IndexMap::default(),
        &ast::ClassTree::default(),
    );
    assert_eq!(
        resolved_mods.len(),
        1,
        "fill() modifier should be resolved for non-`each` distribution"
    );

    let mut scalar_comp = comp.clone();
    distribute_mods_for_element(&mut scalar_comp, &resolved_mods, &[1]);
    let first = scalar_comp.modifications.get("k").expect("missing k mod");
    match first {
        ast::Expression::Terminal { token, .. } => assert_eq!(token.text.as_ref(), "5"),
        _ => panic!("distributed modifier should be scalar"),
    }

    distribute_mods_for_element(&mut scalar_comp, &resolved_mods, &[2]);
    let second = scalar_comp.modifications.get("k").expect("missing k mod");
    match second {
        ast::Expression::Terminal { token, .. } => assert_eq!(token.text.as_ref(), "5"),
        _ => panic!("distributed modifier should be scalar"),
    }
}

#[test]
fn test_distribute_component_ref_mods_for_element_indexes_proven_array_reference() {
    let mut comp = ast::Component::empty_with_span(test_span());
    comp.modifications
        .insert("cellData".to_string(), make_comp_ref_expr(&["arr", "v"]));

    let mut parent_components = IndexMap::default();
    parent_components.insert(
        "arr".to_string(),
        ast::Component {
            name: "arr".to_string(),
            shape: vec![3],
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut scalar_comp = comp.clone();
    let resolved_mod_names = std::collections::HashSet::new();
    distribute_component_ref_mods_for_element(
        &mut scalar_comp,
        &comp,
        &resolved_mod_names,
        &ast::ClassTree::default(),
        &parent_components,
        &[2],
    )
    .expect("component-reference modifier distribution should succeed");

    let ast::Expression::ComponentReference(cref) = scalar_comp
        .modifications
        .get("cellData")
        .expect("missing distributed component reference")
    else {
        panic!("component-reference modifier should be indexed");
    };
    assert_eq!(cref.parts.len(), 2);
    let Some(subs) = &cref.parts[0].subs else {
        panic!("array-introducing part should be indexed");
    };
    let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subs[0] else {
        panic!("expected integer index");
    };
    assert_eq!(token.text.as_ref(), "2");
}

#[test]
fn test_component_ref_modifier_composes_shifted_and_strided_range_selection() {
    let mut parent_components = IndexMap::default();
    parent_components.insert(
        "source".to_string(),
        ast::Component {
            name: "source".to_string(),
            shape: vec![8, 2],
            ..ast::Component::empty_with_span(test_span())
        },
    );
    let binding = ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: make_token("source"),
            subs: Some(vec![
                ast::Subscript::Expression(ast::Expression::Range {
                    start: Arc::new(make_int_expr(2)),
                    step: Some(Arc::new(make_int_expr(2))),
                    end: Arc::new(make_int_expr(8)),
                    span: test_span(),
                }),
                ast::Subscript::Range {
                    token: make_token(":"),
                },
            ]),
        }],
            def_id: None,
            target_def_id: None,
        span: test_span(),
    });

    let projected = index_binding_for_element(
        &ast::ClassTree::default(),
        &parent_components,
        &binding,
        &[3],
    )
    .expect("range selection should be projected");
    let ast::Expression::ComponentReference(reference) = projected else {
        panic!("expected component reference");
    };
    let subscripts = reference.parts[0]
        .subs
        .as_ref()
        .expect("projected subscripts");
    let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = &subscripts[0] else {
        panic!("expected scalarized range index");
    };
    assert_eq!(token.text.as_ref(), "6");
    assert!(matches!(subscripts[1], ast::Subscript::Range { .. }));
}
