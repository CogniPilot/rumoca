use super::*;
use rumoca_core::{BytePos, Span};

fn span() -> Span {
    Span::new(
        rumoca_core::SourceId::from_source_name("component_family_tests"),
        BytePos(0),
        BytePos(4),
    )
}

fn qualified(parts: &[(&str, &[i64])]) -> ast::QualifiedName {
    ast::QualifiedName {
        parts: parts
            .iter()
            .map(|(name, subs)| ((*name).to_string(), subs.to_vec()))
            .collect(),
    }
}

const NO_ANCESTORS: &[String] = &[];

fn reindex<'a>(segment: &'a str, depth: usize, tuple: &'a [i64]) -> FamilyReindex<'a> {
    FamilyReindex {
        ancestors: NO_ANCESTORS,
        segment,
        depth,
        template_tuple: &[1],
        tuple,
    }
}

/// A family nested one level down, e.g. `bank[1].pin[k]`.
fn nested_reindex<'a>(
    ancestors: &'a [String],
    segment: &'a str,
    tuple: &'a [i64],
) -> FamilyReindex<'a> {
    FamilyReindex {
        ancestors,
        segment,
        depth: ancestors.len(),
        template_tuple: &[1],
        tuple,
    }
}

#[test]
fn reindex_rewrites_only_the_family_segment() {
    let ancestors = vec!["plug_p".to_string()];
    let name = qualified(&[("plug_p", &[]), ("pin", &[1]), ("v", &[])]);
    let rewritten = nested_reindex(&ancestors, "pin", &[3]).qualified_name(&name);
    assert_eq!(rewritten.to_flat_string(), "plug_p.pin[3].v");
}

#[test]
fn reindex_leaves_lexical_scopes_untouched() {
    // A lexical class scope carries no subscripts, so it must never be
    // rewritten even when a segment name collides with the family segment.
    let ancestors = vec!["Package".to_string()];
    let scope = qualified(&[("Package", &[]), ("pin", &[])]);
    let rewritten = nested_reindex(&ancestors, "pin", &[3]).qualified_name(&scope);
    assert_eq!(rewritten.to_flat_string(), "Package.pin");
}

#[test]
fn reindex_leaves_sibling_instances_untouched() {
    // A family rooted at `right.pin` must not touch `left.pin[1]`, which is a
    // different instance that merely reuses the segment name. Without the
    // ancestor-prefix check `flat_path`'s "not a member" contract is false and
    // the sibling is silently rewritten.
    let ancestors = vec!["right".to_string()];
    let mapper = nested_reindex(&ancestors, "pin", &[2]);

    let sibling = qualified(&[("left", &[]), ("pin", &[1]), ("v", &[])]);
    assert_eq!(
        mapper.qualified_name(&sibling).to_flat_string(),
        "left.pin[1].v"
    );
    let member = qualified(&[("right", &[]), ("pin", &[1]), ("v", &[])]);
    assert_eq!(
        mapper.qualified_name(&member).to_flat_string(),
        "right.pin[2].v"
    );

    assert_eq!(mapper.flat_path("left.pin[1].v"), None);
    assert_eq!(
        mapper.flat_path("right.pin[1].v").as_deref(),
        Some("right.pin[2].v")
    );
    assert_eq!(mapper.flat_path_or_same("left.pin[1].v"), "left.pin[1].v");

    let sibling_path = rumoca_core::ComponentPath::from_flat_path("left.pin[1].v");
    assert_eq!(
        mapper.component_path(&sibling_path).to_flat_string(),
        "left.pin[1].v"
    );

    let sibling_reference = ast::instance::component_reference_for_instance(
        &sibling,
        span().require_provenance("test").expect("span"),
        None,
    );
    assert_eq!(
        rumoca_core::ComponentPath::from_component_reference(
            &mapper.component_reference(&sibling_reference)
        )
        .to_flat_string(),
        "left.pin[1].v"
    );

    let sibling_endpoint = ast::InstanceConnectionEndpoint {
        parts: vec![
            ("left".to_string(), Vec::new()),
            ("pin".to_string(), vec![AffineForm::constant(1, 1)]),
        ],
    };
    assert_eq!(
        mapper.connection_endpoint(&sibling_endpoint),
        sibling_endpoint
    );
}

#[test]
fn reindex_leaves_other_domain_points_untouched() {
    let name = qualified(&[("c", &[2]), ("v", &[])]);
    let rewritten = reindex("c", 0, &[3]).qualified_name(&name);
    assert_eq!(rewritten.to_flat_string(), "c[2].v");
}

#[test]
fn reindex_rewrites_component_reference_and_preserves_spans() {
    let name = qualified(&[("c", &[1]), ("v", &[])]);
    let reference = ast::instance::component_reference_for_instance(
        &name,
        span().require_provenance("test").expect("span"),
        None,
    );
    let rewritten = reindex("c", 0, &[4]).component_reference(&reference);
    let rumoca_core::Subscript::Index {
        value,
        span: sub_span,
    } = &rewritten.parts[0].subs[0]
    else {
        panic!("expected an index subscript");
    };
    assert_eq!(*value, 4);
    assert_eq!(*sub_span, span());
}

#[test]
fn reindex_rewrites_rendered_flat_paths() {
    let mapper = reindex("c", 0, &[2]);
    assert_eq!(mapper.flat_path("c[1].r").as_deref(), Some("c[2].r"));
    assert_eq!(mapper.flat_path("c[1]").as_deref(), Some("c[2]"));
    assert_eq!(mapper.flat_path("other.c[1]"), None);
    assert_eq!(mapper.flat_path(""), None);
    assert_eq!(mapper.flat_path_or_same("outer.R"), "outer.R");
}

#[test]
fn reindex_rewrites_component_paths() {
    let path = rumoca_core::ComponentPath::from_flat_path("c[1].r[2].R");
    let rewritten = reindex("c", 0, &[5]).component_path(&path);
    assert_eq!(rewritten.to_flat_string(), "c[5].r[2].R");
}

#[test]
fn reindex_rewrites_constant_connection_endpoint_subscripts() {
    let endpoint = ast::InstanceConnectionEndpoint {
        parts: vec![
            ("c".to_string(), vec![AffineForm::constant(1, 1)]),
            ("pin".to_string(), vec![AffineForm::unit_binder(0, 1)]),
        ],
    };
    let rewritten = reindex("c", 0, &[7]).connection_endpoint(&endpoint);
    assert_eq!(rewritten.parts[0].1[0], AffineForm::constant(7, 1));
    assert_eq!(rewritten.parts[1].1[0], AffineForm::unit_binder(0, 1));
}

#[test]
fn family_member_component_reindexes_instance_paths() {
    let mut template = ast::InstanceData {
        qualified_name: qualified(&[("c", &[1]), ("R", &[])]),
        ..Default::default()
    };
    template.component_ref = Some(ast::instance::component_reference_for_instance(
        &template.qualified_name,
        span().require_provenance("test").expect("span"),
        None,
    ));
    template.declaration_source_scope = Some(qualified(&[("Package", &[]), ("Cell", &[])]));
    template.binding_source_scope = Some(qualified(&[("c", &[1])]));
    template
        .attribute_source_scopes
        .insert("start".to_string(), qualified(&[("c", &[1])]));
    template.oc_record_path = Some("c[1].frame".to_string());

    let member = family_member_component(&template, ast::InstanceId(9), &reindex("c", 0, &[2]));
    assert_eq!(member.instance_id, ast::InstanceId(9));
    assert_eq!(member.qualified_name.to_flat_string(), "c[2].R");
    assert_eq!(
        member
            .component_ref
            .as_ref()
            .map(rumoca_core::ComponentPath::from_component_reference)
            .map(|path| path.to_flat_string()),
        Some("c[2].R".to_string())
    );
    // Lexical declaration scopes never carry subscripts and stay put.
    assert_eq!(
        member.declaration_source_scope.map(|s| s.to_flat_string()),
        Some("Package.Cell".to_string())
    );
    assert_eq!(
        member.binding_source_scope.map(|s| s.to_flat_string()),
        Some("c[2]".to_string())
    );
    assert_eq!(
        member.attribute_source_scopes["start"].to_flat_string(),
        "c[2]"
    );
    assert_eq!(member.oc_record_path.as_deref(), Some("c[2].frame"));
}

#[test]
fn family_member_class_reindexes_origins_and_connections() {
    let template = ast::ClassInstanceData {
        instance_id: ast::InstanceId(1),
        qualified_name: qualified(&[("c", &[1])]),
        source_scope: Some(qualified(&[("Package", &[]), ("Cell", &[])])),
        equations: vec![ast::InstanceEquation {
            equation: ast::Equation::Empty,
            origin: qualified(&[("c", &[1])]),
            source_scope: None,
            source_scope_id: None,
            span: span(),
        }],
        connections: vec![ast::InstanceConnection {
            a: qualified(&[("c", &[1]), ("p", &[])]),
            b: qualified(&[("c", &[1]), ("n", &[])]),
            connector_type: None,
            span: span(),
            scope: "c[1]".to_string(),
            family: None,
        }],
        ..Default::default()
    };

    let member = family_member_class(&template, ast::InstanceId(11), &reindex("c", 0, &[3]));
    assert_eq!(member.qualified_name.to_flat_string(), "c[3]");
    assert!(member.connections[0].family.is_none());
    assert_eq!(member.equations[0].origin.to_flat_string(), "c[3]");
    assert_eq!(member.connections[0].a.to_flat_string(), "c[3].p");
    assert_eq!(member.connections[0].b.to_flat_string(), "c[3].n");
    assert_eq!(member.connections[0].scope, "c[3]");
    // Lexical source scope is unaffected by instance reindexing.
    assert_eq!(
        member.source_scope.map(|s| s.to_flat_string()),
        Some("Package.Cell".to_string())
    );
}

#[test]
fn family_member_class_reindexes_compact_connection_family_endpoints() {
    // A `for k in 1:2 loop connect(pins[k], b[k].p)` inside a replicated array
    // element is stored compactly, so `reindex_connection` must re-root both
    // family endpoints at the derived domain point while leaving the binder
    // subscripts of the inner arrays symbolic. Only the scalar `a`/`b` fields
    // are visible in a flattened model, so a regression here is silent.
    let template = ast::ClassInstanceData {
        instance_id: ast::InstanceId(1),
        qualified_name: qualified(&[("bank", &[1])]),
        connections: vec![ast::InstanceConnection {
            a: qualified(&[("bank", &[1]), ("pins", &[1])]),
            b: qualified(&[("bank", &[1]), ("b", &[1]), ("p", &[])]),
            connector_type: None,
            span: span(),
            scope: "bank[1]".to_string(),
            family: Some(ast::InstanceConnectionFamily {
                domain: rumoca_core::StructuredIndexDomain {
                    binders: vec![rumoca_core::StructuredIndexBinder {
                        id: 0,
                        display_name: "k".to_string(),
                        lower: 1,
                        upper: 2,
                        step: 1,
                    }],
                },
                a: ast::InstanceConnectionEndpoint {
                    parts: vec![
                        ("bank".to_string(), vec![AffineForm::constant(1, 1)]),
                        ("pins".to_string(), vec![AffineForm::unit_binder(0, 1)]),
                    ],
                },
                b: ast::InstanceConnectionEndpoint {
                    parts: vec![
                        ("bank".to_string(), vec![AffineForm::constant(1, 1)]),
                        ("b".to_string(), vec![AffineForm::unit_binder(0, 1)]),
                        ("p".to_string(), Vec::new()),
                    ],
                },
            }),
        }],
        ..Default::default()
    };

    let member = family_member_class(&template, ast::InstanceId(12), &reindex("bank", 0, &[4]));
    let family = member.connections[0]
        .family
        .as_ref()
        .expect("the compact family must survive replication");
    assert_eq!(family.a.parts[0].1[0], AffineForm::constant(4, 1));
    assert_eq!(family.b.parts[0].1[0], AffineForm::constant(4, 1));
    // Binder-carrying subscripts index the *inner* arrays and stay symbolic.
    assert_eq!(family.a.parts[1].1[0], AffineForm::unit_binder(0, 1));
    assert_eq!(family.b.parts[1].1[0], AffineForm::unit_binder(0, 1));
    assert!(family.b.parts[2].1.is_empty());
    // The domain itself is a property of the source `for`, not of the element.
    assert_eq!(family.domain.binders[0].upper, 2);
    // The scalar diagnostic endpoints move with the family.
    assert_eq!(member.connections[0].a.to_flat_string(), "bank[4].pins[1]");
    assert_eq!(member.connections[0].scope, "bank[4]");
}
