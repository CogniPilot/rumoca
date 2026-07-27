//! Regression coverage for the borrowing connection-family view (SPEC_0032 §1).
//!
//! Flatten used to deep-clone the whole instance overlay just to materialize
//! the scalar members of compact connection families. These tests pin the
//! observable contract that replaced it: a class carrying a compact family must
//! flatten to exactly the equations produced by the equivalent list of scalar
//! connections, in the same order.

use super::*;
use rumoca_core::{AffineForm, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_ast as ast;

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("connection_family_view_tests.mo"),
        3,
        9,
    )
}

fn connector_model(count: i64) -> flat::Model {
    let mut model = flat::Model::new();
    for side in ["a", "b"] {
        for index in 1..=count {
            for (member, is_flow) in [("v", false), ("i", true)] {
                let name = rumoca_core::VarName::new(format!("{side}[{index}].p.{member}"));
                model.add_variable(
                    name.clone(),
                    flat::Variable {
                        name: name.clone(),
                        flow: is_flow,
                        ..flat::Variable::empty_with_span(test_span())
                    },
                );
            }
        }
    }
    model
}

fn endpoint(side: &str) -> ast::InstanceConnectionEndpoint {
    ast::InstanceConnectionEndpoint {
        parts: vec![
            (side.to_string(), vec![AffineForm::unit_binder(0, 1)]),
            ("p".to_string(), Vec::new()),
        ],
    }
}

fn scalar_connection(index: i64) -> ast::InstanceConnection {
    let mut a = ast::QualifiedName::new();
    a.push("a".to_string(), vec![index]);
    a.push("p".to_string(), Vec::new());
    let mut b = ast::QualifiedName::new();
    b.push("b".to_string(), vec![index]);
    b.push("p".to_string(), Vec::new());
    ast::InstanceConnection {
        a,
        b,
        connector_type: None,
        span: test_span(),
        scope: String::new(),
        family: None,
    }
}

fn family_connection(count: i64) -> ast::InstanceConnection {
    ast::InstanceConnection {
        // MLS diagnostics keep the first scalar member spelled out.
        family: Some(ast::InstanceConnectionFamily {
            domain: StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: count,
                    step: 1,
                }],
            },
            a: endpoint("a"),
            b: endpoint("b"),
        }),
        ..scalar_connection(1)
    }
}

fn overlay_with(connections: Vec<ast::InstanceConnection>) -> ast::InstanceOverlay {
    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_class(ast::ClassInstanceData {
        instance_id: ast::InstanceId::new(0),
        qualified_name: ast::QualifiedName::new(),
        connections,
        ..Default::default()
    });
    overlay
}

fn generated_equations(overlay: &ast::InstanceOverlay, count: i64) -> Vec<String> {
    let mut flat = connector_model(count);
    let mut forest = crate::vcg::OverconstrainedEquationForest::new(&Default::default(), &[], &[]);
    equation_generation::process_connections(&mut flat, overlay, false, &mut forest)
        .expect("connection processing should succeed");
    flat.equations
        .iter()
        .map(|equation| format!("{equation:?}"))
        .collect()
}

#[test]
fn range_subscript_connect_family_flattens_identically() {
    let compact = overlay_with(vec![family_connection(3)]);
    let scalar = overlay_with((1..=3).map(scalar_connection).collect());
    let compact_equations = generated_equations(&compact, 3);
    assert!(!compact_equations.is_empty());
    assert_eq!(compact_equations, generated_equations(&scalar, 3));
}

#[test]
fn for_loop_connect_family_flattens_without_overlay_materialization() {
    // A four-point domain plus a trailing family-free connection: the view must
    // preserve both the intra-family ordinal order and the surrounding order.
    let mut compact_connections = vec![family_connection(4)];
    let mut trailing = scalar_connection(1);
    trailing.a.parts[0].1 = vec![1];
    trailing.b.parts[0].1 = vec![2];
    compact_connections.push(trailing.clone());

    let mut scalar_connections: Vec<ast::InstanceConnection> =
        (1..=4).map(scalar_connection).collect();
    scalar_connections.push(trailing);

    let compact = overlay_with(compact_connections);
    let scalar = overlay_with(scalar_connections);
    let compact_equations = generated_equations(&compact, 4);
    assert!(!compact_equations.is_empty());
    assert_eq!(compact_equations, generated_equations(&scalar, 4));
}

#[test]
fn cardinality_counts_see_every_family_member() {
    let overlay = overlay_with(vec![family_connection(3)]);
    let mut ctx = crate::Context::new();
    crate::function_precollect::compute_cardinality_counts(&mut ctx, &overlay)
        .expect("cardinality counts should succeed");
    for index in 1..=3 {
        for side in ["a", "b"] {
            let path = format!("{side}[{index}].p");
            assert_eq!(
                ctx.cardinality_counts.get(&path),
                Some(&1),
                "expected one cardinality hit for {path}"
            );
        }
    }
}
