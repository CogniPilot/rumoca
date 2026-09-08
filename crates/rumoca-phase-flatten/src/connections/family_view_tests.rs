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
    let mut model = connection_test_model();
    for side in ["a", "b"] {
        for index in 1..=count {
            for (member, is_flow) in [("v", false), ("i", true)] {
                let name = rumoca_core::VarName::new(format!("{side}[{index}].p.{member}"));
                model.add_variable(
                    name.clone(),
                    flat::Variable {
                        name: name.clone(),
                        flow: is_flow,
                        ..connection_test_variable(test_span())
                    },
                );
            }
        }
    }
    model
}

fn endpoint(side: &str) -> ast::InstanceConnectionEndpoint {
    ast::InstanceConnectionEndpoint::new(vec![
        (side.to_string(), vec![AffineForm::unit_binder(0, 1)]),
        ("p".to_string(), Vec::new()),
    ])
    .expect("test endpoint is valid")
}

fn scalar_connection(index: i64) -> ast::InstanceConnection {
    let mut a = ast::QualifiedName::new();
    a.push("a".to_string(), vec![index]);
    a.push("p".to_string(), Vec::new());
    let mut b = ast::QualifiedName::new();
    b.push("b".to_string(), vec![index]);
    b.push("p".to_string(), Vec::new());
    ast::InstanceConnection::scalar(a, b, None, test_span(), String::new())
        .expect("test scalar connection is valid")
}

fn family_connection(count: i64) -> ast::InstanceConnection {
    ast::InstanceConnection::family(
        StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: count,
                step: 1,
            }],
        },
        endpoint("a"),
        endpoint("b"),
        None,
        test_span(),
        String::new(),
    )
    .expect("test family is valid")
}

fn overlay_with(connections: Vec<ast::InstanceConnection>) -> ast::InstanceOverlay {
    let mut overlay = ast::InstanceOverlay::new();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::new(),
            connections,
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    overlay
}

fn generated_equations(connections: Vec<ast::InstanceConnection>, count: i64) -> Vec<String> {
    let mut flat = connector_model(count);
    let effective_type = rumoca_core::TypeId(0x54_0001);
    flat.type_roots.insert(effective_type, effective_type);
    flat.effective_types.insert(
        effective_type,
        rumoca_core::EffectiveType::new(effective_type, effective_type, Vec::<i64>::new())
            .expect("fixture type is concrete"),
    );
    let mut overlay = ast::InstanceOverlay::new();
    overlay.type_roots.insert(effective_type, effective_type);
    let names = flat.variables.keys().cloned().collect::<Vec<_>>();
    for name in names {
        let overlay_id = overlay.alloc_id();
        let flat_id = flat.materialize_instance(flat::InstanceRelation {
            owner: None,
            declaration: None,
            indices: Box::new([]),
            kind: flat::InstanceKind::Materialized,
        });
        assert_eq!(
            overlay_id, flat_id,
            "fixture occurrence identities must agree"
        );
        let variable = flat
            .variables
            .get_mut(&name)
            .expect("fixture variable remains materialized");
        variable.instance_id = flat_id;
        variable.type_id = effective_type;
        overlay
            .add_component(ast::InstanceData {
                instance_id: overlay_id,
                qualified_name: ast::QualifiedName::from_dotted(name.as_str()),
                type_id: effective_type,
                is_primitive: true,
                ..Default::default()
            })
            .expect("fixture component identity is allocated");
    }
    flat.finalize_effective_type_shapes()
        .expect("fixture Flat shapes are finalized");
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::new(),
            connections,
            ..Default::default()
        })
        .expect("fixture class identity is allocated");
    let overconstrained = crate::test_support::finalized_test_overlay(&mut overlay);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();
    equation_generation::process_connections_for_test(&mut flat, &overconstrained, &mut forest)
        .expect("connection processing should succeed");
    flat.equations
        .iter()
        .map(|equation| format!("{equation:?}"))
        .collect()
}

#[test]
fn range_subscript_connect_family_flattens_identically() {
    let compact_equations = generated_equations(vec![family_connection(3)], 3);
    assert!(!compact_equations.is_empty());
    assert_eq!(
        compact_equations,
        generated_equations((1..=3).map(scalar_connection).collect(), 3)
    );
}

#[test]
fn for_loop_connect_family_flattens_without_overlay_materialization() {
    // A four-point domain plus a trailing family-free connection: the view must
    // preserve both the intra-family ordinal order and the surrounding order.
    let mut compact_connections = vec![family_connection(4)];
    let mut trailing_a = ast::QualifiedName::new();
    trailing_a.push("a".to_string(), vec![1]);
    trailing_a.push("p".to_string(), Vec::new());
    let mut trailing_b = ast::QualifiedName::new();
    trailing_b.push("b".to_string(), vec![2]);
    trailing_b.push("p".to_string(), Vec::new());
    let trailing =
        ast::InstanceConnection::scalar(trailing_a, trailing_b, None, test_span(), String::new())
            .expect("test scalar connection is valid");
    compact_connections.push(trailing.clone());

    let mut scalar_connections: Vec<ast::InstanceConnection> =
        (1..=4).map(scalar_connection).collect();
    scalar_connections.push(trailing);

    let compact_equations = generated_equations(compact_connections, 4);
    assert!(!compact_equations.is_empty());
    assert_eq!(
        compact_equations,
        generated_equations(scalar_connections, 4)
    );
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
