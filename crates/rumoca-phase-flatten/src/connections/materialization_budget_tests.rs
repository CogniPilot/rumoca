use super::*;
use rumoca_core::{AffineForm, StructuredIndexBinder, StructuredIndexDomain};

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("connection_materialization_budget.mo"),
        4,
        12,
    )
}

fn endpoint(side: &str) -> ast::InstanceConnectionEndpoint {
    ast::InstanceConnectionEndpoint::new(vec![(
        side.to_string(),
        vec![AffineForm::unit_binder(0, 1)],
    )])
    .expect("test endpoint is valid")
}

fn oversized_overlay() -> ast::InstanceOverlay {
    let count = i64::try_from(crate::equations::MAX_EAGER_RANGE_ELEMENTS)
        .expect("the phase budget fits i64")
        + 1;
    let connection = ast::InstanceConnection::family(
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
        span(),
        String::new(),
    )
    .expect("test family is valid");
    let mut overlay = ast::InstanceOverlay::new();
    let class_instance_id = overlay.alloc_id();
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: class_instance_id,
            qualified_name: ast::QualifiedName::new(),
            connections: vec![connection],
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    overlay
}

#[test]
fn oversized_compact_family_fails_before_flat_or_forest_mutation() {
    let overlay = oversized_overlay();
    let mut flat = connection_test_model();
    let marker = rumoca_core::VarName::new("marker");
    flat.add_test_variable(
        marker.clone(),
        flat::Variable {
            name: marker.clone(),
            ..connection_test_variable(span())
        },
    );
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();
    let forest_before = forest.state_snapshot();

    finalize_connection_test_flat(&mut flat);
    let overconstrained = overlay
        .finalized_overconstrained()
        .expect("connection budget fixture must construct finalized occurrence proofs");
    let error =
        equation_generation::process_connections_for_test(&mut flat, &overconstrained, &mut forest)
            .expect_err("an eager scalar consumer must refuse an oversized compact family");

    assert!(matches!(
        error,
        FlattenError::RangeMaterializationLimit {
            limit: crate::equations::MAX_EAGER_RANGE_ELEMENTS,
            span: error_span,
            ..
        } if error_span == span()
    ));
    assert!(flat.equations.is_empty());
    assert!(flat.structured_equations.is_empty());
    assert!(flat.variables[&marker].connected.is_unconnected());
    assert_eq!(forest.state_snapshot(), forest_before);
}

#[test]
fn oversized_family_under_a_false_conditional_is_pruned_before_the_budget() {
    let mut overlay = oversized_overlay();
    overlay
        .disabled_components
        .insert(rumoca_core::ComponentPath::from_flat_path("a"));
    let mut flat = connection_test_model();
    let marker = rumoca_core::VarName::new("marker");
    flat.add_test_variable(
        marker.clone(),
        flat::Variable {
            name: marker.clone(),
            ..connection_test_variable(span())
        },
    );
    finalize_connection_test_flat(&mut flat);
    let flat_before = connection_mutation_snapshot(&flat);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();
    let forest_before = forest.state_snapshot();

    let overconstrained = overlay
        .finalized_overconstrained()
        .expect("connection budget fixture must construct finalized occurrence proofs");
    equation_generation::process_connections_for_test(&mut flat, &overconstrained, &mut forest)
        .expect("a false conditional connection family is absent before budget evidence is needed");

    assert_eq!(connection_mutation_snapshot(&flat), flat_before);
    assert!(flat.variables[&marker].connected.is_unconnected());
    assert_eq!(forest.state_snapshot(), forest_before);
}

#[test]
fn cardinality_prescan_obeys_the_same_family_budget_atomically() {
    let overlay = oversized_overlay();
    let mut ctx = crate::Context::new();

    let error = crate::function_precollect::compute_cardinality_counts(&mut ctx, &overlay)
        .expect_err("cardinality prescan must not enumerate an oversized family");

    assert!(matches!(
        error,
        FlattenError::RangeMaterializationLimit { .. }
    ));
    assert!(ctx.cardinality_counts.is_empty());
}
