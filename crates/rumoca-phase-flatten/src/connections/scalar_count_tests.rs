use super::*;
use indexmap::IndexSet;

const CONNECTION_FIXTURE_TYPE: rumoca_core::TypeId = rumoca_core::TypeId(0x55_0001);

pub(super) fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("connection_scalar_count_tests.mo"),
        1,
        2,
    )
}

pub(super) fn test_location() -> rumoca_core::Location {
    rumoca_core::Location {
        start_line: 1,
        start_column: 1,
        end_line: 1,
        end_column: 2,
        start: 1,
        end: 2,
        source: rumoca_core::SourceId::from_source_name("connection_scalar_count_tests.mo"),
    }
}

pub(super) fn array_variable(dims: Vec<i64>, flow: bool) -> flat::Variable {
    flat::Variable {
        dims,
        flow,
        is_primitive: true,
        ..connection_test_variable(test_span())
    }
}

pub(super) fn add_materialized_variable(
    flat: &mut flat::Model,
    name: rumoca_core::VarName,
    mut variable: flat::Variable,
) {
    variable.name = name.clone();
    variable.instance_id = flat.materialize_instance(flat::InstanceRelation {
        owner: None,
        declaration: None,
        indices: Box::new([]),
        kind: flat::InstanceKind::Materialized,
    });
    flat.add_variable(name, variable);
}

pub(super) fn paired_overlay_root(
    flat: &mut flat::Model,
) -> (ast::InstanceOverlay, rumoca_core::InstanceId) {
    let mut overlay = ast::InstanceOverlay::new();
    overlay
        .type_roots
        .insert(CONNECTION_FIXTURE_TYPE, CONNECTION_FIXTURE_TYPE);
    let overlay_root = overlay.alloc_id();
    let flat_root = flat.materialize_instance(flat::InstanceRelation {
        owner: None,
        declaration: None,
        indices: Box::new([]),
        kind: flat::InstanceKind::Materialized,
    });
    assert_eq!(overlay_root, flat_root, "fixture root identity must agree");
    (overlay, overlay_root)
}

pub(super) fn add_paired_component(
    flat: &mut flat::Model,
    overlay: &mut ast::InstanceOverlay,
    owner: rumoca_core::InstanceId,
    name: &str,
    dims: Vec<i64>,
) {
    let overlay_id = overlay.alloc_id();
    let flat_id = flat.materialize_instance(flat::InstanceRelation {
        owner: Some(owner),
        declaration: None,
        indices: Box::new([]),
        kind: flat::InstanceKind::Materialized,
    });
    assert_eq!(overlay_id, flat_id, "fixture component identity must agree");
    let variable = flat
        .variables
        .get_mut(&rumoca_core::VarName::new(name))
        .expect("paired fixture Flat declaration exists");
    variable.name = rumoca_core::VarName::new(name);
    variable.instance_id = flat_id;
    overlay
        .add_component(ast::InstanceData {
            instance_id: overlay_id,
            owner_class_id: Some(owner),
            qualified_name: ast::QualifiedName::from_dotted(name),
            declaration_source_scope: Some(ast::QualifiedName::from_ident("Root")),
            source_location: test_location(),
            type_id: CONNECTION_FIXTURE_TYPE,
            dims,
            is_primitive: true,
            ..Default::default()
        })
        .expect("paired fixture component identity is allocated");
}

pub(super) fn generate_equality_equations(
    flat: &mut flat::Model,
    variables: &[rumoca_core::VarName],
    span: rumoca_core::Span,
    forest: &mut crate::vcg::OverconstrainedEquationForest,
) -> Result<(), FlattenError> {
    finalize_connection_test_flat(flat);
    let mut overlay = ast::InstanceOverlay::new();
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let overconstrained = overlay
        .finalized_overconstrained()
        .expect("ordinary equality fixture must construct finalized occurrence proofs");
    equation_generation::generate_equality_equations(
        flat,
        &overconstrained,
        variables,
        span,
        forest,
    )
}

pub(super) fn generate_flow_equation(
    flat: &mut flat::Model,
    variables: &[rumoca_core::VarName],
    scope: &str,
    interface_flows: &IndexMap<String, IndexSet<rumoca_core::VarName>>,
    span: rumoca_core::Span,
) -> Result<(), FlattenError> {
    finalize_connection_test_flat(flat);
    equation_generation::generate_flow_equation(flat, variables, scope, interface_flows, span)
}

pub(super) fn generate_outside_stream_equations(
    flat: &mut flat::Model,
    endpoints: &InterfaceStreamEndpointsByScope,
    stream_endpoints: &super::stream_operators::StreamConnectionEndpoints,
) -> Result<(), FlattenError> {
    finalize_connection_test_flat(flat);
    equation_generation::generate_outside_stream_equations(flat, endpoints, stream_endpoints)
}

pub(super) fn generate_unconnected_flow_equations(
    flat: &mut flat::Model,
) -> Result<(), FlattenError> {
    finalize_connection_test_flat(flat);
    equation_generation::generate_unconnected_flow_equations(flat)
}

#[test]
fn equality_generation_rejects_mismatched_array_sizes() {
    let mut flat = connection_test_model();
    let lhs = rumoca_core::VarName::new("a.v");
    let rhs = rumoca_core::VarName::new("b.v");
    flat.add_variable(lhs.clone(), array_variable(vec![2], false));
    flat.add_variable(rhs.clone(), array_variable(vec![3], false));
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = generate_equality_equations(&mut flat, &[lhs, rhs], test_span(), &mut forest)
        .expect_err("mismatched potential arrays must not be truncated");

    assert!(error.to_string().contains("incompatible connector types"));
    assert!(flat.equations.is_empty());
}

/// MLS §10.5 defines the selected value: `a.v[1]` of `a.v[2, 3]` denotes three
/// scalars, and only those three elements become connected. The second row of
/// each declaration stays unconnected.
#[test]
fn equality_generation_marks_only_the_selected_rows_of_a_compact_array() {
    let mut flat = connection_test_model();
    let lhs_base = rumoca_core::VarName::new("a.v");
    let rhs_base = rumoca_core::VarName::new("b.v");
    flat.add_variable(lhs_base.clone(), array_variable(vec![2, 3], false));
    flat.add_variable(rhs_base.clone(), array_variable(vec![2, 3], false));
    let (mut overlay, root) = paired_overlay_root(&mut flat);
    add_paired_component(&mut flat, &mut overlay, root, "a.v", vec![2, 3]);
    add_paired_component(&mut flat, &mut overlay, root, "b.v", vec![2, 3]);
    let catalog = crate::test_support::finalized_test_overlay(&mut overlay);
    finalize_connection_test_flat(&mut flat);
    let lhs = rumoca_core::VarName::new("a.v[1]");
    let rhs = rumoca_core::VarName::new("b.v[1]");
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    equation_generation::generate_equality_equations(
        &mut flat,
        &catalog,
        &[lhs, rhs],
        test_span(),
        &mut forest,
    )
    .expect("a leading selection has a checked connected-domain owner");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 3);
    for base in [&lhs_base, &rhs_base] {
        let domain = &flat.variables[base].connected;
        assert_eq!(
            domain.coverage(&[2, 3]),
            Ok(flat::ConnectedCoverage::Partial)
        );
        assert!(domain.covers(&[1]));
        assert!(domain.covers(&[1, 3]));
        assert!(!domain.covers(&[2]));
        assert_eq!(
            domain.unconnected_coordinates(&[2, 3]),
            Ok(vec![vec![2, 1], vec![2, 2], vec![2, 3]])
        );
    }
}

#[test]
fn equality_generation_accepts_an_exact_full_domain_selection() {
    let mut flat = connection_test_model();
    flat.add_variable(
        rumoca_core::VarName::new("a.v"),
        array_variable(vec![1, 3], false),
    );
    flat.add_variable(
        rumoca_core::VarName::new("b.v"),
        array_variable(vec![1, 3], false),
    );
    let (mut overlay, root) = paired_overlay_root(&mut flat);
    add_paired_component(&mut flat, &mut overlay, root, "a.v", vec![1, 3]);
    add_paired_component(&mut flat, &mut overlay, root, "b.v", vec![1, 3]);
    let catalog = crate::test_support::finalized_test_overlay(&mut overlay);
    finalize_connection_test_flat(&mut flat);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    equation_generation::generate_equality_equations(
        &mut flat,
        &catalog,
        &[
            rumoca_core::VarName::new("a.v[1]"),
            rumoca_core::VarName::new("b.v[1]"),
        ],
        test_span(),
        &mut forest,
    )
    .expect("selecting the sole leading element denotes the complete compact declaration");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 3);
    assert!(
        flat.variables
            .values()
            .all(|variable| !variable.connected.is_unconnected())
    );
}

#[test]
fn equal_cardinality_does_not_launder_incompatible_array_shapes() {
    for flow in [false, true] {
        let mut flat = connection_test_model();
        let lhs = rumoca_core::VarName::new("a.v");
        let rhs = rumoca_core::VarName::new("b.v");
        flat.add_variable(lhs.clone(), array_variable(vec![2, 3], flow));
        flat.add_variable(rhs.clone(), array_variable(vec![3, 2], flow));

        let error = if flow {
            generate_flow_equation(
                &mut flat,
                &[lhs.clone(), rhs.clone()],
                "",
                &IndexMap::default(),
                test_span(),
            )
        } else {
            generate_equality_equations(
                &mut flat,
                &[lhs.clone(), rhs.clone()],
                test_span(),
                &mut crate::vcg::OverconstrainedEquationForest::empty(),
            )
        }
        .expect_err("connector arrays require identical dimensions, not equal products");

        assert!(matches!(error, FlattenError::IncompatibleConnectors { .. }));
        assert!(flat.equations.is_empty());
        assert!(flat.structured_equations.is_empty());
        assert!(flat.variables[&lhs].connected.is_unconnected());
        assert!(flat.variables[&rhs].connected.is_unconnected());
    }
}

/// The fully subscripted element denotes a scalar, so the count stays 1 and
/// exactly that element of each declaration becomes connected.
#[test]
fn equality_generation_marks_exactly_one_fully_subscripted_compact_element() {
    let mut flat = connection_test_model();
    let lhs_base = rumoca_core::VarName::new("a.v");
    let rhs_base = rumoca_core::VarName::new("b.v");
    flat.add_variable(lhs_base.clone(), array_variable(vec![2, 3], false));
    flat.add_variable(rhs_base.clone(), array_variable(vec![2, 3], false));
    let (mut overlay, root) = paired_overlay_root(&mut flat);
    add_paired_component(&mut flat, &mut overlay, root, "a.v", vec![2, 3]);
    add_paired_component(&mut flat, &mut overlay, root, "b.v", vec![2, 3]);
    let catalog = crate::test_support::finalized_test_overlay(&mut overlay);
    finalize_connection_test_flat(&mut flat);
    let lhs = rumoca_core::VarName::new("a.v[1,2]");
    let rhs = rumoca_core::VarName::new("b.v[2,3]");
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    equation_generation::generate_equality_equations(
        &mut flat,
        &catalog,
        &[lhs, rhs],
        test_span(),
        &mut forest,
    )
    .expect("a scalar element selection has a checked connected-domain owner");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 1);
    assert!(flat.structured_equations.is_empty());
    assert_eq!(
        flat.variables[&lhs_base]
            .connected
            .selections()
            .collect::<Vec<_>>(),
        vec![&[1, 2][..]]
    );
    assert_eq!(
        flat.variables[&rhs_base]
            .connected
            .selections()
            .collect::<Vec<_>>(),
        vec![&[2, 3][..]]
    );
}

/// CONN-008 (MLS §9.2) still rejects endpoints whose denoted shapes differ, and
/// with the same typed error as the whole-declaration mismatch above.
#[test]
fn equality_generation_rejects_array_elements_with_different_leaf_counts() {
    let mut flat = connection_test_model();
    let lhs_base = rumoca_core::VarName::new("a.v");
    let rhs_base = rumoca_core::VarName::new("b.v");
    flat.add_variable(lhs_base, array_variable(vec![2, 3], false));
    flat.add_variable(rhs_base, array_variable(vec![2, 2], false));
    let lhs = rumoca_core::VarName::new("a.v[1]");
    let rhs = rumoca_core::VarName::new("b.v[1]");
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = generate_equality_equations(&mut flat, &[lhs, rhs], test_span(), &mut forest)
        .expect_err("Real[3] and Real[2] slices are not connection compatible");

    assert!(matches!(error, FlattenError::IncompatibleConnectors { .. }));
    assert!(flat.equations.is_empty());
}

/// The flow sum counts the same leaves: `a.i[1]` of `a.i[2, 3]` and `b.i[3]`
/// sum pointwise over three scalars. Only the selected row of `a.i` joins the
/// set, so the MLS §9.2 zero-flow planner of a later transaction still owes the
/// second row its three `= 0` rows and owes `b.i` nothing.
#[test]
fn partial_compact_flow_array_selection_zeroes_exactly_the_untouched_row() {
    let mut flat = connection_test_model();
    let lhs_base = rumoca_core::VarName::new("a.i");
    add_materialized_variable(
        &mut flat,
        lhs_base.clone(),
        array_variable(vec![2, 3], true),
    );
    let rhs = rumoca_core::VarName::new("b.i");
    add_materialized_variable(&mut flat, rhs.clone(), array_variable(vec![3], true));
    let lhs = rumoca_core::VarName::new("a.i[1]");

    generate_flow_equation(
        &mut flat,
        &[lhs, rhs.clone()],
        "",
        &IndexMap::default(),
        test_span(),
    )
    .expect("a leading flow selection has a checked connected-domain owner");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 3);
    assert_eq!(
        flat.variables[&lhs_base].connected.coverage(&[2, 3]),
        Ok(flat::ConnectedCoverage::Partial)
    );
    assert_eq!(
        flat.variables[&rhs].connected.coverage(&[3]),
        Ok(flat::ConnectedCoverage::Whole)
    );

    generate_unconnected_flow_equations(&mut flat)
        .expect("the committed domain decides the zero-flow rows");

    let zero_rows: Vec<String> = flat.equations[1..]
        .iter()
        .map(|equation| match &equation.origin {
            flat::EquationOrigin::UnconnectedFlow { variable } => {
                assert_eq!(equation.scalar_count, 1);
                variable.clone()
            }
            other => panic!("only zero-flow rows may follow the flow sum, got {other:?}"),
        })
        .collect();
    assert_eq!(zero_rows, ["a.i[2,1]", "a.i[2,2]", "a.i[2,3]"]);
}

#[test]
fn process_connections_rolls_back_earlier_sets_when_a_later_set_is_refused() {
    let mut flat = connection_test_model();
    for name in ["p", "q"] {
        let var_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name,
                is_primitive: true,
                ..connection_test_variable(test_span())
            },
        );
    }
    for (name, dims) in [("a.i", vec![2, 3]), ("b.i", vec![3]), ("c.i", Vec::new())] {
        let var_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name,
                dims,
                flow: true,
                is_primitive: true,
                ..connection_test_variable(test_span())
            },
        );
    }

    // `a.i[1]` and `a.i[1,1]` are distinct rendered members that both denote
    // element `[1,1]` of `a.i`. They land in two different flow sets at the
    // root scope, so the second set's claim on that element is refused after
    // the `p`/`q` set and the first `a.i` set were already planned.
    let mut selected_a = ast::QualifiedName::from_dotted("a.i");
    selected_a.parts.last_mut().expect("a.i has a leaf").1 = vec![1];
    let mut selected_a_scalar = ast::QualifiedName::from_dotted("a.i");
    selected_a_scalar
        .parts
        .last_mut()
        .expect("a.i has a leaf")
        .1 = vec![1, 1];
    let (mut overlay, root_id) = paired_overlay_root(&mut flat);
    add_paired_component(&mut flat, &mut overlay, root_id, "p", Vec::new());
    add_paired_component(&mut flat, &mut overlay, root_id, "q", Vec::new());
    add_paired_component(&mut flat, &mut overlay, root_id, "a.i", vec![2, 3]);
    add_paired_component(&mut flat, &mut overlay, root_id, "b.i", vec![3]);
    add_paired_component(&mut flat, &mut overlay, root_id, "c.i", Vec::new());
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: root_id,
            qualified_name: ast::QualifiedName::from_ident("Root"),
            source_scope: Some(ast::QualifiedName::from_ident("Root")),
            connections: vec![
                ast::InstanceConnection::scalar(
                    ast::QualifiedName::from_ident("p"),
                    ast::QualifiedName::from_ident("q"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
                ast::InstanceConnection::scalar(
                    selected_a,
                    ast::QualifiedName::from_dotted("b.i"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
                ast::InstanceConnection::scalar(
                    selected_a_scalar,
                    ast::QualifiedName::from_dotted("c.i"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
            ],
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let overconstrained = overlay
        .finalized_overconstrained()
        .expect("connection fixture must construct finalized occurrence proofs");
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    finalize_connection_test_flat(&mut flat);
    let error = process_connections_for_test(&mut flat, &overconstrained, &mut forest)
        .expect_err("a late double-sum refusal must roll back the whole pass");

    assert!(
        matches!(error, FlattenError::InvalidConnectionEvidence { .. }),
        "{error}"
    );
    assert!(
        error.to_string().contains("summed by two connection sets"),
        "{error}"
    );
    assert!(flat.equations.is_empty());
    assert!(flat.structured_equations.is_empty());
    assert!(
        flat.variables
            .values()
            .all(|variable| variable.connected.is_unconnected())
    );
}

#[test]
fn process_connections_refuses_out_of_range_flat_selection_without_mutation() {
    let mut flat = connection_test_model();
    for (name, dims) in [("a.i", vec![2]), ("b.i", Vec::new())] {
        let var_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name,
                dims,
                flow: true,
                is_primitive: true,
                ..connection_test_variable(test_span())
            },
        );
    }

    // The source occurrence is deliberately a three-element array, proving
    // `a.i[3]` as legal Instance input. The retained Flat declaration below
    // contradicts that proof with extent two; route discovery must refuse the
    // contradiction instead of treating the endpoint as a composite connector.
    let mut selected_a = ast::QualifiedName::from_dotted("a.i");
    selected_a.parts.last_mut().expect("a.i has a leaf").1 = vec![3];
    let (mut overlay, root_id) = paired_overlay_root(&mut flat);
    add_paired_component(&mut flat, &mut overlay, root_id, "a.i", vec![3]);
    add_paired_component(&mut flat, &mut overlay, root_id, "b.i", Vec::new());
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: root_id,
            qualified_name: ast::QualifiedName::from_ident("Root"),
            source_scope: Some(ast::QualifiedName::from_ident("Root")),
            connections: vec![
                ast::InstanceConnection::scalar(
                    selected_a,
                    ast::QualifiedName::from_dotted("b.i"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("the Instance connection selection is source-legal"),
            ],
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");

    finalize_connection_test_flat(&mut flat);
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let overconstrained = overlay
        .finalized_overconstrained()
        .expect("connection fixture must construct finalized occurrence proofs");
    let before = connection_mutation_snapshot(&flat);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();
    let forest_before = forest.state_snapshot();

    let error = process_connections_for_test(&mut flat, &overconstrained, &mut forest)
        .expect_err("contradictory Flat selection evidence must fail closed");

    assert!(
        error.to_string().contains("outside its declared dimension"),
        "unexpected refusal: {error}"
    );
    assert_eq!(connection_mutation_snapshot(&flat), before);
    assert_eq!(forest.state_snapshot(), forest_before);
}

#[test]
fn process_connections_accepts_singleton_full_domain_selection() {
    let mut flat = connection_test_model();
    for (name, dims) in [("a.v", vec![1]), ("b.v", Vec::new())] {
        let var_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name,
                dims,
                is_primitive: true,
                ..connection_test_variable(test_span())
            },
        );
    }

    let mut selected_a = ast::QualifiedName::from_dotted("a.v");
    selected_a.parts.last_mut().expect("a.v has a leaf").1 = vec![1];
    let (mut overlay, root_id) = paired_overlay_root(&mut flat);
    add_paired_component(&mut flat, &mut overlay, root_id, "a.v", vec![1]);
    add_paired_component(&mut flat, &mut overlay, root_id, "b.v", Vec::new());
    overlay
        .add_class(ast::ClassInstanceData {
            instance_id: root_id,
            qualified_name: ast::QualifiedName::from_ident("Root"),
            source_scope: Some(ast::QualifiedName::from_ident("Root")),
            connections: vec![
                ast::InstanceConnection::scalar(
                    selected_a,
                    ast::QualifiedName::from_dotted("b.v"),
                    None,
                    test_span(),
                    String::new(),
                )
                .expect("singleton full-domain selection is source-legal"),
            ],
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");

    finalize_connection_test_flat(&mut flat);
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let overconstrained = overlay
        .finalized_overconstrained()
        .expect("connection fixture must construct finalized occurrence proofs");
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();
    process_connections_for_test(&mut flat, &overconstrained, &mut forest)
        .expect("a singleton leading selection denotes the complete compact declaration");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 1);
    assert!(
        flat.variables
            .values()
            .all(|variable| !variable.connected.is_unconnected())
    );
}

#[test]
fn singleton_leading_extent_selection_can_mark_the_whole_flow_declaration() {
    let mut flat = connection_test_model();
    let lhs_base = rumoca_core::VarName::new("a.i");
    let rhs = rumoca_core::VarName::new("b.i");
    add_materialized_variable(
        &mut flat,
        lhs_base.clone(),
        array_variable(vec![1, 3], true),
    );
    add_materialized_variable(&mut flat, rhs.clone(), array_variable(vec![3], true));

    generate_flow_equation(
        &mut flat,
        &[rumoca_core::VarName::new("a.i[1]"), rhs.clone()],
        "",
        &IndexMap::default(),
        test_span(),
    )
    .expect("selecting the only leading coordinate denotes the entire declaration");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 3);
    assert!(!flat.variables[&lhs_base].connected.is_unconnected());
    assert!(!flat.variables[&rhs].connected.is_unconnected());
}

/// MLS §10.5 for a subscript that sits on an *inner* path segment. A collapsed
/// connector-array member `a.e` of dims `[2, 3]` is denoted by `a[1].e` as
/// `Real[3]`, so the connection covers three scalars. The embedded-index branch
/// used to answer a constant 1 without consulting any declaration, which is the
/// same defect the trailing-subscript case had.
#[test]
fn embedded_index_partial_compact_selection_counts_the_denoted_leaves() {
    let mut flat = connection_test_model();
    flat.add_variable(
        rumoca_core::VarName::new("a.e"),
        array_variable(vec![2, 3], false),
    );

    assert_eq!(
        resolve_var_scalar_count(&flat, &rumoca_core::VarName::new("a[1].e")),
        Some(3)
    );
}

/// Two embedded subscripts consume two leading dimensions.
#[test]
fn embedded_index_full_scalar_selection_counts_the_trailing_dimension() {
    let mut flat = connection_test_model();
    flat.add_variable(
        rumoca_core::VarName::new("a.e"),
        array_variable(vec![2, 3, 4], false),
    );

    assert_eq!(
        resolve_var_scalar_count(&flat, &rumoca_core::VarName::new("a[1,2].e")),
        Some(4)
    );
}

/// A compact member of an already-expanded occurrence (`a[1].e` of rank one)
/// selected by one trailing element connects exactly that element of the
/// occurrence's own declaration.
#[test]
fn per_occurrence_compact_array_member_selection_marks_one_element() {
    let mut flat = connection_test_model();
    for base in ["a[1].e", "b[1].e"] {
        flat.add_variable(
            rumoca_core::VarName::new(base),
            array_variable(vec![2], false),
        );
    }
    let (mut overlay, root) = paired_overlay_root(&mut flat);
    add_paired_component(&mut flat, &mut overlay, root, "a[1].e", vec![2]);
    add_paired_component(&mut flat, &mut overlay, root, "b[1].e", vec![2]);
    let catalog = crate::test_support::finalized_test_overlay(&mut overlay);
    finalize_connection_test_flat(&mut flat);
    let lhs = rumoca_core::VarName::new("a[1].e[1]");
    let rhs = rumoca_core::VarName::new("b[1].e[2]");
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    equation_generation::generate_equality_equations(
        &mut flat,
        &catalog,
        &[lhs, rhs],
        test_span(),
        &mut forest,
    )
    .expect("a compact member selection has a checked connected-domain owner");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 1);
    assert_eq!(
        flat.variables[&rumoca_core::VarName::new("a[1].e")]
            .connected
            .selections()
            .collect::<Vec<_>>(),
        vec![&[1][..]]
    );
    assert_eq!(
        flat.variables[&rumoca_core::VarName::new("b[1].e")]
            .connected
            .selections()
            .collect::<Vec<_>>(),
        vec![&[2][..]]
    );
}

#[test]
fn longest_selection_owner_rejects_bounds_rank_and_ambiguity_before_mutation() {
    for (endpoint, extra_owner, expected_reason) in [
        ("a[1].e[3]", None, "outside its declared dimension"),
        (
            "a[1].e[1][1]",
            None,
            "subscripts select a declaration of rank",
        ),
        (
            "a[1].e[1]",
            Some("a.e[1]"),
            "ambiguous longest Flat declaration owners",
        ),
    ] {
        let mut flat = connection_test_model();
        let owner = rumoca_core::VarName::new("a[1].e");
        let peer = rumoca_core::VarName::new("peer");
        flat.add_variable(owner.clone(), array_variable(vec![2], false));
        flat.add_variable(peer.clone(), array_variable(Vec::new(), false));
        if let Some(extra_owner) = extra_owner {
            flat.add_variable(
                rumoca_core::VarName::new(extra_owner),
                array_variable(vec![2], false),
            );
        }
        let mut forest = crate::vcg::OverconstrainedEquationForest::empty();
        let forest_before = forest.state_snapshot();

        let error = generate_equality_equations(
            &mut flat,
            &[rumoca_core::VarName::new(endpoint), peer.clone()],
            test_span(),
            &mut forest,
        )
        .expect_err("invalid or ambiguous selection evidence must fail before mutation");

        assert!(
            error.to_string().contains(expected_reason),
            "{endpoint}: {error}"
        );
        assert!(flat.equations.is_empty());
        assert!(flat.structured_equations.is_empty());
        assert!(
            flat.variables
                .values()
                .all(|variable| variable.connected.is_unconnected())
        );
        assert_eq!(forest.state_snapshot(), forest_before);
    }
}

#[test]
fn longest_selection_owner_is_identical_at_scan_threshold_plus_or_minus_one() {
    for candidate_count in [
        MAX_DIRECT_CONNECTION_SELECTION_CANDIDATES - 1,
        MAX_DIRECT_CONNECTION_SELECTION_CANDIDATES,
        MAX_DIRECT_CONNECTION_SELECTION_CANDIDATES + 1,
    ] {
        let rank = candidate_count - 1;
        let coordinates = std::iter::repeat_n("1", rank).collect::<Vec<_>>().join(",");
        let endpoint = rumoca_core::VarName::new(format!("x[{coordinates}]"));
        let mut flat = connection_test_model();
        flat.add_variable(
            rumoca_core::VarName::new("x"),
            array_variable(vec![1; rank], false),
        );

        let selected = declared_array_element_evidence(&endpoint, &flat)
            .expect("a high-rank literal selection is valid evidence")
            .expect("both lookup strategies find the exact owner");

        assert_eq!(selected.base, rumoca_core::VarName::new("x"));
        assert_eq!(selected.indices, vec![1; rank]);
    }
}

#[test]
fn declaration_scan_preserves_deterministic_ambiguity_refusal() {
    let endpoint = (0..11)
        .map(|index| format!("p{index}[1]"))
        .collect::<Vec<_>>()
        .join(".");
    let left_owner = (0..11)
        .map(|index| {
            if index == 0 {
                "p0[1]".to_string()
            } else {
                format!("p{index}")
            }
        })
        .collect::<Vec<_>>()
        .join(".");
    let right_owner = (0..11)
        .map(|index| {
            if index == 1 {
                "p1[1]".to_string()
            } else {
                format!("p{index}")
            }
        })
        .collect::<Vec<_>>()
        .join(".");
    let mut flat = connection_test_model();
    for owner in [&left_owner, &right_owner] {
        flat.add_variable(
            rumoca_core::VarName::new(owner),
            array_variable(vec![1; 10], false),
        );
    }

    let error = match declared_array_element_evidence(&rumoca_core::VarName::new(endpoint), &flat) {
        Err(error) => error,
        Ok(_) => panic!("equal-rank declaration owners must never be selected by map order"),
    };

    let mut sorted = [left_owner, right_owner];
    sorted.sort();
    assert!(error.contains("ambiguous longest Flat declaration owners"));
    assert!(error.contains(&sorted.join(", ")), "{error}");
}

#[test]
fn embedded_indices_must_be_concrete_and_in_bounds_before_mutation() {
    for endpoint in ["a[0].i", "a[3].i", "a[x].i"] {
        let mut flat = connection_test_model();
        let base = rumoca_core::VarName::new("a.i");
        let peer = rumoca_core::VarName::new("b.i");
        flat.add_variable(base.clone(), array_variable(vec![2, 3], true));
        flat.add_variable(peer.clone(), array_variable(vec![3], true));

        let error = generate_flow_equation(
            &mut flat,
            &[rumoca_core::VarName::new(endpoint), peer.clone()],
            "",
            &IndexMap::default(),
            test_span(),
        )
        .expect_err("an unproven embedded selection must never enter Flat connection IR");

        assert!(
            matches!(error, FlattenError::InvalidConnectionEvidence { .. }),
            "{error}"
        );
        assert!(flat.equations.is_empty());
        assert!(flat.structured_equations.is_empty());
        assert!(flat.variables[&base].connected.is_unconnected());
        assert!(flat.variables[&peer].connected.is_unconnected());
    }
}

/// Selecting every declared dimension denotes one scalar. A dimensionless base
/// cannot stand in for a connector-array field whose rank was lost: the two
/// states are indistinguishable, so the latter must fail closed.
#[test]
fn embedded_index_endpoints_require_enough_declared_rank() {
    let mut flat = connection_test_model();
    flat.add_variable(
        rumoca_core::VarName::new("a.e"),
        array_variable(vec![2], false),
    );
    flat.add_variable(
        rumoca_core::VarName::new("b.e"),
        array_variable(Vec::new(), false),
    );

    assert_eq!(
        resolve_var_scalar_count(&flat, &rumoca_core::VarName::new("a[1].e")),
        Some(1)
    );
    assert_eq!(
        resolve_var_scalar_count(&flat, &rumoca_core::VarName::new("b[1].e")),
        None
    );
}

/// No declaration for the index-free path is no evidence, not one scalar:
/// callers distinguish the two, and a fabricated 1 collapses a mixed
/// scalar/array flow set onto a single Kirchhoff equation.
#[test]
fn embedded_index_endpoints_without_a_declaration_are_unknown() {
    let flat = connection_test_model();

    assert_eq!(
        resolve_var_scalar_count(&flat, &rumoca_core::VarName::new("a[1].e")),
        None
    );
}

#[test]
fn equality_generation_rejects_two_unknown_cardinalities_without_mutation() {
    let mut flat = connection_test_model();
    let lhs = rumoca_core::VarName::new("missing.a");
    let rhs = rumoca_core::VarName::new("missing.b");
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = generate_equality_equations(&mut flat, &[lhs, rhs], test_span(), &mut forest)
        .expect_err("missing endpoint evidence must not be fabricated as one scalar");

    assert!(matches!(
        error,
        FlattenError::UndefinedVariable { ref name, .. } if name == "missing.a"
    ));
    assert!(flat.equations.is_empty());
    assert!(flat.structured_equations.is_empty());
}

#[test]
fn equality_generation_prevalidates_the_whole_chain_before_mutation() {
    let mut flat = connection_test_model();
    let first = rumoca_core::VarName::new("known.a");
    let second = rumoca_core::VarName::new("known.b");
    let missing = rumoca_core::VarName::new("missing.c");
    flat.add_variable(first.clone(), array_variable(Vec::new(), false));
    flat.add_variable(second.clone(), array_variable(Vec::new(), false));
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = generate_equality_equations(
        &mut flat,
        &[first.clone(), second.clone(), missing],
        test_span(),
        &mut forest,
    )
    .expect_err("a late missing member must be found before emitting an earlier equality");

    assert!(matches!(error, FlattenError::UndefinedVariable { .. }));
    assert!(flat.equations.is_empty());
    assert!(flat.structured_equations.is_empty());
    assert!(flat.variables[&first].connected.is_unconnected());
    assert!(flat.variables[&second].connected.is_unconnected());
}

#[test]
fn invalid_rank_and_cardinality_refuse_before_flow_mutation() {
    let fixtures = [
        (vec![], "a[1].i"),
        (vec![2], "a[1,1].i"),
        (vec![i64::MAX, 2], "a.i"),
    ];
    for (dims, endpoint) in fixtures {
        let mut flat = connection_test_model();
        let base = rumoca_core::VarName::new("a.i");
        let peer = rumoca_core::VarName::new("b.i");
        flat.add_variable(base.clone(), array_variable(dims, true));
        flat.add_variable(peer.clone(), array_variable(Vec::new(), true));

        let result = generate_flow_equation(
            &mut flat,
            &[rumoca_core::VarName::new(endpoint), peer.clone()],
            "",
            &IndexMap::default(),
            test_span(),
        );
        let error = match result {
            Ok(()) => panic!("`{endpoint}` with the supplied dimensions must fail closed"),
            Err(error) => error,
        };

        assert!(
            matches!(error, FlattenError::InvalidConnectionEvidence { .. }),
            "{error}"
        );
        assert!(flat.equations.is_empty());
        assert!(flat.variables[&base].connected.is_unconnected());
        assert!(flat.variables[&peer].connected.is_unconnected());
    }
}

#[test]
fn negative_connection_dimension_is_refused_by_flat_shape_construction() {
    let mut flat = connection_test_model();
    let name = rumoca_core::VarName::new("a.i");
    let mut variable = array_variable(vec![-1], true);
    variable.name = name.clone();
    flat.add_variable(name.clone(), variable);

    let error = flat
        .finalize_effective_type_shapes()
        .expect_err("negative dimensions cannot acquire the Flat shape proof");

    assert!(matches!(
        error,
        flat::ModelShapeContractError::Variable(
            flat::VariableShapeContractError::NegativeDimension { variable, .. }
        ) if variable == name
    ));
    assert!(flat.equations.is_empty());
}

#[test]
fn a_zero_extent_remains_a_proven_empty_flow_set() {
    for dims in [[0, i64::MAX, 2], [i64::MAX, 2, 0]] {
        let mut flat = connection_test_model();
        let lhs = rumoca_core::VarName::new("a.i");
        let rhs = rumoca_core::VarName::new("b.i");
        flat.add_variable(lhs.clone(), array_variable(dims.to_vec(), true));
        flat.add_variable(rhs.clone(), array_variable(dims.to_vec(), true));

        generate_flow_equation(
            &mut flat,
            &[lhs.clone(), rhs.clone()],
            "",
            &IndexMap::default(),
            test_span(),
        )
        .expect("a zero extent proves an empty flow array in every dimension order");

        assert!(flat.equations.is_empty());
        assert!(flat.variables[&lhs].connected.is_unconnected());
        assert!(flat.variables[&rhs].connected.is_unconnected());
    }
}

#[test]
fn a_strict_selection_of_an_empty_trailing_axis_is_an_empty_flow_set() {
    let mut flat = connection_test_model();
    let lhs = rumoca_core::VarName::new("a.i");
    let rhs = rumoca_core::VarName::new("b.i");
    flat.add_variable(lhs.clone(), array_variable(vec![2, 0], true));
    flat.add_variable(rhs.clone(), array_variable(vec![0], true));

    generate_flow_equation(
        &mut flat,
        &[rumoca_core::VarName::new("a.i[1]"), rhs.clone()],
        "",
        &IndexMap::default(),
        test_span(),
    )
    .expect("selecting Real[0] from Real[2,0] denotes a proven empty set");

    assert!(flat.equations.is_empty());
    assert!(flat.variables[&lhs].connected.is_unconnected());
    assert!(flat.variables[&rhs].connected.is_unconnected());
}

#[test]
fn empty_stream_subdomain_does_not_mark_its_declaration_connected() {
    let mut flat = connection_test_model();
    let lhs = rumoca_core::VarName::new("a.h");
    let rhs = rumoca_core::VarName::new("b.h");
    let mut lhs_variable = array_variable(vec![2, 0], false);
    lhs_variable.stream = true;
    let mut rhs_variable = array_variable(vec![0], false);
    rhs_variable.stream = true;
    flat.add_variable(lhs.clone(), lhs_variable);
    flat.add_variable(rhs.clone(), rhs_variable);

    mark_stream_connection_set(
        &mut flat,
        &[rumoca_core::VarName::new("a.h[1]"), rhs.clone()],
        test_span(),
    )
    .expect("an empty stream subdomain is representable without connected bits");

    assert!(flat.variables[&lhs].connected.is_unconnected());
    assert!(flat.variables[&rhs].connected.is_unconnected());
}

#[test]
fn zero_cardinality_does_not_launder_incompatible_shapes() {
    let mut flat = connection_test_model();
    let lhs = rumoca_core::VarName::new("a.i");
    let rhs = rumoca_core::VarName::new("b.i");
    flat.add_variable(lhs.clone(), array_variable(vec![0, 2], true));
    flat.add_variable(rhs.clone(), array_variable(vec![0, 3], true));

    let error = generate_flow_equation(
        &mut flat,
        &[lhs.clone(), rhs.clone()],
        "",
        &IndexMap::default(),
        test_span(),
    )
    .expect_err("different empty array shapes are not connection compatible");

    assert!(matches!(error, FlattenError::IncompatibleConnectors { .. }));
    assert!(flat.equations.is_empty());
    assert!(flat.variables[&lhs].connected.is_unconnected());
    assert!(flat.variables[&rhs].connected.is_unconnected());
}

#[test]
fn oversized_equal_shapes_refuse_before_equality_mutation() {
    let mut flat = connection_test_model();
    let lhs = rumoca_core::VarName::new("a.v");
    let rhs = rumoca_core::VarName::new("b.v");
    let oversized = vec![i64::MAX, 2];
    flat.add_variable(lhs.clone(), array_variable(oversized.clone(), false));
    flat.add_variable(rhs.clone(), array_variable(oversized, false));
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = generate_equality_equations(
        &mut flat,
        &[lhs.clone(), rhs.clone()],
        test_span(),
        &mut forest,
    )
    .expect_err("an unrepresentable structured domain must fail before connection mutation");

    assert!(
        matches!(error, FlattenError::InvalidConnectionEvidence { .. }),
        "{error}"
    );
    assert!(flat.equations.is_empty());
    assert!(flat.structured_equations.is_empty());
    assert!(flat.variables[&lhs].connected.is_unconnected());
    assert!(flat.variables[&rhs].connected.is_unconnected());
}

#[test]
fn flow_generation_rejects_one_unknown_cardinality_before_marking_known_members() {
    let mut flat = connection_test_model();
    let known = rumoca_core::VarName::new("known.i");
    let missing = rumoca_core::VarName::new("missing.i");
    flat.add_variable(known.clone(), array_variable(vec![3], true));

    let error = generate_flow_equation(
        &mut flat,
        &[known.clone(), missing],
        "",
        &IndexMap::default(),
        test_span(),
    )
    .expect_err("one unknown member must not disappear from a flow set");

    assert!(matches!(
        error,
        FlattenError::UndefinedVariable { ref name, .. } if name == "missing.i"
    ));
    assert!(flat.equations.is_empty());
    assert!(flat.variables[&known].connected.is_unconnected());
}

#[test]
fn flow_generation_rejects_an_entirely_unknown_set() {
    let mut flat = connection_test_model();
    let variables = [
        rumoca_core::VarName::new("missing.a.i"),
        rumoca_core::VarName::new("missing.b.i"),
    ];

    let error =
        generate_flow_equation(&mut flat, &variables, "", &IndexMap::default(), test_span())
            .expect_err("an unknown flow set must not become a scalar equation");

    assert!(matches!(error, FlattenError::UndefinedVariable { .. }));
    assert!(flat.equations.is_empty());
}

#[test]
fn outside_stream_generation_prevalidates_every_endpoint_before_mutation() {
    let mut flat = connection_test_model();
    let known = rumoca_core::VarName::new("known.h_outflow");
    let missing = rumoca_core::VarName::new("missing.h_outflow");
    flat.add_variable(
        known.clone(),
        flat::Variable {
            name: known.clone(),
            stream: true,
            ..connection_test_variable(test_span())
        },
    );
    let mut endpoints = InterfaceStreamEndpointsByScope::default();
    endpoints
        .entry(String::new())
        .or_default()
        .insert(known.clone(), test_span());
    endpoints
        .entry(String::new())
        .or_default()
        .insert(missing, test_span());
    let stream_endpoints =
        super::stream_operators::build_stream_connection_endpoints(&flat, &[], &endpoints)
            .expect("an empty stream-set inventory is valid");

    let error = generate_outside_stream_equations(&mut flat, &endpoints, &stream_endpoints)
        .expect_err("a missing outside stream endpoint must fail before generating earlier rows");

    assert!(matches!(
        error,
        FlattenError::UndefinedVariable { ref name, .. } if name == "missing.h_outflow"
    ));
    assert!(flat.equations.is_empty());
    assert!(flat.variables[&known].connected.is_unconnected());
}

#[test]
fn selected_outside_stream_rejects_out_of_bounds_value_coordinate_atomically() {
    let mut flat = connection_test_model();
    let declaration = rumoca_core::VarName::new("port.h_outflow");
    flat.add_variable(
        declaration.clone(),
        flat::Variable {
            name: declaration.clone(),
            dims: vec![1],
            stream: true,
            ..connection_test_variable(test_span())
        },
    );
    let mut endpoints = InterfaceStreamEndpointsByScope::default();
    endpoints
        .entry(String::new())
        .or_default()
        .insert(rumoca_core::VarName::new("port.h_outflow[2]"), test_span());
    let stream_endpoints =
        super::stream_operators::build_stream_connection_endpoints(&flat, &[], &endpoints)
            .expect("an empty stream-set inventory is valid");

    let error = generate_outside_stream_equations(&mut flat, &endpoints, &stream_endpoints)
        .expect_err("an outside-stream owner cannot launder an out-of-bounds selected value");

    assert!(matches!(
        error,
        FlattenError::InvalidConnectionEvidence { .. }
    ));
    assert!(flat.equations.is_empty());
    assert!(flat.structured_equations.is_empty());
    assert!(flat.variables[&declaration].connected.is_unconnected());
}

#[test]
fn duplicate_selected_stream_occurrence_is_refused_by_flat_construction() {
    let mut flat = connection_test_model();
    let declaration = rumoca_core::VarName::new("port.h_outflow");
    let duplicate = rumoca_core::VarName::new("other.h_outflow");
    for name in [&declaration, &duplicate] {
        flat.add_variable(
            name.clone(),
            flat::Variable {
                instance_id: rumoca_core::InstanceId::new(7),
                name: name.clone(),
                dims: vec![1],
                stream: true,
                ..connection_test_variable(test_span())
            },
        );
    }
    assert!(matches!(
        flat.validate_shape_contract(),
        Err(flat::ModelShapeContractError::DuplicateVariableInstanceId {
            instance_id,
            ..
        }) if instance_id == rumoca_core::InstanceId::new(7)
    ));
    assert!(flat.equations.is_empty());
    assert!(flat.structured_equations.is_empty());
    assert!(flat.variables[&declaration].connected.is_unconnected());
    assert!(flat.variables[&duplicate].connected.is_unconnected());
}

#[test]
fn outside_stream_generation_prevalidates_every_provenance_before_mutation() {
    let mut flat = connection_test_model();
    let first = rumoca_core::VarName::new("first.h_outflow");
    let second = rumoca_core::VarName::new("second.h_outflow");
    for name in [&first, &second] {
        flat.add_variable(
            name.clone(),
            flat::Variable {
                name: name.clone(),
                stream: true,
                ..connection_test_variable(test_span())
            },
        );
    }
    let mut endpoints = InterfaceStreamEndpointsByScope::default();
    let scope = endpoints.entry(String::new()).or_default();
    scope.insert(first.clone(), test_span());
    scope.insert(second.clone(), rumoca_core::Span::DUMMY);
    let stream_endpoints =
        super::stream_operators::build_stream_connection_endpoints(&flat, &[], &endpoints)
            .expect("an empty stream-set inventory is valid");

    generate_outside_stream_equations(&mut flat, &endpoints, &stream_endpoints)
        .expect_err("late missing provenance must fail before an earlier endpoint is emitted");

    assert!(flat.equations.is_empty());
    assert!(flat.structured_equations.is_empty());
    assert!(flat.variables[&first].connected.is_unconnected());
    assert!(flat.variables[&second].connected.is_unconnected());
}

#[test]
fn unconnected_flow_generation_prevalidates_every_provenance_before_mutation() {
    let mut flat = connection_test_model();
    let first = rumoca_core::VarName::new("first.i");
    let second = rumoca_core::VarName::new("second.i");
    flat.add_variable(first.clone(), array_variable(Vec::new(), true));
    flat.add_variable(
        second.clone(),
        flat::Variable {
            name: second,
            flow: true,
            source_span: rumoca_core::Span::DUMMY,
            ..connection_test_variable(rumoca_core::Span::DUMMY)
        },
    );

    generate_unconnected_flow_equations(&mut flat)
        .expect_err("late missing provenance must be detected before the first flow-zero row");

    assert!(flat.equations.is_empty());
    assert!(flat.structured_equations.is_empty());
    assert!(flat.variables[&first].connected.is_unconnected());
}

/// A flow set whose member carries an embedded index keeps the leaf count of
/// what that member denotes, instead of shrinking the sum to one scalar, and
/// marks exactly the denoted row so the other row still receives its zero rows.
#[test]
fn partial_embedded_compact_flow_selection_sums_and_zeroes_the_denoted_leaves() {
    let mut flat = connection_test_model();
    let base = rumoca_core::VarName::new("a.i");
    add_materialized_variable(&mut flat, base.clone(), array_variable(vec![2, 3], true));
    let rhs = rumoca_core::VarName::new("b.i");
    add_materialized_variable(&mut flat, rhs.clone(), array_variable(vec![3], true));
    let lhs = rumoca_core::VarName::new("a[1].i");

    generate_flow_equation(
        &mut flat,
        &[lhs, rhs.clone()],
        "",
        &IndexMap::default(),
        test_span(),
    )
    .expect("an embedded leading selection has a checked connected-domain owner");
    generate_unconnected_flow_equations(&mut flat)
        .expect("the committed domain decides the zero-flow rows");

    assert_eq!(flat.equations[0].scalar_count, 3);
    assert!(matches!(
        flat.equations[0].origin,
        flat::EquationOrigin::FlowSum { .. }
    ));
    let zero_rows: Vec<String> = flat.equations[1..]
        .iter()
        .map(|equation| match &equation.origin {
            flat::EquationOrigin::UnconnectedFlow { variable } => variable.clone(),
            other => panic!("only zero-flow rows may follow the flow sum, got {other:?}"),
        })
        .collect();
    assert_eq!(zero_rows, ["a.i[2,1]", "a.i[2,2]", "a.i[2,3]"]);
    assert_eq!(
        flat.variables[&base]
            .connected
            .selections()
            .collect::<Vec<_>>(),
        vec![&[1][..]]
    );
    assert_eq!(
        flat.variables[&rhs].connected.coverage(&[3]),
        Ok(flat::ConnectedCoverage::Whole)
    );
}

#[test]
fn flow_generation_rejects_multiple_array_sizes() {
    let mut flat = connection_test_model();
    let lhs = rumoca_core::VarName::new("a.i");
    let rhs = rumoca_core::VarName::new("b.i");
    flat.add_variable(lhs.clone(), array_variable(vec![2], true));
    flat.add_variable(rhs.clone(), array_variable(vec![3], true));

    let error = generate_flow_equation(
        &mut flat,
        &[lhs, rhs],
        "",
        &IndexMap::default(),
        test_span(),
    )
    .expect_err("mismatched flow arrays must not inherit the first size");

    assert!(error.to_string().contains("incompatible connector types"));
    assert!(flat.equations.is_empty());
}
