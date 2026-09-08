//! MLS §10.5 / §9.1 endpoint-subscript admissibility for `connect` arguments.
//!
//! These tests pin every arm of the acceptance contract stated in the module
//! docs — a subscripted endpoint that names a declared occurrence, one whose
//! base has no declaration in view, one whose declaration has retained or
//! symbolic dimensions, one whose visible rank is not authoritative because a
//! redeclaration was consumed, and the provable dimensionless and
//! over-subscripted violations. Refusals occur before connection-set mutation
//! and never drop the selector onto the whole component.

use super::*;

/// Lexical scope of the class that declares the components and writes the
/// connection.
const OWNING_SCOPE: &str = "Root";
const CONNECTOR_TYPE: rumoca_core::TypeId = rumoca_core::TypeId(0x53_0001);

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("connection_endpoint_subscript_tests.mo"),
        40,
        56,
    )
}

fn process_test_connections(
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    forest: &mut crate::vcg::OverconstrainedEquationForest,
) -> Result<(), FlattenError> {
    finalize_connection_test_flat(flat);
    let overconstrained = overlay
        .finalized_overconstrained()
        .expect("endpoint fixture must construct finalized occurrence proofs");
    equation_generation::process_connections_for_test(flat, &overconstrained, forest)
}

fn declaration_location() -> rumoca_core::Location {
    rumoca_core::Location {
        start_line: 3,
        start_column: 3,
        end_line: 3,
        end_column: 7,
        start: 12,
        end: 16,
        source: rumoca_core::SourceId::from_source_name(
            "connection_endpoint_subscript_tests_declaration.mo",
        ),
    }
}

/// Flat model for two scalar connectors `a` and `b`, each with one potential
/// and one flow member.
fn two_scalar_connectors() -> flat::Model {
    let mut flat = connection_test_model();
    for (name, flow) in [("a.e", false), ("a.f", true), ("b.e", false), ("b.f", true)] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                flow,
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
    }
    flat
}

/// One declared connector component owned by the root class instance.
///
/// `declaration_scope` is the lexical scope the declaration was written in;
/// passing [`OWNING_SCOPE`] models a component declared by the same class that
/// instantiated it, which is what makes its rank authoritative.
fn declared_connector(
    instance_id: rumoca_core::InstanceId,
    owner_class_id: rumoca_core::InstanceId,
    name: &str,
    dims: Vec<i64>,
    dims_expr: Vec<ast::Subscript>,
    declaration_scope: &str,
) -> ast::InstanceData {
    ast::InstanceData {
        instance_id,
        qualified_name: ast::QualifiedName::from_dotted(name),
        owner_class_id: Some(owner_class_id),
        declaration_source_scope: Some(ast::QualifiedName::from_ident(declaration_scope)),
        source_location: declaration_location(),
        type_id: CONNECTOR_TYPE,
        dims,
        dims_expr,
        is_connector_type: true,
        ..Default::default()
    }
}

/// Root class instance owning `connections`, declared in [`OWNING_SCOPE`].
fn root_class(
    instance_id: rumoca_core::InstanceId,
    connections: Vec<ast::InstanceConnection>,
) -> ast::ClassInstanceData {
    ast::ClassInstanceData {
        instance_id,
        qualified_name: ast::QualifiedName::from_ident(OWNING_SCOPE),
        source_scope: Some(ast::QualifiedName::from_ident(OWNING_SCOPE)),
        connections,
        ..Default::default()
    }
}

/// One `connect(<endpoint>[subscripts], b)` statement.
fn connect_element(endpoint: &str, subscripts: Vec<i64>) -> ast::InstanceConnection {
    let mut a = ast::QualifiedName::from_dotted(endpoint);
    let last = a.parts.len() - 1;
    a.parts[last].1 = subscripts;
    ast::InstanceConnection::scalar(
        a,
        ast::QualifiedName::from_ident("b"),
        None,
        test_span(),
        String::new(),
    )
    .expect("test scalar connection is valid")
}

/// Overlay declaring component `a` with the given dimensions plus a connection
/// `connect(a[1], b)`.
fn overlay_connecting_element_of(
    dims: Vec<i64>,
    dims_expr: Vec<ast::Subscript>,
) -> ast::InstanceOverlay {
    overlay_connecting(dims, dims_expr, OWNING_SCOPE, vec![1])
}

/// Overlay for `connect(a[subscripts], b)` where `a` is declared in
/// `declaration_scope` with `dims`/`dims_expr`.
fn overlay_connecting(
    dims: Vec<i64>,
    dims_expr: Vec<ast::Subscript>,
    declaration_scope: &str,
    subscripts: Vec<i64>,
) -> ast::InstanceOverlay {
    overlay_connecting_with_redeclare(dims, dims_expr, declaration_scope, subscripts, false)
}

fn overlay_connecting_with_redeclare(
    dims: Vec<i64>,
    dims_expr: Vec<ast::Subscript>,
    declaration_scope: &str,
    subscripts: Vec<i64>,
    had_redeclare: bool,
) -> ast::InstanceOverlay {
    let mut overlay = ast::InstanceOverlay::new();
    overlay.type_roots.insert(CONNECTOR_TYPE, CONNECTOR_TYPE);
    let root_id = overlay.alloc_id();
    let a_id = overlay.alloc_id();
    let b_id = overlay.alloc_id();
    overlay
        .add_component(ast::InstanceData {
            had_redeclare,
            ..declared_connector(a_id, root_id, "a", dims, dims_expr, declaration_scope)
        })
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(declared_connector(
            b_id,
            root_id,
            "b",
            Vec::new(),
            Vec::new(),
            OWNING_SCOPE,
        ))
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_class(root_class(root_id, vec![connect_element("a", subscripts)]))
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    overlay
}

#[test]
fn connect_subscript_on_a_dimensionless_declaration_is_rejected() {
    let mut flat = two_scalar_connectors();
    let overlay = overlay_connecting_element_of(Vec::new(), Vec::new());
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("a subscript on a declaration without dimensions must be rejected");

    assert!(
        matches!(
            error,
            FlattenError::SubscriptedDimensionlessConnector { .. }
        ),
        "expected the typed endpoint-subscript error, got: {error:?}"
    );
    assert!(
        error.to_string().contains("a[1]") && error.to_string().contains("without dimensions"),
        "error must name the offending endpoint: {error}"
    );
}

#[test]
fn dimensionless_endpoint_rejection_cites_the_declaration_site() {
    use rumoca_core::PhaseError as _;

    let mut flat = two_scalar_connectors();
    let overlay = overlay_connecting_element_of(Vec::new(), Vec::new());
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("a subscript on a declaration without dimensions must be rejected");

    let diagnostic = error.to_diagnostic();
    let spans: Vec<rumoca_core::Span> = diagnostic.labels.iter().map(|label| label.span).collect();
    assert!(
        spans.contains(&test_span()),
        "diagnostic must point at the connect endpoint, got: {spans:?}"
    );
    assert!(
        spans.contains(&declaration_location().span()),
        "diagnostic must cite the declaration site, got: {spans:?}"
    );
}

/// The whole point of the rejection: no path may drop the subscript and connect
/// the entire component `a` instead of the element the source asked for.
#[test]
fn dimensionless_endpoint_subscript_is_never_dropped_onto_the_whole_component() {
    let mut flat = two_scalar_connectors();
    let overlay = overlay_connecting_element_of(Vec::new(), Vec::new());
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let _ = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("a subscript on a declaration without dimensions must be rejected");

    assert!(
        flat.equations.is_empty(),
        "the rejected connection must not have produced connection equations: {:?}",
        flat.equations
            .iter()
            .map(|eq| eq.origin.to_string())
            .collect::<Vec<_>>()
    );
    assert!(
        flat.variables
            .values()
            .all(|variable| variable.connected.is_unconnected()),
        "the rejected connection must not have marked members connected"
    );
}

/// `connect(a[1], b)` on a declared `a[2]` whose Flat members are scalar
/// (`a.e`, `a.f`) names an element of a connector array whose retained Flat
/// declaration carries no rank. Nothing in view proves which element `a.e`
/// denotes, so the selection remains a typed refusal rather than a guess that
/// would connect the whole member.
#[test]
fn compact_element_connection_without_a_ranked_flat_member_is_refused() {
    let mut flat = connection_test_model();
    for (name, flow) in [("a.e", false), ("a.f", true), ("b.e", false), ("b.f", true)] {
        flat.add_test_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                flow,
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
    }
    let overlay = overlay_connecting_element_of(vec![2], Vec::new());
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("an element selection of an unranked Flat member cannot be proven");

    assert!(
        matches!(error, FlattenError::InvalidConnectionEvidence { .. }),
        "unexpected refusal: {error:?}"
    );
    assert!(
        error.to_string().contains("no retained rank"),
        "the refusal names the missing rank, not a fixture defect: {error}"
    );
    assert!(flat.equations.is_empty());
    assert!(
        flat.variables
            .values()
            .all(|variable| variable.connected.is_unconnected())
    );
}

/// Acceptance before rejection: a declaration that still carries a dimension
/// expression has not proven a rank of zero, so its element connections stay
/// admissible.
#[test]
fn symbolic_compact_element_connection_is_refused_without_a_proven_domain() {
    let mut flat = two_scalar_connectors();
    let overlay = overlay_connecting_element_of(
        Vec::new(),
        vec![ast::Subscript::Expression(ast::Expression::Empty {
            span: test_span(),
        })],
    );
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("an unresolved compact domain cannot prove safe partial connectivity");

    assert!(
        matches!(error, FlattenError::InvalidConnectionEvidence { .. }),
        "unexpected refusal: {error:?}"
    );
    assert!(flat.equations.is_empty());
}

/// An endpoint that names a declared element occurrence (the scalarized
/// representation of a connector array) carries its subscript legitimately.
#[test]
fn declared_occurrence_without_matching_flat_leaf_is_refused() {
    let mut flat = two_scalar_connectors();
    let mut overlay = ast::InstanceOverlay::new();
    overlay.type_roots.insert(CONNECTOR_TYPE, CONNECTOR_TYPE);
    let root_id = overlay.alloc_id();
    let a_id = overlay.alloc_id();
    let b_id = overlay.alloc_id();
    let element_id = overlay.alloc_id();
    overlay
        .add_component(declared_connector(
            a_id,
            root_id,
            "a",
            Vec::new(),
            Vec::new(),
            OWNING_SCOPE,
        ))
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(declared_connector(
            b_id,
            root_id,
            "b",
            Vec::new(),
            Vec::new(),
            OWNING_SCOPE,
        ))
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(ast::InstanceData {
            instance_id: element_id,
            owner_class_id: Some(root_id),
            qualified_name: ast::QualifiedName {
                parts: vec![("a".to_string(), vec![1])],
            },
            source_location: declaration_location(),
            type_id: CONNECTOR_TYPE,
            is_connector_type: true,
            ..Default::default()
        })
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_class(root_class(root_id, vec![connect_element("a", vec![1])]))
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("the overlay occurrence alone cannot invent a missing scalar Flat leaf");

    assert!(
        matches!(error, FlattenError::InvalidConnectionEvidence { .. }),
        "unexpected refusal: {error:?}"
    );
    assert!(flat.equations.is_empty());
}

/// MLS §7.3 lets a redeclaration add dimensions to a `replaceable C a;`.
/// Instantiation keeps only the redeclared type, so the component reaches this
/// phase carrying the original declaration's rank of zero. That zero is
/// evidence about rumoca, not about the model — OMC accepts the same source —
/// so it must not be reported as a user error.
#[test]
fn redeclared_component_rank_is_not_authoritative_evidence() {
    let mut flat = two_scalar_connectors();
    let overlay =
        overlay_connecting_with_redeclare(Vec::new(), Vec::new(), OWNING_SCOPE, vec![1], true);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let result = process_test_connections(&mut flat, &overlay, &mut forest);

    assert!(
        !matches!(
            result,
            Err(FlattenError::SubscriptedDimensionlessConnector { .. })
        ),
        "a rank dropped by the redeclare gap must not be blamed on the source: {result:?}"
    );
}

/// A redeclaration written on an *enclosing* declaration
/// (`Holder h(redeclare C a[2])`) drops the dimensions of everything
/// instantiated beneath it, so the mark has to be honoured for ancestors too.
#[test]
fn redeclared_ancestor_makes_a_nested_rank_unproven() {
    let mut flat = connection_test_model();
    for (name, flow) in [
        ("h.a.e", false),
        ("h.a.f", true),
        ("b.e", false),
        ("b.f", true),
    ] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                flow,
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
    }
    let mut overlay = ast::InstanceOverlay::new();
    overlay.type_roots.insert(CONNECTOR_TYPE, CONNECTOR_TYPE);
    let root_id = overlay.alloc_id();
    let holder_id = overlay.alloc_id();
    let nested_id = overlay.alloc_id();
    let b_id = overlay.alloc_id();
    // Only the *enclosing* component carries the redeclare marker.
    overlay
        .add_component(ast::InstanceData {
            had_redeclare: true,
            ..declared_connector(
                holder_id,
                root_id,
                "h",
                Vec::new(),
                Vec::new(),
                OWNING_SCOPE,
            )
        })
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(declared_connector(
            nested_id,
            root_id,
            "h.a",
            Vec::new(),
            Vec::new(),
            "Holder",
        ))
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(declared_connector(
            b_id,
            root_id,
            "b",
            Vec::new(),
            Vec::new(),
            OWNING_SCOPE,
        ))
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_class(root_class(root_id, vec![connect_element("h.a", vec![1])]))
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let result = process_test_connections(&mut flat, &overlay, &mut forest);

    assert!(
        !matches!(
            result,
            Err(FlattenError::SubscriptedDimensionlessConnector { .. })
        ),
        "a rank under a redeclared enclosing declaration is unproven: {result:?}"
    );
}

/// The suppression follows the *path*, not the type: a redeclaration written on
/// one declaration must not excuse a sibling that shares its class.
/// `Holder h; Holder h2(redeclare C a[2]); connect(h.a[1], s)` keeps `h.a`
/// judged, because nothing on `h`'s own path was ever redeclared. Pins the
/// precision of the ancestor walk, which a whole-class or type-keyed marker
/// would lose.
#[test]
fn redeclared_sibling_does_not_make_an_untouched_path_unproven() {
    let mut flat = connection_test_model();
    for (name, flow) in [
        ("h.a.e", false),
        ("h.a.f", true),
        ("h2.a.e", false),
        ("h2.a.f", true),
        ("b.e", false),
        ("b.f", true),
    ] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                flow,
                is_primitive: true,
                source_span: test_span(),
                ..connection_test_variable(test_span())
            },
        );
    }
    let mut overlay = ast::InstanceOverlay::new();
    overlay.type_roots.insert(CONNECTOR_TYPE, CONNECTOR_TYPE);
    let root_id = overlay.alloc_id();
    let h_id = overlay.alloc_id();
    let h_a_id = overlay.alloc_id();
    let h2_id = overlay.alloc_id();
    let h2_a_id = overlay.alloc_id();
    let b_id = overlay.alloc_id();
    overlay
        .add_component(declared_connector(
            h_id,
            root_id,
            "h",
            Vec::new(),
            Vec::new(),
            "Holder",
        ))
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(declared_connector(
            h_a_id,
            root_id,
            "h.a",
            Vec::new(),
            Vec::new(),
            "Holder",
        ))
        .expect("fixture occurrence insertion must succeed");
    // Only the sibling `h2` carries the redeclaration.
    overlay
        .add_component(ast::InstanceData {
            had_redeclare: true,
            ..declared_connector(h2_id, root_id, "h2", Vec::new(), Vec::new(), OWNING_SCOPE)
        })
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(declared_connector(
            h2_a_id,
            root_id,
            "h2.a",
            Vec::new(),
            Vec::new(),
            "Holder",
        ))
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_component(declared_connector(
            b_id,
            root_id,
            "b",
            Vec::new(),
            Vec::new(),
            OWNING_SCOPE,
        ))
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_class(root_class(root_id, vec![connect_element("h.a", vec![1])]))
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("a redeclared sibling proves nothing about this path");

    assert!(
        matches!(
            error,
            FlattenError::SubscriptedDimensionlessConnector { .. }
        ),
        "expected the untouched sibling path to stay judged, got: {error:?}"
    );
}

/// Inheritance alone changes nothing: a component declared in a base class that
/// no redeclaration ever touched keeps an authoritative rank, so the check
/// still judges it. Pins that suppressing the redeclare case did not silently
/// surrender every inherited declaration.
#[test]
fn inherited_but_never_redeclared_rank_is_authoritative_evidence() {
    let mut flat = two_scalar_connectors();
    // Declared in `Base`, connected from `Root`, never redeclared.
    let overlay = overlay_connecting(Vec::new(), Vec::new(), "Base", vec![1]);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("an inherited rank-zero component still proves the subscript impossible");

    assert!(matches!(
        error,
        FlattenError::SubscriptedDimensionlessConnector { .. }
    ));
}

/// Acceptance arm: nothing in view declares the endpoint's base, so this phase
/// has no rank to judge against and must not invent one. Expandable-bus members
/// arrive this way.
#[test]
fn subscripted_base_without_shape_evidence_is_refused_before_generation() {
    let mut flat = two_scalar_connectors();
    let mut overlay = ast::InstanceOverlay::new();
    overlay.type_roots.insert(CONNECTOR_TYPE, CONNECTOR_TYPE);
    let root_id = overlay.alloc_id();
    let b_id = overlay.alloc_id();
    overlay
        .add_component(declared_connector(
            b_id,
            root_id,
            "b",
            Vec::new(),
            Vec::new(),
            OWNING_SCOPE,
        ))
        .expect("fixture occurrence insertion must succeed");
    overlay
        .add_class(root_class(root_id, vec![connect_element("a", vec![1])]))
        .expect("fixture occurrence insertion must succeed");
    let _ = crate::test_support::finalized_test_overlay(&mut overlay);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("connection generation cannot guess the denoted shape");

    assert!(
        matches!(error, FlattenError::InvalidConnectionEvidence { .. }),
        "unexpected refusal: {error:?}"
    );
    assert!(flat.equations.is_empty());
}

/// Every supplied index must consume one retained declaration dimension. An
/// over-subscripted endpoint is refused before connection-set mutation instead
/// of disappearing as an unmatched connection.
#[test]
fn over_subscripted_endpoint_is_refused_before_connection_ir() {
    let mut flat = two_scalar_connectors();
    let overlay = overlay_connecting(vec![2], Vec::new(), OWNING_SCOPE, vec![1, 2]);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_test_connections(&mut flat, &overlay, &mut forest)
        .expect_err("over-subscripting must not survive as an empty connection");

    assert!(
        matches!(error, FlattenError::InvalidConnectionEvidence { .. }),
        "unexpected refusal: {error:?}"
    );
    assert!(flat.equations.is_empty());
}
