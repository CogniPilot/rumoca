//! MLS §10.5 / §9.1 endpoint-subscript admissibility for `connect` arguments.
//!
//! These tests pin every arm of the acceptance contract stated in the module
//! docs — a subscripted endpoint that names a declared occurrence, one whose
//! base has no declaration in view, one whose declaration has any dimension
//! (including the over-subscripted case this check does not judge), one whose
//! declaration still carries dimension expressions, and one whose visible rank
//! is not authoritative because a redeclaration was consumed for it or for an
//! enclosing declaration — plus the single provable violation, which is
//! reported against both the connect endpoint and the declaration site and
//! never silently dropped onto the whole component.

use super::*;

/// Lexical scope of the class that declares the components and writes the
/// connection.
const OWNING_SCOPE: &str = "Root";

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("connection_endpoint_subscript_tests.mo"),
        40,
        56,
    )
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
    let mut flat = flat::Model::new();
    for (name, flow) in [("a.e", false), ("a.f", true), ("b.e", false), ("b.f", true)] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                flow,
                is_primitive: true,
                source_span: test_span(),
                ..flat::Variable::empty_with_span(test_span())
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
    instance_id: u32,
    name: &str,
    dims: Vec<i64>,
    dims_expr: Vec<ast::Subscript>,
    declaration_scope: &str,
) -> ast::InstanceData {
    ast::InstanceData {
        instance_id: rumoca_core::InstanceId(instance_id),
        qualified_name: ast::QualifiedName::from_dotted(name),
        owner_class_id: Some(rumoca_core::InstanceId(0)),
        declaration_source_scope: Some(ast::QualifiedName::from_ident(declaration_scope)),
        source_location: declaration_location(),
        dims,
        dims_expr,
        is_connector_type: true,
        ..Default::default()
    }
}

/// Root class instance owning `connections`, declared in [`OWNING_SCOPE`].
fn root_class(connections: Vec<ast::InstanceConnection>) -> ast::ClassInstanceData {
    ast::ClassInstanceData {
        instance_id: rumoca_core::InstanceId(0),
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
    ast::InstanceConnection {
        a,
        b: ast::QualifiedName::from_ident("b"),
        connector_type: None,
        span: test_span(),
        scope: String::new(),
        family: None,
    }
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
    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_component(declared_connector(
        1,
        "a",
        dims,
        dims_expr,
        declaration_scope,
    ));
    overlay.add_component(declared_connector(
        2,
        "b",
        Vec::new(),
        Vec::new(),
        OWNING_SCOPE,
    ));
    overlay.add_class(root_class(vec![connect_element("a", subscripts)]));
    overlay
}

#[test]
fn connect_subscript_on_a_dimensionless_declaration_is_rejected() {
    let mut flat = two_scalar_connectors();
    let overlay = overlay_connecting_element_of(Vec::new(), Vec::new());
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_connections(&mut flat, &overlay, false, &mut forest)
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

    let error = process_connections(&mut flat, &overlay, false, &mut forest)
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

    let _ = process_connections(&mut flat, &overlay, false, &mut forest)
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
        flat.variables.values().all(|variable| !variable.connected),
        "the rejected connection must not have marked members connected"
    );
}

#[test]
fn connect_subscript_within_the_declared_rank_is_accepted() {
    let mut flat = two_scalar_connectors();
    let overlay = overlay_connecting_element_of(vec![2], Vec::new());
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    process_connections(&mut flat, &overlay, false, &mut forest)
        .expect("an element of a declared connector array is a legal connect argument");
}

/// Acceptance before rejection: a declaration that still carries a dimension
/// expression has not proven a rank of zero, so its element connections stay
/// admissible.
#[test]
fn connect_subscript_on_a_symbolically_dimensioned_declaration_is_accepted() {
    let mut flat = two_scalar_connectors();
    let overlay = overlay_connecting_element_of(
        Vec::new(),
        vec![ast::Subscript::Expression(ast::Expression::Empty {
            span: test_span(),
        })],
    );
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    process_connections(&mut flat, &overlay, false, &mut forest)
        .expect("an unevaluated dimension has not proven the declaration to be dimensionless");
}

/// An endpoint that names a declared element occurrence (the scalarized
/// representation of a connector array) carries its subscript legitimately.
#[test]
fn connect_subscript_naming_a_declared_element_occurrence_is_accepted() {
    let mut flat = two_scalar_connectors();
    let mut overlay = overlay_connecting_element_of(Vec::new(), Vec::new());
    overlay.add_component(ast::InstanceData {
        instance_id: rumoca_core::InstanceId(3),
        qualified_name: ast::QualifiedName {
            parts: vec![("a".to_string(), vec![1])],
        },
        source_location: declaration_location(),
        is_connector_type: true,
        ..Default::default()
    });
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    process_connections(&mut flat, &overlay, false, &mut forest)
        .expect("a declared element occurrence is a legal connect argument");
}

/// MLS §7.3 lets a redeclaration add dimensions to a `replaceable C a;`.
/// Instantiation keeps only the redeclared type, so the component reaches this
/// phase carrying the original declaration's rank of zero. That zero is
/// evidence about rumoca, not about the model — OMC accepts the same source —
/// so it must not be reported as a user error.
#[test]
fn redeclared_component_rank_is_not_authoritative_evidence() {
    let mut flat = two_scalar_connectors();
    let mut overlay = overlay_connecting_element_of(Vec::new(), Vec::new());
    overlay.add_component(ast::InstanceData {
        had_redeclare: true,
        ..declared_connector(1, "a", Vec::new(), Vec::new(), OWNING_SCOPE)
    });
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let result = process_connections(&mut flat, &overlay, false, &mut forest);

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
    let mut flat = flat::Model::new();
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
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    let mut overlay = ast::InstanceOverlay::new();
    // Only the *enclosing* component carries the redeclare marker.
    overlay.add_component(ast::InstanceData {
        had_redeclare: true,
        ..declared_connector(1, "h", Vec::new(), Vec::new(), OWNING_SCOPE)
    });
    overlay.add_component(declared_connector(
        2,
        "h.a",
        Vec::new(),
        Vec::new(),
        "Holder",
    ));
    overlay.add_component(declared_connector(
        3,
        "b",
        Vec::new(),
        Vec::new(),
        OWNING_SCOPE,
    ));
    overlay.add_class(root_class(vec![connect_element("h.a", vec![1])]));
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let result = process_connections(&mut flat, &overlay, false, &mut forest);

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
    let mut flat = flat::Model::new();
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
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_component(declared_connector(1, "h", Vec::new(), Vec::new(), "Holder"));
    overlay.add_component(declared_connector(
        2,
        "h.a",
        Vec::new(),
        Vec::new(),
        "Holder",
    ));
    // Only the sibling `h2` carries the redeclaration.
    overlay.add_component(ast::InstanceData {
        had_redeclare: true,
        ..declared_connector(3, "h2", Vec::new(), Vec::new(), OWNING_SCOPE)
    });
    overlay.add_component(declared_connector(
        4,
        "h2.a",
        Vec::new(),
        Vec::new(),
        "Holder",
    ));
    overlay.add_component(declared_connector(
        5,
        "b",
        Vec::new(),
        Vec::new(),
        OWNING_SCOPE,
    ));
    overlay.add_class(root_class(vec![connect_element("h.a", vec![1])]));
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = process_connections(&mut flat, &overlay, false, &mut forest)
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

    let error = process_connections(&mut flat, &overlay, false, &mut forest)
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
fn connect_subscript_on_a_base_with_no_declaration_in_view_is_accepted() {
    let mut flat = two_scalar_connectors();
    let mut overlay = ast::InstanceOverlay::new();
    overlay.add_component(declared_connector(
        2,
        "b",
        Vec::new(),
        Vec::new(),
        OWNING_SCOPE,
    ));
    overlay.add_class(root_class(vec![connect_element("a", vec![1])]));
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    process_connections(&mut flat, &overlay, false, &mut forest)
        .expect("an endpoint whose base has no declaration in view is not this check's business");
}

/// Acceptance arm the shipped predicate really has: any non-zero declared rank
/// is accepted, including an over-subscripted endpoint. This check deliberately
/// does not judge that shape — the flat representation of a collapsed
/// connector-array member is indistinguishable from it — so the endpoint simply
/// matches nothing here and the model is caught downstream as an `E011`
/// structural singularity (task #82). Typecheck's `ET009` (MLS §10.5.1) is the
/// natural owner once it walks connect arguments.
#[test]
fn over_subscripted_endpoint_on_a_declared_array_is_not_judged_here() {
    let mut flat = two_scalar_connectors();
    let overlay = overlay_connecting(vec![2], Vec::new(), OWNING_SCOPE, vec![1, 2]);
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    process_connections(&mut flat, &overlay, false, &mut forest)
        .expect("over-subscripting a declared array is not rejected by this check");
}
