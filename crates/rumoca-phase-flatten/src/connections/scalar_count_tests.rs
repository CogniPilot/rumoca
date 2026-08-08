use super::*;

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("connection_scalar_count_tests.mo"),
        1,
        2,
    )
}

fn array_variable(dims: Vec<i64>, flow: bool) -> flat::Variable {
    flat::Variable {
        dims,
        flow,
        ..flat::Variable::empty_with_span(test_span())
    }
}

#[test]
fn equality_generation_rejects_mismatched_array_sizes() {
    let mut flat = flat::Model::new();
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

/// MLS §10.5: `a.v[1]` of a `Real[2,3]` declaration denotes `Real[3]`, so the
/// generated equality covers three scalars. Counting the subscripted endpoint
/// as one scalar leaves the model short of equations (MLS §4.8) and gets a
/// legal model rejected as unbalanced.
#[test]
fn equality_generation_counts_the_leaves_of_an_array_element_endpoint() {
    let mut flat = flat::Model::new();
    let lhs_base = rumoca_core::VarName::new("a.v");
    let rhs_base = rumoca_core::VarName::new("b.v");
    flat.add_variable(lhs_base, array_variable(vec![2, 3], false));
    flat.add_variable(rhs_base, array_variable(vec![2, 3], false));
    let lhs = rumoca_core::VarName::new("a.v[1]");
    let rhs = rumoca_core::VarName::new("b.v[1]");
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    generate_equality_equations(&mut flat, &[lhs, rhs], test_span(), &mut forest)
        .expect("connecting two same-shaped array slices is legal");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(
        flat.equations[0].scalar_count, 3,
        "an element of a Real[2,3] declaration denotes Real[3]"
    );
}

/// The fully subscripted element denotes a scalar, so the count stays 1.
#[test]
fn equality_generation_counts_a_fully_subscripted_element_as_one_scalar() {
    let mut flat = flat::Model::new();
    let lhs_base = rumoca_core::VarName::new("a.v");
    let rhs_base = rumoca_core::VarName::new("b.v");
    flat.add_variable(lhs_base, array_variable(vec![2, 3], false));
    flat.add_variable(rhs_base, array_variable(vec![2, 3], false));
    let lhs = rumoca_core::VarName::new("a.v[1,2]");
    let rhs = rumoca_core::VarName::new("b.v[2,3]");
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    generate_equality_equations(&mut flat, &[lhs, rhs], test_span(), &mut forest)
        .expect("connecting two scalar elements is legal");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(flat.equations[0].scalar_count, 1);
}

/// CONN-008 (MLS §9.2) still rejects endpoints whose denoted shapes differ, and
/// with the same typed error as the whole-declaration mismatch above.
#[test]
fn equality_generation_rejects_array_elements_with_different_leaf_counts() {
    let mut flat = flat::Model::new();
    let lhs_base = rumoca_core::VarName::new("a.v");
    let rhs_base = rumoca_core::VarName::new("b.v");
    flat.add_variable(lhs_base, array_variable(vec![2, 3], false));
    flat.add_variable(rhs_base, array_variable(vec![2, 2], false));
    let lhs = rumoca_core::VarName::new("a.v[1]");
    let rhs = rumoca_core::VarName::new("b.v[1]");
    let mut forest = crate::vcg::OverconstrainedEquationForest::empty();

    let error = generate_equality_equations(&mut flat, &[lhs, rhs], test_span(), &mut forest)
        .expect_err("Real[3] and Real[2] slices are not connection compatible");

    assert!(error.to_string().contains("incompatible connector types"));
    assert!(flat.equations.is_empty());
}

/// The flow sum counts the same leaves. Counting a slice endpoint as one scalar
/// also mis-triggers the mixed scalar/array Kirchhoff collapse below.
#[test]
fn flow_generation_counts_the_leaves_of_an_array_element_endpoint() {
    let mut flat = flat::Model::new();
    let lhs_base = rumoca_core::VarName::new("a.i");
    flat.add_variable(lhs_base, array_variable(vec![2, 3], true));
    let rhs = rumoca_core::VarName::new("b.i");
    flat.add_variable(rhs.clone(), array_variable(vec![3], true));
    let lhs = rumoca_core::VarName::new("a.i[1]");

    generate_flow_equation(
        &mut flat,
        &[lhs, rhs],
        "",
        &IndexMap::default(),
        test_span(),
    )
    .expect("connecting a flow slice to a same-shaped flow array is legal");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(
        flat.equations[0].scalar_count, 3,
        "the flow sum covers the three leaves of the connected slice"
    );
}

/// MLS §10.5 for a subscript that sits on an *inner* path segment. A collapsed
/// connector-array member `a.e` of dims `[2, 3]` is denoted by `a[1].e` as
/// `Real[3]`, so the connection covers three scalars. The embedded-index branch
/// used to answer a constant 1 without consulting any declaration, which is the
/// same defect the trailing-subscript case had.
#[test]
fn embedded_index_endpoints_count_the_leaves_their_base_denotes() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("a.e"),
        array_variable(vec![2, 3], false),
    );

    assert_eq!(
        resolve_var_scalar_count(&flat, &rumoca_core::VarName::new("a[1].e")),
        Some(3),
        "one embedded subscript consumes one leading declared dimension"
    );
}

/// Two embedded subscripts consume two leading dimensions.
#[test]
fn embedded_index_endpoints_consume_one_dimension_per_subscript() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("a.e"),
        array_variable(vec![2, 3, 4], false),
    );

    assert_eq!(
        resolve_var_scalar_count(&flat, &rumoca_core::VarName::new("a[1,2].e")),
        Some(4)
    );
}

/// When the subscripts select every declared dimension away — or the collapsed
/// declaration lost its dimensions, which this phase cannot tell apart — the
/// member denotes one scalar, exactly as before.
#[test]
fn embedded_index_endpoints_denote_a_scalar_once_every_dimension_is_selected() {
    let mut flat = flat::Model::new();
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
        Some(1)
    );
}

/// No declaration for the index-free path is no evidence, not one scalar:
/// callers distinguish the two, and a fabricated 1 collapses a mixed
/// scalar/array flow set onto a single Kirchhoff equation.
#[test]
fn embedded_index_endpoints_without_a_declaration_are_unknown() {
    let flat = flat::Model::new();

    assert_eq!(
        resolve_var_scalar_count(&flat, &rumoca_core::VarName::new("a[1].e")),
        None
    );
}

/// A flow set whose member carries an embedded index keeps the leaf count of
/// what that member denotes, instead of shrinking the sum to one scalar.
#[test]
fn flow_generation_counts_the_leaves_of_an_embedded_index_endpoint() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("a.i"),
        array_variable(vec![2, 3], true),
    );
    let rhs = rumoca_core::VarName::new("b.i");
    flat.add_variable(rhs.clone(), array_variable(vec![3], true));
    let lhs = rumoca_core::VarName::new("a[1].i");

    generate_flow_equation(
        &mut flat,
        &[lhs, rhs],
        "",
        &IndexMap::default(),
        test_span(),
    )
    .expect("connecting a collapsed member element to a same-shaped flow array is legal");

    assert_eq!(flat.equations.len(), 1);
    assert_eq!(
        flat.equations[0].scalar_count, 3,
        "`a[1].i` of a collapsed `a.i` with dims [2, 3] denotes Real[3]"
    );
}

#[test]
fn flow_generation_rejects_multiple_array_sizes() {
    let mut flat = flat::Model::new();
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
