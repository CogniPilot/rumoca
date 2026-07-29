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
