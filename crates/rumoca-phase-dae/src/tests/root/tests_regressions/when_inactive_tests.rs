use super::*;

#[test]
fn test_todae_inactive_ordinary_when_equation_uses_pre_during_initialization() {
    let mut flat = Model::new();
    for name in ["trigger", "y"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                is_discrete_type: true,
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let mut when_clause =
        flat::WhenClause::new(make_var_ref("trigger"), crate::test_support::test_span());
    when_clause.add_equation(flat::WhenEquation::assign(
        VarName::new("y"),
        Expression::Literal {
            value: Literal::Boolean(true),
            span: crate::test_support::test_span(),
        },
        crate::test_support::test_span(),
        "ordinary when assignment",
    ));
    flat.when_clauses.push(when_clause);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("ordinary when-equation should lower to a discrete update");

    let eq = dae
        .discrete
        .valued_updates
        .iter()
        .find(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "y"))
        .expect("expected guarded ordinary when update for y");
    let rumoca_core::Expression::If { else_branch, .. } = &eq.rhs else {
        panic!("expected guarded ordinary when update to lower to an If expression");
    };
    let rumoca_core::Expression::VarRef { name, .. } = else_branch.as_ref() else {
        panic!("inactive ordinary when branch must be a __pre__.* parameter reference");
    };
    assert!(
        name.as_str().starts_with("__pre__."),
        "expected __pre__.* reference, got {}",
        name.as_str(),
    );
}
