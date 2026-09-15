//! Exact scalar and tensor coordinate definitions used by state reduction.

use super::*;

#[test]
fn a_static_one_projection_of_a_singleton_state_payload_is_an_exact_anchor() {
    for subscript in [
        ProjectionSubscript::LiteralOne,
        ProjectionSubscript::BoundParameter,
    ] {
        let model = projected_state_model(1, subscript);
        assert!(
            model.inspect(|view| sort(view).is_err()),
            "the two equal state payloads are singular before demotion"
        );
        model.inspect(|view| {
            let equalities = SystemEqualities::collect(view);
            let x = variable_index(view, "x");
            let q = variable_index(view, "q");
            assert_eq!(
                equalities.anchor_of(x),
                Some((EqualityAnchor::State(q), EqualitySign::Same)),
                "the singleton aggregate wins the construction-capability tie"
            );
            assert_eq!(
                equalities.redundant_states().collect::<Vec<_>>(),
                vec![(x, EqualityAnchor::State(q), EqualitySign::Same)]
            );
        });
        let prepared = prepare_for_solve(&model).expect("singleton projection is reducible");
        let transformed = match prepared {
            PreparedDae::Transformed { dae, .. } => dae,
            PreparedDae::Borrowed { .. } => panic!("singleton projection requires state demotion"),
        };
        assert_eq!(role(&transformed, "x"), dae::VariableRole::Algebraic);
        assert_eq!(role(&transformed, "q"), dae::VariableRole::State);
        transformed
            .inspect(|view| assert!(sort(view).is_ok(), "replacement DAE matches perfectly"));
    }
}

#[test]
fn row_major_vector_pin_demotes_the_complete_state_payload() {
    let model = pinned_vector_state_model();
    let source_error = model
        .inspect(|view| sort(view).map(|_| ()))
        .expect_err("the pinned vector is high-index before demotion");
    assert!(matches!(source_error, StructuralError::Singular { .. }));
    let prepared = prepare_for_solve(&model).expect("the complete vector pin is reducible");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("the pinned vector requires a demotion"),
    };
    assert_eq!(role(&transformed, "q"), dae::VariableRole::Algebraic);
    transformed.inspect(|view| {
        assert!(sort(view).is_ok(), "the rebuilt vector system matches");
        let q = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "q")
            .map(|(_, variable)| variable)
            .expect("the demoted vector survives");
        assert_eq!(q.value_type().dimensions(), [3]);
    });
}

#[test]
fn checked_function_body_proves_vector_state_derivative() {
    let model = function_defined_vector_state_model();
    let source_error = model
        .inspect(|view| sort(view).map(|_| ()))
        .expect_err("the function-defined vector state is high-index before demotion");
    assert!(matches!(source_error, StructuralError::Singular { .. }));
    let prepared = prepare_for_solve(&model).expect("checked call body is differentiable");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("the redundant vector state requires demotion"),
    };
    assert_eq!(role(&transformed, "w"), dae::VariableRole::Algebraic);
    transformed.inspect(|view| {
        assert!(sort(view).is_ok(), "the rebuilt vector system matches");
        let w = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "w")
            .map(|(_, variable)| variable)
            .expect("the demoted vector survives");
        assert_eq!(w.value_type().dimensions(), [3]);
        let spin = view.function(view.function_id(0).unwrap()).unwrap();
        let links = spin.derivatives().collect::<Vec<_>>();
        assert_eq!(
            links.len(),
            1,
            "state demotion must preserve the checked derivative link"
        );
        assert_eq!(links[0].target(), view.function_id(1).unwrap());
        assert_eq!(links[0].tangent_inputs().collect::<Vec<_>>(), [1]);
        assert_eq!(
            links[0].inputs()[0],
            rumoca_core::FunctionDerivativeInput::ZeroDerivative
        );
    });
}

#[test]
fn non_singleton_and_dynamic_projections_fail_closed() {
    for model in [
        projected_state_model(2, ProjectionSubscript::LiteralOne),
        projected_state_model(1, ProjectionSubscript::Parameter),
    ] {
        model.inspect(|view| {
            let equalities = SystemEqualities::collect(view);
            assert!(
                equalities.redundant_states().next().is_none(),
                "a partial or dynamically selected payload proves no state redundancy"
            );
        });
    }
}

#[test]
fn projected_algebraic_definition_differentiates_through_its_proven_state_anchor() {
    let model = projected_algebraic_definition_model();
    let candidate = model.inspect(|view| {
        let x = variable_index(view, "x");
        let candidates =
            direct_state_constraints(view, &constraints::DifferentiationFacts::collect(view));
        candidates
            .admissible
            .into_iter()
            .chain(candidates.conditional)
            .find(|candidate| {
                let crate::dae_transform::StateDefinition::Expression(rhs) = candidate.rhs else {
                    return false;
                };
                let rhs = view.expression_id(rhs as usize).unwrap();
                candidate.state == x
                    && matches!(
                        singleton_real_projection(view, rhs),
                        Some(SingletonRealProjection::Algebraic(_))
                    )
            })
            .expect("invariant projection retains its proven algebraic-to-state substitution")
    });
    let rebuilt = rebuild_with_state_demotion(&model, candidate)
        .expect("the admitted projection differentiates through its exact state anchor");
    assert_eq!(role(&rebuilt, "x"), dae::VariableRole::Algebraic);
    rebuilt.inspect(|view| assert!(sort(view).is_ok()));
}

#[test]
fn an_opposed_pin_proves_constancy_without_naming_the_pinned_value() {
    let model = opposed_pin_model();
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        let (anchor, sign) = equalities
            .anchor_of(variable_index(view, "q"))
            .expect("the opposed pin still proves the class constant");
        assert_eq!(sign, EqualitySign::Same, "a pinned class carries no sign");
        assert!(
            matches!(anchor, EqualityAnchor::Invariant { .. }),
            "`q + I = 0` pins the class to a time-invariant value"
        );
        assert_eq!(
            equalities.anchor_expression(
                view,
                anchor,
                view.variable(
                    view.variable_id(variable_index(view, "q") as usize)
                        .unwrap()
                )
                .unwrap()
                .value_type()
            ),
            None,
            "the class sits at `-I`, which no source expression names"
        );
    });
}
