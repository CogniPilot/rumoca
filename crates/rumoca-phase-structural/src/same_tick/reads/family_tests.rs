use super::*;
use rumoca_core::{SourceMap, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName};

fn family_model(binder_read: bool) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "Real x[2]; Real a[2]; equation for i in 1:2 loop a[i]=der(x[i]); end for;";
    let source = sources.add("matched-family.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |model| {
        let vector = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [2]),
                at,
            )
        })?;
        let domain = model.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        provenance: rumoca_core::IndexBinderProvenance::Generated,
                        id: 0,
                        display_name: "i".to_owned(),
                        lower: 1,
                        upper: 2,
                        step: 1,
                    }],
                },
                at,
            )
        })?;
        let binder = model.domains(|domains| domains.binder(domain, 0, at))?;
        let (state, target) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("x"), vector, at, Default::default())?,
                variables.algebraic(VarName::new("a"), vector, at, Default::default())?,
            ))
        })?;
        let body = model.expressions(|expressions| {
            let target = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(target))?;
            let mut derivative = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            if binder_read {
                let index = expressions.at(at).binder(binder)?;
                let subscript = dae::Subscript::Index {
                    expression: index,
                    provenance: at,
                };
                derivative = expressions.at(at).index(derivative, [subscript])?;
                derivative = expressions.at(at).array([derivative, derivative])?;
            }
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, target, derivative)
        })?;
        model.continuous(|rows| {
            rows.structured_family(
                at,
                domain,
                rumoca_core::ComprehensionScalarView::RowMajorProjection,
                |family| family.body(body),
            )?;
            Ok(())
        })
    })
    .unwrap()
}

fn matched_sources<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
    count: usize,
) -> Vec<MatchedDerivativeSource<'dae>> {
    let state = super::tests::derivative(view).1;
    let body = family.bodies().iter().next().unwrap();
    (0..count)
        .map(|scalar| MatchedDerivativeSource {
            state,
            state_scalar: scalar,
            expression: body,
            expression_scalar: scalar,
            domain_point: Some((family.domain(), vec![i64::try_from(scalar).unwrap() + 1])),
        })
        .collect()
}

fn family(view: dae::DaeView<'_>) -> dae::StructuredFamilyView<'_> {
    let dae::ContinuousOwnerView::Structured { family, .. } =
        view.continuous_owners().next().unwrap()
    else {
        panic!("structured source");
    };
    family
}

#[test]
fn partial_derivative_matching_does_not_discard_a_structured_algebraic_writer() {
    family_model(false).inspect(|view| {
        let family = family(view);
        let body = family.bodies().iter().next().unwrap();
        let sources = matched_sources(view, family, 1);
        let indexed = sources
            .iter()
            .cloned()
            .map(|source| ((source.state.index(), source.state_scalar), source))
            .collect();
        assert_eq!(
            crate::causal_definitions::exact_row_major_family_body(view, family),
            Some(body)
        );
        assert!(!complete_derivative_residuals(view, &indexed).contains(&body));
        let plan = crate::CausalDiscretePlan::derive(view).unwrap();
        assert!(matches!(
            SameTickDefinitions::derive_with_derivative_sources(view, &plan, sources),
            Err(SameTickOrderError::MissingDerivative { .. })
        ));
    });
}

#[test]
fn binder_bearing_families_do_not_issue_whole_definition_refresh() {
    family_model(true).inspect(|view| {
        let family = family(view);
        let body = family.bodies().iter().next().unwrap();
        let sources = matched_sources(view, family, 2);
        let indexed = sources
            .iter()
            .cloned()
            .map(|source| ((source.state.index(), source.state_scalar), source))
            .collect();
        assert!(view.expression(body).unwrap().binder_domain().is_some());
        assert!(crate::causal_definitions::exact_row_major_family_body(view, family).is_none());
        assert!(!complete_derivative_residuals(view, &indexed).contains(&body));
        let plan = crate::CausalDiscretePlan::derive(view).unwrap();
        let definitions =
            SameTickDefinitions::derive_with_derivative_sources(view, &plan, sources).unwrap();
        let target = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "a")
            .unwrap()
            .0;
        assert!(definitions.definition(target.index()).is_none());
    });
}
