use super::*;
use rumoca_core::{SourceMap, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName};

fn array_domain() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".to_owned(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    }
}

fn model() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "Real x[2]; Real rhs[2]; discrete Real b; equation rhs={10,b}; der(x)=rhs;";
    let source = sources.add("projected-alias-merge.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |model| {
        let domain = model.domains(|domains| domains.structured(array_domain(), at))?;
        let (real, vector) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::array(dae::ScalarType::Real, [2]),
                    at,
                )?,
            ))
        })?;
        let (x, rhs, b) = model.variables(|vars| {
            Ok((
                vars.state(VarName::new("x"), vector, at, Default::default())?,
                vars.algebraic(VarName::new("rhs"), vector, at, Default::default())?,
                vars.discrete_real(VarName::new("b"), real, at, Default::default())?,
            ))
        })?;
        let rows = model.expressions(|expr| {
            let derivative = expr
                .at(at)
                .coordinate(dae::CoordinateInput::Derivative(x))?;
            for lane in [1, 2] {
                let expression = expr.at(at).literal(dae::DaeLiteral::Integer(lane))?;
                expr.at(at).index(
                    derivative,
                    [dae::Subscript::Index {
                        expression,
                        provenance: at,
                    }],
                )?;
            }
            let rhs = expr
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(rhs))?;
            let b = expr
                .at(at)
                .coordinate(dae::CoordinateInput::DiscreteReal(b))?;
            let ten = expr.at(at).literal(dae::DaeLiteral::Real(10.0))?;
            let value = expr.at(at).array([ten, b])?;
            Ok([
                expr.at(at)
                    .binary(dae::BinaryOperator::Subtract, derivative, rhs)?,
                expr.at(at)
                    .binary(dae::BinaryOperator::Subtract, rhs, value)?,
            ])
        })?;
        model.continuous(|equations| {
            for value in rows {
                equations.structured_family(
                    at,
                    domain,
                    rumoca_core::ComprehensionScalarView::RowMajorProjection,
                    |family| family.body(value),
                )?;
            }
            Ok(())
        })
    })
    .unwrap()
}

fn definitions(view: dae::DaeView<'_>) -> SameTickDefinitions<'_> {
    let (_, state) = super::tests::derivative(view);
    let dae::ContinuousOwnerView::Structured { family, .. } =
        view.continuous_owners().next().unwrap()
    else {
        panic!("whole-array derivative row");
    };
    let plan = crate::CausalDiscretePlan::derive(view).unwrap();
    let body = family.bodies().iter().next().unwrap();
    SameTickDefinitions::derive_with_derivative_sources(
        view,
        &plan,
        (0..2).map(|scalar| MatchedDerivativeSource {
            state,
            state_scalar: scalar,
            expression: body,
            expression_scalar: scalar,
            domain_point: Some((family.domain(), vec![i64::try_from(scalar).unwrap() + 1])),
        }),
    )
    .unwrap()
}

#[test]
fn fused_projected_alias_facts_retain_both_lanes_in_either_merge_order() {
    model().inspect(|view| {
        let definitions = definitions(view);
        let variable = |name| {
            view.variables()
                .find(|(_, v)| v.name().as_str() == name)
                .unwrap()
                .0
                .index()
        };
        let rhs = variable("rhs");
        let b = variable("b");
        let facts = (0..view.expression_count())
            .filter_map(|i| view.expression_id(i))
            .filter(|&id| {
                matches!(
                    view.expression(id).unwrap().operation(),
                    dae::ExpressionOperation::Index { .. }
                )
            })
            .map(|id| {
                let mut collector = definitions.event_read_collector(view);
                collector.expression(id).unwrap();
                collector.finish().unwrap()
            })
            .collect::<Vec<_>>();
        assert_eq!(facts.len(), 2);
        assert_eq!(facts[0].dependencies(rhs), Some(BTreeSet::new()));
        assert_eq!(facts[1].dependencies(rhs), Some(BTreeSet::from([b])));
        for (first, second) in [(&facts[0], &facts[1]), (&facts[1], &facts[0])] {
            let fused = EventReadFacts::merge(first, &[second]);
            assert_eq!(fused.current(), &BTreeSet::from([rhs, b]));
            assert_eq!(fused.dependencies(rhs), Some(BTreeSet::from([b])));
        }
    });
}
