use super::*;
use std::collections::BTreeSet;

#[test]
fn independent_rate_values_retain_their_state_relation() {
    let counts = [1, 3, 4096].map(|extent| {
        let model = rate_model(extent, RateDefinition::Independent);
        let source = ReductionSource::new(&model);
        let constraint = source.inspect(|view, facts| {
            constraints::index_reduction_constraints(view, facts)
                .into_iter()
                .find(|candidate| candidate.lifted_algebraic == Some(2))
                .expect("q=2*der(x) can replay the independent der(x)=v equation")
        });
        let (rebuilt, manifold) = rebuild_holonomic_constraint(&source, &constraint, &[]).unwrap();
        assert_eq!(manifold.len(), 1);
        rebuilt.inspect(|view| {
            assert_eq!(
                retained_states(view, manifold[0].expression),
                BTreeSet::from([1, 2])
            );
            assert_eq!(view.continuous_owners().count(), 3);
            (view.expression_count(), view.variable_count())
        })
    });
    assert!(counts.windows(2).all(|pair| pair[0] == pair[1]));
}

#[test]
fn a_rate_definition_cannot_supply_its_own_holonomic_replacement() {
    for extent in [1, 3, 4096] {
        rate_model(extent, RateDefinition::Independent).inspect(|view| {
            let facts = constraints::DifferentiationFacts::collect(view);
            assert!(
                constraints::index_reduction_constraints(view, &facts)
                    .iter()
                    .all(|candidate| candidate.owner_ordinal >= 2),
                "der(x)=v and der(v)=-x remain authoritative dynamics equations"
            );
        });
    }
}

#[test]
fn missing_cyclic_and_duplicate_rate_definitions_cannot_supply_values() {
    for definition in [
        RateDefinition::Missing,
        RateDefinition::Cyclic,
        RateDefinition::Duplicate,
    ] {
        rate_model(3, definition).inspect(|view| {
            let facts = constraints::DifferentiationFacts::collect(view);
            assert!(
                constraints::index_reduction_constraints(view, &facts)
                    .iter()
                    .all(|candidate| candidate.lifted_algebraic != Some(2)),
                "{definition:?} cannot prove a rate value"
            );
        });
    }
}

fn retained_states(view: dae::DaeView<'_>, expression: u32) -> BTreeSet<u32> {
    let mut states = BTreeSet::new();
    dae::ExpressionTraversal::new().visit_pruned(
        view,
        [view.expression_id(expression as usize).unwrap()],
        |_, node| {
            match node.operation() {
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) => {
                    states.insert(state.index());
                }
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(_)) => {
                    panic!("a retained value relation must replay the rate's independent value");
                }
                _ => {}
            }
            true
        },
    );
    states
}

#[derive(Clone, Copy, Debug)]
enum RateDefinition {
    Independent,
    Missing,
    Cyclic,
    Duplicate,
}

fn rate_model(extent: u32, definition: RateDefinition) -> dae::Dae {
    let rate = match definition {
        RateDefinition::Independent => "der(x)=v;",
        RateDefinition::Missing => "",
        RateDefinition::Cyclic => "der(x)=2*der(x);",
        RateDefinition::Duplicate => "der(x)=v; der(x)=-x;",
    };
    let text = format!(
        "model RateValue parameter Integer n={extent}; Real x[n],v[n],q[n];
        equation {rate} der(v)=-x; q=2*der(x); end RateValue;"
    );
    let mut sources = SourceMap::new();
    let source = sources.add("rate_value.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at))?;
        let (x, v, q) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("x"), vector, at, Default::default())?,
                variables.state(VarName::new("v"), vector, at, Default::default())?,
                variables.algebraic(VarName::new("q"), vector, at, Default::default())?,
            ))
        })?;
        let rows = model.expressions(|e| {
            let position = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let value = e.at(at).coordinate(dae::CoordinateInput::State(v))?;
            let q = e.at(at).coordinate(dae::CoordinateInput::Algebraic(q))?;
            let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let dv = e.at(at).coordinate(dae::CoordinateInput::Derivative(v))?;
            let two = e.at(at).literal(dae::DaeLiteral::Real(2.0))?;
            let twice_dx = e.at(at).binary(dae::BinaryOperator::Multiply, two, dx)?;
            let negative_position = e.at(at).unary(dae::UnaryOperator::Negate, position)?;
            let mut rows = Vec::new();
            let rhs = if matches!(definition, RateDefinition::Cyclic) {
                twice_dx
            } else {
                value
            };
            if !matches!(definition, RateDefinition::Missing) {
                rows.push(e.at(at).binary(dae::BinaryOperator::Subtract, dx, rhs)?);
            }
            if matches!(definition, RateDefinition::Duplicate) {
                rows.push(
                    e.at(at)
                        .binary(dae::BinaryOperator::Subtract, dx, negative_position)?,
                );
            }
            rows.push(
                e.at(at)
                    .binary(dae::BinaryOperator::Subtract, dv, negative_position)?,
            );
            rows.push(
                e.at(at)
                    .binary(dae::BinaryOperator::Subtract, q, twice_dx)?,
            );
            Ok(rows)
        })?;
        model.continuous(|c| {
            for row in rows {
                c.value_equation(at, row)?;
            }
            Ok(())
        })
    })
    .unwrap()
}
