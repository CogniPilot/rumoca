use super::*;
use std::collections::BTreeSet;

#[test]
fn a_range_valued_rate_retains_real_tensor_derivatives() {
    let text = "Real x[3]; equation der(x)=1:3; x[1:2:3]=time*(1:2:3);";
    let mut sources = SourceMap::new();
    let source = sources.add("range_rate.mo", text);
    let at = source_provenance(source, text, text);
    let model = dae::Dae::construct(sources, |model| {
        let vector =
            model.types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let x = model
            .variables(|vars| vars.state(VarName::new("x"), vector, at, Default::default()))?;
        let rows = model.expressions(|e| {
            let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let x = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let time = e.at(at).coordinate(dae::CoordinateInput::Time)?;
            let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let two = e.at(at).literal(dae::DaeLiteral::Integer(2))?;
            let three = e.at(at).literal(dae::DaeLiteral::Integer(3))?;
            let rates = e.at(at).range(one, None, three)?;
            let rate = e.at(at).binary(dae::BinaryOperator::Subtract, dx, rates)?;
            let indices = e.at(at).range(one, Some(two), three)?;
            let selected = e.at(at).index(
                x,
                [dae::Subscript::Slice {
                    expression: indices,
                    provenance: at,
                }],
            )?;
            let position = e
                .at(at)
                .binary(dae::BinaryOperator::Multiply, time, indices)?;
            let constraint = e
                .at(at)
                .binary(dae::BinaryOperator::Subtract, selected, position)?;
            Ok([rate, constraint])
        })?;
        model.continuous(|c| {
            for row in rows {
                c.value_equation(at, row)?;
            }
            Ok(())
        })
    })
    .unwrap();
    let constraint = model.inspect(|view| {
        constraints::holonomic_constraints(view)
            .into_iter()
            .find(|c| c.owner_ordinal == 1)
            .expect("the slice has a position constraint")
    });
    assert_eq!(constraint.proof.maximum_order, 2);
    let (rebuilt, manifold) =
        rebuild_holonomic_constraint(&ReductionSource::new(&model), &constraint, &[]).unwrap();
    assert_eq!(manifold.len(), 2);
    rebuilt.inspect(|view| {
        let dae::ContinuousOwnerView::Structured { family, .. } =
            view.continuous_owners().nth(1).unwrap()
        else {
            panic!("the second derivative replaces the position equation");
        };
        let tangent = view.expression_id(manifold[1].expression as usize).unwrap();
        for row in [tangent, family.bodies().get(0).unwrap()] {
            let ty = view.expression(row).unwrap().value_type();
            assert_eq!(ty.scalar_type(), dae::ScalarType::Real);
            assert_eq!(ty.dimensions(), [2]);
            assert_eq!(
                rumoca_eval_dae::NumericEvaluator::new(view)
                    .expression(row)
                    .unwrap(),
                [0.0, 0.0]
            );
        }
    });
}

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
