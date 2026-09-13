use super::*;

#[test]
fn tensor_linear_map_proof_does_not_enumerate_the_unknown_extent() {
    let counts = [3, 4096].map(|extent| {
        let model = linear_map_model(extent, false);
        model.inspect(|view| {
            let facts = constraints::DifferentiationFacts::collect(view);
            let block = facts.auxiliary_blocks[2].as_ref().expect("linear rate map");
            assert_eq!(block.extent, extent);
            assert_eq!(&*block.state_anchors, &[0, 1]);
            let dae::ContinuousOwnerView::Structured { family, .. } =
                view.continuous_owner(0).unwrap()
            else {
                panic!("whole tensor equation");
            };
            assert!(block.contains_residual(family.bodies().get(0).unwrap().index()));
            assert_eq!(block.operands().count(), 2);
            assert_eq!(block.coefficient_node_count(), 3);
            (
                view.expression_count(),
                view.variable_count(),
                view.continuous_owners().count(),
            )
        })
    });
    assert_eq!(counts[0], counts[1]);
}

#[test]
fn tensor_linear_map_refuses_unknown_dependent_coefficients() {
    let model = linear_map_model(3, true);
    model.inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        assert!(facts.auxiliary_blocks.iter().all(Option::is_none));
    });
}

fn linear_map_model(extent: u32, nonlinear: bool) -> dae::Dae {
    let equation = if nonlinear {
        "omega = rate[1] * rate;"
    } else {
        "omega = theta * rate;"
    };
    let text =
        format!("Real omega[{extent}]; Real theta; Real rate[{extent}]; equation {equation}");
    let mut sources = SourceMap::new();
    let source = sources.add("linear_map.mo", &text);
    let at = source_provenance(source, &text, equation);
    dae::Dae::construct(sources, |model| {
        let (scalar, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)?,
            ))
        })?;
        let omega =
            model.variables(|v| v.state(VarName::new("omega"), vector, at, Default::default()))?;
        let theta =
            model.variables(|v| v.state(VarName::new("theta"), scalar, at, Default::default()))?;
        let rate = model
            .variables(|v| v.algebraic(VarName::new("rate"), vector, at, Default::default()))?;
        let residual = model.expressions(|expressions| {
            let omega = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(omega))?;
            let theta = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(theta))?;
            let rate = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(rate))?;
            let map = if nonlinear {
                let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
                let first = expressions.at(at).index(
                    rate,
                    [dae::Subscript::Index {
                        expression: one,
                        provenance: at,
                    }],
                )?;
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Multiply, first, rate)?
            } else {
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Multiply, theta, rate)?
            };
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, omega, map)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap()
}
