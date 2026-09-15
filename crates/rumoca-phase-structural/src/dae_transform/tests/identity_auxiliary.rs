use super::*;

#[test]
fn identity_auxiliary_maps_keep_exact_state_and_derivative_aliases() {
    let mut counts = Vec::new();
    for extent in [1, 3, 4096] {
        let model = identity_map_model(extent, false);
        let source = ReductionSource::new(&model);
        let candidate = source.inspect(|view, facts| {
            assert!(facts.auxiliary_blocks[2].is_some());
            constraints::index_reduction_constraints(view, facts)
                .into_iter()
                .find(|candidate| candidate.owner_ordinal == 1)
                .expect("independent constraint through the auxiliary vector")
        });
        let (rebuilt, manifold) =
            reconstruction::rebuild_holonomic_constraint(&source, &candidate, &[]).unwrap();
        assert_eq!(manifold.len(), 1);
        rebuilt.inspect(|view| {
            let retained = view.expression_id(manifold[0].expression as usize).unwrap();
            let dae::ContinuousOwnerView::Residual { equation, .. } =
                view.continuous_owner(1).unwrap()
            else {
                panic!("source norm constraint is scalar");
            };
            let mut calls = 0;
            dae::ExpressionTraversal::new().visit_pruned(
                view,
                [retained, equation.residual()],
                |_, node| {
                    calls += usize::from(matches!(
                        node.operation(),
                        dae::ExpressionOperation::Call { .. }
                    ));
                    true
                },
            );
            assert_eq!(
                calls, 0,
                "an identity reconstruction must expose x and der(x) directly"
            );
            counts.push((
                view.expression_count(),
                view.variable_count(),
                view.function_count(),
            ));
        });
    }
    assert!(
        counts.windows(2).all(|pair| pair[0] == pair[1]),
        "identity reconstruction must retain compact tensor owners: {counts:?}"
    );
}

#[test]
fn parameter_scaled_auxiliary_maps_retain_the_checked_solve() {
    let model = identity_map_model(3, true);
    let source = ReductionSource::new(&model);
    let candidate = source.inspect(|view, facts| {
        constraints::index_reduction_constraints(view, facts)
            .into_iter()
            .find(|candidate| candidate.owner_ordinal == 1)
            .unwrap()
    });
    let (rebuilt, manifold) =
        reconstruction::rebuild_holonomic_constraint(&source, &candidate, &[]).unwrap();
    rebuilt.inspect(|view| {
        let mut saw_call = false;
        dae::ExpressionTraversal::new().visit_pruned(
            view,
            [view.expression_id(manifold[0].expression as usize).unwrap()],
            |_, node| {
                saw_call |= matches!(node.operation(), dae::ExpressionOperation::Call { .. });
                true
            },
        );
        assert!(
            saw_call,
            "a parameter-dependent coefficient is not the identity"
        );
    });
}

fn identity_map_model(extent: u32, scaled: bool) -> dae::Dae {
    let map = if scaled {
        "theta*q=x"
    } else {
        "q+theta*zeros(n)=x"
    };
    let text = format!(
        "parameter Integer n={extent}; Real x[n]; parameter Real theta=1; Real q[n]; Real z[n]; equation {map}; q*q+z*z=1;"
    );
    let mut sources = SourceMap::new();
    let source = sources.add("identity_auxiliary.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let (scalar, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)?,
            ))
        })?;
        let one = model.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(1.0)))?;
        let x = model.variables(|v| v.state(VarName::new("x"), vector, at, Default::default()))?;
        let theta = model.variables(|v| {
            v.parameter(
                VarName::new("theta"),
                scalar,
                at,
                dae::VariableAttributes {
                    binding: Some(one),
                    ..Default::default()
                },
            )
        })?;
        let q =
            model.variables(|v| v.algebraic(VarName::new("q"), vector, at, Default::default()))?;
        let z = model.variables(|v| v.state(VarName::new("z"), vector, at, Default::default()))?;
        let rows = model.expressions(|e| {
            let x = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let theta = e
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(theta))?;
            let q = e.at(at).coordinate(dae::CoordinateInput::Algebraic(q))?;
            let z = e.at(at).coordinate(dae::CoordinateInput::State(z))?;
            let n = e
                .at(at)
                .literal(dae::DaeLiteral::Integer(i64::from(extent)))?;
            let zero_vector = e.at(at).builtin(dae::PureBuiltin::Zeros, [n])?;
            let coefficient = e.at(at).binary(
                dae::BinaryOperator::Multiply,
                theta,
                if scaled { q } else { zero_vector },
            )?;
            let map = if scaled {
                coefficient
            } else {
                e.at(at).binary(dae::BinaryOperator::Add, q, coefficient)?
            };
            let definition = e.at(at).binary(dae::BinaryOperator::Subtract, map, x)?;
            let q_norm = e.at(at).binary(dae::BinaryOperator::Multiply, q, q)?;
            let z_norm = e.at(at).binary(dae::BinaryOperator::Multiply, z, z)?;
            let sum = e.at(at).binary(dae::BinaryOperator::Add, q_norm, z_norm)?;
            let constraint = e.at(at).binary(dae::BinaryOperator::Subtract, sum, one)?;
            Ok([definition, constraint])
        })?;
        model.continuous(|c| {
            c.value_equation(at, rows[0])?;
            c.value_equation(at, rows[1])?;
            Ok(())
        })
    })
    .unwrap()
}
