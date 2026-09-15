//! A derivative anchor must not make a value definition an independent constraint.

use super::*;

fn anchored_connector(dimensions: Vec<u32>, opposite: bool) -> dae::Dae {
    connector_with_return_equation(dimensions, opposite, false, true)
}

fn connector_with_return_equation(
    dimensions: Vec<u32>,
    opposite: bool,
    round_trip: bool,
    with_displacement: bool,
) -> dae::Dae {
    let (text, product_value) =
        connector_text(&dimensions, opposite, round_trip, with_displacement);
    let mut sources = SourceMap::new();
    let source = sources.add("anchored_connector.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let ty = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, dimensions), at)
        })?;
        let (length, position, relative, port, forwarded) = model.variables(|variables| {
            Ok((
                variables.parameter(VarName::new("L"), ty, at, Default::default())?,
                variables.state(VarName::new("p"), ty, at, Default::default())?,
                variables.state(
                    VarName::new("s"),
                    ty,
                    at,
                    dae::VariableAttributes {
                        state_select: rumoca_core::StateSelect::Prefer,
                        ..Default::default()
                    },
                )?,
                variables.algebraic(VarName::new("q"), ty, at, Default::default())?,
                variables.algebraic(VarName::new("r"), ty, at, Default::default())?,
            ))
        })?;
        let equations = model.expressions(|expressions| {
            let l = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(length))?;
            let p = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(position))?;
            let s = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(relative))?;
            let q = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(port))?;
            let r = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(forwarded))?;
            let signed = if opposite {
                expressions.at(at).unary(dae::UnaryOperator::Negate, q)?
            } else {
                q
            };
            let alias = if round_trip {
                expressions.at(at).binary(dae::BinaryOperator::Add, r, p)?
            } else {
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, p, signed)?
            };
            let distance = expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, l, q)?;
            let balance = expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, s, distance)?;
            let product = expressions
                .at(at)
                .binary(dae::BinaryOperator::Multiply, p, q)?;
            let one = expressions
                .at(at)
                .literal(dae::DaeLiteral::Real(product_value))?;
            let independent =
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, product, one)?;
            let forward = expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, r, q)?;
            Ok([alias, balance, independent, forward])
        })?;
        model.continuous(|continuous| {
            let retained = equations
                .into_iter()
                .enumerate()
                .filter(|(index, _)| *index != 1 || with_displacement);
            for (_, equation) in retained {
                continuous.value_equation(at, equation)?;
            }
            Ok(())
        })?;
        add_kinematics(model, ty, position, relative, at)
    })
    .expect("source connector and offset equations construct")
}

fn connector_text(
    dimensions: &[u32],
    opposite: bool,
    round_trip: bool,
    with_displacement: bool,
) -> (String, f64) {
    let shape = if dimensions.is_empty() {
        String::new()
    } else {
        format!("{dimensions:?}")
    };
    let sign = if opposite { "-" } else { "" };
    let product_value = if round_trip {
        -f64::from(dimensions.iter().product::<u32>())
    } else {
        1.0
    };
    let balance_text = if with_displacement { "s=L-q;" } else { "" };
    let zero_text = if dimensions.is_empty() {
        "0".to_owned()
    } else {
        format!("zeros({})", dimensions[0])
    };
    let alias_text = if round_trip {
        format!("r+p={zero_text}")
    } else {
        format!("p={sign}q")
    };
    let text = format!(
        "parameter Real L{shape}; Real p{shape}; Real s{shape}(stateSelect=StateSelect.prefer); Real q{shape}; Real r{shape}; Real u{shape}; Real v{shape}; Real a{shape}; Real b{shape}; equation {alias_text}; {balance_text} p*q={product_value}; r=q; der(p)=u; der(s)=v; der(u)=a; der(v)=b;"
    );
    (text, product_value)
}

fn add_kinematics<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    ty: dae::ValueTypeId<'dae>,
    position: dae::StateId<'dae>,
    relative: dae::StateId<'dae>,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let (u, v, a, b) = model.variables(|variables| {
        Ok((
            variables.state(VarName::new("u"), ty, at, Default::default())?,
            variables.state(VarName::new("v"), ty, at, Default::default())?,
            variables.algebraic(VarName::new("a"), ty, at, Default::default())?,
            variables.algebraic(VarName::new("b"), ty, at, Default::default())?,
        ))
    })?;
    for (state, rhs) in [
        (position, dae::CoordinateInput::State(u)),
        (relative, dae::CoordinateInput::State(v)),
        (u, dae::CoordinateInput::Algebraic(a)),
        (v, dae::CoordinateInput::Algebraic(b)),
    ] {
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let rhs = expressions.at(at).coordinate(rhs)?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, derivative, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))?;
    }
    Ok(())
}

#[test]
fn value_aliases_cannot_supply_their_own_holonomic_replacement() {
    for (dimensions, opposite) in [
        (vec![], false),
        (vec![], true),
        (vec![3], false),
        (vec![3], true),
    ] {
        anchored_connector(dimensions, opposite).inspect(|view| {
            let candidates = constraints::holonomic_constraints(view);
            assert!(
                candidates
                    .iter()
                    .all(|candidate| candidate.owner_ordinal != 0),
                "the value substitution cancels p=+/-q despite distinct derivative anchors"
            );
            assert!(
                candidates
                    .iter()
                    .any(|candidate| candidate.owner_ordinal == 2),
                "p*q=1 is an independent constraint on the source states"
            );
        });
    }
}

#[test]
fn preferred_displaced_state_can_use_an_independent_exact_derivative() {
    for opposite in [false, true] {
        let model = anchored_connector(vec![], opposite);
        let candidate = model.inspect(|view| {
            let facts = constraints::DifferentiationFacts::collect(view);
            let candidates = constraints::direct_state_constraints(view, &facts);
            let relative = view
                .variables()
                .find(|(_, variable)| variable.name().as_str() == "s")
                .unwrap()
                .0
                .index();
            candidates.admissible.into_iter()
                .find(|candidate| candidate.state == relative)
                .expect("s=L-q can differentiate q through independent p even when s anchors the affine class")
        });
        let rebuilt = rebuild_with_state_demotion(&model, candidate)
            .expect("the independent derivative proof reconstructs");
        rebuilt.inspect(|view| {
            let u = view
                .variables()
                .find(|(_, variable)| variable.name().as_str() == "u")
                .unwrap()
                .0;
            let dae::ContinuousOwnerView::Residual { equation, .. } =
                view.continuous_owners().nth(5).unwrap()
            else {
                panic!("the source velocity equation is retained");
            };
            assert!(
                dae::expr_contains_var(view, equation.residual(), u),
                "der(s) is replaced through the independent position's velocity"
            );
        });
    }
}

#[test]
fn retained_tangent_replays_its_exact_state_anchor_proof() {
    for (dimensions, opposite) in [
        (vec![], false),
        (vec![], true),
        (vec![3], false),
        (vec![3], true),
    ] {
        let model = anchored_connector(dimensions, opposite);
        let constraint = model.inspect(|view| {
            constraints::holonomic_constraints(view)
                .into_iter()
                .find(|candidate| candidate.owner_ordinal == 2)
                .expect("the product is an independent position constraint")
        });
        assert_eq!(constraint.proof.maximum_order, 2);
        let (rebuilt, manifold) =
            rebuild_holonomic_constraint(&ReductionSource::new(&model), &constraint, &[])
                .expect("the proved position and velocity constraints reconstruct");
        assert_eq!(manifold.len(), 2);
        rebuilt.inspect(|view| {
            let tangent = view.expression_id(manifold[1].expression as usize).unwrap();
            let variable = |name| {
                view.variables()
                    .find(|(_, variable)| variable.name().as_str() == name)
                    .unwrap()
                    .0
            };
            assert!(dae::expr_contains_var(view, tangent, variable("p")));
            assert!(dae::expr_contains_var(view, tangent, variable("u")));
            assert!(
                !dae::expr_contains_var(view, tangent, variable("v")),
                "the exact p/q relation differentiates through u, independently of the displaced state's v"
            );
        });
    }
}

#[test]
fn lifted_coordinate_differentiates_its_exact_value_definition() {
    for (dimensions, opposite) in [
        (vec![], false),
        (vec![], true),
        (vec![1], false),
        (vec![1], true),
        (vec![3], false),
        (vec![3], true),
    ] {
        let model = anchored_connector(dimensions, opposite);
        let constraint = model.inspect(|view| {
            let facts = constraints::DifferentiationFacts::collect(view);
            let candidate = constraints::index_reduction_constraints(view, &facts)
                .into_iter()
                .find(|candidate| {
                    candidate.owner_ordinal == 3 && candidate.lifted_algebraic.is_some()
                })
                .expect("the forwarded connector has a liftable exact definition");
            let p = view
                .variables()
                .find(|(_, v)| v.name().as_str() == "p")
                .unwrap()
                .0
                .index();
            assert_eq!(
                &*candidate.proof.anchored_states,
                &[p],
                "the lift differentiates the exact position it will retain"
            );
            candidate
        });
        let (rebuilt, manifold) =
            rebuild_holonomic_constraint(&ReductionSource::new(&model), &constraint, &[])
                .expect("the connector lift reconstructs");
        assert_eq!(manifold.len(), 1);
        rebuilt.inspect(|view| {
            let variable = |name| {
                view.variables()
                    .find(|(_, v)| v.name().as_str() == name)
                    .unwrap()
                    .0
            };
            let residual = match view.continuous_owners().nth(3).unwrap() {
                dae::ContinuousOwnerView::Residual { equation, .. } => equation.residual(),
                dae::ContinuousOwnerView::Structured { family, .. } => {
                    family.bodies().iter().next().unwrap()
                }
            };
            assert!(dae::expr_contains_var(view, residual, variable("u")));
            assert!(!dae::expr_contains_var(view, residual, variable("v")));
        });
    }
}

#[test]
fn undoing_a_lift_restores_its_original_connector_equation() {
    let model = connector_with_return_equation(vec![], false, true, true);
    let constraint = model.inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        constraints::index_reduction_constraints(view, &facts)
            .into_iter()
            .find(|candidate| candidate.owner_ordinal == 3 && candidate.lifted_algebraic.is_some())
            .unwrap()
    });
    let (lifted, manifold) =
        rebuild_holonomic_constraint(&ReductionSource::new(&model), &constraint, &[]).unwrap();
    let candidate = lifted.inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        constraints::direct_state_constraints(view, &facts)
            .admissible
            .into_iter()
            .find(|candidate| Some(candidate.state) == constraint.lifted_algebraic)
            .unwrap()
    });
    let (restored, manifold) = reconstruction::rebuild_with_state_demotion_and_manifold(
        &ReductionSource::new(&lifted),
        candidate,
        &manifold,
    )
    .unwrap();
    assert!(manifold.is_empty());
    restored.inspect(|view| {
        let variable = |name| {
            view.variables()
                .find(|(_, v)| v.name().as_str() == name)
                .unwrap()
                .0
        };
        let dae::ContinuousOwnerView::Residual { equation, .. } =
            view.continuous_owners().nth(3).unwrap()
        else {
            panic!("the connector equation keeps its source owner");
        };
        assert!(dae::expr_contains_var(
            view,
            equation.residual(),
            variable("r")
        ));
        assert!(
            dae::expr_contains_var(view, equation.residual(), variable("q")),
            "restore r=q, not the state-only r=-p manifold that used other source equalities"
        );
        assert!(!dae::expr_contains_var(
            view,
            equation.residual(),
            variable("p")
        ));
    });
}

#[test]
fn lifted_value_cannot_use_its_own_connection_as_evidence() {
    for dimensions in [vec![], vec![1], vec![3]] {
        assert_independent_lift_value(dimensions);
    }
}

fn assert_independent_lift_value(dimensions: Vec<u32>) {
    let model = connector_with_return_equation(dimensions.clone(), false, true, true);
    let constraint = model.inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        constraints::index_reduction_constraints(view, &facts)
            .into_iter()
            .find(|candidate| candidate.owner_ordinal == 3 && candidate.lifted_algebraic.is_some())
            .expect("the independent s=L-q equation provides a lift definition")
    });
    let (lifted, manifold) =
        rebuild_holonomic_constraint(&ReductionSource::new(&model), &constraint, &[]).unwrap();
    lifted.inspect(|view| {
        let residual = view.expression_id(manifold[0].expression as usize).unwrap();
        assert_eq!(view.expression(residual).unwrap().value_type().dimensions(), dimensions);
        assert_eq!(
            connector_value(view, residual),
            -1.5,
            "the retained equation must reject p=2,q=-0.5,r=-2,s=1.5,L=1, which violates original r=q"
        );
    });
}

#[test]
fn circular_additive_aliases_cannot_prove_a_lift() {
    let model = connector_with_return_equation(vec![], false, true, false);
    model.inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        assert!(
            constraints::index_reduction_constraints(view, &facts)
                .iter()
                .all(|candidate| candidate.owner_ordinal != 2
                    || candidate.lifted_algebraic.is_none()),
            "r+p=0 cannot prove q=-p independently of the r=q equation being replaced"
        );
    });
}

fn connector_value<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> f64 {
    match view.expression(expression).unwrap().operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) => *value,
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => *value as f64,
        dae::ExpressionOperation::Coordinate(coordinate) => {
            let id: dae::VariableId<'_> = match coordinate {
                dae::CoordinateView::State(id) => id.into(),
                dae::CoordinateView::Algebraic(id) => id.into(),
                dae::CoordinateView::Parameter(id) => id.into(),
                _ => panic!("fixture evaluates only position coordinates"),
            };
            match view.variable(id).unwrap().name().as_str() {
                "p" => 2.0,
                "q" => -0.5,
                "r" => -2.0,
                "s" => 1.5,
                "L" => 1.0,
                _ => panic!("fixture position expression has an unexpected variable"),
            }
        }
        dae::ExpressionOperation::Unary { operator, operand } => {
            let value = connector_value(view, operand);
            match operator {
                dae::UnaryOperator::Plus => value,
                dae::UnaryOperator::Negate => -value,
                _ => panic!("fixture arithmetic is signed Real addition"),
            }
        }
        dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
            let (lhs, rhs) = (connector_value(view, lhs), connector_value(view, rhs));
            match operator {
                dae::BinaryOperator::Add => lhs + rhs,
                dae::BinaryOperator::Subtract => lhs - rhs,
                _ => panic!("fixture arithmetic is signed Real addition"),
            }
        }
        _ => panic!("fixture expression uses no other operations"),
    }
}
