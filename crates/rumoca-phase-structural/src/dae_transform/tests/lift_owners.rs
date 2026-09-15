use super::*;

#[test]
fn algebraic_lift_cannot_invert_its_own_defining_equation() {
    for extent in [1, 3, 4096] {
        let model = lift_model(extent, false);
        model.inspect(|view| {
            let facts = constraints::DifferentiationFacts::collect(view);
            let dae::ContinuousOwnerView::Structured { family, .. } =
                view.continuous_owner(1).unwrap()
            else {
                panic!("the vector definition retains its owner");
            };
            let root = family.bodies().get(0).unwrap();
            let block = facts.auxiliary_blocks[2]
                .as_ref()
                .expect("the defining row can solve for u through q");
            assert!(block.contains_residual(root.index()));
            let (_, definition) =
                crate::residual_normalization::equation_sides(view, root).unwrap();
            assert!(!block.contains_residual(definition.index()));
            assert!(
                constraints::index_reduction_constraints(view, &facts)
                    .iter()
                    .all(|candidate| {
                        candidate.owner_ordinal != 1 || candidate.lifted_algebraic != Some(3)
                    }),
                "lifting v must not reconstruct through the equation being replaced"
            );
        });
    }
}

#[test]
fn algebraic_lift_retains_an_independent_source_state_definition() {
    let counts = [1, 3, 4096].map(|extent| {
        let model = lift_model(extent, true);
        let source = ReductionSource::new(&model);
        let constraint = source.inspect(|view, facts| {
            constraints::index_reduction_constraints(view, facts)
                .into_iter()
                .find(|candidate| {
                    candidate.owner_ordinal == 1 && candidate.lifted_algebraic == Some(3)
                })
                .expect("u=x supplies an independent source state")
        });
        let (rebuilt, manifold) = rebuild_holonomic_constraint(&source, &constraint, &[]).unwrap();
        assert_eq!(manifold.len(), 1);
        rebuilt.inspect(|view| {
            let states = retained_states(view, manifold[0].expression);
            assert_eq!(
                states,
                [0, 3].into_iter().collect(),
                "the retained relation ties v to x, not q"
            );
            (
                view.expression_count(),
                view.variable_count(),
                view.continuous_owners().count(),
            )
        })
    });
    assert!(counts.windows(2).all(|pair| pair[0] == pair[1]));
}

fn retained_states(view: dae::DaeView<'_>, expression: u32) -> std::collections::BTreeSet<u32> {
    let mut states = std::collections::BTreeSet::new();
    dae::ExpressionTraversal::new().visit_pruned(
        view,
        [view.expression_id(expression as usize).unwrap()],
        |_, node| {
            if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) =
                node.operation()
            {
                states.insert(state.index());
            }
            true
        },
    );
    states
}

fn lift_model(extent: u32, independent: bool) -> dae::Dae {
    let rhs = if independent { "x" } else { "der(x)" };
    let motion = if independent {
        "der(x)=ones(n)"
    } else {
        "a=-q"
    };
    let text = format!("function shift input Real u[:],p[size(u,1)]; output Real v[size(u,1)]; algorithm v:=u+p; end shift;
        function forward input Real v[:]; output Real q[size(v,1)]; algorithm q:=v; end forward;
        model LiftOwner parameter Integer n={extent};
        Real x[n],q[n],u[n],v[n],a[n]; Real first;
        equation u={rhs}; v=shift(u,zeros(n)); q=forward(v); der(q)=a; {motion}; first=q[1]; end LiftOwner;");
    let mut sources = SourceMap::new();
    let source = sources.add("lift_owner.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let (vector, scalar) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
            ))
        })?;
        let shift = shift_function(model, vector, at)?;
        let forward = forward_function(model, vector, at)?;
        let (x, q, u, v, a, first) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("x"), vector, at, Default::default())?,
                variables.state(VarName::new("q"), vector, at, Default::default())?,
                variables.algebraic(VarName::new("u"), vector, at, Default::default())?,
                variables.algebraic(VarName::new("v"), vector, at, Default::default())?,
                variables.algebraic(VarName::new("a"), vector, at, Default::default())?,
                variables.algebraic(VarName::new("first"), scalar, at, Default::default())?,
            ))
        })?;
        let rows = model.expressions(|e| {
            let x_value = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let q_value = e.at(at).coordinate(dae::CoordinateInput::State(q))?;
            let u = e.at(at).coordinate(dae::CoordinateInput::Algebraic(u))?;
            let v = e.at(at).coordinate(dae::CoordinateInput::Algebraic(v))?;
            let a = e.at(at).coordinate(dae::CoordinateInput::Algebraic(a))?;
            let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let dq = e.at(at).coordinate(dae::CoordinateInput::Derivative(q))?;
            let n = e
                .at(at)
                .literal(dae::DaeLiteral::Integer(i64::from(extent)))?;
            let zeros = e.at(at).builtin(dae::PureBuiltin::Zeros, [n])?;
            let call = e.at(at).call(shift, 0, [u, zeros])?;
            let forwarded = e.at(at).call(forward, 0, [v])?;
            let mut rows = vec![
                e.at(at).binary(
                    dae::BinaryOperator::Subtract,
                    u,
                    if independent { x_value } else { dx },
                )?,
                e.at(at).binary(dae::BinaryOperator::Subtract, v, call)?,
                e.at(at)
                    .binary(dae::BinaryOperator::Subtract, q_value, forwarded)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, dq, a)?,
            ];
            rows.push(if independent {
                let ones = e.at(at).builtin(dae::PureBuiltin::Ones, [n])?;
                e.at(at).binary(dae::BinaryOperator::Subtract, dx, ones)?
            } else {
                e.at(at).binary(dae::BinaryOperator::Add, a, q_value)?
            });
            let first = e
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(first))?;
            let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let component = e.at(at).index(
                q_value,
                [dae::Subscript::Index {
                    expression: one,
                    provenance: at,
                }],
            )?;
            rows.push(
                e.at(at)
                    .binary(dae::BinaryOperator::Subtract, first, component)?,
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

fn forward_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let (function, ()) = model.function(
        dae::FunctionSignature::new(VarName::new("forward"), [vector], [vector], at),
        |model, reservation| {
            let input = model.functions(|f| f.parameter(&reservation, VarName::new("v"), 0, at))?;
            let output = model.functions(|f| f.output(&reservation, VarName::new("q"), 0, at))?;
            let value = model.expressions(|e| e.at(at).function_parameter(input))?;
            let mut body = model.functions(|f| f.begin(reservation, at))?;
            model.functions(|f| f.assign(&mut body, output, value, at))?;
            model.functions(|f| f.define(body, at))
        },
    )?;
    Ok(function)
}

fn shift_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let (function, ()) = model.function(
        dae::FunctionSignature::new(VarName::new("shift"), [vector, vector], [vector], at),
        |model, reservation| {
            let u = model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
            let p = model.functions(|f| f.parameter(&reservation, VarName::new("p"), 1, at))?;
            let output = model.functions(|f| f.output(&reservation, VarName::new("v"), 0, at))?;
            let value = model.expressions(|e| {
                let u = e.at(at).function_parameter(u)?;
                let p = e.at(at).function_parameter(p)?;
                e.at(at).binary(dae::BinaryOperator::Add, u, p)
            })?;
            let mut body = model.functions(|f| f.begin(reservation, at))?;
            model.functions(|f| f.assign(&mut body, output, value, at))?;
            model.functions(|f| f.define(body, at))
        },
    )?;
    Ok(function)
}
