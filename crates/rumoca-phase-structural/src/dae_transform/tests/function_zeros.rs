use super::*;

#[test]
fn supplied_derivative_preserves_a_proven_zero_tensor_call() {
    let counts = [1, 3, 4096].map(|extent| check_derivative_call(extent, false));
    assert!(counts.windows(2).all(|pair| pair[0] == pair[1]));
}

#[test]
fn parameter_default_zero_does_not_erase_a_supplied_derivative() {
    check_derivative_call(3, true);
}

fn check_derivative_call(extent: u32, parameter: bool) -> usize {
    let model = zero_call_model(extent, parameter);
    let source = ReductionSource::new(&model);
    let constraint = source.inspect(|view, facts| {
        constraints::index_reduction_constraints(view, facts)
            .into_iter()
            .find(|candidate| candidate.owner_ordinal == 0)
            .expect("the vector relation has a first derivative")
    });
    let (rebuilt, manifold) = rebuild_holonomic_constraint(&source, &constraint, &[]).unwrap();
    assert_eq!(
        manifold.len(),
        1,
        "the fixture exercises the supplied first derivative"
    );
    rebuilt.inspect(|view| {
        let dae::ContinuousOwnerView::Structured { family, .. } = view.continuous_owner(0).unwrap()
        else {
            panic!("vector equation retains its aggregate owner");
        };
        let mut calls = 0;
        dae::ExpressionTraversal::new().visit_pruned(view, family.bodies().iter(), |_, node| {
            calls += usize::from(matches!(
                node.operation(),
                dae::ExpressionOperation::Call { .. }
            ));
            true
        });
        assert_eq!(
            calls,
            usize::from(parameter),
            "a proven zero must not leave an opaque derivative call"
        );
        assert_eq!(
            view.function_count(),
            2,
            "both source functions survive reconstruction"
        );
        view.expression_count()
    })
}

fn zero_call_model(extent: u32, parameter: bool) -> dae::Dae {
    let argument = if parameter { "p" } else { "zeros(n)" };
    let text = format!(
        "function scale input Real x; input Real v[:]; output Real result[size(v,1)];
         algorithm result:=x*v; annotation(derivative=scale_der); end scale;
         function scale_der input Real x; input Real v[:]; input Real dx;
         input Real dv[size(v,1)]; output Real result[size(v,1)];
         algorithm result:=dx*v+x*dv; end scale_der;
         model ZeroFunctionCall parameter Integer n={extent};
         parameter Real p[n]=zeros(n); Real q[n],y[n],x,a[n],b[n];
         equation q=y+scale(x,{argument}); der(q)=a; der(y)=b; der(x)=1;
         end ZeroFunctionCall;"
    );
    let mut sources = SourceMap::new();
    let source = sources.add("zero_function_call.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let (scalar, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)?,
            ))
        })?;
        let scale = scale_function(model, scalar, vector, at, false)?;
        let derivative = scale_function(model, scalar, vector, at, true)?;
        model.functions(|functions| {
            use rumoca_core::FunctionDerivativeInput::Differentiate;
            functions.first_derivative(scale, derivative, [Differentiate, Differentiate], 0, at)
        })?;
        let zero = model.expressions(|e| {
            let n = e
                .at(at)
                .literal(dae::DaeLiteral::Integer(i64::from(extent)))?;
            e.at(at).builtin(dae::PureBuiltin::Zeros, [n])
        })?;
        let (q, y, x, a, b, p) = model.variables(|v| {
            Ok((
                v.state(VarName::new("q"), vector, at, Default::default())?,
                v.state(VarName::new("y"), vector, at, Default::default())?,
                v.state(VarName::new("x"), scalar, at, Default::default())?,
                v.algebraic(VarName::new("a"), vector, at, Default::default())?,
                v.algebraic(VarName::new("b"), vector, at, Default::default())?,
                v.parameter(
                    VarName::new("p"),
                    vector,
                    at,
                    dae::VariableAttributes {
                        binding: Some(zero),
                        ..Default::default()
                    },
                )?,
            ))
        })?;
        let rows = model.expressions(|e| {
            let q_value = e.at(at).coordinate(dae::CoordinateInput::State(q))?;
            let y_value = e.at(at).coordinate(dae::CoordinateInput::State(y))?;
            let x_value = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let argument = if parameter {
                e.at(at).coordinate(dae::CoordinateInput::Parameter(p))?
            } else {
                zero
            };
            let call = e.at(at).call(scale, 0, [x_value, argument])?;
            let rhs = e.at(at).binary(dae::BinaryOperator::Add, y_value, call)?;
            let relation = e
                .at(at)
                .binary(dae::BinaryOperator::Subtract, q_value, rhs)?;
            let mut rows = vec![relation];
            for (state, algebraic) in [(q, a), (y, b)] {
                let derivative = e
                    .at(at)
                    .coordinate(dae::CoordinateInput::Derivative(state))?;
                let value = e
                    .at(at)
                    .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
                rows.push(
                    e.at(at)
                        .binary(dae::BinaryOperator::Subtract, derivative, value)?,
                );
            }
            let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            rows.push(e.at(at).binary(dae::BinaryOperator::Subtract, dx, one)?);
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

fn scale_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    scalar: dae::ValueTypeId<'dae>,
    vector: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
    derivative: bool,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let inputs = if derivative {
        vec![scalar, vector, scalar, vector]
    } else {
        vec![scalar, vector]
    };
    let name = if derivative { "scale_der" } else { "scale" };
    let (function, ()) = model.function(
        dae::FunctionSignature::new(VarName::new(name), inputs, [vector], at),
        |model, reservation| {
            let mut parameters = Vec::new();
            for (index, name) in ["x", "v", "dx", "dv"]
                .into_iter()
                .take(if derivative { 4 } else { 2 })
                .enumerate()
            {
                let parameter = model
                    .functions(|f| f.parameter(&reservation, VarName::new(name), index, at))?;
                parameters.push(model.expressions(|e| e.at(at).function_parameter(parameter))?);
            }
            let output =
                model.functions(|f| f.output(&reservation, VarName::new("result"), 0, at))?;
            let value = model.expressions(|e| {
                if derivative {
                    let lhs = e.at(at).binary(
                        dae::BinaryOperator::Multiply,
                        parameters[2],
                        parameters[1],
                    )?;
                    let rhs = e.at(at).binary(
                        dae::BinaryOperator::Multiply,
                        parameters[0],
                        parameters[3],
                    )?;
                    e.at(at).binary(dae::BinaryOperator::Add, lhs, rhs)
                } else {
                    e.at(at)
                        .binary(dae::BinaryOperator::Multiply, parameters[0], parameters[1])
                }
            })?;
            let mut body = model.functions(|f| f.begin(reservation, at))?;
            model.functions(|f| f.assign(&mut body, output, value, at))?;
            model.functions(|f| f.define(body, at))
        },
    )?;
    Ok(function)
}
