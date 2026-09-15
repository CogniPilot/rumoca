use super::*;
use rumoca_core::FunctionDerivativeInput::Differentiate;

#[test]
fn successive_state_demotions_keep_the_supplied_derivative_chain() {
    let counts = [1, 3, 4096].map(|extent| {
        let model = chain_model(extent);
        let prepared = prepare_for_solve(&model).expect("the supplied derivative chain closes");
        prepared.inspect(|system| {
            let states: Vec<_> = system
                .view
                .variables()
                .filter(|(_, v)| v.role() == dae::VariableRole::State)
                .map(|(_, v)| v.name().to_string())
                .collect();
            assert_eq!(states, ["s", "v"], "dependent q and w are reconstructed");
            assert!(system.manifold.is_empty());
            assert_eq!(system.view.continuous_owners().count(), 5);
            check_wrapper_history(system.view);
            system.view.expression_count()
        })
    });
    assert!(counts.windows(2).all(|pair| pair[0] == pair[1]));
}

fn check_wrapper_history(view: dae::DaeView<'_>) {
    let wrapper = view.function(view.function_id(3).unwrap()).unwrap();
    let mut expression = wrapper.result_values().rhs(0).unwrap();
    for function in [1, 0] {
        let value = view.expression(expression).unwrap();
        assert_eq!(value.function_scope(), Some(wrapper.id()));
        let (source, link) = value
            .call_derivative()
            .expect("scoped history survives demotion");
        assert_eq!(link.function(), view.function_id(function).unwrap());
        expression = source;
    }
    assert!(
        view.expression(expression)
            .unwrap()
            .call_derivative()
            .is_none()
    );
}

fn chain_model(extent: u32) -> dae::Dae {
    let text = format!(
        "function f input Real x; output Real y; algorithm y:=if x>0 then x else -x;
        annotation(derivative=fd); end f;
        function fd input Real x; input Real dx; output Real dy;
        algorithm dy:=if x>0 then dx else -dx; annotation(derivative(order=2)=fdd); end fd;
        function fdd input Real x; input Real dx; input Real ddx; output Real ddy;
        algorithm ddy:=if x>0 then ddx else -ddx; end fdd;
        model Chain parameter Integer n={extent}; Real s,v,q[n],w[n],a[n];
        equation der(s)=v; der(v)=-s; q=f(s)*ones(n); der(q)=w; der(w)=a; end Chain;"
    );
    let mut sources = SourceMap::new();
    let id = sources.add("chain.mo", &text);
    let at = source_provenance(id, &text, &text);
    dae::Dae::construct(sources, |model| {
        let (scalar, vector) = model.types(|t| {
            Ok((
                t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                t.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)?,
            ))
        })?;
        let f = branch_function(model, scalar, at, 0)?;
        let fd = branch_function(model, scalar, at, 1)?;
        let fdd = branch_function(model, scalar, at, 2)?;
        model.functions(|functions| {
            let first = functions.first_derivative(f, fd, [Differentiate], 0, at)?;
            functions.next_derivative(fd, first, fdd, [Differentiate; 2], 0, at)
        })?;
        derivative_wrapper(model, scalar, f, at)?;
        let (s, v, q, w, a) = model.variables(|vars| {
            Ok((
                vars.state(VarName::new("s"), scalar, at, Default::default())?,
                vars.state(VarName::new("v"), scalar, at, Default::default())?,
                vars.state(VarName::new("q"), vector, at, Default::default())?,
                vars.state(VarName::new("w"), vector, at, Default::default())?,
                vars.algebraic(VarName::new("a"), vector, at, Default::default())?,
            ))
        })?;
        let rows = model.expressions(|e| {
            let sv = e.at(at).coordinate(dae::CoordinateInput::State(s))?;
            let vv = e.at(at).coordinate(dae::CoordinateInput::State(v))?;
            let qv = e.at(at).coordinate(dae::CoordinateInput::State(q))?;
            let wv = e.at(at).coordinate(dae::CoordinateInput::State(w))?;
            let av = e.at(at).coordinate(dae::CoordinateInput::Algebraic(a))?;
            let ds = e.at(at).coordinate(dae::CoordinateInput::Derivative(s))?;
            let dv = e.at(at).coordinate(dae::CoordinateInput::Derivative(v))?;
            let dq = e.at(at).coordinate(dae::CoordinateInput::Derivative(q))?;
            let dw = e.at(at).coordinate(dae::CoordinateInput::Derivative(w))?;
            let ns = e.at(at).unary(dae::UnaryOperator::Negate, sv)?;
            let call = e.at(at).call(f, 0, [sv])?;
            let n = e
                .at(at)
                .literal(dae::DaeLiteral::Integer(i64::from(extent)))?;
            let ones = e.at(at).builtin(dae::PureBuiltin::Ones, [n])?;
            let filled = e.at(at).binary(dae::BinaryOperator::Multiply, call, ones)?;
            [(ds, vv), (dv, ns), (qv, filled), (dq, wv), (dw, av)]
                .into_iter()
                .map(|(lhs, rhs)| e.at(at).binary(dae::BinaryOperator::Subtract, lhs, rhs))
                .collect::<Result<Vec<_>, _>>()
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

fn derivative_wrapper<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    scalar: dae::ValueTypeId<'dae>,
    f: dae::FunctionId<'dae>,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let signature = dae::FunctionSignature::new(VarName::new("wrapper"), [scalar; 3], [scalar], at);
    model.function(signature, |model, reservation| {
        let mut args = Vec::new();
        for ordinal in 0..3 {
            let p = model.functions(|f| {
                f.parameter(
                    &reservation,
                    VarName::new(format!("x{ordinal}")),
                    ordinal,
                    at,
                )
            })?;
            args.push(model.expressions(|e| e.at(at).function_parameter(p))?);
        }
        let out = model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
        let value = model.expressions(|e| {
            let primal = e.at(at).call(f, 0, [args[0]])?;
            let first = e
                .at(at)
                .differentiated_call(primal, 0, args[..2].iter().copied())?;
            e.at(at).differentiated_call(first, 0, args)
        })?;
        let mut body = model.functions(|f| f.begin(reservation, at))?;
        model.functions(|f| f.assign(&mut body, out, value, at))?;
        model.functions(|f| f.define(body, at))
    })?;
    Ok(())
}

fn branch_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    scalar: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
    order: usize,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let name = ["f", "fd", "fdd"][order];
    let signature =
        dae::FunctionSignature::new(VarName::new(name), vec![scalar; order + 1], [scalar], at);
    model
        .function(signature, |model, reservation| {
            let mut params = Vec::new();
            for i in 0..=order {
                let p = model.functions(|f| {
                    f.parameter(&reservation, VarName::new(format!("x{i}")), i, at)
                })?;
                params.push(model.expressions(|e| e.at(at).function_parameter(p))?);
            }
            let out = model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
            let value = model.expressions(|e| {
                let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                let guard = e
                    .at(at)
                    .binary(dae::BinaryOperator::Greater, params[0], zero)?;
                let negative = e.at(at).unary(dae::UnaryOperator::Negate, params[order])?;
                e.at(at).conditional([(guard, params[order])], negative)
            })?;
            let mut body = model.functions(|f| f.begin(reservation, at))?;
            model.functions(|f| f.assign(&mut body, out, value, at))?;
            model.functions(|f| f.define(body, at))
        })
        .map(|(id, ())| id)
}
