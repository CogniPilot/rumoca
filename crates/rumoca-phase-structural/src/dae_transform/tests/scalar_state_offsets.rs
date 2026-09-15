use super::*;

#[derive(Clone, Copy, PartialEq)]
enum Form {
    Indirect,
    Explicit,
    TimeVarying,
}

#[test]
fn dependent_state_reconstructs_through_a_constant_offset() {
    for opposite in [false, true] {
        let counts = [vec![], vec![1], vec![3], vec![4096], vec![3, 3]].map(|shape| {
            let model = offset_model(&shape, Form::Indirect, opposite);
            let prepared = prepare_for_solve(&model).expect("independent state basis");
            prepared.inspect(|system| {
                assert_independent_basis(system.view);
                assert!(system.manifold.is_empty());
                assert_eq!(system.view.continuous_owners().count(), 4);
                system.view.expression_count()
            })
        });
        assert_eq!(counts[1], counts[2]);
        assert_eq!(counts[2], counts[3]);
    }
}

fn assert_independent_basis(view: dae::DaeView<'_>) {
    let states = view
        .variables()
        .filter(|(_, v)| v.role() == dae::VariableRole::State)
        .map(|(_, v)| v.name().to_string())
        .collect::<Vec<_>>();
    assert_eq!(states, ["x"]);
    assert_eq!(
        view.variable(view.variable_id(1).unwrap()).unwrap().role(),
        dae::VariableRole::Algebraic
    );
}

#[test]
fn explicit_offset_state_definition_already_reduces() {
    let model = offset_model(&[], Form::Explicit, false);
    let prepared = prepare_for_solve(&model).unwrap();
    prepared.inspect(|system| {
        assert_independent_basis(system.view);
        assert!(system.manifold.is_empty());
    });
}

#[test]
fn varying_offsets_cannot_supply_equal_derivatives() {
    let model = offset_model(&[], Form::TimeVarying, false);
    model.inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        let candidates = constraints::direct_state_constraints(view, &facts);
        assert!(
            candidates
                .admissible
                .iter()
                .chain(&candidates.conditional)
                .all(|candidate| candidate.state != 1)
        );
    });
}

#[test]
fn derivative_only_definition_cannot_supply_a_manifold_value() {
    let model = offset_model(&[], Form::Indirect, false);
    model.inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        let candidates = constraints::direct_state_constraints(view, &facts);
        let candidate = candidates.admissible.iter().find(|c| c.state == 1).unwrap();
        assert!(matches!(
            candidate.rhs,
            StateDefinition::DerivativeExpression(_)
        ));
        let residual = view.continuous_owners().nth(1).unwrap();
        let dae::ContinuousOwnerView::Residual { equation, .. } = residual else {
            panic!()
        };
        let manifold = [ManifoldConstraint {
            expression: equation.residual().index(),
            lifted: None,
        }];
        assert!(constraints::demotion_preserves_manifold_values(
            view,
            &facts,
            candidate,
            &[]
        ));
        assert!(!constraints::demotion_preserves_manifold_values(
            view, &facts, candidate, &manifold
        ));
    });
}

struct OffsetVariables<'dae> {
    x: dae::StateId<'dae>,
    q: dae::StateId<'dae>,
    length: dae::AlgebraicId<'dae>,
    rate: dae::AlgebraicId<'dae>,
    offset: dae::ParameterId<'dae>,
}

fn offset_source(shape: &[u32], form: Form, opposite: bool) -> String {
    let axes = shape
        .iter()
        .map(u32::to_string)
        .collect::<Vec<_>>()
        .join(",");
    let suffix = if axes.is_empty() {
        String::new()
    } else {
        format!("[{axes}]")
    };
    let lift = |value: &str| {
        if axes.is_empty() {
            value.to_string()
        } else {
            format!("({value})*ones({axes})")
        }
    };
    let offset = if form == Form::TimeVarying {
        lift("time")
    } else {
        "offset".to_string()
    };
    let binding = lift("0.25");
    let q = if opposite { "-q" } else { "q" };
    let norm = lift("sqrt(x*x+1)");
    let relation = match (form, opposite) {
        (Form::Explicit, false) => format!("q={norm}-{offset}"),
        (Form::Explicit, true) => format!("q={offset}-({norm})"),
        _ => format!("length={norm}"),
    };
    format!(
        "model Offset parameter Real offset{suffix}={binding};
        Real x(start=1, fixed=true), q{suffix}, length{suffix}, rate{suffix};
        equation der(x)=-x; length={q}+{offset}; {relation}; der(q)=rate; end Offset;"
    )
}

fn offset_model(shape: &[u32], form: Form, opposite: bool) -> dae::Dae {
    let text = offset_source(shape, form, opposite);
    let mut sources = SourceMap::new();
    let source = sources.add("offset.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let (scalar, payload) = model.types(|t| {
            Ok((
                t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                t.derived(
                    dae::ValueType::array(dae::ScalarType::Real, shape.to_vec()),
                    at,
                )?,
            ))
        })?;
        let (one, offset) = model.expressions(|e| {
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let quarter = e.at(at).literal(dae::DaeLiteral::Real(0.25))?;
            Ok((one, broadcast(e, quarter, shape, at)?))
        })?;
        let vars = model.variables(|v| {
            Ok(OffsetVariables {
                x: v.state(
                    VarName::new("x"),
                    scalar,
                    at,
                    dae::VariableAttributes {
                        start: Some(one),
                        fixed: Some(true),
                        ..Default::default()
                    },
                )?,
                q: v.state(VarName::new("q"), payload, at, Default::default())?,
                length: v.algebraic(VarName::new("length"), payload, at, Default::default())?,
                rate: v.algebraic(VarName::new("rate"), payload, at, Default::default())?,
                offset: v.parameter(
                    VarName::new("offset"),
                    payload,
                    at,
                    dae::VariableAttributes {
                        binding: Some(offset),
                        ..Default::default()
                    },
                )?,
            })
        })?;
        let rows = model.expressions(|e| offset_rows(e, vars, shape, form, opposite, at))?;
        model.continuous(|c| {
            for row in rows {
                c.value_equation(at, row)?;
            }
            Ok(())
        })
    })
    .unwrap()
}

fn broadcast<'dae>(
    e: &mut dae::Expressions<'_, 'dae>,
    value: dae::ExprId<'dae>,
    shape: &[u32],
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if shape.is_empty() {
        return Ok(value);
    }
    let axes = shape
        .iter()
        .map(|&n| e.at(at).literal(dae::DaeLiteral::Integer(i64::from(n))))
        .collect::<Result<Vec<_>, _>>()?;
    let ones = e.at(at).builtin(dae::PureBuiltin::Ones, axes)?;
    e.at(at).binary(dae::BinaryOperator::Multiply, value, ones)
}

fn offset_rows<'dae>(
    e: &mut dae::Expressions<'_, 'dae>,
    vars: OffsetVariables<'dae>,
    shape: &[u32],
    form: Form,
    opposite: bool,
    at: dae::DaeProvenance,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let x = e.at(at).coordinate(dae::CoordinateInput::State(vars.x))?;
    let q = e.at(at).coordinate(dae::CoordinateInput::State(vars.q))?;
    let length = e
        .at(at)
        .coordinate(dae::CoordinateInput::Algebraic(vars.length))?;
    let rate = e
        .at(at)
        .coordinate(dae::CoordinateInput::Algebraic(vars.rate))?;
    let offset = if form == Form::TimeVarying {
        let time = e.at(at).coordinate(dae::CoordinateInput::Time)?;
        broadcast(e, time, shape, at)?
    } else {
        e.at(at)
            .coordinate(dae::CoordinateInput::Parameter(vars.offset))?
    };
    let dx = e
        .at(at)
        .coordinate(dae::CoordinateInput::Derivative(vars.x))?;
    let dq = e
        .at(at)
        .coordinate(dae::CoordinateInput::Derivative(vars.q))?;
    let nx = e.at(at).unary(dae::UnaryOperator::Negate, x)?;
    let signed_q = if opposite {
        e.at(at).unary(dae::UnaryOperator::Negate, q)?
    } else {
        q
    };
    let shifted = e
        .at(at)
        .binary(dae::BinaryOperator::Add, signed_q, offset)?;
    let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
    let square = e.at(at).binary(dae::BinaryOperator::Multiply, x, x)?;
    let sum = e.at(at).binary(dae::BinaryOperator::Add, square, one)?;
    let norm = e.at(at).builtin(dae::PureBuiltin::Sqrt, [sum])?;
    let norm = broadcast(e, norm, shape, at)?;
    let relation = if form == Form::Explicit {
        let (lhs, rhs) = if opposite {
            (offset, norm)
        } else {
            (norm, offset)
        };
        (q, e.at(at).binary(dae::BinaryOperator::Subtract, lhs, rhs)?)
    } else {
        (length, norm)
    };
    [(dx, nx), (length, shifted), relation, (dq, rate)]
        .into_iter()
        .map(|(lhs, rhs)| e.at(at).binary(dae::BinaryOperator::Subtract, lhs, rhs))
        .collect()
}
