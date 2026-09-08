//! Adversaries for invariant-operand elimination in connector balances.

use super::*;
use crate::dae_transform::equalities::{EqualityAnchor, EqualitySign, SystemEqualities};

#[derive(Clone, Copy)]
enum OffsetKind {
    Zero,
    Half,
    Varying,
    NonUnitCoefficient,
}

const ZERO_TEXT: &str = "Real x; Real w; Real y; Real v; Real port_x; Real port_y; Real support; Real shifted; Real acc_x; Real acc_y; equation x = port_x; y = port_y; support = 0; shifted = port_x - support; shifted = 2*port_y; der(x) = w; der(y) = v; der(w) = acc_x; der(v) = acc_y; acc_x = 1;";
const HALF_TEXT: &str = "Real x; Real w; Real y; Real v; Real port_x; Real port_y; Real support; Real shifted; Real acc_x; Real acc_y; equation x = port_x; y = port_y; support = 0.5; shifted = port_x - support; shifted = 2*port_y; der(x) = w; der(y) = v; der(w) = acc_x; der(v) = acc_y; acc_x = 1;";
const VARYING_TEXT: &str = "Real x; Real w; Real y; Real v; Real port_x; Real port_y; Real support; Real shifted; Real acc_x; Real acc_y; equation x = port_x; y = port_y; support = v; shifted = port_x - support; shifted = 2*port_y; der(x) = w; der(y) = v; der(w) = acc_x; der(v) = acc_y; acc_x = 1;";
const NON_UNIT_TEXT: &str = "Real x; Real w; Real y; Real v; Real port_x; Real port_y; Real support; Real shifted; Real acc_x; Real acc_y; equation x = port_x; y = port_y; support = 0; shifted = 2*port_x - support; shifted = 2*port_y; der(x) = w; der(y) = v; der(w) = acc_x; der(v) = acc_y; acc_x = 1;";

struct OffsetVariables<'dae> {
    x: dae::StateId<'dae>,
    w: dae::StateId<'dae>,
    y: dae::StateId<'dae>,
    v: dae::StateId<'dae>,
    port_x: dae::AlgebraicId<'dae>,
    port_y: dae::AlgebraicId<'dae>,
    support: dae::AlgebraicId<'dae>,
    shifted: dae::AlgebraicId<'dae>,
    acc_x: dae::AlgebraicId<'dae>,
    acc_y: dae::AlgebraicId<'dae>,
}

fn offset_text(kind: OffsetKind) -> &'static str {
    match kind {
        OffsetKind::Zero => ZERO_TEXT,
        OffsetKind::Half => HALF_TEXT,
        OffsetKind::Varying => VARYING_TEXT,
        OffsetKind::NonUnitCoefficient => NON_UNIT_TEXT,
    }
}

fn offset_equations(kind: OffsetKind) -> [&'static str; 10] {
    let support = match kind {
        OffsetKind::Zero | OffsetKind::NonUnitCoefficient => "support = 0",
        OffsetKind::Half => "support = 0.5",
        OffsetKind::Varying => "support = v",
    };
    let shifted = match kind {
        OffsetKind::NonUnitCoefficient => "shifted = 2*port_x - support",
        _ => "shifted = port_x - support",
    };
    [
        "x = port_x",
        "y = port_y",
        support,
        shifted,
        "shifted = 2*port_y",
        "der(x) = w",
        "der(y) = v",
        "der(w) = acc_x",
        "der(v) = acc_y",
        "acc_x = 1",
    ]
}

fn declare_offset_variables<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<OffsetVariables<'dae>, dae::DaeConstructionError> {
    model.variables(|variables| {
        let attributes = dae::VariableAttributes::default();
        Ok(OffsetVariables {
            x: variables.state(
                VarName::new("x"),
                InstanceId::new(1),
                real,
                at,
                attributes.clone(),
            )?,
            w: variables.state(
                VarName::new("w"),
                InstanceId::new(2),
                real,
                at,
                attributes.clone(),
            )?,
            y: variables.state(
                VarName::new("y"),
                InstanceId::new(3),
                real,
                at,
                attributes.clone(),
            )?,
            v: variables.state(
                VarName::new("v"),
                InstanceId::new(4),
                real,
                at,
                attributes.clone(),
            )?,
            port_x: variables.algebraic(
                VarName::new("port_x"),
                InstanceId::new(5),
                real,
                at,
                attributes.clone(),
            )?,
            port_y: variables.algebraic(
                VarName::new("port_y"),
                InstanceId::new(6),
                real,
                at,
                attributes.clone(),
            )?,
            support: variables.algebraic(
                VarName::new("support"),
                InstanceId::new(7),
                real,
                at,
                attributes.clone(),
            )?,
            shifted: variables.algebraic(
                VarName::new("shifted"),
                InstanceId::new(8),
                real,
                at,
                attributes.clone(),
            )?,
            acc_x: variables.algebraic(
                VarName::new("acc_x"),
                InstanceId::new(9),
                real,
                at,
                attributes.clone(),
            )?,
            acc_y: variables.algebraic(
                VarName::new("acc_y"),
                InstanceId::new(10),
                real,
                at,
                attributes,
            )?,
        })
    })
}

fn coordinate<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    at: dae::DaeProvenance,
    input: dae::CoordinateInput<'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    expressions.at(at).coordinate(input)
}

fn subtraction<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    at: dae::DaeProvenance,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    expressions
        .at(at)
        .binary(dae::BinaryOperator::Subtract, lhs, rhs)
}

fn coordinate_residual<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    at: dae::DaeProvenance,
    lhs: dae::CoordinateInput<'dae>,
    rhs: dae::CoordinateInput<'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let lhs = coordinate(expressions, at, lhs)?;
    let rhs = coordinate(expressions, at, rhs)?;
    subtraction(expressions, at, lhs, rhs)
}

fn offset_residuals<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    variables: &OffsetVariables<'dae>,
    spans: &[dae::DaeProvenance],
    kind: OffsetKind,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    model.expressions(|expressions| {
        let mut residuals = offset_position_residuals(expressions, variables, spans, kind)?;
        residuals.extend(offset_derivative_residuals(expressions, variables, spans)?);
        Ok(residuals)
    })
}

fn offset_position_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    variables: &OffsetVariables<'dae>,
    spans: &[dae::DaeProvenance],
    kind: OffsetKind,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let x = coordinate(
        expressions,
        spans[0],
        dae::CoordinateInput::State(variables.x),
    )?;
    let port_x_equation = coordinate(
        expressions,
        spans[0],
        dae::CoordinateInput::Algebraic(variables.port_x),
    )?;
    let y = coordinate(
        expressions,
        spans[1],
        dae::CoordinateInput::State(variables.y),
    )?;
    let port_y_equation = coordinate(
        expressions,
        spans[1],
        dae::CoordinateInput::Algebraic(variables.port_y),
    )?;
    let support_equation = coordinate(
        expressions,
        spans[2],
        dae::CoordinateInput::Algebraic(variables.support),
    )?;
    let support_rhs = match kind {
        OffsetKind::Zero | OffsetKind::NonUnitCoefficient => expressions
            .at(spans[2])
            .literal(dae::DaeLiteral::Real(0.0))?,
        OffsetKind::Half => expressions
            .at(spans[2])
            .literal(dae::DaeLiteral::Real(0.5))?,
        OffsetKind::Varying => coordinate(
            expressions,
            spans[2],
            dae::CoordinateInput::State(variables.v),
        )?,
    };
    let shifted = coordinate(
        expressions,
        spans[3],
        dae::CoordinateInput::Algebraic(variables.shifted),
    )?;
    let port_x = coordinate(
        expressions,
        spans[3],
        dae::CoordinateInput::Algebraic(variables.port_x),
    )?;
    let support = coordinate(
        expressions,
        spans[3],
        dae::CoordinateInput::Algebraic(variables.support),
    )?;
    let port_x = scaled_port_x(expressions, spans[3], port_x, kind)?;
    let shifted_rhs = subtraction(expressions, spans[3], port_x, support)?;
    let shifted_relation = coordinate(
        expressions,
        spans[4],
        dae::CoordinateInput::Algebraic(variables.shifted),
    )?;
    let port_y_relation = coordinate(
        expressions,
        spans[4],
        dae::CoordinateInput::Algebraic(variables.port_y),
    )?;
    let two = expressions
        .at(spans[4])
        .literal(dae::DaeLiteral::Real(2.0))?;
    let twice_port_y =
        expressions
            .at(spans[4])
            .binary(dae::BinaryOperator::Multiply, two, port_y_relation)?;
    Ok(vec![
        subtraction(expressions, spans[0], x, port_x_equation)?,
        subtraction(expressions, spans[1], y, port_y_equation)?,
        subtraction(expressions, spans[2], support_equation, support_rhs)?,
        subtraction(expressions, spans[3], shifted, shifted_rhs)?,
        subtraction(expressions, spans[4], shifted_relation, twice_port_y)?,
    ])
}

fn scaled_port_x<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    at: dae::DaeProvenance,
    port_x: dae::ExprId<'dae>,
    kind: OffsetKind,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if !matches!(kind, OffsetKind::NonUnitCoefficient) {
        return Ok(port_x);
    }
    let two = expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?;
    expressions
        .at(at)
        .binary(dae::BinaryOperator::Multiply, two, port_x)
}

fn offset_derivative_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    variables: &OffsetVariables<'dae>,
    spans: &[dae::DaeProvenance],
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let velocity = [(variables.x, variables.w), (variables.y, variables.v)]
        .into_iter()
        .enumerate()
        .map(|(index, (state, velocity))| {
            let at = spans[index + 5];
            Ok((
                coordinate(expressions, at, dae::CoordinateInput::State(velocity))?,
                coordinate(expressions, at, dae::CoordinateInput::Derivative(state))?,
            ))
        })
        .collect::<Result<Vec<_>, dae::DaeConstructionError>>()?;
    let acceleration = [
        (variables.w, variables.acc_x),
        (variables.v, variables.acc_y),
    ]
    .into_iter()
    .enumerate()
    .map(|(index, (state, acceleration))| {
        let at = spans[index + 7];
        Ok((
            coordinate(expressions, at, dae::CoordinateInput::Derivative(state))?,
            coordinate(
                expressions,
                at,
                dae::CoordinateInput::Algebraic(acceleration),
            )?,
        ))
    })
    .collect::<Result<Vec<_>, dae::DaeConstructionError>>()?;
    let acc_x = coordinate(
        expressions,
        spans[9],
        dae::CoordinateInput::Algebraic(variables.acc_x),
    )?;
    let one = expressions
        .at(spans[9])
        .literal(dae::DaeLiteral::Real(1.0))?;
    Ok(vec![
        subtraction(expressions, spans[5], velocity[0].1, velocity[0].0)?,
        subtraction(expressions, spans[6], velocity[1].1, velocity[1].0)?,
        subtraction(expressions, spans[7], acceleration[0].0, acceleration[0].1)?,
        subtraction(expressions, spans[8], acceleration[1].0, acceleration[1].1)?,
        subtraction(expressions, spans[9], acc_x, one)?,
    ])
}

fn offset_model(kind: OffsetKind) -> dae::Dae {
    let text = offset_text(kind);
    let equations = offset_equations(kind);
    let mut sources = SourceMap::new();
    let source = sources.add("invariant_balance.mo", text);
    dae::Dae::construct(sources, |model| {
        let declaration = source_provenance(source, text, "equation");
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variables = declare_offset_variables(model, real, declaration)?;
        let spans = equations
            .iter()
            .map(|equation| source_provenance(source, text, equation))
            .collect::<Vec<_>>();
        let residuals = offset_residuals(model, &variables, &spans, kind)?;
        model.continuous(|continuous| {
            for (span, residual) in spans.iter().copied().zip(residuals) {
                continuous.value_equation(span, residual)?;
            }
            Ok(())
        })
    })
    .expect("invariant-balance fixture constructs")
}

struct ParameterSupportVariables<'dae> {
    phi0: dae::ParameterId<'dae>,
    phi0_reservation: dae::VariableReservation<'dae>,
    states: [dae::StateId<'dae>; 4],
    algebraics: [dae::AlgebraicId<'dae>; 6],
}

fn declare_parameter_support_variables<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<ParameterSupportVariables<'dae>, dae::DaeConstructionError> {
    model.variables(|variables| {
        let attributes = dae::VariableAttributes::default;
        let (phi0, phi0_reservation) =
            variables.reserve_parameter(VarName::new("phi0"), InstanceId::new(1), real, at)?;
        Ok(ParameterSupportVariables {
            phi0,
            phi0_reservation,
            states: [
                variables.state(
                    VarName::new("x"),
                    InstanceId::new(2),
                    real,
                    at,
                    attributes(),
                )?,
                variables.state(
                    VarName::new("w"),
                    InstanceId::new(3),
                    real,
                    at,
                    attributes(),
                )?,
                variables.state(
                    VarName::new("y"),
                    InstanceId::new(4),
                    real,
                    at,
                    attributes(),
                )?,
                variables.state(
                    VarName::new("v"),
                    InstanceId::new(5),
                    real,
                    at,
                    attributes(),
                )?,
            ],
            algebraics: [
                variables.algebraic(
                    VarName::new("port_x"),
                    InstanceId::new(6),
                    real,
                    at,
                    attributes(),
                )?,
                variables.algebraic(
                    VarName::new("port_y"),
                    InstanceId::new(7),
                    real,
                    at,
                    attributes(),
                )?,
                variables.algebraic(
                    VarName::new("support"),
                    InstanceId::new(8),
                    real,
                    at,
                    attributes(),
                )?,
                variables.algebraic(
                    VarName::new("mount"),
                    InstanceId::new(9),
                    real,
                    at,
                    attributes(),
                )?,
                variables.algebraic(
                    VarName::new("shifted"),
                    InstanceId::new(10),
                    real,
                    at,
                    attributes(),
                )?,
                variables.algebraic(
                    VarName::new("acc_y"),
                    InstanceId::new(11),
                    real,
                    at,
                    attributes(),
                )?,
            ],
        })
    })
}

fn parameter_support_position_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    variables: &ParameterSupportVariables<'dae>,
    spans: &[dae::DaeProvenance],
    declaration: dae::DaeProvenance,
) -> Result<(dae::ExprId<'dae>, Vec<dae::ExprId<'dae>>), dae::DaeConstructionError> {
    let phi0 = coordinate(
        expressions,
        spans[3],
        dae::CoordinateInput::Parameter(variables.phi0),
    )?;
    let binding = expressions
        .at(declaration)
        .literal(dae::DaeLiteral::Real(0.5))?;
    let port_x = coordinate(
        expressions,
        spans[4],
        dae::CoordinateInput::Algebraic(variables.algebraics[0]),
    )?;
    let support = coordinate(
        expressions,
        spans[4],
        dae::CoordinateInput::Algebraic(variables.algebraics[2]),
    )?;
    let port_minus_support = subtraction(expressions, spans[4], port_x, support)?;
    let two = expressions
        .at(spans[5])
        .literal(dae::DaeLiteral::Real(2.0))?;
    let port_y = coordinate(
        expressions,
        spans[5],
        dae::CoordinateInput::Algebraic(variables.algebraics[1]),
    )?;
    let twice_port_y =
        expressions
            .at(spans[5])
            .binary(dae::BinaryOperator::Multiply, two, port_y)?;
    let mount = coordinate(
        expressions,
        spans[3],
        dae::CoordinateInput::Algebraic(variables.algebraics[3]),
    )?;
    let shifted4 = coordinate(
        expressions,
        spans[4],
        dae::CoordinateInput::Algebraic(variables.algebraics[4]),
    )?;
    let shifted5 = coordinate(
        expressions,
        spans[5],
        dae::CoordinateInput::Algebraic(variables.algebraics[4]),
    )?;
    Ok((
        binding,
        vec![
            coordinate_residual(
                expressions,
                spans[0],
                dae::CoordinateInput::State(variables.states[0]),
                dae::CoordinateInput::Algebraic(variables.algebraics[0]),
            )?,
            coordinate_residual(
                expressions,
                spans[1],
                dae::CoordinateInput::State(variables.states[2]),
                dae::CoordinateInput::Algebraic(variables.algebraics[1]),
            )?,
            coordinate_residual(
                expressions,
                spans[2],
                dae::CoordinateInput::Algebraic(variables.algebraics[2]),
                dae::CoordinateInput::Algebraic(variables.algebraics[3]),
            )?,
            subtraction(expressions, spans[3], mount, phi0)?,
            subtraction(expressions, spans[4], shifted4, port_minus_support)?,
            subtraction(expressions, spans[5], shifted5, twice_port_y)?,
        ],
    ))
}

fn parameter_support_derivative_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    variables: &ParameterSupportVariables<'dae>,
    spans: &[dae::DaeProvenance],
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let one = expressions
        .at(spans[2])
        .literal(dae::DaeLiteral::Real(1.0))?;
    let derivative_w = coordinate(
        expressions,
        spans[2],
        dae::CoordinateInput::Derivative(variables.states[1]),
    )?;
    Ok(vec![
        coordinate_residual(
            expressions,
            spans[0],
            dae::CoordinateInput::Derivative(variables.states[0]),
            dae::CoordinateInput::State(variables.states[1]),
        )?,
        coordinate_residual(
            expressions,
            spans[1],
            dae::CoordinateInput::Derivative(variables.states[2]),
            dae::CoordinateInput::State(variables.states[3]),
        )?,
        subtraction(expressions, spans[2], derivative_w, one)?,
        coordinate_residual(
            expressions,
            spans[3],
            dae::CoordinateInput::Derivative(variables.states[3]),
            dae::CoordinateInput::Algebraic(variables.algebraics[5]),
        )?,
    ])
}

/// The support path in `Rotational.Examples.First`, reduced to the exact
/// parameter pin, connector hop, and displaced component relation at issue.
fn parameter_chained_offset_model() -> dae::Dae {
    const TEXT: &str = "parameter Real phi0=0.5; Real x; Real w; Real y; Real v; Real port_x; Real port_y; Real support; Real mount; Real shifted; Real acc_y; equation x = port_x; y = port_y; support = mount; mount = phi0; shifted = port_x - support; shifted = 2*port_y; der(x) = w; der(y) = v; der(w) = 1; der(v) = acc_y;";
    const EQUATIONS: [&str; 10] = [
        "x = port_x",
        "y = port_y",
        "support = mount",
        "mount = phi0",
        "shifted = port_x - support",
        "shifted = 2*port_y",
        "der(x) = w",
        "der(y) = v",
        "der(w) = 1",
        "der(v) = acc_y",
    ];
    let mut sources = SourceMap::new();
    let source = sources.add("parameter_support_chain.mo", TEXT);
    dae::Dae::construct(sources, |model| {
        let declaration = source_provenance(source, TEXT, "equation");
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variables = declare_parameter_support_variables(model, real, declaration)?;
        let spans = EQUATIONS.map(|equation| source_provenance(source, TEXT, equation));
        let (binding, residuals) = model.expressions(|expressions| {
            let (binding, mut residuals) = parameter_support_position_residuals(
                expressions,
                &variables,
                &spans[..6],
                declaration,
            )?;
            residuals.extend(parameter_support_derivative_residuals(
                expressions,
                &variables,
                &spans[6..],
            )?);
            Ok((binding, residuals))
        })?;
        model.variables(|catalog| {
            catalog.define(
                variables.phi0_reservation,
                dae::VariableAttributes {
                    binding: Some(binding),
                    ..dae::VariableAttributes::default()
                },
                declaration,
            )
        })?;
        model.continuous(|continuous| {
            for (span, residual) in spans.into_iter().zip(residuals) {
                continuous.value_equation(span, residual)?;
            }
            Ok(())
        })
    })
    .expect("parameter-support fixture constructs")
}

fn variable_index(view: dae::DaeView<'_>, name: &str) -> u32 {
    view.variables()
        .find_map(|(id, variable)| (variable.name().as_str() == name).then_some(id.index()))
        .expect("fixture variable exists")
}

fn assert_state_only_operation(operation: dae::ExpressionOperation<'_>) {
    let dae::ExpressionOperation::Coordinate(coordinate) = operation else {
        return;
    };
    assert!(
        !matches!(
            coordinate,
            dae::CoordinateView::Algebraic(_) | dae::CoordinateView::Derivative(_)
        ),
        "the retained manifold is state/invariant-only"
    );
}

fn anchorless_cycle_model() -> dae::Dae {
    const TEXT: &str = "Real a; Real b; Real c; Real d; equation a = b - c; b = a - d;";
    const EQUATIONS: [&str; 2] = ["a = b - c", "b = a - d"];
    let mut sources = SourceMap::new();
    let source = sources.add("anchorless_cycle.mo", TEXT);
    dae::Dae::construct(sources, |model| {
        let declaration = source_provenance(source, TEXT, "equation");
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variables = model.variables(|variables| {
            let attributes = dae::VariableAttributes::default();
            Ok([
                variables.algebraic(
                    VarName::new("a"),
                    InstanceId::new(1),
                    real,
                    declaration,
                    attributes.clone(),
                )?,
                variables.algebraic(
                    VarName::new("b"),
                    InstanceId::new(2),
                    real,
                    declaration,
                    attributes.clone(),
                )?,
                variables.algebraic(
                    VarName::new("c"),
                    InstanceId::new(3),
                    real,
                    declaration,
                    attributes.clone(),
                )?,
                variables.algebraic(
                    VarName::new("d"),
                    InstanceId::new(4),
                    real,
                    declaration,
                    attributes,
                )?,
            ])
        })?;
        let spans = EQUATIONS.map(|equation| source_provenance(source, TEXT, equation));
        let residuals = model.expressions(|expressions| {
            let [a, b, c, d] = variables;
            let a0 = coordinate(expressions, spans[0], dae::CoordinateInput::Algebraic(a))?;
            let b0 = coordinate(expressions, spans[0], dae::CoordinateInput::Algebraic(b))?;
            let c0 = coordinate(expressions, spans[0], dae::CoordinateInput::Algebraic(c))?;
            let rhs0 = subtraction(expressions, spans[0], b0, c0)?;
            let b1 = coordinate(expressions, spans[1], dae::CoordinateInput::Algebraic(b))?;
            let a1 = coordinate(expressions, spans[1], dae::CoordinateInput::Algebraic(a))?;
            let d1 = coordinate(expressions, spans[1], dae::CoordinateInput::Algebraic(d))?;
            let rhs1 = subtraction(expressions, spans[1], a1, d1)?;
            Ok([
                subtraction(expressions, spans[0], a0, rhs0)?,
                subtraction(expressions, spans[1], b1, rhs1)?,
            ])
        })?;
        model.continuous(|continuous| {
            for (span, residual) in spans.into_iter().zip(residuals) {
                continuous.value_equation(span, residual)?;
            }
            Ok(())
        })
    })
    .expect("anchorless-cycle fixture constructs")
}

fn odd_invariant_parity_model() -> dae::Dae {
    const TEXT: &str = "Real x; Real port; Real b; Real a; Real shifted; equation x = port; b = 0; a + b = 0; shifted = port - a;";
    const EQUATIONS: [&str; 4] = ["x = port", "b = 0", "a + b = 0", "shifted = port - a"];
    let mut sources = SourceMap::new();
    let source = sources.add("odd_invariant_parity.mo", TEXT);
    dae::Dae::construct(sources, |model| {
        let declaration = source_provenance(source, TEXT, "equation");
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let (x, [port, b, a, shifted]) = model.variables(|variables| {
            let attributes = dae::VariableAttributes::default();
            Ok((
                variables.state(
                    VarName::new("x"),
                    InstanceId::new(1),
                    real,
                    declaration,
                    attributes.clone(),
                )?,
                [
                    variables.algebraic(
                        VarName::new("port"),
                        InstanceId::new(2),
                        real,
                        declaration,
                        attributes.clone(),
                    )?,
                    variables.algebraic(
                        VarName::new("b"),
                        InstanceId::new(3),
                        real,
                        declaration,
                        attributes.clone(),
                    )?,
                    variables.algebraic(
                        VarName::new("a"),
                        InstanceId::new(4),
                        real,
                        declaration,
                        attributes.clone(),
                    )?,
                    variables.algebraic(
                        VarName::new("shifted"),
                        InstanceId::new(5),
                        real,
                        declaration,
                        attributes,
                    )?,
                ],
            ))
        })?;
        let spans = EQUATIONS.map(|equation| source_provenance(source, TEXT, equation));
        let residuals = model.expressions(|expressions| {
            let x = coordinate(expressions, spans[0], dae::CoordinateInput::State(x))?;
            let port0 = coordinate(expressions, spans[0], dae::CoordinateInput::Algebraic(port))?;
            let b1 = coordinate(expressions, spans[1], dae::CoordinateInput::Algebraic(b))?;
            let zero = expressions
                .at(spans[1])
                .literal(dae::DaeLiteral::Real(0.0))?;
            let a2 = coordinate(expressions, spans[2], dae::CoordinateInput::Algebraic(a))?;
            let b2 = coordinate(expressions, spans[2], dae::CoordinateInput::Algebraic(b))?;
            let opposite = expressions
                .at(spans[2])
                .binary(dae::BinaryOperator::Add, a2, b2)?;
            let shifted3 = coordinate(
                expressions,
                spans[3],
                dae::CoordinateInput::Algebraic(shifted),
            )?;
            let port3 = coordinate(expressions, spans[3], dae::CoordinateInput::Algebraic(port))?;
            let a3 = coordinate(expressions, spans[3], dae::CoordinateInput::Algebraic(a))?;
            let port_minus_a = subtraction(expressions, spans[3], port3, a3)?;
            Ok([
                subtraction(expressions, spans[0], x, port0)?,
                subtraction(expressions, spans[1], b1, zero)?,
                opposite,
                subtraction(expressions, spans[3], shifted3, port_minus_a)?,
            ])
        })?;
        model.continuous(|continuous| {
            for (span, residual) in spans.into_iter().zip(residuals) {
                continuous.value_equation(span, residual)?;
            }
            Ok(())
        })
    })
    .expect("odd-parity fixture constructs")
}

#[test]
fn literal_zero_support_exposes_an_exact_holonomic_edge() {
    let model = offset_model(OffsetKind::Zero);
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        let x = variable_index(view, "x");
        let shifted = variable_index(view, "shifted");
        assert_eq!(
            equalities.value_anchor_of(shifted),
            Some((EqualityAnchor::State(x), EqualitySign::Same))
        );
        assert_eq!(holonomic_constraints(view).len(), 1);
    });
    let prepared = prepare_for_solve(&model).expect("zero-offset constraint reduces");
    let PreparedDae::Transformed { dae, manifold, .. } = prepared else {
        panic!("zero-offset constraint requires a holonomic replacement")
    };
    assert_eq!(manifold.len(), 2);
    dae.inspect(|view| {
        assert!(sort(view).is_ok());
        for expression in manifold {
            let expression = view
                .expression_id(expression as usize)
                .expect("manifold expression resolves");
            dae::for_each_expression(view, expression, |_, node| {
                assert_state_only_operation(node.operation());
            });
        }
    });
}

#[test]
fn nonzero_offset_is_derivative_only_and_cannot_forge_a_manifold() {
    let model = offset_model(OffsetKind::Half);
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        let x = variable_index(view, "x");
        let shifted = variable_index(view, "shifted");
        assert_eq!(
            equalities.anchor_of(shifted),
            Some((EqualityAnchor::State(x), EqualitySign::Same)),
            "the affine edge supplies a derivative anchor"
        );
        assert_eq!(equalities.value_anchor_of(shifted), None);
        assert!(
            holonomic_constraints(view).is_empty(),
            "an unrepresented offset cannot enter a state-only manifold"
        );
    });
    assert!(prepare_for_solve(&model).is_err());
}

#[test]
fn chained_parameter_support_is_derivative_only_until_its_value_is_retained() {
    let model = parameter_chained_offset_model();
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        let x = variable_index(view, "x");
        let shifted = variable_index(view, "shifted");
        assert_eq!(
            equalities.anchor_of(shifted),
            Some((EqualityAnchor::State(x), EqualitySign::Same)),
            "the exact mount hop and affine support edge reach the state derivative"
        );
        assert_eq!(
            equalities.value_anchor_of(shifted),
            None,
            "the parameter displacement is not an offset-free value alias"
        );
        assert!(
            holonomic_constraints(view).is_empty(),
            "the current relation cannot materialize the retained parameter displacement"
        );
    });
    assert!(
        prepare_for_solve(&model).is_err(),
        "the current reducer fails closed instead of dropping the parameter displacement"
    );
}

#[test]
fn varying_and_nonunit_operands_infer_no_edge() {
    for kind in [OffsetKind::Varying, OffsetKind::NonUnitCoefficient] {
        let model = offset_model(kind);
        model.inspect(|view| {
            let shifted = variable_index(view, "shifted");
            let equalities = SystemEqualities::collect(view);
            assert_eq!(equalities.anchor_of(shifted), None);
            assert!(holonomic_constraints(view).is_empty());
        });
    }
}

#[test]
fn an_anchorless_balance_cycle_cannot_start_its_own_proof() {
    let model = anchorless_cycle_model();
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        for variable in 0..view.variable_count() as u32 {
            assert_eq!(equalities.anchor_of(variable), None);
            assert_eq!(equalities.value_anchor_of(variable), None);
        }
        assert!(holonomic_constraints(view).is_empty());
        assert!(sort(view).is_err(), "the unsupported system fails closed");
    });
}

#[test]
fn deferred_balance_order_does_not_change_the_closed_proof() {
    let model = offset_model(OffsetKind::Zero);
    model.inspect(|view| {
        let normal = SystemEqualities::collect(view);
        let reversed = SystemEqualities::collect_with_reversed_deferred_balances(view);
        for variable in 0..view.variable_count() as u32 {
            assert_eq!(normal.anchor_of(variable), reversed.anchor_of(variable));
            assert_eq!(
                normal.value_anchor_of(variable),
                reversed.value_anchor_of(variable)
            );
        }
    });
}

#[test]
fn odd_invariant_parity_keeps_value_sign_but_canonicalizes_zero_derivative() {
    let model = odd_invariant_parity_model();
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        let x = variable_index(view, "x");
        let a = variable_index(view, "a");
        let shifted = variable_index(view, "shifted");
        let (derivative_anchor, derivative_sign) =
            equalities.anchor_of(a).expect("a is invariant-anchored");
        assert!(matches!(
            derivative_anchor,
            EqualityAnchor::Invariant { .. }
        ));
        assert_eq!(derivative_sign, EqualitySign::Same);
        let (value_anchor, value_sign) = equalities
            .value_anchor_of(a)
            .expect("a has an exact invariant value");
        assert!(matches!(value_anchor, EqualityAnchor::Invariant { .. }));
        assert_eq!(value_sign, EqualitySign::Opposite);
        assert_eq!(
            equalities.value_anchor_of(shifted),
            Some((EqualityAnchor::State(x), EqualitySign::Same))
        );
    });
}
