//! Connector-shaped singular systems the equality closure has to see through.
//!
//! Each fixture is written in the exact shape MSL produces: a component body
//! states `phi - flange.phi`, a `connect` states `flange_a.phi - flange_b.phi`,
//! and a node balance states `p.i + n.i`. The fixtures stay small enough to
//! reason about by hand while keeping those spellings literal, so a regression
//! in the closure shows up as a system that stops reducing rather than as a
//! wrong number deep inside a real machine model.

use super::*;
use crate::dae_transform::constraints::{
    direct_state_constraints, explicit_derivative_definitions,
};
use crate::dae_transform::equalities::{
    EqualityAnchor, EqualitySign, SingletonRealProjection, SystemEqualities,
    singleton_real_projection,
};

/// One declared fixture variable, keeping the role it was declared with.
#[derive(Clone, Copy)]
enum Declared<'dae> {
    Parameter(dae::ParameterId<'dae>),
    State(dae::StateId<'dae>),
    Algebraic(dae::AlgebraicId<'dae>),
}

impl<'dae> Declared<'dae> {
    fn value(self) -> dae::CoordinateInput<'dae> {
        match self {
            Self::Parameter(id) => dae::CoordinateInput::Parameter(id),
            Self::State(id) => dae::CoordinateInput::State(id),
            Self::Algebraic(id) => dae::CoordinateInput::Algebraic(id),
        }
    }

    fn derivative(self) -> dae::CoordinateInput<'dae> {
        let Self::State(id) = self else {
            panic!("only a fixture state has a derivative coordinate")
        };
        dae::CoordinateInput::Derivative(id)
    }
}

/// Declare `names` in order, reading the role off a one-character prefix:
/// `p` parameter, `s` state, `a` algebraic.
fn declare<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    declaration: dae::DaeProvenance,
    names: &[&str],
) -> Result<Vec<Declared<'dae>>, dae::DaeConstructionError> {
    model.variables(|variables| {
        names
            .iter()
            .map(|entry| {
                let (role, name) = entry.split_at(1);
                let name = VarName::new(name);
                let attributes = dae::VariableAttributes::default();
                Ok(match role {
                    "p" => Declared::Parameter(variables.parameter(
                        name,
                        real,
                        declaration,
                        attributes,
                    )?),
                    "s" => Declared::State(variables.state(name, real, declaration, attributes)?),
                    _ => Declared::Algebraic(variables.algebraic(
                        name,
                        real,
                        declaration,
                        attributes,
                    )?),
                })
            })
            .collect()
    })
}

/// A fixture body, branded to the construction it fills in.
type FixtureBody = for<'borrow, 'dae> fn(
    &'borrow mut dae::DaeConstruction<'dae>,
    &[Declared<'dae>],
    &[dae::DaeProvenance],
) -> Result<(), dae::DaeConstructionError>;

/// Build one whole-model fixture from its source text, declarations, and the
/// residual bodies its equation spans own.
fn connector_fixture(
    text: &'static str,
    names: &'static [&'static str],
    equations: &'static [&'static str],
    build: FixtureBody,
) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("checked_connector_closure.mo", text);
    dae::Dae::construct(sources, |model| {
        let declaration = source_provenance(source, text, "equation");
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let declared = declare(model, real, declaration, names)?;
        let spans = equations
            .iter()
            .map(|equation| source_provenance(source, text, equation))
            .collect::<Vec<_>>();
        build(model, &declared, &spans)
    })
    .expect("connector fixture DAE is valid")
}

/// Name one declared coordinate at `span`.
fn coordinate<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    span: dae::DaeProvenance,
    coordinate: dae::CoordinateInput<'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    expressions.at(span).coordinate(coordinate)
}

/// Turn `lhs = rhs` pairs into the subtractive residuals a DAE stores.
fn residuals<'dae, const N: usize>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    terms: [(dae::DaeProvenance, dae::ExprId<'dae>, dae::ExprId<'dae>); N],
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    terms
        .into_iter()
        .map(|(span, lhs, rhs)| {
            expressions
                .at(span)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })
        .collect()
}

/// Register `residuals` as this fixture's continuous equations, one per span.
fn register<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    spans: &[dae::DaeProvenance],
    residuals: Vec<dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    model.continuous(|continuous| {
        for (span, residual) in spans.iter().copied().zip(residuals) {
            continuous.value_equation(span, residual)?;
        }
        Ok(())
    })
}

/// `phi1 = flange.phi; phi2 = flange.phi; der(phi1) = a; der(phi2) = b; b = 1`
///
/// Two states are the same angle, connected only through the shared connector
/// algebraic. Nothing here names both states in one residual, so the redundancy
/// is invisible without transitive closure.
const ALIAS_CHAIN_TEXT: &str = "Real phi1; Real phi2; Real flange; Real a; Real b; equation phi1 = flange; phi2 = flange; der(phi1) = a; der(phi2) = b; b = 1;";
const ALIAS_CHAIN_NAMES: &[&str] = &["sphi1", "sphi2", "aflange", "aa", "ab"];
const ALIAS_CHAIN_EQUATIONS: &[&str] = &[
    "phi1 = flange",
    "phi2 = flange",
    "der(phi1) = a",
    "der(phi2) = b",
    "b = 1",
];

fn alias_chain_model() -> dae::Dae {
    connector_fixture(
        ALIAS_CHAIN_TEXT,
        ALIAS_CHAIN_NAMES,
        ALIAS_CHAIN_EQUATIONS,
        |model, declared, spans| {
            let [phi1, phi2, flange, a, b] = *declared else {
                unreachable!("fixture declares five variables")
            };
            let residuals = model.expressions(|expressions| {
                let flange_value = coordinate(expressions, spans[0], flange.value())?;
                let one = expressions
                    .at(spans[4])
                    .literal(dae::DaeLiteral::Real(1.0))?;
                let terms = [
                    (
                        spans[0],
                        coordinate(expressions, spans[0], phi1.value())?,
                        flange_value,
                    ),
                    (
                        spans[1],
                        coordinate(expressions, spans[1], phi2.value())?,
                        flange_value,
                    ),
                    (
                        spans[2],
                        coordinate(expressions, spans[2], phi1.derivative())?,
                        coordinate(expressions, spans[2], a.value())?,
                    ),
                    (
                        spans[3],
                        coordinate(expressions, spans[3], phi2.derivative())?,
                        coordinate(expressions, spans[3], b.value())?,
                    ),
                    (spans[4], coordinate(expressions, spans[4], b.value())?, one),
                ];
                residuals(expressions, terms)
            })?;
            register(model, spans, residuals)
        },
    )
}

/// `phi1 = angle1; phi2 = angle2; angle1 = 2*angle2;`
/// `der(phi1) = w1; der(phi2) = w2; der(w1) = acc1;`
/// `der(w2) = acc2; acc1 = 1`
///
/// The position constraint is written entirely in connector algebraics. The
/// two adjacent component equations prove which state each endpoint names,
/// while the acceleration equations make the second derivative exact.
const HIDDEN_HOLONOMIC_TEXT: &str = "Real phi1; Real w1; Real phi2; Real w2; Real angle1; Real angle2; Real acc1; Real acc2; equation phi1 = angle1; phi2 = angle2; angle1 = 2*angle2; der(phi1) = w1; der(phi2) = w2; der(w1) = acc1; der(w2) = acc2; acc1 = 1;";
const HIDDEN_HOLONOMIC_NAMES: &[&str] = &[
    "sphi1", "sw1", "sphi2", "sw2", "aangle1", "aangle2", "aacc1", "aacc2",
];
const HIDDEN_HOLONOMIC_EQUATIONS: &[&str] = &[
    "phi1 = angle1",
    "phi2 = angle2",
    "angle1 = 2*angle2",
    "der(phi1) = w1",
    "der(phi2) = w2",
    "der(w1) = acc1",
    "der(w2) = acc2",
    "acc1 = 1",
];

fn hidden_holonomic_model() -> dae::Dae {
    connector_fixture(
        HIDDEN_HOLONOMIC_TEXT,
        HIDDEN_HOLONOMIC_NAMES,
        HIDDEN_HOLONOMIC_EQUATIONS,
        |model, declared, spans| {
            let [phi1, w1, phi2, w2, angle1, angle2, acc1, acc2] = *declared else {
                unreachable!("fixture declares eight variables")
            };
            let residuals = model.expressions(|expressions| {
                let two = expressions
                    .at(spans[2])
                    .literal(dae::DaeLiteral::Real(2.0))?;
                let one = expressions
                    .at(spans[7])
                    .literal(dae::DaeLiteral::Real(1.0))?;
                let angle2_value = coordinate(expressions, spans[2], angle2.value())?;
                let twice_angle2 = expressions.at(spans[2]).binary(
                    dae::BinaryOperator::Multiply,
                    two,
                    angle2_value,
                )?;
                let terms = [
                    (
                        spans[0],
                        coordinate(expressions, spans[0], phi1.value())?,
                        coordinate(expressions, spans[0], angle1.value())?,
                    ),
                    (
                        spans[1],
                        coordinate(expressions, spans[1], phi2.value())?,
                        coordinate(expressions, spans[1], angle2.value())?,
                    ),
                    (
                        spans[2],
                        coordinate(expressions, spans[2], angle1.value())?,
                        twice_angle2,
                    ),
                    (
                        spans[3],
                        coordinate(expressions, spans[3], phi1.derivative())?,
                        coordinate(expressions, spans[3], w1.value())?,
                    ),
                    (
                        spans[4],
                        coordinate(expressions, spans[4], phi2.derivative())?,
                        coordinate(expressions, spans[4], w2.value())?,
                    ),
                    (
                        spans[5],
                        coordinate(expressions, spans[5], w1.derivative())?,
                        coordinate(expressions, spans[5], acc1.value())?,
                    ),
                    (
                        spans[6],
                        coordinate(expressions, spans[6], w2.derivative())?,
                        coordinate(expressions, spans[6], acc2.value())?,
                    ),
                    (
                        spans[7],
                        coordinate(expressions, spans[7], acc1.value())?,
                        one,
                    ),
                ];
                residuals(expressions, terms)
            })?;
            register(model, spans, residuals)
        },
    )
}

/// Two independent connector-hidden position constraints. Replacing either
/// one alone cuts the unmatched residue from four to two; only the accumulated
/// pair is sortable. The non-unit scale keeps either constraint out of the
/// direct state-equality demotion lane.
const INDEPENDENT_HOLONOMIC_TEXT: &str = "Real phi1; Real w1; Real phi2; Real w2; Real angle1; Real angle2; Real acc1; Real acc2; Real phi3; Real w3; Real phi4; Real w4; Real angle3; Real angle4; Real acc3; Real acc4; equation phi1 = angle1; phi2 = angle2; angle1 = 2*angle2; der(phi1) = w1; der(phi2) = w2; der(w1) = acc1; der(w2) = acc2; acc1 = 1; phi3 = angle3; phi4 = angle4; angle3 = 2*angle4; der(phi3) = w3; der(phi4) = w4; der(w3) = acc3; der(w4) = acc4; acc3 = 1;";
const INDEPENDENT_HOLONOMIC_NAMES: &[&str] = &[
    "sphi1", "sw1", "sphi2", "sw2", "aangle1", "aangle2", "aacc1", "aacc2", "sphi3", "sw3",
    "sphi4", "sw4", "aangle3", "aangle4", "aacc3", "aacc4",
];
const INDEPENDENT_HOLONOMIC_EQUATIONS: &[&str] = &[
    "phi1 = angle1",
    "phi2 = angle2",
    "angle1 = 2*angle2",
    "der(phi1) = w1",
    "der(phi2) = w2",
    "der(w1) = acc1",
    "der(w2) = acc2",
    "acc1 = 1",
    "phi3 = angle3",
    "phi4 = angle4",
    "angle3 = 2*angle4",
    "der(phi3) = w3",
    "der(phi4) = w4",
    "der(w3) = acc3",
    "der(w4) = acc4",
    "acc3 = 1",
];

/// `pub(super)`: reused by the sibling `reduction_observation` module for the
/// permanent regression's 4-2-0 accumulated holonomic chain.
pub(super) fn independent_holonomic_model() -> dae::Dae {
    connector_fixture(
        INDEPENDENT_HOLONOMIC_TEXT,
        INDEPENDENT_HOLONOMIC_NAMES,
        INDEPENDENT_HOLONOMIC_EQUATIONS,
        build_independent_holonomic,
    )
}

fn build_independent_holonomic<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    declared: &[Declared<'dae>],
    spans: &[dae::DaeProvenance],
) -> Result<(), dae::DaeConstructionError> {
    let [
        phi1,
        w1,
        phi2,
        w2,
        angle1,
        angle2,
        acc1,
        acc2,
        phi3,
        w3,
        phi4,
        w4,
        angle3,
        angle4,
        acc3,
        acc4,
    ] = *declared
    else {
        unreachable!("fixture declares sixteen variables")
    };
    let residuals = model.expressions(|expressions| {
        let mut built = independent_holonomic_half(
            expressions,
            &spans[..8],
            [phi1, w1, phi2, w2, angle1, angle2, acc1, acc2],
        )?;
        built.extend(independent_holonomic_half(
            expressions,
            &spans[8..],
            [phi3, w3, phi4, w4, angle3, angle4, acc3, acc4],
        )?);
        Ok(built)
    })?;
    register(model, spans, residuals)
}

fn independent_holonomic_half<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    spans: &[dae::DaeProvenance],
    declared: [Declared<'dae>; 8],
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let [phi1, w1, phi2, w2, angle1, angle2, acc1, acc2] = declared;
    let two = expressions
        .at(spans[2])
        .literal(dae::DaeLiteral::Real(2.0))?;
    let one = expressions
        .at(spans[7])
        .literal(dae::DaeLiteral::Real(1.0))?;
    let angle2_value = coordinate(expressions, spans[2], angle2.value())?;
    let twice_angle2 =
        expressions
            .at(spans[2])
            .binary(dae::BinaryOperator::Multiply, two, angle2_value)?;
    let terms = [
        (
            spans[0],
            coordinate(expressions, spans[0], phi1.value())?,
            coordinate(expressions, spans[0], angle1.value())?,
        ),
        (
            spans[1],
            coordinate(expressions, spans[1], phi2.value())?,
            coordinate(expressions, spans[1], angle2.value())?,
        ),
        (
            spans[2],
            coordinate(expressions, spans[2], angle1.value())?,
            twice_angle2,
        ),
        (
            spans[3],
            coordinate(expressions, spans[3], phi1.derivative())?,
            coordinate(expressions, spans[3], w1.value())?,
        ),
        (
            spans[4],
            coordinate(expressions, spans[4], phi2.derivative())?,
            coordinate(expressions, spans[4], w2.value())?,
        ),
        (
            spans[5],
            coordinate(expressions, spans[5], w1.derivative())?,
            coordinate(expressions, spans[5], acc1.value())?,
        ),
        (
            spans[6],
            coordinate(expressions, spans[6], w2.derivative())?,
            coordinate(expressions, spans[6], acc2.value())?,
        ),
        (
            spans[7],
            coordinate(expressions, spans[7], acc1.value())?,
            one,
        ),
    ];
    residuals(expressions, terms)
}

/// `pi + ni = 0; ni + q = 0; q = I; psi = pi; der(psi) = v; v = w`
///
/// The flux state is pinned to a constant excitation current through two node
/// balances, which only prove `pi = q` once their signs cancel.
const FLOW_BALANCE_TEXT: &str = "parameter Real I; Real pi; Real ni; Real q; Real psi; Real v; Real w; equation pi + ni = 0; ni + q = 0; q = I; psi = pi; der(psi) = v; v = w;";
const FLOW_BALANCE_NAMES: &[&str] = &["pI", "api", "ani", "aq", "spsi", "av", "aw"];
const FLOW_BALANCE_EQUATIONS: &[&str] = &[
    "pi + ni = 0",
    "ni + q = 0",
    "q = I",
    "psi = pi",
    "der(psi) = v",
    "v = w",
];

fn flow_balance_model() -> dae::Dae {
    connector_fixture(
        FLOW_BALANCE_TEXT,
        FLOW_BALANCE_NAMES,
        FLOW_BALANCE_EQUATIONS,
        |model, declared, spans| {
            let [current, pin, node, q, psi, v, w] = *declared else {
                unreachable!("fixture declares seven variables")
            };
            let residuals = model.expressions(|expressions| {
                let pin_value = coordinate(expressions, spans[0], pin.value())?;
                let node_value = coordinate(expressions, spans[0], node.value())?;
                let q_value = coordinate(expressions, spans[1], q.value())?;
                let v_value = coordinate(expressions, spans[4], v.value())?;
                let current_value = coordinate(expressions, spans[2], current.value())?;
                let psi_value = coordinate(expressions, spans[3], psi.value())?;
                let psi_derivative = coordinate(expressions, spans[4], psi.derivative())?;
                let w_value = coordinate(expressions, spans[5], w.value())?;
                let zero = expressions
                    .at(spans[0])
                    .literal(dae::DaeLiteral::Real(0.0))?;
                let pin_node = expressions.at(spans[0]).binary(
                    dae::BinaryOperator::Add,
                    pin_value,
                    node_value,
                )?;
                let node_q = expressions.at(spans[1]).binary(
                    dae::BinaryOperator::Add,
                    node_value,
                    q_value,
                )?;
                residuals(
                    expressions,
                    [
                        (spans[0], pin_node, zero),
                        (spans[1], node_q, zero),
                        (spans[2], q_value, current_value),
                        (spans[3], psi_value, pin_value),
                        (spans[4], psi_derivative, v_value),
                        (spans[5], v_value, w_value),
                    ],
                )
            })?;
            register(model, spans, residuals)
        },
    )
}

/// `w = der(phi)` — the orientation MSL components actually use.
const REVERSE_DERIVATIVE_TEXT: &str = "Real phi; Real w; equation w = der(phi); der(w) = 1;";
const REVERSE_DERIVATIVE_NAMES: &[&str] = &["sphi", "sw"];
const REVERSE_DERIVATIVE_EQUATIONS: &[&str] = &["w = der(phi)", "der(w) = 1"];

fn reverse_derivative_model() -> dae::Dae {
    connector_fixture(
        REVERSE_DERIVATIVE_TEXT,
        REVERSE_DERIVATIVE_NAMES,
        REVERSE_DERIVATIVE_EQUATIONS,
        |model, declared, spans| {
            let [phi, w] = *declared else {
                unreachable!("fixture declares two variables")
            };
            let residuals = model.expressions(|expressions| {
                let w_value = coordinate(expressions, spans[0], w.value())?;
                let phi_derivative = coordinate(expressions, spans[0], phi.derivative())?;
                let w_derivative = coordinate(expressions, spans[1], w.derivative())?;
                let one = expressions
                    .at(spans[1])
                    .literal(dae::DaeLiteral::Real(1.0))?;
                residuals(
                    expressions,
                    [
                        (spans[0], w_value, phi_derivative),
                        (spans[1], w_derivative, one),
                    ],
                )
            })?;
            register(model, spans, residuals)
        },
    )
}

/// `support = 0; hold = support; w = der(hold)`
///
/// The shape every MSL component with an unused support flange produces: the
/// support angle is pinned to a bare literal rather than to a parameter, and a
/// state is tied to it through a connector algebraic.
const PINNED_SUPPORT_TEXT: &str =
    "Real hold; Real support; Real w; equation support = 0; hold = support; w = der(hold);";
const PINNED_SUPPORT_NAMES: &[&str] = &["shold", "asupport", "aw"];
const PINNED_SUPPORT_EQUATIONS: &[&str] = &["support = 0", "hold = support", "w = der(hold)"];

fn pinned_support_model() -> dae::Dae {
    connector_fixture(
        PINNED_SUPPORT_TEXT,
        PINNED_SUPPORT_NAMES,
        PINNED_SUPPORT_EQUATIONS,
        |model, declared, spans| {
            let [hold, support, w] = *declared else {
                unreachable!("fixture declares three variables")
            };
            let residuals = model.expressions(|expressions| {
                let support_value = coordinate(expressions, spans[0], support.value())?;
                let hold_value = coordinate(expressions, spans[1], hold.value())?;
                let hold_derivative = coordinate(expressions, spans[2], hold.derivative())?;
                let w_value = coordinate(expressions, spans[2], w.value())?;
                let zero = expressions
                    .at(spans[0])
                    .literal(dae::DaeLiteral::Real(0.0))?;
                residuals(
                    expressions,
                    [
                        (spans[0], support_value, zero),
                        (spans[1], hold_value, support_value),
                        (spans[2], w_value, hold_derivative),
                    ],
                )
            })?;
            register(model, spans, residuals)
        },
    )
}

/// `flange = s - L; port = flange; support = 0; hold = port - support;`
/// `w = der(hold); v = der(s); w = 1`
///
/// The shape a rigid body produces: `flange = s - L` states the same position
/// as the body centre displaced by a constant half-length, so the chain from
/// the prescribing component down to the body state is only continuous once
/// that displacement is read as an offset rather than as a third unknown.
const DISPLACED_BODY_TEXT: &str = "parameter Real L; Real s; Real hold; Real flange; Real port; Real support; Real w; Real v; equation flange = s - L; port = flange; support = 0; hold = port - support; w = der(hold); v = der(s); w = 1;";
const DISPLACED_BODY_NAMES: &[&str] = &[
    "pL", "ss", "shold", "aflange", "aport", "asupport", "aw", "av",
];
const DISPLACED_BODY_EQUATIONS: &[&str] = &[
    "flange = s - L",
    "port = flange",
    "support = 0",
    "hold = port - support",
    "w = der(hold)",
    "v = der(s)",
    "w = 1",
];

fn displaced_body_model() -> dae::Dae {
    connector_fixture(
        DISPLACED_BODY_TEXT,
        DISPLACED_BODY_NAMES,
        DISPLACED_BODY_EQUATIONS,
        |model, declared, spans| {
            let [length, s, hold, flange, port, support, w, v] = *declared else {
                unreachable!("fixture declares eight variables")
            };
            let residuals = model.expressions(|expressions| {
                let length_value = coordinate(expressions, spans[0], length.value())?;
                let s_value = coordinate(expressions, spans[0], s.value())?;
                let displaced = expressions.at(spans[0]).binary(
                    dae::BinaryOperator::Subtract,
                    s_value,
                    length_value,
                )?;
                let flange_value = coordinate(expressions, spans[0], flange.value())?;
                let port_value = coordinate(expressions, spans[1], port.value())?;
                let support_value = coordinate(expressions, spans[2], support.value())?;
                let zero = expressions
                    .at(spans[2])
                    .literal(dae::DaeLiteral::Real(0.0))?;
                let hold_value = coordinate(expressions, spans[3], hold.value())?;
                let relative = expressions.at(spans[3]).binary(
                    dae::BinaryOperator::Subtract,
                    port_value,
                    support_value,
                )?;
                let w_value = coordinate(expressions, spans[4], w.value())?;
                let hold_derivative = coordinate(expressions, spans[4], hold.derivative())?;
                let v_value = coordinate(expressions, spans[5], v.value())?;
                let s_derivative = coordinate(expressions, spans[5], s.derivative())?;
                let one = expressions
                    .at(spans[6])
                    .literal(dae::DaeLiteral::Real(1.0))?;
                residuals(
                    expressions,
                    [
                        (spans[0], flange_value, displaced),
                        (spans[1], port_value, flange_value),
                        (spans[2], support_value, zero),
                        (spans[3], hold_value, relative),
                        (spans[4], w_value, hold_derivative),
                        (spans[5], v_value, s_derivative),
                        (spans[6], w_value, one),
                    ],
                )
            })?;
            register(model, spans, residuals)
        },
    )
}

/// `b - L = a; u = der(a); y = der(b); u = 1`
///
/// Two states separated by a constant. Their derivatives are the same quantity
/// and their values are not, and no residual states either state on its own, so
/// the only thing the closure may report here is the derivative.
const DISPLACED_STATES_TEXT: &str = "parameter Real L; Real a; Real b; Real u; Real y; equation b - L = a; u = der(a); y = der(b); u = 1;";
const DISPLACED_STATES_NAMES: &[&str] = &["pL", "sa", "sb", "au", "ay"];
const DISPLACED_STATES_EQUATIONS: &[&str] = &["b - L = a", "u = der(a)", "y = der(b)", "u = 1"];

fn displaced_states_model() -> dae::Dae {
    connector_fixture(
        DISPLACED_STATES_TEXT,
        DISPLACED_STATES_NAMES,
        DISPLACED_STATES_EQUATIONS,
        |model, declared, spans| {
            let [length, a, b, u, y] = *declared else {
                unreachable!("fixture declares five variables")
            };
            let residuals = model.expressions(|expressions| {
                let length_value = coordinate(expressions, spans[0], length.value())?;
                let b_value = coordinate(expressions, spans[0], b.value())?;
                let displaced = expressions.at(spans[0]).binary(
                    dae::BinaryOperator::Subtract,
                    b_value,
                    length_value,
                )?;
                let a_value = coordinate(expressions, spans[0], a.value())?;
                let u_value = coordinate(expressions, spans[1], u.value())?;
                let a_derivative = coordinate(expressions, spans[1], a.derivative())?;
                let y_value = coordinate(expressions, spans[2], y.value())?;
                let b_derivative = coordinate(expressions, spans[2], b.derivative())?;
                let one = expressions
                    .at(spans[3])
                    .literal(dae::DaeLiteral::Real(1.0))?;
                residuals(
                    expressions,
                    [
                        (spans[0], displaced, a_value),
                        (spans[1], u_value, a_derivative),
                        (spans[2], y_value, b_derivative),
                        (spans[3], u_value, one),
                    ],
                )
            })?;
            register(model, spans, residuals)
        },
    )
}

/// `q + I = 0; psi = q; der(psi) = v; v = 1`
///
/// The pin is written with the sign the other way round, so the class is
/// constant at `-I` rather than at `I`.
const OPPOSED_PIN_TEXT: &str =
    "parameter Real I; Real q; Real psi; Real v; equation q + I = 0; psi = q; der(psi) = v; v = 1;";
const OPPOSED_PIN_NAMES: &[&str] = &["pI", "aq", "spsi", "av"];
const OPPOSED_PIN_EQUATIONS: &[&str] = &["q + I = 0", "psi = q", "der(psi) = v", "v = 1"];

fn opposed_pin_model() -> dae::Dae {
    connector_fixture(
        OPPOSED_PIN_TEXT,
        OPPOSED_PIN_NAMES,
        OPPOSED_PIN_EQUATIONS,
        |model, declared, spans| {
            let [current, q, psi, v] = *declared else {
                unreachable!("fixture declares four variables")
            };
            let residuals = model.expressions(|expressions| {
                let q_value = coordinate(expressions, spans[0], q.value())?;
                let current_value = coordinate(expressions, spans[0], current.value())?;
                let opposed = expressions.at(spans[0]).binary(
                    dae::BinaryOperator::Add,
                    q_value,
                    current_value,
                )?;
                let zero = expressions
                    .at(spans[0])
                    .literal(dae::DaeLiteral::Real(0.0))?;
                let psi_value = coordinate(expressions, spans[1], psi.value())?;
                let psi_derivative = coordinate(expressions, spans[2], psi.derivative())?;
                let v_value = coordinate(expressions, spans[2], v.value())?;
                let one = expressions
                    .at(spans[3])
                    .literal(dae::DaeLiteral::Real(1.0))?;
                residuals(
                    expressions,
                    [
                        (spans[0], opposed, zero),
                        (spans[1], psi_value, q_value),
                        (spans[2], psi_derivative, v_value),
                        (spans[3], v_value, one),
                    ],
                )
            })?;
            register(model, spans, residuals)
        },
    )
}

/// `a + b = 0; der(a) = v; der(b) = w; v = 1`
///
/// Two *states* the balance proves opposite, the anti-series shape a pair of
/// opposed inductors or a differential gear writes. The class is exact — no
/// displacement — so everything the closure needs to demote one of them is
/// present except the sign.
const OPPOSED_STATES_TEXT: &str =
    "Real a; Real b; Real v; Real w; equation a + b = 0; der(a) = v; der(b) = w; v = 1;";
const OPPOSED_STATES_NAMES: &[&str] = &["sa", "sb", "av", "aw"];
const OPPOSED_STATES_EQUATIONS: &[&str] = &["a + b = 0", "der(a) = v", "der(b) = w", "v = 1"];

fn opposed_states_model() -> dae::Dae {
    connector_fixture(
        OPPOSED_STATES_TEXT,
        OPPOSED_STATES_NAMES,
        OPPOSED_STATES_EQUATIONS,
        |model, declared, spans| {
            let [a, b, v, w] = *declared else {
                unreachable!("fixture declares four variables")
            };
            let residuals = model.expressions(|expressions| {
                let a_value = coordinate(expressions, spans[0], a.value())?;
                let b_value = coordinate(expressions, spans[0], b.value())?;
                let balance =
                    expressions
                        .at(spans[0])
                        .binary(dae::BinaryOperator::Add, a_value, b_value)?;
                let zero = expressions
                    .at(spans[0])
                    .literal(dae::DaeLiteral::Real(0.0))?;
                let a_derivative = coordinate(expressions, spans[1], a.derivative())?;
                let v_value = coordinate(expressions, spans[1], v.value())?;
                let b_derivative = coordinate(expressions, spans[2], b.derivative())?;
                let w_value = coordinate(expressions, spans[2], w.value())?;
                let one = expressions
                    .at(spans[3])
                    .literal(dae::DaeLiteral::Real(1.0))?;
                residuals(
                    expressions,
                    [
                        (spans[0], balance, zero),
                        (spans[1], a_derivative, v_value),
                        (spans[2], b_derivative, w_value),
                        (spans[3], v_value, one),
                    ],
                )
            })?;
            register(model, spans, residuals)
        },
    )
}

/// `a - b = 0; a + b = 0; der(a) = v; v = 1`
///
/// Two residuals that assert both `a ≡ b` and `a ≡ -b`. Only `a = b = 0`
/// satisfies both, which is not something a closure over signed edges may
/// assume: the class is contradictory, and the closure has to say so rather
/// than pick whichever edge it read last.
const CONTRADICTORY_TEXT: &str =
    "Real a; Real b; Real v; equation a - b = 0; a + b = 0; der(a) = v; v = 1;";
const CONTRADICTORY_NAMES: &[&str] = &["sa", "sb", "av"];
const CONTRADICTORY_EQUATIONS: &[&str] = &["a - b = 0", "a + b = 0", "der(a) = v", "v = 1"];

fn contradictory_equalities_model() -> dae::Dae {
    connector_fixture(
        CONTRADICTORY_TEXT,
        CONTRADICTORY_NAMES,
        CONTRADICTORY_EQUATIONS,
        |model, declared, spans| {
            let [a, b, v] = *declared else {
                unreachable!("fixture declares three variables")
            };
            let residuals = model.expressions(|expressions| {
                let a_value = coordinate(expressions, spans[0], a.value())?;
                let b_value = coordinate(expressions, spans[0], b.value())?;
                let difference = expressions.at(spans[0]).binary(
                    dae::BinaryOperator::Subtract,
                    a_value,
                    b_value,
                )?;
                let sum =
                    expressions
                        .at(spans[1])
                        .binary(dae::BinaryOperator::Add, a_value, b_value)?;
                let zero = expressions
                    .at(spans[0])
                    .literal(dae::DaeLiteral::Real(0.0))?;
                let a_derivative = coordinate(expressions, spans[2], a.derivative())?;
                let v_value = coordinate(expressions, spans[2], v.value())?;
                let one = expressions
                    .at(spans[3])
                    .literal(dae::DaeLiteral::Real(1.0))?;
                residuals(
                    expressions,
                    [
                        (spans[0], difference, zero),
                        (spans[1], sum, zero),
                        (spans[2], a_derivative, v_value),
                        (spans[3], v_value, one),
                    ],
                )
            })?;
            register(model, spans, residuals)
        },
    )
}

/// Whether the aggregate projection carries the only statically provable
/// scalar subscript or a runtime parameter.
#[derive(Clone, Copy)]
enum ProjectionSubscript {
    LiteralOne,
    BoundParameter,
    Parameter,
}

#[derive(Clone, Copy)]
struct ProjectedVariables<'dae> {
    x: dae::StateId<'dae>,
    q: dae::StateId<'dae>,
    i: dae::ParameterId<'dae>,
    a: dae::AlgebraicId<'dae>,
    b: dae::AlgebraicId<'dae>,
    u: dae::AlgebraicId<'dae>,
    v: dae::AlgebraicId<'dae>,
}

/// `der(x) = u; der(q)[i] = v; x = a; a = b; b = q[i]; u = v`
///
/// At extent one with `i = 1` written literally or supplied by a statically
/// evaluable Integer parameter, `q[i]` is the whole Real payload and can anchor
/// the scalar equality class. Larger payloads and runtime indices deliberately
/// use the same equation shape but prove no whole-variable equality.
fn projected_state_model(extent: u32, subscript: ProjectionSubscript) -> dae::Dae {
    const TEXT: &str = "Real x; Real q[:]; Integer i; Real a; Real b; Real u; Real v; equation der(x) = u; der(q)[i] = v; x = a; a = b; b = q[i]; u = v;";
    let mut sources = SourceMap::new();
    let source = sources.add("projected_state.mo", TEXT);
    let at = source_provenance(source, TEXT, "equation");
    dae::Dae::construct(sources, |model| {
        let (real, real_array, integer) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::array(dae::ScalarType::Real, [extent]),
                    at,
                )?,
                types.intern(
                    TypeId::new(2),
                    dae::ValueType::scalar(dae::ScalarType::Integer),
                    at,
                )?,
            ))
        })?;
        let (variables, i_reservation) = model.variables(|variables| {
            let attributes = dae::VariableAttributes::default;
            let (i, i_reservation) = variables.reserve_parameter(VarName::new("i"), integer, at)?;
            Ok((
                ProjectedVariables {
                    x: variables.state(VarName::new("x"), real, at, attributes())?,
                    q: variables.state(VarName::new("q"), real_array, at, attributes())?,
                    i,
                    a: variables.algebraic(VarName::new("a"), real, at, attributes())?,
                    b: variables.algebraic(VarName::new("b"), real, at, attributes())?,
                    u: variables.algebraic(VarName::new("u"), real, at, attributes())?,
                    v: variables.algebraic(VarName::new("v"), real, at, attributes())?,
                },
                i_reservation,
            ))
        })?;
        let (residuals, one) = model.expressions(|expressions| {
            projected_residuals(expressions, variables, at, subscript)
        })?;
        model.variables(|variables| {
            variables.define(
                i_reservation,
                dae::VariableAttributes {
                    binding: matches!(subscript, ProjectionSubscript::BoundParameter)
                        .then_some(one),
                    ..dae::VariableAttributes::default()
                },
                at,
            )
        })?;
        register(model, &[at; 6], residuals)
    })
    .expect("projected-state fixture DAE is valid")
}

fn projected_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    variables: ProjectedVariables<'dae>,
    at: dae::DaeProvenance,
    subscript: ProjectionSubscript,
) -> Result<(Vec<dae::ExprId<'dae>>, dae::ExprId<'dae>), dae::DaeConstructionError> {
    let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
    let index = match subscript {
        ProjectionSubscript::LiteralOne => one,
        ProjectionSubscript::BoundParameter | ProjectionSubscript::Parameter => expressions
            .at(at)
            .coordinate(dae::CoordinateInput::Parameter(variables.i))?,
    };
    let select = |expression| dae::Subscript::Index {
        expression,
        provenance: at,
    };
    let q_value = coordinate(expressions, at, dae::CoordinateInput::State(variables.q))?;
    let q_value = expressions.at(at).index(q_value, [select(index)])?;
    let q_derivative = coordinate(
        expressions,
        at,
        dae::CoordinateInput::Derivative(variables.q),
    )?;
    let q_derivative = expressions.at(at).index(q_derivative, [select(index)])?;
    let x_derivative = coordinate(
        expressions,
        at,
        dae::CoordinateInput::Derivative(variables.x),
    )?;
    let x_value = coordinate(expressions, at, dae::CoordinateInput::State(variables.x))?;
    let a_value = coordinate(
        expressions,
        at,
        dae::CoordinateInput::Algebraic(variables.a),
    )?;
    let b_value = coordinate(
        expressions,
        at,
        dae::CoordinateInput::Algebraic(variables.b),
    )?;
    let u_value = coordinate(
        expressions,
        at,
        dae::CoordinateInput::Algebraic(variables.u),
    )?;
    let v_value = coordinate(
        expressions,
        at,
        dae::CoordinateInput::Algebraic(variables.v),
    )?;
    let residuals = residuals(
        expressions,
        [
            (at, x_derivative, u_value),
            (at, q_derivative, v_value),
            (at, x_value, a_value),
            (at, a_value, b_value),
            (at, b_value, q_value),
            (at, u_value, v_value),
        ],
    )?;
    Ok((residuals, one))
}

#[derive(Clone, Copy)]
struct ProjectedAlgebraicVariables<'dae> {
    s: dae::StateId<'dae>,
    x: dae::StateId<'dae>,
    w: dae::AlgebraicId<'dae>,
    v: dae::AlgebraicId<'dae>,
}

/// A projected algebraic may be equality-anchored on a state, but this slice
/// owns differentiation of projected states only. Admitting `w[1]` as the RHS
/// of the `x` demotion would make reconstruction reach an unsupported Index
/// differentiator arm and panic.
fn projected_algebraic_definition_model() -> dae::Dae {
    const TEXT: &str =
        "Real s; Real x; Real w[1]; Real v; equation der(s) = -s; der(x) = v; x = w[1]; w[1] = s;";
    let mut sources = SourceMap::new();
    let source = sources.add("projected_algebraic_definition.mo", TEXT);
    let at = source_provenance(source, TEXT, "equation");
    dae::Dae::construct(sources, |model| {
        let (real, singleton) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::array(dae::ScalarType::Real, [1]),
                    at,
                )?,
            ))
        })?;
        let variables = model.variables(|variables| {
            let attributes = dae::VariableAttributes::default;
            Ok(ProjectedAlgebraicVariables {
                s: variables.state(VarName::new("s"), real, at, attributes())?,
                x: variables.state(VarName::new("x"), real, at, attributes())?,
                w: variables.algebraic(VarName::new("w"), singleton, at, attributes())?,
                v: variables.algebraic(VarName::new("v"), real, at, attributes())?,
            })
        })?;
        let residuals = model
            .expressions(|expressions| projected_algebraic_residuals(expressions, variables, at))?;
        register(model, &[at; 4], residuals)
    })
    .expect("projected-algebraic fixture DAE is valid")
}

fn pinned_vector_state_model() -> dae::Dae {
    const TEXT: &str = "Real q[3]; Real v[3]; equation q = zeros(3); der(q) = v;";
    let mut sources = SourceMap::new();
    let source = sources.add("pinned_vector_state.mo", TEXT);
    let at = source_provenance(source, TEXT, "equation");
    dae::Dae::construct(sources, |model| {
        let vector = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                at,
            )
        })?;
        let (q, v) = model.variables(|variables| {
            let attributes = dae::VariableAttributes::default;
            Ok((
                variables.state(VarName::new("q"), vector, at, attributes())?,
                variables.algebraic(VarName::new("v"), vector, at, attributes())?,
            ))
        })?;
        let (position, velocity) = model.expressions(|expressions| {
            let q_value = coordinate(expressions, at, dae::CoordinateInput::State(q))?;
            let q_derivative = coordinate(expressions, at, dae::CoordinateInput::Derivative(q))?;
            let v_value = coordinate(expressions, at, dae::CoordinateInput::Algebraic(v))?;
            let extent = expressions.at(at).literal(dae::DaeLiteral::Integer(3))?;
            let zero = expressions
                .at(at)
                .builtin(dae::PureBuiltin::Zeros, [extent])?;
            Ok((
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, q_value, zero)?,
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, q_derivative, v_value)?,
            ))
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(at, position)?;
            continuous.value_equation(at, velocity)
        })
    })
    .expect("pinned vector state fixture is valid")
}

fn function_defined_vector_state_model() -> dae::Dae {
    const TEXT: &str = "parameter Real e[3]; Real omega; Real w[3]; Real alpha; Real a[3]; equation w = spin(e, omega); der(w) = a; der(omega) = alpha; alpha = 1;";
    let mut sources = SourceMap::new();
    let source = sources.add("function_defined_vector_state.mo", TEXT);
    let at = source_provenance(source, TEXT, "equation");
    dae::Dae::construct(sources, |model| {
        let (vector, scalar) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [3]),
                    at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    at,
                )?,
            ))
        })?;
        let (spin, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("spin"), [vector, scalar], [vector], at),
            |model, reservation| {
                let axis = model.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("axis"), 0, at)
                })?;
                let rate = model.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("rate"), 1, at)
                })?;
                let output = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("result"), 0, at)
                })?;
                let value = model.expressions(|expressions| {
                    let axis = expressions.at(at).function_parameter(axis)?;
                    let rate = expressions.at(at).function_parameter(rate)?;
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Multiply, axis, rate)
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, output, value, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        let (axis, omega, w, alpha, a) = model.variables(|variables| {
            let attributes = dae::VariableAttributes::default;
            Ok((
                variables.parameter(VarName::new("e"), vector, at, attributes())?,
                variables.state(VarName::new("omega"), scalar, at, attributes())?,
                variables.state(VarName::new("w"), vector, at, attributes())?,
                variables.algebraic(VarName::new("alpha"), scalar, at, attributes())?,
                variables.algebraic(VarName::new("a"), vector, at, attributes())?,
            ))
        })?;
        let residuals = model.expressions(|expressions| {
            let axis = coordinate(expressions, at, dae::CoordinateInput::Parameter(axis))?;
            let omega_value = coordinate(expressions, at, dae::CoordinateInput::State(omega))?;
            let w_value = coordinate(expressions, at, dae::CoordinateInput::State(w))?;
            let alpha_value = coordinate(expressions, at, dae::CoordinateInput::Algebraic(alpha))?;
            let a_value = coordinate(expressions, at, dae::CoordinateInput::Algebraic(a))?;
            let w_derivative = coordinate(expressions, at, dae::CoordinateInput::Derivative(w))?;
            let omega_derivative =
                coordinate(expressions, at, dae::CoordinateInput::Derivative(omega))?;
            let spin = expressions.at(at).call(spin, 0, [axis, omega_value])?;
            let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            residuals(
                expressions,
                [
                    (at, w_value, spin),
                    (at, w_derivative, a_value),
                    (at, omega_derivative, alpha_value),
                    (at, alpha_value, one),
                ],
            )
        })?;
        register(model, &[at, at, at, at], residuals)
    })
    .expect("function-defined vector state fixture is valid")
}

fn projected_algebraic_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    variables: ProjectedAlgebraicVariables<'dae>,
    at: dae::DaeProvenance,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let s = coordinate(expressions, at, dae::CoordinateInput::State(variables.s))?;
    let x = coordinate(expressions, at, dae::CoordinateInput::State(variables.x))?;
    let w = coordinate(
        expressions,
        at,
        dae::CoordinateInput::Algebraic(variables.w),
    )?;
    let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
    let w = expressions.at(at).index(
        w,
        [dae::Subscript::Index {
            expression: one,
            provenance: at,
        }],
    )?;
    let der_s = coordinate(
        expressions,
        at,
        dae::CoordinateInput::Derivative(variables.s),
    )?;
    let der_x = coordinate(
        expressions,
        at,
        dae::CoordinateInput::Derivative(variables.x),
    )?;
    let v = coordinate(
        expressions,
        at,
        dae::CoordinateInput::Algebraic(variables.v),
    )?;
    let minus_s = expressions.at(at).unary(dae::UnaryOperator::Negate, s)?;
    residuals(
        expressions,
        [(at, der_s, minus_s), (at, der_x, v), (at, x, w), (at, w, s)],
    )
}

fn variable_index(view: dae::DaeView<'_>, name: &str) -> u32 {
    view.variables()
        .find(|(_, variable)| variable.name().as_str() == name)
        .map(|(id, _)| id.index())
        .unwrap_or_else(|| panic!("fixture declares `{name}`"))
}

fn role(dae: &dae::Dae, name: &str) -> dae::VariableRole {
    dae.inspect(|view| {
        view.variables()
            .find(|(_, variable)| variable.name().as_str() == name)
            .map(|(_, variable)| variable.role())
            .unwrap_or_else(|| panic!("reconstructed DAE keeps `{name}`"))
    })
}

/// Every derivative coordinate the reconstructed DAE still names.
fn derivative_coordinates(view: dae::DaeView<'_>) -> Vec<u32> {
    (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .filter_map(|expression| match expression.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) => {
                Some(state.index())
            }
            _ => None,
        })
        .collect()
}

fn algebraic_coordinate<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<u32> {
    let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) =
        view.expression(expression)?.operation()
    else {
        return None;
    };
    Some(id.index())
}

fn negated_algebraic_coordinate<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<u32> {
    let dae::ExpressionOperation::Unary {
        operator: dae::UnaryOperator::Negate,
        mut operand,
    } = view.expression(expression)?.operation()
    else {
        return None;
    };
    if let dae::ExpressionOperation::Unary {
        operator: dae::UnaryOperator::Plus,
        operand: inner,
    } = view.expression(operand)?.operation()
    {
        operand = inner;
    }
    algebraic_coordinate(view, operand)
}

fn is_signed_definition<'dae>(
    view: dae::DaeView<'dae>,
    equation: dae::ResidualEquationView<'dae>,
    signed: u32,
    rhs_coordinate: u32,
) -> bool {
    let Some(residual) = view.expression(equation.residual()) else {
        return false;
    };
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = residual.operation()
    else {
        return false;
    };
    negated_algebraic_coordinate(view, lhs) == Some(signed)
        && algebraic_coordinate(view, rhs) == Some(rhs_coordinate)
}

#[test]
fn connector_alias_chain_demotes_the_redundant_state_and_matches() {
    let model = alias_chain_model();
    assert!(
        model.inspect(|view| sort(view).is_err()),
        "two angles sharing one connector are structurally singular"
    );
    let prepared = prepare_for_solve(&model).expect("alias chain is reducible");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("alias chain requires a state demotion"),
    };
    assert_eq!(role(&transformed, "phi1"), dae::VariableRole::State);
    assert_eq!(role(&transformed, "phi2"), dae::VariableRole::Algebraic);
    transformed.inspect(|view| {
        assert!(sort(view).is_ok(), "replacement DAE matches perfectly");
        assert!(
            !derivative_coordinates(view).contains(&variable_index(view, "phi2")),
            "the demoted angle keeps no derivative coordinate"
        );
    });
}

#[test]
fn alias_closure_reports_the_anchor_state_of_a_connector_chain() {
    let model = alias_chain_model();
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        let phi1 = variable_index(view, "phi1");
        let phi2 = variable_index(view, "phi2");
        let flange = variable_index(view, "flange");
        for member in [phi1, phi2, flange] {
            assert_eq!(
                equalities.anchor_of(member),
                Some((EqualityAnchor::State(phi1), EqualitySign::Same)),
                "every chain member resolves to the kept angle with the same sign"
            );
        }
        assert_eq!(
            equalities
                .redundant_states()
                .map(|(state, _, _)| state)
                .collect::<Vec<_>>(),
            vec![phi2],
            "only the non-anchor state is reported redundant"
        );
    });
}

#[test]
fn opposed_node_balances_pin_a_flux_state_to_its_parameter() {
    let model = flow_balance_model();
    assert!(
        model.inspect(|view| sort(view).is_err()),
        "a flux state pinned to a constant current is structurally singular"
    );
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        let anchor = equalities
            .anchor_of(variable_index(view, "pi"))
            .expect("the pinned current class reports an anchor");
        assert!(
            matches!(
                anchor,
                (EqualityAnchor::Invariant { .. }, EqualitySign::Same)
            ),
            "a class pinned to a parameter reports that invariant"
        );
        assert_eq!(
            equalities.anchor_of(variable_index(view, "ni")),
            Some(anchor),
            "both signs of the node balance land in the same pinned class"
        );
    });
    let prepared = prepare_for_solve(&model).expect("pinned flux state is reducible");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("pinned flux state requires a demotion"),
    };
    assert_eq!(role(&transformed, "psi"), dae::VariableRole::Algebraic);
    transformed.inspect(|view| {
        assert!(sort(view).is_ok(), "replacement DAE matches perfectly");
        assert!(
            derivative_coordinates(view).is_empty(),
            "a constant flux leaves no derivative coordinate behind"
        );
    });
}

#[test]
fn derivative_definitions_are_read_in_either_orientation() {
    let model = reverse_derivative_model();
    model.inspect(|view| {
        let definitions = explicit_derivative_definitions(view);
        let phi = variable_index(view, "phi") as usize;
        let w = variable_index(view, "w") as usize;
        let phi_definition = definitions[phi].expect("`w = der(phi)` defines d/dt phi");
        let definition = view
            .expression(
                view.expression_id(phi_definition as usize)
                    .expect("definition ordinal resolves"),
            )
            .expect("definition identity resolves");
        assert!(
            matches!(
                definition.operation(),
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state))
                    if state.index() as usize == w
            ),
            "the reverse orientation supplies the other side as the definition"
        );
        assert!(
            definitions[w].is_some(),
            "the forward orientation keeps working"
        );
    });
}

#[test]
fn a_state_pinned_to_a_bare_zero_reduces_like_one_pinned_to_a_parameter() {
    let model = pinned_support_model();
    assert!(
        model.inspect(|view| sort(view).is_err()),
        "a state tied to a pinned support is structurally singular"
    );
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        assert!(
            matches!(
                equalities.anchor_of(variable_index(view, "hold")),
                Some((EqualityAnchor::Invariant { .. }, EqualitySign::Same))
            ),
            "a literal pin anchors the class just as a parameter does"
        );
    });
    let prepared = prepare_for_solve(&model).expect("pinned support is reducible");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("pinned support requires a demotion"),
    };
    assert_eq!(role(&transformed, "hold"), dae::VariableRole::Algebraic);
    transformed.inspect(|view| {
        assert!(sort(view).is_ok(), "replacement DAE matches perfectly");
        assert!(
            derivative_coordinates(view).is_empty(),
            "a pinned angle leaves no derivative coordinate behind"
        );
    });
}

#[test]
fn a_displaced_body_chain_supplies_the_derivative_a_demotion_needs() {
    let model = displaced_body_model();
    assert!(
        model.inspect(|view| sort(view).is_err()),
        "a prescribed position tied to a displaced body is structurally singular"
    );
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        let anchored = variable_index(view, "s");
        for member in [variable_index(view, "port"), variable_index(view, "flange")] {
            assert_eq!(
                equalities.anchor_of(member),
                Some((EqualityAnchor::State(anchored), EqualitySign::Same)),
                "the connector chain reaches the body state across the displacement"
            );
        }
        assert!(
            equalities.redundant_states().next().is_none(),
            "no state is proven equal in value to another one here"
        );
    });
    let prepared = prepare_for_solve(&model).expect("displaced body chain is reducible");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("displaced body chain requires a state demotion"),
    };
    assert_eq!(role(&transformed, "hold"), dae::VariableRole::Algebraic);
    assert_eq!(role(&transformed, "s"), dae::VariableRole::State);
    transformed.inspect(|view| {
        assert!(sort(view).is_ok(), "replacement DAE matches perfectly");
        assert!(
            !derivative_coordinates(view).contains(&variable_index(view, "hold")),
            "the demoted position keeps no derivative coordinate"
        );
    });
}

#[test]
fn a_displaced_equality_never_reports_a_redundant_state() {
    let model = displaced_states_model();
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        assert_eq!(
            equalities
                .redundant_states()
                .map(|(state, _, _)| state)
                .collect::<Vec<_>>(),
            Vec::<u32>::new(),
            "`b - L = a` proves a shared derivative, never a shared value"
        );
    });
}

#[test]
fn an_opposite_signed_state_pair_carries_its_sign_into_demotion() {
    let model = opposed_states_model();
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        let a = variable_index(view, "a");
        let b = variable_index(view, "b");
        assert_eq!(
            equalities.anchor_of(b),
            Some((EqualityAnchor::State(a), EqualitySign::Opposite)),
            "`a + b = 0` proves the second state is the first one negated"
        );
        assert_eq!(
            equalities.anchor_of(a),
            Some((EqualityAnchor::State(a), EqualitySign::Same)),
            "the anchor signs against itself"
        );
        assert_eq!(
            equalities.redundant_states().collect::<Vec<_>>(),
            vec![(b, EqualityAnchor::State(a), EqualitySign::Opposite)],
            "the non-anchor state carries the exact sign its class proves"
        );
    });
    assert!(
        model.inspect(|view| sort(view).is_err()),
        "an anti-series state pair is structurally singular"
    );
    let prepared = prepare_for_solve(&model).expect("opposed state pair is reducible");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("opposed state pair requires a state demotion"),
    };
    assert_eq!(role(&transformed, "a"), dae::VariableRole::State);
    assert_eq!(role(&transformed, "b"), dae::VariableRole::Algebraic);
    transformed.inspect(|view| {
        assert!(sort(view).is_ok(), "replacement DAE matches perfectly");
        assert!(
            !derivative_coordinates(view).contains(&variable_index(view, "b")),
            "the demoted state keeps no derivative coordinate"
        );
        let w = variable_index(view, "w");
        let v = variable_index(view, "v");
        let signed_definition = view.continuous_owners().any(|owner| {
            let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
                return false;
            };
            is_signed_definition(view, equation, v, w)
        });
        assert!(
            signed_definition,
            "`der(b) = w` becomes the proved signed definition `-v = w`"
        );
    });
}

#[test]
fn a_connector_hidden_holonomic_constraint_carries_an_anchor_proof() {
    let model = hidden_holonomic_model();
    assert!(
        model.inspect(|view| sort(view).is_err()),
        "the position constraint leaves one acceleration structurally undefined"
    );
    model.inspect(|view| {
        let constraints = holonomic_constraints(view);
        assert_eq!(
            constraints.len(),
            1,
            "only the two-body constraint qualifies"
        );
        let constraint = &constraints[0];
        assert_eq!(
            view.source_text(
                view.expression(view.expression_id(constraint.residual as usize).unwrap())
                    .unwrap()
                    .provenance()
            ),
            Some("angle1 = 2*angle2")
        );
        assert_eq!(constraint.proof.maximum_order, 2);
        assert_eq!(constraint.proof.residual, constraint.residual);
        assert_eq!(
            &*constraint.proof.anchored_states,
            &[variable_index(view, "phi1"), variable_index(view, "phi2")],
            "the certificate records both distinct state anchors"
        );
    });

    let prepared = prepare_for_solve(&model).expect("hidden constraint is reducible");
    let (transformed, manifold) = match prepared {
        PreparedDae::Transformed { dae, manifold, .. } => (dae, manifold),
        PreparedDae::Borrowed { .. } => panic!("hidden constraint requires index reduction"),
    };
    assert_eq!(
        manifold.len(),
        2,
        "value and first derivative stay on the manifold"
    );
    for variable in ["phi1", "w1", "phi2", "w2"] {
        assert_eq!(
            role(&transformed, variable),
            dae::VariableRole::State,
            "holonomic reduction must not demote {variable}"
        );
    }
    transformed.inspect(|view| {
        assert!(
            sort(view).is_ok(),
            "second derivative completes the matching"
        );
    });
}

#[test]
fn independent_holonomic_constraints_accumulate_before_the_dae_escapes() {
    let model = independent_holonomic_model();
    let error = model
        .inspect(|view| sort(view).map(|_| ()))
        .expect_err("two position constraints leave two accelerations undefined");
    assert_eq!(unmatched_residue(&error), Some(4));

    let candidates = model.inspect(holonomic_constraints);
    assert_eq!(candidates.len(), 2);
    assert_eq!(
        candidates
            .iter()
            .map(|candidate| candidate.owner_ordinal)
            .collect::<Vec<_>>(),
        vec![2, 10],
        "candidate order is the continuous-owner order"
    );
    for candidate in &candidates {
        let (single, manifold) = rebuild_holonomic_constraint(&model, candidate, &[])
            .expect("each proved constraint reconstructs independently");
        assert_eq!(manifold.len(), 2);
        let error = single
            .inspect(|view| sort(view).map(|_| ()))
            .expect_err("one replacement cannot solve both independent subsystems");
        assert_eq!(
            unmatched_residue(&error),
            Some(2),
            "each single replacement strictly shrinks but does not close the residue"
        );
    }

    let prepared = prepare_for_solve(&model).expect("the proved replacements accumulate");
    let (transformed, manifold) = match prepared {
        PreparedDae::Transformed { dae, manifold, .. } => (dae, manifold),
        PreparedDae::Borrowed { .. } => panic!("the singular source requires index reduction"),
    };
    assert_eq!(manifold.len(), 4);
    for variable in ["phi1", "w1", "phi2", "w2", "phi3", "w3", "phi4", "w4"] {
        assert_eq!(
            role(&transformed, variable),
            dae::VariableRole::State,
            "holonomic accumulation preserves state {variable}"
        );
    }
    transformed.inspect(|view| {
        assert!(sort(view).is_ok(), "only the complete chain may escape");
        let source_equations = manifold
            .iter()
            .map(|expression| {
                let expression = view
                    .expression_id(*expression as usize)
                    .and_then(|id| view.expression(id))
                    .expect("manifold expression resolves in the replacement");
                view.source_text(expression.provenance())
                    .expect("manifold expression keeps its source constraint")
            })
            .collect::<Vec<_>>();
        assert_eq!(
            source_equations,
            [
                "angle1 = 2*angle2",
                "angle1 = 2*angle2",
                "angle3 = 2*angle4",
                "angle3 = 2*angle4",
            ],
            "each original constraint and its first derivative survive exact remapping"
        );
    });
}

#[test]
fn holonomic_owner_order_makes_discovery_order_irrelevant() {
    let model = independent_holonomic_model();
    let normal = reduce_holonomic_constraint(&model)
        .expect("normal discovery reduces both independent constraints")
        .step
        .expect("normal discovery reaches a final DAE");
    let reversed = reduce_holonomic_constraint_with_enumeration(&model, |candidates| {
        candidates.reverse();
    })
    .expect("reversed discovery reduces both independent constraints")
    .step
    .expect("reversed discovery reaches a final DAE");

    assert_eq!(normal.1, reversed.1, "the exact manifold is deterministic");
    assert_eq!(
        serde_json::to_vec(&normal.0).expect("normal replacement serializes"),
        serde_json::to_vec(&reversed.0).expect("reversed replacement serializes"),
        "owner ordinals make the entire finalized replacement byte-identical",
    );
}

#[test]
fn a_certificate_that_does_not_survive_the_current_dae_fails_closed() {
    let model = independent_holonomic_model();
    let mut proof_round = 0;
    let round = reduce_holonomic_constraint_with_enumeration(&model, |candidates| {
        proof_round += 1;
        if proof_round == 2 {
            // Model a certificate that the first replacement invalidated. The
            // hook runs only on freshly recollected current-DAE certificates;
            // clearing this round would be ineffective if the reducer replayed
            // the pristine list instead of proving the candidate again.
            candidates.clear();
        }
    })
    .expect("losing a current certificate is a conservative refusal");

    assert_eq!(
        proof_round, 2,
        "the current DAE is proved again after the first edge"
    );
    assert!(
        round.step.is_none(),
        "a partial chain whose next proof disappeared never escapes the phase",
    );
    assert!(round.blocked.is_none());
}

#[test]
fn contradicting_equalities_prove_nothing_about_their_class() {
    let model = contradictory_equalities_model();
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        for member in ["a", "b"] {
            assert_eq!(
                equalities.anchor_of(variable_index(view, member)),
                None,
                "a class asserting both `a = b` and `a = -b` reports no anchor"
            );
        }
        assert_eq!(
            equalities
                .redundant_states()
                .map(|(state, _, _)| state)
                .collect::<Vec<_>>(),
            Vec::<u32>::new(),
            "and offers no state as redundant"
        );
    });
}

#[test]
fn a_static_one_projection_of_a_singleton_state_payload_is_an_exact_anchor() {
    for subscript in [
        ProjectionSubscript::LiteralOne,
        ProjectionSubscript::BoundParameter,
    ] {
        let model = projected_state_model(1, subscript);
        assert!(
            model.inspect(|view| sort(view).is_err()),
            "the two equal state payloads are singular before demotion"
        );
        model.inspect(|view| {
            let equalities = SystemEqualities::collect(view);
            let x = variable_index(view, "x");
            let q = variable_index(view, "q");
            assert_eq!(
                equalities.anchor_of(x),
                Some((EqualityAnchor::State(q), EqualitySign::Same)),
                "the singleton aggregate wins the construction-capability tie"
            );
            assert_eq!(
                equalities.redundant_states().collect::<Vec<_>>(),
                vec![(x, EqualityAnchor::State(q), EqualitySign::Same)]
            );
        });
        let prepared = prepare_for_solve(&model).expect("singleton projection is reducible");
        let transformed = match prepared {
            PreparedDae::Transformed { dae, .. } => dae,
            PreparedDae::Borrowed { .. } => panic!("singleton projection requires state demotion"),
        };
        assert_eq!(role(&transformed, "x"), dae::VariableRole::Algebraic);
        assert_eq!(role(&transformed, "q"), dae::VariableRole::State);
        transformed
            .inspect(|view| assert!(sort(view).is_ok(), "replacement DAE matches perfectly"));
    }
}

#[test]
fn row_major_vector_pin_demotes_the_complete_state_payload() {
    let model = pinned_vector_state_model();
    let source_error = model
        .inspect(|view| sort(view).map(|_| ()))
        .expect_err("the pinned vector is high-index before demotion");
    assert!(matches!(source_error, StructuralError::Singular { .. }));
    let prepared = prepare_for_solve(&model).expect("the complete vector pin is reducible");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("the pinned vector requires a demotion"),
    };
    assert_eq!(role(&transformed, "q"), dae::VariableRole::Algebraic);
    transformed.inspect(|view| {
        assert!(sort(view).is_ok(), "the rebuilt vector system matches");
        let q = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "q")
            .map(|(_, variable)| variable)
            .expect("the demoted vector survives");
        assert_eq!(q.value_type().dimensions(), [3]);
    });
}

#[test]
fn checked_function_body_proves_vector_state_derivative() {
    let model = function_defined_vector_state_model();
    let source_error = model
        .inspect(|view| sort(view).map(|_| ()))
        .expect_err("the function-defined vector state is high-index before demotion");
    assert!(matches!(source_error, StructuralError::Singular { .. }));
    let prepared = prepare_for_solve(&model).expect("checked call body is differentiable");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("the redundant vector state requires demotion"),
    };
    assert_eq!(role(&transformed, "w"), dae::VariableRole::Algebraic);
    transformed.inspect(|view| {
        assert!(sort(view).is_ok(), "the rebuilt vector system matches");
        let w = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "w")
            .map(|(_, variable)| variable)
            .expect("the demoted vector survives");
        assert_eq!(w.value_type().dimensions(), [3]);
    });
}

#[test]
fn non_singleton_and_dynamic_projections_fail_closed() {
    for model in [
        projected_state_model(2, ProjectionSubscript::LiteralOne),
        projected_state_model(1, ProjectionSubscript::Parameter),
    ] {
        model.inspect(|view| {
            let equalities = SystemEqualities::collect(view);
            assert!(
                equalities.redundant_states().next().is_none(),
                "a partial or dynamically selected payload proves no state redundancy"
            );
        });
    }
}

#[test]
fn projected_algebraic_definition_fails_closed_before_differentiation() {
    let model = projected_algebraic_definition_model();
    let candidate = model.inspect(|view| {
        let x = variable_index(view, "x");
        let candidates = direct_state_constraints(view);
        assert!(
            candidates
                .admissible
                .iter()
                .chain(&candidates.conditional)
                .filter(|candidate| candidate.state == x)
                .all(|candidate| {
                    let rhs = view
                        .expression_id(candidate.rhs as usize)
                        .expect("candidate RHS resolves");
                    !matches!(
                        singleton_real_projection(view, rhs),
                        Some(SingletonRealProjection::Algebraic(_))
                    )
                }),
            "an unsupported projected-algebraic RHS must not reach reconstruction",
        );
        candidates
            .admissible
            .into_iter()
            .chain(candidates.conditional)
            .find(|candidate| candidate.state == x)
            .expect("the equality closure retains the safe bare-state anchor")
    });
    rebuild_with_state_demotion(&model, candidate)
        .expect("the admitted bare-state anchor reconstructs without a panic");
}

#[test]
fn an_opposed_pin_proves_constancy_without_naming_the_pinned_value() {
    let model = opposed_pin_model();
    model.inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        let (anchor, sign) = equalities
            .anchor_of(variable_index(view, "q"))
            .expect("the opposed pin still proves the class constant");
        assert_eq!(sign, EqualitySign::Same, "a pinned class carries no sign");
        assert!(
            matches!(anchor, EqualityAnchor::Invariant { .. }),
            "`q + I = 0` pins the class to a time-invariant value"
        );
        assert_eq!(
            equalities.anchor_expression(anchor),
            None,
            "the class sits at `-I`, which no source expression names"
        );
    });
}
