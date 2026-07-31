//! Connector-shaped singular systems the equality closure has to see through.
//!
//! Each fixture is written in the exact shape MSL produces: a component body
//! states `phi - flange.phi`, a `connect` states `flange_a.phi - flange_b.phi`,
//! and a node balance states `p.i + n.i`. The fixtures stay small enough to
//! reason about by hand while keeping those spellings literal, so a regression
//! in the closure shows up as a system that stops reducing rather than as a
//! wrong number deep inside a real machine model.

use super::*;
use crate::dae_transform::constraints::explicit_derivative_definitions;
use crate::dae_transform::equalities::{EqualityAnchor, EqualitySign, SystemEqualities};

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
        PreparedDae::Borrowed(_) => panic!("alias chain requires a state demotion"),
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
                .map(|(state, _)| state)
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
            matches!(anchor, (EqualityAnchor::Invariant(_), EqualitySign::Same)),
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
        PreparedDae::Borrowed(_) => panic!("pinned flux state requires a demotion"),
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
                Some((EqualityAnchor::Invariant(_), EqualitySign::Same))
            ),
            "a literal pin anchors the class just as a parameter does"
        );
    });
    let prepared = prepare_for_solve(&model).expect("pinned support is reducible");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed(_) => panic!("pinned support requires a demotion"),
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
