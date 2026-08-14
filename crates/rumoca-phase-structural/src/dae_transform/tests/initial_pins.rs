//! MLS 3.6 §8.6 initial equations stated about a coordinate the runtime does
//! not seed.
//!
//! Every fixture is the same regular (non-singular) system: one state `x` with
//! a derivative definition, one algebraic `y` the model pins with
//! `fixed = true`, and one residual asserting how the two relate. Nothing here
//! is reduced — the point is that a stated initial value goes missing without
//! any index reduction at all, because the runtime seeds `x` from its own
//! `start` and computes `y` from it.

use super::*;

/// How the fixture's residual relates the pinned algebraic to the state.
#[derive(Clone, Copy)]
enum AliasForm {
    /// `y - x`: the two are the same quantity.
    Same,
    /// `y + x`: the two are opposites.
    Opposite,
    /// `y - (x - p)`: the two are displaced by a parameter.
    Displaced,
}

const PINNED_ALIAS_TEXT: &str =
    "parameter Real p = 2; Real x; Real y(start = 1, fixed = true); equation y = x; der(x) = -x;";

/// What one fixture states about the state `x`.
#[derive(Clone, Copy)]
struct StateDeclaration {
    /// The `start` attribute as written, or `None` for the Real default.
    start: Option<f64>,
    /// Whether the model pins that start with `fixed = true`.
    pinned: bool,
}

impl StateDeclaration {
    /// `Real x;` or `Real x(start = value)`: a guess the initialization may
    /// overwrite.
    const fn guess(start: Option<f64>) -> Self {
        Self {
            start,
            pinned: false,
        }
    }

    /// `Real x(start = value, fixed = true)`: a second stated initial value.
    const fn pinned(value: f64) -> Self {
        Self {
            start: Some(value),
            pinned: true,
        }
    }
}

/// `y = ±x (+ p); der(x) = -x` with `y(start = 1, fixed = true)`.
fn pinned_alias_model(form: AliasForm, state: StateDeclaration) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("pinned_alias.mo", PINNED_ALIAS_TEXT);
    let at = |needle: &str| source_provenance(source, PINNED_ALIAS_TEXT, needle);
    dae::Dae::construct(sources, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at("Real x"),
            )
        })?;
        let (p_id, x_id, x_reservation, y_id, y_reservation) = model.variables(|variables| {
            let p_id = variables.parameter(
                VarName::new("p"),
                real,
                at("parameter Real p = 2"),
                dae::VariableAttributes::default(),
            )?;
            let (x_id, x_reservation) =
                variables.reserve_state(VarName::new("x"), real, at("Real x"))?;
            let (y_id, y_reservation) = variables.reserve_algebraic(
                VarName::new("y"),
                real,
                at("Real y(start = 1, fixed = true)"),
            )?;
            Ok((p_id, x_id, x_reservation, y_id, y_reservation))
        })?;
        let (x_start, y_start, residuals) = model.expressions(|expressions| {
            let x_start = match state.start {
                Some(value) => Some(
                    expressions
                        .at(at("Real x"))
                        .literal(dae::DaeLiteral::Real(value))?,
                ),
                None => None,
            };
            let y_start = expressions
                .at(at("start = 1"))
                .literal(dae::DaeLiteral::Real(1.0))?;
            let residuals = pinned_alias_residuals(expressions, source, form, p_id, x_id, y_id)?;
            Ok((x_start, Some(y_start), residuals))
        })?;
        model.variables(|variables| {
            variables.define(
                x_reservation,
                dae::VariableAttributes {
                    start: x_start,
                    fixed: state.pinned.then_some(true),
                    ..dae::VariableAttributes::default()
                },
                at("Real x"),
            )?;
            variables.define(
                y_reservation,
                dae::VariableAttributes {
                    start: y_start,
                    fixed: Some(true),
                    ..dae::VariableAttributes::default()
                },
                at("Real y(start = 1, fixed = true)"),
            )
        })?;
        model.continuous(|continuous| {
            for (span, residual) in residuals {
                continuous.value_equation(span, residual)?;
            }
            Ok(())
        })
    })
    .expect("pinned alias fixture DAE is valid")
}

/// The alias residual and the state's derivative definition.
fn pinned_alias_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    source: rumoca_core::SourceId,
    form: AliasForm,
    p_id: dae::ParameterId<'dae>,
    x_id: dae::StateId<'dae>,
    y_id: dae::AlgebraicId<'dae>,
) -> Result<[(dae::DaeProvenance, dae::ExprId<'dae>); 2], dae::DaeConstructionError> {
    let at = |needle: &str| source_provenance(source, PINNED_ALIAS_TEXT, needle);

    let alias_at = at("y = x");
    let y_value = expressions
        .at(alias_at)
        .coordinate(dae::CoordinateInput::Algebraic(y_id))?;
    let x_value = expressions
        .at(alias_at)
        .coordinate(dae::CoordinateInput::State(x_id))?;
    let alias = match form {
        AliasForm::Same => {
            expressions
                .at(alias_at)
                .binary(dae::BinaryOperator::Subtract, y_value, x_value)?
        }
        AliasForm::Opposite => {
            expressions
                .at(alias_at)
                .binary(dae::BinaryOperator::Add, y_value, x_value)?
        }
        AliasForm::Displaced => {
            let p_value = expressions
                .at(alias_at)
                .coordinate(dae::CoordinateInput::Parameter(p_id))?;
            let displaced =
                expressions
                    .at(alias_at)
                    .binary(dae::BinaryOperator::Subtract, x_value, p_value)?;
            expressions
                .at(alias_at)
                .binary(dae::BinaryOperator::Subtract, y_value, displaced)?
        }
    };

    let rate_at = at("der(x) = -x");
    let rate = expressions
        .at(rate_at)
        .coordinate(dae::CoordinateInput::Derivative(x_id))?;
    let x_again = expressions
        .at(rate_at)
        .coordinate(dae::CoordinateInput::State(x_id))?;
    let decay = expressions
        .at(rate_at)
        .unary(dae::UnaryOperator::Negate, x_again)?;
    let rate_residual =
        expressions
            .at(rate_at)
            .binary(dae::BinaryOperator::Subtract, rate, decay)?;

    Ok([(alias_at, alias), (rate_at, rate_residual)])
}

/// The definitions a prepared system carries, with each term named by what it
/// reads. A checked restatement carries no proof of its own value, so it is
/// reported separately by [`pin_checks`].
fn pin_terms(model: &dae::Dae) -> Vec<(String, Vec<String>)> {
    prepared_pins(model, InitialValueRole::Definition)
}

/// The coordinates a prepared system leaves the initialization instant to check.
fn pin_checks(model: &dae::Dae) -> Vec<String> {
    prepared_pins(model, InitialValueRole::Check)
        .into_iter()
        .map(|(coordinate, _)| coordinate)
        .collect()
}

fn prepared_pins(model: &dae::Dae, role: InitialValueRole) -> Vec<(String, Vec<String>)> {
    let prepared = prepare_for_solve(model).expect("regular fixture prepares");
    prepared.inspect(|prepared| {
        prepared
            .pins
            .iter()
            .filter(|pin| pin.role == role)
            .map(|pin| {
                let coordinate = variable_name(prepared.view, pin.coordinate);
                let terms = pin
                    .value
                    .iter()
                    .map(|term| term_text(prepared.view, *term))
                    .collect();
                (coordinate, terms)
            })
            .collect()
    })
}

/// One pin term, as `p` or `-1` — the constant it reads, with its sign.
fn term_text(view: dae::DaeView<'_>, term: PinTerm) -> String {
    let expression = view
        .expression_id(term.expression as usize)
        .and_then(|id| view.expression(id))
        .expect("pin term resolves");
    let text = match expression.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) => format!("{value}"),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
            variable_name(view, parameter.index())
        }
        _ => panic!("a pin term reads something other than a constant"),
    };
    if term.negated {
        format!("-{text}")
    } else {
        text
    }
}

fn variable_name(view: dae::DaeView<'_>, variable: u32) -> String {
    view.variable_id(variable as usize)
        .and_then(|id| view.variable(id))
        .expect("pin coordinate resolves")
        .name()
        .as_str()
        .to_string()
}

#[test]
fn a_pinned_algebraic_defines_the_state_its_alias_names() {
    assert_eq!(
        pin_terms(&pinned_alias_model(
            AliasForm::Same,
            StateDeclaration::guess(Some(9.0))
        )),
        vec![("x".to_string(), vec!["1".to_string()])],
        "`y = x` with `y` pinned at 1 puts `x` at 1, not at its own guess"
    );
}

#[test]
fn an_opposite_signed_alias_negates_the_transferred_value() {
    assert_eq!(
        pin_terms(&pinned_alias_model(
            AliasForm::Opposite,
            StateDeclaration::guess(None)
        )),
        vec![("x".to_string(), vec!["-1".to_string()])],
        "`y = -x` with `y` pinned at 1 puts `x` at -1"
    );
}

#[test]
fn a_displaced_alias_carries_the_offset_into_the_transferred_value() {
    assert_eq!(
        pin_terms(&pinned_alias_model(
            AliasForm::Displaced,
            StateDeclaration::guess(None)
        )),
        vec![("x".to_string(), vec!["1".to_string(), "p".to_string()])],
        "`y = x - p` with `y` pinned at 1 puts `x` at `1 + p`"
    );
}

#[test]
fn a_state_that_pins_itself_needs_no_transfer() {
    // The runtime already seeds `x` from its own `start`, so the agreeing pin on
    // `y` states nothing new and must not add a row that restates it.
    let model = pinned_alias_model(AliasForm::Same, StateDeclaration::pinned(1.0));
    assert!(
        pin_terms(&model).is_empty(),
        "a state pinned to the same value needs no transferred row"
    );
    assert!(
        pin_checks(&model).is_empty(),
        "two starts proved equal need no runtime check either"
    );
}

/// `y = x - p` with `y(start = 1)` and `x(start = 1)`: the two agree exactly
/// when `p = 0`, which is a question about a parameter value.
///
/// Neither answer is available here, so the phase states no opinion: `x` keeps
/// the value the runtime seeds it with, and `y`'s equation stays a residual for
/// the initialization instant to check.
#[test]
fn an_undecidable_restatement_becomes_a_checked_residual() {
    let model = pinned_alias_model(AliasForm::Displaced, StateDeclaration::pinned(1.0));
    assert!(
        pin_terms(&model).is_empty(),
        "the state the model pins itself is not redefined"
    );
    assert_eq!(
        pin_checks(&model),
        vec!["x".to_string()],
        "the second stated value is checked against the seeded state, not dropped \
         and not rejected"
    );
}

#[test]
fn two_members_of_one_class_may_not_state_different_initial_values() {
    let model = pinned_alias_model(AliasForm::Same, StateDeclaration::pinned(2.0));
    let error = prepare_for_solve(&model)
        .err()
        .expect("two contradicting §8.6 equations have no solution");
    assert!(
        matches!(
            error,
            StructuralError::ConflictingStatedInitialValues { .. }
        ),
        "unexpected error: {error}"
    );
    assert_eq!(
        error.code(),
        crate::diagnostic_codes::ES013_CONFLICTING_STATED_INITIAL_VALUES
    );
}
