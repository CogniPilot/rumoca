//! Initial conditions MLS 3.6 §8.6 states, across a state demotion.
//!
//! §8.6: "For every Real variable vc with fixed = true, the equation
//! vc = startExpression is added to the initialization equations." A demotion
//! turns a state into an algebraic, which has no initialization equation of its
//! own, so a reduction that picks the pinned member of an alias class silently
//! answers the initial-value question with the other member's guess.
//!
//! Every fixture here is the same three-equation system the defect was found
//! on — two states asserted equal by a bare residual, each with its own
//! derivative definition — and differs only in what the two declarations state
//! about their initial values. The residual `x - y` is claimed by the
//! single-residual detector *and* by the equality closure, so the fixtures
//! exercise both demotion paths at once: whichever one the filter fails to
//! cover shows up as the wrong state surviving.

use rumoca_core::StateSelect;

use super::*;

/// What one fixture declaration states about a variable's initial value.
#[derive(Clone, Copy)]
struct StatedInitialValue {
    /// The `start` attribute as written, or `None` when the declaration omits
    /// it and takes the MLS 3.6 §4.8 Real default of zero.
    start: Option<f64>,
    fixed: bool,
    /// `StateSelect` as declared, which decides whether the reduction is even
    /// allowed to consider demoting this member (MLS 3.6 §4.8.9).
    select: StateSelect,
}

impl StatedInitialValue {
    /// `Real v(start = value)`: a guess the solver may overwrite.
    const fn guess(value: f64) -> Self {
        Self {
            start: Some(value),
            fixed: false,
            select: StateSelect::Default,
        }
    }

    /// `Real v(start = value, fixed = true)`: an initial equation.
    const fn pinned(value: f64) -> Self {
        Self {
            start: Some(value),
            fixed: true,
            select: StateSelect::Default,
        }
    }

    /// `Real v(fixed = true)`: the same equation, at the attribute default.
    const fn pinned_at_the_default() -> Self {
        Self {
            start: None,
            fixed: true,
            select: StateSelect::Default,
        }
    }

    /// The same declaration with `StateSelect.always`, which MLS 3.6 §4.8.9
    /// states "the variable is selected as a state", so no reduction may demote
    /// it and every demotion this system offers falls on the other member.
    const fn always_a_state(self) -> Self {
        Self {
            select: StateSelect::Always,
            ..self
        }
    }
}

const ALIASED_PAIR_TEXT: &str =
    "Real x; Real y; Real f; equation x = y; der(x) = f - x; der(y) = 0 - (f + y);";

/// `x = y; der(x) = f - x; der(y) = -(f + y)`
///
/// Structurally singular as written: two states, one algebraic, and three
/// equations, so exactly one of the two states has to be demoted. Both states
/// carry an explicit derivative definition, so either choice reduces — the
/// reduction is free to pick, and what it picks decides whose stated initial
/// value survives.
fn aliased_pair_model(x: StatedInitialValue, y: StatedInitialValue) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("stated_initial_values.mo", ALIASED_PAIR_TEXT);
    let at = |needle: &str| source_provenance(source, ALIASED_PAIR_TEXT, needle);
    dae::Dae::construct(sources, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at("Real x"),
            )
        })?;
        let (x_id, x_reservation, y_id, y_reservation, f_id) = model.variables(|variables| {
            let (x_id, x_reservation) =
                variables.reserve_state(VarName::new("x"), real, at("Real x"))?;
            let (y_id, y_reservation) =
                variables.reserve_state(VarName::new("y"), real, at("Real y"))?;
            let f_id = variables.algebraic(
                VarName::new("f"),
                real,
                at("Real f"),
                dae::VariableAttributes::default(),
            )?;
            Ok((x_id, x_reservation, y_id, y_reservation, f_id))
        })?;
        let (x_start, y_start, residuals) = model.expressions(|expressions| {
            let x_start = stated_start(expressions, at("Real x"), x)?;
            let y_start = stated_start(expressions, at("Real y"), y)?;
            let residuals = aliased_pair_residuals(expressions, source, x_id, y_id, f_id)?;
            Ok((x_start, y_start, residuals))
        })?;
        model.variables(|variables| {
            variables.define(
                x_reservation,
                dae::VariableAttributes {
                    start: x_start,
                    fixed: Some(x.fixed),
                    state_select: x.select,
                    ..dae::VariableAttributes::default()
                },
                at("Real x"),
            )?;
            variables.define(
                y_reservation,
                dae::VariableAttributes {
                    start: y_start,
                    fixed: Some(y.fixed),
                    state_select: y.select,
                    ..dae::VariableAttributes::default()
                },
                at("Real y"),
            )
        })?;
        model.continuous(|continuous| {
            for (span, residual) in residuals {
                continuous.value_equation(span, residual)?;
            }
            Ok(())
        })
    })
    .expect("stated initial value fixture DAE is valid")
}

/// The `start` attribute expression one declaration states, if it states one.
fn stated_start<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    declaration: dae::DaeProvenance,
    stated: StatedInitialValue,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    match stated.start {
        Some(value) => expressions
            .at(declaration)
            .literal(dae::DaeLiteral::Real(value))
            .map(Some),
        None => Ok(None),
    }
}

/// The three residuals of the fixture, in source order.
fn aliased_pair_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    source: rumoca_core::SourceId,
    x_id: dae::StateId<'dae>,
    y_id: dae::StateId<'dae>,
    f_id: dae::AlgebraicId<'dae>,
) -> Result<[(dae::DaeProvenance, dae::ExprId<'dae>); 3], dae::DaeConstructionError> {
    let at = |needle: &str| source_provenance(source, ALIASED_PAIR_TEXT, needle);

    let alias_at = at("x = y");
    let x_value = expressions
        .at(alias_at)
        .coordinate(dae::CoordinateInput::State(x_id))?;
    let y_value = expressions
        .at(alias_at)
        .coordinate(dae::CoordinateInput::State(y_id))?;
    let alias = expressions
        .at(alias_at)
        .binary(dae::BinaryOperator::Subtract, x_value, y_value)?;

    let x_rate_at = at("der(x) = f - x");
    let x_rate = expressions
        .at(x_rate_at)
        .coordinate(dae::CoordinateInput::Derivative(x_id))?;
    let f_in_x = expressions
        .at(x_rate_at)
        .coordinate(dae::CoordinateInput::Algebraic(f_id))?;
    let x_again = expressions
        .at(x_rate_at)
        .coordinate(dae::CoordinateInput::State(x_id))?;
    let x_definition =
        expressions
            .at(x_rate_at)
            .binary(dae::BinaryOperator::Subtract, f_in_x, x_again)?;
    let x_residual =
        expressions
            .at(x_rate_at)
            .binary(dae::BinaryOperator::Subtract, x_rate, x_definition)?;

    let y_rate_at = at("der(y) = 0 - (f + y)");
    let y_rate = expressions
        .at(y_rate_at)
        .coordinate(dae::CoordinateInput::Derivative(y_id))?;
    let f_in_y = expressions
        .at(y_rate_at)
        .coordinate(dae::CoordinateInput::Algebraic(f_id))?;
    let y_again = expressions
        .at(y_rate_at)
        .coordinate(dae::CoordinateInput::State(y_id))?;
    let load = expressions
        .at(y_rate_at)
        .binary(dae::BinaryOperator::Add, f_in_y, y_again)?;
    let zero = expressions
        .at(y_rate_at)
        .literal(dae::DaeLiteral::Real(0.0))?;
    let y_definition =
        expressions
            .at(y_rate_at)
            .binary(dae::BinaryOperator::Subtract, zero, load)?;
    let y_residual =
        expressions
            .at(y_rate_at)
            .binary(dae::BinaryOperator::Subtract, y_rate, y_definition)?;

    Ok([
        (alias_at, alias),
        (x_rate_at, x_residual),
        (y_rate_at, y_residual),
    ])
}

/// What the fixture below asserts the pinned state equals for all time.
#[derive(Clone, Copy)]
enum AssertedValue {
    /// `x = value`: the class is pinned to a time-invariant value, so the
    /// asserted equation states `x` at the initialization instant too.
    Invariant(f64),
    /// `x = c` for a parameter `c`: the class is pinned to a time-invariant
    /// value the phase cannot evaluate, so whether it is the stated one depends
    /// on a number only the initialization instant has.
    Parameter,
    /// `x = time`: an equation that determines `x` at every instant but states
    /// no value this phase can compare a `start` against.
    Time,
}

const ASSERTED_VALUE_TEXT: &str = "parameter Real c; Real x; Real f; equation x = a; der(x) = f;";

/// `x = a; der(x) = f`
///
/// Structurally singular as written: `x - a` names no unknown of a system whose
/// state is `x`, so exactly one demotion reduces it — the one that turns the
/// pinned `x` into an algebraic and replaces `der(x)` by `d/dt a`. Whether that
/// demotion keeps what the model states about `x(t0)` is then a question about
/// `a` alone, which is what makes this the fixture for the anchor kinds
/// [`super::super::constraints::keeps_stated_initial_value`] cannot settle.
fn asserted_value_model(x: StatedInitialValue, asserted: AssertedValue) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("asserted_value.mo", ASSERTED_VALUE_TEXT);
    let at = |needle: &str| source_provenance(source, ASSERTED_VALUE_TEXT, needle);
    dae::Dae::construct(sources, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at("Real x"),
            )
        })?;
        let (c_id, x_id, x_reservation, f_id) = model.variables(|variables| {
            let c_id = variables.parameter(
                VarName::new("c"),
                real,
                at("parameter Real c"),
                dae::VariableAttributes::default(),
            )?;
            let (x_id, x_reservation) =
                variables.reserve_state(VarName::new("x"), real, at("Real x"))?;
            let f_id = variables.algebraic(
                VarName::new("f"),
                real,
                at("Real f"),
                dae::VariableAttributes::default(),
            )?;
            Ok((c_id, x_id, x_reservation, f_id))
        })?;
        let (x_start, residuals) = model.expressions(|expressions| {
            let x_start = stated_start(expressions, at("Real x"), x)?;
            let assertion_at = at("x = a");
            let x_value = expressions
                .at(assertion_at)
                .coordinate(dae::CoordinateInput::State(x_id))?;
            let anchor = match asserted {
                AssertedValue::Invariant(value) => expressions
                    .at(assertion_at)
                    .literal(dae::DaeLiteral::Real(value))?,
                AssertedValue::Parameter => expressions
                    .at(assertion_at)
                    .coordinate(dae::CoordinateInput::Parameter(c_id))?,
                AssertedValue::Time => expressions
                    .at(assertion_at)
                    .coordinate(dae::CoordinateInput::Time)?,
            };
            let assertion = expressions.at(assertion_at).binary(
                dae::BinaryOperator::Subtract,
                x_value,
                anchor,
            )?;
            let rate_at = at("der(x) = f");
            let x_rate = expressions
                .at(rate_at)
                .coordinate(dae::CoordinateInput::Derivative(x_id))?;
            let f_value = expressions
                .at(rate_at)
                .coordinate(dae::CoordinateInput::Algebraic(f_id))?;
            let rate =
                expressions
                    .at(rate_at)
                    .binary(dae::BinaryOperator::Subtract, x_rate, f_value)?;
            Ok((x_start, [(assertion_at, assertion), (rate_at, rate)]))
        })?;
        model.variables(|variables| {
            variables.define(
                x_reservation,
                dae::VariableAttributes {
                    start: x_start,
                    fixed: Some(x.fixed),
                    state_select: x.select,
                    ..dae::VariableAttributes::default()
                },
                at("Real x"),
            )
        })?;
        model.continuous(|continuous| {
            for (span, residual) in residuals {
                continuous.value_equation(span, residual)?;
            }
            Ok(())
        })
    })
    .expect("asserted value fixture DAE is valid")
}

fn role(dae: &dae::Dae, name: &str) -> dae::VariableRole {
    dae.inspect(|view| {
        view.variables()
            .find(|(_, variable)| variable.name().as_str() == name)
            .map(|(_, variable)| variable.role())
            .unwrap_or_else(|| panic!("reconstructed DAE keeps `{name}`"))
    })
}

/// The `start` value a reconstructed DAE still states about `name`, together
/// with its `fixed` flag — the two halves of the MLS 3.6 §8.6 initial equation.
fn stated_initial_value(dae: &dae::Dae, name: &str) -> (Option<f64>, Option<bool>) {
    dae.inspect(|view| {
        let variable = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == name)
            .map(|(_, variable)| variable)
            .unwrap_or_else(|| panic!("reconstructed DAE keeps `{name}`"));
        let start = variable.start().and_then(|start| {
            match view.expression(start).map(|start| start.operation()) {
                Some(dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value))) => {
                    Some(*value)
                }
                _ => None,
            }
        });
        (start, variable.fixed())
    })
}

fn transformed(prepared: PreparedDae<'_>) -> Box<dae::Dae> {
    match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("an aliased state pair requires a demotion"),
    }
}

/// Every MLS 3.6 §8.6 initial equation the prepared system carries: the
/// declaration it was read from, the coordinate it was written onto, and how
/// the runtime has to enforce it.
fn carried_initial_values(prepared: &PreparedDae<'_>) -> Vec<(String, String, InitialValueRole)> {
    prepared.inspect(|system| {
        let name = |variable: u32| {
            system
                .view
                .variable_id(variable as usize)
                .and_then(|id| system.view.variable(id))
                .map(|variable| variable.name().as_str().to_string())
                .expect("a carried initial value names a variable of the prepared system")
        };
        system
            .pins
            .iter()
            .map(|pin| (name(pin.source), name(pin.coordinate), pin.role))
            .collect()
    })
}

#[test]
fn a_pinned_start_decides_which_aliased_state_is_demoted() {
    let model = aliased_pair_model(
        StatedInitialValue::pinned(1.0),
        StatedInitialValue::guess(2.0),
    );
    assert!(
        model.inspect(|view| sort(view).is_err()),
        "two states asserted equal are structurally singular"
    );
    let reduced = transformed(prepare_for_solve(&model).expect("an aliased pair is reducible"));
    assert_eq!(
        role(&reduced, "x"),
        dae::VariableRole::State,
        "the state the model pins with `fixed = true` is the one that survives"
    );
    assert_eq!(
        role(&reduced, "y"),
        dae::VariableRole::Algebraic,
        "the unpinned member of the class is the one demoted"
    );
    assert_eq!(
        stated_initial_value(&reduced, "x"),
        (Some(1.0), Some(true)),
        "the MLS 3.6 section 8.6 initial equation `x = 1.0` survives the reduction"
    );
    reduced.inspect(|view| assert!(sort(view).is_ok(), "replacement DAE matches perfectly"));
}

#[test]
fn the_residual_path_cannot_shadow_the_guarded_closure_candidate() {
    let model = aliased_pair_model(
        StatedInitialValue::pinned(1.0),
        StatedInitialValue::guess(2.0),
    );
    model.inspect(|view| {
        let candidates = crate::dae_transform::constraints::direct_state_constraints(view);
        let demoted = |state: u32| {
            view.variable(
                view.variable_id(state as usize)
                    .expect("candidate state identity resolves"),
            )
            .expect("candidate state declaration resolves")
            .name()
            .as_str()
            .to_string()
        };
        assert_eq!(
            candidates
                .admissible
                .iter()
                .map(|candidate| demoted(candidate.state))
                .collect::<Vec<_>>(),
            vec!["y".to_string()],
            "only the unpinned member is offered unconditionally"
        );
        assert_eq!(
            candidates
                .conditional
                .iter()
                .map(|candidate| demoted(candidate.state))
                .collect::<Vec<_>>(),
            vec!["x".to_string()],
            "the bare residual `x - y` claims the pinned state, and that claim decides \
             what demoting `x` costs rather than being dropped in the per-state dedupe"
        );
    });
}

#[test]
fn two_pinned_members_that_agree_on_the_start_still_reduce() {
    let model = aliased_pair_model(
        StatedInitialValue::pinned(1.0),
        StatedInitialValue::pinned(1.0),
    );
    let reduced = transformed(
        prepare_for_solve(&model).expect("two consistent initial conditions are reducible"),
    );
    assert_eq!(
        role(&reduced, "x"),
        dae::VariableRole::State,
        "the surviving anchor carries the obligation the demoted member states"
    );
    assert_eq!(role(&reduced, "y"), dae::VariableRole::Algebraic);
    assert_eq!(stated_initial_value(&reduced, "x"), (Some(1.0), Some(true)));
}

#[test]
fn an_omitted_start_states_the_same_initial_value_as_an_explicit_zero() {
    let model = aliased_pair_model(
        StatedInitialValue::pinned_at_the_default(),
        StatedInitialValue::pinned(0.0),
    );
    let reduced = transformed(
        prepare_for_solve(&model).expect("the Real start default is the value it defaults to"),
    );
    assert_eq!(role(&reduced, "y"), dae::VariableRole::Algebraic);
    assert_eq!(stated_initial_value(&reduced, "x"), (None, Some(true)));
}

/// MLS 3.6 §8.6 adds `x = 1` and `y = 2` to the initialization equations, and
/// the system asserts `x = y` for all time, so the initialization problem it
/// describes has no solution. The demotion itself is legal — every stated value
/// is still stated about a coordinate the runtime answers afterwards — so what
/// this model needs reported is the contradiction, not an index-reduction
/// message that would send a modeller looking for a missing equation.
#[test]
fn two_pinned_members_that_disagree_report_the_inconsistent_initialization() {
    let model = aliased_pair_model(
        StatedInitialValue::pinned(1.0),
        StatedInitialValue::pinned(2.0),
    );
    let error = prepare_for_solve(&model)
        .err()
        .expect("inconsistent stated initial values are not silently resolved");
    let StructuralError::ConflictingStatedInitialValues {
        variable,
        other,
        span,
        other_span,
    } = error
    else {
        panic!("expected the reported contradiction, got {error:?}");
    };
    let mut named = [variable.as_str(), other.as_str()];
    named.sort_unstable();
    assert_eq!(
        named,
        ["x", "y"],
        "the diagnostic names both declarations that state a value, each once"
    );
    assert!(
        !span.is_dummy() && !other_span.is_dummy(),
        "the refusal carries both declarations that state an initial value"
    );
}

/// The demotion the MSL `StateSelect` shapes force: the model insists the
/// unpinned member is a state, so the only reduction left demotes the pinned
/// one. MLS 3.6 §8.6 states `x = 1` about the *quantity* `x` names, and the
/// system asserts `x = y` for all time, so `y = 1` states the same equation —
/// the value transfers to the coordinate the runtime seeds instead of being
/// discarded with the coordinate it was declared on.
#[test]
fn a_pinned_state_is_demoted_when_the_class_carries_its_value_to_the_survivor() {
    let model = aliased_pair_model(
        StatedInitialValue::pinned(1.0),
        StatedInitialValue::guess(2.0).always_a_state(),
    );
    let prepared = prepare_for_solve(&model)
        .expect("a stated value the class carries is not a reason to stop");
    assert_eq!(
        carried_initial_values(&prepared),
        vec![(
            "x".to_string(),
            "y".to_string(),
            InitialValueRole::Definition
        )],
        "the stated value is carried onto the state that survives the demotion"
    );
    let reduced = transformed(prepared);
    assert_eq!(
        role(&reduced, "x"),
        dae::VariableRole::Algebraic,
        "the pinned member is the one demoted, because it is the only one that may be"
    );
    assert_eq!(role(&reduced, "y"), dae::VariableRole::State);
    reduced.inspect(|view| assert!(sort(view).is_ok(), "replacement DAE matches perfectly"));
}

/// The same shape with nothing left to carry the value to: `x = time` determines
/// `x` at every instant but states no value a `start` can be compared against,
/// so demoting `x` really does discard the MLS 3.6 §8.6 equation `x = 1`.
#[test]
fn a_pinned_state_no_surviving_equation_states_is_still_refused() {
    let model = asserted_value_model(StatedInitialValue::pinned(1.0), AssertedValue::Time);
    assert!(
        model.inspect(|view| sort(view).is_err()),
        "an asserted value that names no unknown is structurally singular"
    );
    let error = prepare_for_solve(&model)
        .err()
        .expect("a stated initial value nothing reproduces is not silently dropped");
    let StructuralError::DroppedStatedInitialValue { variable, span } = error else {
        panic!("expected the refused initial condition, got {error:?}");
    };
    assert_eq!(variable, "x");
    assert!(
        !span.is_dummy(),
        "the refusal carries the declaration that states the initial value"
    );
}

/// A class the system pins to a time-invariant value states that value at the
/// initialization instant too, so a demotion keeps the MLS 3.6 §8.6 equation
/// exactly when the asserted value *is* the stated one. Here it is.
#[test]
fn an_invariant_class_that_asserts_the_stated_value_admits_the_demotion() {
    let model = asserted_value_model(
        StatedInitialValue::pinned(1.0),
        AssertedValue::Invariant(1.0),
    );
    let reduced = transformed(
        prepare_for_solve(&model).expect("the asserted value is the value the model states"),
    );
    assert_eq!(
        role(&reduced, "x"),
        dae::VariableRole::Algebraic,
        "the residual that asserts the value determines the demoted coordinate"
    );
    reduced.inspect(|view| assert!(sort(view).is_ok(), "replacement DAE matches perfectly"));
}

/// The same class pinned to a different value. The demotion leaves `x = 5` in
/// the system and nothing else that mentions `x`, so accepting it would answer
/// the stated `x = 1` with 5 — which is what an unconditional `Invariant` arm
/// did before this refusal was proved rather than assumed.
#[test]
fn an_invariant_class_that_asserts_another_value_refuses_the_demotion() {
    let model = asserted_value_model(
        StatedInitialValue::pinned(1.0),
        AssertedValue::Invariant(5.0),
    );
    let error = prepare_for_solve(&model)
        .err()
        .expect("an asserted value that is not the stated one may not replace it");
    let StructuralError::DroppedStatedInitialValue { variable, .. } = error else {
        panic!("expected the refused initial condition, got {error:?}");
    };
    assert_eq!(variable, "x");
}

/// The same class pinned to a *parameter*. Whether `c` is the stated 1 is a
/// question about a number this phase never has, and MLS 3.6 §8.6 states the
/// balanced-initialization rule as a "should", so an undecided difference is
/// left to the initialization instant rather than refused here.
///
/// `Rotational.Components.Fixed` states exactly this shape — `flange.phi = phi0`
/// with `phi0` a parameter — so refusing it refuses every rigidly held
/// mechanism in the MSL.
#[test]
fn a_parameter_valued_asserted_value_is_left_to_the_initialization_instant() {
    let model = asserted_value_model(StatedInitialValue::pinned(1.0), AssertedValue::Parameter);
    let reduced = transformed(
        prepare_for_solve(&model)
            .expect("a difference that still reads a parameter decides nothing here"),
    );
    assert_eq!(
        role(&reduced, "x"),
        dae::VariableRole::Algebraic,
        "the residual that asserts the value determines the demoted coordinate"
    );
    reduced.inspect(|view| assert!(sort(view).is_ok(), "replacement DAE matches perfectly"));
}

/// The control for the two above: with nothing stated, the same reduction runs
/// without consulting any initial value at all.
#[test]
fn an_unpinned_asserted_value_reduces_without_consulting_any_start() {
    let model = asserted_value_model(
        StatedInitialValue::guess(1.0),
        AssertedValue::Invariant(5.0),
    );
    let reduced =
        transformed(prepare_for_solve(&model).expect("a guess is not an initial condition"));
    assert_eq!(role(&reduced, "x"), dae::VariableRole::Algebraic);
}

#[test]
fn an_unpinned_alias_pair_reduces_without_consulting_any_start() {
    let model = aliased_pair_model(
        StatedInitialValue::guess(1.0),
        StatedInitialValue::guess(2.0),
    );
    let reduced =
        transformed(prepare_for_solve(&model).expect("a guess is not an initial condition"));
    // The control for the two tests above: with nothing *stated*, the residual
    // `x - y` is read as a definition of its left-hand state and `x` is the one
    // demoted. Both fixtures above have that same residual, so the surviving
    // state there is the guard's doing and not an incidental ordering.
    assert_eq!(
        role(&reduced, "x"),
        dae::VariableRole::Algebraic,
        "an unpinned pair keeps the reduction's own residual-order preference"
    );
    assert_eq!(role(&reduced, "y"), dae::VariableRole::State);
}
