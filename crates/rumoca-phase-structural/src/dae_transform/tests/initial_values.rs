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

use super::*;

/// What one fixture declaration states about a variable's initial value.
#[derive(Clone, Copy)]
struct StatedInitialValue {
    /// The `start` attribute as written, or `None` when the declaration omits
    /// it and takes the MLS 3.6 §4.8 Real default of zero.
    start: Option<f64>,
    fixed: bool,
}

impl StatedInitialValue {
    /// `Real v(start = value)`: a guess the solver may overwrite.
    const fn guess(value: f64) -> Self {
        Self {
            start: Some(value),
            fixed: false,
        }
    }

    /// `Real v(start = value, fixed = true)`: an initial equation.
    const fn pinned(value: f64) -> Self {
        Self {
            start: Some(value),
            fixed: true,
        }
    }

    /// `Real v(fixed = true)`: the same equation, at the attribute default.
    const fn pinned_at_the_default() -> Self {
        Self {
            start: None,
            fixed: true,
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
                    ..dae::VariableAttributes::default()
                },
                at("Real x"),
            )?;
            variables.define(
                y_reservation,
                dae::VariableAttributes {
                    start: y_start,
                    fixed: Some(y.fixed),
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
        PreparedDae::Borrowed(_) => panic!("an aliased state pair requires a demotion"),
    }
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
            "only the unpinned member is offered for demotion"
        );
        assert_eq!(
            candidates
                .refused
                .iter()
                .map(|refused| refused.variable.clone())
                .collect::<Vec<_>>(),
            vec!["x".to_string()],
            "the bare residual `x - y` claims the pinned state and is refused there, \
             not left to shadow the closure candidate in the per-state dedupe"
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

#[test]
fn two_pinned_members_that_disagree_name_the_initial_condition_they_would_drop() {
    let model = aliased_pair_model(
        StatedInitialValue::pinned(1.0),
        StatedInitialValue::pinned(2.0),
    );
    let error = prepare_for_solve(&model)
        .err()
        .expect("inconsistent stated initial values are not silently resolved");
    let StructuralError::DroppedStatedInitialValue { variable, span } = error else {
        panic!("expected the refused initial condition, got {error:?}");
    };
    assert_eq!(
        variable, "x",
        "the diagnostic names the pinned variable whose initial equation the \
         reduction would have discarded"
    );
    assert!(
        !span.is_dummy(),
        "the refusal carries the declaration that states the initial value"
    );
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
