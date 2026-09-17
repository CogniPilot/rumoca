//! Static independent coordinates for structurally constrained state manifolds.

mod evaluation;

use std::collections::HashMap;

use rumoca_core::StateSelect;
use rumoca_eval_solve::dense_basis::ColumnChoice;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::{
    FormalDerivativeView, FormalStageCoordinate, FormalStateCoordinate, PreparedDae,
    StructuralError, construct_formal_derivatives, prepare_for_solve,
};

use crate::lower::typed_functions::formal_stages::lower_state_selection_stages;

use evaluation::TrialPoint;

pub(crate) fn prepare<'source>(
    model: &'source dae::Dae,
    overrides: &HashMap<String, f64>,
) -> Result<PreparedDae<'source>, StructuralError> {
    match prepare_for_solve(model) {
        Ok(prepared) => reduce_or_retain(model, prepared, overrides),
        // The ordinary reducer cannot desingularize every constrained system: a
        // buried orientation lock (a quaternion body under a loop joint) leaves
        // an unmatched acceleration residual it reports as structurally
        // singular. The formal-derivative path differentiates and selects an
        // independent basis for exactly those coordinates, so route a singular
        // system through it before surfacing the reducer's failure. Nothing that
        // the reducer already accepts changes: this branch is reached only when
        // it fails.
        Err(error) if matches!(error, StructuralError::Singular { .. }) => {
            recover_singular_via_formal(model, overrides).ok_or(error)
        }
        Err(error) => Err(error),
    }
}

/// Decide the prepared system for a model the ordinary reducer accepted.
///
/// The choice is per-constraint and structural, read off the manifold the
/// structural phase already classified. A holonomic manifold constraint is
/// *definitional* when it is a conserved first integral: its lower-order form is
/// implied by the ODE, so a single differentiation reconstructs a matched state
/// derivative (a unit-quaternion norm whose rate vanishes identically under the
/// norm-preserving kinematics). It is *redundant* when it is a genuine loop
/// closure: over-determining at the position level and closed only by
/// differentiating to acceleration, which introduces a Lagrange multiplier.
///
/// Reduce iff at least one manifold constraint is redundant; retain when every
/// manifold constraint is definitional. Reducing a conserved invariant to a
/// fixed independent basis folds when a selected coordinate passes through zero,
/// whereas retaining its source coordinates and enforcing the invariant through
/// the manifold projection stays regular. A loop closure removes a shared degree
/// of freedom, so retaining it leaves a redundant acceleration residual that
/// costs the solver dearly; it must reduce. A system whose manifold mixes the two
/// across separate blocks reduces as a whole: the candidate construction cannot
/// yet retain one block while reducing another, and retaining a redundant loop
/// closure is the failure this decision exists to avoid, so the presence of any
/// redundant constraint chooses reduce.
fn reduce_or_retain<'source>(
    model: &'source dae::Dae,
    prepared: PreparedDae<'source>,
    overrides: &HashMap<String, f64>,
) -> Result<PreparedDae<'source>, StructuralError> {
    let constrained = prepared.inspect(|system| !system.manifold.is_empty());
    if !constrained {
        return Ok(prepared);
    }
    let formal = construct_formal_derivatives(model)?;
    let dimension = formal.inspect(|formal| formal.formal_dimension());
    let retained = prepared.as_dae().inspect(|view| {
        view.variables()
            .filter(|(_, v)| v.role() == dae::VariableRole::State)
            .map(|(_, v)| v.scalar_count())
            .sum::<usize>()
    });
    if dimension >= retained {
        return Ok(prepared);
    }
    // Construct the reduced candidate first so an infeasible request (an
    // over-constrained `StateSelect.always`, a singular stage Jacobian) still
    // surfaces its exact typed failure rather than being masked by retention.
    let candidate = formal.construct_state_candidate(|formal| select(formal, overrides))?;
    // Retain the source basis when every manifold constraint is a conserved
    // first integral, reduce when any constraint is a redundant loop closure.
    if !prepared.manifold_requires_reduction() {
        return Ok(prepared);
    }
    candidate.into_prepared()
}

/// Attempt the formal-derivative reduction for a system the ordinary reducer
/// left structurally singular. Returns `None` when the formal path does not
/// apply or its selection fails, so the caller reports the reducer's original
/// singularity unchanged.
fn recover_singular_via_formal(
    model: &dae::Dae,
    overrides: &HashMap<String, f64>,
) -> Option<PreparedDae<'static>> {
    let formal = construct_formal_derivatives(model).ok()?;
    formal
        .construct_state_candidate(|formal| select(formal, overrides))
        .ok()?
        .into_prepared()
        .ok()
}

fn select<'formal>(
    formal: FormalDerivativeView<'_, '_, 'formal>,
    overrides: &HashMap<String, f64>,
) -> Result<Vec<FormalStateCoordinate<'formal>>, StructuralError> {
    let required = formal
        .source
        .variables()
        .filter(|(_, variable)| requests_forced_state(*variable))
        .map(|(_, variable)| variable.scalar_count())
        .sum::<usize>();
    if required > formal.formal_dimension() {
        return Err(failure(format!(
            "StateSelect.always requires {required} independent coordinates, but the differential dimension is {}",
            formal.formal_dimension()
        )));
    }
    let programs = lower_state_selection_stages(formal).map_err(failure)?;
    let mut point = TrialPoint::new(formal, overrides)?;
    point.seed_definitions(&programs)?;
    let mut result = Vec::new();
    for stage in programs.stages().iter().filter(|s| s.stage().level() < 0) {
        let coordinates = stage
            .stage()
            .coordinates()
            .flat_map(|coordinate| {
                let count = coordinate.value_variable().scalar_count();
                (0..count).map(move |scalar| (coordinate, scalar))
            })
            .collect::<Vec<_>>();
        let matrix = point.settle(&programs, stage, &coordinates)?;
        let choices = coordinates
            .iter()
            .map(|&(coordinate, scalar)| {
                choice(
                    formal,
                    coordinate,
                    point.has_stated_initial_value(coordinate, scalar),
                )
            })
            .collect::<Vec<_>>();
        let selected = matrix
            .independent_columns(&choices)
            .map_err(|error| match error {
                rumoca_eval_solve::dense_basis::DenseBasisError::Rank => failure(format!(
                    "stage {} has a singular dependent Jacobian under the required state selection",
                    stage.stage().level()
                )),
                error => failure(format!(
                    "stage {} basis selection failed: {error:?}",
                    stage.stage().level()
                )),
            })?;
        for index in selected {
            let (coordinate, scalar) = coordinates[index];
            result.push(formal.state_coordinate(
                coordinate.source(),
                coordinate.order(),
                scalar as u32,
            )?);
        }
    }
    Ok(result)
}

/// A `StateSelect.always` request forces an independent differential state only
/// for a genuine state variable, which owns an independent integration slot. An
/// algebraic or output coordinate carries no such slot: it is structurally
/// determined by the equation system (an alias of another state's derivative, an
/// acceleration-level derivative sensor, or a constraint/function output), so it
/// cannot be an independent state. MLS 3.6 §4.8.8 makes `always` a request, not a
/// guarantee: a coordinate that cannot be a state is demoted rather than failing.
fn requests_forced_state(variable: dae::VariableView<'_>) -> bool {
    variable.state_select() == StateSelect::Always
        && variable.variability() == dae::ExpressionVariability::Continuous
        && variable.role() == dae::VariableRole::State
}

/// Eligibility priority for a demoted `StateSelect.always` algebraic coordinate.
/// It is above every other group (`Prefer` peaks at 6, plus a stated-initial-
/// value bump), so the demoted request is honored as an independent state
/// whenever the stage's constraint structure admits it, and is released to a
/// dependent coordinate only when it cannot be an independent state.
const DEMOTED_ALWAYS_PRIORITY: u8 = 8;

fn choice<'source, 'formal>(
    formal: FormalDerivativeView<'_, 'source, 'formal>,
    coordinate: FormalStageCoordinate<'source, 'formal>,
    stated_initial_value: bool,
) -> ColumnChoice {
    let source = coordinate.source_variable();
    if coordinate.order() == 0 {
        match source.state_select() {
            StateSelect::Always if requests_forced_state(source) => {
                return ColumnChoice::Independent;
            }
            StateSelect::Always => {
                return ColumnChoice::Eligible(
                    DEMOTED_ALWAYS_PRIORITY + u8::from(stated_initial_value),
                );
            }
            StateSelect::Never => return ColumnChoice::Dependent,
            _ => {}
        }
    }
    if formal
        .coordinate(coordinate.source(), coordinate.order() + 1)
        .is_none()
    {
        return ColumnChoice::Dependent;
    }
    let priority = match source.state_select() {
        StateSelect::Prefer => 6,
        StateSelect::Avoid => 0,
        _ if source.role() == dae::VariableRole::State => 4,
        _ => 2,
    };
    ColumnChoice::Eligible(priority + u8::from(stated_initial_value))
}

fn failure(error: impl std::fmt::Display) -> StructuralError {
    StructuralError::UnspannedContractViolation {
        reason: format!("independent state selection: {error}"),
    }
}
