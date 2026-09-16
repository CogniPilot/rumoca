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
    let prepared = prepare_for_solve(model)?;
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
    formal
        .construct_state_candidate(|formal| select(formal, overrides))?
        .into_prepared()
}

fn select<'formal>(
    formal: FormalDerivativeView<'_, '_, 'formal>,
    overrides: &HashMap<String, f64>,
) -> Result<Vec<FormalStateCoordinate<'formal>>, StructuralError> {
    let required = formal
        .source
        .variables()
        .filter(|(_, variable)| {
            variable.state_select() == StateSelect::Always
                && variable.variability() == dae::ExpressionVariability::Continuous
                && matches!(
                    variable.role(),
                    dae::VariableRole::State
                        | dae::VariableRole::Algebraic
                        | dae::VariableRole::Output
                )
        })
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
            .map(|&(coordinate, _)| choice(formal, coordinate))
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

fn choice<'source, 'formal>(
    formal: FormalDerivativeView<'_, 'source, 'formal>,
    coordinate: FormalStageCoordinate<'source, 'formal>,
) -> ColumnChoice {
    let source = coordinate.source_variable();
    if coordinate.order() == 0 {
        match source.state_select() {
            StateSelect::Always => return ColumnChoice::Independent,
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
        StateSelect::Prefer => 3,
        StateSelect::Avoid => 0,
        _ if source.role() == dae::VariableRole::State => 2,
        _ => 1,
    };
    ColumnChoice::Eligible(priority)
}

fn failure(error: impl std::fmt::Display) -> StructuralError {
    StructuralError::UnspannedContractViolation {
        reason: format!("independent state selection: {error}"),
    }
}
