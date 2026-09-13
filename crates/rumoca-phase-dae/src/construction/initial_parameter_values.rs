//! Construct the initialization owner selected from non-Real parameter equations.

use super::*;

pub(super) fn lower<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    analysis: &Analysis,
) -> Result<(), dae::DaeConstructionError> {
    for definition in &analysis.initial_parameter_equations {
        let value = lower_expression(
            construction,
            coordinates,
            functions,
            &definition.value,
            None,
        )?;
        let provenance = dae::DaeProvenance::source(definition.span)?;
        let Some(Coordinate::Parameter(target)) = coordinates.get(&definition.target).copied()
        else {
            return Err(dae::DaeConstructionError::InvalidVariableRole {
                name: definition.target.clone(),
                span: definition.span,
            });
        };
        construction.initialization(|initialization| {
            initialization
                .parameter_initial_value(target, value, provenance)
                .map(|_| ())
        })?;
    }
    Ok(())
}
