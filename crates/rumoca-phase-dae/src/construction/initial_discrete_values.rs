use super::*;

/// Lower every MLS §8.6 discrete initial value the replay proved.
///
/// The initialization system owns the value each discrete coordinate holds
/// when initialization finishes; the equation-section owner (an MLS §8.5
/// `when`, a binding, or an algorithm assignment) keeps every later instant.
/// Targets are lowered in a deterministic name order so the DAE arena — and
/// therefore its wire projection — does not depend on hash iteration order.
pub(super) fn lower_initial_discrete_values<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    analysis: &Analysis,
) -> Result<(), dae::DaeConstructionError> {
    let mut targets = analysis.initial_discrete_values.keys().collect::<Vec<_>>();
    targets.sort_by(|left, right| left.as_str().cmp(right.as_str()));
    for target in targets {
        let definition = &analysis.initial_discrete_values[target];
        let value = lower_expression(
            construction,
            coordinates,
            functions,
            &definition.value,
            None,
        )?;
        let provenance = dae::DaeProvenance::source(definition.span)?;
        let coordinate = coordinates
            .get(target)
            .copied()
            .expect("a planned discrete initial target has a lowered coordinate");
        construction.initialization(|initialization| match coordinate {
            Coordinate::DiscreteReal(id) => initialization
                .discrete_real_initial_value(id, value, provenance)
                .map(|_| ()),
            Coordinate::DiscreteValue(id) => initialization
                .discrete_value_initial_value(id, value, provenance)
                .map(|_| ()),
            _ => Err(dae::DaeConstructionError::InvalidVariableRole {
                name: target.clone(),
                span: definition.span,
            }),
        })?;
    }
    Ok(())
}
