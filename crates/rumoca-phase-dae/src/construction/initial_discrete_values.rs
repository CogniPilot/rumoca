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
        match coordinate {
            Coordinate::DiscreteReal(id) => {
                let value = promote_initial_integer_to_real(construction, value, provenance)?;
                construction.initialization(|initialization| {
                    initialization
                        .discrete_real_initial_value(id, value, provenance)
                        .map(|_| ())
                })
            }
            Coordinate::DiscreteValue(id) => construction.initialization(|initialization| {
                initialization
                    .discrete_value_initial_value(id, value, provenance)
                    .map(|_| ())
            }),
            _ => Err(dae::DaeConstructionError::InvalidVariableRole {
                name: target.clone(),
                span: definition.span,
            }),
        }?;
    }
    Ok(())
}

/// MLS §10.6.13 promotes an Integer expression in a Real assignment context.
///
/// DAE owners retain exact primitive types, so the phase makes the promotion
/// explicit as the checked mixed-numeric identity `value + 0.0`. The ordinary
/// binary constructor derives the Real result type and preserves the value's
/// variability and dependencies; no owner or caller supplies those facts.
fn promote_initial_integer_to_real<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    value: dae::ExprId<'dae>,
    owner: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let value_type =
        construction.expressions(|expressions| expressions.value_type(value, owner))?;
    if !value_type.is_scalar() || value_type.scalar_type() != dae::ScalarType::Integer {
        return Ok(value);
    }
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::InitializationEquation, owner.span())?;
    let zero = construction.expressions(|expressions| {
        expressions
            .at(generated)
            .literal(dae::DaeLiteral::Real(0.0))
    })?;
    construction.expressions(|expressions| {
        expressions
            .at(generated)
            .binary(dae::BinaryOperator::Add, value, zero)
    })
}
