use super::*;

/// MLS 3.7 §4.5.3 concerns source declarations, not the residual operands
/// admitted by Appendix B.1b. Algorithm targets denote their complete storage
/// owner: element assignments retain the other elements through pre(storage).
pub(super) fn validate_declared_discrete_real(flat: &flat::Model) -> Result<(), ToDaeError> {
    let mut when_targets = None;
    for (name, variable) in &flat.variables {
        if !matches!(variable.variability, Variability::Discrete(_))
            || effective_variable_scalar_type(flat, variable) != Some(dae::ScalarType::Real)
            || variable.dims.contains(&0)
        {
            continue;
        }
        if when_targets
            .get_or_insert_with(|| model_algorithms::source_when_targets(flat))
            .contains(name)
        {
            continue;
        }
        return Err(ToDaeError::DiscreteRealWithoutWhen {
            name: name.to_string(),
            span: variable.source_span,
        });
    }
    Ok(())
}
