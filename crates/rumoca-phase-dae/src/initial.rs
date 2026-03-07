//! Initial-equation conversion for ToDAE.

use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;

use crate::flat_to_dae_expression;

/// Determine scalar count for one initial equation.
///
/// - Explicit zero-size equations are skipped.
/// - Inferred zero-size equations are skipped when flatten did not provide
///   an explicit array scalar count.
fn initial_equation_scalar_count(
    explicit_scalar_count: usize,
    inferred_scalar_count: usize,
) -> Option<usize> {
    if explicit_scalar_count == 0 {
        return None;
    }
    if inferred_scalar_count == 0 && explicit_scalar_count <= 1 {
        return None;
    }
    Some(explicit_scalar_count.max(inferred_scalar_count.max(1)))
}

/// Convert initial equations to DAE form.
pub(crate) fn convert_initial_equations<F>(
    dae: &mut dae::Dae,
    flat: &flat::Model,
    prefix_counts: &FxHashMap<String, usize>,
    infer_scalar_count: F,
) where
    F: Fn(&flat::Expression, &flat::Model, &FxHashMap<String, usize>) -> usize,
{
    for eq in &flat.initial_equations {
        let inferred_scalar_count = infer_scalar_count(&eq.residual, flat, prefix_counts);
        let Some(scalar_count) =
            initial_equation_scalar_count(eq.scalar_count, inferred_scalar_count)
        else {
            continue;
        };
        let dae_eq = dae::Equation::residual_array(
            flat_to_dae_expression(&eq.residual),
            eq.span,
            eq.origin.to_string(),
            scalar_count,
        );
        dae.initial_equations.push(dae_eq);
    }
}

#[cfg(test)]
mod tests {
    use super::initial_equation_scalar_count;

    #[test]
    fn test_initial_equation_scalar_count_skips_explicit_zero() {
        assert_eq!(initial_equation_scalar_count(0, 4), None);
    }

    #[test]
    fn test_initial_equation_scalar_count_skips_inferred_zero_without_array_marker() {
        assert_eq!(initial_equation_scalar_count(1, 0), None);
    }

    #[test]
    fn test_initial_equation_scalar_count_prefers_max_nonzero_size() {
        assert_eq!(initial_equation_scalar_count(2, 5), Some(5));
        assert_eq!(initial_equation_scalar_count(5, 2), Some(5));
    }
}
