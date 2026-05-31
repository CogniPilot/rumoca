//! Initial-equation conversion for ToDAE.

use indexmap::IndexMap;
use rumoca_core::{Expression, Literal, Span, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;

use crate::{ToDaeError, flat_to_dae_expression, remap_flat_for_equations};

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
) -> Result<(), ToDaeError>
where
    F: Fn(&rumoca_core::Expression, &flat::Model, &FxHashMap<String, usize>) -> usize,
{
    let mut flat_to_dae_index: IndexMap<usize, usize> = IndexMap::new();

    for (flat_idx, eq) in flat.initial_equations.iter().enumerate() {
        let dae_index_before = dae.initialization.equations.len();
        let expanded_equations =
            crate::equation_conversion::expand_record_field_equation(eq, flat)?
                .unwrap_or_else(|| vec![eq.clone()]);
        for expanded_eq in &expanded_equations {
            let inferred_scalar_count =
                infer_scalar_count(&expanded_eq.residual, flat, prefix_counts);
            let Some(scalar_count) =
                initial_equation_scalar_count(expanded_eq.scalar_count, inferred_scalar_count)
            else {
                continue;
            };
            let dae_eq = dae::Equation::residual_array(
                flat_to_dae_expression(&expanded_eq.residual),
                expanded_eq.span,
                expanded_eq.origin.to_string(),
                scalar_count,
            );
            dae.initialization.equations.push(dae_eq);
        }
        if dae.initialization.equations.len() == dae_index_before + 1 {
            flat_to_dae_index.insert(flat_idx, dae_index_before);
        }
    }

    dae.initialization.for_equations =
        remap_flat_for_equations(&flat.initial_for_equations, &flat_to_dae_index);
    Ok(())
}

/// Add MLS §8.6 fixed-start initialization equations for continuous unknowns.
///
/// DAE owns the semantic initialization problem. Solver-facing row blocks and
/// worklists are derived later from this partition; they must not rediscover
/// fixed-start constraints independently.
pub(crate) fn add_fixed_start_initial_equations(dae: &mut dae::Dae) {
    let mut equations = Vec::new();
    collect_fixed_start_equations(&dae.variables.states, &mut equations);
    collect_fixed_start_equations(&dae.variables.algebraics, &mut equations);
    collect_fixed_start_equations(&dae.variables.outputs, &mut equations);
    dae.initialization.equations.extend(equations);
}

fn collect_fixed_start_equations(
    variables: &IndexMap<VarName, dae::Variable>,
    equations: &mut Vec<dae::Equation>,
) {
    for (name, var) in variables {
        if var.fixed != Some(true) {
            continue;
        }
        if var.size() == 0 {
            continue;
        }
        equations.push(fixed_start_equation(name, var));
    }
}

fn fixed_start_equation(name: &VarName, var: &dae::Variable) -> dae::Equation {
    let span = var.start_attribute_span().unwrap_or(var.source_span);
    // MLS §8.6: Real start attribute defaults to 0.0 when not explicitly set.
    let rhs = var
        .start
        .clone()
        .unwrap_or_else(|| default_real_start(span));
    dae::Equation::explicit_with_scalar_count(
        name.clone(),
        rhs,
        span,
        format!("fixed start initialization for {}", name.as_str()),
        var.size(),
    )
}

fn default_real_start(span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(0.0),
        span,
    }
}

#[cfg(test)]
mod tests {
    use super::{fixed_start_equation, initial_equation_scalar_count};
    use rumoca_core::{Expression, Literal, Span, VarName};
    use rumoca_ir_dae as dae;

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

    #[test]
    fn fixed_start_equation_uses_default_real_start_when_missing() {
        let mut var = dae::Variable {
            name: VarName::new("x"),
            fixed: Some(true),
            ..Default::default()
        };
        let eq = fixed_start_equation(&VarName::new("x"), &var);

        assert_eq!(eq.lhs.as_ref().map(VarName::as_str), Some("x"));
        assert!(matches!(
            eq.rhs,
            Expression::Literal {
                value: Literal::Real(0.0),
                span: Span::DUMMY
            }
        ));

        var.dims = vec![3];
        let eq = fixed_start_equation(&VarName::new("x"), &var);
        assert_eq!(eq.scalar_count, 3);
    }
}
