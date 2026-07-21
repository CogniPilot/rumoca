use std::sync::Arc;

use rumoca_eval_dae::eval::{EvalRuntimeState, eval_expr};
use rumoca_ir_dae as dae;

use super::{
    SolveModelLowerError, build_runtime_parameter_tail_env_with_declared_slots_and_runtime,
    derivative_coefficient_expr, reserve_solve_model_capacity, runtime_tail_error, scalar_names,
    solve_model_contract_violation, solve_model_vec_with_capacity,
};

pub(super) fn order_state_derivative_rows(
    dae_model: &mut dae::Dae,
    state_count: usize,
    params: &[f64],
    runtime: Arc<EvalRuntimeState>,
) -> Result<(), SolveModelLowerError> {
    if state_count == 0 || dae_model.continuous.equations.len() <= state_count {
        return Ok(());
    }
    let Some(first_state) = dae_model.variables.states.values().next() else {
        return Ok(());
    };
    let span = first_state.source_span;
    let mut state_names = solve_model_vec_with_capacity(
        dae_model.variables.states.len(),
        "state derivative name count",
        span,
    )?;
    for (name, var) in &dae_model.variables.states {
        let names = scalar_names(name.as_str(), var)?;
        reserve_solve_model_capacity(
            &mut state_names,
            names.len(),
            "state derivative name count",
            var.source_span,
        )?;
        state_names.extend(names);
    }
    if let Some(ordered_indices) =
        direct_derivative_row_order(dae_model, &state_names, state_count, span)?
    {
        apply_row_order(dae_model, ordered_indices, span)?;
        return Ok(());
    }
    let env = build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        dae_model, params, 0.0, runtime,
    )
    .map_err(|source| runtime_tail_error(dae_model, source))?;
    let equation_count = dae_model.continuous.equations.len();
    let span = dae_model.continuous.equations[0].span;
    let mut used = reserve_state_derivative_order_flags(equation_count, span)?;
    let mut ordered = Vec::new();
    reserve_state_derivative_order_capacity(&mut ordered, equation_count, span)?;
    let mut ordered_indices = Vec::new();
    reserve_state_derivative_order_capacity(&mut ordered_indices, equation_count, span)?;

    for state_name in state_names.iter().take(state_count) {
        let Some((row_idx, _)) = dae_model
            .continuous
            .equations
            .iter()
            .enumerate()
            .filter(|(idx, _)| !used[*idx])
            .filter_map(|(idx, equation)| {
                derivative_coefficient_expr(&equation.rhs, state_name, equation.span)
                    .ok()
                    .and_then(|expr| eval_expr::<f64>(&expr, &env).ok().map(|value| (idx, value)))
            })
            .find(|(_, coeff)| coeff.abs() > 1.0e-15)
        else {
            continue;
        };
        used[row_idx] = true;
        ordered_indices.push(row_idx);
        ordered.push(dae_model.continuous.equations[row_idx].clone());
    }

    if ordered.len() != state_count {
        return Ok(());
    }
    for (idx, equation) in dae_model
        .continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(idx, _)| !used[*idx])
    {
        ordered_indices.push(idx);
        ordered.push(equation.clone());
    }
    remap_structured_families_after_row_order(
        &mut dae_model.continuous.structured_equations,
        &ordered_indices,
        span,
    )?;
    dae_model.continuous.equations = ordered;
    Ok(())
}

fn direct_derivative_row_order(
    dae_model: &dae::Dae,
    state_names: &[String],
    state_count: usize,
    span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, SolveModelLowerError> {
    let analysis =
        crate::lower::analyze_derivative_rhs(dae_model).map_err(SolveModelLowerError::Lower)?;
    let equation_count = dae_model.continuous.equations.len();
    let mut used = reserve_state_derivative_order_flags(equation_count, span)?;
    let mut ordered_indices = Vec::new();
    reserve_state_derivative_order_capacity(&mut ordered_indices, equation_count, span)?;
    for state_name in state_names.iter().take(state_count) {
        let Some(row_idx) = analysis.direct_dae_equation_index_for_state(state_name) else {
            return Ok(None);
        };
        if row_idx >= equation_count || used[row_idx] {
            return Ok(None);
        }
        used[row_idx] = true;
        ordered_indices.push(row_idx);
    }
    for (idx, used) in used.iter().copied().enumerate().take(equation_count) {
        if !used {
            ordered_indices.push(idx);
        }
    }
    Ok(Some(ordered_indices))
}

fn apply_row_order(
    dae_model: &mut dae::Dae,
    ordered_indices: Vec<usize>,
    span: rumoca_core::Span,
) -> Result<(), SolveModelLowerError> {
    let mut ordered = Vec::new();
    reserve_state_derivative_order_capacity(
        &mut ordered,
        dae_model.continuous.equations.len(),
        span,
    )?;
    for idx in &ordered_indices {
        let Some(equation) = dae_model.continuous.equations.get(*idx) else {
            return Ok(());
        };
        ordered.push(equation.clone());
    }
    remap_structured_families_after_row_order(
        &mut dae_model.continuous.structured_equations,
        &ordered_indices,
        span,
    )?;
    dae_model.continuous.equations = ordered;
    Ok(())
}

fn remap_structured_families_after_row_order(
    families: &mut Vec<dae::StructuredEquationFamily>,
    old_indices_in_new_order: &[usize],
    span: rumoca_core::Span,
) -> Result<(), SolveModelLowerError> {
    if families.is_empty() {
        return Ok(());
    }
    let mut old_to_new = Vec::new();
    reserve_state_derivative_order_capacity(&mut old_to_new, old_indices_in_new_order.len(), span)?;
    old_to_new.resize(old_indices_in_new_order.len(), usize::MAX);
    for (new_index, old_index) in old_indices_in_new_order.iter().copied().enumerate() {
        if let Some(slot) = old_to_new.get_mut(old_index) {
            *slot = new_index;
        }
    }
    families.retain_mut(|family| {
        let total = family
            .scalar_view_row_count()
            .expect("validated structured family row count");
        if total == 0 {
            return false;
        }
        let Some(first_new) = old_to_new.get(family.first_equation_index).copied() else {
            return false;
        };
        if first_new == usize::MAX {
            return false;
        }
        for offset in 1..total {
            let Some(new_index) = old_to_new
                .get(family.first_equation_index + offset)
                .copied()
            else {
                return false;
            };
            if new_index != first_new + offset {
                return false;
            }
        }
        family.first_equation_index = first_new;
        true
    });
    Ok(())
}

pub(super) fn reserve_state_derivative_order_flags(
    count: usize,
    span: rumoca_core::Span,
) -> Result<Vec<bool>, SolveModelLowerError> {
    let mut used = Vec::new();
    reserve_state_derivative_order_capacity(&mut used, count, span)?;
    used.resize(count, false);
    Ok(used)
}

pub(super) fn reserve_state_derivative_order_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    span: rumoca_core::Span,
) -> Result<(), SolveModelLowerError> {
    values.try_reserve_exact(capacity).map_err(|_| {
        solve_model_contract_violation(
            "state derivative row ordering capacity overflows".to_string(),
            span,
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("state_derivative_ordering_tests.mo"),
            1,
            2,
        )
    }

    fn family(first_equation_index: usize, count: usize) -> dae::StructuredEquationFamily {
        dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: i64::try_from(count).expect("test count should fit i64"),
                    step: 1,
                }],
            },
            first_equation_index,
            equations_per_point: 1,
            span: test_span(),
            origin: "test".to_string(),
            regular: None,
            template: None,
            interiors_materialized: true,
        }
    }

    #[test]
    fn row_ordering_remaps_intact_structured_family_block() {
        let mut families = vec![family(2, 2), family(6, 2)];

        remap_structured_families_after_row_order(
            &mut families,
            &[4, 5, 0, 1, 2, 3, 6, 7],
            test_span(),
        )
        .expect("structured families should remap");

        assert_eq!(families.len(), 2);
        assert_eq!(families[0].first_equation_index, 4);
        assert_eq!(families[1].first_equation_index, 6);
    }

    #[test]
    fn row_ordering_drops_structured_family_split_by_permutation() {
        let mut families = vec![family(1, 2), family(3, 1)];

        remap_structured_families_after_row_order(&mut families, &[2, 0, 1, 3], test_span())
            .expect("structured families should remap");

        assert_eq!(families.len(), 1);
        assert_eq!(families[0].first_equation_index, 3);
    }
}
