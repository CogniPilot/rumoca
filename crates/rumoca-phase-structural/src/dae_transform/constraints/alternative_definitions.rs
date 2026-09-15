//! Source equalities usable for structural substitution without elimination.

use rumoca_ir_dae as dae;

use super::DifferentiationFacts;
use crate::causal_definitions::{direct_definition, exact_row_major_family_body};

pub(super) fn complete(view: dae::DaeView<'_>, facts: &mut DifferentiationFacts) {
    let mut candidates = view
        .continuous_owners()
        .filter_map(|owner| match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => Some(equation.residual()),
            dae::ContinuousOwnerView::Structured { family, .. } => {
                exact_row_major_family_body(view, family)
            }
        })
        .filter_map(|residual| direct_definition(view, residual))
        .filter(|&(target, rhs)| {
            facts.algebraic_definitions[target.index() as usize].is_none()
                && !dae::expr_contains_var(view, rhs, dae::VariableId::from(target))
        })
        .collect::<Vec<_>>();
    loop {
        let mut changed = false;
        candidates.retain(|&(target, rhs)| {
            let index = target.index() as usize;
            if facts.algebraic_definitions[index].is_some() {
                return false;
            }
            // Prove the value before publishing this edge. A cycle through a
            // missing definition therefore cannot provide its own anchor.
            if !facts.can_materialize_value(view, rhs.index()) {
                return true;
            }
            facts.algebraic_definitions[index] = Some(rhs.index());
            changed = true;
            false
        });
        if !changed {
            break;
        }
    }
}
