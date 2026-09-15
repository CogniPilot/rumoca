//! Prove cancellation in the exact value route before admitting differentiation.

use std::collections::BTreeMap;

use rumoca_ir_dae as dae;

use super::super::equalities::{EqualityAnchor, EqualitySign, additive_operands, is_zero_literal};
use super::DifferentiationFacts;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ValueAtom {
    Zero,
    State(u32),
    Invariant(u32),
}

pub(super) fn materializes_to_zero<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    residual: dae::ExprId<'dae>,
) -> bool {
    let Some(operands) = additive_operands(view, residual) else {
        return false;
    };
    let mut terms = BTreeMap::new();
    for invariant in operands.invariants {
        let atom = if invariant.zero {
            ValueAtom::Zero
        } else {
            ValueAtom::Invariant(invariant.expression)
        };
        if !add_term(&mut terms, atom, invariant.negated) {
            return false;
        }
    }
    for (variable, negated) in operands.variables {
        let Some((atom, opposite)) = variable_atom(view, facts, variable) else {
            return false;
        };
        if !add_term(&mut terms, atom, negated != opposite) {
            return false;
        }
    }
    terms.values().all(|&coefficient| coefficient == 0)
}

fn add_term(terms: &mut BTreeMap<ValueAtom, i64>, atom: ValueAtom, negated: bool) -> bool {
    if atom == ValueAtom::Zero {
        return true;
    }
    let coefficient = terms.entry(atom).or_default();
    let Some(next) = coefficient.checked_add(if negated { -1 } else { 1 }) else {
        return false;
    };
    *coefficient = next;
    true
}

fn variable_atom(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
    variable: u32,
) -> Option<(ValueAtom, bool)> {
    let id = view.variable_id(variable as usize)?;
    // Materialization retains a state's own value, even when another state is
    // in its equality class. Only algebraic reads follow value anchors.
    if view.variable(id)?.role() == dae::VariableRole::State {
        return Some((ValueAtom::State(variable), false));
    }
    // These owners precede equality substitution in the materialization route.
    if facts.auxiliary_blocks[variable as usize].is_some()
        || facts.component_definitions[variable as usize].is_some()
    {
        return None;
    }
    let (anchor, sign) = facts.equalities.value_anchor_of(variable)?;
    let atom = match anchor {
        EqualityAnchor::State(state) => ValueAtom::State(state),
        EqualityAnchor::Invariant {
            value: Some(value), ..
        } => {
            if is_zero_literal(view, view.expression_id(value as usize)?) {
                ValueAtom::Zero
            } else {
                ValueAtom::Invariant(value)
            }
        }
        EqualityAnchor::Invariant { value: None, .. } => return None,
    };
    Some((atom, sign == EqualitySign::Opposite))
}
