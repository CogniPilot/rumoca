//! Whole-domain unit-counter induction, separate from acyclic interval inference.
mod body;
mod effects;
#[cfg(test)]
mod tests;

use super::*;
use effects::{Effects, reads_counter};
use rumoca_core::{DefId, Statement};

/// Non-authoritative family recognition: unsupported recurrences retain the
/// existing authoritative compact-domain rejection; no guessed fact is issued.
pub(in crate::construction) fn infer_finite_for_counter_bounds(
    statements: &[Statement],
    shapes: &mut ShapeEnvironment,
    declarations: &[(VarName, DefId)],
    inputs: &[(VarName, DefId)],
) {
    // Derive all facts from the same incoming environment before publishing.
    let proven = declarations
        .iter()
        .filter_map(|(name, id)| try_counter(statements, shapes, name, *id, inputs))
        .collect::<Vec<_>>();
    for proof in proven {
        shapes.bind_integer_bounds(proof.name, proof.lower, proof.upper);
    }
}

/// Private witness: its envelope holds at every legal read in this sequence.
struct CounterInterval {
    name: VarName,
    lower: i64,
    upper: i64,
}

fn try_counter(
    statements: &[Statement],
    shapes: &ShapeEnvironment,
    name: &VarName,
    declaration: DefId,
    inputs: &[(VarName, DefId)],
) -> Option<CounterInterval> {
    let effects = Effects::inspect(statements, name, declaration);
    if effects.counter_writes != 2 || effects.unknown_counter_write || effects.shadowed {
        return None;
    }
    for (ordinal, pair) in statements.windows(2).enumerate() {
        let Some(initial) = initialized_counter(&pair[0], name, declaration) else {
            continue;
        };
        if reads_counter(&statements[..ordinal], declaration) {
            return None;
        }
        let Statement::For {
            indices, equations, ..
        } = &pair[1]
        else {
            continue;
        };
        let [index] = indices.as_slice() else {
            continue;
        };
        let iterations = finite_iterations(&index.range, shapes, &effects, inputs)?;
        if body::try_updates(equations, declaration)? != 1 {
            return None;
        }
        // No other write can precede/follow this pair: the complete source
        // contains exactly this initializer and this one unit-update site.
        return Some(CounterInterval {
            name: name.clone(),
            lower: initial,
            upper: initial.checked_add(iterations)?,
        });
    }
    None
}

fn initialized_counter(statement: &Statement, name: &VarName, id: DefId) -> Option<i64> {
    let Statement::Assignment { comp, value, .. } = statement else {
        return None;
    };
    if !body::whole_counter(comp, id) || &comp.to_var_name() != name {
        return None;
    }
    let Expression::Literal {
        value: Literal::Integer(initial),
        ..
    } = value
    else {
        return None;
    };
    Some(*initial)
}

fn finite_iterations(
    range: &Expression,
    shapes: &ShapeEnvironment,
    effects: &Effects,
    inputs: &[(VarName, DefId)],
) -> Option<i64> {
    let Expression::Range {
        start, step, end, ..
    } = range
    else {
        return None;
    };
    if !effects.has_immutable_operands(range, inputs) {
        return None;
    }
    let exact = |value: &Expression| {
        let (lower, upper) = shapes.proven_integer_bounds(value)?;
        (lower == upper).then_some(lower)
    };
    let start = exact(start)?;
    let end = exact(end)?;
    let step = step.as_deref().map(exact).unwrap_or(Some(1))?;
    if step == 0 {
        return None;
    }
    if (step > 0 && end < start) || (step < 0 && end > start) {
        return Some(0);
    }
    // Wider arithmetic proves cardinality without overflowing endpoint
    // subtraction; the counter's actual i64 arithmetic must still fit below.
    let distance = (i128::from(end) - i128::from(start)).abs();
    let count = distance / i128::from(step).abs() + 1;
    i64::try_from(count).ok()
}
