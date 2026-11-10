//! MLS §11.2.3: a positive Integer repeatedly divided by a fixed divisor
//! greater than one reaches zero in a proven finite number of passes.

use super::super::super::guard_facts::{FactScope, GuardFacts};
use super::*;
use rumoca_core::BuiltinFunction;

/// The division proof form: a conjunct `k > 0` (or `0 < k`) whose counter
/// `k` is written only by `k := div(k, d)` with a fixed `d > 1`.
pub(super) fn division_bound(
    block: &StatementBlock,
    entry: &GuardFacts,
    scope: FactScope<'_>,
) -> Option<i64> {
    let mut conjuncts = Vec::new();
    collect_conjuncts(&block.cond, &mut conjuncts);
    let written = statements_written_names(&block.stmts);
    conjuncts.into_iter().find_map(|condition| {
        let Expression::Binary { op, lhs, rhs, .. } = condition else {
            return None;
        };
        let counter = match op {
            OpBinary::Gt if fixed_integer(rhs, &written, scope) == Some(0) => lhs,
            OpBinary::Lt if fixed_integer(lhs, &written, scope) == Some(0) => rhs,
            _ => return None,
        };
        let name = plain_reference(counter)?;
        if !scope.integers.contains(name) {
            return None;
        }
        let divisor = sole_division_update(&block.stmts, &written, name, scope)?;
        // `m` reaches zero after `floor(log_d m) + 1` divisions; zero after none.
        let maximum = entry.integer_interval(counter, scope).upper?;
        Some(
            maximum
                .checked_ilog(divisor)
                .map_or(0, |log| i64::from(log) + 1),
        )
    })
}

/// The fixed divisor `d > 1` of the one top-level `counter := div(counter, d)`,
/// when no other statement writes `counter`.
fn sole_division_update(
    statements: &[rumoca_core::Statement],
    written: &HashSet<VarName>,
    counter: &VarName,
    scope: FactScope<'_>,
) -> Option<i64> {
    let divisor = statements.iter().find_map(|statement| {
        let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
            return None;
        };
        if comp.parts().iter().any(|part| !part.subs.is_empty())
            || rumoca_core::component_ref_to_base_reference(comp).var_name() != counter
        {
            return None;
        }
        let Expression::BuiltinCall {
            function: BuiltinFunction::Div,
            args,
            ..
        } = value
        else {
            return None;
        };
        let [numerator, denominator] = args.as_slice() else {
            return None;
        };
        let divisor = fixed_integer(denominator, written, scope)?;
        (plain_reference(numerator) == Some(counter) && divisor > 1).then_some(divisor)
    })?;
    // One walk over the body, reusing one set: the division must be the
    // counter's only writer.
    let mut writes = HashSet::new();
    let writers = statements
        .iter()
        .filter(|statement| {
            writes.clear();
            collect_written_names(std::slice::from_ref(statement), &mut writes);
            writes_name(&writes, counter)
        })
        .count();
    (writers == 1).then_some(divisor)
}
