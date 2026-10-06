//! The exit-or-advance pass bound of a `while` loop (SPEC_0022 ALG-018).
//!
//! `while c loop S end while` where `c` has a top-level conjunct that is a
//! Boolean local `v`, and every path through `S` either leaves `v` last
//! assigned the literal `false`, or raises a counter `k` by a positive literal
//! at a point where the guards of that path prove `k <= U`, and never writes
//! `k` any other way. `k` enters the loop holding the literal `s`. A pass that
//! does not end the loop therefore raises `k` from at least `s` while `k`
//! stays at most `U`, so at most `max(U - s + 1, 0)` passes advance, and the
//! pass after the last of them is the last one: `B = max(U - s + 1, 0) + 1`.
//! A path that ends the loop may write `k` freely: `c` is false afterwards.

use super::super::super::guard_facts::{FactScope, GuardFacts};
use super::*;
use std::collections::BTreeSet;

/// Whether `v`'s last write on a path is the literal `false`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Exit {
    Open,
    False,
}

/// What a path did to the counter.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Advance {
    None,
    Proven,
    Unproven,
}

type Outcomes = BTreeSet<(Exit, Advance)>;

pub(super) fn exit_or_advance_bound(
    block: &StatementBlock,
    known: &EntryValues,
    cx: WhileContext<'_>,
) -> Option<i64> {
    let mut conjuncts = Vec::new();
    collect_conjuncts(&block.cond, &mut conjuncts);
    let mut counters = Vec::new();
    collect_increments(&block.stmts, &mut counters);
    for flag in conjuncts
        .iter()
        .filter_map(|conjunct| plain_reference(conjunct))
    {
        if cx.integers.contains(flag) {
            continue;
        }
        for counter in &counters {
            let Some(&floor) = known.get(counter) else {
                continue;
            };
            let mut proof = PassProof {
                flag,
                counter,
                cx,
                reals: HashSet::new(),
                limit: i64::MIN,
            };
            let start = Outcomes::from([(Exit::Open, Advance::None)]);
            let Some((outcomes, _)) = proof.sequence(&block.stmts, start, GuardFacts::entry())
            else {
                continue;
            };
            if outcomes
                .iter()
                .all(|(exit, advance)| *exit == Exit::False || *advance == Advance::Proven)
            {
                let advancing = proof.limit.checked_sub(floor)?.checked_add(1)?.max(0);
                return advancing.checked_add(1);
            }
        }
    }
    None
}

/// Every name raised by `k := k + d` (a positive literal `d`) in `statements`.
fn collect_increments(statements: &[rumoca_core::Statement], counters: &mut Vec<VarName>) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, .. }
                if comp.parts().iter().all(|part| part.subs.is_empty()) =>
            {
                let target = rumoca_core::component_ref_to_base_reference(comp)
                    .var_name()
                    .clone();
                if is_positive_increment(value, &target) && !counters.contains(&target) {
                    counters.push(target);
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_increments(&block.stmts, counters);
                }
                if let Some(statements) = else_block {
                    collect_increments(statements, counters);
                }
            }
            _ => {}
        }
    }
}

struct PassProof<'a> {
    flag: &'a VarName,
    counter: &'a VarName,
    cx: WhileContext<'a>,
    /// A pass bound reads Integer facts only.
    reals: HashSet<VarName>,
    /// The largest proven value of the counter at any of its increments.
    limit: i64,
}

impl PassProof<'_> {
    fn facts(&self) -> FactScope<'_> {
        FactScope {
            shapes: self.cx.shapes,
            integers: self.cx.integers,
            reals: &self.reals,
        }
    }

    /// The outcomes of every path through `statements` from `outcomes`, and
    /// the guard facts after them; `None` when an increment is unbounded.
    fn sequence(
        &mut self,
        statements: &[rumoca_core::Statement],
        mut outcomes: Outcomes,
        mut facts: GuardFacts,
    ) -> Option<(Outcomes, GuardFacts)> {
        for statement in statements {
            if let rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } = statement
            {
                (outcomes, facts) =
                    self.conditional(cond_blocks, else_block.as_deref(), outcomes, &facts)?;
                continue;
            }
            outcomes = self.statement(statement, &facts, outcomes)?;
            facts.after(statement, self.facts());
        }
        Some((outcomes, facts))
    }

    /// Every branch from the facts its conditions select, and the fall-through
    /// path of an `if` without `else`, joined.
    fn conditional(
        &mut self,
        cond_blocks: &[StatementBlock],
        else_block: Option<&[rumoca_core::Statement]>,
        outcomes: Outcomes,
        facts: &GuardFacts,
    ) -> Option<(Outcomes, GuardFacts)> {
        let conditions = cond_blocks
            .iter()
            .map(|block| &block.cond)
            .collect::<Vec<_>>();
        let mut entries = facts.branch_entries(&conditions, self.facts());
        let fallthrough = entries.pop().expect("an if has a fall-through entry");
        let mut joined = Outcomes::new();
        let mut exits = Vec::with_capacity(entries.len() + 1);
        let branches = cond_blocks.iter().map(|block| block.stmts.as_slice());
        for (statements, entry) in branches.zip(entries) {
            let (found, after) = self.sequence(statements, outcomes.clone(), entry)?;
            joined.extend(found);
            exits.push(after);
        }
        let (found, after) = match else_block {
            Some(statements) => self.sequence(statements, outcomes, fallthrough)?,
            None => (outcomes, fallthrough),
        };
        joined.extend(found);
        exits.push(after);
        Some((joined, GuardFacts::join(&exits)))
    }

    fn statement(
        &mut self,
        statement: &rumoca_core::Statement,
        facts: &GuardFacts,
        outcomes: Outcomes,
    ) -> Option<Outcomes> {
        let (exit, advance) = match plain_assignment(statement) {
            Some((target, value)) => self.assignment_effect(&target, value, facts)?,
            None => {
                let written = statements_written_names(std::slice::from_ref(statement));
                (
                    written.contains(self.flag).then_some(Exit::Open),
                    written.contains(self.counter).then_some(Advance::Unproven),
                )
            }
        };
        Some(
            outcomes
                .into_iter()
                .map(|(old_exit, old_advance)| {
                    (exit.unwrap_or(old_exit), compose(old_advance, advance))
                })
                .collect(),
        )
    }

    /// What one whole scalar assignment does to the flag and the counter.
    fn assignment_effect(
        &mut self,
        target: &VarName,
        value: &Expression,
        facts: &GuardFacts,
    ) -> Option<(Option<Exit>, Option<Advance>)> {
        if target == self.flag {
            let literal_false = matches!(
                value,
                Expression::Literal {
                    value: Literal::Boolean(false),
                    ..
                }
            );
            let exit = if literal_false {
                Exit::False
            } else {
                Exit::Open
            };
            return Some((Some(exit), None));
        }
        if target != self.counter {
            return Some((None, None));
        }
        if !is_positive_increment(value, self.counter) {
            return Some((None, Some(Advance::Unproven)));
        }
        self.limit = self.limit.max(facts.upper_bound(self.counter)?);
        Some((None, Some(Advance::Proven)))
    }
}

/// A whole scalar assignment's target and value.
fn plain_assignment(statement: &rumoca_core::Statement) -> Option<(VarName, &Expression)> {
    let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
        return None;
    };
    comp.parts()
        .iter()
        .all(|part| part.subs.is_empty())
        .then(|| {
            (
                rumoca_core::component_ref_to_base_reference(comp)
                    .var_name()
                    .clone(),
                value,
            )
        })
}

/// A path's counter effect followed by one more statement's.
fn compose(before: Advance, statement: Option<Advance>) -> Advance {
    match (before, statement) {
        (current, None) => current,
        (Advance::Unproven, _) | (_, Some(Advance::Unproven)) => Advance::Unproven,
        _ => Advance::Proven,
    }
}
