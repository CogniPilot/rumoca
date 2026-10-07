//! The progress pass bound of a `while` loop (SPEC_0022 ALG-018).
//!
//! `while c loop S end while` with a counter `k` (a scalar Integer local):
//! every path through `S` either leaves a Boolean local `v` that is a
//! top-level conjunct of `c` last assigned the literal `false` (the loop ends
//! after this pass), or ends with `k` raised by at least 1 above its value at
//! the start of the pass, `k0`. The raise is proven symbolically: along the
//! path every Integer local the pass writes is tracked as `a * k0 + o` with a
//! literal `a` and an interval `o` (so `child := 2 * root` then
//! `root := child` raises `root` by `root`), and the raise `(a - 1) * k0 + o`
//! is bounded below with the interval `k0` has where the path first writes
//! `k` (guard facts, the loop condition and completed element accesses such
//! as `rank[root]`, which prove `root >= 1`). Across the passes that do not
//! end the loop `k0` strictly rises while staying within `[L, U]`, the hull
//! of those intervals, so at most `U - L + 1` such passes run; with an exit
//! flag the pass that ends the loop is one more. The facts at the loop head
//! are its invariant (`GuardFacts::while_head`), so `L` covers every pass.

use super::super::super::guard_facts::{FactScope, GuardFacts};
use super::*;
use rumoca_core::IntegerInterval;
use std::collections::BTreeMap;

/// `a * k0 + o`: the value of an Integer local in terms of the counter's
/// value at the start of the pass.
#[derive(Clone, Copy, PartialEq, Debug)]
struct Affine {
    scale: i64,
    offset: IntegerInterval,
}

impl Affine {
    fn constant(offset: IntegerInterval) -> Self {
        Self { scale: 0, offset }
    }

    fn plus(self, other: Self) -> Option<Self> {
        Some(Self {
            scale: self.scale.checked_add(other.scale)?,
            offset: self.offset.plus(other.offset),
        })
    }

    fn scaled(self, factor: i64) -> Option<Self> {
        Some(Self {
            scale: self.scale.checked_mul(factor)?,
            offset: self.offset.times(IntegerInterval::exact(factor)),
        })
    }

    /// A value both forms admit; `None` when the scales differ.
    fn join(self, other: Self) -> Option<Self> {
        (self.scale == other.scale).then(|| Self {
            scale: self.scale,
            offset: self.offset.hull(other.offset),
        })
    }
}

/// What one path did to the counter: nothing yet, a tracked change from the
/// value `k0` holds in `start`, or an untracked write.
#[derive(Clone, Copy, PartialEq, Debug)]
enum Counter {
    Unchanged,
    Moved {
        value: Affine,
        start: IntegerInterval,
    },
    Unknown,
}

impl Counter {
    fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::Unchanged, Self::Unchanged) => Self::Unchanged,
            (
                Self::Moved { value, start },
                Self::Moved {
                    value: other_value,
                    start: other_start,
                },
            ) => value
                .join(other_value)
                .map_or(Self::Unknown, |value| Self::Moved {
                    value,
                    start: start.hull(other_start),
                }),
            _ => Self::Unknown,
        }
    }
}

/// The state of the paths that reach one point of the pass: those that will
/// end the loop and those that continue, each with what they did to the
/// counter, and the tracked values of the other locals.
#[derive(Clone)]
struct Paths {
    facts: GuardFacts,
    exiting: Option<Counter>,
    continuing: Option<Counter>,
    values: BTreeMap<VarName, Affine>,
}

impl Paths {
    fn join(branches: Vec<Self>) -> Option<Self> {
        let mut branches = branches
            .into_iter()
            .filter(|paths| !paths.facts.is_unreachable());
        let mut joined = branches.next()?;
        for branch in branches {
            joined.facts.join_path(&branch.facts);
            joined.exiting = join_counter(joined.exiting, branch.exiting);
            joined.continuing = join_counter(joined.continuing, branch.continuing);
            joined.values = joined
                .values
                .iter()
                .filter_map(|(name, value)| {
                    Some((name.clone(), value.join(*branch.values.get(name)?)?))
                })
                .collect();
        }
        Some(joined)
    }
}

fn join_counter(lhs: Option<Counter>, rhs: Option<Counter>) -> Option<Counter> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => Some(lhs.join(rhs)),
        (counter, None) | (None, counter) => counter,
    }
}

pub(super) fn progress_bound(
    block: &StatementBlock,
    head: &GuardFacts,
    scope: FactScope<'_>,
) -> Option<i64> {
    let mut conjuncts = Vec::new();
    collect_conjuncts(&block.cond, &mut conjuncts);
    let flags = conjuncts
        .iter()
        .filter_map(|conjunct| plain_reference(conjunct))
        .filter(|name| !scope.integers.contains(*name) && !scope.reals.contains(*name));
    let mut pass = head.clone();
    pass.observe_accesses(&block.cond, scope);
    let pass = pass.assuming(&block.cond, true, scope);
    let mut counters = Vec::new();
    collect_written_integers(&block.stmts, scope, &mut counters);
    std::iter::once(None)
        .chain(flags.map(Some))
        .find_map(|flag| {
            counters.iter().find_map(|counter| {
                PassProof {
                    flag,
                    counter,
                    scope,
                }
                .bound(&block.stmts, &pass)
            })
        })
}

/// Every scalar Integer a top-level or conditional assignment of
/// `statements` writes, in source order.
fn collect_written_integers(
    statements: &[rumoca_core::Statement],
    scope: FactScope<'_>,
    counters: &mut Vec<VarName>,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { .. } => {
                if let Some((target, _)) = plain_assignment(statement)
                    && scope.integers.contains(&target)
                    && !counters.contains(&target)
                {
                    counters.push(target);
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_written_integers(&block.stmts, scope, counters);
                }
                if let Some(statements) = else_block {
                    collect_written_integers(statements, scope, counters);
                }
            }
            _ => {}
        }
    }
}

struct PassProof<'a> {
    flag: Option<&'a VarName>,
    counter: &'a VarName,
    scope: FactScope<'a>,
}

impl PassProof<'_> {
    /// The pass bound when every path ends the loop or proves progress.
    fn bound(&self, statements: &[rumoca_core::Statement], pass: &GuardFacts) -> Option<i64> {
        let start = Paths {
            facts: pass.clone(),
            exiting: None,
            continuing: Some(Counter::Unchanged),
            values: BTreeMap::new(),
        };
        let end = self.sequence(statements, start)?;
        let Some(Counter::Moved { value, start }) = end.continuing else {
            // No path continues: the loop ends after its first pass.
            return end.continuing.is_none().then_some(1);
        };
        // The raise `(scale - 1) * k0 + offset` over the interval of `k0`.
        let raise = start
            .times(IntegerInterval::exact(value.scale.checked_sub(1)?))
            .plus(value.offset);
        if raise.lower? < 1 {
            return None;
        }
        let (lower, upper) = start.bounds()?;
        let advancing = upper.checked_sub(lower)?.checked_add(1)?.max(0);
        advancing.checked_add(i64::from(self.flag.is_some()))
    }

    fn sequence(&self, statements: &[rumoca_core::Statement], mut paths: Paths) -> Option<Paths> {
        for statement in statements {
            paths = match statement {
                rumoca_core::Statement::If {
                    cond_blocks,
                    else_block,
                    ..
                } => self.conditional(cond_blocks, else_block.as_deref(), paths)?,
                _ => self.statement(statement, paths),
            };
        }
        Some(paths)
    }

    /// Every branch from the facts its conditions select, and the
    /// fall-through path of an `if` without `else`, joined.
    fn conditional(
        &self,
        cond_blocks: &[StatementBlock],
        else_block: Option<&[rumoca_core::Statement]>,
        paths: Paths,
    ) -> Option<Paths> {
        let conditions = cond_blocks
            .iter()
            .map(|block| &block.cond)
            .collect::<Vec<_>>();
        let entries = paths.facts.branch_entries(&conditions, self.scope);
        let bodies = cond_blocks
            .iter()
            .map(|block| block.stmts.as_slice())
            .chain(std::iter::once(else_block.unwrap_or(&[])));
        let branches = entries
            .into_iter()
            .zip(bodies)
            .map(|(facts, statements)| {
                self.sequence(
                    statements,
                    Paths {
                        facts,
                        ..paths.clone()
                    },
                )
            })
            .collect::<Option<Vec<_>>>()?;
        Some(Paths::join(branches).unwrap_or(Paths {
            facts: GuardFacts::join(&[]),
            ..paths
        }))
    }

    fn statement(&self, statement: &rumoca_core::Statement, mut paths: Paths) -> Paths {
        match plain_assignment(statement) {
            Some((target, value)) if Some(&target) == self.flag => {
                let ends = matches!(
                    value,
                    Expression::Literal {
                        value: Literal::Boolean(false),
                        ..
                    }
                );
                paths = if ends {
                    Paths {
                        exiting: join_counter(paths.exiting, paths.continuing),
                        continuing: None,
                        ..paths
                    }
                } else {
                    Paths {
                        continuing: join_counter(paths.continuing, paths.exiting),
                        exiting: None,
                        ..paths
                    }
                };
            }
            Some((target, value)) if &target == self.counter => {
                let start = paths
                    .facts
                    .integer_interval(&reference(self.counter), self.scope);
                paths.continuing = paths
                    .continuing
                    .map(|counter| self.write_counter(counter, value, start, &paths));
                paths.exiting = paths
                    .exiting
                    .map(|counter| self.write_counter(counter, value, start, &paths));
            }
            Some((target, value)) if self.scope.integers.contains(&target) => {
                let tracked = paths
                    .continuing
                    .and_then(|counter| self.affine(value, counter, &paths));
                match tracked {
                    Some(tracked) => paths.values.insert(target, tracked),
                    None => paths.values.remove(&target),
                };
            }
            _ => {
                let written = statements_written_names(std::slice::from_ref(statement));
                if self.flag.is_some_and(|flag| written.contains(flag)) {
                    paths.continuing = join_counter(paths.continuing, paths.exiting);
                    paths.exiting = None;
                }
                if written.contains(self.counter) {
                    paths.continuing = paths.continuing.map(|_| Counter::Unknown);
                    paths.exiting = paths.exiting.map(|_| Counter::Unknown);
                }
                paths.values.retain(|name, _| !written.contains(name));
            }
        }
        paths.facts.after(statement, self.scope);
        paths
    }

    /// The counter after it is assigned `value`; the first write records
    /// the interval of `k0`, which the counter still holds there.
    fn write_counter(
        &self,
        counter: Counter,
        value: &Expression,
        start: IntegerInterval,
        paths: &Paths,
    ) -> Counter {
        let start = match counter {
            Counter::Unchanged => start,
            Counter::Moved { start, .. } => start,
            Counter::Unknown => return Counter::Unknown,
        };
        self.affine(value, counter, paths)
            .map_or(Counter::Unknown, |value| Counter::Moved { value, start })
    }

    /// `expression` as `a * k0 + o` on this path, with the counter holding
    /// `counter`.
    fn affine(&self, expression: &Expression, counter: Counter, paths: &Paths) -> Option<Affine> {
        match expression {
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() && name.var_name() == self.counter => match counter {
                Counter::Unchanged => Some(Affine {
                    scale: 1,
                    offset: IntegerInterval::exact(0),
                }),
                Counter::Moved { value, .. } => Some(value),
                Counter::Unknown => None,
            },
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() && paths.values.contains_key(name.var_name()) => {
                paths.values.get(name.var_name()).copied()
            }
            Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs,
                ..
            } => self.affine(rhs, counter, paths)?.scaled(-1),
            Expression::Binary { op, lhs, rhs, .. }
                if matches!(op, OpBinary::Add | OpBinary::Sub | OpBinary::Mul) =>
            {
                let lhs = self.affine(lhs, counter, paths)?;
                let rhs = self.affine(rhs, counter, paths)?;
                match op {
                    OpBinary::Add => lhs.plus(rhs),
                    OpBinary::Sub => lhs.plus(rhs.scaled(-1)?),
                    _ => match (lhs.scale, rhs.scale) {
                        (0, _) => rhs.scaled(exact(lhs.offset)?),
                        (_, 0) => lhs.scaled(exact(rhs.offset)?),
                        _ => None,
                    },
                }
            }
            _ if !self.reads_tracked(expression, paths) => Some(Affine::constant(
                paths.facts.integer_interval(expression, self.scope),
            )),
            _ => None,
        }
    }

    /// Whether `expression` reads the counter or a tracked local, whose
    /// current value its interval alone would not relate to `k0`.
    fn reads_tracked(&self, expression: &Expression, paths: &Paths) -> bool {
        let mut reads = Vec::new();
        expression.collect_var_refs(&mut reads);
        reads
            .iter()
            .any(|name| name == self.counter || paths.values.contains_key(name))
    }
}

fn exact(interval: IntegerInterval) -> Option<i64> {
    let (lower, upper) = interval.bounds()?;
    (lower == upper).then_some(lower)
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
