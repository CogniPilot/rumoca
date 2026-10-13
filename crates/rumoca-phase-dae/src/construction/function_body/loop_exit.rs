//! Early exit of a fold whose whole body is selected by one run-time condition.
//!
//! Compaction turns a `while` loop into `for w in 1:B loop if c then S end if`
//! and a `for` loop over a run-time range into the static capacity of that
//! range with the range-membership test as a guard. A body that does nothing
//! while its selection is false leaves the carried values unchanged, so a pass
//! whose selection is false ends the fold when every later pass is certain to
//! be false as well (MLS §11.2.2 and §11.2.3).
//!
//! A conjunct of the selection is such an exit test when
//!
//! * it reads no binder of the loop and calls no function: the skipped passes
//!   leave the state it reads unchanged, so it stays false; or
//! * it bounds the loop's leading binder in the direction the binder moves
//!   (`i <= n` while `i` ascends, `i >= n` while it descends) by a value that
//!   reads no binder of the loop: the leading binder is monotone and the bound
//!   is unchanged by skipped passes, so the test stays false.
//!
//! The continuation is the conjunction of the exit tests. The selection itself
//! stays in the body, so the continuation only ends the fold early.

use super::*;
use rumoca_core::{BuiltinFunction, Expression, Literal, OpBinary, OpUnary, Statement};

/// The loop whose exit is being derived.
struct OwnLoop<'a, 'dae> {
    binders: &'a HashMap<VarName, dae::DomainBinderId<'dae>>,
    domain: dae::DomainId<'dae>,
    structured: &'a StructuredIndexDomain,
}

/// The generated Booleans the body's selections read, in definition order,
/// each with its value.
type Captures<'s> = &'s [(&'s VarName, &'s Expression)];

/// End a fold at its first iteration whose body selection is false.
pub(super) fn continue_while_selected<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    (binders, domain, structured, owner): (
        &HashMap<VarName, dae::DomainBinderId<'dae>>,
        dae::DomainId<'dae>,
        &StructuredIndexDomain,
        dae::DaeProvenance,
    ),
    (statements, plans): (&[Statement], &[FunctionStatementPlan]),
) -> Result<(), dae::DaeConstructionError> {
    let own = OwnLoop {
        binders,
        domain,
        structured,
    };
    let Some(condition) = own.exit_condition(statements, plans, owner.span()) else {
        return Ok(());
    };
    let continuation = lower_function_expression_scoped(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        loop_body.body(),
        binders,
        &condition,
    )?;
    let provenance = match condition.span() {
        Some(span) => dae::DaeProvenance::source(span)?,
        None => owner,
    };
    construction
        .functions(|functions| functions.continue_loop_while(loop_body, continuation, provenance))
}

impl OwnLoop<'_, '_> {
    /// The conjunction of the exit tests of the body's selection.
    fn exit_condition(
        &self,
        statements: &[Statement],
        plans: &[FunctionStatementPlan],
        span: Span,
    ) -> Option<Expression> {
        if statements.len() != plans.len() {
            return None;
        }
        let mut captures = Vec::new();
        collect_captures(plans, &mut captures);
        let selected = statements
            .iter()
            .zip(plans)
            .filter_map(|(statement, plan)| {
                (!matches!(
                    plan,
                    FunctionStatementPlan::GeneratedBooleanAssignment { .. }
                ))
                .then_some(statement)
            });
        let conjuncts = common_selection(selected, &captures)?;
        let (exits, others): (Vec<_>, Vec<_>) = conjuncts
            .into_iter()
            .partition(|conjunct| self.is_exit_test(conjunct));
        // A test the exit leaves out can guard a test that faults without it,
        // so beside such a test only the tests that cannot fault are kept: any
        // false conjunct of the selection ends the fold.
        let exits = if others.is_empty() {
            exits
        } else {
            exits
                .into_iter()
                .filter(|test| cannot_fault(test))
                .collect()
        };
        if exits.is_empty() {
            return None;
        }
        let mut exits = exits.into_iter();
        let first = exits.next()?.clone();
        Some(exits.fold(first, |lhs, rhs| Expression::Binary {
            op: OpBinary::And,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs.clone()),
            span,
        }))
    }

    fn is_exit_test(&self, conjunct: &Expression) -> bool {
        if conjunct.contains_subexpression(|node| matches!(node, Expression::FunctionCall { .. })) {
            return false;
        }
        !self.reads_own_binder(conjunct) || self.bounds_leading_binder(conjunct)
    }

    fn reads_own_binder(&self, expression: &Expression) -> bool {
        let mut references = Vec::new();
        expression.collect_var_refs(&mut references);
        references
            .iter()
            .any(|reference| self.is_own_binder(reference))
    }

    fn is_own_binder(&self, name: &VarName) -> bool {
        self.binders
            .get(name)
            .is_some_and(|binder| binder.domain() == self.domain)
    }

    /// The leading binder of the loop and whether it ascends. Iteration is
    /// row-major, so only the first binder is monotone over the whole fold.
    fn leading_binder(&self) -> Option<(&VarName, bool)> {
        let step = self.structured.binders.first()?.step;
        let (name, _) = self
            .binders
            .iter()
            .find(|(_, binder)| binder.domain() == self.domain && binder.ordinal() == 0)?;
        (step != 0).then_some((name, step > 0))
    }

    /// `binder <= bound`, `binder < bound`, `bound >= binder`, or `bound >
    /// binder` for an ascending leading binder (mirrored when it descends),
    /// with a bound that reads no binder of the loop.
    fn bounds_leading_binder(&self, conjunct: &Expression) -> bool {
        let Some((leading, ascending)) = self.leading_binder() else {
            return false;
        };
        let Expression::Binary { op, lhs, rhs, .. } = conjunct else {
            return false;
        };
        let is_leading = |side: &Expression| {
            matches!(
                side,
                Expression::VarRef { name, subscripts, .. }
                    if subscripts.is_empty() && name.var_name() == leading
            )
        };
        let (binder_on_lhs, bound) = if is_leading(lhs) {
            (true, rhs)
        } else if is_leading(rhs) {
            (false, lhs)
        } else {
            return false;
        };
        let upper = match (op, binder_on_lhs) {
            (OpBinary::Le | OpBinary::Lt, true) | (OpBinary::Ge | OpBinary::Gt, false) => true,
            (OpBinary::Ge | OpBinary::Gt, true) | (OpBinary::Le | OpBinary::Lt, false) => false,
            _ => return false,
        };
        upper == ascending && !self.reads_own_binder(bound)
    }
}

/// The conjuncts every effective statement waits on: a statement is skipped when one of
/// them is false.
fn common_selection<'s>(
    statements: impl IntoIterator<Item = &'s Statement>,
    captures: Captures<'s>,
) -> Option<Vec<&'s Expression>> {
    let mut common: Option<Vec<&Expression>> = None;
    for statement in statements {
        // A statement that does nothing waits on nothing and skips nothing.
        if matches!(statement, Statement::Empty { .. }) {
            continue;
        }
        let waits = statement_selection(statement, captures)?;
        common = Some(match common {
            None => waits,
            Some(mut common) => {
                common.retain(|conjunct| waits.contains(conjunct));
                common
            }
        });
    }
    common.filter(|common| !common.is_empty())
}

/// The conjuncts that skip one statement when false: a conditional without an
/// else waits on its condition and on whatever its body waits on, and a nested
/// loop on whatever its body waits on.
fn statement_selection<'s>(
    statement: &'s Statement,
    captures: Captures<'s>,
) -> Option<Vec<&'s Expression>> {
    match statement {
        Statement::If {
            cond_blocks,
            else_block: None,
            ..
        } => {
            let [block] = cond_blocks.as_slice() else {
                return None;
            };
            let mut waits = conjuncts(&block.cond, captures);
            waits.extend(common_selection(&block.stmts, captures).unwrap_or_default());
            Some(waits)
        }
        Statement::For {
            indices, equations, ..
        } => {
            // A conjunct that reads a binder of the nested loop varies over
            // that loop, so it neither skips the whole nested statement nor
            // names a value the enclosing loop binds.
            let nested = indices
                .iter()
                .map(|index| VarName::new(&index.ident))
                .collect::<Vec<_>>();
            let waits = common_selection(equations, captures)?;
            Some(
                waits
                    .into_iter()
                    .filter(|conjunct| !reads_any_name(conjunct, &nested))
                    .collect(),
            )
        }
        _ => None,
    }
}

/// Whether `expression` reads any of `names`.
fn reads_any_name(expression: &Expression, names: &[VarName]) -> bool {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    references.iter().any(|reference| names.contains(reference))
}

/// The `and` operands of `expression`, reading the loop's generated selection
/// through its value.
fn conjuncts<'s>(expression: &'s Expression, captures: Captures<'s>) -> Vec<&'s Expression> {
    match expression {
        Expression::Binary {
            op: OpBinary::And,
            lhs,
            rhs,
            ..
        } => {
            let mut operands = conjuncts(lhs, captures);
            operands.extend(conjuncts(rhs, captures));
            operands
        }
        // `if c then v else false` is `c and v`: a nested selection chains its
        // guards this way.
        Expression::If {
            branches,
            else_branch,
            ..
        } if matches!(
            (branches.as_slice(), else_branch.as_ref()),
            (
                [_],
                Expression::Literal {
                    value: Literal::Boolean(false),
                    ..
                }
            )
        ) =>
        {
            let (condition, value) = &branches[0];
            let mut operands = conjuncts(condition, captures);
            operands.extend(conjuncts(value, captures));
            operands
        }
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            // A guard defined earlier reads only guards defined before it.
            match captures
                .iter()
                .position(|(target, _)| name.var_name() == *target)
            {
                Some(at) => conjuncts(captures[at].1, &captures[..at]),
                None => vec![expression],
            }
        }
        _ => vec![expression],
    }
}

/// Scalar logic, comparison and plain arithmetic over variables and literals:
// no indexing, division or call that could fault; `min`, `max` and `abs` are total.
fn cannot_fault(expression: &Expression) -> bool {
    !expression.contains_subexpression(|node| {
        !matches!(
            node,
            Expression::Literal { .. }
                | Expression::VarRef { .. }
                | Expression::Unary {
                    op: OpUnary::Not | OpUnary::Minus,
                    ..
                }
                | Expression::BuiltinCall {
                    function: BuiltinFunction::Min | BuiltinFunction::Max | BuiltinFunction::Abs,
                    ..
                }
                | Expression::Binary {
                    op: OpBinary::Add
                        | OpBinary::Sub
                        | OpBinary::Mul
                        | OpBinary::Eq
                        | OpBinary::Neq
                        | OpBinary::Lt
                        | OpBinary::Le
                        | OpBinary::Gt
                        | OpBinary::Ge
                        | OpBinary::And
                        | OpBinary::Or,
                    ..
                }
        ) || matches!(node, Expression::VarRef { subscripts, .. } if !subscripts.is_empty())
    })
}

/// The generated Booleans of a statement sequence, nested ones included, in
/// definition order. A nested loop captures its own range membership.
fn collect_captures<'p>(
    plans: &'p [FunctionStatementPlan],
    captures: &mut Vec<(&'p VarName, &'p Expression)>,
) {
    for plan in plans {
        match plan {
            FunctionStatementPlan::GeneratedBooleanAssignment { target, value, .. } => {
                captures.push((target, value));
            }
            FunctionStatementPlan::For { statements, .. }
            | FunctionStatementPlan::ProvenBranch { statements, .. } => {
                collect_captures(statements, captures);
            }
            FunctionStatementPlan::If {
                branches, fallback, ..
            } => {
                for branch in branches.iter().chain(fallback) {
                    collect_captures(branch, captures);
                }
            }
            _ => {}
        }
    }
}
