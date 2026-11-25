//! Values a loop defines together with its exit flag.
//!
//! A search loop often runs `while not found loop ... end while` and writes
//! its result only in the pass that sets `found := true`, as MSL's
//! `Modelica.Math.Nonlinear.solveOneNonlinearEquation` does. No path through
//! one pass defines the result, yet it is defined wherever the flag is true:
//! the flag is false before the loop, and inside it every statement that can
//! make the flag true is `found := true` beside a whole assignment of the
//! result in the same statement sequence, which runs to its end or ends the
//! evaluation. So "the result is undefined" implies "the flag is false" before
//! the first pass and after every pass. That implication is recorded as the
//! result's undefined paths (MLS §12.4.4); a later fact that the flag is true,
//! from the condition of a branch or from an assertion, proves it defined.

use super::super::function_returns::{contains_return, statements_break};
use super::*;
use rumoca_core::Literal;

impl FunctionDefinitions {
    /// Note, on entry to the loop at `span` over `body`, the values the body
    /// writes beside every `flag := true`, for each Boolean `flag` that is
    /// false here.
    pub(super) fn note_exit_flags(
        &mut self,
        body: &[rumoca_core::Statement],
        span: Span,
        context: FunctionValidationContext<'_>,
    ) {
        for flag in boolean_scalars(context) {
            if !self.facts.assuming_boolean(&flag, true).is_unreachable() {
                continue;
            }
            let mut companions = Companions::default();
            companions.visit(body, &flag);
            let noted = companions
                .values()
                .into_iter()
                .map(|value| (span, value, flag.clone()))
                .filter(|noted| !self.exit_flags.contains(noted))
                .collect::<Vec<_>>();
            self.exit_flags.extend(noted);
        }
    }

    /// The values the loop at `span` noted on entry.
    pub(in crate::construction::analysis) fn exit_flag_values(&self, span: Span) -> Vec<VarName> {
        self.exit_flags
            .iter()
            .filter(|(noted, _, _)| *noted == span)
            .map(|(_, value, _)| value.clone())
            .collect()
    }

    /// Record, after the loop at `span`, each value it noted that has no
    /// definition on every path as undefined only where its flag is false.
    pub(super) fn track_exit_flag_values(&mut self, span: Span) {
        let noted = std::mem::take(&mut self.exit_flags);
        let (ended, open): (Vec<_>, Vec<_>) =
            noted.into_iter().partition(|(noted, _, _)| *noted == span);
        self.exit_flags = open;
        for (_, value, flag) in ended {
            if self.values.contains_key(&value) {
                continue;
            }
            let undefined = self.facts.assuming_boolean(&flag, false);
            let tracked = self
                .branch_only
                .entry(value)
                .or_insert_with(|| BranchOnlyCoverage {
                    span,
                    guard: None,
                    coverage: None,
                    path_partial: false,
                    seeded: false,
                    undefined_when: None,
                    defined_coverage: None,
                });
            // Paths already known to define the value keep the coverage they
            // proved; a pass that set the flag wrote the whole value.
            match tracked.undefined_when.take() {
                Some(known) => {
                    tracked.undefined_when = Some(known.assuming_boolean(&flag, false));
                    tracked.defined_coverage.get_or_insert(ValueCoverage::Whole);
                }
                None => {
                    tracked.undefined_when = Some(undefined);
                    tracked.defined_coverage = Some(ValueCoverage::Whole);
                }
            }
        }
    }
}

fn boolean_scalars(context: FunctionValidationContext<'_>) -> Vec<VarName> {
    context
        .function
        .outputs
        .iter()
        .chain(&context.function.locals)
        .filter(|value| {
            value.effective_type.dimensions().is_empty()
                && effective_function_scalar_type(context.flat, value)
                    == Some(dae::ScalarType::Boolean)
        })
        .map(|value| VarName::new(&value.name))
        .collect()
}

/// The values assigned as a whole in every statement sequence that sets one
/// flag true, or nothing when some other statement can set it.
#[derive(Default)]
struct Companions {
    values: Option<HashSet<VarName>>,
    unguarded: bool,
}

/// What one statement of a sequence contributes to it.
enum Write {
    SetsFlag,
    Whole(VarName),
    Other,
}

impl Companions {
    fn values(self) -> HashSet<VarName> {
        if self.unguarded {
            return HashSet::new();
        }
        self.values.unwrap_or_default()
    }

    fn visit(&mut self, statements: &[rumoca_core::Statement], flag: &VarName) {
        let mut sets = false;
        let mut whole = HashSet::new();
        for statement in statements {
            match self.visit_statement(statement, flag) {
                Write::SetsFlag => sets = true,
                Write::Whole(target) => drop(whole.insert(target)),
                Write::Other => {}
            }
        }
        if !sets {
            return;
        }
        // A sequence that can leave part-way may set the flag without
        // reaching the assignment beside it.
        if statements_break(statements) || contains_return(statements) {
            self.unguarded = true;
            return;
        }
        self.values = Some(match self.values.take() {
            Some(known) => known.intersection(&whole).cloned().collect(),
            None => whole,
        });
    }

    fn visit_statement(&mut self, statement: &rumoca_core::Statement, flag: &VarName) -> Write {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                return self.assignment(comp, value, flag);
            }
            rumoca_core::Statement::For { equations, .. } => self.visit(equations, flag),
            rumoca_core::Statement::While { block, .. } => self.visit(&block.stmts, flag),
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => cond_blocks
                .iter()
                .map(|block| block.stmts.as_slice())
                .chain(else_block.as_deref())
                .for_each(|arm| self.visit(arm, flag)),
            other => {
                let written =
                    function_ranges::assigned_function_targets(std::slice::from_ref(other));
                self.unguarded |= written.contains(flag.as_str());
            }
        }
        Write::Other
    }

    fn assignment(
        &mut self,
        comp: &rumoca_core::ComponentReference,
        value: &Expression,
        flag: &VarName,
    ) -> Write {
        let target = comp.to_var_name();
        let is_whole = matches!(comp.parts(), [part] if part.subs.is_empty());
        if target != *flag {
            return if is_whole {
                Write::Whole(target)
            } else {
                Write::Other
            };
        }
        match boolean_literal(value) {
            Some(true) if is_whole => Write::SetsFlag,
            Some(false) if is_whole => Write::Other,
            _ => {
                self.unguarded = true;
                Write::Other
            }
        }
    }
}

fn boolean_literal(expression: &Expression) -> Option<bool> {
    match expression {
        Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } => Some(*value),
        _ => None,
    }
}
