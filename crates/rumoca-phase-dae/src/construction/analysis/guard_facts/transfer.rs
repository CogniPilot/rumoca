//! The facts after each statement of a function body, loops included.
//!
//! A loop's head holds an invariant: facts true before its first pass and
//! after every pass that continues. It is found by iterating
//! `head = before ⊔ after_body(head)` from the facts before the loop. A bound
//! that keeps moving is dropped (widening), so the iteration ends after at
//! most one round per bound; the final head is then tightened once by one more
//! pass (`before ⊔ after_body(head)`), kept only when that tighter head is
//! itself proven invariant. A `for` binder holds the elements of its range,
//! evaluated once on entry (MLS §11.2.2); a `while` body runs only where its
//! condition held, and the loop ends where it fails (MLS §11.2.3). A loop
//! whose body can leave it through `break` keeps only the facts about values
//! its body never writes.

use super::*;
use std::collections::BTreeSet;

/// The most head rounds one loop runs before keeping only the facts its body
/// cannot change; each round that does not converge drops at least one
/// bound.
const MAX_HEAD_ROUNDS: usize = 16;

impl GuardFacts {
    /// The facts after `statement` runs from this point.
    pub(in crate::construction::analysis) fn after(
        &mut self,
        statement: &rumoca_core::Statement,
        scope: FactScope<'_>,
    ) {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                self.observe_accesses(value, scope);
                self.observe_target(comp, scope);
                let target = comp.to_var_name();
                match comp.parts() {
                    [part] if part.subs.is_empty() && !scope.is_array(&target) => {
                        self.assign(target, value, scope);
                    }
                    _ => self.forget_written(statement),
                }
            }
            // The statement a generated Boolean is captured by: from here on it
            // holds the value of its definition.
            rumoca_core::Statement::Empty { span } => {
                if let Some(definition) = scope.generated.iter().find(|guard| guard.span == *span) {
                    self.observe_accesses(&definition.value, scope);
                    self.assign(definition.target.clone(), &definition.value, scope);
                }
            }
            rumoca_core::Statement::FunctionCall { args, .. } => {
                for arg in args {
                    self.observe_accesses(arg, scope);
                }
                self.forget_written(statement);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                let conditions = cond_blocks
                    .iter()
                    .map(|block| &block.cond)
                    .collect::<Vec<_>>();
                let mut entries = self.branch_entries(&conditions, scope);
                let branches = cond_blocks
                    .iter()
                    .map(|block| block.stmts.as_slice())
                    .chain(else_block.as_deref());
                for (entry, statements) in entries.iter_mut().zip(branches) {
                    entry.after_sequence(statements, scope);
                }
                *self = Self::join(&entries);
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => *self = self.for_head(indices, equations, scope),
            rumoca_core::Statement::While { block, .. } => {
                let mut exit = self.while_head(block, scope);
                exit.observe_accesses(&block.cond, scope);
                // Only a normal exit evaluated the condition false; a `break`
                // leaves with the condition holding or not.
                *self = if statements_break(&block.stmts) {
                    exit
                } else {
                    exit.assuming(&block.cond, false, scope)
                };
            }
            _ => self.forget_written(statement),
        }
    }

    /// The facts after `statements` run in order from this point.
    pub(in crate::construction::analysis) fn after_sequence(
        &mut self,
        statements: &[rumoca_core::Statement],
        scope: FactScope<'_>,
    ) {
        for statement in statements {
            self.after(statement, scope);
        }
    }

    /// The facts at the head of every pass of a loop whose body is `body`
    /// that keep only what no pass changes: a value the body writes may hold
    /// any pass's value, and a binder (MLS §11.2.2) shadows every fact about
    /// its name.
    pub(in crate::construction::analysis) fn loop_entry(
        &self,
        body: &[rumoca_core::Statement],
        binders: &[VarName],
    ) -> Self {
        self.entry_forgetting(assigned_function_targets(body), binders)
    }

    /// The facts at the head of a loop over `body` as one path sees it: a
    /// branch of the body that these facts exclude never runs, so what it
    /// writes stays known. The exclusions hold at every pass only when no
    /// value they rely on is written by a branch that does run; otherwise
    /// every write counts, as in [`Self::loop_entry`].
    ///
    /// A generated Boolean the body captures is read through its definition,
    /// which the facts at the capture evaluate.
    pub(in crate::construction::analysis) fn loop_entry_on_path(
        &self,
        body: &[rumoca_core::Statement],
        binders: &[VarName],
        scope: FactScope<'_>,
    ) -> Self {
        let mut runs = HashSet::new();
        let mut relied_on = HashSet::new();
        self.clone()
            .collect_running_writes(body, scope, (&mut runs, &mut relied_on));
        if relied_on.iter().any(|name| runs.contains(name)) {
            return self.loop_entry(body, binders);
        }
        self.entry_forgetting(runs, binders)
    }

    fn entry_forgetting(
        &self,
        assigned: impl IntoIterator<Item = String>,
        binders: &[VarName],
    ) -> Self {
        let mut entry = self.clone();
        for name in assigned {
            entry.forget(&VarName::new(name));
        }
        for binder in binders {
            entry.forget(binder);
        }
        entry
    }

    /// [`Self::collect_running_writes`] for one conditional: a branch these
    /// facts exclude contributes the values its conditions read.
    fn collect_running_writes_in_if(
        &self,
        (cond_blocks, else_block): (
            &[rumoca_core::StatementBlock],
            Option<&[rumoca_core::Statement]>,
        ),
        scope: FactScope<'_>,
        (runs, relied_on): (&mut HashSet<String>, &mut HashSet<String>),
    ) {
        let conditions = cond_blocks
            .iter()
            .map(|block| &block.cond)
            .collect::<Vec<_>>();
        let branches = cond_blocks
            .iter()
            .map(|block| block.stmts.as_slice())
            .chain(else_block);
        let mut excluded = false;
        for (mut entry, branch) in self
            .branch_entries(&conditions, scope)
            .into_iter()
            .zip(branches)
        {
            excluded |= entry.is_unreachable();
            if !entry.is_unreachable() {
                entry.collect_running_writes(branch, scope, (runs, relied_on));
            }
        }
        if excluded {
            for condition in &conditions {
                reads_through_captures(condition, scope, relied_on, 0);
            }
        }
    }

    /// Walk `statements` once from these facts: `runs` collects the values
    /// the branches that are not excluded may write, and `relied_on` the
    /// values the exclusions read.
    fn collect_running_writes(
        &mut self,
        statements: &[rumoca_core::Statement],
        scope: FactScope<'_>,
        (runs, relied_on): (&mut HashSet<String>, &mut HashSet<String>),
    ) {
        for statement in statements {
            match statement {
                rumoca_core::Statement::Empty { .. } => self.after(statement, scope),
                rumoca_core::Statement::If {
                    cond_blocks,
                    else_block,
                    ..
                } => self.collect_running_writes_in_if(
                    (cond_blocks, else_block.as_deref()),
                    scope,
                    (runs, relied_on),
                ),
                // A nested loop runs its body: the writes it holds are the ones of
                // the branches not excluded.
                rumoca_core::Statement::For { equations, .. } => {
                    self.clone()
                        .collect_running_writes(equations, scope, (runs, relied_on));
                }
                rumoca_core::Statement::While { block, .. } => {
                    reads_through_captures(&block.cond, scope, relied_on, 0);
                    self.clone()
                        .collect_running_writes(&block.stmts, scope, (runs, relied_on));
                }
                other => runs.extend(assigned_function_targets(std::slice::from_ref(other))),
            }
        }
    }
    /// The invariant at the head of a `for` loop, which is also what holds
    /// after it; the binders are not part of it.
    pub(in crate::construction::analysis) fn for_head(
        &self,
        indices: &[rumoca_core::ForIndex],
        body: &[rumoca_core::Statement],
        scope: FactScope<'_>,
    ) -> Self {
        let binders = indices
            .iter()
            .map(|index| VarName::new(&index.ident))
            .collect::<Vec<_>>();
        // The ranges are evaluated once, before the first pass.
        let binder_facts = indices
            .iter()
            .filter_map(|index| {
                let interval = self.range_interval(&index.range, scope);
                (!interval.is_unbounded()).then(|| {
                    (
                        FactSubject::scalar(VarName::new(&index.ident)),
                        ValueFact::Integer(interval),
                    )
                })
            })
            .collect::<BTreeMap<_, _>>();
        self.loop_head(body, &binders, scope, |head| {
            let mut pass = head.clone();
            for binder in &binders {
                pass.forget(binder);
            }
            pass.path = conjoin(pass.path, Some(binder_facts.clone()));
            pass.after_sequence(body, scope);
            for binder in &binders {
                pass.forget(binder);
            }
            pass
        })
    }

    /// The invariant at the head of a `while` loop: what holds each time its
    /// condition is about to be evaluated.
    pub(in crate::construction::analysis) fn while_head(
        &self,
        block: &rumoca_core::StatementBlock,
        scope: FactScope<'_>,
    ) -> Self {
        self.loop_head(&block.stmts, &[], scope, |head| {
            let mut pass = head.clone();
            pass.observe_accesses(&block.cond, scope);
            let mut pass = pass.assuming(&block.cond, true, scope);
            pass.after_sequence(&block.stmts, scope);
            pass
        })
    }

    fn loop_head(
        &self,
        body: &[rumoca_core::Statement],
        binders: &[VarName],
        scope: FactScope<'_>,
        pass: impl Fn(&Self) -> Self,
    ) -> Self {
        if statements_break(body) {
            return self.loop_entry_on_path(body, binders, scope);
        }
        let next = |head: &Self| {
            let mut joined = self.clone();
            joined.join_path(&pass(head));
            joined
        };
        let thresholds = widening_thresholds(body, scope);
        let mut head = self.clone();
        for round in 0..MAX_HEAD_ROUNDS {
            let candidate = next(&head);
            if candidate.within(&head) {
                // `head` is invariant; `candidate` is tighter and is kept when
                // it is invariant too.
                return Some(candidate)
                    .filter(|candidate| next(candidate).within(candidate))
                    .unwrap_or(head);
            }
            // A moving bound first steps to the next threshold; late rounds
            // drop it, so the head rounds stay bounded.
            let steps = if round < MAX_HEAD_ROUNDS / 2 {
                thresholds.as_slice()
            } else {
                &[]
            };
            head = head.widened(&candidate, steps);
        }
        self.loop_entry_on_path(body, binders, scope)
    }

    /// Whether every fact `other` proves is proven here at least as tightly.
    fn within(&self, other: &Self) -> bool {
        let Some(facts) = &self.path else {
            return true;
        };
        let Some(bounds) = &other.path else {
            return false;
        };
        bounds.iter().all(|(subject, bound)| {
            facts
                .get(subject)
                .is_some_and(|fact| fact_within(*fact, *bound))
        }) && other
            .selections
            .iter()
            .all(|(name, selection)| self.selections.get(name) == Some(selection))
    }

    /// These facts weakened to also cover `next`: each Integer bound `next`
    /// exceeds steps out to the nearest threshold that covers it, or is dropped
    /// when none does; a moving Real bound and each selection `next` does not
    /// repeat are dropped.
    fn widened(&self, next: &Self, thresholds: &[i64]) -> Self {
        let Some(facts) = &self.path else {
            return next.clone();
        };
        let Some(next_facts) = &next.path else {
            return self.clone();
        };
        let path = facts
            .iter()
            .filter_map(|(subject, fact)| {
                let widened = widen_fact(*fact, *next_facts.get(subject)?, thresholds)?;
                Some((subject.clone(), widened))
            })
            .collect();
        let selections = self
            .selections
            .iter()
            .filter(|(name, selection)| next.selections.get(*name) == Some(*selection))
            .map(|(name, selection)| (name.clone(), selection.clone()))
            .collect();
        Self {
            path: Some(path),
            selections,
        }
    }

    /// The Integer values a range expression yields, from the facts here.
    fn range_interval(&self, range: &Expression, scope: FactScope<'_>) -> IntegerInterval {
        let Expression::Range {
            start, step, end, ..
        } = range
        else {
            return IntegerInterval::UNBOUNDED;
        };
        let step = match step {
            None => 1,
            Some(step) => match integer_literal(step) {
                Some(step) => step,
                None => return IntegerInterval::UNBOUNDED,
            },
        };
        let start = self.integer_interval(start, scope);
        let end = self.integer_interval(end, scope);
        // MLS §10.4.1: every element lies between the start and the end.
        match step.signum() {
            1 => IntegerInterval {
                lower: start.lower,
                upper: end.upper,
            },
            -1 => IntegerInterval {
                lower: end.lower,
                upper: start.upper,
            },
            _ => IntegerInterval::UNBOUNDED,
        }
    }

    fn forget_written(&mut self, statement: &rumoca_core::Statement) {
        for name in assigned_function_targets(std::slice::from_ref(statement)) {
            self.forget(&VarName::new(name));
        }
    }

    /// Conjoin the index facts of the element accesses evaluating
    /// `expression` certainly performs: MLS §10.5 makes an index outside
    /// `1..size(a, d)` an error, so a completed access `a[k]` with a scalar
    /// Integer `k` proves `1 <= k <= size(a, d)`. An access inside an
    /// operand of `and`/`or` or in an arm of a conditional expression may be
    /// skipped (MLS §3.3) and proves nothing.
    pub(in crate::construction::analysis) fn observe_accesses(
        &mut self,
        expression: &Expression,
        scope: FactScope<'_>,
    ) {
        let mut facts = BTreeMap::new();
        certain_accesses(expression, scope, &mut facts);
        self.observe(facts);
    }

    fn observe_target(&mut self, target: &rumoca_core::ComponentReference, scope: FactScope<'_>) {
        let mut facts = BTreeMap::new();
        if let [part] = target.parts() {
            index_facts(&VarName::new(&part.ident), &part.subs, scope, &mut facts);
        }
        let subscripts = target.parts().iter().flat_map(|part| &part.subs);
        for subscript in subscripts {
            if let Subscript::Expr { expr, .. } = subscript {
                certain_accesses(expr, scope, &mut facts);
            }
        }
        self.observe(facts);
    }

    fn observe(&mut self, facts: BTreeMap<FactSubject, ValueFact>) {
        if facts.is_empty() {
            return;
        }
        self.path = conjoin(self.path.take(), Some(facts));
        if self.path.is_none() {
            self.selections.clear();
        }
    }
}

fn certain_accesses(
    expression: &Expression,
    scope: FactScope<'_>,
    facts: &mut BTreeMap<FactSubject, ValueFact>,
) {
    if let Some((name, subscripts)) = named_access(expression) {
        index_facts(name, subscripts, scope, facts);
        for subscript in subscripts {
            if let Subscript::Expr { expr, .. } = subscript {
                certain_accesses(expr, scope, facts);
            }
        }
        return;
    }
    match expression {
        Expression::Binary {
            op: OpBinary::And | OpBinary::Or,
            ..
        } => {}
        Expression::Binary { lhs, rhs, .. } => {
            certain_accesses(lhs, scope, facts);
            certain_accesses(rhs, scope, facts);
        }
        Expression::Unary { rhs, .. } => certain_accesses(rhs, scope, facts),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            for arg in args {
                certain_accesses(arg, scope, facts);
            }
        }
        Expression::If { branches, .. } => {
            if let Some((condition, _)) = branches.first() {
                certain_accesses(condition, scope, facts);
            }
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            for element in elements {
                certain_accesses(element, scope, facts);
            }
        }
        _ => {}
    }
}

/// `1 <= k <= size(array, d)` for each scalar Integer subscript `k` on an
/// axis `d` whose extent is proven.
fn index_facts(
    array: &VarName,
    subscripts: &[Subscript],
    scope: FactScope<'_>,
    facts: &mut BTreeMap<FactSubject, ValueFact>,
) {
    let Some(shape) = scope.shapes.get(array) else {
        return;
    };
    for (subscript, extent) in subscripts.iter().zip(shape) {
        let Subscript::Expr { expr, .. } = subscript else {
            continue;
        };
        let Some(subject) = FactSubject::of(expr) else {
            continue;
        };
        if !subject.is_scalar() || !scope.is_integer(&subject) {
            continue;
        }
        let fact = ValueFact::Integer(IntegerInterval::finite(1, i64::from(*extent)));
        let merged = match facts.get(&subject) {
            Some(existing) => existing.meet(fact),
            None => fact,
        };
        facts.insert(subject, merged);
    }
}

/// Whether `fact` proves at least what `bound` does.
fn fact_within(fact: ValueFact, bound: ValueFact) -> bool {
    fn side<T: PartialOrd>(fact: Option<T>, bound: Option<T>, tighter: fn(&T, &T) -> bool) -> bool {
        match (fact, bound) {
            (_, None) => true,
            (Some(fact), Some(bound)) => tighter(&fact, &bound),
            (None, Some(_)) => false,
        }
    }
    match (fact, bound) {
        (ValueFact::Integer(fact), ValueFact::Integer(bound)) => {
            side(fact.lower, bound.lower, |a, b| a >= b)
                && side(fact.upper, bound.upper, |a, b| a <= b)
        }
        (ValueFact::Real(fact), ValueFact::Real(bound)) => {
            side(fact.lower, bound.lower, |a, b| a >= b)
                && side(fact.upper, bound.upper, |a, b| a <= b)
        }
        _ => false,
    }
}

/// `fact` with each bound `next` exceeds dropped; `None` when nothing is
/// left.
/// `fact` widened to cover `next` (see `GuardFacts::widened`); `None` when
/// nothing is left. `thresholds` is sorted ascending.
fn widen_fact(fact: ValueFact, next: ValueFact, thresholds: &[i64]) -> Option<ValueFact> {
    fn keep<T: PartialOrd>(
        bound: Option<T>,
        next: Option<T>,
        holds: fn(&T, &T) -> bool,
    ) -> Option<T> {
        match (bound, next) {
            (Some(bound), Some(next)) if holds(&next, &bound) => Some(bound),
            _ => None,
        }
    }
    let widened = match (fact, next) {
        (ValueFact::Integer(fact), ValueFact::Integer(next)) => {
            let lower = keep(fact.lower, next.lower, |n, b| n >= b).or_else(|| {
                let next = next.lower?;
                thresholds.iter().rev().copied().find(|&step| step <= next)
            });
            let upper = keep(fact.upper, next.upper, |n, b| n <= b).or_else(|| {
                let next = next.upper?;
                thresholds.iter().copied().find(|&step| step >= next)
            });
            ValueFact::Integer(IntegerInterval { lower, upper })
        }
        (ValueFact::Real(fact), ValueFact::Real(next)) => ValueFact::Real(RealInterval {
            lower: keep(fact.lower, next.lower, |n, b| n >= b),
            upper: keep(fact.upper, next.upper, |n, b| n <= b),
        }),
        _ => return None,
    };
    (!widened.is_unbounded()).then_some(widened)
}

/// The Integer literals `body` reads and the extents of the arrays it
/// names, ascending: the values a bound the body moves can settle at (an
/// element access bounds its index by an extent; a relation, by a literal).
fn widening_thresholds(body: &[rumoca_core::Statement], scope: FactScope<'_>) -> Vec<i64> {
    struct Collect<'a> {
        scope: FactScope<'a>,
        values: BTreeSet<i64>,
    }
    impl Collect<'_> {
        fn extents(&mut self, name: &VarName) {
            if let Some(shape) = self.scope.shapes.get(name) {
                self.values
                    .extend(shape.iter().map(|extent| i64::from(*extent)));
            }
        }
    }
    impl rumoca_core::ExpressionVisitor for Collect<'_> {
        fn visit_literal(&mut self, literal: &Literal) {
            if let Literal::Integer(value) = literal {
                self.values.insert(*value);
            }
        }
        fn visit_var_ref(&mut self, reference: &rumoca_core::Reference, subscripts: &[Subscript]) {
            self.extents(reference.var_name());
            self.walk_var_ref(reference, subscripts);
        }
    }
    impl rumoca_ir_flat::visitor::StatementVisitor for Collect<'_> {
        fn visit_component_reference(&mut self, comp: &rumoca_core::ComponentReference) {
            if let Some(part) = comp.parts().first() {
                self.extents(&VarName::new(&part.ident));
            }
            for subscript in comp.parts().iter().flat_map(|part| &part.subs) {
                rumoca_core::ExpressionVisitor::visit_subscript(self, subscript);
            }
        }
    }
    let mut collect = Collect {
        scope,
        values: BTreeSet::new(),
    };
    for statement in body {
        rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut collect, statement);
    }
    collect.values.into_iter().collect()
}

/// Whether `statements` can leave their innermost enclosing loop through
/// `break` (a `break` inside a nested loop leaves only that loop).
fn statements_break(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Break { .. } => true,
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| statements_break(&block.stmts))
                || else_block.as_deref().is_some_and(statements_break)
        }
        _ => false,
    })
}

/// How many captured definitions deep a condition's reads are followed.
const MAX_CAPTURE_DEPTH: usize = 8;

/// Every value `expression` reads, following each generated Boolean to the
/// values its definition reads.
fn reads_through_captures(
    expression: &Expression,
    scope: FactScope<'_>,
    reads: &mut HashSet<String>,
    depth: usize,
) {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    for reference in references {
        reads.insert(reference.as_str().to_string());
        if depth < MAX_CAPTURE_DEPTH
            && let Some(definition) = scope
                .generated
                .iter()
                .find(|guard| guard.target == reference)
        {
            reads_through_captures(&definition.value, scope, reads, depth + 1);
        }
    }
}
