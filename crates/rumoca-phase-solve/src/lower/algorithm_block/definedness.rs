//! Construction proof that no read observes an unassigned local.
//!
//! The GALEC interpreter faults on reading a local (or returning a function
//! output) that was never assigned, and a checked package does not prove that
//! cannot happen. Solve needs a value on every path, so lowering stores zero
//! first; this proof is what makes that zero unobservable, and lowering
//! refuses any read it cannot prove.
//!
//! A local is defined always, or under guard conjunctions: generated code
//! assigns under `if c then` and later reads under the same `c`, re-evaluated
//! with its operands unchanged. Guards compare by their span-free canonical
//! text; assigning anything a guard reads forgets every conjunction using it.
//! A variable written by every branch of an `if` with an `else` is defined
//! under the outer guards, and a loop nest that writes every element of an
//! array through plain iterator subscripts over full axes defines the array.
//! A variable defined at the end of every arm of an `if` with an `else` is
//! defined after it.
//!
//! An element assignment defines that element: `a[k] := 0` makes a later
//! `a[k]` readable while nothing its subscripts read has changed (loop
//! iterators included, so a fact never crosses an iteration), and literal
//! element writes covering every element define the whole array.

use rumoca_ir_galec::ast::{Direction, IfStatement, UserFunction};

use super::*;

/// A guard: canonical condition text and the polarity it holds with.
type Guard = (String, bool);

/// A loop iterator with its static `(start, stop, step)`.
type Iterator = (String, i64, i64, i64);

/// One element of an array: canonical subscripts plus what they resolve to
/// read, so a shadowing iterator never matches an outer one's fact.
type Element = (VarId, String, BTreeSet<VarId>);

/// Where a concrete element write's subscript comes from.
#[derive(Clone, Copy)]
enum Subscript {
    Literal(i64),
    /// Position of the enclosing iterator in the walk's iterator stack.
    Iterator(usize),
}

/// Arrays a loop writes whole, and concrete elements it writes.
type Covered = (Vec<VarId>, Vec<(VarId, Vec<i64>)>);

/// Literal coverage is checked only up to this many elements.
const MAX_COVERED_ELEMENTS: usize = 1024;

#[derive(Default)]
struct Defs {
    always: BTreeSet<VarId>,
    under: BTreeMap<VarId, Vec<BTreeSet<Guard>>>,
    /// What each guard's condition reads.
    guard_reads: BTreeMap<String, BTreeSet<VarId>>,
    /// Guards on the current path whose operands changed since they were
    /// evaluated: nothing defined under them now can be proven later.
    stale: BTreeSet<String>,
    /// Element facts, under guard conjunctions as for `under`.
    elements: BTreeMap<Element, Vec<BTreeSet<Guard>>>,
}

impl Defs {
    fn define(&mut self, id: VarId, guards: &[Guard]) {
        if guards.iter().any(|(guard, _)| self.stale.contains(guard)) {
            return;
        }
        if guards.is_empty() {
            self.always.insert(id);
        } else {
            self.under
                .entry(id)
                .or_default()
                .push(guards.iter().cloned().collect());
        }
    }

    fn define_element(&mut self, element: Element, guards: &[Guard]) {
        if guards.iter().any(|(guard, _)| self.stale.contains(guard)) {
            return;
        }
        self.elements
            .entry(element)
            .or_default()
            .push(guards.iter().cloned().collect());
    }

    fn defined(&self, id: VarId, guards: &[Guard]) -> bool {
        self.always.contains(&id)
            || self
                .under
                .get(&id)
                .is_some_and(|conjunctions| holds(conjunctions, guards))
    }

    fn element_defined(&self, element: &Element, guards: &[Guard]) -> bool {
        self.defined(element.0, guards)
            || self
                .elements
                .get(element)
                .is_some_and(|conjunctions| holds(conjunctions, guards))
    }

    /// `id` changed: a guard reading it no longer repeats its earlier value,
    /// and an element fact whose subscripts read it names another element.
    fn invalidate(&mut self, id: VarId, path: &[Guard]) {
        self.elements
            .retain(|(_, _, reads), _| !reads.contains(&id));
        let stale = self
            .guard_reads
            .iter()
            .filter(|(_, reads)| reads.contains(&id))
            .map(|(guard, _)| guard.clone())
            .collect::<BTreeSet<_>>();
        if stale.is_empty() {
            return;
        }
        self.stale.extend(
            path.iter()
                .filter(|(guard, _)| stale.contains(guard))
                .map(|(guard, _)| guard.clone()),
        );
        for conjunctions in self.under.values_mut().chain(self.elements.values_mut()) {
            conjunctions
                .retain(|conjunction| conjunction.iter().all(|(guard, _)| !stale.contains(guard)));
        }
    }
}

/// Some conjunction holds on `guards`.
fn holds(conjunctions: &[BTreeSet<Guard>], guards: &[Guard]) -> bool {
    conjunctions
        .iter()
        .any(|conjunction| conjunction.iter().all(|guard| guards.contains(guard)))
}

struct Proof<'s, 'a> {
    scope: &'s MethodScope<'a>,
    defs: Defs,
    guards: Vec<Guard>,
    /// Loop iterators in scope with their static `(low, high, step)`.
    loops: Vec<(String, i64, i64, i64)>,
}

/// Every local read in `statements` is preceded by a provable assignment.
pub(super) fn require_defined_reads(
    scope: &MethodScope<'_>,
    statements: &[Spanned<Statement>],
) -> Result<(), AlgorithmBlockLowerError> {
    // Method locals start unassigned.
    prove(scope, statements, |_| false).map(|_| ())
}

/// A function's reads are defined and every output is assigned on every path
/// at its return.
pub(super) fn require_defined_function(
    scope: &MethodScope<'_>,
    function: &UserFunction,
) -> Result<(), AlgorithmBlockLowerError> {
    let defs = prove(scope, &function.statements, |index| {
        function
            .parameters
            .get(index)
            .is_some_and(|parameter| parameter.direction == Direction::Input)
    })?;
    for (index, parameter) in function.parameters.iter().enumerate() {
        if parameter.direction == Direction::Output && !defs.always.contains(&VarId::Local(index)) {
            return Err(unsupported(
                "function output not assigned on every path",
                parameter.decl.span,
            ));
        }
    }
    Ok(())
}

/// `starts_defined(local)` names the locals defined on entry (function
/// inputs).
fn prove(
    scope: &MethodScope<'_>,
    statements: &[Spanned<Statement>],
    starts_defined: impl Fn(usize) -> bool,
) -> Result<Defs, AlgorithmBlockLowerError> {
    let mut proof = Proof {
        scope,
        defs: Defs::default(),
        guards: Vec::new(),
        loops: Vec::new(),
    };
    for index in 0..scope.locals.len() {
        if starts_defined(index) {
            proof.defs.always.insert(VarId::Local(index));
        }
    }
    proof.statements(statements)?;
    Ok(proof.defs)
}

impl Proof<'_, '_> {
    fn statements(
        &mut self,
        statements: &[Spanned<Statement>],
    ) -> Result<(), AlgorithmBlockLowerError> {
        for statement in statements {
            self.statement(&statement.node, statement.span)?;
        }
        Ok(())
    }

    fn statement(
        &mut self,
        statement: &Statement,
        span: Span,
    ) -> Result<(), AlgorithmBlockLowerError> {
        match statement {
            Statement::Assignment { target, value } => {
                self.expression(value, span)?;
                self.assign(target, span)
            }
            Statement::MultiAssignment { targets, call } => {
                self.arguments(call, span)?;
                for target in targets {
                    self.assign(target, span)?;
                }
                Ok(())
            }
            Statement::Call(call) => self.arguments(call, span),
            Statement::If(if_statement) => self.if_statement(if_statement, span),
            Statement::For(for_loop) => self.for_loop(for_loop, span),
            Statement::Limit(targets) => {
                for target in targets {
                    self.limit(target, span)?;
                }
                Ok(())
            }
            Statement::Signal(_) => Ok(()),
        }
    }

    /// Saturation reads and may rewrite its targets, so a guard reading them
    /// no longer repeats (regression: `limit g` between two `g < 1.5` guards).
    fn limit(&mut self, target: &LimitTarget, span: Span) -> Result<(), AlgorithmBlockLowerError> {
        match target {
            LimitTarget::SelfState => {
                for index in 0..self.scope.scope.block.len() {
                    self.defs.invalidate(VarId::Block(index), &self.guards);
                }
            }
            LimitTarget::Reference(reference) => {
                self.expression(&Expression::Ref(reference.clone()), span)?;
                let id = self.scope.resolve(reference)?;
                self.defs.invalidate(id, &self.guards);
            }
        }
        Ok(())
    }

    fn arguments(
        &mut self,
        call: &FunctionCall,
        span: Span,
    ) -> Result<(), AlgorithmBlockLowerError> {
        for argument in &call.arguments {
            self.expression(argument, span)?;
        }
        Ok(())
    }

    /// A whole assignment defines the target; an element assignment defines
    /// that element (and, see `for_loop`, may complete a loop's coverage).
    fn assign(&mut self, target: &Reference, span: Span) -> Result<(), AlgorithmBlockLowerError> {
        let (id, subscripts) = self.scope.resolve_place(target)?;
        for subscript in subscripts {
            self.expression(subscript, span)?;
        }
        self.defs.invalidate(id, &self.guards);
        let guards = self.guards.clone();
        if subscripts.is_empty() {
            self.defs.define(id, &guards);
        } else {
            // `a[a[1]] := ...` may move its own subscript: no fact.
            let element = element(self.scope, id, subscripts);
            if !element.2.contains(&id) {
                self.defs.define_element(element, &guards);
            }
            if self.literally_covered(id) {
                self.defs.define(id, &guards);
            }
        }
        Ok(())
    }

    /// Every element of `id` is defined here through literal subscripts.
    fn literally_covered(&self, id: VarId) -> bool {
        let dims = &self.scope.variable(id).dims;
        let count = dims.iter().try_fold(1usize, |count, &extent| {
            count.checked_mul(usize::try_from(extent).ok()?)
        });
        if !count.is_some_and(|count| (1..=MAX_COVERED_ELEMENTS).contains(&count)) {
            return false;
        }
        let mut index = vec![1i64; dims.len()];
        loop {
            if !self
                .defs
                .element_defined(&(id, literal_key(&index), BTreeSet::new()), &self.guards)
            {
                return false;
            }
            if !advance(&mut index, dims) {
                return true;
            }
        }
    }

    fn if_statement(
        &mut self,
        if_statement: &IfStatement,
        span: Span,
    ) -> Result<(), AlgorithmBlockLowerError> {
        let depth = self.guards.len();
        let mut arms = Vec::new();
        for branch in &if_statement.branches {
            let Condition::Expression(condition) = &branch.condition else {
                return Err(unsupported("signal check", branch.span));
            };
            self.expression(condition, span)?;
            let guard = canonical(condition);
            let mut reads = BTreeSet::new();
            expression_reads(self.scope, condition, &mut reads);
            self.defs
                .guard_reads
                .entry(guard.clone())
                .or_default()
                .extend(reads);
            // A fresh evaluation of the condition.
            self.defs.stale.remove(&guard);
            self.guards.push((guard.clone(), true));
            self.statements(&branch.body)?;
            arms.push(self.guards.clone());
            self.guards.pop();
            // Later branches run where this condition was false.
            self.guards.push((guard, false));
        }
        if let Some(body) = &if_statement.else_body {
            self.statements(body)?;
            arms.push(self.guards.clone());
        }
        self.guards.truncate(depth);
        if if_statement.else_body.is_some() {
            let guards = self.guards.clone();
            // Exactly one arm ran to its end: whatever each arm leaves
            // defined is defined after the chain.
            let every_arm = self
                .defs
                .under
                .keys()
                .copied()
                .filter(|&id| arms.iter().all(|arm| self.defs.defined(id, arm)))
                .collect::<Vec<_>>();
            for id in every_arm {
                self.defs.define(id, &guards);
            }
            let every_branch =
                Body::Chain(&if_statement.branches, if_statement.else_body.as_deref())
                    .definite(self.scope);
            for id in every_branch {
                self.defs.define(id, &guards);
            }
        }
        Ok(())
    }

    fn for_loop(&mut self, for_loop: &ForLoop, span: Span) -> Result<(), AlgorithmBlockLowerError> {
        let (Some(iterator), Some((start, stop, step))) =
            (&for_loop.iterator, loop_range(self.scope, for_loop))
        else {
            return Err(unsupported("for loop with a non-static range", span));
        };
        let runs = if step > 0 {
            start <= stop
        } else {
            start >= stop
        };
        if !runs {
            return Ok(());
        }
        let name = iterator.lexeme().to_owned();
        self.loops.push((name.clone(), start, stop, step));
        let scope = self.scope;
        let covered = scope.with_loop(&name, |depth| -> Result<_, AlgorithmBlockLowerError> {
            // The iterator is (re)bound here and on every iteration: no fact
            // proven under a guard reading an earlier binding carries over
            // (regression: two loops over `i` at the same depth).
            self.defs.invalidate(VarId::Loop(depth), &self.guards);
            self.statements(&for_loop.body)?;
            let covered = self.covered_by(&for_loop.body, (name.clone(), start, stop, step));
            self.defs.invalidate(VarId::Loop(depth), &self.guards);
            Ok(covered)
        })?;
        self.loops.pop();
        let guards = self.guards.clone();
        let (whole, elements) = covered;
        for id in whole {
            self.defs.define(id, &guards);
        }
        for (id, index) in elements {
            self.defs
                .define_element((id, literal_key(&index), BTreeSet::new()), &guards);
            if self.literally_covered(id) {
                self.defs.define(id, &guards);
            }
        }
        Ok(())
    }

    /// Arrays whose every element the loop over `outer` writes: an
    /// unconditional element assignment (possibly inside nested full loops)
    /// whose subscripts are the enclosing iterators, outermost first, each
    /// ranging over its whole axis with step 1.
    ///
    /// Also every concrete element such an unconditional assignment writes when
    /// each subscript is a literal or an enclosing iterator: the loops run, so
    /// every combination of their values is written.
    fn covered_by(&self, body: &[Spanned<Statement>], outer: Iterator) -> Covered {
        let mut covered = (Vec::new(), Vec::new());
        let mut iterators = vec![outer];
        self.collect_covered(body, &mut iterators, &mut covered);
        covered
    }

    fn collect_covered(
        &self,
        body: &[Spanned<Statement>],
        iterators: &mut Vec<Iterator>,
        covered: &mut Covered,
    ) {
        for statement in body {
            match &statement.node {
                Statement::Assignment { target, .. } => {
                    covered.0.extend(self.full_element_write(target, iterators));
                    covered
                        .1
                        .extend(self.concrete_element_writes(target, iterators));
                }
                Statement::For(inner) => self.collect_covered_loop(inner, iterators, covered),
                _ => {}
            }
        }
    }

    /// The concrete (1-based) elements an element assignment whose subscripts
    /// are literals or enclosing iterators writes over the loops' ranges.
    fn concrete_element_writes(
        &self,
        target: &Reference,
        iterators: &[Iterator],
    ) -> Vec<(VarId, Vec<i64>)> {
        let Ok((id, subscripts)) = self.scope.resolve_place(target) else {
            return Vec::new();
        };
        if subscripts.is_empty() || matches!(id, VarId::Loop(_)) {
            return Vec::new();
        }
        // Each subscript is a literal or names one enclosing iterator; a
        // shared iterator takes ONE value per iteration, so the written cells
        // are enumerated over bindings of the iterators, not over axes
        // (`a[i,i]` writes the diagonal).
        let axes = subscripts
            .iter()
            .map(|subscript| self.subscript_source(subscript, iterators))
            .collect::<Option<Vec<_>>>();
        let Some(axes) = axes else {
            return Vec::new();
        };
        let mut bound: Vec<usize> = axes
            .iter()
            .filter_map(|axis| match axis {
                Subscript::Iterator(position) => Some(*position),
                Subscript::Literal(_) => None,
            })
            .collect();
        bound.sort_unstable();
        bound.dedup();
        let values = bound
            .iter()
            .map(|&position| {
                let (_, start, stop, step) = &iterators[position];
                range_values(*start, *stop, *step)
            })
            .collect::<Option<Vec<_>>>();
        let Some(values) = values else {
            return Vec::new();
        };
        combinations(&values)
            .into_iter()
            .map(|binding| {
                let index = axes
                    .iter()
                    .map(|axis| match axis {
                        Subscript::Literal(value) => *value,
                        Subscript::Iterator(position) => {
                            let slot = bound.iter().position(|bound| bound == position);
                            slot.map_or(0, |slot| binding[slot])
                        }
                    })
                    .collect();
                (id, index)
            })
            .collect()
    }

    /// Where a subscript's value comes from: a literal, or the innermost
    /// enclosing iterator of that name (by its position in `iterators`).
    fn subscript_source(
        &self,
        subscript: &Expression,
        iterators: &[Iterator],
    ) -> Option<Subscript> {
        match subscript {
            Expression::Integer(value) => Some(Subscript::Literal(*value)),
            Expression::Ref(reference @ Reference::Local(part)) if part.subscripts.is_empty() => {
                let Ok((VarId::Loop(_), [])) = self.scope.resolve_place(reference) else {
                    return None;
                };
                iterators
                    .iter()
                    .rposition(|(name, ..)| name == part.name.lexeme())
                    .map(Subscript::Iterator)
            }
            _ => None,
        }
    }

    /// A literal subscript's value, or every value of the enclosing iterator a
    /// subscript names.
    fn subscript_values(&self, subscript: &Expression, iterators: &[Iterator]) -> Option<Vec<i64>> {
        match subscript {
            Expression::Integer(value) => Some(vec![*value]),
            Expression::Ref(reference @ Reference::Local(part)) if part.subscripts.is_empty() => {
                let Ok((VarId::Loop(_), [])) = self.scope.resolve_place(reference) else {
                    return None;
                };
                let (_, start, stop, step) = iterators
                    .iter()
                    .rev()
                    .find(|(name, ..)| name == part.name.lexeme())?;
                range_values(*start, *stop, *step)
            }
            _ => None,
        }
    }

    fn collect_covered_loop(
        &self,
        inner: &ForLoop,
        iterators: &mut Vec<Iterator>,
        covered: &mut Covered,
    ) {
        let (Some(iterator), Some((start, stop, step))) =
            (&inner.iterator, loop_range(self.scope, inner))
        else {
            return;
        };
        // A loop that never runs writes nothing.
        if range_values(start, stop, step).is_none_or(|values| values.is_empty()) {
            return;
        }
        iterators.push((iterator.lexeme().to_owned(), start, stop, step));
        self.scope.with_loop(iterator.lexeme(), |_| {
            self.collect_covered(&inner.body, iterators, covered);
        });
        iterators.pop();
    }

    /// An element write `a[i1, ..., in]` whose subscripts are the enclosing
    /// iterators in order, each running `1..extent` with step 1.
    fn full_element_write(&self, target: &Reference, iterators: &[Iterator]) -> Option<VarId> {
        let (id, subscripts) = self.scope.resolve_place(target).ok()?;
        let dims = &self.scope.variable(id).dims;
        if subscripts.len() != iterators.len() || dims.len() != iterators.len() {
            return None;
        }
        // A repeated iterator name shadows the outer binding: every subscript
        // spelled with it means the innermost loop, so the axes are not
        // independent. Decline.
        let names = iterators
            .iter()
            .map(|(name, ..)| name.as_str())
            .collect::<BTreeSet<_>>();
        if names.len() != iterators.len() {
            return None;
        }
        for ((subscript, (iterator, start, stop, step)), &extent) in
            subscripts.iter().zip(iterators).zip(dims)
        {
            let Expression::Ref(Reference::Local(part)) = subscript else {
                return None;
            };
            if part.name.lexeme() != iterator || !part.subscripts.is_empty() {
                return None;
            }
            if (*start, *stop, *step) != (1, i64::from(extent), 1) {
                return None;
            }
        }
        Some(id)
    }

    fn expression(
        &mut self,
        expression: &Expression,
        span: Span,
    ) -> Result<(), AlgorithmBlockLowerError> {
        let mut reads = BTreeSet::new();
        expression_reads(self.scope, expression, &mut reads);
        for id in reads {
            let local = matches!(id, VarId::Local(_));
            if local
                && !self.defs.defined(id, &self.guards)
                && !self.elements_defined(id, expression)
            {
                return Err(unsupported("read of a possibly unassigned local", span));
            }
        }
        Ok(())
    }

    /// Every concrete element a read can denote is defined: its subscripts are
    /// literals or in-scope iterators with static ranges.
    fn every_denoted_element_defined(&self, id: VarId, subscripts: &[Expression]) -> bool {
        let axes = subscripts
            .iter()
            .map(|subscript| self.subscript_values(subscript, &self.loops))
            .collect::<Option<Vec<_>>>();
        let Some(axes) = axes else {
            return false;
        };
        let elements = combinations(&axes);
        !elements.is_empty()
            && elements.iter().all(|index| {
                self.defs
                    .element_defined(&(id, literal_key(index), BTreeSet::new()), &self.guards)
            })
    }

    /// Every read of `id` in `expression` is an element read with a fact.
    fn elements_defined(&self, id: VarId, expression: &Expression) -> bool {
        let mut all = true;
        visit_references(expression, &mut |reference| {
            let Ok((read, subscripts)) = self.scope.resolve_place(reference) else {
                return;
            };
            if read == id {
                all &= !subscripts.is_empty()
                    && (self
                        .defs
                        .element_defined(&element(self.scope, id, subscripts), &self.guards)
                        || self.every_denoted_element_defined(id, subscripts));
            }
        });
        all
    }
}

/// Step a 1-based row-major index to the next element, last axis fastest;
/// false once every element has been visited.
fn advance(index: &mut [i64], dims: &[u32]) -> bool {
    for axis in (0..index.len()).rev() {
        if index[axis] < i64::from(dims[axis]) {
            index[axis] += 1;
            return true;
        }
        index[axis] = 1;
    }
    false
}

/// The canonical key of a concrete element, as a literal subscript spells it.
fn literal_key(index: &[i64]) -> String {
    index.iter().map(|value| format!("{value}i,")).collect()
}

/// The values a static loop range takes, when there are few enough to list.
fn range_values(start: i64, stop: i64, step: i64) -> Option<Vec<i64>> {
    if step == 0 {
        return None;
    }
    let mut values = Vec::new();
    let mut value = start;
    while (step > 0 && value <= stop) || (step < 0 && value >= stop) {
        values.push(value);
        if values.len() > MAX_COVERED_ELEMENTS {
            return None;
        }
        value = value.checked_add(step)?;
    }
    Some(values)
}

/// Every index tuple of the given per-axis values (bounded).
fn combinations(axes: &[Vec<i64>]) -> Vec<Vec<i64>> {
    let count = axes
        .iter()
        .try_fold(1usize, |count, axis| count.checked_mul(axis.len()));
    if count.is_none_or(|count| count > MAX_COVERED_ELEMENTS) {
        return Vec::new();
    }
    axes.iter().fold(vec![Vec::new()], |prefixes, axis| {
        prefixes
            .iter()
            .flat_map(|prefix| {
                axis.iter().map(move |value| {
                    let mut index = prefix.clone();
                    index.push(*value);
                    index
                })
            })
            .collect()
    })
}

/// The element of `id` that `subscripts` name.
fn element(scope: &MethodScope<'_>, id: VarId, subscripts: &[Expression]) -> Element {
    let mut key = String::new();
    let mut reads = BTreeSet::new();
    for subscript in subscripts {
        write_canonical(subscript, &mut key);
        key.push(',');
        expression_reads(scope, subscript, &mut reads);
    }
    (id, key, reads)
}

/// Every reference `expression` reads, subscripts included.
fn visit_references(expression: &Expression, visit: &mut impl FnMut(&Reference)) {
    match expression {
        Expression::Ref(reference) | Expression::Neg(reference) => {
            visit(reference);
            let parts: &[RefPart] = match reference {
                Reference::Local(part) => std::slice::from_ref(part),
                Reference::State(parts) => parts,
            };
            for subscript in parts.iter().flat_map(|part| &part.subscripts) {
                visit_references(subscript, visit);
            }
        }
        Expression::Array(items) => {
            for item in items {
                visit_references(item, visit);
            }
        }
        Expression::Paren(inner) | Expression::Not(inner) => visit_references(inner, visit),
        Expression::Binary { lhs, rhs, .. } => {
            visit_references(lhs, visit);
            visit_references(rhs, visit);
        }
        Expression::If(if_expression) => {
            for (condition, value) in &if_expression.branches {
                visit_references(condition, visit);
                visit_references(value, visit);
            }
            visit_references(&if_expression.else_value, visit);
        }
        Expression::Call(call) => {
            for argument in &call.arguments {
                visit_references(argument, visit);
            }
        }
        _ => {}
    }
}

/// Span-free text of a condition, so two spellings of one guard compare
/// equal.
fn canonical(expression: &Expression) -> String {
    let mut text = String::new();
    write_canonical(expression, &mut text);
    text
}

fn write_canonical(expression: &Expression, text: &mut String) {
    use std::fmt::Write as _;
    match expression {
        Expression::Bool(value) => {
            let _ = write!(text, "{value}");
        }
        Expression::Integer(value) => {
            let _ = write!(text, "{value}i");
        }
        Expression::Real(value) => {
            let _ = write!(text, "{:#x}r", value.to_bits());
        }
        Expression::Ref(reference) => write_reference(reference, text),
        Expression::Neg(reference) => {
            text.push_str("-(");
            write_reference(reference, text);
            text.push(')');
        }
        Expression::Size { array, dimension } => {
            text.push_str("size(");
            write_reference(array, text);
            text.push(',');
            write_canonical(dimension, text);
            text.push(')');
        }
        Expression::Call(call) => {
            let _ = write!(text, "{}(", call.function.lexeme());
            for argument in &call.arguments {
                write_canonical(argument, text);
                text.push(',');
            }
            text.push(')');
        }
        Expression::Paren(inner) => write_canonical(inner, text),
        Expression::Not(inner) => {
            text.push_str("not(");
            write_canonical(inner, text);
            text.push(')');
        }
        Expression::Binary { op, lhs, rhs } => {
            text.push('(');
            write_canonical(lhs, text);
            let _ = write!(text, " {op:?} ");
            write_canonical(rhs, text);
            text.push(')');
        }
        Expression::If(if_expression) => {
            text.push_str("if(");
            for (condition, value) in &if_expression.branches {
                write_canonical(condition, text);
                text.push('?');
                write_canonical(value, text);
                text.push(';');
            }
            write_canonical(&if_expression.else_value, text);
            text.push(')');
        }
        Expression::Array(items) => {
            text.push('{');
            for item in items {
                write_canonical(item, text);
                text.push(',');
            }
            text.push('}');
        }
    }
}

fn write_reference(reference: &Reference, text: &mut String) {
    let (prefix, parts): (&str, &[RefPart]) = match reference {
        Reference::Local(part) => ("", std::slice::from_ref(part)),
        Reference::State(parts) => ("self.", parts),
    };
    text.push_str(prefix);
    for (index, part) in parts.iter().enumerate() {
        if index > 0 {
            text.push('.');
        }
        text.push_str(part.name.lexeme());
        if !part.subscripts.is_empty() {
            text.push('[');
            for subscript in &part.subscripts {
                write_canonical(subscript, text);
                text.push(',');
            }
            text.push(']');
        }
    }
}
