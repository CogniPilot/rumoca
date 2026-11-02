//! Fault-visible state.
//!
//! When Recalibrate or DoStep faults, the GALEC interpreter keeps every block
//! write and raised signal evaluated before the fault, and the block stays
//! started. Top-level statements store as they go, so the Solve evaluator
//! keeps those too. A value-only region (an `if` or `for` body, a lazy
//! if-expression arm, a user function) publishes its effects only when it
//! completes, so an effect evaluated inside a region before a fault in that
//! same region would be lost. Such code is refused; a region that cannot
//! fault, or whose faults all precede its effects, is exact.

use rumoca_ir_galec::ast::IfStatement;

use super::*;

/// What evaluating some code can do.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(super) struct Summary {
    /// It can fault.
    faults: bool,
    /// It can write block state or raise a signal.
    effects: bool,
    /// Inside some region an effect can precede a fault, which would then
    /// lose a fault-visible effect.
    lost: bool,
}

const NOTHING: Summary = Summary {
    faults: false,
    effects: false,
    lost: false,
};
const FAULT: Summary = Summary {
    faults: true,
    ..NOTHING
};
const EFFECT: Summary = Summary {
    effects: true,
    ..NOTHING
};

impl Summary {
    /// Faults and effects in either order.
    pub(super) const fn anything() -> Self {
        Self {
            faults: true,
            effects: true,
            lost: true,
        }
    }

    /// Exactly one of the alternatives runs.
    const fn or(self, other: Self) -> Self {
        Self {
            faults: self.faults || other.faults,
            effects: self.effects || other.effects,
            lost: self.lost || other.lost,
        }
    }

    /// A loop body: every effect of one iteration precedes the faults of the
    /// next.
    const fn repeated(self) -> Self {
        Self {
            lost: self.lost || (self.effects && self.faults),
            ..self
        }
    }
}

/// Summarizing context: loop iterators with their static `[low, high]`
/// ranges, and whether the code runs inside a region.
#[derive(Default)]
struct Ctx {
    loops: Vec<(String, i64, i64)>,
    /// Static ranges of Integer locals (see `local_ranges`).
    locals: BTreeMap<usize, (i128, i128)>,
    region: bool,
    /// An `if` here lowers to one region per statement (a method's top
    /// level, and the bodies of such an `if`).
    split: bool,
}

impl Ctx {
    /// `first`, then `next`; inside a region an effect before a fault is
    /// lost.
    const fn then(&self, first: Summary, next: Summary) -> Summary {
        Summary {
            faults: first.faults || next.faults,
            effects: first.effects || next.effects,
            lost: first.lost || next.lost || (self.region && first.effects && next.faults),
        }
    }

    fn range(&self, name: &str) -> Option<(i64, i64)> {
        self.loops
            .iter()
            .rev()
            .find(|(bound, ..)| bound == name)
            .map(|&(_, low, high)| (low, high))
    }

    /// Summarize `body` as a region; an `if` inside lowers whole.
    fn region(&mut self, body: impl FnOnce(&mut Self) -> Summary) -> Summary {
        let outer = (self.region, self.split);
        (self.region, self.split) = (true, false);
        let summary = body(self);
        (self.region, self.split) = outer;
        summary
    }

    /// Summarize `body` where `condition` holds.
    fn guarded<T>(
        &mut self,
        scope: &MethodScope<'_>,
        condition: &Expression,
        body: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let depth = self.loops.len();
        narrow(scope, self, condition, true);
        let result = body(self);
        self.loops.truncate(depth);
        result
    }

    /// Summarize one statement of a split `if` as its own region.
    fn statement_region(&mut self, body: impl FnOnce(&mut Self) -> Summary) -> Summary {
        let outer = std::mem::replace(&mut self.region, true);
        let summary = body(self);
        self.region = outer;
        summary
    }
}

/// Refuse a method whose regions could lose a fault-visible effect.
pub(super) fn require_fault_visible_state(
    scope: &MethodScope<'_>,
    statements: &[Spanned<Statement>],
) -> Result<(), AlgorithmBlockLowerError> {
    let mut ctx = Ctx {
        split: true,
        locals: local_ranges(scope, statements),
        ..Ctx::default()
    };
    for statement in statements {
        if statement_summary(scope, &mut ctx, &statement.node).lost {
            return Err(unsupported(
                "statement that can fault after a block-visible effect inside one region",
                statement.span,
            ));
        }
    }
    Ok(())
}

/// What a call contributes. A function's top level stores its status as it
/// goes and a fault carries every bit raised before it to the caller
/// (EL007), so only its `if` and `for` bodies are regions; its top-level `if`
/// lowers whole, not split.
pub(super) fn function_summary(
    scope: &MethodScope<'_>,
    statements: &[Spanned<Statement>],
) -> Summary {
    let mut ctx = Ctx {
        locals: local_ranges(scope, statements),
        ..Ctx::default()
    };
    statements_summary(scope, &mut ctx, statements)
}

fn statements_summary(
    scope: &MethodScope<'_>,
    ctx: &mut Ctx,
    statements: &[Spanned<Statement>],
) -> Summary {
    let mut summary = NOTHING;
    for statement in statements {
        let next = statement_summary(scope, ctx, &statement.node);
        summary = ctx.then(summary, next);
    }
    summary
}

fn statement_summary(scope: &MethodScope<'_>, ctx: &mut Ctx, statement: &Statement) -> Summary {
    match statement {
        Statement::Assignment { target, value } => {
            let value = expression_summary(scope, ctx, value);
            let store = store_summary(scope, ctx, target);
            ctx.then(value, store)
        }
        Statement::MultiAssignment { targets, call } => {
            let mut summary = call_summary(scope, ctx, call);
            for target in targets {
                let store = store_summary(scope, ctx, target);
                summary = ctx.then(summary, store);
            }
            summary
        }
        Statement::Call(call) => call_summary(scope, ctx, call),
        Statement::If(if_statement) => if_statement_summary(scope, ctx, if_statement),
        Statement::For(for_loop) => {
            let (Some(iterator), Some((start, stop, step))) =
                (&for_loop.iterator, loop_range(scope, for_loop))
            else {
                return FAULT;
            };
            let (low, high) = if step > 0 {
                (start, stop)
            } else {
                (stop, start)
            };
            ctx.loops.push((iterator.lexeme().to_owned(), low, high));
            let body = scope.with_loop(iterator.lexeme(), |_| {
                ctx.region(|ctx| statements_summary(scope, ctx, &for_loop.body))
            });
            ctx.loops.pop();
            body.repeated()
        }
        // Saturation and raising never fault; both are visible.
        Statement::Limit(_) | Statement::Signal(_) => EFFECT,
    }
}

/// The first condition is evaluated before the region; later conditions and
/// every body belong to it.
fn if_statement_summary(
    scope: &MethodScope<'_>,
    ctx: &mut Ctx,
    if_statement: &IfStatement,
) -> Summary {
    if ctx.split {
        return split_if_summary(scope, ctx, if_statement);
    }
    // Region mode: the first condition runs before the region; each later
    // condition runs on the false path of those before it, so its effects
    // precede everything after it (false-path order).
    let depth = ctx.loops.len();
    let mut branches = Vec::new();
    for branch in &if_statement.branches {
        let Condition::Expression(condition) = &branch.condition else {
            ctx.loops.truncate(depth);
            return FAULT.or(EFFECT).repeated();
        };
        let condition_summary = if branches.is_empty() {
            expression_summary(scope, ctx, condition)
        } else {
            ctx.region(|ctx| expression_summary(scope, ctx, condition))
        };
        let body = ctx.guarded(scope, condition, |ctx| {
            ctx.region(|ctx| statements_summary(scope, ctx, &branch.body))
        });
        branches.push((condition_summary, body));
        narrow(scope, ctx, condition, false);
    }
    let mut tail = match &if_statement.else_body {
        Some(body) => ctx.region(|ctx| statements_summary(scope, ctx, body)),
        None => NOTHING,
    };
    ctx.loops.truncate(depth);
    let mut first = NOTHING;
    for (index, (condition, body)) in branches.into_iter().enumerate().rev() {
        let arms = body.or(tail);
        if index == 0 {
            first = condition;
            tail = arms;
        } else {
            tail = in_region(condition, arms);
        }
    }
    ctx.then(first, tail)
}

/// Push the iterator ranges implied where `condition` evaluates to `holds`:
/// a comparison between a loop iterator and an Integer constant, its
/// negation, or (when it holds) a conjunction of such. This is the
/// selection a tensor slice or concatenation lowers to; anything else
/// implies nothing, which is always sound.
fn narrow(scope: &MethodScope<'_>, ctx: &mut Ctx, condition: &Expression, holds: bool) {
    let (op, lhs, rhs) = match condition {
        Expression::Paren(inner) => return narrow(scope, ctx, inner, holds),
        Expression::Not(inner) => return narrow(scope, ctx, inner, !holds),
        Expression::Binary { op, lhs, rhs } => (*op, lhs, rhs),
        _ => return,
    };
    if op == BinaryOp::And {
        if holds {
            narrow(scope, ctx, lhs, true);
            narrow(scope, ctx, rhs, true);
        }
        return;
    }
    let iterator = |expression: &Expression| match expression {
        Expression::Ref(Reference::Local(part)) if part.subscripts.is_empty() => {
            let name = part.name.lexeme();
            ctx.range(name).map(|range| (name.to_owned(), range))
        }
        _ => None,
    };
    // `bound op iterator` is `iterator op' bound` with the comparison
    // mirrored; where it is false, the comparison is negated.
    let (name, (low, high), op, bound) = match (iterator(lhs), static_int(scope, rhs)) {
        (Some((name, range)), Some(bound)) => (name, range, op, bound),
        _ => match (static_int(scope, lhs), iterator(rhs)) {
            (Some(bound), Some((name, range))) => (name, range, mirrored(op), bound),
            _ => return,
        },
    };
    let op = if holds { Some(op) } else { negated(op) };
    let (low, high) = match op {
        Some(BinaryOp::Ge) => (low.max(bound), high),
        Some(BinaryOp::Gt) => (low.max(bound.saturating_add(1)), high),
        Some(BinaryOp::Le) => (low, high.min(bound)),
        Some(BinaryOp::Lt) => (low, high.min(bound.saturating_sub(1))),
        Some(BinaryOp::Eq) => (low.max(bound), high.min(bound)),
        _ => return,
    };
    ctx.loops.push((name, low, high));
}

const fn mirrored(op: BinaryOp) -> BinaryOp {
    match op {
        BinaryOp::Lt => BinaryOp::Gt,
        BinaryOp::Gt => BinaryOp::Lt,
        BinaryOp::Le => BinaryOp::Ge,
        BinaryOp::Ge => BinaryOp::Le,
        other => other,
    }
}

/// The comparison that holds where `op` is false; `!=` gives no interval.
const fn negated(op: BinaryOp) -> Option<BinaryOp> {
    match op {
        BinaryOp::Lt => Some(BinaryOp::Ge),
        BinaryOp::Ge => Some(BinaryOp::Lt),
        BinaryOp::Le => Some(BinaryOp::Gt),
        BinaryOp::Gt => Some(BinaryOp::Le),
        BinaryOp::Ne => Some(BinaryOp::Eq),
        _ => None,
    }
}

/// `first`, then `next`, inside one region.
const fn in_region(first: Summary, next: Summary) -> Summary {
    Summary {
        faults: first.faults || next.faults,
        effects: first.effects || next.effects,
        lost: first.lost || next.lost || (first.effects && next.faults),
    }
}

/// A split `if`: every condition and every body statement is its own region,
/// published before the next one runs.
fn split_if_summary(scope: &MethodScope<'_>, ctx: &mut Ctx, if_statement: &IfStatement) -> Summary {
    let depth = ctx.loops.len();
    let mut parts = Vec::new();
    for branch in &if_statement.branches {
        let guard = match &branch.condition {
            Condition::Expression(condition) => {
                parts.push(ctx.statement_region(|ctx| expression_summary(scope, ctx, condition)));
                Some(condition)
            }
            Condition::SignalCheck(_) => {
                parts.push(FAULT);
                None
            }
        };
        for statement in &branch.body {
            let summary = |ctx: &mut Ctx| {
                ctx.statement_region(|ctx| statement_summary(scope, ctx, &statement.node))
            };
            parts.push(match guard {
                Some(condition) => ctx.guarded(scope, condition, summary),
                None => summary(ctx),
            });
        }
        if let Some(condition) = guard {
            narrow(scope, ctx, condition, false);
        }
    }
    for statement in if_statement.else_body.iter().flatten() {
        parts.push(ctx.statement_region(|ctx| statement_summary(scope, ctx, &statement.node)));
    }
    ctx.loops.truncate(depth);
    parts.into_iter().fold(NOTHING, |summary, part| Summary {
        faults: summary.faults || part.faults,
        effects: summary.effects || part.effects,
        lost: summary.lost || part.lost,
    })
}

/// Storing to `target`: its subscripts may fault, then the store is visible
/// when the target is block state.
fn store_summary(scope: &MethodScope<'_>, ctx: &Ctx, target: &Reference) -> Summary {
    let Ok((id, subscripts)) = scope.resolve_place(target) else {
        return FAULT;
    };
    let subscripts = subscripts_summary(scope, ctx, id, subscripts);
    if matches!(id, VarId::Block(_)) {
        ctx.then(subscripts, EFFECT)
    } else {
        subscripts
    }
}

/// Element access faults unless every index is statically in range: a
/// constant or a loop iterator whose static range fits the axis.
fn subscripts_summary(
    scope: &MethodScope<'_>,
    ctx: &Ctx,
    id: VarId,
    subscripts: &[Expression],
) -> Summary {
    if subscripts.is_empty() || matches!(id, VarId::Loop(_)) {
        return NOTHING;
    }
    let dims = &scope.variable(id).dims;
    let mut summary = NOTHING;
    for (subscript, &extent) in subscripts.iter().zip(dims) {
        let index = expression_summary(scope, ctx, subscript);
        let in_range = index_range(scope, ctx, subscript)
            .is_some_and(|(low, high)| low >= 1 && high <= i64::from(extent));
        summary = ctx.then(summary, index);
        if !in_range {
            summary = ctx.then(summary, FAULT);
        }
    }
    summary
}

/// The static range of an Integer expression: constants, loop iterators and
/// their sums, differences and products.
fn int_range(scope: &MethodScope<'_>, ctx: &Ctx, expression: &Expression) -> Option<(i128, i128)> {
    if let Some(value) = static_int(scope, expression) {
        return Some((value.into(), value.into()));
    }
    match expression {
        Expression::Ref(Reference::Local(part)) if part.subscripts.is_empty() => {
            if let Some((low, high)) = ctx.range(part.name.lexeme()) {
                return Some((low.into(), high.into()));
            }
            match scope.resolve(&Reference::Local(part.clone())).ok()? {
                VarId::Local(index) => ctx.locals.get(&index).copied(),
                _ => None,
            }
        }
        Expression::Paren(inner) => int_range(scope, ctx, inner),
        Expression::Neg(reference) => {
            let (low, high) = int_range(scope, ctx, &Expression::Ref(reference.clone()))?;
            Some((-high, -low))
        }
        Expression::If(if_expression) => if_expression
            .branches
            .iter()
            .map(|(_, value)| value)
            .chain(std::iter::once(if_expression.else_value.as_ref()))
            .try_fold(None, |union: Option<(i128, i128)>, value| {
                let (low, high) = int_range(scope, ctx, value)?;
                Some(Some(
                    union.map_or((low, high), |(a, b)| (a.min(low), b.max(high))),
                ))
            })?,
        Expression::Binary { op, lhs, rhs } => {
            let (a, b) = int_range(scope, ctx, lhs)?;
            let (c, d) = int_range(scope, ctx, rhs)?;
            match op {
                BinaryOp::Add => Some((a + c, b + d)),
                BinaryOp::Sub => Some((a - d, b - c)),
                BinaryOp::Mul => {
                    let products = [a * c, a * d, b * c, b * d];
                    Some((*products.iter().min()?, *products.iter().max()?))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn index_range(scope: &MethodScope<'_>, ctx: &Ctx, index: &Expression) -> Option<(i64, i64)> {
    let (low, high) = int_range(scope, ctx, index)?;
    Some((i64::try_from(low).ok()?, i64::try_from(high).ok()?))
}

fn call_summary(scope: &MethodScope<'_>, ctx: &Ctx, call: &FunctionCall) -> Summary {
    let mut summary = NOTHING;
    for argument in &call.arguments {
        let argument = expression_summary(scope, ctx, argument);
        summary = ctx.then(summary, argument);
    }
    let call = if scope.scope.functions.is_user(call) {
        // A callee's status raised before its own fault travels with the
        // fault to the bound frame, through any region (EL007); only
        // bits a region already holds unpublished can be lost, which `then`
        // decides from what precedes the call.
        scope.scope.functions.summary(call)
    } else {
        match Builtin::of(call) {
            Some(Builtin::IntegerDivision(_)) => FAULT,
            Some(Builtin::ToInteger) => EFFECT,
            Some(_) => NOTHING,
            None => FAULT.or(EFFECT),
        }
    };
    ctx.then(summary, call)
}

fn expression_summary(scope: &MethodScope<'_>, ctx: &Ctx, expression: &Expression) -> Summary {
    match expression {
        Expression::Bool(_)
        | Expression::Integer(_)
        | Expression::Real(_)
        | Expression::Size { .. } => NOTHING,
        Expression::Ref(reference) => read_summary(scope, ctx, reference),
        Expression::Neg(reference) => {
            let read = read_summary(scope, ctx, reference);
            if may_overflow(scope, expression) {
                ctx.then(read, FAULT)
            } else {
                read
            }
        }
        Expression::Paren(inner) | Expression::Not(inner) => expression_summary(scope, ctx, inner),
        Expression::Binary { op, lhs, rhs } => {
            let lhs_summary = expression_summary(scope, ctx, lhs);
            let rhs_summary = expression_summary(scope, ctx, rhs);
            let operands = ctx.then(lhs_summary, rhs_summary);
            ctx.then(operands, operation_summary(scope, ctx, *op, lhs, rhs))
        }
        Expression::If(if_expression) => if_expression_summary(scope, ctx, if_expression),
        Expression::Call(call) => call_summary(scope, ctx, call),
        Expression::Array(items) => {
            let mut summary = NOTHING;
            for item in items {
                let item = expression_summary(scope, ctx, item);
                summary = ctx.then(summary, item);
            }
            summary
        }
    }
}

/// A Real comparison raises NAN; Integer arithmetic faults on overflow
/// unless its static range lies in the domain.
fn operation_summary(
    scope: &MethodScope<'_>,
    ctx: &Ctx,
    op: BinaryOp,
    lhs: &Expression,
    rhs: &Expression,
) -> Summary {
    let element = |expression: &Expression| {
        expression_type(scope, expression).map(|value_type| value_type.element_type())
    };
    let real = matches!(element(lhs), Some(SolveScalarType::Real { .. }));
    let integer = |expression| matches!(element(expression), Some(SolveScalarType::Integer(_)));
    match op {
        BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge | BinaryOp::Eq | BinaryOp::Ne
            if real =>
        {
            EFFECT
        }
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul
            if (integer(lhs) || integer(rhs)) && !in_domain(scope, ctx, op, lhs, rhs) =>
        {
            FAULT
        }
        BinaryOp::Div | BinaryOp::Pow if integer(lhs) || integer(rhs) => FAULT,
        _ => NOTHING,
    }
}

fn in_domain(
    scope: &MethodScope<'_>,
    ctx: &Ctx,
    op: BinaryOp,
    lhs: &Expression,
    rhs: &Expression,
) -> bool {
    let domain = scope.scope.arithmetic.integer_domain();
    let expression = Expression::Binary {
        op,
        lhs: Box::new(lhs.clone()),
        rhs: Box::new(rhs.clone()),
    };
    int_range(scope, ctx, &expression).is_some_and(|(low, high)| {
        low >= domain.minimum().into() && high <= domain.maximum().into()
    })
}

fn read_summary(scope: &MethodScope<'_>, ctx: &Ctx, reference: &Reference) -> Summary {
    match scope.resolve_place(reference) {
        Ok((id, subscripts)) => subscripts_summary(scope, ctx, id, subscripts),
        Err(_) => FAULT,
    }
}

/// The first condition is evaluated eagerly; with lazy arms every other part
/// is a region (an eager `select` has neither faults nor effects in its arms).
fn if_expression_summary(
    scope: &MethodScope<'_>,
    ctx: &Ctx,
    if_expression: &IfExpression,
) -> Summary {
    let Some(((condition, value), rest)) = if_expression.branches.split_first() else {
        return expression_summary(scope, ctx, &if_expression.else_value);
    };
    let first = expression_summary(scope, ctx, condition);
    let mut region = Ctx {
        loops: ctx.loops.clone(),
        locals: ctx.locals.clone(),
        region: true,
        split: false,
    };
    // Later conditions run in order on the false path (see
    // `if_statement_summary`).
    let head = region.guarded(scope, condition, |region| {
        expression_summary(scope, region, value)
    });
    narrow(scope, &mut region, condition, false);
    let mut branches = Vec::new();
    for (guard, value) in rest {
        let condition = expression_summary(scope, &region, guard);
        let value = region.guarded(scope, guard, |region| {
            expression_summary(scope, region, value)
        });
        branches.push((condition, value));
        narrow(scope, &mut region, guard, false);
    }
    let mut tail = expression_summary(scope, &region, &if_expression.else_value);
    for (condition, value) in branches.into_iter().rev() {
        tail = in_region(condition, value.or(tail));
    }
    let arms = head.or(tail);
    ctx.then(first, arms)
}

/// Flow-insensitive static ranges of scalar Integer locals: the union of the
/// ranges of every whole assignment, iterated to a fixpoint. A local with an
/// assignment of unknown range, or whose range keeps growing (it depends on
/// itself), has none. Sound because no read precedes an assignment (the
/// definedness proof).
fn local_ranges(
    scope: &MethodScope<'_>,
    statements: &[Spanned<Statement>],
) -> BTreeMap<usize, (i128, i128)> {
    let mut ranges = BTreeMap::new();
    let mut unknown = BTreeSet::new();
    for _ in 0..4 {
        let mut ctx = Ctx {
            locals: ranges.clone(),
            ..Ctx::default()
        };
        let mut next = BTreeMap::new();
        collect_ranges(scope, &mut ctx, statements, &mut next, &mut unknown);
        for index in &unknown {
            next.remove(index);
        }
        if next == ranges {
            return ranges;
        }
        ranges = next;
    }
    BTreeMap::new()
}

fn collect_ranges(
    scope: &MethodScope<'_>,
    ctx: &mut Ctx,
    statements: &[Spanned<Statement>],
    next: &mut BTreeMap<usize, (i128, i128)>,
    unknown: &mut BTreeSet<usize>,
) {
    for statement in statements {
        match &statement.node {
            Statement::Assignment { target, value } => {
                let Ok((VarId::Local(index), [])) = scope.resolve_place(target) else {
                    continue;
                };
                let integer = matches!(
                    scope
                        .variable(VarId::Local(index))
                        .value_type
                        .element_type(),
                    SolveScalarType::Integer(_)
                );
                match int_range(scope, ctx, value).filter(|_| integer) {
                    Some((low, high)) => {
                        let entry = next.entry(index).or_insert((low, high));
                        *entry = (entry.0.min(low), entry.1.max(high));
                    }
                    None => {
                        unknown.insert(index);
                    }
                }
            }
            // Saturation moves a value into its declared range, outside the
            // assigned one (regression: `limit k` after `k := 1`).
            Statement::Limit(targets) => {
                unknown.extend(targets.iter().filter_map(|target| match target {
                    LimitTarget::Reference(reference) => match scope.resolve(reference) {
                        Ok(VarId::Local(index)) => Some(index),
                        _ => None,
                    },
                    LimitTarget::SelfState => None,
                }));
            }
            Statement::MultiAssignment { targets, .. } => {
                unknown.extend(targets.iter().filter_map(
                    |target| match scope.resolve_place(target) {
                        Ok((VarId::Local(index), _)) => Some(index),
                        _ => None,
                    },
                ));
            }
            Statement::If(if_statement) => {
                for branch in &if_statement.branches {
                    collect_ranges(scope, ctx, &branch.body, next, unknown);
                }
                if let Some(body) = &if_statement.else_body {
                    collect_ranges(scope, ctx, body, next, unknown);
                }
            }
            Statement::For(for_loop) => {
                let (Some(iterator), Some((start, stop, step))) =
                    (&for_loop.iterator, loop_range(scope, for_loop))
                else {
                    continue;
                };
                let (low, high) = if step > 0 {
                    (start, stop)
                } else {
                    (stop, start)
                };
                ctx.loops.push((iterator.lexeme().to_owned(), low, high));
                scope.with_loop(iterator.lexeme(), |_| {
                    collect_ranges(scope, ctx, &for_loop.body, next, unknown);
                });
                ctx.loops.pop();
            }
            _ => {}
        }
    }
}
