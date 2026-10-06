//! Read-only queries over the checked expression arena.
//!
//! Queries use branded identities directly. They never recover variable
//! identity from display text and never need malformed-expression branches:
//! every operand was checked when its node entered the arena.

use crate::{
    CoordinateView, DaeView, ExprId, ExpressionOperation, ExpressionView, StateId, SubscriptView,
    VariableId,
};

/// Visit one expression DAG, including index and slice expressions.
///
/// Shared nodes are visited once. The root is visited before its operands.
pub fn for_each_expression<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    mut visit: impl FnMut(ExprId<'dae>, ExpressionView<'dae>),
) {
    walk_root(dae, root, &mut |expression, node| {
        visit(expression, node);
        true
    });
}

/// Visit one expression DAG while allowing a semantic owner to stop descent.
///
/// The visitor returns `true` to visit an expression's operands and `false`
/// when the expression is already an owned leaf for the query. Shared nodes
/// are visited once. This keeps dependency selection at the checked DAE
/// boundary without requiring consumers to reproduce the expression grammar.
/// The visited set is this thread's shared stamp table, so a call costs only
/// the nodes it reaches; a multi-root pass uses an [`ExpressionTraversal`].
pub fn for_each_expression_pruned<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    mut visit: impl FnMut(ExprId<'dae>, ExpressionView<'dae>) -> bool,
) {
    walk_root(dae, root, &mut visit);
}

/// One shared walker for every visitor type, so callers do not each compile a
/// copy of the traversal.
fn walk_root<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    visit: &mut dyn FnMut(ExprId<'dae>, ExpressionView<'dae>) -> bool,
) {
    with_shared_stamps(dae, &mut |stamps| {
        walk_pruned(dae, stamps, &mut vec![root], &mut |id, node| {
            Some(visit(id, node))
        });
    });
}

/// Walk `pending`, last first, marking each expression in `stamps`' open pass.
/// The visitor answers `Some(true)` to descend, `Some(false)` to prune, and
/// `None` to stop; the walk returns whether it stopped.
fn walk_pruned<'dae>(
    dae: DaeView<'dae>,
    stamps: &mut StampTable,
    pending: &mut Vec<ExprId<'dae>>,
    visit: &mut dyn FnMut(ExprId<'dae>, ExpressionView<'dae>) -> Option<bool>,
) -> bool {
    while let Some(expression) = pending.pop() {
        if !stamps.mark(expression) {
            continue;
        }
        let node = dae
            .expression(expression)
            .expect("a branded expression identity resolves in its owning DAE");
        match visit(expression, node) {
            Some(true) => push_children(dae, node.operation(), pending),
            Some(false) => {}
            None => return true,
        }
    }
    false
}

/// Generation-stamped visited marks: stamp `0` is never visited, and each pass
/// marks with a fresh generation, so opening a pass never clears the table
/// (except after the generation counter wraps).
#[derive(Debug, Default)]
struct StampTable {
    stamps: Vec<u32>,
    generation: u32,
}

impl StampTable {
    fn begin_pass(&mut self, len: usize) {
        if self.stamps.len() < len {
            self.stamps.resize(len, 0);
        }
        self.generation = self.generation.checked_add(1).unwrap_or_else(|| {
            self.stamps.fill(0);
            1
        });
    }

    /// Mark `expression` in the open pass; `false` when it already was.
    fn mark(&mut self, expression: ExprId<'_>) -> bool {
        let stamp = &mut self.stamps[expression.index() as usize];
        (*stamp != self.generation) && {
            *stamp = self.generation;
            true
        }
    }
}

thread_local! {
    /// The table single-root walks share; a walk nested in another walk's
    /// visitor finds it borrowed and uses a table of its own.
    static SHARED_STAMPS: std::cell::RefCell<StampTable> = std::cell::RefCell::default();
}

fn with_shared_stamps(dae: DaeView<'_>, walk: &mut dyn FnMut(&mut StampTable)) {
    SHARED_STAMPS.with(|shared| {
        let (mut borrowed, mut own);
        let table: &mut StampTable = if let Ok(table) = shared.try_borrow_mut() {
            borrowed = table;
            &mut borrowed
        } else {
            own = StampTable::default();
            &mut own
        };
        table.begin_pass(dae.expression_count());
        walk(table)
    })
}

/// Reusable workspace for pruned expression walks over one DAE.
///
/// The visited set is the whole expression arena, so allocating one per root
/// makes a multi-root query cost `roots * arena` before it looks at a single
/// operand. This workspace is allocated once and reused: each pass stamps
/// nodes with a fresh generation instead of clearing, and the pending stack is
/// kept across passes so a steady-state query allocates nothing at all.
///
/// [`visit_pruned`](Self::visit_pruned) takes all of a pass's roots together
/// and shares one visited set across them. A node reachable from several roots
/// is therefore visited once per pass rather than once per root, which is the
/// exact semantics an accumulating query wants: the visitor decides from the
/// node alone, so a second arrival could only repeat the first answer.
#[derive(Debug, Default)]
pub struct ExpressionTraversal<'dae> {
    stamps: StampTable,
    pending: Vec<ExprId<'dae>>,
}

impl<'dae> ExpressionTraversal<'dae> {
    /// An empty workspace that sizes itself on its first pass.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            stamps: StampTable {
                stamps: Vec::new(),
                generation: 0,
            },
            pending: Vec::new(),
        }
    }

    /// Visit every expression reachable from `roots`, pruned by the visitor.
    ///
    /// The visitor returns `true` to descend into an expression's operands and
    /// `false` when the expression is an owned leaf for the query. Roots are
    /// visited in the order given.
    pub fn visit_pruned(
        &mut self,
        dae: DaeView<'dae>,
        roots: impl IntoIterator<Item = ExprId<'dae>>,
        mut visit: impl FnMut(ExprId<'dae>, ExpressionView<'dae>) -> bool,
    ) {
        self.stamps.begin_pass(dae.expression_count());
        self.pending.clear();
        self.pending.extend(roots);
        self.pending.reverse();
        walk_pruned(dae, &mut self.stamps, &mut self.pending, &mut |id, node| {
            Some(visit(id, node))
        });
    }
}

/// True when any coordinate in `root` belongs to `variable`.
///
/// Current, derivative, `pre`, and role-specific coordinate forms all retain
/// the same declaration identity and therefore all count as uses.
pub fn expr_contains_var<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    variable: VariableId<'dae>,
) -> bool {
    expression_any(dae, root, |node| {
        node.variable_coordinate() == Some(variable)
    })
}

/// True when `root` itself denotes `variable`, optionally through indexing.
///
/// This intentionally does not search arithmetic operands. Use
/// [`expr_contains_var`] for a recursive occurrence query.
pub fn expr_refers_to_var<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    variable: VariableId<'dae>,
) -> bool {
    let node = dae
        .expression(root)
        .expect("a branded expression identity resolves in its owning DAE");
    if node.variable_coordinate() == Some(variable) {
        return true;
    }
    match node.operation() {
        ExpressionOperation::Index { base, .. } => expr_refers_to_var(dae, base, variable),
        _ => false,
    }
}

/// True when `root` contains the derivative coordinate of `state`.
pub fn expr_contains_der_of<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    state: StateId<'dae>,
) -> bool {
    expression_any(dae, root, |node| {
        matches!(
            node.operation(),
            ExpressionOperation::Coordinate(CoordinateView::Derivative(candidate))
                if candidate == state
        )
    })
}

/// True when `root` contains a derivative accepted by `matches`.
pub fn expr_contains_der_of_any<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    mut matches: impl FnMut(StateId<'dae>) -> bool,
) -> bool {
    expression_any(dae, root, |node| match node.operation() {
        ExpressionOperation::Coordinate(CoordinateView::Derivative(state)) => matches(state),
        _ => false,
    })
}

fn expression_any<'dae>(
    dae: DaeView<'dae>,
    root: ExprId<'dae>,
    mut predicate: impl FnMut(ExpressionView<'dae>) -> bool,
) -> bool {
    let mut found = false;
    with_shared_stamps(dae, &mut |stamps| {
        found = walk_pruned(dae, stamps, &mut vec![root], &mut |_, node| {
            (!predicate(node)).then_some(true)
        });
    });
    found
}

fn push_children<'dae>(
    dae: DaeView<'dae>,
    operation: ExpressionOperation<'dae>,
    pending: &mut Vec<ExprId<'dae>>,
) {
    match operation {
        ExpressionOperation::Literal(_)
        | ExpressionOperation::Coordinate(_)
        | ExpressionOperation::FunctionFoldParameter { .. } => {}
        ExpressionOperation::Range(range) => {
            pending.push(range.stop().expression());
            if let Some(step) = range.explicit_step() {
                pending.push(step.expression());
            }
            pending.push(range.start().expression());
        }
        ExpressionOperation::Unary { operand, .. } => pending.push(operand),
        ExpressionOperation::ClockTransfer { source, .. } => pending.push(source),
        ExpressionOperation::Binary { lhs, rhs, .. } => {
            pending.push(rhs);
            pending.push(lhs);
        }
        ExpressionOperation::Conditional(operands)
        | ExpressionOperation::Array(operands)
        | ExpressionOperation::Record(operands)
        | ExpressionOperation::Builtin {
            arguments: operands,
            ..
        }
        | ExpressionOperation::Call {
            arguments: operands,
            ..
        } => pending.extend(operands.iter()),
        ExpressionOperation::StringConversion { value, format, .. } => {
            pending.push(value);
            match format {
                crate::StringConversionFormatView::Options {
                    minimum_length,
                    left_justified,
                    significant_digits,
                } => {
                    pending.extend(minimum_length);
                    pending.extend(left_justified);
                    pending.extend(significant_digits);
                }
                crate::StringConversionFormatView::Format { value } => pending.push(value),
            }
        }
        ExpressionOperation::Comprehension { body, .. } => pending.push(body),
        ExpressionOperation::Field { base, .. } => pending.push(base),
        ExpressionOperation::FunctionValue { definition, .. } => pending.push(definition.rhs()),
        ExpressionOperation::FunctionFoldOutput { fold, .. } => {
            let fold = dae
                .function_fold(fold)
                .expect("checked function fold identity resolves");
            pending.extend(fold.initial_values().rhs_iter());
            pending.extend(fold.update_values().rhs_iter());
            pending.extend(fold.continuation());
        }
        ExpressionOperation::Index { base, subscripts } => {
            push_subscripts(subscripts, pending);
            pending.push(base);
        }
        ExpressionOperation::ArrayUpdate {
            base,
            value,
            subscripts,
        } => {
            push_subscripts(subscripts, pending);
            pending.extend([base, value]);
        }
    }
}

fn push_subscripts<'dae>(subscripts: crate::SubscriptsView<'dae>, pending: &mut Vec<ExprId<'dae>>) {
    for subscript in subscripts.iter() {
        match subscript {
            SubscriptView::Index { expression, .. } | SubscriptView::Slice { expression, .. } => {
                pending.push(expression)
            }
            SubscriptView::Whole { .. } => {}
        }
    }
}
