//! Opt-in native debug attribution; absent from release compiler artifacts.
pub(super) mod guard;

use std::sync::OnceLock;
use std::sync::atomic::{AtomicUsize, Ordering};

use rumoca_ir_dae as dae;

fn enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("RUMOCA_NATIVE_SOURCE_PROFILE").is_some())
}

#[track_caller]
pub(super) fn entry(expression: u32, scalar: usize) {
    static COUNT: AtomicUsize = AtomicUsize::new(0);
    if !enabled() {
        return;
    }
    let ordinal = COUNT.fetch_add(1, Ordering::Relaxed);
    if ordinal < 8 || ordinal.is_multiple_of(1_000) {
        eprintln!(
            "SCALAR_PROJECTION_ENTRY ordinal={ordinal} caller={} expression={expression} scalar={scalar}",
            std::panic::Location::caller()
        );
    }
}

#[derive(Clone, Copy)]
pub(super) enum Membership {
    Dense,
    Field,
    Expression,
    Scalar,
    ScopeBudget,
    Gap,
    WordBudget,
    Cutover,
}

pub(super) fn membership(kind: Membership, key: Option<&super::ScalarExpressionDependency>) {
    static COUNTS: [AtomicUsize; 8] = [const { AtomicUsize::new(0) }; 8];
    static TOTAL: AtomicUsize = AtomicUsize::new(0);
    static EXAMPLES: AtomicUsize = AtomicUsize::new(0);
    if !enabled() {
        return;
    }
    let kind = kind as usize;
    COUNTS[kind].fetch_add(1, Ordering::Relaxed);
    let ordinal = TOTAL.fetch_add(1, Ordering::Relaxed);
    if kind > 0 && EXAMPLES.fetch_add(1, Ordering::Relaxed) < 32 {
        eprintln!("SCALAR_MEMBERSHIP_EXAMPLE kind={kind} key={key:?}");
    }
    if ordinal.is_multiple_of(100_000) {
        let counts = COUNTS.each_ref().map(|count| count.load(Ordering::Relaxed));
        eprintln!(
            "SCALAR_MEMBERSHIP_COUNTS order=dense,field,expression,scalar,scope_budget,gap,word_budget,cutover total={ordinal} counts={counts:?}"
        );
    }
}

pub(super) fn fallback_key(key: &super::ScalarExpressionDependency) {
    static EXAMPLES: AtomicUsize = AtomicUsize::new(0);
    if enabled() && EXAMPLES.fetch_add(1, Ordering::Relaxed) < 32 {
        eprintln!("SCALAR_MEMBERSHIP_FALLBACK_KEY key={key:?}");
    }
}

pub(super) fn clear(generation: u64, scopes: usize, words: usize, reclaimed: bool) {
    static COUNT: AtomicUsize = AtomicUsize::new(0);
    if !enabled() {
        return;
    }
    let ordinal = COUNT.fetch_add(1, Ordering::Relaxed);
    if ordinal < 16 || ordinal.is_multiple_of(1_000) {
        eprintln!(
            "SCALAR_MEMBERSHIP_CLEAR ordinal={ordinal} generation={generation} scoped_pages={scopes} words={words} reclaimed={reclaimed}"
        );
    }
}

pub(super) fn function(
    event: &str,
    definition: dae::FunctionView<'_>,
    dependency: &super::FunctionResultDependency,
    state: (bool, Option<u32>, usize),
) {
    static COUNT: AtomicUsize = AtomicUsize::new(0);
    if !enabled() {
        return;
    }
    let ordinal = COUNT.fetch_add(1, Ordering::Relaxed);
    if ordinal < 32 || ordinal.is_multiple_of(1_000) {
        let (cache_hit, validation_root, depth) = state;
        eprintln!(
            "SCALAR_FUNCTION ordinal={ordinal} event={event} name={} result={dependency:?} cache_hit={cache_hit} validation_root={validation_root:?} depth={depth}",
            definition.name()
        );
    }
}

pub(super) fn fold<'dae>(view: dae::DaeView<'dae>, node: &super::fold_graph::FoldNode<'dae>) {
    static COUNT: AtomicUsize = AtomicUsize::new(0);
    if !enabled() {
        return;
    }
    let ordinal = COUNT.fetch_add(1, Ordering::Relaxed);
    if ordinal < 32 || ordinal.is_multiple_of(1_000) {
        let fold = view.function_fold(node.fold).unwrap();
        let domain = view.domain(fold.domain()).unwrap();
        eprintln!(
            "SCALAR_FOLD ordinal={ordinal} function={} name={} fold={} carried={} scalar={} update={} parent_depth={} domain_points={:?}",
            node.fold.function().index(),
            view.function(node.fold.function()).unwrap().name(),
            node.fold.ordinal(),
            node.carried,
            node.scalar,
            node.update.index(),
            node.parent.len(),
            domain.structured().scalar_count()
        );
    }
}

pub(super) fn point(fold: dae::FunctionFoldId<'_>) {
    static COUNT: AtomicUsize = AtomicUsize::new(0);
    if enabled() {
        let ordinal = COUNT.fetch_add(1, Ordering::Relaxed);
        if ordinal.is_multiple_of(1_000_000) {
            eprintln!(
                "SCALAR_FOLD_POINT total={ordinal} function={} fold={}",
                fold.function().index(),
                fold.ordinal()
            );
        }
    }
}

pub(super) fn graph(
    node: &super::fold_graph::FoldNode<'_>,
    cursor: usize,
    nodes: usize,
    edges: usize,
) {
    static COUNT: AtomicUsize = AtomicUsize::new(0);
    if !enabled() {
        return;
    }
    let ordinal = COUNT.fetch_add(1, Ordering::Relaxed);
    if ordinal < 32 || ordinal.is_multiple_of(100) {
        eprintln!(
            "SCALAR_FOLD_GRAPH ordinal={ordinal} function={} fold={} carried={} scalar={} cursor={cursor} nodes={nodes} edges={edges}",
            node.fold.function().index(),
            node.fold.ordinal(),
            node.carried,
            node.scalar
        );
    }
}

pub(super) fn validation_memo(root: u32, hit: bool, completed: usize) {
    static HITS: AtomicUsize = AtomicUsize::new(0);
    static MISSES: AtomicUsize = AtomicUsize::new(0);
    if !enabled() {
        return;
    }
    let counter = if hit { &HITS } else { &MISSES };
    let ordinal = counter.fetch_add(1, Ordering::Relaxed);
    if ordinal < 4 || ordinal.is_multiple_of(100_000) {
        eprintln!(
            "SCALAR_VALIDATION_MEMO function={root} hit={hit} ordinal={ordinal} completed={completed}"
        );
    }
}

/// A positive owner certificate, not a timing inference or source-name match.
pub(super) fn indexed_write<'dae>(
    node: &super::fold_graph::FoldNode<'dae>,
    value: dae::ExprId<'dae>,
    selected: Option<usize>,
    points: usize,
    extent: u32,
) {
    static COUNT: AtomicUsize = AtomicUsize::new(0);
    if !enabled() {
        return;
    }
    let ordinal = COUNT.fetch_add(1, Ordering::Relaxed);
    if ordinal < 32 {
        eprintln!(
            "SCALAR_INDEXED_WRITE_CERTIFICATE ordinal={ordinal} function={} fold={} carried={} scalar={} update={} value={} parent_depth={} domain_points={points} extent={extent} selected_ordinal={selected:?} proof=owned_constant_binder",
            node.fold.function().index(),
            node.fold.ordinal(),
            node.carried,
            node.scalar,
            node.update.index(),
            value.index(),
            node.parent.len()
        );
    }
}
