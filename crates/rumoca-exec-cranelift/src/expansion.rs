//! Opt-in accounting of native lowering on the calling thread.
//!
//! This is diagnostic evidence, never a semantic plan or a code-generation
//! decision. Source programs are retained in their canonical IR; entry indices
//! identify emission occurrences within this report, not persistent owners.
//! CLIF construction is synchronous; parallel machine-code compilation of
//! already-built functions does not emit operations into this ledger.

use rumoca_core::Span;
use rumoca_ir_solve::{LinearOp, SolvePureCallTable, TypedProgram};
use std::cell::RefCell;

mod accounting;
#[cfg(test)]
mod tests;

pub use accounting::{ExpansionCopy, ExpansionEntry, ExpansionSource, ForcedExpansion};

/// Historical single-shape warning floor, not a scaling proof.
pub const EXPANSION_FLOOR: u64 = 16;

/// Source snapshots and raw accounting for one scoped lowering invocation.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct ExpansionReport {
    pub programs: Vec<TypedProgram>,
    pub tables: Vec<SolvePureCallTable>,
    pub program_tables: Vec<Option<usize>>,
    pub entries: Vec<ExpansionEntry>,
    /// False after a failed definition, unclosed operation or invalid counter.
    /// A refused admission probe is retained as a known outcome, not lost work.
    pub complete: bool,
    pub unattributed_copy_sites: u64,
}

impl ExpansionReport {
    /// Raw legacy heuristic: inspect paired shapes before calling it expansion.
    pub fn suspects(&self) -> impl Iterator<Item = &ExpansionEntry> {
        self.entries
            .iter()
            .filter(|entry| entry.is_unforced_suspect())
    }
}

struct Frame {
    entry: usize,
    function: usize,
    start: usize,
    nested: usize,
}

struct Ledger {
    report: ExpansionReport,
    frames: Vec<Frame>,
    loops: Vec<(usize, u64)>,
    table_context: Option<usize>,
}

impl Default for Ledger {
    fn default() -> Self {
        Self {
            report: ExpansionReport {
                complete: true,
                ..ExpansionReport::default()
            },
            frames: Vec::new(),
            loops: Vec::new(),
            table_context: None,
        }
    }
}

thread_local! {
    static LEDGER: RefCell<Option<Ledger>> = const { RefCell::new(None) };
}

/// Restores an enclosing collection even when `work` unwinds. Concurrent
/// threads and nested invocations have independent ledgers; child threads do
/// not implicitly join a report.
struct Scope(Option<Ledger>);

impl Drop for Scope {
    fn drop(&mut self) {
        LEDGER.with(|cell| {
            cell.replace(self.0.take());
        });
    }
}

/// Collect this thread's native lowering. Nested calls return their own report
/// and restore their caller's ledger. Result errors remain the caller's result;
/// failed individual lowerings also mark the report incomplete.
pub fn record_expansion_report<T>(work: impl FnOnce() -> T) -> (T, ExpansionReport) {
    let _scope = Scope(LEDGER.with(|cell| cell.replace(Some(Ledger::default()))));
    let value = work();
    let report = LEDGER.with(|cell| {
        let mut ledger = cell
            .borrow_mut()
            .take()
            .expect("expansion scope is installed");
        ledger.report.complete &= ledger.frames.is_empty() && ledger.loops.is_empty();
        ledger.report
    });
    (value, report)
}

pub(crate) fn enabled() -> bool {
    LEDGER.with(|cell| cell.borrow().is_some())
}

pub(crate) fn program(source: &TypedProgram, table: Option<&SolvePureCallTable>) -> Option<usize> {
    LEDGER.with(|cell| {
        let mut cell = cell.borrow_mut();
        let ledger = cell.as_mut()?;
        let table = table
            .map(|table| retain_table(ledger, table))
            .or(ledger.table_context);
        let id = ledger.report.programs.len();
        ledger.report.program_tables.push(table);
        ledger.report.programs.push(source.clone());
        Some(id)
    })
}

fn retain_table(ledger: &mut Ledger, table: &SolvePureCallTable) -> usize {
    ledger
        .report
        .tables
        .iter()
        .position(|retained| retained.shares_table_owner(table))
        .unwrap_or_else(|| {
            let id = ledger.report.tables.len();
            ledger.report.tables.push(table.clone());
            id
        })
}

/// Scoped diagnostic binding of the actual retained row emitter's table.
/// The guard stays on the emitting thread; no identity is stored in Solve IR.
pub(crate) struct CallTableScope {
    previous: Option<Option<usize>>,
    _not_send: std::marker::PhantomData<*const ()>,
}

pub(crate) fn call_table_scope(table: Option<&SolvePureCallTable>) -> CallTableScope {
    let previous = LEDGER.with(|cell| {
        let mut cell = cell.borrow_mut();
        let ledger = cell.as_mut()?;
        let table = table.map(|table| retain_table(ledger, table));
        Some(std::mem::replace(&mut ledger.table_context, table))
    });
    CallTableScope {
        previous,
        _not_send: std::marker::PhantomData,
    }
}

impl Drop for CallTableScope {
    fn drop(&mut self) {
        let Some(previous) = self.previous else {
            return;
        };
        LEDGER.with(|cell| {
            if let Some(ledger) = cell.borrow_mut().as_mut() {
                ledger.table_context = previous;
            }
        });
    }
}

/// An operation token is meaningful only inside the collection that issued it.
/// It never escapes a synchronous lowerer invocation.
pub(crate) struct Opened(usize);

pub(crate) fn open(
    source: ExpansionSource,
    kind: &'static str,
    span: Option<Span>,
    elements: u64,
    (function, instructions): (usize, usize),
) -> Option<Opened> {
    LEDGER.with(|cell| {
        let mut cell = cell.borrow_mut();
        let ledger = cell.as_mut()?;
        let entry = ledger.report.entries.len();
        let parent = ledger.frames.last().map(|frame| frame.entry);
        let phase = parent.map_or("definition", |parent| ledger.report.entries[parent].phase);
        let call_table = match &source {
            ExpansionSource::Typed { program, .. } => ledger.report.program_tables[*program],
            _ => ledger.table_context,
        };
        ledger.report.entries.push(ExpansionEntry {
            source,
            phase,
            call_table,
            kind,
            span,
            elements,
            parent,
            instructions: 0,
            loop_bounds: ledger
                .loops
                .iter()
                .filter_map(|(owner, bound)| (*owner == function).then_some(*bound))
                .collect(),
            copies: Vec::new(),
            forced: None,
            succeeded: false,
            path: "ordinary",
            fused_operations: Vec::new(),
        });
        ledger.frames.push(Frame {
            entry,
            function,
            start: instructions,
            nested: 0,
        });
        Some(Opened(ledger.frames.len()))
    })
}

pub(crate) fn finish(opened: Option<Opened>, instructions: usize, succeeded: bool) {
    let Some(Opened(depth)) = opened else {
        return;
    };
    LEDGER.with(|cell| {
        let mut cell = cell.borrow_mut();
        let Some(ledger) = cell.as_mut() else {
            return;
        };
        if depth != ledger.frames.len() {
            ledger.report.complete = false;
            return;
        }
        let Some(frame) = ledger.frames.pop() else {
            return;
        };
        let total = instructions.checked_sub(frame.start);
        let own = total.and_then(|total| total.checked_sub(frame.nested));
        let entry = &mut ledger.report.entries[frame.entry];
        entry.instructions = own.unwrap_or(0) as u64;
        entry.succeeded = succeeded && own.is_some();
        ledger.report.complete &= own.is_some() && (succeeded || entry.phase == "admission-probe");
        if let Some(parent) = ledger.frames.last_mut()
            && parent.function == frame.function
        {
            match total.and_then(|total| parent.nested.checked_add(total)) {
                Some(nested) => parent.nested = nested,
                None => ledger.report.complete = false,
            }
        }
    });
}

fn current(update: impl FnOnce(&mut ExpansionEntry, &[u64])) {
    LEDGER.with(|cell| {
        let mut cell = cell.borrow_mut();
        let Some(ledger) = cell.as_mut() else {
            return;
        };
        if let Some(frame) = ledger.frames.last() {
            let bounds: Vec<_> = ledger
                .loops
                .iter()
                .filter_map(|(owner, bound)| (*owner == frame.function).then_some(*bound))
                .collect();
            update(&mut ledger.report.entries[frame.entry], &bounds);
        }
    });
}

/// Record a logical transfer at its actual emitting site. A bulk call and an
/// unrolled copy both move cells, but do not emit the same number of CLIF ops.
pub(crate) fn moved(cells: u64, method: &'static str, function: usize) {
    LEDGER.with(|cell| {
        let mut cell = cell.borrow_mut();
        let Some(ledger) = cell.as_mut() else {
            return;
        };
        let frame = ledger
            .frames
            .iter()
            .rev()
            .find(|frame| frame.function == function);
        if let Some(frame) = frame {
            let bounds = ledger
                .loops
                .iter()
                .filter_map(|(owner, bound)| (*owner == function).then_some(*bound))
                .collect();
            ledger.report.entries[frame.entry]
                .copies
                .push(ExpansionCopy {
                    cells,
                    method,
                    loop_bounds: bounds,
                });
        } else {
            ledger.report.complete = false;
            ledger.report.unattributed_copy_sites += 1;
        }
    });
}

pub(crate) fn fused_operations(operations: Vec<usize>) {
    current(|entry, _| entry.fused_operations = operations);
}

pub(crate) fn phase(phase: &'static str) {
    current(|entry, _| entry.phase = phase);
}

pub(crate) fn forced(reason: ForcedExpansion) {
    current(|entry, _| entry.forced = Some(reason));
}

pub(crate) fn path(path: &'static str) {
    current(|entry, _| entry.path = path);
}

/// A bound is the constructor-issued domain capacity, not a measured trip
/// count. Early-exit folds can execute fewer iterations.
pub(crate) struct LoopScope(Option<usize>);

pub(crate) fn loop_scope(bound: u64, function: usize) -> LoopScope {
    LoopScope(LEDGER.with(|cell| {
        let mut cell = cell.borrow_mut();
        let ledger = cell.as_mut()?;
        let depth = ledger.loops.len();
        ledger.loops.push((function, bound));
        Some(depth)
    }))
}

impl Drop for LoopScope {
    fn drop(&mut self) {
        let Some(depth) = self.0 else {
            return;
        };
        LEDGER.with(|cell| {
            if let Some(ledger) = cell.borrow_mut().as_mut() {
                ledger.loops.truncate(depth);
            }
        });
    }
}

/// A row operation is retained directly because row functions may be lowered
/// without a TypedProgram. Typed entries reference canonical program snapshots.
pub(crate) fn row(op: &LinearOp, counters: (usize, usize)) -> Option<Opened> {
    if !enabled() {
        return None;
    }
    open(
        ExpansionSource::Row(op.clone()),
        op.kind_name(),
        None,
        op.dst_register_count() as u64,
        counters,
    )
}
