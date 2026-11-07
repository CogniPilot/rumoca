//! Private append storage. No user code, model access, formatting or allocation.

use super::*;

#[derive(Default)]
pub(super) struct Storage {
    pub(super) records: Vec<SettingsRecord>,
    pub(super) cells: Vec<u64>,
    pub(super) instances: Vec<Instance>,
}

#[derive(Default)]
pub(super) struct Instance {
    pub(super) context: Context,
    pub(super) next_operation: u64,
}

#[derive(Default, Clone, Copy)]
pub(super) enum Context {
    #[default]
    Idle,
    Active(u64),
    Invalid,
}

impl Context {
    pub(super) fn is_idle(&self) -> bool {
        matches!(self, Self::Idle)
    }
}

pub(super) struct Shared {
    pub(super) storage: RefCell<Storage>,
    pub(super) incomplete: Cell<u16>,
    pub(super) finalized: Cell<bool>,
    pub(super) record_limit: usize,
    pub(super) cell_limit: usize,
}

impl Shared {
    pub(super) fn mark(&self, reason: IncompleteReason) {
        self.incomplete.set(self.incomplete.get() | reason as u16);
    }

    pub(super) fn borrow(&self) -> Option<std::cell::RefMut<'_, Storage>> {
        if self.finalized.get() {
            self.mark(IncompleteReason::Finalized);
            return None;
        }
        match self.storage.try_borrow_mut() {
            Ok(storage) => Some(storage),
            Err(_) => {
                self.mark(IncompleteReason::BorrowConflict);
                None
            }
        }
    }
}

impl Storage {
    pub(super) fn append(
        &mut self,
        shared: &Shared,
        instance: usize,
        operation: Option<u64>,
        kind: RecordKind,
        scalars: &[f64],
        vector: &[f64],
    ) {
        let available = shared.cell_limit.saturating_sub(self.cells.len());
        let needed = scalars.len().checked_add(vector.len());
        if self.records.len() >= shared.record_limit || needed.is_none_or(|n| n > available) {
            shared.mark(IncompleteReason::Capacity);
            return;
        }
        let start = self.cells.len();
        self.cells
            .extend(scalars.iter().chain(vector).map(|value| value.to_bits()));
        self.records.push(SettingsRecord {
            instance,
            operation,
            kind,
            cells: start..self.cells.len(),
        });
    }
}
