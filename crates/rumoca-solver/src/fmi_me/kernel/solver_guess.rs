//! The kernel's warm-start solver vector and the writes a stamp can name.
//!
//! A derivative refresh settles its coordinate in this vector in place. The
//! settled-coordinate cache then records which write left it there instead of
//! copying the vector: any later mutable borrow is a new write, whoever makes
//! it, so a stamp is stale exactly when the vector may have moved since.

use std::cell::{Cell, Ref, RefCell, RefMut};

pub(super) struct SolverGuess {
    values: RefCell<Vec<f64>>,
    writes: Cell<u64>,
}

impl SolverGuess {
    pub(super) fn new(values: Vec<f64>) -> Self {
        Self {
            values: RefCell::new(values),
            writes: Cell::new(0),
        }
    }

    pub(super) fn borrow(&self) -> Ref<'_, Vec<f64>> {
        self.values.borrow()
    }

    /// Counts as a write before the borrow is handed out, so a stamp taken
    /// under this borrow names it.
    pub(super) fn borrow_mut(&self) -> RefMut<'_, Vec<f64>> {
        self.writes.set(self.writes.get().wrapping_add(1));
        self.values.borrow_mut()
    }

    /// The write that left the vector as it stands.
    pub(super) fn writes(&self) -> u64 {
        self.writes.get()
    }

    /// Run `f` over the vector with `prefix` written over its leading entries,
    /// then put the replaced entries back. The vector is bit for bit what it
    /// was, including when `f` unwinds, so this is not a write.
    pub(super) fn with_prefix<T>(
        &self,
        prefix: &mut [f64],
        f: impl FnOnce(&[f64]) -> T,
    ) -> Option<T> {
        struct Restore<'a> {
            values: RefMut<'a, Vec<f64>>,
            prefix: &'a mut [f64],
        }
        impl Drop for Restore<'_> {
            fn drop(&mut self) {
                let len = self.prefix.len();
                self.values[..len].swap_with_slice(self.prefix);
            }
        }
        let mut values = self.values.try_borrow_mut().ok()?;
        let leading = values.get_mut(..prefix.len())?;
        leading.swap_with_slice(prefix);
        let restore = Restore { values, prefix };
        Some(f(&restore.values))
    }
}
