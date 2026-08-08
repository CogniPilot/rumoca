//! Single-consumer payload cell for parol grammar conversion.
//!
//! # Why this exists
//!
//! parol's generated semantic actions pop each nonterminal payload as an
//! **owned** local, call `(&payload).try_into()` exactly once, and then drop it
//! (see `generated/modelica_grammar_trait.rs`: the `composition` action pops
//! `element_list` and the `standard_class_specifier` action pops `composition`,
//! each converting through a shared reference and dropping the source right
//! afterwards). The conversion therefore only ever holds a `&` to a value that
//! is provably dead once it returns, which used to force a deep clone of the
//! whole class body at every nesting level — quadratic work on deeply nested
//! packages.
//!
//! `TakeCell` makes that move-out explicit: the payload is handed over exactly
//! once, through `&self`.
//!
//! # Failure mode
//!
//! A second [`TakeCell::take`] means the grammar started reading a nonterminal
//! twice, which is a compiler bug, not a recoverable input condition. It panics
//! loudly rather than substituting an empty payload, because silently producing
//! an empty class body is exactly the plausible-but-wrong substitution
//! SPEC_0008 forbids.

use std::cell::RefCell;
use std::fmt;

/// Message used when a parol payload is consumed more than once.
pub(crate) const DOUBLE_TAKE_MESSAGE: &str = "parol payload consumed twice";

/// A payload that a parol conversion moves out of exactly once.
pub(crate) struct TakeCell<T>(RefCell<Option<T>>);

impl<T> TakeCell<T> {
    /// Wrap a payload that has not been consumed yet.
    pub(crate) fn new(value: T) -> Self {
        Self(RefCell::new(Some(value)))
    }

    /// Move the payload out.
    ///
    /// # Panics
    ///
    /// Panics with [`DOUBLE_TAKE_MESSAGE`] if the payload was already taken.
    pub(crate) fn take(&self) -> T {
        self.0.borrow_mut().take().expect(DOUBLE_TAKE_MESSAGE)
    }
}

impl<T: Default> Default for TakeCell<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: Clone> Clone for TakeCell<T> {
    /// Cloning preserves "already taken" rather than resurrecting a payload.
    fn clone(&self) -> Self {
        Self(RefCell::new(self.0.borrow().clone()))
    }
}

impl<T: fmt::Debug> fmt::Debug for TakeCell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.try_borrow() {
            Ok(slot) => slot.fmt(f),
            Err(_) => f.write_str("TakeCell(<in use>)"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TakeCell;

    #[test]
    fn take_returns_original_payload() {
        let cell = TakeCell::new(vec![1_u32, 2, 3]);
        assert_eq!(cell.take(), vec![1, 2, 3]);
    }

    #[test]
    #[should_panic(expected = "parol payload consumed twice")]
    fn double_take_panics() {
        let cell = TakeCell::new(vec![1_u32]);
        let _first = cell.take();
        let _second = cell.take();
    }

    #[test]
    fn clone_of_untaken_cell_carries_the_payload() {
        let cell = TakeCell::new(vec![1_u32]);
        let copy = cell.clone();
        assert_eq!(copy.take(), vec![1]);
        // Taking from the clone must not consume the original.
        assert_eq!(cell.take(), vec![1]);
    }

    #[test]
    #[should_panic(expected = "parol payload consumed twice")]
    fn clone_after_take_is_still_empty() {
        let cell = TakeCell::new(vec![1_u32]);
        let _taken = cell.take();
        let copy = cell.clone();
        let _second = copy.take();
    }

    #[test]
    fn default_wraps_a_default_payload() {
        let cell: TakeCell<Vec<u32>> = TakeCell::default();
        assert_eq!(cell.take(), Vec::<u32>::new());
    }
}
