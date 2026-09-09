//! Drop-control discriminant switch after a partial move.
//!
//! When a `match` moves the payload out of one enum variant and leaves the
//! other variant untouched, rustc's drop elaboration emits, at the end of the
//! function, a switch on the enum's discriminant whose taken-variant branch is
//! empty and whose fallback drops the enum. `core::result::Result::ok` has
//! exactly this shape. `take` reproduces it in first-party code with the same
//! generic payloads, so the shape does not depend on any concrete destructor.
//!
//! `take_reporting` and `take_early` carry the same switch in other positions and
//! are rewritten like `take`. `take_first` is the refusal control: rustc emits a
//! drop flag for its un-moved tuple half, so the continuation after its switch
//! differs from the fallback's and a syntactic rewrite must not fire. `peek`
//! moves nothing and has no such switch.

pub enum Carrier<T, E> {
    Take(T),
    Keep(E),
}

/// The shape under study. After the `match`, `carrier` is partially moved in
/// the `Take` arm and intact in the `Keep` arm, so the function's drop switch
/// reads the discriminant and drops `carrier` only in the `Keep` case.
pub fn take<T, E>(carrier: Carrier<T, E>) -> Option<T> {
    match carrier {
        Carrier::Take(value) => Some(value),
        Carrier::Keep(_) => None,
    }
}

/// The same drop switch with an extra `&mut` argument in scope. The store in
/// the `Keep` arm belongs to the user `match`, not to the drop switch, whose
/// two paths still share their continuation; it is rewritten like `take`.
pub fn take_reporting<T, E>(carrier: Carrier<T, E>, dropped: &mut bool) -> Option<T> {
    match carrier {
        Carrier::Take(value) => Some(value),
        Carrier::Keep(_) => {
            *dropped = true;
            None
        }
    }
}

/// The drop switch nested inside the `if let` fallthrough arm and again at the
/// tail; both occurrences are rewritten, which exercises recursion into branches.
pub fn take_early<T, E>(carrier: Carrier<T, E>) -> Option<T> {
    if let Carrier::Take(value) = carrier {
        return Some(value);
    }
    None
}

/// Control: no payload is moved, so `carrier` is never partially moved and
/// rustc emits an unconditional drop rather than a discriminant switch.
pub fn peek<T, E>(carrier: &Carrier<T, E>) -> bool {
    matches!(carrier, Carrier::Take(_))
}

/// Control: only the first half of the taken payload is moved. rustc tracks the
/// second half with a drop flag, so the continuation after the drop switch
/// begins with a test of that flag while the fallback's does not; the two paths
/// do not share a continuation and a syntactic rewrite must not fire.
pub fn take_first<T, E>(carrier: Carrier<(T, T), E>) -> Option<T> {
    match carrier {
        Carrier::Take((first, _)) => Some(first),
        Carrier::Keep(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Counted<'a>(&'a std::cell::Cell<u32>);
    impl Drop for Counted<'_> {
        fn drop(&mut self) {
            self.0.set(self.0.get() + 1);
        }
    }

    #[test]
    fn take_drops_keep_payload_exactly_once_and_take_payload_never() {
        let drops = std::cell::Cell::new(0);
        assert!(take::<u32, Counted<'_>>(Carrier::Take(7)) == Some(7));
        assert_eq!(drops.get(), 0);
        assert!(take::<u32, Counted<'_>>(Carrier::Keep(Counted(&drops))).is_none());
        assert_eq!(drops.get(), 1);
    }

    #[test]
    fn take_reporting_observes_the_keep_path() {
        let drops = std::cell::Cell::new(0);
        let mut dropped = false;
        assert_eq!(
            take_reporting::<u32, Counted<'_>>(Carrier::Take(3), &mut dropped),
            Some(3)
        );
        assert!(!dropped);
        assert!(
            take_reporting::<u32, Counted<'_>>(Carrier::Keep(Counted(&drops)), &mut dropped)
                .is_none()
        );
        assert!(dropped);
        assert_eq!(drops.get(), 1);
    }

    #[test]
    fn take_first_keeps_the_returned_half_alive_and_drops_the_other_inside() {
        let drops = std::cell::Cell::new(0);
        let first =
            take_first::<Counted<'_>, u32>(Carrier::Take((Counted(&drops), Counted(&drops))));
        assert_eq!(
            drops.get(),
            1,
            "the un-moved second half is dropped inside the call"
        );
        assert!(first.is_some());
        drop(first);
        assert_eq!(
            drops.get(),
            2,
            "the returned first half lives until dropped here"
        );
        assert!(take_first::<Counted<'_>, Counted<'_>>(Carrier::Keep(Counted(&drops))).is_none());
        assert_eq!(drops.get(), 3);
    }

    #[test]
    fn take_early_and_peek_agree_with_take() {
        let drops = std::cell::Cell::new(0);
        assert_eq!(take_early::<u32, Counted<'_>>(Carrier::Take(9)), Some(9));
        assert!(take_early::<u32, Counted<'_>>(Carrier::Keep(Counted(&drops))).is_none());
        assert_eq!(drops.get(), 1);
        assert!(peek::<u32, Counted<'_>>(&Carrier::Take(1)));
        assert!(!peek::<u32, Counted<'_>>(&Carrier::Keep(Counted(&drops))));
        assert_eq!(drops.get(), 2);
    }
}
