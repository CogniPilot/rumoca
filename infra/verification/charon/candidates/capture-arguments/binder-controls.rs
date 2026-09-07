// Depth controls for closure capture-region reconstruction.
//
// The two-region trigger is covered by `closure-lifetimes/two-region-iterator.rs`.
// This file adds the case that trigger cannot see: a closure whose own signature
// is higher-ranked, so the reference to its ADT occurs under an extra region
// binder. A reconstruction that ignores binder depth is correct in the flat case,
// where the required shift is zero, and wrong here. Arity and erasure checks
// cannot distinguish the two, because a bound region at the wrong depth is well
// formed; only the denoted lifetime differs.
//
// The file is split deliberately. The `borrowed_` functions do NOT return their
// reference argument; the `returning_` functions do. That split does NOT isolate
// the axes, which was measured rather than assumed: on the adopted build every
// closure here, borrowed and returning alike, reaches InterpBorrows 1203 in its
// `Fn::call` implementation, so a higher-ranked closure signature meets the
// non-endable axis whether or not the argument is returned.
//
// The isolation that does hold is at the item level. The erased-signature axis
// shows on the FUNCTIONS (InterpProjectors 543 on the two-region forms,
// InterpUtils 151 on the consumer); the non-endable axis shows on their separate
// `call` implementations. So the regression criterion for this file is not that
// it translates clean, which it cannot do while 1203 stands, but:
//
//   no 543 on any function here, 151 gone, 1203 on the `call` impls unchanged.
//
// The stricter translate-clean criterion still applies to
// `closure-lifetimes/two-region-iterator.rs`, which has no higher-ranked closure
// and does reach zero errors on its one-region control.
//
// Source-faithful expectations, stated so a translated signature can be compared
// against them rather than against whatever the translator currently emits:
//
//   borrowed_one_region       one region parameter 'data. The closure is
//                             higher-ranked in 'arg and captures at 'data. 'arg
//                             does not appear in the output.
//   borrowed_two_regions      'data and 'other; the closure captures at 'data
//                             ONLY, so no reconstructed capture region may
//                             denote 'other.
//   borrowed_two_regions_alt  the mirror, capturing at 'other only. Together
//                             these distinguish a reconstruction that resolves
//                             the capture from one that returns the first
//                             region parameter.
//
// The native tests pin the source-level truth that the higher-ranked argument and
// the captured reference are different references at run time, and that the
// closure remains callable at more than one argument lifetime.

/// One region parameter, higher-ranked argument, argument not returned.
/// Positive control for the depth axis.
pub fn borrowed_one_region<'data>(
    value: &'data u8,
) -> impl for<'arg> Fn(&'arg u8) -> (u8, &'data u8) {
    move |argument| (*argument, value)
}

/// Two region parameters, capture at the first.
pub fn borrowed_two_regions<'data, 'other>(
    value: &'data u8,
    _unused: &'other u8,
) -> impl for<'arg> Fn(&'arg u8) -> (u8, &'data u8) {
    move |argument| (*argument, value)
}

/// Two region parameters, capture at the second.
pub fn borrowed_two_regions_alt<'data, 'other>(
    _unused: &'data u8,
    value: &'other u8,
) -> impl for<'arg> Fn(&'arg u8) -> (u8, &'other u8) {
    move |argument| (*argument, value)
}

/// Calls the two-region form, so the closure's `Fn` implementation is reached
/// with the closure type appearing in a method signature.
pub fn consume_borrowed(value: &u8, unused: &u8) -> u8 {
    let apply = borrowed_two_regions(value, unused);
    let local = 3u8;
    let (argument, captured) = apply(&local);
    argument.wrapping_add(*captured)
}

/// Also returns the higher-ranked argument reference. This additionally reaches
/// the non-endable axis, so a failure here is not evidence about depth.
pub fn returning_one_region<'data>(
    value: &'data u8,
) -> impl for<'arg> Fn(&'arg u8) -> (&'arg u8, &'data u8) {
    move |argument| (argument, value)
}

/// Two region parameters, returning the argument reference.
pub fn returning_two_regions<'data, 'other>(
    value: &'data u8,
    _unused: &'other u8,
) -> impl for<'arg> Fn(&'arg u8) -> (&'arg u8, &'data u8) {
    move |argument| (argument, value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_region_capture_is_the_declared_parameter() {
        let value = 7u8;
        let apply = borrowed_one_region(&value);
        let argument = 11u8;
        let (seen, captured) = apply(&argument);
        assert_eq!(seen, 11);
        assert!(std::ptr::eq(captured, &value));
    }

    #[test]
    fn two_region_capture_resolves_to_the_captured_parameter() {
        let value = 7u8;
        let unused = 9u8;
        let argument = 11u8;

        let (seen, captured) = borrowed_two_regions(&value, &unused)(&argument);
        assert_eq!(seen, 11);
        assert!(std::ptr::eq(captured, &value));

        let (seen, captured) = borrowed_two_regions_alt(&value, &unused)(&argument);
        assert_eq!(seen, 11);
        assert!(std::ptr::eq(captured, &unused));
    }

    #[test]
    fn higher_ranked_argument_is_reusable_at_shorter_lifetimes() {
        let value = 7u8;
        let apply = borrowed_one_region(&value);
        {
            let short = 1u8;
            assert_eq!(apply(&short).0, 1);
        }
        let other = 2u8;
        assert_eq!(apply(&other).0, 2);
    }

    #[test]
    fn consumer_reaches_the_closure_implementation() {
        let value = 7u8;
        let unused = 9u8;
        assert_eq!(consume_borrowed(&value, &unused), 10);
    }

    #[test]
    fn returning_forms_keep_argument_and_capture_distinct() {
        let value = 7u8;
        let argument = 11u8;
        let unused = 9u8;

        let (seen, captured) = returning_one_region(&value)(&argument);
        assert!(std::ptr::eq(seen, &argument));
        assert!(std::ptr::eq(captured, &value));

        let (seen, captured) = returning_two_regions(&value, &unused)(&argument);
        assert!(std::ptr::eq(seen, &argument));
        assert!(std::ptr::eq(captured, &value));
    }
}
