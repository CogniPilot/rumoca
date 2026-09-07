// Closure lifetime arguments left erased in a function signature.
//
// Charon resolves an `impl Iterator` return type to the generated closure type
// but leaves the closure's lifetime argument erased. Aeneas repairs this in
// `PrePasses.fix_closure_signature_regions`, which substitutes the erased
// argument only when the function binds exactly one region parameter; with more
// than one it cannot tell which lifetime the closure captured and leaves the
// signature unchanged. The unrepaired erased region then reaches `ty_is_rty`.
//
// Upstream: AeneasVerif/charon#1040, AeneasVerif/aeneas#1207.
//
// `one_region` is the positive control: it binds a single region parameter, so
// the repair applies. The two-region functions differ from it only in the
// number of region parameters. They also show that the ambiguity is real rather
// than an artifact of this shape: the captured lifetime is the first in one and
// the second in the other, and the parameter list alone does not distinguish
// them.

/// One region parameter. The signature repair applies.
pub fn one_region(x: &u8) -> impl Iterator<Item = u8> + '_ {
    (0..1).map(move |_| *x)
}

/// Two region parameters, capturing the first.
pub fn two_regions_capture_first<'a, 'b>(x: &'a u8, _y: &'b u8) -> impl Iterator<Item = u8> + 'a {
    (0..1).map(move |_| *x)
}

/// Two region parameters, capturing the second. Independent of
/// `two_regions_capture_first` in which lifetime reaches the closure.
pub fn two_regions_capture_second<'a, 'b>(_x: &'a u8, y: &'b u8) -> impl Iterator<Item = u8> + 'b {
    (0..1).map(move |_| *y)
}

/// Two region parameters where the closure captures a reference and the item
/// type keeps that lifetime, so the closure state is a borrow rather than a
/// copied scalar. This is the shape the DAE inventory adapters have.
pub fn two_regions_ref_item<'a, 'b>(xs: &'a [u8], _y: &'b u8) -> impl Iterator<Item = &'a u8> + 'a {
    (0..xs.len()).map(move |index| &xs[index])
}

/// Calls the reference-returning adapter.
pub fn consume_ref_item(xs: &[u8], y: &u8) -> u8 {
    let mut iterator = two_regions_ref_item(xs, y);
    match iterator.next() {
        Some(byte) => *byte,
        None => 0,
    }
}

/// Calls a two-region function. The erased signature then reaches the caller's
/// call-result construction as well as the callee's input projector.
pub fn consume_two_regions(x: &u8, y: &u8) -> u8 {
    let mut iterator = two_regions_capture_first(x, y);
    match iterator.next() {
        Some(byte) => byte,
        None => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_region_yields_the_borrowed_byte() {
        let x = 7u8;
        assert_eq!(one_region(&x).collect::<Vec<_>>(), vec![7]);
    }

    #[test]
    fn two_region_captures_are_independent() {
        let x = 7u8;
        let y = 9u8;
        assert_eq!(two_regions_capture_first(&x, &y).collect::<Vec<_>>(), vec![7]);
        assert_eq!(two_regions_capture_second(&x, &y).collect::<Vec<_>>(), vec![9]);
    }

    #[test]
    fn consume_takes_the_first_capture() {
        let x = 7u8;
        let y = 9u8;
        assert_eq!(consume_two_regions(&x, &y), 7);
    }

    #[test]
    fn reference_item_adapter_yields_the_first_byte() {
        let xs = [7u8, 8u8];
        let y = 9u8;
        assert_eq!(consume_ref_item(&xs, &y), 7);
    }
}
