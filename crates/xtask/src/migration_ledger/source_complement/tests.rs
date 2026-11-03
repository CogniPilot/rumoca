use super::*;

fn pair(identity: u32, before: Range<usize>, after: Range<usize>) -> RegionPair {
    RegionPair {
        identity,
        before,
        after,
    }
}

#[test]
fn replacements_of_different_lengths_preserve_each_complement_segment() {
    let before = b"head[old]between[x]tail";
    let after = b"head[new spelling]between[yy]tail";
    let regions = [pair(1, 4..9, 4..18), pair(2, 16..19, 25..29)];
    let witness = prove_complement(before, after, &regions).unwrap();
    witness.check_inputs(before, after, &regions).unwrap();
}

#[test]
fn changed_prefix_middle_suffix_and_extra_outside_text_refuse() {
    let before = b"aXbYc";
    let regions = [pair(1, 1..2, 1..2), pair(2, 3..4, 3..4)];
    for after in [b"zPbQc".as_slice(), b"aPzQc", b"aPbQz", b"aPbQcZ"] {
        assert!(matches!(
            prove_complement(before, after, &regions),
            Err(ComplementRefusal::ChangedComplement)
        ));
    }
}

#[test]
fn an_insertion_or_deletion_needs_exact_anchor_and_surrounding_bytes() {
    let insertion = [pair(4, 1..1, 1..4)];
    prove_complement(b"ab", b"aNEWb", &insertion).unwrap();
    let deletion = [pair(4, 1..4, 1..1)];
    prove_complement(b"aOLD b", b"a b", &deletion).unwrap();
    assert!(matches!(
        prove_complement(b"ab", b"NEWab", &insertion),
        Err(ComplementRefusal::ChangedComplement)
    ));
}

#[test]
fn overlap_out_of_order_and_duplicate_ids_cannot_hide_an_uncovered_segment() {
    for regions in [
        vec![pair(1, 0..3, 0..3), pair(2, 2..4, 2..4)],
        vec![pair(1, 3..4, 3..4), pair(2, 0..1, 0..1)],
        vec![pair(1, 0..1, 0..1), pair(1, 3..4, 3..4)],
    ] {
        assert!(matches!(
            prove_complement(b"abcd", b"abcd", &regions),
            Err(ComplementRefusal::UnorderedOrOverlapping(_)
                | ComplementRefusal::DuplicateIdentity(_))
        ));
    }
}

#[test]
fn hostile_coordinates_refuse_without_indexing_or_wrapping() {
    for (before, after) in [
        (3..2, 0..0),
        (0..usize::MAX, 0..0),
        (0..0, usize::MAX..usize::MAX),
        (0..0, 1..0),
    ] {
        assert!(matches!(
            prove_complement(b"a", b"a", &[pair(7, before, after)]),
            Err(ComplementRefusal::InvalidRange(7))
        ));
    }
}

#[test]
fn empty_or_oversized_region_lists_refuse_even_when_bytes_match() {
    assert!(matches!(
        prove_complement(b"same", b"same", &[]),
        Err(ComplementRefusal::NoRegions)
    ));
    let regions = vec![pair(0, 0..0, 0..0); MAX_REGIONS + 1];
    assert!(matches!(
        prove_complement(b"", b"", &regions),
        Err(ComplementRefusal::RegionLimit)
    ));
}

#[test]
fn equal_copied_inputs_cannot_reuse_the_byte_relation_witness() {
    let before = b"aXb".to_vec();
    let after = b"aYb".to_vec();
    let regions = [pair(3, 1..2, 1..2)];
    let witness = prove_complement(&before, &after, &regions).unwrap();
    assert_eq!(
        witness.check_inputs(&before.clone(), &after, &regions),
        Err(ComplementRefusal::ForeignInput)
    );
    assert_eq!(
        witness.check_inputs(&before, &after.clone(), &regions),
        Err(ComplementRefusal::ForeignInput)
    );
    assert_eq!(
        witness.check_inputs(&before, &after, &regions.clone()),
        Err(ComplementRefusal::ForeignInput)
    );
}

#[test]
fn changed_region_bodies_are_not_semantically_authorized_by_complement_equality() {
    let regions = [pair(9, 1..2, 1..2)];
    // This deliberately succeeds as a byte relation. Only the actual outer
    // syntax/API/conformance products may establish whether Y is legal.
    prove_complement(b"aXb", b"aYb", &regions).unwrap();
}
