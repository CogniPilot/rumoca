//! The data-movement costs emission decisions trade against each other
//! (SPEC_0040 SOLVE-C105).
//!
//! Every quantity is in bytes moved and follows from one fact: the widest lane
//! a native program moves is 8 bytes (a Binary64 cell, an `i64` cell, and an
//! address cell of the 32-bit module memory widened to a cell). Naming a value
//! by address costs one such cell and reading through that address costs one
//! more lane (one indirection), so a copy of more bytes than that is dearer
//! than the address. Issuing a copy costs the same two lanes (its two ends), so
//! two ranges separated by no more bytes than that are cheaper to copy as one
//! range than as two. No decision cites a model, a shape or a measurement.

/// The widest lane: Binary64, `i64`, or an address cell.
pub(crate) const LANE_BYTES: u32 = 8;

/// What it costs to issue one copy, or to name a value by address and read
/// through it once: two lanes (the two ends of a copy; the address cell and
/// one indirection).
pub(crate) const ISSUE_BYTES: u32 = 2 * LANE_BYTES;

/// Whether passing a value of `bytes` bytes by address is cheaper than copying
/// it: the copy exceeds the address plus one indirection.
pub(crate) const fn address_beats_copy(bytes: u32) -> bool {
    bytes > ISSUE_BYTES
}

/// The most cells that may separate two ranges for one copy of both to beat
/// two copies: the bytes copied in the gap cost no more than issuing a second
/// copy.
pub(crate) const fn joined_copy_gap_cells() -> usize {
    (ISSUE_BYTES / LANE_BYTES) as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_address_is_cheaper_than_a_copy_exactly_when_the_copy_exceeds_address_plus_indirection() {
        assert!(!address_beats_copy(LANE_BYTES));
        assert!(!address_beats_copy(ISSUE_BYTES));
        assert!(address_beats_copy(ISSUE_BYTES + 1));
    }

    #[test]
    fn ranges_are_joined_when_the_gap_costs_no_more_than_issuing_another_copy() {
        assert_eq!(joined_copy_gap_cells() as u32 * LANE_BYTES, ISSUE_BYTES);
        assert_eq!(joined_copy_gap_cells(), 2);
    }
}
