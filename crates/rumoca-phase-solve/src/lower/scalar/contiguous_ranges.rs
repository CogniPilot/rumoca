//! Register ranges that already lie end to end.

use rumoca_ir_solve as solve;

/// The start of the one register range that `ranges` form when each non-empty
/// `(start, count)` range begins where the previous one ends, so packing them
/// into one contiguous range needs no copy.
pub(super) fn contiguous_start(
    ranges: impl IntoIterator<Item = (solve::Reg, usize)>,
) -> Option<solve::Reg> {
    let mut non_empty = ranges.into_iter().filter(|&(_, count)| count != 0);
    let (first, first_count) = non_empty.next()?;
    let mut end = first.checked_add(solve::Reg::try_from(first_count).ok()?)?;
    for (start, count) in non_empty {
        if start != end {
            return None;
        }
        end = end.checked_add(solve::Reg::try_from(count).ok()?)?;
    }
    Some(first)
}

#[cfg(test)]
mod tests {
    use super::contiguous_start;

    #[test]
    fn adjacent_ranges_form_one_range_starting_at_the_first() {
        assert_eq!(contiguous_start([(4, 2), (6, 1), (7, 3)]), Some(4));
    }

    #[test]
    fn empty_ranges_do_not_break_adjacency() {
        assert_eq!(contiguous_start([(4, 2), (99, 0), (6, 1)]), Some(4));
    }

    #[test]
    fn a_gap_or_an_overlap_keeps_the_ranges_separate() {
        assert_eq!(contiguous_start([(4, 2), (7, 1)]), None);
        assert_eq!(contiguous_start([(4, 2), (5, 1)]), None);
        assert_eq!(contiguous_start([(6, 1), (4, 2)]), None);
    }

    #[test]
    fn no_non_empty_range_has_no_start() {
        assert_eq!(contiguous_start([(3, 0)]), None);
        assert_eq!(contiguous_start([]), None);
    }
}
