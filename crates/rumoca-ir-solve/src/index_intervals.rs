//! A set of indices as sorted, disjoint, non-adjacent inclusive intervals.
//!
//! Dependency sets of compact programs are mostly ranges: a tensor load reads
//! one contiguous slot range, and every element of a call result over whole
//! inputs depends on the same ranges. Holding them as intervals keeps a union
//! with one more index, a membership query, and a footprint the size of the
//! set's structure rather than its cardinality. Intervals are inclusive, so
//! every `usize`, `usize::MAX` included, is representable.

use std::collections::BTreeSet;
use std::ops::RangeInclusive;

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct IndexIntervals(Vec<(usize, usize)>);

impl IndexIntervals {
    /// The set of `start..end`; empty when the range is.
    #[must_use]
    pub fn range(start: usize, end: usize) -> Self {
        Self(if start < end {
            vec![(start, end - 1)]
        } else {
            Vec::new()
        })
    }

    #[must_use]
    pub fn singleton(index: usize) -> Self {
        Self(vec![(index, index)])
    }

    /// The set of `indices`, in any order.
    #[must_use]
    pub fn of(indices: impl IntoIterator<Item = usize>) -> Self {
        let mut sorted = indices.into_iter().collect::<Vec<_>>();
        sorted.sort_unstable();
        let mut intervals: Vec<(usize, usize)> = Vec::new();
        for index in sorted {
            match intervals.last_mut() {
                Some((_, last)) if *last >= index => {}
                Some((_, last)) if last.checked_add(1) == Some(index) => *last = index,
                _ => intervals.push((index, index)),
            }
        }
        Self(intervals)
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// The number of indices in the set, saturating at `usize::MAX`.
    #[must_use]
    pub fn len(&self) -> usize {
        self.0.iter().fold(0usize, |count, (start, last)| {
            count.saturating_add((last - start).saturating_add(1))
        })
    }

    /// The set's intervals, in ascending order.
    pub fn intervals(&self) -> impl Iterator<Item = RangeInclusive<usize>> + '_ {
        self.0.iter().map(|&(start, last)| start..=last)
    }

    /// The indices of the set, in ascending order.
    pub fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        self.intervals().flatten()
    }

    /// The position of the interval that could hold `index`.
    fn candidate(&self, index: usize) -> Option<usize> {
        self.0
            .partition_point(|&(start, _)| start <= index)
            .checked_sub(1)
    }

    #[must_use]
    pub fn contains(&self, index: usize) -> bool {
        self.candidate(index)
            .is_some_and(|position| index <= self.0[position].1)
    }

    /// Whether every index of `self` is in `other`.
    #[must_use]
    pub fn is_subset(&self, other: &Self) -> bool {
        self.0.iter().all(|&(start, last)| {
            other
                .candidate(start)
                .is_some_and(|position| last <= other.0[position].1)
        })
    }

    /// Add one index, merging it with adjacent intervals in place: a binary
    /// search plus at most one shift of the interval vector, no rebuild.
    pub fn insert(&mut self, index: usize) {
        let after = self.0.partition_point(|&(start, _)| start <= index);
        if after > 0 && index <= self.0[after - 1].1 {
            return;
        }
        let joins_before = after > 0 && self.0[after - 1].1.checked_add(1) == Some(index);
        let joins_after = self
            .0
            .get(after)
            .is_some_and(|&(start, _)| index.checked_add(1) == Some(start));
        match (joins_before, joins_after) {
            (true, true) => {
                self.0[after - 1].1 = self.0[after].1;
                self.0.remove(after);
            }
            (true, false) => self.0[after - 1].1 = index,
            (false, true) => self.0[after].0 = index,
            (false, false) => self.0.insert(after, (index, index)),
        }
    }

    /// The union of two sets, in one merge of their intervals.
    #[must_use]
    pub fn union(&self, other: &Self) -> Self {
        let mut merged: Vec<(usize, usize)> = Vec::with_capacity(self.0.len() + other.0.len());
        let (mut left, mut right) = (self.0.iter().peekable(), other.0.iter().peekable());
        loop {
            let next = match (left.peek(), right.peek()) {
                (Some(&&l), Some(&&r)) if l.0 <= r.0 => left.next(),
                (_, Some(_)) => right.next(),
                (Some(_), None) => left.next(),
                (None, None) => None,
            };
            let Some(&(start, last)) = next else {
                break;
            };
            match merged.last_mut() {
                Some((_, merged_last)) if merged_last.saturating_add(1) >= start => {
                    *merged_last = (*merged_last).max(last);
                }
                _ => merged.push((start, last)),
            }
        }
        Self(merged)
    }

    /// The indices of `self` that are also in `other`.
    #[must_use]
    pub fn intersection(&self, other: &Self) -> Self {
        let mut result = Vec::new();
        let (mut left, mut right) = (0, 0);
        while let (Some(&(ls, ll)), Some(&(rs, rl))) = (self.0.get(left), other.0.get(right)) {
            let (start, last) = (ls.max(rs), ll.min(rl));
            if start <= last {
                result.push((start, last));
            }
            if ll <= rl {
                left += 1;
            } else {
                right += 1;
            }
        }
        Self(result)
    }

    /// Whether an index of `self` other than `except` is a member of
    /// `inventory` that `resolved` does not admit. Resolution only grows, so
    /// an interval whose members are all resolved is recorded in `settled`
    /// and never scanned again: many sets sharing one wide interval cost its
    /// width once.
    pub fn reads_unresolved(
        &self,
        except: usize,
        inventory: &BTreeSet<usize>,
        resolved: impl Fn(usize) -> bool,
        settled: &mut SettledIntervals,
    ) -> bool {
        self.0.iter().any(|&(start, last)| {
            if settled.0.contains(&(start, last)) {
                return false;
            }
            let mut all_resolved = true;
            let unresolved = inventory.range(start..=last).any(|&index| {
                let open = !resolved(index);
                all_resolved &= !open;
                open && index != except
            });
            if all_resolved {
                settled.0.insert((start, last));
            }
            unresolved
        })
    }

    #[must_use]
    pub fn to_set(&self) -> BTreeSet<usize> {
        self.iter().collect()
    }
}

/// Intervals whose inventory members are all resolved; see
/// [`IndexIntervals::reads_unresolved`].
#[derive(Debug, Default)]
pub struct SettledIntervals(BTreeSet<(usize, usize)>);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intervals_coalesce_and_answer_set_queries() {
        let set = IndexIntervals::of([7, 1, 2, 3]);
        assert_eq!(set.intervals().collect::<Vec<_>>(), [1..=3, 7..=7]);
        assert!([1, 2, 3, 7].iter().all(|index| set.contains(*index)));
        assert!(![0, 4, 6, 8].iter().any(|index| set.contains(*index)));
        assert_eq!(set.len(), 4);
        assert!(!set.is_empty());
        assert!(IndexIntervals::of([]).is_empty());
        let mut grown = set.clone();
        for index in [4, 6, 5, 0, 20, usize::MAX] {
            grown.insert(index);
        }
        assert_eq!(
            grown.intervals().collect::<Vec<_>>(),
            [0..=7, 20..=20, usize::MAX..=usize::MAX]
        );
        assert!(set.is_subset(&grown) && !grown.is_subset(&set));
        let union = set.union(&IndexIntervals::range(3, 10));
        assert_eq!(union.intervals().collect::<Vec<_>>(), [1..=9]);
        let common = grown.intersection(&IndexIntervals::of([2, 7, 9, 20, usize::MAX]));
        assert_eq!(common.to_set(), BTreeSet::from([2, 7, 20, usize::MAX]));
    }

    #[test]
    fn a_settled_interval_is_not_scanned_again() {
        let reads = IndexIntervals::range(0, 4).union(&IndexIntervals::singleton(9));
        let inventory = BTreeSet::from([1, 2, 9]);
        let mut settled = SettledIntervals::default();
        assert!(reads.reads_unresolved(9, &inventory, |index| index != 2, &mut settled));
        assert!(!reads.reads_unresolved(9, &inventory, |index| index != 9, &mut settled));
        assert!(settled.0.contains(&(0, 3)));
        assert!(!reads.reads_unresolved(9, &inventory, |_| false, &mut settled));
    }
}
