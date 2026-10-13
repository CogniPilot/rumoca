//! Actual host-write coverage; no entry is allocated per array element.

use std::ops::Range;

#[derive(Clone, Default)]
pub(super) struct InputPresence(Vec<Range<usize>>);

impl InputPresence {
    pub(super) fn try_clone(&self) -> Result<Self, super::NativeTransportError> {
        let mut ranges = Vec::new();
        ranges
            .try_reserve_exact(self.0.len())
            .map_err(|_| super::NativeTransportError::AllocationRefused)?;
        ranges.extend_from_slice(&self.0);
        Ok(Self(ranges))
    }

    pub(super) fn reserve_insertions(
        &mut self,
        count: usize,
    ) -> Result<(), super::NativeTransportError> {
        self.0
            .try_reserve(count)
            .map_err(|_| super::NativeTransportError::AllocationRefused)
    }

    pub(super) fn insert(&mut self, mut range: Range<usize>) {
        if range.is_empty() {
            return;
        }
        assert!(
            self.0.capacity() > self.0.len(),
            "input coverage insertion requires batch reservation"
        );
        let first = self
            .0
            .partition_point(|existing| existing.end < range.start);
        let mut end = first;
        while let Some(existing) = self.0.get(end) {
            if existing.start > range.end {
                break;
            }
            range.start = range.start.min(existing.start);
            range.end = range.end.max(existing.end);
            end += 1;
        }
        self.0.splice(first..end, [range]);
    }

    pub(super) fn first_missing(&self, range: Range<usize>) -> Option<usize> {
        let mut cursor = range.start;
        let first = self.0.partition_point(|existing| existing.end <= cursor);
        for assigned in &self.0[first..] {
            if cursor >= range.end {
                return None;
            }
            if assigned.start > cursor {
                return Some(cursor);
            }
            cursor = cursor.max(assigned.end);
        }
        (cursor < range.end).then_some(cursor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coverage_retains_holes_and_merges_adjacent_actual_writes() {
        let mut assigned = InputPresence::default();
        assigned.reserve_insertions(4).unwrap();
        assigned.insert(5..8);
        assigned.insert(1..3);
        assert_eq!(assigned.first_missing(1..8), Some(3));
        assigned.insert(3..5);
        assert_eq!(assigned.first_missing(1..8), None);
        assert_eq!(assigned.first_missing(0..8), Some(0));
        assert_eq!(assigned.first_missing(1..9), Some(8));
        assert_eq!(assigned.0.as_slice(), std::slice::from_ref(&(1..8)));
        assigned.reserve_insertions(1).unwrap();
        assigned.insert(0..usize::MAX);
        assert_eq!(
            assigned.0.as_slice(),
            std::slice::from_ref(&(0..usize::MAX))
        );
    }
}
