//! First-read timestamps retain exact prefixes without cumulative snapshots.

use super::*;
use std::collections::BTreeMap;
use std::ops::RangeInclusive;

#[derive(Default)]
pub(in crate::structural_pattern) struct ExecutionPrefixBuilder {
    at: usize,
    // Coalesced coverage bounds repeated overlapping reads independently of
    // the number of first-read timestamps inside an already covered range.
    covered: BTreeMap<usize, usize>,
    first_reads: BTreeMap<usize, (usize, usize)>,
}

impl ExecutionPrefixBuilder {
    pub(super) fn include(&mut self, state: DependencyState) {
        for range in state.into_shared().intervals() {
            self.include_interval(*range.start(), *range.end());
        }
    }

    fn include_interval(&mut self, start: usize, last: usize) {
        let first = self
            .covered
            .range(..=start)
            .next_back()
            .filter(|(_, end)| end.saturating_add(1) >= start)
            .map_or(start, |(&begin, _)| begin);
        let overlaps = self
            .covered
            .range(first..=last.saturating_add(1))
            .map(|(&begin, &end)| (begin, end))
            .collect::<Vec<_>>();
        let mut cursor = Some(start);
        let mut merged_last = last;
        for &(begin, end) in &overlaps {
            if let Some(next) = cursor.filter(|next| *next < begin) {
                self.first_reads.insert(next, (begin - 1, self.at));
            }
            cursor = cursor.and_then(|next| end.checked_add(1).map(|after| after.max(next)));
            merged_last = merged_last.max(end);
        }
        if let Some(next) = cursor.filter(|next| *next <= last) {
            self.first_reads.insert(next, (last, self.at));
        }
        for (begin, _) in overlaps {
            self.covered.remove(&begin);
        }
        self.covered.insert(first, merged_last);
    }
}

/// Source-derived execution reads at every valid prefix. Each coordinate
/// interval is retained once, labeled by its first read. Disjoint new guards
/// therefore retain linear metadata rather than cumulative prefix snapshots.
/// Construction visits compact input intervals, never their scalar members.
#[derive(Clone)]
pub struct ScalarProgramExecutionYDependency {
    source_len: usize,
    valid_len: usize,
    first_reads: Box<[(usize, usize, usize)]>,
}

impl ScalarProgramExecutionYDependency {
    pub fn new(program: &[LinearOp]) -> Self {
        let obligations = RefCell::new(ExecutionPrefixBuilder::default());
        let mut walk = DependencyWalk {
            registers: DependencyRegisters::default(),
            outputs: Vec::new(),
            retain_outputs: false,
            span: None,
            fold_carried: None,
            fold_captures: None,
            conditional_captures: None,
            source: DependencySource::SolverY,
            policy: DependencyPolicy::ExecutionPrefixes(&obligations),
        };
        let mut valid_len = 0;
        for (index, operation) in program.iter().enumerate() {
            obligations.borrow_mut().at = index + 1;
            if apply_dependency_op(&mut walk, operation).is_err() {
                break;
            }
            valid_len = index + 1;
        }
        let first_reads = obligations
            .into_inner()
            .first_reads
            .into_iter()
            .filter(|(_, (_, at))| *at <= valid_len)
            .map(|(start, (last, at))| (start, last, at))
            .collect();
        Self {
            source_len: program.len(),
            valid_len,
            first_reads,
        }
    }

    /// `None` when the requested prefix includes an operation the walk refused.
    pub fn prefix(&self, len: usize) -> Option<ExecutionYReadPrefix<'_>> {
        (len <= self.source_len && len <= self.valid_len)
            .then_some(ExecutionYReadPrefix { proof: self, len })
    }
}

/// Borrowed prefix view; querying does not copy or retain another inventory.
#[derive(Clone, Copy)]
pub struct ExecutionYReadPrefix<'a> {
    proof: &'a ScalarProgramExecutionYDependency,
    len: usize,
}

impl ExecutionYReadPrefix<'_> {
    pub fn contains(&self, index: usize) -> bool {
        self.proof
            .first_reads
            .partition_point(|&(start, _, _)| start <= index)
            .checked_sub(1)
            .is_some_and(|position| {
                let (_, last, at) = self.proof.first_reads[position];
                index <= last && at <= self.len
            })
    }

    pub fn intervals(&self) -> impl Iterator<Item = RangeInclusive<usize>> + '_ {
        self.proof
            .first_reads
            .iter()
            .filter(|(_, _, at)| *at <= self.len)
            .map(|&(start, last, _)| start..=last)
    }

    pub fn iter(&self) -> impl Iterator<Item = usize> + '_ {
        self.intervals().flatten()
    }

    pub fn is_empty(&self) -> bool {
        self.intervals().next().is_none()
    }

    /// Union compact intervals without scalar expansion or a retained snapshot.
    pub fn union_with(&self, values: &IndexIntervals) -> IndexIntervals {
        IndexIntervals::of_intervals(self.intervals().chain(values.intervals()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn disjoint_guard_prefixes_retain_each_interval_once() {
        let site = crate::structural_pattern::execution_reads_tests::guarded_site();
        let mut source = vec![LinearOp::Const { dst: 0, value: 1.0 }];
        for index in 0..256 {
            source.extend([
                LinearOp::LoadY {
                    dst: 1,
                    index: index * 2,
                },
                LinearOp::PureCall {
                    dst_start: 2,
                    input_starts: vec![0, 1].into(),
                    site: site.clone(),
                },
            ]);
        }
        source.push(LinearOp::Move { dst: 4, src: 99 });
        let proof = ScalarProgramExecutionYDependency::new(&source);
        assert_eq!(
            proof.first_reads.len(),
            256,
            "no cumulative prefix snapshots"
        );
        for count in 0..=256 {
            let prefix = proof.prefix(1 + count * 2).unwrap();
            assert_eq!(
                prefix.iter().collect::<Vec<_>>(),
                (0..count).map(|i| i * 2).collect::<Vec<_>>()
            );
            assert!(
                !prefix.contains(count * 2),
                "future guards do not enter earlier prefixes"
            );
        }
        assert!(proof.prefix(source.len()).is_none());
    }

    #[test]
    fn overlapping_wide_reads_keep_first_timestamps_without_scalar_expansion() {
        let mut builder = ExecutionPrefixBuilder::default();
        for index in 0..256 {
            builder.at = index + 1;
            builder.include(DependencyState::singleton(index * 2));
        }
        builder.at = 257;
        builder.include_interval(0, usize::MAX);
        assert_eq!(builder.covered.len(), 1);
        assert_eq!(builder.first_reads.len(), 512);
        for _ in 0..256 {
            builder.include_interval(0, usize::MAX);
        }
        assert_eq!(
            builder.first_reads.len(),
            512,
            "covered ranges add no retained metadata"
        );
        let proof = ScalarProgramExecutionYDependency {
            source_len: 257,
            valid_len: 257,
            first_reads: builder
                .first_reads
                .into_iter()
                .map(|(a, (b, at))| (a, b, at))
                .collect(),
        };
        assert!(!proof.prefix(256).unwrap().contains(usize::MAX));
        assert!(proof.prefix(257).unwrap().contains(usize::MAX));
        assert_eq!(
            proof
                .prefix(257)
                .unwrap()
                .union_with(&IndexIntervals::default())
                .intervals()
                .collect::<Vec<_>>(),
            [0..=usize::MAX]
        );
    }
}
