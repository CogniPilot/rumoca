//! The ready rows of a causal refresh ordering, batched by source program.

use std::collections::{BTreeMap, VecDeque};

use rumoca_ir_solve as solve;

/// Ready rows, released one source program at a time: while a ready row
/// shares the last released row's source it goes next, otherwise sources are
/// served in the order they first became ready. Any release order of ready
/// rows is a valid causal order; keeping a source's rows adjacent lets one
/// exact program commit them together, so a wide call program is issued and
/// evaluated once rather than once per row.
#[derive(Default)]
pub(super) struct SourceBatchedQueue {
    by_source: BTreeMap<solve::RefreshScalarProgramSource, VecDeque<usize>>,
    sources: VecDeque<solve::RefreshScalarProgramSource>,
    current: Option<solve::RefreshScalarProgramSource>,
}

impl SourceBatchedQueue {
    pub(super) fn push(&mut self, source: solve::RefreshScalarProgramSource, row: usize) {
        let queue = self.by_source.entry(source).or_default();
        if queue.is_empty() && self.current != Some(source) {
            self.sources.push_back(source);
        }
        queue.push_back(row);
    }

    pub(super) fn pop(&mut self) -> Option<usize> {
        loop {
            if let Some(row) = self
                .current
                .and_then(|source| self.by_source.get_mut(&source)?.pop_front())
            {
                return Some(row);
            }
            let next = self.sources.pop_front()?;
            self.current = Some(next);
        }
    }
}
