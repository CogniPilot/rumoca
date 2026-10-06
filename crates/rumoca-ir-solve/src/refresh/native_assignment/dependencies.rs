//! Exact original-order dependencies with bounded-span candidate discovery.

use std::collections::BTreeSet;

use super::{Checked, Family, refused, span_index::SpanIndex};

pub(super) fn order(families: &[Family]) -> Checked<Vec<usize>> {
    let index = SpanIndex::new(
        families
            .iter()
            .map(|family| family.stage.targets.span.clone()),
    );
    let mut candidates = CandidateOrdinals::new(families.len());
    let mut remaining = Vec::with_capacity(families.len());
    let mut dependents = vec![Vec::new(); families.len()];
    // Materialize one edge orientation only. A checked consumer's temporary
    // incoming vector is discarded after populating its producers' outgoing
    // lists; genuinely dense graphs still retain every original edge.
    for (consumer, family) in families.iter().enumerate() {
        let dependencies = collect(family, families, &index, &mut candidates)?;
        remaining.push(dependencies.len());
        for producer in dependencies {
            dependents[producer].push(consumer);
        }
    }
    let mut ready = remaining
        .iter()
        .enumerate()
        .filter_map(|(index, &count)| (count == 0).then_some(index))
        .collect::<BTreeSet<_>>();
    let mut order = Vec::with_capacity(families.len());
    while let Some(next) = ready.pop_first() {
        order.push(next);
        for &consumer in &dependents[next] {
            remaining[consumer] -= 1;
            if remaining[consumer] == 0 {
                ready.insert(consumer);
            }
        }
    }
    if order.len() != families.len() {
        return refused("native assignment dependency cycle");
    }
    Ok(order)
}

/// Ordinals belong to this immutable family inventory, not the Y layout.
/// Reset only touched ordinals, avoiding a full-inventory scan per consumer.
pub(super) struct CandidateOrdinals {
    present: Vec<bool>,
    touched: Vec<usize>,
}

impl CandidateOrdinals {
    pub(super) fn new(count: usize) -> Self {
        Self {
            present: vec![false; count],
            touched: Vec::new(),
        }
    }

    fn reset(&mut self) {
        for &producer in &self.touched {
            self.present[producer] = false;
        }
        self.touched.clear();
    }

    fn insert(&mut self, producer: usize) {
        if !self.present[producer] {
            self.present[producer] = true;
            self.touched.push(producer);
        }
    }

    fn complete(&self) -> bool {
        self.touched.len() == self.present.len()
    }
}

pub(super) fn collect(
    family: &Family,
    families: &[Family],
    index: &SpanIndex,
    candidates: &mut CandidateOrdinals,
) -> Checked<Vec<usize>> {
    candidates.reset();
    for read in &family.reads {
        if candidates.complete() {
            break;
        }
        index.visit(&read.span, &mut |producer| candidates.insert(producer));
    }
    candidates.touched.sort_unstable();
    let mut dependencies = Vec::new();
    // Discovery can stop once every producer is a candidate. The old
    // producer/read traversal still determines refusal precedence: even after
    // an overlap, every subsequent read must be checked by the exact authority.
    for &producer in &candidates.touched {
        let mut overlaps = false;
        for read in &family.reads {
            overlaps |= read.overlaps(&families[producer].stage.targets)?;
        }
        if overlaps {
            dependencies.push(producer);
        }
    }
    Ok(dependencies)
}
