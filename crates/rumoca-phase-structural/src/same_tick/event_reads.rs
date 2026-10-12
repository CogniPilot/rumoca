//! Ordinary event read facts issued once and retained through final scheduling.

use super::*;

/// The reads of one ordinary event body and its separate entry activation.
/// Only the checked definition owner can issue these facts.
#[derive(Clone)]
pub struct EventReadFacts<'dae> {
    current: BTreeSet<u32>,
    entry: BTreeSet<u32>,
    dependencies: BTreeMap<u32, BTreeSet<u32>>,
    brand: std::marker::PhantomData<dae::VariableId<'dae>>,
}

impl<'dae> EventReadFacts<'dae> {
    #[must_use]
    pub fn current(&self) -> &BTreeSet<u32> {
        &self.current
    }

    #[must_use]
    pub fn entry(&self) -> &BTreeSet<u32> {
        &self.entry
    }

    pub(super) fn dependencies(&self, variable: u32) -> Option<BTreeSet<u32>> {
        let mut pending = self
            .dependencies
            .get(&variable)?
            .iter()
            .copied()
            .collect::<Vec<_>>();
        let mut reads = BTreeSet::new();
        while let Some(read) = pending.pop() {
            if reads.insert(read)
                && let Some(dependencies) = self.dependencies.get(&read)
            {
                pending.extend(dependencies.iter().copied());
            }
        }
        Some(reads)
    }

    /// Merge facts for a proved independent fused group, without reevaluation.
    #[must_use]
    pub fn merge(first: &Self, rest: &[&Self]) -> Self {
        let mut combined = first.clone();
        for facts in rest {
            combined.current.extend(&facts.current);
            combined.entry.extend(&facts.entry);
            combined.dependencies.extend(facts.dependencies.clone());
        }
        combined
    }
}

/// The executable dependency context, distinct from a periodic task schedule.
#[derive(Clone)]
pub enum SameTickReadContext<'dae> {
    ClockPartition,
    OrdinaryEvent(EventReadFacts<'dae>),
}

impl<'dae> SameTickDefinitions<'dae> {
    /// Close the checked direct body and entry reads once at event construction.
    pub fn issue_event_read_facts(
        &self,
        view: dae::DaeView<'dae>,
        current: BTreeSet<u32>,
        entry: BTreeSet<u32>,
    ) -> Result<EventReadFacts<'dae>, SameTickOrderError> {
        let mut dependencies = BTreeMap::new();
        let current = self.visit_variable_read_closure(
            view,
            current,
            ReadRole::Current,
            |variable, reads| {
                dependencies.insert(variable, reads.clone());
            },
        )?;
        Ok(EventReadFacts {
            current,
            dependencies,
            entry: self.variable_read_closure(view, entry)?,
            brand: std::marker::PhantomData,
        })
    }
}

/// Preserve ordinary observation cycles while retaining mandatory entry edges.
pub(super) struct EventOrdering {
    observations: Vec<BTreeSet<usize>>,
    entry_readers: Vec<BTreeSet<usize>>,
}

impl EventOrdering {
    pub(super) fn derive(
        producers: &[SameTickProducer<'_>],
        producer_of: &BTreeMap<u32, usize>,
        direct_reads: &[BTreeSet<u32>],
        coincidence: &ClockCoincidence,
    ) -> Self {
        let direct = producers
            .iter()
            .enumerate()
            .map(|(consumer, producer)| {
                if !matches!(producer.read_context, SameTickReadContext::OrdinaryEvent(_)) {
                    return BTreeSet::new();
                }
                direct_reads[consumer]
                    .iter()
                    .filter_map(|read| {
                        let &owner = producer_of.get(read)?;
                        (owner != consumer
                            && coincidence.domains_overlap(
                                &producer.clock_domains,
                                &producers[owner].clock_domains,
                            )
                            && matches!(
                                producers[owner].read_context,
                                SameTickReadContext::OrdinaryEvent(_)
                            ))
                        .then_some(owner)
                    })
                    .collect()
            })
            .collect::<Vec<BTreeSet<usize>>>();
        let observations = (0..producers.len())
            .map(|start| {
                let mut seen = BTreeSet::new();
                let mut pending = direct[start].iter().copied().collect::<Vec<_>>();
                while let Some(next) = pending.pop() {
                    if seen.insert(next) {
                        pending.extend(direct[next].iter().copied());
                    }
                }
                seen
            })
            .collect();
        let mut entry_readers = vec![BTreeSet::new(); producers.len()];
        for (reader, producer) in producers.iter().enumerate() {
            let SameTickReadContext::OrdinaryEvent(facts) = &producer.read_context else {
                continue;
            };
            for read in facts.entry() {
                if let Some(&owner) = producer_of.get(read)
                    && owner != reader
                    && coincidence
                        .domains_overlap(&producer.clock_domains, &producers[owner].clock_domains)
                {
                    entry_readers[owner].insert(reader);
                }
            }
        }
        Self {
            observations,
            entry_readers,
        }
    }

    pub(super) fn requires_current(&self, consumer: usize, producer: usize) -> bool {
        !self.entry_readers[producer].contains(&consumer)
            && !(self.observations[consumer].contains(&producer)
                && self.observations[producer].contains(&consumer))
    }

    pub(super) fn entry_readers(&self, producer: usize) -> &BTreeSet<usize> {
        &self.entry_readers[producer]
    }

    pub(super) fn earlier_observation_members(
        &self,
        producer: usize,
    ) -> impl Iterator<Item = usize> + '_ {
        (0..producer).filter(move |&earlier| {
            self.observations[producer].contains(&earlier)
                && self.observations[earlier].contains(&producer)
        })
    }
}
