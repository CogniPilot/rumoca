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
            for (variable, reads) in &facts.dependencies {
                combined
                    .dependencies
                    .entry(*variable)
                    .or_default()
                    .extend(reads);
            }
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

/// A source-root collection that retains each exact projected alias adjacency.
/// Fields stay private: only the checked definition owner issues read facts.
pub struct EventReadCollector<'owner, 'dae> {
    definitions: &'owner SameTickDefinitions<'dae>,
    view: dae::DaeView<'dae>,
    current: reads::ReadSeeds,
    entry: reads::ReadSeeds,
}

impl<'dae> EventReadCollector<'_, 'dae> {
    pub fn expression(&mut self, expression: dae::ExprId<'dae>) -> Result<(), SameTickOrderError> {
        self.current.merge(reads::expression_read_seeds(
            self.view,
            expression,
            &self.definitions.derivative_sources,
            Some(self.definitions),
            &|_| false,
        )?);
        Ok(())
    }

    pub fn condition(
        &mut self,
        condition: dae::ConditionId<'dae>,
        excluded: &BTreeSet<dae::ConditionId<'dae>>,
    ) -> Result<(), SameTickOrderError> {
        self.condition_seeds(condition, excluded, &|_| false, false)
    }

    pub fn entry_condition(
        &mut self,
        condition: dae::ConditionId<'dae>,
        projected: impl Fn(dae::ExprId<'dae>) -> bool,
    ) -> Result<(), SameTickOrderError> {
        self.condition_seeds(condition, &BTreeSet::new(), &projected, true)
    }

    fn condition_seeds(
        &mut self,
        condition: dae::ConditionId<'dae>,
        excluded: &BTreeSet<dae::ConditionId<'dae>>,
        projected: &impl Fn(dae::ExprId<'dae>) -> bool,
        entry: bool,
    ) -> Result<(), SameTickOrderError> {
        reads::visit_condition_expressions(self.view, condition, excluded, |expression| {
            let seeds = reads::expression_read_seeds(
                self.view,
                expression,
                &self.definitions.derivative_sources,
                Some(self.definitions),
                projected,
            )?;
            if entry {
                self.entry.merge(seeds);
            } else {
                self.current.merge(seeds);
            }
            Ok(())
        })
    }

    pub fn finish(self) -> Result<EventReadFacts<'dae>, SameTickOrderError> {
        let mut dependencies = BTreeMap::new();
        let current = self.definitions.visit_read_seed_closure(
            self.view,
            self.current,
            ReadRole::Current,
            |variable, reads| {
                dependencies.insert(variable, reads.clone());
            },
        )?;
        let entry = self.definitions.visit_read_seed_closure(
            self.view,
            self.entry,
            ReadRole::Current,
            |_, _| {},
        )?;
        Ok(EventReadFacts {
            current,
            entry,
            dependencies,
            brand: std::marker::PhantomData,
        })
    }
}

impl<'dae> SameTickDefinitions<'dae> {
    pub fn event_read_collector(&self, view: dae::DaeView<'dae>) -> EventReadCollector<'_, 'dae> {
        EventReadCollector {
            definitions: self,
            view,
            current: reads::ReadSeeds::default(),
            entry: reads::ReadSeeds::default(),
        }
    }

    /// Close declaration seeds, retaining the whole-value meaning of each seed.
    #[cfg(test)]
    pub(crate) fn issue_event_read_facts(
        &self,
        view: dae::DaeView<'dae>,
        current: BTreeSet<u32>,
        entry: BTreeSet<u32>,
    ) -> Result<EventReadFacts<'dae>, SameTickOrderError> {
        EventReadCollector {
            definitions: self,
            view,
            current: reads::ReadSeeds::raw(current),
            entry: reads::ReadSeeds::raw(entry),
        }
        .finish()
    }
}

/// Retain mandatory entry edges in the periodic same-tick schedule.
pub(super) struct EventOrdering {
    entry_readers: Vec<BTreeSet<usize>>,
}

impl EventOrdering {
    pub(super) fn derive(
        producers: &[SameTickProducer<'_>],
        producer_of: &BTreeMap<u32, usize>,
        coincidence: &ClockCoincidence,
    ) -> Self {
        let mut entry_readers = vec![BTreeSet::new(); producers.len()];
        for (reader, producer) in producers.iter().enumerate() {
            let SameTickReadContext::OrdinaryEvent(facts) = &producer.read_context else {
                continue;
            };
            let owners = facts
                .entry()
                .iter()
                .filter_map(|read| producer_of.get(read).copied());
            for owner in owners.filter(|&owner| {
                owner != reader
                    && coincidence
                        .domains_overlap(&producer.clock_domains, &producers[owner].clock_domains)
            }) {
                entry_readers[owner].insert(reader);
            }
        }
        Self { entry_readers }
    }

    pub(super) fn requires_current(&self, consumer: usize, producer: usize) -> bool {
        !self.entry_readers[producer].contains(&consumer)
    }

    pub(super) fn entry_readers(&self, producer: usize) -> &BTreeSet<usize> {
        &self.entry_readers[producer]
    }
}
