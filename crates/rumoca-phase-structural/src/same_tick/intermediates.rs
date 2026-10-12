//! Consumer-bound intermediate refresh discovery.

use super::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct IntermediateKey {
    pub(super) variable: u32,
    pub(super) role: ReadRole,
    pub(super) current_consumer: Option<usize>,
}

fn intermediate_reads<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &SameTickDefinitions<'dae>,
    producer: &SameTickProducer<'dae>,
    variable: u32,
) -> Option<(Option<dae::ExprId<'dae>>, BTreeSet<u32>)> {
    let definition = definitions.definition(variable);
    let reads = match &producer.read_context {
        SameTickReadContext::OrdinaryEvent(facts) => facts.dependencies(variable),
        SameTickReadContext::ClockPartition => match definition {
            Some(definition) => {
                let mut reads = BTreeSet::new();
                collect_same_instant_reads(view, definition, &mut reads);
                Some(reads)
            }
            None => definitions
                .opaque
                .get(&variable)
                .map(|reads| reads.clock_partition.clone()),
        },
    };
    reads.map(|reads| (definition, reads))
}

fn discover_read_node<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &SameTickDefinitions<'dae>,
    producer: &SameTickProducer<'dae>,
    consumer: usize,
    variable: u32,
) -> Option<(IntermediateKey, Option<dae::ExprId<'dae>>, BTreeSet<u32>)> {
    let role = match &producer.read_context {
        SameTickReadContext::ClockPartition => ReadRole::ClockPartition,
        SameTickReadContext::OrdinaryEvent(_) => ReadRole::Current,
    };
    if role == ReadRole::Current
        && definitions.variable_id(variable).is_some_and(|id| {
            view.variable(id)
                .is_some_and(|node| matches!(node.identity(), dae::VariableIdentity::Algebraic(_)))
        })
        && definitions.definition(variable).is_some()
    {
        // Ordinary bodies compile this exact algebraic definition under their
        // current DefinitionOwner. The forwarded closure already carries its
        // terminal producer reads, so no clock-boundary refresh is issued.
        return None;
    }
    let (definition, reads) = intermediate_reads(view, definitions, producer, variable)?;
    Some((
        IntermediateKey {
            variable,
            role,
            current_consumer: (role == ReadRole::Current).then_some(consumer),
        },
        definition,
        reads,
    ))
}

/// One discovered non-producer coordinate on a same-tick path.
pub(super) struct IntermediateNode<'dae> {
    pub(super) definition: Option<dae::ExprId<'dae>>,
    pub(super) reads: BTreeSet<u32>,
    pub(super) consumers: BTreeSet<usize>,
}

/// Discover the coordinates whose values sit on a same-tick path into a
/// producer target, together with each one's own direct reads.
///
/// Only coordinates that transitively reach a producer target are kept: those
/// are the storage coordinates whose event-entry values go stale mid-tick. A
/// kept coordinate with no exact definition cannot be refreshed, so it fails
/// construction instead of silently degrading to the pre-C57 stale read.
pub(super) fn discover_intermediates<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &SameTickDefinitions<'dae>,
    producer_of: &BTreeMap<u32, usize>,
    excluded: &BTreeSet<u32>,
    direct_reads: &[BTreeSet<u32>],
    producers: &[SameTickProducer<'dae>],
) -> Result<BTreeMap<IntermediateKey, IntermediateNode<'dae>>, SameTickOrderError> {
    let mut discovered = BTreeMap::<IntermediateKey, IntermediateNode<'dae>>::new();
    let mut pending: Vec<(usize, u32)> = direct_reads
        .iter()
        .enumerate()
        .flat_map(|(consumer, reads)| reads.iter().map(move |&read| (consumer, read)))
        .collect();
    let mut visited = BTreeSet::new();
    while let Some((consumer, variable)) = pending.pop() {
        if !visited.insert((consumer, variable))
            || producer_of.contains_key(&variable)
            || excluded.contains(&variable)
        {
            continue;
        }
        let Some((key, definition, reads)) =
            discover_read_node(view, definitions, &producers[consumer], consumer, variable)
        else {
            continue;
        };
        pending.extend(reads.iter().map(|&read| (consumer, read)));
        let node = discovered.entry(key).or_insert_with(|| IntermediateNode {
            definition,
            reads,
            consumers: BTreeSet::new(),
        });
        node.consumers.insert(consumer);
    }
    let mut reach = BTreeMap::<IntermediateKey, bool>::new();
    let reaches_producer: BTreeMap<IntermediateKey, bool> = discovered
        .keys()
        .copied()
        .collect::<Vec<_>>()
        .into_iter()
        .map(|variable| {
            let mut guard = BTreeSet::new();
            let value =
                reaches_producer_target(variable, producer_of, &discovered, &mut reach, &mut guard);
            (variable, value)
        })
        .collect();
    let mut tick_reach = BTreeMap::<IntermediateKey, bool>::new();
    let reaches_tick_refresh: BTreeMap<IntermediateKey, bool> = discovered
        .keys()
        .copied()
        .collect::<Vec<_>>()
        .into_iter()
        .map(|variable| {
            let mut guard = BTreeSet::new();
            let value = reaches_tick_refresh_definition(
                variable,
                definitions,
                &discovered,
                &mut tick_reach,
                &mut guard,
            );
            (variable, value)
        })
        .collect();
    discovered.retain(|variable, _| {
        reaches_producer.get(variable).copied().unwrap_or(false)
            || reaches_tick_refresh.get(variable).copied().unwrap_or(false)
    });
    if let Some((variable, _)) = discovered
        .iter()
        .find(|(_, node)| node.definition.is_none())
    {
        return Err(unrefreshable_read(
            view,
            producers,
            direct_reads,
            variable.variable,
        ));
    }
    Ok(discovered)
}

/// Whether this definition sits between a producer read and an always-active
/// discrete definition whose stored value must be recomputed for this tick.
fn reaches_tick_refresh_definition(
    variable: IntermediateKey,
    definitions: &SameTickDefinitions<'_>,
    discovered: &BTreeMap<IntermediateKey, IntermediateNode<'_>>,
    reach: &mut BTreeMap<IntermediateKey, bool>,
    guard: &mut BTreeSet<IntermediateKey>,
) -> bool {
    if definitions.refreshes_on_tick(variable.variable) {
        return true;
    }
    if let Some(&known) = reach.get(&variable) {
        return known;
    }
    if !guard.insert(variable) {
        return false;
    }
    let result = discovered.get(&variable).is_some_and(|node| {
        node.reads.iter().any(|&read| {
            reaches_tick_refresh_definition(
                IntermediateKey {
                    variable: read,
                    role: variable.role,
                    current_consumer: variable.current_consumer,
                },
                definitions,
                discovered,
                reach,
                guard,
            )
        })
    });
    guard.remove(&variable);
    reach.insert(variable, result);
    result
}

/// Report a read this module cannot refresh, at the span of a producer that
/// observes it (directly when one does, otherwise the first producer).
fn unrefreshable_read<'dae>(
    view: dae::DaeView<'dae>,
    producers: &[SameTickProducer<'dae>],
    direct_reads: &[BTreeSet<u32>],
    variable: u32,
) -> SameTickOrderError {
    let span = producers
        .iter()
        .zip(direct_reads)
        .find_map(|(producer, reads)| reads.contains(&variable).then_some(producer.span))
        .unwrap_or(producers[0].span);
    SameTickOrderError::UnrefreshableRead {
        span,
        name: variable_name(view, variable),
        detail: "it is written by an owner with no exact unconditional definition, yet its value \
             depends on a producer of this clock partition; write the read through \
             pre()/previous() or give the coordinate a single unconditional definition"
            .to_owned(),
    }
}

fn reaches_producer_target(
    variable: IntermediateKey,
    producer_of: &BTreeMap<u32, usize>,
    discovered: &BTreeMap<IntermediateKey, IntermediateNode<'_>>,
    reach: &mut BTreeMap<IntermediateKey, bool>,
    guard: &mut BTreeSet<IntermediateKey>,
) -> bool {
    if producer_of.contains_key(&variable.variable) {
        return true;
    }
    if let Some(&known) = reach.get(&variable) {
        return known;
    }
    if !guard.insert(variable) {
        return false;
    }
    let result = discovered.get(&variable).is_some_and(|node| {
        node.reads.iter().any(|&read| {
            reaches_producer_target(
                IntermediateKey {
                    variable: read,
                    role: variable.role,
                    current_consumer: variable.current_consumer,
                },
                producer_of,
                discovered,
                reach,
                guard,
            )
        })
    });
    guard.remove(&variable);
    reach.insert(variable, result);
    result
}
