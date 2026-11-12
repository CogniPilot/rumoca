//! Conservative region read sets, retaining coordinate subsets of captures.
use super::substitution::{Read, Substitution};
use super::*;
use crate::SolveProgramRegion;

mod pointwise;

#[derive(Clone, Copy)]
pub(super) struct Context<'a> {
    pub body: &'a TypedProgram,
    pub available: SolvePureCallTableView<'a>,
    pub provenance: Span,
}

pub(super) fn derive(
    context: Context<'_>,
    operation: &SolveOperation,
    registers: &mut [Vec<SolveCallDependency>],
) -> Result<bool, SolveProgramConstructionError> {
    match operation {
        SolveOperation::Conditional {
            condition,
            captures,
            destinations,
            if_true,
            if_false,
        } => conditional(
            context,
            *condition,
            captures,
            destinations,
            [if_true, if_false],
            registers,
        )?,
        SolveOperation::Fold {
            initial,
            captures,
            state_captures,
            destinations,
            transition,
            continuation,
            ..
        } if state_captures.is_empty() => fold(
            context,
            initial,
            captures,
            destinations,
            transition,
            continuation.as_deref(),
            registers,
        )?,
        _ => return Ok(false),
    }
    Ok(true)
}

fn merge(out: &mut Dependencies, dependencies: Vec<SolveCallDependency>) {
    for dependency in dependencies {
        out.insert(dependency);
    }
}

fn region_summaries(
    region: &SolveProgramRegion,
    available: SolvePureCallTableView<'_>,
) -> Result<Box<[Box<[SolveCallDependency]>]>, SolveProgramConstructionError> {
    super::derive(
        region.body(),
        region.inputs().len(),
        region.outputs().len(),
        available,
    )
}

fn insert_operand(
    out: &mut Dependencies,
    sources: &[SolveCallDependency],
    output_rank: usize,
    operand: &SolveValueType,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let access = operations::whole_access(output_rank, operand.dimensions());
    for source in sources {
        out.insert(source.remap_may_depend(&access, provenance)?);
    }
    Ok(())
}

/// The registers a region operation reads, with the substitution all its
/// destinations share.
struct Reads<'a> {
    registers: &'a [Vec<SolveCallDependency>],
    substitution: Substitution,
}

/// Substitute readonly captures; carried inputs and static binders are handled
/// separately by the fold closure, never mistaken for readonly capture leaves.
fn capture_dependencies(
    context: Context<'_>,
    captures: &[SolveRegisterId],
    offset: usize,
    summary: &[SolveCallDependency],
    rank: usize,
    consume: Option<&Coordinates>,
    reads: &mut Reads<'_>,
) -> Result<Vec<SolveCallDependency>, SolveProgramConstructionError> {
    let error = || SolveProgramConstructionError::InvalidCallInterface {
        provenance: context.provenance,
    };
    let mut captured = Vec::new();
    for dependency in summary {
        for leaf in dependency.inputs() {
            let Some(capture) = leaf.checked_sub(offset).and_then(|i| captures.get(i)) else {
                continue;
            };
            let access = match (&dependency.coordinates, consume) {
                (Some(access), Some(consume)) => access.compose(consume).ok_or_else(error)?,
                (Some(access), None) => access.clone(),
                (None, _) => operations::whole_access(
                    rank,
                    context.body.register_types()[capture.index()].dimensions(),
                ),
            };
            captured.push((capture.index(), access));
        }
    }
    reads.substitution.output(
        captured.iter().map(|(register, access)| Read {
            register: *register,
            access: Some(access),
        }),
        reads.registers,
        |source, access| may_depend_coordinates(source, access, context.provenance),
        error,
    )
}

pub(super) fn conditional(
    context: Context<'_>,
    condition: SolveRegisterId,
    captures: &[SolveRegisterId],
    destinations: &[SolveRegisterId],
    branches: [&SolveProgramRegion; 2],
    registers: &mut [Vec<SolveCallDependency>],
) -> Result<(), SolveProgramConstructionError> {
    let summaries = [
        region_summaries(branches[0], context.available)?,
        region_summaries(branches[1], context.available)?,
    ];
    let mut results = Vec::new();
    let mut reads = Reads {
        registers,
        substitution: Substitution::default(),
    };
    for (index, destination) in destinations.iter().enumerate() {
        let rank = context.body.register_types()[destination.index()]
            .dimensions()
            .len();
        let mut out = Dependencies::default();
        insert_operand(
            &mut out,
            &reads.registers[condition.index()],
            rank,
            &context.body.register_types()[condition.index()],
            context.provenance,
        )?;
        for branch in &summaries {
            for dependency in
                capture_dependencies(context, captures, 0, &branch[index], rank, None, &mut reads)?
            {
                out.insert(dependency);
            }
        }
        results.push(out.finish());
    }
    for (destination, result) in destinations.iter().zip(results) {
        registers[destination.index()] = result;
    }
    Ok(())
}

pub(super) fn fold(
    context: Context<'_>,
    initial: &[SolveRegisterId],
    captures: &[SolveRegisterId],
    destinations: &[SolveRegisterId],
    transition: &SolveProgramRegion,
    continuation: Option<&SolveProgramRegion>,
    registers: &mut [Vec<SolveCallDependency>],
) -> Result<(), SolveProgramConstructionError> {
    let mut regions = vec![(transition, region_summaries(transition, context.available)?)];
    if let Some(region) = continuation {
        regions.push((region, region_summaries(region, context.available)?));
    }
    let mut results = Vec::new();
    let mut reads = Reads {
        registers,
        substitution: Substitution::default(),
    };
    for (index, destination) in destinations.iter().enumerate() {
        if continuation.is_none()
            && let Some(result) = pointwise::fold_output(
                context,
                initial,
                captures,
                transition,
                &regions[0].1[index],
                index,
                &mut reads,
            )?
        {
            results.push(result);
            continue;
        }
        let rank = context.body.register_types()[destination.index()]
            .dimensions()
            .len();
        let mut out = Dependencies::default();
        // A carried component reaches this destination only through the carries
        // its transition reads, transitively, so only those initial values and
        // only the capture reads of those updates can flow into it.
        let reached = carried_reach(index, initial.len(), &regions);
        for (component, operand) in initial.iter().enumerate() {
            if !reached[component] {
                continue;
            }
            insert_operand(
                &mut out,
                &reads.registers[operand.index()],
                rank,
                &context.body.register_types()[operand.index()],
                context.provenance,
            )?;
        }
        for (position, (region, summaries)) in regions.iter().enumerate() {
            for (component, (output, summary)) in
                region.outputs().iter().zip(summaries.iter()).enumerate()
            {
                if position == 0 && reached.get(component) == Some(&false) {
                    continue;
                }
                let consume = operations::whole_access(rank, output.dimensions());
                merge(
                    &mut out,
                    capture_dependencies(
                        context,
                        captures,
                        initial.len(),
                        summary,
                        rank,
                        Some(&consume),
                        &mut reads,
                    )?,
                );
            }
        }
        results.push(out.finish());
    }
    for (destination, result) in destinations.iter().zip(results) {
        registers[destination.index()] = result;
    }
    Ok(())
}

/// The carried components whose values can flow into carry `selected` over
/// any number of passes: the smallest set that contains it and every carry an
/// early-exit predicate reads, closed under "the transition output of `c`
/// reads carried input `j`". Region inputs below `carries` are the carried
/// values of the pass; any other leaf is a readonly capture.
///
/// After zero passes a carry is its initial value; after a pass it is a
/// function of the carried inputs its transition output reads, so by induction
/// it depends only on the initial values of this set and on the captures the
/// set's transition outputs read. The fixpoint visits each carry once.
fn carried_reach(
    selected: usize,
    carries: usize,
    regions: &[(&SolveProgramRegion, Box<[Box<[SolveCallDependency]>]>)],
) -> Vec<bool> {
    let carried_reads = |summary: &[SolveCallDependency]| {
        summary
            .iter()
            .flat_map(SolveCallDependency::inputs)
            .filter(|&leaf| leaf < carries)
            .collect::<Vec<_>>()
    };
    let mut reached = vec![false; carries];
    let mut pending = vec![selected];
    for (_, summaries) in regions.iter().skip(1) {
        for summary in summaries.iter() {
            pending.extend(carried_reads(summary));
        }
    }
    while let Some(component) = pending.pop() {
        let Some(seen) = reached.get_mut(component) else {
            continue;
        };
        if std::mem::replace(seen, true) {
            continue;
        }
        if let Some(summary) = regions.first().and_then(|(_, summaries)| summaries.get(component)) {
            pending.extend(carried_reads(summary));
        }
    }
    reached
}
