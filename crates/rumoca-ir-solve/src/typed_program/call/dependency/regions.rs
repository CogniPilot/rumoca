//! Conservative region read sets, retaining coordinate subsets of captures.
use super::substitution::{Read, Substitution};
use super::*;
use crate::SolveProgramRegion;
use crate::typed_program::call::execution_reads::ExecutionReadBuilder;

mod passes;
mod pointwise;

pub(super) use passes::FOLLOWED_WORK_BUDGET;
#[cfg(test)]
pub(super) use passes::{FOLLOWED, FOLLOWED_PASS_BUDGET, FollowedWork};

#[derive(Clone, Copy)]
pub(super) struct Context<'a> {
    pub body: &'a TypedProgram,
    pub available: SolvePureCallTableView<'a>,
    pub provenance: Span,
    /// The operations followed passes of this owner's derivation may still
    /// walk; see [`FOLLOWED_WORK_BUDGET`].
    pub work: &'a std::cell::Cell<usize>,
}

/// Derive a branch or a fold, charging the obligations its regions can meet
/// to `execution`; `false` for any other operation.
pub(super) fn derive(
    context: Context<'_>,
    operation: &SolveOperation,
    integers: &mut map_access::IntegerValues,
    registers: &mut [Vec<SolveCallDependency>],
    execution: &mut ExecutionReadBuilder,
) -> Result<bool, SolveProgramConstructionError> {
    match operation {
        SolveOperation::Conditional {
            condition,
            captures,
            destinations,
            if_true,
            if_false,
        } => {
            if let Some(obligations) = conditional(
                context,
                *condition,
                captures,
                destinations,
                [if_true, if_false],
                integers,
                registers,
            )? {
                execution.region(true, obligations);
            }
        }
        SolveOperation::Fold {
            domain,
            initial,
            captures,
            state_captures,
            destinations,
            transition,
            continuation,
        } if state_captures.is_empty() => {
            let values = fold(
                context,
                Fold {
                    domain,
                    initial,
                    captures,
                    destinations,
                    transition,
                    continuation: continuation.as_deref(),
                },
                integers,
                registers,
                execution,
            )?;
            for (destination, value) in destinations.iter().zip(values) {
                integers.set(*destination, value);
            }
        }
        _ => return Ok(false),
    }
    Ok(true)
}

fn merge(out: &mut Dependencies, dependencies: Vec<SolveCallDependency>) {
    for dependency in dependencies {
        out.insert(dependency);
    }
}

/// The summaries and obligations of a region over every execution whose
/// leading inputs hold the `known` Integer values.
fn region_execution(
    region: &SolveProgramRegion,
    context: Context<'_>,
    known: &[Option<i64>],
) -> Result<super::Followed, SolveProgramConstructionError> {
    super::derive_execution(
        region.body(),
        region.inputs().len(),
        region.outputs().len(),
        context.available,
        known,
        context.work,
    )
}

/// The obligation reads of every element of each of `values`.
fn whole_obligations(
    context: Context<'_>,
    values: &[SolveRegisterId],
    registers: &[Vec<SolveCallDependency>],
) -> Result<Vec<SolveCallDependency>, SolveProgramConstructionError> {
    let mut obligations = Vec::new();
    for value in values {
        obligations.extend(super::whole_value_obligation(
            &registers[value.index()],
            context.body.register_types()[value.index()].dimensions(),
            context.provenance,
        )?);
    }
    Ok(obligations)
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

/// Derive the destinations of a branch; the obligations its branches can meet,
/// over the enclosing body's inputs, or `None` when neither branch has one.
pub(super) fn conditional(
    context: Context<'_>,
    condition: SolveRegisterId,
    captures: &[SolveRegisterId],
    destinations: &[SolveRegisterId],
    branches: [&SolveProgramRegion; 2],
    integers: &map_access::IntegerValues,
    registers: &mut [Vec<SolveCallDependency>],
) -> Result<Option<Vec<SolveCallDependency>>, SolveProgramConstructionError> {
    // A branch runs with the captures of this execution, so a capture whose
    // Integer value is known here is known in the branch.
    let known: Vec<_> = captures
        .iter()
        .map(|capture| integers.value(*capture))
        .collect();
    let followed = [
        region_execution(branches[0], context, &known)?,
        region_execution(branches[1], context, &known)?,
    ];
    // The condition decides which branch's obligations execution meets; each
    // branch meets its own over the captures it is called with.
    let obligations = if followed
        .iter()
        .any(|branch| branch.execution_reads.has_obligations())
    {
        let mut obligations = whole_obligations(context, &[condition], registers)?;
        for (branch, region) in followed.iter().zip(branches) {
            obligations.extend(super::substitute_obligations(
                branch.execution_reads.reads(),
                region.inputs(),
                |leaf| captures.get(leaf).map(|capture| capture.index()),
                registers,
                context.provenance,
            )?);
        }
        Some(obligations)
    } else {
        None
    };
    let [first, second] = followed;
    let summaries = [first.summaries, second.summaries];
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
    Ok(obligations)
}

/// The parts of a fold operation its dependencies derive from.
struct Fold<'a> {
    domain: &'a rumoca_core::StructuredIndexDomain,
    initial: &'a [SolveRegisterId],
    captures: &'a [SolveRegisterId],
    destinations: &'a [SolveRegisterId],
    transition: &'a SolveProgramRegion,
    continuation: Option<&'a SolveProgramRegion>,
}

/// Derive the destinations of a fold and return the Integer value each one
/// holds afterwards, where the loop fixes it.
fn fold(
    context: Context<'_>,
    fold: Fold<'_>,
    integers: &map_access::IntegerValues,
    registers: &mut [Vec<SolveCallDependency>],
    execution: &mut ExecutionReadBuilder,
) -> Result<Vec<Option<i64>>, SolveProgramConstructionError> {
    let Fold {
        domain,
        initial,
        captures,
        destinations,
        transition,
        continuation,
    } = fold;
    // Without an early exit every pass of the static domain runs, so a loop
    // within the budget is followed pass by pass, which is exact per element.
    if continuation.is_none()
        && let Some(followed) = passes::follow(
            context, domain, initial, captures, transition, integers, registers,
        )?
    {
        execution.region(followed.present, followed.obligations);
        for (destination, result) in destinations.iter().zip(followed.carried) {
            registers[destination.index()] = result;
        }
        return Ok(followed.integers);
    }
    let unknown = vec![None; destinations.len()];
    let mut regions = Vec::new();
    let mut obligations = Vec::new();
    for region in std::iter::once(transition).chain(continuation) {
        let followed = region_execution(region, context, &[])?;
        regions.push((region, followed.summaries));
        obligations.push((region, followed.execution_reads));
    }
    fold_obligations(context, initial, captures, &obligations, registers, execution)?;
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
            let unreached =
                |component: &usize| position == 0 && reached.get(*component) == Some(&false);
            for (output, summary) in region
                .outputs()
                .iter()
                .zip(summaries.iter())
                .enumerate()
                .filter(|(component, _)| !unreached(component))
                .map(|(_, pair)| pair)
            {
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
    Ok(unknown)
}

/// Charge the obligations of a fold summarised over all its passes. Region
/// inputs are the carried tuple, the readonly captures, then the binders,
/// whose values depend on nothing. A capture read is substituted through the
/// capture; a carried value can hold what any pass computed, so obligations
/// that read one cover every initial value and capture instead.
fn fold_obligations(
    context: Context<'_>,
    initial: &[SolveRegisterId],
    captures: &[SolveRegisterId],
    regions: &[(&SolveProgramRegion, crate::typed_program::call::execution_reads::ExecutionReads)],
    registers: &[Vec<SolveCallDependency>],
    execution: &mut ExecutionReadBuilder,
) -> Result<(), SolveProgramConstructionError> {
    if !regions.iter().any(|(_, reads)| reads.has_obligations()) {
        return Ok(());
    }
    let carries = initial.len();
    if regions
        .iter()
        .any(|(_, reads)| reads.inputs().iter().any(|&leaf| leaf < carries))
    {
        let mut obligations = whole_obligations(context, initial, registers)?;
        obligations.extend(whole_obligations(context, captures, registers)?);
        execution.region(true, obligations);
        return Ok(());
    }
    let mut obligations = Vec::new();
    for (region, reads) in regions {
        let capture = |leaf: usize| {
            leaf.checked_sub(carries)
                .and_then(|position| captures.get(position))
                .map(|capture| capture.index())
        };
        // Binder leaves read no input: they read an empty dependency list.
        let mut arguments = vec![Vec::new(); region.inputs().len()];
        for (leaf, argument) in arguments.iter_mut().enumerate() {
            if let Some(register) = capture(leaf) {
                argument.clone_from(&registers[register]);
            }
        }
        obligations.extend(super::substitute_obligations(
            reads.reads(),
            region.inputs(),
            Some,
            &arguments,
            context.provenance,
        )?);
    }
    execution.region(true, obligations);
    Ok(())
}

/// A region with the dependency summary of each of its outputs.
type RegionSummaries<'a> = (&'a SolveProgramRegion, Box<[Box<[SolveCallDependency]>]>);

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
fn carried_reach(selected: usize, carries: usize, regions: &[RegionSummaries<'_>]) -> Vec<bool> {
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
        if let Some(summary) = regions
            .first()
            .and_then(|(_, summaries)| summaries.get(component))
        {
            pending.extend(carried_reads(summary));
        }
    }
    reached
}
