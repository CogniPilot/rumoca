//! Conservative region read sets, retaining coordinate subsets of captures.
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

/// Substitute readonly captures; carried inputs and static binders are handled
/// separately by the fold closure, never mistaken for readonly capture leaves.
fn capture_dependencies(
    context: Context<'_>,
    captures: &[SolveRegisterId],
    offset: usize,
    summary: &[SolveCallDependency],
    rank: usize,
    consume: Option<&Coordinates>,
    registers: &[Vec<SolveCallDependency>],
) -> Result<Vec<SolveCallDependency>, SolveProgramConstructionError> {
    let mut out = Dependencies::default();
    for dependency in summary {
        for leaf in dependency.inputs() {
            let Some(capture) = leaf.checked_sub(offset).and_then(|i| captures.get(i)) else {
                continue;
            };
            let access = match (&dependency.coordinates, consume) {
                (Some(access), Some(consume)) => access.compose(consume).ok_or(
                    SolveProgramConstructionError::InvalidCallInterface {
                        provenance: context.provenance,
                    },
                )?,
                (Some(access), None) => access.clone(),
                (None, _) => operations::whole_access(
                    rank,
                    context.body.register_types()[capture.index()].dimensions(),
                ),
            };
            for source in &registers[capture.index()] {
                out.insert(source.remap_may_depend(&access, context.provenance)?);
            }
        }
    }
    Ok(out.finish())
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
    for (index, destination) in destinations.iter().enumerate() {
        let rank = context.body.register_types()[destination.index()]
            .dimensions()
            .len();
        let mut out = Dependencies::default();
        insert_operand(
            &mut out,
            &registers[condition.index()],
            rank,
            &context.body.register_types()[condition.index()],
            context.provenance,
        )?;
        for branch in &summaries {
            for dependency in
                capture_dependencies(context, captures, 0, &branch[index], rank, None, registers)?
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
    for (index, destination) in destinations.iter().enumerate() {
        if continuation.is_none()
            && let Some(result) = pointwise::fold_output(
                context,
                initial,
                captures,
                transition,
                &regions[0].1[index],
                index,
                registers,
            )?
        {
            results.push(result);
            continue;
        }
        let rank = context.body.register_types()[destination.index()]
            .dimensions()
            .len();
        let mut out = Dependencies::default();
        // Any carried component may flow to any other across later iterations.
        // Include all initial values; readonly capture read sets cannot grow.
        for operand in initial {
            insert_operand(
                &mut out,
                &registers[operand.index()],
                rank,
                &context.body.register_types()[operand.index()],
                context.provenance,
            )?;
        }
        for (region, summaries) in &regions {
            for (output, summary) in region.outputs().iter().zip(summaries.iter()) {
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
                        registers,
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
