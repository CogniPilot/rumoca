//! A loop carry whose coordinates cannot exchange influence across iterations.
use super::*;

pub(super) fn fold_output(
    context: Context<'_>,
    initial: &[SolveRegisterId],
    captures: &[SolveRegisterId],
    transition: &SolveProgramRegion,
    summary: &[SolveCallDependency],
    index: usize,
    registers: &[Vec<SolveCallDependency>],
) -> Result<Option<Vec<SolveCallDependency>>, SolveProgramConstructionError> {
    let Some(value) = initial.get(index) else {
        return Ok(None);
    };
    let dimensions = context.body.register_types()[value.index()].dimensions();
    if transition
        .outputs()
        .get(index)
        .map(SolveValueType::dimensions)
        != Some(dimensions)
        || !self_coordinate_only(summary, initial.len(), index, dimensions)
    {
        return Ok(None);
    }
    let mut out = Dependencies::default();
    let identity = Coordinates::identity(dimensions.len());
    // The domain can be empty; preserve the incoming value as well as every
    // iteration's capture reads. Self-coordinate-only recurrence proves that
    // later iterations cannot add another carry or another coordinate.
    for dependency in &registers[value.index()] {
        out.insert(dependency.remap(&identity, context.provenance)?);
    }
    merge(
        &mut out,
        capture_dependencies(
            context,
            captures,
            initial.len(),
            summary,
            dimensions.len(),
            None,
            registers,
        )?,
    );
    Ok(Some(out.finish()))
}

fn self_coordinate_only(
    summary: &[SolveCallDependency],
    carries: usize,
    selected: usize,
    dimensions: &[u32],
) -> bool {
    summary.iter().all(|dependency| {
        dependency.inputs().all(|leaf| {
            leaf >= carries
                || (leaf == selected
                    && match &dependency.coordinates {
                        Some(coordinates) => coordinates.is_identity(dimensions),
                        None => dimensions.is_empty(),
                    })
        })
    })
}
