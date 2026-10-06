//! Source-scoped regions never share register identities with their parent.

use super::*;
use crate::FunctionConditionalProgram;

pub(super) fn capture(owner: Option<usize>, start: usize, count: usize) -> Checked<bool> {
    if owner.is_none_or(|capacity| start.checked_add(count).is_none_or(|end| end > capacity)) {
        return refused("native conditional capture exceeds its exact region owner");
    }
    // The caller checks the complete original capture tuple before entering
    // any region. No own-target capture is admitted, even if unused.
    Ok(false)
}

pub(super) fn derive(
    parent: &mut Dependencies,
    prefix: ProgramPrefix<'_>,
    capture_start: Reg,
    program: &FunctionConditionalProgram,
    target: &Range<usize>,
    layout: &VarLayout,
) -> Checked<bool> {
    program
        .validate()
        .map_err(|_| NativeRefreshAssignmentRefusal("malformed native conditional region owner"))?;
    if program.capture_count != 0
        && parent.depends_on(prefix, capture_start, program.capture_count, 1)?
    {
        return refused("native conditional captures depend on its own assignment target");
    }
    for arm in &program.arms {
        region(
            parent,
            &arm.condition,
            program.capture_count,
            target,
            layout,
        )?;
        region(parent, &arm.result, program.capture_count, target, layout)?;
    }
    region(
        parent,
        &program.fallback,
        program.capture_count,
        target,
        layout,
    )?;
    Ok(false)
}

fn region(
    parent: &mut Dependencies,
    operations: &[LinearOp],
    captures: usize,
    target: &Range<usize>,
    layout: &VarLayout,
) -> Checked<()> {
    let producers = UniqueProgram::new(operations).ok_or(NativeRefreshAssignmentRefusal(
        "native conditional region has overlapping destination versions",
    ))?;
    let dependencies = derive_scoped(producers.view(), target, layout, Some(captures))?;
    super::super::independent_call_inputs(producers.view(), &dependencies)?;
    if dependencies.dependent.iter().any(|dependent| *dependent) {
        return refused("native conditional region reads its own assignment target");
    }
    parent.reads.extend(dependencies.reads);
    Ok(())
}
