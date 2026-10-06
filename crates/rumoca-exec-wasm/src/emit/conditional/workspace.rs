//! Checked maximum live workspace; sibling regions reuse storage, nesting does not.
use super::*;

#[derive(Clone, Copy)]
pub(super) struct Frame {
    pub capture: Reg,
    pub output: Reg,
    pub registers: Reg,
}
impl Frame {
    pub(super) fn new(start: Reg, program: &FunctionConditionalProgram) -> Result<Self, String> {
        let output = add(start, program.capture_count)?;
        let registers = add(output, program.result_count.max(1))?;
        Ok(Self {
            capture: start,
            output,
            registers,
        })
    }
}
fn add(base: Reg, count: usize) -> Result<Reg, String> {
    Reg::try_from(count)
        .ok()
        .and_then(|count| base.checked_add(count))
        .ok_or_else(|| "native conditional workspace overflows".into())
}
pub(in crate::emit) fn register_extent(
    rows: &[Vec<LinearOp>],
    root: usize,
) -> Result<usize, String> {
    let additional = rows.iter().try_fold(0usize, |maximum, row| {
        extra(row).map(|extent| maximum.max(extent))
    })?;
    root.checked_add(additional)
        .ok_or_else(|| "native conditional workspace overflows".into())
}
fn extra(ops: &[LinearOp]) -> Result<usize, String> {
    let mut maximum = 0;
    for op in ops {
        if let LinearOp::FunctionConditional { program, .. } = op {
            let frame = program
                .capture_count
                .checked_add(program.result_count.max(1))
                .ok_or("native conditional workspace overflows")?;
            let mut regions = region(&program.fallback, program.fallback_register_count)?;
            for arm in &program.arms {
                regions = regions
                    .max(region(&arm.condition, arm.condition_register_count)?)
                    .max(region(&arm.result, arm.result_register_count)?);
            }
            maximum = maximum.max(
                frame
                    .checked_add(regions)
                    .ok_or("native conditional workspace overflows")?,
            );
        }
    }
    Ok(maximum)
}
fn region(ops: &[LinearOp], registers: usize) -> Result<usize, String> {
    registers
        .checked_add(extra(ops)?)
        .ok_or_else(|| "native region workspace overflows".into())
}
