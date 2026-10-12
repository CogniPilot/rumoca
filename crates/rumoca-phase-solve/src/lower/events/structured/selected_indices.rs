//! Expose selected-region index projections to the existing full-domain affine proof.
use super::*;
use std::sync::Arc;

#[cfg(test)]
mod tests;

struct Family {
    programs: Vec<Vec<solve::LinearOp>>,
    captures: Vec<Vec<f64>>,
}

struct Region {
    family: Family,
    next: solve::Reg,
    capture_base: Option<usize>,
    span: Span,
}

impl Region {
    fn new(
        points: usize,
        registers: usize,
        capture_base: Option<usize>,
        span: Span,
    ) -> Result<Self, LowerError> {
        Ok(Self {
            family: Family {
                programs: vec![Vec::new(); points],
                captures: vec![Vec::new(); points],
            },
            next: u32::try_from(registers)
                .map_err(|_| LowerError::contract("selected projection register overflow", span))?,
            capture_base,
            span,
        })
    }

    fn reserve(&mut self, count: usize) -> Result<solve::Reg, LowerError> {
        let start = self.next;
        self.next = self
            .next
            .checked_add(u32::try_from(count).map_err(|_| self.overflow())?)
            .ok_or_else(|| self.overflow())?;
        Ok(start)
    }

    fn overflow(&self) -> LowerError {
        LowerError::contract(
            "selected projection register or capture overflow",
            self.span,
        )
    }

    fn append(&mut self, ops: impl IntoIterator<Item = solve::LinearOp>) {
        for (output, op) in self.family.programs.iter_mut().zip(ops) {
            output.push(op);
        }
    }

    fn value(&mut self, values: impl IntoIterator<Item = f64>) -> Result<solve::Reg, LowerError> {
        let dst = self.reserve(1)?;
        match self.capture_base {
            Some(base) => {
                let index = base
                    .checked_add(self.family.captures[0].len())
                    .ok_or_else(|| self.overflow())?;
                self.append(std::iter::repeat_n(
                    solve::LinearOp::LoadFunctionConditionalCapture { dst, index },
                    self.family.programs.len(),
                ));
                for (captures, value) in self.family.captures.iter_mut().zip(values) {
                    captures.push(value);
                }
            }
            None => self.append(
                values
                    .into_iter()
                    .map(|value| solve::LinearOp::Const { dst, value }),
            ),
        }
        Ok(dst)
    }
}

pub(super) fn expose_selected_indices(
    programs: &mut Vec<Vec<solve::LinearOp>>,
    span: Span,
) -> Result<(), LowerError> {
    let counts = programs
        .iter()
        .map(|program| {
            solve::ScalarProgramRegisterFlow::derive(program)
                .map(|flow| flow.register_count())
                .map_err(|error| {
                    LowerError::contract(
                        format!("selected projection register proof: {error}"),
                        span,
                    )
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    *programs = region(programs, &counts, None, span)?.programs;
    Ok(())
}

fn region(
    programs: &[Vec<solve::LinearOp>],
    counts: &[usize],
    capture_base: Option<usize>,
    span: Span,
) -> Result<Family, LowerError> {
    let base = &programs[0];
    if counts.iter().any(|&count| count != counts[0])
        || programs.iter().any(|program| program.len() != base.len())
    {
        return Err(non_affine_structured_program(span));
    }
    let mut output = Region::new(programs.len(), counts[0], capture_base, span)?;
    for position in 0..base.len() {
        let ops = programs
            .iter()
            .map(|program| &program[position])
            .collect::<Vec<_>>();
        match &base[position] {
            solve::LinearOp::Const { dst, .. } if capture_base.is_some() => {
                literal(&ops, &mut output, dst)?;
            }
            solve::LinearOp::LoadIndexedRegister {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => indexed(&ops, &mut output, dst, base, stride, dimensions, indices)?,
            solve::LinearOp::FunctionConditional {
                dst_start,
                capture_start,
                program,
            } => conditional(&ops, &mut output, dst_start, capture_start, program)?,
            _ => output.append(ops.into_iter().cloned()),
        }
    }
    Ok(output.family)
}

fn literal(
    ops: &[&solve::LinearOp],
    output: &mut Region,
    dst: &solve::Reg,
) -> Result<(), LowerError> {
    if ops.iter().all(|op| *op == ops[0]) {
        output.append(ops.iter().map(|op| (*op).clone()));
        return Ok(());
    }
    let values = ops
        .iter()
        .map(|op| match op {
            solve::LinearOp::Const { dst: actual, value } if actual == dst => Ok(*value),
            _ => Err(non_affine_structured_program(output.span)),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let src = output.value(values)?;
    output.append(std::iter::repeat_n(
        solve::LinearOp::Move { dst: *dst, src },
        ops.len(),
    ));
    Ok(())
}

fn indexed(
    ops: &[&solve::LinearOp],
    output: &mut Region,
    dst: &solve::Reg,
    base: &solve::Reg,
    stride: &usize,
    dimensions: &[u32],
    indices: &[solve::TensorIndex],
) -> Result<(), LowerError> {
    let mut point_indices = Vec::with_capacity(ops.len());
    for op in ops {
        let solve::LinearOp::LoadIndexedRegister {
            dst: actual_dst,
            base: actual_base,
            stride: actual_stride,
            dimensions: actual_dimensions,
            indices: actual_indices,
        } = op
        else {
            return Err(non_affine_structured_program(output.span));
        };
        if (
            actual_dst,
            actual_base,
            actual_stride,
            actual_dimensions.as_ref(),
        ) != (dst, base, stride, dimensions)
            || actual_indices.len() != indices.len()
        {
            return Err(non_affine_structured_program(output.span));
        }
        point_indices.push(actual_indices.to_vec());
    }
    for axis in 0..indices.len() {
        if point_indices
            .iter()
            .all(|point| point[axis] == indices[axis])
        {
            continue;
        }
        let values = point_indices
            .iter()
            .map(|point| match point[axis] {
                solve::TensorIndex::Constant(value) => Ok(f64::from(value) + 1.0),
                _ => Err(non_affine_structured_program(output.span)),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let register = output.value(values)?;
        for point in &mut point_indices {
            point[axis] = solve::TensorIndex::Runtime(register);
        }
    }
    output.append(
        point_indices
            .into_iter()
            .map(|indices| solve::LinearOp::LoadIndexedRegister {
                dst: *dst,
                base: *base,
                stride: *stride,
                dimensions: dimensions.into(),
                indices: indices.into_boxed_slice(),
            }),
    );
    Ok(())
}

fn conditional(
    ops: &[&solve::LinearOp],
    output: &mut Region,
    dst_start: &solve::Reg,
    capture_start: &solve::Reg,
    program: &solve::FunctionConditionalProgram,
) -> Result<(), LowerError> {
    if ops.iter().all(|op| *op == ops[0]) {
        output.append(ops.iter().map(|op| (*op).clone()));
        return Ok(());
    }
    let mut programs = Vec::with_capacity(ops.len());
    for op in ops {
        let solve::LinearOp::FunctionConditional {
            dst_start: dst,
            capture_start: capture,
            program: actual,
        } = op
        else {
            return Err(non_affine_structured_program(output.span));
        };
        if dst != dst_start
            || capture != capture_start
            || actual.owner.is_some()
            || actual.capture_count != program.capture_count
            || actual.target_widths != program.target_widths
            || actual.arms.len() != program.arms.len()
        {
            return Err(non_affine_structured_program(output.span));
        }
        programs.push(actual.as_ref());
    }
    let (programs, extras) = conditional_programs(&programs, output.span)?;
    let mut extra_registers = Vec::new();
    for index in 0..extras[0].len() {
        extra_registers.push(output.value(extras.iter().map(|values| values[index]))?);
    }
    let capture = output.reserve(
        program
            .capture_count
            .checked_add(extra_registers.len())
            .ok_or_else(|| output.overflow())?,
    )?;
    for index in 0..program.capture_count {
        let offset = u32::try_from(index).map_err(|_| output.overflow())?;
        let src = capture_start
            .checked_add(offset)
            .ok_or_else(|| output.overflow())?;
        output.append(std::iter::repeat_n(
            solve::LinearOp::Move {
                dst: capture + offset,
                src,
            },
            ops.len(),
        ));
    }
    for (index, src) in extra_registers.into_iter().enumerate() {
        let offset = u32::try_from(program.capture_count + index).map_err(|_| output.overflow())?;
        output.append(std::iter::repeat_n(
            solve::LinearOp::Move {
                dst: capture + offset,
                src,
            },
            ops.len(),
        ));
    }
    output.append(
        programs
            .into_iter()
            .map(|program| solve::LinearOp::FunctionConditional {
                dst_start: *dst_start,
                capture_start: capture,
                program: Arc::new(program),
            }),
    );
    Ok(())
}

fn conditional_programs(
    programs: &[&solve::FunctionConditionalProgram],
    span: Span,
) -> Result<(Vec<solve::FunctionConditionalProgram>, Vec<Vec<f64>>), LowerError> {
    let base = programs[0];
    let mut captures = vec![Vec::new(); programs.len()];
    let mut arms = vec![Vec::new(); programs.len()];
    for arm in 0..base.arms.len() {
        let condition = conditional_region(programs, &mut captures, span, |program| {
            (
                &program.arms[arm].condition,
                program.arms[arm].condition_register_count,
            )
        })?;
        let result = conditional_region(programs, &mut captures, span, |program| {
            (
                &program.arms[arm].result,
                program.arms[arm].result_register_count,
            )
        })?;
        for ((arms, condition), result) in arms.iter_mut().zip(condition).zip(result) {
            arms.push((condition, result));
        }
    }
    let fallbacks = conditional_region(programs, &mut captures, span, |program| {
        (&program.fallback, program.fallback_register_count)
    })?;
    let capture_count = base
        .capture_count
        .checked_add(captures[0].len())
        .ok_or_else(|| LowerError::contract("selected projection capture overflow", span))?;
    let checked = arms
        .into_iter()
        .zip(fallbacks)
        .map(|(arms, fallback)| {
            solve::FunctionConditionalProgram::checked(
                capture_count,
                base.target_widths.clone(),
                arms,
                fallback,
            )
            .map_err(|error| {
                LowerError::contract(
                    format!("selected projection conditional proof: {error}"),
                    span,
                )
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok((checked, captures))
}

fn conditional_region<'a>(
    programs: &[&'a solve::FunctionConditionalProgram],
    captures: &mut [Vec<f64>],
    span: Span,
    select: impl Fn(&'a solve::FunctionConditionalProgram) -> (&'a Vec<solve::LinearOp>, usize),
) -> Result<Vec<Vec<solve::LinearOp>>, LowerError> {
    let (regions, counts): (Vec<_>, Vec<_>) =
        programs.iter().map(|program| select(program)).unzip();
    let regions = regions.into_iter().cloned().collect::<Vec<_>>();
    let capture_base = programs[0]
        .capture_count
        .checked_add(captures[0].len())
        .ok_or_else(|| LowerError::contract("selected projection capture overflow", span))?;
    let family = region(&regions, &counts, Some(capture_base), span)?;
    for (captures, extra) in captures.iter_mut().zip(family.captures) {
        captures.extend(extra);
    }
    Ok(family.programs)
}
