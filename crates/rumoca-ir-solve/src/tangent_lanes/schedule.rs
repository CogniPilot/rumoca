//! The one-direction torn tangent issued as ordered assignment rows over an
//! extended slot vector, so a backend runs every sweep of one tear Jacobian
//! in one call by printing ordinary rows.
//!
//! Slots: `[y | seed | coefficient | recovered | residual | progress]`. The
//! rows read their AD seed from the seed slots in place. Each row is a step or
//! residual group's JVP program, unchanged but for its stores, which are
//! replaced by the commits the per-row sweep
//! performs between programs (`TornTangentEvaluator::eval_directions`): the
//! coefficient phase stores each group's coefficients, unseeds the group,
//! seeds the next and counts the committed groups in `progress`; each tear
//! column then stores every step's `-out / coefficient` to its seed and
//! recovered slots and the reduced rows' outputs, and resets the seeds.

use std::collections::BTreeMap;
use std::ops::Range;

use super::plan::TornTangentPlan;
use crate::{
    BinaryOp, ExactAssignmentProgramBuilder, IssuedAssignmentRows, IssuedAssignmentRowsError,
    LinearOp, ScalarProgramBlock, UnaryOp,
};

/// Why a torn tangent plan does not issue as assignment rows; the block then
/// keeps the per-row tangent.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum TornTangentScheduleRefusal {
    #[error("the plan evaluates multi-lane programs, not one direction at a time")]
    NotDirectional,
    #[error("JVP program {program} is not in the issued rows")]
    MissingProgram { program: usize },
    #[error("JVP program {program} stores through {operation}")]
    Store {
        program: usize,
        operation: &'static str,
    },
    #[error("JVP program {program} has no output {output}")]
    Output { program: usize, output: usize },
    #[error("JVP program {program} has no free register for its commits")]
    Registers { program: usize },
    #[error("the issued rows do not construct: {0}")]
    Rows(IssuedAssignmentRowsError),
}

/// Slot offsets of a [`TornTangentSchedule`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TornTangentSlots {
    /// Solver-Y length; the point occupies slots `0..y`.
    pub y: usize,
    pub seed: usize,
    /// One coefficient per causal step, in step order.
    pub coefficient: usize,
    /// `recovered[step * tears + column]`.
    pub recovered: usize,
    /// `residual[row * tears + column]`.
    pub residual: usize,
    /// Number of coefficient groups committed so far.
    pub progress: usize,
    pub total: usize,
}

/// The issued rows of one torn block's tangent, with the slot layout and the
/// coefficient groups the host reads after the call.
#[derive(Clone, Debug)]
pub struct TornTangentSchedule {
    rows: IssuedAssignmentRows,
    slots: TornTangentSlots,
    steps: usize,
    tears: usize,
    /// Exclusive step ends of the coefficient groups, in commit order.
    group_ends: Box<[usize]>,
}

impl TornTangentSchedule {
    #[must_use]
    pub const fn rows(&self) -> &IssuedAssignmentRows {
        &self.rows
    }

    #[must_use]
    pub const fn slots(&self) -> TornTangentSlots {
        self.slots
    }

    #[must_use]
    pub const fn steps(&self) -> usize {
        self.steps
    }

    #[must_use]
    pub const fn tears(&self) -> usize {
        self.tears
    }

    /// Causal steps whose coefficients are committed once `progress` groups
    /// have committed; `None` when `progress` is not a committed group count.
    #[must_use]
    pub fn committed_steps(&self, progress: f64) -> Option<usize> {
        if progress == 0.0 {
            return Some(0);
        }
        self.group_ends
            .iter()
            .enumerate()
            .find(|&(index, _)| (index + 1) as f64 == progress)
            .map(|(_, &end)| end)
    }
}

impl TornTangentPlan {
    /// Issue this one-direction plan over the JVP rows `jvp` it was derived
    /// from, for a solver Y of `y_len` values.
    pub fn issue_schedule(
        &self,
        jvp: &ScalarProgramBlock,
        y_len: usize,
    ) -> Result<TornTangentSchedule, TornTangentScheduleRefusal> {
        if !self.directional() {
            return Err(TornTangentScheduleRefusal::NotDirectional);
        }
        let steps = self.steps().len();
        let tears = self.tear_targets().len();
        let seed = y_len;
        let coefficient = seed + y_len;
        let recovered = coefficient + steps;
        let residual = recovered + steps * tears;
        let progress = residual + tears * tears;
        let slots = TornTangentSlots {
            y: y_len,
            seed,
            coefficient,
            recovered,
            residual,
            progress,
            total: progress + 1,
        };
        let mut issue = Issue {
            plan: self,
            jvp,
            slots,
            step_groups: groups(self.steps().iter().map(|step| step.group)),
            residual_groups: groups(self.residuals().iter().map(|row| row.group)),
            bodies: BTreeMap::new(),
            rows: Vec::new(),
            spans: Vec::new(),
            targets: Vec::new(),
        };
        issue.coefficients()?;
        for column in 0..tears {
            issue.column(column)?;
        }
        let group_ends = issue.step_groups.iter().map(|group| group.end).collect();
        let rows = IssuedAssignmentRows::new(issue.rows, issue.spans, issue.targets, slots.total)
            .and_then(|rows| rows.with_seed(seed..seed + y_len))
            .map_err(TornTangentScheduleRefusal::Rows)?;
        Ok(TornTangentSchedule {
            rows,
            slots,
            steps,
            tears,
            group_ends,
        })
    }
}

/// Consecutive member ranges of the entries whose `group` counts are given.
fn groups(counts: impl Iterator<Item = usize>) -> Vec<Range<usize>> {
    counts
        .enumerate()
        .filter(|&(_, count)| count > 0)
        .map(|(index, count)| index..index + count)
        .collect()
}

/// A JVP program prepared once: its operations without stores, and the
/// register of each output.
struct Body {
    ops: Vec<LinearOp>,
    outputs: Vec<u32>,
}

struct Issue<'a> {
    plan: &'a TornTangentPlan,
    jvp: &'a ScalarProgramBlock,
    slots: TornTangentSlots,
    step_groups: Vec<Range<usize>>,
    residual_groups: Vec<Range<usize>>,
    bodies: BTreeMap<usize, Body>,
    rows: Vec<Vec<LinearOp>>,
    spans: Vec<rumoca_core::Span>,
    targets: Vec<usize>,
}

type Commits = Vec<(u32, usize)>;

/// The (output, coefficient slot) of each member of a step group.
fn coefficient_members(
    plan: &TornTangentPlan,
    group: &Range<usize>,
    coefficient: usize,
) -> Vec<(usize, usize)> {
    group
        .clone()
        .zip(&plan.steps()[group.clone()])
        .map(|(member, step)| (step.source.output, coefficient + member))
        .collect()
}

/// One step's recovered tangent in a tear column: `-out / coefficient`,
/// committed to the step's seed slot and its recovered slot.
struct Recovered {
    output: usize,
    coefficient: usize,
    seed: usize,
    recovered: usize,
}

impl Recovered {
    fn commit(
        &self,
        builder: &mut ExactAssignmentProgramBuilder<'_>,
        outputs: &[u32],
        program: usize,
        commits: &mut Commits,
    ) -> Result<(), TornTangentScheduleRefusal> {
        let out = output_register(outputs, program, self.output)?;
        let held = register(builder, program)?;
        push(
            builder,
            program,
            LinearOp::LoadY {
                dst: held,
                index: self.coefficient,
            },
        )?;
        let negated = register(builder, program)?;
        push(
            builder,
            program,
            LinearOp::Unary {
                dst: negated,
                op: UnaryOp::Neg,
                arg: out,
            },
        )?;
        let value = register(builder, program)?;
        push(
            builder,
            program,
            LinearOp::Binary {
                dst: value,
                op: BinaryOp::Div,
                lhs: negated,
                rhs: held,
            },
        )?;
        commits.push((value, self.seed));
        commits.push((value, self.recovered));
        Ok(())
    }
}

impl Issue<'_> {
    /// Seed slots of a group's members.
    fn group_seeds(&self, group: Option<&Range<usize>>) -> Option<Vec<usize>> {
        let group = group?;
        Some(
            self.plan.steps()[group.clone()]
                .iter()
                .map(|step| self.slots.seed + step.target)
                .collect(),
        )
    }

    /// The seed slot of tear column `column`, when there is one.
    fn tear_seed(&self, column: usize) -> Vec<usize> {
        self.plan
            .tear_targets()
            .get(column)
            .map(|&tear| self.slots.seed + tear)
            .into_iter()
            .collect()
    }

    /// Seed the first group (or the first tear), then one row per step group:
    /// its coefficients, the group unseeded, the next seeded, and the count.
    fn coefficients(&mut self) -> Result<(), TornTangentScheduleRefusal> {
        let first = self
            .group_seeds(self.step_groups.first())
            .unwrap_or_else(|| self.tear_seed(0));
        let lead = self
            .step_groups
            .first()
            .map(|group| self.plan.steps()[group.start].source.program)
            .or_else(|| self.plan.residuals().first().map(|row| row.source.program))
            .ok_or(TornTangentScheduleRefusal::MissingProgram { program: 0 })?;
        self.constants(lead, &[(1.0, &first)])?;
        for index in 0..self.step_groups.len() {
            let group = self.step_groups[index].clone();
            let program = self.plan.steps()[group.start].source.program;
            let unseed = self.group_seeds(Some(&group)).unwrap_or_default();
            let next = self
                .group_seeds(self.step_groups.get(index + 1))
                .unwrap_or_else(|| self.tear_seed(0));
            let members = coefficient_members(self.plan, &group, self.slots.coefficient);
            let progress = self.slots.progress;
            self.row(program, |builder, outputs, commits| {
                stores(outputs, program, &members, commits)?;
                fill(builder, program, (0.0, &unseed), commits)?;
                fill(builder, program, (1.0, &next), commits)?;
                let count = constant(builder, program, (index + 1) as f64)?;
                commits.push((count, progress));
                Ok(())
            })?;
        }
        Ok(())
    }

    /// Tear column `column`: every step group's recovered tangents, then the
    /// reduced rows, the last of which resets the seeds and seeds the next tear.
    fn column(&mut self, column: usize) -> Result<(), TornTangentScheduleRefusal> {
        let tears = self.plan.tear_targets().len();
        for group in self.step_groups.clone() {
            let program = self.plan.steps()[group.start].source.program;
            let members: Vec<_> = group
                .clone()
                .zip(&self.plan.steps()[group.clone()])
                .map(|(member, step)| Recovered {
                    output: step.source.output,
                    coefficient: self.slots.coefficient + member,
                    seed: self.slots.seed + step.target,
                    recovered: self.slots.recovered + member * tears + column,
                })
                .collect();
            self.row(program, |builder, outputs, commits| {
                members
                    .iter()
                    .try_for_each(|member| member.commit(builder, outputs, program, commits))
            })?;
        }
        let mut reset = self.tear_seed(column);
        reset.extend(
            self.plan
                .steps()
                .iter()
                .map(|step| self.slots.seed + step.target),
        );
        let next = self.tear_seed(column + 1);
        for (position, group) in self.residual_groups.clone().into_iter().enumerate() {
            let program = self.plan.residuals()[group.start].source.program;
            let last = position + 1 == self.residual_groups.len();
            let members: Vec<_> = group
                .clone()
                .zip(&self.plan.residuals()[group.clone()])
                .map(|(row, entry)| {
                    (
                        entry.source.output,
                        self.slots.residual + row * tears + column,
                    )
                })
                .collect();
            let (reset, next) = if last {
                (&reset[..], &next[..])
            } else {
                (&[][..], &[][..])
            };
            self.row(program, |builder, outputs, commits| {
                stores(outputs, program, &members, commits)?;
                fill(builder, program, (0.0, reset), commits)?;
                fill(builder, program, (1.0, next), commits)
            })?;
        }
        Ok(())
    }

    fn body(&mut self, program: usize) -> Result<&Body, TornTangentScheduleRefusal> {
        if !self.bodies.contains_key(&program) {
            let source = self
                .jvp
                .programs()
                .get(program)
                .ok_or(TornTangentScheduleRefusal::MissingProgram { program })?;
            let body = prepare_body(program, source)?;
            self.bodies.insert(program, body);
        }
        Ok(&self.bodies[&program])
    }

    fn span(&self, program: usize) -> Result<rumoca_core::Span, TornTangentScheduleRefusal> {
        self.jvp
            .program_span(program)
            .ok_or(TornTangentScheduleRefusal::MissingProgram { program })
    }

    /// One row of constant stores, spanned by `program`.
    fn constants(
        &mut self,
        program: usize,
        stores: &[(f64, &[usize])],
    ) -> Result<(), TornTangentScheduleRefusal> {
        let mut ops = Vec::new();
        let mut builder = ExactAssignmentProgramBuilder::new(&mut ops)
            .ok_or(TornTangentScheduleRefusal::Registers { program })?;
        let mut commits = Vec::new();
        for &(value, slots) in stores {
            let register = constant(&mut builder, program, value)?;
            commits.extend(slots.iter().map(|&slot| (register, slot)));
        }
        drop(builder);
        let span = self.span(program)?;
        self.finish(ops, commits, span);
        Ok(())
    }

    /// One row: `program`'s prepared body, the operations `commit` appends,
    /// and its commits as stores in order.
    fn row(
        &mut self,
        program: usize,
        commit: impl FnOnce(
            &mut ExactAssignmentProgramBuilder<'_>,
            &[u32],
            &mut Commits,
        ) -> Result<(), TornTangentScheduleRefusal>,
    ) -> Result<(), TornTangentScheduleRefusal> {
        let span = self.span(program)?;
        let body = self.body(program)?;
        let mut ops = body.ops.clone();
        let outputs = body.outputs.clone();
        let mut builder = ExactAssignmentProgramBuilder::new(&mut ops)
            .ok_or(TornTangentScheduleRefusal::Registers { program })?;
        let mut commits = Vec::new();
        commit(&mut builder, &outputs, &mut commits)?;
        drop(builder);
        self.finish(ops, commits, span);
        Ok(())
    }

    fn finish(&mut self, mut ops: Vec<LinearOp>, commits: Commits, span: rumoca_core::Span) {
        for (register, slot) in commits {
            ops.push(LinearOp::StoreOutput { src: register });
            self.targets.push(slot);
        }
        self.rows.push(ops);
        self.spans.push(span);
    }
}

/// Commit each `(output, slot)` of the program's outputs.
fn stores(
    outputs: &[u32],
    program: usize,
    members: &[(usize, usize)],
    commits: &mut Commits,
) -> Result<(), TornTangentScheduleRefusal> {
    for &(output, slot) in members {
        commits.push((output_register(outputs, program, output)?, slot));
    }
    Ok(())
}

/// Commit `value` to every slot of `slots`; nothing when there is none.
fn fill(
    builder: &mut ExactAssignmentProgramBuilder<'_>,
    program: usize,
    (value, slots): (f64, &[usize]),
    commits: &mut Commits,
) -> Result<(), TornTangentScheduleRefusal> {
    if slots.is_empty() {
        return Ok(());
    }
    let register = constant(builder, program, value)?;
    commits.extend(slots.iter().map(|&slot| (register, slot)));
    Ok(())
}

fn output_register(
    outputs: &[u32],
    program: usize,
    output: usize,
) -> Result<u32, TornTangentScheduleRefusal> {
    outputs
        .get(output)
        .copied()
        .ok_or(TornTangentScheduleRefusal::Output { program, output })
}

fn register(
    builder: &mut ExactAssignmentProgramBuilder<'_>,
    program: usize,
) -> Result<u32, TornTangentScheduleRefusal> {
    builder
        .allocate()
        .ok_or(TornTangentScheduleRefusal::Registers { program })
}

fn push(
    builder: &mut ExactAssignmentProgramBuilder<'_>,
    program: usize,
    operation: LinearOp,
) -> Result<(), TornTangentScheduleRefusal> {
    builder
        .push(operation)
        .ok_or(TornTangentScheduleRefusal::Registers { program })
}

fn constant(
    builder: &mut ExactAssignmentProgramBuilder<'_>,
    program: usize,
    value: f64,
) -> Result<u32, TornTangentScheduleRefusal> {
    let dst = register(builder, program)?;
    push(builder, program, LinearOp::Const { dst, value })?;
    Ok(dst)
}

/// `source` with its stores removed, and the register of each output in order.
fn prepare_body(program: usize, source: &[LinearOp]) -> Result<Body, TornTangentScheduleRefusal> {
    let mut ops = Vec::with_capacity(source.len());
    let mut outputs = Vec::new();
    for operation in source {
        match operation {
            LinearOp::StoreOutput { src } => outputs.push(*src),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                for offset in 0..*count {
                    let register = offset
                        .checked_mul(*stride)
                        .and_then(|step| u32::try_from(step).ok())
                        .and_then(|step| start.checked_add(step))
                        .ok_or(TornTangentScheduleRefusal::Registers { program })?;
                    outputs.push(register);
                }
            }
            LinearOp::StoreOutputFoldTensorUpdate { .. } => {
                return Err(TornTangentScheduleRefusal::Store {
                    program,
                    operation: "StoreOutputFoldTensorUpdate",
                });
            }
            operation => ops.push(operation.clone()),
        }
    }
    Ok(Body { ops, outputs })
}

#[cfg(test)]
mod tests;
