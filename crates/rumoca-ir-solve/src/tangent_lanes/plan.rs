//! Certificates that evaluate a projection block's Jacobian from tangent
//! lanes: the reduced tear Jacobian of a torn block and the colored Jacobian
//! of a block solved whole.

use super::{TangentLaneError, TangentLaneProgram};
use crate::{
    BlockTearing, LinearOp, MAX_TENSOR_LANES, ProjectionJacobianApplication, ScalarProgramBlock,
    StructuralPattern,
};
use std::collections::{BTreeMap, BTreeSet};

/// Where one row's tangents come from: output `output` of lane program
/// `program` of the plan, or of JVP program `program` for a plan evaluated one
/// direction at a time.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TangentRowSource {
    pub program: usize,
    pub output: usize,
}

/// One causal step of a torn block's tangent sweep.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TornTangentStep {
    /// Residual row solved for `target`.
    pub row: usize,
    /// Solver-Y index the step assigns.
    pub target: usize,
    /// The step's tangent is `-b / a`: `b` the row's tangent with the target
    /// held, `a` the coefficient of the target, taken from the last lane,
    /// which seeds the target alone.
    pub source: TangentRowSource,
    /// Tear columns whose tangents reach the step through the rows it reads.
    pub reached: Box<[usize]>,
    /// Steps answered by one evaluation of this step's program, counting
    /// itself; zero when an earlier step's evaluation answers it.
    pub group: usize,
}

/// One reduced residual row of a torn block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TornTangentResidual {
    pub row: usize,
    pub source: TangentRowSource,
    /// Rows answered by one evaluation of this row's program, counting
    /// itself; zero when an earlier row's evaluation answers it.
    pub group: usize,
}

/// Reduced tear Jacobian of one torn block from tangents.
///
/// Lanes `0..tears` carry the tangents of the tear columns; the last lane
/// seeds a causal step's own target to obtain its coefficient. Each causal
/// step's tangent follows from the implicit function theorem on its row in
/// sweep order, reading earlier steps' tangents through the seeds, and the
/// reduced residual rows then give the Jacobian columns directly.
///
/// The lanes are evaluated together by multi-lane programs when every row's
/// JVP program widens and the lanes fit; otherwise the plan evaluates each lane
/// as its own direction through the rows' JVP programs, first every step's
/// coefficient and then one tear column at a time. Each lane equals its
/// one-direction evaluation, so both forms give the same Jacobian. A plan
/// refuses only a row with no JVP program.
#[derive(Clone, Debug, PartialEq)]
pub struct TornTangentPlan {
    tear_targets: Box<[usize]>,
    lanes: usize,
    /// Multi-lane programs; empty when the plan evaluates one direction at a
    /// time through the JVP programs.
    programs: Vec<TangentLaneProgram>,
    directional: bool,
    steps: Vec<TornTangentStep>,
    residuals: Vec<TornTangentResidual>,
}

/// Position of every stored output of `block`: output index -> (program, offset).
fn output_positions(block: &ScalarProgramBlock) -> BTreeMap<usize, (usize, usize)> {
    let mut positions = BTreeMap::new();
    let mut ordinal = 0;
    for (program, ops) in block.programs().iter().enumerate() {
        for offset in 0..ScalarProgramBlock::program_output_count(ops) {
            if let Some(&output) = block.output_indices().get(ordinal) {
                positions.insert(output, (program, offset));
            }
            ordinal += 1;
        }
    }
    positions
}

/// Group consecutive causal steps of one program whose outputs depend on no
/// other group member's target. One evaluation then answers the group: the
/// coefficient lane seeds every member's target and each member's output
/// reads only its own, and no member's tangent reaches another member's
/// output, so each member's lanes equal its own evaluation's bit for bit.
fn group_steps(steps: &mut [TornTangentStep], dependencies: &[Option<BTreeSet<usize>>]) {
    let mut start = 0;
    while start < steps.len() {
        let mut end = start + 1;
        while end < steps.len()
            && steps[end].source.program == steps[start].source.program
            && (start..end).all(|member| {
                let independent = |reader: usize, target: usize| {
                    dependencies[reader]
                        .as_ref()
                        .is_some_and(|reads| !reads.contains(&target))
                };
                independent(end, steps[member].target) && independent(member, steps[end].target)
            })
        {
            end += 1;
        }
        steps[start].group = end - start;
        for step in &mut steps[start + 1..end] {
            step.group = 0;
        }
        start = end;
    }
}

/// Group consecutive reduced rows of one program: they write no seed, so one
/// evaluation answers them all.
fn group_residuals(residuals: &mut [TornTangentResidual]) {
    let mut start = 0;
    while start < residuals.len() {
        let program = residuals[start].source.program;
        let end = residuals[start..]
            .iter()
            .position(|residual| residual.source.program != program)
            .map_or(residuals.len(), |offset| start + offset);
        residuals[start].group = end - start;
        for residual in &mut residuals[start + 1..end] {
            residual.group = 0;
        }
        start = end;
    }
}

/// Solver-Y indices whose seed a JVP program reads.
fn seed_reads(program: &[LinearOp]) -> Vec<usize> {
    let mut reads = Vec::new();
    for op in program {
        match op {
            LinearOp::LoadSeed { index, .. } => reads.push(*index),
            LinearOp::TensorLoad {
                seed_start: Some(start),
                count,
                ..
            } => reads.extend(*start..*start + *count),
            _ => {}
        }
    }
    reads.sort_unstable();
    reads.dedup();
    reads
}

/// Lane programs built on demand, one per distinct source program; with
/// `directional`, the sources name the JVP programs themselves.
struct LanePrograms<'a> {
    jvp: &'a ScalarProgramBlock,
    lanes: usize,
    directional: bool,
    positions: BTreeMap<usize, (usize, usize)>,
    built: BTreeMap<usize, usize>,
    programs: Vec<TangentLaneProgram>,
    /// Per-output seed dependencies of each JVP program read so far; `None`
    /// when the program's dependencies are not derivable.
    dependencies: BTreeMap<usize, Option<Vec<BTreeSet<usize>>>>,
}

impl LanePrograms<'_> {
    /// The seeds output `output` of JVP program `program` depends on; `None`
    /// when not derivable.
    fn output_dependencies(&mut self, program: usize, output: usize) -> Option<BTreeSet<usize>> {
        let jvp = self.jvp;
        self.dependencies
            .entry(program)
            .or_insert_with(|| {
                StructuralPattern::derive_output_seed_index_dependencies(
                    &jvp.programs()[program],
                    None,
                )
                .ok()
            })
            .as_ref()
            .and_then(|outputs| outputs.get(output).cloned())
    }

    /// The tangent source of implicit row `row` and the seeds its program
    /// reads; a row without a JVP program, or whose program does not widen,
    /// declines the plan.
    fn source(&mut self, row: usize) -> Result<(TangentRowSource, Vec<usize>), TangentLaneError> {
        let &(program, output) = self
            .positions
            .get(&row)
            .ok_or(TangentLaneError::NoTangent { row })?;
        let ops = &self.jvp.programs()[program];
        let reads = seed_reads(ops);
        if self.directional {
            return Ok((TangentRowSource { program, output }, reads));
        }
        let built = match self.built.get(&program) {
            Some(&built) => built,
            None => {
                self.programs
                    .push(TangentLaneProgram::replicate(ops, self.lanes)?);
                let built = self.programs.len() - 1;
                self.built.insert(program, built);
                built
            }
        };
        Ok((
            TangentRowSource {
                program: built,
                output,
            },
            reads,
        ))
    }
}

impl TornTangentPlan {
    /// Build the plan of `tearing` over the solver-Y JVP rows `jvp`, whose
    /// output `i` is the tangent of implicit row `i`: multi-lane when every
    /// program widens and the lanes fit, one direction at a time otherwise.
    pub fn derive(
        tearing: &BlockTearing,
        jvp: &ScalarProgramBlock,
    ) -> Result<Self, TangentLaneError> {
        let tears = tearing.tear_y_indices.len();
        if tears == 0 {
            return Err(TangentLaneError::LaneCount { lanes: 1 });
        }
        if tears + 1 < MAX_TENSOR_LANES {
            match Self::derive_form(tearing, jvp, false) {
                Err(TangentLaneError::NoTangent { row }) => {
                    return Err(TangentLaneError::NoTangent { row });
                }
                Err(_) => {}
                built => return built,
            }
        }
        Self::derive_form(tearing, jvp, true)
    }

    /// Build the one-direction form of the plan of `tearing`, for an evaluator
    /// whose one-direction JVP programs are cheaper than their lane widening.
    pub fn derive_directional(
        tearing: &BlockTearing,
        jvp: &ScalarProgramBlock,
    ) -> Result<Self, TangentLaneError> {
        if tearing.tear_y_indices.is_empty() {
            return Err(TangentLaneError::LaneCount { lanes: 1 });
        }
        Self::derive_form(tearing, jvp, true)
    }

    fn derive_form(
        tearing: &BlockTearing,
        jvp: &ScalarProgramBlock,
        directional: bool,
    ) -> Result<Self, TangentLaneError> {
        let lanes = tearing.tear_y_indices.len() + 1;
        let mut builder = LanePrograms {
            jvp,
            lanes,
            directional,
            positions: output_positions(jvp),
            built: BTreeMap::new(),
            programs: Vec::new(),
            dependencies: BTreeMap::new(),
        };
        let mut reached_by: BTreeMap<usize, Vec<usize>> = tearing
            .tear_y_indices
            .iter()
            .enumerate()
            .map(|(column, &target)| (target, vec![column]))
            .collect();
        let mut steps = Vec::with_capacity(tearing.causal_steps.len());
        let mut dependencies = Vec::with_capacity(tearing.causal_steps.len());
        for step in &tearing.causal_steps {
            let (source, reads) = builder.source(step.row)?;
            // A row that does not read its own target has no coefficient.
            if reads.binary_search(&step.y_index).is_err() {
                return Err(TangentLaneError::NoTangent { row: step.row });
            }
            let mut reached = reads
                .iter()
                .filter(|read| **read != step.y_index)
                .filter_map(|read| reached_by.get(read))
                .flatten()
                .copied()
                .collect::<Vec<_>>();
            reached.sort_unstable();
            reached.dedup();
            reached_by.insert(step.y_index, reached.clone());
            let &(program, output) = builder
                .positions
                .get(&step.row)
                .ok_or(TangentLaneError::NoTangent { row: step.row })?;
            dependencies.push(builder.output_dependencies(program, output));
            steps.push(TornTangentStep {
                row: step.row,
                target: step.y_index,
                source,
                reached: reached.into_boxed_slice(),
                group: 1,
            });
        }
        let mut residuals = tearing
            .residual_rows
            .iter()
            .map(|&row| {
                Ok(TornTangentResidual {
                    row,
                    source: builder.source(row)?.0,
                    group: 1,
                })
            })
            .collect::<Result<Vec<_>, TangentLaneError>>()?;
        group_steps(&mut steps, &dependencies);
        group_residuals(&mut residuals);
        Ok(Self {
            tear_targets: tearing.tear_y_indices.clone().into_boxed_slice(),
            lanes,
            programs: builder.programs,
            directional,
            steps,
            residuals,
        })
    }

    /// Whether the plan evaluates one direction at a time through the JVP
    /// programs instead of multi-lane programs.
    #[must_use]
    pub const fn directional(&self) -> bool {
        self.directional
    }

    /// Solver-Y index of each tear column.
    #[must_use]
    pub fn tear_targets(&self) -> &[usize] {
        &self.tear_targets
    }

    /// Lanes of every program: the tears plus the coefficient lane.
    #[must_use]
    pub const fn lanes(&self) -> usize {
        self.lanes
    }

    #[must_use]
    pub fn programs(&self) -> &[TangentLaneProgram] {
        &self.programs
    }

    #[must_use]
    pub fn steps(&self) -> &[TornTangentStep] {
        &self.steps
    }

    #[must_use]
    pub fn residuals(&self) -> &[TornTangentResidual] {
        &self.residuals
    }
}

/// One multi-lane evaluation of a colored Jacobian application's program.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColoredLaneCall {
    /// Lane program of the plan.
    pub program: usize,
    /// The color each lane seeds.
    pub colors: Box<[usize]>,
    /// `(lane, program output offset, destination offset)` of every value
    /// this evaluation places.
    pub placements: Box<[(usize, usize, usize)]>,
}

/// Every call of one application program, in first-use order: the colors
/// calling it and their `(lane, offset, destination)` placements.
struct ProgramUse {
    program: usize,
    colors: Vec<usize>,
    placements: Vec<(usize, usize, usize)>,
}

fn program_uses(application: &ProjectionJacobianApplication) -> Vec<ProgramUse> {
    let mut uses: Vec<ProgramUse> = Vec::new();
    let calls = application
        .colors()
        .iter()
        .enumerate()
        .flat_map(|(color, entry)| {
            entry
                .outputs()
                .programs()
                .iter()
                .map(move |call| (color, call))
        });
    for (color, call) in calls {
        let index = match uses
            .iter()
            .position(|entry| entry.program == call.program())
        {
            Some(index) => index,
            None => {
                uses.push(ProgramUse {
                    program: call.program(),
                    colors: Vec::new(),
                    placements: Vec::new(),
                });
                uses.len() - 1
            }
        };
        let entry = &mut uses[index];
        let lane = entry.colors.len();
        entry.colors.push(color);
        let placed = call.placements().iter();
        entry
            .placements
            .extend(placed.map(|&(offset, destination)| (lane, offset, destination)));
    }
    uses
}

/// A colored Jacobian application evaluated with one lane per color: each
/// distinct program of the application runs once with one lane for every
/// color that calls it, instead of once per color. Lane `j` of a call seeds
/// the seed indices of color `colors[j]`; each placement writes the value the
/// one-direction call of that color would write.
#[derive(Clone, Debug, PartialEq)]
pub struct ColoredTangentPlan {
    color_seeds: Box<[Box<[usize]>]>,
    programs: Vec<TangentLaneProgram>,
    calls: Vec<ColoredLaneCall>,
    output_len: usize,
}

impl ColoredTangentPlan {
    /// Build the plan of an issued colored Jacobian application.
    pub fn derive(application: &ProjectionJacobianApplication) -> Result<Self, TangentLaneError> {
        let uses = program_uses(application);
        let mut programs = Vec::with_capacity(uses.len());
        let mut calls = Vec::with_capacity(uses.len());
        for ProgramUse {
            program,
            colors,
            placements,
        } in uses
        {
            if colors.len() >= MAX_TENSOR_LANES {
                return Err(TangentLaneError::LaneCount {
                    lanes: colors.len(),
                });
            }
            let ops =
                application
                    .source()
                    .programs()
                    .get(program)
                    .ok_or(TangentLaneError::Coloring {
                        row: program,
                        column: 0,
                    })?;
            programs.push(TangentLaneProgram::replicate(ops, colors.len())?);
            calls.push(ColoredLaneCall {
                program: programs.len() - 1,
                colors: colors.into_boxed_slice(),
                placements: placements.into_boxed_slice(),
            });
        }
        Ok(Self {
            color_seeds: application
                .colors()
                .iter()
                .map(|color| color.seed_indices().into())
                .collect(),
            programs,
            calls,
            output_len: application.output_len(),
        })
    }

    /// Seed indices of each color.
    #[must_use]
    pub fn color_seeds(&self) -> &[Box<[usize]>] {
        &self.color_seeds
    }

    #[must_use]
    pub fn programs(&self) -> &[TangentLaneProgram] {
        &self.programs
    }

    #[must_use]
    pub fn calls(&self) -> &[ColoredLaneCall] {
        &self.calls
    }

    /// Length of the column-major output buffer the placements address.
    #[must_use]
    pub const fn output_len(&self) -> usize {
        self.output_len
    }
}
