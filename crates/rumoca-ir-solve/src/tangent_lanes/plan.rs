//! Certificates that evaluate a projection block's Jacobian from tangent
//! lanes: the reduced tear Jacobian of a torn block and the colored Jacobian
//! of a block solved whole.

use super::{TangentLaneError, TangentLaneProgram};
use crate::{
    BlockTearing, LinearOp, MAX_TENSOR_LANES, ProjectionJacobianApplication, ScalarProgramBlock,
};
use std::collections::BTreeMap;

/// Where one row's tangents come from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TangentRowSource {
    /// Output `output` of lane program `program` of the plan.
    Lanes { program: usize, output: usize },
    /// The row has no multi-lane JVP; its tangent is a finite difference of
    /// the primal row along the lane directions.
    FiniteDifference,
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
}

/// One reduced residual row of a torn block.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TornTangentResidual {
    pub row: usize,
    pub source: TangentRowSource,
}

/// Reduced tear Jacobian of one torn block from multi-lane tangents.
///
/// Lanes `0..tears` carry the tangents of the tear columns; the last lane
/// seeds a causal step's own target to obtain its coefficient. Each causal
/// step's tangent follows from the implicit function theorem on its row in
/// sweep order, reading earlier steps' tangents through the seeds, and the
/// reduced residual rows then give the Jacobian columns directly.
#[derive(Clone, Debug, PartialEq)]
pub struct TornTangentPlan {
    tear_targets: Box<[usize]>,
    lanes: usize,
    programs: Vec<TangentLaneProgram>,
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

/// Lane programs built on demand, one per distinct source program.
struct LanePrograms<'a> {
    jvp: &'a ScalarProgramBlock,
    lanes: usize,
    positions: BTreeMap<usize, (usize, usize)>,
    built: BTreeMap<usize, Option<usize>>,
    programs: Vec<TangentLaneProgram>,
}

impl LanePrograms<'_> {
    /// The tangent source of implicit row `row` and the seeds its program reads.
    fn source(&mut self, row: usize) -> (TangentRowSource, Vec<usize>) {
        let Some(&(program, output)) = self.positions.get(&row) else {
            return (TangentRowSource::FiniteDifference, Vec::new());
        };
        let ops = &self.jvp.programs()[program];
        let reads = seed_reads(ops);
        let built = match self.built.get(&program) {
            Some(built) => *built,
            None => {
                let built = TangentLaneProgram::replicate(ops, self.lanes)
                    .ok()
                    .map(|lanes| {
                        self.programs.push(lanes);
                        self.programs.len() - 1
                    });
                self.built.insert(program, built);
                built
            }
        };
        let source = built.map_or(TangentRowSource::FiniteDifference, |program| {
            TangentRowSource::Lanes { program, output }
        });
        (source, reads)
    }
}

impl TornTangentPlan {
    /// Build the plan of `tearing` over the solver-Y JVP rows `jvp`, whose
    /// output `i` is the tangent of implicit row `i`.
    pub fn derive(
        tearing: &BlockTearing,
        jvp: &ScalarProgramBlock,
    ) -> Result<Self, TangentLaneError> {
        let tears = tearing.tear_y_indices.len();
        let lanes = tears + 1;
        if tears == 0 || lanes >= MAX_TENSOR_LANES {
            return Err(TangentLaneError::LaneCount { lanes });
        }
        let mut builder = LanePrograms {
            jvp,
            lanes,
            positions: output_positions(jvp),
            built: BTreeMap::new(),
            programs: Vec::new(),
        };
        let mut reached_by: BTreeMap<usize, Vec<usize>> = tearing
            .tear_y_indices
            .iter()
            .enumerate()
            .map(|(column, &target)| (target, vec![column]))
            .collect();
        let mut steps = Vec::with_capacity(tearing.causal_steps.len());
        for step in &tearing.causal_steps {
            let (mut source, reads) = builder.source(step.row);
            // A row that does not read its own target has no coefficient.
            if reads.binary_search(&step.y_index).is_err() {
                source = TangentRowSource::FiniteDifference;
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
            steps.push(TornTangentStep {
                row: step.row,
                target: step.y_index,
                source,
                reached: reached.into_boxed_slice(),
            });
        }
        let residuals = tearing
            .residual_rows
            .iter()
            .map(|&row| TornTangentResidual {
                row,
                source: builder.source(row).0,
            })
            .collect();
        Ok(Self {
            tear_targets: tearing.tear_y_indices.clone().into_boxed_slice(),
            lanes,
            programs: builder.programs,
            steps,
            residuals,
        })
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
