//! Certificates that evaluate a projection block's Jacobian from tangent
//! lanes: the reduced tear Jacobian of a torn block and the colored Jacobian
//! of a block solved whole.

use super::{TangentLaneError, TangentLaneProgram};
use crate::{BlockTearing, LinearOp, MAX_TENSOR_LANES, ScalarProgramBlock};
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

/// One nonzero of a colored block Jacobian.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ColoredTangentEntry {
    /// Block-local row and column.
    pub row: usize,
    pub column: usize,
    /// Lane (color) that carries the column.
    pub lane: usize,
    pub source: TangentRowSource,
}

/// Colored Jacobian of a block from multi-lane tangents: lane `c` seeds every
/// column of color `c`, so one evaluation of each row program yields every
/// structural nonzero.
#[derive(Clone, Debug, PartialEq)]
pub struct ColoredTangentPlan {
    columns: Box<[usize]>,
    colors: Box<[usize]>,
    lanes: usize,
    programs: Vec<TangentLaneProgram>,
    entries: Vec<ColoredTangentEntry>,
}

impl ColoredTangentPlan {
    /// Build the plan of a block with implicit `rows`, solver-Y `columns`,
    /// block-local structural nonzeros `pattern`, and column color `groups`.
    pub fn derive(
        rows: &[usize],
        columns: &[usize],
        pattern: &[(usize, usize)],
        groups: &[Box<[u32]>],
        jvp: &ScalarProgramBlock,
    ) -> Result<Self, TangentLaneError> {
        let lanes = groups.len();
        if lanes == 0 || lanes >= MAX_TENSOR_LANES {
            return Err(TangentLaneError::LaneCount { lanes });
        }
        let mut colors = vec![usize::MAX; columns.len()];
        let colored = groups
            .iter()
            .enumerate()
            .flat_map(|(color, group)| group.iter().map(move |column| (*column as usize, color)));
        for (column, color) in colored.filter(|(column, _)| *column < columns.len()) {
            colors[column] = color;
        }
        let mut builder = LanePrograms {
            jvp,
            lanes,
            positions: output_positions(jvp),
            built: BTreeMap::new(),
            programs: Vec::new(),
        };
        let mut entries = Vec::with_capacity(pattern.len());
        for &(row, column) in pattern {
            let lane = colors.get(column).copied().filter(|lane| *lane < lanes);
            let (Some(lane), Some(&implicit_row)) = (lane, rows.get(row)) else {
                return Err(TangentLaneError::Coloring { row, column });
            };
            entries.push(ColoredTangentEntry {
                row,
                column,
                lane,
                source: builder.source(implicit_row).0,
            });
        }
        Ok(Self {
            columns: columns.into(),
            colors: colors.into_boxed_slice(),
            lanes,
            programs: builder.programs,
            entries,
        })
    }

    /// Solver-Y index of each block column.
    #[must_use]
    pub fn columns(&self) -> &[usize] {
        &self.columns
    }

    /// Color (lane) of each block column.
    #[must_use]
    pub fn colors(&self) -> &[usize] {
        &self.colors
    }

    #[must_use]
    pub const fn lanes(&self) -> usize {
        self.lanes
    }

    #[must_use]
    pub fn programs(&self) -> &[TangentLaneProgram] {
        &self.programs
    }

    #[must_use]
    pub fn entries(&self) -> &[ColoredTangentEntry] {
        &self.entries
    }
}
