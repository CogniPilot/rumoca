//! Multi-lane tangent programs derived from forward-mode JVP programs.
//!
//! A forward-mode JVP program evaluates one Jacobian-vector product: its seed
//! loads read one direction `v`, and every other value is either independent
//! of `v` (the primal and the coefficients derived from it) or depends on it
//! (the tangent). [`TangentLaneProgram::replicate`] turns such a program into
//! one that evaluates `lanes` directions at once, with the primal computed
//! once:
//!
//! - a scalar operation that reads no seed-dependent value runs once;
//! - a scalar operation that does runs once per lane, on that lane's copy of
//!   its seed-dependent registers;
//! - an interleaved dual tensor aggregate (`lanes: 2`: one primal and one
//!   tangent lane per element) becomes the same aggregate with one primal and
//!   `lanes` tangent lanes per element, so its primal lane runs once and each
//!   tangent lane follows the dual lane's formula.
//!
//! Each lane therefore computes what the one-direction program computes for
//! its direction. Seeds are element-major: lane `l` of seed index `i` is
//! `seed[i * lanes + l]`, which is also how a widened tensor load reads its
//! tangent lanes. Outputs are lane-major: lane `l` of the program's output `o`
//! is output `l * m + o` for `m` outputs.
//!
//! A typed directional pure call runs once per lane, recomputing its primal
//! body in each; its primal results are seed-independent and read once.

mod operands;
mod plan;
mod regions;
mod replicate;

pub use plan::{
    ColoredTangentEntry, ColoredTangentPlan, TangentRowSource, TornTangentPlan,
    TornTangentResidual, TornTangentStep,
};

use crate::linear_op::ScalarProgramRegisterFlow;
use crate::{LinearOp, MAX_TENSOR_LANES, ScalarProgramBlock, ScalarProgramRegisterError};
use regions::Regions;
use replicate::Replication;

/// A checked multi-lane tangent program (see the module documentation).
#[derive(Clone, Debug, PartialEq)]
pub struct TangentLaneProgram {
    ops: Vec<LinearOp>,
    lanes: usize,
    lane_outputs: usize,
    register_count: usize,
}

/// Why a JVP program has no multi-lane form.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum TangentLaneError {
    #[error("tangent lane count {lanes} is outside 1..={max}", max = MAX_TENSOR_LANES - 1)]
    LaneCount { lanes: usize },
    #[error("operation {op_index} ({operation}) has no multi-lane form")]
    Unsupported {
        op_index: usize,
        operation: &'static str,
    },
    #[error("block entry ({row}, {column}) has no row or no column color")]
    Coloring { row: usize, column: usize },
    #[error("the program's dual aggregates overlap out of lane phase")]
    DualLayout,
    #[error("the lane program's register file overflows")]
    RegisterOverflow,
    #[error("the lane program is not a checked scalar program: {0:?}")]
    Register(ScalarProgramRegisterError),
}

impl From<ScalarProgramRegisterError> for TangentLaneError {
    fn from(error: ScalarProgramRegisterError) -> Self {
        Self::Register(error)
    }
}

impl TangentLaneProgram {
    /// The lane operations: a checked scalar program whose tensor
    /// aggregates may carry up to `lanes + 1` interleaved lanes.
    #[must_use]
    pub fn ops(&self) -> &[LinearOp] {
        &self.ops
    }

    /// Number of tangent lanes evaluated together.
    #[must_use]
    pub const fn lanes(&self) -> usize {
        self.lanes
    }

    /// Outputs of one lane (the source program's output count).
    #[must_use]
    pub const fn lane_outputs(&self) -> usize {
        self.lane_outputs
    }

    /// Register capacity proved when the program was checked.
    #[must_use]
    pub const fn register_count(&self) -> usize {
        self.register_count
    }

    /// Evaluate `program`'s seed-dependent operations over `lanes` directions.
    pub fn replicate(program: &[LinearOp], lanes: usize) -> Result<Self, TangentLaneError> {
        if lanes == 0 || lanes >= MAX_TENSOR_LANES {
            return Err(TangentLaneError::LaneCount { lanes });
        }
        let registers = ScalarProgramRegisterFlow::derive(program)?.register_count();
        let lane_outputs = ScalarProgramBlock::program_output_count(program);
        let scalar_end = registers
            .checked_mul(lanes)
            .ok_or(TangentLaneError::RegisterOverflow)?;
        let regions = Regions::derive(program, registers, scalar_end, lanes)
            .ok_or(TangentLaneError::DualLayout)?;
        let mut builder = Replication::new(registers, lanes, lane_outputs, regions)?;
        for (op_index, op) in program.iter().enumerate() {
            builder.push(op_index, op)?;
        }
        let ops = builder.finish();
        let register_count =
            ScalarProgramRegisterFlow::derive_tangent_lanes(&ops)?.register_count();
        Ok(Self {
            ops,
            lanes,
            lane_outputs,
            register_count,
        })
    }
}

fn unsupported(op_index: usize, op: &LinearOp) -> TangentLaneError {
    TangentLaneError::Unsupported {
        op_index,
        operation: op.kind_name(),
    }
}

#[cfg(test)]
mod tests;
