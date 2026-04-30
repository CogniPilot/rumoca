//! Solver-facing prepared IR.
//!
//! This crate contains data consumed by simulation backends after DAE-level
//! structural/lowering phases. It must stay free of DAE evaluation and phase
//! logic.

mod layout;
mod linear_op;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

pub use layout::{ScalarSlot, VarLayout, scalar_slot_p, scalar_slot_y};
pub use linear_op::{BinaryOp, CompareOp, LinearOp, Reg, UnaryOp};

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct RowBlock {
    pub rows: Vec<Vec<LinearOp>>,
}

impl RowBlock {
    pub fn new(rows: Vec<Vec<LinearOp>>) -> Self {
        Self { rows }
    }

    pub fn len(&self) -> usize {
        self.rows.len()
    }

    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveProblem {
    pub layout: VarLayout,
    pub residual: RowBlock,
    pub jacobian_v: RowBlock,
    pub initial_residual: RowBlock,
    pub initial_jacobian_v: RowBlock,
    pub root_conditions: RowBlock,
    pub discrete_rhs: RowBlock,
    pub solve_layout: SolveLayout,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolverNameIndexMaps {
    pub names: Vec<String>,
    pub name_to_idx: HashMap<String, usize>,
    pub base_to_indices: HashMap<String, Vec<usize>>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveLayout {
    pub solver_maps: SolverNameIndexMaps,
    pub parameter_count: usize,
    pub compiled_parameter_len: usize,
    pub input_scalar_names: Vec<String>,
    pub discrete_real_scalar_names: Vec<String>,
    pub discrete_valued_scalar_names: Vec<String>,
}

impl SolveLayout {
    pub fn solver_maps(&self) -> &SolverNameIndexMaps {
        &self.solver_maps
    }

    pub fn input_scalar_names(&self) -> &[String] {
        &self.input_scalar_names
    }

    pub fn has_runtime_parameter_tail(&self) -> bool {
        !self.input_scalar_names.is_empty()
            || !self.discrete_real_scalar_names.is_empty()
            || !self.discrete_valued_scalar_names.is_empty()
    }

    pub fn solver_idx_for_target(&self, target: &str) -> Option<usize> {
        solver_idx_for_target(target, &self.solver_maps.name_to_idx)
    }
}

pub fn solver_idx_for_target(target: &str, name_to_idx: &HashMap<String, usize>) -> Option<usize> {
    if let Some(&idx) = name_to_idx.get(target) {
        return Some(idx);
    }
    if let Some((base, raw_indices)) = target.split_once('[') {
        let indices = raw_indices.strip_suffix(']').unwrap_or(raw_indices);
        let all_one = indices
            .split(',')
            .all(|part| part.trim() == "1" || part.trim().is_empty());
        if all_one {
            return name_to_idx.get(base).copied();
        }
    }
    None
}
