//! Solve lowering tests, grouped by the part of the Solve problem they fix.
//!
//! This module owns only the source fixture every group shares; each submodule
//! states one lowering responsibility and builds its own checked DAE models.

use rumoca_core::{SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{ComputeNode, LinearOp, ScalarSlot};

use crate::{LowerError, lower_solve_package, lower_solve_problem};

mod affine_blocks;
mod affine_derivatives;
mod charts;
mod clocks;
mod continuous;
mod discrete;
mod events;
mod expressions;
mod fmi;
mod initial_discrete_values;
mod initialization;
mod initialization_owner;
mod input_storage;
mod sampling;
mod scalar_affine;
mod scoped_assignments;
mod temporal;

pub(crate) use scoped_assignments::{scoped_assignment_body, scoped_assignment_loop};

pub(crate) struct TestSource {
    pub(crate) map: SourceMap,
    source: rumoca_core::SourceId,
}

impl TestSource {
    pub(crate) fn new(text: &str) -> Self {
        let mut map = SourceMap::new();
        let source = map.add("solve.mo", text);
        Self { map, source }
    }

    pub(crate) fn at(&self, start: usize, end: usize) -> dae::DaeProvenance {
        dae::DaeProvenance::source(Span::from_offsets(self.source, start, end)).unwrap()
    }
}
