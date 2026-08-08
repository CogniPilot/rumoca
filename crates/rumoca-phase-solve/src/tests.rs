//! Solve lowering tests, grouped by the part of the Solve problem they fix.
//!
//! This module owns only the source fixture every group shares; each submodule
//! states one lowering responsibility and builds its own checked DAE models.

use rumoca_core::{SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{ComputeNode, LinearOp, ScalarSlot};

use crate::{LowerError, lower_solve_problem};

mod affine_derivatives;
mod clocks;
mod continuous;
mod discrete;
mod events;
mod expressions;
mod initial_discrete_values;
mod initialization;
mod sampling;
mod temporal;

struct TestSource {
    map: SourceMap,
    source: rumoca_core::SourceId,
}

impl TestSource {
    fn new(text: &str) -> Self {
        let mut map = SourceMap::new();
        let source = map.add("solve.mo", text);
        Self { map, source }
    }

    fn at(&self, start: usize, end: usize) -> dae::DaeProvenance {
        dae::DaeProvenance::source(Span::from_offsets(self.source, start, end)).unwrap()
    }
}
