//! Raw diagnostic data; none of these classifications steer native emission.

use rumoca_core::Span;
use rumoca_ir_solve::LinearOp;

/// Source occurrence in canonical IR, not an inferred scalar expansion.
#[derive(Clone, Debug, PartialEq)]
pub enum ExpansionSource {
    Typed { program: usize, operation: usize },
    Row(LinearOp),
    AssignmentOutputs(Vec<LinearOp>),
    FunctionFold(Box<rumoca_ir_solve::FunctionFoldProgram>),
}

/// A deliberately bounded emitting path selected by the existing lowerer.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ForcedExpansion {
    StaticUnroll { limit: u64 },
}

/// One logical copy at an actual lowering site.
#[derive(Clone, Debug, PartialEq)]
pub struct ExpansionCopy {
    pub cells: u64,
    pub method: &'static str,
    pub loop_bounds: Vec<u64>,
}

impl ExpansionCopy {
    /// Capacity-weighted upper bound, never a dynamic measurement. Overflow is
    /// unknown rather than a saturated or silently smaller count.
    pub fn capacity_cells(&self) -> Option<u64> {
        self.loop_bounds
            .iter()
            .try_fold(self.cells, |total, bound| total.checked_mul(*bound))
    }
}

/// Allocated CLIF instructions before optimization for one source operation. Nested entries on the same function
/// are subtracted; all child entries retain their parent occurrence.
#[derive(Clone, Debug, PartialEq)]
pub struct ExpansionEntry {
    pub source: ExpansionSource,
    pub phase: &'static str,
    pub call_table: Option<usize>,
    pub kind: &'static str,
    pub span: Option<Span>,
    pub elements: u64,
    pub parent: Option<usize>,
    pub instructions: u64,
    pub loop_bounds: Vec<u64>,
    pub copies: Vec<ExpansionCopy>,
    pub forced: Option<ForcedExpansion>,
    pub succeeded: bool,
    /// Actual chosen path: deferred fusion, flat map, static matrix, etc.
    pub path: &'static str,
    pub fused_operations: Vec<usize>,
}

impl ExpansionEntry {
    /// Preserve the old single-shape heuristic as a warning, not evidence of
    /// proportional code growth. Scaling controls distinguish fixed overhead.
    pub fn is_suspect(&self) -> bool {
        (self.elements > super::EXPANSION_FLOOR && self.instructions >= self.elements)
            || self
                .copies
                .iter()
                .any(|copy| !copy.loop_bounds.is_empty() && copy.cells > super::EXPANSION_FLOOR)
    }

    pub fn is_unforced_suspect(&self) -> bool {
        self.forced.is_none() && self.is_suspect()
    }
}
