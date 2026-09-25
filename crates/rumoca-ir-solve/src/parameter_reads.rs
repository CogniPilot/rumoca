//! Which parameter slots a Solve problem's programs read.

use std::collections::BTreeSet;

use crate::{LinearOp, LinearOpSliceKind, SolveProblem, SolveVisitor, TensorInputKind};

/// Every parameter storage index some program of `problem` reads.
///
/// A parameter slot outside this set is folded into the programs that use it
/// (SPEC_0040 STRUCT-T10(a)), so setting it could not take effect: the FMI C
/// profile publishes such a parameter as a constant, and every override
/// surface rejects it.
#[must_use]
pub fn read_parameter_slots(problem: &SolveProblem) -> BTreeSet<usize> {
    let mut reads = ParameterReads(BTreeSet::new());
    let Ok(()) = reads.visit_solve_problem(problem);
    reads.0
}

struct ParameterReads(BTreeSet<usize>);

impl SolveVisitor for ParameterReads {
    type Error = std::convert::Infallible;
    fn visit_linear_op(
        &mut self,
        _kind: LinearOpSliceKind,
        _index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        match op {
            LinearOp::LoadP { index, .. } => {
                self.0.insert(*index);
            }
            LinearOp::LoadIndexedP { base, count, .. } => self.0.extend(*base..*base + *count),
            LinearOp::TensorLoad {
                input: TensorInputKind::P,
                input_start,
                count,
                ..
            } => self.0.extend(*input_start..*input_start + *count),
            _ => {}
        }
        Ok(())
    }
}
