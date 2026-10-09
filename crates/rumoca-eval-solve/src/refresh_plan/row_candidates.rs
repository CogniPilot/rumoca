//! Exact scalar-row candidate inventory for refresh-plan construction.

use rumoca_ir_solve as solve;

use super::OutputRowPosition;
use super::source_catalog::{CanonicalScalarProgram, CanonicalScalarProgramCatalog};

pub(super) struct RefreshRowCandidate<'catalog, 'source> {
    pub(super) equation: usize,
    pub(super) target: usize,
    pub(super) position: OutputRowPosition,
    pub(super) program: &'catalog CanonicalScalarProgram<'source>,
}

/// Preserve equation order, including duplicate targets and refused analyses.
/// Eligibility depends only on the immutable layout and canonical catalog.
pub(super) fn refresh_row_candidates<'catalog, 'source>(
    problem: &'catalog solve::SolveProblem,
    catalog: &'catalog CanonicalScalarProgramCatalog<'source>,
) -> impl Iterator<Item = RefreshRowCandidate<'catalog, 'source>> + 'catalog {
    let targets =
        problem.solve_layout.state_scalar_count()..problem.solve_layout.solver_scalar_count();
    problem
        .continuous
        .implicit_row_targets
        .iter()
        .enumerate()
        .filter_map(move |(equation, target)| {
            let Some(solve::ScalarSlot::Y { index, .. }) = target else {
                return None;
            };
            if !targets.contains(index) {
                return None;
            }
            let position = *catalog.positions().get(&equation)?;
            Some(RefreshRowCandidate {
                equation,
                target: *index,
                position,
                program: catalog.program(position.program_index)?,
            })
        })
}
