//! The input side of the event-consumer closure (`solve::InputEventReach`).

use std::collections::BTreeSet;

use indexmap::IndexMap;
use rumoca_ir_solve as solve;

use crate::EvalSolveError;

use super::event_dependencies::{EventConsumerReads, extend_event_consumer_reads};
use super::{
    OutputRowPosition, RefreshPlan, RefreshProgramAccess, collect_dependency_closure,
    dependency_blocks_by_target, dependency_target_rows,
};

/// Issue the input reach of `problem` from the event-consumer list and the
/// closure its event refresh plans are built from.
///
/// It fails closed: every input reaches when the dependency owner refuses a
/// program, when a structured update or an event transaction is present, when
/// a `pre` copy reads a solver coordinate, or when the algebraic refresh is
/// not an exact causal schedule.
pub(super) fn issue_input_event_reach<A: RefreshProgramAccess + ?Sized>(
    problem: &solve::SolveProblem,
    implicit_block: &A,
    algebraic: &RefreshPlan,
    output_positions: &IndexMap<usize, OutputRowPosition>,
) -> solve::InputEventReach {
    let layout = &problem.solve_layout;
    let input_scalars = layout.input_scalar_names().len();
    match event_owner_parameter_reads(problem, implicit_block, algebraic, output_positions) {
        Ok(Some(reads)) => solve::InputEventReach::from_parameter_reads(
            layout.parameter_count,
            input_scalars,
            reads,
        ),
        Ok(None) | Err(_) => solve::InputEventReach::all(input_scalars),
    }
}

/// The parameter slots the event owners read, directly or through the
/// algebraic definitions of the non-state coordinates they read; `None` when
/// some owner cannot be analysed.
fn event_owner_parameter_reads<A: RefreshProgramAccess + ?Sized>(
    problem: &solve::SolveProblem,
    implicit_block: &A,
    algebraic: &RefreshPlan,
    output_positions: &IndexMap<usize, OutputRowPosition>,
) -> Result<Option<BTreeSet<usize>>, EvalSolveError> {
    // An event iteration settles a simultaneous block from its own warm start
    // and commits the result as the continuous-time seed, so with such a
    // block the settled coordinate itself is an operand every input reaches.
    if !algebraic.causal_solution_certified
        || algebraic
            .value_stages
            .iter()
            .any(|stage| matches!(stage, solve::RefreshStage::ProjectionBlock { .. }))
    {
        return Ok(None);
    }
    let state_count = problem.solve_layout.state_scalar_count();
    let mut reads = EventConsumerReads::with_parameters();
    extend_event_consumer_reads(&mut reads, problem, state_count, None)?;
    for clock_index in 0..problem.clocks.periodic_event_schedules.len() {
        let Some(clock) = problem.clocks.periodic_clock_id(clock_index) else {
            return Ok(None);
        };
        extend_event_consumer_reads(&mut reads, problem, state_count, Some(clock))?;
    }
    if !extend_instant_owner_reads(&mut reads, problem, state_count)? {
        return Ok(None);
    }
    let (solver_reads, mut parameters, opaque) = reads.into_parts();
    if opaque {
        return Ok(None);
    }
    let span = implicit_block.first_span();
    let target_to_row = dependency_target_rows(algebraic, span)?;
    let block_by_target = dependency_blocks_by_target(problem, algebraic, state_count, span)?;
    // The closure follows only coordinates with an algebraic definition; a
    // non-state coordinate without one has no dependencies to follow.
    if (state_count..problem.solve_layout.solver_scalar_count()).any(|target| {
        !target_to_row.contains_key(&target) && !block_by_target.contains_key(&target)
    }) {
        return Ok(None);
    }
    let (needed, needed_blocks) = collect_dependency_closure(
        algebraic,
        implicit_block,
        solver_reads,
        &target_to_row,
        &block_by_target,
        output_positions,
        state_count,
    )?;
    let mut programs = BTreeSet::new();
    for source in needed.iter().filter_map(|target| target_to_row.get(target)) {
        let Some(program) = implicit_block.source_index(*source) else {
            return Ok(None);
        };
        programs.insert(program);
    }
    let blocks = algebraic.simultaneous_plan.blocks.scalar_blocks();
    for block_index in &needed_blocks {
        let Some(block) = blocks.get(*block_index) else {
            return Ok(None);
        };
        for equation_index in &block.rows {
            let Some(position) = output_positions.get(equation_index) else {
                return Ok(None);
            };
            programs.insert(position.program_index);
        }
    }
    for program_index in programs {
        let Some(program) = implicit_block.program(program_index) else {
            return Ok(None);
        };
        let span = implicit_block.program_span(program_index);
        parameters.extend(
            solve::StructuralPattern::derive_p_dependency_union(program, span)
                .map_err(|error| crate::sparsity::from_pattern_error(error, span))?,
        );
    }
    Ok(Some(parameters))
}

/// The owners an event instant evaluates beside the event-consumer list:
/// dynamic time-event rows, delay rows, clock-partition intermediates and
/// `pre` copies. Returns `false` for a `pre` copy of a solver coordinate,
/// which every event exit latches again whatever the inputs are.
fn extend_instant_owner_reads(
    reads: &mut EventConsumerReads,
    problem: &solve::SolveProblem,
    state_count: usize,
) -> Result<bool, EvalSolveError> {
    let delays = &problem.events.delays;
    for block in [
        &problem.events.dynamic_time_event_rhs,
        &delays.source_rhs,
        &delays.delay_time_rhs,
        &delays.delay_max_rhs,
        &problem.discrete.clock_partition_intermediates,
    ] {
        reads.extend_block(block, state_count)?;
    }
    for binding in &problem.solve_layout.pre_param_bindings {
        match binding.source {
            solve::PreParamSource::P { index } => reads.insert_parameter(index),
            solve::PreParamSource::Y { .. } => return Ok(false),
        }
    }
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::refresh_plan::CanonicalScalarProgramCatalog;

    /// Two inputs `u` and `w`, `algebraics` non-state coordinates and one root
    /// condition `program`.
    fn reach_with_root(
        program: Vec<solve::LinearOp>,
        algebraics: usize,
        algebraic: &RefreshPlan,
    ) -> solve::InputEventReach {
        let mut problem = solve::SolveProblem::default();
        problem.solve_layout.algebraic_scalar_count = algebraics;
        problem.solve_layout.input_scalar_names.push_scalar("u");
        problem.solve_layout.input_scalar_names.push_scalar("w");
        problem.events.root_conditions = solve::ScalarProgramBlock::with_output_indices(
            vec![program],
            vec![solve::source_span_from_offsets(1, 0, 1)],
            vec![0],
        )
        .expect("one root condition is a scalar-program owner");
        let catalog = CanonicalScalarProgramCatalog::construct(&problem.continuous.implicit_rhs)
            .expect("an empty implicit block has a catalog");
        issue_input_event_reach(&problem, &catalog, algebraic, catalog.positions())
    }

    fn certified() -> RefreshPlan {
        RefreshPlan {
            causal_solution_certified: true,
            ..RefreshPlan::default()
        }
    }

    fn bits(reach: &solve::InputEventReach) -> Vec<bool> {
        (0..reach.len()).map(|input| reach.reaches(input)).collect()
    }

    #[test]
    fn a_root_marks_only_the_input_it_reads() {
        let reads_w = vec![
            solve::LinearOp::LoadP { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ];
        assert_eq!(
            bits(&reach_with_root(reads_w, 0, &certified())),
            [false, true]
        );
    }

    #[test]
    fn an_owner_reading_a_coordinate_the_closure_cannot_follow_marks_every_input() {
        // The root reads a non-state coordinate the algebraic refresh does not
        // define, so its dependencies are unknown.
        let reads_y = vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ];
        assert_eq!(
            bits(&reach_with_root(reads_y, 1, &certified())),
            [true, true]
        );
    }

    #[test]
    fn an_uncertified_algebraic_refresh_marks_every_input() {
        let reads_w = vec![
            solve::LinearOp::LoadP { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ];
        assert_eq!(
            bits(&reach_with_root(reads_w, 0, &RefreshPlan::default())),
            [true, true]
        );
    }
}
