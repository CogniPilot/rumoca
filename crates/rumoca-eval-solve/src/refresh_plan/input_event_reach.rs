//! The input side of the event-consumer closure ([`InputEventReach`]).

use std::collections::BTreeSet;

use indexmap::IndexMap;
use rumoca_ir_solve as solve;

use crate::EvalSolveError;

use super::event_dependencies::{EventConsumerReads, extend_event_consumer_reads};
use super::{
    CanonicalScalarProgramCatalog, OutputRowPosition, RefreshPlan, RefreshProgramAccess,
    collect_dependency_closure, dependency_blocks_by_target, dependency_target_rows,
};

/// One bit per declared input scalar: whether a write to it can change an
/// operand of any event owner in the same instant.
///
/// The event owners are the consumers the event refresh plans close over
/// (root conditions, discrete rows, runtime, post-commit and guarded
/// assignments, action conditions and messages) together with the dynamic
/// time-event rows, the delay rows, the clock-partition intermediates and the
/// `pre` copies. A bit is set when one of them reads the input's parameter
/// slot or a non-state solver coordinate whose algebraic definition reads it;
/// a continuous state is never input-dependent within one instant.
///
/// The only way to obtain one is [`derive_input_event_reach`] over a checked
/// problem: no constructor takes caller-supplied reads or bits, and nothing
/// attaches a ready-made fact to another problem. It is not serialized; a
/// decoded model derives it from its own checked refresh owners exactly as a
/// freshly lowered one does.
///
/// ```compile_fail
/// use rumoca_eval_solve::refresh_plan::InputEventReach;
/// // A caller cannot forge an empty reach.
/// let forged = InputEventReach::from_parameter_reads(0, 1, []);
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InputEventReach {
    reached: Box<[bool]>,
}

impl InputEventReach {
    /// Every input reaches an event owner: the fail-closed answer.
    fn all(input_scalars: usize) -> Self {
        Self {
            reached: vec![true; input_scalars].into(),
        }
    }

    /// The inputs among `parameter_reads`, the parameter slots the event
    /// owners and their algebraic closure read. Inputs are the slots
    /// `first_input_parameter .. first_input_parameter + input_scalars`.
    fn from_parameter_reads(
        first_input_parameter: usize,
        input_scalars: usize,
        parameter_reads: impl IntoIterator<Item = usize>,
    ) -> Self {
        let mut reached = vec![false; input_scalars];
        for slot in parameter_reads {
            if let Some(bit) = slot
                .checked_sub(first_input_parameter)
                .and_then(|input| reached.get_mut(input))
            {
                *bit = true;
            }
        }
        Self {
            reached: reached.into(),
        }
    }

    /// Whether a write to input scalar `input` can reach an event owner. An
    /// input this fact does not cover reaches.
    #[must_use]
    pub fn reaches(&self, input: usize) -> bool {
        self.reached.get(input).copied().unwrap_or(true)
    }

    /// The number of input scalars the fact covers.
    #[must_use]
    pub fn len(&self) -> usize {
        self.reached.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.reached.is_empty()
    }
}

/// Derive the input reach of `problem` from its own checked refresh owners
/// and canonical rows.
///
/// Compact owners, whose plans are the whole projection inventory, and
/// canonical rows without a catalog mark every input.
#[must_use]
pub fn derive_input_event_reach(problem: &solve::SolveProblem) -> InputEventReach {
    let input_scalars = problem.solve_layout.input_scalar_names().len();
    let owners = &problem.continuous.refresh_owners;
    if owners.has_compact_source_recipe() {
        return InputEventReach::all(input_scalars);
    }
    match CanonicalScalarProgramCatalog::construct(&problem.continuous.implicit_rhs) {
        Ok(catalog) => {
            issue_input_event_reach(problem, &catalog, owners.algebraic(), catalog.positions())
        }
        Err(_) => InputEventReach::all(input_scalars),
    }
}

/// Issue the input reach of `problem` from the event-consumer list and the
/// closure its event refresh plans are built from.
///
/// It fails closed: every input reaches when the dependency owner refuses a
/// program, when a structured update or an event transaction is present, when
/// a `pre` copy reads a solver coordinate, or when the algebraic refresh is
/// not an exact causal schedule.
fn issue_input_event_reach<A: RefreshProgramAccess + ?Sized>(
    problem: &solve::SolveProblem,
    implicit_block: &A,
    algebraic: &RefreshPlan,
    output_positions: &IndexMap<usize, OutputRowPosition>,
) -> InputEventReach {
    let layout = &problem.solve_layout;
    let input_scalars = layout.input_scalar_names().len();
    match event_owner_parameter_reads(problem, implicit_block, algebraic, output_positions) {
        Ok(Some(reads)) => {
            InputEventReach::from_parameter_reads(layout.parameter_count, input_scalars, reads)
        }
        Ok(None) | Err(_) => InputEventReach::all(input_scalars),
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
    let (solver_reads, parameters, opaque) = reads.into_parts();
    // Reads without their parameter slots cannot bound the reach.
    let Some(mut parameters) = parameters.filter(|_| !opaque) else {
        return Ok(None);
    };
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
            solve::StructuralPattern::derive_execution_p_dependency_union(program, span)
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
    ) -> InputEventReach {
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

    fn bits(reach: &InputEventReach) -> Vec<bool> {
        (0..reach.len()).map(|input| reach.reaches(input)).collect()
    }

    #[test]
    fn only_inputs_among_the_parameter_reads_are_marked() {
        let reach = InputEventReach::from_parameter_reads(4, 3, [0, 3, 5, 7, 900]);
        assert_eq!(bits(&reach), [false, true, false]);
        // An input the fact does not cover reaches.
        assert!(reach.reaches(3));
        assert_eq!(bits(&InputEventReach::all(2)), [true, true]);
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

    /// Emptiness is the input extent the fact covers, never its bits: a
    /// problem without inputs has an empty reach, while inputs that no event
    /// owner reads give a reach that is not empty and reaches nothing.
    #[test]
    fn the_reach_is_empty_only_for_a_problem_without_inputs() {
        let without_inputs = derive_input_event_reach(&solve::SolveProblem::default());
        assert!(without_inputs.is_empty());
        assert_eq!(without_inputs.len(), 0);
        let reads_no_input = vec![
            solve::LinearOp::Const { dst: 0, value: 1.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ];
        let unreached = reach_with_root(reads_no_input, 0, &certified());
        assert!(!unreached.is_empty());
        assert_eq!(bits(&unreached), [false, false]);
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

    // SOURCE-ONLY PROPOSED INSERTION inside the EXISTING
    // crates/rumoca-eval-solve/src/refresh_plan/input_event_reach.rs::tests.
    // Uses that module's existing `super::*`, `bits`, `reach_with_root`, `certified`.
    // Not compiled/executed; sole author must integrate the IDENTICAL fixture in
    // isolated baseline and candidate sources after separate executable admission.

    fn baseline_guard_table(guard: bool, logarithm: bool) -> solve::SolvePureCallTable {
        let profile = solve::SolvePureCallTable::default().arithmetic();
        let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
        let span = solve::source_span_from_offsets(813, 1, 2);
        solve::SolvePureCallTable::construct(profile, |table| {
            table.add_owner(
                solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(813).unwrap()),
                vec![real.clone(), real.clone()],
                vec![solve::SolvePureCallOutput::result(real)],
                span,
                |builder, inputs, outputs| {
                    let x = builder.load(inputs[0], span)?;
                    let q = builder.load(inputs[1], span)?;
                    let checked = logarithm
                        .then(|| builder.unary(solve::SolveUnaryOperator::Log, q, span))
                        .transpose()?
                        .unwrap_or(q);
                    let check = solve::SolveFiniteCheck::Primal {
                        program: 7,
                        operation: 3,
                    };
                    guard
                        .then(|| builder.check_finite(checked, check, span))
                        .transpose()?;
                    builder.store(outputs[0], x, span)
                },
            )?;
            Ok(())
        })
        .unwrap()
    }

    fn baseline_guard_program(
        guard: bool,
        logarithm: bool,
        result_used: bool,
    ) -> Vec<solve::LinearOp> {
        let table = baseline_guard_table(guard, logarithm);
        vec![
            solve::LinearOp::Const { dst: 0, value: 2.0 },
            solve::LinearOp::LoadP { dst: 1, index: 1 },
            solve::LinearOp::PureCall {
                dst_start: 2,
                input_starts: vec![0, 1].into(),
                site: table.owners()[0].call_site(),
            },
            solve::LinearOp::Const {
                dst: 3,
                value: 42.0,
            },
            solve::LinearOp::StoreOutput {
                src: if result_used { 2 } else { 3 },
            },
        ]
    }

    #[test]
    fn baseline_api_guard_only_input_must_reach_event_even_when_result_is_unused() {
        for logarithm in [false, true] {
            for result_used in [false, true] {
                let plain = baseline_guard_program(false, logarithm, result_used);
                assert_eq!(
                    bits(&reach_with_root(plain, 0, &certified())),
                    [false, false]
                );
                let guarded = baseline_guard_program(true, logarithm, result_used);
                assert!(
                    solve::StructuralPattern::derive_p_dependency_union(&guarded, None)
                        .unwrap()
                        .is_empty(),
                    "value-only control must remain unchanged"
                );
                assert_eq!(
                    bits(&reach_with_root(guarded, 0, &certified())),
                    [false, true],
                    "guard-only q is input1: logarithm={logarithm}, result_used={result_used}"
                );
            }
        }
    }

    #[test]
    fn baseline_api_guard_log_has_the_original_fault_and_recovers_on_a_good_call() {
        let table = baseline_guard_table(true, true);
        let owner = &table.owners()[0];
        let input = |index: usize, value: f64| {
            crate::TypedValue::construct(
                owner.inputs()[index].clone(),
                vec![solve::SolveValueKind::Real64(value.to_bits())],
            )
            .unwrap()
        };
        for q in [1.0, -1.0, 1.0] {
            let result =
                crate::eval_pure_call_checked(&table, owner.id(), &[input(0, 2.0), input(1, q)]);
            if q > 0.0 {
                assert_eq!(
                    result.unwrap()[0].elements(),
                    &[solve::SolveValueKind::Real64(2.0f64.to_bits())]
                );
            } else {
                let error = result.unwrap_err();
                assert!(matches!(
                    error,
                    crate::TypedProgramEvalError::NonFinite {
                        check: solve::SolveFiniteCheck::Primal {
                            program: 7,
                            operation: 3
                        },
                        element: 0,
                        ..
                    }
                ));
                assert_eq!(
                    error.source_span(),
                    Some(solve::source_span_from_offsets(813, 1, 2))
                );
            }
        }
    }

    /// `y0 = w` and `y1 = f(2, y0)`, where `f` returns its first input and
    /// checks the second one is finite; one root reads `y1`.
    fn guarded_closure_problem() -> (solve::SolveProblem, crate::PreparedScalarProgramBlock) {
        let site = baseline_guard_table(true, false).owners()[0].call_site();
        let define_y0 = vec![
            solve::LinearOp::LoadP { dst: 0, index: 1 },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 1,
                rhs: 0,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ];
        let define_y1 = vec![
            solve::LinearOp::Const { dst: 0, value: 2.0 },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::PureCall {
                dst_start: 2,
                input_starts: vec![0, 1].into(),
                site,
            },
            solve::LinearOp::LoadY { dst: 3, index: 1 },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 3,
                rhs: 2,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ];
        let block = crate::PreparedScalarProgramBlock::new(
            solve::ScalarProgramBlock::with_program_spans(
                vec![define_y0, define_y1],
                vec![solve::source_span_from_offsets(817, 1, 2); 2],
            )
            .unwrap(),
        )
        .unwrap();
        let mut problem = solve::SolveProblem::default();
        problem.solve_layout.algebraic_scalar_count = 2;
        problem.solve_layout.input_scalar_names.push_scalar("u");
        problem.solve_layout.input_scalar_names.push_scalar("w");
        problem.layout = solve::VarLayout::from_parts(
            (0..2)
                .map(|index| (format!("y{index}"), solve::scalar_slot_y(index)))
                .collect(),
            2,
            0,
        );
        problem.continuous.implicit_rhs =
            solve::ComputeBlock::from_scalar_program_block(block.block().clone());
        problem.continuous.implicit_row_targets = (0..2)
            .map(|index| Some(solve::scalar_slot_y(index)))
            .collect::<Vec<_>>()
            .into();
        problem.continuous.algebraic_projection_plan.blocks = (0..2)
            .map(|index| solve::AlgebraicProjectionBlock {
                rows: vec![index],
                y_indices: vec![index],
                tearing: None,
                alternate_charts: Vec::new(),
            })
            .collect::<Vec<_>>()
            .into();
        problem.events.root_conditions = solve::ScalarProgramBlock::with_output_indices(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            vec![solve::source_span_from_offsets(818, 0, 1)],
            vec![0],
        )
        .unwrap();
        (problem, block)
    }

    #[test]
    fn an_exact_assignment_guard_carries_the_closure_to_its_producer() {
        let (problem, block) = guarded_closure_problem();
        let plan = super::super::build_algebraic_refresh_plan(&problem, &block).unwrap();
        assert!(
            plan.causal_solution_certified
                && !plan
                    .value_stages
                    .iter()
                    .any(|stage| matches!(stage, solve::RefreshStage::ProjectionBlock { .. })),
            "{plan:?}"
        );
        let exact = super::super::issued_exact_assignment_rows(&plan);
        assert!(
            exact.contains_key(&1),
            "y1 is an issued exact assignment whose value does not read y0"
        );
        let catalog =
            CanonicalScalarProgramCatalog::construct(&problem.continuous.implicit_rhs).unwrap();
        // The root reads y1; executing y1's assignment checks y0, which reads w.
        assert_eq!(
            bits(&issue_input_event_reach(
                &problem,
                &catalog,
                &plan,
                catalog.positions()
            )),
            [false, true]
        );
    }

    #[test]
    fn a_selected_discrete_output_carries_its_program_guard_into_the_event_seeds() {
        // The selected output stores a constant; the program also makes an
        // unused call whose guard (when present) reads y0.
        for guard in [false, true] {
            let site = baseline_guard_table(guard, false).owners()[0].call_site();
            let program = vec![
                solve::LinearOp::Const { dst: 0, value: 2.0 },
                solve::LinearOp::LoadY { dst: 1, index: 0 },
                solve::LinearOp::PureCall {
                    dst_start: 2,
                    input_starts: vec![0, 1].into(),
                    site,
                },
                solve::LinearOp::StoreOutput { src: 0 },
            ];
            let mut problem = solve::SolveProblem::default();
            problem.solve_layout.algebraic_scalar_count = 1;
            problem.discrete.rhs = solve::ScalarProgramBlock::with_output_indices(
                vec![program],
                vec![solve::source_span_from_offsets(819, 0, 1)],
                vec![0],
            )
            .unwrap();
            problem.discrete.clock_owners = vec![None];
            let mut reads = EventConsumerReads::default();
            super::super::event_dependencies::extend_event_consumer_reads(
                &mut reads, &problem, 0, None,
            )
            .unwrap();
            let (y, _, _) = reads.into_parts();
            assert_eq!(y.may_contain(0), guard, "guard={guard}");
        }
    }
}
