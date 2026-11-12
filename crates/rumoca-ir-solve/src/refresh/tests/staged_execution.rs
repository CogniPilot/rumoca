//! Straight-line reading of refresh plans under each evaluator dispatch.

use super::*;

/// Each step as the coordinates it settles: the targets of an assignment
/// schedule, the block of a projection, or the complete plan.
fn settled(owners: &ContinuousRefreshOwners, steps: &[StagedRefreshStep<'_>]) -> Vec<String> {
    steps
        .iter()
        .map(|step| match step {
            StagedRefreshStep::Assignments(schedule) => {
                let targets = schedule
                    .program_ids()
                    .iter()
                    .flat_map(|id| {
                        owners
                            .exact_assignment_program(*id)
                            .expect("a schedule names issued programs")
                            .target_indices()
                            .iter()
                            .copied()
                    })
                    .collect::<Vec<_>>();
                format!("assign {targets:?}")
            }
            StagedRefreshStep::Project {
                block_index,
                seeds,
                seed_rows,
            } => format!(
                "project {block_index} seeds={} seed_rows={}",
                seeds.is_some(),
                seed_rows.len()
            ),
            StagedRefreshStep::ProjectComplete => "complete".to_string(),
        })
        .collect()
}

fn mixed_owners() -> ContinuousRefreshOwners {
    ContinuousRefreshOwners::checked_for_source(
        &two_output_source(true),
        mixed_projection_exact_plan(true),
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .expect("the projection settles the following exact assignment")
}

#[test]
fn certified_schedule_replays_every_value_stage_in_order() {
    let owners = mixed_owners();
    let plan = owners.algebraic();
    assert_eq!(
        plan.possible_stage_schedules(),
        &[
            RefreshStageSchedule::Certified,
            RefreshStageSchedule::Uncertified
        ]
    );
    let steps = owners
        .staged_refresh_steps(plan, RefreshStageSchedule::Certified)
        .expect("every certified stage is exact");
    // The affine projection block carries no seed schedule, so its stage
    // projects from the coordinate the causal seed sweep left.
    assert_eq!(
        settled(&owners, &steps),
        [
            "assign [0, 1]",
            "project 0 seeds=false seed_rows=0",
            "assign [1]"
        ]
    );
}

#[test]
fn uncertified_schedule_warm_starts_the_complete_projection() {
    let owners = mixed_owners();
    let steps = owners
        .staged_refresh_steps(owners.algebraic(), RefreshStageSchedule::Uncertified)
        .expect("the uncertified dispatch is always executable");
    assert_eq!(settled(&owners, &steps), ["complete"]);

    let causal = ContinuousRefreshOwners::checked_for_source(
        &two_output_source(false),
        two_row_dynamic_plan(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .expect("independent outputs form one causal schedule");
    let plan = causal.algebraic();
    assert_eq!(
        plan.possible_stage_schedules(),
        &[RefreshStageSchedule::Uncertified],
        "a plan without value stages has only the uncertified dispatch"
    );
    let steps = causal
        .staged_refresh_steps(plan, RefreshStageSchedule::Uncertified)
        .expect("the causal seed rows replay one issued schedule");
    assert_eq!(settled(&causal, &steps), ["assign [0, 1]", "complete"]);

    assert!(
        causal
            .staged_refresh_steps(&RefreshPlan::default(), RefreshStageSchedule::Uncertified)
            .expect("an empty plan is executable")
            .is_empty(),
        "an empty plan executes nothing"
    );
}

#[test]
fn staged_reading_refuses_incomplete_and_non_exact_stages() {
    let owners = mixed_owners();
    let mut truncated = owners.algebraic().clone();
    truncated.value_stages.truncate(2);
    assert_eq!(
        owners
            .staged_refresh_steps(&truncated, RefreshStageSchedule::Certified)
            .unwrap_err(),
        StagedRefreshRefusal::IncompleteCoverage
    );

    let mut widened = owners.algebraic().clone();
    for stage in &mut widened.value_stages {
        if let RefreshStage::ProjectionBlock { plan, .. } = stage {
            let mut blocks = plan.blocks.scalar_blocks().to_vec();
            blocks.push(blocks[0].clone());
            plan.blocks = blocks.into();
        }
    }
    assert_eq!(
        owners
            .staged_refresh_steps(&widened, RefreshStageSchedule::Certified)
            .unwrap_err(),
        StagedRefreshRefusal::NonExactStage,
        "a projection stage owns exactly one block"
    );

    let mut reselected = owners.algebraic().clone();
    for stage in &mut reselected.value_stages {
        if let RefreshStage::ExactAssignments { dynamic_rows, .. } = stage {
            *dynamic_rows = selection(2, [0]);
        }
    }
    assert!(
        owners
            .staged_refresh_steps(&reselected, RefreshStageSchedule::Certified)
            .is_err(),
        "a stage whose rows no issued schedule replays is refused"
    );
}

#[test]
fn seed_rescue_restores_seed_targets_and_block_unknowns_once() {
    let plan = mixed_projection_exact_plan(true);
    let block = crate::AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![3, 1],
        tearing: None,
        alternate_charts: Vec::new(),
    };
    let seeds = selection(plan.rows.len(), [0, 1]);
    assert_eq!(
        projection_seed_rescue_targets(plan.selected_rows(&seeds), &block),
        [0, 1, 3]
    );
    let none = RefreshRowSelection::default();
    assert_eq!(
        projection_seed_rescue_targets(plan.selected_rows(&none), &block),
        [1, 3]
    );
}

#[test]
fn grouped_rows_report_their_source_output_offsets() {
    let offsets = two_row_dynamic_plan()
        .rows
        .iter()
        .map(AlgebraicRefreshRow::output_offset)
        .collect::<Vec<_>>();
    assert_eq!(offsets, [0, 1]);
}

fn derivative_component(plan: RefreshPlan) -> crate::fmi::FmiComponent {
    let mut model = crate::SolveModel::default();
    model.problem.layout = crate::VarLayout::from_parts(Default::default(), 2, 0);
    model.problem.solve_layout.algebraic_scalar_count = 2;
    model.initial_y = crate::SolveInitialValues::repeat(0.0, 2).unwrap();
    model.problem.continuous.implicit_rhs = two_output_source(false);
    model.problem.continuous.implicit_row_targets =
        vec![Some(crate::scalar_slot_y(0)), Some(crate::scalar_slot_y(1))].into();
    model.problem.continuous.algebraic_projection_plan.blocks = (0..2)
        .map(|row| crate::AlgebraicProjectionBlock {
            rows: vec![row],
            y_indices: vec![row],
            ..Default::default()
        })
        .collect::<Vec<_>>()
        .into();
    model.problem.continuous.refresh_owners = ContinuousRefreshOwners::checked_for_source(
        &model.problem.continuous.implicit_rhs,
        RefreshPlan::default(),
        plan,
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .unwrap();
    crate::fmi::FmiComponent::construct(model, Vec::new()).unwrap()
}

#[test]
fn derivative_operation_keeps_the_uncertified_complete_projection_step() {
    let component = derivative_component(two_row_dynamic_plan());
    let source = component.runtime_view().try_derivative_operation().unwrap();
    let owners = &source.problem().continuous.refresh_owners;
    let steps = source.staged_dependencies().unwrap();
    assert_eq!(settled(owners, &steps), ["assign [0, 1]", "complete"]);
    let StagedRefreshStep::Assignments(schedule) = steps[0] else {
        panic!("the canonical causal warm start must precede projection")
    };
    assert!(std::ptr::eq(
        schedule,
        owners
            .exact_assignment_schedule(source.dependencies().dynamic_causal_sequence)
            .unwrap()
    ));
}

#[test]
fn derivative_operation_reads_its_own_causal_certificate_without_projection() {
    let mut plan = two_row_dynamic_plan();
    plan.causal_solution_certified = true;
    let component = derivative_component(plan);
    let source = component.runtime_view().try_derivative_operation().unwrap();
    let owners = &source.problem().continuous.refresh_owners;
    assert_eq!(
        settled(owners, &source.staged_dependencies().unwrap()),
        ["assign [0, 1]"]
    );
}
