//! Complete issued dispatches; a refusal never becomes a smaller pass.
use super::*;
use std::collections::BTreeSet;

pub(super) struct Report {
    pub(super) admitted: usize,
    pub(super) failures: usize,
    pub(super) outputs: Vec<Vec<u8>>,
}

pub(super) fn inspect(model: &solve::SolveModel) -> Report {
    let owners = &model.problem.continuous.refresh_owners;
    let cases = fixtures::cases(model);
    let mut identities = BTreeSet::new();
    let mut report = Report {
        admitted: 0,
        failures: 0,
        outputs: Vec::new(),
    };
    let plans = [
        owners.algebraic(),
        owners.derivative(),
        owners.root(),
        owners.event(),
    ]
    .into_iter()
    .chain(owners.clock_events());
    let schedules = plans
        .flat_map(|plan| {
            plan.possible_stage_schedules()
                .iter()
                .filter_map(move |&dispatch| owners.staged_refresh_steps(plan, dispatch).ok())
        })
        .flatten()
        .filter_map(|step| match step {
            solve::StagedRefreshStep::Assignments(schedule) => Some(schedule),
            solve::StagedRefreshStep::Project { seeds, .. } => seeds,
            solve::StagedRefreshStep::ProjectComplete => None,
        });
    for schedule in schedules {
        if identities.insert(schedule.sequence_id()) {
            sequence(model, schedule, &cases, &mut report);
        }
    }
    report
}

fn sequence(
    model: &solve::SolveModel,
    schedule: &solve::ExactRefreshAssignmentSchedule,
    cases: &[fixtures::Case],
    report: &mut Report,
) {
    let problem = &model.problem;
    // A refused sequence remains refused; it is never replaced by a smaller one.
    let Ok(compiled) = rumoca_exec_wasm::compile_exact_assignment_schedule_wasm(
        &problem.continuous.implicit_rhs,
        &problem.continuous.refresh_owners,
        schedule,
        &problem.layout,
        &model.pure_calls,
    ) else {
        return;
    };
    report.admitted += 1;
    let mut runner = execution::Execution::new(
        compiled.module_bytes(),
        compiled.scratch_bytes(),
        &problem.layout,
    );
    for case in cases {
        let result = runner.run(case);
        let passed = execution::canonical(model, schedule, case).is_ok_and(|expected| {
            result.status == Some(0)
                && result.output == expected
                && result.parameter_immutable
                && result.guards_unchanged
        });
        report.failures += usize::from(!passed);
        report.outputs.push(result.output);
    }
}
