//! Provisional source classification becomes admission only after stage coverage.
use super::{NativeEvaluationRefusal, NativeRefreshAssignmentStage};
use crate::{
    CheckedAssertionInvocation, LinearOp, LinearOpSliceKind, SolveEventActionKind, SolveProblem,
    SolvePureCallTable, SolveVisitor,
};
use std::collections::BTreeSet;

#[cfg(test)]
mod tests;

pub(super) struct AssignedReadActions<'model> {
    problem: &'model SolveProblem,
    table: &'model SolvePureCallTable,
}

impl<'model> AssignedReadActions<'model> {
    pub(super) fn classify(
        problem: &'model SolveProblem,
        table: &'model SolvePureCallTable,
    ) -> Option<Self> {
        if problem.events.actions.is_empty()
            || problem
                .events
                .actions
                .iter()
                .any(|action| !is_assigned_read_action(table, action))
        {
            return None;
        }
        Some(Self { problem, table })
    }

    pub(super) fn belongs_to(&self, problem: &SolveProblem) -> bool {
        std::ptr::eq(self.problem, problem)
    }

    pub(super) fn check_stage_coverage(
        &self,
        stages: &[NativeRefreshAssignmentStage],
    ) -> Result<(), NativeEvaluationRefusal> {
        let mut coverage = StageCoverage {
            guards: self,
            actions: BTreeSet::new(),
        };
        for stage in stages {
            coverage.visit_compute_block(stage.value_kernel())?;
        }
        if coverage.actions.len() != self.problem.events.actions.len() {
            return Err(NativeEvaluationRefusal::EventActions);
        }
        Ok(())
    }
}

struct StageCoverage<'guard, 'model> {
    guards: &'guard AssignedReadActions<'model>,
    actions: BTreeSet<usize>,
}

impl SolveVisitor for StageCoverage<'_, '_> {
    type Error = NativeEvaluationRefusal;

    fn enter_linear_op_slice(
        &mut self,
        _kind: LinearOpSliceKind,
        ops: &[LinearOp],
    ) -> Result<(), Self::Error> {
        for (index, operation) in ops.iter().enumerate() {
            self.cover_operation(ops, index, operation)?;
        }
        Ok(())
    }
}

fn is_assigned_read_action(table: &SolvePureCallTable, action: &crate::SolveEventAction) -> bool {
    if action.kind != SolveEventActionKind::Assert || action.clock_owner.is_some() {
        return false;
    }
    let Some(projection) = action.assertion_projection.as_ref() else {
        return false;
    };
    if !table.matches_site(projection.site()) {
        return false;
    }
    let Some(flow) = table
        .owner(projection.site().owner())
        .and_then(|owner| owner.assertion_flow())
    else {
        return false;
    };
    flow.assigned_read_predicates()
        .contains(&projection.predicate_output())
}

impl StageCoverage<'_, '_> {
    fn cover_operation(
        &mut self,
        ops: &[LinearOp],
        index: usize,
        operation: &LinearOp,
    ) -> Result<(), NativeEvaluationRefusal> {
        let LinearOp::PureCall { site, .. } = operation else {
            return Ok(());
        };
        if !site
            .outputs()
            .iter()
            .any(|output| output.assertion_level().is_some())
        {
            return Ok(());
        }
        let invocation = CheckedAssertionInvocation::new(
            self.guards.table,
            ops,
            index,
            &self.guards.problem.events.actions,
        )
        .ok_or(NativeEvaluationRefusal::EventActions)?;
        let flow = invocation
            .owner()
            .assertion_flow()
            .ok_or(NativeEvaluationRefusal::EventActions)?;
        for action in invocation.projections() {
            if !flow
                .assigned_read_predicates()
                .contains(&action.predicate_output())
            {
                return Err(NativeEvaluationRefusal::EventActions);
            }
            self.actions.insert(action.action_index());
        }
        Ok(())
    }
}
