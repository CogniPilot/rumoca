//! Differentiate a construction-certified mixed assignment/projection sweep.
use super::*;

#[derive(Clone)]
pub(super) enum Step {
    Assignment(solve::RefreshSequenceId),
    Projection {
        block_index: usize,
        plan: solve::AlgebraicProjectionPlan,
    },
}

type Selection = (BTreeSet<solve::RefreshSequenceId>, Vec<Step>);

pub(super) fn select(
    owners: &solve::ContinuousRefreshOwners,
    plan: &solve::RefreshPlan,
    structural: &solve::ContinuousStructuralArtifacts,
) -> Option<Selection> {
    if plan.causal_solution_certified {
        return Some((
            BTreeSet::from([plan.static_causal_sequence, plan.dynamic_causal_sequence]),
            Vec::new(),
        ));
    }
    if !plan.value_stage_schedule_is_certified(structural) {
        return None;
    }
    let issued = owners
        .staged_refresh_steps(plan, plan.stage_schedule(structural))
        .ok()?;
    let mut sequences = BTreeSet::new();
    let mut steps = Vec::new();
    for step in issued {
        match step {
            solve::StagedRefreshStep::Assignments(schedule) => {
                sequences.insert(schedule.sequence_id());
                steps.push(Step::Assignment(schedule.sequence_id()));
            }
            solve::StagedRefreshStep::Project { block_index, .. } => {
                let projection = plan.value_stages.iter().find_map(|stage| match stage {
                    solve::RefreshStage::ProjectionBlock {
                        block_index: index,
                        plan,
                        ..
                    } if *index == block_index => Some(plan.clone()),
                    _ => None,
                })?;
                steps.push(Step::Projection {
                    block_index,
                    plan: projection,
                });
            }
            solve::StagedRefreshStep::ProjectComplete => return None,
        }
    }
    Some((sequences, steps))
}

impl CausalSeedSchedules {
    #[cfg(test)]
    pub(in super::super) fn has_mixed_steps(&self) -> bool {
        !self.mixed_steps.is_empty()
    }

    pub(in super::super) fn apply_mixed(
        &self,
        lin: AlgebraicLinearization<'_>,
        y: &[f64],
        seed: &mut [f64],
        context: RowEvalContext<'_>,
        mut project: impl FnMut(
            &solve::AlgebraicProjectionPlan,
            usize,
            &mut [f64],
        ) -> Result<(), RuntimeSolveError>,
    ) -> Result<bool, RuntimeSolveError> {
        if self.mixed_steps.is_empty() {
            return Ok(false);
        }
        // Preparing the whole issued sweep must succeed before any seed changes.
        if self.mixed_steps.iter().any(|step| match step {
            Step::Assignment(id) => !self.schedules.contains_key(id),
            Step::Projection { .. } => false,
        }) {
            return Ok(false);
        }
        let mut incoming = self.incoming.borrow_mut();
        copy_runtime_values_into(&mut incoming, seed, "mixed seed snapshot")?;
        let result = self.apply_mixed_steps(lin, y, seed, context, &mut project);
        if let Err(error) = result {
            seed.copy_from_slice(&incoming);
            return Err(error);
        }
        Ok(true)
    }

    fn apply_mixed_steps(
        &self,
        lin: AlgebraicLinearization<'_>,
        y: &[f64],
        seed: &mut [f64],
        context: RowEvalContext<'_>,
        project: &mut impl FnMut(
            &solve::AlgebraicProjectionPlan,
            usize,
            &mut [f64],
        ) -> Result<(), RuntimeSolveError>,
    ) -> Result<(), RuntimeSolveError> {
        let mut outputs = self.outputs.borrow_mut();
        for step in self.mixed_steps.iter() {
            match step {
                Step::Assignment(id) => {
                    self.schedules[id].apply(lin, y, seed, context, &mut outputs)?
                }
                Step::Projection { block_index, plan } => project(plan, *block_index, seed)?,
            }
        }
        Ok(())
    }
}
