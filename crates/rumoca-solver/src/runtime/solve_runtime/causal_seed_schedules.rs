//! Differentiate the issued exact assignment segments, preserving their commits.
//! A complete causal certificate permits a pure assignment sweep; certified
//! mixed schedules retain their coupled residual projections. No seed-dependent
//! evaluation result is reused across invocations.
use super::*;

mod mixed;

struct SeedSchedule {
    program: DirectionalProgram,
    targets: Vec<Vec<usize>>,
}

#[derive(Default)]
pub(super) struct CausalSeedSchedules {
    schedules: FxHashMap<solve::RefreshSequenceId, Rc<SeedSchedule>>,
    mixed_steps: Rc<[mixed::Step]>,
    outputs: RefCell<Vec<f64>>,
    incoming: RefCell<Vec<f64>>,
}

impl Clone for CausalSeedSchedules {
    fn clone(&self) -> Self {
        Self {
            schedules: self.schedules.clone(),
            mixed_steps: Rc::clone(&self.mixed_steps),
            outputs: RefCell::default(),
            incoming: RefCell::default(),
        }
    }
}

impl CausalSeedSchedules {
    pub(super) fn construct(
        model: &solve::SolveModel,
        backend: Option<&Rc<dyn SolveExecutionBackend>>,
    ) -> Result<Self, EvalSolveError> {
        let continuous = &model.problem.continuous;
        let owners = &continuous.refresh_owners;
        let plan = owners.derivative();
        let blocks = model.artifacts.continuous.structural.algebraic_projection();
        // The existing source-bound certificate covers primal and full Y+P
        // directional operations, including discarded effects and nested regions.
        if !plan.simultaneous_block_indices.iter().all(|&index| {
            blocks
                .get(index)
                .is_some_and(|block| block.linearization_is_repeatable())
        }) {
            return Ok(Self::default());
        }
        let Some((sequences, mixed_steps)) =
            mixed::select(owners, plan, &model.artifacts.continuous.structural)
        else {
            return Ok(Self::default());
        };
        let mut schedules = FxHashMap::default();
        for schedule in owners.exact_assignment_schedules() {
            if !sequences.contains(&schedule.sequence_id()) {
                continue;
            }
            let primal = interpreted_schedules::prepare_schedule(
                schedule,
                &continuous.implicit_rhs,
                owners,
            )?;
            let source = primal.block.block();
            let directional = solve_eval::ad::lower_scalar_program_block_full_jvp(
                source,
                model.problem.layout.y_scalars(),
            )
            .map_err(|error| EvalSolveError::InvalidRow {
                message: error.to_string(),
                span: error.source_span(),
            })?;
            let mut target_cursor = 0;
            let targets = source
                .programs()
                .iter()
                .map(|ops| {
                    let count = solve::ScalarProgramBlock::program_output_count(ops);
                    let selected = primal.targets[target_cursor..target_cursor + count].to_vec();
                    target_cursor += count;
                    selected
                })
                .collect();
            let program =
                DirectionalProgram::prepare("causal_assignment_seed", directional, backend, None)?;
            schedules.insert(
                schedule.sequence_id(),
                Rc::new(SeedSchedule { program, targets }),
            );
        }
        Ok(Self {
            schedules,
            mixed_steps: mixed_steps.into(),
            ..Self::default()
        })
    }

    pub(super) fn apply(
        &self,
        plan: &solve::RefreshPlan,
        lin: AlgebraicLinearization<'_>,
        y: &[f64],
        seed: &mut [f64],
        context: RowEvalContext<'_>,
    ) -> Result<bool, RuntimeSolveError> {
        if !plan.causal_solution_certified {
            return Ok(false);
        }
        let stages = [
            (
                plan.static_causal_rows().is_empty(),
                plan.static_causal_sequence,
            ),
            (
                plan.dynamic_causal_rows().is_empty(),
                plan.dynamic_causal_sequence,
            ),
        ];
        // Require the whole selected sweep before mutating any seed coordinate.
        if stages
            .iter()
            .any(|(empty, id)| !empty && !self.schedules.contains_key(id))
        {
            return Ok(false);
        }
        self.apply_selected(&stages, lin, y, seed, context)?;
        Ok(true)
    }

    fn apply_selected(
        &self,
        stages: &[(bool, solve::RefreshSequenceId)],
        lin: AlgebraicLinearization<'_>,
        y: &[f64],
        seed: &mut [f64],
        context: RowEvalContext<'_>,
    ) -> Result<(), RuntimeSolveError> {
        let mut incoming = self.incoming.borrow_mut();
        copy_runtime_values_into(&mut incoming, seed, "causal seed snapshot")?;
        let result = self.apply_stages(stages, lin, y, seed, context);
        if result.is_err() {
            seed.copy_from_slice(&incoming);
        }
        result
    }

    fn apply_stages(
        &self,
        stages: &[(bool, solve::RefreshSequenceId)],
        lin: AlgebraicLinearization<'_>,
        y: &[f64],
        seed: &mut [f64],
        context: RowEvalContext<'_>,
    ) -> Result<(), RuntimeSolveError> {
        let mut outputs = self.outputs.borrow_mut();
        for (_, id) in stages.iter().filter(|(empty, _)| !empty) {
            let schedule = &self.schedules[id];
            schedule.apply(lin, y, seed, context, &mut outputs)?;
        }
        Ok(())
    }
}

impl SeedSchedule {
    fn apply(
        &self,
        lin: AlgebraicLinearization<'_>,
        y: &[f64],
        seed: &mut [f64],
        context: RowEvalContext<'_>,
        outputs: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        if self.program.eval_programs_committing(
            0..self.targets.len(),
            (y, lin.params, lin.t),
            seed,
            context,
            &mut |program, outputs, seed| commit_outputs(&self.targets[program], outputs, seed),
        )? {
            return Ok(());
        }
        for (program, targets) in self.targets.iter().enumerate() {
            self.program.eval_program_outputs(
                program,
                solve_eval::JacobianEvalInputs {
                    y,
                    p: lin.params,
                    t: lin.t,
                    seed,
                },
                context,
                outputs,
            )?;
            commit_outputs(targets, outputs, seed)?;
        }
        Ok(())
    }
}

fn commit_outputs(
    targets: &[usize],
    outputs: &[f64],
    seed: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    if outputs.len() != targets.len() || outputs.iter().any(|value| !value.is_finite()) {
        return Err(RuntimeSolveError::solve_ir(
            "invalid causal assignment seed outputs",
        ));
    }
    for (&target, &value) in targets.iter().zip(outputs.iter()) {
        let slot = seed.get_mut(target).ok_or_else(|| {
            RuntimeSolveError::solve_ir("causal assignment seed target outside solver storage")
        })?;
        *slot = value;
    }
    Ok(())
}

#[cfg(test)]
mod tests;
