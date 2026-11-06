//! The one-direction torn tangent as committing sweeps: one over the step
//! coefficients, then one per tear column through the steps and reduced rows.

use std::ops::Range;

use rumoca_ir_solve::TornTangentPlan;

use super::{
    DirectionSweep, EvalSolveError, TornTangentEvaluator, TornTangentJacobian,
    causal_coefficient_is_finite, seed_group,
};

/// The fresh programs of a one-direction plan in sweep order, each with the
/// entries its one evaluation answers; fixed when the evaluator is prepared.
pub(super) struct SweepLists {
    /// The program of every step group, in step order.
    step_programs: Vec<usize>,
    /// The steps each entry of `step_programs` answers.
    step_groups: Vec<Range<usize>>,
    /// A tear column's programs: `step_programs`, then every residual group.
    column_programs: Vec<usize>,
    /// The reduced rows each residual program of `column_programs` answers.
    residual_groups: Vec<Range<usize>>,
}

impl SweepLists {
    pub(super) fn new(plan: &TornTangentPlan) -> Self {
        let step_groups: Vec<_> = (0..plan.steps().len())
            .filter(|&index| plan.steps()[index].group > 0)
            .map(|index| index..index + plan.steps()[index].group)
            .collect();
        let residual_groups: Vec<_> = (0..plan.residuals().len())
            .filter(|&row| plan.residuals()[row].group > 0)
            .map(|row| row..row + plan.residuals()[row].group)
            .collect();
        let step_programs: Vec<_> = step_groups
            .iter()
            .map(|group| plan.steps()[group.start].source.program)
            .collect();
        let column_programs = step_programs
            .iter()
            .copied()
            .chain(
                residual_groups
                    .iter()
                    .map(|group| plan.residuals()[group.start].source.program),
            )
            .collect();
        Self {
            step_programs,
            step_groups,
            column_programs,
            residual_groups,
        }
    }
}

/// How one sweep of the torn tangent ended.
enum Swept {
    /// Every program ran and committed.
    Complete,
    /// The owner declined before any program ran.
    Declined,
    /// A coefficient was not finite; the commit that read it stopped the sweep.
    NonFinite,
}

impl TornTangentEvaluator {
    /// The one-direction form of [`Self::eval_directions`] as committing
    /// sweeps over `seed`, which is all zero on entry. `Ok(None)` when the
    /// owner declines the first sweep, with `seed` still all zero; the caller
    /// restores the tear and step targets on every other return.
    pub(super) fn eval_direction_sweeps<E: From<EvalSolveError>>(
        &self,
        lists: &SweepLists,
        sweep: &mut DirectionSweep<'_, E>,
        seed: &mut [f64],
    ) -> Result<Option<Option<TornTangentJacobian>>, E> {
        let steps = self.plan.steps();
        let tears = self.plan.lanes() - 1;
        let mut coefficients = vec![0.0; steps.len()];
        let mut swept_any = false;
        if !lists.step_programs.is_empty() {
            match self.sweep_coefficients(lists, sweep, seed, &mut coefficients)? {
                Swept::Complete => swept_any = true,
                Swept::Declined => return Ok(None),
                Swept::NonFinite => return Ok(Some(None)),
            }
        }
        let mut columns = ColumnCommits {
            plan: &self.plan,
            lists,
            coefficients: &coefficients,
            column: 0,
            committed: 0,
            recovered: vec![0.0; steps.len() * tears],
            residual: vec![0.0; tears * tears],
        };
        for (column, &tear) in self.plan.tear_targets().iter().enumerate() {
            seed[tear] = 1.0;
            columns.column = column;
            columns.committed = 0;
            let ran = sweep(&lists.column_programs, seed, &mut |program, out, seed| {
                columns.commit(program, out, seed)
            })?;
            match finished(ran, columns.committed, lists.column_programs.len())? {
                Swept::Complete => swept_any = true,
                Swept::Declined if !swept_any => {
                    seed[tear] = 0.0;
                    return Ok(None);
                }
                Swept::Declined | Swept::NonFinite => {
                    return Err(sweep_defect("declined after an earlier sweep ran"));
                }
            }
            seed[tear] = 0.0;
            for step in steps {
                seed[step.target] = 0.0;
            }
        }
        Ok(Some(Some(TornTangentJacobian {
            residual: columns.residual,
            recovered: columns.recovered,
        })))
    }

    /// Every step group's coefficient, its members seeded alone: group `k`
    /// is seeded before the sweep or by the commit of group `k - 1`, and
    /// unseeded by its own commit. A non-finite coefficient stops the sweep
    /// through the one error this invocation's commit returns.
    fn sweep_coefficients<E: From<EvalSolveError>>(
        &self,
        lists: &SweepLists,
        sweep: &mut DirectionSweep<'_, E>,
        seed: &mut [f64],
        coefficients: &mut [f64],
    ) -> Result<Swept, E> {
        let steps = self.plan.steps();
        let mut commits = CoefficientCommits {
            steps,
            lists,
            coefficients,
            committed: 0,
            stopped: false,
        };
        seed_group(seed, &steps[lists.step_groups[0].clone()], (1, 0), 1.0);
        let ran = sweep(&lists.step_programs, seed, &mut |program, out, seed| {
            commits.commit(program, out, seed)
        });
        match (ran, commits.stopped) {
            (Err(_), true) => Ok(Swept::NonFinite),
            (Ok(_), true) => Err(sweep_defect("continued after its commit stopped it")),
            (Err(error), false) => Err(error),
            (Ok(ran), false) => {
                let swept = finished(ran, commits.committed, lists.step_programs.len())?;
                if matches!(swept, Swept::Declined) {
                    seed_group(seed, &steps[lists.step_groups[0].clone()], (1, 0), 0.0);
                }
                Ok(swept)
            }
        }
    }
}

/// The commits of the coefficient sweep, with the invocation-local record of
/// whether one of them stopped it.
struct CoefficientCommits<'a> {
    steps: &'a [rumoca_ir_solve::TornTangentStep],
    lists: &'a SweepLists,
    coefficients: &'a mut [f64],
    committed: usize,
    stopped: bool,
}

impl CoefficientCommits<'_> {
    /// Read group `committed`'s coefficients, unseed it and seed the next.
    fn commit<E: From<EvalSolveError>>(
        &mut self,
        program: usize,
        out: &[f64],
        seed: &mut [f64],
    ) -> Result<(), E> {
        let group = next_group(&self.lists.step_programs, self.committed, program)?;
        let group = self.lists.step_groups[group].clone();
        seed_group(seed, &self.steps[group.clone()], (1, 0), 0.0);
        self.committed += 1;
        for index in group.clone() {
            self.coefficients[index] = out[self.steps[index].source.output];
        }
        if !self.coefficients[group]
            .iter()
            .all(|coefficient| causal_coefficient_is_finite(*coefficient))
        {
            self.stopped = true;
            return Err(sweep_defect("stopped at a non-finite causal coefficient"));
        }
        if let Some(next) = self.lists.step_groups.get(self.committed) {
            seed_group(seed, &self.steps[next.clone()], (1, 0), 1.0);
        }
        Ok(())
    }
}

/// The commits of one tear column's sweep: the steps' recovered tangents,
/// each seeding its target, then the reduced rows.
struct ColumnCommits<'a> {
    plan: &'a TornTangentPlan,
    lists: &'a SweepLists,
    coefficients: &'a [f64],
    column: usize,
    committed: usize,
    recovered: Vec<f64>,
    residual: Vec<f64>,
}

impl ColumnCommits<'_> {
    fn commit<E: From<EvalSolveError>>(
        &mut self,
        program: usize,
        out: &[f64],
        seed: &mut [f64],
    ) -> Result<(), E> {
        let group = next_group(&self.lists.column_programs, self.committed, program)?;
        self.committed += 1;
        let tears = self.plan.lanes() - 1;
        let column = self.column;
        if let Some(steps) = self.lists.step_groups.get(group) {
            for index in steps.clone() {
                let step = &self.plan.steps()[index];
                let value = -out[step.source.output] / self.coefficients[index];
                seed[step.target] = value;
                self.recovered[index * tears + column] = value;
            }
        } else {
            let rows = &self.lists.residual_groups[group - self.lists.step_groups.len()];
            for row in rows.clone() {
                self.residual[row * tears + column] = out[self.plan.residuals()[row].source.output];
            }
        }
        Ok(())
    }
}

/// The position of the `committed`-th commit of a sweep over `programs`,
/// checking that it commits `program`, the next one in order.
fn next_group<E: From<EvalSolveError>>(
    programs: &[usize],
    committed: usize,
    program: usize,
) -> Result<usize, E> {
    match programs.get(committed) {
        Some(&expected) if expected == program => Ok(committed),
        _ => Err(sweep_defect("committed outside its program order")),
    }
}

/// How a sweep that returned `ran` after `committed` of `expected` commits
/// ended; any other combination is an owner defect.
fn finished<E: From<EvalSolveError>>(
    ran: bool,
    committed: usize,
    expected: usize,
) -> Result<Swept, E> {
    match (ran, committed) {
        (true, committed) if committed == expected => Ok(Swept::Complete),
        (false, 0) => Ok(Swept::Declined),
        _ => Err(sweep_defect(
            "ended without committing every program exactly once",
        )),
    }
}

fn sweep_defect<E: From<EvalSolveError>>(what: &str) -> E {
    E::from(EvalSolveError::InvalidRow {
        message: format!("torn tangent sweep {what}"),
        span: None,
    })
}
