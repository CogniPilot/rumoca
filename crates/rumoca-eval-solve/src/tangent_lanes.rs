//! Evaluation of checked multi-lane tangent programs.

use std::cell::RefCell;

use rumoca_ir_solve::{ColoredTangentPlan, TangentLaneProgram, TangentRowSource, TornTangentPlan};

use crate::{
    EvalSolveError, OutputCursor, PreparedRowEval, RowEvalContext, RowEvalScratch,
    SimulationRuntimeState, eval_row_prepared_maybe_fast, row_input_requirements,
    validate_input_requirements, validate_output_len,
};

/// A [`TangentLaneProgram`] prepared for repeated evaluation.
///
/// Seeds are element-major (`seed[i * lanes + l]` is lane `l` of seed index
/// `i`) and outputs lane-major (`out[l * m + o]` is lane `l` of output `o`).
pub struct PreparedTangentLaneProgram {
    program: TangentLaneProgram,
    scratch: RefCell<RowEvalScratch>,
}

impl PreparedTangentLaneProgram {
    #[must_use]
    pub fn new(program: TangentLaneProgram) -> Self {
        Self {
            program,
            scratch: RefCell::new(RowEvalScratch::default()),
        }
    }

    #[must_use]
    pub const fn program(&self) -> &TangentLaneProgram {
        &self.program
    }

    /// Evaluate every lane of every output into `out`.
    pub fn eval(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        let local_runtime_state;
        let context = match context.runtime_state {
            Some(_) => context,
            None => {
                local_runtime_state = SimulationRuntimeState::new();
                context.with_runtime_state(&local_runtime_state)
            }
        };
        let ops = self.program.ops();
        validate_output_len(out, self.program.lanes() * self.program.lane_outputs())?;
        validate_input_requirements(row_input_requirements(ops)?, y, p, context.seed)?;
        let mut scratch = self.scratch.borrow_mut();
        let mut sink = OutputCursor::new(out);
        eval_row_prepared_maybe_fast(
            PreparedRowEval::new(ops, self.program.register_count(), y, p, t, context),
            true,
            &mut scratch,
            &mut sink,
        )
    }
}

/// The point, context, and primal rows one tangent evaluation reads. The
/// primal rows evaluate the finite-difference fallback of a row without a
/// multi-lane JVP; without them such a plan declines.
#[derive(Clone, Copy)]
pub struct TangentPoint<'a> {
    pub y: &'a [f64],
    pub p: &'a [f64],
    pub t: f64,
    pub context: RowEvalContext<'a>,
    pub primal: Option<&'a crate::PreparedScalarProgramBlock>,
    /// Relative step of the finite-difference fallback.
    pub fd_step: f64,
}

impl TangentPoint<'_> {
    /// Directional finite difference of implicit row `row` along `direction`
    /// (`(y index, component)` pairs); `None` without primal rows.
    fn row_difference(
        &self,
        row: usize,
        direction: &[(usize, f64)],
        base: f64,
    ) -> Result<Option<f64>, EvalSolveError> {
        let Some((primal, (program, offset))) = self
            .primal
            .and_then(|primal| Some((primal, primal.row_output_position(row)?)))
        else {
            return Ok(None);
        };
        let scale = direction
            .iter()
            .map(|(index, _)| self.y[*index].abs())
            .fold(1.0, f64::max);
        let norm = direction
            .iter()
            .map(|(_, component)| component.abs())
            .fold(0.0, f64::max);
        if norm == 0.0 {
            return Ok(Some(0.0));
        }
        let step = self.fd_step * scale / norm;
        let mut perturbed = self.y.to_vec();
        for &(index, component) in direction {
            perturbed[index] += step * component;
        }
        let value = primal.eval_row_output_unchecked_with_context(
            program,
            offset,
            &perturbed,
            self.p,
            self.t,
            self.context,
        )?;
        Ok(Some((value - base) / step))
    }

    fn row_value(&self, row: usize) -> Result<Option<f64>, EvalSolveError> {
        let Some((primal, (program, offset))) = self
            .primal
            .and_then(|primal| Some((primal, primal.row_output_position(row)?)))
        else {
            return Ok(None);
        };
        primal
            .eval_row_output_unchecked_with_context(
                program,
                offset,
                self.y,
                self.p,
                self.t,
                self.context,
            )
            .map(Some)
    }
}

/// The reduced tear Jacobian and the recovered-coordinate sensitivities of
/// one torn block, both row-major with one column per tear.
#[derive(Clone, Debug, PartialEq)]
pub struct TornTangentJacobian {
    pub residual: Vec<f64>,
    pub recovered: Vec<f64>,
}

/// A [`TornTangentPlan`] prepared for repeated evaluation.
pub struct TornTangentEvaluator {
    plan: TornTangentPlan,
    programs: Vec<PreparedTangentLaneProgram>,
}

impl TornTangentEvaluator {
    #[must_use]
    pub fn new(plan: TornTangentPlan) -> Self {
        let programs = plan
            .programs()
            .iter()
            .cloned()
            .map(PreparedTangentLaneProgram::new)
            .collect();
        Self { plan, programs }
    }

    #[must_use]
    pub const fn plan(&self) -> &TornTangentPlan {
        &self.plan
    }

    /// Evaluate the reduced tear Jacobian at `point`, whose causal
    /// coordinates hold the sweep of its tears. `None` when a causal
    /// coefficient vanishes or a finite-difference row has no primal rows.
    pub fn eval(
        &self,
        point: TangentPoint<'_>,
    ) -> Result<Option<TornTangentJacobian>, EvalSolveError> {
        let lanes = self.plan.lanes();
        let tears = lanes - 1;
        let mut seed = vec![0.0; point.y.len() * lanes];
        for (column, &target) in self.plan.tear_targets().iter().enumerate() {
            seed[target * lanes + column] = 1.0;
        }
        let mut active = self.plan.tear_targets().to_vec();
        let mut recovered = Vec::with_capacity(self.plan.steps().len() * tears);
        let mut out = Vec::new();
        for step in self.plan.steps() {
            seed[step.target * lanes + tears] = 1.0;
            let Some(tangents) =
                self.row_tangents(point, &seed, &active, step.row, step.source, &mut out)?
            else {
                return Ok(None);
            };
            let coefficient = tangents[tears];
            if coefficient == 0.0 || !coefficient.is_finite() {
                return Ok(None);
            }
            seed[step.target * lanes + tears] = 0.0;
            for (lane, tangent) in tangents[..tears].iter().enumerate() {
                let value = -tangent / coefficient;
                seed[step.target * lanes + lane] = value;
                recovered.push(value);
            }
            active.push(step.target);
        }
        let mut residual = vec![0.0; tears * tears];
        for (row, entry) in self.plan.residuals().iter().enumerate() {
            let Some(tangents) =
                self.row_tangents(point, &seed, &active, entry.row, entry.source, &mut out)?
            else {
                return Ok(None);
            };
            residual[row * tears..(row + 1) * tears].copy_from_slice(&tangents[..tears]);
        }
        Ok(Some(TornTangentJacobian {
            residual,
            recovered,
        }))
    }

    /// Every lane's tangent of implicit row `row` under `seed`.
    fn row_tangents(
        &self,
        point: TangentPoint<'_>,
        seed: &[f64],
        active: &[usize],
        row: usize,
        source: TangentRowSource,
        out: &mut Vec<f64>,
    ) -> Result<Option<Vec<f64>>, EvalSolveError> {
        let lanes = self.plan.lanes();
        let TangentRowSource::Lanes { program, output } = source else {
            return self.difference_tangents(point, seed, active, row);
        };
        let prepared = &self.programs[program];
        let outputs = prepared.program().lane_outputs();
        out.resize(lanes * outputs, 0.0);
        prepared.eval(
            point.y,
            point.p,
            point.t,
            RowEvalContext {
                seed: Some(seed),
                ..point.context
            },
            out,
        )?;
        Ok(Some(
            (0..lanes)
                .map(|lane| out[lane * outputs + output])
                .collect(),
        ))
    }

    /// Finite-difference tangents of a row without a multi-lane JVP: each
    /// tangent lane along the tangents set so far, the coefficient lane along
    /// the row's own target.
    fn difference_tangents(
        &self,
        point: TangentPoint<'_>,
        seed: &[f64],
        active: &[usize],
        row: usize,
    ) -> Result<Option<Vec<f64>>, EvalSolveError> {
        let lanes = self.plan.lanes();
        let Some(base) = point.row_value(row)? else {
            return Ok(None);
        };
        let target = self
            .plan
            .steps()
            .iter()
            .find(|step| step.row == row)
            .map(|step| step.target);
        let mut tangents = Vec::with_capacity(lanes);
        for lane in 0..lanes {
            let mut direction = active
                .iter()
                .map(|&index| (index, seed[index * lanes + lane]))
                .filter(|(_, component)| *component != 0.0)
                .collect::<Vec<_>>();
            if lane + 1 == lanes {
                direction.extend(target.map(|target| (target, 1.0)));
            }
            match point.row_difference(row, &direction, base)? {
                Some(tangent) => tangents.push(tangent),
                None => return Ok(None),
            }
        }
        Ok(Some(tangents))
    }
}

/// A [`ColoredTangentPlan`] prepared for repeated evaluation.
pub struct ColoredTangentEvaluator {
    plan: ColoredTangentPlan,
    programs: Vec<PreparedTangentLaneProgram>,
}

impl ColoredTangentEvaluator {
    #[must_use]
    pub fn new(plan: ColoredTangentPlan) -> Self {
        let programs = plan
            .programs()
            .iter()
            .cloned()
            .map(PreparedTangentLaneProgram::new)
            .collect();
        Self { plan, programs }
    }

    #[must_use]
    pub const fn plan(&self) -> &ColoredTangentPlan {
        &self.plan
    }

    /// The value of every plan entry at `point`, in entry order; `rows` maps
    /// block rows to implicit rows. `None` when a finite-difference entry has
    /// no primal rows.
    pub fn eval(
        &self,
        point: TangentPoint<'_>,
        rows: &[usize],
    ) -> Result<Option<Vec<f64>>, EvalSolveError> {
        let lanes = self.plan.lanes();
        let mut seed = vec![0.0; point.y.len() * lanes];
        for (&column, &color) in self.plan.columns().iter().zip(self.plan.colors()) {
            seed[column * lanes + color] = 1.0;
        }
        let mut outputs = Vec::with_capacity(self.programs.len());
        for prepared in &self.programs {
            let mut out = vec![0.0; lanes * prepared.program().lane_outputs()];
            prepared.eval(
                point.y,
                point.p,
                point.t,
                RowEvalContext {
                    seed: Some(&seed),
                    ..point.context
                },
                &mut out,
            )?;
            outputs.push(out);
        }
        let mut values = Vec::with_capacity(self.plan.entries().len());
        for entry in self.plan.entries() {
            let value = match entry.source {
                TangentRowSource::Lanes { program, output } => {
                    let width = self.programs[program].program().lane_outputs();
                    Some(outputs[program][entry.lane * width + output])
                }
                TangentRowSource::FiniteDifference => {
                    self.difference_entry(point, rows[entry.row], entry.lane)?
                }
            };
            let Some(value) = value else {
                return Ok(None);
            };
            values.push(value);
        }
        Ok(Some(values))
    }

    /// Finite difference of implicit row `row` along every column of color `lane`.
    fn difference_entry(
        &self,
        point: TangentPoint<'_>,
        row: usize,
        lane: usize,
    ) -> Result<Option<f64>, EvalSolveError> {
        let Some(base) = point.row_value(row)? else {
            return Ok(None);
        };
        let direction = self
            .plan
            .columns()
            .iter()
            .zip(self.plan.colors())
            .filter(|(_, color)| **color == lane)
            .map(|(column, _)| (*column, 1.0))
            .collect::<Vec<_>>();
        point.row_difference(row, &direction, base)
    }
}
