//! The coupled event residual's exact directional derivative.
//!
//! The residual evaluates its rows at the settled point: the unknowns written
//! into the event coordinates, the root relation overrides pinned, and the
//! runtime assignments applied until stable. Its tangent follows the same
//! path: the direction seeds the unknowns, the pinned relation memories stay
//! constant, the runtime assignments' JVP carries the seed to their targets
//! until it stops changing, and each residual row takes its target's seed
//! minus its row's JVP (the implicit rows' JVP directly).

mod program;

use std::borrow::Cow;
use std::rc::Rc;

use rumoca_eval_solve::{EvalSolveError, RowEvalContext};
use rumoca_ir_solve as solve;

use super::super::SolveExecutionBackend;
use super::super::initial_projection::write_update_tangents;
use super::{CoupledEventResidual, CoupledEventSystem, CoupledEventUnknown};
use crate::RuntimeSolveError;
use crate::runtime::solve_events::event_eval_params_with_relation_overrides;
use program::TangentProgram;

/// The discrete event rows' JVP programs a coupled solve differentiates,
/// prepared with the runtime before any coupled solve can execute.
#[derive(Clone)]
pub(in super::super) struct CoupledEventJvps {
    discrete: Option<TangentProgram>,
    runtime: Option<TangentProgram>,
    guarded: Option<TangentProgram>,
    structured: Option<TangentProgram>,
}

impl CoupledEventJvps {
    pub(in super::super) fn prepare(
        artifacts: &solve::DiscreteSolveArtifacts,
        backend: Option<&Rc<dyn SolveExecutionBackend>>,
    ) -> Result<Self, EvalSolveError> {
        let prepare = |label, block: &Option<solve::ScalarProgramBlock>| {
            block
                .as_ref()
                .map(|source| TangentProgram::prepare(label, source, backend))
                .transpose()
        };
        Ok(Self {
            discrete: prepare("event_discrete_jvp", &artifacts.rhs_jacobian_v)?,
            runtime: prepare(
                "event_runtime_assignment_jvp",
                &artifacts.runtime_assignment_jacobian_v,
            )?,
            guarded: prepare("event_guarded_jvp", &artifacts.guarded_jacobian_v)?,
            structured: prepare("event_structured_jvp", &artifacts.structured_jacobian_v)?,
        })
    }
}

fn required<'a>(
    block: &'a Option<TangentProgram>,
    rows: &str,
) -> Result<&'a TangentProgram, RuntimeSolveError> {
    block
        .as_ref()
        .ok_or_else(|| RuntimeSolveError::DirectionalDerivativeUnavailable {
            reason: format!("the {rows} have no directional derivative"),
        })
}

/// A slot's position in a `[solver-y | parameter]` seed.
fn seed_index(slot: solve::ScalarSlot, y_len: usize) -> Option<usize> {
    match slot {
        solve::ScalarSlot::Y { index, .. } => Some(index),
        solve::ScalarSlot::P { index, .. } => Some(y_len + index),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => None,
    }
}

fn seed_value(seed: &[f64], slot: solve::ScalarSlot, y_len: usize) -> f64 {
    seed_index(slot, y_len).map_or(0.0, |index| seed[index])
}

/// The point and seed a residual row's tangent is evaluated at.
#[derive(Clone, Copy)]
struct RowPoint<'a> {
    y: &'a [f64],
    p: &'a [f64],
    seed: &'a [f64],
    context: RowEvalContext<'a>,
}

impl CoupledEventSystem<'_> {
    /// The residual's directional derivative at `unknowns` along `direction`.
    pub(super) fn residual_tangent(
        &self,
        unknowns: &[f64],
        direction: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let (y, p) = self.settled_point(unknowns)?;
        let y_len = y.len();
        let seed_len = (y_len + p.len()).max(
            self.runtime
                .scalar
                .implicit_jacobian_v
                .source()
                .requirements()
                .seed_len,
        );
        let mut seed = vec![0.0; seed_len];
        for (unknown, value) in self.inventory.unknowns.iter().zip(direction) {
            let index = match *unknown {
                CoupledEventUnknown::Y(index) => index,
                CoupledEventUnknown::P(index) => y_len + index,
            };
            seed[index] = *value;
        }
        let targets = &self
            .runtime
            .model
            .problem
            .events
            .root_relation_memory_targets;
        for &(root, _) in self.snapshot.root_relation_overrides {
            if let Some(index) = targets
                .get(root)
                .copied()
                .flatten()
                .and_then(|target| seed_index(target, y_len))
            {
                seed[index] = 0.0;
            }
        }
        self.runtime_assignment_tangents(&y, &p, &mut seed)?;
        self.row_tangents(&y, &p, &seed, out)
    }

    /// Carry `seed` through the runtime assignments until it stops changing.
    fn runtime_assignment_tangents(
        &self,
        y: &[f64],
        p: &[f64],
        seed: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let targets = &self
            .runtime
            .model
            .problem
            .discrete
            .runtime_assignment_targets;
        if targets.is_empty() {
            return Ok(());
        }
        let block = required(&self.jvps.runtime, "runtime assignments")?;
        let mut values = vec![0.0; targets.len()];
        for _ in 0..self.max_iters {
            block.eval_with_context(
                y,
                p,
                self.t,
                RowEvalContext {
                    seed: Some(seed),
                    ..self.runtime.row_eval_context()
                },
                &mut values,
            )?;
            if !write_update_tangents(targets, &values, y.len(), seed) {
                return Ok(());
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "runtime assignment tangents did not converge at t={}",
            self.t
        )))
    }

    /// Each residual row's tangent under `seed` at the settled point.
    fn row_tangents(
        &self,
        y: &[f64],
        p: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let runtime = self.runtime;
        let context = RowEvalContext {
            seed: Some(seed),
            ..runtime.row_eval_context()
        };
        let mut implicit = vec![0.0; runtime.scalar.implicit_jacobian_v.source().len()];
        runtime.scalar.implicit_jacobian_v.eval(
            rumoca_eval_solve::JacobianEvalInputs {
                y,
                p,
                t: self.t,
                seed,
            },
            context,
            &mut implicit,
        )?;
        let row_p: Cow<'_, [f64]> = if self.snapshot.root_relation_overrides.is_empty() {
            Cow::Borrowed(p)
        } else {
            Cow::Owned(event_eval_params_with_relation_overrides(
                &runtime.model.problem.events.root_relation_memory_targets,
                self.snapshot.root_relation_overrides,
                p,
            )?)
        };
        let point = RowPoint {
            y,
            p: &row_p,
            seed,
            context,
        };
        let mut guarded = (usize::MAX, Vec::new());
        for (slot, row) in out.iter_mut().zip(&self.inventory.residuals) {
            *slot = match *row {
                CoupledEventResidual::Implicit { row } => {
                    super::implicit_row_value(&implicit, row)?
                }
                CoupledEventResidual::Discrete { row } => self.discrete_tangent(row, point)?,
                CoupledEventResidual::Guarded {
                    program,
                    output,
                    target,
                } => self.guarded_tangent((program, output, target), point, &mut guarded)?,
                CoupledEventResidual::StructuredDiscrete { row } => {
                    self.structured_tangent(row, point)?
                }
            };
        }
        Ok(())
    }

    fn discrete_tangent(&self, row: usize, point: RowPoint<'_>) -> Result<f64, RuntimeSolveError> {
        let block = required(&self.jvps.discrete, "discrete update rows")?;
        let (program, offset) = block.row_output_position(row).ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "discrete output {row} has no directional derivative program"
            ))
        })?;
        let target = self.runtime.model.problem.discrete.update_targets[row];
        let tangent = block.eval_row_output_unchecked_with_context(
            program,
            offset,
            point.y,
            point.p,
            self.t,
            point.context,
        )?;
        Ok(seed_value(point.seed, target, point.y.len()) - tangent)
    }

    /// A guarded output's tangent; `cache` holds the last evaluated program's
    /// outputs.
    fn guarded_tangent(
        &self,
        (program, output, target): (usize, usize, solve::ScalarSlot),
        point: RowPoint<'_>,
        cache: &mut (usize, Vec<f64>),
    ) -> Result<f64, RuntimeSolveError> {
        if cache.0 != program {
            required(&self.jvps.guarded, "guarded assignments")?
                .eval_row_outputs_unchecked_with_context(
                    program,
                    point.y,
                    point.p,
                    self.t,
                    point.context,
                    &mut cache.1,
                )?;
            cache.0 = program;
        }
        Ok(seed_value(point.seed, target, point.y.len()) - cache.1[output])
    }

    fn structured_tangent(
        &self,
        row: usize,
        point: RowPoint<'_>,
    ) -> Result<f64, RuntimeSolveError> {
        let entry = self.runtime.scalar.structured_discrete_rows.rows()[row];
        let tangent = required(&self.jvps.structured, "structured discrete updates")?
            .eval_row_unchecked_with_context(
                entry.source_row,
                point.y,
                point.p,
                self.t,
                point.context,
            )?;
        Ok(seed_value(point.seed, entry.target, point.y.len()) - tangent)
    }
}
