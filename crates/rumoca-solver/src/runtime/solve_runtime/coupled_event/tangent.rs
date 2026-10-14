//! The coupled event residual's exact directional derivative.
//!
//! The residual evaluates its rows at the settled point: the unknowns written
//! into the event coordinates, the root relation overrides pinned, and the
//! runtime assignments applied until stable. Its tangent follows the same
//! path: the direction seeds the unknowns, the pinned relation memories stay
//! constant, the runtime assignments' JVP carries the seed to their targets
//! until it stops changing, and each residual row takes its target's seed
//! minus its row's JVP (the implicit rows' JVP directly).

use std::borrow::Cow;

use rumoca_eval_solve::{JacobianEvalInputs, RowEvalContext};
use rumoca_ir_solve as solve;

use super::super::admitted_compiled_error;
use super::super::initial_projection::write_update_tangents;
use super::prepared_jvps::{PreparedJvp, required};
use super::{CoupledEventResidual, CoupledEventSystem, CoupledEventUnknown};
use crate::RuntimeSolveError;
use crate::runtime::solve_events::event_eval_params_with_relation_overrides;

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
            block.eval(
                JacobianEvalInputs {
                    y,
                    p,
                    t: self.t,
                    seed,
                },
                self.runtime.row_eval_context(),
                self.runtime.model.external_tables.as_slice(),
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
        let mut implicit = vec![0.0; runtime.scalar.implicit_jacobian_v.len()];
        if let Some(code) = runtime.scalar.compiled_implicit_full_jacobian_v() {
            code.call(
                y,
                p,
                self.t,
                seed,
                runtime.model.external_tables.as_slice(),
                &mut implicit,
            )
            .map_err(|error| {
                admitted_compiled_error(
                    "coupled implicit JVP",
                    runtime.scalar.implicit_jacobian_v.block().program_spans(),
                    error,
                )
            })?;
        } else {
            runtime.scalar.implicit_jacobian_v.eval_with_context(
                y,
                p,
                self.t,
                context,
                &mut implicit,
            )?;
        }
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
        let (program, offset) = block.source().row_output_position(row).ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "discrete output {row} has no directional derivative program"
            ))
        })?;
        let target = self.runtime.model.problem.discrete.update_targets[row];
        let tangent = self.jvp_output(block, (program, offset), point)?;
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
            required(&self.jvps.guarded, "guarded assignments")?.eval_program_outputs(
                program,
                JacobianEvalInputs {
                    y: point.y,
                    p: point.p,
                    t: self.t,
                    seed: point.seed,
                },
                point.context,
                self.runtime.model.external_tables.as_slice(),
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
        let tangent = self.jvp_output(
            required(&self.jvps.structured, "structured discrete updates")?,
            (entry.source_row, 0),
            point,
        )?;
        Ok(seed_value(point.seed, entry.target, point.y.len()) - tangent)
    }
    fn jvp_output(
        &self,
        block: &PreparedJvp,
        coordinate: (usize, usize),
        point: RowPoint<'_>,
    ) -> Result<f64, RuntimeSolveError> {
        block.eval_output(
            coordinate,
            JacobianEvalInputs {
                y: point.y,
                p: point.p,
                t: self.t,
                seed: point.seed,
            },
            point.context,
            self.runtime.model.external_tables.as_slice(),
        )
    }
}
