use crate::{
    CoupledEventNewtonModel, RuntimeSolveError, discrete_row_active_at, discrete_row_pre_mode,
    runtime_values_changed, solve_coupled_event_newton,
};
use rumoca_ir_solve as solve;

use super::SolveRuntime;
use super::event_update::{
    DiscretePreSnapshot, DiscreteRowEvalInput, DiscreteRowsSettleInput, EventEvalParamCache,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CoupledEventUnknown {
    Y(usize),
    P(usize),
}

#[derive(Clone, Copy, Debug)]
enum CoupledEventResidual {
    Implicit { row: usize },
    Discrete { row: usize },
}

struct CoupledEventInventory {
    unknowns: Vec<CoupledEventUnknown>,
    residuals: Vec<CoupledEventResidual>,
}

struct CoupledEventSystem<'a> {
    runtime: &'a SolveRuntime,
    snapshot: &'a DiscretePreSnapshot<'a>,
    base_y: &'a [f64],
    base_p: &'a [f64],
    inventory: &'a CoupledEventInventory,
    t: f64,
    tol: f64,
    max_iters: usize,
}

impl SolveRuntime {
    pub(super) fn solve_coupled_event_rows(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        input: &mut DiscreteRowsSettleInput<'_>,
    ) -> Result<bool, RuntimeSolveError> {
        let inventory = self.coupled_event_inventory(snapshot, input.t)?;
        if inventory
            .residuals
            .iter()
            .all(|row| matches!(row, CoupledEventResidual::Implicit { .. }))
        {
            return Err(coupled_recovery_error(
                input.t,
                "no Real-valued event update rows are available for the coupled solve",
            ));
        }
        let before_y = input.y.to_vec();
        let before_p = input.p.to_vec();
        let mut unknowns = read_unknowns(&inventory.unknowns, input.y, input.p)?;
        let system = CoupledEventSystem {
            runtime: self,
            snapshot,
            base_y: input.y,
            base_p: input.p,
            inventory: &inventory,
            t: input.t,
            tol: input.tol,
            max_iters: input.max_iters,
        };
        tracing::debug!(
            target: "rumoca_eval_solve::event",
            unknown_count = unknowns.len(),
            time = input.t,
            "event fixed-point iteration stalled; starting coupled Newton recovery"
        );
        solve_coupled_event_newton(&system, &mut unknowns, input.tol, input.max_iters)
            .map_err(|error| coupled_recovery_error(input.t, &error.to_string()))?;
        write_unknowns(&inventory.unknowns, &unknowns, input.y, input.p)?;
        self.apply_root_relation_memory_overrides(
            snapshot.root_relation_overrides,
            input.y,
            input.p,
            input.tol,
        )?;
        self.apply_runtime_assignments_until_stable(
            input.y,
            input.p,
            input.t,
            input.tol,
            input.max_iters,
        )?;
        Ok(runtime_values_changed(&before_y, input.y, input.tol)
            || runtime_values_changed(&before_p, input.p, input.tol))
    }

    fn coupled_event_inventory(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        t: f64,
    ) -> Result<CoupledEventInventory, RuntimeSolveError> {
        let mut inventory = CoupledEventInventory {
            unknowns: Vec::new(),
            residuals: Vec::new(),
        };
        for block in &self
            .model
            .problem
            .continuous
            .algebraic_projection_plan
            .blocks
        {
            if block.rows.len() != block.y_indices.len() {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "coupled event algebraic block has {} rows for {} unknowns",
                    block.rows.len(),
                    block.y_indices.len()
                )));
            }
            for (&row, &y_index) in block.rows.iter().zip(&block.y_indices) {
                push_coupled_inventory_entry(
                    &mut inventory,
                    CoupledEventUnknown::Y(y_index),
                    CoupledEventResidual::Implicit { row },
                )?;
            }
        }
        for (row, target) in self
            .model
            .problem
            .discrete
            .update_targets
            .iter()
            .copied()
            .enumerate()
        {
            if !discrete_row_active_at(&self.model, row, t)? {
                continue;
            }
            let mode = discrete_row_pre_mode(&self.model, row)?;
            if !snapshot.row_filter.accepts(mode) {
                continue;
            }
            let Some(unknown) = self.real_event_unknown(target) else {
                continue;
            };
            push_coupled_inventory_entry(
                &mut inventory,
                unknown,
                CoupledEventResidual::Discrete { row },
            )?;
        }
        Ok(inventory)
    }

    fn real_event_unknown(&self, target: solve::ScalarSlot) -> Option<CoupledEventUnknown> {
        match target {
            solve::ScalarSlot::Y { index, .. } => Some(CoupledEventUnknown::Y(index)),
            solve::ScalarSlot::P { index, .. } if self.is_discrete_real_parameter(index) => {
                Some(CoupledEventUnknown::P(index))
            }
            solve::ScalarSlot::Time
            | solve::ScalarSlot::P { .. }
            | solve::ScalarSlot::Constant(_) => None,
        }
    }

    fn is_discrete_real_parameter(&self, index: usize) -> bool {
        let layout = &self.model.problem.solve_layout;
        let start = layout.parameter_count + layout.input_scalar_names.len();
        let end = start + layout.discrete_real_scalar_names.len();
        (start..end).contains(&index)
    }
}

impl CoupledEventNewtonModel for CoupledEventSystem<'_> {
    fn eval_residual(
        &self,
        unknowns: &[f64],
        residual: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if residual.len() != self.inventory.residuals.len() {
            return Err(RuntimeSolveError::solve_ir(
                "coupled event residual buffer does not match the event inventory",
            ));
        }
        let mut y = self.base_y.to_vec();
        let mut p = self.base_p.to_vec();
        write_unknowns(&self.inventory.unknowns, unknowns, &mut y, &mut p)?;
        self.runtime.apply_root_relation_memory_overrides(
            self.snapshot.root_relation_overrides,
            &mut y,
            &mut p,
            self.tol,
        )?;
        self.runtime.apply_runtime_assignments_until_stable(
            &mut y,
            &mut p,
            self.t,
            self.tol,
            self.max_iters,
        )?;
        self.eval_residual_rows(&y, &p, residual)
    }

    fn variable_scale(&self, index: usize) -> f64 {
        self.inventory
            .unknowns
            .get(index)
            .copied()
            .map_or(1.0, |unknown| self.unknown_scale(unknown))
    }

    fn residual_scale(&self, index: usize) -> f64 {
        self.variable_scale(index)
    }
}

impl CoupledEventSystem<'_> {
    fn eval_residual_rows(
        &self,
        y: &[f64],
        p: &[f64],
        residual: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut implicit = vec![0.0; self.runtime.implicit_scalar_rhs.len()];
        self.runtime.implicit_scalar_rhs.eval_with_context(
            y,
            p,
            self.t,
            self.runtime.row_eval_context(),
            &mut implicit,
        )?;
        let mut eval_p_cache = EventEvalParamCache::default();
        for (slot, row) in residual.iter_mut().zip(&self.inventory.residuals) {
            *slot = match *row {
                CoupledEventResidual::Implicit { row } => implicit_row_value(&implicit, row)?,
                CoupledEventResidual::Discrete { row } => {
                    self.eval_discrete_residual(row, y, p, &mut eval_p_cache)?
                }
            };
        }
        Ok(())
    }

    fn eval_discrete_residual(
        &self,
        row: usize,
        y: &[f64],
        p: &[f64],
        eval_p_cache: &mut EventEvalParamCache,
    ) -> Result<f64, RuntimeSolveError> {
        let value = self
            .runtime
            .eval_discrete_row_for_pre_snapshot(
                DiscreteRowEvalInput {
                    snapshot: self.snapshot,
                    row_idx: row,
                    eval_y: y,
                    eval_p: p,
                    t: self.t,
                    tol: self.tol,
                },
                eval_p_cache,
            )?
            .ok_or_else(|| filtered_discrete_row_error(row))?;
        let target = self.runtime.model.problem.discrete.update_targets[row];
        Ok(scalar_slot_value(target, y, p)? - value)
    }

    fn unknown_scale(&self, unknown: CoupledEventUnknown) -> f64 {
        match unknown {
            CoupledEventUnknown::Y(index) => self.runtime.model.solver_variable_scale(index),
            CoupledEventUnknown::P(index) => self
                .base_p
                .get(index)
                .copied()
                .filter(|value| value.is_finite())
                .map_or(1.0, |value| value.abs().max(1.0)),
        }
    }
}

fn push_coupled_inventory_entry(
    inventory: &mut CoupledEventInventory,
    unknown: CoupledEventUnknown,
    residual: CoupledEventResidual,
) -> Result<(), RuntimeSolveError> {
    if inventory.unknowns.contains(&unknown) {
        return Err(RuntimeSolveError::solve_ir(format!(
            "coupled event inventory contains multiple equations for {unknown:?}"
        )));
    }
    inventory.unknowns.push(unknown);
    inventory.residuals.push(residual);
    Ok(())
}

fn read_unknowns(
    inventory: &[CoupledEventUnknown],
    y: &[f64],
    p: &[f64],
) -> Result<Vec<f64>, RuntimeSolveError> {
    inventory
        .iter()
        .copied()
        .map(|unknown| match unknown {
            CoupledEventUnknown::Y(index) => indexed_value("y", y, index),
            CoupledEventUnknown::P(index) => indexed_value("p", p, index),
        })
        .collect()
}

fn write_unknowns(
    inventory: &[CoupledEventUnknown],
    unknowns: &[f64],
    y: &mut [f64],
    p: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    if inventory.len() != unknowns.len() {
        return Err(RuntimeSolveError::solve_ir(
            "coupled event unknown vector does not match the event inventory",
        ));
    }
    for (target, value) in inventory.iter().copied().zip(unknowns.iter().copied()) {
        match target {
            CoupledEventUnknown::Y(index) => write_indexed_value("y", y, index, value)?,
            CoupledEventUnknown::P(index) => write_indexed_value("p", p, index, value)?,
        }
    }
    Ok(())
}

fn scalar_slot_value(
    target: solve::ScalarSlot,
    y: &[f64],
    p: &[f64],
) -> Result<f64, RuntimeSolveError> {
    match target {
        solve::ScalarSlot::Y { index, .. } => indexed_value("y", y, index),
        solve::ScalarSlot::P { index, .. } => indexed_value("p", p, index),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => Err(
            RuntimeSolveError::solve_ir("coupled event residual target is not a writable slot"),
        ),
    }
}

fn indexed_value(name: &str, values: &[f64], index: usize) -> Result<f64, RuntimeSolveError> {
    values.get(index).copied().ok_or_else(|| {
        RuntimeSolveError::solve_ir(format!(
            "coupled event {name}[{index}] is outside vector length {}",
            values.len()
        ))
    })
}

fn write_indexed_value(
    name: &str,
    values: &mut [f64],
    index: usize,
    value: f64,
) -> Result<(), RuntimeSolveError> {
    let len = values.len();
    let slot = values.get_mut(index).ok_or_else(|| {
        RuntimeSolveError::solve_ir(format!(
            "coupled event {name}[{index}] is outside vector length {len}"
        ))
    })?;
    *slot = value;
    Ok(())
}

fn implicit_row_value(values: &[f64], row: usize) -> Result<f64, RuntimeSolveError> {
    values.get(row).copied().ok_or_else(|| {
        RuntimeSolveError::solve_ir(format!(
            "coupled event implicit row {row} is outside residual vector length {}",
            values.len()
        ))
    })
}

fn filtered_discrete_row_error(row: usize) -> RuntimeSolveError {
    RuntimeSolveError::solve_ir(format!(
        "coupled event inventory contains filtered discrete row {row}"
    ))
}

fn coupled_recovery_error(t: f64, reason: &str) -> RuntimeSolveError {
    RuntimeSolveError::solve_ir(format!(
        "discrete event equations did not converge at t={t}; coupled Newton recovery failed: \
         {reason}"
    ))
}

#[cfg(test)]
mod tests {
    use crate::EventActionOutcome;
    use rumoca_ir_solve::{BinaryOp, LinearOp, ScalarProgramBlock};

    use super::*;
    use crate::runtime::solve_runtime::{EventUpdateRowFilter, ProjectedEventUpdateInput};

    fn coupled_event_test_model() -> solve::SolveModel {
        let implicit = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::LoadP { dst: 1, index: 0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            rumoca_core::Span::DUMMY,
        );
        let discrete = ScalarProgramBlock::with_source_span(
            vec![
                vec![
                    LinearOp::Const { dst: 0, value: 2.0 },
                    LinearOp::LoadY { dst: 1, index: 0 },
                    LinearOp::Binary {
                        dst: 2,
                        op: BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    LinearOp::StoreOutput { src: 2 },
                ],
                vec![
                    LinearOp::Const { dst: 0, value: 1.0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
            ],
            rumoca_core::Span::DUMMY,
        );
        solve::SolveModel {
            problem: solve::SolveProblem {
                solve_layout: solve::SolveLayout {
                    solver_maps: solve::SolverNameIndexMaps {
                        names: vec!["z".to_string()],
                        ..Default::default()
                    },
                    algebraic_scalar_count: 1,
                    discrete_real_scalar_names: vec!["d".to_string()],
                    discrete_valued_scalar_names: vec!["mode".to_string()],
                    ..Default::default()
                },
                continuous: solve::ContinuousSolveSystem {
                    implicit_rhs: solve::ComputeBlock::from_scalar_program_block(implicit),
                    implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                    algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                        blocks: vec![solve::AlgebraicProjectionBlock {
                            rows: vec![0],
                            y_indices: vec![0],
                        }],
                    },
                    ..Default::default()
                },
                discrete: solve::DiscreteSolveSystem {
                    rhs: discrete,
                    update_targets: vec![solve::scalar_slot_p(0), solve::scalar_slot_p(1)],
                    row_roles: vec![
                        solve::DiscreteRowRole::Equation,
                        solve::DiscreteRowRole::Equation,
                    ],
                    pre_modes: vec![
                        solve::DiscreteEventPreMode::FollowCurrent,
                        solve::DiscreteEventPreMode::FollowCurrent,
                    ],
                    observation_refresh: vec![false, false],
                    clock_owners: vec![None, None],
                    ..Default::default()
                },
                ..Default::default()
            },
            initial_y: vec![0.0],
            ..Default::default()
        }
    }

    #[test]
    fn projected_event_update_recovers_picard_oscillation_with_coupled_newton() {
        let runtime =
            SolveRuntime::new(&coupled_event_test_model()).expect("event model should prepare");
        let mut y = vec![0.0];
        let mut p = vec![0.0, 0.0];
        let event_pre_y = y.clone();
        let event_pre_p = p.clone();

        let outcome = runtime
            .apply_projected_event_update(
                ProjectedEventUpdateInput {
                    y: &mut y,
                    p: &mut p,
                    t: 0.0,
                    tol: 1.0e-12,
                    event_pre_y: &event_pre_y,
                    event_pre_p: &event_pre_p,
                    max_iters: 4,
                    row_filter: EventUpdateRowFilter::All,
                    root_relation_overrides: &[],
                },
                |solver_y, params| {
                    let changed = (solver_y[0] - params[0]).abs() > 1.0e-12;
                    solver_y[0] = params[0];
                    Ok(changed)
                },
            )
            .expect("coupled Newton should recover the oscillating event fixed point");

        assert_eq!(outcome, EventActionOutcome::Continue);
        assert!((y[0] - 1.0).abs() < 1.0e-10, "unexpected z={}", y[0]);
        assert!((p[0] - 1.0).abs() < 1.0e-10, "unexpected d={}", p[0]);
        assert_eq!(p[1], 1.0, "the discrete-valued mode must remain frozen");
    }
}
