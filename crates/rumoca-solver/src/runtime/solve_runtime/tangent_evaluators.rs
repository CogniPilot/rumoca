//! Tangent evaluators the runtime prepares once per projection plan.

use super::*;

/// The tangent evaluator of each projection block's tearing over the
/// solver-Y JVP rows `jvp`, aligned with `plan.blocks`.
pub(super) fn torn_tangent_evaluators(
    plan: &solve::AlgebraicProjectionPlan,
    jvp: &solve::ScalarProgramBlock,
    compiled: bool,
) -> Rc<[Option<rumoca_eval_solve::TornTangentEvaluator>]> {
    // One-direction plans all read the same JVP rows; prepare them once.
    let mut shared = None;
    plan.blocks
        .scalar_blocks()
        .iter()
        .map(|block| {
            let tearing = block.tearing.as_ref()?;
            // A backend-compiled JVP answers each direction natively, faster
            // than the interpreted lane widening and with the same values.
            let plan = if compiled {
                solve::TornTangentPlan::derive_directional(tearing, jvp)
            } else {
                solve::TornTangentPlan::derive(tearing, jvp)
            }
            .ok()?;
            rumoca_eval_solve::TornTangentEvaluator::sharing_directions(plan, jvp, &mut shared).ok()
        })
        .collect()
}

/// The colored tangent evaluator of each projection block's issued Jacobian
/// application, aligned with `plan.blocks` and their structures.
pub(super) fn colored_tangent_evaluators(
    plan: &solve::AlgebraicProjectionPlan,
    structures: &solve::ContinuousStructuralArtifacts,
) -> Rc<[Option<rumoca_eval_solve::ColoredTangentEvaluator>]> {
    // Blocks over one shared source program share its widening and preparation.
    let mut widened = solve::TangentLaneCatalog::default();
    let mut prepared = rumoca_eval_solve::PreparedLaneCatalog::default();
    plan.blocks
        .scalar_blocks()
        .iter()
        .zip(structures.algebraic_projection())
        .map(|(block, structure)| {
            if !rumoca_eval_solve::projection_policy::jacobian_sources().colored_lanes {
                return None;
            }
            let application = structure.jacobian_application().filter(|application| {
                application.rows() == block.rows && application.y_indices() == block.y_indices
            })?;
            let plan = solve::ColoredTangentPlan::derive_sharing(application, &mut widened).ok()?;
            Some(rumoca_eval_solve::ColoredTangentEvaluator::sharing(
                plan,
                &mut prepared,
            ))
        })
        .collect()
}

/// One torn block's tangent issued as assignment rows over its extended slots
/// and compiled as one schedule: every sweep of a tear Jacobian in one call.
pub(super) struct CompiledTornTangent {
    issued: solve::TornTangentSchedule,
    schedule: Rc<dyn CompiledSolveAssignmentSchedule>,
    slots: RefCell<Vec<f64>>,
}

/// Each torn block's compiled tangent schedule, aligned with `evaluators`.
/// The schedule is issued only for a compiled JVP owner without a native
/// committing sweep (which already runs the sweeps in one native call); a plan
/// the issuer refuses or a schedule the backend declines keeps the per-row
/// tangent.
pub(super) fn compiled_torn_tangents(
    evaluators: &[Option<rumoca_eval_solve::TornTangentEvaluator>],
    jvp: &solve::ScalarProgramBlock,
    backend: Option<&Rc<dyn SolveExecutionBackend>>,
    y_len: usize,
) -> Rc<[Option<Rc<CompiledTornTangent>>]> {
    evaluators
        .iter()
        .map(|evaluator| {
            let backend = backend?;
            let issued = optional_compiled(
                "torn_tangent_schedule",
                evaluator.as_ref()?.plan().issue_schedule(jvp, y_len),
            )?;
            let schedule = optional_compiled(
                "torn_tangent_rows",
                backend.compile_issued_assignment_rows(issued.rows()),
            )?;
            Some(Rc::new(CompiledTornTangent {
                issued,
                schedule,
                slots: RefCell::new(Vec::new()),
            }))
        })
        .collect()
}

impl CompiledTornTangent {
    /// The reduced tear Jacobian at `y` from one schedule call, with the
    /// per-row tangent's outcomes ([`rumoca_eval_solve::torn_tangent_schedule_outcome`]).
    pub(super) fn eval(
        &self,
        (y, p, t): (&[f64], &[f64], f64),
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<rumoca_eval_solve::TornTangentJacobian>, RuntimeSolveError> {
        let layout = self.issued.slots();
        if y.len() != layout.y {
            return Err(RuntimeSolveError::solve_ir(
                "torn tangent point differs from its issued solver Y",
            ));
        }
        let mut slots = self.slots.borrow_mut();
        resize_runtime_values(&mut slots, layout.total, 0.0, "torn tangent slots")?;
        slots[..layout.y].copy_from_slice(y);
        slots[layout.y..].fill(0.0);
        let called = self.schedule.call(&mut slots, p, t, tables);
        rumoca_eval_solve::torn_tangent_schedule_outcome(&self.issued, &slots, called)
    }
}
