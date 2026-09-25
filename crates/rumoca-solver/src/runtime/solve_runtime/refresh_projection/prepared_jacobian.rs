use super::*;

pub(crate) fn projection_jacobian_source(
    canonical: &solve::ComputeBlock,
    structures: &solve::ContinuousStructuralArtifacts,
) -> Result<solve::ScalarProgramBlock, EvalSolveError> {
    if let [solve::ComputeNode::ScalarPrograms(source)] = canonical.nodes.as_slice()
        && let Some(bound) = structures.algebraic_jacobian_source()
        && !source.shares_program_owner(bound)
    {
        return Err(EvalSolveError::InvalidRow {
            message: "projection metadata belongs to a different canonical Jacobian source".into(),
            span: source.first_source_span(),
        });
    }
    to_scalar_program_block(canonical).map_err(Into::into)
}

pub(crate) fn prepare_projection_jacobians(
    structures: &solve::ContinuousStructuralArtifacts,
    primal: &PreparedScalarProgramBlock,
    source: &solve::ScalarProgramBlock,
    compiled: Option<&dyn CompiledSolveJacobianExpression>,
) -> Result<Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>>, EvalSolveError> {
    structures
        .algebraic_projection()
        .iter()
        .map(|structure| {
            let Some((compiled, application)) = compiled.zip(structure.jacobian_application())
            else {
                return Ok(None);
            };
            if !source.shares_program_owner(application.canonical_source()) {
                return Ok(None);
            }
            let all_forward = application.rows().iter().all(|&row| {
                primal
                    .row_output_position(row)
                    .is_some_and(|(program, _)| !primal.reverse_row_y_gradient_supported(program))
            });
            if !all_forward {
                return Ok(None);
            }
            compiled
                .prepare_projection(application)
                .map_err(|message| EvalSolveError::InvalidRow {
                    message,
                    span: application.source().first_source_span(),
                })
        })
        .collect()
}

impl RefreshProjectionModel<'_> {
    pub(super) fn eval_prepared_jacobian(
        &self,
        structure: &solve::JacobianStructure,
        (rows, y_indices): (&[usize], &[usize]),
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        if !self.jacobian_v.is_solver_y_only() {
            return Ok(false);
        }
        let compiled = structure.jacobian_application().and_then(|application| {
            let compiled = self
                .runtime
                .compiled_algebraic_jacobians
                .get(application.block_index())?
                .as_ref()?;
            Some((application, compiled))
        });
        let Some((application, compiled)) = compiled else {
            return Ok(false);
        };
        let index = application.block_index();
        let owns_structure = self
            .runtime
            .continuous_structural
            .algebraic_projection()
            .get(index)
            .is_some_and(|owned| std::ptr::eq(owned, structure));
        if !owns_structure || application.rows() != rows || application.y_indices() != y_indices {
            return Err(RuntimeSolveError::solve_ir(
                "prepared Jacobian source coordinates differ",
            ));
        }
        compiled
            .call(y, p, t, self.runtime.model.external_tables.as_slice(), out)
            .map_err(RuntimeSolveError::solve_ir)?;
        Ok(true)
    }
}

impl RefreshProjectionModel<'_> {
    /// Every structural entry `(row, column, value)` of the block from its
    /// colored tangent plan: one multi-lane evaluation with one lane per color,
    /// when no backend compiled the projection JVP.
    pub(super) fn eval_colored_tangent_entries(
        &self,
        structure: &solve::JacobianStructure,
        (rows, y_indices): (&[usize], &[usize]),
        (y, p, t): (&[f64], &[f64], f64),
    ) -> Result<Option<crate::runtime::projection::JacobianEntries>, RuntimeSolveError> {
        if !self.jacobian_v.is_solver_y_only() {
            return Ok(None);
        }
        // A backend-compiled JVP evaluates the colors natively; the lanes serve
        // the interpreted runtime.
        if self
            .runtime
            .compiled_implicit_projection_jacobian_v
            .is_some()
        {
            return Ok(None);
        }
        let owned = self.runtime.continuous_structural.algebraic_projection();
        let Some(index) = owned
            .iter()
            .position(|owned| std::ptr::eq(owned, structure))
        else {
            return Ok(None);
        };
        let (Some(Some(evaluator)), Some(block)) = (
            self.runtime.colored_tangents.get(index),
            self.runtime
                .model
                .problem
                .continuous
                .algebraic_projection_plan
                .blocks
                .get(index),
        ) else {
            return Ok(None);
        };
        if block.rows != rows || block.y_indices != y_indices {
            return Ok(None);
        }
        let n = rows.len();
        let mut out = vec![0.0; evaluator.plan().output_len()];
        evaluator.eval(
            (y, p, t),
            self.runtime.row_eval_context(),
            y.len() + p.len(),
            &mut out,
        )?;
        let placed = evaluator
            .plan()
            .calls()
            .iter()
            .flat_map(|call| call.placements.iter().map(|placement| placement.2));
        Ok(Some(
            placed
                .map(|destination| (destination % n, destination / n, out[destination]))
                .collect(),
        ))
    }
}

pub(crate) fn validate_projection_primal_source(
    primal: &solve::ScalarProgramBlock,
    structures: &solve::ContinuousStructuralArtifacts,
) -> Result<(), EvalSolveError> {
    for application in structures
        .algebraic_projection()
        .iter()
        .filter_map(solve::JacobianStructure::jacobian_application)
    {
        if application
            .primal_source()
            .is_some_and(|source| !primal.shares_program_owner(source))
        {
            return Err(EvalSolveError::InvalidRow {
                message: "projection metadata belongs to a different canonical primal source"
                    .into(),
                span: primal.first_source_span(),
            });
        }
    }
    Ok(())
}
