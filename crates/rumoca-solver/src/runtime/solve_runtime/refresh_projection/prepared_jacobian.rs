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
            if !source.shares_program_owner(application.source()) {
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
        let Some(application) = structure.jacobian_application() else {
            return Ok(false);
        };
        let index = application.block_index();
        let Some(Some(compiled)) = self.runtime.compiled_algebraic_jacobians.get(index) else {
            return Ok(false);
        };
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
