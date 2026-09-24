use super::*;
use crate::runtime::projection::{JacobianMatrix, JacobianStorage};

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
    pub(super) fn lease_affine_storage(
        &self,
        structure: &solve::JacobianStructure,
        (rows, y_indices): (&[usize], &[usize]),
        y_len: usize,
    ) -> Result<
        Option<std::cell::RefMut<'_, crate::runtime::projection::JacobianStorage>>,
        RuntimeSolveError,
    > {
        let Some(application) = structure.jacobian_application() else {
            return Ok(None);
        };
        self.runtime.algebraic_refresh.validated_for(
            self.runtime.state_count,
            self.runtime.solver_count,
            y_len,
        )?;
        let index = application.block_index();
        let owns_structure = self
            .runtime
            .continuous_structural
            .algebraic_projection()
            .get(index)
            .is_some_and(|owned| std::ptr::eq(owned, structure));
        if !owns_structure || application.rows() != rows || application.y_indices() != y_indices {
            return Err(RuntimeSolveError::solve_ir(
                "affine Jacobian storage source coordinates differ",
            ));
        }
        let slot = self
            .runtime
            .affine_jacobian_storage
            .get(index)
            .ok_or_else(|| RuntimeSolveError::solve_ir("affine Jacobian storage owner missing"))?;
        let mut storage = slot.try_borrow_mut().map_err(|_| {
            RuntimeSolveError::solve_ir("affine Jacobian storage is already leased")
        })?;
        let matrix = storage
            .get_or_insert_with(|| JacobianStorage::new(structure, rows.len(), y_indices.len()));
        if matrix.shape() != (rows.len(), y_indices.len()) {
            return Err(RuntimeSolveError::solve_ir(
                "affine Jacobian storage dimensions differ",
            ));
        }
        Ok(Some(std::cell::RefMut::map(storage, |matrix| {
            matrix.as_mut().expect("initialized affine matrix storage")
        })))
    }

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
