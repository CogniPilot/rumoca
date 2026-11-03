//! Borrowed primal inputs for construction, excluding detached derivatives.

#[cfg(test)]
mod tests;

use crate::{
    BoundExactRefreshAssignment, ComputeSourceView, ContinuousRefreshConstructionError,
    ContinuousSolveSystem, SolveLayout, SolveModel, SolveProblemShapeContractError,
    SolvePureCallTable, VarLayout,
};

/// One exact borrowed model's primal sources, original helper namespace and
/// assignment tuples. This is construction input, not a derivative graph,
/// sparsity/zero certificate, executable admission or target plan.
///
/// Construction has no independently supplied pattern, layout, table or body.
/// Derived artifacts are deliberately unavailable through this view: the
/// derivative constructor must derive its own numerical and dependency facts.
///
/// The originating model stays borrowed through construction:
/// ```compile_fail
/// use rumoca_ir_solve::SolveModel;
/// fn replace_source(model: &mut SolveModel) {
///     let source = model.continuous_numerical_source().unwrap();
///     model.problem.layout = Default::default();
///     let _ = source.coordinates();
/// }
/// ```
#[derive(Debug)]
pub struct ContinuousNumericalSource<'source> {
    layout: &'source VarLayout,
    coordinates: &'source SolveLayout,
    calls: &'source SolvePureCallTable,
    continuous: &'source ContinuousSolveSystem,
    implicit: ComputeSourceView<'source>,
    derivative: ComputeSourceView<'source>,
    manifold: ComputeSourceView<'source>,
    assignments: Box<[BoundExactRefreshAssignment<'source>]>,
}

/// Original local admission failures, before a numerical graph is issued.
#[derive(Debug)]
pub enum ContinuousNumericalSourceError {
    Source(Box<SolveProblemShapeContractError>),
    Refresh(ContinuousRefreshConstructionError),
}

impl std::fmt::Display for ContinuousNumericalSourceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Source(error) => error.fmt(f),
            Self::Refresh(error) => error.fmt(f),
        }
    }
}

impl std::error::Error for ContinuousNumericalSourceError {}

impl ContinuousNumericalSourceError {
    /// Retain existing diagnostics; the legacy refresh error has no span field.
    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::Source(error) => error.source_span(),
            Self::Refresh(_) => None,
        }
    }
}

fn source_error(error: SolveProblemShapeContractError) -> ContinuousNumericalSourceError {
    ContinuousNumericalSourceError::Source(Box::new(error))
}

impl SolveModel {
    /// Bind the current primal sources through their existing canonical local
    /// checks. No model snapshot, scalar coordinate catalog, derived artifact
    /// validation or backend preparation is performed.
    ///
    /// A source view alone does not admit a Jacobian domain. The numerical
    /// constructor still owns complete coverage, chart/projection admission,
    /// derivative rules and source-bound output issuance.
    pub fn continuous_numerical_source(
        &self,
    ) -> Result<ContinuousNumericalSource<'_>, ContinuousNumericalSourceError> {
        use ContinuousNumericalSourceError::Refresh;
        let problem = &self.problem;
        if problem.schema_version != crate::SOLVE_SCHEMA_VERSION {
            return Err(source_error(
                SolveProblemShapeContractError::SchemaVersion {
                    actual: problem.schema_version,
                    expected: crate::SOLVE_SCHEMA_VERSION,
                },
            ));
        }
        problem
            .layout
            .validate_shape_contract()
            .map_err(SolveProblemShapeContractError::Layout)
            .map_err(source_error)?;
        crate::validate_variable_storage_runs(problem).map_err(source_error)?;
        problem.validate_solver_storage().map_err(source_error)?;
        crate::validate_problem_call_references(problem, &self.pure_calls).map_err(source_error)?;
        let continuous = &problem.continuous;
        let implicit = continuous
            .implicit_rhs
            .checked_source_view("continuous.implicit_rhs")
            .map_err(source_error)?;
        let derivative = continuous
            .derivative_rhs
            .checked_source_view("continuous.derivative_rhs")
            .map_err(source_error)?;
        let manifold = continuous
            .manifold_residual
            .checked_source_view("continuous.manifold_residual")
            .map_err(source_error)?;
        let owners = &continuous.refresh_owners;
        owners
            .validate_against(&continuous.implicit_rhs)
            .map_err(Refresh)?;
        let assignments = owners
            .exact_assignment_programs_for_source(&continuous.implicit_rhs)
            .map_err(Refresh)?;
        Ok(ContinuousNumericalSource {
            layout: &problem.layout,
            coordinates: &problem.solve_layout,
            calls: &self.pure_calls,
            continuous,
            implicit,
            derivative,
            manifold,
            assignments,
        })
    }
}

impl<'source> ContinuousNumericalSource<'source> {
    pub fn layout(&self) -> &'source VarLayout {
        self.layout
    }

    pub fn coordinates(&self) -> &'source SolveLayout {
        self.coordinates
    }

    pub fn calls(&self) -> &'source SolvePureCallTable {
        self.calls
    }

    pub fn continuous(&self) -> &'source ContinuousSolveSystem {
        self.continuous
    }

    pub fn implicit_source(&self) -> ComputeSourceView<'source> {
        self.implicit
    }

    pub fn derivative_source(&self) -> ComputeSourceView<'source> {
        self.derivative
    }

    pub fn manifold_source(&self) -> ComputeSourceView<'source> {
        self.manifold
    }

    /// Each original assignment owner appears once, not once per schedule use.
    pub fn assignments(&self) -> &[BoundExactRefreshAssignment<'source>] {
        &self.assignments
    }
}
