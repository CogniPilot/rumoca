//! The checked models the ME refresh view is built from.
//!
//! The view reads the same facts of either source: the checked FMI C
//! component, whose C settles the algebraic coordinates of one instance, or a
//! complete lowered model admitted to the kernel, whose target compiles the
//! same kernel for a device and runs it once per lane (SPEC_0044 ME-PROJ-001).

use rumoca_ir_solve as solve;

use crate::errors::CodegenError;

/// What the ME refresh view reads of one checked model.
pub(crate) trait RefreshSource {
    fn problem(&self) -> &solve::SolveProblem;
    fn artifacts(&self) -> &solve::SolveArtifacts;
    fn pure_calls(&self) -> &solve::SolvePureCallTable;
    /// The finite positive characteristic scale of one solver variable, the
    /// scale the linked algebraic projection uses.
    fn solver_variable_scale(&self, index: usize) -> f64;
    /// The initial solver vector and parameter values, the point at which the
    /// linked kernel resolves each reduced chart's state-binding rows.
    fn instantiation_point(&self) -> (&[f64], &[f64]);
    /// The dependency levels of the parameter bindings the kernel settles.
    fn parameter_binding_levels(&self) -> usize;
    /// Who orders the continuous-time evaluations of the rendered kernel.
    fn executor(&self) -> solve::RefreshExecutor;
}

impl RefreshSource for solve::fmi::FmiCCodegenView {
    fn problem(&self) -> &solve::SolveProblem {
        Self::problem(self)
    }
    fn artifacts(&self) -> &solve::SolveArtifacts {
        Self::artifacts(self)
    }
    fn pure_calls(&self) -> &solve::SolvePureCallTable {
        Self::pure_calls(self)
    }
    fn solver_variable_scale(&self, index: usize) -> f64 {
        Self::solver_variable_scale(self, index)
    }
    fn instantiation_point(&self) -> (&[f64], &[f64]) {
        Self::instantiation_point(self)
    }
    fn parameter_binding_levels(&self) -> usize {
        Self::parameter_binding_levels(self)
    }
    /// An FMU's importer orders its trial evaluations.
    fn executor(&self) -> solve::RefreshExecutor {
        solve::RefreshExecutor::ImporterDriven
    }
}

/// A complete lowered model, without an FMI component, admitted to the C
/// kernel (SPEC_0044 ME-PROJ-001). Only an admitted model is a refresh source,
/// so no view is built from a model the kernel refuses.
pub(crate) struct AdmittedSolveModel<'a> {
    model: &'a solve::SolveModel,
    bindings: solve::fmi::SolveBindingExecution,
}

impl<'a> AdmittedSolveModel<'a> {
    pub(crate) fn new(model: &'a solve::SolveModel) -> Result<Self, CodegenError> {
        crate::codegen::solve_renderer::require_builtin_fmi_template_domain(&model.problem)?;
        let bindings = solve::fmi::SolveBindingExecution::checked(model).map_err(|reason| {
            CodegenError::dae_preparation_failed(
                format!("unsupported-feature:projection-kernel: {reason}"),
                None,
            )
        })?;
        Ok(Self { model, bindings })
    }

    /// The admitted parameter bindings, from which the unit's includer renders
    /// its `rmc_apply_updates`.
    pub(crate) fn bindings(&self) -> &solve::fmi::SolveBindingExecution {
        &self.bindings
    }
}

impl RefreshSource for AdmittedSolveModel<'_> {
    fn problem(&self) -> &solve::SolveProblem {
        &self.model.problem
    }
    fn artifacts(&self) -> &solve::SolveArtifacts {
        &self.model.artifacts
    }
    fn pure_calls(&self) -> &solve::SolvePureCallTable {
        &self.model.pure_calls
    }
    fn solver_variable_scale(&self, index: usize) -> f64 {
        self.model.solver_variable_scale(index)
    }
    fn instantiation_point(&self) -> (&[f64], &[f64]) {
        (&self.model.initial_y, &self.model.parameters)
    }
    fn parameter_binding_levels(&self) -> usize {
        self.bindings.levels()
    }
    /// The target's own integrator steps each lane.
    fn executor(&self) -> solve::RefreshExecutor {
        solve::RefreshExecutor::IntegratorDriven
    }
}
