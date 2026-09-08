//! Reusable solve-target template renderer: one typed context, many
//! template strings (split from `codegen/mod.rs` to stay under the
//! SPEC_0021 file-size limit).

use minijinja::Value;
use rumoca_ir_solve as solve;
use std::sync::Arc;

use crate::{TemplateBindings, UntrustedRenderedText};

use super::render_solve;
use super::{
    CodegenError, LazyDerivativeNodesValue, LazyScalarRowsValue, solve_template_blocks_value,
    target_template_environment,
};

#[derive(Debug)]
pub struct PreparedSolveModelRendering {
    context: Value,
}

impl PreparedSolveModelRendering {
    fn shared(model: Arc<solve::SolveModel>, model_name: &str) -> Result<Self, CodegenError> {
        Ok(Self {
            context: solve_render_context_value(model, Some(model_name))?,
        })
    }

    /// Build a renderer by taking ownership of one complete checked Solve root.
    /// No DAE or independently supplied artifact graph can enter this context.
    pub fn prepare(model: solve::SolveModel) -> Result<Self, CodegenError> {
        let handle = super::solve_lazy::SolveRenderHandle::standalone(std::sync::Arc::new(model));
        Ok(Self {
            context: solve_render_context_value_with_handles(handle, None)?,
        })
    }

    fn render_builtin_content(
        &self,
        body: &'static str,
        model_name: &str,
    ) -> Result<String, CodegenError> {
        let mut env = target_template_environment();
        env.add_template("product_specific_solve_body", body)?;
        let template = env.get_template("product_specific_solve_body")?;
        Ok(template.render(minijinja::context! {
            model_name => model_name,
            ..self.context.clone()
        })?)
    }

    #[cfg(test)]
    pub(crate) fn new_for_test(
        model: Arc<solve::SolveModel>,
        model_name: &str,
    ) -> Result<Self, CodegenError> {
        Self::shared(model, model_name)
    }

    /// Renderer for one checked FMI component. The FMI entry is the opaque
    /// constructor-validated metadata/storage binding; Solve operations remain
    /// lazy so large tensor programs are not materialized as template maps.
    ///
    /// The correlated view is the **only** input. Its retained kernel supplies
    /// the Solve program and the artifacts, so no second argument can pair this
    /// metadata with a different model, and the FMI templates read no DAE at
    /// all, so this path builds no DAE template context and takes no `Dae`
    /// argument that could be an unrelated model:
    ///
    /// Should an FMI template ever need a DAE fact, it must travel inside the
    /// correlated component rather than arrive beside it.
    ///
    /// The view is taken in its event-free type-state because these templates
    /// render each entry from a Solve storage run and a present `start`, and
    /// describe no semantic event instant. A component that is not that shape
    /// is outside this input domain rather than a case to diagnose here; a
    /// caller narrows the view with
    /// [`solve::fmi::FmiCodegenView::try_event_free`] first.
    #[cfg(test)]
    pub fn render(&self, template: &str) -> Result<String, CodegenError> {
        let mut env = target_template_environment();
        env.add_template("inline", template)?;
        let tmpl = env.get_template("inline")?;
        Ok(tmpl.render(&self.context)?)
    }

    /// Render with immutable package metadata in addition to the checked FMI
    /// and Solve products. Package identities are minted once by the generic
    /// artifact layer; FMI templates consume them without inventing a second
    /// identity source.
    #[cfg(test)]
    pub fn render_with_bindings(
        &self,
        template: &str,
        artifact: &TemplateBindings<'_>,
    ) -> Result<String, CodegenError> {
        let mut env = target_template_environment();
        env.add_template("inline", template)?;
        let tmpl = env.get_template("inline")?;
        Ok(tmpl.render(artifact.render_context(minijinja::context! {
            ..self.context.clone()
        })?)?)
    }

    /// Render one Solve-model body into pathless, identity-free text.
    pub fn render_solve_model_content(
        &self,
        body_template: &str,
        artifact: &TemplateBindings<'_>,
    ) -> Result<UntrustedRenderedText, CodegenError> {
        let mut env = target_template_environment();
        env.add_template("checked_solve_body", body_template)?;
        let template = env.get_template("checked_solve_body")?;
        let content = template.render(artifact.render_context(minijinja::context! {
            ..self.context.clone()
        })?)?;
        Ok(UntrustedRenderedText::new(content))
    }
}

/// Prepared FMI-component presentation capability. It is a different type
/// from Solve-model presentation, so callers cannot pair an FMI view with a
/// Solve-model template specification.
#[derive(Debug)]
pub struct PreparedFmiComponentRendering {
    context: Value,
}

/// Affine admission for the complete built-in FMI template domain.
///
/// Construction consumes the event-free component and decides every remaining
/// template-domain fact once. Target capability validation may inspect the
/// issued facts before rendering consumes this carrier; neither can construct
/// or recover the underlying proof independently.
#[must_use]
#[derive(Debug)]
pub struct AdmittedFmiRenderingInput {
    component: solve::fmi::FmiEventFreeCodegenView,
    algebraic_domain: AdmittedFmiAlgebraicDomain,
    tensor_inventory: solve::ComputeNodeCounts,
}

#[derive(Debug)]
enum AdmittedFmiAlgebraicDomain {
    Empty,
    ExactAssignments,
}

impl AdmittedFmiRenderingInput {
    /// Consume and validate the complete FMI template input domain.
    pub fn issue(component: solve::fmi::FmiEventFreeCodegenView) -> Result<Self, CodegenError> {
        let problem = component.problem();
        if solve::solve_has_initialization(problem) {
            return Err(CodegenError::dae_preparation_failed(
                "built-in FMI templates do not implement initialization owners",
                None,
            ));
        }
        let continuous = problem.continuous();
        let has_algebraic_system = !continuous.implicit_rhs().is_empty()
            || !continuous.algebraic_projection_plan().is_empty()
            || problem.solve_layout().algebraic_scalar_count() != 0;
        let algebraic_domain = if has_algebraic_system {
            if !super::solve_lazy::explicit_algebraic_assignment_complete(problem) {
                return Err(CodegenError::dae_preparation_failed(
                    "built-in FMI templates cannot render residual algebraic systems",
                    None,
                ));
            }
            AdmittedFmiAlgebraicDomain::ExactAssignments
        } else {
            AdmittedFmiAlgebraicDomain::Empty
        };
        if problem.uses_linear_solve_component()
            || problem
                .initialization()
                .residual()
                .uses_linear_solve_component()
        {
            return Err(CodegenError::dae_preparation_failed(
                "built-in FMI templates do not implement tensor linear-solve components",
                None,
            ));
        }
        let mut tensor_inventory = problem.compute_node_counts();
        tensor_inventory.add_assign(problem.initialization().residual().compute_node_counts());
        Ok(Self {
            component,
            algebraic_domain,
            tensor_inventory,
        })
    }

    /// Whether this admitted component contains an algebraic system whose
    /// construction-issued implementation is exact assignment.
    #[must_use]
    pub const fn has_exact_algebraic_system(&self) -> bool {
        matches!(
            self.algebraic_domain,
            AdmittedFmiAlgebraicDomain::ExactAssignments
        )
    }

    /// Construction-issued tensor inventory for target capability matching.
    pub const fn tensor_inventory(&self) -> solve::ComputeNodeCounts {
        self.tensor_inventory
    }
}

impl PreparedFmiComponentRendering {
    /// Prepare rendering from one consumed, already admitted FMI input.
    pub fn from_admitted(admitted: AdmittedFmiRenderingInput) -> Result<Self, CodegenError> {
        let AdmittedFmiRenderingInput { component, .. } = admitted;
        let handle = super::solve_lazy::SolveRenderHandle::fmi(component);
        Ok(Self {
            context: solve_render_context_value_with_handles(handle, None)?,
        })
    }

    #[cfg(test)]
    pub(crate) fn prepare(
        component: solve::fmi::FmiEventFreeCodegenView,
    ) -> Result<Self, CodegenError> {
        Self::from_admitted(AdmittedFmiRenderingInput::issue(component)?)
    }

    #[cfg(test)]
    pub(crate) fn render(&self, template: &str) -> Result<String, CodegenError> {
        let mut env = target_template_environment();
        env.add_template("inline", template)?;
        let template = env.get_template("inline")?;
        Ok(template.render(&self.context)?)
    }

    #[cfg(test)]
    pub(crate) fn render_with_bindings(
        &self,
        template: &str,
        artifact: &TemplateBindings<'_>,
    ) -> Result<String, CodegenError> {
        let mut env = target_template_environment();
        env.add_template("inline", template)?;
        let template = env.get_template("inline")?;
        Ok(
            template.render(artifact.render_context(minijinja::context! {
                ..self.context.clone()
            })?)?,
        )
    }

    /// Render one FMI-component body into pathless, identity-free text.
    pub fn render_fmi_component_content(
        &self,
        body_template: &str,
        artifact: &TemplateBindings<'_>,
    ) -> Result<UntrustedRenderedText, CodegenError> {
        let mut env = target_template_environment();
        env.add_template("checked_fmi_body", body_template)?;
        let template = env.get_template("checked_fmi_body")?;
        let content = template.render(artifact.render_context(minijinja::context! {
            ..self.context.clone()
        })?)?;
        Ok(UntrustedRenderedText::new(content))
    }
}

/// Render the exact built-in MLIR execution product without exposing the
/// generic template registry or a caller-selected template.
pub fn render_mlir_execution_model(
    model: Arc<solve::SolveModel>,
    model_name: &str,
) -> Result<String, CodegenError> {
    PreparedSolveModelRendering::shared(model, model_name)?
        .render_builtin_content(crate::templates::mlir_execution_template(), model_name)
}

/// Render the exact built-in CasADi execution product without exposing the
/// generic template registry or a caller-selected template.
pub fn render_casadi_execution_model(
    model: solve::SolveModel,
    model_name: &str,
) -> Result<String, CodegenError> {
    PreparedSolveModelRendering::prepare(model)?
        .render_builtin_content(crate::templates::casadi_execution_template(), model_name)
}

pub(super) fn solve_render_context_value(
    model: Arc<solve::SolveModel>,
    model_name: Option<&str>,
) -> Result<Value, CodegenError> {
    solve_render_context_value_with_handles(
        super::solve_lazy::SolveRenderHandle::standalone(model),
        model_name,
    )
}

fn solve_render_context_value_with_handles(
    handle: super::solve_lazy::SolveRenderHandle,
    model_name: Option<&str>,
) -> Result<Value, CodegenError> {
    // Lazy `solve` / `solve_derivative_nodes` (see `solve_lazy`): structural
    // fields serialize on demand and op lists materialize one op at a time, so a
    // ~150k-op model costs O(one program) here instead of ~5 GB of eager `Value`
    // materialization (`from_serialize(solve_problem)` alone was ~4.7 GB).
    let fmi_entry = handle.fmi_value();
    let solve_problem = handle.problem();
    let artifacts = handle.artifacts();
    let solve_value = super::solve_lazy::solve_value(handle.clone())?;
    let artifacts_value = super::solve_lazy::artifacts_value(handle.clone())?;
    let solve_blocks = solve_template_blocks_value(solve_problem, artifacts)?;
    let derivative_nodes = Value::from_object(LazyDerivativeNodesValue::new(
        solve_problem.continuous().derivative_rhs().clone(),
    ));
    let has_implicit_rows = solve_problem.continuous().implicit_rhs().len()? > 0;
    let implicit_rows = Value::from_object(LazyScalarRowsValue::new(
        solve_problem.continuous().implicit_rhs().clone(),
    )?);
    let implicit_jacobian_rows = if !has_implicit_rows {
        Value::from_object(render_solve::SolveRowsValue::new(Vec::new()))
    } else if artifacts
        .continuous()
        .implicit_jacobian_v_scalar
        .programs()
        .is_empty()
    {
        Value::from_object(LazyScalarRowsValue::new(
            artifacts.continuous().implicit_jacobian_v.clone(),
        )?)
    } else {
        Value::from_object(render_solve::SolveRowsValue::new(
            artifacts
                .continuous()
                .implicit_jacobian_v_scalar
                .programs()
                .to_vec(),
        ))
    };
    let full_jacobian_rows = artifacts.continuous().full_jacobian_v.clone();
    let full_jacobian_rows = Value::from_object(render_solve::SolveRowsValue::new(
        full_jacobian_rows.programs().to_vec(),
    ));
    let mut context = vec![
        ("solve", solve_value.clone()),
        ("solve_artifacts", artifacts_value),
        ("ir", solve_value),
        ("ir_kind", Value::from("solve")),
        ("solve_blocks", solve_blocks),
        ("solve_derivative_nodes", derivative_nodes),
        ("solve_implicit_rows", implicit_rows),
        ("solve_jacobian_rows", implicit_jacobian_rows),
        ("solve_full_jacobian_rows", full_jacobian_rows),
    ];
    if let Some(fmi_entry) = fmi_entry {
        context.push(("fmi", fmi_entry));
    }
    if let Some(model_name) = model_name {
        context.push(("model_name", Value::from(model_name)));
    }
    Ok(context.into_iter().collect())
}

pub(super) fn c_renderable_derivative_nodes(
    block: &solve::ComputeBlock,
) -> Result<Vec<solve::ComputeNode>, CodegenError> {
    let scalar = rumoca_eval_solve::to_scalar_program_block(block)?;
    if scalar.is_empty() {
        Ok(Vec::new())
    } else {
        Ok(vec![solve::ComputeNode::ScalarPrograms(scalar)])
    }
}
