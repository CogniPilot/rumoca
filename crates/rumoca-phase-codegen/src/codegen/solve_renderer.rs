//! Reusable solve-target template renderer: one typed context, many
//! template strings (split from `codegen/mod.rs` to stay under the
//! SPEC_0021 file-size limit).

use minijinja::Value;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::render_solve;
use super::{
    CodegenError, LazyDerivativeNodesValue, LazyScalarRowsValue, create_environment,
    dae_template_json_for_solve_context, solve_template_blocks_value,
};

#[derive(Debug)]
pub struct SolveTemplateRenderer {
    context: Value,
}

impl SolveTemplateRenderer {
    pub fn new(
        problem: &solve::SolveProblem,
        artifacts: &solve::SolveArtifacts,
        model_name: &str,
    ) -> Result<Self, CodegenError> {
        Ok(Self {
            context: solve_render_context_value(problem, artifacts, Some(model_name))?,
        })
    }

    /// Renderer over typed inputs that also exposes the (scalarized) DAE to
    /// templates and applies the external-function guard per render. This is
    /// the multi-file target path: one context, many template strings.
    pub fn new_with_dae(
        problem: &solve::SolveProblem,
        artifacts: &solve::SolveArtifacts,
        dae_model: &dae::Dae,
    ) -> Result<Self, CodegenError> {
        let dae_entry = checked_dae_template_value(dae_model)?;
        Ok(Self {
            context: solve_render_context_value_with_dae(problem, artifacts, None, dae_entry)?,
        })
    }

    /// Build a renderer by taking ownership of the lowered Solve IR. Target
    /// builders that do not reuse these values avoid cloning the complete
    /// program and artifact graphs into the lazy template context.
    pub fn new_owned_with_dae(
        problem: solve::SolveProblem,
        artifacts: solve::SolveArtifacts,
        dae_model: &dae::Dae,
    ) -> Result<Self, CodegenError> {
        let dae_entry = checked_dae_template_value(dae_model)?;
        let handle = super::solve_lazy::SolveRenderHandle::standalone(
            std::sync::Arc::new(problem),
            std::sync::Arc::new(artifacts),
        );
        Ok(Self {
            context: solve_render_context_value_with_handles(handle, None, dae_entry)?,
        })
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
    /// ```compile_fail
    /// # use rumoca_phase_codegen::SolveTemplateRenderer;
    /// fn pair(
    ///     component: rumoca_ir_solve::fmi::FmiEventFreeCodegenView,
    ///     foreign: &rumoca_ir_dae::Dae,
    /// ) {
    ///     let _ = SolveTemplateRenderer::new_owned_with_fmi(component, foreign);
    /// }
    /// ```
    ///
    /// Should an FMI template ever need a DAE fact, it must travel inside the
    /// correlated component rather than arrive beside it.
    ///
    /// The consuming C profile proves event freedom or parameter-dependent
    /// assertions before this renderer exists. The original event-free view
    /// converts after checking the same parameter-initialization profile.
    pub fn new_owned_with_fmi(
        component: impl TryInto<solve::fmi::FmiCCodegenView, Error: std::fmt::Display>,
    ) -> Result<Self, CodegenError> {
        let component = component
            .try_into()
            .map_err(|error| CodegenError::template(error.to_string()))?;
        require_builtin_fmi_template_domain(component.problem())?;
        let pure_calls = Value::from_serialize(component.pure_calls());
        let assertion_messages = super::fmi_c_assertions::messages(component.problem())?;
        let handle = super::solve_lazy::SolveRenderHandle::fmi(component);
        let context = solve_render_context_value_with_handles(handle, None, Value::default())?;
        Ok(Self {
            context: minijinja::context! { typed_pure_calls => pure_calls, fmi_assertion_messages => assertion_messages, ..context },
        })
    }

    pub fn render(&self, template: &str) -> Result<String, CodegenError> {
        let mut env = create_environment();
        env.add_template("inline", template)?;
        let tmpl = env.get_template("inline")?;
        Ok(tmpl.render(&self.context)?)
    }

    pub fn render_with_name(
        &self,
        template: &str,
        model_name: &str,
    ) -> Result<String, CodegenError> {
        self.render_with_name_and_artifact(template, model_name, &())
    }

    /// Render with immutable package metadata in addition to the checked FMI
    /// and Solve products. Package identities are minted once by the generic
    /// artifact layer; FMI templates consume them without inventing a second
    /// identity source.
    pub fn render_with_name_and_artifact<T: serde::Serialize>(
        &self,
        template: &str,
        model_name: &str,
        artifact: &T,
    ) -> Result<String, CodegenError> {
        let mut env = create_environment();
        env.add_template("inline", template)?;
        let tmpl = env.get_template("inline")?;
        Ok(tmpl.render(minijinja::context! {
            model_name => model_name,
            artifact => Value::from_serialize(artifact),
            ..self.context.clone()
        })?)
    }
}

/// Prove the complete current built-in FMI template domain before a renderer
/// exists. The event/storage inventory is already carried by the input
/// type-state; this owns the remaining Solve capabilities the C templates do
/// not implement.
fn require_builtin_fmi_template_domain(problem: &solve::SolveProblem) -> Result<(), CodegenError> {
    let continuous = &problem.continuous;
    let has_algebraic_system = !continuous.implicit_rhs.is_empty()
        || !continuous.algebraic_projection_plan.is_empty()
        || problem.solve_layout.algebraic_scalar_count() != 0;
    if has_algebraic_system && !super::solve_lazy::explicit_algebraic_assignment_complete(problem) {
        return Err(CodegenError::dae_preparation_failed(
            "built-in FMI templates cannot render residual algebraic systems",
            None,
        ));
    }
    if problem.uses_linear_solve_component()
        || problem
            .initialization
            .residual
            .uses_linear_solve_component()
    {
        return Err(CodegenError::dae_preparation_failed(
            "built-in FMI templates do not implement tensor linear-solve components",
            None,
        ));
    }
    Ok(())
}

fn checked_dae_template_value(dae: &dae::Dae) -> Result<Value, CodegenError> {
    Ok(Value::from_serialize(dae_template_json_for_solve_context(
        dae,
    )?))
}

pub(super) fn solve_render_context_value(
    solve_problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
    model_name: Option<&str>,
) -> Result<Value, CodegenError> {
    solve_render_context_value_with_dae(solve_problem, artifacts, model_name, Value::default())
}

fn solve_render_context_value_with_dae(
    solve_problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
    model_name: Option<&str>,
    dae_entry: Value,
) -> Result<Value, CodegenError> {
    solve_render_context_value_with_handles(
        super::solve_lazy::SolveRenderHandle::standalone(
            std::sync::Arc::new(solve_problem.clone()),
            std::sync::Arc::new(artifacts.clone()),
        ),
        model_name,
        dae_entry,
    )
}

fn solve_render_context_value_with_handles(
    handle: super::solve_lazy::SolveRenderHandle,
    model_name: Option<&str>,
    dae_entry: Value,
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
        solve_problem.continuous.derivative_rhs.clone(),
    ));
    let has_implicit_rows = solve_problem.continuous.implicit_rhs.len()? > 0;
    let implicit_rows = Value::from_object(LazyScalarRowsValue::new(
        solve_problem.continuous.implicit_rhs.clone(),
    )?);
    let implicit_jacobian_rows = if !has_implicit_rows {
        Value::from_object(render_solve::SolveRowsValue::new(Vec::new()))
    } else if artifacts
        .continuous
        .implicit_jacobian_v_scalar
        .programs()
        .is_empty()
    {
        Value::from_object(LazyScalarRowsValue::new(
            artifacts.continuous.implicit_jacobian_v.clone(),
        )?)
    } else {
        Value::from_object(render_solve::SolveRowsValue::new(
            artifacts
                .continuous
                .implicit_jacobian_v_scalar
                .programs()
                .to_vec(),
        ))
    };
    let full_jacobian_rows = artifacts.continuous.full_jacobian_v.clone();
    let full_jacobian_rows = Value::from_object(render_solve::SolveRowsValue::new(
        full_jacobian_rows.programs().to_vec(),
    ));
    Ok(match model_name {
        Some(name) => minijinja::context! {
            dae => dae_entry.clone(),
            fmi => fmi_entry.clone(),
            solve => solve_value.clone(),
            solve_artifacts => artifacts_value,
            ir => solve_value,
            ir_kind => "solve",
            model_name => name,
            solve_blocks => solve_blocks,
            solve_derivative_nodes => derivative_nodes,
            solve_implicit_rows => implicit_rows,
            solve_jacobian_rows => implicit_jacobian_rows,
            solve_full_jacobian_rows => full_jacobian_rows,
        },
        None => minijinja::context! {
            dae => dae_entry.clone(),
            fmi => fmi_entry.clone(),
            solve => solve_value.clone(),
            solve_artifacts => artifacts_value,
            ir => solve_value,
            ir_kind => "solve",
            solve_blocks => solve_blocks,
            solve_derivative_nodes => derivative_nodes,
            solve_implicit_rows => implicit_rows,
            solve_jacobian_rows => implicit_jacobian_rows,
            solve_full_jacobian_rows => full_jacobian_rows,
        },
    })
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
