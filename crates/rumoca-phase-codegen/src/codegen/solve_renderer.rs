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
    scalar_derivative_programs: bool,
}

impl SolveTemplateRenderer {
    /// Complete solver-model view for custom execution templates. Callers keep
    /// the original DAE separately for source analysis; the optional DAE view
    /// supplied here is the exact prepared source used during lowering.
    pub fn new_owned_model_with_dae(
        model: std::sync::Arc<solve::SolveModel>,
        dae_model: &dae::Dae,
    ) -> Result<Self, CodegenError> {
        let model_entry = super::solve_lazy::model_value(model.clone())?;
        let handle = super::solve_lazy::SolveRenderHandle::Model(model);
        let renderer = solve_render_context_value_with_handles(
            handle,
            None,
            checked_dae_template_value(dae_model)?,
        )?;
        Ok(Self {
            context: minijinja::context! { solve_model => model_entry, ..renderer.context },
            ..renderer
        })
    }

    pub fn new(
        problem: &solve::SolveProblem,
        artifacts: &solve::SolveArtifacts,
        model_name: &str,
    ) -> Result<Self, CodegenError> {
        solve_render_context_value_with_dae(problem, artifacts, Some(model_name), Value::default())
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
        solve_render_context_value_with_dae(problem, artifacts, None, dae_entry)
    }

    /// Build a renderer by taking ownership of the lowered Solve IR and the
    /// model's pure-call owners from the same Solve lowering, so scalar
    /// targets inline `PureCall` ops instead of refusing the programs that
    /// contain them. Target builders that do not reuse these values avoid
    /// cloning the complete program and artifact graphs into the context.
    pub fn new_owned_with_dae_and_calls(
        problem: solve::SolveProblem,
        artifacts: solve::SolveArtifacts,
        pure_calls: solve::SolvePureCallTable,
        dae_model: &dae::Dae,
    ) -> Result<Self, CodegenError> {
        let dae_entry = checked_dae_template_value(dae_model)?;
        let handle = super::solve_lazy::SolveRenderHandle::standalone_with_calls(
            std::sync::Arc::new(problem),
            std::sync::Arc::new(artifacts),
            std::sync::Arc::new(pure_calls),
        );
        solve_render_context_value_with_handles(handle, None, dae_entry)
    }

    /// [`Self::new`] with the model's pure-call owners (see
    /// [`Self::new_owned_with_dae_and_calls`]).
    pub fn new_owned_with_calls(
        problem: solve::SolveProblem,
        artifacts: solve::SolveArtifacts,
        pure_calls: solve::SolvePureCallTable,
        model_name: &str,
    ) -> Result<Self, CodegenError> {
        let handle = super::solve_lazy::SolveRenderHandle::standalone_with_calls(
            std::sync::Arc::new(problem),
            std::sync::Arc::new(artifacts),
            std::sync::Arc::new(pure_calls),
        );
        solve_render_context_value_with_handles(handle, Some(model_name), Value::default())
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
        let me_refresh = super::me_projection::me_refresh_value(&component)?;
        let pure_calls = super::pure_call_families::PureCallFamilies::new(component.pure_calls())?;
        let assertion_observations =
            super::fmi_c_assertions::observations(component.problem(), component.pure_calls())?;
        let assertion_messages = super::fmi_c_assertions::messages(component.problem())?;
        let assertion_message_rows = assertion_messages.rows_value()?;
        let assertion_messages = Value::from_serialize(&assertion_messages.parts);
        let uses_real_extremum = super::real_extremum::used_by(
            component.problem(),
            component.artifacts(),
            Some(component.pure_calls()),
            None,
        );
        let handle = super::solve_lazy::SolveRenderHandle::fmi(component);
        let fmi = handle.fmi_value()?;
        require_dense_value_references(&fmi)?;
        let text_starts = super::fmi_c_assertions::text_starts(&fmi)?;
        let mut renderer = solve_render_context_value_with_handles(handle, None, Value::default())?;
        renderer.context = minijinja::context! { uses_real_extremum => uses_real_extremum, typed_pure_calls => pure_calls.owners_value(), typed_directional_calls => pure_calls.directional_value(), pure_call_symbols => pure_calls.symbols_value(), fmi_assertion_observations => assertion_observations, fmi_assertion_messages => assertion_messages, fmi_assertion_message_rows => assertion_message_rows, fmi_text_starts => text_starts, me_refresh => me_refresh, ..renderer.context };
        Ok(renderer)
    }

    pub fn render(&self, template: &str) -> Result<String, CodegenError> {
        let mut env = create_environment();
        env.add_template("inline", template)?;
        let tmpl = env.get_template("inline")?;
        Ok(tmpl.render(&self.context)?)
    }

    /// Admit the derivative programs for a target that requests scalar fallback.
    /// The same checked scalar view is retained in this renderer and consumed
    /// by its templates; admission performs no second expansion.
    pub fn require_scalar_derivative_programs(&self) -> Result<(), CodegenError> {
        if !self.scalar_derivative_programs {
            return Err(CodegenError::template(
                "unsupported-feature:scalar.derivative_rhs: checked derivative programs \
                 cannot be represented faithfully within the scalar projection budget",
            ));
        }
        Ok(())
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
/// type-state, and the algebraic refresh is admitted only by its checked ME
/// refresh view; this owns the remaining Solve capabilities the C templates do
/// not implement. The state-derivative kernel evaluates a linear-solve
/// component with the kernel's dense elimination (`linear_solve_kernel`); the
/// residual, projection, and initialization blocks, whose directional
/// derivatives the C templates also render, do not.
pub(super) fn require_builtin_fmi_template_domain(
    problem: &solve::SolveProblem,
) -> Result<(), CodegenError> {
    let continuous = &problem.continuous;
    if continuous.implicit_rhs.uses_linear_solve_component()
        || continuous.residual.uses_linear_solve_component()
        || continuous.manifold_residual.uses_linear_solve_component()
        || problem
            .initialization
            .residual()
            .uses_linear_solve_component()
    {
        return Err(CodegenError::dae_preparation_failed(
            "built-in FMI templates implement tensor linear-solve components only in the \
             state-derivative kernel",
            None,
        ));
    }
    Ok(())
}

/// The C components index their value-reference table by the FMI 3 value
/// reference itself: time is 0, the variables are `1..=N` in inventory order
/// and the state derivatives start at `N + 1`. Refuse any other numbering
/// rather than emit a table that would read the wrong storage.
fn require_dense_value_references(fmi: &Value) -> Result<(), CodegenError> {
    let refuse =
        || CodegenError::template("FMI value references are not dense from 1 in inventory order");
    let mut expected = 1_i64;
    for variable in fmi.get_attr("variables")?.try_iter()? {
        if variable.get_attr("value_reference_fmi3")?.as_i64() != Some(expected) {
            return Err(refuse());
        }
        expected += 1;
    }
    if fmi
        .get_attr("derivative_value_reference_base_fmi3")?
        .as_i64()
        != Some(expected)
    {
        return Err(refuse());
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
        .map(|renderer| renderer.context)
}

fn solve_render_context_value_with_dae(
    solve_problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
    model_name: Option<&str>,
    dae_entry: Value,
) -> Result<SolveTemplateRenderer, CodegenError> {
    solve_render_context_value_with_handles(
        super::solve_lazy::SolveRenderHandle::standalone(
            std::sync::Arc::new(solve_problem.clone()),
            std::sync::Arc::new(artifacts.clone()),
        ),
        model_name,
        dae_entry,
    )
}

/// Default value of every compiled parameter slot (`P` layout order), from
/// each parameter's statically evaluated binding (else start) values in the DAE
/// template context. `None` where no value is statically known (a parameter
/// computed by initialization, an input, a discrete or relation-memory slot),
/// so a target can emit NaN there and let `init_params` fill computed slots.
fn parameter_defaults(problem: &solve::SolveProblem, dae_entry: &Value) -> Vec<Option<f64>> {
    let mut defaults = vec![None; problem.layout.p_scalars()];
    let Ok(variables) = dae_entry.get_attr("variables").and_then(|v| v.try_iter()) else {
        return defaults;
    };
    for variable in variables {
        let Some(name) = variable
            .get_attr("name")
            .ok()
            .and_then(|n| n.as_str().map(str::to_owned))
        else {
            continue;
        };
        let Some(solve::ScalarSlot::P { index, .. }) = problem.layout.binding(&name) else {
            continue;
        };
        let Ok(attributes) = variable.get_attr("attributes") else {
            continue;
        };
        let values = ["binding_values", "start_values"]
            .into_iter()
            .filter_map(|key| attributes.get_attr(key).ok())
            .find(|values| !values.is_none() && !values.is_undefined());
        let Some(values) = values.and_then(|v| v.try_iter().ok()) else {
            continue;
        };
        for (offset, value) in values.enumerate() {
            if let Some(slot) = defaults.get_mut(index + offset) {
                *slot = f64::try_from(value).ok();
            }
        }
    }
    defaults
}

/// The scalar name of every compiled parameter slot (`P` layout order), from
/// the layout's scalar bindings; `None` for a slot no scalar name binds.
fn parameter_names(problem: &solve::SolveProblem) -> Vec<Option<String>> {
    let mut names = vec![None; problem.layout.p_scalars()];
    for (name, slot) in problem.layout.bindings() {
        if problem.layout.shape(name.as_str()).is_some() {
            continue; // an array base name; its scalars carry the element names
        }
        if let solve::ScalarSlot::P { index, .. } = slot
            && let Some(entry) = names.get_mut(*index)
        {
            *entry = Some(name.as_str().to_string());
        }
    }
    names
}

fn solve_render_context_value_with_handles(
    handle: super::solve_lazy::SolveRenderHandle,
    model_name: Option<&str>,
    dae_entry: Value,
) -> Result<SolveTemplateRenderer, CodegenError> {
    // Lazy `solve` / `solve_derivative_nodes` (see `solve_lazy`): structural
    // fields serialize on demand and op lists materialize one op at a time, so a
    // ~150k-op model costs O(one program) here instead of ~5 GB of eager `Value`
    // materialization (`from_serialize(solve_problem)` alone was ~4.7 GB).
    let fmi_entry = handle.fmi_value()?;
    let solve_problem = handle.problem();
    if solve_problem
        .discrete
        .structured_updates
        .iter()
        .any(|update| matches!(update.role, solve::DiscreteRowRole::QuotientMemory { .. }))
    {
        return Err(CodegenError::template(
            "generated Solve targets do not yet support certified structured quotient initialization".to_string()));
    }
    let artifacts = handle.artifacts();
    let solve_value = super::solve_lazy::solve_value(handle.clone())?;
    let artifacts_value = super::solve_lazy::artifacts_value(handle.clone())?;
    let prepared_blocks =
        solve_template_blocks_value(solve_problem, artifacts, handle.inlined_pure_calls())?;
    let scalar_derivative_programs = prepared_blocks.scalar_derivative_programs;
    let solve_blocks = prepared_blocks.value;
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
        Value::from_object(render_solve::SolveRowsValue::from_block(
            &artifacts.continuous.implicit_jacobian_v_scalar,
        ))
    };
    let full_jacobian_rows = Value::from_object(render_solve::SolveRowsValue::from_block(
        &artifacts.continuous.full_jacobian_v,
    ));
    let uses_real_extremum = super::real_extremum::used_by(
        solve_problem,
        artifacts,
        handle.pure_calls(),
        handle.inlined_pure_calls(),
    );
    let parameter_defaults = Value::from(parameter_defaults(solve_problem, &dae_entry));
    let parameter_names = Value::from(parameter_names(solve_problem));
    let context = match model_name {
        Some(name) => minijinja::context! {
            solve_parameter_defaults => parameter_defaults.clone(),
            solve_parameter_names => parameter_names.clone(),
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
            uses_real_extremum => uses_real_extremum,
        },
        None => minijinja::context! {
            solve_parameter_defaults => parameter_defaults,
            solve_parameter_names => parameter_names,
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
            uses_real_extremum => uses_real_extremum,
        },
    };
    Ok(SolveTemplateRenderer {
        context,
        scalar_derivative_programs,
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
