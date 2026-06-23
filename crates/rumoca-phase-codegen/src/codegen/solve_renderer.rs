//! Reusable solve-target template renderer: one typed context, many
//! template strings (split from `codegen/mod.rs` to stay under the
//! SPEC_0021 file-size limit).

use minijinja::Value;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::render_solve;
use super::{
    CodegenError, LazyDerivativeNodesValue, LazyScalarRowsValue, create_environment,
    dae_template_json, reject_external_functions_for_simulation_template,
    solve_template_blocks_value,
};

/// Lazily materialized `dae` template entry for solve-target contexts.
///
/// FMI-style solve targets read DAE metadata in the `dae_template_json`
/// shape; building that JSON costs seconds on large models while most solve
/// targets never touch `dae` at all, so it is computed on first access.
#[derive(Debug)]
struct LazyDaeTemplateJson {
    dae: std::sync::Arc<dae::Dae>,
    value: std::sync::OnceLock<Option<Value>>,
}

impl LazyDaeTemplateJson {
    /// Materialization failure surfaces as `None`/empty here, which the
    /// strict-undefined template environment turns into a render error at
    /// the access site — a visible failure, not a silent default.
    fn materialized(&self) -> Option<&Value> {
        self.value
            .get_or_init(|| match dae_template_json(&self.dae) {
                Ok(json) => Some(Value::from_serialize(&json)),
                Err(_) => None,
            })
            .as_ref()
    }
}

impl minijinja::value::Object for LazyDaeTemplateJson {
    fn repr(self: &std::sync::Arc<Self>) -> minijinja::value::ObjectRepr {
        minijinja::value::ObjectRepr::Map
    }

    fn get_value(self: &std::sync::Arc<Self>, key: &Value) -> Option<Value> {
        self.materialized()?.get_item(key).ok()
    }

    fn enumerate(self: &std::sync::Arc<Self>) -> minijinja::value::Enumerator {
        match self.materialized().map(Value::try_iter) {
            Some(Ok(iter)) => minijinja::value::Enumerator::Values(iter.collect()),
            _ => minijinja::value::Enumerator::Empty,
        }
    }
}

#[derive(Debug)]
pub struct SolveTemplateRenderer {
    context: Value,
    /// Scalarized DAE retained for the per-template external-function guard.
    guard_dae: Option<std::sync::Arc<dae::Dae>>,
}

impl SolveTemplateRenderer {
    pub fn new(
        problem: &solve::SolveProblem,
        artifacts: &solve::SolveArtifacts,
        model_name: &str,
    ) -> Result<Self, CodegenError> {
        Ok(Self {
            context: solve_render_context_value(problem, artifacts, Some(model_name))?,
            guard_dae: None,
        })
    }

    /// Renderer over typed inputs that also exposes the (scalarized) DAE to
    /// templates and applies the external-function guard per render. This is
    /// the multi-file target path: one context, many template strings.
    pub fn new_with_dae(
        problem: &solve::SolveProblem,
        artifacts: &solve::SolveArtifacts,
        dae_model: dae::Dae,
    ) -> Result<Self, CodegenError> {
        let dae_model = std::sync::Arc::new(dae_model);
        let dae_entry = Value::from_object(LazyDaeTemplateJson {
            dae: dae_model.clone(),
            value: std::sync::OnceLock::new(),
        });
        Ok(Self {
            context: solve_render_context_value_with_dae(problem, artifacts, None, dae_entry)?,
            guard_dae: Some(dae_model),
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
        if let Some(dae_model) = &self.guard_dae {
            reject_external_functions_for_simulation_template(dae_model, template)?;
        }
        let mut env = create_environment();
        env.add_template("inline", template)?;
        let tmpl = env.get_template("inline")?;
        Ok(tmpl.render(minijinja::context! {
            model_name => model_name,
            ..self.context.clone()
        })?)
    }
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
    // Lazy `solve` / `solve_derivative_nodes` (see `solve_lazy`): structural
    // fields serialize on demand and op lists materialize one op at a time, so a
    // ~150k-op model costs O(one program) here instead of ~5 GB of eager `Value`
    // materialization (`from_serialize(solve_problem)` alone was ~4.7 GB).
    let problem_arc = std::sync::Arc::new(solve_problem.clone());
    let artifacts_arc = std::sync::Arc::new(artifacts.clone());
    let solve_value = super::solve_lazy::solve_value(problem_arc.clone(), artifacts_arc.clone())?;
    let artifacts_value = super::solve_lazy::artifacts_value(artifacts_arc.clone())?;
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
        .programs
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
                .programs
                .clone(),
        ))
    };
    let full_jacobian_rows = artifacts.continuous.full_jacobian_v.clone();
    let full_jacobian_rows = Value::from_object(render_solve::SolveRowsValue::new(
        full_jacobian_rows.programs,
    ));
    Ok(match model_name {
        Some(name) => minijinja::context! {
            dae => dae_entry.clone(),
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
