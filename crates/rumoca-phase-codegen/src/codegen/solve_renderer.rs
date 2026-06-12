//! Reusable solve-target template renderer: one typed context, many
//! template strings (split from `codegen/mod.rs` to stay under the
//! SPEC_0021 file-size limit).

use minijinja::Value;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::render_solve;
use super::{
    CodegenError, create_environment, dae_template_json,
    reject_external_functions_for_simulation_template, solve_template_blocks_value,
};

/// Lazily materialized `dae` template entry for solve-target contexts.
///
/// FMI-style solve targets read DAE metadata in the `dae_template_json`
/// shape; building that JSON costs seconds on large models while most solve
/// targets never touch `dae` at all, so it is computed on first access.
#[derive(Debug)]
struct LazyDaeTemplateJson {
    dae: std::sync::Arc<dae::Dae>,
    value: std::sync::OnceLock<Value>,
}

impl LazyDaeTemplateJson {
    fn materialized(&self) -> &Value {
        self.value.get_or_init(|| {
            dae_template_json(&self.dae)
                .map(|json| Value::from_serialize(&json))
                .unwrap_or_default()
        })
    }
}

impl minijinja::value::Object for LazyDaeTemplateJson {
    fn repr(self: &std::sync::Arc<Self>) -> minijinja::value::ObjectRepr {
        minijinja::value::ObjectRepr::Map
    }

    fn get_value(self: &std::sync::Arc<Self>, key: &Value) -> Option<Value> {
        self.materialized().get_item(key).ok()
    }

    fn enumerate(self: &std::sync::Arc<Self>) -> minijinja::value::Enumerator {
        let keys = self
            .materialized()
            .try_iter()
            .map(|iter| iter.collect::<Vec<_>>())
            .unwrap_or_default();
        minijinja::value::Enumerator::Values(keys)
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
    let solve_value = Value::from_serialize(solve_problem);
    let artifacts_value = Value::from_serialize(artifacts);
    let solve_blocks = solve_template_blocks_value(solve_problem, artifacts)?;
    let derivative_nodes = Value::from_serialize(&solve_problem.continuous.derivative_rhs.nodes);
    let implicit_rows =
        rumoca_eval_solve::to_scalar_program_block(&solve_problem.continuous.implicit_rhs);
    let jacobian_rows =
        rumoca_eval_solve::to_scalar_program_block(&artifacts.continuous.implicit_jacobian_v);
    // Row arrays (matching the historical `.programs` shape) as typed
    // objects so the row renderers take the typed fast path.
    let implicit_rows =
        Value::from_object(render_solve::SolveRowsValue::new(implicit_rows.programs));
    let jacobian_rows =
        Value::from_object(render_solve::SolveRowsValue::new(jacobian_rows.programs));
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
            solve_jacobian_rows => jacobian_rows,
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
            solve_jacobian_rows => jacobian_rows,
        },
    })
}
