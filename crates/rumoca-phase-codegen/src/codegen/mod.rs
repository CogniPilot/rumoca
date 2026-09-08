//! Template-driven code generation and shared render helpers.

use crate::errors::{CodegenError, render_err};
use minijinja::{Environment, UndefinedBehavior, Value};
use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rumoca_ir_solve as solve;

use crate::{TemplateBindings, UntrustedRenderedText};

mod algorithm_code_renderer;
#[cfg(test)]
mod checked_dae_diagnostic_tests;
#[cfg(test)]
mod checked_dae_tests;
#[cfg(test)]
mod codegen_test_support;
#[cfg(test)]
mod command_registry_tests;
mod dae_backend;
mod dae_diagnostics;
mod discrete_render_view;
#[cfg(test)]
mod fmi_projection_tests;
#[cfg(test)]
mod galec_golden_tests;
#[cfg(test)]
mod galec_manifest_template_tests;
mod render_solve;
mod render_solve_ops;
#[cfg(test)]
mod scalar_plan_template_tests;
mod scalar_program_plan;
mod solve_algorithm_production_renderer;
mod solve_lazy;
mod solve_renderer;
#[cfg(test)]
mod solve_template_context_tests;
#[cfg(test)]
mod stencil_codegen_tests;
#[cfg(test)]
mod wgsl_ode_tests;

use render_solve::{
    render_linsolve_mlir_function, render_matmul_mlir_function,
    render_solve_row_output_wgsl_function, render_wgsl_kernel_schedule_json_function,
    render_wgsl_kernel_workgroup_total_function, render_wgsl_native_family_inventory_json_function,
};

pub use solve_algorithm_production_renderer::render_solve_algorithm_production_file;
pub use solve_lazy::explicit_algebraic_assignment_complete;

/// Result type for internal render functions.
pub(crate) type RenderResult = Result<String, minijinja::Error>;

/// Read one required field from a dynamic template value.
///
/// This is transport validation only: it does not interpret an IR variant or
/// choose target syntax. Serialized maps return `undefined` for a missing key,
/// so callers need one fail-closed check shared across template commands.
pub(crate) fn get_field(value: &Value, name: &str) -> Result<Value, minijinja::Error> {
    if let Ok(result) = value.get_attr(name)
        && !result.is_undefined()
        && !result.is_none()
    {
        return Ok(result);
    }
    if let Ok(result) = value.get_item(&Value::from(name))
        && !result.is_undefined()
        && !result.is_none()
    {
        return Ok(result);
    }
    Err(minijinja::Error::new(
        minijinja::ErrorKind::UndefinedError,
        format!("field '{name}' not found"),
    ))
}

pub(crate) fn render_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
) -> Result<Vec<T>, minijinja::Error> {
    let mut values = Vec::new();
    reserve_render_capacity(&mut values, capacity, context)?;
    Ok(values)
}

pub(crate) fn reserve_render_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
) -> Result<(), minijinja::Error> {
    values
        .try_reserve_exact(additional)
        .map_err(|_| render_err(format!("{context} exceeds host memory limits")))
}

/// Strictly parsed target-completion presentation. The template engine and
/// source remain phase-owned; publication can supply only the final output
/// path and receives inert rendered text.
#[derive(Debug)]
pub struct PreparedCompletionMessage {
    source: Option<Box<str>>,
    model_name: Box<str>,
    target_name: Box<str>,
}

impl PreparedCompletionMessage {
    pub fn construct(
        source: Option<&str>,
        model_name: &str,
        target_name: &str,
    ) -> Result<Self, CodegenError> {
        if let Some(source) = source {
            let mut environment = target_template_environment();
            environment.add_template("completion_message", source)?;
            let _ = environment.get_template("completion_message")?;
        }
        Ok(Self {
            source: source.map(Into::into),
            model_name: model_name.into(),
            target_name: target_name.into(),
        })
    }

    pub fn render(&self, output_path: &str) -> Result<Option<String>, CodegenError> {
        self.source
            .as_deref()
            .map(|source| {
                let mut environment = target_template_environment();
                environment.add_template("completion_message", source)?;
                environment
                    .get_template("completion_message")?
                    .render(minijinja::context! {
                        out_dir => output_path,
                        model_name => self.model_name.as_ref(),
                        target_name => self.target_name.as_ref(),
                    })
                    .map_err(Into::into)
            })
            .transpose()
    }
}

pub fn dae_template_json(dae: &dae::Dae) -> Result<serde_json::Value, CodegenError> {
    dae_backend::project(dae).map_err(|error| {
        CodegenError::dae_preparation_failed(error.to_string(), Some(error.span()))
    })
}

fn dae_template_value(dae: &dae::Dae) -> Result<Value, CodegenError> {
    Ok(Value::from_serialize(dae_template_json(dae)?))
}

/// Prepared Flat presentation capability.
///
/// Preparation proves the materialized-equation-view requirement and
/// serializes the checked Flat context exactly once, before any rendering
/// begins; it is a different type from the DAE capability, so a Flat
/// template cannot be paired with a projected DAE context.
#[derive(Debug)]
pub struct PreparedFlatRendering {
    value: Value,
}

/// Prepared AST presentation capability.
///
/// AST serialization is infallible, but retaining the serialized value keeps
/// every post-admission renderer free of raw semantic roots.
#[derive(Debug)]
pub struct PreparedAstRendering {
    value: Value,
}

/// Prepared DAE presentation capability.
///
/// Preparation runs the checked DAE projection, including attribute
/// evaluation and shape checks, exactly once before any rendering begins.
/// The retained source map serves only render-time diagnostics.
pub struct PreparedDaeRendering {
    source_map: rumoca_core::SourceMap,
    value: Value,
}

fn require_materialized_flat_equation_view(flat_model: &flat::Model) -> Result<(), CodegenError> {
    for (partition, families) in [
        (
            "regular equations",
            flat_model.structured_equations.as_slice(),
        ),
        (
            "initial equations",
            flat_model.initial_structured_equations.as_slice(),
        ),
    ] {
        if let Some(family) = families
            .iter()
            .find(|family| !family.interiors_materialized)
        {
            return Err(CodegenError::NonMaterializedStructuredFamily {
                partition,
                origin: family.origin.to_string(),
                span: Some(family.span),
            });
        }
    }
    Ok(())
}

fn render_dae_context(
    tmpl: &minijinja::Template<'_, '_>,
    prepared: &PreparedDaeRendering,
    model_name: Option<&str>,
    artifact: Option<&TemplateBindings<'_>>,
) -> Result<String, CodegenError> {
    let dae_value = prepared.value.clone();
    let semantic = match model_name {
        Some(name) => minijinja::context! {
            dae => dae_value.clone(),
            ir => dae_value,
            ir_kind => "dae",
            model_name => name,
        },
        None => minijinja::context! {
            dae => dae_value.clone(),
            ir => dae_value,
            ir_kind => "dae",
        },
    };
    let rendered = match artifact {
        Some(artifact) => artifact
            .render_context(semantic)
            .and_then(|context| tmpl.render(context)),
        None => tmpl.render(semantic),
    };
    rendered.map_err(|error| dae_diagnostics::render_error(&prepared.source_map, error))
}

fn solve_template_blocks_value(
    solve_problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
) -> Result<Value, CodegenError> {
    Ok(minijinja::context! {
        continuous => minijinja::context! {
            implicit_rhs => solve_template_compute_block_json(solve_problem.continuous().implicit_rhs())?,
            residual => solve_template_compute_block_json(solve_problem.continuous().residual())?,
            derivative_rhs => solve_template_compute_block_json(solve_problem.continuous().derivative_rhs())?,
        },
        artifacts => minijinja::context! {
            continuous => minijinja::context! {
                implicit_jacobian_v => solve_template_compute_block_json(&artifacts.continuous().implicit_jacobian_v)?,
            },
        },
    })
}

#[derive(Debug)]
struct LazyScalarProgramsValue {
    scalar: std::sync::Arc<solve::ScalarProgramBlock>,
}

impl LazyScalarProgramsValue {
    fn new(scalar: std::sync::Arc<solve::ScalarProgramBlock>) -> Self {
        Self { scalar }
    }

    fn scalar(&self) -> &std::sync::Arc<solve::ScalarProgramBlock> {
        &self.scalar
    }
}

impl minijinja::value::Object for LazyScalarProgramsValue {
    fn repr(self: &std::sync::Arc<Self>) -> minijinja::value::ObjectRepr {
        minijinja::value::ObjectRepr::Map
    }

    fn get_value(self: &std::sync::Arc<Self>, key: &Value) -> Option<Value> {
        let scalar = self.scalar();
        match key.as_str()? {
            "programs" => Some(Value::from_object(solve_lazy::SolveProgramsObject {
                block: scalar.clone(),
            })),
            "program_spans" => Some(Value::from_serialize(scalar.program_spans())),
            "output_indices" => Some(Value::from_serialize(scalar.output_indices())),
            _ => None,
        }
    }

    fn enumerate(self: &std::sync::Arc<Self>) -> minijinja::value::Enumerator {
        minijinja::value::Enumerator::Values(vec![
            Value::from("programs"),
            Value::from("program_spans"),
            Value::from("output_indices"),
        ])
    }
}

#[derive(Debug)]
pub(in crate::codegen) struct LazyScalarRowsValue {
    block: std::sync::Arc<solve::ComputeBlock>,
    row_count: usize,
    scalar: std::sync::OnceLock<Option<std::sync::Arc<Vec<Vec<solve::LinearOp>>>>>,
}

impl LazyScalarRowsValue {
    pub(in crate::codegen) fn new(block: solve::ComputeBlock) -> Result<Self, CodegenError> {
        let row_count = block.len()?;
        Ok(Self {
            block: std::sync::Arc::new(block),
            row_count,
            scalar: std::sync::OnceLock::new(),
        })
    }

    fn rows(&self) -> Option<&std::sync::Arc<Vec<Vec<solve::LinearOp>>>> {
        self.scalar
            .get_or_init(|| {
                rumoca_eval_solve::to_scalar_program_block(&self.block)
                    .ok()
                    .map(|scalar| std::sync::Arc::new(scalar.programs().to_vec()))
            })
            .as_ref()
    }
}

impl minijinja::value::Object for LazyScalarRowsValue {
    fn repr(self: &std::sync::Arc<Self>) -> minijinja::value::ObjectRepr {
        minijinja::value::ObjectRepr::Seq
    }

    fn get_value(self: &std::sync::Arc<Self>, key: &Value) -> Option<Value> {
        let index = key.as_usize()?;
        let rows = self.rows()?;
        (index < rows.len())
            .then(|| Value::from_object(render_solve::SolveRowValue::new(rows.clone(), index)))
    }

    fn enumerate(self: &std::sync::Arc<Self>) -> minijinja::value::Enumerator {
        minijinja::value::Enumerator::Seq(self.row_count)
    }
}

#[derive(Debug)]
pub(in crate::codegen) struct LazyDerivativeNodesValue {
    block: std::sync::Arc<solve::ComputeBlock>,
    nodes: std::sync::OnceLock<Option<std::sync::Arc<Vec<solve::ComputeNode>>>>,
}

impl LazyDerivativeNodesValue {
    pub(in crate::codegen) fn new(block: solve::ComputeBlock) -> Self {
        Self {
            block: std::sync::Arc::new(block),
            nodes: std::sync::OnceLock::new(),
        }
    }

    fn nodes(&self) -> Option<&std::sync::Arc<Vec<solve::ComputeNode>>> {
        self.nodes
            .get_or_init(|| {
                solve_renderer::c_renderable_derivative_nodes(&self.block)
                    .ok()
                    .map(std::sync::Arc::new)
            })
            .as_ref()
    }
}

impl minijinja::value::Object for LazyDerivativeNodesValue {
    fn repr(self: &std::sync::Arc<Self>) -> minijinja::value::ObjectRepr {
        minijinja::value::ObjectRepr::Seq
    }

    fn get_value(self: &std::sync::Arc<Self>, key: &Value) -> Option<Value> {
        let index = key.as_usize()?;
        self.nodes()?.get(index).map(Value::from_serialize)
    }

    fn enumerate(self: &std::sync::Arc<Self>) -> minijinja::value::Enumerator {
        match self.nodes() {
            Some(nodes) => minijinja::value::Enumerator::Seq(nodes.len()),
            None => minijinja::value::Enumerator::Empty,
        }
    }
}

fn solve_template_compute_block_json(block: &solve::ComputeBlock) -> Result<Value, CodegenError> {
    let partition = render_solve::native_family_template_partition(block)?;
    let uses_linear_solve = compute_block_uses_linear_solve_component(block);
    // Lazy nodes (one ComputeNode -> ops materialized on demand) so blocks whose
    // nodes contain large op programs don't materialize as eager Values.
    let nodes = solve_lazy::nodes_value(std::sync::Arc::new(block.clone()))?;
    let output_count = block.len()?;
    let scalar = std::sync::Arc::new(rumoca_eval_solve::to_scalar_program_block(block)?);
    let scalar_plan =
        Value::from_object(scalar_program_plan::ScalarProgramPlan::new(scalar.clone())?);
    let scalar_programs = Value::from_object(LazyScalarProgramsValue::new(scalar));
    let fallback_programs = Value::from_object(render_solve::SolveRowsValue::new(
        partition.fallback_programs,
    ));
    let scalar_fallback_rows = Value::from_object(render_solve::SolveScalarFallbackRowsValue::new(
        partition.scalar_fallback_rows,
    ));
    let native_families = Value::from_object(render_solve::SolveNativeFamiliesValue::new(
        partition.families,
    ));
    let native_dense_nodes = Value::from_object(render_solve::SolveNativeDenseNodesValue::new(
        partition.native_dense_nodes,
    )?);
    Ok(minijinja::context! {
        nodes => nodes,
        scalar_plan => scalar_plan,
        scalar_programs => scalar_programs,
        fallback_programs => fallback_programs,
        native_families => native_families,
        native_dense_nodes => native_dense_nodes,
        scalar_fallback_rows => scalar_fallback_rows,
        output_count => output_count,
        tensor_node_count => block.tensor_node_count(),
        map_family_count => partition.map_family_count,
        stencil_family_count => partition.stencil_family_count,
        scalar_programs_use_linear_solve_component => uses_linear_solve,
    })
}

fn scalar_program_block_uses_linear_solve_component(block: &solve::ScalarProgramBlock) -> bool {
    block
        .programs()
        .iter()
        .flatten()
        .any(|op| matches!(op, solve::LinearOp::LinearSolveComponent { .. }))
}

fn compute_block_uses_linear_solve_component(block: &solve::ComputeBlock) -> bool {
    block.nodes.iter().any(|node| match node {
        solve::ComputeNode::ScalarPrograms(block) => {
            scalar_program_block_uses_linear_solve_component(block)
        }
        solve::ComputeNode::LinSolve { .. } => true,
        solve::ComputeNode::Map { .. }
        | solve::ComputeNode::AffineStencil { .. }
        | solve::ComputeNode::MatMul { .. } => false,
    })
}

fn render_flat_context(
    tmpl: &minijinja::Template<'_, '_>,
    prepared: &PreparedFlatRendering,
    model_name: Option<&str>,
    artifact: Option<&TemplateBindings<'_>>,
) -> RenderResult {
    let flat_value = prepared.value.clone();
    let semantic = match model_name {
        Some(name) => minijinja::context! {
            flat => flat_value.clone(),
            ir => flat_value,
            ir_kind => "flat",
            model_name => name,
        },
        None => minijinja::context! {
            flat => flat_value.clone(),
            ir => flat_value,
            ir_kind => "flat",
        },
    };
    match artifact {
        Some(artifact) => tmpl.render(artifact.render_context(semantic)?),
        None => tmpl.render(semantic),
    }
}

fn render_ast_context(
    tmpl: &minijinja::Template<'_, '_>,
    prepared: &PreparedAstRendering,
    model_name: Option<&str>,
    artifact: Option<&TemplateBindings<'_>>,
) -> RenderResult {
    let ast_value = prepared.value.clone();
    let semantic = match model_name {
        Some(name) => minijinja::context! {
            ast => ast_value.clone(),
            ir => ast_value,
            ir_kind => "ast",
            model_name => name,
        },
        None => minijinja::context! {
            ast => ast_value.clone(),
            ir => ast_value,
            ir_kind => "ast",
        },
    };
    match artifact {
        Some(artifact) => tmpl.render(artifact.render_context(semantic)?),
        None => tmpl.render(semantic),
    }
}

/// Render one target output-path template against a model name.
///
/// This produces only pathless [`UntrustedRenderedText`]; interpreting the
/// bytes as a checked output path is the caller's responsibility.
pub fn render_output_path(
    output_path_template: &str,
    model_name: &str,
) -> Result<UntrustedRenderedText, CodegenError> {
    let mut env = target_template_environment();
    env.add_template("checked_output_path", output_path_template)?;
    let template = env.get_template("checked_output_path")?;
    let content = template.render(minijinja::context! { model_name => model_name })?;
    Ok(UntrustedRenderedText::new(content))
}

/// Render one AST body template into pathless, identity-free text.
pub fn render_ast_template_content(
    body_template: &str,
    prepared: &PreparedAstRendering,
    artifact: &TemplateBindings<'_>,
) -> Result<UntrustedRenderedText, CodegenError> {
    let mut env = target_template_environment();
    env.add_template("checked_ast_body", body_template)?;
    let template = env.get_template("checked_ast_body")?;
    let content = render_ast_context(&template, prepared, None, Some(artifact))?;
    Ok(UntrustedRenderedText::new(content))
}

/// Render one Flat body template into pathless, identity-free text.
pub fn render_flat_template_content(
    prepared: &PreparedFlatRendering,
    body_template: &str,
    artifact: &TemplateBindings<'_>,
) -> Result<UntrustedRenderedText, CodegenError> {
    let mut env = target_template_environment();
    env.add_template("checked_flat_body", body_template)?;
    let template = env.get_template("checked_flat_body")?;
    let content = render_flat_context(&template, prepared, None, Some(artifact))?;
    Ok(UntrustedRenderedText::new(content))
}

/// Render one DAE body template into pathless, identity-free text.
pub fn render_dae_template_content(
    prepared: &PreparedDaeRendering,
    body_template: &str,
    artifact: &TemplateBindings<'_>,
) -> Result<UntrustedRenderedText, CodegenError> {
    let mut env = target_template_environment();
    env.add_template("checked_dae_body", body_template)?;
    let template = env.get_template("checked_dae_body")?;
    let content = render_dae_context(&template, prepared, None, Some(artifact))?;
    Ok(UntrustedRenderedText::new(content))
}

/// Prepare the exact checked AST presentation capability.
#[must_use]
pub fn prepare_ast_rendering(ast_tree: &ast::ClassTree) -> PreparedAstRendering {
    PreparedAstRendering {
        value: Value::from_serialize(ast_tree),
    }
}

/// Prepare the exact checked Flat presentation capability.
pub fn prepare_flat_rendering(
    flat_model: &flat::Model,
) -> Result<PreparedFlatRendering, CodegenError> {
    require_materialized_flat_equation_view(flat_model)?;
    Ok(PreparedFlatRendering {
        value: Value::from_serialize(flat_model),
    })
}

/// Prepare the exact checked DAE presentation capability.
pub fn prepare_dae_rendering(dae_model: &dae::Dae) -> Result<PreparedDaeRendering, CodegenError> {
    Ok(PreparedDaeRendering {
        source_map: dae_model.source_map().clone(),
        value: dae_template_value(dae_model)?,
    })
}

/// Render a validated standalone Algorithm Code block as GALEC `.alg` source.
///
/// The template is the compiler's built-in Algorithm Code source template;
/// callers cannot supply arbitrary target text at this boundary.
pub fn render_checked_algorithm_block_source(
    block: &rumoca_ir_galec::package::CheckedAlgorithmBlock,
) -> Result<String, CodegenError> {
    let template = crate::templates::builtin_template_source("galec", "model.alg.jinja")
        .ok_or_else(|| CodegenError::template("built-in GALEC source template is missing"))?;
    let mut env = target_template_environment();
    env.add_template("inline", template)?;
    let tmpl = env.get_template("inline")?;
    let view = crate::views::algorithm_code::CheckedAlgorithmBlockView::new(block)
        .map_err(CodegenError::template)?;
    Ok(tmpl.render(minijinja::context! {
        algorithm_code => Value::from_serialize(view),
        ir_kind => "algorithm_code",
    })?)
}

/// Render a DAE using a template string.
///
/// The template receives the checked DAE semantic projection as `dae`.
///
/// # Example Template
///
/// ```jinja
/// {% for variable in dae.variables %}
/// {{ variable.role }} {{ variable.name | sanitize }}
/// {% endfor %}
/// ```
///
/// # Available Filters
///
/// - `sanitize` - Replace dots with underscores
/// - Standard minijinja filters (length, upper, lower, etc.)
#[cfg(test)]
fn render_template(dae: &dae::Dae, template: &str) -> Result<String, CodegenError> {
    render_inline_dae_template(dae, template, None)
}

/// Render a DAE using a template string, with an additional model name in context.
///
/// The template receives both `dae` and `model_name` as context variables.
/// This is useful for templates that name their checked DAE artifact.
#[cfg(test)]
fn render_template_with_name(
    dae: &dae::Dae,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    render_inline_dae_template(dae, template, Some(model_name))
}

#[cfg(test)]
#[cfg(test)]
fn render_inline_dae_template(
    dae: &dae::Dae,
    template: &str,
    model_name: Option<&str>,
) -> Result<String, CodegenError> {
    let mut env = target_template_environment();
    env.add_template("inline", template)?;
    let tmpl = env.get_template("inline")?;
    let prepared = PreparedDaeRendering {
        source_map: dae.source_map().clone(),
        value: dae_template_value(dae)?,
    };
    render_dae_context(&tmpl, &prepared, model_name, None)
}

/// Create the single production MiniJinja environment used to validate and
/// render target templates.
#[doc(hidden)]
pub fn target_template_environment() -> Environment<'static> {
    let mut env = Environment::new();
    // `debug()` receives MiniJinja's State and prints the complete context.
    // Target contexts contain least-authority per-file identity scalars, so no
    // state-introspecting default global is admitted to the production grammar.
    env.remove_global("debug");
    // Artifact bytes are target-owned. Preserve an explicit final newline so
    // strict text and compiler formats can state their EOF policy in templates.
    env.set_keep_trailing_newline(true);
    // Preserve template source on ordinary render failures in release builds.
    // MiniJinja clones this debug context only when constructing an error.
    env.set_debug(true);
    // Fail fast on missing fields/variables in templates.
    env.set_undefined_behavior(UndefinedBehavior::Strict);
    // Custom filters
    env.add_filter("sanitize", sanitize_filter);
    env.add_filter("modelica_string_escape", modelica_string_escape_filter);
    // eFMI manifest render env (contract §3b): autoescape is OFF, so every
    // text value is escaped explicitly and every raw f64 is rendered as a
    // valid xs:double lexical.
    env.add_filter("xml_escape", xml_escape_filter);
    env.add_filter("xs_double", xs_double_filter);
    env.add_function(
        "render_solve_row_output_wgsl",
        render_solve_row_output_wgsl_function,
    );
    env.add_function(
        "render_solve_native_family_wgsl",
        render_solve::render_solve_native_family_wgsl_function,
    );
    env.add_function(
        "render_solve_native_family_mlir",
        render_solve::render_solve_native_family_mlir_function,
    );
    env.add_function(
        "render_solve_native_family_output_index_wgsl",
        render_solve::render_solve_native_family_output_index_wgsl_function,
    );
    env.add_function(
        "render_solve_native_family_output_map_start",
        render_solve::render_solve_native_family_output_map_start_function,
    );
    env.add_function(
        "wgsl_kernel_schedule_json",
        render_wgsl_kernel_schedule_json_function,
    );
    env.add_function(
        "wgsl_kernel_workgroup_total",
        render_wgsl_kernel_workgroup_total_function,
    );
    env.add_function(
        "wgsl_native_family_inventory_json",
        render_wgsl_native_family_inventory_json_function,
    );
    env.add_function("render_matmul_mlir", render_matmul_mlir_function);
    env.add_function("render_linsolve_mlir", render_linsolve_mlir_function);
    env.add_function("fail", fail_function);
    dae_diagnostics::register(&mut env);

    env
}

/// Filter to sanitize variable names for simple emitted identifiers.
///
/// Replaces dots and other non-identifier characters with underscores.
fn sanitize_filter(value: Value) -> String {
    let s = value.to_string();
    let mut result = String::with_capacity(s.len());
    for ch in s.chars() {
        if ch.is_alphanumeric() || ch == '_' {
            result.push(ch);
        } else if ch == ']' {
            // Drop closing brackets to avoid a trailing underscore.
        } else {
            result.push('_');
        }
    }
    result
}

/// Delegate Modelica string-body escaping to the grammar-owned canonical codec.
///
/// This filter deliberately does not add quote tokens, inspect semantic IR, or
/// choose a dialect. The DAE target's lexical template owns the surrounding
/// Modelica syntax.
fn modelica_string_escape_filter(value: &str) -> String {
    ::rumoca_core::escape_modelica_string(value)
}

/// XML-escape a text value: the five predefined entities `& < > " '`.
///
/// The eFMI manifest templates render under an autoescape-OFF, strict
/// environment (contract §3b), so every interpolated text value is piped
/// through this filter: `{{ name | xml_escape }}`. Control-char rejection is
/// NOT this filter's job — that stays a validator on the context.
pub(crate) fn xml_escape_str(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&apos;"),
            _ => out.push(ch),
        }
    }
    out
}

fn xml_escape_filter(value: String) -> String {
    xml_escape_str(&value)
}

/// Render a finite `f64` as a portable real literal with explicit decimal
/// places and a signed lowercase exponent when scientific notation is needed.
///
/// The result is valid both as `xs:double` and as a GALEC Real token. Keeping
/// this as a documented template filter avoids target-language rendering in
/// semantic IR or lowering crates.
pub(crate) fn xs_double_str(value: f64) -> Result<String, minijinja::Error> {
    if !value.is_finite() {
        return Err(render_err("non-finite value has no portable real literal"));
    }
    let plain = format!("{value}");
    if !plain.contains('e') && plain.len() <= 21 {
        return Ok(ensure_real_decimal(plain));
    }
    let scientific = format!("{value:e}");
    let (mantissa, exponent) = scientific
        .split_once('e')
        .expect("LowerExp for f64 contains an exponent");
    let sign = if exponent.starts_with('-') { "" } else { "+" };
    Ok(format!(
        "{}e{sign}{exponent}",
        ensure_real_decimal(mantissa.to_owned())
    ))
}

fn ensure_real_decimal(mut text: String) -> String {
    if !text.contains('.') {
        text.push_str(".0");
    }
    text
}

fn xs_double_filter(value: f64) -> Result<String, minijinja::Error> {
    xs_double_str(value)
}

fn value_to_string(value: &Value) -> String {
    value
        .as_str()
        .map(str::to_owned)
        .unwrap_or_else(|| value.to_string().trim_matches('"').to_string())
}

/// Fail template rendering with an explicit message.
///
/// Templates use this to declare target-specific capability constraints
/// without pushing those policies into Rust-side backend branching.
fn fail_function(message: Value) -> RenderResult {
    Err(render_err(dae_diagnostics::template_message(message)))
}

pub use algorithm_code_renderer::{
    AlgorithmCodeTemplateRenderer, render_correlated_algorithm_code_file,
    render_packaged_algorithm_code_file,
};
pub use solve_renderer::{
    AdmittedFmiRenderingInput, PreparedFmiComponentRendering, PreparedSolveModelRendering,
    render_casadi_execution_model, render_mlir_execution_model,
};
