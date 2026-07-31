//! Template-driven code generation and shared render helpers.

use crate::errors::{CodegenError, render_err};
use indexmap::IndexMap;
use minijinja::{Environment, UndefinedBehavior, Value};
use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rumoca_ir_solve as solve;
use serde::Serialize;
use std::path::Path;

#[cfg(test)]
mod checked_dae_diagnostic_tests;
#[cfg(test)]
mod checked_dae_tests;
#[cfg(test)]
mod codegen_test_support;
mod dae_backend;
mod dae_diagnostics;
mod expr_config;
#[cfg(test)]
mod galec_golden_tests;
#[cfg(test)]
mod galec_manifest_template_tests;
mod render_expr;
mod render_solve;
mod render_solve_ops;
mod render_stmt;
#[cfg(test)]
mod scalar_plan_template_tests;
mod scalar_program_plan;
mod solve_lazy;
mod solve_renderer;
#[cfg(test)]
mod solve_sparse_output_tests;
#[cfg(test)]
mod solve_template_context_tests;
#[cfg(test)]
mod stencil_codegen_tests;
mod symbol_alloc;
#[cfg(test)]
mod wgsl_solve_tests;

pub(crate) use expr_config::{ExprConfig, IfStyle, get_str_attr};
use render_expr::{get_field, is_variant, render_expression};
use render_solve::{
    render_linsolve_mlir_function, render_matmul_mlir_function,
    render_solve_row_output_wgsl_function, render_solve_row_wgsl_function,
    render_wgsl_kernel_schedule_json_function, render_wgsl_kernel_workgroup_total_function,
    render_wgsl_native_family_inventory_json_function,
};
use render_stmt::{render_equation, render_flat_equation, render_statement, render_statements};
use symbol_alloc::{
    allocate_symbols_function, emitted_symbol, lookup_symbol_value, symbol_function,
    target_symbols_function,
};

/// Result type for internal render functions.
pub(crate) type RenderResult = Result<String, minijinja::Error>;

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

fn render_string_with_capacity(
    capacity: usize,
    context: &'static str,
) -> Result<String, minijinja::Error> {
    let mut value = String::new();
    reserve_render_string_capacity(&mut value, capacity, context)?;
    Ok(value)
}

fn reserve_render_string_capacity(
    value: &mut String,
    additional: usize,
    context: &'static str,
) -> Result<(), minijinja::Error> {
    value
        .try_reserve_exact(additional)
        .map_err(|_| render_err(format!("{context} exceeds host memory limits")))
}

pub(crate) fn join_usize_values(
    values: &[usize],
    separator: &str,
    context: &'static str,
) -> Result<String, minijinja::Error> {
    let mut rendered = render_vec_with_capacity(values.len(), context)?;
    for value in values {
        rendered.push(value.to_string());
    }
    Ok(rendered.join(separator))
}

/// Supported IR roots for template rendering.
#[derive(Debug, Clone, Copy)]
pub enum CodegenInput<'a> {
    Dae(&'a dae::Dae),
    Solve {
        problem: &'a solve::SolveProblem,
        artifacts: &'a solve::SolveArtifacts,
    },
    Flat(&'a flat::Model),
    Ast(&'a ast::ClassTree),
    AlgorithmCode(&'a rumoca_ir_galec::package::AlgorithmCodePackage),
}

fn dae_template_json_for_solve_context(dae: &dae::Dae) -> Result<serde_json::Value, CodegenError> {
    dae_template_json(dae)
}

pub fn dae_template_json(dae: &dae::Dae) -> Result<serde_json::Value, CodegenError> {
    dae_backend::project(dae).map_err(|error| {
        CodegenError::dae_preparation_failed(error.to_string(), Some(error.span()))
    })
}

fn dae_template_value(dae: &dae::Dae) -> Result<Value, CodegenError> {
    Ok(Value::from_serialize(dae_template_json(dae)?))
}

fn render_with_input_context(
    tmpl: &minijinja::Template<'_, '_>,
    input: CodegenInput<'_>,
    model_name: Option<&str>,
) -> Result<String, CodegenError> {
    let rendered = match (input, model_name) {
        (CodegenInput::Dae(dae_model), name) => render_dae_context(tmpl, dae_model, name)?,
        (CodegenInput::Solve { problem, artifacts }, name) => {
            render_solve_context(tmpl, problem, artifacts, name)?
        }
        (CodegenInput::Flat(flat_model), name) => render_flat_context(tmpl, flat_model, name)?,
        (CodegenInput::Ast(ast_tree), name) => render_ast_context(tmpl, ast_tree, name)?,
        (CodegenInput::AlgorithmCode(package), name) => {
            let view = crate::views::algorithm_code::AlgorithmCodeView::new(package);
            match name {
                Some(model_name) => tmpl.render(minijinja::context! {
                    algorithm_code => minijinja::Value::from_serialize(view),
                    ir_kind => "algorithm_code",
                    model_name,
                })?,
                None => tmpl.render(minijinja::context! {
                    algorithm_code => minijinja::Value::from_serialize(view),
                    ir_kind => "algorithm_code",
                })?,
            }
        }
    };
    Ok(rendered)
}

fn render_dae_context(
    tmpl: &minijinja::Template<'_, '_>,
    dae_model: &dae::Dae,
    model_name: Option<&str>,
) -> Result<String, CodegenError> {
    let dae_value = dae_template_value(dae_model)?;
    let rendered = match model_name {
        Some(name) => tmpl.render(minijinja::context! {
            dae => dae_value.clone(),
            ir => dae_value,
            ir_kind => "dae",
            model_name => name,
        }),
        None => tmpl.render(minijinja::context! {
            dae => dae_value.clone(),
            ir => dae_value,
            ir_kind => "dae",
        }),
    };
    rendered.map_err(|error| dae_diagnostics::render_error(dae_model, error))
}

fn render_solve_context(
    tmpl: &minijinja::Template<'_, '_>,
    solve_problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
    model_name: Option<&str>,
) -> Result<String, CodegenError> {
    Ok(tmpl.render(solve_render_context_value(
        solve_problem,
        artifacts,
        model_name,
    )?)?)
}

fn solve_template_blocks_value(
    solve_problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
) -> Result<Value, CodegenError> {
    Ok(minijinja::context! {
        continuous => minijinja::context! {
            implicit_rhs => solve_template_compute_block_json(&solve_problem.continuous.implicit_rhs)?,
            residual => solve_template_compute_block_json(&solve_problem.continuous.residual)?,
            derivative_rhs => solve_template_compute_block_json(&solve_problem.continuous.derivative_rhs)?,
        },
        artifacts => minijinja::context! {
            continuous => minijinja::context! {
                implicit_jacobian_v => solve_template_compute_block_json(&artifacts.continuous.implicit_jacobian_v)?,
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
    flat_model: &flat::Model,
    model_name: Option<&str>,
) -> RenderResult {
    let flat_value = Value::from_serialize(flat_model);
    match model_name {
        Some(name) => tmpl.render(minijinja::context! {
            flat => flat_value.clone(),
            ir => flat_value,
            ir_kind => "flat",
            model_name => name,
        }),
        None => tmpl.render(minijinja::context! {
            flat => flat_value.clone(),
            ir => flat_value,
            ir_kind => "flat",
        }),
    }
}

fn render_ast_context(
    tmpl: &minijinja::Template<'_, '_>,
    ast_tree: &ast::ClassTree,
    model_name: Option<&str>,
) -> RenderResult {
    let ast_value = Value::from_serialize(ast_tree);
    match model_name {
        Some(name) => tmpl.render(minijinja::context! {
            ast => ast_value.clone(),
            ir => ast_value,
            ir_kind => "ast",
            model_name => name,
        }),
        None => tmpl.render(minijinja::context! {
            ast => ast_value.clone(),
            ir => ast_value,
            ir_kind => "ast",
        }),
    }
}

/// Render any supported IR using a template string.
pub fn render_template_for_input(
    input: CodegenInput<'_>,
    template: &str,
) -> Result<String, CodegenError> {
    let mut env = create_environment();
    env.add_template("inline", template)?;
    let tmpl = env.get_template("inline")?;
    render_with_input_context(&tmpl, input, None)
}

/// Render any supported IR using a template string, with model name.
pub fn render_template_with_name_for_input(
    input: CodegenInput<'_>,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    let mut env = create_environment();
    env.add_template("inline", template)?;
    let tmpl = env.get_template("inline")?;
    render_with_input_context(&tmpl, input, Some(model_name))
}

/// Render a checked Algorithm Code package with immutable, caller-owned
/// artifact facts.
///
/// `artifact` is deliberately generic: the artifact layer may expose
/// identities, timestamps, and checksum edges, while this phase remains
/// stateless and unaware of any concrete package format.
pub fn render_algorithm_code_template_with_artifact<T: Serialize>(
    package: &rumoca_ir_galec::package::AlgorithmCodePackage,
    artifact: &T,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    let mut env = create_environment();
    env.add_template("inline", template)?;
    let tmpl = env.get_template("inline")?;
    let view = crate::views::algorithm_code::AlgorithmCodeView::new(package)
        .map_err(CodegenError::template)?;
    Ok(tmpl.render(minijinja::context! {
        algorithm_code => Value::from_serialize(view),
        artifact => Value::from_serialize(artifact),
        ir_kind => "algorithm_code",
        model_name,
    })?)
}

/// Render a validated standalone Algorithm Code block.
///
/// This is the `.alg` editor boundary: it deliberately exposes no manifest
/// or artifact metadata that cannot be derived from the parsed block.
pub fn render_checked_algorithm_block_template_with_artifact<T: Serialize>(
    block: &rumoca_ir_galec::package::CheckedAlgorithmBlock,
    artifact: &T,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    let mut env = create_environment();
    env.add_template("inline", template)?;
    let tmpl = env.get_template("inline")?;
    let view = crate::views::algorithm_code::CheckedAlgorithmBlockView::new(block)
        .map_err(CodegenError::template)?;
    Ok(tmpl.render(minijinja::context! {
        algorithm_code => Value::from_serialize(view),
        artifact => Value::from_serialize(artifact),
        ir_kind => "algorithm_code",
        model_name,
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
/// # Built-in Functions
///
/// - `render_expr(expr, config)` - Render expression with operator config
///
/// # Available Filters
///
/// - `sanitize` - Replace dots with underscores
/// - Standard minijinja filters (length, upper, lower, etc.)
pub fn render_template(dae: &dae::Dae, template: &str) -> Result<String, CodegenError> {
    render_template_for_input(CodegenInput::Dae(dae), template)
}

/// Render a DAE using a template string, with an additional model name in context.
///
/// The template receives both `dae` and `model_name` as context variables.
/// This is useful for templates that need the model name (e.g., flat Modelica output).
pub fn render_template_with_name(
    dae: &dae::Dae,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    render_template_with_name_for_input(CodegenInput::Dae(dae), template, model_name)
}

/// Render a DAE using a template file.
///
/// This is the recommended approach for customizable templates.
///
/// # Example
///
/// ```ignore
/// let code = render_template_file(&dae, "templates/casadi.py.jinja")?;
/// ```
pub fn render_template_file(
    dae: &dae::Dae,
    path: impl AsRef<Path>,
) -> Result<String, CodegenError> {
    let path_ref = path.as_ref();
    let template = std::fs::read_to_string(path_ref)
        .map_err(|e| CodegenError::template(format!("Failed to read template: {e}")))?;

    let mut env = create_environment();
    env.add_template("file", &template)?;

    let tmpl = env.get_template("file")?;
    render_with_input_context(&tmpl, CodegenInput::Dae(dae), None)
}

/// Render a Model using a template string, with an additional model name in context.
///
/// The template receives `flat` (the Model) and `model_name` as context variables.
/// This is used for rendering flat Modelica output for OMC comparison.
pub fn render_flat_template_with_name(
    flat: &flat::Model,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    render_template_with_name_for_input(CodegenInput::Flat(flat), template, model_name)
}

/// Reusable solve-template renderer.
///
/// Building the template context serializes the full `SolveProblem`; doing
/// that once and rendering many templates against it is dramatically
/// cheaper than calling `render_solve_template_with_name` per template on
/// large models.
/// Render a solver IR problem using a template string and model name.
pub fn render_solve_template_with_name(
    solve: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    render_template_with_name_for_input(
        CodegenInput::Solve {
            problem: solve,
            artifacts,
        },
        template,
        model_name,
    )
}

/// Render an AST class tree using a template string.
///
/// The template receives the AST structure as `ast`.
pub fn render_ast_template(ast: &ast::ClassTree, template: &str) -> Result<String, CodegenError> {
    render_template_for_input(CodegenInput::Ast(ast), template)
}

/// Render an AST class tree using a template string and model name.
///
/// The template receives both `ast` and `model_name`.
pub fn render_ast_template_with_name(
    ast: &ast::ClassTree,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    render_template_with_name_for_input(CodegenInput::Ast(ast), template, model_name)
}

/// Create a minijinja environment with all custom filters and functions.
fn create_environment() -> Environment<'static> {
    let mut env = Environment::new();
    // Preserve template source on ordinary render failures in release builds.
    // MiniJinja clones this debug context only when constructing an error.
    env.set_debug(true);
    // Fail fast on missing fields/variables in templates.
    env.set_undefined_behavior(UndefinedBehavior::Strict);
    env.add_template(
        "algorithm-code-source.jinja",
        include_str!("../templates/galec/model.alg.jinja"),
    )
    .expect("built-in Algorithm Code source template must parse");
    env.add_template(
        "algorithm-code-manifest.jinja",
        include_str!("../templates/galec/manifest.xml.jinja"),
    )
    .expect("built-in Algorithm Code manifest template must parse");
    env.add_template(
        "galec-model.c.jinja",
        include_str!("../templates/embedded-c-galec/model.c.jinja"),
    )
    .expect("built-in GALEC-derived C source template must parse");
    env.add_template(
        "galec-model.h.jinja",
        include_str!("../templates/embedded-c-galec/model.h.jinja"),
    )
    .expect("built-in GALEC-derived C header template must parse");

    // Custom filters
    env.add_filter("sanitize", sanitize_filter);
    env.add_filter("product", product_filter);
    env.add_filter("last_segment", last_segment_filter);
    // eFMI manifest render env (contract §3b): autoescape is OFF, so every
    // text value is escaped explicitly and every raw f64 is rendered as a
    // valid xs:double lexical.
    env.add_filter("xml_escape", xml_escape_filter);
    env.add_filter("xs_double", xs_double_filter);

    // Helpers for target-local emitted symbols. Flattening supplies globally
    // unique Modelica names; templates provide target keyword/generated-alias policy.
    env.add_function("allocate_symbols", allocate_symbols_function);
    env.add_function("target_symbols", target_symbols_function);
    env.add_function("symbol", symbol_function);
    env.add_function("source_ref", source_ref_function);

    // Custom functions for expression rendering
    env.add_function("render_expr", render_expr_function);
    env.add_function("render_event_indicator", render_event_indicator_function);
    env.add_function("render_solve_row_wgsl", render_solve_row_wgsl_function);
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
    env.add_function("render_equation", render_equation_function);

    // Custom functions for statement rendering (MLS §12: function bodies)
    env.add_function("render_statement", render_statement_function);
    env.add_function("render_statements", render_statements_function);

    // Custom function for flat equation rendering (Model residual equations)
    env.add_function("render_flat_equation", render_flat_equation_function);

    // Custom function for detecting self-referential (builtin alias) functions
    env.add_function("is_self_call", is_self_call_function);
    env.add_function("fail", fail_function);
    dae_diagnostics::register(&mut env);

    env
}

/// Sanitize a name for use as a simple emitted identifier.
///
/// Replaces all non-alphanumeric/underscore characters with `_`. Target
/// reserved words are handled by `allocate_symbols` with a template-supplied
/// policy, not by this lossy fallback.
pub(crate) fn sanitize_name(name: &str) -> String {
    let mut result = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_alphanumeric() || ch == '_' {
            result.push(ch);
        } else if ch == ']' {
            // Drop closing brackets to avoid trailing underscores.
            // After for-loop unrolling, VarRef names like "Kp[1]" get sanitized
            // here; replacing ']' with '_' would produce "Kp_1_" instead of "Kp_1".
        } else {
            result.push('_');
        }
    }
    result
}

/// Plain-name passthrough for renderers that opt out of symbol allocation.
pub(crate) fn escape_reserved_keyword(name: &str) -> String {
    name.to_string()
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
            // Drop closing brackets (see sanitize_name for rationale)
        } else {
            result.push('_');
        }
    }
    result
}

/// Filter to extract the last dot-separated segment of a name.
///
/// Used in templates: `{{ "Modelica.Math.sin" | last_segment }}` -> `"sin"`
fn last_segment_filter(value: Value) -> String {
    let s = value.to_string().replace('"', "");
    rumoca_core::top_level_last_segment(&s).to_string()
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

/// Filter to compute the product of all elements in a sequence.
///
/// Used by MX template: `{{ var.dims | product }}` -> total scalar size.
fn product_filter(value: Value) -> Result<Value, minijinja::Error> {
    let Some(len) = value.len() else {
        return Ok(Value::from(1));
    };
    let mut result: i64 = 1;
    for i in 0..len {
        if let Ok(item) = value.get_item(&Value::from(i)) {
            let item = item.as_i64().unwrap_or(1);
            result = result
                .checked_mul(item)
                .ok_or_else(|| render_err("product filter overflows Modelica integer range"))?;
        }
    }
    Ok(Value::from(result))
}

fn value_to_string(value: &Value) -> String {
    value
        .as_str()
        .map(str::to_owned)
        .unwrap_or_else(|| value.to_string().trim_matches('"').to_string())
}

fn dims_from_value(value: &Value) -> Result<Vec<usize>, minijinja::Error> {
    let Some(len) = value.len() else {
        return Ok(Vec::new());
    };
    let mut dims = render_vec_with_capacity(len, "render dimension count")?;
    for i in 0..len {
        if let Ok(item) = value.get_item(&Value::from(i))
            && let Some(dim) = item.as_i64()
            && dim > 0
        {
            dims.push(
                usize::try_from(dim)
                    .map_err(|_| render_err(format!("dimension {dim} exceeds host index range")))?,
            );
        }
    }
    Ok(dims)
}

fn value_list_strings(value: &Value) -> Result<Vec<String>, minijinja::Error> {
    let Some(len) = value.len() else {
        return Ok(Vec::new());
    };
    let mut out = render_vec_with_capacity(len, "template value string count")?;
    for i in 0..len {
        if let Ok(item) = value.get_item(&Value::from(i)) {
            out.push(value_to_string(&item));
        }
    }
    Ok(out)
}

fn value_symbol_aliases(value: &Value) -> Result<Vec<(String, String)>, minijinja::Error> {
    let Some(len) = value.len() else {
        return Ok(Vec::new());
    };
    let mut out = render_vec_with_capacity(len, "symbol alias count")?;
    for i in 0..len {
        let item = value
            .get_item(&Value::from(i))
            .map_err(|err| render_err(format!("symbol alias entry {i} is not readable: {err}")))?;
        let alias = get_field(&item, "alias")
            .map(|value| value_to_string(&value))
            .map_err(|err| render_err(format!("symbol alias entry {i} missing alias: {err}")))?;
        let target = get_field(&item, "target")
            .map(|value| value_to_string(&value))
            .map_err(|err| render_err(format!("symbol alias entry {i} missing target: {err}")))?;
        if alias.is_empty() || target.is_empty() {
            return Err(render_err(format!(
                "symbol alias entry {i} must have non-empty alias and target"
            )));
        }
        out.push((alias, target));
    }
    Ok(out)
}

fn checked_subscripts_for_flat_index(
    dims: &[usize],
    flat_index: usize,
) -> Result<Vec<usize>, minijinja::Error> {
    if dims.is_empty() {
        return Ok(Vec::new());
    }
    let mut remaining = flat_index
        .checked_sub(1)
        .ok_or_else(|| render_err("source_ref flat index must be one-based"))?;
    let mut subscripts =
        render_vec_with_capacity(dims.len(), "checked source_ref subscript count")?;
    subscripts.extend(std::iter::repeat_n(1, dims.len()));
    for dim_idx in (0..dims.len()).rev() {
        let dim = dims[dim_idx].max(1);
        subscripts[dim_idx] = (remaining % dim) + 1;
        remaining /= dim;
    }
    if remaining != 0 {
        return Err(render_err(format!(
            "source_ref flat index {flat_index} exceeds dimensions {dims:?}"
        )));
    }
    Ok(subscripts)
}

fn checked_source_subscript_suffix(
    dims: &[usize],
    flat_index: usize,
) -> Result<String, minijinja::Error> {
    let subscripts = checked_subscripts_for_flat_index(dims, flat_index)?;
    if subscripts.is_empty() {
        Ok(flat_index.to_string())
    } else {
        join_usize_values(&subscripts, ",", "source_ref rendered subscript count")
    }
}

/// Return the source-reference key for a scalarized array element.
///
/// Examples:
/// - `source_ref("x", [4], 3)` -> `x[3]`
/// - `source_ref("leg.f", [4,3], 4)` -> `leg.f[2,1]`
fn source_ref_function(name: Value, dims: Value, flat_index: Value) -> RenderResult {
    let name = value_to_string(&name);
    let dims = dims_from_value(&dims)?;
    if dims.is_empty() {
        return Ok(name);
    }
    let index = flat_index.as_usize().ok_or_else(|| {
        render_err(format!(
            "source_ref flat index `{flat_index}` is not numeric"
        ))
    })?;
    Ok(format!(
        "{}[{}]",
        name,
        checked_source_subscript_suffix(&dims, index)?
    ))
}

/// Fail template rendering with an explicit message.
///
/// Templates use this to declare target-specific capability constraints
/// without pushing those policies into Rust-side backend branching.
fn fail_function(message: Value) -> RenderResult {
    Err(render_err(dae_diagnostics::template_message(message)))
}

/// Detect whether a function is a trivial self-call (builtin alias).
///
/// Returns true if the function body is a single assignment whose RHS is a
/// direct `FunctionCall` back to the function itself (e.g. `y := sin(x)`).
///
/// Usage in templates:
/// ```jinja
/// {% if is_self_call(func_name, func) %}...{% endif %}
/// ```
fn is_self_call_function(func_name: Value, func: Value) -> Result<bool, minijinja::Error> {
    use render_expr::get_field;
    let name_str = func_name.to_string().replace('"', "");
    let Ok(body) = get_field(&func, "body") else {
        return Ok(false);
    };
    let Some(len) = body.len() else {
        return Ok(false);
    };
    // Only match trivial bodies: exactly one assignment whose RHS is a direct
    // FunctionCall to self (e.g. `result := sin(u)`). This avoids matching
    // complex functions that happen to contain a nested self-reference.
    if len != 1 {
        return Ok(false);
    }
    let Ok(stmt) = body.get_item(&Value::from(0)) else {
        return Ok(false);
    };
    let Ok(assign) = get_field(&stmt, "Assignment") else {
        return Ok(false);
    };
    let Ok(value) = get_field(&assign, "value") else {
        return Ok(false);
    };
    // Check if value is a direct FunctionCall to self
    if let Ok(func_call) = get_field(&value, "FunctionCall")
        && let Ok(name) = get_field(&func_call, "name")
    {
        // A serialized reference is one record whose `name` is its spelling;
        // reading it any other way recovers the record's debug text instead.
        return Ok(render_expr::render_serialized_name(&name) == name_str);
    }
    Ok(false)
}

/// Built-in expression renderer function.
///
/// Usage in templates:
/// ```jinja
/// {{ render_expr(expr, config) }}
/// ```
///
/// The config object can contain:
/// - `prefix` - Prefix for function calls (e.g., "ca." for CasADi, "np." for numpy)
/// - `power` - Power operator syntax (e.g., "**" for Python, "^" for Julia)
/// - `and_op` - Logical AND (e.g., "and", "&&")
/// - `or_op` - Logical OR (e.g., "or", "||")
/// - `not_op` - Logical NOT (e.g., "not ", "!")
/// - `true_val` - True literal (e.g., "True", "true")
/// - `false_val` - False literal (e.g., "False", "false")
/// - `array_start` - Array literal start (e.g., "[", "{")
/// - `array_end` - Array literal end (e.g., "]", "}")
/// - `if_else` - If-else style: "python" (if_else(c,t,e)), "ternary" (c ? t : e), "julia" (c ? t : e)
/// - `mul_elem_fn` - Optional function for element-wise multiply (e.g., "ca.times")
fn render_expr_function(expr: Value, config: Value) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    render_expression(&expr, &cfg)
}

/// Render a relation as a numeric root function for FMI event indicators.
///
/// DAE `relation` entries are boolean expressions such as `a < b`, but FMI
/// event indicators are real-valued zero-crossing functions. For relational
/// binary operators, emit the residual `a - b`; for non-relational expressions
/// fall back to the generic renderer.
fn render_event_indicator_function(expr: Value, config: Value) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    render_event_indicator(&expr, &cfg)
}

fn render_event_indicator(expr: &Value, cfg: &ExprConfig) -> RenderResult {
    let binary = get_field(expr, "Binary").unwrap_or_else(|_| expr.clone());
    let Ok(op) = get_field(&binary, "op") else {
        return render_expression(expr, cfg);
    };
    if !is_relation_operator(&op) {
        return render_expression(expr, cfg);
    }

    let lhs = get_field(&binary, "lhs")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Relation expression missing 'lhs' field"))?;
    let rhs = get_field(&binary, "rhs")
        .and_then(|v| render_expression(&v, cfg))
        .map_err(|_| render_err("Relation expression missing 'rhs' field"))?;
    Ok(format!("(({lhs}) - ({rhs}))"))
}

fn is_relation_operator(op: &Value) -> bool {
    is_variant(op, "Lt")
        || is_variant(op, "Le")
        || is_variant(op, "Gt")
        || is_variant(op, "Ge")
        || is_variant(op, "Eq")
        || is_variant(op, "Neq")
}

/// Render an equation in `lhs = rhs` form.
///
/// For explicit equations (lhs is set), renders `lhs = rhs`.
/// For residual equations (lhs is None), decomposes top-level subtraction
/// into `lhs_expr = rhs_expr`. Falls back to `0 = expr` if no subtraction.
///
/// Usage in templates:
/// ```jinja
/// {{ render_equation(eq, config) }}
/// ```
fn render_equation_function(eq: Value, config: Value) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    render_equation(&eq, &cfg)
}

/// Render a Equation (residual form) to `lhs = rhs`.
///
/// Equation has a `residual` field (not `rhs`/`lhs`).
/// Decomposes top-level `Binary::Sub` into `lhs = rhs` form.
/// Falls back to `0 = expr` if no subtraction.
///
/// Usage in templates:
/// ```jinja
/// {{ render_flat_equation(eq, config) }}
/// ```
fn render_flat_equation_function(eq: Value, config: Value) -> RenderResult {
    let cfg = ExprConfig::from_value(&config);
    render_flat_equation(&eq, &cfg)
}

/// Render a single statement (MLS §12: function body statements).
///
/// Usage in templates:
/// ```jinja
/// {% for stmt in func.body %}
/// {{ render_statement(stmt, cfg, indent) }}
/// {% endfor %}
/// ```
fn render_statement_function(stmt: Value, config: Value, indent: Value) -> RenderResult {
    let mut cfg = ExprConfig::from_value(&config);
    // Function bodies use local arrays / lists, so array subscripts must
    // always use bracket notation — see render_statements_function.
    cfg.subscript_underscore = false;
    let indent_str = indent.as_str().unwrap_or("    ");
    render_statement(&stmt, &cfg, indent_str)
}

/// Render a list of statements (MLS §12: function body).
///
/// Usage in templates:
/// ```jinja
/// {{ render_statements(func.body, cfg, "    ") }}
/// ```
fn render_statements_function(stmts: Value, config: Value, indent: Value) -> RenderResult {
    let mut cfg = ExprConfig::from_value(&config);
    // Function bodies use local arrays/lists, so array subscripts must always
    // use bracket notation (y[i]) rather than top-level scalar aliases (y_i).
    cfg.subscript_underscore = false;
    let indent_str = indent.as_str().unwrap_or("    ");
    render_statements(&stmts, &cfg, indent_str)
}

pub use solve_renderer::SolveTemplateRenderer;
use solve_renderer::solve_render_context_value;
