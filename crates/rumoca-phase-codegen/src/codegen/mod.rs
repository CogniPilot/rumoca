//! Code generation implementation.
//!
//! This module provides a simple template rendering function. The DAE is
//! serialized and passed directly to minijinja templates, which can then
//! walk the expression tree and generate code as needed.
//!
//! For common cases, templates can use the built-in `render_expr` function
//! which handles the recursive tree walking with configurable operator syntax.

use crate::errors::{CodegenError, render_err};
use minijinja::{Environment, UndefinedBehavior, Value};
use rumoca_core::{Expression, Reference, Span};
use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use rumoca_ir_solve as solve;
use std::collections::{HashMap, HashSet};
use std::path::Path;

mod render_c;
mod render_expr;
mod render_solve;
mod render_stmt;

use render_expr::{get_field, is_variant, render_expression};
use render_solve::{
    render_linsolve_mlir_function, render_matmul_c_function, render_matmul_mlir_function,
    render_optional_solve_slot_assign_c_function, render_solve_pre_param_binding_c_function,
    render_solve_row_c_function, render_solve_row_rust_function,
    render_solve_slot_assign_c_function,
};
use render_stmt::{render_equation, render_flat_equation, render_statement, render_statements};

/// Result type for internal render functions.
pub(crate) type RenderResult = Result<String, minijinja::Error>;

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
}

pub fn dae_template_json(dae: &dae::Dae) -> Result<serde_json::Value, CodegenError> {
    let mut value = serde_json::to_value(dae).map_err(|e| CodegenError::SerializationFailed {
        message: format!("DAE: {e}"),
    })?;
    let object = value
        .as_object_mut()
        .ok_or_else(|| CodegenError::SerializationFailed {
            message: "DAE did not serialize to a JSON object".to_string(),
        })?;
    let enum_type_names = enum_type_names_from_ordinals(dae);
    let symbol_refs = source_refs_from_dae(dae, &enum_type_names);
    let condition_aliases =
        condition_aliases_from_dae(dae).map_err(|e| CodegenError::SerializationFailed {
            message: format!("condition_aliases: {e}"),
        })?;
    object.insert(
        "model_description".to_string(),
        serde_json::to_value(&dae.metadata.model_description).map_err(|e| {
            CodegenError::SerializationFailed {
                message: format!("model_description: {e}"),
            }
        })?,
    );
    object.insert(
        "enum_type_names".to_string(),
        serde_json::to_value(enum_type_names).map_err(|e| CodegenError::SerializationFailed {
            message: format!("enum_type_names: {e}"),
        })?,
    );
    object.insert(
        "symbol_refs".to_string(),
        serde_json::to_value(symbol_refs).map_err(|e| CodegenError::SerializationFailed {
            message: format!("symbol_refs: {e}"),
        })?,
    );
    object.insert(
        "condition_aliases".to_string(),
        serde_json::Value::Array(condition_aliases),
    );
    Ok(value)
}

/// Reusable Minijinja context for rendering multiple templates from one DAE.
pub struct DaeTemplateContext {
    dae_value: Value,
}

impl DaeTemplateContext {
    pub fn from_dae_json(dae_json: &serde_json::Value) -> Self {
        Self {
            dae_value: Value::from_serialize(dae_json),
        }
    }

    pub fn from_dae(dae: &dae::Dae) -> Result<Self, CodegenError> {
        Ok(Self::from_dae_json(&dae_template_json(dae)?))
    }

    pub fn render(&self, template: &str) -> Result<String, CodegenError> {
        let mut env = create_environment();
        env.add_template("inline", template)?;
        let tmpl = env.get_template("inline")?;
        let rendered = tmpl.render(minijinja::context! {
            dae => self.dae_value.clone(),
            ir => self.dae_value.clone(),
            ir_kind => "dae",
        })?;
        Ok(rendered)
    }

    pub fn render_with_name(
        &self,
        template: &str,
        model_name: &str,
    ) -> Result<String, CodegenError> {
        let mut env = create_environment();
        env.add_template("inline", template)?;
        let tmpl = env.get_template("inline")?;
        let rendered = tmpl.render(minijinja::context! {
            dae => self.dae_value.clone(),
            ir => self.dae_value.clone(),
            ir_kind => "dae",
            model_name => model_name,
        })?;
        Ok(rendered)
    }
}

fn condition_aliases_from_dae(dae: &dae::Dae) -> Result<Vec<serde_json::Value>, serde_json::Error> {
    dae.conditions
        .equations
        .iter()
        .filter_map(|eq| eq.lhs.as_ref().map(|lhs| (lhs, &eq.rhs)))
        .map(|(lhs, relation)| {
            Ok(serde_json::json!({
                "condition": Expression::VarRef {
                    name: Reference::from_var_name(lhs.clone()),
                    subscripts: Vec::new(),
                    span: Span::DUMMY,
                },
                "relation": relation,
            }))
        })
        .collect()
}

/// Extract unique enum type names from enum literal ordinals.
/// E.g., from `Modelica.Blocks.Types.Smoothness.LinearSegments` to
/// `Modelica.Blocks.Types.Smoothness`.
fn enum_type_names_from_ordinals(dae: &dae::Dae) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for name in dae.symbols.enum_literal_ordinals.keys() {
        if let Some((type_name, _literal_name)) = rumoca_core::split_last_top_level(name)
            && seen.insert(type_name.to_string())
        {
            result.push(type_name.to_string());
        }
    }
    result
}

fn source_refs_from_dae(dae: &dae::Dae, enum_type_names: &[String]) -> Vec<String> {
    let mut refs = HashSet::new();

    for vars in [
        &dae.variables.states,
        &dae.variables.algebraics,
        &dae.variables.inputs,
        &dae.variables.outputs,
        &dae.variables.parameters,
        &dae.variables.constants,
        &dae.variables.discrete_reals,
        &dae.variables.discrete_valued,
    ] {
        for (name, var) in vars {
            add_source_refs_for_var(name.as_str(), &var.dims, &mut refs);
        }
    }

    for (func_name, func) in &dae.symbols.functions {
        refs.insert(func_name.as_str().to_string());
        add_function_output_projection_refs(func_name.as_str(), func, &mut refs);
    }
    for func in dae.symbols.functions.values() {
        for var in func
            .inputs
            .iter()
            .chain(func.outputs.iter())
            .chain(func.locals.iter())
        {
            add_source_refs_for_var(var.name.as_str(), &var.dims, &mut refs);
        }
    }
    for name in dae.symbols.enum_literal_ordinals.keys() {
        refs.insert(name.clone());
    }
    for name in enum_type_names {
        refs.insert(name.clone());
    }

    let mut refs = refs.into_iter().collect::<Vec<_>>();
    refs.sort_by(|a, b| {
        symbol_ref_priority(a)
            .cmp(&symbol_ref_priority(b))
            .then_with(|| a.cmp(b))
    });
    refs
}

fn add_function_output_projection_refs(
    func_name: &str,
    func: &rumoca_core::Function,
    refs: &mut HashSet<String>,
) {
    for output in &func.outputs {
        let dims: Vec<usize> = output
            .dims
            .iter()
            .filter_map(|dim| (*dim > 0).then_some(*dim as usize))
            .collect();
        let count = if dims.is_empty() {
            1
        } else {
            dims.iter().product::<usize>()
        };
        for element_idx in 1..=count {
            let selector = if count == 1 {
                output.name.as_str().to_string()
            } else {
                format!("{}[{}]", output.name.as_str(), element_idx)
            };
            refs.insert(format!("{func_name}.{selector}"));
        }
    }
}

fn add_source_refs_for_var(name: &str, dims: &[i64], refs: &mut HashSet<String>) {
    refs.insert(name.to_string());

    let dims: Vec<usize> = dims
        .iter()
        .filter_map(|dim| (*dim > 0).then_some(*dim as usize))
        .collect();
    if dims.is_empty() {
        return;
    }

    let total = dims.iter().product::<usize>();
    for flat_index in 1..=total {
        refs.insert(format!(
            "{}[{}]",
            name,
            source_subscript_suffix(&dims, flat_index)
        ));
    }
}

fn symbol_ref_priority(reference: &str) -> (usize, usize) {
    let (base, _) = split_modelica_subscript(reference);
    let depth = split_modelica_path(base).len();
    let indexed = usize::from(reference.contains('['));
    (depth, indexed)
}

#[derive(Clone)]
struct SymbolPolicy {
    reserved: HashSet<String>,
    generated_prefixes: Vec<String>,
    separator: String,
}

impl SymbolPolicy {
    fn language_neutral() -> Self {
        Self {
            reserved: HashSet::new(),
            generated_prefixes: Vec::new(),
            separator: "_".to_string(),
        }
    }

    fn from_value(value: &Value) -> Self {
        let mut policy = Self::language_neutral();

        if let Some(separator) = get_str_attr(value, "separator")
            && !separator.is_empty()
        {
            policy.separator = separator;
        }
        if let Ok(reserved) = value.get_attr("reserved") {
            for item in value_list_strings(&reserved) {
                policy.reserved.insert(item);
            }
        }
        if let Ok(prefixes) = value.get_attr("generated_prefixes") {
            policy.generated_prefixes = value_list_strings(&prefixes);
        }

        policy
    }
}

struct SymbolAllocator {
    used: HashSet<String>,
    policy: SymbolPolicy,
}

impl SymbolAllocator {
    fn new(policy: SymbolPolicy) -> Self {
        let used = policy.reserved.clone();
        Self { used, policy }
    }

    fn allocate(
        &mut self,
        candidates: &[String],
        candidate_counts: &HashMap<String, usize>,
    ) -> String {
        for candidate in candidates {
            if candidate_counts.get(candidate).copied().unwrap_or(0) <= 1 && self.try_use(candidate)
            {
                return candidate.clone();
            }
        }

        for candidate in candidates {
            if self.try_use(candidate) {
                return candidate.clone();
            }
        }

        let base = candidates
            .last()
            .cloned()
            .unwrap_or_else(|| "value".to_string());
        for idx in 2.. {
            let candidate = format!("{base}_{idx}");
            if self.try_use(&candidate) {
                return candidate;
            }
        }
        unreachable!("exhausted usize suffix range while allocating a unique codegen name")
    }

    fn try_use(&mut self, candidate: &str) -> bool {
        if candidate.is_empty()
            || self.used.contains(candidate)
            || self.policy.reserved.contains(candidate)
        {
            return false;
        }
        if self
            .policy
            .generated_prefixes
            .iter()
            .any(|prefix| self.used.contains(&format!("{prefix}{candidate}")))
        {
            return false;
        }

        self.used.insert(candidate.to_string());
        for prefix in &self.policy.generated_prefixes {
            self.used.insert(format!("{prefix}{candidate}"));
        }
        true
    }
}

fn allocate_symbols_function(symbol_refs: Value, policy: Value) -> Value {
    let policy = SymbolPolicy::from_value(&policy);
    let references = value_list_strings(&symbol_refs);
    let symbols = allocate_symbols_for_refs(references, policy);
    Value::from_serialize(symbols)
}

fn target_symbols_function(symbol_refs: Value, policy: Value) -> Value {
    let policy = SymbolPolicy::from_value(&policy);
    let references = value_list_strings(&symbol_refs);
    let mut requests = references
        .into_iter()
        .map(|reference| {
            let candidates = symbol_candidates(&reference, &policy);
            (reference, candidates)
        })
        .collect::<Vec<_>>();

    requests.sort_by(|(a_ref, a_candidates), (b_ref, b_candidates)| {
        symbol_ref_priority(a_ref)
            .cmp(&symbol_ref_priority(b_ref))
            .then_with(|| {
                a_candidates
                    .first()
                    .map(String::as_str)
                    .unwrap_or("")
                    .cmp(b_candidates.first().map(String::as_str).unwrap_or(""))
            })
            .then_with(|| a_ref.cmp(b_ref))
    });

    let mut candidate_counts = HashMap::<String, usize>::new();
    for (_, candidates) in &requests {
        let mut seen = HashSet::new();
        for candidate in candidates {
            if seen.insert(candidate) {
                *candidate_counts.entry(candidate.clone()).or_insert(0) += 1;
            }
        }
    }

    let mut allocator = SymbolAllocator::new(policy);
    let mut out = HashMap::new();
    for (reference, candidates) in requests {
        let symbol = allocator.allocate(&candidates, &candidate_counts);
        out.insert(reference, symbol);
    }
    Value::from_serialize(out)
}

fn allocate_symbols_for_refs(
    mut references: Vec<String>,
    policy: SymbolPolicy,
) -> HashMap<String, String> {
    references.sort_by(|a, b| {
        symbol_ref_priority(a)
            .cmp(&symbol_ref_priority(b))
            .then_with(|| a.cmp(b))
    });
    references.dedup();

    let requests = references
        .into_iter()
        .map(|reference| {
            let candidates = symbol_candidates(&reference, &policy);
            (reference, candidates)
        })
        .collect::<Vec<_>>();

    let mut candidate_counts = HashMap::<String, usize>::new();
    for (_, candidates) in &requests {
        let mut seen = HashSet::new();
        for candidate in candidates {
            if seen.insert(candidate) {
                *candidate_counts.entry(candidate.clone()).or_insert(0) += 1;
            }
        }
    }

    let mut allocator = SymbolAllocator::new(policy);
    let mut out = HashMap::new();
    for (reference, candidates) in requests {
        let symbol = allocator.allocate(&candidates, &candidate_counts);
        out.insert(reference, symbol);
    }
    out
}

fn symbol_candidates(modelica_ref: &str, policy: &SymbolPolicy) -> Vec<String> {
    let (base_ref, subscript) = split_modelica_subscript(modelica_ref);
    let suffix = subscript.map(|s| scalarized_subscript_suffix(s, &policy.separator));
    let segments = split_modelica_path(base_ref);
    let mut candidates = Vec::new();
    for start in (0..segments.len()).rev() {
        let base = segments[start..]
            .iter()
            .map(|segment| readable_identifier_segment(segment))
            .filter(|segment| !segment.is_empty())
            .collect::<Vec<_>>()
            .join(&policy.separator);
        if base.is_empty() {
            continue;
        }
        candidates.push(with_optional_suffix(
            &base,
            suffix.as_deref(),
            &policy.separator,
        ));
    }

    if candidates.is_empty() {
        candidates.push(with_optional_suffix(
            "value",
            suffix.as_deref(),
            &policy.separator,
        ));
    }
    candidates.dedup();
    candidates
}

fn split_modelica_subscript(reference: &str) -> (&str, Option<&str>) {
    if let Some((base, subscript)) = rumoca_core::split_trailing_subscript_suffix(reference) {
        return (base, Some(subscript));
    }
    (reference, None)
}

fn split_modelica_path(name: &str) -> Vec<&str> {
    let mut segments = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (idx, ch) in name.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => depth = depth.saturating_sub(1),
            '.' if depth == 0 => {
                segments.push(&name[start..idx]);
                start = idx + 1;
            }
            _ => {}
        }
    }
    segments.push(&name[start..]);
    segments
}

fn readable_identifier_segment(segment: &str) -> String {
    let mut out = String::with_capacity(segment.len());
    let mut last_was_underscore = false;
    for ch in segment.chars() {
        let valid = ch.is_ascii_alphanumeric() || ch == '_';
        if valid {
            if out.is_empty() && ch.is_ascii_digit() {
                out.push('_');
            }
            out.push(ch);
            last_was_underscore = ch == '_';
        } else if !last_was_underscore {
            out.push('_');
            last_was_underscore = true;
        }
    }
    while out.ends_with('_') {
        out.pop();
    }
    if out.is_empty() {
        "value".to_string()
    } else {
        out
    }
}

fn scalarized_subscript_suffix(subscript: &str, separator: &str) -> String {
    subscript
        .split(',')
        .map(str::trim)
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join(separator)
}

fn with_optional_suffix(base: &str, suffix: Option<&str>, separator: &str) -> String {
    match suffix {
        Some(suffix) if !suffix.is_empty() => format!("{base}{separator}{suffix}"),
        _ => base.to_string(),
    }
}

fn symbol_function(symbols: Value, name: Value) -> String {
    let name = value_to_string(&name);
    lookup_symbol_value(Some(&symbols), &name).unwrap_or_else(|| {
        symbol_candidates(&name, &SymbolPolicy::language_neutral())
            .into_iter()
            .next()
            .unwrap_or_else(|| "value".to_string())
    })
}

pub(crate) fn lookup_symbol_value(symbols: Option<&Value>, name: &str) -> Option<String> {
    let symbols = symbols?;
    symbols
        .get_item(&Value::from(name))
        .ok()
        .filter(|value| !value.is_undefined() && !value.is_none())
        .map(|value| value_to_string(&value))
        .filter(|value| !value.is_empty())
}

pub(crate) fn emitted_symbol_or_fallback(reference: &str, cfg: &ExprConfig) -> String {
    lookup_symbol_value(cfg.symbols.as_ref(), reference).unwrap_or_else(|| {
        if cfg.sanitize_dots || cfg.subscript_underscore {
            sanitize_name(reference)
        } else {
            escape_reserved_keyword(reference)
        }
    })
}

fn dae_template_value(dae: &dae::Dae) -> Result<Value, CodegenError> {
    Ok(Value::from_serialize(dae_template_json(dae)?))
}

fn reject_external_functions_for_simulation_template(
    dae_model: &dae::Dae,
    template: &str,
) -> Result<(), CodegenError> {
    if !template_emits_simulation_function_bodies(template) {
        return Ok(());
    }
    if external_function_codegen_opt_in_enabled() {
        return Ok(());
    }
    if let Some((name, _)) = dae_model
        .symbols
        .functions
        .iter()
        .find(|(_, function)| function.external.is_some())
    {
        return Err(CodegenError::external_function_not_callable(name.as_str()));
    }
    Ok(())
}

fn reject_external_functions_in_json_for_simulation_template(
    dae_json: &serde_json::Value,
    template: &str,
) -> Result<(), CodegenError> {
    if !template_emits_simulation_function_bodies(template) {
        return Ok(());
    }
    if external_function_codegen_opt_in_enabled() {
        return Ok(());
    }
    let Some(functions) = dae_json
        .get("functions")
        .and_then(serde_json::Value::as_object)
    else {
        return Ok(());
    };
    if let Some((name, _)) = functions.iter().find(|(_, function)| {
        function
            .get("external")
            .is_some_and(|external| !external.is_null())
    }) {
        return Err(CodegenError::external_function_not_callable(name.as_str()));
    }
    Ok(())
}

fn external_function_codegen_opt_in_enabled() -> bool {
    false
}

fn template_emits_simulation_function_bodies(template: &str) -> bool {
    template.contains("func.external")
        && (template.contains("FMI 2.0 API")
            || template.contains("FMI 3.0 API")
            || template.contains("step("))
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
    };
    Ok(rendered)
}

fn render_dae_context(
    tmpl: &minijinja::Template<'_, '_>,
    dae_model: &dae::Dae,
    model_name: Option<&str>,
) -> Result<String, CodegenError> {
    let dae_value = dae_template_value(dae_model)?;
    let solve_value = optional_object_field(&dae_value, "solve");
    match model_name {
        Some(name) => Ok(tmpl.render(minijinja::context! {
            dae => dae_value.clone(),
            solve => solve_value,
            ir => dae_value,
            ir_kind => "dae",
            model_name => name,
        })?),
        None => Ok(tmpl.render(minijinja::context! {
            dae => dae_value.clone(),
            solve => solve_value,
            ir => dae_value,
            ir_kind => "dae",
        })?),
    }
}

fn render_solve_context(
    tmpl: &minijinja::Template<'_, '_>,
    solve_problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
    model_name: Option<&str>,
) -> Result<String, CodegenError> {
    let solve_value = Value::from_serialize(solve_problem);
    let artifacts_value = Value::from_serialize(artifacts);
    let solve_blocks = solve_template_blocks_value(solve_problem, artifacts)?;
    let derivative_nodes = Value::from_serialize(&solve_problem.continuous.derivative_rhs.nodes);
    let implicit_rows =
        rumoca_eval_solve::to_scalar_program_block(&solve_problem.continuous.implicit_rhs);
    let jacobian_rows =
        rumoca_eval_solve::to_scalar_program_block(&artifacts.continuous.implicit_jacobian_v);
    let implicit_rows = Value::from_serialize(&implicit_rows);
    let jacobian_rows = Value::from_serialize(&jacobian_rows);
    match model_name {
        Some(name) => Ok(tmpl.render(minijinja::context! {
            solve => solve_value.clone(),
            solve_artifacts => artifacts_value,
            ir => solve_value,
            ir_kind => "solve",
            model_name => name,
            solve_blocks => solve_blocks,
            solve_derivative_nodes => derivative_nodes,
            solve_implicit_rows => implicit_rows,
            solve_jacobian_rows => jacobian_rows,
        })?),
        None => Ok(tmpl.render(minijinja::context! {
            solve => solve_value.clone(),
            solve_artifacts => artifacts_value,
            ir => solve_value,
            ir_kind => "solve",
            solve_blocks => solve_blocks,
            solve_derivative_nodes => derivative_nodes,
            solve_implicit_rows => implicit_rows,
            solve_jacobian_rows => jacobian_rows,
        })?),
    }
}

fn solve_template_blocks_value(
    solve_problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
) -> Result<Value, CodegenError> {
    let value = serde_json::json!({
        "continuous": {
            "implicit_rhs": solve_template_compute_block_json(&solve_problem.continuous.implicit_rhs)?,
            "derivative_rhs": solve_template_compute_block_json(&solve_problem.continuous.derivative_rhs)?,
        },
        "artifacts": {
            "continuous": {
                "implicit_jacobian_v": solve_template_compute_block_json(&artifacts.continuous.implicit_jacobian_v)?,
            }
        }
    });
    Ok(Value::from_serialize(value))
}

fn solve_template_compute_block_json(
    block: &solve::ComputeBlock,
) -> Result<serde_json::Value, CodegenError> {
    let scalar_programs = rumoca_eval_solve::to_scalar_program_block(block);
    Ok(serde_json::json!({
        "nodes": serde_json::to_value(&block.nodes).map_err(|e| {
            CodegenError::SerializationFailed {
                message: format!("Solve ComputeBlock nodes: {e}"),
            }
        })?,
        "scalar_programs": serde_json::to_value(&scalar_programs).map_err(|e| {
            CodegenError::SerializationFailed {
                message: format!("Solve ComputeBlock scalar fallback rows: {e}"),
            }
        })?,
        "output_count": block.len(),
        "tensor_node_count": block.tensor_node_count(),
        "scalar_programs_use_linear_solve_component": scalar_program_block_uses_linear_solve_component(&scalar_programs),
    }))
}

fn scalar_program_block_uses_linear_solve_component(block: &solve::ScalarProgramBlock) -> bool {
    block
        .programs
        .iter()
        .flatten()
        .any(|op| matches!(op, solve::LinearOp::LinearSolveComponent { .. }))
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
    if let CodegenInput::Dae(dae_model) = input {
        reject_external_functions_for_simulation_template(dae_model, template)?;
    }
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
    if let CodegenInput::Dae(dae_model) = input {
        reject_external_functions_for_simulation_template(dae_model, template)?;
    }
    let mut env = create_environment();
    env.add_template("inline", template)?;
    let tmpl = env.get_template("inline")?;
    render_with_input_context(&tmpl, input, Some(model_name))
}

/// Render a DAE using a template string.
///
/// The template receives the full DAE structure as `dae` and can access
/// any field using standard Jinja2 syntax.
///
/// # Example Template
///
/// ```jinja
/// # States: {{ dae.x | length }}
/// {% for name, var in dae.x %}
/// {{ name | sanitize }} = Symbol('{{ name }}')
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

/// Render a template using a pre-built `dae` JSON context object.
///
/// This is useful when callers need to augment the canonical DAE context with
/// additional template-only metadata.
pub fn render_template_with_dae_json(
    dae_json: &serde_json::Value,
    template: &str,
) -> Result<String, CodegenError> {
    reject_external_functions_in_json_for_simulation_template(dae_json, template)?;
    let mut env = create_environment();
    env.add_template("inline", template)?;

    let dae_value = Value::from_serialize(dae_json);
    let tmpl = env.get_template("inline")?;
    let solve_value = optional_object_field(&dae_value, "solve");
    let ir_kind = template_ir_kind_from_dae_json(dae_json);
    let ir_value = if ir_kind == "solve" {
        solve_value.clone()
    } else {
        dae_value.clone()
    };
    let solve_blocks = solve_blocks_from_dae_json(dae_json)?;
    let solve_derivative_nodes = solve_derivative_nodes_from_dae_json(dae_json);
    let solve_jacobian_rows = solve_jacobian_rows_from_dae_json(dae_json);
    let result = tmpl.render(minijinja::context! {
        dae => dae_value.clone(),
        solve => solve_value,
        ir => ir_value,
        ir_kind => ir_kind,
        solve_blocks => solve_blocks,
        solve_derivative_nodes => solve_derivative_nodes,
        solve_jacobian_rows => solve_jacobian_rows,
    })?;

    Ok(result)
}

pub fn render_template_with_dae_json_and_name(
    dae_json: &serde_json::Value,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    reject_external_functions_in_json_for_simulation_template(dae_json, template)?;
    let mut env = create_environment();
    env.add_template("inline", template)?;

    let dae_value = Value::from_serialize(dae_json);
    let solve_value = optional_object_field(&dae_value, "solve");
    let ir_kind = template_ir_kind_from_dae_json(dae_json);
    let ir_value = if ir_kind == "solve" {
        solve_value.clone()
    } else {
        dae_value.clone()
    };
    let solve_blocks = solve_blocks_from_dae_json(dae_json)?;
    let solve_derivative_nodes = solve_derivative_nodes_from_dae_json(dae_json);
    let solve_jacobian_rows = solve_jacobian_rows_from_dae_json(dae_json);
    let tmpl = env.get_template("inline")?;
    let result = tmpl.render(minijinja::context! {
        dae => dae_value.clone(),
        solve => solve_value,
        ir => ir_value,
        ir_kind => ir_kind,
        model_name => model_name,
        solve_blocks => solve_blocks,
        solve_derivative_nodes => solve_derivative_nodes,
        solve_jacobian_rows => solve_jacobian_rows,
    })?;

    Ok(result)
}

fn optional_object_field(value: &Value, name: &str) -> Value {
    get_field(value, name).unwrap_or_else(|_| Value::from_serialize(serde_json::Map::new()))
}

fn template_ir_kind_from_dae_json(dae_json: &serde_json::Value) -> &'static str {
    if dae_json
        .get("__ir_kind")
        .and_then(serde_json::Value::as_str)
        == Some("solve")
    {
        "solve"
    } else {
        "dae"
    }
}

fn solve_blocks_from_dae_json(dae_json: &serde_json::Value) -> Result<Value, CodegenError> {
    if template_ir_kind_from_dae_json(dae_json) != "solve" {
        return Ok(Value::from_serialize(serde_json::json!({})));
    }
    let Some(solve_json) = dae_json.get("solve") else {
        return Ok(Value::from_serialize(serde_json::json!({})));
    };
    let mut problem_json = solve_json.clone();
    if let Some(object) = problem_json.as_object_mut() {
        object.remove("artifacts");
    }
    let problem: solve::SolveProblem =
        serde_json::from_value(problem_json).map_err(|err| CodegenError::SerializationFailed {
            message: format!("SolveProblem template context: {err}"),
        })?;
    let artifacts = solve_artifacts_for_template_blocks(solve_json)?;
    solve_template_blocks_value(&problem, &artifacts)
}

fn solve_artifacts_for_template_blocks(
    solve_json: &serde_json::Value,
) -> Result<solve::SolveArtifacts, CodegenError> {
    let mut artifacts = solve::SolveArtifacts::default();
    let Some(implicit_jacobian_v) = solve_json.pointer("/artifacts/continuous/implicit_jacobian_v")
    else {
        return Ok(artifacts);
    };
    artifacts.continuous.implicit_jacobian_v = serde_json::from_value(implicit_jacobian_v.clone())
        .map_err(|err| CodegenError::SerializationFailed {
            message: format!("SolveArtifacts implicit_jacobian_v template context: {err}"),
        })?;
    Ok(artifacts)
}

fn solve_derivative_nodes_from_dae_json(dae_json: &serde_json::Value) -> Value {
    Value::from_serialize(
        dae_json
            .pointer("/solve/continuous/derivative_rhs/nodes")
            .unwrap_or(&serde_json::Value::Array(Vec::new())),
    )
}

fn solve_jacobian_rows_from_dae_json(dae_json: &serde_json::Value) -> Value {
    let rows = dae_json
        .pointer("/solve/artifacts/continuous/full_jacobian_v/programs")
        .cloned()
        .or_else(|| {
            dae_json
                .pointer("/solve/artifacts/continuous/implicit_jacobian_v/nodes")
                .map(scalar_programs_from_compute_nodes)
        })
        .unwrap_or_else(|| serde_json::Value::Array(Vec::new()));
    Value::from_serialize(&rows)
}

fn scalar_programs_from_compute_nodes(nodes: &serde_json::Value) -> serde_json::Value {
    let rows = nodes
        .as_array()
        .into_iter()
        .flatten()
        .filter_map(|node| node.get("ScalarPrograms"))
        .filter_map(|node| node.get("programs"))
        .filter_map(serde_json::Value::as_array)
        .flat_map(|rows| rows.iter().cloned())
        .collect();
    serde_json::Value::Array(rows)
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
    reject_external_functions_for_simulation_template(dae, &template)?;

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
    // Fail fast on missing fields/variables in templates.
    env.set_undefined_behavior(UndefinedBehavior::Strict);

    // Custom filters
    env.add_filter("sanitize", sanitize_filter);
    env.add_filter("product", product_filter);
    env.add_filter("last_segment", last_segment_filter);
    env.add_filter("json", json_filter);

    // Helpers for target-local emitted symbols. Flattening supplies globally
    // unique Modelica names; templates provide target keyword/generated-alias policy.
    env.add_function("allocate_symbols", allocate_symbols_function);
    env.add_function("target_symbols", target_symbols_function);
    env.add_function("symbol", symbol_function);
    env.add_function("source_ref", source_ref_function);

    // Custom functions for expression rendering
    env.add_function("render_expr", render_expr_function);
    env.add_function("render_event_indicator", render_event_indicator_function);
    env.add_function("render_solve_row_c", render_solve_row_c_function);
    env.add_function("render_solve_row_rust", render_solve_row_rust_function);
    env.add_function(
        "render_solve_slot_assign_c",
        render_solve_slot_assign_c_function,
    );
    env.add_function(
        "render_optional_solve_slot_assign_c",
        render_optional_solve_slot_assign_c_function,
    );
    env.add_function(
        "render_solve_pre_param_binding_c",
        render_solve_pre_param_binding_c_function,
    );
    env.add_function("render_matmul_c", render_matmul_c_function);
    env.add_function("render_matmul_mlir", render_matmul_mlir_function);
    env.add_function("render_linsolve_mlir", render_linsolve_mlir_function);
    env.add_function("render_equation", render_equation_function);

    // Custom functions for statement rendering (MLS §12: function bodies)
    env.add_function("render_statement", render_statement_function);
    env.add_function("render_statements", render_statements_function);
    env.add_function(
        "render_function_statements",
        render_function_statements_function,
    );

    // Custom function for flat equation rendering (Model residual equations)
    env.add_function("render_flat_equation", render_flat_equation_function);

    // Render the symbolic scalar name for an array element. DAE residuals keep
    // Modelica multi-dimensional subscripts while codegen iterates linear slots.
    env.add_function("array_scalar_name", array_scalar_name_function);

    // Custom function for detecting self-referential (builtin alias) functions
    env.add_function("is_self_call", is_self_call_function);
    env.add_function(
        "unsupported_c_function_body",
        unsupported_c_function_body_function,
    );
    env.add_function(
        "unsupported_c_function_name",
        unsupported_c_function_name_function,
    );
    env.add_function("fail", fail_function);

    // Extract explicit ODE rhs from residual equation: 0 = der(x) - expr → expr
    env.add_function("ode_rhs", render_c::ode_rhs_function);
    // Find derivative expression for a specific state variable
    env.add_function("ode_rhs_for_state", render_c::ode_rhs_for_state_function);

    // Find explicit RHS for an algebraic variable from residual: 0 = y - expr → expr
    env.add_function("alg_rhs_for_var", render_c::alg_rhs_for_var_function);
    env.add_function(
        "alg_rhs_for_var_with_dae",
        render_c::alg_rhs_for_var_with_dae_function,
    );
    env.add_function(
        "alg_rhs_for_var_or_self",
        render_c::alg_rhs_for_var_or_self_function,
    );
    env.add_function(
        "discrete_rhs_for_var",
        render_c::discrete_rhs_for_var_function,
    );
    env.add_function(
        "event_indicator_expr",
        render_c::event_indicator_expr_function,
    );

    // Index into an array expression to render element i (1-based)
    env.add_function(
        "render_expr_at_index",
        render_c::render_expr_at_index_function,
    );
    env.add_function(
        "parameter_binding_rhs",
        render_c::parameter_binding_rhs_function,
    );

    // Check if an expression is a string literal for scalar templates.
    env.add_function("is_string_literal", render_c::is_string_literal_function);
    env.add_function("expr_has_var_ref", render_c::expr_has_var_ref_function);
    env.add_function(
        "expr_has_dynamic_multidim_index",
        render_c::expr_has_dynamic_multidim_index_function,
    );
    env.add_function(
        "initial_rhs_for_var",
        render_c::initial_rhs_for_var_function,
    );

    // Check if a function has record-typed parameters
    env.add_function("has_complex_params", render_c::has_complex_params_function);

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

/// Filter to compute the product of all elements in a sequence.
///
/// Used by MX template: `{{ var.dims | product }}` -> total scalar size.
fn product_filter(value: Value) -> Value {
    let Some(len) = value.len() else {
        return Value::from(1);
    };
    let mut result: i64 = 1;
    for i in 0..len {
        if let Ok(item) = value.get_item(&Value::from(i)) {
            result *= item.as_i64().unwrap_or(1);
        }
    }
    Value::from(result)
}

/// Filter to render a template value as JSON.
fn json_filter(value: Value) -> Result<String, minijinja::Error> {
    serde_json::to_string(&value).map_err(|err| render_err(format!("JSON render failed: {err}")))
}

fn array_scalar_name_function(base_name: Value, dims: Value, linear_index: Value) -> RenderResult {
    let name = base_name
        .as_str()
        .map(str::to_owned)
        .unwrap_or_else(|| base_name.to_string().trim_matches('"').to_string());
    let dims = array_dims_from_value(dims)?;
    let Some(linear_index) = linear_index.as_usize() else {
        return Err(render_err(format!(
            "array scalar index for {name} must be a positive integer"
        )));
    };
    render_array_scalar_name(&name, &dims, linear_index)
}

fn array_dims_from_value(dims: Value) -> Result<Vec<usize>, minijinja::Error> {
    let Some(len) = dims.len() else {
        return Ok(Vec::new());
    };
    let mut out = Vec::with_capacity(len);
    for i in 0..len {
        let item = dims.get_item(&Value::from(i))?;
        let Some(dim) = item.as_usize() else {
            return Err(render_err(format!(
                "array dimension at index {i} is not an integer"
            )));
        };
        if dim == 0 {
            return Err(render_err(format!("array dimension at index {i} is zero")));
        }
        out.push(dim);
    }
    Ok(out)
}

fn render_array_scalar_name(name: &str, dims: &[usize], linear_index: usize) -> RenderResult {
    if dims.is_empty() {
        return Ok(name.to_string());
    }
    if linear_index == 0 {
        return Err(render_err(format!(
            "array scalar index for {name} is one-based and cannot be zero"
        )));
    }
    let total = dims
        .iter()
        .try_fold(1usize, |acc, dim| acc.checked_mul(*dim));
    let Some(total) = total else {
        return Err(render_err(format!(
            "array dimensions for {name} overflow usize"
        )));
    };
    if linear_index > total {
        return Err(render_err(format!(
            "array scalar index {linear_index} for {name} exceeds scalar size {total}"
        )));
    }

    let mut remainder = linear_index - 1;
    let mut subscripts = vec![0usize; dims.len()];
    for (slot, dim) in subscripts.iter_mut().rev().zip(dims.iter().rev()) {
        *slot = (remainder % *dim) + 1;
        remainder /= *dim;
    }
    let rendered = subscripts
        .iter()
        .map(usize::to_string)
        .collect::<Vec<_>>()
        .join(",");
    Ok(format!("{name}[{rendered}]"))
}

fn unsupported_c_function_body_function(func: Value) -> Result<bool, minijinja::Error> {
    use render_expr::get_field;

    if let Ok(external) = get_field(&func, "external")
        && !external.is_undefined()
    {
        return Ok(true);
    }

    if let Ok(inputs) = get_field(&func, "inputs")
        && let Some(len) = inputs.len()
    {
        for i in 0..len {
            let Ok(input) = inputs.get_item(&Value::from(i)) else {
                continue;
            };
            if let Ok(dims) = get_field(&input, "dims")
                && dims.len().unwrap_or(0) > 0
            {
                return Ok(true);
            }
        }
    }

    if let Ok(locals) = get_field(&func, "locals")
        && let Some(len) = locals.len()
    {
        for i in 0..len {
            let Ok(local) = locals.get_item(&Value::from(i)) else {
                continue;
            };
            if let Ok(dims) = get_field(&local, "dims")
                && dims.len().unwrap_or(0) > 0
            {
                return Ok(true);
            }
        }
    }

    let Ok(body) = get_field(&func, "body") else {
        return Ok(false);
    };
    let body_debug = body.to_string();
    Ok(body_debug.contains("NamedArgument")
        || body_debug.contains("FieldAccess")
        || body_debug.contains("Assert")
        || body_debug.contains("outputs")
        || body_debug.contains("String(")
        || body_debug.contains("Modelica.Utilities.Strings")
        || body_debug.contains("Modelica_Utilities_Strings"))
}

fn unsupported_c_function_name_function(func_name: Value) -> Result<bool, minijinja::Error> {
    let name = func_name.to_string().replace('"', "");
    Ok(
        name.contains("Modelica.Media.Interfaces.PartialSimpleMedium")
            || name.contains("Modelica_Media_Interfaces_PartialSimpleMedium")
            || name.contains("Modelica.Media.Interfaces.PartialMedium")
            || name.contains("Modelica_Media_Interfaces_PartialMedium"),
    )
}

fn value_to_string(value: &Value) -> String {
    value
        .as_str()
        .map(str::to_owned)
        .unwrap_or_else(|| value.to_string().trim_matches('"').to_string())
}

fn dims_from_value(value: &Value) -> Vec<usize> {
    let Some(len) = value.len() else {
        return Vec::new();
    };
    let mut dims = Vec::with_capacity(len);
    for i in 0..len {
        if let Ok(item) = value.get_item(&Value::from(i))
            && let Some(dim) = item.as_i64()
            && dim > 0
        {
            dims.push(dim as usize);
        }
    }
    dims
}

fn value_list_strings(value: &Value) -> Vec<String> {
    let Some(len) = value.len() else {
        return Vec::new();
    };
    let mut out = Vec::with_capacity(len);
    for i in 0..len {
        if let Ok(item) = value.get_item(&Value::from(i)) {
            out.push(value_to_string(&item));
        }
    }
    out
}

fn subscripts_for_flat_index(dims: &[usize], flat_index: usize) -> Vec<usize> {
    if dims.is_empty() {
        return Vec::new();
    }

    let mut remaining = flat_index.saturating_sub(1);
    let mut subscripts = vec![1; dims.len()];
    for dim_idx in (0..dims.len()).rev() {
        let dim = dims[dim_idx].max(1);
        subscripts[dim_idx] = (remaining % dim) + 1;
        remaining /= dim;
    }
    subscripts
}

fn source_subscript_suffix(dims: &[usize], flat_index: usize) -> String {
    let subscripts = subscripts_for_flat_index(dims, flat_index);
    if subscripts.is_empty() {
        flat_index.max(1).to_string()
    } else {
        subscripts
            .iter()
            .map(usize::to_string)
            .collect::<Vec<_>>()
            .join(",")
    }
}

/// Return the source-reference key for a scalarized array element.
///
/// Examples:
/// - `source_ref("x", [4], 3)` -> `x[3]`
/// - `source_ref("leg.f", [4,3], 4)` -> `leg.f[2,1]`
fn source_ref_function(name: Value, dims: Value, flat_index: Value) -> String {
    let name = value_to_string(&name);
    let dims = dims_from_value(&dims);
    if dims.is_empty() {
        return name;
    }
    let index = flat_index.as_usize().unwrap_or(1).max(1);
    format!("{}[{}]", name, source_subscript_suffix(&dims, index))
}

/// Fail template rendering with an explicit message.
///
/// Templates use this to declare target-specific capability constraints
/// without pushing those policies into Rust-side backend branching.
fn fail_function(message: Value) -> RenderResult {
    let msg = message
        .as_str()
        .map(str::to_owned)
        .unwrap_or_else(|| message.to_string());
    Err(render_err(msg))
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
        let call_name = get_field(&name, "0")
            .map(|v| v.to_string().replace('"', ""))
            .unwrap_or_else(|_| name.to_string().replace('"', ""));
        return Ok(call_name == name_str);
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

fn render_function_statements_function(
    stmts: Value,
    config: Value,
    indent: Value,
    return_value: Value,
) -> RenderResult {
    let mut cfg = ExprConfig::from_value(&config);
    cfg.subscript_underscore = false;
    cfg.return_value = return_value.as_str().map(|value| value.to_string());
    let indent_str = indent.as_str().unwrap_or("    ");
    render_statements(&stmts, &cfg, indent_str)
}

// ── ExprConfig and helpers ───────────────────────────────────────────

/// Configuration for expression rendering.
#[derive(Clone)]
pub(crate) struct ExprConfig {
    pub(crate) prefix: String,
    pub(crate) power: String,
    pub(crate) and_op: String,
    pub(crate) or_op: String,
    pub(crate) not_op: String,
    pub(crate) true_val: String,
    pub(crate) false_val: String,
    pub(crate) array_start: String,
    pub(crate) array_end: String,
    pub(crate) if_style: IfStyle,
    /// When false, keep dots in variable/function names instead of replacing with underscores.
    pub(crate) sanitize_dots: bool,
    /// When true, use 1-based indexing (Modelica) instead of 0-based (Python).
    pub(crate) one_based_index: bool,
    /// When true, use Modelica builtin names (abs, min, max) instead of Python (fabs, fmin, fmax).
    pub(crate) modelica_builtins: bool,
    /// Optional function for element-wise multiply (e.g., `ca.times` for CasADi).
    pub(crate) mul_elem_fn: Option<String>,
    /// Optional function-call form for power (e.g., `ca.power` for CasADi).
    /// When set, `a^b` renders as `power_fn(a, b)` instead of `a ** b`.
    pub(crate) power_fn: Option<String>,
    /// Subscript rendering style: "bracket" (default: `x[0]`) or "underscore" (`x_1`, 1-based).
    /// The "underscore" style matches the C template's unpack_vars naming convention.
    pub(crate) subscript_underscore: bool,
    /// Override function name for `IfStyle::Function` (default: `"if_else"`).
    /// E.g., set to `"IfElse.ifelse"` for Julia ModelingToolkit.
    pub(crate) if_else_fn: Option<String>,
    /// When true, render Modelica range `start:end` as Python `range(start, end + 1)`
    /// and array comprehensions with `[...]` instead of `{...}`.
    pub(crate) python_range: bool,
    /// Override function name for `sum()` calls on non-literal arrays.
    /// Default is `"sum1"` (CasADi convention, rendered as `prefix + sum1`).
    /// Templates can set this to a runtime helper name.
    pub(crate) sum_fn: String,
    /// When true, render all numeric literals as float constants with `f` suffix.
    /// E.g., `8` → `8.0f`, `3.14` → `3.14f`.
    pub(crate) float_literals: bool,
    /// Optional source-reference to emitted-symbol map provided by a template.
    pub(crate) symbols: Option<Value>,
    /// Render-time substitutions for expression-level unrolling.
    pub(crate) substitutions: Vec<(String, String)>,
    /// Optional C/Python expression returned for bare Modelica `return` statements.
    pub(crate) return_value: Option<String>,
}

#[derive(Clone, Copy)]
pub(crate) enum IfStyle {
    /// Python-style: ca.if_else(cond, then, else)
    Function,
    /// Ternary: cond ? then : else
    Ternary,
    /// Modelica-style: if cond then expr elseif cond2 then expr2 else expr3
    Modelica,
}

impl Default for ExprConfig {
    fn default() -> Self {
        Self {
            prefix: String::new(),
            power: "**".to_string(),
            and_op: "and".to_string(),
            or_op: "or".to_string(),
            not_op: "not ".to_string(),
            true_val: "True".to_string(),
            false_val: "False".to_string(),
            array_start: "[".to_string(),
            array_end: "]".to_string(),
            if_style: IfStyle::Function,
            sanitize_dots: true,
            one_based_index: false,
            modelica_builtins: false,
            mul_elem_fn: None,
            power_fn: None,
            subscript_underscore: false,
            if_else_fn: None,
            python_range: false,
            sum_fn: "sum1".to_string(),
            float_literals: false,
            symbols: None,
            substitutions: Vec::new(),
            return_value: None,
        }
    }
}

/// Helper to get a string attribute from a Value.
pub(crate) fn get_str_attr(v: &Value, attr: &str) -> Option<String> {
    v.get_attr(attr)
        .ok()
        .and_then(|val| val.as_str().map(|s| s.to_string()))
}

impl ExprConfig {
    pub(crate) fn from_value(v: &Value) -> Self {
        let mut cfg = Self::default();

        if let Some(s) = get_str_attr(v, "prefix") {
            cfg.prefix = s;
        }
        if let Some(s) = get_str_attr(v, "power") {
            cfg.power = s;
        }
        if let Some(s) = get_str_attr(v, "and_op") {
            cfg.and_op = s;
        }
        if let Some(s) = get_str_attr(v, "or_op") {
            cfg.or_op = s;
        }
        if let Some(s) = get_str_attr(v, "not_op") {
            cfg.not_op = s;
        }
        if let Some(s) = get_str_attr(v, "true_val") {
            cfg.true_val = s;
        }
        if let Some(s) = get_str_attr(v, "false_val") {
            cfg.false_val = s;
        }
        if let Some(s) = get_str_attr(v, "array_start") {
            cfg.array_start = s;
        }
        if let Some(s) = get_str_attr(v, "array_end") {
            cfg.array_end = s;
        }
        if let Some(s) = get_str_attr(v, "if_style") {
            cfg.if_style = match s.as_str() {
                "ternary" => IfStyle::Ternary,
                "modelica" => IfStyle::Modelica,
                _ => IfStyle::Function,
            };
        }
        if let Ok(val) = v.get_attr("sanitize_dots")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.sanitize_dots = val.is_true();
        }
        if let Ok(val) = v.get_attr("one_based_index")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.one_based_index = val.is_true();
        }
        if let Ok(val) = v.get_attr("modelica_builtins")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.modelica_builtins = val.is_true();
        }
        if let Some(s) = get_str_attr(v, "mul_elem_fn")
            && !s.is_empty()
        {
            cfg.mul_elem_fn = Some(s);
        }
        if let Some(s) = get_str_attr(v, "power_fn")
            && !s.is_empty()
        {
            cfg.power_fn = Some(s);
        }
        if let Ok(val) = v.get_attr("subscript_underscore")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.subscript_underscore = val.is_true();
        }
        if let Some(s) = get_str_attr(v, "if_else_fn")
            && !s.is_empty()
        {
            cfg.if_else_fn = Some(s);
        }
        if let Ok(val) = v.get_attr("python_range")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.python_range = val.is_true();
        }
        if let Some(s) = get_str_attr(v, "sum_fn")
            && !s.is_empty()
        {
            cfg.sum_fn = s;
        }
        if let Ok(val) = v.get_attr("float_literals")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.float_literals = val.is_true();
        }
        if let Ok(val) = v.get_attr("symbols")
            && !val.is_undefined()
            && !val.is_none()
        {
            cfg.symbols = Some(val);
        }
        if let Some(s) = get_str_attr(v, "return_value")
            && !s.is_empty()
        {
            cfg.return_value = Some(s);
        }

        cfg
    }
}

#[cfg(test)]
mod local_tests {
    use super::*;

    #[test]
    fn enum_type_names_use_top_level_parent_scope() {
        let mut dae = dae::Dae::default();
        dae.symbols.enum_literal_ordinals.insert(
            "Modelica.Blocks.Types.Smoothness.LinearSegments".to_string(),
            1,
        );
        dae.symbols
            .enum_literal_ordinals
            .insert("Pkg.Enum[index.with.dot].Choice".to_string(), 1);

        assert_eq!(
            enum_type_names_from_ordinals(&dae),
            vec![
                "Modelica.Blocks.Types.Smoothness".to_string(),
                "Pkg.Enum[index.with.dot]".to_string(),
            ]
        );
    }
}

#[cfg(test)]
mod codegen_tests;
#[cfg(test)]
mod fmi_template_tests;
#[cfg(test)]
mod strict_render_tests;
