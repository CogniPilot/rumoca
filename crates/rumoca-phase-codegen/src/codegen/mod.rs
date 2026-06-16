//! Code generation implementation.
//!
//! This module provides a simple template rendering function. The DAE is
//! serialized and passed directly to minijinja templates, which can then
//! walk the expression tree and generate code as needed.
//!
//! For common cases, templates can use the built-in `render_expr` function
//! which handles the recursive tree walking with configurable operator syntax.

use crate::errors::{CodegenError, render_err};
use indexmap::{IndexMap, IndexSet};
use minijinja::{Environment, UndefinedBehavior, Value};
use rumoca_core::{Expression, ExpressionVisitor, Span, Subscript};
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
mod solve_lazy;

use render_expr::{get_field, is_variant, render_expression};
use render_solve::{
    render_linsolve_mlir_function, render_matmul_c_function, render_matmul_mlir_function,
    render_optional_solve_slot_assign_c_function, render_solve_block_c_function,
    render_solve_block_rust_function, render_solve_pre_param_binding_c_function,
    render_solve_row_c_function, render_solve_row_rust_function, render_solve_row_wgsl_function,
    render_solve_slot_assign_c_function, solve_block_output_count_function,
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
    let symbol_aliases = symbol_aliases_from_dae(dae)?;
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
        "symbol_aliases".to_string(),
        serde_json::to_value(symbol_aliases).map_err(|e| CodegenError::SerializationFailed {
            message: format!("symbol_aliases: {e}"),
        })?,
    );
    object.insert(
        "condition_aliases".to_string(),
        serde_json::Value::Array(condition_aliases),
    );
    Ok(value)
}

fn condition_aliases_from_dae(dae: &dae::Dae) -> Result<Vec<serde_json::Value>, serde_json::Error> {
    dae.conditions
        .equations
        .iter()
        .filter_map(|eq| eq.lhs.as_ref().map(|lhs| (lhs, &eq.rhs)))
        .map(|(lhs, relation)| {
            Ok(serde_json::json!({
                "condition": Expression::VarRef {
                    name: lhs.clone(),
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
    let mut seen = IndexSet::new();
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
    let mut refs = IndexSet::new();

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
    refs: &mut IndexSet<String>,
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

fn add_source_refs_for_var(name: &str, dims: &[i64], refs: &mut IndexSet<String>) {
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

fn symbol_aliases_from_dae(dae: &dae::Dae) -> Result<Vec<serde_json::Value>, CodegenError> {
    let mut declared_refs = IndexSet::new();
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
            add_source_refs_for_var(name.as_str(), &var.dims, &mut declared_refs);
        }
    }

    let mut aliases = IndexMap::<String, String>::new();
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
            add_symbol_aliases_for_variable_exprs(
                name.as_str(),
                var,
                &declared_refs,
                &mut aliases,
            )?;
        }
    }

    Ok(aliases
        .into_iter()
        .map(|(alias, target)| {
            serde_json::json!({
                "alias": alias,
                "target": target,
            })
        })
        .collect())
}

fn add_symbol_aliases_for_variable_exprs(
    owner_name: &str,
    var: &dae::Variable,
    declared_refs: &IndexSet<String>,
    aliases: &mut IndexMap<String, String>,
) -> Result<(), CodegenError> {
    for expr in [
        var.start.as_ref(),
        var.min.as_ref(),
        var.max.as_ref(),
        var.nominal.as_ref(),
    ]
    .into_iter()
    .flatten()
    {
        add_symbol_aliases_for_expr(owner_name, expr, declared_refs, aliases)?;
    }
    Ok(())
}

fn add_symbol_aliases_for_expr(
    owner_name: &str,
    expr: &Expression,
    declared_refs: &IndexSet<String>,
    aliases: &mut IndexMap<String, String>,
) -> Result<(), CodegenError> {
    let mut collector = dae::VarRefWithSubscriptsCollector::new();
    collector.visit_expression(expr);
    for (name, subscripts) in collector.into_refs() {
        let Some(target_base) =
            resolve_attribute_expr_alias_target(owner_name, name.as_str(), declared_refs)
        else {
            continue;
        };
        insert_symbol_alias(aliases, name.as_str().to_string(), target_base.clone())?;
        let Some(indices) = literal_positive_indices(&subscripts) else {
            continue;
        };
        let alias = dae::format_subscript_key(name.as_str(), &indices);
        let target = dae::format_subscript_key(&target_base, &indices);
        if declared_refs.contains(&target) {
            insert_symbol_alias(aliases, alias, target)?;
        }
    }
    Ok(())
}

fn resolve_attribute_expr_alias_target(
    owner_name: &str,
    reference: &str,
    declared_refs: &IndexSet<String>,
) -> Option<String> {
    if declared_refs.contains(reference) || rumoca_core::split_last_top_level(reference).is_some() {
        return None;
    }
    let (owner_scope, _) = rumoca_core::split_last_top_level(owner_name)?;
    let candidate = format!("{owner_scope}.{reference}");
    declared_refs.contains(&candidate).then_some(candidate)
}

fn insert_symbol_alias(
    aliases: &mut IndexMap<String, String>,
    alias: String,
    target: String,
) -> Result<(), CodegenError> {
    if alias == target {
        return Ok(());
    }
    if let Some(existing) = aliases.get(&alias) {
        if existing == &target {
            return Ok(());
        }
        return Err(CodegenError::SerializationFailed {
            message: format!(
                "conflicting emitted symbol aliases for `{alias}`: `{existing}` and `{target}`"
            ),
        });
    }
    aliases.insert(alias, target);
    Ok(())
}

fn literal_positive_indices(subscripts: &[Subscript]) -> Option<Vec<usize>> {
    if subscripts.is_empty() {
        return None;
    }
    subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } => {
                usize::try_from(*value).ok().filter(|value| *value > 0)
            }
            Subscript::Expr { .. } | Subscript::Colon { .. } => None,
        })
        .collect()
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
    ) -> RenderResult {
        for candidate in candidates {
            let count = candidate_counts.get(candidate).copied().ok_or_else(|| {
                render_err(format!(
                    "symbol candidate `{candidate}` missing from allocation count table"
                ))
            })?;
            if count <= 1 && self.try_use(candidate) {
                return Ok(candidate.clone());
            }
        }

        for candidate in candidates {
            if self.try_use(candidate) {
                return Ok(candidate.clone());
            }
        }

        let base = candidates
            .last()
            .ok_or_else(|| render_err("symbol allocation requires at least one candidate"))?;
        for idx in 2.. {
            let candidate = format!("{base}_{idx}");
            if self.try_use(&candidate) {
                return Ok(candidate);
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

fn allocate_symbols_function(symbol_refs: Value, policy: Value) -> Result<Value, minijinja::Error> {
    let policy = SymbolPolicy::from_value(&policy);
    let references = value_list_strings(&symbol_refs);
    let symbols = allocate_symbols_for_refs(references, policy)?;
    Ok(Value::from_serialize(symbols))
}

fn target_symbols_function(
    symbol_refs: Value,
    policy: Value,
    symbol_aliases: Value,
) -> Result<Value, minijinja::Error> {
    let policy = SymbolPolicy::from_value(&policy);
    let references = value_list_strings(&symbol_refs);
    let mut requests = references
        .into_iter()
        .map(|reference| {
            let candidates = symbol_candidates(&reference, &policy)?;
            Ok((reference, candidates))
        })
        .collect::<Result<Vec<_>, minijinja::Error>>()?;

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
    let mut out = IndexMap::new();
    for (reference, candidates) in requests {
        let symbol = allocator.allocate(&candidates, &candidate_counts)?;
        out.insert(reference, symbol);
    }
    for (alias, target) in value_symbol_aliases(&symbol_aliases)? {
        let symbol = out.get(&target).cloned().ok_or_else(|| {
            render_err(format!(
                "symbol alias `{alias}` targets unknown source reference `{target}`"
            ))
        })?;
        out.insert(alias, symbol);
    }
    Ok(Value::from_serialize(out))
}

fn allocate_symbols_for_refs(
    mut references: Vec<String>,
    policy: SymbolPolicy,
) -> Result<IndexMap<String, String>, minijinja::Error> {
    references.sort_by(|a, b| {
        symbol_ref_priority(a)
            .cmp(&symbol_ref_priority(b))
            .then_with(|| a.cmp(b))
    });
    references.dedup();

    let requests = references
        .into_iter()
        .map(|reference| {
            let candidates = symbol_candidates(&reference, &policy)?;
            Ok((reference, candidates))
        })
        .collect::<Result<Vec<_>, minijinja::Error>>()?;

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
    let mut out = IndexMap::new();
    for (reference, candidates) in requests {
        let symbol = allocator.allocate(&candidates, &candidate_counts)?;
        out.insert(reference, symbol);
    }
    Ok(out)
}

fn symbol_candidates(
    modelica_ref: &str,
    policy: &SymbolPolicy,
) -> Result<Vec<String>, minijinja::Error> {
    let (base_ref, subscript) = split_modelica_subscript(modelica_ref);
    let suffix = subscript.map(|s| scalarized_subscript_suffix(s, &policy.separator));
    let segments = split_modelica_path(base_ref);
    let mut candidates = Vec::new();
    for start in (0..segments.len()).rev() {
        let base = segments[start..]
            .iter()
            .filter_map(|segment| readable_identifier_segment(segment))
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
        return Err(render_err(format!(
            "source reference `{modelica_ref}` does not contain any valid identifier segment"
        )));
    }
    candidates.dedup();
    Ok(candidates)
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

fn readable_identifier_segment(segment: &str) -> Option<String> {
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
    (!out.is_empty()).then_some(out)
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

fn symbol_function(symbols: Value, name: Value) -> RenderResult {
    let name = value_to_string(&name);
    lookup_symbol_value(Some(&symbols), &name).ok_or_else(|| {
        render_err(format!(
            "missing emitted symbol for source reference `{name}`"
        ))
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

pub(crate) fn emitted_symbol(reference: &str, cfg: &ExprConfig) -> RenderResult {
    if let Some(symbols) = cfg.symbols.as_ref() {
        return lookup_symbol_value(Some(symbols), reference).ok_or_else(|| {
            render_err(format!(
                "missing emitted symbol for source reference `{reference}`"
            ))
        });
    }
    if cfg.sanitize_dots || cfg.subscript_underscore {
        Ok(sanitize_name(reference))
    } else {
        Ok(escape_reserved_keyword(reference))
    }
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
            derivative_rhs => solve_template_compute_block_json(&solve_problem.continuous.derivative_rhs)?,
        },
        artifacts => minijinja::context! {
            continuous => minijinja::context! {
                implicit_jacobian_v => solve_template_compute_block_json(&artifacts.continuous.implicit_jacobian_v)?,
            },
        },
    })
}

fn solve_template_compute_block_json(block: &solve::ComputeBlock) -> Result<Value, CodegenError> {
    let scalar_programs = rumoca_eval_solve::to_scalar_program_block(block);
    let uses_linear_solve = scalar_program_block_uses_linear_solve_component(&scalar_programs);
    // Lazy nodes (one ComputeNode -> ops materialized on demand) so blocks whose
    // nodes contain large op programs don't materialize as eager Values.
    let nodes = solve_lazy::nodes_value(std::sync::Arc::new(block.clone()));
    let program_spans = Value::from_serialize(&scalar_programs.program_spans);
    // Affine row families (stencils) let GPU backends emit one parametric
    // kernel per family instead of one switch case per row; rows not in a
    // family stay addressable through `stencil_residual_rows`.
    let partition = stencil::partition_rows(&scalar_programs.programs);
    let stencil_residual_rows = Value::from_serialize(&partition.residual_rows);
    let stencils = Value::from_object(render_solve::SolveStencilsValue::new(partition.families));
    // Rows go to templates as typed objects: per-row serde bridging made
    // rendering the dominant compile cost on large models.
    let programs = Value::from_object(render_solve::SolveRowsValue::new(scalar_programs.programs));
    Ok(minijinja::context! {
        nodes => nodes,
        scalar_programs => minijinja::context! {
            programs => programs,
            program_spans => program_spans,
        },
        stencils => stencils,
        stencil_residual_rows => stencil_residual_rows,
        output_count => block.len(),
        tensor_node_count => block.tensor_node_count(),
        scalar_programs_use_linear_solve_component => uses_linear_solve,
    })
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
    // Fail fast on missing fields/variables in templates.
    env.set_undefined_behavior(UndefinedBehavior::Strict);

    // Custom filters
    env.add_filter("sanitize", sanitize_filter);
    env.add_filter("product", product_filter);
    env.add_filter("last_segment", last_segment_filter);

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
    env.add_function("render_solve_block_c", render_solve_block_c_function);
    env.add_function("render_solve_block_rust", render_solve_block_rust_function);
    env.add_function("store_output_count", solve_block_output_count_function);
    env.add_function("render_solve_row_wgsl", render_solve_row_wgsl_function);
    env.add_function(
        "render_solve_stencil_wgsl",
        render_solve::render_solve_stencil_wgsl_function,
    );
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

    // Custom function for flat equation rendering (Model residual equations)
    env.add_function("render_flat_equation", render_flat_equation_function);

    // Custom function for detecting self-referential (builtin alias) functions
    env.add_function("is_self_call", is_self_call_function);
    env.add_function("fail", fail_function);

    // Extract explicit ODE rhs from residual equation: 0 = der(x) - expr → expr
    env.add_function("ode_rhs", render_c::ode_rhs_function);
    // Find derivative expression for a specific state variable
    env.add_function("ode_rhs_for_state", render_c::ode_rhs_for_state_function);

    // Find explicit RHS for an algebraic variable from residual: 0 = y - expr → expr
    env.add_function("alg_rhs_for_var", render_c::alg_rhs_for_var_function);
    env.add_function(
        "alg_rhs_for_var_or_self",
        render_c::alg_rhs_for_var_or_self_function,
    );
    env.add_function(
        "discrete_rhs_for_var",
        render_c::discrete_rhs_for_var_function,
    );

    // Index into an array expression to render element i (1-based)
    env.add_function(
        "render_expr_at_index",
        render_c::render_expr_at_index_function,
    );

    // Check if an expression is a string literal for scalar templates.
    env.add_function("is_string_literal", render_c::is_string_literal_function);

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

fn value_symbol_aliases(value: &Value) -> Result<Vec<(String, String)>, minijinja::Error> {
    let Some(len) = value.len() else {
        return Ok(Vec::new());
    };
    let mut out = Vec::with_capacity(len);
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
    let index = flat_index
        .as_usize()
        .expect("source_ref flat index must be numeric")
        .max(1);
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
    /// Optional aliases from Appendix-B condition memory (`c[i]`) to live
    /// relation expressions for backends that do not run event iteration.
    pub(crate) condition_aliases: Option<Value>,
    /// Render-time substitutions for expression-level unrolling.
    pub(crate) substitutions: Vec<(String, String)>,
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
            condition_aliases: None,
            substitutions: Vec::new(),
        }
    }
}

/// Helper to get a string attribute from a Value.
pub(crate) fn get_str_attr(v: &Value, attr: &str) -> Option<String> {
    v.get_attr(attr)
        .ok()
        .and_then(|val| val.as_str().map(|s| s.to_string()))
}

fn get_present_attr(v: &Value, attr: &str) -> Option<Value> {
    let val = v.get_attr(attr).ok()?;
    (!val.is_undefined() && !val.is_none()).then_some(val)
}

fn get_bool_attr(v: &Value, attr: &str) -> Option<bool> {
    Some(get_present_attr(v, attr)?.is_true())
}

fn get_non_empty_str_attr(v: &Value, attr: &str) -> Option<String> {
    get_str_attr(v, attr).filter(|s| !s.is_empty())
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
        if let Some(enabled) = get_bool_attr(v, "sanitize_dots") {
            cfg.sanitize_dots = enabled;
        }
        if let Some(enabled) = get_bool_attr(v, "one_based_index") {
            cfg.one_based_index = enabled;
        }
        if let Some(enabled) = get_bool_attr(v, "modelica_builtins") {
            cfg.modelica_builtins = enabled;
        }
        if let Some(s) = get_non_empty_str_attr(v, "mul_elem_fn") {
            cfg.mul_elem_fn = Some(s);
        }
        if let Some(s) = get_non_empty_str_attr(v, "power_fn") {
            cfg.power_fn = Some(s);
        }
        if let Some(enabled) = get_bool_attr(v, "subscript_underscore") {
            cfg.subscript_underscore = enabled;
        }
        if let Some(s) = get_non_empty_str_attr(v, "if_else_fn") {
            cfg.if_else_fn = Some(s);
        }
        if let Some(enabled) = get_bool_attr(v, "python_range") {
            cfg.python_range = enabled;
        }
        if let Some(s) = get_non_empty_str_attr(v, "sum_fn") {
            cfg.sum_fn = s;
        }
        if let Some(enabled) = get_bool_attr(v, "float_literals") {
            cfg.float_literals = enabled;
        }
        if let Some(val) = get_present_attr(v, "symbols") {
            cfg.symbols = Some(val);
        }
        if let Some(val) = get_present_attr(v, "condition_aliases") {
            cfg.condition_aliases = Some(val);
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

mod stencil;

mod solve_renderer;
pub use solve_renderer::SolveTemplateRenderer;
use solve_renderer::solve_render_context_value;

#[cfg(test)]
mod codegen_block_render_tests;
#[cfg(test)]
mod codegen_tests;
#[cfg(test)]
mod fmi_template_tests;
#[cfg(test)]
mod strict_render_tests;
#[cfg(test)]
mod wgsl_solve_tests;
