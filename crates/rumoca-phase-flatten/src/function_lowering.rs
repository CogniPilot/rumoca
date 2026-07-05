//! Record parameter lowering for flattened functions.
//!
//! This module handles post-collection passes that transform function signatures
//! and call sites:
//! - Decomposing record-typed parameters into scalar fields
//! - Normalizing structured record-field component references
//! - Rewriting FieldAccess expressions on decomposed record params to direct VarRef

// SPEC_0021 file-size exception: record/function lowering is still cohesive
// while Kelvin parity work is in flight; split plan is to move constructor
// projection and field-reference projection helpers into focused modules after
// the active FMU correctness path is stable.

use crate::errors::FlattenError;
use rumoca_core::{ExpressionRewriter, ExpressionVisitor, StatementRewriter};
use rumoca_ir_flat as flat;
use std::collections::{HashMap, HashSet};

fn record_fields_from_constructor_metadata(
    functions: &flat::VarNameIndexMap<rumoca_core::Function>,
    type_name: &str,
    owner_function_name: Option<&str>,
) -> Option<Vec<rumoca_core::FunctionParam>> {
    let candidates = functions
        .iter()
        .filter(|(name, function)| {
            function.is_constructor
                && rumoca_core::qualified_type_name_matches(name.as_str(), type_name)
        })
        .filter(|(_, function)| !function.inputs.is_empty())
        .collect::<Vec<_>>();

    if candidates.is_empty() {
        return None;
    }

    if type_name.contains('.') {
        return candidates
            .into_iter()
            .max_by_key(|(name, function)| {
                (
                    name.as_str() == type_name,
                    function.inputs.len(),
                    name.as_str().len(),
                )
            })
            .map(|(_, function)| function.inputs.to_vec());
    }

    if let Some(contextual_type_name) =
        owner_function_name.and_then(|name| contextual_record_type_name(name, type_name))
        && let Some((_, function)) = candidates
            .iter()
            .copied()
            .filter(|(name, _)| {
                rumoca_core::qualified_type_name_matches(name.as_str(), &contextual_type_name)
            })
            .max_by_key(|(name, function)| {
                (
                    name.as_str() == contextual_type_name,
                    function.inputs.len(),
                    name.as_str().len(),
                )
            })
    {
        return Some(function.inputs.to_vec());
    }

    candidates
        .into_iter()
        .min_by_key(|(name, function)| {
            (
                name.as_str() != type_name,
                function.inputs.len(),
                name.as_str().len(),
            )
        })
        .map(|(_, function)| function.inputs.to_vec())
}

fn contextual_record_type_name(function_name: &str, type_name: &str) -> Option<String> {
    let namespace = rumoca_core::ComponentPath::from_flat_path(function_name).parent()?;
    Some(
        namespace
            .join(&rumoca_core::ComponentPath::from_flat_path(type_name))
            .to_flat_string(),
    )
}

/// Rewrite FieldAccess on decomposed record params to direct VarRef.
fn rewrite_field_access_in_statement(stmt: &mut rumoca_core::Statement, params: &HashSet<String>) {
    *stmt = RecordFieldAccessRewriter { params }.rewrite_statement(stmt);
}

struct RecordFieldAccessRewriter<'a> {
    params: &'a HashSet<String>,
}

impl ExpressionRewriter for RecordFieldAccessRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let Some(rewritten) = rewrite_component_ref_record_field(expr, self.params) {
            return rewritten;
        }

        if let rumoca_core::Expression::FieldAccess { base, field, span } = expr
            && let rumoca_core::Expression::Index {
                base: indexed_base,
                subscripts,
                ..
            } = base.as_ref()
            && let rumoca_core::Expression::VarRef { name, .. } = indexed_base.as_ref()
            && self.params.contains(name.as_str())
        {
            return rumoca_core::Expression::VarRef {
                name: record_param_field_reference(name.as_str(), field, *span),
                subscripts: subscripts.clone(),
                span: *span,
            };
        }

        if let rumoca_core::Expression::FieldAccess { base, field, span } = expr
            && let rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } = base.as_ref()
            && subscripts.is_empty()
            && self.params.contains(name.as_str())
        {
            return rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(format!("{}_{}", name.as_str(), field)),
                subscripts: vec![],
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}

impl StatementRewriter for RecordFieldAccessRewriter<'_> {}

fn rewrite_record_param_size_refs_in_function(
    func: &mut rumoca_core::Function,
    shape_sources: &[(String, String)],
) {
    if shape_sources.is_empty() {
        return;
    }
    let mut rewriter = RecordParamSizeRewriter { shape_sources };
    for param in func
        .inputs
        .iter_mut()
        .chain(func.outputs.iter_mut())
        .chain(func.locals.iter_mut())
    {
        if let Some(default) = &mut param.default {
            *default = rewriter.rewrite_expression(default);
        }
    }
    for stmt in &mut func.body {
        *stmt = rewriter.rewrite_statement(stmt);
    }
}

struct RecordParamSizeRewriter<'a> {
    shape_sources: &'a [(String, String)],
}

impl ExpressionRewriter for RecordParamSizeRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        let rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };

        let mut args = self.rewrite_expressions(args);
        if let Some(first_arg) = args.first_mut()
            && let rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } = first_arg
            && subscripts.is_empty()
        {
            if let Some((_, shape_source)) = self
                .shape_sources
                .iter()
                .find(|(param_name, _)| param_name == name.as_str())
            {
                *first_arg = rumoca_core::Expression::VarRef {
                    name: record_param_reference(shape_source, *span),
                    subscripts: Vec::new(),
                    span: *span,
                };
            } else if let Some(reference) = name.component_ref()
                && let Some(record) = reference.parts.first()
                && let Some((_, shape_source)) = self
                    .shape_sources
                    .iter()
                    .find(|(param_name, _)| param_name == record.ident.as_str())
            {
                *first_arg = rumoca_core::Expression::VarRef {
                    name: record_param_reference(shape_source, *span),
                    subscripts: Vec::new(),
                    span: *span,
                };
            }
        }

        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args,
            span: *span,
        }
    }
}

impl StatementRewriter for RecordParamSizeRewriter<'_> {}

fn rewrite_component_ref_record_field(
    expr: &rumoca_core::Expression,
    params: &HashSet<String>,
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }

    let reference = name.component_ref()?;
    let [record, field] = reference.parts.as_slice() else {
        return None;
    };
    if !record.subs.is_empty() || !field.subs.is_empty() || !params.contains(record.ident.as_str())
    {
        return None;
    }

    Some(record_param_field_var_ref(
        record.ident.as_str(),
        field.ident.as_str(),
        *span,
    ))
}

fn record_param_field_var_ref(
    param: &str,
    field: &str,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: record_param_field_reference(param, field, span),
        subscripts: vec![],
        span,
    }
}

fn record_param_field_reference(
    param: &str,
    field: &str,
    span: rumoca_core::Span,
) -> rumoca_core::Reference {
    let name = format!("{param}_{field}");
    let component_ref = rumoca_core::ComponentReference {
        local: false,
        span,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.clone(),
            span,
            subs: Vec::new(),
        }],
        def_id: None,
    };
    rumoca_core::Reference::with_component_reference(name, component_ref)
}

/// Normalize record-field component references in a function body.
///
/// Modelica record field syntax can arrive as a structured component reference
/// (`state.x`) rather than an explicit `FieldAccess`. This pass preserves that
/// structure as `FieldAccess` so later passes can either keep the record
/// parameter intact or decompose both the signature and body together.
pub(super) fn rewrite_record_field_access_in_body(func: &mut rumoca_core::Function) {
    let mut record_param_names: HashSet<String> = HashSet::new();
    for input in &func.inputs {
        if input.type_class == Some(rumoca_core::ClassType::Record) {
            record_param_names.insert(input.name.clone());
        }
    }
    if record_param_names.is_empty() {
        return;
    }
    for stmt in &mut func.body {
        normalize_record_field_access_in_statement(stmt, &record_param_names);
    }
}

fn normalize_record_field_access_in_statement(
    stmt: &mut rumoca_core::Statement,
    params: &HashSet<String>,
) {
    *stmt = RecordFieldAccessNormalizer { params }.rewrite_statement(stmt);
}

struct RecordFieldAccessNormalizer<'a> {
    params: &'a HashSet<String>,
}

impl ExpressionRewriter for RecordFieldAccessNormalizer<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let Some(rewritten) = component_ref_record_field_access(expr, self.params) {
            return rewritten;
        }
        self.walk_expression(expr)
    }
}

impl StatementRewriter for RecordFieldAccessNormalizer<'_> {}

fn component_ref_record_field_access(
    expr: &rumoca_core::Expression,
    params: &HashSet<String>,
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }

    let reference = name.component_ref()?;
    let [record, field] = reference.parts.as_slice() else {
        return None;
    };
    if !record.subs.is_empty() || !field.subs.is_empty() || !params.contains(record.ident.as_str())
    {
        return None;
    }

    Some(rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::VarRef {
            name: record_param_reference(record.ident.as_str(), record.span),
            subscripts: vec![],
            span: record.span,
        }),
        field: field.ident.clone(),
        span: *span,
    })
}

fn record_param_reference(param: &str, span: rumoca_core::Span) -> rumoca_core::Reference {
    let component_ref = rumoca_core::ComponentReference {
        local: false,
        span,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: param.to_string(),
            span,
            subs: Vec::new(),
        }],
        def_id: None,
    };
    rumoca_core::Reference::with_component_reference(param.to_string(), component_ref)
}

// =============================================================================
// Post-collection passes: record decomposition
// =============================================================================

/// Decompose record-typed function parameters and rewrite call sites.
///
/// For each function with a known record-typed input (e.g., `Complex`):
/// 1. Replace the record input with scalar field inputs in the signature.
/// 2. Rewrite FieldAccess in the body to VarRef.
/// 3. Walk all equations/functions and decompose call-site arguments.
pub(crate) fn lower_record_function_params(flat: &mut flat::Model) -> Result<(), FlattenError> {
    let flat_variable_names = flat.variables.keys().cloned().collect::<HashSet<_>>();
    let constructor_input_names_by_type = flat
        .functions
        .iter()
        .filter(|(_, function)| function.is_constructor)
        .map(|(name, function)| {
            (
                name.as_str().to_string(),
                function
                    .inputs
                    .iter()
                    .map(|input| input.name.clone())
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<HashMap<_, _>>();
    let record_fields_by_function_input = flat
        .functions
        .iter()
        .flat_map(|(function_name, function)| {
            function
                .inputs
                .iter()
                .enumerate()
                .filter_map(|(idx, input)| {
                    if input.type_class != Some(rumoca_core::ClassType::Record) {
                        return None;
                    }
                    record_fields_from_constructor_metadata(
                        &flat.functions,
                        &input.type_name,
                        Some(function_name.as_str()),
                    )
                    .map(|fields| ((function_name.as_str().to_string(), idx), fields))
                })
                .collect::<Vec<_>>()
        })
        .collect::<HashMap<_, _>>();
    let record_constructor_fields = flat
        .functions
        .iter()
        .filter(|(_, function)| function.is_constructor)
        .map(|(name, function)| (name.as_str().to_string(), function.inputs.to_vec()))
        .collect::<Vec<_>>();

    let mut decomposition_map: HashMap<String, Vec<DecomposedParam>> = HashMap::new();
    let mut local_decomposed_params: HashMap<String, HashSet<String>> = HashMap::new();

    for (func_name, func) in flat.functions.iter_mut() {
        let mut decomposed: Vec<DecomposedParam> = Vec::new();
        for (idx, input) in func.inputs.iter().enumerate() {
            if let Some(fields) =
                record_fields_by_function_input.get(&(func_name.as_str().to_string(), idx))
            {
                decomposed.push(DecomposedParam {
                    original_index: idx,
                    param_name: input.name.clone(),
                    type_name: input.type_name.clone(),
                    fields: fields.clone(),
                    already_decomposed: false,
                });
            }
        }
        if decomposed.is_empty() {
            let already_decomposed =
                infer_existing_decomposed_params(func, &record_constructor_fields);
            if !already_decomposed.is_empty() {
                rewrite_existing_decomposed_inputs(func, &already_decomposed);
                decomposition_map.insert(func_name.as_str().to_string(), already_decomposed);
            }
            continue;
        }

        local_decomposed_params.insert(
            func_name.as_str().to_string(),
            decomposed.iter().map(|d| d.param_name.clone()).collect(),
        );

        // Rewrite FieldAccess in body
        let param_names: HashSet<String> =
            decomposed.iter().map(|d| d.param_name.clone()).collect();
        let shape_sources = decomposed
            .iter()
            .filter_map(|d| {
                record_param_shape_source(d).map(|source| (d.param_name.clone(), source))
            })
            .collect::<Vec<_>>();
        rewrite_record_param_size_refs_in_function(func, &shape_sources);
        for stmt in &mut func.body {
            rewrite_field_access_in_statement(stmt, &param_names);
        }

        // Replace record inputs with scalar field inputs
        let old_inputs = std::mem::take(&mut func.inputs);
        for (idx, input) in old_inputs.into_iter().enumerate() {
            let Some(dp) = decomposed.iter().find(|d| d.original_index == idx) else {
                func.inputs.push(input);
                continue;
            };
            for field in &dp.fields {
                func.inputs
                    .push(decomposed_record_field_param(&dp.param_name, &input, field));
            }
        }

        decomposition_map.insert(func_name.as_str().to_string(), decomposed);
    }

    if decomposition_map.is_empty() {
        project_overexpanded_function_calls(flat);
        return Ok(());
    }

    // Rewrite call sites in equations, variable bindings, and function bodies.
    for eq in &mut flat.equations {
        decompose_record_call_args_in_expr(
            &mut eq.residual,
            &decomposition_map,
            None,
            &flat_variable_names,
            &constructor_input_names_by_type,
        )?;
    }
    for eq in &mut flat.initial_equations {
        decompose_record_call_args_in_expr(
            &mut eq.residual,
            &decomposition_map,
            None,
            &flat_variable_names,
            &constructor_input_names_by_type,
        )?;
    }
    for var in flat.variables.values_mut() {
        if let Some(ref mut binding) = var.binding {
            decompose_record_call_args_in_expr(
                binding,
                &decomposition_map,
                None,
                &flat_variable_names,
                &constructor_input_names_by_type,
            )?;
        }
        if let Some(ref mut start) = var.start {
            decompose_record_call_args_in_expr(
                start,
                &decomposition_map,
                None,
                &flat_variable_names,
                &constructor_input_names_by_type,
            )?;
        }
        if let Some(ref mut min) = var.min {
            decompose_record_call_args_in_expr(
                min,
                &decomposition_map,
                None,
                &flat_variable_names,
                &constructor_input_names_by_type,
            )?;
        }
        if let Some(ref mut max) = var.max {
            decompose_record_call_args_in_expr(
                max,
                &decomposition_map,
                None,
                &flat_variable_names,
                &constructor_input_names_by_type,
            )?;
        }
        if let Some(ref mut nominal) = var.nominal {
            decompose_record_call_args_in_expr(
                nominal,
                &decomposition_map,
                None,
                &flat_variable_names,
                &constructor_input_names_by_type,
            )?;
        }
    }
    for (func_name, func) in flat.functions.iter_mut() {
        let local_record_params = local_decomposed_params.get(func_name.as_str());
        for stmt in &mut func.body {
            decompose_record_call_args_in_stmt(
                stmt,
                &decomposition_map,
                local_record_params,
                &flat_variable_names,
                &constructor_input_names_by_type,
            )?;
        }
    }
    project_overexpanded_function_calls(flat);
    Ok(())
}

fn project_overexpanded_function_calls(flat: &mut flat::Model) {
    let signatures = flat
        .functions
        .iter()
        .filter(|(_, function)| !function.inputs.is_empty())
        .map(|(name, function)| (name.as_str().to_string(), function.inputs.to_vec()))
        .collect::<HashMap<_, _>>();
    if signatures.is_empty() {
        return;
    }
    let flat_variable_names = flat
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect::<HashSet<_>>();
    let mut rewriter = OverexpandedFunctionCallProjector {
        signatures,
        flat_variable_names,
    };
    for eq in &mut flat.equations {
        eq.residual = rewriter.rewrite_expression(&eq.residual);
    }
    for eq in &mut flat.initial_equations {
        eq.residual = rewriter.rewrite_expression(&eq.residual);
    }
    for var in flat.variables.values_mut() {
        if let Some(binding) = &mut var.binding {
            *binding = rewriter.rewrite_expression(binding);
        }
        if let Some(start) = &mut var.start {
            *start = rewriter.rewrite_expression(start);
        }
        if let Some(min) = &mut var.min {
            *min = rewriter.rewrite_expression(min);
        }
        if let Some(max) = &mut var.max {
            *max = rewriter.rewrite_expression(max);
        }
        if let Some(nominal) = &mut var.nominal {
            *nominal = rewriter.rewrite_expression(nominal);
        }
    }
    for function in flat.functions.values_mut() {
        for statement in &mut function.body {
            *statement = rewriter.rewrite_statement(statement);
        }
    }
}

struct OverexpandedFunctionCallProjector {
    signatures: HashMap<String, Vec<rumoca_core::FunctionParam>>,
    flat_variable_names: HashSet<String>,
}

impl ExpressionRewriter for OverexpandedFunctionCallProjector {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        let rewritten_args = self.rewrite_expressions(args);
        if *is_constructor {
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args: rewritten_args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        let args = self
            .signatures
            .get(name.as_str())
            .and_then(|inputs| {
                project_actuals_to_input_slots_with_known_vars(
                    &rewritten_args,
                    inputs,
                    &self.flat_variable_names,
                )
            })
            .unwrap_or(rewritten_args);
        rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args,
            is_constructor: *is_constructor,
            span: *span,
        }
    }
}

impl StatementRewriter for OverexpandedFunctionCallProjector {}

#[cfg(test)]
fn project_actuals_to_input_slots(
    actuals: &[rumoca_core::Expression],
    inputs: &[rumoca_core::FunctionParam],
) -> Option<Vec<rumoca_core::Expression>> {
    project_actuals_to_input_slots_with_known_vars(actuals, inputs, &HashSet::new())
}

fn project_actuals_to_input_slots_with_known_vars(
    actuals: &[rumoca_core::Expression],
    inputs: &[rumoca_core::FunctionParam],
    flat_variable_names: &HashSet<String>,
) -> Option<Vec<rumoca_core::Expression>> {
    if actuals.len() < inputs.len() || actuals.iter().any(is_named_function_arg_marker) {
        return None;
    }
    project_mixed_decomposed_input_slots(actuals, inputs, flat_variable_names)
        .or_else(|| project_field_access_actuals(actuals, inputs))
        .or_else(|| {
            let mut projected = Vec::new();
            for input in inputs {
                let actual = actuals
                    .iter()
                    .find(|actual| actual_leaf_matches_input(actual, &input.name))?;
                projected.push(actual.clone());
            }
            Some(projected)
        })
}

enum InputSlot<'a> {
    Scalar(&'a rumoca_core::FunctionParam),
    Decomposed(&'a [rumoca_core::FunctionParam]),
}

fn project_mixed_decomposed_input_slots(
    actuals: &[rumoca_core::Expression],
    inputs: &[rumoca_core::FunctionParam],
    flat_variable_names: &HashSet<String>,
) -> Option<Vec<rumoca_core::Expression>> {
    let slots = decomposed_input_slots(inputs);
    if !slots
        .iter()
        .any(|slot| matches!(slot, InputSlot::Decomposed(_)))
    {
        return None;
    }

    let mut projected = Vec::new();
    let mut cursor = 0;
    let mut used_prefixes = HashSet::<String>::new();
    for slot in slots {
        match slot {
            InputSlot::Scalar(input) => {
                if let Some(actual) = actuals.get(cursor) {
                    projected.push(actual.clone());
                    cursor += 1;
                } else if input.default.is_some() {
                    continue;
                } else {
                    return None;
                }
            }
            InputSlot::Decomposed(fields) => {
                let (prefix, fields, consumed) = project_actuals_to_decomposed_input_slot(
                    &actuals[cursor..],
                    fields,
                    &used_prefixes,
                    flat_variable_names,
                )?;
                projected.extend(fields);
                cursor += consumed;
                used_prefixes.insert(prefix);
            }
        }
    }

    (projected.len() <= inputs.len()
        && inputs[projected.len()..]
            .iter()
            .all(|input| input.default.is_some()))
    .then_some(projected)
}

fn decomposed_input_slots(inputs: &[rumoca_core::FunctionParam]) -> Vec<InputSlot<'_>> {
    let mut slots = Vec::new();
    let mut idx = 0;
    while idx < inputs.len() {
        let Some((prefix, _)) = decomposed_input_prefix_and_leaf(&inputs[idx].name) else {
            slots.push(InputSlot::Scalar(&inputs[idx]));
            idx += 1;
            continue;
        };

        let mut end = idx + 1;
        while end < inputs.len()
            && decomposed_input_prefix_and_leaf(&inputs[end].name)
                .is_some_and(|(candidate, _)| candidate == prefix)
        {
            end += 1;
        }

        if end - idx > 1 {
            slots.push(InputSlot::Decomposed(&inputs[idx..end]));
        } else {
            slots.push(InputSlot::Scalar(&inputs[idx]));
        }
        idx = end;
    }
    slots
}

fn decomposed_input_prefix_and_leaf(input_name: &str) -> Option<(&str, &str)> {
    let (prefix, leaf) = input_name.rsplit_once('_')?;
    (!prefix.is_empty() && !leaf.is_empty()).then_some((prefix, leaf))
}

fn project_actuals_to_decomposed_input_slot(
    actuals: &[rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
    used_prefixes: &HashSet<String>,
    flat_variable_names: &HashSet<String>,
) -> Option<(String, Vec<rumoca_core::Expression>, usize)> {
    if let Some(projected) =
        project_scalar_base_plus_tail_decomposed_slot(actuals, fields, flat_variable_names)
    {
        return Some(projected);
    }

    let blocks = actual_field_blocks(actuals, fields);
    if let Some(selected) = blocks
        .iter()
        .find(|block| !used_prefixes.contains(&block.prefix) && block_has_all_fields(block, fields))
        .or_else(|| {
            blocks
                .iter()
                .find(|block| block_has_all_fields(block, fields))
        })
    {
        let mut projected = Vec::new();
        for field in fields {
            let expected_leaf = decomposed_input_field_leaf(&field.name);
            let (_, actual) = selected
                .fields
                .iter()
                .find(|(actual_leaf, _)| function_field_names_match(expected_leaf, actual_leaf))?;
            projected.push((*actual).clone());
        }
        return Some((selected.prefix.clone(), projected, selected.end));
    }

    project_positional_scalar_decomposed_slot(actuals, fields)
}

fn project_scalar_base_plus_tail_decomposed_slot(
    actuals: &[rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
    flat_variable_names: &HashSet<String>,
) -> Option<(String, Vec<rumoca_core::Expression>, usize)> {
    if fields.len() < 2 || actuals.len() < (fields.len() * 2) - 1 {
        return None;
    }

    let first_base = scalar_var_field_block_base(actuals.get(..fields.len())?, fields)?;
    if !flat_variable_names.contains(&first_base.0) {
        return None;
    }

    let tail_len = fields.len() - 1;
    let tail = actuals.get(fields.len()..fields.len() + tail_len)?;
    let mut projected = Vec::with_capacity(fields.len());
    projected.push(first_base.1);
    projected.extend(tail.iter().cloned());
    Some((first_base.0, projected, fields.len() + tail_len))
}

fn scalar_var_field_block_base(
    field_actuals: &[rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
) -> Option<(String, rumoca_core::Expression)> {
    let mut base_name = None::<String>;
    let mut base_expr = None::<rumoca_core::Expression>;
    for (actual, field) in field_actuals.iter().zip(fields) {
        let expected_leaf = decomposed_input_field_leaf(&field.name);
        let (name, base) = scalar_var_field_base(actual, expected_leaf)?;
        if base_name
            .as_deref()
            .is_some_and(|existing| existing != name.as_str())
        {
            return None;
        }
        base_name.get_or_insert(name);
        base_expr.get_or_insert(base);
    }
    Some((base_name?, base_expr?))
}

fn scalar_var_field_base(
    actual: &rumoca_core::Expression,
    expected_leaf: &str,
) -> Option<(String, rumoca_core::Expression)> {
    match actual {
        rumoca_core::Expression::FieldAccess {
            base,
            field: actual_field,
            ..
        } => {
            if !function_field_names_match(expected_leaf, actual_field) {
                return None;
            }
            let rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } = base.as_ref()
            else {
                return None;
            };
            if !subscripts.is_empty() {
                return None;
            }
            Some((name.as_str().to_string(), base.as_ref().clone()))
        }
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } => {
            if !subscripts.is_empty() {
                return None;
            }
            let prefix = name
                .as_str()
                .strip_suffix(&format!(".{expected_leaf}"))
                .or_else(|| name.as_str().strip_suffix(&format!("_{expected_leaf}")))?;
            if prefix.is_empty() {
                return None;
            }
            Some((prefix.to_string(), flat_var_ref_expression(prefix, *span)))
        }
        _ => None,
    }
}

fn flat_var_ref_expression(name: &str, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            name.to_string(),
            rumoca_core::ComponentReference::from_flat_segments(name, span, None),
        ),
        subscripts: vec![],
        span,
    }
}

fn project_positional_scalar_decomposed_slot(
    actuals: &[rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
) -> Option<(String, Vec<rumoca_core::Expression>, usize)> {
    if actuals.len() <= fields.len() {
        return None;
    }
    let field_actuals = actuals.get(..fields.len())?;
    if field_actuals
        .iter()
        .any(|actual| actual_leaf_name(actual).is_some())
    {
        return None;
    }
    Some((
        format!("__positional_decomposed_{}", fields[0].name),
        field_actuals.to_vec(),
        fields.len(),
    ))
}

struct ActualFieldBlock<'a> {
    prefix: String,
    end: usize,
    fields: Vec<(String, &'a rumoca_core::Expression)>,
}

fn actual_field_blocks<'a>(
    actuals: &'a [rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
) -> Vec<ActualFieldBlock<'a>> {
    let mut blocks = Vec::new();
    let mut idx = 0;
    while idx < actuals.len() {
        let Some((prefix, leaf)) = actual_record_field(&actuals[idx], fields) else {
            idx += 1;
            continue;
        };
        let mut block = ActualFieldBlock {
            prefix,
            end: idx + 1,
            fields: vec![(leaf, &actuals[idx])],
        };
        idx += 1;
        while idx < actuals.len() {
            let Some((candidate_prefix, candidate_leaf)) =
                actual_record_field(&actuals[idx], fields)
            else {
                break;
            };
            if candidate_prefix != block.prefix {
                break;
            }
            block.fields.push((candidate_leaf, &actuals[idx]));
            block.end = idx + 1;
            idx += 1;
        }
        blocks.push(block);
    }
    blocks
}

fn actual_record_field(
    actual: &rumoca_core::Expression,
    fields: &[rumoca_core::FunctionParam],
) -> Option<(String, String)> {
    if let Some(field) = flattened_var_ref_field(actual, fields) {
        return Some(field);
    }
    let rumoca_core::Expression::FieldAccess {
        base,
        field: actual_field,
        ..
    } = actual
    else {
        return None;
    };
    let expected_leaf = fields.iter().find_map(|field| {
        let leaf = decomposed_input_field_leaf(&field.name);
        function_field_names_match(leaf, actual_field).then_some(leaf)
    })?;
    let prefix =
        expression_summary_for_projection(base).unwrap_or_else(|| format!("{:?}", base.as_ref()));
    Some((prefix, expected_leaf.to_string()))
}

fn flattened_var_ref_field(
    actual: &rumoca_core::Expression,
    fields: &[rumoca_core::FunctionParam],
) -> Option<(String, String)> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = actual
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    for field in fields {
        let expected_leaf = decomposed_input_field_leaf(&field.name);
        if let Some(prefix) = name.as_str().strip_suffix(&format!(".{expected_leaf}"))
            && !prefix.is_empty()
        {
            return Some((prefix.to_string(), expected_leaf.to_string()));
        }
        if let Some(prefix) = name.as_str().strip_suffix(&format!("_{expected_leaf}"))
            && !prefix.is_empty()
        {
            return Some((prefix.to_string(), expected_leaf.to_string()));
        }
    }
    None
}

fn block_has_all_fields(
    block: &ActualFieldBlock<'_>,
    fields: &[rumoca_core::FunctionParam],
) -> bool {
    fields.iter().all(|field| {
        let expected_leaf = decomposed_input_field_leaf(&field.name);
        block
            .fields
            .iter()
            .any(|(actual_leaf, _)| function_field_names_match(expected_leaf, actual_leaf))
    })
}

fn decomposed_input_field_leaf(input_name: &str) -> &str {
    input_name
        .rsplit_once('_')
        .map(|(_, leaf)| leaf)
        .unwrap_or(input_name)
}

fn is_named_function_arg_marker(arg: &rumoca_core::Expression) -> bool {
    matches!(
        arg,
        rumoca_core::Expression::FunctionCall { name, .. }
            if name.as_str().starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
    )
}

fn actual_leaf_matches_input(actual: &rumoca_core::Expression, input_name: &str) -> bool {
    let Some(leaf) = actual_leaf_name(actual) else {
        return false;
    };
    function_field_names_match(input_name, leaf)
}

fn actual_leaf_name(actual: &rumoca_core::Expression) -> Option<&str> {
    match actual {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => name.as_str().rsplit(['.', '_']).next(),
        rumoca_core::Expression::FieldAccess { field, .. } => Some(field.as_str()),
        _ => None,
    }
}

struct DecomposedParam {
    original_index: usize,
    param_name: String,
    type_name: String,
    fields: Vec<rumoca_core::FunctionParam>,
    already_decomposed: bool,
}

fn record_param_shape_source(param: &DecomposedParam) -> Option<String> {
    param
        .fields
        .iter()
        .find(|field| field.dims.is_empty() && field.shape_expr.is_empty())
        .or_else(|| param.fields.first())
        .map(|field| format!("{}_{}", param.param_name, field.name))
}

fn decomposed_record_field_param(
    param_name: &str,
    original_param: &rumoca_core::FunctionParam,
    field: &rumoca_core::FunctionParam,
) -> rumoca_core::FunctionParam {
    let mut param = field.clone();
    param.name = format!("{}_{}", param_name, field.name);
    param.dims = original_param
        .dims
        .iter()
        .chain(field.dims.iter())
        .copied()
        .collect();
    param.shape_expr = original_param
        .shape_expr
        .iter()
        .chain(field.shape_expr.iter())
        .cloned()
        .collect();
    param
}

fn named_constructor_arg<'a>(
    ctor_args: &'a [rumoca_core::Expression],
    field: &str,
) -> Option<&'a rumoca_core::Expression> {
    for arg in ctor_args {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            ..
        } = arg
            && name.as_str().strip_prefix("__rumoca_named_arg__.") == Some(field)
        {
            return args.first();
        }
    }
    None
}

fn decompose_record_call_args_in_stmt(
    stmt: &mut rumoca_core::Statement,
    map: &HashMap<String, Vec<DecomposedParam>>,
    local_record_params: Option<&HashSet<String>>,
    flat_variable_names: &HashSet<rumoca_core::VarName>,
    constructor_input_names_by_type: &HashMap<String, Vec<String>>,
) -> Result<(), FlattenError> {
    let mut decomposer = RecordCallArgDecomposer {
        map,
        local_record_params,
        flat_variable_names,
        constructor_input_names_by_type,
        error: None,
    };
    *stmt = decomposer.rewrite_statement(stmt);
    match decomposer.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

fn decompose_record_call_args_in_expr(
    expr: &mut rumoca_core::Expression,
    map: &HashMap<String, Vec<DecomposedParam>>,
    local_record_params: Option<&HashSet<String>>,
    flat_variable_names: &HashSet<rumoca_core::VarName>,
    constructor_input_names_by_type: &HashMap<String, Vec<String>>,
) -> Result<(), FlattenError> {
    let mut decomposer = RecordCallArgDecomposer {
        map,
        local_record_params,
        flat_variable_names,
        constructor_input_names_by_type,
        error: None,
    };
    *expr = decomposer.rewrite_expression(expr);
    match decomposer.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

struct RecordCallArgDecomposer<'a> {
    map: &'a HashMap<String, Vec<DecomposedParam>>,
    local_record_params: Option<&'a HashSet<String>>,
    flat_variable_names: &'a HashSet<rumoca_core::VarName>,
    constructor_input_names_by_type: &'a HashMap<String, Vec<String>>,
    error: Option<FlattenError>,
}

impl RecordCallArgDecomposer<'_> {
    fn args_for_call(
        &mut self,
        name: &rumoca_core::Reference,
        rewritten_args: Vec<rumoca_core::Expression>,
    ) -> Vec<rumoca_core::Expression> {
        let Some(decomposed) = self.map.get(name.as_str()) else {
            return rewritten_args;
        };
        match decompose_record_call_args(
            name.as_str(),
            &rewritten_args,
            decomposed,
            self.local_record_params,
            self.flat_variable_names,
            self.constructor_input_names_by_type,
        ) {
            Ok(args) => args,
            Err(error) => {
                self.error.get_or_insert(error);
                rewritten_args
            }
        }
    }
}

impl ExpressionRewriter for RecordCallArgDecomposer<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        {
            let rewritten_args = self.rewrite_expressions(args);
            let args = self.args_for_call(name, rewritten_args);
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}

impl StatementRewriter for RecordCallArgDecomposer<'_> {}

fn decompose_record_call_args(
    function_name: &str,
    old_args: &[rumoca_core::Expression],
    decomposed: &[DecomposedParam],
    local_record_params: Option<&HashSet<String>>,
    flat_variable_names: &HashSet<rumoca_core::VarName>,
    constructor_input_names_by_type: &HashMap<String, Vec<String>>,
) -> Result<Vec<rumoca_core::Expression>, FlattenError> {
    if let Some(args) =
        project_already_decomposed_call_args(old_args, decomposed, flat_variable_names)
    {
        return Ok(args);
    }
    if decomposed.len() == 1
        && let Some(args) =
            project_decomposed_scalar_actuals(old_args, &decomposed[0].fields, flat_variable_names)
    {
        return Ok(args);
    }

    let mut args = Vec::new();
    let mut old_idx = 0;
    let mut consumed = HashSet::new();
    let decomposed_param_names = decomposed
        .iter()
        .map(|param| param.param_name.as_str())
        .collect::<HashSet<_>>();
    let has_multiple_decomposed_params = decomposed.len() > 1;
    for dp in decomposed {
        while old_idx < dp.original_index && old_idx < old_args.len() {
            if !consumed.contains(&old_idx)
                && !is_named_arg_for_any(old_args.get(old_idx), &decomposed_param_names)
            {
                args.push(old_args[old_idx].clone());
            }
            old_idx += 1;
        }
        if let Some((arg_idx, value)) = named_function_arg_value(old_args, &dp.param_name) {
            let as_named_fields =
                has_multiple_decomposed_params || output_args_have_named_slots(&args);
            expand_record_arg(
                function_name,
                &dp.param_name,
                value,
                &dp.type_name,
                &dp.fields,
                local_record_params,
                flat_variable_names,
                constructor_input_names_by_type,
                as_named_fields,
                &mut args,
            )?;
            consumed.insert(arg_idx);
        } else if old_idx < old_args.len() {
            while old_idx < old_args.len()
                && is_named_arg_for_any(old_args.get(old_idx), &decomposed_param_names)
            {
                old_idx += 1;
            }
            if old_idx >= old_args.len() {
                continue;
            }
            let as_named_fields = output_args_have_named_slots(&args);
            expand_record_arg(
                function_name,
                &dp.param_name,
                &old_args[old_idx],
                &dp.type_name,
                &dp.fields,
                local_record_params,
                flat_variable_names,
                constructor_input_names_by_type,
                as_named_fields,
                &mut args,
            )?;
            consumed.insert(old_idx);
            old_idx += 1;
        }
    }
    while old_idx < old_args.len() {
        if !consumed.contains(&old_idx) {
            args.push(old_args[old_idx].clone());
        }
        old_idx += 1;
    }
    Ok(args)
}

fn infer_existing_decomposed_params(
    func: &rumoca_core::Function,
    record_constructor_fields: &[(String, Vec<rumoca_core::FunctionParam>)],
) -> Vec<DecomposedParam> {
    if func.is_constructor {
        return Vec::new();
    }
    let body_uses = flattened_record_field_uses(func);
    let mut by_prefix: HashMap<String, DecomposedParam> = HashMap::new();

    for (idx, input) in func.inputs.iter().enumerate() {
        for (type_name, fields) in record_constructor_fields
            .iter()
            .filter(|(_, fields)| !fields.is_empty())
        {
            let Some((prefix, _)) = split_flattened_record_input(&input.name, fields) else {
                continue;
            };
            let used_fields = body_uses.get(prefix.as_str());
            let projected_fields = fields
                .iter()
                .filter(|field| {
                    func.inputs
                        .iter()
                        .any(|input| input.name == format!("{prefix}_{}", field.name))
                        || used_fields.is_some_and(|used| used.contains(field.name.as_str()))
                })
                .cloned()
                .collect::<Vec<_>>();
            if projected_fields.is_empty() {
                continue;
            }
            by_prefix
                .entry(prefix.clone())
                .and_modify(|param| {
                    param.original_index = param.original_index.min(idx);
                    param.fields = merge_record_fields(&param.fields, &projected_fields);
                })
                .or_insert_with(|| DecomposedParam {
                    original_index: idx,
                    param_name: prefix,
                    type_name: type_name.clone(),
                    fields: projected_fields,
                    already_decomposed: true,
                });
        }
    }

    let mut decomposed = by_prefix.into_values().collect::<Vec<_>>();
    decomposed.sort_by_key(|param| param.original_index);
    decomposed
}

fn split_flattened_record_input(
    input_name: &str,
    fields: &[rumoca_core::FunctionParam],
) -> Option<(String, String)> {
    fields.iter().find_map(|field| {
        let prefix = input_name.strip_suffix(&format!("_{}", field.name))?;
        (!prefix.is_empty()).then(|| (prefix.to_string(), field.name.clone()))
    })
}

fn merge_record_fields(
    existing: &[rumoca_core::FunctionParam],
    additional: &[rumoca_core::FunctionParam],
) -> Vec<rumoca_core::FunctionParam> {
    let mut merged = existing.to_vec();
    for field in additional {
        if !merged.iter().any(|candidate| candidate.name == field.name) {
            merged.push(field.clone());
        }
    }
    merged
}

fn rewrite_existing_decomposed_inputs(
    func: &mut rumoca_core::Function,
    decomposed: &[DecomposedParam],
) {
    let old_inputs = std::mem::take(&mut func.inputs);
    let mut idx = 0;
    while idx < old_inputs.len() {
        let Some(param) = decomposed.iter().find(|param| param.original_index == idx) else {
            func.inputs.push(old_inputs[idx].clone());
            idx += 1;
            continue;
        };
        for field in &param.fields {
            let name = format!("{}_{}", param.param_name, field.name);
            if let Some(existing) = old_inputs.iter().find(|input| input.name == name) {
                func.inputs.push(existing.clone());
            } else {
                func.inputs.push(flattened_record_field_param(&name, field));
            }
        }
        idx += old_inputs[idx..]
            .iter()
            .take_while(|input| {
                param
                    .fields
                    .iter()
                    .any(|field| input.name == format!("{}_{}", param.param_name, field.name))
            })
            .count()
            .max(1);
    }
}

fn flattened_record_field_param(
    name: &str,
    field: &rumoca_core::FunctionParam,
) -> rumoca_core::FunctionParam {
    let mut input = field.clone();
    input.name = name.to_string();
    input
}

fn flattened_record_field_uses(func: &rumoca_core::Function) -> HashMap<String, HashSet<String>> {
    let mut collector = FlattenedRecordFieldUseCollector {
        fields: HashMap::new(),
    };
    for statement in &func.body {
        collect_statement_record_field_uses(statement, &mut collector);
    }
    collector.fields
}

struct FlattenedRecordFieldUseCollector {
    fields: HashMap<String, HashSet<String>>,
}

impl ExpressionVisitor for FlattenedRecordFieldUseCollector {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if subscripts.is_empty()
            && let Some((prefix, field)) = name.as_str().split_once('_')
            && !prefix.is_empty()
            && !field.is_empty()
        {
            self.fields
                .entry(prefix.to_string())
                .or_default()
                .insert(field.to_string());
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

fn collect_statement_record_field_uses(
    statement: &rumoca_core::Statement,
    collector: &mut FlattenedRecordFieldUseCollector,
) {
    match statement {
        rumoca_core::Statement::Assignment { value, .. }
        | rumoca_core::Statement::Reinit { value, .. } => collector.visit_expression(value),
        rumoca_core::Statement::FunctionCall { args, .. } => {
            for arg in args {
                collector.visit_expression(arg);
            }
        }
        rumoca_core::Statement::For { equations, .. } => {
            for statement in equations {
                collect_statement_record_field_uses(statement, collector);
            }
        }
        rumoca_core::Statement::While { block, .. } => {
            collector.visit_expression(&block.cond);
            for statement in &block.stmts {
                collect_statement_record_field_uses(statement, collector);
            }
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                collector.visit_expression(&block.cond);
                for statement in &block.stmts {
                    collect_statement_record_field_uses(statement, collector);
                }
            }
            if let Some(else_block) = else_block {
                for statement in else_block {
                    collect_statement_record_field_uses(statement, collector);
                }
            }
        }
        rumoca_core::Statement::When { blocks, .. } => {
            for block in blocks {
                collector.visit_expression(&block.cond);
                for statement in &block.stmts {
                    collect_statement_record_field_uses(statement, collector);
                }
            }
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            collector.visit_expression(condition);
            collector.visit_expression(message);
            if let Some(level) = level {
                collector.visit_expression(level);
            }
        }
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => {}
    }
}

fn project_already_decomposed_call_args(
    old_args: &[rumoca_core::Expression],
    decomposed: &[DecomposedParam],
    flat_variable_names: &HashSet<rumoca_core::VarName>,
) -> Option<Vec<rumoca_core::Expression>> {
    if !decomposed.iter().any(|param| param.already_decomposed) {
        return None;
    }
    let mut args = old_args.to_vec();
    let mut changed = false;
    for (param_idx, dp) in decomposed.iter().enumerate() {
        if !dp.already_decomposed {
            continue;
        }
        let end = decomposed
            .iter()
            .skip(param_idx + 1)
            .map(|param| param.original_index)
            .find(|next_index| *next_index > dp.original_index)
            .unwrap_or(args.len());
        let available_end = end.min(args.len());
        let slice = args.get(dp.original_index..available_end)?;
        if let Some(projected) =
            project_decomposed_scalar_actuals(slice, &dp.fields, flat_variable_names)
        {
            args.splice(dp.original_index..available_end, projected);
            changed = true;
        }
    }
    changed.then_some(args)
}

fn project_decomposed_scalar_actuals(
    actuals: &[rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
    flat_variable_names: &HashSet<rumoca_core::VarName>,
) -> Option<Vec<rumoca_core::Expression>> {
    if let Some(projected) = project_flattened_var_ref_actuals(actuals, fields) {
        return Some(projected);
    }
    if let Some(projected) = project_field_access_actuals(actuals, fields) {
        return Some(projected);
    }
    if actuals.is_empty() || actuals.len() > fields.len() {
        return None;
    }
    let base = matching_scalar_constructor_base(actuals, fields)?;
    let rumoca_core::Expression::FunctionCall {
        args: constructor_args,
        ..
    } = base
    else {
        return None;
    };
    let positional = constructor_args.iter().collect::<Vec<_>>();
    constructor_positional_args_project_expected_fields(&positional, fields, flat_variable_names)
        .or_else(|| project_fields_from_common_base(base, fields))
}

fn project_field_access_actuals(
    actuals: &[rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
) -> Option<Vec<rumoca_core::Expression>> {
    let mut bases = Vec::<(String, rumoca_core::Expression)>::new();
    for actual in actuals {
        let rumoca_core::Expression::FieldAccess { .. } = actual else {
            continue;
        };
        for base_expr in projection_base_candidates(actual, fields) {
            let base_key = expression_summary_for_projection(base_expr)?;
            if !bases.iter().any(|(existing, _)| existing == &base_key) {
                bases.push((base_key, base_expr.clone()));
            }
        }
    }
    bases.sort_by_key(|(base, _)| base.matches('.').count());

    for (base, base_expr) in bases {
        let mut projected = Vec::new();
        for field in fields {
            let actual = actuals.iter().find(|actual| {
                let rumoca_core::Expression::FieldAccess {
                    base: actual_base,
                    field: actual_field,
                    ..
                } = actual
                else {
                    return false;
                };
                expression_summary_for_projection(actual_base).as_deref() == Some(base.as_str())
                    && function_field_names_match(&field.name, actual_field)
            });
            if let Some(actual) = actual {
                projected.push(actual.clone());
            } else if let Some(field_name) = projected_field_name(&field.name) {
                let Some(span) = base_expr
                    .span()
                    .or_else(|| (!field.span.is_dummy()).then_some(field.span))
                else {
                    projected.clear();
                    break;
                };
                projected.push(rumoca_core::Expression::FieldAccess {
                    base: Box::new(base_expr.clone()),
                    field: field_name.to_string(),
                    span,
                });
            } else {
                projected.clear();
                break;
            }
        }
        if projected.len() == fields.len() {
            return Some(projected);
        }
    }
    None
}

fn projection_base_candidates<'a>(
    actual: &'a rumoca_core::Expression,
    fields: &[rumoca_core::FunctionParam],
) -> Vec<&'a rumoca_core::Expression> {
    let mut candidates = Vec::new();
    let rumoca_core::Expression::FieldAccess { base, .. } = actual else {
        return candidates;
    };
    candidates.push(base.as_ref());

    let mut current = base.as_ref();
    while let rumoca_core::Expression::FieldAccess {
        base: parent,
        field,
        ..
    } = current
    {
        if fields
            .iter()
            .any(|expected| function_field_names_match(&expected.name, field))
        {
            candidates.push(parent.as_ref());
        }
        current = parent.as_ref();
    }
    candidates
}

fn projected_field_name(input_or_field_name: &str) -> Option<&str> {
    input_or_field_name
        .rsplit_once('_')
        .map(|(_, suffix)| suffix)
        .or(Some(input_or_field_name))
}

fn expression_summary_for_projection(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => Some(format!(
            "{}{}",
            name.as_str(),
            render_projection_subscripts(subscripts)?
        )),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => Some(format!(
            "{}{}",
            expression_summary_for_projection(base)?,
            render_projection_subscripts(subscripts)?
        )),
        rumoca_core::Expression::FieldAccess { base, field, .. } => Some(format!(
            "{}.{}",
            expression_summary_for_projection(base)?,
            field
        )),
        _ => None,
    }
}

fn render_projection_subscripts(subscripts: &[rumoca_core::Subscript]) -> Option<String> {
    let mut rendered = String::new();
    for subscript in subscripts {
        let rumoca_core::Subscript::Index { value, .. } = subscript else {
            return None;
        };
        rendered.push('[');
        rendered.push_str(&value.to_string());
        rendered.push(']');
    }
    Some(rendered)
}

fn function_field_names_match(expected: &str, actual: &str) -> bool {
    expected == actual
        || expected
            .rsplit_once('_')
            .is_some_and(|(_, suffix)| suffix == actual)
}

fn project_flattened_var_ref_actuals(
    actuals: &[rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
) -> Option<Vec<rumoca_core::Expression>> {
    let mut prefix: Option<String> = None;
    let mut projected = Vec::new();
    for field in fields {
        let (actual_prefix, actual) = actuals.iter().find_map(|actual| {
            let rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } = actual
            else {
                return None;
            };
            if !subscripts.is_empty() {
                return None;
            }
            let (candidate_prefix, candidate_field) =
                split_flattened_record_input(name.as_str(), fields)?;
            function_field_names_match(&field.name, &candidate_field)
                .then_some((candidate_prefix, actual))
        })?;
        if let Some(existing_prefix) = &prefix {
            if existing_prefix != &actual_prefix {
                return None;
            }
        } else {
            prefix = Some(actual_prefix);
        }
        projected.push(actual.clone());
    }
    Some(projected)
}

fn matching_scalar_constructor_base<'a>(
    actuals: &'a [rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
) -> Option<&'a rumoca_core::Expression> {
    let mut base = None;
    for (actual, field) in actuals.iter().zip(fields.iter()) {
        let rumoca_core::Expression::FieldAccess {
            base: current_base,
            field: current_field,
            ..
        } = actual
        else {
            return None;
        };
        if current_field != &field.name {
            return None;
        }
        if let Some(existing_base) = base {
            if existing_base != current_base.as_ref() {
                return None;
            }
        } else {
            base = Some(current_base.as_ref());
        }
    }
    base
}

fn project_fields_from_common_base(
    base: &rumoca_core::Expression,
    fields: &[rumoca_core::FunctionParam],
) -> Option<Vec<rumoca_core::Expression>> {
    let span = base.span()?;
    Some(
        fields
            .iter()
            .map(|field| rumoca_core::Expression::FieldAccess {
                base: Box::new(base.clone()),
                field: field.name.clone(),
                span,
            })
            .collect(),
    )
}

fn is_named_arg_for_any(
    arg: Option<&rumoca_core::Expression>,
    param_names: &HashSet<&str>,
) -> bool {
    named_function_arg_name(arg).is_some_and(|name| param_names.contains(name))
}

fn output_args_have_named_slots(args: &[rumoca_core::Expression]) -> bool {
    args.iter()
        .any(|arg| named_function_arg_name(Some(arg)).is_some())
}

fn named_function_arg_name(arg: Option<&rumoca_core::Expression>) -> Option<&str> {
    let rumoca_core::Expression::FunctionCall { name, .. } = arg? else {
        return None;
    };
    name.as_str().strip_prefix("__rumoca_named_arg__.")
}

fn named_function_arg_value<'a>(
    args: &'a [rumoca_core::Expression],
    param_name: &str,
) -> Option<(usize, &'a rumoca_core::Expression)> {
    args.iter().enumerate().find_map(|(idx, arg)| {
        let rumoca_core::Expression::FunctionCall {
            name,
            args: named_args,
            ..
        } = arg
        else {
            return None;
        };
        (name.as_str().strip_prefix("__rumoca_named_arg__.") == Some(param_name))
            .then(|| named_args.first().map(|value| (idx, value)))
            .flatten()
    })
}

/// Expand a record argument into scalar field arguments.
fn expand_record_arg(
    function_name: &str,
    param_name: &str,
    arg: &rumoca_core::Expression,
    expected_type_name: &str,
    fields: &[rumoca_core::FunctionParam],
    local_record_params: Option<&HashSet<String>>,
    flat_variable_names: &HashSet<rumoca_core::VarName>,
    constructor_input_names_by_type: &HashMap<String, Vec<String>>,
    as_named_fields: bool,
    out: &mut Vec<rumoca_core::Expression>,
) -> Result<(), FlattenError> {
    // Constructor call Complex(re, im) → extract positional/named args
    if let rumoca_core::Expression::FunctionCall {
        name,
        args: ctor_args,
        is_constructor: true,
        span,
        ..
    } = arg
    {
        let positional: Vec<&rumoca_core::Expression> = ctor_args
            .iter()
            .filter(|a| {
                !matches!(a, rumoca_core::Expression::FunctionCall { is_constructor: true, name, .. }
                if name.as_str().starts_with("__rumoca_named_arg__."))
            })
            .collect();
        let constructor_matches_expected =
            rumoca_core::qualified_type_name_matches(name.as_str(), expected_type_name);
        if constructor_matches_expected
            && let Some(record_value) =
                record_value_constructor_proxy(ctor_args, param_name, fields)
        {
            if let Some(values) = record_value_constructor_proxy_project_fields(
                record_value,
                fields,
                constructor_input_names_by_type,
            ) {
                for (field, value) in fields.iter().zip(values) {
                    push_expanded_record_field_arg(out, param_name, field, as_named_fields, value);
                }
                return Ok(());
            }
            for field in fields {
                let source_span = record_field_access_source_span(record_value, field)?;
                push_expanded_record_field_arg(
                    out,
                    param_name,
                    field,
                    as_named_fields,
                    rumoca_core::Expression::FieldAccess {
                        base: Box::new(record_value.clone()),
                        field: field.name.clone(),
                        span: source_span,
                    },
                );
            }
            return Ok(());
        }
        if constructor_matches_expected
            && positional.len() == 1
            && let Some(values) = record_value_constructor_proxy_project_fields(
                positional[0],
                fields,
                constructor_input_names_by_type,
            )
        {
            for (field, value) in fields.iter().zip(values) {
                push_expanded_record_field_arg(out, param_name, field, as_named_fields, value);
            }
            return Ok(());
        }
        if !constructor_matches_expected
            && constructor_positional_args_match_fields(&positional, fields)
        {
            for (field, value) in fields.iter().zip(positional.iter()) {
                push_expanded_record_field_arg(
                    out,
                    param_name,
                    field,
                    as_named_fields,
                    (*value).clone(),
                );
            }
            return Ok(());
        }
        if !constructor_matches_expected
            && let Some(values) = constructor_positional_args_project_expected_fields(
                &positional,
                fields,
                flat_variable_names,
            )
        {
            for (field, value) in fields.iter().zip(values) {
                push_expanded_record_field_arg(out, param_name, field, as_named_fields, value);
            }
            return Ok(());
        }

        for (i, field) in fields.iter().enumerate() {
            let named = named_constructor_arg(ctor_args, field.name.as_str());
            if let Some(val) = named {
                push_expanded_record_field_arg(
                    out,
                    param_name,
                    field,
                    as_named_fields,
                    val.clone(),
                );
            } else if constructor_matches_expected && i < positional.len() {
                push_expanded_record_field_arg(
                    out,
                    param_name,
                    field,
                    as_named_fields,
                    positional[i].clone(),
                );
            } else if let Some(default) = &field.default {
                push_expanded_record_field_arg(
                    out,
                    param_name,
                    field,
                    as_named_fields,
                    default.clone(),
                );
            } else if !constructor_matches_expected {
                let source_span = record_field_access_source_span(arg, field)?;
                push_expanded_record_field_arg(
                    out,
                    param_name,
                    field,
                    as_named_fields,
                    rumoca_core::Expression::FieldAccess {
                        base: Box::new(arg.clone()),
                        field: field.name.clone(),
                        span: source_span,
                    },
                );
            } else {
                return Err(missing_record_constructor_field_error(
                    function_name,
                    field.name.as_str(),
                    *span,
                ));
            }
        }
        return Ok(());
    }

    // Variable reference → emit field VarRefs
    if let rumoca_core::Expression::VarRef { name, span, .. } = arg {
        if local_record_params.is_some_and(|params| params.contains(name.as_str())) {
            for field in fields {
                push_expanded_record_field_arg(
                    out,
                    param_name,
                    field,
                    as_named_fields,
                    record_param_field_var_ref(name.as_str(), field.name.as_str(), *span),
                );
            }
            return Ok(());
        }

        for field in fields {
            push_expanded_record_field_arg(
                out,
                param_name,
                field,
                as_named_fields,
                rumoca_core::Expression::VarRef {
                    name: record_field_reference(name, field.name.as_str(), *span),
                    subscripts: vec![],
                    span: *span,
                },
            );
        }
        return Ok(());
    }

    // General expression → emit FieldAccess
    for field in fields {
        let source_span = record_field_access_source_span(arg, field)?;
        push_expanded_record_field_arg(
            out,
            param_name,
            field,
            as_named_fields,
            rumoca_core::Expression::FieldAccess {
                base: Box::new(arg.clone()),
                field: field.name.clone(),
                span: source_span,
            },
        );
    }
    Ok(())
}

fn constructor_positional_args_match_fields(
    positional: &[&rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
) -> bool {
    positional.len() == fields.len()
        && !fields.is_empty()
        && positional
            .iter()
            .zip(fields.iter())
            .all(|(arg, field)| expression_leaf_name(arg) == Some(field.name.as_str()))
}

fn constructor_positional_args_project_expected_fields(
    positional: &[&rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
    flat_variable_names: &HashSet<rumoca_core::VarName>,
) -> Option<Vec<rumoca_core::Expression>> {
    if positional.is_empty() || fields.is_empty() {
        return None;
    }
    if let Some(values) = constructor_positional_args_project_from_matching_ref(
        positional,
        fields,
        flat_variable_names,
    ) {
        return Some(values);
    }
    let prefix = common_record_field_prefix(positional)
        .or_else(|| matching_record_field_prefix(positional, fields, flat_variable_names))?;
    let span = positional.iter().find_map(|expr| expr.span())?;
    fields
        .iter()
        .map(|field| {
            let name = format!("{prefix}.{}", field.name);
            let var_name = rumoca_core::VarName::new(&name);
            flat_variable_names
                .contains(&var_name)
                .then(|| rumoca_core::Expression::VarRef {
                    name: record_field_projected_reference(&prefix, field.name.as_str(), span),
                    subscripts: vec![],
                    span,
                })
        })
        .collect()
}

fn constructor_positional_args_project_from_matching_ref(
    positional: &[&rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
    flat_variable_names: &HashSet<rumoca_core::VarName>,
) -> Option<Vec<rumoca_core::Expression>> {
    positional
        .iter()
        .find_map(|arg| projected_values_from_matching_ref(arg, fields, flat_variable_names))
}

fn projected_values_from_matching_ref(
    arg: &rumoca_core::Expression,
    fields: &[rumoca_core::FunctionParam],
    flat_variable_names: &HashSet<rumoca_core::VarName>,
) -> Option<Vec<rumoca_core::Expression>> {
    let rumoca_core::Expression::VarRef { name, span, .. } = arg else {
        return None;
    };
    let component_ref = name.component_ref()?;
    let leaf = component_ref.parts.last()?.ident.as_str();
    if !fields.iter().any(|field| field.name.as_str() == leaf) {
        return None;
    }
    let prefix_len = component_ref.parts.len().checked_sub(1)?;
    let prefix_parts = component_ref.parts[..prefix_len].to_vec();
    if prefix_parts.is_empty() {
        return None;
    }
    let projected_names = fields
        .iter()
        .map(|field| {
            let mut parts = prefix_parts.clone();
            parts.push(rumoca_core::ComponentRefPart {
                ident: field.name.clone(),
                span: field.span,
                subs: Vec::new(),
            });
            let projected_ref = rumoca_core::ComponentReference {
                local: component_ref.local,
                span: component_ref.span,
                parts,
                def_id: None,
            };
            projected_ref.to_var_name()
        })
        .collect::<Vec<_>>();
    if !projected_names
        .iter()
        .all(|name| flat_variable_names.contains(name))
    {
        return None;
    }
    Some(
        projected_names
            .iter()
            .map(|projected_name| rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    projected_name.as_str().to_string(),
                    rumoca_core::ComponentReference::from_flat_segments(
                        projected_name.as_str(),
                        *span,
                        None,
                    ),
                ),
                subscripts: vec![],
                span: *span,
            })
            .collect(),
    )
}

fn matching_record_field_prefix(
    positional: &[&rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
    _flat_variable_names: &HashSet<rumoca_core::VarName>,
) -> Option<String> {
    positional.iter().find_map(|arg| {
        let prefix = expression_field_prefix(arg)?;
        let leaf = expression_leaf_name(arg)?;
        fields
            .iter()
            .any(|field| field.name.as_str() == leaf)
            .then_some(prefix)
    })
}

fn common_record_field_prefix(positional: &[&rumoca_core::Expression]) -> Option<String> {
    let mut prefixes = positional.iter().map(|arg| expression_field_prefix(arg));
    let first = prefixes.next()??;
    prefixes
        .all(|prefix| prefix.as_deref() == Some(first.as_str()))
        .then_some(first)
}

fn expression_field_prefix(expr: &rumoca_core::Expression) -> Option<String> {
    let rumoca_core::Expression::VarRef { name, .. } = expr else {
        return None;
    };
    if let Some(component_ref) = name.component_ref() {
        let field_count = component_ref.parts.len().checked_sub(1)?;
        let prefix_parts = component_ref.parts[..field_count]
            .iter()
            .map(render_component_ref_part)
            .collect::<Option<Vec<_>>>()?;
        return (!prefix_parts.is_empty()).then(|| prefix_parts.join("."));
    }
    flat_var_name_prefix_text(name.as_str()).map(str::to_string)
}

fn render_component_ref_part(part: &rumoca_core::ComponentRefPart) -> Option<String> {
    let mut rendered = part.ident.clone();
    for subscript in &part.subs {
        match subscript {
            rumoca_core::Subscript::Index { value, .. } => {
                rendered.push('[');
                rendered.push_str(&value.to_string());
                rendered.push(']');
            }
            rumoca_core::Subscript::Colon { .. } | rumoca_core::Subscript::Expr { .. } => {
                return None;
            }
        }
    }
    Some(rendered)
}

fn record_field_projected_reference(
    prefix: &str,
    field: &str,
    span: rumoca_core::Span,
) -> rumoca_core::Reference {
    let name = format!("{prefix}.{field}");
    let mut parts = rumoca_core::ComponentPath::from_flat_path(prefix)
        .parts()
        .iter()
        .map(|part| rumoca_core::ComponentRefPart {
            ident: (*part).to_string(),
            span,
            subs: Vec::new(),
        })
        .collect::<Vec<_>>();
    parts.push(rumoca_core::ComponentRefPart {
        ident: field.to_string(),
        span,
        subs: Vec::new(),
    });
    rumoca_core::Reference::with_component_reference(
        name,
        rumoca_core::ComponentReference {
            local: false,
            span,
            parts,
            def_id: None,
        },
    )
}

fn push_expanded_record_field_arg(
    out: &mut Vec<rumoca_core::Expression>,
    param_name: &str,
    field: &rumoca_core::FunctionParam,
    as_named_field: bool,
    value: rumoca_core::Expression,
) {
    if as_named_field {
        let span = value.span().unwrap_or(field.span);
        out.push(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new(format!(
                "{}{param_name}_{}",
                rumoca_core::NAMED_FUNCTION_ARG_PREFIX,
                field.name
            )),
            args: vec![value],
            is_constructor: true,
            span,
        });
    } else {
        out.push(value);
    }
}

fn record_value_constructor_proxy<'a>(
    ctor_args: &'a [rumoca_core::Expression],
    param_name: &str,
    fields: &[rumoca_core::FunctionParam],
) -> Option<&'a rumoca_core::Expression> {
    if fields.len() <= 1 || ctor_args.iter().any(named_function_arg_name_expr) {
        return None;
    }
    let [record_value] = ctor_args else {
        return None;
    };
    (expression_leaf_name(record_value) == Some(param_name)).then_some(record_value)
}

fn record_value_constructor_proxy_project_fields(
    record_value: &rumoca_core::Expression,
    fields: &[rumoca_core::FunctionParam],
    constructor_input_names_by_type: &HashMap<String, Vec<String>>,
) -> Option<Vec<rumoca_core::Expression>> {
    let rumoca_core::Expression::FieldAccess {
        base,
        field: record_field,
        span,
    } = record_value
    else {
        return None;
    };
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
        ..
    } = base.as_ref()
    else {
        return None;
    };
    let constructor_fields = constructor_input_names_by_type.get(name.as_str())?;
    fields
        .iter()
        .map(|field| {
            let projected = format!("{record_field}_{}", field.name);
            constructor_projected_actual(args, constructor_fields, &projected).or_else(|| {
                constructor_fields.contains(&projected).then(|| {
                    rumoca_core::Expression::FieldAccess {
                        base: base.clone(),
                        field: projected,
                        span: *span,
                    }
                })
            })
        })
        .collect()
}

fn constructor_projected_actual(
    args: &[rumoca_core::Expression],
    constructor_fields: &[String],
    projected: &str,
) -> Option<rumoca_core::Expression> {
    if let Some(value) = named_constructor_arg(args, projected) {
        return Some(value.clone());
    }
    let index = constructor_fields
        .iter()
        .position(|field| field.as_str() == projected)?;
    args.iter()
        .filter(|arg| named_function_arg_name(Some(arg)).is_none())
        .nth(index)
        .cloned()
}

fn named_function_arg_name_expr(arg: &rumoca_core::Expression) -> bool {
    named_function_arg_name(Some(arg)).is_some()
}

fn expression_leaf_name(expr: &rumoca_core::Expression) -> Option<&str> {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => name
            .component_ref()
            .and_then(|component_ref| component_ref.parts.last())
            .map(|part| part.ident.as_str())
            .or_else(|| flat_var_name_leaf_text(name.as_str())),
        rumoca_core::Expression::Index { base, .. } => expression_leaf_name(base),
        rumoca_core::Expression::FieldAccess { field, .. } => Some(field.as_str()),
        _ => None,
    }
}

fn flat_var_name_prefix_text(name: &str) -> Option<&str> {
    let dot = name.rfind('.')?;
    Some(&name[..dot])
}

fn flat_var_name_leaf_text(name: &str) -> Option<&str> {
    let dot = name.rfind('.')?;
    Some(&name[dot + 1..])
}

fn missing_record_constructor_field_error(
    function_name: &str,
    field: &str,
    span: rumoca_core::Span,
) -> FlattenError {
    if span.is_dummy() {
        return FlattenError::missing_source_context(format!(
            "record constructor argument `{field}` for `{function_name}` has no source span"
        ));
    }
    FlattenError::invalid_function_call_args(
        function_name,
        format!("missing record constructor field `{field}` after record parameter lowering"),
        span,
    )
}

fn record_field_access_source_span(
    arg: &rumoca_core::Expression,
    field: &rumoca_core::FunctionParam,
) -> Result<rumoca_core::Span, FlattenError> {
    arg.span()
        .or_else(|| (!field.span.is_dummy()).then_some(field.span))
        .ok_or_else(|| {
            FlattenError::missing_source_context(format!(
                "record field `{}` access has no source span",
                field.name
            ))
        })
}

fn record_field_reference(
    base: &rumoca_core::Reference,
    field: &str,
    span: rumoca_core::Span,
) -> rumoca_core::Reference {
    let name = format!("{}.{}", base.as_str(), field);
    let Some(mut component_ref) = base.component_ref().cloned() else {
        return rumoca_core::Reference::new(name);
    };
    component_ref.parts.push(rumoca_core::ComponentRefPart {
        ident: field.to_string(),
        span,
        subs: Vec::new(),
    });
    rumoca_core::Reference::with_component_reference(name, component_ref)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ClassType, Literal, Span, VarName};

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("function_lowering_test.mo"),
            1,
            2,
        )
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: vec![],
            span: Span::DUMMY,
        }
    }

    fn int_lit(value: i64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span: Span::DUMMY,
        }
    }

    fn field_access(base: rumoca_core::Expression, field: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::FieldAccess {
            base: Box::new(base),
            field: field.to_string(),
            span: test_span(),
        }
    }

    fn assignment_to(name: &str, value: rumoca_core::Expression) -> rumoca_core::Statement {
        rumoca_core::Statement::Assignment {
            comp: rumoca_core::ComponentReference {
                local: false,
                span: Span::DUMMY,
                parts: vec![rumoca_core::ComponentRefPart {
                    ident: name.to_string(),
                    span: Span::DUMMY,
                    subs: vec![],
                }],
                def_id: None,
            },
            value,
            span: Span::DUMMY,
        }
    }

    fn component_ref_expr(parts: &[&str]) -> rumoca_core::Expression {
        let span = test_span();
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                parts.join("."),
                rumoca_core::ComponentReference {
                    local: false,
                    span,
                    parts: parts
                        .iter()
                        .map(|part| rumoca_core::ComponentRefPart {
                            ident: (*part).to_string(),
                            span,
                            subs: Vec::new(),
                        })
                        .collect(),
                    def_id: None,
                },
            ),
            subscripts: vec![],
            span,
        }
    }

    fn named_arg(name: &str, value: rumoca_core::Expression) -> rumoca_core::Expression {
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new(format!("__rumoca_named_arg__.{name}")),
            args: vec![value],
            is_constructor: true,
            span: Span::DUMMY,
        }
    }

    fn named_arg_var_ref<'a>(
        arg: &'a rumoca_core::Expression,
        slot: &str,
    ) -> Option<&'a rumoca_core::Reference> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            ..
        } = arg
        else {
            return None;
        };
        if name.as_str().strip_prefix("__rumoca_named_arg__.") != Some(slot) {
            return None;
        }
        let rumoca_core::Expression::VarRef { name, .. } = args.first()? else {
            return None;
        };
        Some(name)
    }

    fn record_constructor_named(name: &str, fields: &[&str]) -> rumoca_core::Function {
        let mut constructor = rumoca_core::Function::new(name, Span::DUMMY);
        constructor.is_constructor = true;
        for field in fields {
            constructor.add_input(rumoca_core::FunctionParam::new(*field, "Real", test_span()));
        }
        constructor
    }

    fn record_constructor() -> rumoca_core::Function {
        let mut constructor = rumoca_core::Function::new("Pkg.Record", Span::DUMMY);
        constructor.is_constructor = true;
        constructor.add_input(rumoca_core::FunctionParam::new("a", "Real", test_span()));
        constructor.add_input(
            rumoca_core::FunctionParam::new("b", "Real", test_span()).with_dims(vec![3]),
        );
        constructor
    }

    fn function_with_record_input() -> rumoca_core::Function {
        let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("r", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        function.body.push(assignment_to(
            "y",
            rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("r")),
                field: "a".to_string(),
                span: Span::DUMMY,
            },
        ));
        function
    }

    #[test]
    fn record_param_lowering_uses_constructor_signature_metadata() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());
        flat.add_function(function_with_record_input());
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![var_ref("rec")],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let function = flat
            .functions
            .get(&VarName::new("Pkg.f"))
            .expect("function remains");
        let input_names = function
            .inputs
            .iter()
            .map(|input| input.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(input_names, vec!["r_a", "r_b"]);
        assert_eq!(function.inputs[0].dims, Vec::<i64>::new());
        assert_eq!(function.inputs[1].dims, vec![3]);
        let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
            panic!("expected assignment");
        };
        assert!(matches!(
            value,
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "r_a"
        ));
        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.a"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.b"
        ));
    }

    #[test]
    fn record_param_lowering_expands_record_actual_for_already_decomposed_signature() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
        function.add_input(rumoca_core::FunctionParam::new("r_a", "Real", test_span()));
        function.add_input(
            rumoca_core::FunctionParam::new("r_b", "Real", test_span()).with_dims(vec![3]),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        function.body.push(assignment_to("y", var_ref("r_a")));
        flat.add_function(function);

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![var_ref("rec")],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.a"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.b"
        ));
    }

    #[test]
    fn record_param_lowering_projects_overcomplete_decomposed_actuals() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
        function.add_input(rumoca_core::FunctionParam::new("r_a", "Real", test_span()));
        function.add_input(
            rumoca_core::FunctionParam::new("r_b", "Real", test_span()).with_dims(vec![3]),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        function.body.push(assignment_to("y", var_ref("r_a")));
        flat.add_function(function);

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![
                    var_ref("rec_phase"),
                    var_ref("rec_a"),
                    var_ref("rec_b"),
                    var_ref("rec_extra"),
                ],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec_a"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec_b"
        ));
    }

    #[test]
    fn record_param_lowering_projects_mixed_scalar_and_decomposed_slots() {
        let mut flat = flat::Model::new();

        let mut function = rumoca_core::Function::new("Pkg.setSmoothState", Span::DUMMY);
        for input in [
            "x",
            "state_a_p",
            "state_a_T",
            "state_a_X",
            "state_b_p",
            "state_b_T",
            "state_b_X",
            "x_small",
        ] {
            function.add_input(rumoca_core::FunctionParam::new(input, "Real", test_span()));
        }
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        function.body.push(assignment_to("y", var_ref("x")));
        let inputs = function.inputs.clone();
        flat.add_function(function);

        let call_args = vec![
            var_ref("m_flow_ext"),
            var_ref("state1.p"),
            var_ref("state1.T"),
            var_ref("state1.X"),
            var_ref("state1.T"),
            var_ref("state1.X"),
            var_ref("state1.p"),
            var_ref("state1.T"),
            var_ref("state1.X"),
            var_ref("state1.X"),
            var_ref("state2.p"),
            var_ref("state2.T"),
            var_ref("state2.X"),
            var_ref("state2.T"),
            var_ref("state2.X"),
            var_ref("x_small_actual"),
        ];
        assert_eq!(
            project_actuals_to_input_slots(&call_args, &inputs)
                .expect("overexpanded actuals should project")
                .len(),
            8
        );

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.setSmoothState"),
                args: call_args,
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        let arg_names = args
            .iter()
            .map(|arg| match arg {
                rumoca_core::Expression::VarRef { name, .. } => name.as_str(),
                _ => panic!("expected var ref"),
            })
            .collect::<Vec<_>>();
        assert_eq!(
            arg_names,
            vec![
                "m_flow_ext",
                "state1.p",
                "state1.T",
                "state1.X",
                "state2.p",
                "state2.T",
                "state2.X",
                "x_small_actual"
            ]
        );
    }

    #[test]
    fn record_param_lowering_projects_field_access_decomposed_slot_with_scalar_tail() {
        let mut flat = flat::Model::new();

        let mut function = rumoca_core::Function::new("Pkg.brushVoltageDrop", Span::DUMMY);
        for input in ["brushParameters_V", "brushParameters_ILinear", "i"] {
            function.add_input(rumoca_core::FunctionParam::new(input, "Real", test_span()));
        }
        function.add_output(rumoca_core::FunctionParam::new("v", "Real", test_span()));
        function.body.push(assignment_to("v", var_ref("i")));
        let inputs = function.inputs.clone();
        flat.add_function(function);

        let call_args = vec![
            field_access(int_lit(0), "V"),
            field_access(int_lit(0), "ILinear"),
            var_ref("computed_current"),
            var_ref("dcpm.IaNominal"),
        ];
        assert_eq!(
            project_actuals_to_input_slots(&call_args, &inputs)
                .expect("overexpanded field-access actuals should project")
                .len(),
            3
        );

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.brushVoltageDrop"),
                args: call_args,
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 3);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::FieldAccess { field, .. } if field == "V"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::FieldAccess { field, .. } if field == "ILinear"
        ));
        assert!(matches!(
            &args[2],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "computed_current"
        ));
    }

    #[test]
    fn record_param_lowering_projects_positional_scalar_decomposed_slot() {
        let mut flat = flat::Model::new();

        let mut function = rumoca_core::Function::new("Pkg.abs", Span::DUMMY);
        for input in ["c_re", "c_im"] {
            function.add_input(rumoca_core::FunctionParam::new(input, "Real", test_span()));
        }
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        function.body.push(assignment_to("y", var_ref("c_re")));
        let inputs = function.inputs.clone();
        flat.add_function(function);

        let call_args = vec![
            int_lit(1),
            int_lit(0),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Complex"),
                args: vec![int_lit(1), int_lit(0)],
                is_constructor: true,
                span: Span::DUMMY,
            },
        ];
        assert_eq!(
            project_actuals_to_input_slots(&call_args, &inputs)
                .expect("overexpanded scalar actuals should project")
                .len(),
            2
        );

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.abs"),
                args: call_args,
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                ..
            }
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(0),
                ..
            }
        ));
    }

    #[test]
    fn record_param_lowering_projects_scalar_constructor_fields_without_fake_members() {
        let mut flat = flat::Model::new();
        flat.variables.insert(
            VarName::new("P"),
            flat::Variable::empty_with_span(test_span()),
        );
        flat.variables.insert(
            VarName::new("Q"),
            flat::Variable::empty_with_span(test_span()),
        );

        let mut function = rumoca_core::Function::new("Pkg.arg", Span::DUMMY);
        for input in ["c_re", "c_im"] {
            function.add_input(rumoca_core::FunctionParam::new(input, "Real", test_span()));
        }
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        function.body.push(assignment_to("y", var_ref("c_re")));
        flat.add_function(function);

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.arg"),
                args: vec![
                    field_access(var_ref("P"), "re"),
                    field_access(var_ref("P"), "im"),
                    var_ref("Q"),
                ],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "P"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "Q"
        ));
    }

    #[test]
    fn record_param_lowering_projects_flattened_scalar_constructor_fields_with_default_tail() {
        let mut flat = flat::Model::new();
        flat.variables.insert(
            VarName::new("voltageSource.P"),
            flat::Variable::empty_with_span(test_span()),
        );
        flat.variables.insert(
            VarName::new("voltageSource.Q"),
            flat::Variable::empty_with_span(test_span()),
        );

        let mut function = rumoca_core::Function::new("Modelica.ComplexMath.arg", Span::DUMMY);
        function.add_input(rumoca_core::FunctionParam::new("c_re", "Real", test_span()));
        function.add_input(rumoca_core::FunctionParam::new("c_im", "Real", test_span()));
        function.add_input(
            rumoca_core::FunctionParam::new("phi0", "Real", test_span()).with_default(int_lit(0)),
        );
        function.add_output(rumoca_core::FunctionParam::new("phi", "Real", test_span()));
        function.body.push(assignment_to("phi", var_ref("c_re")));
        flat.add_function(function);

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Modelica.ComplexMath.arg"),
                args: vec![
                    var_ref("voltageSource.P.re"),
                    var_ref("voltageSource.P.im"),
                    var_ref("voltageSource.Q"),
                ],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "voltageSource.P"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "voltageSource.Q"
        ));
    }

    #[test]
    fn record_param_lowering_projects_repeated_field_access_actuals() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named(
            "Pkg.Record",
            &["phase", "h", "d", "T", "p"],
        ));

        let mut function = rumoca_core::Function::new("Pkg.pressure", Span::DUMMY);
        for field in ["phase", "h", "d", "T", "p"] {
            function.add_input(rumoca_core::FunctionParam::new(
                format!("state_{field}"),
                "Real",
                test_span(),
            ));
        }
        function.add_output(rumoca_core::FunctionParam::new("p", "Real", test_span()));
        function.body.push(assignment_to("p", var_ref("state_p")));
        flat.add_function(function);

        let field_access =
            |base: rumoca_core::Expression, field: &str| rumoca_core::Expression::FieldAccess {
                base: Box::new(base),
                field: field.to_string(),
                span: test_span(),
            };
        let source_state = field_access(var_ref("source"), "state");
        let source_state_phase = field_access(source_state.clone(), "phase");
        let source_state_phase_phase = field_access(source_state_phase.clone(), "phase");

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.pressure"),
                args: vec![
                    field_access(source_state_phase_phase.clone(), "phase"),
                    field_access(source_state_phase_phase.clone(), "h"),
                    field_access(source_state_phase_phase.clone(), "d"),
                    field_access(source_state_phase_phase.clone(), "T"),
                    field_access(source_state_phase_phase.clone(), "p"),
                    field_access(source_state_phase.clone(), "h"),
                    field_access(source_state_phase.clone(), "d"),
                    field_access(source_state_phase.clone(), "T"),
                    field_access(source_state_phase.clone(), "p"),
                    field_access(source_state.clone(), "h"),
                    field_access(source_state.clone(), "d"),
                    field_access(source_state.clone(), "T"),
                    field_access(source_state, "p"),
                ],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 5);
        assert_eq!(
            expression_summary_for_projection(&args[0]).as_deref(),
            Some("source.state.phase")
        );
    }

    #[test]
    fn record_param_lowering_projects_repeated_field_access_actuals_from_indexed_base() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named(
            "Pkg.Record",
            &["phase", "h", "d", "T", "p"],
        ));

        let mut function = rumoca_core::Function::new("Pkg.pressure", Span::DUMMY);
        for field in ["phase", "h", "d", "T", "p"] {
            function.add_input(rumoca_core::FunctionParam::new(
                format!("state_{field}"),
                "Real",
                test_span(),
            ));
        }
        function.add_output(rumoca_core::FunctionParam::new("p", "Real", test_span()));
        function.body.push(assignment_to("p", var_ref("state_p")));
        flat.add_function(function);

        let field_access =
            |base: rumoca_core::Expression, field: &str| rumoca_core::Expression::FieldAccess {
                base: Box::new(base),
                field: field.to_string(),
                span: test_span(),
            };
        let indexed_state = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pipe.flowModel.states"),
            subscripts: vec![rumoca_core::Subscript::Index {
                value: 1,
                span: test_span(),
            }],
            span: test_span(),
        };
        let state_phase = field_access(indexed_state.clone(), "phase");
        let state_phase_phase = field_access(state_phase.clone(), "phase");

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.pressure"),
                args: vec![
                    field_access(state_phase_phase.clone(), "phase"),
                    field_access(state_phase_phase.clone(), "h"),
                    field_access(state_phase_phase.clone(), "d"),
                    field_access(state_phase_phase.clone(), "T"),
                    field_access(state_phase_phase.clone(), "p"),
                    field_access(state_phase.clone(), "h"),
                    field_access(state_phase.clone(), "d"),
                    field_access(state_phase.clone(), "T"),
                    field_access(state_phase.clone(), "p"),
                    field_access(indexed_state.clone(), "h"),
                    field_access(indexed_state.clone(), "d"),
                    field_access(indexed_state.clone(), "T"),
                    field_access(indexed_state, "p"),
                ],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 5);
        assert_eq!(
            expression_summary_for_projection(&args[0]).as_deref(),
            Some("pipe.flowModel.states[1].phase")
        );
    }

    #[test]
    fn record_param_lowering_projects_overexpanded_field_chain_from_indexed_record_base() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named(
            "Pkg.Record",
            &["phase", "h", "d", "T", "p"],
        ));

        let mut function = rumoca_core::Function::new("Pkg.pressure", Span::DUMMY);
        for field in ["phase", "h", "d", "T", "p"] {
            function.add_input(rumoca_core::FunctionParam::new(
                format!("state_{field}"),
                "Real",
                test_span(),
            ));
        }
        function.add_output(rumoca_core::FunctionParam::new("p", "Real", test_span()));
        function.body.push(assignment_to("p", var_ref("state_p")));
        flat.add_function(function);

        let field_access =
            |base: rumoca_core::Expression, field: &str| rumoca_core::Expression::FieldAccess {
                base: Box::new(base),
                field: field.to_string(),
                span: test_span(),
            };
        let indexed_state = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pipe.flowModel.states"),
            subscripts: vec![rumoca_core::Subscript::Index {
                value: 1,
                span: test_span(),
            }],
            span: test_span(),
        };
        let state_phase = field_access(indexed_state, "phase");
        let state_phase_phase = field_access(state_phase, "phase");

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.pressure"),
                args: vec![
                    field_access(state_phase_phase.clone(), "phase"),
                    field_access(state_phase_phase.clone(), "h"),
                    field_access(state_phase_phase.clone(), "d"),
                    field_access(state_phase_phase.clone(), "T"),
                    field_access(state_phase_phase, "p"),
                ],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 5);
        assert_eq!(
            expression_summary_for_projection(&args[0]).as_deref(),
            Some("pipe.flowModel.states[1].phase")
        );
    }

    #[test]
    fn record_param_lowering_rewrites_variable_attribute_calls() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());
        flat.add_function(function_with_record_input());

        let mut variable = flat::Variable::empty_with_span(test_span());
        variable.name = VarName::new("x");
        variable.nominal = Some(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.f"),
            args: vec![var_ref("rec")],
            is_constructor: false,
            span: Span::DUMMY,
        });
        flat.add_variable(VarName::new("x"), variable);

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let variable = flat.variables.get(&VarName::new("x")).expect("variable");
        let Some(rumoca_core::Expression::FunctionCall { args, .. }) = &variable.nominal else {
            panic!("expected function call nominal");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.a"
        ));
    }

    #[test]
    fn record_param_lowering_expands_named_record_actual_value() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());
        flat.add_function(function_with_record_input());
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![named_arg("r", var_ref("rec"))],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.a"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.b"
        ));
    }

    #[test]
    fn record_param_lowering_does_not_positionally_expand_mismatched_constructor() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());
        flat.add_function(record_constructor_named("Pkg.OtherRecord", &["x", "y"]));
        flat.add_function(function_with_record_input());
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::new("Pkg.OtherRecord"),
                    args: vec![var_ref("first"), var_ref("second")],
                    is_constructor: true,
                    span: Span::DUMMY,
                }],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::FieldAccess { field, .. } if field == "a"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::FieldAccess { field, .. } if field == "b"
        ));
    }

    #[test]
    fn record_param_lowering_expands_single_record_value_constructor_proxy() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());
        flat.add_function(function_with_record_input());
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::new("Pkg.Record"),
                    args: vec![component_ref_expr(&["carrier", "r"])],
                    is_constructor: true,
                    span: test_span(),
                }],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::FieldAccess { base, field, .. }
                if field == "a"
                    && matches!(base.as_ref(), rumoca_core::Expression::VarRef { name, .. }
                        if name.as_str() == "carrier.r")
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::FieldAccess { base, field, .. }
                if field == "b"
                    && matches!(base.as_ref(), rumoca_core::Expression::VarRef { name, .. }
                        if name.as_str() == "carrier.r")
        ));
    }

    #[test]
    fn record_param_lowering_expands_multiple_named_record_actuals() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut function = rumoca_core::Function::new("Pkg.combine", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("left", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_input(
            rumoca_core::FunctionParam::new("right", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        function.body.push(assignment_to(
            "y",
            rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("left")),
                field: "a".to_string(),
                span: Span::DUMMY,
            },
        ));
        flat.add_function(function);
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.combine"),
                args: vec![
                    named_arg("right", var_ref("r2")),
                    named_arg("left", var_ref("r1")),
                ],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        let actuals = args
            .iter()
            .map(|arg| match arg {
                rumoca_core::Expression::FunctionCall {
                    name,
                    args,
                    is_constructor: true,
                    ..
                } => {
                    let value = match args.first() {
                        Some(rumoca_core::Expression::VarRef { name, .. }) => {
                            name.as_str().to_string()
                        }
                        other => format!("{other:?}"),
                    };
                    format!("{}={value}", name.as_str())
                }
                other => format!("{other:?}"),
            })
            .collect::<Vec<_>>();
        assert_eq!(
            actuals,
            vec![
                "__rumoca_named_arg__.left_a=r1.a",
                "__rumoca_named_arg__.left_b=r1.b",
                "__rumoca_named_arg__.right_a=r2.a",
                "__rumoca_named_arg__.right_b=r2.b"
            ]
        );
    }

    #[test]
    fn record_param_lowering_keeps_decomposed_positional_record_named_after_named_slots() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut function = rumoca_core::Function::new("Pkg.withScale", Span::DUMMY);
        function.add_input(rumoca_core::FunctionParam::new(
            "scale",
            "Real",
            test_span(),
        ));
        function.add_input(
            rumoca_core::FunctionParam::new("r", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        flat.add_function(function);
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.withScale"),
                args: vec![named_arg("scale", var_ref("gain")), var_ref("rec")],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 3);
        assert!(matches!(
            named_arg_var_ref(&args[0], "scale"),
            Some(name) if name.as_str() == "gain"
        ));
        assert!(matches!(
            named_arg_var_ref(&args[1], "r_a"),
            Some(name) if name.as_str() == "rec.a"
        ));
        assert!(matches!(
            named_arg_var_ref(&args[2], "r_b"),
            Some(name) if name.as_str() == "rec.b"
        ));
    }

    #[test]
    fn record_param_lowering_uses_mismatched_constructor_positional_field_values() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());
        flat.add_function(record_constructor_named("Pkg.OtherRecord", &["x", "y"]));

        let mut function = rumoca_core::Function::new("Pkg.withScale", Span::DUMMY);
        function.add_input(rumoca_core::FunctionParam::new(
            "scale",
            "Real",
            test_span(),
        ));
        function.add_input(
            rumoca_core::FunctionParam::new("r", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        flat.add_function(function);
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.withScale"),
                args: vec![
                    named_arg("scale", var_ref("gain")),
                    named_arg(
                        "r",
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::Reference::new("Pkg.OtherRecord"),
                            args: vec![
                                component_ref_expr(&["rec", "a"]),
                                component_ref_expr(&["rec", "b"]),
                            ],
                            is_constructor: true,
                            span: Span::DUMMY,
                        },
                    ),
                ],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 3);
        let actuals = args
            .iter()
            .map(|arg| match arg {
                rumoca_core::Expression::FunctionCall {
                    name,
                    args,
                    is_constructor: true,
                    ..
                } => {
                    let value = match args.first() {
                        Some(rumoca_core::Expression::VarRef { name, .. }) => {
                            name.as_str().to_string()
                        }
                        other => format!("{other:?}"),
                    };
                    format!("{}={value}", name.as_str())
                }
                other => format!("{other:?}"),
            })
            .collect::<Vec<_>>();
        assert_eq!(
            actuals,
            vec![
                "__rumoca_named_arg__.scale=gain",
                "__rumoca_named_arg__.r_a=rec.a",
                "__rumoca_named_arg__.r_b=rec.b"
            ]
        );
    }

    #[test]
    fn record_param_lowering_projects_mismatched_constructor_fields_from_flat_namespace() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named(
            "Pkg.EfficiencyParameters",
            &["V_flow", "eta"],
        ));
        flat.add_function(record_constructor_named(
            "Pkg.Generic",
            &["pressure_V_flow", "pressure_dp"],
        ));
        flat.add_variable(
            rumoca_core::VarName::new("per.hydraulicEfficiency.V_flow"),
            flat::Variable::empty_with_span(test_span()),
        );
        flat.add_variable(
            rumoca_core::VarName::new("per.hydraulicEfficiency.eta"),
            flat::Variable::empty_with_span(test_span()),
        );

        let mut function = rumoca_core::Function::new("Pkg.efficiency", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("per", "Pkg.EfficiencyParameters", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        flat.add_function(function);
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.efficiency"),
                args: vec![named_arg(
                    "per",
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::Reference::new("Pkg.Generic"),
                        args: vec![
                            component_ref_expr(&["per", "hydraulicEfficiency", "V_flow"]),
                            component_ref_expr(&["per", "hydraulicEfficiency", "dp"]),
                        ],
                        is_constructor: true,
                        span: Span::DUMMY,
                    },
                )],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        let flat_variable_names = flat.variables.keys().cloned().collect::<HashSet<_>>();
        let fields = record_fields_from_constructor_metadata(
            &flat.functions,
            "Pkg.EfficiencyParameters",
            None,
        )
        .expect("record fields");
        let probe_actuals = [
            component_ref_expr(&["per", "hydraulicEfficiency", "V_flow"]),
            component_ref_expr(&["per", "hydraulicEfficiency", "dp"]),
        ];
        let probe_refs = probe_actuals.iter().collect::<Vec<_>>();
        assert!(
            constructor_positional_args_project_expected_fields(
                &probe_refs,
                &fields,
                &flat_variable_names,
            )
            .is_some(),
            "direct projection helper should project from the visible field prefix"
        );

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "per.hydraulicEfficiency.V_flow"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "per.hydraulicEfficiency.eta"
        ));
    }

    #[test]
    fn record_param_lowering_does_not_synthesize_missing_fields_from_partial_prefix() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named("Medium.State", &["p", "T", "X"]));
        flat.add_function(record_constructor_named(
            "Medium.setState_phX",
            &["p", "h", "X"],
        ));
        flat.add_variable(
            rumoca_core::VarName::new("port_a.p"),
            flat::Variable::empty_with_span(test_span()),
        );
        flat.add_variable(
            rumoca_core::VarName::new("port_a.h_outflow"),
            flat::Variable::empty_with_span(test_span()),
        );
        flat.add_variable(
            rumoca_core::VarName::new("port_a.Xi_outflow"),
            flat::Variable::empty_with_span(test_span()),
        );

        let mut density = rumoca_core::Function::new("Medium.density", Span::DUMMY);
        density.add_input(
            rumoca_core::FunctionParam::new("state", "Medium.State", test_span())
                .with_type_class(ClassType::Record),
        );
        density.add_output(rumoca_core::FunctionParam::new("d", "Real", test_span()));
        flat.add_function(density);
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Medium.density"),
                args: vec![rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::new("Medium.setState_phX"),
                    args: vec![
                        component_ref_expr(&["port_a", "p"]),
                        component_ref_expr(&["port_a", "h_outflow"]),
                        component_ref_expr(&["port_a", "Xi_outflow"]),
                    ],
                    is_constructor: true,
                    span: test_span(),
                }],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 3);
        assert!(args.iter().all(|arg| {
            !matches!(
                arg,
                rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "port_a.T"
            )
        }));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::FieldAccess { base, field, .. }
                if field == "T"
            && matches!(base.as_ref(), rumoca_core::Expression::FunctionCall { name, .. }
                if name.as_str() == "Medium.setState_phX")
        ));
    }

    #[test]
    fn record_param_lowering_prefers_more_specific_constructor_metadata() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named(
            "Modelica.Media.Interfaces.PartialSimpleMedium.ThermodynamicState",
            &["p", "T"],
        ));
        flat.add_function(record_constructor_named(
            "Buildings.Media.Air.ThermodynamicState",
            &["p", "T", "X"],
        ));

        let mut function = rumoca_core::Function::new("Buildings.Media.Air.h", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("state", "ThermodynamicState", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("h", "Real", test_span()));
        function.body.push(assignment_to(
            "h",
            rumoca_core::Expression::Index {
                base: Box::new(rumoca_core::Expression::FieldAccess {
                    base: Box::new(var_ref("state")),
                    field: "X".to_string(),
                    span: test_span(),
                }),
                subscripts: vec![rumoca_core::Subscript::Index {
                    value: 1,
                    span: test_span(),
                }],
                span: test_span(),
            },
        ));
        flat.add_function(function);

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let function = flat
            .functions
            .get(&VarName::new("Buildings.Media.Air.h"))
            .expect("function remains");
        let input_names = function
            .inputs
            .iter()
            .map(|input| input.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(input_names, vec!["state_p", "state_T", "state_X"]);
        let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
            panic!("expected assignment");
        };
        assert!(matches!(
            value,
            rumoca_core::Expression::Index { base, .. }
                if matches!(base.as_ref(), rumoca_core::Expression::VarRef { name, .. }
                    if name.as_str() == "state_X")
        ));
    }

    #[test]
    fn record_param_lowering_keeps_unqualified_inherited_record_in_owner_context() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named(
            "Modelica.Media.Interfaces.PartialSimpleMedium.ThermodynamicState",
            &["p", "T"],
        ));
        flat.add_function(record_constructor_named(
            "Buildings.Media.Air.ThermodynamicState",
            &["p", "T", "X"],
        ));

        let mut function =
            rumoca_core::Function::new("Buildings.Media.Water.temperature", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("state", "ThermodynamicState", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("T", "Real", test_span()));
        function.body.push(assignment_to(
            "T",
            rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("state")),
                field: "T".to_string(),
                span: test_span(),
            },
        ));
        flat.add_function(function);

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let function = flat
            .functions
            .get(&VarName::new("Buildings.Media.Water.temperature"))
            .expect("function remains");
        let input_names = function
            .inputs
            .iter()
            .map(|input| input.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(input_names, vec!["state_p", "state_T"]);
    }

    #[test]
    fn record_param_lowering_projects_partial_mismatched_constructor_field_prefix() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named(
            "Pkg.EfficiencyParameters",
            &["V_flow", "eta"],
        ));
        flat.add_function(record_constructor_named(
            "Pkg.Generic",
            &["pressure_V_flow", "pressure_dp"],
        ));
        flat.add_variable(
            rumoca_core::VarName::new("per.pum[1].hydraulicEfficiency.V_flow"),
            flat::Variable::empty_with_span(test_span()),
        );
        flat.add_variable(
            rumoca_core::VarName::new("per.pum[1].hydraulicEfficiency.eta"),
            flat::Variable::empty_with_span(test_span()),
        );

        let mut function = rumoca_core::Function::new("Pkg.efficiency", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("per", "Pkg.EfficiencyParameters", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        flat.add_function(function);
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.efficiency"),
                args: vec![named_arg(
                    "per",
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::Reference::new("Pkg.Generic"),
                        args: vec![
                            var_ref("per.pum[1].hydraulicEfficiency.V_flow"),
                            rumoca_core::Expression::FieldAccess {
                                base: Box::new(rumoca_core::Expression::FunctionCall {
                                    name: rumoca_core::Reference::new("Pkg.EfficiencyParameters"),
                                    args: vec![
                                        named_arg("V_flow", var_ref("default_v")),
                                        named_arg("eta", var_ref("default_eta")),
                                    ],
                                    is_constructor: true,
                                    span: Span::DUMMY,
                                }),
                                field: "dp".to_string(),
                                span: test_span(),
                            },
                        ],
                        is_constructor: true,
                        span: Span::DUMMY,
                    },
                )],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "per.pum[1].hydraulicEfficiency.V_flow"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "per.pum[1].hydraulicEfficiency.eta"
        ));
    }

    #[test]
    fn constructor_projection_requires_all_projected_record_fields_to_exist() {
        let mut flat_variable_names = HashSet::new();
        flat_variable_names.insert(rumoca_core::VarName::new("userValve.port_a.p"));
        let fields = ["phase", "h", "d", "T", "p"]
            .into_iter()
            .map(|field| rumoca_core::FunctionParam::new(field, "Real", test_span()))
            .collect::<Vec<_>>();
        let actual = component_ref_expr(&["userValve", "port_a", "p"]);
        let actuals = [&actual];

        let projected = constructor_positional_args_project_expected_fields(
            &actuals,
            &fields,
            &flat_variable_names,
        );

        assert!(
            projected.is_none(),
            "a matching leaf like port_a.p must not imply that port_a is a complete record"
        );
    }

    #[test]
    fn record_param_lowering_projects_nested_constructor_proxy_fields() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named(
            "Pkg.EfficiencyParameters",
            &["V_flow", "eta"],
        ));
        flat.add_function(record_constructor_named(
            "Pkg.Generic",
            &[
                "pressure_V_flow",
                "pressure_dp",
                "hydraulicEfficiency_V_flow",
                "hydraulicEfficiency_eta",
            ],
        ));

        let mut function = rumoca_core::Function::new("Pkg.efficiency", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("per", "Pkg.EfficiencyParameters", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        flat.add_function(function);
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.efficiency"),
                args: vec![rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::new("Pkg.EfficiencyParameters"),
                    args: vec![rumoca_core::Expression::FieldAccess {
                        base: Box::new(rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::Reference::new("Pkg.Generic"),
                            args: vec![
                                var_ref("pressure_v"),
                                var_ref("pressure_dp"),
                                var_ref("hyd_v"),
                                var_ref("hyd_eta"),
                            ],
                            is_constructor: true,
                            span: Span::DUMMY,
                        }),
                        field: "hydraulicEfficiency".to_string(),
                        span: test_span(),
                    }],
                    is_constructor: true,
                    span: Span::DUMMY,
                }],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "hyd_v"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "hyd_eta"
        ));
    }

    #[test]
    fn record_array_param_lowering_rewrites_indexed_field_access() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut function = rumoca_core::Function::new("Pkg.sumA", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("r", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record)
                .with_dims(vec![0])
                .with_shape_expr(vec![rumoca_core::Subscript::colon(Span::DUMMY)]),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        function.body.push(assignment_to(
            "y",
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sum,
                args: vec![rumoca_core::Expression::FieldAccess {
                    base: Box::new(rumoca_core::Expression::Index {
                        base: Box::new(var_ref("r")),
                        subscripts: vec![rumoca_core::Subscript::colon(Span::DUMMY)],
                        span: Span::DUMMY,
                    }),
                    field: "a".to_string(),
                    span: Span::DUMMY,
                }],
                span: Span::DUMMY,
            },
        ));
        flat.add_function(function);

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let function = flat
            .functions
            .get(&VarName::new("Pkg.sumA"))
            .expect("function remains");
        let input_names = function
            .inputs
            .iter()
            .map(|input| (input.name.as_str(), input.dims.as_slice()))
            .collect::<Vec<_>>();
        assert_eq!(input_names, vec![("r_a", &[0][..]), ("r_b", &[0, 3][..])]);
        let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
            panic!("expected assignment");
        };
        let rumoca_core::Expression::BuiltinCall { args, .. } = value else {
            panic!("expected builtin call");
        };
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "r_a" && matches!(subscripts.as_slice(), [rumoca_core::Subscript::Colon { .. }])
        ));
    }

    #[test]
    fn record_array_param_lowering_rewrites_size_of_original_record_param() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut function = rumoca_core::Function::new("Pkg.rms", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("r", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record)
                .with_dims(vec![0])
                .with_shape_expr(vec![rumoca_core::Subscript::colon(Span::DUMMY)]),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        function.locals.push(rumoca_core::FunctionParam {
            def_id: None,
            name: "n".to_string(),
            type_name: "Integer".to_string(),
            default: Some(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Size,
                args: vec![
                    var_ref("r"),
                    rumoca_core::Expression::Literal {
                        value: Literal::Integer(1),
                        span: Span::DUMMY,
                    },
                ],
                span: Span::DUMMY,
            }),
            ..rumoca_core::FunctionParam::new("n", "Integer", test_span())
        });
        flat.add_function(function);

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let function = flat
            .functions
            .get(&VarName::new("Pkg.rms"))
            .expect("function remains");
        let Some(default) = function.locals[0].default.as_ref() else {
            panic!("expected local default");
        };
        let rumoca_core::Expression::BuiltinCall { args, .. } = default else {
            panic!("expected size builtin");
        };
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "r_a"
        ));
    }

    #[test]
    fn record_param_lowering_rewrites_size_of_record_field_ref() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut function = rumoca_core::Function::new("Pkg.sizeField", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("r", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        function.locals.push(rumoca_core::FunctionParam {
            def_id: None,
            name: "n".to_string(),
            type_name: "Integer".to_string(),
            default: Some(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Size,
                args: vec![
                    component_ref_expr(&["r", "b"]),
                    rumoca_core::Expression::Literal {
                        value: Literal::Integer(1),
                        span: Span::DUMMY,
                    },
                ],
                span: Span::DUMMY,
            }),
            ..rumoca_core::FunctionParam::new("n", "Integer", test_span())
        });
        flat.add_function(function);

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let function = flat
            .functions
            .get(&VarName::new("Pkg.sizeField"))
            .expect("function remains");
        let Some(default) = function.locals[0].default.as_ref() else {
            panic!("expected local default");
        };
        let rumoca_core::Expression::BuiltinCall { args, .. } = default else {
            panic!("expected size builtin");
        };
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "r_a"
        ));
    }

    #[test]
    fn record_param_lowering_leaves_unknown_record_metadata_unexpanded() {
        let mut flat = flat::Model::new();
        flat.add_function(function_with_record_input());
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![rumoca_core::Expression::Literal {
                    value: Literal::Real(1.0),
                    span: Span::DUMMY,
                }],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let function = flat
            .functions
            .get(&VarName::new("Pkg.f"))
            .expect("function remains");
        assert_eq!(function.inputs.len(), 1);
        assert_eq!(function.inputs[0].name, "r");
        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 1);
    }

    #[test]
    fn record_param_lowering_keeps_external_object_inputs_opaque() {
        let mut flat = flat::Model::new();
        let mut external_constructor = rumoca_core::Function::new("Pkg.ExternalTable", Span::DUMMY);
        external_constructor.is_constructor = true;
        external_constructor.add_input(rumoca_core::FunctionParam::new(
            "fileName",
            "String",
            test_span(),
        ));
        external_constructor.add_input(rumoca_core::FunctionParam::new(
            "tableName",
            "String",
            test_span(),
        ));
        flat.add_function(external_constructor);

        let mut external_reader = rumoca_core::Function::new("Pkg.getMin", Span::DUMMY);
        external_reader.add_input(
            rumoca_core::FunctionParam::new("tableID", "Pkg.ExternalTable", test_span())
                .with_type_class(ClassType::Class),
        );
        external_reader.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        flat.add_function(external_reader);
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.getMin"),
                args: vec![var_ref("tableID")],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat)
            .expect("opaque external object should not decompose");

        let function = flat
            .functions
            .get(&VarName::new("Pkg.getMin"))
            .expect("function remains");
        assert_eq!(function.inputs.len(), 1);
        assert_eq!(function.inputs[0].name, "tableID");
        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 1);
    }

    #[test]
    fn record_field_normalization_uses_structured_component_ref_parts() {
        let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("state", "Pkg.State", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        function
            .body
            .push(assignment_to("y", component_ref_expr(&["state", "x"])));

        rewrite_record_field_access_in_body(&mut function);

        let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
            panic!("expected assignment");
        };
        let rumoca_core::Expression::FieldAccess { base, field, .. } = value else {
            panic!("expected normalized field access, got {value:?}");
        };
        assert_eq!(field, "x");
        assert!(matches!(
            base.as_ref(),
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "state"
        ));
    }

    #[test]
    fn nested_record_param_call_uses_decomposed_caller_locals() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut callee = rumoca_core::Function::new("Pkg.g", Span::DUMMY);
        callee.add_input(
            rumoca_core::FunctionParam::new("r", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record),
        );
        callee.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        callee.body.push(assignment_to(
            "y",
            rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("r")),
                field: "a".to_string(),
                span: Span::DUMMY,
            },
        ));
        flat.add_function(callee);

        let mut caller = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
        caller.add_input(
            rumoca_core::FunctionParam::new("state", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record),
        );
        caller.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        caller.body.push(assignment_to(
            "y",
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.g"),
                args: vec![var_ref("state")],
                is_constructor: false,
                span: Span::DUMMY,
            },
        ));
        flat.add_function(caller);

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let function = flat
            .functions
            .get(&VarName::new("Pkg.f"))
            .expect("caller remains");
        let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
            panic!("expected assignment");
        };
        let rumoca_core::Expression::FunctionCall { args, .. } = value else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "state_a"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "state_b"
        ));
    }
}
