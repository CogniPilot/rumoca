//! Record parameter lowering for flattened functions.
//!
//! This module handles post-collection passes that transform function signatures
//! and call sites:
//! - Decomposing record-typed parameters into scalar fields
//! - Normalizing structured record-field component references
//! - Rewriting FieldAccess expressions on decomposed record params to direct VarRef

use rumoca_core::{ExpressionRewriter, StatementRewriter};
use rumoca_ir_flat as flat;
use std::collections::{HashMap, HashSet};

fn record_fields_from_constructor_metadata(
    functions: &flat::VarNameIndexMap<rumoca_core::Function>,
    type_name: &str,
) -> Option<Vec<rumoca_core::FunctionParam>> {
    functions
        .iter()
        .find(|(name, function)| {
            function.is_constructor
                && rumoca_core::qualified_type_name_matches(name.as_str(), type_name)
        })
        .map(|(_, function)| function.inputs.to_vec())
        .filter(|fields: &Vec<rumoca_core::FunctionParam>| !fields.is_empty())
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
pub(crate) fn lower_record_function_params(flat: &mut flat::Model) {
    let record_fields_by_type = flat
        .functions
        .values()
        .flat_map(|function| function.inputs.iter())
        .filter(|input| input.type_class == Some(rumoca_core::ClassType::Record))
        .filter_map(|input| {
            record_fields_from_constructor_metadata(&flat.functions, &input.type_name)
                .map(|fields| (input.type_name.clone(), fields))
        })
        .collect::<HashMap<_, _>>();

    let mut decomposition_map: HashMap<String, Vec<DecomposedParam>> = HashMap::new();
    let mut local_decomposed_params: HashMap<String, HashSet<String>> = HashMap::new();

    for (func_name, func) in flat.functions.iter_mut() {
        let mut decomposed: Vec<DecomposedParam> = Vec::new();
        for (idx, input) in func.inputs.iter().enumerate() {
            if let Some(fields) = record_fields_by_type.get(&input.type_name) {
                decomposed.push(DecomposedParam {
                    original_index: idx,
                    param_name: input.name.clone(),
                    type_name: input.type_name.clone(),
                    fields: fields.clone(),
                });
            }
        }
        if decomposed.is_empty() {
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
        return;
    }

    // Rewrite call sites in equations, variable bindings, and function bodies.
    for eq in &mut flat.equations {
        decompose_record_call_args_in_expr(&mut eq.residual, &decomposition_map, None);
    }
    for eq in &mut flat.initial_equations {
        decompose_record_call_args_in_expr(&mut eq.residual, &decomposition_map, None);
    }
    for var in flat.variables.values_mut() {
        if let Some(ref mut binding) = var.binding {
            decompose_record_call_args_in_expr(binding, &decomposition_map, None);
        }
        if let Some(ref mut start) = var.start {
            decompose_record_call_args_in_expr(start, &decomposition_map, None);
        }
    }
    for (func_name, func) in flat.functions.iter_mut() {
        let local_record_params = local_decomposed_params.get(func_name.as_str());
        for stmt in &mut func.body {
            decompose_record_call_args_in_stmt(stmt, &decomposition_map, local_record_params);
        }
    }
}

struct DecomposedParam {
    original_index: usize,
    param_name: String,
    type_name: String,
    fields: Vec<rumoca_core::FunctionParam>,
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
            span: rumoca_core::Span::DUMMY,
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
) {
    *stmt = RecordCallArgDecomposer {
        map,
        local_record_params,
    }
    .rewrite_statement(stmt);
}

fn decompose_record_call_args_in_expr(
    expr: &mut rumoca_core::Expression,
    map: &HashMap<String, Vec<DecomposedParam>>,
    local_record_params: Option<&HashSet<String>>,
) {
    *expr = RecordCallArgDecomposer {
        map,
        local_record_params,
    }
    .rewrite_expression(expr);
}

struct RecordCallArgDecomposer<'a> {
    map: &'a HashMap<String, Vec<DecomposedParam>>,
    local_record_params: Option<&'a HashSet<String>>,
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
            let args = if let Some(decomposed) = self.map.get(name.as_str()) {
                decompose_record_call_args(
                    name.as_str(),
                    &rewritten_args,
                    decomposed,
                    self.local_record_params,
                )
            } else {
                rewritten_args
            };
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
) -> Vec<rumoca_core::Expression> {
    let mut args = Vec::new();
    let mut old_idx = 0;
    let mut consumed = HashSet::new();
    for dp in decomposed {
        while old_idx < dp.original_index && old_idx < old_args.len() {
            if !consumed.contains(&old_idx) {
                args.push(old_args[old_idx].clone());
            }
            old_idx += 1;
        }
        if let Some((arg_idx, value)) = named_function_arg_value(old_args, &dp.param_name) {
            trace_record_decomposition(function_name, &dp.param_name, value, &dp.fields);
            expand_record_arg(
                value,
                &dp.type_name,
                &dp.fields,
                local_record_params,
                &mut args,
            );
            consumed.insert(arg_idx);
        } else if old_idx < old_args.len() {
            trace_record_decomposition(
                function_name,
                &dp.param_name,
                &old_args[old_idx],
                &dp.fields,
            );
            expand_record_arg(
                &old_args[old_idx],
                &dp.type_name,
                &dp.fields,
                local_record_params,
                &mut args,
            );
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
    args
}

fn trace_record_decomposition(
    function_name: &str,
    param_name: &str,
    arg: &rumoca_core::Expression,
    fields: &[rumoca_core::FunctionParam],
) {
    if std::env::var_os("RUMOCA_TRACE_RECORD_DECOMP").is_none() {
        return;
    }
    if !function_name.contains("Buildings.Fluid.Movers.BaseClasses.Characteristics") {
        return;
    }
    let field_names = fields
        .iter()
        .map(|field| field.name.as_str())
        .collect::<Vec<_>>()
        .join(",");
    eprintln!(
        "[rumoca-record-decomp] function={function_name} param={param_name} fields={field_names} arg={arg:?}"
    );
}

fn named_function_arg_value<'a>(
    args: &'a [rumoca_core::Expression],
    param_name: &str,
) -> Option<(usize, &'a rumoca_core::Expression)> {
    args.iter().enumerate().find_map(|(idx, arg)| {
        let rumoca_core::Expression::FunctionCall {
            name,
            args: named_args,
            is_constructor: _,
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
    arg: &rumoca_core::Expression,
    expected_type_name: &str,
    fields: &[rumoca_core::FunctionParam],
    local_record_params: Option<&HashSet<String>>,
    out: &mut Vec<rumoca_core::Expression>,
) {
    // Constructor call Complex(re, im) → extract positional/named args.
    // Positional extraction is only valid for the record type being decomposed;
    // otherwise a different record with a different field order can silently map
    // unrelated fields into the target signature.
    if let rumoca_core::Expression::FunctionCall {
        name,
        args: ctor_args,
        is_constructor: true,
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
        let positional_record_base = (!constructor_matches_expected)
            .then(|| projected_record_base_from_positionals(&positional, fields));

        for (i, field) in fields.iter().enumerate() {
            let named = named_constructor_arg(ctor_args, field.name.as_str());
            if let Some(val) = named {
                out.push(val.clone());
            } else if constructor_matches_expected && i < positional.len() {
                out.push(positional[i].clone());
            } else if let Some(Some(base)) = positional_record_base.as_ref() {
                out.push(rumoca_core::Expression::VarRef {
                    name: record_field_reference(
                        base,
                        field.name.as_str(),
                        rumoca_core::Span::DUMMY,
                    ),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                });
            } else {
                out.push(rumoca_core::Expression::FieldAccess {
                    base: Box::new(arg.clone()),
                    field: field.name.clone(),
                    span: rumoca_core::Span::DUMMY,
                });
            }
        }
        return;
    }

    // Variable reference → emit field VarRefs
    if let rumoca_core::Expression::VarRef { name, span, .. } = arg {
        if local_record_params.is_some_and(|params| params.contains(name.as_str())) {
            for field in fields {
                out.push(record_param_field_var_ref(
                    name.as_str(),
                    field.name.as_str(),
                    *span,
                ));
            }
            return;
        }

        for field in fields {
            out.push(rumoca_core::Expression::VarRef {
                name: record_field_reference(name, field.name.as_str(), *span),
                subscripts: vec![],
                span: *span,
            });
        }
        return;
    }

    // General expression → emit FieldAccess
    for field in fields {
        out.push(rumoca_core::Expression::FieldAccess {
            base: Box::new(arg.clone()),
            field: field.name.clone(),
            span: rumoca_core::Span::DUMMY,
        });
    }
}

fn projected_record_base_from_positionals(
    args: &[&rumoca_core::Expression],
    fields: &[rumoca_core::FunctionParam],
) -> Option<rumoca_core::Reference> {
    if args.is_empty() {
        return None;
    }
    if let Some(first_field) = fields.first()
        && let Some((base, field)) = var_ref_parent_and_field(args[0])
        && field == first_field.name
    {
        return Some(base);
    }

    let mut base: Option<rumoca_core::Reference> = None;
    for arg in args {
        let (parent, _) = var_ref_parent_and_field(arg)?;
        if let Some(existing) = &base {
            if existing.as_str() != parent.as_str() {
                return None;
            }
        } else {
            base = Some(parent);
        }
    }
    base
}

fn var_ref_parent_and_field(
    expr: &rumoca_core::Expression,
) -> Option<(rumoca_core::Reference, String)> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let field = record_reference_last_field(name)?;
    let parent = parent_record_reference(name)?;
    Some((parent, field))
}

fn record_reference_last_field(reference: &rumoca_core::Reference) -> Option<String> {
    if let Some(component_ref) = reference.component_ref() {
        return component_ref.parts.last().map(|part| part.ident.clone());
    }
    reference
        .as_str()
        .rsplit_once('.')
        .map(|(_, field)| field.to_string())
}

fn parent_record_reference(reference: &rumoca_core::Reference) -> Option<rumoca_core::Reference> {
    if let Some(component_ref) = reference.component_ref() {
        let parent_len = component_ref.parts.len().checked_sub(1)?;
        if parent_len == 0 {
            return None;
        }
        let mut parent = component_ref.clone();
        parent.parts.truncate(parent_len);
        let name = parent
            .parts
            .iter()
            .map(|part| part.ident.as_str())
            .collect::<Vec<_>>()
            .join(".");
        return Some(rumoca_core::Reference::with_component_reference(
            name, parent,
        ));
    }

    let (parent, _) = reference.as_str().rsplit_once('.')?;
    Some(rumoca_core::Reference::new(parent))
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

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: vec![],
            span: Span::DUMMY,
        }
    }

    fn int_literal(value: i64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: Literal::Integer(value),
            span: Span::DUMMY,
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
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                parts.join("."),
                rumoca_core::ComponentReference {
                    local: false,
                    span: Span::DUMMY,
                    parts: parts
                        .iter()
                        .map(|part| rumoca_core::ComponentRefPart {
                            ident: (*part).to_string(),
                            span: Span::DUMMY,
                            subs: Vec::new(),
                        })
                        .collect(),
                    def_id: None,
                },
            ),
            subscripts: vec![],
            span: Span::DUMMY,
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

    fn record_constructor() -> rumoca_core::Function {
        let mut constructor = rumoca_core::Function::new("Pkg.Record", Span::DUMMY);
        constructor.is_constructor = true;
        constructor.add_input(rumoca_core::FunctionParam::new("a", "Real"));
        constructor.add_input(rumoca_core::FunctionParam::new("b", "Real").with_dims(vec![3]));
        constructor
    }

    fn record_constructor_named(name: &str, fields: &[&str]) -> rumoca_core::Function {
        let mut constructor = rumoca_core::Function::new(name, Span::DUMMY);
        constructor.is_constructor = true;
        for field in fields {
            constructor.add_input(rumoca_core::FunctionParam::new(*field, "Real"));
        }
        constructor
    }

    fn function_with_record_input() -> rumoca_core::Function {
        let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("r", "Pkg.Record").with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
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

        lower_record_function_params(&mut flat);

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

        lower_record_function_params(&mut flat);

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
    fn record_param_lowering_uses_named_record_value_after_positional_args() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut function = rumoca_core::Function::new("Pkg.g", Span::DUMMY);
        function.add_input(rumoca_core::FunctionParam::new("u", "Real"));
        function.add_input(
            rumoca_core::FunctionParam::new("r", "Pkg.Record").with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
        function.body.push(assignment_to(
            "y",
            rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("r")),
                field: "a".to_string(),
                span: Span::DUMMY,
            },
        ));
        flat.add_function(function);

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.g"),
                args: vec![var_ref("scale"), named_arg("r", var_ref("rec"))],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat);

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 3);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "scale"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.a"
        ));
        assert!(matches!(
            &args[2],
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "rec.b"
        ));
    }

    #[test]
    fn record_param_lowering_does_not_positionally_expand_mismatched_constructor() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut other_constructor = rumoca_core::Function::new("Pkg.OtherRecord", Span::DUMMY);
        other_constructor.is_constructor = true;
        other_constructor.add_input(rumoca_core::FunctionParam::new("a", "Real"));
        other_constructor.add_input(rumoca_core::FunctionParam::new("c", "Real"));
        flat.add_function(other_constructor);
        flat.add_function(function_with_record_input());

        let mismatched_constructor = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.OtherRecord"),
            args: vec![
                rumoca_core::Expression::Literal {
                    value: Literal::Real(1.0),
                    span: Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: Literal::Real(2.0),
                    span: Span::DUMMY,
                },
            ],
            is_constructor: true,
            span: Span::DUMMY,
        };
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![named_arg("r", mismatched_constructor)],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat);

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
    fn record_param_lowering_reprojects_mismatched_constructor_field_group() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named(
            "Pkg.EfficiencyParameters",
            &["V_flow", "eta"],
        ));
        flat.add_function(record_constructor_named(
            "Pkg.Generic",
            &["pressure_V_flow", "pressure_dp"],
        ));

        let mut function = rumoca_core::Function::new("Pkg.efficiency", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("per", "Pkg.EfficiencyParameters")
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
        function.body.push(assignment_to(
            "y",
            rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("per")),
                field: "eta".to_string(),
                span: Span::DUMMY,
            },
        ));
        flat.add_function(function);

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.efficiency"),
                args: vec![named_arg(
                    "per",
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::Reference::new("Pkg.Generic"),
                        args: vec![
                            component_ref_expr(&["pump", "hydraulicEfficiency", "V_flow"]),
                            component_ref_expr(&["pump", "hydraulicEfficiency", "dp"]),
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

        lower_record_function_params(&mut flat);

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "pump.hydraulicEfficiency.V_flow"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "pump.hydraulicEfficiency.eta"
        ));
    }

    #[test]
    fn record_param_lowering_reprojects_from_first_matching_field_when_later_args_are_stale() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named(
            "Pkg.EfficiencyParameters",
            &["V_flow", "eta"],
        ));
        flat.add_function(record_constructor_named(
            "Pkg.Generic",
            &["pressure_V_flow", "pressure_dp"],
        ));

        let mut function = rumoca_core::Function::new("Pkg.efficiency", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("per", "Pkg.EfficiencyParameters")
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
        function.body.push(assignment_to(
            "y",
            rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("per")),
                field: "eta".to_string(),
                span: Span::DUMMY,
            },
        ));
        flat.add_function(function);

        let stale_second_arg = rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.EfficiencyParameters"),
                args: vec![
                    named_arg(
                        "V_flow",
                        rumoca_core::Expression::Array {
                            elements: vec![int_literal(0)],
                            is_matrix: false,
                            span: Span::DUMMY,
                        },
                    ),
                    named_arg(
                        "eta",
                        rumoca_core::Expression::Array {
                            elements: vec![rumoca_core::Expression::Literal {
                                value: Literal::Real(0.7),
                                span: Span::DUMMY,
                            }],
                            is_matrix: false,
                            span: Span::DUMMY,
                        },
                    ),
                ],
                is_constructor: true,
                span: Span::DUMMY,
            }),
            field: "dp".to_string(),
            span: Span::DUMMY,
        };

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.efficiency"),
                args: vec![named_arg(
                    "per",
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::Reference::new("Pkg.Generic"),
                        args: vec![
                            component_ref_expr(&["pump", "hydraulicEfficiency", "V_flow"]),
                            stale_second_arg,
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

        lower_record_function_params(&mut flat);

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 2);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "pump.hydraulicEfficiency.V_flow"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "pump.hydraulicEfficiency.eta"
        ));
    }

    #[test]
    fn record_param_lowering_expands_named_record_field_actual_value() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());
        flat.add_function(function_with_record_input());
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![named_arg(
                    "r",
                    rumoca_core::Expression::FieldAccess {
                        base: Box::new(var_ref("plant.unit")),
                        field: "recordValue".to_string(),
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

        lower_record_function_params(&mut flat);

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
    fn record_param_lowering_uses_callee_record_signature_for_named_field_actual() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor_named(
            "Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters",
            &["V_flow", "dp"],
        ));
        flat.add_function(record_constructor_named(
            "Buildings.Fluid.Movers.BaseClasses.Characteristics.efficiencyParameters",
            &["V_flow", "eta"],
        ));

        let mut pressure = rumoca_core::Function::new(
            "Buildings.Fluid.Movers.BaseClasses.Characteristics.pressure",
            Span::DUMMY,
        );
        pressure.add_input(
            rumoca_core::FunctionParam::new(
                "per",
                "Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters",
            )
            .with_type_class(ClassType::Record),
        );
        pressure.add_output(rumoca_core::FunctionParam::new("dp", "Real"));
        pressure.body.push(assignment_to(
            "dp",
            rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("per")),
                field: "dp".to_string(),
                span: Span::DUMMY,
            },
        ));
        flat.add_function(pressure);

        let mut efficiency = rumoca_core::Function::new(
            "Buildings.Fluid.Movers.BaseClasses.Characteristics.efficiency",
            Span::DUMMY,
        );
        efficiency.add_input(
            rumoca_core::FunctionParam::new(
                "per",
                "Buildings.Fluid.Movers.BaseClasses.Characteristics.efficiencyParameters",
            )
            .with_type_class(ClassType::Record),
        );
        efficiency.add_input(rumoca_core::FunctionParam::new("u", "Real"));
        efficiency.add_output(rumoca_core::FunctionParam::new("y", "Real"));
        efficiency.body.push(assignment_to(
            "y",
            rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("per")),
                field: "eta".to_string(),
                span: Span::DUMMY,
            },
        ));
        flat.add_function(efficiency);

        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new(
                    "Buildings.Fluid.Movers.BaseClasses.Characteristics.efficiency",
                ),
                args: vec![
                    named_arg("per", var_ref("pump.eff.per.hydraulicEfficiency")),
                    named_arg("u", var_ref("pump.eff.V_flow")),
                ],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new(
                    "Buildings.Fluid.Movers.BaseClasses.Characteristics.pressure",
                ),
                args: vec![named_arg("per", var_ref("pump.eff.pCur1"))],
                is_constructor: false,
                span: Span::DUMMY,
            },
            Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "probe_pressure".to_string(),
            },
        ));

        lower_record_function_params(&mut flat);

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 3);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "pump.eff.per.hydraulicEfficiency.V_flow"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "pump.eff.per.hydraulicEfficiency.eta"
        ));
    }

    #[test]
    fn record_array_param_lowering_rewrites_indexed_field_access() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut function = rumoca_core::Function::new("Pkg.sumA", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("r", "Pkg.Record")
                .with_type_class(ClassType::Record)
                .with_dims(vec![0])
                .with_shape_expr(vec![rumoca_core::Subscript::colon(Span::DUMMY)]),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
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

        lower_record_function_params(&mut flat);

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
            rumoca_core::FunctionParam::new("r", "Pkg.Record")
                .with_type_class(ClassType::Record)
                .with_dims(vec![0])
                .with_shape_expr(vec![rumoca_core::Subscript::colon(Span::DUMMY)]),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
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
            ..rumoca_core::FunctionParam::new("n", "Integer")
        });
        flat.add_function(function);

        lower_record_function_params(&mut flat);

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
            rumoca_core::FunctionParam::new("r", "Pkg.Record").with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
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
            ..rumoca_core::FunctionParam::new("n", "Integer")
        });
        flat.add_function(function);

        lower_record_function_params(&mut flat);

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

        lower_record_function_params(&mut flat);

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
    fn record_field_normalization_uses_structured_component_ref_parts() {
        let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("state", "Pkg.State")
                .with_type_class(ClassType::Record),
        );
        function.add_output(rumoca_core::FunctionParam::new("y", "Real"));
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
            rumoca_core::FunctionParam::new("r", "Pkg.Record").with_type_class(ClassType::Record),
        );
        callee.add_output(rumoca_core::FunctionParam::new("y", "Real"));
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
            rumoca_core::FunctionParam::new("state", "Pkg.Record")
                .with_type_class(ClassType::Record),
        );
        caller.add_output(rumoca_core::FunctionParam::new("y", "Real"));
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

        lower_record_function_params(&mut flat);

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
