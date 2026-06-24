//! Record parameter lowering for flattened functions.
//!
//! This module handles post-collection passes that transform function signatures
//! and call sites:
//! - Decomposing record-typed parameters into scalar fields
//! - Normalizing structured record-field component references
//! - Rewriting FieldAccess expressions on decomposed record params to direct VarRef

use crate::errors::FlattenError;
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
pub(crate) fn lower_record_function_params(flat: &mut flat::Model) -> Result<(), FlattenError> {
    let record_fields_by_type = flat
        .functions
        .values()
        .flat_map(|function| function.inputs.iter())
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
        return Ok(());
    }

    // Rewrite call sites in equations, variable bindings, and function bodies.
    for eq in &mut flat.equations {
        decompose_record_call_args_in_expr(&mut eq.residual, &decomposition_map, None)?;
    }
    for eq in &mut flat.initial_equations {
        decompose_record_call_args_in_expr(&mut eq.residual, &decomposition_map, None)?;
    }
    for var in flat.variables.values_mut() {
        if let Some(ref mut binding) = var.binding {
            decompose_record_call_args_in_expr(binding, &decomposition_map, None)?;
        }
        if let Some(ref mut start) = var.start {
            decompose_record_call_args_in_expr(start, &decomposition_map, None)?;
        }
    }
    for (func_name, func) in flat.functions.iter_mut() {
        let local_record_params = local_decomposed_params.get(func_name.as_str());
        for stmt in &mut func.body {
            decompose_record_call_args_in_stmt(stmt, &decomposition_map, local_record_params)?;
        }
    }
    Ok(())
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
) -> Result<(), FlattenError> {
    let mut decomposer = RecordCallArgDecomposer {
        map,
        local_record_params,
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
) -> Result<(), FlattenError> {
    let mut decomposer = RecordCallArgDecomposer {
        map,
        local_record_params,
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
) -> Result<Vec<rumoca_core::Expression>, FlattenError> {
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

fn named_function_arg_name_expr(arg: &rumoca_core::Expression) -> bool {
    named_function_arg_name(Some(arg)).is_some()
}

fn expression_leaf_name(expr: &rumoca_core::Expression) -> Option<&str> {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => name
            .component_ref()
            .and_then(|component_ref| component_ref.parts.last())
            .map(|part| part.ident.as_str()),
        rumoca_core::Expression::Index { base, .. } => expression_leaf_name(base),
        rumoca_core::Expression::FieldAccess { field, .. } => Some(field.as_str()),
        _ => None,
    }
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
