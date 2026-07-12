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
    type_def_id: Option<rumoca_core::DefId>,
) -> Option<(String, Vec<rumoca_core::FunctionParam>)> {
    // Resolved declaration identity owns semantic lookup (SPEC_0001). The
    // textual name remains a compatibility fallback for synthetic/test IR
    // created without front-end type metadata.
    let constructor = type_def_id
        .and_then(|type_def_id| {
            functions.values().find(|function| {
                function.def_id == Some(type_def_id)
                    && function.is_constructor
                    && !function.inputs.is_empty()
            })
        })
        .or_else(|| {
            functions
                .get(&rumoca_core::VarName::new(type_name))
                .filter(|function| function.is_constructor && !function.inputs.is_empty())
        })?;
    Some((
        constructor.name.as_str().to_string(),
        constructor.inputs.to_vec(),
    ))
}

/// Rewrite FieldAccess on decomposed record params to direct VarRef.
fn rewrite_field_access_in_statement(
    stmt: &mut rumoca_core::Statement,
    params: &[DecomposedParam],
) {
    *stmt = RecordFieldAccessRewriter { params }.rewrite_statement(stmt);
}

/// Reconstruct a decomposed record input when the function body uses the
/// complete value (for example, `next := predicted`).  Field accesses are
/// lowered directly to primitive inputs, but a whole-record use must remain a
/// record-valued expression for assignments and calls that consume the record.
fn rewrite_whole_record_params_in_statement(
    stmt: &mut rumoca_core::Statement,
    params: &[DecomposedParam],
) {
    *stmt = WholeRecordParamRewriter { params }.rewrite_statement(stmt);
}

struct WholeRecordParamRewriter<'a> {
    params: &'a [DecomposedParam],
}

impl ExpressionRewriter for WholeRecordParamRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
            && subscripts.is_empty()
            && let Some(param) = self
                .params
                .iter()
                .find(|param| param.param_name == name.as_str())
        {
            return rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new(param.constructor_name.clone()),
                args: param
                    .fields
                    .iter()
                    .map(|field| record_param_field_var_ref(&param.param_name, &field.name, *span))
                    .collect(),
                is_constructor: true,
                span: *span,
            };
        }

        self.walk_expression(expr)
    }
}

impl StatementRewriter for WholeRecordParamRewriter<'_> {}

struct RecordFieldAccessRewriter<'a> {
    params: &'a [DecomposedParam],
}

impl ExpressionRewriter for RecordFieldAccessRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::FieldAccess { base, field, span } = expr
            && let rumoca_core::Expression::Index {
                base: indexed_base,
                subscripts,
                ..
            } = base.as_ref()
            && let rumoca_core::Expression::VarRef { name, .. } = indexed_base.as_ref()
            && let Some(param) = self
                .params
                .iter()
                .find(|param| param.param_name == name.as_str())
            && param.fields.iter().any(|f| f.name == *field)
        {
            return rumoca_core::Expression::VarRef {
                name: record_param_field_reference(name.as_str(), field, *span),
                subscripts: subscripts.clone(),
                span: *span,
            };
        }

        if let Some((param, segments, span)) = record_param_path(expr, self.params)
            && let Some(rewritten) = fuse_record_param_path(param, &segments, span)
        {
            return rewritten;
        }
        self.walk_expression(expr)
    }
}

impl StatementRewriter for RecordFieldAccessRewriter<'_> {}

/// Resolve an expression that names a field path rooted at a decomposed record
/// parameter (`element.rotation.q` as a structured component reference, a
/// dotted flat name, or a chain of `FieldAccess` nodes) into the parameter and
/// the field-path segments below it.
fn record_param_path<'a>(
    expr: &rumoca_core::Expression,
    params: &'a [DecomposedParam],
) -> Option<(
    &'a DecomposedParam,
    Vec<rumoca_core::ComponentRefPart>,
    rumoca_core::Span,
)> {
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } if subscripts.is_empty() => {
            let parts = match name.component_ref() {
                Some(reference) => reference.parts.clone(),
                None => name
                    .segments()
                    .into_iter()
                    .map(|segment| rumoca_core::ComponentRefPart {
                        ident: segment.to_string(),
                        span: *span,
                        subs: Vec::new(),
                    })
                    .collect(),
            };
            let (head, rest) = parts.split_first()?;
            if !head.subs.is_empty() {
                return None;
            }
            let param = params.iter().find(|param| param.param_name == head.ident)?;
            Some((param, rest.to_vec(), *span))
        }
        rumoca_core::Expression::FieldAccess { base, field, span } => {
            let (param, mut segments, _) = record_param_path(base, params)?;
            segments.push(rumoca_core::ComponentRefPart {
                ident: field.clone(),
                span: *span,
                subs: Vec::new(),
            });
            Some((param, segments, *span))
        }
        _ => None,
    }
}

/// Fuse the longest leading run of field-path segments that names a decomposed
/// field of `param` into the flat `<param>_<field>` variable. Any remaining
/// segments (deeper record nesting not yet decomposed on this pass) stay as
/// structured component-reference parts so a later fixpoint pass can fuse them.
fn fuse_record_param_path(
    param: &DecomposedParam,
    segments: &[rumoca_core::ComponentRefPart],
    span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    for fused_len in (1..=segments.len()).rev() {
        let fused = &segments[..fused_len];
        if fused.iter().any(|part| !part.subs.is_empty()) {
            continue;
        }
        let field_name = fused
            .iter()
            .map(|part| part.ident.as_str())
            .collect::<Vec<_>>()
            .join("_");
        if !param.fields.iter().any(|field| field.name == field_name) {
            continue;
        }
        if fused_len == segments.len() {
            return Some(record_param_field_var_ref(
                &param.param_name,
                &field_name,
                span,
            ));
        }
        let mut parts = vec![rumoca_core::ComponentRefPart {
            ident: format!("{}_{}", param.param_name, field_name),
            span,
            subs: Vec::new(),
        }];
        parts.extend(segments[fused_len..].iter().cloned());
        let component_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts,
            def_id: None,
        };
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(component_ref),
            subscripts: vec![],
            span,
        });
    }
    None
}

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
            && let Some((_, shape_source)) = self
                .shape_sources
                .iter()
                .find(|(param_name, _)| param_name == name.as_str())
        {
            *first_arg = rumoca_core::Expression::VarRef {
                name: record_param_reference(shape_source, *span),
                subscripts: Vec::new(),
                span: *span,
            };
        }

        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args,
            span: *span,
        }
    }
}

impl StatementRewriter for RecordParamSizeRewriter<'_> {}

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
    _span: rumoca_core::Span,
) -> rumoca_core::Reference {
    let name = format!("{param}_{field}");
    rumoca_core::Reference::generated(name)
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

fn record_param_reference(param: &str, _span: rumoca_core::Span) -> rumoca_core::Reference {
    rumoca_core::Reference::generated(param)
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
    // Each pass decomposes one record-nesting level of every function input;
    // record types cannot legally be recursive, so the fixpoint is bounded by
    // the deepest record nesting in the model.
    const MAX_RECORD_NESTING_PASSES: usize = 32;
    for _ in 0..MAX_RECORD_NESTING_PASSES {
        if !lower_record_function_params_once(flat)? {
            return Ok(());
        }
    }
    Err(FlattenError::internal(format!(
        "record parameter lowering did not converge after {MAX_RECORD_NESTING_PASSES} passes \
         (record type nesting too deep or cyclic)"
    )))
}

/// One decomposition pass. Returns whether any record parameter was decomposed.
// SPEC_0021: Exception - top-level record-parameter normalization pass keeps
// signature rewriting and call-site rewriting in one atomic compiler phase.
#[allow(clippy::too_many_lines)]
fn lower_record_function_params_once(flat: &mut flat::Model) -> Result<bool, FlattenError> {
    let record_fields_by_function_input = flat
        .functions
        .iter()
        .flat_map(|(function_name, function)| {
            function
                .inputs
                .iter()
                .enumerate()
                .filter(|(_, input)| input.type_class == Some(rumoca_core::ClassType::Record))
                .filter_map(|(input_index, input)| {
                    record_fields_from_constructor_metadata(
                        &flat.functions,
                        &input.type_name,
                        input.type_def_id,
                    )
                    .map(|metadata| ((function_name.clone(), input_index), metadata))
                })
                .collect::<Vec<_>>()
        })
        .collect::<HashMap<_, _>>();

    let mut decomposition_map: HashMap<String, Vec<DecomposedParam>> = HashMap::new();
    let mut local_decomposed_params: HashMap<String, HashSet<String>> = HashMap::new();

    for (func_name, func) in flat.functions.iter_mut() {
        let mut decomposed: Vec<DecomposedParam> = Vec::new();
        for (idx, input) in func.inputs.iter().enumerate() {
            if let Some((constructor_name, fields)) =
                record_fields_by_function_input.get(&(func_name.clone(), idx))
            {
                decomposed.push(DecomposedParam {
                    original_index: idx,
                    param_name: input.name.clone(),
                    constructor_name: constructor_name.clone(),
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
        let shape_sources = decomposed
            .iter()
            .filter_map(|d| {
                record_param_shape_source(d).map(|source| (d.param_name.clone(), source))
            })
            .collect::<Vec<_>>();
        rewrite_record_param_size_refs_in_function(func, &shape_sources);
        for stmt in &mut func.body {
            rewrite_field_access_in_statement(stmt, &decomposed);
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
        return Ok(false);
    }

    // Rewrite call sites in equations, variable bindings, and function bodies.
    for eq in &mut flat.equations {
        decompose_record_call_args_in_expr(&mut eq.residual, &decomposition_map, None)?;
    }
    for eq in &mut flat.initial_equations {
        decompose_record_call_args_in_expr(&mut eq.residual, &decomposition_map, None)?;
    }
    for assertion in flat
        .assert_equations
        .iter_mut()
        .chain(flat.initial_assert_equations.iter_mut())
    {
        decompose_record_call_args_in_expr(&mut assertion.condition, &decomposition_map, None)?;
        decompose_record_call_args_in_expr(&mut assertion.message, &decomposition_map, None)?;
        if let Some(level) = &mut assertion.level {
            decompose_record_call_args_in_expr(level, &decomposition_map, None)?;
        }
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
        // Decompose calls before reconstructing remaining whole-record uses.
        // Otherwise a call argument such as `inverse(reference)` first becomes
        // an Element constructor; recursively decomposing that constructor can
        // consume one level of its nested record before the outer call sees it.
        // Normalize generated field paths, then reconstruct only values that
        // genuinely remain record-valued after call decomposition.
        if let Some(decomposed) = decomposition_map.get(func_name.as_str()) {
            for stmt in &mut func.body {
                rewrite_field_access_in_statement(stmt, decomposed);
                rewrite_whole_record_params_in_statement(stmt, decomposed);
            }
        }
    }
    Ok(true)
}

struct DecomposedParam {
    original_index: usize,
    param_name: String,
    constructor_name: String,
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

impl StatementRewriter for RecordCallArgDecomposer<'_> {
    fn rewrite_statement(&mut self, stmt: &rumoca_core::Statement) -> rumoca_core::Statement {
        let rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } = stmt
        else {
            return self.walk_statement(stmt);
        };
        let rewritten_comp = self.rewrite_component_reference(comp);
        let call_name = rumoca_core::Reference::with_component_reference(
            rewritten_comp.to_var_name().as_str().to_string(),
            rewritten_comp.clone(),
        );
        let rewritten_args = self.rewrite_expressions(args);
        let args = self.args_for_call(&call_name, rewritten_args);
        rumoca_core::Statement::FunctionCall {
            comp: rewritten_comp,
            args,
            outputs: outputs
                .iter()
                .map(|output| self.rewrite_component_reference(output))
                .collect(),
            span: *span,
        }
    }
}

fn decompose_record_call_args(
    function_name: &str,
    old_args: &[rumoca_core::Expression],
    decomposed: &[DecomposedParam],
    local_record_params: Option<&HashSet<String>>,
) -> Result<Vec<rumoca_core::Expression>, FlattenError> {
    let mut args = Vec::new();
    let mut positional_index = 0usize;
    for arg in old_args {
        if let Some((name, value, span)) = named_function_arg(arg) {
            let Some(dp) = decomposed.iter().find(|dp| dp.param_name == name) else {
                args.push(arg.clone());
                continue;
            };
            let mut expanded = Vec::new();
            expand_record_arg(
                function_name,
                value,
                &dp.fields,
                local_record_params,
                &mut expanded,
            )?;
            for (field, value) in dp.fields.iter().zip(expanded) {
                args.push(named_function_arg_marker(
                    format!("{}_{}", dp.param_name, field.name),
                    value,
                    span,
                ));
            }
            continue;
        }
        if let Some(dp) = decomposed
            .iter()
            .find(|dp| dp.original_index == positional_index)
        {
            expand_record_arg(
                function_name,
                arg,
                &dp.fields,
                local_record_params,
                &mut args,
            )?;
        } else {
            args.push(arg.clone());
        }
        positional_index += 1;
    }
    Ok(args)
}

fn named_function_arg(
    arg: &rumoca_core::Expression,
) -> Option<(&str, &rumoca_core::Expression, rumoca_core::Span)> {
    let rumoca_core::Expression::FunctionCall {
        name, args, span, ..
    } = arg
    else {
        return None;
    };
    let name = name
        .as_str()
        .strip_prefix(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)?;
    let [value] = args.as_slice() else {
        return None;
    };
    Some((name, value, *span))
}

fn named_function_arg_marker(
    name: String,
    value: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(format!(
            "{}{name}",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        args: vec![value],
        is_constructor: true,
        span,
    }
}

/// Expand a record argument into scalar field arguments.
// SPEC_0021: Exception - exhaustive record argument forms share field-default
// and zero-sized-field semantics in this compiler boundary.
#[allow(clippy::excessive_nesting)]
fn expand_record_arg(
    function_name: &str,
    arg: &rumoca_core::Expression,
    fields: &[rumoca_core::FunctionParam],
    local_record_params: Option<&HashSet<String>>,
    out: &mut Vec<rumoca_core::Expression>,
) -> Result<(), FlattenError> {
    // Constructor call Complex(re, im) → extract positional/named args
    if let rumoca_core::Expression::FunctionCall {
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

        for (i, field) in fields.iter().enumerate() {
            if let Some(empty) = empty_record_field_arg(field, *span) {
                out.push(empty);
                continue;
            }
            let named = named_constructor_arg(ctor_args, field.name.as_str());
            if let Some(val) = named {
                out.push(val.clone());
            } else if i < positional.len() {
                out.push(positional[i].clone());
            } else if let Some(default) = &field.default {
                out.push(default.clone());
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
                if let Some(empty) = empty_record_field_arg(field, *span) {
                    out.push(empty);
                    continue;
                }
                out.push(record_param_field_var_ref(
                    name.as_str(),
                    field.name.as_str(),
                    *span,
                ));
            }
            return Ok(());
        }

        for field in fields {
            if let Some(empty) = empty_record_field_arg(field, *span) {
                out.push(empty);
                continue;
            }
            out.push(rumoca_core::Expression::VarRef {
                name: record_field_reference(name, field.name.as_str(), *span),
                subscripts: vec![],
                span: *span,
            });
        }
        return Ok(());
    }

    // General expression → emit FieldAccess
    for field in fields {
        let source_span = record_field_access_source_span(arg, field)?;
        if let Some(empty) = empty_record_field_arg(field, source_span) {
            out.push(empty);
            continue;
        }
        out.push(rumoca_core::Expression::FieldAccess {
            base: Box::new(arg.clone()),
            field: field.name.clone(),
            span: source_span,
        });
    }
    Ok(())
}

fn empty_record_field_arg(
    field: &rumoca_core::FunctionParam,
    span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    (!field.dims.is_empty() && field.dims.iter().all(|dim| *dim >= 0) && field.dims.contains(&0))
        .then_some(rumoca_core::Expression::Array {
            elements: Vec::new(),
            is_matrix: field.dims.len() == 2,
            span,
        })
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
        let rumoca_core::Expression::VarRef { name, .. } = value else {
            panic!("expected rewritten record-field reference");
        };
        assert!(
            name.is_generated(),
            "decomposed record fields are compiler-generated function locals"
        );
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
    fn record_param_lowering_preserves_named_argument_slots() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());
        flat.add_function(function_with_record_input());
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![named_function_arg_marker(
                    "r".to_string(),
                    var_ref("rec"),
                    test_span(),
                )],
                is_constructor: false,
                span: test_span(),
            },
            test_span(),
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } = &flat.equations[0].residual else {
            panic!("expected function call");
        };
        let names_and_values = args
            .iter()
            .map(|arg| {
                let (name, value, _) = named_function_arg(arg).expect("named decomposed argument");
                let rumoca_core::Expression::VarRef { name: value, .. } = value else {
                    panic!("expected record field reference");
                };
                (name.to_string(), value.as_str().to_string())
            })
            .collect::<Vec<_>>();
        assert_eq!(
            names_and_values,
            vec![
                ("r_a".to_string(), "rec.a".to_string()),
                ("r_b".to_string(), "rec.b".to_string()),
            ]
        );
    }

    #[test]
    fn record_param_lowering_rewrites_runtime_assertion_calls() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());
        flat.add_function(function_with_record_input());
        flat.assert_equations.push(flat::AssertEquation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![var_ref("rec")],
                is_constructor: false,
                span: test_span(),
            },
            rumoca_core::Expression::Literal {
                value: Literal::String("record assertion".to_string()),
                span: test_span(),
            },
            None,
            test_span(),
            flat::EquationOrigin::ComponentEquation {
                component: "probe".to_string(),
            },
        ));

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let rumoca_core::Expression::FunctionCall { args, .. } =
            &flat.assert_equations[0].condition
        else {
            panic!("expected assertion function call");
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
    fn record_param_lowering_reconstructs_whole_record_uses() {
        let mut flat = flat::Model::new();
        flat.add_function(record_constructor());

        let mut function = rumoca_core::Function::new("Pkg.copyRecord", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("source", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record),
        );
        function.add_output(
            rumoca_core::FunctionParam::new("result", "Pkg.Record", test_span())
                .with_type_class(ClassType::Record),
        );
        function
            .body
            .push(assignment_to("result", var_ref("source")));
        flat.add_function(function);

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let function = flat
            .functions
            .get(&VarName::new("Pkg.copyRecord"))
            .expect("function remains");
        assert_eq!(
            function
                .inputs
                .iter()
                .map(|input| input.name.as_str())
                .collect::<Vec<_>>(),
            vec!["source_a", "source_b"]
        );
        let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
            panic!("expected assignment");
        };
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } = value
        else {
            panic!("expected reconstructed record constructor, got {value:?}");
        };
        assert!(*is_constructor);
        assert_eq!(name.as_str(), "Pkg.Record");
        assert!(matches!(
            args.as_slice(),
            [
                rumoca_core::Expression::VarRef { name: first, .. },
                rumoca_core::Expression::VarRef { name: second, .. }
            ] if first.as_str() == "source_a" && second.as_str() == "source_b"
        ));
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

    #[test]
    fn nested_record_param_with_qualified_type_is_decomposed_to_fixpoint() {
        let mut flat = flat::Model::new();

        let mut inner_constructor = rumoca_core::Function::new("Pkg.Inner", Span::DUMMY);
        inner_constructor.is_constructor = true;
        inner_constructor.add_input(rumoca_core::FunctionParam::new(
            "value",
            "Real",
            test_span(),
        ));
        flat.add_function(inner_constructor);

        let mut outer_constructor = rumoca_core::Function::new("Pkg.Outer", Span::DUMMY);
        outer_constructor.is_constructor = true;
        outer_constructor.add_input(
            rumoca_core::FunctionParam::new("inner", "Pkg.Inner", test_span())
                .with_type_class(ClassType::Record),
        );
        flat.add_function(outer_constructor);

        let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
        function.add_input(
            rumoca_core::FunctionParam::new("outer", "Pkg.Outer", test_span())
                .with_type_class(ClassType::Record),
        );
        flat.add_function(function);

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let function = flat
            .functions
            .get(&VarName::new("Pkg.f"))
            .expect("function remains");
        assert_eq!(
            function
                .inputs
                .iter()
                .map(|input| input.name.as_str())
                .collect::<Vec<_>>(),
            vec!["outer_inner_value"]
        );
        assert!(
            function
                .inputs
                .iter()
                .all(|input| input.type_class.is_none())
        );
    }

    #[test]
    fn nested_record_call_arg_projections_follow_decomposed_caller_inputs() {
        let mut flat = flat::Model::new();

        let mut rotation_constructor = rumoca_core::Function::new("Pkg.Rotation", Span::DUMMY);
        rotation_constructor.is_constructor = true;
        rotation_constructor.add_input(
            rumoca_core::FunctionParam::new("interfaceMarker", "Real", test_span())
                .with_dims(vec![0]),
        );
        rotation_constructor.add_input(
            rumoca_core::FunctionParam::new("q", "Real", test_span()).with_dims(vec![4]),
        );
        flat.add_function(rotation_constructor);

        let mut element_constructor = rumoca_core::Function::new("Pkg.Element", Span::DUMMY);
        element_constructor.is_constructor = true;
        element_constructor.add_input(
            rumoca_core::FunctionParam::new("position", "Real", test_span()).with_dims(vec![3]),
        );
        element_constructor.add_input(
            rumoca_core::FunctionParam::new("rotation", "Pkg.Rotation", test_span())
                .with_type_class(ClassType::Record),
        );
        flat.add_function(element_constructor);

        let mut inverse = rumoca_core::Function::new("Pkg.inverse", Span::DUMMY);
        inverse.add_input(
            rumoca_core::FunctionParam::new("element", "Pkg.Element", test_span())
                .with_type_class(ClassType::Record),
        );
        inverse.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        flat.add_function(inverse);

        let mut caller = rumoca_core::Function::new("Pkg.caller", Span::DUMMY);
        caller.add_input(
            rumoca_core::FunctionParam::new("reference", "Pkg.Element", test_span())
                .with_type_class(ClassType::Record),
        );
        caller.add_output(rumoca_core::FunctionParam::new("y", "Real", test_span()));
        caller.body.push(assignment_to(
            "y",
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.inverse"),
                args: vec![var_ref("reference")],
                is_constructor: false,
                span: Span::DUMMY,
            },
        ));
        flat.add_function(caller);

        lower_record_function_params(&mut flat).expect("record parameter lowering should pass");

        let caller = flat
            .functions
            .get(&VarName::new("Pkg.caller"))
            .expect("caller remains");
        let rumoca_core::Statement::Assignment { value, .. } = &caller.body[0] else {
            panic!("expected assignment");
        };
        let rumoca_core::Expression::FunctionCall { args, .. } = value else {
            panic!("expected function call");
        };
        assert_eq!(args.len(), 3);
        assert!(matches!(
            &args[0],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "reference_position"
        ));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::Array { elements, .. } if elements.is_empty()
        ));
        assert!(matches!(
            &args[2],
            rumoca_core::Expression::VarRef { name, .. }
                if name.as_str() == "reference_rotation_q"
        ));
    }
}
