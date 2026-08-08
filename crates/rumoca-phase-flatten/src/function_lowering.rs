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
    span: rumoca_core::Span,
) -> Result<(String, rumoca_core::DefId, Vec<rumoca_core::FunctionParam>), FlattenError> {
    let type_def_id = type_def_id.ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(
            type_name,
            "record function parameter type",
            span,
        )
    })?;
    let constructor =
        rumoca_core::resolve_record_constructor(functions.values(), type_name, type_def_id)
            .map_err(|error| {
                FlattenError::missing_resolved_class_metadata(
                    type_name,
                    format!("record constructor metadata: {error}"),
                    span,
                )
            })?;
    let constructor_def_id = constructor.def_id.ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(
            type_name,
            "record constructor identity",
            constructor.span,
        )
    })?;
    Ok((
        constructor.name.as_str().to_string(),
        constructor_def_id,
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
                name: rumoca_core::Reference::with_component_reference(
                    &param.constructor_name,
                    rumoca_core::ComponentReference::construct(
                        false,
                        *span,
                        vec![rumoca_core::ComponentRefPart {
                            ident: param.constructor_name.clone(),
                            span: *span,
                            subs: Vec::new(),
                            def_id: param.constructor_def_id,
                        }],
                    )
                    .expect("record constructor metadata has a nonzero resolved identity"),
                ),
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
        if let Some(rewritten) = indexed_record_param_field(expr, self.params) {
            return rewritten;
        }
        if let rumoca_core::Expression::FieldAccess {
            base, field, span, ..
        } = expr
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

/// Preserve tensor indexing while moving an indexed record field onto its
/// decomposed function-input coordinate (`state.q[1]` -> `state_q[1]`).
fn indexed_record_param_field(
    expression: &rumoca_core::Expression,
    params: &[DecomposedParam],
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    } = expression
    else {
        return None;
    };
    if subscripts.is_empty() {
        return None;
    }
    let reference = name.component_ref()?;
    let (root, fields) = reference.parts().split_first()?;
    if !root.subs.is_empty() || fields.is_empty() {
        return None;
    }
    let param = params.iter().find(|param| param.param_name == root.ident)?;
    let mut rewritten = fuse_record_param_path(param, fields, *span)?;
    let rumoca_core::Expression::VarRef {
        subscripts: rewritten_subscripts,
        ..
    } = &mut rewritten
    else {
        return None;
    };
    if !rewritten_subscripts.is_empty() {
        return None;
    }
    *rewritten_subscripts = subscripts.clone();
    Some(rewritten)
}

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
            if let Some(reference) = name.component_ref() {
                let (head, rest) = reference.parts().split_first()?;
                if !head.subs.is_empty() {
                    return None;
                }
                let param = params.iter().find(|param| param.param_name == head.ident)?;
                return Some((param, rest.to_vec(), *span));
            }
            let param = params
                .iter()
                .find(|param| param.param_name == name.as_str())?;
            Some((param, Vec::new(), *span))
        }
        rumoca_core::Expression::FieldAccess {
            base,
            field,
            field_def_id,
            span,
        } => {
            let (param, mut segments, _) = record_param_path(base, params)?;
            segments.push(rumoca_core::ComponentRefPart {
                ident: field.clone(),
                span: *span,
                subs: Vec::new(),
                def_id: *field_def_id,
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
            def_id: fused.last().expect("a fused field path is nonempty").def_id,
        }];
        parts.extend(segments[fused_len..].iter().cloned());
        let component_ref = rumoca_core::ComponentReference::construct(false, span, parts)
            .expect("decomposed record paths are nonempty and identity-bearing");
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

/// Coalesce record-result field writes when delaying their RHS evaluation to
/// the final field write is proved semantics-preserving.
pub(super) fn coalesce_proven_record_output_assignments(func: &mut rumoca_core::Function) {
    let outputs = func
        .outputs
        .iter()
        .filter(|output| output.type_class == Some(rumoca_core::ClassType::Record))
        .map(|output| output.name.clone())
        .collect::<Vec<_>>();
    for output in outputs {
        coalesce_one_record_output(&mut func.body, &output);
    }
}

/// Convert a nonescaping per-iteration scratch assignment into SSA form.
pub(super) fn inline_proven_loop_scratch_assignments(func: &mut rumoca_core::Function) {
    let locals = func
        .locals
        .iter()
        .map(|local| local.name.as_str())
        .collect::<HashSet<_>>();
    for index in 0..func.body.len() {
        let (before, tail) = func.body.split_at_mut(index + 1);
        let statement = &mut before[index];
        let rumoca_core::Statement::For { equations, .. } = statement else {
            continue;
        };
        let Some((scratch, replacement)) = leading_loop_scratch(equations, &locals) else {
            continue;
        };
        if statements_read_name(tail, &scratch) {
            continue;
        }
        let mut rewriter = LoopScratchRewriter {
            scratch: &scratch,
            replacement: &replacement,
        };
        for statement in equations.iter_mut().skip(1) {
            let rumoca_core::Statement::Assignment { value, .. } = statement else {
                continue;
            };
            *value = rewriter.rewrite_expression(value);
        }
        equations.remove(0);
    }
}

fn leading_loop_scratch(
    equations: &[rumoca_core::Statement],
    locals: &HashSet<&str>,
) -> Option<(String, rumoca_core::Expression)> {
    let [
        rumoca_core::Statement::Assignment { comp, value, .. },
        rest @ ..,
    ] = equations
    else {
        return None;
    };
    let [target] = comp.parts() else {
        return None;
    };
    if !target.subs.is_empty() || !locals.contains(target.ident.as_str()) {
        return None;
    }
    let scratch = target.ident.clone();
    let mut rhs_reads = Vec::new();
    value.collect_var_refs(&mut rhs_reads);
    if rhs_reads.iter().any(|name| name.as_str() == scratch)
        || rest.iter().any(|statement| {
            !matches!(statement, rumoca_core::Statement::Assignment { .. })
                || statement_assigns_name(statement, &scratch)
        })
    {
        return None;
    }
    Some((scratch, value.clone()))
}

fn statement_assigns_name(statement: &rumoca_core::Statement, name: &str) -> bool {
    let rumoca_core::Statement::Assignment { comp, .. } = statement else {
        return false;
    };
    comp.parts().first().is_some_and(|root| root.ident == name)
}

fn statements_read_name(statements: &[rumoca_core::Statement], name: &str) -> bool {
    statements.iter().any(|statement| {
        let mut reads = Vec::new();
        match statement {
            rumoca_core::Statement::Assignment { value, .. } => value.collect_var_refs(&mut reads),
            _ => return true,
        }
        reads.iter().any(|reference| {
            crate::path_utils::first_path_segment_without_index(reference.as_str()) == Some(name)
        })
    })
}

struct LoopScratchRewriter<'a> {
    scratch: &'a str,
    replacement: &'a rumoca_core::Expression,
}

impl ExpressionRewriter for LoopScratchRewriter<'_> {
    fn rewrite_expression(
        &mut self,
        expression: &rumoca_core::Expression,
    ) -> rumoca_core::Expression {
        if let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = expression
            && name.as_str() == self.scratch
            && subscripts.is_empty()
        {
            return self.replacement.clone();
        }
        self.walk_expression(expression)
    }
}

fn coalesce_one_record_output(statements: &mut Vec<rumoca_core::Statement>, output: &str) {
    let assignments = statements
        .iter()
        .enumerate()
        .filter_map(|(index, statement)| {
            record_output_field_assignment(statement, output).map(|value| (index, value))
        })
        .collect::<Vec<_>>();
    let Some(&(last, _)) = assignments.last() else {
        return;
    };
    if assignments.len() < 2
        || assignments[..assignments.len() - 1]
            .iter()
            .any(|(index, value)| !record_write_may_move(statements, *index, last, output, value))
    {
        return;
    }

    let ordered_assignment_indices = assignments
        .iter()
        .map(|(index, _)| *index)
        .collect::<Vec<_>>();
    let assignment_count = assignments.len();
    drop(assignments);
    inline_prior_record_field_values(statements, output, &ordered_assignment_indices);
    let assignment_indices = ordered_assignment_indices
        .iter()
        .copied()
        .collect::<HashSet<_>>();
    let mut moved = Vec::with_capacity(assignment_count);
    let mut retained = Vec::with_capacity(statements.len());
    let insertion = statements[..=last]
        .iter()
        .enumerate()
        .filter(|(index, _)| !assignment_indices.contains(index))
        .count();
    for (index, statement) in std::mem::take(statements).into_iter().enumerate() {
        if assignment_indices.contains(&index) {
            moved.push(statement);
        } else {
            retained.push(statement);
        }
    }
    retained.splice(insertion..insertion, moved);
    *statements = retained;
}

fn inline_prior_record_field_values(
    statements: &mut [rumoca_core::Statement],
    output: &str,
    assignment_indices: &[usize],
) {
    let mut fields = HashMap::new();
    for index in assignment_indices {
        let rumoca_core::Statement::Assignment { comp, value, .. } = &mut statements[*index] else {
            unreachable!("record output assignment indices name assignments")
        };
        let [root, field] = comp.parts() else {
            unreachable!("record output assignment has one field part")
        };
        let mut rewriter = PriorRecordFieldRewriter {
            output,
            fields: &fields,
        };
        *value = rewriter.rewrite_expression(value);
        if field.subs.is_empty() {
            fields.insert(field.def_id, value.clone());
        }
        debug_assert_eq!(root.ident, output);
    }
}

struct PriorRecordFieldRewriter<'a> {
    output: &'a str,
    fields: &'a HashMap<rumoca_core::DefId, rumoca_core::Expression>,
}

impl ExpressionRewriter for PriorRecordFieldRewriter<'_> {
    fn rewrite_expression(
        &mut self,
        expression: &rumoca_core::Expression,
    ) -> rumoca_core::Expression {
        if let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = expression
            && subscripts.is_empty()
            && let Some(reference) = name.component_ref()
            && let [root, field] = reference.parts()
            && root.ident == self.output
            && root.subs.is_empty()
            && field.subs.is_empty()
            && let Some(value) = self.fields.get(&field.def_id)
        {
            return value.clone();
        }
        if let rumoca_core::Expression::FieldAccess {
            base, field_def_id, ..
        } = expression
            && let rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } = base.as_ref()
            && name.as_str() == self.output
            && subscripts.is_empty()
            && let Some(value) = self.fields.get(field_def_id)
        {
            return value.clone();
        }
        self.walk_expression(expression)
    }
}

fn record_output_field_assignment<'a>(
    statement: &'a rumoca_core::Statement,
    output: &str,
) -> Option<&'a rumoca_core::Expression> {
    let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
        return None;
    };
    let [root, _field] = comp.parts() else {
        return None;
    };
    (root.ident == output).then_some(value)
}

fn record_write_may_move(
    statements: &[rumoca_core::Statement],
    start: usize,
    end: usize,
    output: &str,
    value: &rumoca_core::Expression,
) -> bool {
    let mut references = Vec::new();
    value.collect_var_refs(&mut references);
    let dependencies = references
        .iter()
        .filter_map(|reference| {
            crate::path_utils::first_path_segment_without_index(reference.as_str())
        })
        .collect::<HashSet<_>>();
    statements[start + 1..end].iter().all(|statement| {
        let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
            return false;
        };
        let assigned = comp.parts().first().map(|part| part.ident.as_str());
        if assigned.is_some_and(|assigned| dependencies.contains(assigned)) {
            return false;
        }
        let mut reads = Vec::new();
        value.collect_var_refs(&mut reads);
        !reads.iter().any(|reference| {
            crate::path_utils::first_path_segment_without_index(reference.as_str()) == Some(output)
        })
    })
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
    let [record, field] = reference.parts() else {
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
        field_def_id: field.def_id,
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
    seed_complete_record_defaults(flat);
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

fn seed_complete_record_defaults(flat: &mut flat::Model) {
    let constructors = flat
        .functions
        .values()
        .filter(|function| function.is_constructor)
        .filter_map(|function| {
            let def_id = function.def_id?;
            let instance_id = function.instance_id?;
            function
                .inputs
                .iter()
                .all(|input| input.default.is_some())
                .then(|| {
                    (
                        (def_id, function.name.as_str().to_string()),
                        (function.name.clone(), instance_id),
                    )
                })
        })
        .collect::<HashMap<_, _>>();
    for function in flat
        .functions
        .values_mut()
        .filter(|function| !function.is_constructor)
    {
        for value in function.outputs.iter_mut().chain(&mut function.locals) {
            if value.default.is_some() || value.type_class != Some(rumoca_core::ClassType::Record) {
                continue;
            }
            let Some(type_def_id) = value.type_def_id else {
                continue;
            };
            let Some((constructor_name, instance_id)) =
                constructors.get(&(type_def_id, value.type_name.clone()))
            else {
                continue;
            };
            value.default = Some(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::from_var_name(constructor_name.clone())
                    .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                        instance_id: *instance_id,
                        base_part_count: 0,
                        transitively_non_replaceable: false,
                    }),
                args: Vec::new(),
                is_constructor: true,
                span: value.span,
            });
        }
    }
}

/// One decomposition pass. Returns whether any record parameter was decomposed.
fn lower_record_function_params_once(flat: &mut flat::Model) -> Result<bool, FlattenError> {
    let mut record_fields_by_function_input = HashMap::new();
    let mut record_metadata_by_exposure = HashMap::new();
    for (function_name, function) in &flat.functions {
        // A record constructor is the compact aggregate owner. Decomposing its
        // own nested-record fields would turn that layout into an executable
        // scalar signature and make later whole-record reconstruction call a
        // different pseudo-constructor. Only ordinary callable inputs cross
        // this scalar ABI boundary.
        if function.is_constructor {
            continue;
        }
        for (input_index, input) in function.inputs.iter().enumerate() {
            if input.type_class != Some(rumoca_core::ClassType::Record) {
                continue;
            }
            let type_def_id = input.type_def_id.ok_or_else(|| {
                FlattenError::missing_resolved_class_metadata(
                    &input.type_name,
                    "record function parameter type",
                    input.span,
                )
            })?;
            let key = (type_def_id, input.type_name.clone());
            if !record_metadata_by_exposure.contains_key(&key) {
                let metadata = record_fields_from_constructor_metadata(
                    &flat.functions,
                    &input.type_name,
                    Some(type_def_id),
                    input.span,
                )?;
                record_metadata_by_exposure.insert(key.clone(), metadata);
            }
            let metadata = record_metadata_by_exposure
                .get(&key)
                .expect("record metadata inserted above")
                .clone();
            record_fields_by_function_input.insert((function_name.clone(), input_index), metadata);
        }
    }

    let mut decomposition_map = RecordDecompositionMap::default();
    let mut local_decomposed_params: HashMap<String, HashSet<String>> = HashMap::new();

    for (func_name, func) in flat.functions.iter_mut() {
        let mut decomposed: Vec<DecomposedParam> = Vec::new();
        for (idx, input) in func.inputs.iter().enumerate() {
            if let Some((constructor_name, constructor_def_id, fields)) =
                record_fields_by_function_input.get(&(func_name.clone(), idx))
            {
                decomposed.push(DecomposedParam {
                    original_index: idx,
                    param_name: input.name.clone(),
                    constructor_name: constructor_name.clone(),
                    constructor_def_id: *constructor_def_id,
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

        decomposition_map.insert(
            func_name.as_str().to_string(),
            func.instance_id,
            func.def_id,
            decomposed,
        );
    }

    if decomposition_map.is_empty() {
        return Ok(false);
    }

    rewrite_decomposed_record_call_sites(flat, &decomposition_map, &local_decomposed_params)?;
    Ok(true)
}

fn rewrite_decomposed_record_call_sites(
    flat: &mut flat::Model,
    decomposition_map: &RecordDecompositionMap,
    local_decomposed_params: &HashMap<String, HashSet<String>>,
) -> Result<(), FlattenError> {
    // Rewrite every executable expression surface in Flat. Keeping this in
    // lockstep with final call-argument materialization is part of the stage
    // contract: a decomposed signature may never coexist with a source-shaped
    // record argument in a compact template or auxiliary attribute.
    for eq in &mut flat.equations {
        decompose_record_call_args_in_expr(&mut eq.residual, decomposition_map, None)?;
    }
    for eq in &mut flat.initial_equations {
        decompose_record_call_args_in_expr(&mut eq.residual, decomposition_map, None)?;
    }
    for assertion in flat
        .assert_equations
        .iter_mut()
        .chain(flat.initial_assert_equations.iter_mut())
    {
        decompose_record_call_args_in_expr(&mut assertion.condition, decomposition_map, None)?;
        decompose_record_call_args_in_expr(&mut assertion.message, decomposition_map, None)?;
        if let Some(level) = &mut assertion.level {
            decompose_record_call_args_in_expr(level, decomposition_map, None)?;
        }
    }
    for var in flat.variables.values_mut() {
        for expression in [
            &mut var.binding,
            &mut var.start,
            &mut var.min,
            &mut var.max,
            &mut var.nominal,
        ]
        .into_iter()
        .flatten()
        {
            decompose_record_call_args_in_expr(expression, decomposition_map, None)?;
        }
    }
    for family in flat
        .structured_equations
        .iter_mut()
        .chain(flat.initial_structured_equations.iter_mut())
    {
        if let Some(template) = family.template.as_mut() {
            for expression in &mut template.body {
                decompose_record_call_args_in_expr(expression, decomposition_map, None)?;
            }
        }
    }
    for algorithm in flat
        .algorithms
        .iter_mut()
        .chain(flat.initial_algorithms.iter_mut())
    {
        for statement in &mut algorithm.statements {
            decompose_record_call_args_in_stmt(statement, decomposition_map, None)?;
        }
    }
    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            decompose_record_call_args_in_expr(&mut branch.condition, decomposition_map, None)?;
            decompose_record_calls_in_when_equations(&mut branch.equations, decomposition_map)?;
        }
    }
    for (func_name, func) in flat.functions.iter_mut() {
        let local_record_params = local_decomposed_params.get(func_name.as_str());
        let aggregate_record_values = func
            .outputs
            .iter()
            .chain(func.locals.iter())
            .filter(|parameter| parameter.type_class == Some(rumoca_core::ClassType::Record))
            .map(|parameter| parameter.name.clone())
            .collect::<HashSet<_>>();
        for parameter in func
            .inputs
            .iter_mut()
            .chain(func.outputs.iter_mut())
            .chain(func.locals.iter_mut())
        {
            decompose_record_calls_in_function_parameter(
                parameter,
                decomposition_map,
                local_record_params,
                Some(&aggregate_record_values),
            )?;
        }
        for stmt in &mut func.body {
            decompose_record_call_args_in_stmt_scoped(
                stmt,
                decomposition_map,
                local_record_params,
                Some(&aggregate_record_values),
            )?;
        }
        // Decompose calls before reconstructing remaining whole-record uses.
        // Otherwise a call argument such as `inverse(reference)` first becomes
        // an Element constructor; recursively decomposing that constructor can
        // consume one level of its nested record before the outer call sees it.
        // Normalize generated field paths, then reconstruct only values that
        // genuinely remain record-valued after call decomposition.
        if let Some(decomposed) = decomposition_map.get_name(func_name.as_str()) {
            for stmt in &mut func.body {
                rewrite_field_access_in_statement(stmt, decomposed);
                rewrite_whole_record_params_in_statement(stmt, decomposed);
            }
        }
    }
    Ok(())
}

fn decompose_record_calls_in_function_parameter(
    parameter: &mut rumoca_core::FunctionParam,
    decomposition_map: &RecordDecompositionMap,
    local_record_params: Option<&HashSet<String>>,
    aggregate_record_values: Option<&HashSet<String>>,
) -> Result<(), FlattenError> {
    for expression in [
        &mut parameter.default,
        &mut parameter.min,
        &mut parameter.max,
    ]
    .into_iter()
    .flatten()
    {
        decompose_record_call_args_in_expr_scoped(
            expression,
            decomposition_map,
            local_record_params,
            aggregate_record_values,
        )?;
    }
    for subscript in &mut parameter.shape_expr {
        if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
            decompose_record_call_args_in_expr_scoped(
                expr,
                decomposition_map,
                local_record_params,
                aggregate_record_values,
            )?;
        }
    }
    Ok(())
}

fn decompose_record_calls_in_when_equations(
    equations: &mut [flat::WhenEquation],
    decomposition_map: &RecordDecompositionMap,
) -> Result<(), FlattenError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                decompose_record_call_args_in_expr(value, decomposition_map, None)?;
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                decompose_record_call_args_in_expr(condition, decomposition_map, None)?;
                decompose_record_call_args_in_expr(message, decomposition_map, None)?;
                if let Some(level) = level.as_deref_mut() {
                    decompose_record_call_args_in_expr(level, decomposition_map, None)?;
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                decompose_record_call_args_in_expr(message, decomposition_map, None)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    decompose_record_call_args_in_expr(condition, decomposition_map, None)?;
                    decompose_record_calls_in_when_equations(branch_equations, decomposition_map)?;
                }
                if let Some(else_branch) = else_branch {
                    decompose_record_calls_in_when_equations(else_branch, decomposition_map)?;
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                decompose_record_call_args_in_expr(function, decomposition_map, None)?;
            }
        }
    }
    Ok(())
}

struct DecomposedParam {
    original_index: usize,
    param_name: String,
    constructor_name: String,
    constructor_def_id: rumoca_core::DefId,
    fields: Vec<rumoca_core::FunctionParam>,
}

/// Record-parameter lowering follows callable identity, not the spelling a
/// call happened to carry before late canonicalization.
///
/// A replaceable/package-qualified call can still have its source-relative
/// name when this pass runs. Its resolved function instance or declaration is
/// the stable fact shared with the collected function table. The name index is
/// retained only for compiler-generated calls that have no source identity.
#[derive(Default)]
struct RecordDecompositionMap {
    by_name: HashMap<String, Vec<DecomposedParam>>,
    name_by_instance: HashMap<rumoca_core::FunctionInstanceId, String>,
    unique_name_by_def: HashMap<rumoca_core::DefId, Option<String>>,
}

impl RecordDecompositionMap {
    fn insert(
        &mut self,
        name: String,
        instance_id: Option<rumoca_core::FunctionInstanceId>,
        def_id: Option<rumoca_core::DefId>,
        decomposition: Vec<DecomposedParam>,
    ) {
        if let Some(instance_id) = instance_id {
            self.name_by_instance.insert(instance_id, name.clone());
        }
        if let Some(def_id) = def_id {
            self.unique_name_by_def
                .entry(def_id)
                .and_modify(|candidate| *candidate = None)
                .or_insert_with(|| Some(name.clone()));
        }
        self.by_name.insert(name, decomposition);
    }

    fn is_empty(&self) -> bool {
        self.by_name.is_empty()
    }

    fn get_name(&self, name: &str) -> Option<&[DecomposedParam]> {
        self.by_name.get(name).map(Vec::as_slice)
    }

    fn get_reference(&self, reference: &rumoca_core::Reference) -> Option<&[DecomposedParam]> {
        if let Some(resolved) = reference.resolved_function()
            && let Some(name) = self.name_by_instance.get(&resolved.instance_id)
        {
            return self.get_name(name);
        }
        if let Some(name) = reference
            .target_def_id()
            .and_then(|def_id| self.unique_name_by_def.get(&def_id))
            .and_then(Option::as_ref)
        {
            return self.get_name(name);
        }
        self.get_name(reference.as_str())
    }
}

fn record_param_shape_source(param: &DecomposedParam) -> Option<String> {
    param
        .fields
        .iter()
        .find(|field| field.dimensions().is_empty() && field.shape_expr.is_empty())
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
    // A record field binding is a constructor default, not a default for every
    // function input whose type happens to be that record. Calls already pass
    // every projected field when they supply the record argument. Retaining
    // the constructor binding here both changes MLS function-default semantics
    // and leaves sibling field references (for example `weight = mass * g`)
    // outside the decomposed function-input coordinate space.
    param.default = None;
    let dimensions = original_param
        .dimensions()
        .iter()
        .chain(field.dimensions().iter())
        .copied()
        .collect::<Vec<_>>();
    param.effective_type = rumoca_core::EffectiveType::new(
        field.effective_type.nominal_type(),
        field.effective_type.canonical_type(),
        dimensions,
    )
    .expect("concatenating checked function dimensions preserves the type contract");
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
    map: &RecordDecompositionMap,
    local_record_params: Option<&HashSet<String>>,
) -> Result<(), FlattenError> {
    decompose_record_call_args_in_stmt_scoped(stmt, map, local_record_params, None)
}

fn decompose_record_call_args_in_stmt_scoped(
    stmt: &mut rumoca_core::Statement,
    map: &RecordDecompositionMap,
    local_record_params: Option<&HashSet<String>>,
    aggregate_record_values: Option<&HashSet<String>>,
) -> Result<(), FlattenError> {
    let mut decomposer = RecordCallArgDecomposer {
        map,
        local_record_params,
        aggregate_record_values,
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
    map: &RecordDecompositionMap,
    local_record_params: Option<&HashSet<String>>,
) -> Result<(), FlattenError> {
    decompose_record_call_args_in_expr_scoped(expr, map, local_record_params, None)
}

fn decompose_record_call_args_in_expr_scoped(
    expr: &mut rumoca_core::Expression,
    map: &RecordDecompositionMap,
    local_record_params: Option<&HashSet<String>>,
    aggregate_record_values: Option<&HashSet<String>>,
) -> Result<(), FlattenError> {
    let mut decomposer = RecordCallArgDecomposer {
        map,
        local_record_params,
        aggregate_record_values,
        error: None,
    };
    *expr = decomposer.rewrite_expression(expr);
    match decomposer.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

struct RecordCallArgDecomposer<'a> {
    map: &'a RecordDecompositionMap,
    local_record_params: Option<&'a HashSet<String>>,
    aggregate_record_values: Option<&'a HashSet<String>>,
    error: Option<FlattenError>,
}

impl RecordCallArgDecomposer<'_> {
    fn args_for_call(
        &mut self,
        name: &rumoca_core::Reference,
        rewritten_args: Vec<rumoca_core::Expression>,
    ) -> Vec<rumoca_core::Expression> {
        let Some(decomposed) = self.map.get_reference(name) else {
            return rewritten_args;
        };
        match decompose_record_call_args(
            name.as_str(),
            &rewritten_args,
            decomposed,
            self.local_record_params,
            self.aggregate_record_values,
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
        let rewritten_comp = self.rewrite_reference(comp);
        let rewritten_args = self.rewrite_expressions(args);
        let args = self.args_for_call(&rewritten_comp, rewritten_args);
        rumoca_core::Statement::FunctionCall {
            comp: rewritten_comp,
            args,
            outputs: outputs
                .iter()
                .map(|output| {
                    output
                        .as_ref()
                        .map(|output| self.rewrite_component_reference(output))
                })
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
    aggregate_record_values: Option<&HashSet<String>>,
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
                aggregate_record_values,
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
                aggregate_record_values,
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
        name: rumoca_core::Reference::generated(format!(
            "{}{name}",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        args: vec![value],
        is_constructor: true,
        span,
    }
}

/// Expand a record argument into scalar field arguments.
fn expand_record_arg(
    function_name: &str,
    arg: &rumoca_core::Expression,
    fields: &[rumoca_core::FunctionParam],
    local_record_params: Option<&HashSet<String>>,
    aggregate_record_values: Option<&HashSet<String>>,
    out: &mut Vec<rumoca_core::Expression>,
) -> Result<(), FlattenError> {
    if expand_record_constructor_arg(function_name, arg, fields, out)? {
        return Ok(());
    }

    if expand_record_var_ref(
        arg,
        fields,
        local_record_params,
        aggregate_record_values,
        out,
    )? {
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
            field_def_id: field.def_id.ok_or_else(|| {
                FlattenError::missing_resolved_class_metadata(
                    &field.name,
                    "record function field identity",
                    source_span,
                )
            })?,
            span: source_span,
        });
    }
    Ok(())
}

fn expand_record_var_ref(
    arg: &rumoca_core::Expression,
    fields: &[rumoca_core::FunctionParam],
    local_record_params: Option<&HashSet<String>>,
    aggregate_record_values: Option<&HashSet<String>>,
    out: &mut Vec<rumoca_core::Expression>,
) -> Result<bool, FlattenError> {
    let rumoca_core::Expression::VarRef { name, span, .. } = arg else {
        return Ok(false);
    };
    // A generated name is a record group this pass itself synthesized while
    // decomposing an enclosing record parameter (`left.rotation` ->
    // `left_rotation`). It names no source declaration, so it is projected by
    // the same generated scheme that created it rather than through a
    // structured declaration chain it never had.
    let is_local = name.is_generated()
        || local_record_params.is_some_and(|params| params.contains(name.as_str()));
    if aggregate_record_values.is_some_and(|values| values.contains(name.as_str())) {
        for field in fields {
            let source_span = record_field_access_source_span(arg, field)?;
            if let Some(empty) = empty_record_field_arg(field, source_span) {
                out.push(empty);
                continue;
            }
            out.push(rumoca_core::Expression::FieldAccess {
                base: Box::new(arg.clone()),
                field: field.name.clone(),
                field_def_id: field
                    .def_id
                    .expect("record argument metadata validates field identity"),
                span: source_span,
            });
        }
        return Ok(true);
    }
    for field in fields {
        if let Some(empty) = empty_record_field_arg(field, *span) {
            out.push(empty);
            continue;
        }
        if is_local {
            out.push(record_param_field_var_ref(
                name.as_str(),
                field.name.as_str(),
                *span,
            ));
        } else {
            out.push(rumoca_core::Expression::VarRef {
                name: record_field_reference(
                    name,
                    field.name.as_str(),
                    field
                        .def_id
                        .expect("record argument metadata validates field identity"),
                    *span,
                )?,
                subscripts: vec![],
                span: *span,
            });
        }
    }
    Ok(true)
}

fn expand_record_constructor_arg(
    function_name: &str,
    arg: &rumoca_core::Expression,
    fields: &[rumoca_core::FunctionParam],
    out: &mut Vec<rumoca_core::Expression>,
) -> Result<bool, FlattenError> {
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
            out.push(record_constructor_field_arg(
                function_name,
                ctor_args,
                &positional,
                i,
                field,
                *span,
            )?);
        }
        return Ok(true);
    }
    Ok(false)
}

fn record_constructor_field_arg(
    function_name: &str,
    ctor_args: &[rumoca_core::Expression],
    positional: &[&rumoca_core::Expression],
    index: usize,
    field: &rumoca_core::FunctionParam,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, FlattenError> {
    if let Some(empty) = empty_record_field_arg(field, span) {
        return Ok(empty);
    }
    if let Some(value) = named_constructor_arg(ctor_args, field.name.as_str()) {
        return Ok(value.clone());
    }
    if let Some(value) = positional.get(index) {
        return Ok((*value).clone());
    }
    if let Some(default) = &field.default {
        return Ok(default.clone());
    }
    Err(missing_record_constructor_field_error(
        function_name,
        field.name.as_str(),
        span,
    ))
}

fn empty_record_field_arg(
    field: &rumoca_core::FunctionParam,
    span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    // A zero in `dims` is also the shape sentinel for an unresolved/flexible
    // axis such as `Real x[:]`.  Only a retained literal-zero declaration is
    // proof that the field is genuinely empty.
    field
        .shape_expr
        .iter()
        .any(|subscript| matches!(subscript, rumoca_core::Subscript::Index { value: 0, .. }))
        .then_some(rumoca_core::Expression::Array {
            elements: Vec::new(),
            is_matrix: field.dimensions().len() == 2,
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
    field_def_id: rumoca_core::DefId,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Reference, FlattenError> {
    let provenance = rumoca_core::ProvenanceSpan::new(span, "record function field projection")
        .map_err(|error| FlattenError::missing_source_context(error.to_string()))?;
    base.with_appended_field(field, field_def_id, provenance)
        .map_err(|error| {
            FlattenError::missing_resolved_class_metadata(
                field,
                format!("record function field projection: {error}"),
                span,
            )
        })
}

#[cfg(test)]
#[path = "function_lowering/tests.rs"]
mod tests;
