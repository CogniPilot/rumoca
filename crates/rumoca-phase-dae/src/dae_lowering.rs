//! DAE-level lowering passes for code generation.
//!
//! SPEC_0021 file-size exception: split plan is to move focused lowering
//! helpers into owned submodules after BOPTEST parity stabilization.
//!
//! This module contains record function parameter decomposition, array size
//! argument insertion, parameter dependency sorting, and vector equation
//! scalarization that operate on the DAE IR before code generation.

use crate::ToDaeError;
use crate::scalar_size::compute_var_size;
use indexmap::{IndexMap, IndexSet};
use rumoca_core::{ExpressionRewriter, ExpressionVisitor, StatementRewriter};
use rumoca_ir_dae as dae;
use rumoca_ir_dae::DaeExpressionRewriter;
use std::collections::{BTreeMap, HashMap, HashSet};

type Dae = dae::Dae;
type RecordArgMap = HashMap<String, Vec<(usize, Vec<String>)>>;

/// DAE value prepared for code generators that need DAE-level convenience
/// rewrites without mutating the simulation DAE.
#[derive(Debug, Clone)]
pub struct CodegenDae {
    dae: Dae,
}

impl CodegenDae {
    /// Borrow the prepared DAE.
    pub fn as_dae(&self) -> &dae::Dae {
        &self.dae
    }

    /// Consume the wrapper and return the prepared DAE.
    pub fn into_dae(self) -> dae::Dae {
        self.dae
    }
}

/// Prepare a DAE copy for code generators.
///
/// This applies codegen-only rewrites such as record-parameter decomposition
/// and array-size argument insertion to a cloned DAE so the caller cannot
/// accidentally reuse a simulation DAE after backend-specific mutation.
#[must_use]
pub fn prepare_dae_for_codegen(dae: &dae::Dae) -> CodegenDae {
    let mut prepared = dae.clone();
    lower_record_function_params_dae(&mut prepared);
    unwrap_block_constructor_value_wrappers_dae(&mut prepared);
    insert_array_size_args_dae(&mut prepared);
    CodegenDae { dae: prepared }
}

pub(crate) fn unwrap_block_constructor_value_wrappers_dae(dae: &mut Dae) {
    let wrapper_names = block_constructor_value_wrapper_names(dae);
    if wrapper_names.is_empty() {
        return;
    }
    BlockConstructorValueWrapperUnwrapper {
        names: &wrapper_names,
    }
    .rewrite_dae(dae);
}

fn block_constructor_value_wrapper_names(dae: &Dae) -> HashSet<String> {
    dae.symbols
        .functions
        .iter()
        .filter_map(|(name, function)| {
            let block_like_constructor = function.is_constructor
                && function.body.is_empty()
                && function.outputs.is_empty()
                && function
                    .inputs
                    .iter()
                    .any(|param| param.type_class == Some(rumoca_core::ClassType::Connector));
            block_like_constructor.then(|| name.as_str().to_string())
        })
        .collect()
}

struct BlockConstructorValueWrapperUnwrapper<'a> {
    names: &'a HashSet<String>,
}

impl ExpressionRewriter for BlockConstructorValueWrapperUnwrapper<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        {
            let rewritten_args = self.rewrite_expressions(args);
            if *is_constructor && rewritten_args.len() == 1 && self.names.contains(name.as_str()) {
                return rewritten_args[0].clone();
            }
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args: rewritten_args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}

impl StatementRewriter for BlockConstructorValueWrapperUnwrapper<'_> {}

impl DaeExpressionRewriter for BlockConstructorValueWrapperUnwrapper<'_> {}

// =============================================================================
// Record function parameter decomposition (DAE level)
// =============================================================================

pub(crate) fn record_constructor_fields_from_metadata<'a, I>(
    functions: I,
    type_name: &str,
) -> Option<Vec<rumoca_core::FunctionParam>>
where
    I: IntoIterator<Item = (&'a rumoca_core::VarName, &'a rumoca_core::Function)>,
{
    functions
        .into_iter()
        .find(|(name, function)| {
            function.is_constructor
                && rumoca_core::qualified_type_name_matches(name.as_str(), type_name)
        })
        .map(|(_, function)| function.inputs.clone())
        .filter(|fields| !fields.is_empty())
}

fn record_fields_from_constructor_metadata(
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    type_name: &str,
) -> Option<Vec<rumoca_core::FunctionParam>> {
    record_constructor_fields_from_metadata(functions.iter(), type_name)
}

/// Decompose record-typed function params in the DAE. This rewrites function
/// signatures (replacing `c: Complex` with `c_re, c_im: Real`) and decomposes
/// call-site arguments throughout all DAE equations.
/// Decompose record-typed function params in the DAE for code generators.
/// Must NOT be called before simulation — only before codegen rendering.
pub fn lower_record_function_params_dae(dae: &mut Dae) {
    let record_fields_by_type = dae
        .symbols
        .functions
        .values()
        .flat_map(|function| function.inputs.iter())
        .filter(|input| input.type_class == Some(rumoca_core::ClassType::Record))
        .filter_map(|input| {
            record_fields_from_constructor_metadata(&dae.symbols.functions, &input.type_name)
                .map(|fields| (input.type_name.clone(), fields))
        })
        .collect::<HashMap<_, _>>();

    for func in dae.symbols.functions.values_mut() {
        ensure_decomposed_air_state_x_input(func);
    }

    let mut requirements: HashMap<String, Vec<DaeRecordParamRequirement>> = HashMap::new();
    for (func_name, func) in dae.symbols.functions.iter() {
        let mut decomposed: Vec<DaeRecordParamRequirement> = Vec::new();
        for (idx, input) in func.inputs.iter().enumerate() {
            log_record_lowering_debug(func_name.as_str(), func, input);
            if let Some(fields) = record_fields_by_type.get(&input.type_name) {
                let mut fields = fields.clone();
                merge_inferred_decomposed_record_fields(
                    &mut fields,
                    infer_decomposed_record_fields_from_function_body(func, &input.name),
                );
                ensure_air_thermodynamic_state_fields(func.name.as_str(), input, &mut fields, func);
                decomposed.push(DaeRecordParamRequirement {
                    original_index: idx,
                    param_name: input.name.clone(),
                    fields,
                });
            } else if input.type_class == Some(rumoca_core::ClassType::Record) {
                let mut fields =
                    infer_decomposed_record_fields_from_function_body(func, &input.name);
                ensure_air_thermodynamic_state_fields(func.name.as_str(), input, &mut fields, func);
                if !fields.is_empty() {
                    decomposed.push(DaeRecordParamRequirement {
                        original_index: idx,
                        param_name: input.name.clone(),
                        fields,
                    });
                }
            }
        }
        if !decomposed.is_empty() {
            requirements.insert(func_name.as_str().to_string(), decomposed);
        }
    }

    if requirements.is_empty() {
        return;
    }

    propagate_nested_record_field_requirements(&dae.symbols.functions, &mut requirements);

    // Identify functions with record params and rewrite their signatures.
    let mut decomp_map: HashMap<String, Vec<(usize, Vec<String>)>> = HashMap::new();

    for (func_name, func) in dae.symbols.functions.iter_mut() {
        let Some(decomposed) = requirements.get(func_name.as_str()) else {
            continue;
        };
        let local_record_params = decomposed
            .iter()
            .map(|requirement| requirement.param_name.clone())
            .collect::<HashSet<_>>();
        for stmt in &mut func.body {
            rewrite_dae_record_field_access_in_statement(stmt, &local_record_params);
        }

        // Replace record inputs with scalar field inputs
        let old_inputs = std::mem::take(&mut func.inputs);
        for (idx, input) in old_inputs.into_iter().enumerate() {
            let Some(dp) = decomposed.iter().find(|dp| dp.original_index == idx) else {
                func.inputs.push(input);
                continue;
            };
            for field in &dp.fields {
                let mut param = field.clone();
                param.name = format!("{}_{}", dp.param_name, field.name);
                param.dims = input
                    .dims
                    .iter()
                    .chain(field.dims.iter())
                    .copied()
                    .collect();
                param.shape_expr = input
                    .shape_expr
                    .iter()
                    .chain(field.shape_expr.iter())
                    .cloned()
                    .collect();
                func.inputs.push(param);
            }
        }

        let entry: Vec<(usize, Vec<String>)> = decomposed
            .iter()
            .map(|dp| {
                (
                    dp.original_index,
                    dp.fields.iter().map(|field| field.name.clone()).collect(),
                )
            })
            .collect();
        decomp_map.insert(func_name.as_str().to_string(), entry);
    }

    if decomp_map.is_empty() {
        return;
    }

    DaeRecordArgDecomposer {
        map: &decomp_map,
        local_record_params: None,
    }
    .rewrite_dae(dae);
    for (func_name, func) in dae.symbols.functions.iter_mut() {
        let local_record_params = requirements.get(func_name.as_str()).map(|requirements| {
            requirements
                .iter()
                .map(|requirement| requirement.param_name.clone())
                .collect::<HashSet<_>>()
        });
        for stmt in &mut func.body {
            decompose_record_args_dae_stmt(stmt, &decomp_map, local_record_params.as_ref());
            if let Some(local_record_params) = local_record_params.as_ref() {
                rewrite_dae_record_field_access_in_statement(stmt, local_record_params);
            }
        }
    }
}

fn log_record_lowering_debug(
    map_name: &str,
    func: &rumoca_core::Function,
    input: &rumoca_core::FunctionParam,
) {
    if !record_lowering_debug_enabled() || !func.name.as_str().contains("specificEnthalpy") {
        return;
    }
    log_record_lowering_debug_line(format!(
        "DEBUG DAE RECORD LOWERING map_name={} func_name={} input={} type={} class={:?}",
        map_name,
        func.name.as_str(),
        input.name,
        input.type_name,
        input.type_class
    ));
}

fn record_lowering_debug_enabled() -> bool {
    #[cfg(feature = "tracing")]
    {
        tracing::enabled!(
            target: "rumoca_phase_dae::record_lowering",
            tracing::Level::DEBUG
        )
    }
    #[cfg(not(feature = "tracing"))]
    {
        false
    }
}

fn log_record_lowering_debug_line(message: String) {
    #[cfg(feature = "tracing")]
    tracing::debug!(target: "rumoca_phase_dae::record_lowering", message = %message);

    #[cfg(not(feature = "tracing"))]
    let _ = message;
}

fn ensure_decomposed_air_state_x_input(func: &mut rumoca_core::Function) {
    if !func.name.as_str().contains(".Media.Air.")
        || !func.inputs.iter().any(|input| input.name == "state_p")
        || !func.inputs.iter().any(|input| input.name == "state_T")
        || func.inputs.iter().any(|input| input.name == "state_X")
    {
        return;
    }
    if !function_body_references_name(func, "state_X") {
        return;
    }
    let width = inferred_indexed_name_width(func, "state_X").unwrap_or(1);
    func.inputs
        .push(rumoca_core::FunctionParam::new("state_X", "MassFraction").with_dims(vec![width]));
}

fn function_body_references_name(func: &rumoca_core::Function, name: &str) -> bool {
    func.inputs
        .iter()
        .chain(func.outputs.iter())
        .chain(func.locals.iter())
        .any(|param| {
            param
                .default
                .as_ref()
                .is_some_and(|expr| expression_references_name(expr, name))
        })
        || func
            .body
            .iter()
            .any(|stmt| statement_references_name(stmt, name))
}

fn statement_references_name(stmt: &rumoca_core::Statement, name: &str) -> bool {
    match stmt {
        rumoca_core::Statement::Assignment { value, .. }
        | rumoca_core::Statement::Reinit { value, .. } => expression_references_name(value, name),
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            indices
                .iter()
                .any(|index| expression_references_name(&index.range, name))
                || equations
                    .iter()
                    .any(|stmt| statement_references_name(stmt, name))
        }
        rumoca_core::Statement::While { block, .. } => {
            expression_references_name(&block.cond, name)
                || block
                    .stmts
                    .iter()
                    .any(|stmt| statement_references_name(stmt, name))
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| {
                expression_references_name(&block.cond, name)
                    || block
                        .stmts
                        .iter()
                        .any(|stmt| statement_references_name(stmt, name))
            }) || else_block.as_ref().is_some_and(|else_block| {
                else_block
                    .iter()
                    .any(|stmt| statement_references_name(stmt, name))
            })
        }
        rumoca_core::Statement::When { blocks, .. } => blocks.iter().any(|block| {
            expression_references_name(&block.cond, name)
                || block
                    .stmts
                    .iter()
                    .any(|stmt| statement_references_name(stmt, name))
        }),
        rumoca_core::Statement::FunctionCall { args, .. } => {
            args.iter().any(|arg| expression_references_name(arg, name))
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            expression_references_name(condition, name)
                || expression_references_name(message, name)
                || level
                    .as_ref()
                    .is_some_and(|level| expression_references_name(level, name))
        }
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => false,
    }
}

fn expression_references_name(expr: &rumoca_core::Expression, name: &str) -> bool {
    match expr {
        rumoca_core::Expression::VarRef {
            name: reference,
            subscripts,
            ..
        } => {
            reference.as_str() == name
                || reference.as_str().starts_with(&format!("{name}["))
                || subscripts
                    .iter()
                    .any(|subscript| subscript_references_name(subscript, name))
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expression_references_name(lhs, name) || expression_references_name(rhs, name)
        }
        rumoca_core::Expression::Unary { rhs, .. } => expression_references_name(rhs, name),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(cond, value)| {
                expression_references_name(cond, name) || expression_references_name(value, name)
            }) || expression_references_name(else_branch, name)
        }
        rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            args.iter().any(|arg| expression_references_name(arg, name))
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            expression_references_name(start, name)
                || step
                    .as_ref()
                    .is_some_and(|step| expression_references_name(step, name))
                || expression_references_name(end, name)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            expression_references_name(base, name)
                || subscripts
                    .iter()
                    .any(|subscript| subscript_references_name(subscript, name))
        }
        rumoca_core::Expression::FieldAccess { base, .. } => expression_references_name(base, name),
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            expression_references_name(expr, name)
                || indices
                    .iter()
                    .any(|index| expression_references_name(&index.range, name))
                || filter
                    .as_ref()
                    .is_some_and(|filter| expression_references_name(filter, name))
        }
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => false,
    }
}

fn subscript_references_name(subscript: &rumoca_core::Subscript, name: &str) -> bool {
    if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
        return expression_references_name(expr, name);
    }
    false
}

fn collect_max_literal_index_for_name_in_statement(
    stmt: &rumoca_core::Statement,
    name: &str,
    max_index: &mut Option<usize>,
) {
    match stmt {
        rumoca_core::Statement::Assignment { value, .. } => {
            collect_max_literal_index_for_name(value, name, max_index);
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                collect_max_literal_index_for_name(&block.cond, name, max_index);
                for stmt in &block.stmts {
                    collect_max_literal_index_for_name_in_statement(stmt, name, max_index);
                }
            }
            if let Some(else_block) = else_block {
                for stmt in else_block {
                    collect_max_literal_index_for_name_in_statement(stmt, name, max_index);
                }
            }
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            for index in indices {
                collect_max_literal_index_for_name(&index.range, name, max_index);
            }
            for stmt in equations {
                collect_max_literal_index_for_name_in_statement(stmt, name, max_index);
            }
        }
        rumoca_core::Statement::While { block, .. } => {
            collect_max_literal_index_for_name(&block.cond, name, max_index);
            for stmt in &block.stmts {
                collect_max_literal_index_for_name_in_statement(stmt, name, max_index);
            }
        }
        rumoca_core::Statement::When { blocks, .. } => {
            for block in blocks {
                collect_max_literal_index_for_name(&block.cond, name, max_index);
                for stmt in &block.stmts {
                    collect_max_literal_index_for_name_in_statement(stmt, name, max_index);
                }
            }
        }
        rumoca_core::Statement::FunctionCall { args, .. } => {
            for arg in args {
                collect_max_literal_index_for_name(arg, name, max_index);
            }
        }
        rumoca_core::Statement::Reinit { value, .. } => {
            collect_max_literal_index_for_name(value, name, max_index);
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            collect_max_literal_index_for_name(condition, name, max_index);
            collect_max_literal_index_for_name(message, name, max_index);
            if let Some(level) = level {
                collect_max_literal_index_for_name(level, name, max_index);
            }
        }
        rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. }
        | rumoca_core::Statement::Empty { .. } => {}
    }
}

fn collect_max_literal_index_for_name(
    expr: &rumoca_core::Expression,
    name: &str,
    max_index: &mut Option<usize>,
) {
    match expr {
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            if expression_is_name(base, name) {
                for subscript in subscripts {
                    if let Some(index) = literal_positive_index(subscript) {
                        *max_index = Some(max_index.map_or(index, |current| current.max(index)));
                    }
                }
            }
            collect_max_literal_index_for_name(base, name, max_index);
            for subscript in subscripts {
                if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
                    collect_max_literal_index_for_name(expr, name, max_index);
                }
            }
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            collect_max_literal_index_for_name(lhs, name, max_index);
            collect_max_literal_index_for_name(rhs, name, max_index);
        }
        rumoca_core::Expression::Unary { rhs: expr, .. }
        | rumoca_core::Expression::FieldAccess { base: expr, .. } => {
            collect_max_literal_index_for_name(expr, name, max_index);
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            for arg in args {
                collect_max_literal_index_for_name(arg, name, max_index);
            }
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, branch) in branches {
                collect_max_literal_index_for_name(cond, name, max_index);
                collect_max_literal_index_for_name(branch, name, max_index);
            }
            collect_max_literal_index_for_name(else_branch, name, max_index);
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            collect_max_literal_index_for_name(start, name, max_index);
            if let Some(step) = step {
                collect_max_literal_index_for_name(step, name, max_index);
            }
            collect_max_literal_index_for_name(end, name, max_index);
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            collect_max_literal_index_for_name(expr, name, max_index);
            for index in indices {
                collect_max_literal_index_for_name(&index.range, name, max_index);
            }
            if let Some(filter) = filter {
                collect_max_literal_index_for_name(filter, name, max_index);
            }
        }
        rumoca_core::Expression::VarRef { .. }
        | rumoca_core::Expression::Literal { .. }
        | rumoca_core::Expression::Empty { .. } => {}
    }
}

fn expression_is_name(expr: &rumoca_core::Expression, name: &str) -> bool {
    match expr {
        rumoca_core::Expression::VarRef {
            name: expr_name,
            subscripts,
            ..
        } => subscripts.is_empty() && expr_name.as_str() == name,
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let Some((prefix, suffix)) = split_last_field_name(name) else {
                return false;
            };
            field == suffix && expression_is_name(base, prefix)
        }
        _ => false,
    }
}

fn split_last_field_name(name: &str) -> Option<(&str, &str)> {
    let idx = name.rfind('.')?;
    let prefix = &name[..idx];
    let suffix = &name[idx + 1..];
    (!prefix.is_empty() && !suffix.is_empty()).then_some((prefix, suffix))
}

fn literal_positive_index(subscript: &rumoca_core::Subscript) -> Option<usize> {
    let rumoca_core::Subscript::Expr { expr, .. } = subscript else {
        return None;
    };
    let rumoca_core::Expression::Literal { value, .. } = expr.as_ref() else {
        return None;
    };
    match value {
        rumoca_core::Literal::Integer(value) if *value > 0 => Some(*value as usize),
        _ => None,
    }
}

fn ensure_air_thermodynamic_state_fields(
    func_name: &str,
    input: &rumoca_core::FunctionParam,
    fields: &mut Vec<rumoca_core::FunctionParam>,
    func: &rumoca_core::Function,
) {
    if input.name != "state" || !func_name.contains(".Media.Air.") {
        return;
    }
    if fields.iter().any(|field| field.name == "X") {
        return;
    }
    let width = inferred_indexed_name_width(func, "state_X")
        .or_else(|| inferred_indexed_name_width(func, "state.X"))
        .unwrap_or(1);
    fields.push(rumoca_core::FunctionParam::new("X", "MassFraction").with_dims(vec![width]));
}

fn inferred_indexed_name_width(func: &rumoca_core::Function, name: &str) -> Option<i64> {
    let mut max_index = None;
    for param in func
        .inputs
        .iter()
        .chain(func.outputs.iter())
        .chain(func.locals.iter())
    {
        if let Some(default) = param.default.as_ref() {
            collect_max_literal_index_for_name(default, name, &mut max_index);
        }
    }
    for stmt in &func.body {
        collect_max_literal_index_for_name_in_statement(stmt, name, &mut max_index);
    }
    max_index.map(|idx| idx as i64)
}

fn rewrite_dae_record_field_access_in_statement(
    stmt: &mut rumoca_core::Statement,
    params: &HashSet<String>,
) {
    *stmt = DaeRecordFieldAccessRewriter { params }.rewrite_statement(stmt);
}

struct DaeRecordFieldAccessRewriter<'a> {
    params: &'a HashSet<String>,
}

impl ExpressionRewriter for DaeRecordFieldAccessRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let Some(rewritten) = rewrite_dae_component_ref_record_field(expr, self.params) {
            return rewritten;
        }
        if let rumoca_core::Expression::FieldAccess { base, field, span } = expr
            && let rumoca_core::Expression::VarRef { name, .. } = base.as_ref()
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

impl StatementRewriter for DaeRecordFieldAccessRewriter<'_> {}

fn rewrite_dae_component_ref_record_field(
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
    for param in params {
        let Some(field) = name.as_str().strip_prefix(&format!("{param}.")) else {
            continue;
        };
        if field.is_empty() || field.contains('.') {
            continue;
        }
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(format!("{param}_{field}")),
            subscripts: vec![],
            span: *span,
        });
    }
    None
}

#[derive(Clone)]
struct DaeRecordParamRequirement {
    original_index: usize,
    param_name: String,
    fields: Vec<rumoca_core::FunctionParam>,
}

fn propagate_nested_record_field_requirements(
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    requirements: &mut HashMap<String, Vec<DaeRecordParamRequirement>>,
) {
    loop {
        let snapshot = requirements.clone();
        let mut additions: Vec<(String, String, Vec<rumoca_core::FunctionParam>)> = Vec::new();
        for (func_name, func) in functions {
            let Some(caller_requirements) = snapshot.get(func_name.as_str()) else {
                continue;
            };
            collect_nested_record_field_demands_in_statements(
                &func.body,
                func_name.as_str(),
                caller_requirements,
                &snapshot,
                &mut additions,
            );
        }
        let mut changed = false;
        for (func_name, param_name, fields) in additions {
            let Some(params) = requirements.get_mut(func_name.as_str()) else {
                continue;
            };
            let Some(requirement) = params
                .iter_mut()
                .find(|requirement| requirement.param_name == param_name)
            else {
                continue;
            };
            let before = requirement.fields.len();
            merge_inferred_decomposed_record_fields(&mut requirement.fields, fields);
            changed |= requirement.fields.len() != before;
        }
        if !changed {
            break;
        }
    }
}

fn collect_nested_record_field_demands_in_statements(
    statements: &[rumoca_core::Statement],
    caller_name: &str,
    caller_requirements: &[DaeRecordParamRequirement],
    all_requirements: &HashMap<String, Vec<DaeRecordParamRequirement>>,
    additions: &mut Vec<(String, String, Vec<rumoca_core::FunctionParam>)>,
) {
    for stmt in statements {
        collect_nested_record_field_demands_in_statement(
            stmt,
            caller_name,
            caller_requirements,
            all_requirements,
            additions,
        );
    }
}

fn collect_nested_record_field_demands_in_statement(
    stmt: &rumoca_core::Statement,
    caller_name: &str,
    caller_requirements: &[DaeRecordParamRequirement],
    all_requirements: &HashMap<String, Vec<DaeRecordParamRequirement>>,
    additions: &mut Vec<(String, String, Vec<rumoca_core::FunctionParam>)>,
) {
    match stmt {
        rumoca_core::Statement::Assignment { value, .. }
        | rumoca_core::Statement::Reinit { value, .. } => collect_nested_record_field_demands(
            value,
            caller_name,
            caller_requirements,
            all_requirements,
            additions,
        ),
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            for index in indices {
                collect_nested_record_field_demands(
                    &index.range,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
            collect_nested_record_field_demands_in_statements(
                equations,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
        }
        rumoca_core::Statement::While { block, .. } => {
            collect_nested_record_field_demands(
                &block.cond,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
            collect_nested_record_field_demands_in_statements(
                &block.stmts,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                collect_nested_record_field_demands(
                    &block.cond,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
                collect_nested_record_field_demands_in_statements(
                    &block.stmts,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
            if let Some(else_block) = else_block {
                collect_nested_record_field_demands_in_statements(
                    else_block,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
        }
        rumoca_core::Statement::When { blocks, .. } => {
            for block in blocks {
                collect_nested_record_field_demands(
                    &block.cond,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
                collect_nested_record_field_demands_in_statements(
                    &block.stmts,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
        }
        rumoca_core::Statement::FunctionCall { args, .. } => {
            for arg in args {
                collect_nested_record_field_demands(
                    arg,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            collect_nested_record_field_demands(
                condition,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
            collect_nested_record_field_demands(
                message,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
            if let Some(level) = level {
                collect_nested_record_field_demands(
                    level,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
        }
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => {}
    }
}

fn collect_nested_record_field_demands(
    expr: &rumoca_core::Expression,
    caller_name: &str,
    caller_requirements: &[DaeRecordParamRequirement],
    all_requirements: &HashMap<String, Vec<DaeRecordParamRequirement>>,
    additions: &mut Vec<(String, String, Vec<rumoca_core::FunctionParam>)>,
) {
    match expr {
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            if let Some(callee_requirements) = all_requirements.get(name.as_str()) {
                for callee_requirement in callee_requirements {
                    let Some(arg) = args.get(callee_requirement.original_index) else {
                        continue;
                    };
                    if let rumoca_core::Expression::VarRef { name, .. } = arg {
                        for caller_requirement in caller_requirements {
                            if name.as_str() == caller_requirement.param_name {
                                additions.push((
                                    caller_name.to_string(),
                                    caller_requirement.param_name.clone(),
                                    callee_requirement.fields.clone(),
                                ));
                            }
                        }
                    }
                }
            }
            for arg in args {
                collect_nested_record_field_demands(
                    arg,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            for arg in args {
                collect_nested_record_field_demands(
                    arg,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            collect_nested_record_field_demands(
                lhs,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
            collect_nested_record_field_demands(
                rhs,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
        }
        rumoca_core::Expression::Unary { rhs, .. }
        | rumoca_core::Expression::Index { base: rhs, .. }
        | rumoca_core::Expression::FieldAccess { base: rhs, .. } => {
            collect_nested_record_field_demands(
                rhs,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, value) in branches {
                collect_nested_record_field_demands(
                    cond,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
                collect_nested_record_field_demands(
                    value,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
            collect_nested_record_field_demands(
                else_branch,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            collect_nested_record_field_demands(
                start,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
            if let Some(step) = step {
                collect_nested_record_field_demands(
                    step,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
            collect_nested_record_field_demands(
                end,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            collect_nested_record_field_demands(
                expr,
                caller_name,
                caller_requirements,
                all_requirements,
                additions,
            );
            for index in indices {
                collect_nested_record_field_demands(
                    &index.range,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
            if let Some(filter) = filter {
                collect_nested_record_field_demands(
                    filter,
                    caller_name,
                    caller_requirements,
                    all_requirements,
                    additions,
                );
            }
        }
        rumoca_core::Expression::VarRef { .. }
        | rumoca_core::Expression::Literal { .. }
        | rumoca_core::Expression::Empty { .. } => {}
    }
}

fn merge_inferred_decomposed_record_fields(
    fields: &mut Vec<rumoca_core::FunctionParam>,
    inferred: Vec<rumoca_core::FunctionParam>,
) {
    for field in inferred {
        if !fields
            .iter()
            .any(|existing| existing.name.as_str() == field.name.as_str())
        {
            fields.push(field);
        }
    }
}

fn infer_decomposed_record_fields_from_function_body(
    func: &rumoca_core::Function,
    param_name: &str,
) -> Vec<rumoca_core::FunctionParam> {
    let prefix = format!("{param_name}_");
    let mut fields = IndexSet::<String>::new();
    for param in func
        .inputs
        .iter()
        .chain(func.outputs.iter())
        .chain(func.locals.iter())
    {
        if let Some(default) = &param.default {
            collect_decomposed_record_field_refs(default, &prefix, &mut fields);
            collect_record_field_access_refs(default, param_name, &mut fields);
        }
    }
    for stmt in &func.body {
        collect_decomposed_record_field_refs_in_statement(stmt, &prefix, &mut fields);
        collect_record_field_access_refs_in_statement(stmt, param_name, &mut fields);
    }
    fields
        .into_iter()
        .map(|field| rumoca_core::FunctionParam::new(field, "Real"))
        .collect()
}

fn collect_record_field_access_refs(
    expr: &rumoca_core::Expression,
    param_name: &str,
    fields: &mut IndexSet<String>,
) {
    if let Some(field) = record_field_access_name(expr, param_name) {
        fields.insert(field);
    }
    match expr {
        rumoca_core::Expression::VarRef { subscripts, .. } => {
            for subscript in subscripts {
                collect_record_field_access_refs_in_subscript(subscript, param_name, fields);
            }
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            collect_record_field_access_refs(lhs, param_name, fields);
            collect_record_field_access_refs(rhs, param_name, fields);
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            collect_record_field_access_refs(rhs, param_name, fields);
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, value) in branches {
                collect_record_field_access_refs(cond, param_name, fields);
                collect_record_field_access_refs(value, param_name, fields);
            }
            collect_record_field_access_refs(else_branch, param_name, fields);
        }
        rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            for arg in args {
                collect_record_field_access_refs(arg, param_name, fields);
            }
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            collect_record_field_access_refs(start, param_name, fields);
            if let Some(step) = step {
                collect_record_field_access_refs(step, param_name, fields);
            }
            collect_record_field_access_refs(end, param_name, fields);
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            collect_record_field_access_refs(base, param_name, fields);
            for subscript in subscripts {
                collect_record_field_access_refs_in_subscript(subscript, param_name, fields);
            }
        }
        rumoca_core::Expression::FieldAccess { base, .. } => {
            collect_record_field_access_refs(base, param_name, fields);
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            collect_record_field_access_refs(expr, param_name, fields);
            for index in indices {
                collect_record_field_access_refs(&index.range, param_name, fields);
            }
            if let Some(filter) = filter {
                collect_record_field_access_refs(filter, param_name, fields);
            }
        }
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => {}
    }
}

fn record_field_access_name(expr: &rumoca_core::Expression, param_name: &str) -> Option<String> {
    if let rumoca_core::Expression::FieldAccess { base, field, .. } = expr
        && let rumoca_core::Expression::VarRef { name, .. } = base.as_ref()
        && name.as_str() == param_name
    {
        return Some(field.clone());
    }
    if let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
        && subscripts.is_empty()
        && let Some(field) = name.as_str().strip_prefix(&format!("{param_name}."))
        && !field.is_empty()
        && !field.contains('.')
    {
        return Some(field.to_string());
    }
    None
}

fn collect_record_field_access_refs_in_statement(
    stmt: &rumoca_core::Statement,
    param_name: &str,
    fields: &mut IndexSet<String>,
) {
    match stmt {
        rumoca_core::Statement::Assignment { value, .. }
        | rumoca_core::Statement::Reinit { value, .. } => {
            collect_record_field_access_refs(value, param_name, fields);
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            for index in indices {
                collect_record_field_access_refs(&index.range, param_name, fields);
            }
            for stmt in equations {
                collect_record_field_access_refs_in_statement(stmt, param_name, fields);
            }
        }
        rumoca_core::Statement::While { block, .. } => {
            collect_record_field_access_refs(&block.cond, param_name, fields);
            for stmt in &block.stmts {
                collect_record_field_access_refs_in_statement(stmt, param_name, fields);
            }
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                collect_record_field_access_refs(&block.cond, param_name, fields);
                for stmt in &block.stmts {
                    collect_record_field_access_refs_in_statement(stmt, param_name, fields);
                }
            }
            if let Some(else_block) = else_block {
                for stmt in else_block {
                    collect_record_field_access_refs_in_statement(stmt, param_name, fields);
                }
            }
        }
        rumoca_core::Statement::When { blocks, .. } => {
            for block in blocks {
                collect_record_field_access_refs(&block.cond, param_name, fields);
                for stmt in &block.stmts {
                    collect_record_field_access_refs_in_statement(stmt, param_name, fields);
                }
            }
        }
        rumoca_core::Statement::FunctionCall { args, .. } => {
            for arg in args {
                collect_record_field_access_refs(arg, param_name, fields);
            }
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            collect_record_field_access_refs(condition, param_name, fields);
            collect_record_field_access_refs(message, param_name, fields);
            if let Some(level) = level {
                collect_record_field_access_refs(level, param_name, fields);
            }
        }
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => {}
    }
}

fn collect_record_field_access_refs_in_subscript(
    subscript: &rumoca_core::Subscript,
    param_name: &str,
    fields: &mut IndexSet<String>,
) {
    if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
        collect_record_field_access_refs(expr, param_name, fields);
    }
}

fn collect_decomposed_record_field_refs(
    expr: &rumoca_core::Expression,
    prefix: &str,
    fields: &mut IndexSet<String>,
) {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if let Some(field) = decomposed_record_field_name(name.as_str(), prefix) {
                fields.insert(field);
            }
            for subscript in subscripts {
                collect_decomposed_record_field_refs_in_subscript(subscript, prefix, fields);
            }
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            collect_decomposed_record_field_refs(lhs, prefix, fields);
            collect_decomposed_record_field_refs(rhs, prefix, fields);
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            collect_decomposed_record_field_refs(rhs, prefix, fields);
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, value) in branches {
                collect_decomposed_record_field_refs(cond, prefix, fields);
                collect_decomposed_record_field_refs(value, prefix, fields);
            }
            collect_decomposed_record_field_refs(else_branch, prefix, fields);
        }
        rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            for arg in args {
                collect_decomposed_record_field_refs(arg, prefix, fields);
            }
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            collect_decomposed_record_field_refs(start, prefix, fields);
            if let Some(step) = step {
                collect_decomposed_record_field_refs(step, prefix, fields);
            }
            collect_decomposed_record_field_refs(end, prefix, fields);
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            collect_decomposed_record_field_refs(base, prefix, fields);
            for subscript in subscripts {
                collect_decomposed_record_field_refs_in_subscript(subscript, prefix, fields);
            }
        }
        rumoca_core::Expression::FieldAccess { base, .. } => {
            collect_decomposed_record_field_refs(base, prefix, fields);
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            collect_decomposed_record_field_refs(expr, prefix, fields);
            for index in indices {
                collect_decomposed_record_field_refs(&index.range, prefix, fields);
            }
            if let Some(filter) = filter {
                collect_decomposed_record_field_refs(filter, prefix, fields);
            }
        }
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => {}
    }
}

fn decomposed_record_field_name(name: &str, prefix: &str) -> Option<String> {
    let field = name.strip_prefix(prefix)?;
    let field = field.split_once('[').map_or(field, |(field, _)| field);
    (!field.is_empty() && !field.contains('.')).then(|| field.to_string())
}

fn collect_decomposed_record_field_refs_in_statement(
    stmt: &rumoca_core::Statement,
    prefix: &str,
    fields: &mut IndexSet<String>,
) {
    match stmt {
        rumoca_core::Statement::Assignment { value, .. }
        | rumoca_core::Statement::Reinit { value, .. } => {
            collect_decomposed_record_field_refs(value, prefix, fields);
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            for index in indices {
                collect_decomposed_record_field_refs(&index.range, prefix, fields);
            }
            for stmt in equations {
                collect_decomposed_record_field_refs_in_statement(stmt, prefix, fields);
            }
        }
        rumoca_core::Statement::While { block, .. } => {
            collect_decomposed_record_field_refs(&block.cond, prefix, fields);
            for stmt in &block.stmts {
                collect_decomposed_record_field_refs_in_statement(stmt, prefix, fields);
            }
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                collect_decomposed_record_field_refs(&block.cond, prefix, fields);
                for stmt in &block.stmts {
                    collect_decomposed_record_field_refs_in_statement(stmt, prefix, fields);
                }
            }
            if let Some(else_block) = else_block {
                for stmt in else_block {
                    collect_decomposed_record_field_refs_in_statement(stmt, prefix, fields);
                }
            }
        }
        rumoca_core::Statement::When { blocks, .. } => {
            for block in blocks {
                collect_decomposed_record_field_refs(&block.cond, prefix, fields);
                for stmt in &block.stmts {
                    collect_decomposed_record_field_refs_in_statement(stmt, prefix, fields);
                }
            }
        }
        rumoca_core::Statement::FunctionCall { args, .. } => {
            for arg in args {
                collect_decomposed_record_field_refs(arg, prefix, fields);
            }
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            collect_decomposed_record_field_refs(condition, prefix, fields);
            collect_decomposed_record_field_refs(message, prefix, fields);
            if let Some(level) = level {
                collect_decomposed_record_field_refs(level, prefix, fields);
            }
        }
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => {}
    }
}

fn collect_decomposed_record_field_refs_in_subscript(
    subscript: &rumoca_core::Subscript,
    prefix: &str,
    fields: &mut IndexSet<String>,
) {
    if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
        collect_decomposed_record_field_refs(expr, prefix, fields);
    }
}

fn decompose_record_args_dae_stmt(
    stmt: &mut rumoca_core::Statement,
    map: &RecordArgMap,
    local_record_params: Option<&HashSet<String>>,
) {
    *stmt = DaeRecordArgDecomposer {
        map,
        local_record_params,
    }
    .rewrite_statement(stmt);
}

struct DaeRecordArgDecomposer<'a> {
    map: &'a RecordArgMap,
    local_record_params: Option<&'a HashSet<String>>,
}

impl ExpressionRewriter for DaeRecordArgDecomposer<'_> {
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
                decompose_dae_record_args(&rewritten_args, decomposed, self.local_record_params)
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

impl StatementRewriter for DaeRecordArgDecomposer<'_> {}

impl DaeExpressionRewriter for DaeRecordArgDecomposer<'_> {}

fn decompose_dae_record_args(
    old_args: &[rumoca_core::Expression],
    decomposed: &[(usize, Vec<String>)],
    local_record_params: Option<&HashSet<String>>,
) -> Vec<rumoca_core::Expression> {
    let mut args = Vec::new();
    let mut old_idx = 0;
    for (param_idx, fields) in decomposed {
        while old_idx < *param_idx && old_idx < old_args.len() {
            args.push(old_args[old_idx].clone());
            old_idx += 1;
        }
        if old_idx < old_args.len() {
            expand_dae_record_arg(&old_args[old_idx], fields, local_record_params, &mut args);
            old_idx += 1;
        }
    }
    while old_idx < old_args.len() {
        args.push(old_args[old_idx].clone());
        old_idx += 1;
    }
    args
}

fn named_constructor_arg_dae<'a>(
    ctor_args: &'a [rumoca_core::Expression],
    field: &str,
) -> Option<&'a rumoca_core::Expression> {
    for arg in ctor_args {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            span: _,
        } = arg
            && name.as_str().strip_prefix("__rumoca_named_arg__.") == Some(field)
        {
            return args.first();
        }
    }
    None
}

fn expand_dae_record_arg(
    arg: &rumoca_core::Expression,
    fields: &[String],
    local_record_params: Option<&HashSet<String>>,
    out: &mut Vec<rumoca_core::Expression>,
) {
    // Constructor call → extract positional/named args
    if let rumoca_core::Expression::FunctionCall {
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

        for (i, field) in fields.iter().enumerate() {
            let named = named_constructor_arg_dae(ctor_args, field.as_str());
            if let Some(val) = named {
                out.push(val.clone());
            } else if i < positional.len() {
                out.push(positional[i].clone());
            } else {
                out.push(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
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
                out.push(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new(format!("{}_{}", name.as_str(), field)),
                    subscripts: vec![],
                    span: *span,
                });
            }
            return;
        }
        for field in fields {
            out.push(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(format!("{}.{}", name.as_str(), field)),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            });
        }
        return;
    }

    // Check for FieldAccess on the record variable (e.g. `c.re` passed directly)
    if let rumoca_core::Expression::FieldAccess { .. } = arg {
        // Single field access on a record — just push the base.field expression
        // This handles cases like passing `c.re` where `c` was the original record param.
        out.push(arg.clone());
        // Pad remaining fields with 0.0
        for _ in 1..fields.len() {
            out.push(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            });
        }
        return;
    }

    // Scalar expression passed to a record-typed parameter (e.g. Real expr passed
    // where Complex is expected) — treat as the first field, zero-fill the rest.
    // This avoids generating invalid C like `(expr).re`.
    if is_obviously_scalar(arg) {
        out.push(arg.clone());
        for _ in 1..fields.len() {
            out.push(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            });
        }
        return;
    }

    // General expression → emit FieldAccess
    for field in fields {
        out.push(rumoca_core::Expression::FieldAccess {
            base: Box::new(arg.clone()),
            field: field.clone(),
            span: rumoca_core::Span::DUMMY,
        });
    }
}

/// Returns true if the expression is obviously a scalar (not a record type).
fn is_obviously_scalar(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::Literal { value: _, .. } => true,
        rumoca_core::Expression::Binary { .. } | rumoca_core::Expression::Unary { .. } => true,
        rumoca_core::Expression::BuiltinCall { function, .. } => {
            // Math builtins return scalars
            matches!(
                function,
                rumoca_core::BuiltinFunction::Abs
                    | rumoca_core::BuiltinFunction::Sign
                    | rumoca_core::BuiltinFunction::Sqrt
                    | rumoca_core::BuiltinFunction::Sin
                    | rumoca_core::BuiltinFunction::Cos
                    | rumoca_core::BuiltinFunction::Tan
                    | rumoca_core::BuiltinFunction::Asin
                    | rumoca_core::BuiltinFunction::Acos
                    | rumoca_core::BuiltinFunction::Atan
                    | rumoca_core::BuiltinFunction::Atan2
                    | rumoca_core::BuiltinFunction::Sinh
                    | rumoca_core::BuiltinFunction::Cosh
                    | rumoca_core::BuiltinFunction::Tanh
                    | rumoca_core::BuiltinFunction::Exp
                    | rumoca_core::BuiltinFunction::Log
                    | rumoca_core::BuiltinFunction::Log10
                    | rumoca_core::BuiltinFunction::Floor
                    | rumoca_core::BuiltinFunction::Ceil
                    | rumoca_core::BuiltinFunction::Min
                    | rumoca_core::BuiltinFunction::Max
                    | rumoca_core::BuiltinFunction::Sum
                    | rumoca_core::BuiltinFunction::Size
                    | rumoca_core::BuiltinFunction::Der
                    | rumoca_core::BuiltinFunction::Pre
                    | rumoca_core::BuiltinFunction::Mod
                    | rumoca_core::BuiltinFunction::Rem
                    | rumoca_core::BuiltinFunction::Div
            )
        }
        rumoca_core::Expression::If { else_branch, .. } => is_obviously_scalar(else_branch),
        // User-defined function calls return scalar double in generated C
        rumoca_core::Expression::FunctionCall { .. } => true,
        _ => false,
    }
}

/// Insert size arguments for variable-size array params at DAE call sites.
/// Must NOT be called before simulation — only before codegen rendering.
pub fn insert_array_size_args_dae(dae: &mut Dae) {
    let array_param_map: HashMap<String, Vec<usize>> = dae
        .symbols
        .functions
        .iter()
        .filter_map(|(name, func)| {
            let indices: Vec<usize> = func
                .inputs
                .iter()
                .enumerate()
                .filter(|(_, p)| !p.dims.is_empty())
                .map(|(i, _)| i)
                .collect();
            if indices.is_empty() {
                None
            } else {
                Some((name.as_str().to_string(), indices))
            }
        })
        .collect();

    if array_param_map.is_empty() {
        return;
    }

    DaeSizeArgInserter {
        map: &array_param_map,
    }
    .rewrite_dae(dae);
    for func in dae.symbols.functions.values_mut() {
        for stmt in &mut func.body {
            insert_size_args_dae_stmt(stmt, &array_param_map);
        }
    }
}

#[cfg(test)]
mod record_lowering_tests;

fn insert_size_args_dae_stmt(stmt: &mut rumoca_core::Statement, map: &HashMap<String, Vec<usize>>) {
    *stmt = DaeSizeArgInserter { map }.rewrite_statement(stmt);
}

struct DaeSizeArgInserter<'a> {
    map: &'a HashMap<String, Vec<usize>>,
}

impl ExpressionRewriter for DaeSizeArgInserter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        {
            let mut args = self.rewrite_expressions(args);
            if let Some(array_indices) = self.map.get(name.as_str()) {
                insert_dae_size_args(&mut args, array_indices);
            }
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

impl StatementRewriter for DaeSizeArgInserter<'_> {}

impl DaeExpressionRewriter for DaeSizeArgInserter<'_> {}

fn insert_dae_size_args(args: &mut Vec<rumoca_core::Expression>, array_indices: &[usize]) {
    for &param_idx in array_indices.iter().rev() {
        if param_idx >= args.len() {
            continue;
        }
        // If the argument is an Array literal with known element count, use the
        // literal count directly instead of size(), which some backends cannot
        // render for compound literals.
        let size_expr = array_size_expr_for_arg(&args[param_idx]);
        args.insert(param_idx + 1, size_expr);
    }
}

fn array_size_expr_for_arg(arg: &rumoca_core::Expression) -> rumoca_core::Expression {
    if let rumoca_core::Expression::Array { elements, .. } = arg {
        return rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(elements.len() as i64),
            span: rumoca_core::Span::DUMMY,
        };
    }

    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![
            arg.clone(),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

// =============================================================================

/// Topologically sort parameters so that start-value dependencies are ordered.
///
/// If parameter A's `start` expression references parameter B, then B must
/// appear before A in the parameter map. This ensures code generators that
/// evaluate start values sequentially produce correct numeric results.
///
/// Falls back to the original order if cycles are detected.
pub(crate) fn sort_parameters_by_start_dependency(dae: &mut Dae) {
    let param_names: IndexSet<rumoca_core::VarName> =
        dae.variables.parameters.keys().cloned().collect();
    if param_names.len() <= 1 {
        return;
    }

    // Build adjacency: param → set of params its start expression depends on
    let mut deps: IndexMap<usize, IndexSet<usize>> = IndexMap::new();
    for (idx, (_, var)) in dae.variables.parameters.iter().enumerate() {
        let Some(ref start_expr) = var.start else {
            continue;
        };
        for ref_name in collect_expression_var_refs(start_expr) {
            let Some(dep_idx) = param_names.get_index_of(&ref_name) else {
                continue;
            };
            if dep_idx != idx {
                deps.entry(idx).or_default().insert(dep_idx);
            }
        }
    }

    // If no dependencies exist, nothing to reorder.
    if deps.is_empty() {
        return;
    }

    // Kahn's algorithm for topological sort
    let n = param_names.len();
    let mut in_degree = vec![0usize; n];
    // Build forward edges: if node depends on dep, then dep → node
    let mut forward: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (&node, predecessors) in &deps {
        for &pred in predecessors {
            forward[pred].push(node);
            in_degree[node] += 1;
        }
    }

    let mut queue: std::collections::VecDeque<usize> = std::collections::VecDeque::new();
    for (i, &deg) in in_degree.iter().enumerate() {
        if deg == 0 {
            queue.push_back(i);
        }
    }

    let mut sorted_indices = Vec::with_capacity(n);
    while let Some(node) = queue.pop_front() {
        sorted_indices.push(node);
        for &next in &forward[node] {
            in_degree[next] -= 1;
            if in_degree[next] == 0 {
                queue.push_back(next);
            }
        }
    }

    if sorted_indices.len() != n {
        // Cycle detected — keep original order (conservative fallback)
        return;
    }

    // Rebuild the parameters IndexMap in sorted order
    let old_params: Vec<(rumoca_core::VarName, dae::Variable)> =
        dae.variables.parameters.drain(..).collect();
    for &idx in &sorted_indices {
        let (name, var) = old_params[idx].clone();
        dae.variables.parameters.insert(name, var);
    }
}

/// Collect all VarRef names from an expression tree.
fn collect_expression_var_refs(expr: &rumoca_core::Expression) -> Vec<rumoca_core::VarName> {
    let mut collector = VarRefListCollector { refs: Vec::new() };
    collector.visit_expression(expr);
    collector.refs
}

struct VarRefListCollector {
    refs: Vec<rumoca_core::VarName>,
}

impl ExpressionVisitor for VarRefListCollector {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        self.refs.push(name.var_name().clone());
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

// =============================================================================
// Vector equation scalarization
// =============================================================================

/// Scalarize vector equations that reference "phantom" base names.
///
/// In Modelica, connector arrays like `plug_p.pin[3]` produce scalarized
/// variables (`sineVoltage.plug_p.pin[1].v`, `…pin[2].v`, `…pin[3].v`)
/// but some component-level equations reference the unsubscripted base name
/// (`sineVoltage.plug_p.pin.v`) as a vector.  These phantom base names do
/// not appear in any DAE variable map, so backends that render equations
/// directly (CasADi, SymPy, JAX) produce undefined identifiers.
///
/// This pass detects equations with `scalar_count > 1` whose expressions
/// contain such phantom VarRefs, and expands each into `scalar_count`
/// scalar equations — one per element — with every phantom VarRef replaced
/// by its indexed variant and every declared-array VarRef subscripted.
pub fn scalarize_phantom_vector_equations(dae: &mut Dae) -> Result<(), ToDaeError> {
    let known_names = build_known_var_name_set(dae);
    let phantom_map = build_phantom_expansion_map(&known_names);
    let array_dims = build_array_dims_map(dae);

    canonicalize_embedded_subscript_equation_list(&mut dae.continuous.equations, &array_dims);
    canonicalize_embedded_subscript_equation_list(&mut dae.initialization.equations, &array_dims);
    canonicalize_embedded_subscript_equation_list(&mut dae.discrete.real_updates, &array_dims);
    canonicalize_embedded_subscript_equation_list(&mut dae.discrete.valued_updates, &array_dims);
    canonicalize_embedded_subscript_equation_list(&mut dae.conditions.equations, &array_dims);

    if phantom_map.is_empty() {
        return Ok(());
    }

    scalarize_equation_list(
        &mut dae.continuous.equations,
        &phantom_map,
        &array_dims,
        &dae.symbols.functions,
    )?;
    scalarize_equation_list(
        &mut dae.initialization.equations,
        &phantom_map,
        &array_dims,
        &dae.symbols.functions,
    )?;
    scalarize_equation_list(
        &mut dae.discrete.real_updates,
        &phantom_map,
        &array_dims,
        &dae.symbols.functions,
    )?;
    scalarize_equation_list(
        &mut dae.discrete.valued_updates,
        &phantom_map,
        &array_dims,
        &dae.symbols.functions,
    )?;
    scalarize_equation_list(
        &mut dae.conditions.equations,
        &phantom_map,
        &array_dims,
        &dae.symbols.functions,
    )?;
    Ok(())
}

/// Build the set of all variable names known to the DAE.
fn build_known_var_name_set(dae: &Dae) -> HashSet<String> {
    let mut names = HashSet::new();
    for map in [
        &dae.variables.states,
        &dae.variables.algebraics,
        &dae.variables.inputs,
        &dae.variables.outputs,
        &dae.variables.parameters,
        &dae.variables.constants,
        &dae.variables.discrete_reals,
        &dae.variables.discrete_valued,
    ] {
        for name in map.keys() {
            names.insert(name.as_str().to_string());
        }
    }
    names
}

/// Build a map from stripped base name → sorted list of actual indexed names.
///
/// For example, if the DAE has `sineVoltage.plug_p.pin[1].v`,
/// `sineVoltage.plug_p.pin[2].v`, `sineVoltage.plug_p.pin[3].v`, the map
/// will contain `"sineVoltage.plug_p.pin.v" → ["…pin[1].v", "…pin[2].v", "…pin[3].v"]`.
///
/// Only base names that do NOT themselves appear in `known_names` are included
/// (i.e., only phantom base names that need expansion).
fn build_phantom_expansion_map(known_names: &HashSet<String>) -> HashMap<String, Vec<String>> {
    // Group all known names by their stripped base
    let mut base_to_indexed: HashMap<String, BTreeMap<String, ()>> = HashMap::new();
    for name in known_names {
        let base = super::path_utils::strip_all_subscripts(name);
        if base != *name {
            // This name has subscripts — record it under the base
            base_to_indexed
                .entry(base)
                .or_default()
                .insert(name.clone(), ());
        }
    }

    // Only keep entries where the base name itself is NOT a known variable.
    // Singleton connector arrays still need this mapping: a scalar equation may
    // refer to `ports.C_outflow` while the declared variable is
    // `ports[1].C_outflow`.
    let mut result = HashMap::new();
    for (base, indexed) in base_to_indexed {
        if !known_names.contains(&base) && !indexed.is_empty() {
            result.insert(base, indexed.into_keys().collect());
        }
    }
    result
}

/// Build a map from variable name → dims for declared array variables.
fn build_array_dims_map(dae: &Dae) -> HashMap<String, Vec<i64>> {
    let mut dims_map = HashMap::new();
    for map in [
        &dae.variables.states,
        &dae.variables.algebraics,
        &dae.variables.inputs,
        &dae.variables.outputs,
        &dae.variables.parameters,
        &dae.variables.constants,
        &dae.variables.discrete_reals,
        &dae.variables.discrete_valued,
    ] {
        for (name, var) in map {
            if !var.dims.is_empty() {
                dims_map.insert(name.as_str().to_string(), var.dims.clone());
            }
        }
    }
    dims_map
}

fn canonicalize_embedded_subscript_equation_list(
    equations: &mut [dae::Equation],
    array_dims: &HashMap<String, Vec<i64>>,
) {
    let mut canonicalizer = EmbeddedSubscriptCanonicalizer { array_dims };
    for equation in equations {
        equation.rhs = canonicalizer.rewrite_expression(&equation.rhs);
    }
}

struct EmbeddedSubscriptCanonicalizer<'a> {
    array_dims: &'a HashMap<String, Vec<i64>>,
}

impl ExpressionRewriter for EmbeddedSubscriptCanonicalizer<'_> {
    fn walk_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if subscripts.is_empty()
            && let Some(scalar_name) = rumoca_core::parse_scalar_name(name.as_str())
            && self.array_dims.contains_key(scalar_name.base)
        {
            return rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(scalar_name.base.to_string()),
                subscripts: scalar_name
                    .indices
                    .into_iter()
                    .map(|index| rumoca_core::Subscript::generated_index(index, span))
                    .collect(),
                span,
            };
        }
        rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

fn expr_phantom_ref_width(
    expr: &rumoca_core::Expression,
    phantom_map: &HashMap<String, Vec<String>>,
) -> Option<usize> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => phantom_map.get(name.as_str()).map(Vec::len),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => merge_phantom_widths(
            expr_phantom_ref_width(lhs, phantom_map),
            expr_phantom_ref_width(rhs, phantom_map),
        ),
        rumoca_core::Expression::Unary { rhs, .. } => expr_phantom_ref_width(rhs, phantom_map),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => args
            .iter()
            .filter_map(|arg| expr_phantom_ref_width(arg, phantom_map))
            .max(),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => branches
            .iter()
            .flat_map(|(condition, value)| {
                [
                    expr_phantom_ref_width(condition, phantom_map),
                    expr_phantom_ref_width(value, phantom_map),
                ]
            })
            .chain(std::iter::once(expr_phantom_ref_width(
                else_branch,
                phantom_map,
            )))
            .flatten()
            .max(),
        rumoca_core::Expression::ArrayComprehension { expr, filter, .. } => merge_phantom_widths(
            expr_phantom_ref_width(expr, phantom_map),
            filter
                .as_ref()
                .and_then(|filter| expr_phantom_ref_width(filter, phantom_map)),
        ),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => subscripts
            .iter()
            .filter_map(|subscript| match subscript {
                rumoca_core::Subscript::Expr { expr, .. } => {
                    expr_phantom_ref_width(expr, phantom_map)
                }
                rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => None,
            })
            .chain(expr_phantom_ref_width(base, phantom_map))
            .max(),
        rumoca_core::Expression::FieldAccess { base, .. } => {
            expr_phantom_ref_width(base, phantom_map)
        }
        _ => None,
    }
}

fn merge_phantom_widths(left: Option<usize>, right: Option<usize>) -> Option<usize> {
    left.into_iter().chain(right).max()
}

fn expr_has_array_comprehension(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::ArrayComprehension { .. } => true,
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expr_has_array_comprehension(lhs) || expr_has_array_comprehension(rhs)
        }
        rumoca_core::Expression::Unary { rhs, .. } => expr_has_array_comprehension(rhs),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_has_array_comprehension)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expr_has_array_comprehension(condition) || expr_has_array_comprehension(value)
            }) || expr_has_array_comprehension(else_branch)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            elements.iter().any(expr_has_array_comprehension)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            expr_has_array_comprehension(start)
                || step
                    .as_ref()
                    .is_some_and(|step| expr_has_array_comprehension(step))
                || expr_has_array_comprehension(end)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            expr_has_array_comprehension(base)
                || subscripts.iter().any(subscript_has_array_comprehension)
        }
        rumoca_core::Expression::FieldAccess { base, .. } => expr_has_array_comprehension(base),
        _ => false,
    }
}

fn subscript_has_array_comprehension(subscript: &rumoca_core::Subscript) -> bool {
    match subscript {
        rumoca_core::Subscript::Expr { expr, .. } => expr_has_array_comprehension(expr),
        rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => false,
    }
}

/// Scalarize an expression at index `k` (0-based).
///
/// - Phantom VarRefs are replaced by the k-th indexed variant from `phantom_map`.
/// - Declared array VarRefs (with no subscripts) get subscript `[k+1]` (1-based)
///   only while expanding an equation that already contains a phantom reference.
///   MLS §10.6: ordinary declared-array equations must remain array equations so
///   later matrix-aware scalarization can preserve linear algebra semantics.
/// - All other expressions are recursively processed.
fn scalarize_expr_at(
    expr: &rumoca_core::Expression,
    k: usize,
    phantom_map: &HashMap<String, Vec<String>>,
    array_dims: &HashMap<String, Vec<i64>>,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let ctx = ScalarizeExprContext {
        k,
        phantom_map,
        array_dims,
        functions,
    };
    scalarize_expr_with_context(expr, &ctx)
}

struct ScalarizeExprContext<'a> {
    k: usize,
    phantom_map: &'a HashMap<String, Vec<String>>,
    array_dims: &'a HashMap<String, Vec<i64>>,
    functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
}

fn scalarize_expr_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => scalarize_var_ref_at(name, subscripts, ctx.k, ctx.phantom_map, ctx.array_dims)
            .map(Ok)
            .unwrap_or_else(|| Ok(expr.clone())),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            Ok(rumoca_core::Expression::Binary {
                op: op.clone(),
                lhs: Box::new(scalarize_expr_with_context(lhs, ctx)?),
                rhs: Box::new(scalarize_expr_with_context(rhs, ctx)?),
                span: rumoca_core::Span::DUMMY,
            })
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => Ok(rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(scalarize_expr_with_context(rhs, ctx)?),
            span: rumoca_core::Span::DUMMY,
        }),
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            if let Some(expr) = scalarize_builtin_array_constructor_at(
                *function,
                args,
                ctx.k,
                ctx.phantom_map,
                ctx.array_dims,
                ctx.functions,
            )? {
                return Ok(expr);
            }
            Ok(rumoca_core::Expression::BuiltinCall {
                function: *function,
                args: args
                    .iter()
                    .map(|a| scalarize_expr_with_context(a, ctx))
                    .collect::<Result<Vec<_>, _>>()?,
                span: rumoca_core::Span::DUMMY,
            })
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } => scalarize_function_call_at(name, args, *is_constructor, *span, ctx),
        rumoca_core::Expression::FieldAccess { base, field, span } => {
            Ok(rumoca_core::Expression::FieldAccess {
                base: Box::new(scalarize_expr_with_context(base, ctx)?),
                field: field.clone(),
                span: *span,
            })
        }
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => scalarize_index_at(base, subscripts, *span, ctx),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => scalarize_if_expr_at(branches, else_branch, ctx),
        rumoca_core::Expression::Array { elements, .. } => {
            // An array literal in a vector equation context: extract element k
            if ctx.k < elements.len() {
                scalarize_expr_with_context(&elements[ctx.k], ctx)
            } else {
                Ok(expr.clone())
            }
        }
        rumoca_core::Expression::ArrayComprehension {
            expr: inner,
            indices,
            filter,
            ..
        } => scalarize_array_comprehension_at(expr, inner, indices, filter.as_deref(), ctx),
        _ => Ok(expr.clone()),
    }
}

fn scalarize_index_at(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if let rumoca_core::Expression::VarRef {
        name,
        subscripts: base_subscripts,
        ..
    } = base
        && base_subscripts.is_empty()
        && let Some(dims) = ctx.array_dims.get(name.as_str())
        && let Some(selected) = scalarized_array_subscripts_at(dims, subscripts, ctx.k)
    {
        return Ok(rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: selected,
            span,
        });
    }

    Ok(rumoca_core::Expression::Index {
        base: Box::new(scalarize_expr_with_context(base, ctx)?),
        subscripts: subscripts
            .iter()
            .map(|subscript| scalarize_subscript_with_context(subscript, ctx))
            .collect::<Result<Vec<_>, _>>()?,
        span,
    })
}

fn scalarized_array_subscripts_at(
    dims: &[i64],
    subscripts: &[rumoca_core::Subscript],
    k: usize,
) -> Option<Vec<rumoca_core::Subscript>> {
    if subscripts.len() > dims.len() {
        return None;
    }
    let mut choices = Vec::with_capacity(dims.len());
    let mut selected_dims = Vec::new();
    for (dim_idx, dim) in dims.iter().copied().enumerate() {
        let dim = usize::try_from(dim).ok()?;
        if dim == 0 {
            return None;
        }
        let indices = match subscripts.get(dim_idx) {
            Some(rumoca_core::Subscript::Index { value, .. }) if *value > 0 => {
                vec![*value as usize]
            }
            Some(rumoca_core::Subscript::Expr { expr, .. }) => {
                vec![literal_positive_integer_expr(expr)?]
            }
            Some(rumoca_core::Subscript::Colon { .. }) | None => (1..=dim).collect(),
            _ => return None,
        };
        if indices.iter().any(|index| *index == 0 || *index > dim) {
            return None;
        }
        if indices.len() > 1 {
            selected_dims.push(indices.len() as i64);
        }
        choices.push(indices);
    }

    let selected_subscripts = if selected_dims.is_empty() {
        if k == 0 {
            Vec::new()
        } else {
            return None;
        }
    } else {
        dae::flat_index_to_subscripts(&selected_dims, k)?
    };
    let mut selected_iter = selected_subscripts.into_iter();
    let mut out = Vec::with_capacity(choices.len());
    for indices in choices {
        let selected = if indices.len() == 1 {
            indices[0]
        } else {
            let selected = selected_iter.next()?;
            indices[selected - 1]
        };
        out.push(rumoca_core::Subscript::generated_index(
            selected as i64,
            rumoca_core::Span::DUMMY,
        ));
    }
    Some(out)
}

fn literal_positive_integer_expr(expr: &rumoca_core::Expression) -> Option<usize> {
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        ..
    } = expr
    else {
        return None;
    };
    (*value > 0).then_some(*value as usize)
}

fn scalarize_subscript_with_context(
    subscript: &rumoca_core::Subscript,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Subscript, ToDaeError> {
    match subscript {
        rumoca_core::Subscript::Expr { expr, span } => Ok(rumoca_core::Subscript::Expr {
            expr: Box::new(scalarize_expr_with_context(expr, ctx)?),
            span: *span,
        }),
        rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => {
            Ok(subscript.clone())
        }
    }
}

fn scalarize_array_comprehension_at(
    original: &rumoca_core::Expression,
    inner: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if filter.is_some() || indices.len() != 1 {
        return Ok(original.clone());
    }
    let Some(value) = scalarized_comprehension_index_value(&indices[0].range, ctx.k) else {
        return Ok(original.clone());
    };
    let mut substitution = ComprehensionIndexSubstitution {
        name: indices[0].name.clone(),
        value,
    };
    let selected = substitution.rewrite_expression(inner);
    scalarize_expr_with_context(&selected, ctx)
}

fn scalarized_comprehension_index_value(range: &rumoca_core::Expression, k: usize) -> Option<i64> {
    let rumoca_core::Expression::Range {
        start,
        step,
        end: _,
        ..
    } = range
    else {
        return None;
    };
    let start = integer_literal_value(start)?;
    let step = match step.as_deref() {
        Some(step) => integer_literal_value(step)?,
        None => 1,
    };
    Some(start + (k as i64) * step)
}

fn integer_literal_value(expr: &rumoca_core::Expression) -> Option<i64> {
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        ..
    } = expr
    else {
        return None;
    };
    Some(*value)
}

struct ComprehensionIndexSubstitution {
    name: String,
    value: i64,
}

impl ExpressionRewriter for ComprehensionIndexSubstitution {
    fn walk_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if name.as_str() == self.name && subscripts.is_empty() {
            return rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(self.value),
                span,
            };
        }
        rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }

    fn walk_array_comprehension_expression(
        &mut self,
        expr: &rumoca_core::Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&rumoca_core::Expression>,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if indices.iter().any(|index| index.name == self.name) {
            return rumoca_core::Expression::ArrayComprehension {
                expr: Box::new(expr.clone()),
                indices: indices.to_vec(),
                filter: filter.cloned().map(Box::new),
                span,
            };
        }
        ExpressionRewriter::walk_array_comprehension_expression(self, expr, indices, filter, span)
    }
}

fn scalarize_var_ref_at(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    k: usize,
    phantom_map: &HashMap<String, Vec<String>>,
    array_dims: &HashMap<String, Vec<i64>>,
) -> Option<rumoca_core::Expression> {
    let n = name.as_str();
    if !subscripts.is_empty() {
        return None;
    }
    if let Some(variants) = phantom_map.get(n)
        && k < variants.len()
    {
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(variants[k].clone()),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        });
    }
    array_dims
        .contains_key(n)
        .then(|| rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                (k + 1) as i64,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        })
}

fn scalarize_function_call_at(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    is_constructor: bool,
    span: rumoca_core::Span,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let first_output_size =
        first_function_output_size(name.as_str(), ctx.functions).ok_or_else(|| {
            ToDaeError::runtime_contract_violation_at(
                format!("missing function output metadata for `{name}`"),
                span,
            )
        })?;
    if first_output_size > 1 {
        return Ok(rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|arg| vectorize_phantom_expr(arg, ctx.phantom_map))
                    .collect(),
                is_constructor,
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                (ctx.k + 1) as i64,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        });
    }
    Ok(rumoca_core::Expression::FunctionCall {
        name: name.clone(),
        args: args
            .iter()
            .map(|a| scalarize_expr_with_context(a, ctx))
            .collect::<Result<Vec<_>, _>>()?,
        is_constructor,
        span: rumoca_core::Span::DUMMY,
    })
}

fn scalarize_if_expr_at(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    Ok(rumoca_core::Expression::If {
        branches: branches
            .iter()
            .map(|(condition, value)| {
                Ok((
                    scalarize_expr_with_context(condition, ctx)?,
                    scalarize_expr_with_context(value, ctx)?,
                ))
            })
            .collect::<Result<Vec<_>, ToDaeError>>()?,
        else_branch: Box::new(scalarize_expr_with_context(else_branch, ctx)?),
        span: rumoca_core::Span::DUMMY,
    })
}

fn scalarize_builtin_array_constructor_at(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    k: usize,
    phantom_map: &HashMap<String, Vec<String>>,
    array_dims: &HashMap<String, Vec<i64>>,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
    match function {
        rumoca_core::BuiltinFunction::Zeros => Ok(Some(real_literal(0.0))),
        rumoca_core::BuiltinFunction::Ones => Ok(Some(real_literal(1.0))),
        rumoca_core::BuiltinFunction::Fill => {
            let Some(value) = args.first() else {
                return Ok(None);
            };
            scalarize_expr_at(value, k, phantom_map, array_dims, functions).map(Some)
        }
        rumoca_core::BuiltinFunction::Identity => {
            let Some(n) = args.first().and_then(literal_positive_usize) else {
                return Ok(None);
            };
            let row = k / n;
            let col = k % n;
            Ok(Some(real_literal(if row == col { 1.0 } else { 0.0 })))
        }
        _ => Ok(None),
    }
}

fn literal_positive_usize(expr: &rumoca_core::Expression) -> Option<usize> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => usize::try_from(*value).ok().filter(|value| *value > 0),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } if *value > 0.0 && value.fract() == 0.0 => Some(*value as usize),
        _ => None,
    }
}

fn real_literal(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn first_function_output_size(
    name: &str,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Option<usize> {
    let lookup_name = rumoca_core::VarName::new(name);
    let function = functions.get(&lookup_name)?;
    let output = function.outputs.first()?;
    Some(compute_var_size(&output.dims))
}

fn vectorize_phantom_expr(
    expr: &rumoca_core::Expression,
    phantom_map: &HashMap<String, Vec<String>>,
) -> rumoca_core::Expression {
    PhantomVectorizer { phantom_map }.rewrite_expression(expr)
}

struct PhantomVectorizer<'a> {
    phantom_map: &'a HashMap<String, Vec<String>>,
}

impl ExpressionRewriter for PhantomVectorizer<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if subscripts.is_empty()
            && let Some(variants) = self.phantom_map.get(name.as_str())
        {
            return rumoca_core::Expression::Array {
                elements: variants
                    .iter()
                    .map(|variant| rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::new(variant.clone()),
                        subscripts: Vec::new(),
                        span,
                    })
                    .collect(),
                is_matrix: false,
                span,
            };
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }

    fn walk_binary_expression(
        &mut self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        _span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let lhs = self.rewrite_expression(lhs);
        let rhs = self.rewrite_expression(rhs);
        vectorized_binary_expr(op.clone(), lhs, rhs)
    }

    fn walk_unary_expression(
        &mut self,
        op: &rumoca_core::OpUnary,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let rhs = self.rewrite_expression(rhs);
        if let rumoca_core::Expression::Array { elements, .. } = rhs {
            return rumoca_core::Expression::Array {
                elements: elements
                    .into_iter()
                    .map(|element| rumoca_core::Expression::Unary {
                        op: op.clone(),
                        rhs: Box::new(element),
                        span,
                    })
                    .collect(),
                is_matrix: false,
                span,
            };
        }
        rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(rhs),
            span,
        }
    }
}

fn vectorized_binary_expr(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    match (lhs, rhs) {
        (
            rumoca_core::Expression::Array {
                elements: lhs_values,
                ..
            },
            rumoca_core::Expression::Array {
                elements: rhs_values,
                ..
            },
        ) if lhs_values.len() == rhs_values.len() => {
            array_from_binary_elements(op, lhs_values.into_iter().zip(rhs_values).collect())
        }
        (
            rumoca_core::Expression::Array {
                elements: lhs_values,
                ..
            },
            rhs,
        ) => array_from_binary_elements(
            op,
            lhs_values
                .into_iter()
                .map(|lhs| (lhs, rhs.clone()))
                .collect(),
        ),
        (
            lhs,
            rumoca_core::Expression::Array {
                elements: rhs_values,
                ..
            },
        ) => array_from_binary_elements(
            op,
            rhs_values
                .into_iter()
                .map(|rhs| (lhs.clone(), rhs))
                .collect(),
        ),
        (lhs, rhs) => rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        },
    }
}

fn array_from_binary_elements(
    op: rumoca_core::OpBinary,
    pairs: Vec<(rumoca_core::Expression, rumoca_core::Expression)>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements: pairs
            .into_iter()
            .map(|(lhs, rhs)| rumoca_core::Expression::Binary {
                op: op.clone(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: rumoca_core::Span::DUMMY,
            })
            .collect(),
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    }
}

/// Process an equation list, expanding vector equations with phantom refs.
fn scalarize_equation_list(
    equations: &mut Vec<dae::Equation>,
    phantom_map: &HashMap<String, Vec<String>>,
    array_dims: &HashMap<String, Vec<i64>>,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<(), ToDaeError> {
    let mut new_equations = Vec::with_capacity(equations.len());
    for eq in equations.drain(..) {
        let phantom_width = expr_phantom_ref_width(&eq.rhs, phantom_map);
        if eq.scalar_count > 1 && (phantom_width.is_some() || expr_has_array_comprehension(&eq.rhs))
        {
            // Expand into scalar_count individual equations
            for k in 0..eq.scalar_count {
                let scalar_rhs = scalarize_expr_at(&eq.rhs, k, phantom_map, array_dims, functions)?;
                let origin = format!("{} [scalarized {}]", eq.origin, k + 1);
                new_equations.push(scalarized_equation_at(
                    &eq,
                    scalar_rhs,
                    k,
                    origin,
                    phantom_map,
                    array_dims,
                ));
            }
        } else if phantom_width == Some(1) {
            let scalar_rhs = scalarize_expr_at(&eq.rhs, 0, phantom_map, array_dims, functions)?;
            new_equations.push(dae::Equation {
                rhs: scalar_rhs,
                ..eq
            });
        } else {
            new_equations.push(eq);
        }
    }
    *equations = new_equations;
    Ok(())
}

fn scalarized_equation_at(
    eq: &dae::Equation,
    scalar_rhs: rumoca_core::Expression,
    k: usize,
    origin: String,
    phantom_map: &HashMap<String, Vec<String>>,
    array_dims: &HashMap<String, Vec<i64>>,
) -> dae::Equation {
    let Some(lhs) = &eq.lhs else {
        return dae::Equation::residual(scalar_rhs, eq.span, origin);
    };
    dae::Equation::explicit(
        scalarize_lhs_name_at(lhs, k, phantom_map, array_dims),
        scalar_rhs,
        eq.span,
        origin,
    )
}

fn scalarize_lhs_name_at(
    name: &rumoca_core::VarName,
    k: usize,
    phantom_map: &HashMap<String, Vec<String>>,
    array_dims: &HashMap<String, Vec<i64>>,
) -> rumoca_core::VarName {
    if let Some(variants) = phantom_map.get(name.as_str())
        && let Some(variant) = variants.get(k)
    {
        return rumoca_core::VarName::new(variant);
    }
    if let Some(dims) = array_dims.get(name.as_str()) {
        return dae::scalar_name_for_flat_index(name, dims, k);
    }
    rumoca_core::VarName::new(format!("{}[{}]", name.as_str(), k + 1))
}

#[cfg(test)]
mod tests;
