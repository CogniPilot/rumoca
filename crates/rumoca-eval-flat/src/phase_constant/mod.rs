//! Flat expression evaluation for the flatten phase.
//!
//! This module provides evaluation functions for flat expressions during the
//! flattening phase. It handles:
//! - Integer expression evaluation (parameters, builtins, user functions)
//! - Real expression evaluation
//! - Boolean expression evaluation (comparisons, logical operations)
//! - Array dimension inference from bindings
//! - Enumeration value resolution
//!
//! These functions are used for compile-time constant evaluation per MLS §4.4.

use rustc_hash::FxHashMap;

use crate::constant::{EvalContext, Value};
use rumoca_ir_flat as flat;

use rumoca_core::{ComponentPath, ExpressionVisitor, scoped_component_path_candidates};

mod boolean_eval;
mod enum_identity;

pub use boolean_eval::try_eval_flat_expr_boolean;
use enum_identity::EnumCanonicalizer;
pub use enum_identity::canonicalize_enum_literal;

// Conditional tracing support (SPEC_0008)
#[cfg(feature = "tracing")]
use tracing::debug;

/// Build an EvalContext from known parameter values and functions.
pub fn build_eval_context(
    known_ints: &FxHashMap<String, i64>,
    known_reals: &FxHashMap<String, f64>,
    known_bools: &FxHashMap<String, bool>,
    array_dims: &FxHashMap<String, Vec<i64>>,
    functions: &FxHashMap<String, rumoca_core::Function>,
) -> EvalContext {
    let parameter_capacity =
        known_ints.len() + known_reals.len() + known_bools.len() + array_dims.len();
    let mut eval_ctx = EvalContext::with_capacity(parameter_capacity, 0, functions.len() * 2);
    for (k, v) in known_ints {
        eval_ctx.add_parameter(k.clone(), Value::Integer(*v));
    }
    for (k, v) in known_reals {
        eval_ctx.add_parameter(k.clone(), Value::Real(*v));
    }
    for (k, v) in known_bools {
        eval_ctx.add_parameter(k.clone(), Value::Bool(*v));
    }
    for (k, v) in array_dims {
        eval_ctx.add_array_dimensions(k.clone(), v.clone());
    }
    for func in functions.values() {
        eval_ctx.add_function(func.clone());
    }
    eval_ctx
}

fn build_param_value_context(
    ctx: &ParamEvalContext<'_>,
    enum_canonicalizer: &EnumCanonicalizer,
) -> EvalContext {
    let mut eval_ctx = build_eval_context(
        ctx.known_ints,
        ctx.known_reals,
        ctx.known_bools,
        ctx.array_dims,
        ctx.functions,
    );
    for (name, literal) in ctx.known_enums {
        // An empty rendered name carries no enumeration identity and cannot key
        // a lookup, so there is nothing to register for it.
        let Some(identity) = enum_canonicalizer.canonicalize(literal) else {
            continue;
        };
        let value = identity.to_value();
        eval_ctx.add_parameter(name.clone(), value.clone());
        eval_ctx.add_parameter(identity.to_flat_string(), value);
    }
    eval_ctx.set_lookup_scope(
        ctx.var_context
            .map(ComponentPath::from_flat_path)
            .and_then(|path| path.parent()),
    );
    eval_ctx.enable_unique_suffix_lookup();
    eval_ctx
}

fn eval_param_expr(expr: &rumoca_core::Expression, ctx: &ParamEvalContext<'_>) -> Option<Value> {
    ParamEvaluator::new(ctx).eval_value(expr, ctx.var_context)
}

/// Context for compile-time parameter expression evaluation (MLS §4.4).
pub struct ParamEvalContext<'a> {
    pub known_ints: &'a FxHashMap<String, i64>,
    pub known_reals: &'a FxHashMap<String, f64>,
    pub known_bools: &'a FxHashMap<String, bool>,
    pub known_enums: &'a FxHashMap<String, String>,
    pub array_dims: &'a FxHashMap<String, Vec<i64>>,
    /// Functions available for evaluation.
    pub functions: &'a FxHashMap<String, rumoca_core::Function>,
    /// The fully qualified name of the variable whose binding we're evaluating.
    /// Used to resolve unqualified modification bindings to parent scope (MLS §7.2).
    pub var_context: Option<&'a str>,
}

impl<'a> ParamEvalContext<'a> {
    pub fn new(
        known_ints: &'a FxHashMap<String, i64>,
        known_reals: &'a FxHashMap<String, f64>,
        known_bools: &'a FxHashMap<String, bool>,
        known_enums: &'a FxHashMap<String, String>,
        array_dims: &'a FxHashMap<String, Vec<i64>>,
        functions: &'a FxHashMap<String, rumoca_core::Function>,
        var_context: Option<&'a str>,
    ) -> Self {
        Self {
            known_ints,
            known_reals,
            known_bools,
            known_enums,
            array_dims,
            functions,
            var_context,
        }
    }
}

/// Reusable evaluator for one stable parameter inventory.
///
/// Flatten evaluates many bindings against the same maps during each
/// fixed-point pass. Preparing those maps once avoids rebuilding and cloning
/// the complete parameter/function inventory for every expression.
pub struct ParamEvaluator {
    eval_ctx: EvalContext,
    enum_canonicalizer: EnumCanonicalizer,
}

impl ParamEvaluator {
    pub fn new(ctx: &ParamEvalContext<'_>) -> Self {
        let enum_canonicalizer = EnumCanonicalizer::new(ctx.known_enums);
        let eval_ctx = build_param_value_context(ctx, &enum_canonicalizer);
        Self {
            eval_ctx,
            enum_canonicalizer,
        }
    }

    fn set_var_context(&mut self, var_context: Option<&str>) {
        self.eval_ctx.set_lookup_scope(
            var_context
                .map(ComponentPath::from_flat_path)
                .and_then(|path| path.parent()),
        );
    }

    fn eval_value(
        &mut self,
        expr: &rumoca_core::Expression,
        var_context: Option<&str>,
    ) -> Option<Value> {
        self.set_var_context(var_context);
        register_enum_comparison_candidates(expr, &self.enum_canonicalizer, &mut self.eval_ctx);
        crate::constant::eval_expr(expr, &self.eval_ctx).ok()
    }

    pub fn eval_integer(
        &mut self,
        expr: &rumoca_core::Expression,
        var_context: Option<&str>,
    ) -> Option<i64> {
        self.eval_value(expr, var_context)
            .and_then(|value| value.as_integer())
    }

    pub fn eval_boolean(
        &mut self,
        expr: &rumoca_core::Expression,
        var_context: Option<&str>,
    ) -> Option<bool> {
        self.eval_value(expr, var_context)
            .and_then(|value| value.as_bool())
    }

    pub fn eval_real(
        &mut self,
        expr: &rumoca_core::Expression,
        var_context: Option<&str>,
    ) -> Option<f64> {
        self.eval_value(expr, var_context)
            .and_then(|value| value.to_real())
    }
}

/// Try to evaluate a flat expression to an integer value with context and array dimensions.
///
/// Same as try_eval_flat_expr_integer but also handles size() calls using array dimensions.
pub fn try_eval_flat_expr_integer_with_dims(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<i64> {
    // Call with empty bools/enums/functions (convenience for callers without those contexts)
    let ctx = ParamEvalContext {
        known_ints,
        known_reals: &FxHashMap::default(),
        known_bools: &FxHashMap::default(),
        known_enums: &FxHashMap::default(),
        array_dims,
        functions: &FxHashMap::default(),
        var_context: None,
    };
    try_eval_integer_with_context(expr, &ctx)
}

/// Integer evaluation with full context.
pub fn try_eval_integer_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Option<i64> {
    eval_param_expr(expr, ctx).and_then(|value| value.as_integer())
}

/// Try to evaluate a flat expression to a boolean value with full context.
///
/// This extends `try_eval_flat_expr_boolean` with scoped VarRef resolution
/// via `var_context` (MLS §7.2), so unqualified enum/bool refs in parameter
/// bindings can be evaluated while computing integer if-expressions.
pub fn try_eval_flat_expr_boolean_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Option<bool> {
    eval_param_expr(expr, ctx).and_then(|value| value.as_bool())
}

/// Evaluate a flat expression to a real using scoped lookup context.
pub fn try_eval_real_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Option<f64> {
    eval_param_expr(expr, ctx).and_then(|value| value.to_real())
}

/// Infer array dimensions from an array literal binding.
pub fn try_infer_better_dims(var: &flat::Variable) -> Vec<i64> {
    if let Some(binding) = &var.binding
        && let Some(inferred) = infer_array_dimensions(binding)
        && inferred.len() > var.dims.len()
    {
        return inferred;
    }
    var.dims.clone()
}

/// MLS §10.1: When a variable is declared with unspecified dimensions (`:`) and
/// bound to an array literal, the dimensions can be inferred from the literal's structure.
pub fn infer_array_dimensions(expr: &rumoca_core::Expression) -> Option<Vec<i64>> {
    infer_array_dimensions_full_with_conds(
        expr,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
    )
}

/// Infer array dimensions with full context including conditional expression support.
pub fn infer_array_dimensions_full_with_conds(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    let known_reals = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = ParamEvalContext {
        known_ints,
        known_reals: &known_reals,
        known_bools,
        known_enums,
        array_dims,
        functions: &functions,
        var_context: None,
    };
    infer_array_dimensions_with_context(expr, &ctx)
}

/// Infer array dimensions with function output shape metadata available.
pub fn infer_array_dimensions_full_with_functions(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    infer_array_dimensions_with_context(expr, ctx)
}

fn infer_array_dimensions_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    match expr {
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            ..
        } => infer_array_literal_dimensions_with_context(elements, *is_matrix, ctx),
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            infer_builtin_call_dimensions_with_context(*function, args, ctx)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => infer_range_dimensions_with_context(start, step.as_deref(), end, ctx),
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            infer_array_comprehension_dimensions_with_context(expr, indices, filter.as_deref(), ctx)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => infer_if_dimensions_with_context(branches, else_branch, ctx),
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            infer_user_function_call_dimensions(name, args, ctx)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let dims = infer_array_dimensions_with_context(base, ctx)?;
            project_dims_by_subscripts(&dims, subscripts, ctx)
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let dims = lookup_array_dims_in_scope(name.as_str(), ctx.var_context, ctx.array_dims)?;
            project_dims_by_subscripts(&dims, subscripts, ctx)
        }
        _ => None,
    }
}

fn project_dims_by_subscripts(
    dims: &[i64],
    subscripts: &[rumoca_core::Subscript],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    let mut projected = Vec::new();
    let mut dim_index = 0usize;
    for subscript in subscripts {
        let dim = *dims.get(dim_index)?;
        match subscript {
            rumoca_core::Subscript::Index { .. } => {}
            rumoca_core::Subscript::Expr { expr, .. } => {
                try_eval_integer_with_context(expr, ctx)?;
            }
            rumoca_core::Subscript::Colon { .. } => projected.push(dim),
        }
        dim_index += 1;
    }
    projected.extend_from_slice(&dims[dim_index..]);
    Some(projected)
}

fn infer_user_function_call_dimensions(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    let func = ctx.functions.get(name.as_str())?;
    let output = func.outputs.first()?;
    if output.shape_expr.is_empty() {
        return concrete_param_dims(output).or_else(|| broadcast_function_arg_dims(args, ctx));
    }

    let mut local_ints = ctx.known_ints.clone();
    let mut local_reals = ctx.known_reals.clone();
    let mut local_bools = ctx.known_bools.clone();
    bind_function_dimension_args(
        func,
        args,
        ctx,
        &mut local_ints,
        &mut local_reals,
        &mut local_bools,
    )?;

    let local_ctx = ParamEvalContext {
        known_ints: &local_ints,
        known_reals: &local_reals,
        known_bools: &local_bools,
        known_enums: ctx.known_enums,
        array_dims: ctx.array_dims,
        functions: ctx.functions,
        var_context: None,
    };

    output
        .shape_expr
        .iter()
        .enumerate()
        .map(|(index, subscript)| eval_param_shape_subscript(output, index, subscript, &local_ctx))
        .collect()
}

fn concrete_param_dims(param: &rumoca_core::FunctionParam) -> Option<Vec<i64>> {
    if param.dimensions().is_empty() {
        return None;
    }
    Some(param.dimensions().to_vec())
}

fn broadcast_function_arg_dims(
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    args.iter()
        .map(function_arg_value)
        .filter_map(|arg| infer_function_arg_dims(arg, ctx))
        .max_by_key(Vec::len)
        .filter(|dims| !dims.is_empty())
}

fn function_arg_value(arg: &rumoca_core::Expression) -> &rumoca_core::Expression {
    if let Some((_, value)) = named_call_arg(arg) {
        value
    } else {
        arg
    }
}

fn named_call_arg(expr: &rumoca_core::Expression) -> Option<(&str, &rumoca_core::Expression)> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
        ..
    } = expr
    else {
        return None;
    };
    let arg_name = name
        .as_str()
        .strip_prefix(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)?;
    (args.len() == 1).then(|| (arg_name, &args[0]))
}

fn infer_function_arg_dims(
    arg: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    if let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = arg
        && subscripts.is_empty()
    {
        return lookup_array_dims_in_scope(name.as_str(), ctx.var_context, ctx.array_dims);
    }

    infer_array_dimensions_full_with_functions(arg, ctx)
}

fn infer_array_literal_dimensions_with_context(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    if elements.is_empty() {
        return Some(vec![0]);
    }

    if is_matrix {
        return infer_matrix_constructor_dimensions_with_context(elements, ctx);
    }

    let mut dims = vec![elements.len() as i64];
    if let Some(first) = elements.first()
        && let Some(inner_dims) = infer_array_dimensions_with_context(first, ctx)
    {
        dims.extend(inner_dims);
    }
    Some(dims)
}

fn infer_matrix_constructor_dimensions_with_context(
    elements: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    let has_nested_rows = matches!(
        elements.first(),
        Some(rumoca_core::Expression::Array { .. })
    );
    if !has_nested_rows {
        return infer_matrix_row_dimensions_with_context(elements, ctx)
            .map(|(rows, cols)| vec![rows, cols]);
    }

    let mut rows = 0i64;
    let mut expected_cols = None;
    for row in elements {
        let rumoca_core::Expression::Array {
            elements: row_elements,
            ..
        } = row
        else {
            return None;
        };
        let (row_count, col_count) = infer_matrix_row_dimensions_with_context(row_elements, ctx)?;
        match expected_cols {
            Some(expected) if expected != col_count => return None,
            None => expected_cols = Some(col_count),
            _ => {}
        }
        rows += row_count;
    }

    Some(vec![rows, expected_cols?])
}

fn infer_matrix_row_dimensions_with_context(
    elements: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<(i64, i64)> {
    let single_entry = elements.len() == 1;
    let mut expected_rows = None;
    let mut cols = 0i64;
    for element in elements {
        let dims = infer_array_dimensions_with_context(element, ctx)
            .or_else(|| scalar_matrix_entry_dims(element))?;
        let (entry_rows, entry_cols) = matrix_entry_dimensions(&dims, single_entry)?;
        match expected_rows {
            Some(expected) if expected != entry_rows => return None,
            None => expected_rows = Some(entry_rows),
            _ => {}
        }
        cols += entry_cols;
    }
    Some((expected_rows?, cols))
}

fn scalar_matrix_entry_dims(expr: &rumoca_core::Expression) -> Option<Vec<i64>> {
    matches!(expr, rumoca_core::Expression::Literal { .. }).then(Vec::new)
}

fn infer_array_comprehension_dimensions_with_context(
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    if filter.is_some() {
        return None;
    }

    let mut dims = Vec::with_capacity(indices.len().saturating_add(1));
    for index in indices {
        let range_dims = infer_array_dimensions_with_context(&index.range, ctx)?;
        if range_dims.is_empty() {
            return None;
        }
        let iter_size = range_dims
            .iter()
            .copied()
            .fold(1i64, |acc, dim| acc.saturating_mul(dim.max(0)));
        dims.push(iter_size);
    }

    if let Some(mut inner_dims) = infer_array_dimensions_with_context(expr, ctx) {
        dims.append(&mut inner_dims);
    }

    Some(dims)
}

fn infer_builtin_call_dimensions_with_context(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    match function {
        rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones => {
            eval_dimension_args_with_context(args, ctx)
        }
        rumoca_core::BuiltinFunction::Fill => {
            if args.len() < 2 {
                return None;
            }
            eval_dimension_args_with_context(&args[1..], ctx)
        }
        rumoca_core::BuiltinFunction::Linspace => {
            if args.len() != 3 {
                return None;
            }
            let n = try_eval_integer_with_context(&args[2], ctx)?;
            (n >= 2).then_some(vec![n])
        }
        rumoca_core::BuiltinFunction::Identity => {
            if args.len() != 1 {
                return None;
            }
            let n = try_eval_integer_with_context(&args[0], ctx)?;
            Some(vec![n, n])
        }
        rumoca_core::BuiltinFunction::Vector => {
            if args.len() != 1 {
                return None;
            }
            let dims = infer_array_dimensions_with_context(&args[0], ctx)?;
            Some(vec![dims.iter().copied().product()])
        }
        rumoca_core::BuiltinFunction::Matrix => {
            if args.len() != 1 {
                return None;
            }
            let dims = infer_array_dimensions_with_context(&args[0], ctx)?;
            match dims.as_slice() {
                [] => Some(vec![1, 1]),
                [len] => Some(vec![*len, 1]),
                [_, _] => Some(dims),
                _ => None,
            }
        }
        _ => None,
    }
}

fn eval_dimension_args_with_context(
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    let mut dims = Vec::with_capacity(args.len());
    for arg in args {
        dims.push(try_eval_integer_with_context(arg, ctx)?);
    }
    (!dims.is_empty()).then_some(dims)
}

fn infer_if_dimensions_with_context(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    for (cond, then_expr) in branches {
        match try_eval_flat_expr_boolean_with_context(cond, ctx) {
            Some(true) => return infer_array_dimensions_with_context(then_expr, ctx),
            Some(false) => continue,
            None => return None,
        }
    }
    infer_array_dimensions_with_context(else_branch, ctx)
}

fn bind_function_dimension_args(
    func: &rumoca_core::Function,
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
    local_ints: &mut FxHashMap<String, i64>,
    local_reals: &mut FxHashMap<String, f64>,
    local_bools: &mut FxHashMap<String, bool>,
) -> Option<()> {
    let mut positional = 0usize;
    for arg in args {
        let (param_name, value_expr) = if let Some((name, value)) = named_call_arg(arg) {
            (name, value)
        } else {
            let param = func.inputs.get(positional)?;
            positional += 1;
            (param.name.as_str(), arg)
        };
        bind_dimension_arg_value(
            param_name,
            value_expr,
            ctx,
            local_ints,
            local_reals,
            local_bools,
        )?;
    }

    for param in &func.inputs {
        if local_ints.contains_key(&param.name)
            || local_reals.contains_key(&param.name)
            || local_bools.contains_key(&param.name)
        {
            continue;
        }
        let default = param.default.as_ref()?;
        bind_dimension_arg_value(
            &param.name,
            default,
            ctx,
            local_ints,
            local_reals,
            local_bools,
        )?;
    }
    Some(())
}

fn bind_dimension_arg_value(
    name: &str,
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
    local_ints: &mut FxHashMap<String, i64>,
    local_reals: &mut FxHashMap<String, f64>,
    local_bools: &mut FxHashMap<String, bool>,
) -> Option<()> {
    if let Some(value) = try_eval_integer_with_context(expr, ctx) {
        local_ints.insert(name.to_string(), value);
        return Some(());
    }
    if let Some(value) = try_eval_real_with_context(expr, ctx) {
        local_reals.insert(name.to_string(), value);
        return Some(());
    }
    if let Some(value) = try_eval_flat_expr_boolean_with_context(expr, ctx) {
        local_bools.insert(name.to_string(), value);
        return Some(());
    }
    None
}

fn eval_param_shape_subscript(
    param: &rumoca_core::FunctionParam,
    index: usize,
    subscript: &rumoca_core::Subscript,
    ctx: &ParamEvalContext<'_>,
) -> Option<i64> {
    match subscript {
        rumoca_core::Subscript::Index { value, .. } => Some(*value),
        rumoca_core::Subscript::Expr { expr, .. } => try_eval_integer_with_context(expr, ctx),
        rumoca_core::Subscript::Colon { .. } => param.dimensions().get(index).copied(),
    }
}

fn matrix_entry_dimensions(dims: &[i64], single_entry: bool) -> Option<(i64, i64)> {
    match dims {
        [] => Some((1, 1)),
        [len] if single_entry => Some((*len, 1)),
        [len] => Some((*len, 1)),
        [rows, cols] => Some((*rows, *cols)),
        _ => None,
    }
}

fn infer_range_dimensions_with_context(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Option<Vec<i64>> {
    let start_val = try_eval_integer_with_context(start, ctx)?;
    let end_val = try_eval_integer_with_context(end, ctx)?;
    let step_val = step
        .map(|s| try_eval_integer_with_context(s, ctx))
        .unwrap_or(Some(1))?;

    if step_val == 0 {
        return None;
    }

    let len = if step_val > 0 {
        if end_val >= start_val {
            (end_val - start_val) / step_val + 1
        } else {
            0
        }
    } else if start_val >= end_val {
        (start_val - end_val) / (-step_val) + 1
    } else {
        0
    };

    Some(vec![len])
}

/// Walk up the scope chain looking for array dimensions.
fn lookup_dims_in_ancestors(
    array_name: &str,
    start_scope: &str,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    let scope = ComponentPath::from_flat_path(start_scope);
    let array_path = ComponentPath::from_flat_path(array_name);
    for candidate in scoped_component_path_candidates(&array_path, &scope)
        .into_iter()
        .skip(1)
    {
        if let Some(dims) = array_dims.get(&candidate) {
            #[cfg(feature = "tracing")]
            debug!(array = %array_name, qualified = %candidate, dims = ?dims, "found in ancestor");
            return Some(dims.clone());
        }
    }
    None
}

/// Look up array dimensions with scope resolution.
///
/// Tries to find array dimensions by:
/// 1. Direct lookup (for already qualified names)
/// 2. Qualified with var_context scope (e.g., `lines` -> `world.x_label.lines`)
/// 3. Parent scope resolution (walking up the scope chain)
fn lookup_array_dims_in_scope(
    array_name: &str,
    var_context: Option<&str>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Option<Vec<i64>> {
    // 1. Try direct lookup first
    if let Some(dims) = array_dims.get(array_name) {
        #[cfg(feature = "tracing")]
        debug!(array = %array_name, dims = ?dims, "found array dimensions (direct)");
        return Some(dims.clone());
    }

    // 2. If we have var_context, try scoped lookups
    let context = var_context?;
    let enclosing = ComponentPath::from_flat_path(context).parent()?;
    let array_path = ComponentPath::from_flat_path(array_name);

    // Try the enclosing scope first
    let qualified = enclosing.join(&array_path).to_flat_string();
    if let Some(dims) = array_dims.get(&qualified) {
        #[cfg(feature = "tracing")]
        debug!(array = %array_name, qualified = %qualified, dims = ?dims, "found in parent scope");
        return Some(dims.clone());
    }

    // 3. Walk up ancestor scopes
    lookup_dims_in_ancestors(array_name, &enclosing.to_flat_string(), array_dims)
}

/// Evaluate user function calls that return a real value.
pub fn eval_user_func_real(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext,
) -> Option<f64> {
    let span = name.span().or_else(|| {
        ctx.functions
            .get(name.as_str())
            .and_then(|function| (!function.span.is_dummy()).then_some(function.span))
    })?;
    eval_param_expr(
        &rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: args.to_vec(),
            is_constructor: false,
            span,
        },
        ctx,
    )
    .and_then(|value| value.to_real())
}

/// Try to evaluate a flat expression to a real value.
pub fn try_eval_flat_expr_real(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_reals: &FxHashMap<String, f64>,
) -> Option<f64> {
    let eval_ctx = build_eval_context(
        known_ints,
        known_reals,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
    );
    crate::constant::eval_expr(expr, &eval_ctx)
        .ok()
        .and_then(|value| value.to_real())
}

/// Try to extract an enumeration value from a flat expression.
pub fn try_extract_enum_value(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let name_str = name.to_string();
            if subscripts.is_empty() && looks_like_enum_literal_path(&name_str) {
                Some(name_str)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Try to evaluate a flat expression to an enum literal with context.
///
/// This supports:
/// - direct enum literals (`Types.Dynamics.SteadyState`)
/// - enum parameter references
/// - conditional enum expressions where conditions are compile-time evaluable
///   (MLS §4.9.5, §8.3.4).
pub fn try_eval_flat_expr_enum(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
) -> Option<String> {
    let param_ctx = ParamEvalContext {
        known_ints,
        known_reals: &FxHashMap::default(),
        known_bools,
        known_enums,
        array_dims: &FxHashMap::default(),
        functions: &FxHashMap::default(),
        var_context: None,
    };
    let mut evaluator = ParamEvaluator::new(&param_ctx);
    evaluator.set_var_context(None);
    register_enum_value_candidates(expr, &evaluator.enum_canonicalizer, &mut evaluator.eval_ctx);
    crate::constant::eval_expr(expr, &evaluator.eval_ctx)
        .ok()
        .and_then(|value| {
            value.as_enum().map(|(type_name, literal)| {
                if type_name.is_empty() {
                    literal.to_string()
                } else {
                    format!("{type_name}.{literal}")
                }
            })
        })
}

fn register_enum_value_candidates(
    expr: &rumoca_core::Expression,
    enum_canonicalizer: &EnumCanonicalizer,
    eval_ctx: &mut EvalContext,
) {
    match expr {
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (_, value) in branches {
                register_enum_value_candidates(value, enum_canonicalizer, eval_ctx);
            }
            register_enum_value_candidates(else_branch, enum_canonicalizer, eval_ctx);
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty()
            && !eval_ctx.parameters.contains_key(name.as_str())
            && looks_like_enum_literal_path(name.as_str()) =>
        {
            if let Some(identity) = enum_canonicalizer.canonicalize(name.as_str()) {
                eval_ctx.add_parameter(name.to_string(), identity.to_value());
            }
        }
        _ => {}
    }
}

fn register_enum_comparison_candidates(
    expr: &rumoca_core::Expression,
    enum_canonicalizer: &EnumCanonicalizer,
    eval_ctx: &mut EvalContext,
) {
    EnumComparisonRegistrar {
        enum_canonicalizer,
        eval_ctx,
    }
    .visit_expression(expr);
}

struct EnumComparisonRegistrar<'a> {
    enum_canonicalizer: &'a EnumCanonicalizer,
    eval_ctx: &'a mut EvalContext,
}

impl ExpressionVisitor for EnumComparisonRegistrar<'_> {
    fn visit_binary(
        &mut self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
    ) {
        if matches!(op, rumoca_core::OpBinary::Eq | rumoca_core::OpBinary::Neq) {
            register_enum_value_candidates(lhs, self.enum_canonicalizer, self.eval_ctx);
            register_enum_value_candidates(rhs, self.enum_canonicalizer, self.eval_ctx);
        }
        self.walk_binary(op, lhs, rhs);
    }
}

/// Check whether a dotted path is likely an enum literal reference.
///
/// Enum literals can be globally qualified (`Modelica.Fluid.Types.Dynamics.X`)
/// or scope-qualified (`pipe.Types.ModelStructure.a_v_b`). To avoid misclassifying
/// plain dotted parameter refs (e.g. `pipe1.system.energyDynamics`), require at
/// least one non-final path segment to be type-like (uppercase-initial).
pub fn looks_like_enum_literal_path(path: &str) -> bool {
    let parts = ComponentPath::from_flat_path(path).into_parts();
    if parts.len() < 2 {
        return false;
    }

    parts[..parts.len() - 1]
        .iter()
        .any(|segment| segment.chars().next().is_some_and(char::is_uppercase))
}

#[cfg(test)]
mod tests;
