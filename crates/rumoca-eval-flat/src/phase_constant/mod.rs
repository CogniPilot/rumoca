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

use crate::constant::{
    EvalContext, EvalError, ResolvedIdentityInventory, ResolvedOccurrenceKey, ResolvedShapeBinding,
    ResolvedValueBinding, Value,
};
use rumoca_ir_flat as flat;

use rumoca_core::{ComponentPath, ExpressionVisitor, scoped_component_path_candidates};

mod boolean_eval;
mod enum_identity;

use crate::constant::{
    CheckedCallPlan, bind_formal_extents, checked_call_plan, resolve_function_occurrence,
    selected_formal_index,
};
pub use boolean_eval::try_eval_flat_expr_boolean;
use enum_identity::EnumCanonicalizer;
pub use enum_identity::canonicalize_enum_literal;

// Conditional tracing support (SPEC_0008)
#[cfg(feature = "tracing")]
use tracing::debug;

/// Build an EvalContext from known parameter values and functions.
pub fn build_structural_eval_context(
    known_ints: &FxHashMap<String, i64>,
    known_reals: &FxHashMap<String, f64>,
    known_bools: &FxHashMap<String, bool>,
    array_dims: &FxHashMap<String, Vec<i64>>,
    functions: &FxHashMap<String, rumoca_core::Function>,
) -> Result<EvalContext, EvalError> {
    let parameter_capacity =
        known_ints.len() + known_reals.len() + known_bools.len() + array_dims.len();
    let mut eval_ctx =
        EvalContext::structural_preidentity_with_capacity(parameter_capacity, functions.len() * 2);
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
    try_issue_function_facts(&mut eval_ctx, functions.values())?;
    Ok(eval_ctx)
}

fn build_param_value_context(
    ctx: &ParamEvalContext<'_>,
) -> Result<(EvalContext, PreparedEnumAuthority), EvalError> {
    let parameter_capacity =
        ctx.known_ints.len() + ctx.known_reals.len() + ctx.known_bools.len() + ctx.array_dims.len();
    let (mut eval_ctx, enum_authority) = match ctx.identity_inventory {
        ParamIdentityInventory::StructuralPreIdentity { known_enums } => {
            let canonicalizer = EnumCanonicalizer::new(known_enums);
            let mut eval_ctx = EvalContext::structural_preidentity_with_capacity(
                parameter_capacity,
                ctx.functions.len() * 2,
            );
            for (name, literal) in known_enums {
                let Some(identity) = canonicalizer.canonicalize(literal) else {
                    continue;
                };
                let value = identity.to_value();
                eval_ctx.add_parameter(name.clone(), value.clone());
                eval_ctx.add_parameter(identity.to_flat_string(), value);
            }
            (eval_ctx, PreparedEnumAuthority::Structural(canonicalizer))
        }
        ParamIdentityInventory::Resolved { inventory } => {
            let ResolvedParamInventory {
                values,
                shapes,
                enum_catalog,
            } = inventory;
            let inventory = ResolvedIdentityInventory::try_from_bindings(
                values
                    .iter()
                    .map(|(identity, value)| ResolvedValueBinding {
                        identity: *identity,
                        value: value.clone(),
                    })
                    .collect(),
                shapes
                    .iter()
                    .map(|(identity, dimensions)| ResolvedShapeBinding {
                        identity: *identity,
                        dimensions: dimensions.clone(),
                    })
                    .collect(),
            )?;
            (
                EvalContext::resolved(
                    parameter_capacity,
                    ctx.functions.len() * 2,
                    inventory,
                    enum_catalog.clone(),
                ),
                PreparedEnumAuthority::Resolved,
            )
        }
    };
    for (k, v) in ctx.known_ints {
        eval_ctx.add_parameter(k.clone(), Value::Integer(*v));
    }
    for (k, v) in ctx.known_reals {
        eval_ctx.add_parameter(k.clone(), Value::Real(*v));
    }
    for (k, v) in ctx.known_bools {
        eval_ctx.add_parameter(k.clone(), Value::Bool(*v));
    }
    for (k, v) in ctx.array_dims {
        eval_ctx.add_array_dimensions(k.clone(), v.clone());
    }
    try_issue_function_facts(&mut eval_ctx, ctx.functions.values())?;
    eval_ctx.set_lookup_scope(
        ctx.var_context
            .map(ComponentPath::from_flat_path)
            .and_then(|path| path.parent()),
    );
    Ok((eval_ctx, enum_authority))
}

fn try_issue_function_facts<'a>(
    eval_ctx: &mut EvalContext,
    functions: impl IntoIterator<Item = &'a rumoca_core::Function>,
) -> Result<(), EvalError> {
    let functions = functions.into_iter().collect::<Vec<_>>();
    let mut issued: FxHashMap<rumoca_core::FunctionInstanceId, &'a rumoca_core::Function> =
        FxHashMap::default();
    for function in functions {
        if let Some(instance_id) = function.instance_id {
            if issued
                .get(&instance_id)
                .is_some_and(|existing| *existing == function)
            {
                continue;
            }
            issued.entry(instance_id).or_insert(function);
        }
        eval_ctx.try_add_function(function.clone())?;
    }
    Ok(())
}

fn resolve_param_function_occurrence<'a>(
    occurrence: &'a rumoca_core::Reference,
    ctx: &'a ParamEvalContext<'_>,
    span: rumoca_core::Span,
) -> Result<crate::constant::CheckedFunctionTarget<'a>, EvalError> {
    let has_pending = ctx
        .functions
        .values()
        .any(|function| function.instance_id.is_none());
    if occurrence.resolved_function().is_none() && has_pending {
        return Err(EvalError::PendingCallableIdentity {
            name: occurrence.as_str().to_string(),
            span,
        });
    }
    resolve_function_occurrence(occurrence, ctx.functions.values(), span)
}

fn eval_param_expr(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Value>, EvalError> {
    ParamEvaluator::new(ctx)?.eval_value(expr, ctx.var_context)
}

/// Context for compile-time parameter expression evaluation (MLS §4.4).
pub struct ParamEvalContext<'a> {
    pub known_ints: &'a FxHashMap<String, i64>,
    pub known_reals: &'a FxHashMap<String, f64>,
    pub known_bools: &'a FxHashMap<String, bool>,
    pub array_dims: &'a FxHashMap<String, Vec<i64>>,
    /// Functions available for evaluation.
    pub functions: &'a FxHashMap<String, rumoca_core::Function>,
    /// Identity-keyed values and shapes for post-Resolve references, or an
    /// explicit declaration that the input is structural pre-identity IR.
    pub identity_inventory: ParamIdentityInventory<'a>,
    /// The fully qualified name of the variable whose binding we're evaluating.
    /// Used to resolve unqualified modification bindings to parent scope (MLS §7.2).
    pub var_context: Option<&'a str>,
}

#[derive(Clone, Copy)]
pub enum ParamIdentityInventory<'a> {
    Resolved {
        inventory: ResolvedParamInventory<'a>,
    },
    StructuralPreIdentity {
        known_enums: &'a FxHashMap<String, String>,
    },
}

#[derive(Clone, Copy)]
pub struct ResolvedParamInventory<'a> {
    values: &'a FxHashMap<ResolvedOccurrenceKey, Value>,
    shapes: &'a FxHashMap<ResolvedOccurrenceKey, Vec<i64>>,
    enum_catalog: &'a crate::constant::ResolvedEnumCatalog,
}

impl<'a> ResolvedParamInventory<'a> {
    pub fn new(
        values: &'a FxHashMap<ResolvedOccurrenceKey, Value>,
        shapes: &'a FxHashMap<ResolvedOccurrenceKey, Vec<i64>>,
        enum_catalog: &'a crate::constant::ResolvedEnumCatalog,
    ) -> Self {
        Self {
            values,
            shapes,
            enum_catalog,
        }
    }
}

impl<'a> ParamEvalContext<'a> {
    pub fn new_structural(
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
            array_dims,
            functions,
            identity_inventory: ParamIdentityInventory::StructuralPreIdentity { known_enums },
            var_context,
        }
    }

    pub fn new_resolved(
        known_ints: &'a FxHashMap<String, i64>,
        known_reals: &'a FxHashMap<String, f64>,
        known_bools: &'a FxHashMap<String, bool>,
        array_dims: &'a FxHashMap<String, Vec<i64>>,
        functions: &'a FxHashMap<String, rumoca_core::Function>,
        inventory: ResolvedParamInventory<'a>,
        var_context: Option<&'a str>,
    ) -> Self {
        Self {
            known_ints,
            known_reals,
            known_bools,
            array_dims,
            functions,
            identity_inventory: ParamIdentityInventory::Resolved { inventory },
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
    enum_authority: PreparedEnumAuthority,
}

enum PreparedEnumAuthority {
    Structural(EnumCanonicalizer),
    Resolved,
}

impl ParamEvaluator {
    pub fn new(ctx: &ParamEvalContext<'_>) -> Result<Self, EvalError> {
        let (eval_ctx, enum_authority) = build_param_value_context(ctx)?;
        Ok(Self {
            eval_ctx,
            enum_authority,
        })
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
    ) -> Result<Option<Value>, EvalError> {
        self.set_var_context(var_context);
        if let PreparedEnumAuthority::Structural(canonicalizer) = &self.enum_authority {
            register_enum_comparison_candidates(expr, canonicalizer, &mut self.eval_ctx);
        }
        crate::constant::eval_optional(expr, &self.eval_ctx)
    }

    pub fn eval_integer(
        &mut self,
        expr: &rumoca_core::Expression,
        var_context: Option<&str>,
    ) -> Result<Option<i64>, EvalError> {
        let value = self.eval_value(expr, var_context)?;
        project_optional_value(expr, value, "Integer", Value::as_integer)
    }

    pub fn eval_boolean(
        &mut self,
        expr: &rumoca_core::Expression,
        var_context: Option<&str>,
    ) -> Result<Option<bool>, EvalError> {
        let value = self.eval_value(expr, var_context)?;
        project_optional_value(expr, value, "Boolean", Value::as_bool)
    }

    pub fn eval_real(
        &mut self,
        expr: &rumoca_core::Expression,
        var_context: Option<&str>,
    ) -> Result<Option<f64>, EvalError> {
        let value = self.eval_value(expr, var_context)?;
        project_optional_value(expr, value, "Real", Value::to_real)
    }
}

fn project_optional_value<T>(
    expr: &rumoca_core::Expression,
    value: Option<Value>,
    expected: &'static str,
    project: impl FnOnce(&Value) -> Option<T>,
) -> Result<Option<T>, EvalError> {
    let Some(value) = value else {
        return Ok(None);
    };
    let span = expr.span().ok_or_else(|| {
        EvalError::missing_source_context("constant value projection is missing source provenance")
    })?;
    project(&value)
        .map(Some)
        .ok_or_else(|| EvalError::type_mismatch(expected, value.type_name(), span))
}

/// Integer evaluation with full context.
pub fn try_eval_integer_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Result<Option<i64>, EvalError> {
    ParamEvaluator::new(ctx)?.eval_integer(expr, ctx.var_context)
}

/// Try to evaluate a flat expression to a boolean value with full context.
///
/// This extends `try_eval_flat_expr_boolean` with scoped VarRef resolution
/// via `var_context` (MLS §7.2), so unqualified enum/bool refs in parameter
/// bindings can be evaluated while computing integer if-expressions.
pub fn try_eval_flat_expr_boolean_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Result<Option<bool>, EvalError> {
    ParamEvaluator::new(ctx)?.eval_boolean(expr, ctx.var_context)
}

/// Evaluate a flat expression to a real using scoped lookup context.
pub fn try_eval_real_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext,
) -> Result<Option<f64>, EvalError> {
    ParamEvaluator::new(ctx)?.eval_real(expr, ctx.var_context)
}

/// Infer array dimensions from an array literal binding.
pub fn try_infer_better_dims(var: &flat::Variable) -> Result<Vec<i64>, EvalError> {
    try_infer_better_dims_with_functions(var, &FxHashMap::default())
}

/// [`try_infer_better_dims`] against the callable catalog the binding's
/// user-function calls resolve in.
///
/// A binding such as `orientation[m] = f(m)` states its shape through the
/// callee's declared output, so the callee must be present to read it. Only
/// the declared output shape is consulted: no body is executed, and an actual
/// the empty value context cannot settle leaves the declared dimensions in
/// force rather than producing an error.
pub fn try_infer_better_dims_with_functions(
    var: &flat::Variable,
    functions: &FxHashMap<String, rumoca_core::Function>,
) -> Result<Vec<i64>, EvalError> {
    if let Some(binding) = &var.binding
        && let Some(inferred) = infer_array_dimensions_checked_with_functions(binding, functions)?
        && inferred.len() > var.dims.len()
    {
        return Ok(inferred);
    }
    Ok(var.dims.clone())
}

/// MLS §10.1: When a variable is declared with unspecified dimensions (`:`) and
/// bound to an array literal, the dimensions can be inferred from the literal's structure.
pub fn infer_array_dimensions_checked(
    expr: &rumoca_core::Expression,
) -> Result<Option<Vec<i64>>, EvalError> {
    infer_array_dimensions_checked_with_functions(expr, &FxHashMap::default())
}

/// [`infer_array_dimensions_checked`] against the callable catalog the
/// expression's user-function calls resolve in; every value map stays empty.
pub fn infer_array_dimensions_checked_with_functions(
    expr: &rumoca_core::Expression,
    functions: &FxHashMap<String, rumoca_core::Function>,
) -> Result<Option<Vec<i64>>, EvalError> {
    let known_ints = FxHashMap::default();
    let known_reals = FxHashMap::default();
    let known_bools = FxHashMap::default();
    let known_enums = FxHashMap::default();
    let array_dims = FxHashMap::default();
    let ctx = ParamEvalContext::new_structural(
        &known_ints,
        &known_reals,
        &known_bools,
        &known_enums,
        &array_dims,
        functions,
        None,
    );
    infer_array_dimensions_full_with_functions(expr, &ctx)
}

/// Infer array dimensions with full context including conditional expression support.
pub fn infer_array_dimensions_full_with_conds(
    expr: &rumoca_core::Expression,
    known_ints: &FxHashMap<String, i64>,
    known_bools: &FxHashMap<String, bool>,
    known_enums: &FxHashMap<String, String>,
    array_dims: &FxHashMap<String, Vec<i64>>,
) -> Result<Option<Vec<i64>>, EvalError> {
    let known_reals = FxHashMap::default();
    let functions = FxHashMap::default();
    let ctx = ParamEvalContext::new_structural(
        known_ints,
        &known_reals,
        known_bools,
        known_enums,
        array_dims,
        &functions,
        None,
    );
    infer_array_dimensions_full_with_functions(expr, &ctx)
}

/// Infer array dimensions with function output shape metadata available.
pub fn infer_array_dimensions_full_with_functions(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    let span = expr.span().ok_or_else(|| {
        EvalError::missing_source_context("array-shape expression has no source provenance")
    })?;
    crate::constant::validate_semantic_expression(expr, span)?;
    infer_array_dimensions_with_context(expr, ctx)
}

fn infer_array_dimensions_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
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
            start,
            step,
            end,
            span,
        } => infer_range_dimensions_with_context(start, step.as_deref(), end, *span, ctx),
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
        rumoca_core::Expression::FunctionCall { .. } => {
            infer_user_function_call_dimensions(expr, ctx)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let Some(dims) = infer_array_dimensions_with_context(base, ctx)? else {
                return Ok(None);
            };
            project_dims_by_subscripts(&dims, subscripts, ctx)
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let Some(dims) = reference_array_dimensions(name, ctx)? else {
                return Ok(None);
            };
            project_dims_by_subscripts(&dims, subscripts, ctx)
        }
        _ => Ok(None),
    }
}

fn project_dims_by_subscripts(
    dims: &[i64],
    subscripts: &[rumoca_core::Subscript],
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    let mut projected = Vec::new();
    let mut dim_index = 0usize;
    for subscript in subscripts {
        let Some(dim) = dims.get(dim_index).copied() else {
            return Ok(None);
        };
        match subscript {
            rumoca_core::Subscript::Index { .. } => {}
            rumoca_core::Subscript::Expr { expr, .. } => {
                let Some(_) = try_eval_integer_with_context(expr, ctx)? else {
                    return Ok(None);
                };
            }
            rumoca_core::Subscript::Colon { .. } => projected.push(dim),
        }
        dim_index += 1;
    }
    projected.extend_from_slice(&dims[dim_index..]);
    Ok(Some(projected))
}

fn infer_user_function_call_dimensions(
    call: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    let call_span = call.span().ok_or_else(|| {
        EvalError::missing_source_context("function-shape call has no source provenance")
    })?;
    let rumoca_core::Expression::FunctionCall { name, .. } = call else {
        return Err(EvalError::InvalidSemanticIr {
            reason: "function-shape inference received a non-call expression".to_string(),
            span: call_span,
        });
    };
    let target = resolve_param_function_occurrence(name, ctx, call_span)?;
    let plan = checked_call_plan(target, call, call_span)?;
    let Some(call) = CheckedShapeCall::construct(plan, ctx)? else {
        return Ok(None);
    };
    call.output_dimensions()
}

fn infer_function_arg_dims(
    arg: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    if let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = arg
        && subscripts.is_empty()
    {
        return reference_array_dimensions(name, ctx);
    }

    infer_array_dimensions_full_with_functions(arg, ctx)
}

fn reference_array_dimensions(
    reference: &rumoca_core::Reference,
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    match ctx.identity_inventory {
        ParamIdentityInventory::Resolved { inventory } => {
            let (Some(instance_id), Some(root_def_id)) =
                (reference.instance_id(), reference.root_def_id())
            else {
                let Some(span) = reference.span().filter(|span| !span.is_dummy()) else {
                    return Err(EvalError::missing_source_context(format!(
                        "resolved array reference `{}` lacks source provenance",
                        reference.as_str()
                    )));
                };
                return Err(EvalError::InvalidSemanticIr {
                    reason: format!(
                        "resolved array reference `{}` lacks its complete occurrence identity",
                        reference.as_str()
                    ),
                    span,
                });
            };
            Ok(inventory
                .shapes
                .get(&ResolvedOccurrenceKey {
                    instance_id,
                    root_def_id,
                })
                .cloned())
        }
        ParamIdentityInventory::StructuralPreIdentity { .. } => Ok(lookup_array_dims_in_scope(
            reference.as_str(),
            ctx.var_context,
            ctx.array_dims,
        )),
    }
}

fn infer_array_literal_dimensions_with_context(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    if elements.is_empty() {
        return Ok(Some(vec![0]));
    }

    if is_matrix {
        return infer_matrix_constructor_dimensions_with_context(elements, ctx);
    }

    let mut dims = vec![elements.len() as i64];
    if let Some(first) = elements.first()
        && let Some(inner_dims) = infer_array_dimensions_with_context(first, ctx)?
    {
        dims.extend(inner_dims);
    }
    Ok(Some(dims))
}

fn infer_matrix_constructor_dimensions_with_context(
    elements: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    let has_nested_rows = matches!(
        elements.first(),
        Some(rumoca_core::Expression::Array { .. })
    );
    if !has_nested_rows {
        return Ok(infer_matrix_row_dimensions_with_context(elements, ctx)?
            .map(|(rows, cols)| vec![rows, cols]));
    }

    let mut rows = 0i64;
    let mut expected_cols = None;
    for row in elements {
        let rumoca_core::Expression::Array {
            elements: row_elements,
            ..
        } = row
        else {
            return Ok(None);
        };
        let Some((row_count, col_count)) =
            infer_matrix_row_dimensions_with_context(row_elements, ctx)?
        else {
            return Ok(None);
        };
        match expected_cols {
            Some(expected) if expected != col_count => return Ok(None),
            None => expected_cols = Some(col_count),
            _ => {}
        }
        rows += row_count;
    }

    Ok(expected_cols.map(|cols| vec![rows, cols]))
}

fn infer_matrix_row_dimensions_with_context(
    elements: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<(i64, i64)>, EvalError> {
    let single_entry = elements.len() == 1;
    let mut expected_rows = None;
    let mut cols = 0i64;
    for element in elements {
        let dims = match infer_array_dimensions_with_context(element, ctx)? {
            Some(dims) => dims,
            None => {
                let Some(dims) = scalar_matrix_entry_dims(element) else {
                    return Ok(None);
                };
                dims
            }
        };
        let Some((entry_rows, entry_cols)) = matrix_entry_dimensions(&dims, single_entry) else {
            return Ok(None);
        };
        match expected_rows {
            Some(expected) if expected != entry_rows => return Ok(None),
            None => expected_rows = Some(entry_rows),
            _ => {}
        }
        cols += entry_cols;
    }
    Ok(expected_rows.map(|rows| (rows, cols)))
}

fn scalar_matrix_entry_dims(expr: &rumoca_core::Expression) -> Option<Vec<i64>> {
    matches!(expr, rumoca_core::Expression::Literal { .. }).then(Vec::new)
}

fn infer_array_comprehension_dimensions_with_context(
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    if filter.is_some() {
        return Ok(None);
    }

    let mut dims = Vec::with_capacity(indices.len().saturating_add(1));
    for index in indices {
        let Some(range_dims) = infer_array_dimensions_with_context(&index.range, ctx)? else {
            return Ok(None);
        };
        if range_dims.is_empty() {
            return Ok(None);
        }
        let iter_size = range_dims
            .iter()
            .copied()
            .fold(1i64, |acc, dim| acc.saturating_mul(dim.max(0)));
        dims.push(iter_size);
    }

    if let Some(mut inner_dims) = infer_array_dimensions_with_context(expr, ctx)? {
        dims.append(&mut inner_dims);
    }

    Ok(Some(dims))
}

fn infer_builtin_call_dimensions_with_context(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    match function {
        rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones => {
            eval_dimension_args_with_context(args, ctx)
        }
        rumoca_core::BuiltinFunction::Fill => {
            if args.len() < 2 {
                return Ok(None);
            }
            eval_dimension_args_with_context(&args[1..], ctx)
        }
        rumoca_core::BuiltinFunction::Linspace => {
            if args.len() != 3 {
                return Ok(None);
            }
            let Some(n) = try_eval_integer_with_context(&args[2], ctx)? else {
                return Ok(None);
            };
            Ok((n >= 2).then_some(vec![n]))
        }
        rumoca_core::BuiltinFunction::Identity => {
            if args.len() != 1 {
                return Ok(None);
            }
            let Some(n) = try_eval_integer_with_context(&args[0], ctx)? else {
                return Ok(None);
            };
            Ok(Some(vec![n, n]))
        }
        rumoca_core::BuiltinFunction::Vector => {
            if args.len() != 1 {
                return Ok(None);
            }
            let Some(dims) = infer_array_dimensions_with_context(&args[0], ctx)? else {
                return Ok(None);
            };
            Ok(Some(vec![dims.iter().copied().product()]))
        }
        rumoca_core::BuiltinFunction::Matrix => {
            if args.len() != 1 {
                return Ok(None);
            }
            let Some(dims) = infer_array_dimensions_with_context(&args[0], ctx)? else {
                return Ok(None);
            };
            Ok(match dims.as_slice() {
                [] => Some(vec![1, 1]),
                [len] => Some(vec![*len, 1]),
                [_, _] => Some(dims),
                _ => None,
            })
        }
        _ => Ok(None),
    }
}

fn eval_dimension_args_with_context(
    args: &[rumoca_core::Expression],
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    let mut dims = Vec::with_capacity(args.len());
    for arg in args {
        let Some(dim) = try_eval_integer_with_context(arg, ctx)? else {
            return Ok(None);
        };
        dims.push(dim);
    }
    Ok((!dims.is_empty()).then_some(dims))
}

fn infer_if_dimensions_with_context(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    for (cond, then_expr) in branches {
        match try_eval_flat_expr_boolean_with_context(cond, ctx)? {
            Some(true) => return infer_array_dimensions_with_context(then_expr, ctx),
            Some(false) => continue,
            None => return Ok(None),
        }
    }
    infer_array_dimensions_with_context(else_branch, ctx)
}

struct CheckedShapeCall<'a> {
    plan: CheckedCallPlan<'a>,
    frame: EvalContext,
    enum_authority: PreparedEnumAuthority,
    formal_shapes: Vec<Vec<i64>>,
    formal_extents: Vec<Vec<i64>>,
    vectorization: CallVectorization<'a>,
}

enum CallVectorization<'a> {
    Scalar,
    Vectorized {
        shape: Vec<i64>,
        _authority: rumoca_core::AutomaticVectorizationAuthority<'a>,
    },
}

impl<'a> CheckedShapeCall<'a> {
    /// Bind the call's actuals and declared extents against the value context.
    ///
    /// `None` states that the context lacks a fact the declared shape depends
    /// on: an array actual whose shape is not known, or a declared extent
    /// whose expression names a value the context does not hold. The fold
    /// then reports no shape at all, which is the honest answer for
    /// parameter-dependent shape information that is not yet available, and
    /// is never a substitute value. A contradiction (a shape mismatch, a
    /// negative extent, a non-Integer extent) remains an error.
    fn construct(
        plan: CheckedCallPlan<'a>,
        ctx: &ParamEvalContext<'_>,
    ) -> Result<Option<Self>, EvalError> {
        let func = plan.function();
        let (frame, enum_authority) = build_param_value_context(ctx)?;
        let formal_shapes = vec![Vec::new(); func.inputs.len()];
        let formal_extents = vec![Vec::new(); func.inputs.len()];
        let mut call = Self {
            plan,
            frame,
            enum_authority,
            formal_shapes,
            formal_extents,
            vectorization: CallVectorization::Scalar,
        };
        if !call.bind_explicit_actuals(ctx)?
            || !call.bind_default_actuals(ctx)?
            || !call.evaluate_formal_extents()?
        {
            return Ok(None);
        }
        Ok(Some(call))
    }

    fn bind_explicit_actuals(&mut self, ctx: &ParamEvalContext<'_>) -> Result<bool, EvalError> {
        for index in 0..self.plan.function().inputs.len() {
            let Some(expr) = self.plan.explicit(index) else {
                continue;
            };
            let value = eval_param_expr(expr, ctx)?;
            let shape = actual_shape(
                expr,
                value.as_ref(),
                ctx,
                &self.plan.function().inputs[index],
            )?;
            if !self.bind_actual(index, shape, value)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn bind_default_actuals(&mut self, ctx: &ParamEvalContext<'_>) -> Result<bool, EvalError> {
        for order_index in 0..self.plan.default_order().len() {
            let index = self.plan.default_order()[order_index];
            let Some(expr) = self.plan.default(index) else {
                return Err(EvalError::Internal {
                    message: "checked default order selected an explicit actual".to_string(),
                });
            };
            let value = eval_prepared_frame_expr(expr, &mut self.frame, &self.enum_authority)?;
            let shape = match value.as_ref() {
                Some(value) => Some(checked_actual_value_shape(
                    value,
                    &self.plan.function().inputs[index],
                )?),
                None => self
                    .aliased_formal_shape(expr)
                    .or(infer_function_arg_dims(expr, ctx)?),
            };
            if !self.bind_actual(index, shape, value)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Bind one actual; `false` when an array formal's actual has no known
    /// shape, so the call cannot state a shape in this context.
    fn bind_actual(
        &mut self,
        index: usize,
        shape: Option<Vec<i64>>,
        value: Option<Value>,
    ) -> Result<bool, EvalError> {
        let param = &self.plan.function().inputs[index];
        let param_name = param.name.clone();
        let param_span = param.span;
        let scalar_formal = param.dimensions().is_empty();
        let Some(shape) = shape else {
            return Ok(scalar_formal);
        };
        if shape.iter().any(|extent| *extent < 0) {
            return Err(EvalError::function_error(
                format!("argument `{param_name}` has negative extent in {shape:?}"),
                param_span,
            ));
        }
        if scalar_formal && !shape.is_empty() {
            self.admit_vectorization(param_span, &shape)?;
        }
        self.formal_shapes[index] = shape.clone();
        let param = &self.plan.function().inputs[index];
        bind_formal_shape(&mut self.frame, param, shape)?;
        if let Some(value) = value {
            bind_formal_value(&mut self.frame, param, value)?;
        }
        Ok(true)
    }

    fn admit_vectorization(
        &mut self,
        param_span: rumoca_core::Span,
        actual: &[i64],
    ) -> Result<(), EvalError> {
        match &self.vectorization {
            CallVectorization::Vectorized {
                shape: expected, ..
            } if expected != actual => Err(EvalError::function_error(
                format!("vectorized arguments have mismatched shapes {expected:?} and {actual:?}"),
                param_span,
            )),
            CallVectorization::Vectorized { .. } => Ok(()),
            CallVectorization::Scalar => {
                let authority = self.plan.vectorization_authority().map_err(|_| {
                    EvalError::function_error(
                        format!(
                            "automatic vectorization of `{}` requires the exact call occurrence to be transitively non-replaceable",
                            self.plan.function().name
                        ),
                        self.plan.span(),
                    )
                })?;
                self.vectorization = CallVectorization::Vectorized {
                    shape: actual.to_vec(),
                    _authority: authority,
                };
                Ok(())
            }
        }
    }

    /// Whether every formal a declared extent of `param` reads is bound in
    /// the call frame.
    ///
    /// Formals live in the frame under their declaration identity, so a
    /// formal whose actual the value context could not settle is simply
    /// absent there. Reading it would not be an unknown name but a reference
    /// with no occurrence identity, which the frame's authority refuses as
    /// malformed; asking first keeps that refusal for genuinely malformed
    /// references and makes an unsettled actual the "no shape" answer. An
    /// array formal is bound by its checked shape alone: `size(x, k)` reads
    /// the shape, and no extent expression can consume an array value.
    fn extent_reads_are_bound(&self, param: &rumoca_core::FunctionParam) -> bool {
        let function = self.plan.function();
        let unbound_formal = |def_id: rumoca_core::DefId| {
            let Some(declared) = function
                .inputs
                .iter()
                .chain(function.outputs.iter())
                .chain(function.locals.iter())
                .find(|declared| declared.def_id == Some(def_id))
            else {
                return false;
            };
            let bound = self.frame.has_def_value(def_id)
                || (!declared.dimensions().is_empty() && self.frame.has_def_shape(def_id));
            !bound
        };
        !param.shape_expr.iter().any(|subscript| match subscript {
            rumoca_core::Subscript::Expr { expr, .. } => expr.contains_subexpression(|candidate| {
                matches!(
                    candidate,
                    rumoca_core::Expression::VarRef { name, .. }
                        if name.root_def_id().is_some_and(unbound_formal)
                )
            }),
            _ => false,
        })
    }

    /// Check every array formal's actual against its declared extents;
    /// `false` when a declared extent is not evaluable in this context.
    fn evaluate_formal_extents(&mut self) -> Result<bool, EvalError> {
        for (index, param) in self.plan.function().inputs.iter().enumerate() {
            if param.dimensions().is_empty() {
                continue;
            }
            if !self.extent_reads_are_bound(param) {
                return Ok(false);
            }
            let Some(expected) =
                evaluated_param_dimensions(param, &mut self.frame, &self.enum_authority)?
            else {
                return Ok(false);
            };
            // `expected` settles every written dimension to an integer, with
            // the retained sentinel standing in for `:`. Which of those are
            // exact obligations is decided from the formal itself, so an
            // unspecified dimension binds the actual's extent (the size
            // `size(x, k)` then reads) instead of refusing it, while a
            // literal or evaluated extent must match exactly.
            self.formal_extents[index] =
                bind_formal_extents(param, &expected, &self.formal_shapes[index])?;
            bind_formal_shape(&mut self.frame, param, self.formal_extents[index].clone())?;
        }
        Ok(true)
    }

    fn aliased_formal_shape(&self, expr: &rumoca_core::Expression) -> Option<Vec<i64>> {
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = expr
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        selected_formal_index(self.plan.function(), name)
            .map(|index| self.formal_shapes[index].clone())
    }

    fn output_dimensions(&self) -> Result<Option<Vec<i64>>, EvalError> {
        let Some(output) = self.plan.function().outputs.first() else {
            return Ok(None);
        };
        if !self.extent_reads_are_bound(output) {
            return Ok(None);
        }
        let mut frame = self.frame.clone();
        let mut dims = match &self.vectorization {
            CallVectorization::Vectorized { shape, .. } => shape.clone(),
            CallVectorization::Scalar => Vec::new(),
        };
        let Some(declared) = evaluated_param_dimensions(output, &mut frame, &self.enum_authority)?
        else {
            return Ok(None);
        };
        dims.extend(declared);
        Ok((!dims.is_empty()).then_some(dims))
    }
}

fn eval_prepared_frame_expr(
    expr: &rumoca_core::Expression,
    frame: &mut EvalContext,
    enum_authority: &PreparedEnumAuthority,
) -> Result<Option<Value>, EvalError> {
    if let PreparedEnumAuthority::Structural(canonicalizer) = enum_authority {
        register_enum_comparison_candidates(expr, canonicalizer, frame);
        register_enum_value_candidates(expr, canonicalizer, frame);
    }
    crate::constant::eval_optional(expr, frame)
}

fn bind_formal_shape(
    frame: &mut EvalContext,
    param: &rumoca_core::FunctionParam,
    shape: Vec<i64>,
) -> Result<(), EvalError> {
    let Some(def_id) = param.def_id else {
        return Err(EvalError::InvalidSemanticIr {
            reason: format!("function formal `{}` has no DefId", param.name),
            span: param.span,
        });
    };
    frame.add_def_array_dimensions(def_id, shape);
    Ok(())
}

fn bind_formal_value(
    frame: &mut EvalContext,
    param: &rumoca_core::FunctionParam,
    value: Value,
) -> Result<(), EvalError> {
    let Some(def_id) = param.def_id else {
        return Err(EvalError::InvalidSemanticIr {
            reason: format!("function formal `{}` has no DefId", param.name),
            span: param.span,
        });
    };
    frame.add_def_parameter(def_id, value);
    Ok(())
}

fn actual_shape(
    expr: &rumoca_core::Expression,
    value: Option<&Value>,
    ctx: &ParamEvalContext<'_>,
    param: &rumoca_core::FunctionParam,
) -> Result<Option<Vec<i64>>, EvalError> {
    match value {
        Some(value) => checked_actual_value_shape(value, param).map(Some),
        None => infer_function_arg_dims(expr, ctx),
    }
}

fn checked_actual_value_shape(
    value: &Value,
    param: &rumoca_core::FunctionParam,
) -> Result<Vec<i64>, EvalError> {
    if !matches!(value, Value::Array(_)) {
        return Ok(Vec::new());
    }
    crate::constant::value::checked_rectangular_shape(
        value,
        &param.type_name,
        param.dimensions().len(),
        param.span,
    )
}

/// The declared extents of a formal in the call frame, or `None` when a
/// declared extent expression names a value the frame does not hold (an
/// actual the value context could not settle). A frame that holds the value
/// but not as an Integer, or yields a negative extent, is a contradiction.
fn evaluated_param_dimensions(
    param: &rumoca_core::FunctionParam,
    frame: &mut EvalContext,
    enum_authority: &PreparedEnumAuthority,
) -> Result<Option<Vec<i64>>, EvalError> {
    if param.shape_expr.len() != param.dimensions().len() {
        return Ok(Some(param.dimensions().to_vec()));
    }
    let mut dimensions = Vec::with_capacity(param.shape_expr.len());
    for (index, subscript) in param.shape_expr.iter().enumerate() {
        let extent = match subscript {
            rumoca_core::Subscript::Index { value, .. } => *value,
            rumoca_core::Subscript::Colon { .. } => param.dimensions()[index],
            rumoca_core::Subscript::Expr { expr, .. } => {
                let Some(value) = eval_prepared_frame_expr(expr, frame, enum_authority)? else {
                    return Ok(None);
                };
                value.as_integer().ok_or_else(|| {
                    EvalError::type_mismatch("Integer", value.type_name(), param.span)
                })?
            }
        };
        if extent < 0 {
            return Err(EvalError::function_error(
                format!(
                    "dimension {} of `{}` evaluates to negative extent {extent}",
                    index + 1,
                    param.name
                ),
                param.span,
            ));
        }
        dimensions.push(extent);
    }
    Ok(Some(dimensions))
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
    span: rumoca_core::Span,
    ctx: &ParamEvalContext<'_>,
) -> Result<Option<Vec<i64>>, EvalError> {
    if step.is_none() {
        let start_value = try_eval_flat_expr_enum_value_with_context(start, ctx)?;
        let end_value = try_eval_flat_expr_enum_value_with_context(end, ctx)?;
        match (start_value.as_ref(), end_value.as_ref()) {
            (Some(Value::ResolvedEnum(start)), Some(Value::ResolvedEnum(end))) => {
                if start.declaration() != end.declaration() {
                    return Err(EvalError::type_mismatch(
                        start.display_type(),
                        end.display_type(),
                        span,
                    ));
                }
                let len = if end.ordinal() >= start.ordinal() {
                    end.ordinal() - start.ordinal() + 1
                } else {
                    0
                };
                return Ok(Some(vec![len]));
            }
            (Some(Value::ResolvedEnum(value)), Some(other))
            | (Some(other), Some(Value::ResolvedEnum(value))) => {
                return Err(EvalError::type_mismatch(
                    value.display_type(),
                    other.type_name(),
                    span,
                ));
            }
            _ => {}
        }
    }

    let Some(start_val) = try_eval_integer_with_context(start, ctx)? else {
        return Ok(None);
    };
    let Some(end_val) = try_eval_integer_with_context(end, ctx)? else {
        return Ok(None);
    };
    let step_val = match step {
        Some(step) => {
            let Some(value) = try_eval_integer_with_context(step, ctx)? else {
                return Ok(None);
            };
            value
        }
        None => 1,
    };

    if step_val == 0 {
        return Err(EvalError::range_error(
            "range step cannot be zero",
            step.and_then(rumoca_core::Expression::span)
                .filter(|span| !span.is_dummy())
                .unwrap_or(span),
        ));
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

    Ok(Some(vec![len]))
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
) -> Result<Option<f64>, EvalError> {
    let target = resolve_param_function_occurrence(name, ctx, rumoca_core::Span::DUMMY)?;
    let target_span = (!target.function().span.is_dummy()).then_some(target.function().span);
    let Some(span) = name.span().or(target_span) else {
        return Err(EvalError::missing_source_context(
            "function constant evaluation is missing source provenance",
        ));
    };
    let call = rumoca_core::Expression::FunctionCall {
        name: name.clone(),
        args: args.to_vec(),
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span,
    };
    let value = eval_param_expr(&call, ctx)?;
    project_optional_value(&call, value, "Real", Value::to_real)
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
) -> Result<Option<String>, EvalError> {
    let known_reals = FxHashMap::with_hasher(rustc_hash::FxBuildHasher);
    let array_dims = FxHashMap::with_hasher(rustc_hash::FxBuildHasher);
    let functions = FxHashMap::with_hasher(rustc_hash::FxBuildHasher);
    let param_ctx = ParamEvalContext::new_structural(
        known_ints,
        &known_reals,
        known_bools,
        known_enums,
        &array_dims,
        &functions,
        None,
    );
    try_eval_flat_expr_enum_with_context(expr, &param_ctx)
}

pub fn try_eval_flat_expr_enum_with_context(
    expr: &rumoca_core::Expression,
    param_ctx: &ParamEvalContext<'_>,
) -> Result<Option<String>, EvalError> {
    let Some(value) = try_eval_flat_expr_enum_value_with_context(expr, param_ctx)? else {
        return Ok(None);
    };
    let Some((type_name, literal)) = value.as_enum() else {
        let span = expr.span().ok_or_else(|| {
            EvalError::missing_source_context(
                "enumeration constant expression has no source provenance",
            )
        })?;
        return Err(EvalError::type_mismatch(
            "Enumeration",
            value.type_name(),
            span,
        ));
    };
    Ok(Some(if type_name.is_empty() {
        literal.to_string()
    } else {
        format!("{type_name}.{literal}")
    }))
}

pub fn try_eval_flat_expr_enum_value_with_context(
    expr: &rumoca_core::Expression,
    param_ctx: &ParamEvalContext<'_>,
) -> Result<Option<Value>, EvalError> {
    let mut evaluator = ParamEvaluator::new(param_ctx)?;
    evaluator.set_var_context(None);
    if let PreparedEnumAuthority::Structural(canonicalizer) = &evaluator.enum_authority {
        register_enum_value_candidates(expr, canonicalizer, &mut evaluator.eval_ctx);
    }
    crate::constant::eval_optional(expr, &evaluator.eval_ctx)
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
        } if subscripts.is_empty() && looks_like_enum_literal_path(name.as_str()) => {
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
