//! Expression dispatch: the top-level constant evaluator and the per-node
//! evaluators for references, calls, conditionals, arrays, and subscripts.

use rumoca_core::{ExpressionVisitor, FallibleExpressionVisitor, Span};

use super::builtin_dispatch::{eval_builtin_function, validate_builtin_arity};
use super::builtins::eval_builtin_in_context;
use super::context::EvalContext;
use super::errors::EvalError;
use super::operators::{eval_binary_op, eval_unary_op};
use super::range_eval::eval_range;
use super::value::{Value, ValueSelection, integer_subscript_index, materialize_value_selection};
use super::{
    BuiltinFunction, DEFAULT_EVAL_BUDGET, EvalLimits, Expression, Function, Literal, OpBinary,
    OpUnary, Subscript, function_eval,
};

/// Evaluate a flat expression to a constant value.
///
/// Returns an error if the expression cannot be evaluated at compile time
/// (e.g., references time-varying variables, uses unsupported operations).
pub fn eval_expr(expr: &Expression, ctx: &EvalContext) -> Result<Value, EvalError> {
    let span = expr.span().ok_or_else(|| {
        EvalError::missing_source_context("constant expression is missing source provenance")
    })?;
    validate_semantic_expression_in_context(expr, ctx, span)?;
    eval_expr_unchecked(expr, ctx, span)
}

/// Evaluate with a span for error reporting.
pub fn eval_expr_with_span(
    expr: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    validate_semantic_expression_in_context(expr, ctx, span)?;
    eval_expr_unchecked(expr, ctx, span)
}

fn eval_expr_unchecked(
    expr: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let span = expr.span().unwrap_or(span);
    match expr {
        Expression::Literal { value: lit, .. } => Ok(eval_literal(lit)),
        Expression::VarRef {
            name, subscripts, ..
        } => eval_var_ref(name, subscripts, ctx, span),
        Expression::Binary { op, lhs, rhs, .. } => eval_flat_binary(op, lhs, rhs, ctx, span),
        Expression::Unary { op, rhs, .. } => eval_flat_unary(op, rhs, ctx, span),
        Expression::BuiltinCall { function, args, .. } => {
            eval_builtin_call(function, args, ctx, span)
        }
        Expression::FunctionCall { .. } if function_eval::is_partial_application(expr) => {
            Err(EvalError::UnsupportedExpression {
                kind: "function-valued partial application".to_string(),
                span,
            })
        }
        Expression::FunctionCall { .. } => eval_fn_call(expr, ctx, span),
        Expression::StringConversion { .. } => Err(EvalError::UnsupportedExpression {
            kind: "predefined String conversion".to_string(),
            span,
        }),
        Expression::If {
            branches,
            else_branch,
            ..
        } => eval_flat_if(branches, else_branch, ctx, span),
        Expression::Array { elements, .. } => eval_flat_array(elements, ctx, span),
        Expression::Range {
            start, step, end, ..
        } => eval_range(start, step.as_deref(), end, ctx, span),
        Expression::ArrayComprehension { .. } => Err(EvalError::UnsupportedExpression {
            kind: "ArrayComprehension".to_string(),
            span,
        }),
        Expression::Index {
            base, subscripts, ..
        } => {
            // Expanded array components are stored as exact scalar projections
            // (for example, `f[2]`). Prefer that semantic key before evaluating
            // `f` as an aggregate: the context may also contain a scalar named
            // `f` from a different occurrence, which is not evidence that the
            // written projection indexes that scalar.
            if ctx.permits_structural_name_lookup()
                && let Some(path) = rumoca_core::flat_expression_component_path(expr)
                && let Some(value) = ctx.get(&path.to_flat_string())
            {
                return Ok(value.clone());
            }
            eval_flat_index(base, subscripts, ctx, span)
        }
        Expression::Tuple { elements, .. } => eval_flat_array(elements, ctx, span),
        Expression::FieldAccess {
            base,
            field,
            field_def_id,
            ..
        } => {
            if ctx.permits_structural_name_lookup()
                && let Some(path) = rumoca_core::flat_expression_component_path(expr)
                && let Some(value) = ctx.get(&path.to_flat_string())
            {
                return Ok(value.clone());
            }
            // Field access on complex expressions (e.g., func().field)
            // requires evaluating the base and then extracting the field
            let base_val = eval_expr_unchecked(base, ctx, span)?;
            if function_eval::is_exact_single_record_output(base, *field_def_id, ctx, span)? {
                return Ok(base_val);
            }
            eval_field_access(&base_val, field, span)
        }
        Expression::Empty { .. } => Err(EvalError::InvalidSemanticIr {
            reason: "empty expression reached constant evaluation".to_string(),
            span,
        }),
    }
}

struct SemanticExpressionValidator {
    fallback_span: Span,
}

struct FunctionCallShapeValidator<'a> {
    ctx: &'a EvalContext,
    fallback_span: Span,
}

impl FunctionCallShapeValidator<'_> {
    fn validate_function_call(
        &self,
        expr: &Expression,
        name: &rumoca_core::Reference,
        call_span: Span,
    ) -> Result<(), EvalError> {
        let span = span_or(call_span, self.fallback_span);
        let named_marker = named_function_call_arg(expr, span)?;
        if name.as_str() == "String" {
            return Err(EvalError::InvalidSemanticIr {
                reason: "predefined String conversion must use Expression::StringConversion"
                    .to_string(),
                span,
            });
        }
        if named_marker.is_some() || function_eval::is_partial_application(expr) {
            return Ok(());
        }
        self.ctx.refuse_pending_callable(name, span)?;
        let target = resolve_context_function_occurrence(name, self.ctx, span)?;
        drop(checked_call_plan(target, expr, span)?);
        Ok(())
    }
}

impl FallibleExpressionVisitor for FunctionCallShapeValidator<'_> {
    type Error = EvalError;

    fn visit_expression(&mut self, expr: &Expression) -> Result<(), Self::Error> {
        if let Expression::ArrayComprehension { indices, span, .. } = expr
            && indices.is_empty()
        {
            return Err(EvalError::InvalidSemanticIr {
                reason: "array comprehension has no index declarations".to_string(),
                span: span_or(*span, self.fallback_span),
            });
        }
        if let Expression::BuiltinCall {
            function,
            args,
            span,
        } = expr
        {
            validate_builtin_arity(*function, args.len(), span_or(*span, self.fallback_span))?;
        }
        if let Expression::FunctionCall { name, span, .. } = expr {
            self.validate_function_call(expr, name, *span)?;
        }
        self.walk_expression(expr)
    }
}

impl FallibleExpressionVisitor for SemanticExpressionValidator {
    type Error = EvalError;

    fn visit_expression(&mut self, expr: &Expression) -> Result<(), Self::Error> {
        let span = expr.span().unwrap_or(self.fallback_span);
        match expr {
            Expression::Empty { .. } => Err(EvalError::InvalidSemanticIr {
                reason: "empty expression is not valid semantic IR".to_string(),
                span,
            }),
            Expression::Unary {
                op: OpUnary::Empty, ..
            } => Err(EvalError::InvalidSemanticIr {
                reason: "empty unary operator is not valid semantic IR".to_string(),
                span,
            }),
            Expression::Binary {
                op: op @ (OpBinary::Empty | OpBinary::Assign),
                ..
            } => Err(EvalError::InvalidSemanticIr {
                reason: format!("binary operator {op:?} is not valid in a semantic expression"),
                span,
            }),
            _ => self.walk_expression(expr),
        }
    }
}

pub(crate) fn validate_semantic_expression(
    expr: &Expression,
    fallback_span: Span,
) -> Result<(), EvalError> {
    SemanticExpressionValidator { fallback_span }.visit_expression(expr)
}

pub(super) fn validate_semantic_expression_in_context(
    expr: &Expression,
    ctx: &EvalContext,
    fallback_span: Span,
) -> Result<(), EvalError> {
    validate_semantic_expression(expr, fallback_span)?;
    FunctionCallShapeValidator { ctx, fallback_span }.visit_expression(expr)
}

fn span_or(span: Span, fallback: Span) -> Span {
    if span.is_dummy() { fallback } else { span }
}

enum CheckedCallSlot<'a> {
    Explicit(&'a Expression),
    Default(&'a Expression),
}

pub(crate) struct CheckedCallPlan<'a> {
    target: CheckedFunctionTarget<'a>,
    slots: Vec<CheckedCallSlot<'a>>,
    default_order: Vec<usize>,
    span: Span,
}

pub(crate) struct CheckedFunctionTarget<'a> {
    function: &'a Function,
    occurrence: &'a rumoca_core::Reference,
    resolved: rumoca_core::ResolvedFunctionReference,
}

impl<'a> CheckedFunctionTarget<'a> {
    pub(crate) fn function(&self) -> &'a Function {
        self.function
    }
}

pub(crate) struct EvaluatedCall<'a> {
    plan: CheckedCallPlan<'a>,
    explicit_values: Vec<Option<Value>>,
}

impl<'a> CheckedCallPlan<'a> {
    pub(crate) fn function(&self) -> &'a Function {
        self.target.function
    }

    pub(crate) fn explicit(&self, index: usize) -> Option<&'a Expression> {
        match self.slots.get(index)? {
            CheckedCallSlot::Explicit(expr) => Some(expr),
            _ => None,
        }
    }
    pub(crate) fn default(&self, index: usize) -> Option<&'a Expression> {
        match self.slots.get(index)? {
            CheckedCallSlot::Default(expr) => Some(expr),
            _ => None,
        }
    }

    pub(crate) fn default_order(&self) -> &[usize] {
        &self.default_order
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }

    pub(crate) fn vectorization_authority(
        &self,
    ) -> Result<
        rumoca_core::AutomaticVectorizationAuthority<'a>,
        rumoca_core::AutomaticVectorizationRefusal,
    > {
        self.function()
            .automatic_vectorization_authority(self.target.resolved)
    }

    pub(crate) fn evaluate_explicit(
        self,
        mut evaluate: impl FnMut(usize, &Expression) -> Result<Value, EvalError>,
    ) -> Result<EvaluatedCall<'a>, EvalError> {
        let mut explicit_values = Vec::with_capacity(self.slots.len());
        for (index, slot) in self.slots.iter().enumerate() {
            let value = match slot {
                CheckedCallSlot::Explicit(expr) => Some(evaluate(index, expr)?),
                CheckedCallSlot::Default(_) => None,
            };
            explicit_values.push(value);
        }
        Ok(EvaluatedCall {
            plan: self,
            explicit_values,
        })
    }
}

impl<'a> EvaluatedCall<'a> {
    pub(crate) fn plan(&self) -> &CheckedCallPlan<'a> {
        &self.plan
    }

    pub(crate) fn explicit_value(&self, index: usize) -> Option<&Value> {
        self.explicit_values.get(index)?.as_ref()
    }
}

pub(crate) fn resolve_function_occurrence<'a>(
    occurrence: &'a rumoca_core::Reference,
    functions: impl IntoIterator<Item = &'a Function>,
    span: Span,
) -> Result<CheckedFunctionTarget<'a>, EvalError> {
    let Some(resolved) = occurrence.resolved_function() else {
        return Err(EvalError::InvalidSemanticIr {
            reason: format!(
                "user-function call `{}` has no resolved function occurrence",
                occurrence.as_str()
            ),
            span,
        });
    };
    let function = rumoca_core::resolve_function_instance(functions, resolved.instance_id)
        .map_err(|error| EvalError::InvalidSemanticIr {
            reason: format!(
                "resolved user-function call `{}` has invalid occurrence identity: {error}",
                occurrence.as_str()
            ),
            span,
        })?;
    Ok(CheckedFunctionTarget {
        function,
        occurrence,
        resolved,
    })
}

pub(crate) fn resolve_context_function_occurrence<'a>(
    occurrence: &'a rumoca_core::Reference,
    context: &'a EvalContext,
    span: Span,
) -> Result<CheckedFunctionTarget<'a>, EvalError> {
    context.refuse_pending_callable(occurrence, span)?;
    let Some(resolved) = occurrence.resolved_function() else {
        return Err(EvalError::InvalidSemanticIr {
            reason: format!(
                "user-function call `{}` has no resolved function occurrence",
                occurrence.as_str()
            ),
            span,
        });
    };
    let Some(function) = context.finalized_function_by_instance(resolved.instance_id) else {
        return Err(EvalError::InvalidSemanticIr {
            reason: format!(
                "resolved user-function call `{}` selects absent function instance {}",
                occurrence.as_str(),
                resolved.instance_id.index()
            ),
            span,
        });
    };
    Ok(CheckedFunctionTarget {
        function,
        occurrence,
        resolved,
    })
}

pub(crate) fn checked_call_plan<'a>(
    target: CheckedFunctionTarget<'a>,
    call: &'a Expression,
    span: Span,
) -> Result<CheckedCallPlan<'a>, EvalError> {
    let Expression::FunctionCall {
        name: occurrence,
        args,
        ..
    } = call
    else {
        return Err(EvalError::InvalidSemanticIr {
            reason: "checked user-function call is not a call expression".to_string(),
            span,
        });
    };
    if !std::ptr::eq(occurrence, target.occurrence) {
        return Err(EvalError::InvalidSemanticIr {
            reason: "checked function target belongs to a different call occurrence".to_string(),
            span,
        });
    }
    checked_call_plan_from_parts(target, args, span)
}

pub(crate) fn checked_statement_call_plan<'a>(
    target: CheckedFunctionTarget<'a>,
    statement: &'a rumoca_core::Statement,
    span: Span,
) -> Result<CheckedCallPlan<'a>, EvalError> {
    let rumoca_core::Statement::FunctionCall { comp, args, .. } = statement else {
        return Err(EvalError::InvalidSemanticIr {
            reason: "checked function statement is not a call statement".to_string(),
            span,
        });
    };
    if !std::ptr::eq(comp, target.occurrence) {
        return Err(EvalError::InvalidSemanticIr {
            reason: "checked function target belongs to a different call statement".to_string(),
            span,
        });
    }
    checked_call_plan_from_parts(target, args, span)
}

fn checked_call_plan_from_parts<'a>(
    target: CheckedFunctionTarget<'a>,
    args: &'a [Expression],
    span: Span,
) -> Result<CheckedCallPlan<'a>, EvalError> {
    let function = target.function;
    let mut slots: Vec<Option<&Expression>> = vec![None; function.inputs.len()];
    let mut next_positional = 0;
    let mut seen_named = false;
    for arg in args {
        let (index, value) = if let Some((name, value)) = named_function_call_arg(arg, span)? {
            seen_named = true;
            let Some(index) = function.inputs.iter().position(|input| input.name == name) else {
                return Err(EvalError::function_error(
                    format!(
                        "unknown named argument {name} for function {}",
                        function.name
                    ),
                    span,
                ));
            };
            (index, value)
        } else {
            if seen_named {
                return Err(EvalError::function_error(
                    format!(
                        "positional argument after named argument in {}",
                        function.name
                    ),
                    span,
                ));
            }
            if function.inputs.get(next_positional).is_none() {
                return Err(EvalError::function_error(
                    format!("too many arguments for function {}", function.name),
                    span,
                ));
            }
            let index = next_positional;
            next_positional += 1;
            (index, arg)
        };
        if slots[index].replace(value).is_some() {
            return Err(EvalError::function_error(
                format!(
                    "duplicate argument {} for function {}",
                    function.inputs[index].name, function.name
                ),
                span,
            ));
        }
    }
    let mut checked = Vec::with_capacity(function.inputs.len());
    for (input, explicit) in function.inputs.iter().zip(slots) {
        match (explicit, input.default.as_ref()) {
            (Some(value), _) => checked.push(CheckedCallSlot::Explicit(value)),
            (None, Some(default)) => checked.push(CheckedCallSlot::Default(default)),
            (None, None) => {
                return Err(EvalError::function_error(
                    format!(
                        "missing required argument {} for function {}",
                        input.name, function.name
                    ),
                    span,
                ));
            }
        }
    }
    let default_order = checked_default_order(function, &checked, span)?;
    Ok(CheckedCallPlan {
        target,
        slots: checked,
        default_order,
        span,
    })
}

fn checked_default_order(
    function: &Function,
    slots: &[CheckedCallSlot<'_>],
    span: Span,
) -> Result<Vec<usize>, EvalError> {
    let mut bound = slots
        .iter()
        .map(|slot| matches!(slot, CheckedCallSlot::Explicit(_)))
        .collect::<Vec<_>>();
    let mut order = Vec::new();
    while bound.iter().any(|is_bound| !is_bound) {
        let ready = function
            .inputs
            .iter()
            .enumerate()
            .position(|(index, param)| {
                !bound[index]
                    && default_dependencies_bound(param, function, |def_id| {
                        function
                            .inputs
                            .iter()
                            .position(|input| input.def_id == Some(def_id))
                            .is_some_and(|dependency| bound[dependency])
                    })
            });
        let Some(index) = ready else {
            let Some(index) = bound.iter().position(|is_bound| !is_bound) else {
                return Err(EvalError::Internal {
                    message: "checked call lost its pending default".to_string(),
                });
            };
            return Err(EvalError::CircularDependency {
                path: function.inputs[index].name.clone(),
                span,
            });
        };
        bound[index] = true;
        order.push(index);
    }
    Ok(order)
}

pub(crate) fn selected_formal_index(
    function: &Function,
    reference: &rumoca_core::Reference,
) -> Option<usize> {
    let read = reference.root_def_id()?;
    function
        .inputs
        .iter()
        .position(|input| input.def_id == Some(read))
}

pub(crate) fn default_dependencies_bound(
    param: &rumoca_core::FunctionParam,
    function: &Function,
    mut is_bound: impl FnMut(rumoca_core::DefId) -> bool,
) -> bool {
    let declared = || {
        function
            .inputs
            .iter()
            .chain(function.outputs.iter())
            .chain(function.locals.iter())
    };
    default_read_roots(param).iter().all(|root| {
        let Some(read) = root.def_id else {
            return false;
        };
        let selected = declared().find(|entry| entry.def_id == Some(read));
        match selected {
            Some(_) => is_bound(read),
            None => true,
        }
    })
}

struct ReadRoot {
    def_id: Option<rumoca_core::DefId>,
}

fn default_read_roots(param: &rumoca_core::FunctionParam) -> Vec<ReadRoot> {
    #[derive(Default)]
    struct ReadRootCollector {
        roots: Vec<ReadRoot>,
    }
    impl rumoca_core::ExpressionVisitor for ReadRootCollector {
        fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
            self.roots.push(ReadRoot {
                def_id: name.root_def_id(),
            });
            self.walk_var_ref(name, subscripts);
        }
    }
    let mut reads = ReadRootCollector::default();
    if let Some(default) = &param.default {
        reads.visit_expression(default);
    }
    for subscript in &param.shape_expr {
        reads.visit_subscript(subscript);
    }
    reads.roots
}

/// Evaluate a variable reference.
fn eval_var_ref(
    reference: &rumoca_core::Reference,
    subscripts: &[Subscript],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let name = reference.as_str();
    ctx.validate_resolved_reference(reference, span)?;
    // First try as a parameter
    if let Some(value) = ctx.get_reference(reference) {
        let value = value.clone();
        return if subscripts.is_empty() {
            Ok(value)
        } else {
            apply_subscripts(&value, subscripts, ctx, span)
        };
    }
    // Then try as an enum literal from context
    if let Some(value) = ctx.get_enum_reference(reference) {
        return Ok(Value::ResolvedEnum(value.clone()));
    }
    // A declared `fixed = false` parameter resolves; only its value is absent
    // (MLS §8.6). Reporting that as an unknown name reads as a resolution
    // defect, so name the construct the fold actually hit.
    if reference.root_def_id().is_none()
        && let Some(source) = ctx.deferred_parameter(name)
    {
        return Err(EvalError::initialization_deferred(name, source, span));
    }
    // DON'T guess that qualified names are enums - this causes bugs where
    // qualified variable names like "data.m" are incorrectly treated as enum literals
    // when they haven't been evaluated yet in multi-pass parameter evaluation.
    // Enum literals are explicitly added to context via add_parameter().
    Err(EvalError::unknown_variable(name, span))
}

/// Evaluate a binary expression.
fn eval_flat_binary(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    if matches!(op, OpBinary::And | OpBinary::Or) {
        return eval_flat_logical(op, lhs, rhs, ctx, span);
    }
    let lhs_val = eval_expr_unchecked(lhs, ctx, span)?;
    let rhs_val = eval_expr_unchecked(rhs, ctx, span)?;
    eval_binary_op(op, &lhs_val, &rhs_val, span)
}

fn eval_flat_logical(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    match eval_expr_unchecked(lhs, ctx, span) {
        Ok(Value::Bool(lhs)) if logical_value_determines_result(op, lhs) => Ok(Value::Bool(lhs)),
        Ok(lhs) => {
            let rhs = eval_expr_unchecked(rhs, ctx, span)?;
            eval_binary_op(op, &lhs, &rhs, span)
        }
        // A deferred parameter (MLS §8.6) is undetermined in exactly the sense
        // this short circuit tolerates: the operand has no value yet, and the
        // other operand may still decide the result on its own.
        Err(lhs_error) if lhs_error.runtime_dependent_reason().is_some() => {
            match eval_expr_unchecked(rhs, ctx, span) {
                Ok(Value::Bool(rhs)) if logical_value_determines_result(op, rhs) => {
                    Ok(Value::Bool(rhs))
                }
                Ok(Value::Bool(_)) => Err(lhs_error),
                Ok(value) => Err(EvalError::type_mismatch("Boolean", value.type_name(), span)),
                Err(rhs_error) if rhs_error.is_control_independent_failure() => Err(rhs_error),
                Err(_) => Err(lhs_error),
            }
        }
        Err(error) => Err(error),
    }
}

fn logical_value_determines_result(op: &OpBinary, value: bool) -> bool {
    matches!((op, value), (OpBinary::And, false) | (OpBinary::Or, true))
}

/// Evaluate a unary expression.
fn eval_flat_unary(
    op: &OpUnary,
    rhs: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let rhs_val = eval_expr_unchecked(rhs, ctx, span)?;
    eval_unary_op(op, &rhs_val, span)
}

/// Evaluate a builtin call expression.
fn eval_builtin_call(
    function: &BuiltinFunction,
    args: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    if matches!(function, BuiltinFunction::Size | BuiltinFunction::Ndims)
        && let Some(value) = eval_shape_builtin(function, args, ctx, span)?
    {
        return Ok(value);
    }
    let arg_values: Vec<Value> = args
        .iter()
        .map(|a| eval_expr_unchecked(a, ctx, span))
        .collect::<Result<_, _>>()?;
    if matches!(function, BuiltinFunction::Integer) {
        return eval_builtin_in_context(function.name(), &arg_values, ctx, span);
    }
    eval_builtin_function(function, &arg_values, span)
}

fn eval_shape_builtin(
    function: &BuiltinFunction,
    args: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Option<Value>, EvalError> {
    let Some(first) = args.first() else {
        return Ok(None);
    };
    let dimensions = match first {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            ctx.validate_resolved_reference(name, span)?;
            ctx.get_array_dimensions_reference(name)
        }
        _ if ctx.permits_structural_name_lookup() => {
            rumoca_core::flat_expression_component_path(first)
                .and_then(|path| ctx.get_array_dimensions(&path.to_flat_string()))
        }
        _ => None,
    };
    let Some(dims) = dimensions else {
        return Ok(None);
    };
    if matches!(function, BuiltinFunction::Ndims) {
        let rank = i64::try_from(dims.len()).map_err(|_| EvalError::Internal {
            message: "array rank exceeds i64 during ndims evaluation".to_string(),
        })?;
        return Ok(Some(Value::Integer(rank)));
    }
    match args {
        [_] => Ok(Some(Value::Array(
            dims.iter().copied().map(Value::Integer).collect(),
        ))),
        [_, dimension] => {
            let dimension = eval_expr_unchecked(dimension, ctx, span)?
                .as_integer()
                .ok_or_else(|| EvalError::type_mismatch("Integer", "non-integer", span))?;
            let zero_based = dimension
                .checked_sub(1)
                .ok_or_else(|| EvalError::function_error("dimension out of range", span))?;
            let index = usize::try_from(zero_based)
                .map_err(|_| EvalError::function_error("dimension out of range", span))?;
            if index >= dims.len() {
                return Err(EvalError::function_error("dimension out of range", span));
            }
            Ok(Some(Value::Integer(dims[index])))
        }
        _ => Ok(None),
    }
}

/// Evaluate a function call expression.
fn eval_fn_call(call: &Expression, ctx: &EvalContext, span: Span) -> Result<Value, EvalError> {
    let Expression::FunctionCall { name, .. } = call else {
        return Err(EvalError::InvalidSemanticIr {
            reason: "function-call evaluator received a non-call expression".to_string(),
            span,
        });
    };
    let target = resolve_context_function_occurrence(name, ctx, span)?;
    eval_user_function(target, call, ctx, span)
}

/// Evaluate a user-defined function.
fn eval_user_function(
    target: CheckedFunctionTarget<'_>,
    call_expr: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    // Impure/external refusal lives in eval_function_with_call_args, the one
    // entrance every call path shares.
    let plan = checked_call_plan(target, call_expr, span)?;
    let call = plan.evaluate_explicit(|_, expr| {
        if function_eval::is_partial_application(expr) {
            let partial_span = expr.span().ok_or_else(|| {
                EvalError::missing_source_context(
                    "function-valued partial application has no source provenance",
                )
            })?;
            return Err(EvalError::UnsupportedExpression {
                kind: "function-valued partial application".to_string(),
                span: partial_span,
            });
        }
        eval_expr_unchecked(expr, ctx, span)
    })?;
    function_eval::eval_function_with_checked_call(call, ctx, &EvalLimits::default(), 0, span)
}

fn named_function_call_arg(
    expr: &Expression,
    fallback_span: Span,
) -> Result<Option<(&str, &Expression)>, EvalError> {
    let Expression::FunctionCall {
        name,
        args,
        is_constructor,
        call_kind,
        ..
    } = expr
    else {
        return Ok(None);
    };
    match rumoca_core::classify_named_function_arg_marker(name, args, *is_constructor, *call_kind) {
        rumoca_core::NamedFunctionArgMarker::NotMarker => Ok(None),
        rumoca_core::NamedFunctionArgMarker::Valid { name, value } => Ok(Some((name, value))),
        rumoca_core::NamedFunctionArgMarker::Malformed => Err(EvalError::InvalidSemanticIr {
            reason: format!(
                "malformed named-argument marker `{}`: expected a constructor invocation with one value",
                name.as_str()
            ),
            span: expr.span().unwrap_or(fallback_span),
        }),
    }
}

/// Evaluate an if expression.
fn eval_flat_if(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let mut unknown_branch_values = Vec::new();
    let mut unknown_control_error = None;
    for (cond, then_expr) in branches {
        match eval_expr_unchecked(cond, ctx, span) {
            Ok(Value::Bool(true)) => {
                let selected = eval_expr_unchecked(then_expr, ctx, span);
                return select_known_branch_result(
                    selected,
                    &unknown_branch_values,
                    unknown_control_error.take(),
                    span,
                );
            }
            Ok(Value::Bool(false)) => {}
            Ok(value) => {
                return Err(EvalError::type_mismatch("Boolean", value.type_name(), span));
            }
            Err(error) if error.runtime_dependent_reason().is_some() => {
                match eval_expr_unchecked(then_expr, ctx, span) {
                    Ok(value) => {
                        unknown_branch_values.push(value);
                        unknown_control_error.get_or_insert(error);
                    }
                    Err(branch_error) if branch_error.is_control_independent_failure() => {
                        return Err(branch_error);
                    }
                    Err(_) => return Err(error),
                }
            }
            Err(error) => {
                if error.is_control_independent_failure() {
                    return Err(error);
                }
                if let Some(control_error) = unknown_control_error.take() {
                    return Err(control_error);
                }
                return Err(error);
            }
        }
    }
    let else_value = match eval_expr_unchecked(else_branch, ctx, span) {
        Ok(value) => value,
        Err(error) if error.is_control_independent_failure() => return Err(error),
        Err(error) => match unknown_control_error {
            Some(control_error) => return Err(control_error),
            None => return Err(error),
        },
    };
    require_equal_unknown_outcomes(&unknown_branch_values, else_value, span)
}

fn select_known_branch_result(
    selected: Result<Value, EvalError>,
    unknown_branch_values: &[Value],
    unknown_control_error: Option<EvalError>,
    span: Span,
) -> Result<Value, EvalError> {
    match selected {
        Ok(value) => require_equal_unknown_outcomes(unknown_branch_values, value, span),
        Err(error) if error.is_control_independent_failure() => Err(error),
        Err(error) => Err(unknown_control_error.unwrap_or(error)),
    }
}

fn require_equal_unknown_outcomes(
    unknown_branch_values: &[Value],
    outcome: Value,
    span: Span,
) -> Result<Value, EvalError> {
    if unknown_branch_values
        .iter()
        .all(|value| values_semantically_equal(value, &outcome))
    {
        Ok(outcome)
    } else {
        Err(EvalError::not_constant(
            "if-expression condition is not constant and branches differ",
            span,
        ))
    }
}

fn values_semantically_equal(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Real(lhs), Value::Real(rhs)) => lhs == rhs || lhs.to_bits() == rhs.to_bits(),
        (Value::Array(lhs), Value::Array(rhs)) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs)
                    .all(|(lhs, rhs)| values_semantically_equal(lhs, rhs))
        }
        (Value::Record(lhs), Value::Record(rhs)) => {
            lhs.len() == rhs.len()
                && lhs.iter().all(|(name, lhs)| {
                    rhs.get(name)
                        .is_some_and(|rhs| values_semantically_equal(lhs, rhs))
                })
        }
        _ => lhs == rhs,
    }
}

/// Evaluate an array expression.
fn eval_flat_array(
    elements: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let values: Vec<Value> = elements
        .iter()
        .map(|e| eval_expr_unchecked(e, ctx, span))
        .collect::<Result<_, _>>()?;
    Ok(Value::Array(values))
}

/// Evaluate an index expression.
fn eval_flat_index(
    base: &Expression,
    subscripts: &[Subscript],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let base_val = eval_expr_unchecked(base, ctx, span)?;
    apply_subscripts(&base_val, subscripts, ctx, span)
}

/// Evaluate field access on a record value.
fn eval_field_access(base_val: &Value, field: &str, span: Span) -> Result<Value, EvalError> {
    match base_val {
        Value::Record(fields) => {
            if let Some(value) = fields.get(field) {
                Ok(value.clone())
            } else {
                Err(EvalError::TypeMismatch {
                    expected: format!("record with field '{}'", field),
                    actual: format!("record without field '{}'", field),
                    span,
                })
            }
        }
        _ => Err(EvalError::TypeMismatch {
            expected: "record".to_string(),
            actual: format!("{:?}", base_val),
            span,
        }),
    }
}

/// Convert a literal to a value.
fn eval_literal(lit: &Literal) -> Value {
    match lit {
        Literal::Real(v) => Value::Real(*v),
        Literal::Integer(v) => Value::Integer(*v),
        Literal::Boolean(v) => Value::Bool(*v),
        Literal::String(s) => Value::String(s.clone()),
    }
}

/// Apply subscripts to a value.
fn apply_subscripts(
    value: &Value,
    subscripts: &[Subscript],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    materialize_value_selection(value, subscripts, span, |subscript, len| {
        eval_read_selection(subscript, len, ctx, span)
    })
}

fn eval_read_selection(
    subscript: &Subscript,
    len: usize,
    ctx: &EvalContext,
    span: Span,
) -> Result<ValueSelection, EvalError> {
    match subscript {
        Subscript::Index { value, .. } => Ok(ValueSelection::Element(*value)),
        Subscript::Colon { .. } => {
            if len >= DEFAULT_EVAL_BUDGET {
                return Err(EvalError::UnsupportedExpression {
                    kind: "whole-dimension selection is beyond the constant-evaluation node budget"
                        .to_string(),
                    span,
                });
            }
            let end = i64::try_from(len).map_err(|_| EvalError::Internal {
                message: "array extent exceeds i64 during subscript evaluation".to_string(),
            })?;
            Ok(ValueSelection::Slice((1..=end).collect()))
        }
        Subscript::Expr { expr, .. } => {
            let value = eval_expr_unchecked(expr, ctx, span)?;
            read_selection_from_value(value, span)
        }
    }
}

fn read_selection_from_value(value: Value, span: Span) -> Result<ValueSelection, EvalError> {
    match value {
        Value::Array(indices) => {
            if indices.len() >= DEFAULT_EVAL_BUDGET {
                return Err(EvalError::UnsupportedExpression {
                    kind: "Integer-vector selection is beyond the constant-evaluation node budget"
                        .to_string(),
                    span,
                });
            }
            indices
                .iter()
                .map(|index| integer_subscript_index(index, span))
                .collect::<Result<Vec<_>, _>>()
                .map(ValueSelection::Slice)
        }
        other => integer_subscript_index(&other, span).map(ValueSelection::Element),
    }
}

/// Evaluate an expression when an unresolved runtime value is an expected outcome.
///
/// Only failures explicitly classified as runtime-dependent become `None`.
/// Model errors and invalid semantic IR remain typed errors so opportunistic
/// constant folding cannot silently discard them.
pub fn eval_optional(expr: &Expression, ctx: &EvalContext) -> Result<Option<Value>, EvalError> {
    match eval_expr(expr, ctx) {
        Ok(value) => Ok(Some(value)),
        Err(error) if error.runtime_dependent_reason().is_some() => Ok(None),
        Err(error) => Err(error),
    }
}

/// Try to evaluate an expression to an integer.
///
/// `None` means only that the value is legitimately unavailable at translation
/// time. Evaluation and result-type failures remain typed errors.
pub fn try_eval_integer(expr: &Expression, ctx: &EvalContext) -> Result<Option<i64>, EvalError> {
    optional_typed_value(expr, ctx, "Integer", Value::as_integer)
}

/// Try to evaluate an expression to a real.
///
/// `None` means only that the value is legitimately unavailable at translation
/// time. Evaluation and result-type failures remain typed errors.
pub fn try_eval_real(expr: &Expression, ctx: &EvalContext) -> Result<Option<f64>, EvalError> {
    optional_typed_value(expr, ctx, "Real", Value::to_real)
}

/// Try to evaluate an expression to a boolean.
///
/// `None` means only that the value is legitimately unavailable at translation
/// time. Evaluation and result-type failures remain typed errors.
pub fn try_eval_bool(expr: &Expression, ctx: &EvalContext) -> Result<Option<bool>, EvalError> {
    optional_typed_value(expr, ctx, "Boolean", Value::as_bool)
}

fn optional_typed_value<T>(
    expr: &Expression,
    ctx: &EvalContext,
    expected: &'static str,
    project: impl FnOnce(&Value) -> Option<T>,
) -> Result<Option<T>, EvalError> {
    let Some(value) = eval_optional(expr, ctx)? else {
        return Ok(None);
    };
    let Some(projected) = project(&value) else {
        let span = expr.span().ok_or_else(|| {
            EvalError::missing_source_context("typed constant expression has no source provenance")
        })?;
        return Err(EvalError::type_mismatch(expected, value.type_name(), span));
    };
    Ok(Some(projected))
}
