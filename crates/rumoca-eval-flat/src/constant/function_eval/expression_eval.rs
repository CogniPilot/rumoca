use super::*;
use crate::constant::builtins::eval_builtin_in_context;

/// Evaluate an expression in function context (has access to locals).
pub(super) fn eval_expr_in_function(
    expr: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let eval_state = EvalState {
        span: expr.span().unwrap_or(eval.span),
        ..*eval
    };
    let eval = &eval_state;
    match expr {
        Expression::Empty { .. } => Err(EvalError::InvalidSemanticIr {
            reason: "empty expression reached user-function evaluation".to_string(),
            span: eval.span,
        }),
        Expression::Literal { value: literal, .. } => eval_literal(literal),
        Expression::VarRef {
            name, subscripts, ..
        } => eval_var_ref(name, subscripts, env, eval),
        Expression::Binary { op, lhs, rhs, .. } => eval_binary(op, lhs, rhs, env, eval),
        Expression::Unary { op, rhs, .. } => eval_unary(op, rhs, env, eval),
        Expression::FunctionCall { .. } if is_partial_application(expr) => {
            Err(EvalError::UnsupportedExpression {
                kind: "function-valued partial application".to_string(),
                span: eval.span,
            })
        }
        Expression::FunctionCall { .. } => eval_fn_call_expr(expr, env, eval),
        Expression::StringConversion { .. } => Err(EvalError::UnsupportedExpression {
            kind: "predefined String conversion".to_string(),
            span: eval.span,
        }),
        Expression::BuiltinCall { function, args, .. } => {
            let arg_values: Vec<Value> = args
                .iter()
                .map(|arg| eval_expr_in_function(arg, env, eval))
                .collect::<Result<_, _>>()?;
            eval_builtin_in_context(function.name(), &arg_values, eval.ctx, eval.span)
        }
        Expression::Array { elements, .. } => eval_array_expr(elements, env, eval),
        Expression::Range {
            start, step, end, ..
        } => eval_range_expr_inline(start, step, end, env, eval),
        Expression::If {
            branches,
            else_branch,
            ..
        } => eval_if_expr(branches, else_branch, env, eval),
        Expression::Index {
            base, subscripts, ..
        } => eval_array_index(base, subscripts, env, eval),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => eval_array_comprehension(expr, indices, filter, env, eval),
        Expression::Tuple { elements, .. } => eval_array_expr(elements, env, eval),
        Expression::FieldAccess {
            base,
            field,
            field_def_id,
            ..
        } => {
            let base_val = eval_expr_in_function(base, env, eval)?;
            if is_exact_single_record_output(base, *field_def_id, eval.ctx, eval.span)? {
                return Ok(base_val);
            }
            let record = base_val.as_record().ok_or_else(|| {
                EvalError::type_mismatch("Record", base_val.type_name(), eval.span)
            })?;
            record
                .get(field)
                .cloned()
                .ok_or_else(|| EvalError::unknown_variable(field, eval.span))
        }
    }
}

/// Whether one Flat field node is the exact output selector around a call that
/// returns one record value.
///
/// Flat keeps function results as output-owned values for DAE lowering, so a
/// source `f(...).member` may arrive as `f(...).result.member`. The constant
/// evaluator returns a single output directly. The selector may therefore be
/// erased only when the resolved call target and the selected output's `DefId`
/// agree and that output is declared as a record; rendered names prove none of
/// those facts.
pub(in crate::constant) fn is_exact_single_record_output(
    base: &Expression,
    field: rumoca_core::DefId,
    ctx: &EvalContext,
    span: rumoca_core::Span,
) -> Result<bool, EvalError> {
    let Expression::FunctionCall { name, .. } = base else {
        return Ok(false);
    };
    let target = crate::constant::resolve_context_function_occurrence(name, ctx, span)?;
    let function = target.function();
    Ok(matches!(function.outputs.as_slice(), [output]
            if output.def_id == Some(field)
                && output.type_class == Some(rumoca_core::ClassType::Record)))
}

/// Evaluate a literal expression.
fn eval_literal(literal: &Literal) -> Result<Value, EvalError> {
    Ok(match literal {
        Literal::Real(v) => Value::Real(*v),
        Literal::Integer(v) => Value::Integer(*v),
        Literal::Boolean(v) => Value::Bool(*v),
        Literal::String(v) => Value::String(v.clone()),
    })
}

/// Evaluate a variable reference.
pub(super) fn eval_var_ref(
    reference: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let name = reference.as_str();
    let is_direct = reference.segments().len() == 1;
    if is_direct && let Some(val) = env.get_reference(reference) {
        return apply_subscripts_flat(val.clone(), subscripts, env, eval);
    }
    if is_direct && env.is_declared_reference(reference) {
        return Err(EvalError::UnsupportedExpression {
            kind: format!("function component `{name}` is read before initialization"),
            span: eval.span,
        });
    }
    // MLS 3.6 §12.2: a record component's fields are read through the joined
    // reference Flat renders, so `z.im` names the field `im` of the bound local
    // `z`. Resolving it here is also what keeps the enumeration fallback below
    // honest — a reference whose head segment is a component in scope is never
    // an enumeration literal, and guessing one folded `z.im` to the enumeration
    // value `z.im` instead of the record field.
    if let Some(value) = read_bound_field_path(reference, env, eval)? {
        return apply_subscripts_flat(value, subscripts, env, eval);
    }
    let declared_head = reference.segments().first().is_some_and(|segment| {
        if reference.root_def_id().is_some() {
            env.selected_declaration_name(reference).is_some()
        } else {
            env.is_declared(segment.as_ref())
        }
    });
    if declared_head {
        return Err(EvalError::UnsupportedExpression {
            kind: format!("function component `{name}` is read before initialization"),
            span: eval.span,
        });
    }
    eval.ctx.validate_resolved_reference(reference, eval.span)?;
    if let Some(val) = eval.ctx.get_reference(reference) {
        return Ok(val.clone());
    }
    if let Some(value) = eval.ctx.get_enum_reference(reference) {
        return Ok(Value::ResolvedEnum(value.clone()));
    }
    // Enumeration literals are registered in `EvalContext` after resolution.
    // A dotted spelling alone proves neither that the reference is an enum nor
    // which enum declaration owns it. Guessing here turned unresolved package
    // constants into plausible enumeration values inside user functions.
    Err(EvalError::unknown_variable(name, eval.span))
}

/// Read a nested reference as a field path into a component bound in `env`.
///
/// The segmentation is [`rumoca_core::Reference::segments`], the reference's own
/// top-level split, so a dot inside an index expression stays inside its
/// segment.
///
/// `Ok(None)` means the reference does not start at a bound component, so the
/// caller may still read it as something else. Once the head *is* bound the
/// reference is settled here, and a segment this evaluator cannot follow —
/// a subscripted field, or a value it holds as something other than a record —
/// is reported as a form it has no rule for. That refusal leaves the value for
/// the runtime; it never lets the enumeration fallback below invent one.
fn read_bound_field_path(
    reference: &rumoca_core::Reference,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Option<Value>, EvalError> {
    let segments = reference.segments();
    let Some((head, fields)) = segments.split_first() else {
        return Ok(None);
    };
    if fields.is_empty() {
        return Ok(None);
    }
    let selected = if env.iterator_binding(head, reference).is_some() {
        *head
    } else {
        let Some(selected) = env.selected_declaration_name(reference) else {
            return Ok(None);
        };
        selected
    };
    let Some(root) = env.get(selected) else {
        return Ok(None);
    };
    let mut current = root.clone();
    for field in fields {
        current = current
            .as_record()
            .and_then(|record| record.get(*field))
            .cloned()
            .ok_or_else(|| EvalError::UnsupportedExpression {
                kind: format!(
                    "field `{field}` of a bound {} value",
                    current.type_name().to_lowercase()
                ),
                span: eval.span,
            })?;
    }
    Ok(Some(current))
}

/// Evaluate a binary expression.
fn eval_binary(
    op: &rumoca_core::OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let lhs_val = eval_expr_in_function(lhs, env, eval)?;
    let rhs_val = eval_expr_in_function(rhs, env, eval)?;
    super::super::operators::eval_binary_op(op, &lhs_val, &rhs_val, eval.span)
}

/// Evaluate a unary expression.
fn eval_unary(
    op: &rumoca_core::OpUnary,
    rhs: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let rhs_val = eval_expr_in_function(rhs, env, eval)?;
    super::super::operators::eval_unary_op(op, &rhs_val, eval.span)
}

/// Evaluate a function call expression.
fn eval_fn_call_expr(
    call: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let Expression::FunctionCall { name, .. } = call else {
        return Err(EvalError::InvalidSemanticIr {
            reason: "function-call evaluator received a non-call expression".to_string(),
            span: eval.span,
        });
    };
    let target = crate::constant::resolve_context_function_occurrence(name, eval.ctx, eval.span)?;
    let plan = crate::constant::checked_call_plan(target, call, eval.span)?;
    let call = plan.evaluate_explicit(|_, argument| {
        if is_partial_application(argument) {
            let partial_span = argument.span().ok_or_else(|| {
                EvalError::missing_source_context(
                    "function-valued call argument has no source provenance",
                )
            })?;
            return Err(EvalError::UnsupportedExpression {
                kind: "function-valued partial application".to_string(),
                span: partial_span,
            });
        }
        eval_expr_in_function(argument, env, eval)
    })?;
    eval_function_with_checked_call(call, eval.ctx, eval.limits, eval.depth + 1, eval.span)
}

/// Whether Flat explicitly identifies this expression as a function value.
///
/// This evaluator has no function-value representation, so it must defer the
/// form instead of invoking it and rejecting its intentionally unbound formal.
pub(in crate::constant) fn is_partial_application(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::FunctionCall {
            call_kind: rumoca_core::FunctionCallKind::PartialApplication,
            ..
        }
    )
}

/// Evaluate an array expression.
fn eval_array_expr(
    elements: &[Expression],
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let values: Vec<Value> = elements
        .iter()
        .map(|e| eval_expr_in_function(e, env, eval))
        .collect::<Result<_, _>>()?;
    Ok(Value::Array(values))
}

/// Evaluate a range expression inline.
fn eval_range_expr_inline(
    start: &Expression,
    step: &Option<Box<Expression>>,
    end: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let start_val = eval_expr_in_function(start, env, eval)?;
    let end_val = eval_expr_in_function(end, env, eval)?;
    let step_val = step
        .as_ref()
        .map(|s| eval_expr_in_function(s, env, eval))
        .transpose()?;
    super::super::range_eval::eval_value_range(&start_val, step_val.as_ref(), &end_val, eval.span)
}

/// Evaluate an if expression.
fn eval_if_expr(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    for (cond, then_expr) in branches {
        let cond_val = eval_expr_in_function(cond, env, eval)?;
        let is_true = cond_val
            .as_bool()
            .ok_or_else(|| EvalError::type_mismatch("Boolean", cond_val.type_name(), eval.span))?;
        if is_true {
            return eval_expr_in_function(then_expr, env, eval);
        }
    }
    eval_expr_in_function(else_branch, env, eval)
}

/// Evaluate an array index expression.
fn eval_array_index(
    base: &Expression,
    subscripts: &[Subscript],
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let base_val = eval_expr_in_function(base, env, eval)?;
    apply_subscripts_flat(base_val, subscripts, env, eval)
}

/// Evaluate an array comprehension: `{expr for i in range if filter}`.
fn eval_array_comprehension(
    expr: &Expression,
    indices: &[ComprehensionIndex],
    filter: &Option<Box<Expression>>,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    if indices.is_empty() {
        return Err(EvalError::InvalidSemanticIr {
            reason: "array comprehension has no index declarations".to_string(),
            span: eval.span,
        });
    }

    // Comprehension indices are lexical bindings too. Evaluate against a
    // private environment so they cannot leak into the surrounding function.
    let mut local_env = env.clone();
    let mut budget = ComprehensionBudget::new(eval.limits.max_iterations);

    match eval_comprehension_recursive(expr, indices, filter, &mut local_env, eval, &mut budget)? {
        ComprehensionEvaluation::Value(value) => Ok(value),
        ComprehensionEvaluation::FilteredOut => Err(EvalError::Internal {
            message: "non-empty comprehension lost its enclosing index level".to_string(),
        }),
    }
}

enum ComprehensionEvaluation {
    Value(Value),
    FilteredOut,
}

struct ComprehensionBudget {
    work_remaining: usize,
    nodes_remaining: usize,
}

impl ComprehensionBudget {
    fn new(limit: usize) -> Self {
        Self {
            work_remaining: limit,
            nodes_remaining: limit,
        }
    }

    fn spend_work(&mut self, span: Span) -> Result<(), EvalError> {
        self.work_remaining =
            self.work_remaining
                .checked_sub(1)
                .ok_or_else(|| EvalError::UnsupportedExpression {
                    kind: "array comprehension exceeds the constant-evaluation work budget"
                        .to_string(),
                    span,
                })?;
        Ok(())
    }

    fn retain_nodes(&mut self, nodes: Option<usize>, span: Span) -> Result<(), EvalError> {
        let nodes = nodes.ok_or_else(|| EvalError::UnsupportedExpression {
            kind: "array comprehension retained-node count exceeds the host index range"
                .to_string(),
            span,
        })?;
        self.nodes_remaining = self.nodes_remaining.checked_sub(nodes).ok_or_else(|| {
            EvalError::UnsupportedExpression {
                kind: "array comprehension exceeds the constant-evaluation retained-node budget"
                    .to_string(),
                span,
            }
        })?;
        Ok(())
    }
}

/// Recursively evaluate nested comprehension indices.
fn eval_comprehension_recursive(
    expr: &Expression,
    indices: &[ComprehensionIndex],
    filter: &Option<Box<Expression>>,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
    budget: &mut ComprehensionBudget,
) -> Result<ComprehensionEvaluation, EvalError> {
    if indices.is_empty() {
        // Base case: all indices bound, check filter and evaluate expression
        if let Some(filter_expr) = filter {
            let filter_val = eval_expr_in_function(filter_expr, env, eval)?;
            let passes = filter_val.as_bool().ok_or_else(|| {
                EvalError::type_mismatch("Boolean", filter_val.type_name(), eval.span)
            })?;
            if !passes {
                return Ok(ComprehensionEvaluation::FilteredOut);
            }
        }
        let value = eval_expr_in_function(expr, env, eval)?;
        budget.retain_nodes(materialized_node_count(&value), eval.span)?;
        return Ok(ComprehensionEvaluation::Value(value));
    }

    let index = &indices[0];
    let var_name = index.name.to_string();
    let range_values = eval_range_expr(&index.range, env, eval)?;
    let remaining = &indices[1..];

    budget.retain_nodes(Some(1), eval.span)?;
    let mut results = Vec::new();

    for val in range_values {
        budget.spend_work(eval.span)?;
        let binding_depth = env.loop_bindings.len();
        env.loop_bindings.push((var_name.clone(), val));

        let result = eval_comprehension_recursive(expr, remaining, filter, env, eval, budget);
        env.loop_bindings.truncate(binding_depth);
        let result = result?;

        match result {
            ComprehensionEvaluation::Value(value) => results.push(value),
            ComprehensionEvaluation::FilteredOut => {}
        }
    }

    Ok(ComprehensionEvaluation::Value(Value::Array(results)))
}

/// Evaluate a range expression to a vector of values.
pub(super) fn eval_range_expr(
    expr: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Vec<Value>, EvalError> {
    match expr {
        Expression::Range {
            start, step, end, ..
        } => {
            let start_val = eval_expr_in_function(start, env, eval)?;
            let end_val = eval_expr_in_function(end, env, eval)?;
            let step_val = step
                .as_ref()
                .map(|s| eval_expr_in_function(s, env, eval))
                .transpose()?;
            let result = super::super::range_eval::eval_value_range(
                &start_val,
                step_val.as_ref(),
                &end_val,
                eval.span,
            )?;
            match result {
                Value::Array(arr) => Ok(arr),
                _ => Err(EvalError::type_mismatch(
                    "Array",
                    result.type_name(),
                    eval.span,
                )),
            }
        }
        _ => {
            let val = eval_expr_in_function(expr, env, eval)?;
            match val {
                Value::Array(arr) => Ok(arr),
                _ => Err(EvalError::type_mismatch(
                    "Array or Range",
                    val.type_name(),
                    eval.span,
                )),
            }
        }
    }
}

/// Convert a ComponentReference to a simple name string.
pub(super) fn component_ref_to_name(cr: &ComponentReference) -> String {
    cr.parts()
        .iter()
        .map(|p| p.ident.to_string())
        .collect::<Vec<_>>()
        .join(".")
}
