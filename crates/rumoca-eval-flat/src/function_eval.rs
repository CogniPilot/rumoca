//! User-defined function evaluation for compile-time constant expressions.
//!
//! This module interprets Modelica function algorithm bodies to evaluate
//! user-defined functions with constant arguments at compile time.
//!
//! Implements compile-time constant folding of user-defined functions per MLS:
//! - Pure functions with constant arguments → constant expression (MLS §12.3)
//! - Statement-by-statement interpretation with environment tracking (MLS §12.4)
//! - For loop semantics per MLS §11.2.2.2
//! - While loop semantics per MLS §11.2.2.3
//! - If statement semantics per MLS §11.2.2.1
//! - Assignment semantics per MLS §11.2.1
//! - Recursion and iteration limits for safety

use indexmap::IndexMap;
use rumoca_core::Span;
use rumoca_ir_flat::{
    ComponentReference, ComprehensionIndex, Expression, ForIndex, Function, Literal, Statement,
    StatementBlock, Subscript,
};

use crate::EvalContext;
use crate::errors::EvalError;
use crate::value::Value;

/// Execution limits for function evaluation.
#[derive(Debug, Clone)]
pub struct EvalLimits {
    /// Maximum recursion depth for nested function calls.
    pub recursion_depth: usize,
    /// Maximum total iterations across all loops.
    pub max_iterations: usize,
}

impl Default for EvalLimits {
    fn default() -> Self {
        Self {
            recursion_depth: 256,
            max_iterations: 100_000,
        }
    }
}

/// Evaluation state bundling common parameters to reduce argument count.
pub struct EvalState<'a> {
    pub ctx: &'a EvalContext,
    pub limits: &'a EvalLimits,
    pub depth: usize,
    pub span: Span,
}

/// Control flow during statement execution.
#[derive(Debug, Clone, Copy, PartialEq)]
enum FlowControl {
    /// Normal statement execution, continue to next statement.
    Continue,
    /// Break from current loop.
    Break,
    /// Return from function (output values are ready).
    Return,
}

/// Function execution environment with mutable variable bindings.
struct FunctionEnv {
    /// Input parameters (bound from arguments).
    inputs: IndexMap<String, Value>,
    /// Output variables (assigned by function body).
    outputs: IndexMap<String, Value>,
    /// Local/protected variables.
    locals: IndexMap<String, Value>,
}

impl FunctionEnv {
    /// Create a new environment with parameters bound from arguments.
    fn new(func: &Function, args: Vec<Value>, span: Span) -> Result<Self, EvalError> {
        let inputs = Self::bind_inputs(func, args, span)?;
        let outputs = Self::init_params(&func.outputs);
        let locals = Self::init_params(&func.locals);
        Ok(Self {
            inputs,
            outputs,
            locals,
        })
    }

    /// Bind input arguments to parameters.
    fn bind_inputs(
        func: &Function,
        args: Vec<Value>,
        span: Span,
    ) -> Result<IndexMap<String, Value>, EvalError> {
        let mut inputs = IndexMap::new();
        if args.len() >= func.inputs.len() {
            for (param, arg) in func.inputs.iter().zip(args) {
                inputs.insert(param.name.clone(), arg);
            }
            return Ok(inputs);
        }
        // Handle case where we have fewer args than inputs
        for (i, param) in func.inputs.iter().enumerate() {
            let value = Self::get_input_value(func, &args, i, param, span)?;
            inputs.insert(param.name.clone(), value);
        }
        Ok(inputs)
    }

    /// Get input value for a parameter at index i.
    fn get_input_value(
        func: &Function,
        args: &[Value],
        i: usize,
        param: &rumoca_ir_flat::FunctionParam,
        span: Span,
    ) -> Result<Value, EvalError> {
        if i < args.len() {
            return Ok(args[i].clone());
        }
        if param.default.is_some() {
            return Err(EvalError::function_error(
                format!(
                    "missing argument {} for function {} (defaults not yet supported)",
                    param.name, func.name
                ),
                span,
            ));
        }
        Err(EvalError::function_error(
            format!(
                "missing required argument {} for function {}",
                param.name, func.name
            ),
            span,
        ))
    }

    /// Initialize parameters with default values.
    fn init_params(params: &[rumoca_ir_flat::FunctionParam]) -> IndexMap<String, Value> {
        params
            .iter()
            .map(|p| (p.name.clone(), type_default_value(&p.type_name, &p.dims)))
            .collect()
    }

    /// Look up a variable by name (checks inputs, outputs, locals).
    fn get(&self, name: &str) -> Option<&Value> {
        self.inputs
            .get(name)
            .or_else(|| self.outputs.get(name))
            .or_else(|| self.locals.get(name))
    }

    /// Set a variable value (must be output or local, not input).
    fn set(&mut self, name: &str, value: Value) -> bool {
        if self.outputs.contains_key(name) {
            self.outputs.insert(name.to_string(), value);
            true
        } else if self.locals.contains_key(name) {
            self.locals.insert(name.to_string(), value);
            true
        } else {
            false
        }
    }

    /// Get the return value (single output or tuple of outputs).
    fn return_value(&self) -> Value {
        if self.outputs.len() == 1 {
            self.outputs
                .values()
                .next()
                .cloned()
                .unwrap_or(Value::Integer(0))
        } else {
            // Multiple outputs -> tuple/array
            Value::Array(self.outputs.values().cloned().collect())
        }
    }
}

/// Create a default value for a given type.
fn type_default_value(type_name: &str, dims: &[i64]) -> Value {
    let scalar = match type_name {
        "Real" => Value::Real(0.0),
        "Integer" => Value::Integer(0),
        "Boolean" => Value::Bool(false),
        "String" => Value::String(String::new()),
        _ => Value::Real(0.0), // Default to Real for unknown types
    };

    if dims.is_empty() {
        scalar
    } else {
        // Create nested arrays for each dimension
        create_array_value(&scalar, dims)
    }
}

/// Create a multi-dimensional array filled with a default value.
fn create_array_value(default: &Value, dims: &[i64]) -> Value {
    if dims.is_empty() {
        default.clone()
    } else {
        let size = dims[0] as usize;
        let inner = create_array_value(default, &dims[1..]);
        Value::Array(vec![inner; size])
    }
}

/// Evaluate a user-defined function with constant arguments.
///
/// Returns the function result or an error if evaluation fails.
pub fn eval_function(
    func: &Function,
    args: Vec<Value>,
    ctx: &EvalContext,
    limits: &EvalLimits,
    depth: usize,
    span: Span,
) -> Result<Value, EvalError> {
    // Check recursion limit
    if depth > limits.recursion_depth {
        return Err(EvalError::function_error(
            format!(
                "recursion depth exceeded ({}) in function {}",
                limits.recursion_depth, func.name
            ),
            span,
        ));
    }

    // Create function environment and evaluation state
    let mut env = FunctionEnv::new(func, args, span)?;
    let mut iteration_count = 0;
    let eval = EvalState {
        ctx,
        limits,
        depth,
        span,
    };
    let mut state = StmtState {
        env: &mut env,
        iteration_count: &mut iteration_count,
    };

    // Execute function body (algorithm statements)
    for stmt in func.body.iter() {
        let flow = eval_statement(stmt, &mut state, &eval)?;
        match flow {
            FlowControl::Return => break,
            FlowControl::Break => {
                return Err(EvalError::function_error(
                    "break outside of loop".to_string(),
                    span,
                ));
            }
            FlowControl::Continue => {}
        }
    }

    let result = env.return_value();
    Ok(result)
}

/// Mutable state during statement evaluation.
struct StmtState<'a> {
    env: &'a mut FunctionEnv,
    iteration_count: &'a mut usize,
}

/// Evaluate a statement, returning control flow indication.
fn eval_statement(
    stmt: &Statement,
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    match stmt {
        Statement::Empty => Ok(FlowControl::Continue),
        Statement::Assignment { comp, value } => eval_assignment(comp, value, state.env, eval),
        Statement::Return => Ok(FlowControl::Return),
        Statement::Break => Ok(FlowControl::Break),
        Statement::If {
            cond_blocks,
            else_block,
        } => eval_if_statement(cond_blocks, else_block, state, eval),
        Statement::For { indices, equations } => {
            eval_for_statement(indices, equations, state, eval)
        }
        Statement::While(block) => eval_while_statement(block, state, eval),
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => eval_fn_call_stmt(comp, args, outputs, state.env, eval),
        Statement::When(_) => Err(EvalError::not_constant(
            "when statement in function",
            eval.span,
        )),
        Statement::Reinit { .. } | Statement::Assert { .. } => Ok(FlowControl::Continue),
    }
}

/// Evaluate an assignment statement.
fn eval_assignment(
    comp: &ComponentReference,
    value: &Expression,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    let val = eval_expr_in_function(value, env, eval)?;
    let name = component_ref_to_name(comp);
    if env.set(&name, val.clone()) {
        return Ok(FlowControl::Continue);
    }
    // Check if it's an array slice assignment (e.g., x[1:n] := ...)
    if let Some((base_name, subscripts)) = parse_subscripted_assignment(comp) {
        if has_range_subscript(&subscripts) {
            assign_array_slice(env, &base_name, &subscripts, val, eval)?;
            return Ok(FlowControl::Continue);
        }
        // Fall through to element assignment for non-range subscripts
        let indices: Vec<Expression> = subscripts
            .into_iter()
            .filter_map(|s| {
                if let Subscript::Expr(e) = s {
                    Some(*e)
                } else {
                    None
                }
            })
            .collect();
        if !indices.is_empty() {
            assign_array_element(env, &base_name, &indices, val, eval)?;
            return Ok(FlowControl::Continue);
        }
    }
    Err(EvalError::function_error(
        format!("cannot assign to variable: {}", name),
        eval.span,
    ))
}

/// Evaluate an if statement.
fn eval_if_statement(
    cond_blocks: &[StatementBlock],
    else_block: &Option<Vec<Statement>>,
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    for block in cond_blocks {
        let cond_val = eval_expr_in_function(&block.cond, state.env, eval)?;
        let is_true = cond_val
            .as_bool()
            .ok_or_else(|| EvalError::type_mismatch("Boolean", cond_val.type_name(), eval.span))?;
        if is_true {
            return eval_stmt_list(&block.stmts, state, eval);
        }
    }
    // All conditions false, execute else branch
    if let Some(else_stmts) = else_block {
        return eval_stmt_list(else_stmts, state, eval);
    }
    Ok(FlowControl::Continue)
}

/// Evaluate a list of statements.
fn eval_stmt_list(
    stmts: &[Statement],
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    for stmt in stmts {
        let flow = eval_statement(stmt, state, eval)?;
        if flow != FlowControl::Continue {
            return Ok(flow);
        }
    }
    Ok(FlowControl::Continue)
}

/// Evaluate a function call statement.
fn eval_fn_call_stmt(
    comp: &ComponentReference,
    args: &[Expression],
    outputs: &[Expression],
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    let func_name = component_ref_to_name(comp);

    // Skip special built-in statements that appear as function calls
    // These are runtime-only operations that should be no-ops during constant evaluation
    match func_name.as_str() {
        "assert" | "print" | "terminate" | "Modelica.Utilities.Streams.print" => {
            return Ok(FlowControl::Continue);
        }
        _ => {}
    }

    let arg_values: Vec<Value> = args
        .iter()
        .map(|a| eval_expr_in_function(a, env, eval))
        .collect::<Result<_, _>>()?;
    let result = call_function(&func_name, arg_values, eval)?;
    if !outputs.is_empty() {
        assign_fn_outputs(outputs, result, env, eval)?;
    }
    Ok(FlowControl::Continue)
}

/// Assign function outputs to variables.
fn assign_fn_outputs(
    outputs: &[Expression],
    result: Value,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    match &result {
        Value::Array(arr) if arr.len() == outputs.len() => {
            assign_multiple_outputs(outputs, arr, env, eval)
        }
        _ if outputs.len() == 1 => assign_single_output(&outputs[0], result, env, eval),
        _ => Err(EvalError::function_error(
            format!(
                "function output count mismatch: expected {}, got {:?}",
                outputs.len(),
                result
            ),
            eval.span,
        )),
    }
}

/// Assign multiple outputs from array result.
fn assign_multiple_outputs(
    outputs: &[Expression],
    arr: &[Value],
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    for (output_expr, val) in outputs.iter().zip(arr.iter()) {
        if let Expression::VarRef { name, subscripts } = output_expr {
            if !subscripts.is_empty() {
                continue;
            }
            let name = name.as_str().to_string();
            if !env.set(&name, val.clone()) {
                return Err(EvalError::function_error(
                    format!("cannot assign to output variable: {}", name),
                    eval.span,
                ));
            }
        }
    }
    Ok(())
}

/// Assign single output from result.
fn assign_single_output(
    output: &Expression,
    result: Value,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    if let Expression::VarRef { name, subscripts } = output {
        if !subscripts.is_empty() {
            return Ok(());
        }
        let name = name.as_str().to_string();
        if !env.set(&name, result) {
            return Err(EvalError::function_error(
                format!("cannot assign to output variable: {}", name),
                eval.span,
            ));
        }
    }
    Ok(())
}

/// Evaluate a for loop.
fn eval_for_statement(
    indices: &[ForIndex],
    body: &[Statement],
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    if indices.is_empty() {
        return Ok(FlowControl::Continue);
    }
    let index = &indices[0];
    let var_name = index.ident.to_string();
    let range_values = eval_range_expr(&index.range, state.env, eval)?;
    let original = state.env.locals.get(&var_name).cloned();

    let result = eval_for_loop_body(
        &var_name,
        &range_values,
        indices,
        body,
        state,
        eval,
        &original,
    );

    // Restore original value
    restore_loop_var(state.env, &var_name, original);
    result
}

/// Execute the for loop iterations.
fn eval_for_loop_body(
    var_name: &str,
    range_values: &[Value],
    indices: &[ForIndex],
    body: &[Statement],
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
    original: &Option<Value>,
) -> Result<FlowControl, EvalError> {
    for val in range_values {
        check_iteration_limit(state.iteration_count, eval)?;
        state.env.locals.insert(var_name.to_string(), val.clone());

        let flow = if indices.len() > 1 {
            eval_for_statement(&indices[1..], body, state, eval)?
        } else {
            eval_stmt_list(body, state, eval)?
        };

        match flow {
            FlowControl::Break => break,
            FlowControl::Return => {
                restore_loop_var(state.env, var_name, original.clone());
                return Ok(FlowControl::Return);
            }
            FlowControl::Continue => {}
        }
    }
    Ok(FlowControl::Continue)
}

/// Check iteration limit and increment counter.
fn check_iteration_limit(
    iteration_count: &mut usize,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    *iteration_count += 1;
    if *iteration_count > eval.limits.max_iterations {
        return Err(EvalError::function_error(
            format!(
                "iteration limit exceeded ({}) in function",
                eval.limits.max_iterations
            ),
            eval.span,
        ));
    }
    Ok(())
}

/// Restore loop variable to original value or remove it.
fn restore_loop_var(env: &mut FunctionEnv, var_name: &str, original: Option<Value>) {
    if let Some(orig) = original {
        env.locals.insert(var_name.to_string(), orig);
    } else {
        env.locals.swap_remove(var_name);
    }
}

/// Evaluate a while loop.
fn eval_while_statement(
    block: &StatementBlock,
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    loop {
        check_iteration_limit(state.iteration_count, eval)?;

        let cond_val = eval_expr_in_function(&block.cond, state.env, eval)?;
        let is_true = cond_val
            .as_bool()
            .ok_or_else(|| EvalError::type_mismatch("Boolean", cond_val.type_name(), eval.span))?;

        if !is_true {
            break;
        }

        let flow = eval_stmt_list(&block.stmts, state, eval)?;
        match flow {
            FlowControl::Break => return Ok(FlowControl::Continue),
            FlowControl::Return => return Ok(FlowControl::Return),
            FlowControl::Continue => {}
        }
    }
    Ok(FlowControl::Continue)
}

/// Evaluate an expression in function context (has access to locals).
fn eval_expr_in_function(
    expr: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    match expr {
        Expression::Empty => Ok(Value::Integer(0)),
        Expression::Literal(literal) => eval_literal(literal),
        Expression::VarRef { name, subscripts } => eval_var_ref(name, subscripts, env, eval),
        Expression::Binary { op, lhs, rhs } => eval_binary(op, lhs, rhs, env, eval),
        Expression::Unary { op, rhs } => eval_unary(op, rhs, env, eval),
        Expression::FunctionCall { name, args, .. } => eval_fn_call_expr(name, args, env, eval),
        Expression::BuiltinCall { function, args } => {
            let arg_values: Vec<Value> = args
                .iter()
                .map(|arg| eval_expr_in_function(arg, env, eval))
                .collect::<Result<_, _>>()?;
            crate::eval_builtin(function.name(), &arg_values, eval.span)
        }
        Expression::Array { elements, .. } => eval_array_expr(elements, env, eval),
        Expression::Range { start, step, end } => {
            eval_range_expr_inline(start, step, end, env, eval)
        }
        Expression::If {
            branches,
            else_branch,
        } => eval_if_expr(branches, else_branch, env, eval),
        Expression::Index { base, subscripts } => eval_array_index(base, subscripts, env, eval),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => eval_array_comprehension(expr, indices, filter, env, eval),
        Expression::Tuple { elements } => eval_array_expr(elements, env, eval),
        Expression::FieldAccess { base, field } => {
            let base_val = eval_expr_in_function(base, env, eval)?;
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
fn eval_var_ref(
    name: &rumoca_ir_flat::VarName,
    subscripts: &[rumoca_ir_flat::Subscript],
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let name = name.as_str();
    // Check function environment
    if let Some(val) = env.get(name) {
        return apply_subscripts_flat(val.clone(), subscripts, env, eval);
    }
    // Check global context
    if let Some(val) = eval.ctx.get(name) {
        return Ok(val.clone());
    }
    // Check for enum literals
    if let Some((type_name, literal)) = eval.ctx.get_enum(name) {
        return Ok(Value::Enum(type_name.clone(), literal.clone()));
    }
    // Try parsing as qualified enum
    if name.contains('.') {
        let parts: Vec<&str> = name.rsplitn(2, '.').collect();
        if parts.len() == 2 {
            return Ok(Value::Enum(parts[1].to_string(), parts[0].to_string()));
        }
    }
    Err(EvalError::unknown_variable(name, eval.span))
}

/// Evaluate a binary expression.
fn eval_binary(
    op: &rumoca_ir_flat::OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let lhs_val = eval_expr_in_function(lhs, env, eval)?;
    let rhs_val = eval_expr_in_function(rhs, env, eval)?;
    crate::eval_binary_op(op, &lhs_val, &rhs_val, eval.span)
}

/// Evaluate a unary expression.
fn eval_unary(
    op: &rumoca_ir_flat::OpUnary,
    rhs: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let rhs_val = eval_expr_in_function(rhs, env, eval)?;
    crate::eval_unary_op(op, &rhs_val, eval.span)
}

/// Evaluate a function call expression.
fn eval_fn_call_expr(
    name: &rumoca_ir_flat::VarName,
    args: &[Expression],
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let func_name = name.as_str();
    let arg_values: Vec<Value> = args
        .iter()
        .map(|a| eval_expr_in_function(a, env, eval))
        .collect::<Result<_, _>>()?;
    call_function(func_name, arg_values, eval)
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
    eval_range_values(&start_val, step_val.as_ref(), &end_val, eval.span)
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
        // No indices means just evaluate the expression directly
        return eval_expr_in_function(expr, env, eval);
    }

    // We need a mutable copy of locals to bind loop variables
    let mut local_env = FunctionEnv {
        inputs: env.inputs.clone(),
        outputs: env.outputs.clone(),
        locals: env.locals.clone(),
    };

    eval_comprehension_recursive(expr, indices, filter, &mut local_env, eval)
}

/// Recursively evaluate nested comprehension indices.
fn eval_comprehension_recursive(
    expr: &Expression,
    indices: &[ComprehensionIndex],
    filter: &Option<Box<Expression>>,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    if indices.is_empty() {
        // Base case: all indices bound, check filter and evaluate expression
        if let Some(filter_expr) = filter {
            let filter_val = eval_expr_in_function(filter_expr, env, eval)?;
            let passes = filter_val.as_bool().ok_or_else(|| {
                EvalError::type_mismatch("Boolean", filter_val.type_name(), eval.span)
            })?;
            if !passes {
                // Filter fails, return empty marker (will be filtered out)
                return Ok(Value::Array(vec![]));
            }
        }
        return eval_expr_in_function(expr, env, eval);
    }

    // Get current index and remaining indices
    let index = &indices[0];
    let var_name = index.name.to_string();
    let range_values = eval_range_expr(&index.range, env, eval)?;
    let remaining = &indices[1..];

    let mut results = Vec::new();

    // Save original value if it exists
    let original = env.locals.get(&var_name).cloned();

    for val in range_values {
        env.locals.insert(var_name.clone(), val);

        let result = eval_comprehension_recursive(expr, remaining, filter, env, eval)?;

        // Handle filtered results (empty arrays indicate filtered-out elements)
        match &result {
            Value::Array(arr) if arr.is_empty() && filter.is_some() && remaining.is_empty() => {
                // Element was filtered out, skip it
            }
            _ => {
                results.push(result);
            }
        }
    }

    // Restore original value
    if let Some(orig) = original {
        env.locals.insert(var_name, orig);
    } else {
        env.locals.swap_remove(&var_name);
    }

    Ok(Value::Array(results))
}

/// Call a function (builtin or user-defined).
fn call_function(name: &str, args: Vec<Value>, eval: &EvalState<'_>) -> Result<Value, EvalError> {
    if crate::is_builtin(name) {
        return crate::eval_builtin(name, &args, eval.span);
    }
    if let Some(func) = eval.ctx.functions.get(name) {
        if !func.pure {
            return Err(EvalError::not_constant(
                format!("impure function: {}", name),
                eval.span,
            ));
        }
        if func.external.is_some() {
            return Err(EvalError::not_constant(
                format!("external function: {}", name),
                eval.span,
            ));
        }
        return eval_function(func, args, eval.ctx, eval.limits, eval.depth + 1, eval.span);
    }
    Err(EvalError::not_constant(
        format!("unknown function: {}", name),
        eval.span,
    ))
}

/// Evaluate a range expression to a vector of values.
fn eval_range_expr(
    expr: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Vec<Value>, EvalError> {
    match expr {
        Expression::Range { start, step, end } => {
            let start_val = eval_expr_in_function(start, env, eval)?;
            let end_val = eval_expr_in_function(end, env, eval)?;
            let step_val = step
                .as_ref()
                .map(|s| eval_expr_in_function(s, env, eval))
                .transpose()?;
            let result = eval_range_values(&start_val, step_val.as_ref(), &end_val, eval.span)?;
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

/// Evaluate range values to an array.
fn eval_range_values(
    start: &Value,
    step: Option<&Value>,
    end: &Value,
    span: Span,
) -> Result<Value, EvalError> {
    // Integer range
    if let (Some(s), Some(e)) = (start.as_integer(), end.as_integer()) {
        let step_int = match step {
            Some(v) => v
                .as_integer()
                .ok_or_else(|| EvalError::type_mismatch("Integer", v.type_name(), span))?,
            None => 1,
        };

        if step_int == 0 {
            return Err(EvalError::range_error("step cannot be zero", span));
        }

        let mut values = Vec::new();
        let mut i = s;
        if step_int > 0 {
            while i <= e {
                values.push(Value::Integer(i));
                i += step_int;
            }
        } else {
            while i >= e {
                values.push(Value::Integer(i));
                i += step_int;
            }
        }
        return Ok(Value::Array(values));
    }

    // Real range
    let s = start
        .to_real()
        .ok_or_else(|| EvalError::type_mismatch("numeric", start.type_name(), span))?;
    let e = end
        .to_real()
        .ok_or_else(|| EvalError::type_mismatch("numeric", end.type_name(), span))?;
    let step_f = match step {
        Some(v) => v
            .to_real()
            .ok_or_else(|| EvalError::type_mismatch("numeric", v.type_name(), span))?,
        None => 1.0,
    };

    if step_f == 0.0 {
        return Err(EvalError::range_error("step cannot be zero", span));
    }

    let mut values = Vec::new();
    let mut v = s;
    if step_f > 0.0 {
        while v <= e + f64::EPSILON {
            values.push(Value::Real(v));
            v += step_f;
        }
    } else {
        while v >= e - f64::EPSILON {
            values.push(Value::Real(v));
            v += step_f;
        }
    }
    Ok(Value::Array(values))
}

/// Convert a ComponentReference to a simple name string.
fn component_ref_to_name(cr: &ComponentReference) -> String {
    cr.parts
        .iter()
        .map(|p| p.ident.to_string())
        .collect::<Vec<_>>()
        .join(".")
}

/// Parse subscripted assignment: x[i], x[i,j], x[1:n] -> (base_name, subscripts)
fn parse_subscripted_assignment(comp: &ComponentReference) -> Option<(String, Vec<Subscript>)> {
    if comp.parts.len() != 1 {
        return None;
    }
    let part = &comp.parts[0];
    if part.subs.is_empty() {
        return None;
    }
    Some((part.ident.to_string(), part.subs.clone()))
}

/// Check if any subscript contains a range expression.
fn has_range_subscript(subscripts: &[Subscript]) -> bool {
    subscripts.iter().any(|s| match s {
        Subscript::Expr(e) => matches!(e.as_ref(), Expression::Range { .. }),
        Subscript::Colon => true,
        Subscript::Index(_) => false,
    })
}

/// Assign to an array slice (e.g., x[1:n] := values).
fn assign_array_slice(
    env: &mut FunctionEnv,
    base_name: &str,
    subscripts: &[Subscript],
    value: Value,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    let arr = env
        .get(base_name)
        .ok_or_else(|| EvalError::unknown_variable(base_name, eval.span))?
        .clone();

    // For now, only support single-dimension slice assignment
    if subscripts.len() != 1 {
        return Err(EvalError::function_error(
            "multi-dimensional slice assignment not yet supported".to_string(),
            eval.span,
        ));
    }

    let new_arr = match &subscripts[0] {
        Subscript::Expr(expr) => {
            if let Expression::Range { start, step, end } = expr.as_ref() {
                let start_val = eval_expr_in_function(start, env, eval)?;
                let end_val = eval_expr_in_function(end, env, eval)?;
                let step_val = step
                    .as_ref()
                    .map(|s| eval_expr_in_function(s, env, eval))
                    .transpose()?;

                let start_idx = start_val.as_integer().ok_or_else(|| {
                    EvalError::type_mismatch("Integer", start_val.type_name(), eval.span)
                })? as usize;
                let end_idx = end_val.as_integer().ok_or_else(|| {
                    EvalError::type_mismatch("Integer", end_val.type_name(), eval.span)
                })? as usize;
                let step_int = extract_step_value(&step_val, eval.span)?;

                set_array_slice(arr, start_idx, end_idx, step_int, value, eval.span)?
            } else {
                return Err(EvalError::function_error(
                    "expected range subscript for slice assignment".to_string(),
                    eval.span,
                ));
            }
        }
        Subscript::Colon => {
            // Full slice assignment x[:] := values
            // Replace entire array with values
            value
        }
        Subscript::Index(_) => {
            return Err(EvalError::function_error(
                "expected range subscript for slice assignment".to_string(),
                eval.span,
            ));
        }
    };

    if !env.set(base_name, new_arr) {
        return Err(EvalError::function_error(
            format!("cannot assign to array: {}", base_name),
            eval.span,
        ));
    }
    Ok(())
}

/// Extract step value from an optional Value, defaulting to 1.
fn extract_step_value(step_val: &Option<Value>, span: Span) -> Result<usize, EvalError> {
    match step_val {
        Some(v) => {
            let step = v
                .as_integer()
                .ok_or_else(|| EvalError::type_mismatch("Integer", v.type_name(), span))?;
            Ok(step as usize)
        }
        None => Ok(1),
    }
}

/// Set a slice of an array (indices are 1-based).
fn set_array_slice(
    arr: Value,
    start: usize,
    end: usize,
    step: usize,
    values: Value,
    span: Span,
) -> Result<Value, EvalError> {
    let mut vec = arr
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", arr.type_name(), span))?
        .clone();

    let value_arr = values
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", values.type_name(), span))?;

    // Calculate expected slice length
    let slice_len = if step == 1 {
        end.saturating_sub(start) + 1
    } else {
        ((end - start) / step) + 1
    };

    if value_arr.len() != slice_len {
        return Err(EvalError::function_error(
            format!(
                "slice assignment size mismatch: expected {} elements, got {}",
                slice_len,
                value_arr.len()
            ),
            span,
        ));
    }

    // Assign values to slice positions (1-based indexing)
    let mut value_idx = 0;
    let mut i = start;
    while i <= end && value_idx < value_arr.len() {
        if i < 1 || i > vec.len() {
            return Err(EvalError::IndexOutOfBounds {
                index: i as i64,
                size: vec.len(),
                span,
            });
        }
        vec[i - 1] = value_arr[value_idx].clone();
        value_idx += 1;
        i += step;
    }

    Ok(Value::Array(vec))
}

/// Assign to an array element.
fn assign_array_element(
    env: &mut FunctionEnv,
    base_name: &str,
    indices: &[Expression],
    value: Value,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    let arr = env
        .get(base_name)
        .ok_or_else(|| EvalError::unknown_variable(base_name, eval.span))?
        .clone();
    let idx_values = eval_index_expressions(indices, env, eval)?;
    let new_arr = set_array_element(arr, &idx_values, value, eval.span)?;
    if !env.set(base_name, new_arr) {
        return Err(EvalError::function_error(
            format!("cannot assign to array: {}", base_name),
            eval.span,
        ));
    }
    Ok(())
}

/// Evaluate index expressions to integer values.
fn eval_index_expressions(
    indices: &[Expression],
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Vec<i64>, EvalError> {
    indices
        .iter()
        .map(|e| {
            let v = eval_expr_in_function(e, env, eval)?;
            v.as_integer()
                .ok_or_else(|| EvalError::type_mismatch("Integer", v.type_name(), eval.span))
        })
        .collect()
}

/// Set an element in a (possibly nested) array.
fn set_array_element(
    arr: Value,
    indices: &[i64],
    value: Value,
    span: Span,
) -> Result<Value, EvalError> {
    if indices.is_empty() {
        return Ok(value);
    }

    let mut vec = arr
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", arr.type_name(), span))?
        .clone();

    let idx = indices[0] as usize;
    if idx < 1 || idx > vec.len() {
        return Err(EvalError::IndexOutOfBounds {
            index: indices[0],
            size: vec.len(),
            span,
        });
    }

    if indices.len() == 1 {
        vec[idx - 1] = value;
    } else {
        vec[idx - 1] = set_array_element(vec[idx - 1].clone(), &indices[1..], value, span)?;
    }

    Ok(Value::Array(vec))
}

/// Apply AST subscripts to a value.
fn apply_subscripts_flat(
    value: Value,
    subs: &[Subscript],
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let mut current = value;
    for sub in subs {
        current = apply_single_subscript(current, sub, env, eval)?;
    }
    Ok(current)
}

/// Apply a single subscript to a value.
fn apply_single_subscript(
    current: Value,
    sub: &Subscript,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    match sub {
        Subscript::Expr(expr) => {
            let idx_val = eval_expr_in_function(expr, env, eval)?;
            let idx = idx_val.as_integer().ok_or_else(|| {
                EvalError::type_mismatch("Integer", idx_val.type_name(), eval.span)
            })? as usize;
            let arr = current
                .as_array()
                .ok_or_else(|| EvalError::type_mismatch("Array", current.type_name(), eval.span))?;
            if idx < 1 || idx > arr.len() {
                return Err(EvalError::IndexOutOfBounds {
                    index: idx as i64,
                    size: arr.len(),
                    span: eval.span,
                });
            }
            Ok(arr[idx - 1].clone())
        }
        Subscript::Colon => Ok(current),
        Subscript::Index(idx) => {
            let arr = current
                .as_array()
                .ok_or_else(|| EvalError::type_mismatch("Array", current.type_name(), eval.span))?;
            let idx_i64 = *idx;
            let idx_usize = idx_i64 as usize;
            if idx_usize < 1 || idx_usize > arr.len() {
                return Err(EvalError::IndexOutOfBounds {
                    index: idx_i64,
                    size: arr.len(),
                    span: eval.span,
                });
            }
            Ok(arr[idx_usize - 1].clone())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_flat as flat;

    fn make_simple_function() -> Function {
        // function f(input Real x) output Real y; algorithm y := x * 2; end f;
        use rumoca_ir_ast as ast;
        use std::sync::Arc;

        let mut func = Function::new("test.f", Span::DUMMY);
        func.add_input(flat::FunctionParam::new("x", "Real"));
        func.add_output(flat::FunctionParam::new("y", "Real"));
        func.pure = true;

        // y := x * 2
        let ast_stmt = rumoca_ir_ast::Statement::Assignment {
            comp: ast::ComponentReference {
                local: false,
                def_id: None,
                parts: vec![ast::ComponentRefPart {
                    ident: ast::Token {
                        text: Arc::from("y"),
                        ..Default::default()
                    },
                    subs: None,
                }],
            },
            value: ast::Expression::Binary {
                op: ast::OpBinary::Mul(ast::Token::default()),
                lhs: Arc::new(ast::Expression::ComponentReference(
                    ast::ComponentReference {
                        local: false,
                        def_id: None,
                        parts: vec![ast::ComponentRefPart {
                            ident: ast::Token {
                                text: Arc::from("x"),
                                ..Default::default()
                            },
                            subs: None,
                        }],
                    },
                )),
                rhs: Arc::new(ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: ast::Token {
                        text: Arc::from("2"),
                        ..Default::default()
                    },
                }),
            },
        };
        func.body = vec![rumoca_ir_flat::Statement::from_ast_with_def_map(
            &ast_stmt, None,
        )];

        func
    }

    #[test]
    fn test_simple_function() {
        let func = make_simple_function();
        let mut ctx = EvalContext::new();
        ctx.functions.insert("test.f".to_string(), func.clone());

        let result = eval_function(
            &func,
            vec![Value::Real(5.0)],
            &ctx,
            &EvalLimits::default(),
            0,
            Span::DUMMY,
        )
        .unwrap();

        assert!((result.to_real().unwrap() - 10.0).abs() < 1e-10);
    }

    #[test]
    fn test_recursion_limit() {
        // Create a recursive function that will exceed the limit
        let mut func = Function::new("test.recurse", Span::DUMMY);
        func.add_input(flat::FunctionParam::new("n", "Integer"));
        func.add_output(flat::FunctionParam::new("y", "Integer"));
        func.pure = true;

        // Simple function that always returns 0 (to test limit checking)
        use rumoca_ir_ast as ast;
        use std::sync::Arc;
        let ast_stmt = rumoca_ir_ast::Statement::Assignment {
            comp: ast::ComponentReference {
                local: false,
                def_id: None,
                parts: vec![ast::ComponentRefPart {
                    ident: ast::Token {
                        text: Arc::from("y"),
                        ..Default::default()
                    },
                    subs: None,
                }],
            },
            value: ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token: ast::Token {
                    text: Arc::from("0"),
                    ..Default::default()
                },
            },
        };
        func.body = vec![rumoca_ir_flat::Statement::from_ast_with_def_map(
            &ast_stmt, None,
        )];

        let ctx = EvalContext::new();
        let limits = EvalLimits {
            recursion_depth: 5,
            max_iterations: 1000,
        };

        // This should succeed at depth 5
        let result = eval_function(
            &func,
            vec![Value::Integer(1)],
            &ctx,
            &limits,
            5,
            Span::DUMMY,
        );
        assert!(result.is_ok());

        // This should fail at depth 6
        let result = eval_function(
            &func,
            vec![Value::Integer(1)],
            &ctx,
            &limits,
            6,
            Span::DUMMY,
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_while_loop() {
        // Test: function countTo4() output Integer count; algorithm count := 0; while count < 4 loop count := count + 1; end while; end countTo4;
        use rumoca_ir_ast as ast;
        use std::sync::Arc;

        let mut func = Function::new("test.countTo4", Span::DUMMY);
        func.add_output(flat::FunctionParam::new("count", "Integer"));
        func.pure = true;

        // count := 0
        let init_stmt = ast::Statement::Assignment {
            comp: ast::ComponentReference {
                local: false,
                def_id: None,
                parts: vec![ast::ComponentRefPart {
                    ident: ast::Token {
                        text: Arc::from("count"),
                        ..Default::default()
                    },
                    subs: None,
                }],
            },
            value: ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token: ast::Token {
                    text: Arc::from("0"),
                    ..Default::default()
                },
            },
        };

        // while count < 4 loop count := count + 1; end while
        let while_stmt = ast::Statement::While(ast::StatementBlock {
            cond: ast::Expression::Binary {
                op: ast::OpBinary::Lt(ast::Token::default()),
                lhs: Arc::new(ast::Expression::ComponentReference(
                    ast::ComponentReference {
                        local: false,
                        def_id: None,
                        parts: vec![ast::ComponentRefPart {
                            ident: ast::Token {
                                text: Arc::from("count"),
                                ..Default::default()
                            },
                            subs: None,
                        }],
                    },
                )),
                rhs: Arc::new(ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: ast::Token {
                        text: Arc::from("4"),
                        ..Default::default()
                    },
                }),
            },
            stmts: vec![ast::Statement::Assignment {
                comp: ast::ComponentReference {
                    local: false,
                    def_id: None,
                    parts: vec![ast::ComponentRefPart {
                        ident: ast::Token {
                            text: Arc::from("count"),
                            ..Default::default()
                        },
                        subs: None,
                    }],
                },
                value: ast::Expression::Binary {
                    op: ast::OpBinary::Add(ast::Token::default()),
                    lhs: Arc::new(ast::Expression::ComponentReference(
                        ast::ComponentReference {
                            local: false,
                            def_id: None,
                            parts: vec![ast::ComponentRefPart {
                                ident: ast::Token {
                                    text: Arc::from("count"),
                                    ..Default::default()
                                },
                                subs: None,
                            }],
                        },
                    )),
                    rhs: Arc::new(ast::Expression::Terminal {
                        terminal_type: ast::TerminalType::UnsignedInteger,
                        token: ast::Token {
                            text: Arc::from("1"),
                            ..Default::default()
                        },
                    }),
                },
            }],
        });

        func.body = vec![
            rumoca_ir_flat::Statement::from_ast_with_def_map(&init_stmt, None),
            rumoca_ir_flat::Statement::from_ast_with_def_map(&while_stmt, None),
        ];

        let ctx = EvalContext::new();
        let result =
            eval_function(&func, vec![], &ctx, &EvalLimits::default(), 0, Span::DUMMY).unwrap();

        assert_eq!(result.as_integer(), Some(4), "while loop should count to 4");
    }
}
