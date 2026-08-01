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
use rumoca_core::{
    ComponentReference, ComprehensionIndex, Expression, ForIndex, Function, Literal, Span,
    Statement, StatementBlock, Subscript,
};

use rumoca_core::ExpressionVisitor;

use super::errors::EvalError;
use super::value::Value;
use super::{EvalContext, EvalIndexMap};

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
#[derive(Clone, Copy)]
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

/// Which declaration list [`FunctionEnv::declare_all`] is binding.
#[derive(Clone, Copy)]
enum DeclaredKind {
    Output,
    Local,
}

/// The order a function's outputs and locals may be bound in on entry.
///
/// MLS 3.6 §12.4.4: the declaration bindings "are executed in an order where a
/// variable is not used before its binding"; the *only* error the rule names is
/// that no such order exists. So this is a topological sort over "declaration
/// `d` reads declaration `e`", seeded in written order so an independent set
/// keeps its declaration order, with a cycle reported by name.
///
/// A declaration's reads include the names its written extent mentions
/// (MLS §12.2), because the shaped default cannot be built before them either.
fn binding_order(
    func: &Function,
    span: Span,
) -> Result<Vec<(DeclaredKind, &rumoca_core::FunctionParam)>, EvalError> {
    let declarations: Vec<(DeclaredKind, &rumoca_core::FunctionParam)> = func
        .outputs
        .iter()
        .map(|output| (DeclaredKind::Output, output))
        .chain(func.locals.iter().map(|local| (DeclaredKind::Local, local)))
        .collect();
    if declarations.len() < 2 {
        return Ok(declarations);
    }
    let position: EvalIndexMap<usize> = declarations
        .iter()
        .enumerate()
        .map(|(index, (_, param))| (param.name.clone(), index))
        .collect();

    let mut ordered = Vec::with_capacity(declarations.len());
    let mut state = vec![VisitState::Unvisited; declarations.len()];
    for index in 0..declarations.len() {
        visit_declaration(
            index,
            &declarations,
            &position,
            &mut state,
            &mut ordered,
            span,
        )?;
    }
    Ok(ordered
        .into_iter()
        .map(|index| declarations[index])
        .collect())
}

#[derive(Clone, Copy, PartialEq)]
enum VisitState {
    Unvisited,
    InProgress,
    Placed,
}

fn visit_declaration(
    index: usize,
    declarations: &[(DeclaredKind, &rumoca_core::FunctionParam)],
    position: &EvalIndexMap<usize>,
    state: &mut [VisitState],
    ordered: &mut Vec<usize>,
    span: Span,
) -> Result<(), EvalError> {
    match state[index] {
        VisitState::Placed => return Ok(()),
        VisitState::InProgress => {
            return Err(EvalError::CircularDependency {
                path: declarations[index].1.name.clone(),
                span,
            });
        }
        VisitState::Unvisited => {}
    }
    state[index] = VisitState::InProgress;
    for read in declaration_reads(declarations[index].1) {
        let Some(dependency) = position.get(&read).copied() else {
            continue;
        };
        if dependency != index {
            visit_declaration(dependency, declarations, position, state, ordered, span)?;
        }
    }
    state[index] = VisitState::Placed;
    ordered.push(index);
    Ok(())
}

/// Every name the declaration of `param` reads: its binding and its extents.
fn declaration_reads(param: &rumoca_core::FunctionParam) -> Vec<String> {
    let mut reads = ReferenceCollector::default();
    if let Some(default) = &param.default {
        reads.visit_expression(default);
    }
    for subscript in &param.shape_expr {
        reads.visit_subscript(subscript);
    }
    reads.names
}

#[derive(Default)]
struct ReferenceCollector {
    names: Vec<String>,
}

impl rumoca_core::ExpressionVisitor for ReferenceCollector {
    fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
        self.names.push(name.as_str().to_string());
        self.walk_var_ref(name, subscripts);
    }
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

/// Function-call argument after expression evaluation.
#[derive(Debug, Clone)]
pub struct FunctionCallArg {
    pub name: Option<String>,
    pub value: Value,
}

impl FunctionCallArg {
    pub fn positional(value: Value) -> Self {
        Self { name: None, value }
    }

    pub fn named(name: String, value: Value) -> Self {
        Self {
            name: Some(name),
            value,
        }
    }
}

impl FunctionEnv {
    fn new_with_call_args(
        func: &Function,
        args: Vec<FunctionCallArg>,
        eval: &EvalState<'_>,
    ) -> Result<Self, EvalError> {
        let inputs = Self::bind_inputs(func, args, eval.span)?;
        let mut env = Self {
            inputs,
            outputs: IndexMap::new(),
            locals: IndexMap::new(),
        };
        env.declare_all(func, eval)?;
        Ok(env)
    }

    /// Bind input arguments to parameters.
    fn bind_inputs(
        func: &Function,
        args: Vec<FunctionCallArg>,
        span: Span,
    ) -> Result<IndexMap<String, Value>, EvalError> {
        let mut inputs = IndexMap::new();
        let mut next_positional = 0;
        let mut seen_named = false;

        for arg in args {
            match arg.name {
                Some(name) => {
                    seen_named = true;
                    Self::bind_named_input(func, &mut inputs, name, arg.value, span)?;
                }
                None => {
                    Self::bind_positional_input(
                        func,
                        &mut inputs,
                        next_positional,
                        seen_named,
                        arg.value,
                        span,
                    )?;
                    next_positional += 1;
                }
            }
        }

        for param in &func.inputs {
            if inputs.contains_key(&param.name) {
                continue;
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
            return Err(EvalError::function_error(
                format!(
                    "missing required argument {} for function {}",
                    param.name, func.name
                ),
                span,
            ));
        }
        Ok(inputs)
    }

    fn bind_named_input(
        func: &Function,
        inputs: &mut IndexMap<String, Value>,
        name: String,
        value: Value,
        span: Span,
    ) -> Result<(), EvalError> {
        if !func.inputs.iter().any(|param| param.name == name) {
            return Err(EvalError::function_error(
                format!("unknown named argument {name} for function {}", func.name),
                span,
            ));
        }
        if inputs.insert(name.clone(), value).is_some() {
            return Err(EvalError::function_error(
                format!("duplicate argument {name} for function {}", func.name),
                span,
            ));
        }
        Ok(())
    }

    fn bind_positional_input(
        func: &Function,
        inputs: &mut IndexMap<String, Value>,
        next_positional: usize,
        seen_named: bool,
        value: Value,
        span: Span,
    ) -> Result<(), EvalError> {
        if seen_named {
            return Err(EvalError::function_error(
                format!("positional argument after named argument in {}", func.name),
                span,
            ));
        }
        let Some(param) = func.inputs.get(next_positional) else {
            return Err(EvalError::function_error(
                format!("too many arguments for function {}", func.name),
                span,
            ));
        };
        if inputs.insert(param.name.clone(), value).is_some() {
            return Err(EvalError::function_error(
                format!(
                    "duplicate argument {} for function {}",
                    param.name, func.name
                ),
                span,
            ));
        }
        Ok(())
    }

    /// Bind a function's declared outputs and locals on entry.
    ///
    /// MLS 3.6 §12.4.4 makes a declaration equation inside a function the value
    /// the component holds on entry, and fixes the order they run in: the
    /// bindings "are executed in an order where a variable is not used before
    /// its binding", an error being reported only when no such order exists.
    /// That is a topological order over the declarations, not the written one —
    /// `Integer a = b + 1; Integer b = 2;` is a legal acyclic program OMC folds
    /// to `3`, so evaluating in declaration order would refuse it. A cycle is
    /// the one case §12.4.4 calls an error, and it is reported by name.
    ///
    /// Substituting a type default for a declaration that *has* a binding is
    /// what silently folded `Integer mBasic = integer(m/n)` to `0`, so a
    /// binding that cannot be evaluated propagates its error and the whole call
    /// refuses to fold: there is no second value the entry state could take.
    ///
    /// A declaration without a binding gets the zero of its declared shape,
    /// which is the value the DAE lowering creates for it and the container a
    /// later `y[i] := …` writes into.
    fn declare_all(&mut self, func: &Function, eval: &EvalState<'_>) -> Result<(), EvalError> {
        for (kind, param) in binding_order(func, eval.span)? {
            let value = match &param.default {
                Some(default) => eval_expr_in_function(default, self, eval)?,
                None => self.shaped_default(param, eval)?,
            };
            match kind {
                DeclaredKind::Output => self.outputs.insert(param.name.clone(), value),
                DeclaredKind::Local => self.locals.insert(param.name.clone(), value),
            };
        }
        Ok(())
    }

    /// The zero of `param`'s declared shape, for a declaration with no binding.
    ///
    /// A declared extent is now read from the call's own bound inputs
    /// (MLS §12.2), so the size of the container about to be allocated is
    /// decided by model values. The evaluator's element budget bounds that
    /// decision and reports a form it will not fold rather than materializing
    /// whatever the model asked for.
    fn shaped_default(
        &self,
        param: &rumoca_core::FunctionParam,
        eval: &EvalState<'_>,
    ) -> Result<Value, EvalError> {
        let dimensions = self.declared_dimensions(param, eval)?;
        let budget = i64::try_from(eval.limits.max_iterations).unwrap_or(i64::MAX);
        let elements = dimensions
            .iter()
            .try_fold(1_i64, |total, extent| total.checked_mul(*extent));
        if elements.is_none_or(|elements| elements > budget) {
            return Err(EvalError::UnsupportedExpression {
                kind: format!(
                    "declared extent {dimensions:?} of `{}` is beyond the \
                     constant-evaluation element budget",
                    param.name
                ),
                span: eval.span,
            });
        }
        Ok(type_default_value(&param.type_name, &dimensions))
    }

    /// The declared extent of `param` in the environment bound so far.
    ///
    /// MLS 3.6 §12.2 admits a function component's array dimension "given by
    /// the input formal parameters", so `output Real orientation[m]` only has
    /// an extent once `m` is bound. `effective_type` can only carry the extent
    /// of a declaration whose dimensions are literal, and reports `0` for the
    /// rest; the written `shape_expr` is what actually names `m`.
    ///
    /// An extent this environment cannot settle refuses the call, exactly as an
    /// unsettleable binding does. Falling back to the declared `0` built an
    /// *empty* container that `size()` and a later loop then read as the
    /// component's real extent — a wrong value, not a missing one.
    fn declared_dimensions(
        &self,
        param: &rumoca_core::FunctionParam,
        eval: &EvalState<'_>,
    ) -> Result<Vec<i64>, EvalError> {
        let declared = param.dimensions();
        if param.shape_expr.len() != declared.len() {
            return Ok(declared.to_vec());
        }
        param
            .shape_expr
            .iter()
            .zip(declared)
            .map(|(subscript, fallback)| self.declared_extent(param, subscript, *fallback, eval))
            .collect()
    }

    /// One declared dimension of `param`.
    fn declared_extent(
        &self,
        param: &rumoca_core::FunctionParam,
        subscript: &Subscript,
        fallback: i64,
        eval: &EvalState<'_>,
    ) -> Result<i64, EvalError> {
        let extent = match subscript {
            Subscript::Index { value, .. } => *value,
            // A written dimension that is a plain `:` carries no extent at all,
            // so there is nothing for this environment to settle.
            Subscript::Colon { .. } => fallback,
            Subscript::Expr { expr, .. } => {
                let value = eval_expr_in_function(expr, self, eval)?;
                value.as_integer().ok_or_else(|| {
                    EvalError::type_mismatch("Integer", value.type_name(), eval.span)
                })?
            }
        };
        if extent < 0 {
            return Err(EvalError::function_error(
                format!(
                    "declared dimension of `{}` evaluates to the negative extent {extent}",
                    param.name
                ),
                eval.span,
            ));
        }
        Ok(extent)
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
    fn return_value(&self, span: Span) -> Result<Value, EvalError> {
        if self.outputs.len() == 1 {
            self.outputs.values().next().cloned().ok_or_else(|| {
                EvalError::function_error("single-output function has no output slot", span)
            })
        } else {
            // Multiple outputs -> tuple/array
            Ok(Value::Array(self.outputs.values().cloned().collect()))
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
    let args = args.into_iter().map(FunctionCallArg::positional).collect();
    eval_function_with_call_args(func, args, ctx, limits, depth, span)
}

/// Evaluate a user-defined function with already evaluated positional/named arguments.
pub fn eval_function_with_call_args(
    func: &Function,
    args: Vec<FunctionCallArg>,
    ctx: &EvalContext,
    limits: &EvalLimits,
    depth: usize,
    span: Span,
) -> Result<Value, EvalError> {
    if depth > limits.recursion_depth {
        return Err(EvalError::function_error(
            format!(
                "recursion depth exceeded ({}) in function {}",
                limits.recursion_depth, func.name
            ),
            span,
        ));
    }

    let eval = EvalState {
        ctx,
        limits,
        depth,
        span,
    };
    let mut env = FunctionEnv::new_with_call_args(func, args, &eval)?;
    if func.is_constructor {
        return record_constructor_value(func, &env, span);
    }
    let mut iteration_count = 0;
    let mut state = StmtState {
        env: &mut env,
        iteration_count: &mut iteration_count,
    };

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

    env.return_value(span)
}

/// The record a record-constructor call folds to.
///
/// MLS 3.6 §12.6: the implicitly defined record constructor takes the record's
/// declared components as its inputs and returns an instance of the record, so
/// the call is the record built from the bound arguments in declaration order.
/// It has no algorithm to interpret and no output formal parameter, and reading
/// it as an ordinary function is what folded `Complex(re, im)` to the empty
/// tuple of its (absent) outputs.
fn record_constructor_value(
    func: &Function,
    env: &FunctionEnv,
    span: Span,
) -> Result<Value, EvalError> {
    let mut fields = IndexMap::with_capacity(func.inputs.len());
    for component in &func.inputs {
        let value = env.inputs.get(&component.name).ok_or_else(|| {
            EvalError::function_error(
                format!(
                    "record constructor {} has no value for component {}",
                    func.name, component.name
                ),
                span,
            )
        })?;
        fields.insert(component.name.clone(), value.clone());
    }
    Ok(Value::Record(fields))
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
        Statement::Empty { .. } => Ok(FlowControl::Continue),
        Statement::Assignment { comp, value, .. } => eval_assignment(comp, value, state.env, eval),
        Statement::Return { .. } => Ok(FlowControl::Return),
        Statement::Break { .. } => Ok(FlowControl::Break),
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => eval_if_statement(cond_blocks, else_block, state, eval),
        Statement::For {
            indices, equations, ..
        } => eval_for_statement(indices, equations, state, eval),
        Statement::While { block, .. } => eval_while_statement(block, state, eval),
        Statement::FunctionCall {
            comp,
            args,
            outputs,
            ..
        } => eval_fn_call_stmt(comp, args, outputs, state.env, eval),
        Statement::When { .. } => Err(EvalError::not_constant(
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
    // MLS 3.6 §10.5 "Indexing": "The array indexing operator `name[…]` is used
    // to access array elements for retrieval of their values or for updating
    // these values." So `y[i] := e` and `y[a:b] := e` update elements of `y`;
    // the whole-value assignment below is reachable only for an unsubscripted
    // target. Dispatching on the joined name first made `orientation[1:3] := v`
    // replace the whole vector with `v`, which folded `symmetricOrientation(6)`
    // to three elements instead of six.
    if let Some((base_name, subscripts)) = parse_subscripted_assignment(comp) {
        assign_subscripted(env, &base_name, &subscripts, val, eval)?;
        return Ok(FlowControl::Continue);
    }
    let name = component_ref_to_name(comp);
    if env.set(&name, val) {
        return Ok(FlowControl::Continue);
    }
    Err(EvalError::function_error(
        format!("cannot assign to variable: {}", name),
        eval.span,
    ))
}

/// Assign through a subscripted target, writing into the component's value.
fn assign_subscripted(
    env: &mut FunctionEnv,
    base_name: &str,
    subscripts: &[Subscript],
    value: Value,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    if has_range_subscript(subscripts) {
        return assign_array_slice(env, base_name, subscripts, value, eval);
    }
    let indices = eval_subscript_indices(subscripts, env, eval)?;
    assign_array_element(env, base_name, &indices, value, eval)
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
    outputs: &[Option<ComponentReference>],
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
    outputs: &[Option<ComponentReference>],
    result: Value,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    match &result {
        Value::Array(arr) if arr.len() == outputs.len() => {
            assign_multiple_outputs(outputs, arr, env, eval)
        }
        _ if outputs.len() == 1 => {
            if let Some(output) = &outputs[0] {
                assign_single_output(output, result, env, eval)
            } else {
                Ok(())
            }
        }
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
    outputs: &[Option<ComponentReference>],
    arr: &[Value],
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    for (output, val) in outputs.iter().zip(arr.iter()) {
        let Some(output) = output else {
            continue;
        };
        let name = component_ref_to_name(output);
        if !env.set(&name, val.clone()) {
            return Err(EvalError::function_error(
                format!("cannot assign to output variable: {}", name),
                eval.span,
            ));
        }
    }
    Ok(())
}

/// Assign single output from result.
fn assign_single_output(
    output: &ComponentReference,
    result: Value,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    let name = component_ref_to_name(output);
    if !env.set(&name, result) {
        return Err(EvalError::function_error(
            format!("cannot assign to output variable: {}", name),
            eval.span,
        ));
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
    let eval_state = EvalState {
        span: expr.span().unwrap_or(eval.span),
        ..*eval
    };
    let eval = &eval_state;
    match expr {
        Expression::Empty { .. } => Err(EvalError::UnsupportedExpression {
            kind: "empty expression".to_string(),
            span: eval.span,
        }),
        Expression::Literal { value: literal, .. } => eval_literal(literal),
        Expression::VarRef {
            name, subscripts, ..
        } => eval_var_ref(name, subscripts, env, eval),
        Expression::Binary { op, lhs, rhs, .. } => eval_binary(op, lhs, rhs, env, eval),
        Expression::Unary { op, rhs, .. } => eval_unary(op, rhs, env, eval),
        Expression::FunctionCall { name, args, .. } => eval_fn_call_expr(name, args, env, eval),
        Expression::StringConversion { .. } => Err(EvalError::UnsupportedExpression {
            kind: "predefined String conversion".to_string(),
            span: eval.span,
        }),
        Expression::BuiltinCall { function, args, .. } => {
            let arg_values: Vec<Value> = args
                .iter()
                .map(|arg| eval_expr_in_function(arg, env, eval))
                .collect::<Result<_, _>>()?;
            super::eval_builtin(function.name(), &arg_values, eval.span)
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
        Expression::FieldAccess { base, field, .. } => {
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
    reference: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let name = reference.as_str();
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
    // MLS 3.6 §12.2: a record component's fields are read through the joined
    // reference Flat renders, so `z.im` names the field `im` of the bound local
    // `z`. Resolving it here is also what keeps the enumeration fallback below
    // honest — a reference whose head segment is a component in scope is never
    // an enumeration literal, and guessing one folded `z.im` to the enumeration
    // value `z.im` instead of the record field.
    if let Some(value) = read_bound_field_path(reference, env, eval)? {
        return apply_subscripts_flat(value, subscripts, env, eval);
    }
    // Try parsing as qualified enum.
    if let Some((type_name, literal)) = reference.scope_split() {
        return Ok(Value::Enum(type_name.to_string(), literal.to_string()));
    }
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
    let Some(root) = env.get(head) else {
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
    super::operators::eval_binary_op(op, &lhs_val, &rhs_val, eval.span)
}

/// Evaluate a unary expression.
fn eval_unary(
    op: &rumoca_core::OpUnary,
    rhs: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let rhs_val = eval_expr_in_function(rhs, env, eval)?;
    super::operators::eval_unary_op(op, &rhs_val, eval.span)
}

/// Evaluate a function call expression.
fn eval_fn_call_expr(
    name: &rumoca_core::Reference,
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
    if super::is_builtin(name) {
        return super::eval_builtin(name, &args, eval.span);
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
        Expression::Range {
            start, step, end, ..
        } => {
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

        return Ok(Value::Array(super::range_eval::collect_int_range(
            s, e, step_int,
        )));
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
    if !s.is_finite() || !e.is_finite() || !step_f.is_finite() {
        return Err(EvalError::range_error(
            "range bounds and step must be finite",
            span,
        ));
    }

    super::range_eval::collect_real_range(s, e, step_f, span).map(Value::Array)
}

/// Convert a ComponentReference to a simple name string.
fn component_ref_to_name(cr: &ComponentReference) -> String {
    cr.parts()
        .iter()
        .map(|p| p.ident.to_string())
        .collect::<Vec<_>>()
        .join(".")
}

/// Parse subscripted assignment: x[i], x[i,j], x[1:n] -> (base_name, subscripts)
fn parse_subscripted_assignment(comp: &ComponentReference) -> Option<(String, Vec<Subscript>)> {
    if comp.parts().len() != 1 {
        return None;
    }
    let part = &comp.parts()[0];
    if part.subs.is_empty() {
        return None;
    }
    Some((part.ident.to_string(), part.subs.clone()))
}

/// Check if any subscript contains a range expression.
fn has_range_subscript(subscripts: &[Subscript]) -> bool {
    subscripts.iter().any(|s| match s {
        Subscript::Expr { expr: e, .. } => matches!(e.as_ref(), Expression::Range { .. }),
        Subscript::Colon { .. } => true,
        Subscript::Index { .. } => false,
    })
}

/// Assign through a slice target (`x[a:b] := values`, `x[:] := values`).
///
/// Every index the subscript enumerates is computed in signed arithmetic before
/// anything is written, so a descending range, an empty range and an
/// out-of-range index are all decided rather than discovered mid-write. The
/// previous unsigned form panicked on `v[3:-1:1]` and divided by zero on a zero
/// step; both are subscripts the parser accepts and OMC rejects cleanly.
fn assign_array_slice(
    env: &mut FunctionEnv,
    base_name: &str,
    subscripts: &[Subscript],
    value: Value,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    let target = env
        .get(base_name)
        .ok_or_else(|| EvalError::unknown_variable(base_name, eval.span))?
        .clone();
    if subscripts.len() != 1 {
        return Err(EvalError::function_error(
            "multi-dimensional slice assignment not yet supported".to_string(),
            eval.span,
        ));
    }
    let elements = target
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", target.type_name(), eval.span))?;
    let indices = slice_indices(&subscripts[0], elements.len(), env, eval)?;
    let new_value = set_array_slice(target, &indices, value, eval.span)?;
    if !env.set(base_name, new_value) {
        return Err(EvalError::function_error(
            format!("cannot assign to array: {}", base_name),
            eval.span,
        ));
    }
    Ok(())
}

/// The 1-based indices a slice subscript enumerates over a container of `len`.
///
/// MLS 3.6 §10.5 gives `:` the whole dimension, and §10.4.1 gives `a:s:b` the
/// values `a, a+s, …` up to `b` — empty when the step points away from `b`, and
/// undefined for a zero step, which is reported rather than divided by.
fn slice_indices(
    subscript: &Subscript,
    len: usize,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Vec<i64>, EvalError> {
    let Subscript::Expr { expr, .. } = subscript else {
        // `x[:] := …` names every element of the dimension. It is a slice like
        // any other, so it is size-checked against the container the same way;
        // treating it as a whole-value replacement is what let
        // `v[:] := {1,2,3,4}` resize an `Integer v[2]`.
        if matches!(subscript, Subscript::Colon { .. }) {
            return Ok((1..=len as i64).collect());
        }
        return Err(EvalError::function_error(
            "expected range subscript for slice assignment".to_string(),
            eval.span,
        ));
    };
    let Expression::Range {
        start, step, end, ..
    } = expr.as_ref()
    else {
        return Err(EvalError::function_error(
            "expected range subscript for slice assignment".to_string(),
            eval.span,
        ));
    };
    let start = slice_bound(start, env, eval)?;
    let end = slice_bound(end, env, eval)?;
    let step = match step {
        Some(step) => slice_bound(step, env, eval)?,
        None => 1,
    };
    if step == 0 {
        return Err(EvalError::range_error(
            "range step must not be zero in a slice assignment target",
            eval.span,
        ));
    }
    let budget = i64::try_from(eval.limits.max_iterations).unwrap_or(i64::MAX);
    let mut indices = Vec::new();
    let mut current = start;
    while (step > 0 && current <= end) || (step < 0 && current >= end) {
        indices.push(current);
        if indices.len() as i64 > budget {
            return Err(EvalError::UnsupportedExpression {
                kind: "slice assignment target is beyond the constant-evaluation element budget"
                    .to_string(),
                span: eval.span,
            });
        }
        let Some(next) = current.checked_add(step) else {
            break;
        };
        current = next;
    }
    Ok(indices)
}

fn slice_bound(
    expr: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<i64, EvalError> {
    let value = eval_expr_in_function(expr, env, eval)?;
    value
        .as_integer()
        .ok_or_else(|| EvalError::type_mismatch("Integer", value.type_name(), eval.span))
}

/// Write `values` into the 1-based `indices` of `target`.
fn set_array_slice(
    target: Value,
    indices: &[i64],
    values: Value,
    span: Span,
) -> Result<Value, EvalError> {
    let mut elements = target
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", target.type_name(), span))?
        .clone();
    // MLS 3.6 §10.6.1: an assignment requires both sides to have the same
    // number of dimensions and the same sizes, so an empty slice takes an empty
    // array and nothing else — `v[2:1] := {9}` is rejected, not silently
    // written nowhere.
    let assigned = values
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", values.type_name(), span))?;
    if assigned.len() != indices.len() {
        return Err(EvalError::function_error(
            format!(
                "slice assignment size mismatch: target names {} element(s), value has {}",
                indices.len(),
                assigned.len()
            ),
            span,
        ));
    }
    for (index, value) in indices.iter().zip(assigned) {
        let slot = usize::try_from(*index)
            .ok()
            .filter(|slot| (1..=elements.len()).contains(slot))
            .ok_or(EvalError::IndexOutOfBounds {
                index: *index,
                size: elements.len(),
                span,
            })?;
        elements[slot - 1] = coerce_to_declared(&elements[slot - 1], value.clone());
    }
    Ok(Value::Array(elements))
}

/// MLS 3.6 §10.6.13: an Integer value written into a Real component is
/// converted to Real.
///
/// The declared element type is carried by the value already in the slot, which
/// the declaration's shaped default established. Without this,
/// `orientation[1] := 0` left an `Integer` inside a declared `Real[2]`, and the
/// mixed array compared unequal to the all-Real value the same function
/// produces on its other branch.
fn coerce_to_declared(slot: &Value, value: Value) -> Value {
    match (slot, value) {
        (Value::Real(_), Value::Integer(written)) => Value::Real(written as f64),
        (Value::Array(slots), Value::Array(written)) if slots.len() == written.len() => {
            Value::Array(
                slots
                    .iter()
                    .zip(written)
                    .map(|(slot, written)| coerce_to_declared(slot, written))
                    .collect(),
            )
        }
        (_, value) => value,
    }
}

/// Assign to an array element.
fn assign_array_element(
    env: &mut FunctionEnv,
    base_name: &str,
    indices: &[i64],
    value: Value,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    let arr = env
        .get(base_name)
        .ok_or_else(|| EvalError::unknown_variable(base_name, eval.span))?
        .clone();
    let new_arr = set_array_element(arr, indices, value, eval.span)?;
    if !env.set(base_name, new_arr) {
        return Err(EvalError::function_error(
            format!("cannot assign to array: {}", base_name),
            eval.span,
        ));
    }
    Ok(())
}

/// Evaluate the element indices of an assignment target.
///
/// A literal `Subscript::Index` is as much an element index as an evaluated
/// expression is; reading only the expression form dropped `y[1] := …`
/// entirely, so the whole subscript list was empty and the target degraded to
/// the whole component.
fn eval_subscript_indices(
    subscripts: &[Subscript],
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Vec<i64>, EvalError> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } => Ok(*value),
            Subscript::Expr { expr, .. } => {
                let value = eval_expr_in_function(expr, env, eval)?;
                value.as_integer().ok_or_else(|| {
                    EvalError::type_mismatch("Integer", value.type_name(), eval.span)
                })
            }
            Subscript::Colon { .. } => Err(EvalError::function_error(
                "whole-dimension assignment target mixed with element indices",
                eval.span,
            )),
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

    let idx = usize::try_from(indices[0])
        .ok()
        .filter(|idx| (1..=vec.len()).contains(idx))
        .ok_or(EvalError::IndexOutOfBounds {
            index: indices[0],
            size: vec.len(),
            span,
        })?;

    if indices.len() == 1 {
        // MLS §10.6.13 again: the slot carries the declared element type.
        vec[idx - 1] = coerce_to_declared(&vec[idx - 1], value);
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
        Subscript::Expr { expr, .. } => {
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
        Subscript::Colon { .. } => Ok(current),
        Subscript::Index { value: idx, .. } => {
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
mod tests;
