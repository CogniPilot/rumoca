//! Constant expression evaluator for Modelica.
//!
//! This crate provides compile-time evaluation of Modelica expressions,
//! used for:
//! - Evaluating parameter values
//! - Computing array dimensions
//! - Resolving for-loop ranges
//! - Evaluating if-equation conditions
//! - Evaluating user-defined functions with constant arguments (MLS §12)

pub mod builtins;
pub mod errors;
pub mod function_eval;
pub mod value;

pub use builtins::{eval_builtin, is_builtin};
pub use errors::EvalError;
pub use function_eval::{EvalLimits, eval_function};
pub use value::Value;

use indexmap::IndexMap;
use rumoca_core::{EvalLookup, Span};
use rumoca_ir_flat as flat;
use std::borrow::Cow;

type BuiltinFunction = flat::BuiltinFunction;
type Expression = flat::Expression;
type Function = flat::Function;
type Literal = flat::Literal;
type OpBinary = flat::OpBinary;
type OpUnary = flat::OpUnary;
type Subscript = flat::Subscript;
type VarName = flat::VarName;

/// Evaluation context providing variable/parameter values.
pub struct EvalContext {
    /// Parameter values by name (e.g., "component.subcomponent.param" -> value)
    pub parameters: IndexMap<String, Value>,

    /// Enum values: "TypeName.LiteralName" -> (TypeName, LiteralName)
    pub enum_literals: IndexMap<String, (String, String)>,

    /// User-defined function definitions for constant evaluation (MLS §12).
    pub functions: IndexMap<String, Function>,
}

impl Default for EvalContext {
    fn default() -> Self {
        Self::new()
    }
}

impl EvalContext {
    /// Create an empty evaluation context.
    pub fn new() -> Self {
        Self {
            parameters: IndexMap::new(),
            enum_literals: IndexMap::new(),
            functions: IndexMap::new(),
        }
    }

    /// Add a function definition for constant evaluation.
    pub fn add_function(&mut self, func: Function) {
        let full_name = func.name.to_string();
        // Add with full name
        self.functions.insert(full_name.clone(), func.clone());
        // Also add with short name (last component) for function body lookups
        // This enables recursive calls inside function bodies that use unqualified names
        if let Some(short_name) = full_name.rsplit('.').next()
            && short_name != full_name
            && !self.functions.contains_key(short_name)
        {
            let mut short_func = func;
            short_func.name = VarName::new(short_name);
            self.functions.insert(short_name.to_string(), short_func);
        }
    }

    /// Add a parameter value.
    pub fn add_parameter(&mut self, name: impl Into<String>, value: Value) {
        self.parameters.insert(name.into(), value);
    }

    /// Look up a variable/parameter by name.
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.parameters.get(name)
    }

    /// Look up an enum literal by qualified name.
    pub fn get_enum(&self, name: &str) -> Option<&(String, String)> {
        self.enum_literals.get(name)
    }
}

fn lookup_scoped<'a, T>(map: &'a IndexMap<String, T>, name: &str, scope: &str) -> Option<&'a T> {
    let mut current_scope = Some(scope);
    while let Some(scope_name) = current_scope {
        let candidate = if scope_name.is_empty() {
            name.to_string()
        } else {
            format!("{scope_name}.{name}")
        };
        if let Some(value) = map.get(&candidate) {
            return Some(value);
        }

        current_scope = scope_name.rsplit_once('.').map(|(parent, _)| parent);
        if current_scope.is_none() && !scope_name.is_empty() {
            current_scope = Some("");
        }
    }
    None
}

impl EvalLookup for EvalContext {
    fn lookup_integer(&self, name: &str, scope: &str) -> Option<i64> {
        lookup_scoped(&self.parameters, name, scope).and_then(Value::as_integer)
    }

    fn lookup_real(&self, name: &str, scope: &str) -> Option<f64> {
        lookup_scoped(&self.parameters, name, scope).and_then(Value::to_real)
    }

    fn lookup_boolean(&self, name: &str, scope: &str) -> Option<bool> {
        lookup_scoped(&self.parameters, name, scope).and_then(Value::as_bool)
    }

    fn lookup_enum<'a>(&'a self, name: &str, scope: &str) -> Option<Cow<'a, str>> {
        if let Some((type_name, literal)) = lookup_scoped(&self.enum_literals, name, scope) {
            return Some(Cow::Owned(format!("{type_name}.{literal}")));
        }
        lookup_scoped(&self.parameters, name, scope)
            .and_then(Value::as_enum)
            .map(|(type_name, literal)| Cow::Owned(format!("{type_name}.{literal}")))
    }
}

/// Evaluate a flat expression to a constant value.
///
/// Returns an error if the expression cannot be evaluated at compile time
/// (e.g., references time-varying variables, uses unsupported operations).
pub fn eval_expr(expr: &Expression, ctx: &EvalContext) -> Result<Value, EvalError> {
    eval_expr_with_span(expr, ctx, Span::DUMMY)
}

/// Evaluate with a span for error reporting.
pub fn eval_expr_with_span(
    expr: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    match expr {
        Expression::Literal(lit) => Ok(eval_literal(lit)),
        Expression::VarRef { name, subscripts } => {
            eval_var_ref(name.as_str(), subscripts, ctx, span)
        }
        Expression::Binary { op, lhs, rhs } => eval_flat_binary(op, lhs, rhs, ctx, span),
        Expression::Unary { op, rhs } => eval_flat_unary(op, rhs, ctx, span),
        Expression::BuiltinCall { function, args } => eval_builtin_call(function, args, ctx, span),
        Expression::FunctionCall { name, args, .. } => eval_fn_call(name.as_str(), args, ctx, span),
        Expression::If {
            branches,
            else_branch,
        } => eval_flat_if(branches, else_branch, ctx, span),
        Expression::Array { elements, .. } => eval_flat_array(elements, ctx, span),
        Expression::Range { start, step, end } => {
            eval_range(start, step.as_deref(), end, ctx, span)
        }
        Expression::ArrayComprehension { .. } => Err(EvalError::UnsupportedExpression {
            kind: "ArrayComprehension".to_string(),
            span,
        }),
        Expression::Index { base, subscripts } => eval_flat_index(base, subscripts, ctx, span),
        Expression::Tuple { elements } => eval_flat_array(elements, ctx, span),
        Expression::FieldAccess { base, field } => {
            // Field access on complex expressions (e.g., func().field)
            // requires evaluating the base and then extracting the field
            let base_val = eval_expr_with_span(base, ctx, span)?;
            eval_field_access(&base_val, field, span)
        }
        Expression::Empty => Ok(Value::Integer(0)),
    }
}

/// Evaluate a variable reference.
fn eval_var_ref(
    name: &str,
    subscripts: &[Subscript],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    // First try as a parameter
    if let Some(value) = ctx.get(name) {
        let value = value.clone();
        return if subscripts.is_empty() {
            Ok(value)
        } else {
            apply_subscripts(&value, subscripts, ctx, span)
        };
    }
    // Then try as an enum literal from context
    if let Some((type_name, literal)) = ctx.get_enum(name) {
        return Ok(Value::Enum(type_name.clone(), literal.clone()));
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
    let lhs_val = eval_expr_with_span(lhs, ctx, span)?;
    let rhs_val = eval_expr_with_span(rhs, ctx, span)?;
    eval_binary_op(op, &lhs_val, &rhs_val, span)
}

/// Evaluate a unary expression.
fn eval_flat_unary(
    op: &OpUnary,
    rhs: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let rhs_val = eval_expr_with_span(rhs, ctx, span)?;
    eval_unary_op(op, &rhs_val, span)
}

/// Evaluate a builtin call expression.
fn eval_builtin_call(
    function: &BuiltinFunction,
    args: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let arg_values: Vec<Value> = args
        .iter()
        .map(|a| eval_expr_with_span(a, ctx, span))
        .collect::<Result<_, _>>()?;
    eval_builtin_function(function, &arg_values, span)
}

/// Evaluate a function call expression.
fn eval_fn_call(
    name: &str,
    args: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    if is_builtin(name) {
        let arg_values: Vec<Value> = args
            .iter()
            .map(|a| eval_expr_with_span(a, ctx, span))
            .collect::<Result<_, _>>()?;
        return eval_builtin(name, &arg_values, span);
    }
    if let Some(func) = ctx.functions.get(name) {
        return eval_user_function(func, args, ctx, span);
    }
    Err(EvalError::not_constant(
        format!("unknown function: {}", name),
        span,
    ))
}

/// Evaluate a user-defined function.
fn eval_user_function(
    func: &Function,
    args: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    if !func.pure {
        return Err(EvalError::not_constant(
            format!("impure function: {}", func.name),
            span,
        ));
    }
    if func.external.is_some() {
        return Err(EvalError::not_constant(
            format!("external function: {}", func.name),
            span,
        ));
    }
    let mut arg_values: Vec<Value> = Vec::new();
    for a in args.iter() {
        match eval_expr_with_span(a, ctx, span) {
            Ok(v) => {
                arg_values.push(v);
            }
            Err(e) => {
                return Err(e);
            }
        }
    }
    eval_function(func, arg_values, ctx, &EvalLimits::default(), 0, span)
}

/// Evaluate an if expression.
fn eval_flat_if(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    for (cond, then_expr) in branches {
        let cond_val = eval_expr_with_span(cond, ctx, span)?;
        let is_true = cond_val
            .as_bool()
            .ok_or_else(|| EvalError::type_mismatch("Boolean", cond_val.type_name(), span))?;
        if is_true {
            return eval_expr_with_span(then_expr, ctx, span);
        }
    }
    eval_expr_with_span(else_branch, ctx, span)
}

/// Evaluate an array expression.
fn eval_flat_array(
    elements: &[Expression],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let values: Vec<Value> = elements
        .iter()
        .map(|e| eval_expr_with_span(e, ctx, span))
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
    let base_val = eval_expr_with_span(base, ctx, span)?;
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

/// Evaluate a range expression to an array.
fn eval_range(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let start_val = eval_expr_with_span(start, ctx, span)?;
    let end_val = eval_expr_with_span(end, ctx, span)?;

    // Determine if we have integer or real range
    match (start_val.as_integer(), end_val.as_integer()) {
        (Some(s), Some(e)) => eval_integer_range(s, e, step, ctx, span),
        _ => eval_real_range(&start_val, &end_val, step, ctx, span),
    }
}

/// Evaluate an integer range.
fn eval_integer_range(
    s: i64,
    e: i64,
    step: Option<&Expression>,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let step_int = match step {
        Some(step_expr) => {
            let step_val = eval_expr_with_span(step_expr, ctx, span)?;
            step_val
                .as_integer()
                .ok_or_else(|| EvalError::type_mismatch("Integer", step_val.type_name(), span))?
        }
        None => 1,
    };

    if step_int == 0 {
        return Err(EvalError::range_error("step cannot be zero", span));
    }

    let values = collect_int_range(s, e, step_int);
    Ok(Value::Array(values))
}

/// Collect integer range values.
fn collect_int_range(start: i64, end: i64, step: i64) -> Vec<Value> {
    let mut values = Vec::new();
    let mut i = start;
    if step > 0 {
        while i <= end {
            values.push(Value::Integer(i));
            i += step;
        }
    } else {
        while i >= end {
            values.push(Value::Integer(i));
            i += step;
        }
    }
    values
}

/// Evaluate a real range.
fn eval_real_range(
    start_val: &Value,
    end_val: &Value,
    step: Option<&Expression>,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let s = start_val
        .to_real()
        .ok_or_else(|| EvalError::type_mismatch("Real or Integer", start_val.type_name(), span))?;
    let e = end_val
        .to_real()
        .ok_or_else(|| EvalError::type_mismatch("Real or Integer", end_val.type_name(), span))?;

    let step_f = match step {
        Some(step_expr) => {
            let step_val = eval_expr_with_span(step_expr, ctx, span)?;
            step_val.to_real().ok_or_else(|| {
                EvalError::type_mismatch("Real or Integer", step_val.type_name(), span)
            })?
        }
        None => 1.0,
    };

    if step_f == 0.0 {
        return Err(EvalError::range_error("step cannot be zero", span));
    }

    let values = collect_real_range(s, e, step_f);
    Ok(Value::Array(values))
}

/// Collect real range values.
fn collect_real_range(start: f64, end: f64, step: f64) -> Vec<Value> {
    let mut values = Vec::new();
    let mut v = start;
    if step > 0.0 {
        while v <= end + f64::EPSILON {
            values.push(Value::Real(v));
            v += step;
        }
    } else {
        while v >= end - f64::EPSILON {
            values.push(Value::Real(v));
            v += step;
        }
    }
    values
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

/// Evaluate a binary operation.
fn eval_binary_op(op: &OpBinary, lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match op {
        OpBinary::Add(_) | OpBinary::AddElem(_) => eval_add(lhs, rhs, span),
        OpBinary::Sub(_) | OpBinary::SubElem(_) => eval_sub(lhs, rhs, span),
        // MLS array semantics: `*` is linear algebra multiply; `.*` is element-wise.
        OpBinary::Mul(_) => eval_mul(lhs, rhs, span),
        OpBinary::MulElem(_) => eval_mul_elem(lhs, rhs, span),
        OpBinary::Div(_) | OpBinary::DivElem(_) => eval_div(lhs, rhs, span),
        OpBinary::Exp(_) | OpBinary::ExpElem(_) => eval_exp(lhs, rhs, span),
        OpBinary::Eq(_) => eval_eq(lhs, rhs),
        OpBinary::Neq(_) => eval_neq(lhs, rhs),
        OpBinary::Lt(_) => eval_lt(lhs, rhs, span),
        OpBinary::Le(_) => eval_le(lhs, rhs, span),
        OpBinary::Gt(_) => eval_gt(lhs, rhs, span),
        OpBinary::Ge(_) => eval_ge(lhs, rhs, span),
        OpBinary::And(_) => eval_and(lhs, rhs, span),
        OpBinary::Or(_) => eval_or(lhs, rhs, span),
        OpBinary::Empty | OpBinary::Assign(_) => Err(EvalError::UnsupportedExpression {
            kind: format!("binary operator: {:?}", op),
            span,
        }),
    }
}

/// Evaluate a unary operation.
fn eval_unary_op(op: &OpUnary, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match op {
        OpUnary::Minus(_) | OpUnary::DotMinus(_) => eval_negate(rhs, span),
        OpUnary::Plus(_) | OpUnary::DotPlus(_) => Ok(rhs.clone()),
        OpUnary::Not(_) => eval_not(rhs, span),
        OpUnary::Empty => Ok(rhs.clone()),
    }
}

// Arithmetic operations

fn eval_add(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a + b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 + b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a + *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
        (Value::Array(a), Value::Array(b)) => {
            if a.len() != b.len() {
                return Err(EvalError::function_error(
                    format!("array size mismatch: {} vs {}", a.len(), b.len()),
                    span,
                ));
            }
            let result: Vec<Value> = a
                .iter()
                .zip(b.iter())
                .map(|(x, y)| eval_add(x, y, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        _ => Err(EvalError::type_mismatch(
            "numeric or array",
            format!("{} + {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_sub(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a - b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 - b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a - *b as f64)),
        (Value::Array(a), Value::Array(b)) => {
            if a.len() != b.len() {
                return Err(EvalError::function_error(
                    format!("array size mismatch: {} vs {}", a.len(), b.len()),
                    span,
                ));
            }
            let result: Vec<Value> = a
                .iter()
                .zip(b.iter())
                .map(|(x, y)| eval_sub(x, y, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        _ => Err(EvalError::type_mismatch(
            "numeric or array",
            format!("{} - {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_mul(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 * b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a * *b as f64)),
        // Scalar-array scaling is shared between `*` and `.*`.
        (Value::Integer(_) | Value::Real(_), Value::Array(_))
        | (Value::Array(_), Value::Integer(_) | Value::Real(_)) => eval_mul_elem(lhs, rhs, span),
        // Array-array `*` follows matrix/vector linear algebra semantics.
        (Value::Array(_), Value::Array(_)) => eval_matrix_mul(lhs, rhs, span),
        _ => Err(EvalError::type_mismatch(
            "numeric, vector, or matrix",
            format!("{} * {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_mul_elem(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 * b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a * *b as f64)),
        // Scalar * Array
        (Value::Integer(a), Value::Array(arr)) | (Value::Array(arr), Value::Integer(a)) => {
            let result: Vec<Value> = arr
                .iter()
                .map(|v| eval_mul_elem(&Value::Integer(*a), v, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        (Value::Real(a), Value::Array(arr)) | (Value::Array(arr), Value::Real(a)) => {
            let result: Vec<Value> = arr
                .iter()
                .map(|v| eval_mul_elem(&Value::Real(*a), v, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        // Element-wise array multiplication
        (Value::Array(a), Value::Array(b)) => {
            if a.len() != b.len() {
                return Err(EvalError::function_error(
                    format!("array size mismatch: {} vs {}", a.len(), b.len()),
                    span,
                ));
            }
            let result: Vec<Value> = a
                .iter()
                .zip(b.iter())
                .map(|(x, y)| eval_mul_elem(x, y, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        _ => Err(EvalError::type_mismatch(
            "numeric or array",
            format!("{} .* {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

enum ArrayShape<'a> {
    Vector(&'a [Value]),
    Matrix(Vec<&'a [Value]>),
}

fn eval_matrix_mul(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    let lhs_shape = classify_array_shape(lhs, span)?;
    let rhs_shape = classify_array_shape(rhs, span)?;
    match (lhs_shape, rhs_shape) {
        // Modelica vector * vector is dot-product.
        (ArrayShape::Vector(lhs_vec), ArrayShape::Vector(rhs_vec)) => {
            eval_dot_product(lhs_vec, rhs_vec, span)
        }
        // Matrix * vector => vector.
        (ArrayShape::Matrix(lhs_rows), ArrayShape::Vector(rhs_vec)) => {
            eval_matrix_vector_mul(&lhs_rows, rhs_vec, span)
        }
        // Vector * matrix => vector.
        (ArrayShape::Vector(lhs_vec), ArrayShape::Matrix(rhs_rows)) => {
            eval_vector_matrix_mul(lhs_vec, &rhs_rows, span)
        }
        // Matrix * matrix => matrix.
        (ArrayShape::Matrix(lhs_rows), ArrayShape::Matrix(rhs_rows)) => {
            eval_matrix_matrix_mul(&lhs_rows, &rhs_rows, span)
        }
    }
}

fn classify_array_shape<'a>(value: &'a Value, span: Span) -> Result<ArrayShape<'a>, EvalError> {
    let Value::Array(elements) = value else {
        return Err(EvalError::type_mismatch("Array", value.type_name(), span));
    };

    if elements.iter().all(|item| !matches!(item, Value::Array(_))) {
        return Ok(ArrayShape::Vector(elements.as_slice()));
    }

    if elements.iter().any(|item| !matches!(item, Value::Array(_))) {
        return Err(EvalError::function_error(
            "mixed-rank arrays are not valid matrix operands".to_string(),
            span,
        ));
    }

    let mut rows = Vec::with_capacity(elements.len());
    let mut n_cols: Option<usize> = None;
    for row in elements {
        let Value::Array(row_elems) = row else {
            unreachable!("guarded above");
        };
        if row_elems.iter().any(|item| matches!(item, Value::Array(_))) {
            return Err(EvalError::function_error(
                "rank > 2 arrays are not supported in matrix multiplication".to_string(),
                span,
            ));
        }
        match n_cols {
            Some(expected) if expected != row_elems.len() => {
                return Err(EvalError::function_error(
                    "matrix rows must have the same length".to_string(),
                    span,
                ));
            }
            None => {
                n_cols = Some(row_elems.len());
            }
            _ => {}
        }
        rows.push(row_elems.as_slice());
    }

    Ok(ArrayShape::Matrix(rows))
}

fn eval_dot_product(lhs_vec: &[Value], rhs_vec: &[Value], span: Span) -> Result<Value, EvalError> {
    if lhs_vec.len() != rhs_vec.len() {
        return Err(EvalError::function_error(
            format!(
                "vector dot-product size mismatch: {} vs {}",
                lhs_vec.len(),
                rhs_vec.len()
            ),
            span,
        ));
    }

    let mut acc = Value::Integer(0);
    for (lhs, rhs) in lhs_vec.iter().zip(rhs_vec.iter()) {
        let product = eval_numeric_mul(lhs, rhs, span)?;
        acc = eval_add(&acc, &product, span)?;
    }
    Ok(acc)
}

fn eval_matrix_vector_mul(
    lhs_rows: &[&[Value]],
    rhs_vec: &[Value],
    span: Span,
) -> Result<Value, EvalError> {
    let lhs_cols = lhs_rows.first().map_or(0, |row| row.len());
    if lhs_cols != rhs_vec.len() {
        return Err(EvalError::function_error(
            format!(
                "matrix-vector size mismatch: left cols {} vs right size {}",
                lhs_cols,
                rhs_vec.len()
            ),
            span,
        ));
    }

    let mut result = Vec::with_capacity(lhs_rows.len());
    for row in lhs_rows {
        result.push(eval_dot_product(row, rhs_vec, span)?);
    }
    Ok(Value::Array(result))
}

fn eval_vector_matrix_mul(
    lhs_vec: &[Value],
    rhs_rows: &[&[Value]],
    span: Span,
) -> Result<Value, EvalError> {
    if rhs_rows.is_empty() {
        return Ok(Value::Array(Vec::new()));
    }

    if lhs_vec.len() != rhs_rows.len() {
        return Err(EvalError::function_error(
            format!(
                "vector-matrix size mismatch: left size {} vs right rows {}",
                lhs_vec.len(),
                rhs_rows.len()
            ),
            span,
        ));
    }

    let rhs_cols = rhs_rows[0].len();
    let mut out = Vec::with_capacity(rhs_cols);
    for (col, _) in rhs_rows[0].iter().enumerate() {
        let mut acc = Value::Integer(0);
        for (lhs_val, rhs_row) in lhs_vec.iter().zip(rhs_rows.iter()) {
            let product = eval_numeric_mul(lhs_val, &rhs_row[col], span)?;
            acc = eval_add(&acc, &product, span)?;
        }
        out.push(acc);
    }
    Ok(Value::Array(out))
}

fn eval_matrix_matrix_mul(
    lhs_rows: &[&[Value]],
    rhs_rows: &[&[Value]],
    span: Span,
) -> Result<Value, EvalError> {
    if rhs_rows.is_empty() {
        return Ok(Value::Array(Vec::new()));
    }

    let lhs_cols = lhs_rows.first().map_or(0, |row| row.len());
    let rhs_rows_count = rhs_rows.len();
    let rhs_cols = rhs_rows[0].len();

    if lhs_cols != rhs_rows_count {
        return Err(EvalError::function_error(
            format!(
                "matrix-matrix size mismatch: left cols {} vs right rows {}",
                lhs_cols, rhs_rows_count
            ),
            span,
        ));
    }

    let mut result_rows = Vec::with_capacity(lhs_rows.len());
    for lhs_row in lhs_rows {
        let mut out_row = Vec::with_capacity(rhs_cols);
        for (col, _) in rhs_rows[0].iter().enumerate() {
            let mut acc = Value::Integer(0);
            for (lhs_val, rhs_row) in lhs_row.iter().zip(rhs_rows.iter()) {
                let product = eval_numeric_mul(lhs_val, &rhs_row[col], span)?;
                acc = eval_add(&acc, &product, span)?;
            }
            out_row.push(acc);
        }
        result_rows.push(Value::Array(out_row));
    }
    Ok(Value::Array(result_rows))
}

fn eval_numeric_mul(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 * b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a * *b as f64)),
        _ => Err(EvalError::type_mismatch(
            "numeric scalar",
            format!("{} * {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_div(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => {
            if *b == 0 {
                return Err(EvalError::DivisionByZero { span });
            }
            // Integer division in Modelica produces Real
            Ok(Value::Real(*a as f64 / *b as f64))
        }
        (Value::Real(a), Value::Real(b)) => {
            if *b == 0.0 {
                return Err(EvalError::DivisionByZero { span });
            }
            Ok(Value::Real(a / b))
        }
        (Value::Integer(a), Value::Real(b)) => {
            if *b == 0.0 {
                return Err(EvalError::DivisionByZero { span });
            }
            Ok(Value::Real(*a as f64 / b))
        }
        (Value::Real(a), Value::Integer(b)) => {
            if *b == 0 {
                return Err(EvalError::DivisionByZero { span });
            }
            Ok(Value::Real(a / *b as f64))
        }
        (Value::Array(a), Value::Array(b)) => {
            if a.len() != b.len() {
                return Err(EvalError::function_error(
                    format!("array size mismatch: {} vs {}", a.len(), b.len()),
                    span,
                ));
            }
            let result: Vec<Value> = a
                .iter()
                .zip(b.iter())
                .map(|(x, y)| eval_div(x, y, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        _ => Err(EvalError::type_mismatch(
            "numeric",
            format!("{} / {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_exp(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => {
            if *b >= 0 {
                Ok(Value::Integer(a.pow(*b as u32)))
            } else {
                Ok(Value::Real((*a as f64).powf(*b as f64)))
            }
        }
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a.powf(*b))),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real((*a as f64).powf(*b))),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a.powi(*b as i32))),
        _ => Err(EvalError::type_mismatch(
            "numeric",
            format!("{} ^ {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_negate(v: &Value, span: Span) -> Result<Value, EvalError> {
    match v {
        Value::Integer(x) => Ok(Value::Integer(-x)),
        Value::Real(x) => Ok(Value::Real(-x)),
        Value::Array(arr) => {
            let result: Vec<Value> = arr
                .iter()
                .map(|x| eval_negate(x, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        _ => Err(EvalError::type_mismatch("numeric", v.type_name(), span)),
    }
}

// Comparison operations

fn eval_eq(lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    // Handle mixed Integer/Real comparisons
    match (lhs, rhs) {
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) == *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a == (*b as f64))),
        _ => Ok(Value::Bool(lhs == rhs)),
    }
}

fn eval_neq(lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    // Handle mixed Integer/Real comparisons
    match (lhs, rhs) {
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) != *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a != (*b as f64))),
        _ => Ok(Value::Bool(lhs != rhs)),
    }
}

fn eval_lt(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a < b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a < b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) < *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a < *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a < b)),
        _ => Err(EvalError::type_mismatch(
            "comparable",
            format!("{} < {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_le(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a <= b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a <= b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) <= *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a <= *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a <= b)),
        _ => Err(EvalError::type_mismatch(
            "comparable",
            format!("{} <= {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_gt(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a > b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a > b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) > *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a > *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a > b)),
        _ => Err(EvalError::type_mismatch(
            "comparable",
            format!("{} > {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_ge(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a >= b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a >= b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) >= *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a >= *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a >= b)),
        _ => Err(EvalError::type_mismatch(
            "comparable",
            format!("{} >= {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

// Logical operations

fn eval_and(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    let a = lhs
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", lhs.type_name(), span))?;
    let b = rhs
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", rhs.type_name(), span))?;
    Ok(Value::Bool(a && b))
}

fn eval_or(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    let a = lhs
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", lhs.type_name(), span))?;
    let b = rhs
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", rhs.type_name(), span))?;
    Ok(Value::Bool(a || b))
}

fn eval_not(v: &Value, span: Span) -> Result<Value, EvalError> {
    let b = v
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", v.type_name(), span))?;
    Ok(Value::Bool(!b))
}

/// Evaluate a builtin function call.
fn eval_builtin_function(
    func: &BuiltinFunction,
    args: &[Value],
    span: Span,
) -> Result<Value, EvalError> {
    match func {
        // Math functions
        BuiltinFunction::Abs => eval_builtin("abs", args, span),
        BuiltinFunction::Sign => eval_builtin("sign", args, span),
        BuiltinFunction::Sqrt => eval_builtin("sqrt", args, span),
        BuiltinFunction::Div => eval_builtin("div", args, span),
        BuiltinFunction::Mod => eval_builtin("mod", args, span),
        BuiltinFunction::Rem => eval_builtin("rem", args, span),
        BuiltinFunction::Floor => eval_builtin("floor", args, span),
        BuiltinFunction::Ceil => eval_builtin("ceil", args, span),
        BuiltinFunction::Min => eval_builtin("min", args, span),
        BuiltinFunction::Max => eval_builtin("max", args, span),

        // Trig functions
        BuiltinFunction::Sin => eval_builtin("sin", args, span),
        BuiltinFunction::Cos => eval_builtin("cos", args, span),
        BuiltinFunction::Tan => eval_builtin("tan", args, span),
        BuiltinFunction::Asin => eval_builtin("asin", args, span),
        BuiltinFunction::Acos => eval_builtin("acos", args, span),
        BuiltinFunction::Atan => eval_builtin("atan", args, span),
        BuiltinFunction::Atan2 => eval_builtin("atan2", args, span),
        BuiltinFunction::Sinh => eval_builtin("sinh", args, span),
        BuiltinFunction::Cosh => eval_builtin("cosh", args, span),
        BuiltinFunction::Tanh => eval_builtin("tanh", args, span),

        // Exp/log
        BuiltinFunction::Exp => eval_builtin("exp", args, span),
        BuiltinFunction::Log => eval_builtin("log", args, span),
        BuiltinFunction::Log10 => eval_builtin("log10", args, span),

        // Array functions
        BuiltinFunction::Size => eval_builtin("size", args, span),
        BuiltinFunction::Ndims => eval_builtin("ndims", args, span),
        BuiltinFunction::Sum => eval_builtin("sum", args, span),
        BuiltinFunction::Product => eval_builtin("product", args, span),
        BuiltinFunction::Zeros => eval_builtin("zeros", args, span),
        BuiltinFunction::Ones => eval_builtin("ones", args, span),
        BuiltinFunction::Fill => eval_builtin("fill", args, span),
        BuiltinFunction::Linspace => eval_builtin("linspace", args, span),
        BuiltinFunction::Cat => eval_builtin("cat", args, span),

        // Pass-through builtins
        BuiltinFunction::NoEvent => args.first().cloned().ok_or_else(|| {
            EvalError::not_constant("noEvent requires 1 argument".to_string(), span)
        }),
        BuiltinFunction::Smooth => args.get(1).cloned().ok_or_else(|| {
            EvalError::not_constant("smooth requires 2 arguments".to_string(), span)
        }),
        BuiltinFunction::Homotopy => args.first().cloned().ok_or_else(|| {
            EvalError::not_constant("homotopy requires 1 argument".to_string(), span)
        }),
        BuiltinFunction::Delay => args
            .first()
            .cloned()
            .ok_or_else(|| EvalError::not_constant("delay requires 1 argument".to_string(), span)),
        BuiltinFunction::Integer => eval_builtin("floor", args, span),
        BuiltinFunction::SemiLinear => eval_builtin("semiLinear", args, span),

        // These are runtime-only functions
        BuiltinFunction::Der
        | BuiltinFunction::Pre
        | BuiltinFunction::Edge
        | BuiltinFunction::Change
        | BuiltinFunction::Reinit
        | BuiltinFunction::Sample
        | BuiltinFunction::Initial
        | BuiltinFunction::Terminal => Err(EvalError::not_constant(
            format!("runtime function: {:?}", func),
            span,
        )),

        // Other array/matrix functions that need more work
        BuiltinFunction::Scalar
        | BuiltinFunction::Vector
        | BuiltinFunction::Matrix
        | BuiltinFunction::Identity
        | BuiltinFunction::Diagonal
        | BuiltinFunction::Transpose
        | BuiltinFunction::OuterProduct
        | BuiltinFunction::Symmetric
        | BuiltinFunction::Cross
        | BuiltinFunction::Skew => Err(EvalError::UnsupportedExpression {
            kind: format!("matrix function: {:?}", func),
            span,
        }),
    }
}

/// Apply subscripts to a value.
fn apply_subscripts(
    value: &Value,
    subscripts: &[Subscript],
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let mut current = value.clone();

    for subscript in subscripts {
        match subscript {
            Subscript::Index(idx) => {
                let idx = *idx as usize;

                let arr = current
                    .as_array()
                    .ok_or_else(|| EvalError::type_mismatch("Array", current.type_name(), span))?;

                // Modelica uses 1-based indexing
                if idx < 1 || idx > arr.len() {
                    return Err(EvalError::IndexOutOfBounds {
                        index: idx as i64,
                        size: arr.len(),
                        span,
                    });
                }
                current = arr[idx - 1].clone();
            }
            Subscript::Colon => {
                // Colon means "all elements" - just pass through
                // (this is a simplification; real slicing would need more work)
            }
            Subscript::Expr(expr) => {
                // Evaluate the expression to get the index
                let idx_val = eval_expr_with_span(expr, ctx, span)?;
                let idx = idx_val
                    .as_integer()
                    .ok_or_else(|| EvalError::type_mismatch("Integer", idx_val.type_name(), span))?
                    as usize;

                let arr = current
                    .as_array()
                    .ok_or_else(|| EvalError::type_mismatch("Array", current.type_name(), span))?;

                // Modelica uses 1-based indexing
                if idx < 1 || idx > arr.len() {
                    return Err(EvalError::IndexOutOfBounds {
                        index: idx as i64,
                        size: arr.len(),
                        span,
                    });
                }
                current = arr[idx - 1].clone();
            }
        }
    }

    Ok(current)
}

/// Try to evaluate an expression to an integer.
/// Returns None if evaluation fails or result is not an integer.
pub fn try_eval_integer(expr: &Expression, ctx: &EvalContext) -> Option<i64> {
    eval_expr(expr, ctx).ok().and_then(|v| v.as_integer())
}

/// Try to evaluate an expression to a real.
/// Returns None if evaluation fails or result is not numeric.
pub fn try_eval_real(expr: &Expression, ctx: &EvalContext) -> Option<f64> {
    eval_expr(expr, ctx).ok().and_then(|v| v.to_real())
}

/// Try to evaluate an expression to a boolean.
/// Returns None if evaluation fails or result is not a boolean.
pub fn try_eval_bool(expr: &Expression, ctx: &EvalContext) -> Option<bool> {
    eval_expr(expr, ctx).ok().and_then(|v| v.as_bool())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_int(v: i64) -> Expression {
        Expression::Literal(Literal::Integer(v))
    }

    fn make_real(v: f64) -> Expression {
        Expression::Literal(Literal::Real(v))
    }

    fn make_bool(v: bool) -> Expression {
        Expression::Literal(Literal::Boolean(v))
    }

    fn make_vector(values: &[i64]) -> Expression {
        Expression::Array {
            elements: values.iter().map(|v| make_int(*v)).collect(),
            is_matrix: false,
        }
    }

    fn make_matrix(rows: &[&[i64]]) -> Expression {
        Expression::Array {
            elements: rows
                .iter()
                .map(|row| Expression::Array {
                    elements: row.iter().map(|v| make_int(*v)).collect(),
                    is_matrix: false,
                })
                .collect(),
            is_matrix: true,
        }
    }

    #[test]
    fn test_eval_literal() {
        let ctx = EvalContext::new();

        let expr = make_int(42);
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_integer(), Some(42));

        let expr = make_real(2.5);
        let result = eval_expr(&expr, &ctx).unwrap();
        assert!((result.as_real().unwrap() - 2.5).abs() < 1e-10);

        let expr = make_bool(true);
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_bool(), Some(true));
    }

    #[test]
    fn test_eval_binary() {
        let ctx = EvalContext::new();

        // 3 + 4 = 7
        let expr = Expression::Binary {
            op: OpBinary::Add(Default::default()),
            lhs: Box::new(make_int(3)),
            rhs: Box::new(make_int(4)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_integer(), Some(7));

        // 10 - 3 = 7
        let expr = Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(make_int(10)),
            rhs: Box::new(make_int(3)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_integer(), Some(7));

        // 3 * 4 = 12
        let expr = Expression::Binary {
            op: OpBinary::Mul(Default::default()),
            lhs: Box::new(make_int(3)),
            rhs: Box::new(make_int(4)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_integer(), Some(12));

        // 10 / 4 = 2.5 (Real result in Modelica)
        let expr = Expression::Binary {
            op: OpBinary::Div(Default::default()),
            lhs: Box::new(make_int(10)),
            rhs: Box::new(make_int(4)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert!((result.as_real().unwrap() - 2.5).abs() < 1e-10);
    }

    #[test]
    fn test_eval_mul_vs_mul_elem_vector_semantics() {
        let ctx = EvalContext::new();
        let lhs = make_vector(&[1, 2, 3]);
        let rhs = make_vector(&[4, 5, 6]);

        // `*` performs dot-product on vectors.
        let mul_expr = Expression::Binary {
            op: OpBinary::Mul(Default::default()),
            lhs: Box::new(lhs.clone()),
            rhs: Box::new(rhs.clone()),
        };
        let mul_result = eval_expr(&mul_expr, &ctx).unwrap();
        assert_eq!(mul_result, Value::Integer(32));

        // `.*` keeps element-wise vector semantics.
        let mul_elem_expr = Expression::Binary {
            op: OpBinary::MulElem(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };
        let mul_elem_result = eval_expr(&mul_elem_expr, &ctx).unwrap();
        assert_eq!(
            mul_elem_result,
            Value::Array(vec![
                Value::Integer(4),
                Value::Integer(10),
                Value::Integer(18)
            ])
        );
    }

    #[test]
    fn test_eval_matrix_multiplication_semantics() {
        let ctx = EvalContext::new();

        // [[1,2],[3,4]] * [[5,6],[7,8]] = [[19,22],[43,50]]
        let lhs_matrix = make_matrix(&[&[1, 2], &[3, 4]]);
        let rhs_matrix = make_matrix(&[&[5, 6], &[7, 8]]);
        let matrix_mul_expr = Expression::Binary {
            op: OpBinary::Mul(Default::default()),
            lhs: Box::new(lhs_matrix.clone()),
            rhs: Box::new(rhs_matrix.clone()),
        };
        let matrix_mul_result = eval_expr(&matrix_mul_expr, &ctx).unwrap();
        assert_eq!(
            matrix_mul_result,
            Value::Array(vec![
                Value::Array(vec![Value::Integer(19), Value::Integer(22)]),
                Value::Array(vec![Value::Integer(43), Value::Integer(50)])
            ])
        );

        // Element-wise matrix multiply remains shape-preserving.
        let matrix_mul_elem_expr = Expression::Binary {
            op: OpBinary::MulElem(Default::default()),
            lhs: Box::new(lhs_matrix),
            rhs: Box::new(rhs_matrix),
        };
        let matrix_mul_elem_result = eval_expr(&matrix_mul_elem_expr, &ctx).unwrap();
        assert_eq!(
            matrix_mul_elem_result,
            Value::Array(vec![
                Value::Array(vec![Value::Integer(5), Value::Integer(12)]),
                Value::Array(vec![Value::Integer(21), Value::Integer(32)])
            ])
        );
    }

    #[test]
    fn test_eval_comparison() {
        let ctx = EvalContext::new();

        // 3 < 4 = true
        let expr = Expression::Binary {
            op: OpBinary::Lt(Default::default()),
            lhs: Box::new(make_int(3)),
            rhs: Box::new(make_int(4)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_bool(), Some(true));

        // 3 == 3 = true
        let expr = Expression::Binary {
            op: OpBinary::Eq(Default::default()),
            lhs: Box::new(make_int(3)),
            rhs: Box::new(make_int(3)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_bool(), Some(true));
    }

    #[test]
    fn test_eval_unary() {
        let ctx = EvalContext::new();

        // -5
        let expr = Expression::Unary {
            op: OpUnary::Minus(Default::default()),
            rhs: Box::new(make_int(5)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_integer(), Some(-5));

        // not true = false
        let expr = Expression::Unary {
            op: OpUnary::Not(Default::default()),
            rhs: Box::new(make_bool(true)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_bool(), Some(false));
    }

    #[test]
    fn test_eval_array() {
        let ctx = EvalContext::new();

        let expr = Expression::Array {
            elements: vec![make_int(1), make_int(2), make_int(3)],
            is_matrix: false,
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        let arr = result.as_array().unwrap();
        assert_eq!(arr.len(), 3);
        assert_eq!(arr[0].as_integer(), Some(1));
        assert_eq!(arr[2].as_integer(), Some(3));
    }

    #[test]
    fn test_eval_range() {
        let ctx = EvalContext::new();

        // 1:5 = {1, 2, 3, 4, 5}
        let expr = Expression::Range {
            start: Box::new(make_int(1)),
            step: None,
            end: Box::new(make_int(5)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        let arr = result.as_array().unwrap();
        assert_eq!(arr.len(), 5);
        assert_eq!(arr[0].as_integer(), Some(1));
        assert_eq!(arr[4].as_integer(), Some(5));

        // 1:2:5 = {1, 3, 5}
        let expr = Expression::Range {
            start: Box::new(make_int(1)),
            step: Some(Box::new(make_int(2))),
            end: Box::new(make_int(5)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        let arr = result.as_array().unwrap();
        assert_eq!(arr.len(), 3);
        assert_eq!(arr[0].as_integer(), Some(1));
        assert_eq!(arr[1].as_integer(), Some(3));
        assert_eq!(arr[2].as_integer(), Some(5));
    }

    #[test]
    fn test_eval_if() {
        let ctx = EvalContext::new();

        // if true then 1 else 2
        let expr = Expression::If {
            branches: vec![(make_bool(true), make_int(1))],
            else_branch: Box::new(make_int(2)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_integer(), Some(1));

        // if false then 1 else 2
        let expr = Expression::If {
            branches: vec![(make_bool(false), make_int(1))],
            else_branch: Box::new(make_int(2)),
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_integer(), Some(2));
    }

    #[test]
    fn test_eval_parameter() {
        let mut ctx = EvalContext::new();
        ctx.add_parameter("n", Value::Integer(10));
        ctx.add_parameter("x", Value::Real(2.5));

        let expr = Expression::VarRef {
            name: "n".into(),
            subscripts: vec![],
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_integer(), Some(10));

        let expr = Expression::VarRef {
            name: "x".into(),
            subscripts: vec![],
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert!((result.as_real().unwrap() - 2.5).abs() < 1e-10);
    }

    #[test]
    fn test_eval_builtin_call() {
        let ctx = EvalContext::new();

        // abs(-5) = 5
        let expr = Expression::BuiltinCall {
            function: BuiltinFunction::Abs,
            args: vec![make_int(-5)],
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert_eq!(result.as_integer(), Some(5));

        // sqrt(4.0) = 2.0
        let expr = Expression::BuiltinCall {
            function: BuiltinFunction::Sqrt,
            args: vec![make_real(4.0)],
        };
        let result = eval_expr(&expr, &ctx).unwrap();
        assert!((result.as_real().unwrap() - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_try_eval_helpers() {
        let mut ctx = EvalContext::new();
        ctx.add_parameter("n", Value::Integer(5));

        let expr = Expression::VarRef {
            name: "n".into(),
            subscripts: vec![],
        };

        assert_eq!(try_eval_integer(&expr, &ctx), Some(5));
        assert_eq!(try_eval_real(&expr, &ctx), Some(5.0));
        assert_eq!(try_eval_bool(&expr, &ctx), None);
    }

    #[test]
    fn test_eval_lookup_trait_resolves_scoped_values() {
        let mut ctx = EvalContext::new();
        ctx.add_parameter("sys.n", Value::Integer(5));
        ctx.add_parameter("sys.inner.pi", Value::Real(3.0));
        ctx.add_parameter("sys.flag", Value::Bool(true));
        ctx.enum_literals.insert(
            "sys.mode".to_string(),
            ("Modes".to_string(), "Fast".to_string()),
        );

        assert_eq!(ctx.lookup_integer("n", "sys.inner"), Some(5));
        assert_eq!(ctx.lookup_real("pi", "sys.inner"), Some(3.0));
        assert_eq!(ctx.lookup_boolean("flag", "sys.inner"), Some(true));
        assert_eq!(
            ctx.lookup_enum("mode", "sys.inner").as_deref(),
            Some("Modes.Fast")
        );
    }
}
