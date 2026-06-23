use super::*;
use rumoca_core::ExpressionVisitor;
use std::fmt::Write as _;

mod builtin_eval;
pub(super) use builtin_eval::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    MissingBinding {
        name: String,
    },
    MissingFunction {
        name: String,
    },
    UnsupportedExpression {
        kind: &'static str,
    },
    ShapeMismatch {
        context: &'static str,
        expected: usize,
        actual: usize,
    },
    StatementIterationLimit {
        statement: &'static str,
        max_iterations: usize,
    },
    ShortRuntimeVector {
        vector: &'static str,
        expected: usize,
        actual: usize,
    },
    Spanned {
        source: Box<EvalError>,
        span: rumoca_core::Span,
    },
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingBinding { name } => write!(f, "missing binding for `{name}`"),
            Self::MissingFunction { name } => write!(f, "missing function `{name}`"),
            Self::UnsupportedExpression { kind } => {
                write!(f, "unsupported expression in DAE evaluation: {kind}")
            }
            Self::ShapeMismatch {
                context,
                expected,
                actual,
            } => write!(
                f,
                "shape mismatch in DAE evaluation for {context}: expected {expected} value(s), got {actual}"
            ),
            Self::StatementIterationLimit {
                statement,
                max_iterations,
            } => write!(
                f,
                "{statement} statement exceeded DAE evaluation iteration limit of {max_iterations}"
            ),
            Self::ShortRuntimeVector {
                vector,
                expected,
                actual,
            } => write!(
                f,
                "short DAE runtime vector `{vector}`: expected at least {expected} value(s), got {actual}"
            ),
            Self::Spanned { source, .. } => source.fmt(f),
        }
    }
}

impl std::error::Error for EvalError {}

impl EvalError {
    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::Spanned { source, span } => source
                .source_span()
                .or_else(|| (!span.is_dummy()).then_some(*span)),
            _ => None,
        }
    }

    pub fn with_fallback_span(self, span: rumoca_core::Span) -> Self {
        if span.is_dummy() || self.source_span().is_some() {
            return self;
        }
        Self::Spanned {
            source: Box::new(self),
            span,
        }
    }

    pub fn missing_binding_name(&self) -> Option<&str> {
        match self {
            Self::MissingBinding { name } => Some(name.as_str()),
            Self::Spanned { source, .. } => source.missing_binding_name(),
            _ => None,
        }
    }
}

/// Evaluate a scalar expression, returning an error for missing bindings or
/// expression forms that the DAE evaluator cannot safely reduce to a scalar.
pub fn eval_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    validate_expr(expr, env)?;
    Ok(eval_expr_or_default(expr, env))
}

/// Evaluate a rumoca_core::Expression to a value of type T.
pub(crate) fn eval_expr_or_default<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> T {
    match expr {
        rumoca_core::Expression::Literal { value: lit, .. } => eval_literal::<T>(lit),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => eval_var_ref::<T>(name.var_name(), subscripts, env),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => eval_binary::<T>(op, lhs, rhs, env),
        rumoca_core::Expression::Unary { op, rhs, .. } => eval_unary::<T>(op, rhs, env),
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            eval_builtin::<T>(*function, args, env)
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => eval_function_call::<T>(name.var_name(), args, *is_constructor, env),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => eval_if::<T>(branches, else_branch, env),
        rumoca_core::Expression::Array { elements, .. } => {
            if let Some(first) = elements.first() {
                eval_expr_or_default::<T>(first, env)
            } else {
                T::zero()
            }
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => eval_index_expr::<T>(base, subscripts, env),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            eval_field_access::<T>(base, field, env)
        }
        rumoca_core::Expression::Empty { .. } => T::zero(),
        rumoca_core::Expression::Range { .. }
        | rumoca_core::Expression::Tuple { .. }
        | rumoca_core::Expression::ArrayComprehension { .. } => T::zero(),
    }
}

fn validate_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    match expr {
        rumoca_core::Expression::Literal { value, .. } => validate_literal(value),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            for subscript in subscripts {
                if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
                    if matches!(expr.as_ref(), rumoca_core::Expression::Range { .. }) {
                        validate_array_argument(expr, env)?;
                    } else {
                        validate_expr(expr, env)?;
                    }
                }
            }
            try_eval_var_ref(name.var_name(), subscripts, env).map(|_| ())
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            validate_binary_expr(op, lhs, rhs, env)
        }
        rumoca_core::Expression::Unary { rhs, .. } => validate_expr(rhs, env),
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            validate_builtin_call(*function, args, env)
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => {
            if let Some(result) = validate_external_table_call(name.as_str(), args, env) {
                return result;
            }
            if *is_constructor || name.as_str() == "Complex" {
                return validate_expr_slice_checked(args, env);
            }
            if is_runtime_special_function_name(name.as_str()) {
                // Runtime special functions own aggregate argument semantics
                // such as table matrices and clock constructors. The scalar
                // validator cannot recursively validate those arguments
                // without rejecting valid non-scalar Modelica expressions.
                return Ok(());
            }
            if let Some(function) = env.functions.get(name.as_str()) {
                return validate_user_function_call_args(name.as_str(), function, args, env);
            }
            validate_expr_slice_checked(args, env)?;
            function_call_supported(name.as_str(), env)
                .then_some(())
                .ok_or_else(|| EvalError::MissingFunction {
                    name: name.to_string(),
                })
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, value) in branches {
                validate_expr(cond, env)?;
                validate_expr(value, env)?;
            }
            validate_expr(else_branch, env)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => try_eval_index_expr(base, subscripts, env).map(|_| ()),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            try_eval_field_access(base, field, env).map(|_| ())
        }
        rumoca_core::Expression::Empty { .. } => {
            Err(EvalError::UnsupportedExpression { kind: "empty" })
        }
        rumoca_core::Expression::Array { .. } => {
            Err(EvalError::UnsupportedExpression { kind: "array" })
        }
        rumoca_core::Expression::Range { .. } => {
            Err(EvalError::UnsupportedExpression { kind: "range" })
        }
        rumoca_core::Expression::Tuple { .. } => {
            Err(EvalError::UnsupportedExpression { kind: "tuple" })
        }
        rumoca_core::Expression::ArrayComprehension { .. } => {
            Err(EvalError::UnsupportedExpression {
                kind: "array comprehension",
            })
        }
    }
}

fn validate_literal(lit: &rumoca_core::Literal) -> Result<(), EvalError> {
    match lit {
        rumoca_core::Literal::Real(_)
        | rumoca_core::Literal::Integer(_)
        | rumoca_core::Literal::Boolean(_) => Ok(()),
        rumoca_core::Literal::String(_) => Err(EvalError::UnsupportedExpression {
            kind: "string literal",
        }),
    }
}

fn validate_expr_slice_checked<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    for arg in args {
        validate_expr(arg, env)?;
    }
    Ok(())
}

fn validate_binary_expr<T: SimFloat>(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    if matches!(
        op,
        rumoca_core::OpBinary::Empty | rumoca_core::OpBinary::Assign
    ) {
        return Err(EvalError::UnsupportedExpression {
            kind: "placeholder binary operator",
        });
    }
    if matches!(op, rumoca_core::OpBinary::Mul) && validate_scalar_array_product(lhs, rhs, env)? {
        return Ok(());
    }
    validate_expr(lhs, env)?;
    validate_expr(rhs, env)
}

fn validate_scalar_array_product<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    validate_array_argument(lhs, env)?;
    validate_array_argument(rhs, env)?;
    if eval_vector_dot_product(lhs, rhs, env).is_some() {
        return Ok(true);
    }
    Ok(
        eval_binary_array_values(&rumoca_core::OpBinary::Mul, lhs, rhs, env)
            .is_some_and(|values| values.len() == 1),
    )
}

fn validate_builtin_call<T: SimFloat>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    if matches!(function, rumoca_core::BuiltinFunction::Size) {
        return validate_size_call(args, env);
    }
    if args.len() == 1 && builtin_accepts_array_argument(function) {
        return validate_array_argument(&args[0], env);
    }
    validate_expr_slice_checked(args, env)
}

fn validate_size_call<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let Some(array_arg) = args.first() else {
        return Ok(());
    };
    if size_arg_has_known_shape(array_arg, env) {
        return validate_expr_slice_checked(&args[1..], env);
    }
    if let rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: fill_args,
        ..
    } = array_arg
        && fill_args.len() >= 2
    {
        validate_expr_slice_checked(&fill_args[1..], env)?;
        return validate_expr_slice_checked(&args[1..], env);
    }
    validate_array_argument(array_arg, env)?;
    validate_expr_slice_checked(&args[1..], env)
}

fn size_arg_has_known_shape<T: SimFloat>(expr: &rumoca_core::Expression, env: &VarEnv<T>) -> bool {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() && env.dims.contains_key(name.as_str()) => true,
        rumoca_core::Expression::Array { .. } => true,
        _ => false,
    }
}

fn validate_external_table_call<T: SimFloat>(
    name: &str,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<Result<(), EvalError>> {
    let short_name = rumoca_core::top_level_last_segment(name);
    match short_name {
        "ExternalCombiTimeTable" => Some(validate_external_table_constructor(args, env, true)),
        "ExternalCombiTable1D" => Some(validate_external_table_constructor(args, env, false)),
        "getTimeTableTmax"
        | "getTimeTableTmin"
        | "getTable1DAbscissaUmax"
        | "getTable1DAbscissaUmin" => Some(validate_external_table_bound_call(args, env)),
        "getTimeTableValueNoDer"
        | "getTimeTableValueNoDer2"
        | "getTimeTableValue"
        | "getTable1DValueNoDer"
        | "getTable1DValueNoDer2"
        | "getTable1DValue" => Some(validate_external_table_lookup_call(args, env)),
        "getNextTimeEvent" => Some(validate_external_table_next_event_call(args, env)),
        _ => None,
    }
}

fn validate_external_table_constructor<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
    is_time_table: bool,
) -> Result<(), EvalError> {
    let table_arg = external_table_constructor_arg(args, "table", 2).ok_or(
        EvalError::UnsupportedExpression {
            kind: "external table constructor",
        },
    )?;
    let Some(table_matrix) = eval_external_table_data_matrix(args, env, is_time_table) else {
        trace_external_table_data_arg(table_arg);
        if let Some(name) = missing_external_table_binding_name(table_arg, env) {
            return Err(EvalError::MissingBinding { name });
        }
        return Err(EvalError::UnsupportedExpression {
            kind: "external table data",
        });
    };
    validate_external_table_matrix(&table_matrix)?;

    let columns_idx = if is_time_table { 4 } else { 3 };
    if let Some(columns) = external_table_constructor_arg(args, "columns", columns_idx) {
        validate_array_argument(columns, env)?;
        validate_external_table_columns_arg(columns, env, table_matrix[0].len())?;
    }
    let smoothness_idx = if is_time_table { 5 } else { 4 };
    if let Some(smoothness) = external_table_constructor_arg(args, "smoothness", smoothness_idx) {
        validate_expr(smoothness, env)?;
    }
    let extrapolation_idx = if is_time_table { 6 } else { 5 };
    if let Some(extrapolation) =
        external_table_constructor_arg(args, "extrapolation", extrapolation_idx)
    {
        validate_expr(extrapolation, env)?;
    }
    Ok(())
}

fn missing_external_table_binding_name<T: SimFloat>(
    table_arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<String> {
    match table_arg {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty()
            && !env.vars.contains_key(name.as_str())
            && !env.start_exprs.contains_key(name.as_str())
            && !env.dims.contains_key(name.as_str()) =>
        {
            Some(name.to_string())
        }
        _ => None,
    }
}

pub(super) fn external_table_constructor_arg<'a>(
    args: &'a [rumoca_core::Expression],
    name: &str,
    positional_idx: usize,
) -> Option<&'a rumoca_core::Expression> {
    let (named_args, positional_args) = split_named_and_positional_call_args(args);
    named_args
        .get(name)
        .copied()
        .or_else(|| positional_args.get(positional_idx).copied())
}

fn validate_user_function_call_args<T: SimFloat>(
    function_name: &str,
    function: &rumoca_core::Function,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let (named_args, positional_args) = split_named_and_positional_call_args(args);
    if positional_args.len() > function.inputs.len() {
        return Err(EvalError::UnsupportedExpression {
            kind: "function call arity",
        });
    }
    for named in named_args.keys() {
        if !function.inputs.iter().any(|input| input.name == *named) {
            return Err(EvalError::UnsupportedExpression {
                kind: "function call named argument",
            });
        }
    }
    let mut local_env = env.clone();
    local_env.vars = VarScope::child_of(&env.vars);
    let mut positional_idx = 0usize;
    for input in &function.inputs {
        let arg = named_args.get(input.name.as_str()).copied().or_else(|| {
            let next = positional_args.get(positional_idx).copied();
            if next.is_some() {
                positional_idx += 1;
            }
            next
        });
        if let Some(arg) = arg {
            if bind_user_function_input_for_validation(&mut local_env, input, arg, env)? {
                continue;
            }
            if !copy_selected_input_fields_for_validation(&mut local_env, &input.name, arg, env)? {
                eval_expr::<T>(arg, env)?;
            }
        } else if input.default.is_none() {
            return Err(EvalError::MissingBinding {
                name: format!("{function_name}.{}", input.name),
            });
        } else if let Some(default_expr) = &input.default {
            let value = eval_expr::<T>(default_expr, &local_env)?;
            local_env.set(&input.name, value);
        }
    }
    Ok(())
}

fn bind_user_function_input_for_validation<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    input: &rumoca_core::FunctionParam,
    arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    if function_param_is_function(input)
        && let Some(closure) = function_closure_from_arg(arg, env)
    {
        local_env
            .function_closures
            .insert(input.name.clone(), closure);
        return Ok(true);
    }
    if function_param_is_aggregate(input) {
        validate_array_argument(arg, env)?;
        let values = if let Some(expected_len) = concrete_function_param_size(&input.dims) {
            eval_shaped_array_values::<T>(arg, env, expected_len)?
        } else {
            eval_array_values::<T>(arg, env)
        };
        bind_aggregate_input_for_validation(local_env, &input.name, &input.dims, values);
        return Ok(true);
    }
    if let Ok(value) = eval_expr::<T>(arg, env) {
        local_env.set(&input.name, value);
        return Ok(true);
    }
    Ok(false)
}

fn function_param_is_function(input: &rumoca_core::FunctionParam) -> bool {
    input.type_name.to_ascii_lowercase().contains("function")
}

fn function_param_is_aggregate(input: &rumoca_core::FunctionParam) -> bool {
    !input.dims.is_empty() || !input.shape_expr.is_empty()
}

fn concrete_function_param_size(dims: &[i64]) -> Option<usize> {
    if dims.is_empty() {
        return None;
    }
    dims.iter().try_fold(1usize, |acc, dim| {
        usize::try_from(*dim)
            .ok()
            .filter(|dim| *dim > 0)
            .and_then(|dim| acc.checked_mul(dim))
    })
}

fn bind_aggregate_input_for_validation<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    input_name: &str,
    declared_dims: &[i64],
    values: Vec<T>,
) {
    if values.is_empty() {
        return;
    }
    let inferred_dims = infer_dims_from_values(declared_dims, values.len());
    let dims = inferred_dims
        .iter()
        .map(|dim| i64::try_from(*dim).unwrap_or(i64::MAX))
        .collect::<Vec<_>>();
    set_array_entries(local_env, input_name, &dims, &values);
    std::sync::Arc::make_mut(&mut local_env.dims).insert(input_name.to_string(), dims);
}

fn copy_selected_input_fields_for_validation<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    input_name: &str,
    arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    let Some(arg_path) = eval_field_access_path(arg, env) else {
        return Ok(false);
    };
    let src_prefix = format!("{arg_path}.");
    let dst_prefix = format!("{input_name}.");
    let mut copied = false;
    for (key, value) in env.vars.prefixed_entries(&src_prefix) {
        let Some(suffix) = key.strip_prefix(&src_prefix) else {
            continue;
        };
        local_env.set(&format!("{dst_prefix}{suffix}"), *value);
        copied = true;
    }
    for key in super::prefixed_start_expr_keys(env, &src_prefix).iter() {
        let Some(start_expr) = env.start_exprs.get(key.as_str()) else {
            continue;
        };
        let Some(suffix) = key.strip_prefix(&src_prefix) else {
            continue;
        };
        let dst = format!("{dst_prefix}{suffix}");
        if local_env.vars.contains_key(dst.as_str()) {
            continue;
        }
        match eval_expr::<T>(start_expr, env) {
            Ok(value) => {
                local_env.set(&dst, value);
                copied = true;
            }
            Err(err) if err.missing_binding_name().is_some() => {}
            Err(err) => return Err(err),
        }
    }
    Ok(copied)
}

fn trace_external_table_data_arg(table_arg: &rumoca_core::Expression) {
    if std::env::var_os("RUMOCA_TRACE_EXTERNAL_TABLE_DATA").is_none() {
        return;
    }
    eprintln!("unsupported external table data arg: {table_arg:#?}");
}

fn validate_external_table_columns_arg<T: SimFloat>(
    columns_arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
    data_col_count: usize,
) -> Result<(), EvalError> {
    for column in eval_array_like_f64_values(columns_arg, env) {
        let rounded = column.round();
        if !rounded.is_finite()
            || (rounded - column).abs() > 1.0e-9
            || rounded < 1.0
            || rounded > data_col_count as f64
        {
            return Err(EvalError::UnsupportedExpression {
                kind: "external table columns",
            });
        }
    }
    Ok(())
}

fn validate_external_table_matrix(table_matrix: &[Vec<f64>]) -> Result<(), EvalError> {
    let Some(first_row) = table_matrix.first() else {
        return Err(EvalError::UnsupportedExpression {
            kind: "external table data",
        });
    };
    let data_col_count = first_row.len();
    if data_col_count < 2 {
        return Err(EvalError::UnsupportedExpression {
            kind: "external table data",
        });
    }
    for row in table_matrix {
        if row.len() != data_col_count || row.iter().any(|value| !value.is_finite()) {
            return Err(EvalError::UnsupportedExpression {
                kind: "external table data",
            });
        }
    }
    let spec = ExternalTableSpec {
        data: table_matrix.to_vec(),
        columns: Vec::new(),
        smoothness: 1,
        extrapolation: 1,
    };
    table_x_bounds(&spec)
        .filter(|(x_min, x_max)| x_min <= x_max)
        .map(|_| ())
        .ok_or(EvalError::UnsupportedExpression {
            kind: "external table data",
        })
}

fn validate_external_table_bound_call<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    validate_external_table_id_arg(args, env)?;
    Ok(())
}

fn validate_external_table_lookup_call<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let spec = validate_external_table_id_arg(args, env)?;
    validate_external_table_scalar_arg(args, 1, env)?;
    if let Some(spec) = spec {
        validate_external_table_lookup_column(args, env, &spec)?;
    }
    validate_external_table_scalar_arg(args, 2, env)
}

fn validate_external_table_next_event_call<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    validate_external_table_id_arg(args, env)?;
    validate_external_table_scalar_arg(args, 1, env)
}

fn validate_external_table_id_arg<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<ExternalTableSpec>, EvalError> {
    let table_id_arg = args.first().ok_or(EvalError::UnsupportedExpression {
        kind: "external table lookup",
    })?;
    validate_expr(table_id_arg, env)?;
    if expr_contains_function_call(table_id_arg) {
        return Ok(None);
    }
    let table_id = eval_expr::<T>(table_id_arg, env)?.real();
    let spec = lookup_external_table_in_registry(&env.runtime.external_tables, table_id)
        .ok_or_else(|| EvalError::MissingBinding {
            name: format!("external table {}", table_id.round() as i64),
        })?;
    validate_external_table_matrix(&spec.data)?;
    Ok(Some(spec))
}

fn validate_external_table_lookup_column<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
    spec: &ExternalTableSpec,
) -> Result<(), EvalError> {
    let col_arg = args.get(1).ok_or(EvalError::UnsupportedExpression {
        kind: "external table lookup",
    })?;
    let raw_col = eval_expr::<T>(col_arg, env)?.real();
    let rounded = raw_col.round();
    let output_count = if spec.columns.is_empty() {
        spec.data
            .first()
            .map(|row| row.len().saturating_sub(1))
            .unwrap_or(0)
    } else {
        spec.columns.len()
    };
    if !rounded.is_finite()
        || (rounded - raw_col).abs() > 1.0e-9
        || rounded < 1.0
        || rounded > output_count as f64
    {
        return Err(EvalError::UnsupportedExpression {
            kind: "external table lookup column",
        });
    }
    Ok(())
}

fn validate_external_table_scalar_arg<T: SimFloat>(
    args: &[rumoca_core::Expression],
    idx: usize,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let arg = args.get(idx).ok_or(EvalError::UnsupportedExpression {
        kind: "external table lookup",
    })?;
    validate_expr(arg, env)
}

fn expr_contains_function_call(expr: &rumoca_core::Expression) -> bool {
    let mut checker = FunctionCallChecker { found: false };
    checker.visit_expression(expr);
    checker.found
}

struct FunctionCallChecker {
    found: bool,
}

impl ExpressionVisitor for FunctionCallChecker {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_function_call(
        &mut self,
        _name: &rumoca_core::Reference,
        _args: &[rumoca_core::Expression],
        _is_constructor: bool,
    ) {
        self.found = true;
    }
}

fn builtin_accepts_array_argument(function: rumoca_core::BuiltinFunction) -> bool {
    matches!(
        function,
        rumoca_core::BuiltinFunction::Min
            | rumoca_core::BuiltinFunction::Max
            | rumoca_core::BuiltinFunction::Sum
            | rumoca_core::BuiltinFunction::Product
            | rumoca_core::BuiltinFunction::Diagonal
            | rumoca_core::BuiltinFunction::Abs
            | rumoca_core::BuiltinFunction::Sign
            | rumoca_core::BuiltinFunction::Sqrt
            | rumoca_core::BuiltinFunction::Sin
            | rumoca_core::BuiltinFunction::Cos
            | rumoca_core::BuiltinFunction::Tan
            | rumoca_core::BuiltinFunction::Asin
            | rumoca_core::BuiltinFunction::Acos
            | rumoca_core::BuiltinFunction::Atan
            | rumoca_core::BuiltinFunction::Sinh
            | rumoca_core::BuiltinFunction::Cosh
            | rumoca_core::BuiltinFunction::Tanh
            | rumoca_core::BuiltinFunction::Exp
            | rumoca_core::BuiltinFunction::Log
            | rumoca_core::BuiltinFunction::Log10
            | rumoca_core::BuiltinFunction::Floor
            | rumoca_core::BuiltinFunction::Integer
            | rumoca_core::BuiltinFunction::Ceil
            | rumoca_core::BuiltinFunction::NoEvent
            | rumoca_core::BuiltinFunction::Delay
            | rumoca_core::BuiltinFunction::Vector
    )
}

fn validate_array_argument<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    match expr {
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            ..
        } => {
            for element in elements {
                validate_array_argument(element, env)?;
            }
            if *is_matrix {
                validate_matrix_literal_interleaving(elements, env)?;
            }
            Ok(())
        }
        rumoca_core::Expression::Tuple { elements, .. } => {
            for element in elements {
                validate_array_argument(element, env)?;
            }
            Ok(())
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            validate_expr(start, env)?;
            if let Some(step) = step {
                validate_expr(step, env)?;
            }
            validate_expr(end, env)?;
            validate_range_argument(start, step.as_deref(), end, env)
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            for index in indices {
                validate_array_argument(&index.range, env)?;
            }
            validate_array_comprehension_body(expr, indices, filter.as_deref(), env)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                validate_expr(condition, env)?;
                validate_array_argument(value, env)?;
            }
            validate_array_argument(else_branch, env)
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. }
            if args.len() == 1 && builtin_accepts_array_argument(*function) =>
        {
            validate_array_argument(&args[0], env)
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty()
            && array_values_from_env_name_generic::<T>(name.as_str(), env).is_some() =>
        {
            Ok(())
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if !subscripts.is_empty()
            && subscripts
                .iter()
                .all(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
            && array_values_from_env_name_generic::<T>(name.as_str(), env).is_some() =>
        {
            Ok(())
        }
        rumoca_core::Expression::FieldAccess { base, field, .. }
            if eval_field_access_array_values(base, field, env).is_some() =>
        {
            Ok(())
        }
        _ => validate_expr(expr, env),
    }
}

fn validate_array_comprehension_body<T: SimFloat>(
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    validate_array_comprehension_body_at(0, expr, indices, filter, env)
}

fn validate_array_comprehension_body_at<T: SimFloat>(
    level: usize,
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    if level >= indices.len() {
        if let Some(filter) = filter {
            validate_expr(filter, env)?;
        }
        return validate_array_argument(expr, env);
    }

    let index = &indices[level];
    for value in eval_array_values::<T>(&index.range, env) {
        let mut local_env = env.clone();
        local_env.set(index.name.as_str(), value);
        validate_array_comprehension_body_at(level + 1, expr, indices, filter, &local_env)?;
    }
    Ok(())
}

fn validate_matrix_literal_interleaving<T: SimFloat>(
    elements: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    if elements.is_empty()
        || elements
            .iter()
            .all(|element| matches!(element, rumoca_core::Expression::Array { .. }))
    {
        return Ok(());
    }
    let column_lengths = elements
        .iter()
        .map(|element| eval_array_like_values::<T>(element, env).len())
        .collect::<Vec<_>>();
    let Some(row_count) = column_lengths.iter().copied().max() else {
        return Ok(());
    };
    if row_count == 0
        || column_lengths
            .iter()
            .any(|length| *length == 0 || (*length != 1 && *length != row_count))
    {
        return Err(EvalError::UnsupportedExpression {
            kind: "matrix literal",
        });
    }
    Ok(())
}

fn validate_range_argument<T: SimFloat>(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let start_v = eval_expr::<T>(start, env)?.real();
    let end_v = eval_expr::<T>(end, env)?.real();
    let step_v = if let Some(step) = step {
        eval_expr::<T>(step, env)?.real()
    } else if end_v >= start_v {
        1.0
    } else {
        -1.0
    };
    if !start_v.is_finite()
        || !end_v.is_finite()
        || !step_v.is_finite()
        || step_v.abs() <= f64::EPSILON
    {
        return Err(EvalError::UnsupportedExpression { kind: "range" });
    }
    Ok(())
}

fn function_call_supported<T: SimFloat>(name: &str, env: &VarEnv<T>) -> bool {
    let short_name = rumoca_core::top_level_last_segment(name);
    name == "Complex"
        || rumoca_core::BuiltinFunction::from_name(short_name).is_some()
        || rumoca_core::BuiltinFunction::from_name(&short_name.to_ascii_lowercase()).is_some()
        || env.function_closures.contains_key(name)
        || is_runtime_special_function_name(name)
        || env.functions.contains_key(name)
}

pub(super) fn eval_index_expr<T: SimFloat>(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> T {
    let Some(indices) = eval_index_subscripts(subscripts, env) else {
        return T::zero();
    };

    if let Some(path) = eval_field_access_path(base, env)
        && let Some(value) = eval_index_from_env_path(&path, &indices, env)
    {
        return value;
    }

    eval_index_from_nested_expr(base, &indices, env).unwrap_or_else(T::zero)
}

fn try_eval_index_expr<T: SimFloat>(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let indices = try_eval_index_subscripts(subscripts, env)?;

    if let Some(path) = try_eval_field_access_path(base, env)?
        && let Some(value) = eval_index_from_env_path(&path, &indices, env)
    {
        return Ok(value);
    }

    try_eval_index_from_nested_expr(base, &indices, env).ok_or_else(|| EvalError::MissingBinding {
        name: checked_index_missing_name(base, &indices, env),
    })
}

fn try_eval_index_subscripts<T: SimFloat>(
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let raw = match subscript {
            rumoca_core::Subscript::Index { value: i, .. } => *i as f64,
            rumoca_core::Subscript::Expr { expr, .. } => eval_expr::<T>(expr, env)?.real().round(),
            rumoca_core::Subscript::Colon { .. } => {
                return Err(EvalError::UnsupportedExpression {
                    kind: "colon index",
                });
            }
        };
        if !raw.is_finite() || raw < 1.0 {
            return Err(EvalError::UnsupportedExpression {
                kind: "invalid index",
            });
        }
        indices.push(raw as usize);
    }
    Ok(indices)
}

pub(super) fn eval_index_subscripts<T: SimFloat>(
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> Option<Vec<usize>> {
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let raw = match subscript {
            rumoca_core::Subscript::Index { value: i, .. } => *i as f64,
            rumoca_core::Subscript::Expr { expr, .. } => {
                eval_expr_or_default::<T>(expr, env).real().round()
            }
            rumoca_core::Subscript::Colon { .. } => return None,
        };
        if !raw.is_finite() || raw < 1.0 {
            return None;
        }
        indices.push(raw as usize);
    }
    Some(indices)
}

pub(super) fn eval_index_from_env_path<T: SimFloat>(
    base_path: &str,
    indices: &[usize],
    env: &VarEnv<T>,
) -> Option<T> {
    if indices.is_empty() {
        return env.vars.get(base_path).copied();
    }

    let mut direct_key = String::with_capacity(base_path.len() + indices.len() * 4 + 2);
    direct_key.push_str(base_path);
    direct_key.push('[');
    append_indices(&mut direct_key, indices);
    direct_key.push(']');
    if let Some(value) = env.vars.get(&direct_key).copied() {
        return Some(value);
    }

    let dims = env.dims.get(base_path)?;
    if dims.len() != indices.len() {
        return None;
    }

    let mut flat_index = 0usize;
    for (dim, index) in dims.iter().zip(indices.iter()) {
        let dim_usize = usize::try_from(*dim).ok()?;
        if dim_usize == 0 || *index > dim_usize {
            return None;
        }
        flat_index = flat_index.saturating_mul(dim_usize);
        flat_index = flat_index.saturating_add(index.saturating_sub(1));
    }
    direct_key.clear();
    write!(direct_key, "{base_path}[{}]", flat_index + 1).expect("write to String never fails");
    let flat_key = direct_key;
    env.vars.get(&flat_key).copied()
}

fn checked_index_missing_name<T: SimFloat>(
    base: &rumoca_core::Expression,
    indices: &[usize],
    env: &VarEnv<T>,
) -> String {
    if let Ok(Some(path)) = try_eval_field_access_path(base, env) {
        return format!("{}[{}]", path, index_list(indices));
    }
    "indexed expression".to_string()
}

fn index_list(indices: &[usize]) -> String {
    let mut out = String::new();
    append_indices(&mut out, indices);
    out
}

fn append_indices(buffer: &mut String, indices: &[usize]) {
    for (pos, index) in indices.iter().enumerate() {
        if pos > 0 {
            buffer.push(',');
        }
        write!(buffer, "{index}").expect("write to String never fails");
    }
}

fn try_eval_index_from_nested_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    indices: &[usize],
    env: &VarEnv<T>,
) -> Option<T> {
    if indices.is_empty() {
        return eval_expr::<T>(expr, env).ok();
    }

    match expr {
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            let idx0 = indices[0].checked_sub(1)?;
            let element = elements.get(idx0)?;
            try_eval_index_from_nested_expr(element, &indices[1..], env)
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Transpose,
            ..
        } => eval_matrix_index(expr, indices, env),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => eval_index_from_env_path(name.as_str(), indices, env),
        _ if indices.len() == 1 => {
            let values = eval_array_like_values::<T>(expr, env);
            values.get(indices[0].checked_sub(1)?).copied()
        }
        _ => None,
    }
}

pub(super) fn eval_index_from_nested_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    indices: &[usize],
    env: &VarEnv<T>,
) -> Option<T> {
    if indices.is_empty() {
        return Some(eval_expr_or_default::<T>(expr, env));
    }

    match expr {
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            let idx0 = indices[0].checked_sub(1)?;
            let element = elements.get(idx0)?;
            eval_index_from_nested_expr(element, &indices[1..], env)
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Transpose,
            ..
        } => eval_matrix_index(expr, indices, env),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => eval_index_from_env_path(name.as_str(), indices, env),
        _ if indices.len() == 1 => {
            let values = eval_array_like_values::<T>(expr, env);
            values.get(indices[0].checked_sub(1)?).copied()
        }
        _ => None,
    }
}

pub(super) fn with_function_call_stack<R>(
    runtime: &EvalRuntimeState,
    name: &str,
    f: impl FnOnce() -> R,
) -> R {
    runtime
        .function_call_stack
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .push(name.to_string());
    let out = f();
    let _ = runtime
        .function_call_stack
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .pop();
    out
}

pub(super) fn current_function_call_name(runtime: &EvalRuntimeState) -> Option<String> {
    runtime
        .function_call_stack
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .last()
        .cloned()
}

pub(super) fn eval_subscript_indices<T: SimFloat>(
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> Vec<String> {
    subscripts
        .iter()
        .map(|sub| match sub {
            rumoca_core::Subscript::Index { value: i, .. } => i.to_string(),
            rumoca_core::Subscript::Expr { expr, .. } => eval_expr_or_default::<T>(expr, env)
                .real()
                .round()
                .to_string(),
            rumoca_core::Subscript::Colon { .. } => ":".to_string(),
        })
        .collect()
}

pub(super) fn eval_field_access_path<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                Some(name.as_str().to_string())
            } else {
                let idx = eval_subscript_indices(subscripts, env);
                Some(format!("{}[{}]", name.as_str(), idx.join(",")))
            }
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let prefix = eval_field_access_path(base, env)?;
            Some(format!("{prefix}.{field}"))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let prefix = eval_field_access_path(base, env)?;
            let idx = eval_subscript_indices(subscripts, env);
            Some(format!("{prefix}[{}]", idx.join(",")))
        }
        _ => None,
    }
}

fn try_eval_field_access_path<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<String>, EvalError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                Ok(Some(name.as_str().to_string()))
            } else {
                build_indexed_name(name.as_str(), subscripts, env).map(Some)
            }
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let Some(prefix) = try_eval_field_access_path(base, env)? else {
                return Ok(None);
            };
            Ok(Some(format!("{prefix}.{field}")))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let Some(prefix) = try_eval_field_access_path(base, env)? else {
                return Ok(None);
            };
            let idx = try_eval_index_subscripts(subscripts, env)?;
            Ok(Some(format!("{prefix}[{}]", index_list(&idx))))
        }
        _ => Ok(None),
    }
}

const NAMED_CONSTRUCTOR_ARG_PREFIX: &str = "__rumoca_named_arg__.";

pub(super) fn decode_named_constructor_arg(
    expr: &rumoca_core::Expression,
) -> Option<(&str, &rumoca_core::Expression)> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: _,
        ..
    } = expr
    else {
        return None;
    };
    let named = name.as_str().strip_prefix(NAMED_CONSTRUCTOR_ARG_PREFIX)?;
    let value = args.first()?;
    Some((named, value))
}

pub(super) fn split_named_and_positional_call_args(
    args: &[rumoca_core::Expression],
) -> (
    HashMap<&str, &rumoca_core::Expression>,
    Vec<&rumoca_core::Expression>,
) {
    let mut named_args: HashMap<&str, &rumoca_core::Expression> = HashMap::new();
    let mut positional_args: Vec<&rumoca_core::Expression> = Vec::new();
    for arg in args {
        if let Some((name, value_expr)) = decode_named_constructor_arg(arg) {
            named_args.insert(name, value_expr);
        } else {
            positional_args.push(arg);
        }
    }
    (named_args, positional_args)
}

pub(super) fn bind_constructor_inputs<T: SimFloat>(
    constructor: &rumoca_core::Function,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> (VarEnv<T>, Vec<T>) {
    let mut local_env = env.clone();
    let mut input_values = Vec::with_capacity(constructor.inputs.len());
    let (named_args, positional_args) = split_named_and_positional_call_args(args);
    let mut positional_idx = 0usize;
    for input in &constructor.inputs {
        let value = if let Some(arg_expr) = named_args.get(input.name.as_str()) {
            eval_expr_or_default::<T>(arg_expr, &local_env)
        } else if let Some(arg_expr) = positional_args.get(positional_idx) {
            positional_idx += 1;
            eval_expr_or_default::<T>(arg_expr, &local_env)
        } else if let Some(default_expr) = &input.default {
            eval_expr_or_default::<T>(default_expr, &local_env)
        } else if let Some(existing) = local_env.vars.get(&input.name).copied() {
            existing
        } else {
            T::zero()
        };
        local_env.set(&input.name, value);
        input_values.push(value);
    }
    (local_env, input_values)
}

pub(super) fn eval_field_access_constructor_by_signature<T: SimFloat>(
    base_name: &rumoca_core::VarName,
    args: &[rumoca_core::Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    let constructor = env.functions.get(base_name.as_str())?;
    let (local_env, input_values) = bind_constructor_inputs(constructor, args, env);

    if let Some((idx, _)) = constructor
        .inputs
        .iter()
        .enumerate()
        .find(|(_, input)| input.name == field)
    {
        return input_values.get(idx).copied();
    }

    if let Some(output) = constructor
        .outputs
        .iter()
        .find(|output| output.name == field)
    {
        if let Some(default_expr) = &output.default {
            return Some(eval_expr_or_default::<T>(default_expr, &local_env));
        }
        if let Some(value) = local_env.vars.get(&output.name).copied() {
            return Some(value);
        }
    }

    None
}

pub(super) fn eval_field_access<T: SimFloat>(
    base: &rumoca_core::Expression,
    field: &str,
    env: &VarEnv<T>,
) -> T {
    if let Some(branch) = selected_if_branch(base, env) {
        return eval_field_access(branch, field, env);
    }
    if let Some(projected) = projected_record_field_expr(base, env) {
        return eval_field_access(&projected, field, env);
    }

    if let rumoca_core::Expression::Index {
        base, subscripts, ..
    } = base
        && let Some(value) = eval_indexed_field_access(base, subscripts, field, env)
    {
        return value;
    }

    if let Some(path) = eval_field_access_path(base, env) {
        let key = format!("{path}.{field}");
        if let Some(value) = env.vars.get(&key).copied() {
            return value;
        }
        if let Ok(Some(value)) = try_eval_outer_component_field_alias(&path, field, env) {
            return value;
        }
    }

    if let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor,
        ..
    } = base
    {
        if *is_constructor
            && let Some(value) =
                eval_field_access_constructor_by_signature(name.var_name(), args, field, env)
        {
            return value;
        }
        match try_eval_function_record_scalar_field(name, args, field, env) {
            Ok(Some(value)) => return value,
            Ok(None) => {}
            Err(EvalError::UnsupportedExpression {
                kind: "record function output field shape",
            })
            | Err(EvalError::ShapeMismatch { .. }) => {}
            Err(_) => {}
        }

        let selected = rumoca_core::VarName::new(format!("{}.{}", name.as_str(), field));
        let value = eval_function_call::<T>(&selected, args, false, env);
        if value.real().is_finite() {
            return value;
        }
    }

    T::zero()
}

fn try_eval_field_access<T: SimFloat>(
    base: &rumoca_core::Expression,
    field: &str,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if let Some(branch) = try_selected_if_branch(base, env)? {
        return try_eval_field_access(branch, field, env);
    }
    if let Some(projected) = try_projected_record_field_expr(base, env)? {
        return try_eval_field_access(&projected, field, env);
    }

    if let rumoca_core::Expression::Index {
        base, subscripts, ..
    } = base
    {
        let indices = try_eval_index_subscripts(subscripts, env)?;
        if let Some(value) = eval_indexed_field_from_nested_expr(base, &indices, field, env) {
            return Ok(value);
        }
        return Err(EvalError::MissingBinding {
            name: checked_indexed_field_missing_name(base, &indices, field, env),
        });
    }

    if let Some(path) = try_eval_field_access_path(base, env)? {
        let key = format!("{path}.{field}");
        if let Some(value) = env.vars.get(&key).copied() {
            return Ok(value);
        }
        if let Some(value) = try_eval_outer_component_field_alias(&path, field, env)? {
            return Ok(value);
        }
        return Err(EvalError::MissingBinding { name: key });
    }

    if let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor,
        ..
    } = base
    {
        if *is_constructor
            && let Some(value) =
                eval_field_access_constructor_by_signature(name.var_name(), args, field, env)
        {
            return Ok(value);
        }
        if let Some(value) = try_eval_function_record_scalar_field(name, args, field, env)? {
            return Ok(value);
        }

        let selected = rumoca_core::VarName::new(format!("{}.{}", name.as_str(), field));
        if function_call_supported(selected.as_str(), env) {
            validate_expr_slice_checked(args, env)?;
            return Ok(eval_function_call::<T>(&selected, args, false, env));
        }
    }

    trace_unsupported_field_access(base, field);
    Err(EvalError::UnsupportedExpression {
        kind: unsupported_field_access_kind(base),
    })
}

fn try_eval_outer_component_field_alias<T: SimFloat>(
    path: &str,
    field: &str,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let mut cursor = path;
    while let Some((parent, _)) = cursor.rsplit_once('.') {
        if let Some(value) = pressure_drop_modifier_alias(parent, field, env) {
            return Ok(Some(value));
        }
        let parent_key = format!("{parent}.{field}");
        if let Some(value) = env.vars.get(&parent_key).copied() {
            return Ok(Some(value));
        }
        cursor = parent;
    }

    let Some((_, outer_name)) = path.rsplit_once('.') else {
        return Ok(None);
    };
    let global_key = format!("{outer_name}.{field}");
    if let Some(value) = env.vars.get(&global_key).copied() {
        return Ok(Some(value));
    }
    Ok(None)
}

fn pressure_drop_modifier_alias<T: SimFloat>(
    owner: &str,
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    let sources = match field {
        "dp1_nominal" => ["PreDroWat", "dp1_nominal", "dp_nominal"],
        "dp2_nominal" => ["PreDroAir", "dp2_nominal", "dp_nominal"],
        _ => return None,
    };
    sources.into_iter().find_map(|source| {
        let key = format!("{owner}.{source}");
        env.vars.get(key.as_str()).copied().or_else(|| {
            env.start_exprs
                .get(key.as_str())
                .and_then(|start| eval_expr::<T>(start, env).ok())
        })
    })
}

fn pressure_drop_modifier_alias_in_scope<T: SimFloat>(
    owner: &str,
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    let mut cursor = owner;
    loop {
        if let Some(value) = pressure_drop_modifier_alias(cursor, field, env) {
            return Some(value);
        }
        let Some((parent, _)) = cursor.rsplit_once('.') else {
            return None;
        };
        cursor = parent;
    }
}

fn unsupported_field_access_kind(expr: &rumoca_core::Expression) -> &'static str {
    match expr {
        rumoca_core::Expression::Literal { .. } => "field access over literal",
        rumoca_core::Expression::VarRef { .. } => "field access over unresolved variable",
        rumoca_core::Expression::Binary { .. } => "field access over binary expression",
        rumoca_core::Expression::Unary { .. } => "field access over unary expression",
        rumoca_core::Expression::BuiltinCall { .. } => "field access over builtin call",
        rumoca_core::Expression::FunctionCall { .. } => {
            "field access over unsupported function call"
        }
        rumoca_core::Expression::If { .. } => "field access over if expression",
        rumoca_core::Expression::Array { .. } => "field access over array expression",
        rumoca_core::Expression::Index { .. } => "field access over unresolved indexed expression",
        rumoca_core::Expression::FieldAccess { .. } => "field access over unresolved field access",
        rumoca_core::Expression::Empty { .. } => "field access over empty expression",
        rumoca_core::Expression::Range { .. } => "field access over range expression",
        rumoca_core::Expression::Tuple { .. } => "field access over tuple expression",
        rumoca_core::Expression::ArrayComprehension { .. } => {
            "field access over array comprehension"
        }
    }
}

fn trace_unsupported_field_access(base: &rumoca_core::Expression, field: &str) {
    if std::env::var_os("RUMOCA_TRACE_UNSUPPORTED_FIELD_ACCESS").is_none() {
        return;
    }
    eprintln!("unsupported field access .{field} over base: {base:#?}");
}

fn projected_record_field_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::FieldAccess { base, field, .. } = expr else {
        return None;
    };
    if let Some(branch) = selected_if_branch(base, env) {
        return Some(rumoca_core::Expression::FieldAccess {
            base: Box::new(branch.clone()),
            field: field.clone(),
            span: expr.span().unwrap_or(rumoca_core::Span::DUMMY),
        });
    }
    constructor_named_field_expr(base, field)
}

fn try_projected_record_field_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<rumoca_core::Expression>, EvalError> {
    let rumoca_core::Expression::FieldAccess { base, field, .. } = expr else {
        return Ok(None);
    };
    if let Some(branch) = try_selected_if_branch(base, env)? {
        return Ok(Some(rumoca_core::Expression::FieldAccess {
            base: Box::new(branch.clone()),
            field: field.clone(),
            span: expr.span().unwrap_or(rumoca_core::Span::DUMMY),
        }));
    }
    Ok(constructor_named_field_expr(base, field))
}

fn constructor_named_field_expr(
    expr: &rumoca_core::Expression,
    field: &str,
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::FunctionCall {
        args,
        is_constructor: true,
        ..
    } = expr
    else {
        return None;
    };
    args.iter().find_map(|arg| {
        let (name, value) = decode_named_constructor_arg(arg)?;
        (name == field).then(|| value.clone())
    })
}

fn selected_if_branch<'a, T: SimFloat>(
    expr: &'a rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<&'a rumoca_core::Expression> {
    let rumoca_core::Expression::If {
        branches,
        else_branch,
        ..
    } = expr
    else {
        return None;
    };

    for (condition, branch) in branches {
        if eval_expr_or_default::<T>(condition, env).real() != 0.0 {
            return Some(branch);
        }
    }
    Some(else_branch)
}

fn try_selected_if_branch<'a, T: SimFloat>(
    expr: &'a rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<&'a rumoca_core::Expression>, EvalError> {
    let rumoca_core::Expression::If {
        branches,
        else_branch,
        ..
    } = expr
    else {
        return Ok(None);
    };

    for (condition, branch) in branches {
        validate_expr(condition, env)?;
        if eval_expr_or_default::<T>(condition, env).real() != 0.0 {
            return Ok(Some(branch));
        }
    }
    Ok(Some(else_branch))
}

fn try_eval_function_record_scalar_field<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let Some(function) = env.functions.get(name.as_str()) else {
        return Ok(None);
    };
    let Some(output) = function.outputs.first() else {
        return Ok(None);
    };
    if output.type_class != Some(rumoca_core::ClassType::Record) {
        return Ok(None);
    }

    if let Some(value) =
        super::special::eval_state_accessor_from_set_state(name.as_str(), args, field, env)
    {
        return Ok(Some(value));
    }

    let output_name = format!("{}.{}", output.name, field);
    let output_path = if let Some(field_param) =
        super::array_helpers::record_constructor_field_param(function, output, field, env)
    {
        super::array_helpers::function_param_size(field_param).ok_or(
            EvalError::UnsupportedExpression {
                kind: "record function output field shape",
            },
        )?;
        let flat_index = 0;
        super::array_helpers::function_output_path(&output_name, &field_param.dims, flat_index)
            .ok_or(EvalError::UnsupportedExpression {
                kind: "record function output field index",
            })?
    } else {
        output_name
    };
    match eval_user_function_output_path_pub(name.var_name(), args, output_path.as_str(), env) {
        Ok(value) => Ok(Some(value)),
        Err(EvalError::MissingBinding { .. }) => Ok(
            super::special::eval_state_accessor_from_set_state(name.as_str(), args, field, env),
        ),
        Err(err) => Err(err),
    }
}

fn checked_indexed_field_missing_name<T: SimFloat>(
    base: &rumoca_core::Expression,
    indices: &[usize],
    field: &str,
    env: &VarEnv<T>,
) -> String {
    if let Ok(Some(path)) = try_eval_field_access_path(base, env) {
        return format!("{}[{}].{}", path, index_list(indices), field);
    }
    "indexed field expression".to_string()
}

fn eval_indexed_field_access<T: SimFloat>(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    let indices = eval_index_subscripts(subscripts, env)?;
    eval_indexed_field_from_nested_expr(base, &indices, field, env)
}

fn eval_indexed_field_from_nested_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    indices: &[usize],
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    if indices.is_empty() {
        return Some(eval_field_access(expr, field, env));
    }

    match expr {
        // MLS Chapter 10 array indexing selects the element before later
        // component selection, so array/tuple literals must recurse first.
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            let idx0 = indices[0].checked_sub(1)?;
            let element = elements.get(idx0)?;
            eval_indexed_field_from_nested_expr(element, &indices[1..], field, env)
        }
        _ => {
            let path = eval_field_access_path(expr, env)?;
            let joined = indices
                .iter()
                .map(|idx| idx.to_string())
                .collect::<Vec<_>>()
                .join(",");
            let key = format!("{path}[{joined}].{field}");
            env.vars.get(&key).copied()
        }
    }
}

pub(super) fn eval_literal<T: SimFloat>(lit: &rumoca_core::Literal) -> T {
    match lit {
        rumoca_core::Literal::Real(v) => T::from_f64(*v),
        rumoca_core::Literal::Integer(v) => T::from_f64(*v as f64),
        rumoca_core::Literal::Boolean(v) => T::from_bool(*v),
        rumoca_core::Literal::String(_) => T::zero(),
    }
}

/// Build the full variable name from a base name and subscripts.
///
/// Returns the base name unchanged when subscripts are empty; otherwise
/// appends evaluated subscript indices (e.g. `x` + `[1,2]` → `x[1,2]`).
fn build_indexed_name<T: SimFloat>(
    name: &str,
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> Result<String, EvalError> {
    if subscripts.is_empty() {
        return Ok(name.to_string());
    }
    let mut indexed = String::with_capacity(name.len() + subscripts.len() * 4 + 2);
    indexed.push_str(name);
    indexed.push('[');
    for (pos, subscript) in subscripts.iter().enumerate() {
        if pos > 0 {
            indexed.push(',');
        }
        match subscript {
            rumoca_core::Subscript::Index { value: i, .. } => {
                let index = resolve_end_sentinel_index(name, pos, *i, env).unwrap_or(*i);
                write!(indexed, "{index}").expect("write to String never fails");
            }
            rumoca_core::Subscript::Expr { expr, .. } => {
                let index = eval_expr::<T>(expr, env)?.real() as i64;
                let index = resolve_end_sentinel_index(name, pos, index, env).unwrap_or(index);
                write!(indexed, "{index}").expect("write to String never fails");
            }
            rumoca_core::Subscript::Colon { .. } => {
                return Err(EvalError::UnsupportedExpression {
                    kind: "colon subscript",
                });
            }
        }
    }
    indexed.push(']');
    Ok(indexed)
}

fn resolve_end_sentinel_index<T: SimFloat>(
    name: &str,
    pos: usize,
    index: i64,
    env: &VarEnv<T>,
) -> Option<i64> {
    if index != 0 {
        return None;
    }
    env.dims.get(name)?.get(pos).copied().filter(|dim| *dim > 0)
}

pub(super) fn eval_var_ref<T: SimFloat>(
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> T {
    try_eval_var_ref(name, subscripts, env).unwrap_or_else(|_| T::zero())
}

fn try_eval_var_ref<T: SimFloat>(
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if subscripts.is_empty() {
        return try_eval_var_ref_no_subscripts(name.as_str(), env).ok_or_else(|| {
            EvalError::MissingBinding {
                name: name.to_string(),
            }
        });
    }
    if subscripts.len() == 1
        && let rumoca_core::Subscript::Expr { expr, .. } = &subscripts[0]
        && matches!(expr.as_ref(), rumoca_core::Expression::Range { .. })
    {
        let values = try_eval_array_like_values(
            &rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(name.as_str()),
                subscripts: subscripts.to_vec(),
                span: rumoca_core::Span::DUMMY,
            },
            env,
        )?;
        if let [value] = values.as_slice() {
            return Ok(*value);
        }
        return Err(EvalError::UnsupportedExpression {
            kind: "range slice scalar value",
        });
    }
    let indexed_name = build_indexed_name(name.as_str(), subscripts, env)?;
    env.vars
        .get(&indexed_name)
        .copied()
        .or_else(|| try_eval_var_ref_no_subscripts(name.as_str(), env))
        .or_else(|| trace_substance_nominal_default(name.as_str()))
        .ok_or(EvalError::MissingBinding { name: indexed_name })
}

/// Look up a variable with no explicit subscripts.
/// Handles names with embedded subscript expressions like `x[(2-1)]`.
#[cfg(test)]
pub(super) fn eval_var_ref_no_subscripts<T: SimFloat>(raw: &str, env: &VarEnv<T>) -> T {
    try_eval_var_ref_no_subscripts(raw, env).unwrap_or_else(T::zero)
}

fn try_eval_var_ref_no_subscripts<T: SimFloat>(raw: &str, env: &VarEnv<T>) -> Option<T> {
    if let Some(value) = lowered_pre_parameter_value(raw, env) {
        return Some(value);
    }
    if let Some(&v) = env.vars.get(raw) {
        return Some(v);
    }
    if let Some(value) = enclosing_scope_var_alias(raw, env) {
        return Some(value);
    }
    if let Some((owner, field)) = raw.rsplit_once('.')
        && let Some(value) = pressure_drop_modifier_alias_in_scope(owner, field, env)
    {
        return Some(value);
    }
    if let Some(caller) = current_function_call_name(&env.runtime)
        && let Some(field) = complex_field_selection_from_path(&caller)
    {
        let selected_key = format!("{raw}.{field}");
        if let Some(&v) = env.vars.get(selected_key.as_str()) {
            return Some(v);
        }
    }
    // If name contains brackets with expressions, try normalizing.
    if raw.contains('[') {
        if let Some(v) =
            normalize_var_name::<T>(raw, env).and_then(|n| env.vars.get(n.as_str()).copied())
        {
            return Some(v);
        }
        if let Some(base_name) = unity_subscript_base_name(raw)
            && let Some(&v) = env.vars.get(base_name.as_str())
        {
            return Some(v);
        }
    }
    if let Some(ordinal) = lookup_enum_literal_ordinal(raw, &env.enum_literal_ordinals) {
        return Some(T::from_f64(ordinal as f64));
    }
    if let Some(value) = trace_substance_nominal_default(raw) {
        return Some(value);
    }
    None
}

fn enclosing_scope_var_alias<T: SimFloat>(raw: &str, env: &VarEnv<T>) -> Option<T> {
    let (owner, field) = raw.rsplit_once('.')?;
    let mut cursor = owner;
    while let Some((parent, _)) = cursor.rsplit_once('.') {
        let key = format!("{parent}.{field}");
        if let Some(value) = env.vars.get(key.as_str()).copied() {
            return Some(value);
        }
        cursor = parent;
    }
    None
}

fn trace_substance_nominal_default<T: SimFloat>(raw: &str) -> Option<T> {
    raw.ends_with(".C_nominal").then(|| T::from_f64(1.0e-2))
}

fn lowered_pre_parameter_value<T: SimFloat>(raw: &str, env: &VarEnv<T>) -> Option<T> {
    let target = raw.strip_prefix("__pre__.")?;
    if let Some(value) = lookup_pre_value_in(&env.runtime, target) {
        return Some(T::from_f64(value));
    }
    if let Some(normalized) = normalize_var_name::<T>(target, env)
        && let Some(value) = lookup_pre_value_in(&env.runtime, normalized.as_str())
    {
        return Some(T::from_f64(value));
    }
    if let Some(base_name) = unity_subscript_base_name(target)
        && let Some(value) = lookup_pre_value_in(&env.runtime, base_name.as_str())
    {
        return Some(T::from_f64(value));
    }
    None
}

pub(super) fn lookup_enum_literal_ordinal(
    raw: &str,
    ordinals: &IndexMap<String, i64>,
) -> Option<i64> {
    if let Some(&ordinal) = ordinals.get(raw) {
        return Some(ordinal);
    }
    let (prefix, literal) = rumoca_core::split_last_top_level(raw)?;
    if let Some(unquoted) = strip_quoted_identifier(literal) {
        let alt = format!("{prefix}.{unquoted}");
        return ordinals
            .get(&alt)
            .copied()
            .or_else(|| lookup_enum_literal_by_unambiguous_suffix(literal, ordinals));
    }
    let alt = format!("{prefix}.'{literal}'");
    ordinals
        .get(&alt)
        .copied()
        .or_else(|| lookup_enum_literal_by_unambiguous_suffix(literal, ordinals))
}

pub(super) fn strip_quoted_identifier(segment: &str) -> Option<&str> {
    if segment.len() >= 2 && segment.starts_with('\'') && segment.ends_with('\'') {
        Some(&segment[1..segment.len() - 1])
    } else {
        None
    }
}

fn lookup_enum_literal_by_unambiguous_suffix(
    raw_literal: &str,
    ordinals: &IndexMap<String, i64>,
) -> Option<i64> {
    let target = canonical_enum_literal_segment(raw_literal);
    let mut found = None;
    for (name, ordinal) in ordinals {
        let Some((_, literal)) = rumoca_core::split_last_top_level(name) else {
            continue;
        };
        if canonical_enum_literal_segment(literal) != target {
            continue;
        }
        match found {
            Some(existing) if existing != *ordinal => return None,
            Some(_) => {}
            None => found = Some(*ordinal),
        }
    }
    found
}

fn canonical_enum_literal_segment(segment: &str) -> &str {
    strip_quoted_identifier(segment).unwrap_or(segment)
}

pub(super) fn unity_subscript_base_name(name: &str) -> Option<String> {
    let mut base = String::with_capacity(name.len());
    let mut depth = 0usize;
    let mut current = String::new();
    let mut saw_subscript = false;

    for ch in name.chars() {
        match ch {
            '[' => {
                depth += 1;
                if depth == 1 {
                    current.clear();
                    saw_subscript = true;
                } else {
                    current.push(ch);
                }
            }
            ']' => {
                if depth == 1 {
                    let trimmed = current.trim();
                    validate_unity_subscript_text(trimmed)?;
                    current.clear();
                } else if depth > 1 {
                    current.push(ch);
                }
                depth = depth.saturating_sub(1);
            }
            _ if depth == 0 => base.push(ch),
            _ => current.push(ch),
        }
    }

    (saw_subscript && depth == 0).then_some(base)
}

pub(super) fn is_unity_subscript_text(text: &str) -> bool {
    text == "1"
        || text
            .parse::<f64>()
            .ok()
            .is_some_and(|v| v.is_finite() && v == 1.0)
}

pub(super) fn validate_unity_subscript_text(text: &str) -> Option<()> {
    is_unity_subscript_text(text).then_some(())
}

/// Normalize a variable name by evaluating constant subscript expressions.
///
/// For example: `"x[(2 - 1)]"` → `"x[1]"`, `"a[(3 + 1)].b"` → `"a[4].b"`
pub(super) fn normalize_var_name<T: SimFloat>(name: &str, env: &VarEnv<T>) -> Option<String> {
    let mut result = String::with_capacity(name.len());
    let mut chars = name.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch != '[' {
            result.push(ch);
            continue;
        }
        let subscript_str = collect_bracketed(&mut chars);
        let val = subscript_str
            .trim()
            .parse::<i64>()
            .map(|v| v as f64)
            .unwrap_or_else(|_| eval_simple_int_expr(&subscript_str, env));
        result.push('[');
        result.push_str(&(val as i64).to_string());
        result.push(']');
    }

    if result != name { Some(result) } else { None }
}

/// Collect characters between `[` and matching `]`, handling nesting.
pub(super) fn collect_bracketed(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
    let mut depth = 1;
    let mut s = String::new();
    for c in chars.by_ref() {
        match c {
            '[' => depth += 1,
            ']' if depth == 1 => break,
            ']' => depth -= 1,
            _ => {}
        }
        s.push(c);
    }
    s
}

/// Evaluate a simple integer expression from a subscript string.
/// Handles: integer literals, parenthesized expressions, +, -, *, and variable references.
pub(super) fn eval_simple_int_expr<T: SimFloat>(s: &str, env: &VarEnv<T>) -> f64 {
    let s = s.trim();
    if let Ok(v) = s.parse::<i64>() {
        return v as f64;
    }
    if s.chars()
        .all(|c| c.is_alphanumeric() || c == '_' || c == '.')
    {
        return env.get(s).real();
    }
    if s.starts_with('(') && s.ends_with(')') {
        return eval_simple_int_expr(&s[1..s.len() - 1], env);
    }
    // Try binary ops: scan right-to-left for +/- then * (respecting parens)
    if let Some(v) = try_split_binop(s, b"+-", env) {
        return v;
    }
    if let Some(v) = try_split_binop(s, b"*", env) {
        return v;
    }
    0.0
}

/// Try splitting `s` at a binary operator (rightmost, outside parens).
pub(super) fn try_split_binop<T: SimFloat>(s: &str, ops: &[u8], env: &VarEnv<T>) -> Option<f64> {
    let bytes = s.as_bytes();
    let mut depth = 0i32;
    for i in (1..bytes.len()).rev() {
        match bytes[i] {
            b')' => depth += 1,
            b'(' => depth -= 1,
            op if depth == 0 && ops.contains(&op) => {
                let left = s[..i].trim();
                if left.is_empty() {
                    continue;
                }
                let l = eval_simple_int_expr(left, env);
                let r = eval_simple_int_expr(&s[i + 1..], env);
                return Some(match op {
                    b'+' => l + r,
                    b'-' => l - r,
                    b'*' => l * r,
                    _ => 0.0,
                });
            }
            _ => {}
        }
    }
    None
}

pub(super) fn eval_vector_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            let dims = env.dims.get(name.as_str())?;
            if dims.len() != 1 || dims[0] <= 1 {
                return None;
            }
            array_values_from_env_name_generic(name.as_str(), env).filter(|values| values.len() > 1)
        }
        rumoca_core::Expression::Array { is_matrix, .. } if !*is_matrix => {
            let values = eval_array_values(expr, env);
            (values.len() > 1).then_some(values)
        }
        _ => None,
    }
}

pub(super) fn eval_vector_dot_product<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<T> {
    let lhs_values = eval_vector_values(lhs, env)?;
    let rhs_values = eval_vector_values(rhs, env)?;
    if lhs_values.len() != rhs_values.len() || lhs_values.is_empty() {
        return None;
    }

    Some(
        lhs_values
            .iter()
            .zip(rhs_values.iter())
            .fold(T::zero(), |acc, (l, r)| acc + (*l * *r)),
    )
}

pub(super) fn eval_binary<T: SimFloat>(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> T {
    if matches!(op, rumoca_core::OpBinary::Mul)
        && let Some(dot) = eval_vector_dot_product(lhs, rhs, env)
    {
        return dot;
    }
    if matches!(op, rumoca_core::OpBinary::Mul)
        && let Some(values) = eval_binary_array_values(op, lhs, rhs, env)
        && let Some(first) = values.first().copied()
    {
        return first;
    }

    let l = eval_expr_or_default::<T>(lhs, env);
    let r = eval_expr_or_default::<T>(rhs, env);
    match op {
        rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => l + r,
        rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => l - r,
        rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => l * r,
        rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => {
            if r.real() == 0.0 {
                // 0/0 = 0 (simulation convention, avoids NaN propagation);
                // nonzero/0 = infinity (IEEE 754 convention).
                if l.real() == 0.0 {
                    T::zero()
                } else {
                    T::infinity()
                }
            } else {
                l / r
            }
        }
        rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => l.powf(r),
        rumoca_core::OpBinary::And => T::from_bool(l.to_bool() && r.to_bool()),
        rumoca_core::OpBinary::Or => T::from_bool(l.to_bool() || r.to_bool()),
        rumoca_core::OpBinary::Lt => T::from_bool(l.lt(r)),
        rumoca_core::OpBinary::Le => T::from_bool(l.le(r)),
        rumoca_core::OpBinary::Gt => T::from_bool(l.gt(r)),
        rumoca_core::OpBinary::Ge => T::from_bool(l.ge(r)),
        rumoca_core::OpBinary::Eq => T::from_bool(l.eq_approx(r)),
        rumoca_core::OpBinary::Neq => T::from_bool(!l.eq_approx(r)),
        rumoca_core::OpBinary::Empty | rumoca_core::OpBinary::Assign => T::zero(),
    }
}

pub(super) fn eval_unary<T: SimFloat>(
    op: &rumoca_core::OpUnary,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> T {
    let r = eval_expr_or_default::<T>(rhs, env);
    match op {
        rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => -r,
        rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus => r,
        rumoca_core::OpUnary::Not => T::from_bool(!r.to_bool()),
        rumoca_core::OpUnary::Empty => r,
    }
}
