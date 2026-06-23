use super::*;
use rumoca_core::ExpressionVisitor;

mod checked_eval;
pub use checked_eval::eval_expr;
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
    InvalidShape {
        context: &'static str,
        reason: String,
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
            Self::InvalidShape { context, reason } => {
                write!(f, "invalid DAE evaluation shape for {context}: {reason}")
            }
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

    pub fn with_span_if_missing(self, span: rumoca_core::Span) -> Self {
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
        } => validate_function_call_expr(name, args, *is_constructor, env),
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

fn validate_function_call_expr<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    is_constructor: bool,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    if let Some(result) = validate_external_table_call(name, args, env) {
        return result;
    }
    if is_constructor || name.as_str() == "Complex" {
        return validate_expr_slice_checked(args, env);
    }
    if is_runtime_special_function_name(name.var_name()) {
        // Runtime special functions own aggregate argument semantics such as
        // table matrices and clock constructors.
        return Ok(());
    }
    if validate_function_closure_call(name, args, env)? {
        return Ok(());
    }
    if let Some(function) = env.functions.get(name.as_str()) {
        return validate_user_function_call_args(name.as_str(), function, args, env);
    }
    if let Some((resolved_name, _selection)) = resolve_user_function_target(name.as_str(), env)
        && let Some(function) = env.functions.get(resolved_name.as_str())
    {
        return validate_user_function_call_args(resolved_name.as_str(), function, args, env);
    }
    if let Some(function) = builtin_function_from_call_name(name.var_name()) {
        return validate_builtin_call(function, args, env);
    }
    validate_expr_slice_checked(args, env)?;
    function_call_supported(name.var_name(), env)
        .then_some(())
        .ok_or_else(|| EvalError::MissingFunction {
            name: name.to_string(),
        })
}

fn validate_function_closure_call<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    let Some(closure) = env.function_closures.get(name.as_str()) else {
        return Ok(false);
    };
    let function = env
        .functions
        .get(closure.target_name.as_str())
        .ok_or_else(|| EvalError::MissingFunction {
            name: closure.target_name.to_string(),
        })?;
    let mut merged_args = Vec::with_capacity(args.len() + closure.bound_args.len());
    merged_args.extend(args.iter().cloned());
    merged_args.extend(closure.bound_args.iter().cloned());
    validate_user_function_call_args(closure.target_name.as_str(), function, &merged_args, env)?;
    Ok(true)
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
    if !expression_is_array_like(lhs, env)? && !expression_is_array_like(rhs, env)? {
        return Ok(false);
    }
    validate_array_argument(lhs, env)?;
    validate_array_argument(rhs, env)?;
    if eval_vector_dot_product(lhs, rhs, env).is_some() {
        return Ok(true);
    }
    Ok(
        eval_binary_array_values(&rumoca_core::OpBinary::Mul, lhs, rhs, env)?
            .is_some_and(|values| values.len() == 1),
    )
}

fn expression_is_array_like<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    match expr {
        rumoca_core::Expression::Array { .. }
        | rumoca_core::Expression::Tuple { .. }
        | rumoca_core::Expression::Range { .. }
        | rumoca_core::Expression::ArrayComprehension { .. } => Ok(true),
        rumoca_core::Expression::BuiltinCall { function, args, .. }
            if args.len() == 1 && builtin_accepts_array_argument(*function) =>
        {
            expression_is_array_like(&args[0], env)
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Ok(encoded_slice_field_values(name.as_str(), env)?.is_some()
            || array_values_from_env_name_generic::<T>(name.as_str(), env)?.is_some()),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            match try_eval_field_access_array_values(base, field, env) {
                Ok(_) => Ok(true),
                Err(EvalError::MissingBinding { .. })
                | Err(EvalError::UnsupportedExpression {
                    kind: "field access array value",
                }) => Ok(false),
                Err(err) => Err(err),
            }
        }
        _ => Ok(false),
    }
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
    validate_array_argument(array_arg, env)?;
    validate_expr_slice_checked(&args[1..], env)
}

fn size_arg_has_known_shape<T: SimFloat>(expr: &rumoca_core::Expression, env: &VarEnv<T>) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            ..
        } if subscripts.is_empty() && env.dims.contains_key(name.as_str())
    )
}

fn validate_external_table_call<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<Result<(), EvalError>> {
    let short_name = name.last_segment();
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
    if !matches!(table_arg, rumoca_core::Expression::Empty { .. }) {
        validate_array_argument(table_arg, env)?;
    }
    let table_matrix = validate_external_table_constructor_data(args, env, is_time_table)?.ok_or(
        EvalError::UnsupportedExpression {
            kind: "external table data",
        },
    )?;

    let columns_idx = if is_time_table { 4 } else { 3 };
    if let Some(columns) = external_table_constructor_arg(args, "columns", columns_idx) {
        if matches!(columns, rumoca_core::Expression::Empty { .. }) {
            return Ok(());
        }
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
            if bind_user_function_input_for_validation(
                &mut local_env,
                function_name,
                input,
                arg,
                env,
            )? {
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
    function_name: &str,
    input: &rumoca_core::FunctionParam,
    arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    if function_param_is_function(input)
        && let Some(closure) = function_closure_from_arg(arg, env)
    {
        local_env
            .function_closures
            .insert(input.name.clone(), closure.clone());
        local_env
            .function_closures
            .insert(format!("{function_name}.{}", input.name), closure);
        return Ok(true);
    }
    if function_param_is_aggregate(input) {
        validate_array_argument(arg, env)?;
        let values = if input.shape_expr.is_empty()
            && let Some(expected_len) = concrete_function_param_size(&input.dims)
        {
            eval_shaped_array_values::<T>(arg, env, expected_len)?
        } else {
            eval_array_values::<T>(arg, env)?
        };
        bind_aggregate_input_for_validation(local_env, &input.name, &input.dims, values)?;
        return Ok(true);
    }
    if copy_record_function_output_fields(local_env, input, arg, env)? {
        return Ok(true);
    }
    if let Ok(value) = eval_expr::<T>(arg, env) {
        bind_function_scalar_input(local_env, function_name, &input.name, value);
        return Ok(true);
    }
    Ok(false)
}

pub(super) fn bind_function_scalar_input<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    function_name: &str,
    input_name: &str,
    value: T,
) {
    local_env.set(input_name, value);
    local_env.set(&format!("{function_name}.{input_name}"), value);
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
            .and_then(|dim| acc.checked_mul(dim))
    })
}

fn bind_aggregate_input_for_validation<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    input_name: &str,
    declared_dims: &[i64],
    values: Vec<T>,
) -> Result<(), EvalError> {
    if values.is_empty() {
        return Ok(());
    }
    let inferred_dims = infer_dims_from_values(declared_dims, values.len())?;
    let dims = inferred_dims
        .iter()
        .map(|dim| {
            i64::try_from(*dim).map_err(|_| EvalError::UnsupportedExpression {
                kind: "array dimensions",
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    set_array_entries(local_env, input_name, &dims, &values);
    std::sync::Arc::make_mut(&mut local_env.dims).insert(input_name.to_string(), dims);
    Ok(())
}

fn copy_selected_input_fields_for_validation<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    input_name: &str,
    arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    let Some(arg_path) = try_eval_field_access_path(arg, env)? else {
        return Ok(false);
    };
    let src_prefix = format!("{arg_path}.");
    let dst_prefix = format!("{input_name}.");
    let mut copied = false;
    for (key, value) in &env.vars {
        let Some(suffix) = key.strip_prefix(&src_prefix) else {
            continue;
        };
        local_env.set(&format!("{dst_prefix}{suffix}"), *value);
        copied = true;
    }
    for (key, start_expr) in env.start_exprs.iter() {
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

fn validate_external_table_constructor_data<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
    is_time_table: bool,
) -> Result<Option<Vec<Vec<f64>>>, EvalError> {
    let table_matrix = eval_external_table_data_matrix(args, env, is_time_table)?;
    if let Some(table_matrix) = &table_matrix {
        validate_external_table_matrix(table_matrix)?;
    }
    Ok(table_matrix)
}

fn validate_external_table_columns_arg<T: SimFloat>(
    columns_arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
    data_col_count: usize,
) -> Result<(), EvalError> {
    for column in eval_array_like_f64_values(columns_arg, env)? {
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
        let first_row = spec.data.first().ok_or(EvalError::ShapeMismatch {
            context: "external table lookup",
            expected: 1,
            actual: 0,
        })?;
        first_row
            .len()
            .checked_sub(1)
            .filter(|count| *count > 0)
            .ok_or(EvalError::UnsupportedExpression {
                kind: "external table lookup output column",
            })?
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

pub(in crate::eval) fn validate_array_argument<T: SimFloat>(
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
        } if subscripts.is_empty() && encoded_slice_field_values(name.as_str(), env)?.is_some() => {
            Ok(())
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty()
            && array_values_from_env_name_generic::<T>(name.as_str(), env)?.is_some() =>
        {
            Ok(())
        }
        rumoca_core::Expression::FieldAccess { base, field, .. }
            if try_eval_field_access_array_values(base, field, env).is_ok() =>
        {
            Ok(())
        }
        _ => validate_expr(expr, env),
    }
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
        .map(|element| eval_array_like_values::<T>(element, env).map(|values| values.len()))
        .collect::<Result<Vec<_>, EvalError>>()?;
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

fn function_call_supported<T: SimFloat>(name: &rumoca_core::VarName, env: &VarEnv<T>) -> bool {
    name.as_str() == "Complex"
        || builtin_function_from_call_name(name).is_some()
        || env.function_closures.contains_key(name.as_str())
        || is_runtime_special_function_name(name)
        || env.functions.contains_key(name.as_str())
}

fn builtin_function_from_call_name(
    name: &rumoca_core::VarName,
) -> Option<rumoca_core::BuiltinFunction> {
    let short_name = name.last_segment();
    rumoca_core::BuiltinFunction::from_name(short_name)
        .or_else(|| rumoca_core::BuiltinFunction::from_name(&short_name.to_ascii_lowercase()))
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
    if let Some(value) = try_eval_index_from_array_like_expr(base, &indices, env)? {
        return Ok(value);
    }

    try_eval_index_from_nested_expr(base, &indices, env).ok_or_else(|| EvalError::MissingBinding {
        name: checked_index_missing_name(base, &indices, env),
    })
}

fn try_eval_index_from_array_like_expr<T: SimFloat>(
    base: &rumoca_core::Expression,
    indices: &[usize],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    if indices.is_empty() {
        return Ok(None);
    }
    let values = eval_array_like_values(base, env)?;
    let dims = array_like_index_dims(base, &values, indices, env)?;
    let Some(flat_index) = flat_index_from_dims(&dims, indices) else {
        return Ok(None);
    };
    Ok(values.get(flat_index).copied())
}

fn array_like_index_dims<T: SimFloat>(
    base: &rumoca_core::Expression,
    values: &[T],
    indices: &[usize],
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    let dims = try_infer_runtime_expr_dims(base, env)?;
    if !dims.is_empty() {
        Ok(dims)
    } else if indices.len() == 1 {
        Ok(vec![values.len()])
    } else {
        Err(EvalError::UnsupportedExpression {
            kind: "array-like index shape",
        })
    }
}

fn flat_index_from_dims(dims: &[usize], indices: &[usize]) -> Option<usize> {
    if dims.len() != indices.len() {
        return None;
    }
    let mut flat_index = 0usize;
    for (dim, index) in dims.iter().zip(indices.iter()) {
        if *dim == 0 || *index == 0 || *index > *dim {
            return None;
        }
        flat_index = flat_index.saturating_mul(*dim);
        flat_index = flat_index.saturating_add(index.saturating_sub(1));
    }
    Some(flat_index)
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

pub(super) fn eval_index_from_env_path<T: SimFloat>(
    base_path: &str,
    indices: &[usize],
    env: &VarEnv<T>,
) -> Option<T> {
    if indices.is_empty() {
        return env.vars.get(base_path).copied();
    }

    let dims = env.dims.get(base_path)?;
    if dims.len() != indices.len() {
        return None;
    }
    let dims = dims
        .iter()
        .map(|dim| usize::try_from(*dim).ok())
        .collect::<Option<Vec<_>>>()?;
    let flat_index = flat_index_from_dims(&dims, indices)?;
    let values = array_values_from_env_name_generic(base_path, env).ok()??;
    values.get(flat_index).copied()
}

fn checked_index_missing_name<T: SimFloat>(
    base: &rumoca_core::Expression,
    indices: &[usize],
    env: &VarEnv<T>,
) -> String {
    if let Ok(Some(path)) = try_eval_field_access_path(base, env) {
        return dae::format_subscript_key(&path, indices);
    }
    "indexed expression".to_string()
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
        } => eval_matrix_index(expr, indices, env).ok().flatten(),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => eval_index_from_env_path(name.as_str(), indices, env),
        _ if indices.len() == 1 => {
            let values = eval_array_like_values::<T>(expr, env).ok()?;
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

fn eval_subscript_indices<T: SimFloat>(
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    subscripts
        .iter()
        .map(|sub| match sub {
            rumoca_core::Subscript::Index { value, span } => {
                positive_subscript_index(*value, *span)
            }
            rumoca_core::Subscript::Expr { expr, span } => {
                let value = eval_expr::<T>(expr, env)?.real();
                finite_integer_subscript_index(value, *span)
            }
            rumoca_core::Subscript::Colon { span } => Err(EvalError::UnsupportedExpression {
                kind: "colon subscript",
            }
            .with_span_if_missing(*span)),
        })
        .collect()
}

fn positive_subscript_index(index: i64, span: rumoca_core::Span) -> Result<usize, EvalError> {
    usize::try_from(index)
        .ok()
        .filter(|index| *index > 0)
        .ok_or_else(|| {
            EvalError::UnsupportedExpression {
                kind: "subscript index",
            }
            .with_span_if_missing(span)
        })
}

fn finite_integer_subscript_index(value: f64, span: rumoca_core::Span) -> Result<usize, EvalError> {
    if !value.is_finite() || value.fract() != 0.0 {
        return Err(EvalError::UnsupportedExpression {
            kind: "subscript expression",
        }
        .with_span_if_missing(span));
    }
    positive_subscript_index(value as i64, span)
}

pub(super) fn try_eval_field_access_path<T: SimFloat>(
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
                let indices = eval_subscript_indices(subscripts, env)?;
                Ok(Some(dae::format_subscript_key(name.as_str(), &indices)))
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
            let indices = eval_subscript_indices(subscripts, env)?;
            Ok(Some(dae::format_subscript_key(&prefix, &indices)))
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
) -> Result<(VarEnv<T>, Vec<T>), EvalError> {
    let mut local_env = env.clone();
    let mut input_values = Vec::with_capacity(constructor.inputs.len());
    let (named_args, positional_args) = split_named_and_positional_call_args(args);
    let mut positional_idx = 0usize;
    for input in &constructor.inputs {
        let value = if let Some(arg_expr) = named_args.get(input.name.as_str()) {
            eval_expr::<T>(arg_expr, &local_env)?
        } else if let Some(arg_expr) = positional_args.get(positional_idx) {
            positional_idx += 1;
            eval_expr::<T>(arg_expr, &local_env)?
        } else if let Some(default_expr) = &input.default {
            eval_expr::<T>(default_expr, &local_env)?
        } else if let Some(existing) = local_env.vars.get(&input.name).copied() {
            existing
        } else {
            return Err(EvalError::MissingBinding {
                name: input.name.clone(),
            });
        };
        bind_function_scalar_input(
            &mut local_env,
            constructor.name.as_str(),
            &input.name,
            value,
        );
        input_values.push(value);
    }
    Ok((local_env, input_values))
}

pub(super) fn eval_field_access_constructor_by_signature<T: SimFloat>(
    base_name: &rumoca_core::VarName,
    args: &[rumoca_core::Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let Some(constructor) = env.functions.get(base_name.as_str()) else {
        return Ok(None);
    };
    let (local_env, input_values) = bind_constructor_inputs(constructor, args, env)?;

    if let Some((idx, _)) = constructor
        .inputs
        .iter()
        .enumerate()
        .find(|(_, input)| input.name == field)
    {
        return Ok(input_values.get(idx).copied());
    }

    if let Some(output) = constructor
        .outputs
        .iter()
        .find(|output| output.name == field)
    {
        if let Some(default_expr) = &output.default {
            return eval_expr::<T>(default_expr, &local_env).map(Some);
        }
        if let Some(value) = local_env.vars.get(&output.name).copied() {
            return Ok(Some(value));
        }
    }

    Ok(None)
}

fn try_eval_field_access<T: SimFloat>(
    base: &rumoca_core::Expression,
    field: &str,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if let rumoca_core::Expression::Index {
        base, subscripts, ..
    } = base
    {
        let indices = try_eval_index_subscripts(subscripts, env)?;
        if let Some(value) = eval_indexed_field_from_nested_expr(base, &indices, field, env)? {
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
        return Err(EvalError::MissingBinding { name: key });
    }

    if let rumoca_core::Expression::If {
        branches,
        else_branch,
        ..
    } = base
    {
        for (cond, then_expr) in branches {
            if eval_expr::<T>(cond, env)?.real() != 0.0 {
                return try_eval_field_access(then_expr, field, env);
            }
        }
        return try_eval_field_access(else_branch, field, env);
    }

    if let Some(projected) = projected_record_field_expr(base) {
        return try_eval_field_access(&projected, field, env);
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
                eval_field_access_constructor_by_signature(name.var_name(), args, field, env)?
        {
            return Ok(value);
        }
        if let Some(value) = try_eval_function_record_scalar_field(name, args, field, env)? {
            return Ok(value);
        }

        let selected = rumoca_core::VarName::new(format!("{}.{}", name.as_str(), field));
        if function_call_supported(&selected, env) {
            validate_expr_slice_checked(args, env)?;
            return eval_function_call::<T>(&selected, args, false, env);
        }
    }

    Err(EvalError::UnsupportedExpression {
        kind: "field access",
    })
}

fn projected_record_field_expr(expr: &rumoca_core::Expression) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::FieldAccess { base, field, .. } = expr else {
        return None;
    };
    constructor_named_field_expr(base, field)
}

fn constructor_named_field_expr(
    expr: &rumoca_core::Expression,
    field: &str,
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::FunctionCall {
        args,
        is_constructor,
        ..
    } = expr
    else {
        return None;
    };
    if !is_constructor {
        return None;
    }
    args.iter().find_map(|arg| {
        let (name, value) = decode_named_constructor_arg(arg)?;
        (name == field).then(|| value.clone())
    })
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

    if let Ok(Some(value)) =
        super::special::eval_state_accessor_from_set_state(name.var_name(), args, field, env)
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
        Err(EvalError::MissingBinding { .. }) => {
            super::special::eval_state_accessor_from_set_state(name.var_name(), args, field, env)
        }
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
        return format!("{}.{}", dae::format_subscript_key(&path, indices), field);
    }
    "indexed field expression".to_string()
}

fn eval_indexed_field_from_nested_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    indices: &[usize],
    field: &str,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    if indices.is_empty() {
        return try_eval_field_access(expr, field, env).map(Some);
    }

    match expr {
        // MLS Chapter 10 array indexing selects the element before later
        // component selection, so array/tuple literals must recurse first.
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            let Some(idx0) = indices[0].checked_sub(1) else {
                return Ok(None);
            };
            let Some(element) = elements.get(idx0) else {
                return Ok(None);
            };
            eval_indexed_field_from_nested_expr(element, &indices[1..], field, env)
        }
        _ => {
            let Some(path) = try_eval_field_access_path(expr, env)? else {
                return Ok(None);
            };
            let key = format!("{}.{field}", dae::format_subscript_key(&path, indices));
            Ok(env.vars.get(&key).copied())
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

fn try_eval_var_ref<T: SimFloat>(
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if subscripts.is_empty() {
        return eval_var_ref_no_subscripts(name.as_str(), env)?.ok_or_else(|| {
            EvalError::MissingBinding {
                name: name.to_string(),
            }
        });
    }
    if subscripts
        .iter()
        .any(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
    {
        return Err(EvalError::UnsupportedExpression {
            kind: "colon subscript",
        });
    }
    if subscripts.len() == 1
        && let rumoca_core::Subscript::Expr { expr, .. } = &subscripts[0]
        && matches!(expr.as_ref(), rumoca_core::Expression::Range { .. })
    {
        let values = eval_array_values(
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
    let indices = try_eval_index_subscripts(subscripts, env)?;
    if let Some(value) = eval_index_from_env_path(name.as_str(), &indices, env) {
        return Ok(value);
    }
    Err(EvalError::MissingBinding {
        name: name.to_string(),
    })
}

/// Look up a variable with no explicit subscripts.
pub(super) fn eval_var_ref_no_subscripts<T: SimFloat>(
    raw: &str,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    if let Some(value) = lowered_pre_parameter_value(raw, env)? {
        return Ok(Some(value));
    }
    if let Some(&v) = env.vars.get(raw) {
        return Ok(Some(v));
    }
    if let Some(caller) = current_function_call_name(&env.runtime)
        && let Some(field) = complex_field_selection_from_path(&caller)
    {
        let selected_key = format!("{raw}.{field}");
        if let Some(&v) = env.vars.get(selected_key.as_str()) {
            return Ok(Some(v));
        }
    }
    if let Some(ordinal) = lookup_enum_literal_ordinal(raw, &env.enum_literal_ordinals) {
        return Ok(Some(T::from_f64(ordinal as f64)));
    }
    Ok(None)
}

fn lowered_pre_parameter_value<T: SimFloat>(
    raw: &str,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let Some(target) = raw.strip_prefix("__pre__.") else {
        return Ok(None);
    };
    if let Some(value) = lookup_pre_value_in(&env.runtime, target) {
        return Ok(Some(T::from_f64(value)));
    }
    Ok(None)
}

pub(super) fn lookup_enum_literal_ordinal(
    raw: &str,
    ordinals: &IndexMap<String, i64>,
) -> Option<i64> {
    if let Some(&ordinal) = ordinals.get(raw) {
        return Some(ordinal);
    }
    let mut raw_parts = rumoca_core::ComponentPath::from_flat_path(raw).into_parts();
    if raw_parts.len() < 2 {
        return None;
    }
    let literal = raw_parts.pop()?;
    let literal = literal.as_str();
    let prefix = raw_parts.join(".");
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
        let path = rumoca_core::ComponentPath::from_flat_path(name);
        let Some(literal) = (path.len() >= 2).then(|| path.parts().last()).flatten() else {
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

pub(super) fn try_eval_vector_values<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            let Some(dims) = env.dims.get(name.as_str()) else {
                return Ok(None);
            };
            if dims.len() != 1 || dims[0] <= 1 {
                return Ok(None);
            }
            Ok(array_values_from_env_name_generic(name.as_str(), env)?
                .filter(|values| values.len() > 1))
        }
        rumoca_core::Expression::Array { is_matrix, .. } if !*is_matrix => {
            let values = eval_array_values(expr, env)?;
            Ok((values.len() > 1).then_some(values))
        }
        _ => Ok(None),
    }
}

pub(super) fn eval_vector_dot_product<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Option<T> {
    try_eval_vector_dot_product(lhs, rhs, env).ok().flatten()
}

pub(super) fn try_eval_vector_dot_product<T: SimFloat>(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let Some(lhs_values) = try_eval_vector_values(lhs, env)? else {
        return Ok(None);
    };
    let Some(rhs_values) = try_eval_vector_values(rhs, env)? else {
        return Ok(None);
    };
    if lhs_values.len() != rhs_values.len() || lhs_values.is_empty() {
        return Ok(None);
    }

    Ok(Some(
        lhs_values
            .iter()
            .zip(rhs_values.iter())
            .fold(T::zero(), |acc, (l, r)| acc + (*l * *r)),
    ))
}
