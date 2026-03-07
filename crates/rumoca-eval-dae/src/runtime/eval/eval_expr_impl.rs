use super::*;

/// Evaluate a dae::Expression to a value of type T.
pub fn eval_expr<T: SimFloat>(expr: &dae::Expression, env: &VarEnv<T>) -> T {
    match expr {
        dae::Expression::Literal(lit) => eval_literal::<T>(lit),
        dae::Expression::VarRef { name, subscripts } => eval_var_ref::<T>(name, subscripts, env),
        dae::Expression::Binary { op, lhs, rhs } => eval_binary::<T>(op, lhs, rhs, env),
        dae::Expression::Unary { op, rhs } => eval_unary::<T>(op, rhs, env),
        dae::Expression::BuiltinCall { function, args } => eval_builtin::<T>(*function, args, env),
        dae::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => eval_function_call::<T>(name, args, *is_constructor, env),
        dae::Expression::If {
            branches,
            else_branch,
        } => eval_if::<T>(branches, else_branch, env),
        dae::Expression::Array { elements, .. } => {
            if let Some(first) = elements.first() {
                eval_expr::<T>(first, env)
            } else {
                T::zero()
            }
        }
        dae::Expression::Index { base, subscripts } => eval_index_expr::<T>(base, subscripts, env),
        dae::Expression::FieldAccess { base, field } => eval_field_access::<T>(base, field, env),
        dae::Expression::Empty => T::zero(),
        dae::Expression::Range { .. }
        | dae::Expression::Tuple { .. }
        | dae::Expression::ArrayComprehension { .. } => T::zero(),
    }
}

pub(super) fn eval_index_expr<T: SimFloat>(
    base: &dae::Expression,
    subscripts: &[dae::Subscript],
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

pub(super) fn eval_index_subscripts<T: SimFloat>(
    subscripts: &[dae::Subscript],
    env: &VarEnv<T>,
) -> Option<Vec<usize>> {
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let raw = match subscript {
            dae::Subscript::Index(i) => *i as f64,
            dae::Subscript::Expr(expr) => eval_expr::<T>(expr, env).real().round(),
            dae::Subscript::Colon => return None,
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

    let joined = indices
        .iter()
        .map(|idx| idx.to_string())
        .collect::<Vec<_>>()
        .join(",");
    let direct_key = format!("{base_path}[{joined}]");
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
    let flat_key = format!("{base_path}[{}]", flat_index + 1);
    env.vars.get(&flat_key).copied()
}

pub(super) fn eval_index_from_nested_expr<T: SimFloat>(
    expr: &dae::Expression,
    indices: &[usize],
    env: &VarEnv<T>,
) -> Option<T> {
    if indices.is_empty() {
        return Some(eval_expr::<T>(expr, env));
    }

    match expr {
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            let idx0 = indices[0].checked_sub(1)?;
            let element = elements.get(idx0)?;
            eval_index_from_nested_expr(element, &indices[1..], env)
        }
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            eval_index_from_env_path(name.as_str(), indices, env)
        }
        _ if indices.len() == 1 => {
            let values = eval_array_like_values::<T>(expr, env);
            values.get(indices[0].checked_sub(1)?).copied()
        }
        _ => None,
    }
}

pub(super) fn with_function_call_stack<R>(name: &str, f: impl FnOnce() -> R) -> R {
    FUNC_CALL_STACK.with(|stack| stack.borrow_mut().push(name.to_string()));
    let out = f();
    FUNC_CALL_STACK.with(|stack| {
        let mut stack = stack.borrow_mut();
        let _ = stack.pop();
    });
    out
}

pub(super) fn current_function_call_name() -> Option<String> {
    FUNC_CALL_STACK.with(|stack| stack.borrow().last().cloned())
}

pub(super) fn eval_subscript_indices<T: SimFloat>(
    subscripts: &[dae::Subscript],
    env: &VarEnv<T>,
) -> Vec<String> {
    subscripts
        .iter()
        .map(|sub| match sub {
            dae::Subscript::Index(i) => i.to_string(),
            dae::Subscript::Expr(expr) => eval_expr::<T>(expr, env).real().round().to_string(),
            dae::Subscript::Colon => ":".to_string(),
        })
        .collect()
}

pub(super) fn eval_field_access_path<T: SimFloat>(
    expr: &dae::Expression,
    env: &VarEnv<T>,
) -> Option<String> {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            if subscripts.is_empty() {
                Some(name.as_str().to_string())
            } else {
                let idx = eval_subscript_indices(subscripts, env);
                Some(format!("{}[{}]", name.as_str(), idx.join(",")))
            }
        }
        dae::Expression::FieldAccess { base, field } => {
            let prefix = eval_field_access_path(base, env)?;
            Some(format!("{prefix}.{field}"))
        }
        _ => None,
    }
}

pub(super) fn eval_field_access_constructor<T: SimFloat>(
    base_name: &dae::VarName,
    args: &[dae::Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    let field_idx = match field {
        // Modelica.Complex and most scalar record constructors use positional
        // constructor arguments in declared field order.
        "re" => 0,
        "im" => 1,
        _ => return None,
    };
    let arg = args.get(field_idx)?;
    let _ = base_name;
    Some(eval_expr::<T>(arg, env))
}

const NAMED_CONSTRUCTOR_ARG_PREFIX: &str = "__rumoca_named_arg__.";

pub(super) fn decode_named_constructor_arg(
    expr: &dae::Expression,
) -> Option<(&str, &dae::Expression)> {
    let dae::Expression::FunctionCall {
        name,
        args,
        is_constructor: _,
    } = expr
    else {
        return None;
    };
    let named = name.as_str().strip_prefix(NAMED_CONSTRUCTOR_ARG_PREFIX)?;
    let value = args.first()?;
    Some((named, value))
}

pub(super) fn split_named_and_positional_call_args(
    args: &[dae::Expression],
) -> (HashMap<&str, &dae::Expression>, Vec<&dae::Expression>) {
    let mut named_args: HashMap<&str, &dae::Expression> = HashMap::new();
    let mut positional_args: Vec<&dae::Expression> = Vec::new();
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
    constructor: &dae::Function,
    args: &[dae::Expression],
    env: &VarEnv<T>,
) -> (VarEnv<T>, Vec<T>) {
    let mut local_env = env.clone();
    let mut input_values = Vec::with_capacity(constructor.inputs.len());
    let (named_args, positional_args) = split_named_and_positional_call_args(args);
    let mut positional_idx = 0usize;
    for input in &constructor.inputs {
        let value = if let Some(arg_expr) = named_args.get(input.name.as_str()) {
            eval_expr::<T>(arg_expr, &local_env)
        } else if let Some(arg_expr) = positional_args.get(positional_idx) {
            positional_idx += 1;
            eval_expr::<T>(arg_expr, &local_env)
        } else if let Some(default_expr) = &input.default {
            eval_expr::<T>(default_expr, &local_env)
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
    base_name: &dae::VarName,
    args: &[dae::Expression],
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
            return Some(eval_expr::<T>(default_expr, &local_env));
        }
        if let Some(value) = local_env.vars.get(&output.name).copied() {
            return Some(value);
        }
    }

    None
}

pub(super) fn eval_field_access<T: SimFloat>(
    base: &dae::Expression,
    field: &str,
    env: &VarEnv<T>,
) -> T {
    if let Some(path) = eval_field_access_path(base, env) {
        let key = format!("{path}.{field}");
        if let Some(value) = env.vars.get(&key).copied() {
            return value;
        }
    }

    if let dae::Expression::FunctionCall {
        name,
        args,
        is_constructor,
    } = base
    {
        if *is_constructor
            && let Some(value) = eval_field_access_constructor(name, args, field, env)
        {
            return value;
        }
        if *is_constructor
            && let Some(value) = eval_field_access_constructor_by_signature(name, args, field, env)
        {
            return value;
        }

        let projected = dae::VarName::new(format!("{}.{}", name.as_str(), field));
        let value = eval_function_call::<T>(&projected, args, false, env);
        if value.real().is_finite() {
            return value;
        }
    }

    T::zero()
}

pub(super) fn eval_literal<T: SimFloat>(lit: &dae::Literal) -> T {
    match lit {
        dae::Literal::Real(v) => T::from_f64(*v),
        dae::Literal::Integer(v) => T::from_f64(*v as f64),
        dae::Literal::Boolean(v) => T::from_bool(*v),
        dae::Literal::String(_) => T::zero(),
    }
}

pub(super) fn eval_var_ref<T: SimFloat>(
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    env: &VarEnv<T>,
) -> T {
    if subscripts.is_empty() {
        return eval_var_ref_no_subscripts(name.as_str(), env);
    }
    {
        let indices: Vec<String> = subscripts
            .iter()
            .map(|s| match s {
                dae::Subscript::Index(i) => format!("{}", i),
                dae::Subscript::Expr(expr) => {
                    let v = eval_expr::<T>(expr, env);
                    format!("{}", v.real() as i64)
                }
                dae::Subscript::Colon => ":".to_string(),
            })
            .collect();
        let indexed_name = format!("{}[{}]", name.as_str(), indices.join(","));
        let val = env.vars.get(&indexed_name).copied();
        val.unwrap_or_else(|| env.get(name.as_str()))
    }
}

/// Look up a variable with no explicit subscripts.
/// Handles names with embedded subscript expressions like `x[(2-1)]`.
pub(super) fn eval_var_ref_no_subscripts<T: SimFloat>(raw: &str, env: &VarEnv<T>) -> T {
    if let Some(&v) = env.vars.get(raw) {
        return v;
    }
    if let Some(caller) = current_function_call_name() {
        let projection_field = if caller.ends_with(".re") {
            Some("re")
        } else if caller.ends_with(".im") {
            Some("im")
        } else {
            None
        };
        if let Some(field) = projection_field {
            let projected_key = format!("{raw}.{field}");
            if let Some(&v) = env.vars.get(projected_key.as_str()) {
                return v;
            }
        }
    }
    // If name contains brackets with expressions, try normalizing.
    if raw.contains('[') {
        if let Some(v) =
            normalize_var_name::<T>(raw, env).and_then(|n| env.vars.get(n.as_str()).copied())
        {
            return v;
        }
        if let Some(base_name) = unity_subscript_base_name(raw)
            && let Some(&v) = env.vars.get(base_name.as_str())
        {
            return v;
        }
    }
    if let Some(ordinal) = lookup_enum_literal_ordinal(raw, &env.enum_literal_ordinals) {
        return T::from_f64(ordinal as f64);
    }
    T::zero()
}

pub(super) fn lookup_enum_literal_ordinal(
    raw: &str,
    ordinals: &IndexMap<String, i64>,
) -> Option<i64> {
    if let Some(&ordinal) = ordinals.get(raw) {
        return Some(ordinal);
    }
    let (prefix, literal) = raw.rsplit_once('.')?;
    if let Some(unquoted) = strip_quoted_identifier(literal) {
        let alt = format!("{prefix}.{unquoted}");
        return ordinals.get(&alt).copied();
    }
    let alt = format!("{prefix}.'{literal}'");
    ordinals.get(&alt).copied()
}

pub(super) fn strip_quoted_identifier(segment: &str) -> Option<&str> {
    if segment.len() >= 2 && segment.starts_with('\'') && segment.ends_with('\'') {
        Some(&segment[1..segment.len() - 1])
    } else {
        None
    }
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
    expr: &dae::Expression,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    match expr {
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            let dims = env.dims.get(name.as_str())?;
            if dims.len() != 1 || dims[0] <= 1 {
                return None;
            }
            array_values_from_env_name_generic(name.as_str(), env).filter(|values| values.len() > 1)
        }
        dae::Expression::Array { is_matrix, .. } if !*is_matrix => {
            let values = eval_array_values(expr, env);
            (values.len() > 1).then_some(values)
        }
        _ => None,
    }
}

pub(super) fn eval_vector_dot_product<T: SimFloat>(
    lhs: &dae::Expression,
    rhs: &dae::Expression,
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
    op: &rumoca_ir_core::OpBinary,
    lhs: &dae::Expression,
    rhs: &dae::Expression,
    env: &VarEnv<T>,
) -> T {
    if matches!(op, rumoca_ir_core::OpBinary::Mul(_))
        && let Some(dot) = eval_vector_dot_product(lhs, rhs, env)
    {
        return dot;
    }

    let l = eval_expr::<T>(lhs, env);
    let r = eval_expr::<T>(rhs, env);
    match op {
        rumoca_ir_core::OpBinary::Add(_) | rumoca_ir_core::OpBinary::AddElem(_) => l + r,
        rumoca_ir_core::OpBinary::Sub(_) | rumoca_ir_core::OpBinary::SubElem(_) => l - r,
        rumoca_ir_core::OpBinary::Mul(_) | rumoca_ir_core::OpBinary::MulElem(_) => l * r,
        rumoca_ir_core::OpBinary::Div(_) | rumoca_ir_core::OpBinary::DivElem(_) => {
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
        rumoca_ir_core::OpBinary::Exp(_) | rumoca_ir_core::OpBinary::ExpElem(_) => l.powf(r),
        rumoca_ir_core::OpBinary::And(_) => T::from_bool(l.to_bool() && r.to_bool()),
        rumoca_ir_core::OpBinary::Or(_) => T::from_bool(l.to_bool() || r.to_bool()),
        rumoca_ir_core::OpBinary::Lt(_) => T::from_bool(l.lt(r)),
        rumoca_ir_core::OpBinary::Le(_) => T::from_bool(l.le(r)),
        rumoca_ir_core::OpBinary::Gt(_) => T::from_bool(l.gt(r)),
        rumoca_ir_core::OpBinary::Ge(_) => T::from_bool(l.ge(r)),
        rumoca_ir_core::OpBinary::Eq(_) => T::from_bool(l.eq_approx(r)),
        rumoca_ir_core::OpBinary::Neq(_) => T::from_bool(!l.eq_approx(r)),
        rumoca_ir_core::OpBinary::Empty | rumoca_ir_core::OpBinary::Assign(_) => T::zero(),
    }
}

pub(super) fn eval_unary<T: SimFloat>(
    op: &rumoca_ir_core::OpUnary,
    rhs: &dae::Expression,
    env: &VarEnv<T>,
) -> T {
    let r = eval_expr::<T>(rhs, env);
    match op {
        rumoca_ir_core::OpUnary::Minus(_) | rumoca_ir_core::OpUnary::DotMinus(_) => -r,
        rumoca_ir_core::OpUnary::Plus(_) | rumoca_ir_core::OpUnary::DotPlus(_) => r,
        rumoca_ir_core::OpUnary::Not(_) => T::from_bool(!r.to_bool()),
        rumoca_ir_core::OpUnary::Empty => r,
    }
}

pub(super) fn eval_builtin_pre<T: SimFloat>(args: &[dae::Expression], env: &VarEnv<T>) -> T {
    let Some(arg0) = args.first() else {
        return T::zero();
    };

    if let dae::Expression::VarRef { name, subscripts } = arg0 {
        let key = if subscripts.is_empty() {
            name.as_str().to_string()
        } else {
            let indices = eval_subscript_indices(subscripts, env);
            format!("{}[{}]", name.as_str(), indices.join(","))
        };

        if let Some(value) = lookup_pre_value(&key) {
            return T::from_f64(value);
        }
        if let Some(normalized) = normalize_var_name::<T>(&key, env)
            && let Some(value) = lookup_pre_value(normalized.as_str())
        {
            return T::from_f64(value);
        }
        if let Some(base_name) = unity_subscript_base_name(&key)
            && let Some(value) = lookup_pre_value(base_name.as_str())
        {
            return T::from_f64(value);
        }
    }

    eval_expr::<T>(arg0, env)
}

pub(super) fn eval_builtin<T: SimFloat>(
    function: dae::BuiltinFunction,
    args: &[dae::Expression],
    env: &VarEnv<T>,
) -> T {
    let arg = |i: usize| -> T {
        args.get(i)
            .map(|a| eval_expr::<T>(a, env))
            .unwrap_or(T::zero())
    };

    match function {
        dae::BuiltinFunction::Der => {
            if let Some(dae::Expression::VarRef { name, .. }) = args.first() {
                let der_name = format!("der({})", name.as_str());
                env.get(&der_name)
            } else {
                T::zero()
            }
        }
        dae::BuiltinFunction::Pre => eval_builtin_pre(args, env),

        // Math functions
        dae::BuiltinFunction::Abs => arg(0).abs(),
        dae::BuiltinFunction::Sign => arg(0).sign(),
        dae::BuiltinFunction::Sqrt => arg(0).sqrt(),
        dae::BuiltinFunction::Floor | dae::BuiltinFunction::Integer => arg(0).floor(),
        dae::BuiltinFunction::Ceil => arg(0).ceil(),
        dae::BuiltinFunction::Min => eval_builtin_min(args, env),
        dae::BuiltinFunction::Max => eval_builtin_max(args, env),
        dae::BuiltinFunction::Div => eval_div_mod_rem(arg(0), arg(1), DivKind::Div),
        dae::BuiltinFunction::Mod => eval_div_mod_rem(arg(0), arg(1), DivKind::Mod),
        dae::BuiltinFunction::Rem => eval_div_mod_rem(arg(0), arg(1), DivKind::Rem),
        dae::BuiltinFunction::SemiLinear => {
            let x = arg(0);
            if x.real() >= 0.0 {
                arg(1) * x
            } else {
                arg(2) * x
            }
        }

        // Trig / hyperbolic / exp
        _ => eval_builtin_math_and_event(function, args, env),
    }
}

pub(super) fn eval_builtin_min<T: SimFloat>(args: &[dae::Expression], env: &VarEnv<T>) -> T {
    if args.is_empty() {
        return T::zero();
    }
    if args.len() == 1 {
        return reduce_array_argument(&args[0], env, |acc, v| acc.min(v), T::zero());
    }
    let mut it = args.iter().map(|expr| eval_expr::<T>(expr, env));
    let first = it.next().unwrap_or_else(T::zero);
    it.fold(first, |acc, v| acc.min(v))
}

pub(super) fn eval_builtin_max<T: SimFloat>(args: &[dae::Expression], env: &VarEnv<T>) -> T {
    if args.is_empty() {
        return T::zero();
    }
    if args.len() == 1 {
        return reduce_array_argument(&args[0], env, |acc, v| acc.max(v), T::zero());
    }
    let mut it = args.iter().map(|expr| eval_expr::<T>(expr, env));
    let first = it.next().unwrap_or_else(T::zero);
    it.fold(first, |acc, v| acc.max(v))
}

pub(super) fn reduce_array_argument<T: SimFloat, F: FnMut(T, T) -> T>(
    arg: &dae::Expression,
    env: &VarEnv<T>,
    reduce: F,
    default: T,
) -> T {
    let mut values = eval_array_like_values(arg, env).into_iter();
    let Some(first) = values.next() else {
        return default;
    };
    values.fold(first, reduce)
}

pub(super) enum DivKind {
    Div,
    Mod,
    Rem,
}

pub(super) fn eval_div_mod_rem<T: SimFloat>(x: T, divisor: T, kind: DivKind) -> T {
    if divisor.real() == 0.0 {
        return T::zero();
    }
    match kind {
        DivKind::Div => (x / divisor).trunc(),
        DivKind::Mod | DivKind::Rem => x.modulo(divisor),
    }
}

pub(super) fn eval_builtin_math_and_event<T: SimFloat>(
    function: dae::BuiltinFunction,
    args: &[dae::Expression],
    env: &VarEnv<T>,
) -> T {
    let arg = |i: usize| -> T {
        args.get(i)
            .map(|a| eval_expr::<T>(a, env))
            .unwrap_or(T::zero())
    };

    match function {
        // Trigonometric
        dae::BuiltinFunction::Sin => arg(0).sin(),
        dae::BuiltinFunction::Cos => arg(0).cos(),
        dae::BuiltinFunction::Tan => arg(0).tan(),
        dae::BuiltinFunction::Asin => arg(0).asin(),
        dae::BuiltinFunction::Acos => arg(0).acos(),
        dae::BuiltinFunction::Atan => arg(0).atan(),
        dae::BuiltinFunction::Atan2 => arg(0).atan2(arg(1)),
        // Hyperbolic
        dae::BuiltinFunction::Sinh => arg(0).sinh(),
        dae::BuiltinFunction::Cosh => arg(0).cosh(),
        dae::BuiltinFunction::Tanh => arg(0).tanh(),
        // Exponential/logarithmic
        dae::BuiltinFunction::Exp => arg(0).exp(),
        dae::BuiltinFunction::Log => arg(0).ln(),
        dae::BuiltinFunction::Log10 => arg(0).log10(),

        // Event-related / pass-through
        dae::BuiltinFunction::Edge => {
            let current = arg(0).to_bool();
            let previous = eval_builtin_pre(&args[..args.len().min(1)], env).to_bool();
            if current && !previous {
                T::one()
            } else {
                T::zero()
            }
        }
        dae::BuiltinFunction::Change => {
            let current = arg(0).to_bool();
            let previous = eval_builtin_pre(&args[..args.len().min(1)], env).to_bool();
            if current != previous {
                T::one()
            } else {
                T::zero()
            }
        }
        dae::BuiltinFunction::Initial => {
            if env.is_initial {
                T::one()
            } else {
                T::zero()
            }
        }
        dae::BuiltinFunction::Terminal => T::zero(),
        dae::BuiltinFunction::Sample => eval_builtin_sample(args, env),
        dae::BuiltinFunction::Reinit => arg(1),
        dae::BuiltinFunction::NoEvent | dae::BuiltinFunction::Delay => arg(0),
        dae::BuiltinFunction::Smooth => arg(1),
        dae::BuiltinFunction::Homotopy => arg(0),

        // Reduction operators
        dae::BuiltinFunction::Sum => eval_builtin_sum(args, env),
        dae::BuiltinFunction::Product => eval_builtin_product(args, env),

        // Size builtin: size(A, dim) → dimension of A along dim
        dae::BuiltinFunction::Size => {
            if let Some(dae::Expression::VarRef { name, .. }) = args.first()
                && let Some(d) = env.dims.get(name.as_str())
            {
                let dim_idx = if args.len() > 1 {
                    arg(1).real() as usize
                } else {
                    1
                };
                let val = d.get(dim_idx.saturating_sub(1)).copied().unwrap_or(1);
                return T::from_f64(val as f64);
            }
            T::one()
        }

        // Array functions with scalar fallbacks
        dae::BuiltinFunction::Zeros => T::zero(),
        dae::BuiltinFunction::Ones => T::one(),
        dae::BuiltinFunction::Fill
        | dae::BuiltinFunction::Scalar
        | dae::BuiltinFunction::Vector => arg(0),
        dae::BuiltinFunction::Linspace => eval_linspace_values(args, env)
            .first()
            .copied()
            .unwrap_or_else(T::zero),
        dae::BuiltinFunction::Identity => T::one(),
        dae::BuiltinFunction::Cat => eval_cat_f64_values(args, env)
            .first()
            .copied()
            .map(T::from_f64)
            .unwrap_or_else(T::zero),

        // Unsupported array functions
        _ => {
            warn_once!(
                WARNED_ARRAY_BUILTINS,
                "Array builtin function {:?} not supported in scalar simulation evaluator, \
                 returning NaN. Results may be incorrect.",
                function
            );
            T::nan()
        }
    }
}
