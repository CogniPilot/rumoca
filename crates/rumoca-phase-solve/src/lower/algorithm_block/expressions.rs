//! Lowering of GALEC expressions.

use super::*;

pub(super) fn lower_expression<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    expression: &Expression,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    let arithmetic = scope.scope.arithmetic;
    match expression {
        Expression::Bool(value) => builder.constant(SolveValue::boolean(*value), span),
        Expression::Real(value) => builder.constant(SolveValue::real(arithmetic, *value), span),
        Expression::Integer(value) => builder.constant(
            SolveValue::integer(arithmetic, *value)
                .map_err(|_| SolveProgramConstructionError::TypeMismatch { provenance: span })?,
            span,
        ),
        Expression::Ref(reference) => lower_reference(builder, scope, env, reference, span),
        Expression::Neg(reference) => {
            let value = lower_reference(builder, scope, env, reference, span)?;
            builder.unary(SolveUnaryOperator::Negate, value, span)
        }
        Expression::Size { .. } => {
            let size = static_int(scope, expression)
                .ok_or(SolveProgramConstructionError::InvalidRegion { provenance: span })?;
            builder.constant(
                SolveValue::integer(arithmetic, size).map_err(|_| {
                    SolveProgramConstructionError::TypeMismatch { provenance: span }
                })?,
                span,
            )
        }
        Expression::Paren(inner) => lower_expression(builder, scope, env, inner, span),
        Expression::Not(inner) => {
            let value = lower_expression(builder, scope, env, inner, span)?;
            builder.unary(SolveUnaryOperator::Not, value, span)
        }
        Expression::Binary { op, lhs, rhs } => {
            let lhs = lower_expression(builder, scope, env, lhs, span)?;
            if *op == BinaryOp::Pow && exponent_is_two(rhs) && is_real_scalar(builder, lhs, span)? {
                // A Real square is the correctly rounded multiply, with its
                // base evaluated once (as the GALEC C export always printed
                // it): no `powf` on the target.
                return builder.binary(SolveBinaryOperator::Multiply, lhs, lhs, span);
            }
            let rhs = lower_expression(builder, scope, env, rhs, span)?;
            lower_binary(builder, scope, env, *op, lhs, rhs, span)
        }
        Expression::If(if_expression) => {
            lower_if_expression(builder, scope, env, if_expression, span)
        }
        Expression::Call(call) => lower_call(builder, scope, env, call, span),
        // The pre-pass refused every other expression.
        _ => Err(SolveProgramConstructionError::InvalidRegion { provenance: span }),
    }
}

/// Whether an exponent is exactly the literal 2: bare, parenthesized, or
/// as the `real(2)` the GALEC projection wraps a mixed-type power in.
fn exponent_is_two(exponent: &Expression) -> bool {
    match exponent {
        Expression::Integer(value) => *value == 2,
        Expression::Real(value) => value.to_bits() == 2.0_f64.to_bits(),
        Expression::Paren(inner) => exponent_is_two(inner),
        Expression::Call(call) => {
            Builtin::of(call) == Some(Builtin::ToReal)
                && matches!(call.arguments.as_slice(), [argument] if exponent_is_two(argument))
        }
        _ => false,
    }
}

fn is_real_scalar<'p>(
    builder: &TypedProgramBuilder<'p>,
    value: ProgramRegister<'p>,
    span: Span,
) -> Result<bool, SolveProgramConstructionError> {
    let value_type = builder.value_type_of(value, span)?;
    Ok(value_type.dimensions().is_empty()
        && matches!(value_type.element_type(), SolveScalarType::Real { .. }))
}

/// A variable's value, or one element of it (1-based Modelica indices).
fn lower_reference<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    reference: &Reference,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    let (id, subscripts) = scope
        .resolve_place(reference)
        .map_err(|_| SolveProgramConstructionError::InvalidRegion { provenance: span })?;
    let value = env.read(builder, scope, id, span)?;
    if subscripts.is_empty() {
        return Ok(value);
    }
    let indices = lower_indices(builder, scope, env, subscripts, span)?;
    builder.project_element_dynamic(value, &indices, span)
}

fn lower_if_expression<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    if_expression: &IfExpression,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    if needs_lazy_arms(scope, if_expression) {
        return lower_lazy_arms(
            builder,
            scope,
            env,
            &if_expression.branches,
            &if_expression.else_value,
            span,
        );
    }
    // No arm can fault or raise a signal, so eager evaluation in source
    // order and a `select` chain compute exactly what GALEC computes.
    let mut arms = Vec::with_capacity(if_expression.branches.len());
    for (condition, value) in &if_expression.branches {
        let condition = lower_expression(builder, scope, env, condition, span)?;
        let value = lower_expression(builder, scope, env, value, span)?;
        arms.push((condition, value));
    }
    let mut result = lower_expression(builder, scope, env, &if_expression.else_value, span)?;
    for (condition, value) in arms.into_iter().rev() {
        result = builder.select(condition, value, result, span)?;
    }
    Ok(result)
}

/// Whether an arm GALEC may not evaluate could fault or raise a signal: then
/// only the selected arm may run. Every condition but
/// the first, and every value, is such an arm.
fn needs_lazy_arms(scope: &MethodScope<'_>, if_expression: &IfExpression) -> bool {
    let effectful = |expression: &Expression| {
        may_fault(expression) || has_comparison(expression) || may_overflow(scope, expression)
    };
    if_expression
        .branches
        .iter()
        .enumerate()
        .any(|(arm, (condition, value))| (arm > 0 && effectful(condition)) || effectful(value))
        || effectful(&if_expression.else_value)
}

/// Whether evaluating `expression` does Integer arithmetic, which faults on
/// overflow.
pub(super) fn may_overflow(scope: &MethodScope<'_>, expression: &Expression) -> bool {
    let integer = |expression: &Expression| {
        expression_type(scope, expression).is_some_and(|value_type| {
            matches!(value_type.element_type(), SolveScalarType::Integer(_))
        })
    };
    match expression {
        Expression::Neg(_) => integer(expression),
        Expression::Binary { op, lhs, rhs } => {
            (matches!(
                op,
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Pow
            ) && (integer(lhs) || integer(rhs)))
                || may_overflow(scope, lhs)
                || may_overflow(scope, rhs)
        }
        Expression::Paren(inner) | Expression::Not(inner) => may_overflow(scope, inner),
        Expression::If(if_expression) => {
            if_expression.branches.iter().any(|(condition, value)| {
                may_overflow(scope, condition) || may_overflow(scope, value)
            }) || may_overflow(scope, &if_expression.else_value)
        }
        Expression::Call(call) => call
            .arguments
            .iter()
            .any(|argument| may_overflow(scope, argument)),
        _ => false,
    }
}

/// Whether evaluating `expression` compares, which may raise NAN.
fn has_comparison(expression: &Expression) -> bool {
    match expression {
        Expression::Binary { op, lhs, rhs } => {
            matches!(
                op,
                BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::Le
                    | BinaryOp::Ge
                    | BinaryOp::Eq
                    | BinaryOp::Ne
            ) || has_comparison(lhs)
                || has_comparison(rhs)
        }
        Expression::Paren(inner) | Expression::Not(inner) => has_comparison(inner),
        Expression::If(if_expression) => {
            if_expression
                .branches
                .iter()
                .any(|(condition, value)| has_comparison(condition) || has_comparison(value))
                || has_comparison(&if_expression.else_value)
        }
        // `integer()` raises NAN or OVERFLOW.
        // A user function may raise anything its clause declares.
        Expression::Call(call) => {
            Builtin::of(call).is_none_or(|builtin| builtin == Builtin::ToInteger)
                || call.arguments.iter().any(has_comparison)
        }
        _ => false,
    }
}

/// The type an expression evaluates to, from declarations and literals.
pub(super) fn expression_type(
    scope: &MethodScope<'_>,
    expression: &Expression,
) -> Option<SolveValueType> {
    let arithmetic = scope.scope.arithmetic;
    let boolean = || SolveValueType::scalar(SolveScalarType::Boolean);
    match expression {
        Expression::Bool(_) | Expression::Not(_) => Some(boolean()),
        Expression::Integer(_) | Expression::Size { .. } => Some(SolveValueType::scalar(
            SolveScalarType::Integer(arithmetic.integer_domain()),
        )),
        Expression::Real(_) => Some(SolveValueType::scalar(SolveScalarType::Real {
            format: arithmetic.real_format(),
        })),
        Expression::Ref(reference) | Expression::Neg(reference) => {
            let (id, subscripts) = scope.resolve_place(reference).ok()?;
            let whole = scope.value_type(id);
            Some(if subscripts.is_empty() {
                whole
            } else {
                SolveValueType::scalar(whole.element_type())
            })
        }
        Expression::Paren(inner) => expression_type(scope, inner),
        Expression::Binary { op, lhs, .. } => match op {
            BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::Le
            | BinaryOp::Ge
            | BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::And
            | BinaryOp::Or => Some(boolean()),
            _ => expression_type(scope, lhs),
        },
        Expression::If(if_expression) => if_expression
            .branches
            .first()
            .and_then(|(_, value)| expression_type(scope, value)),
        Expression::Call(call) if scope.scope.functions.is_user(call) => {
            scope.scope.functions.output_type(call)
        }
        Expression::Call(call) => {
            let builtin = Builtin::of(call)?;
            let result = builtin.result(arithmetic);
            // A unary built-in applies element-wise: the argument's shape.
            match (builtin, call.arguments.first()) {
                (Builtin::Unary(_), Some(argument)) => {
                    let shape = expression_type(scope, argument)?.dimensions().to_vec();
                    Some(if shape.is_empty() {
                        SolveValueType::scalar(result)
                    } else {
                        SolveValueType::tensor(result, shape).ok()?
                    })
                }
                _ => Some(SolveValueType::scalar(result)),
            }
        }
        Expression::Array(_) => None,
    }
}

/// What one lazily lowered arm computes.
enum LazyArm<'a> {
    Value(&'a Expression),
    Rest(&'a [(Expression, Expression)], &'a Expression),
}

/// `if c then v elseif ... else w` with only the selected arm evaluated:
/// each step is a `conditional` whose regions carry the result and the six
/// signal status bits, so an unselected arm neither faults nor raises.
fn lower_lazy_arms<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    arms: &[(Expression, Expression)],
    else_value: &Expression,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    let Some(((condition, value), rest)) = arms.split_first() else {
        return lower_expression(builder, scope, env, else_value, span);
    };
    let condition = lower_expression(builder, scope, env, condition, span)?;
    lower_lazy_select(
        builder, scope, env, condition, value, rest, else_value, span,
    )
}

/// `if condition then value elseif ... else else_value` for an evaluated
/// `condition`, each arm a region carrying the result and the status bits.
// SPEC_0021: Exception - one lazy-select chain threads the builder, scope, env, arms and status channels together.
#[allow(clippy::too_many_arguments)]
pub(super) fn lower_lazy_select<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    condition: ProgramRegister<'p>,
    value: &Expression,
    rest: &[(Expression, Expression)],
    else_value: &Expression,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    let value_type = expression_type(scope, value)
        .ok_or(SolveProgramConstructionError::TypeMismatch { provenance: span })?;
    let status = scope
        .scope
        .status
        .iter()
        .map(|&index| VarId::Block(index))
        .collect::<Vec<_>>();
    let mut read = status.iter().copied().collect::<BTreeSet<_>>();
    expression_reads(scope, value, &mut read);
    for (later_condition, later_value) in rest {
        expression_reads(scope, later_condition, &mut read);
        expression_reads(scope, later_value, &mut read);
    }
    expression_reads(scope, else_value, &mut read);
    let captures = read
        .into_iter()
        .filter(|&id| env.available(scope, id))
        .collect::<Vec<_>>();
    let mut capture_values = Vec::with_capacity(captures.len());
    for &id in &captures {
        capture_values.push(env.read(builder, scope, id, span)?);
    }
    let mut output_types = vec![value_type];
    output_types.extend(
        status
            .iter()
            .map(|_| SolveValueType::scalar(SolveScalarType::Boolean)),
    );
    let results = builder.conditional(
        condition,
        &capture_values,
        output_types,
        span,
        |region, inputs, outputs| {
            lower_lazy_arm(
                region,
                scope,
                &captures,
                &status,
                inputs,
                outputs,
                &LazyArm::Value(value),
                span,
            )
        },
        |region, inputs, outputs| {
            let arm = LazyArm::Rest(rest, else_value);
            lower_lazy_arm(
                region, scope, &captures, &status, inputs, outputs, &arm, span,
            )
        },
    )?;
    for (&id, &value) in status.iter().zip(&results[1..]) {
        env.write(builder, scope, id, value, span)?;
    }
    Ok(results[0])
}

// SPEC_0021: Exception - one region per lazy arm needs the parent scope, captures and status channels together.
#[allow(clippy::too_many_arguments)]
fn lower_lazy_arm<'r>(
    region: &mut TypedProgramBuilder<'r>,
    scope: &MethodScope<'_>,
    captures: &[VarId],
    status: &[VarId],
    inputs: &[ProgramSlot<'r>],
    outputs: &[ProgramSlot<'r>],
    arm: &LazyArm<'_>,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    let mut env = Env {
        values: BTreeMap::new(),
        slots: None,
        definite: BTreeSet::new(),
    };
    for (&id, &input) in captures.iter().zip(inputs) {
        env.values.insert(id, region.load(input, span)?);
    }
    let value = match arm {
        LazyArm::Value(value) => lower_expression(region, scope, &mut env, value, span)?,
        LazyArm::Rest(rest, else_value) => {
            lower_lazy_arms(region, scope, &mut env, rest, else_value, span)?
        }
    };
    region.store(outputs[0], value, span)?;
    for (&id, &output) in status.iter().zip(&outputs[1..]) {
        let value = env
            .values
            .get(&id)
            .copied()
            .ok_or(SolveProgramConstructionError::UninitializedSlot { provenance: span })?;
        region.store(output, value, span)?;
    }
    Ok(())
}

fn lower_call<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    call: &FunctionCall,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    if scope.scope.functions.is_user(call) {
        return lower_user_call(builder, scope, env, call, span)?
            .into_iter()
            .next()
            .ok_or(SolveProgramConstructionError::InvalidCallInterface { provenance: span });
    }
    let builtin = Builtin::of(call)
        .ok_or(SolveProgramConstructionError::InvalidRegion { provenance: span })?;
    let mut arguments = Vec::with_capacity(call.arguments.len());
    for argument in &call.arguments {
        arguments.push(lower_expression(builder, scope, env, argument, span)?);
    }
    match (builtin, arguments.as_slice()) {
        (Builtin::Constant(value), []) => {
            builder.constant(SolveValue::real(scope.scope.arithmetic, value), span)
        }
        (Builtin::Unary(operator), [value]) => builder.unary(operator, *value, span),
        (Builtin::IsNaN, [value]) => {
            builder.compare(SolveCompareOperator::NotEqual, *value, *value, span)
        }
        (Builtin::Atan2, [y, x]) => builder.binary(SolveBinaryOperator::Atan2, *y, *x, span),
        // A raw comparison: a built-in raises no NAN (only operators do).
        (Builtin::Extremum(operator, _), [a, b]) => {
            let first = builder.compare(operator, *a, *b, span)?;
            builder.select(first, *a, *b, span)
        }
        (Builtin::ToReal, [value]) => {
            builder.convert(SolveConversionOperator::IntegerToReal, *value, span)
        }
        (Builtin::IntegerDivision(operator), [a, b]) => builder.binary(operator, *a, *b, span),
        (Builtin::ToInteger, [value]) => {
            lower_integer_conversion(builder, scope, env, *value, span)
        }
        _ => Err(SolveProgramConstructionError::InvalidRegion { provenance: span }),
    }
}

/// `integer(x)`: the truncation when it lies in the Integer domain, else
/// zero with NAN (x is NaN) or OVERFLOW raised. The conversion only ever sees
/// an in-range value, so it cannot fault.
fn lower_integer_conversion<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    value: ProgramRegister<'p>,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    let arithmetic = scope.scope.arithmetic;
    let (low, high) = integer_conversion_limits(arithmetic)
        .ok_or(SolveProgramConstructionError::InvalidRegion { provenance: span })?;
    let real = |builder: &mut TypedProgramBuilder<'p>, value: f64| {
        builder.constant(SolveValue::real(arithmetic, value), span)
    };
    let nan = builder.compare(SolveCompareOperator::NotEqual, value, value, span)?;
    let truncated = builder.unary(SolveUnaryOperator::Truncate, value, span)?;
    let low = real(builder, low)?;
    let high = real(builder, high)?;
    let above = builder.compare(SolveCompareOperator::GreaterEqual, truncated, low, span)?;
    let below = builder.compare(SolveCompareOperator::Less, truncated, high, span)?;
    let in_domain = builder.binary(SolveBinaryOperator::And, above, below, span)?;
    let zero = real(builder, 0.0)?;
    let safe = builder.select(in_domain, truncated, zero, span)?;
    let result = builder.convert(SolveConversionOperator::RealToIntegerTowardZero, safe, span)?;
    let out_of_domain = builder.unary(SolveUnaryOperator::Not, in_domain, span)?;
    let finite = builder.unary(SolveUnaryOperator::Not, nan, span)?;
    let overflow = builder.binary(SolveBinaryOperator::And, finite, out_of_domain, span)?;
    raise(builder, scope, env, NAN_SIGNAL, nan, span)?;
    raise(builder, scope, env, OVERFLOW_SIGNAL, overflow, span)?;
    Ok(result)
}

/// OR `condition` into the status bit of `signal` (an index into `SIGNALS`).
pub(super) fn raise<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    signal: usize,
    condition: ProgramRegister<'p>,
    span: Span,
) -> Result<(), SolveProgramConstructionError> {
    scope.raised.borrow_mut().insert(signal);
    let status = VarId::Block(scope.scope.status[signal]);
    let raised = env.read(builder, scope, status, span)?;
    let raised = builder.binary(SolveBinaryOperator::Or, raised, condition, span)?;
    env.write(builder, scope, status, raised, span)
}

fn lower_binary<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    op: BinaryOp,
    lhs: ProgramRegister<'p>,
    rhs: ProgramRegister<'p>,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    let mut compare = |operator| lower_compare(builder, scope, env, operator, lhs, rhs, span);
    // `None` is division, whose operator the operand type decides below.
    let arithmetic = match op {
        BinaryOp::Lt => return compare(SolveCompareOperator::Less),
        BinaryOp::Gt => return compare(SolveCompareOperator::Greater),
        BinaryOp::Le => return compare(SolveCompareOperator::LessEqual),
        BinaryOp::Ge => return compare(SolveCompareOperator::GreaterEqual),
        BinaryOp::Eq => return compare(SolveCompareOperator::Equal),
        BinaryOp::Ne => return compare(SolveCompareOperator::NotEqual),
        BinaryOp::Add => Some(SolveBinaryOperator::Add),
        BinaryOp::Sub => Some(SolveBinaryOperator::Subtract),
        BinaryOp::Mul => Some(SolveBinaryOperator::Multiply),
        BinaryOp::Div => None,
        BinaryOp::Pow => Some(SolveBinaryOperator::Power),
        BinaryOp::And => Some(SolveBinaryOperator::And),
        BinaryOp::Or => Some(SolveBinaryOperator::Or),
    };
    let (lhs, rhs) = broadcast_scalar(builder, lhs, rhs, span)?;
    let real = matches!(
        builder.value_type_of(lhs, span)?.element_type(),
        SolveScalarType::Real { .. }
    );
    // MLS/GALEC Integer division truncates toward zero and faults on a zero
    // divisor or an unrepresentable quotient (SOLVE-C33).
    let operator = arithmetic.unwrap_or(if real {
        SolveBinaryOperator::Divide
    } else {
        SolveBinaryOperator::IntegerQuotient
    });
    builder.binary(operator, lhs, rhs, span)
}

/// GALEC arithmetic between an array and a scalar applies the scalar to every
/// element: fill it to the array's shape first.
fn broadcast_scalar<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    lhs: ProgramRegister<'p>,
    rhs: ProgramRegister<'p>,
    span: Span,
) -> Result<(ProgramRegister<'p>, ProgramRegister<'p>), SolveProgramConstructionError> {
    let lhs_dims = builder.value_type_of(lhs, span)?.dimensions().to_vec();
    let rhs_dims = builder.value_type_of(rhs, span)?.dimensions().to_vec();
    match (lhs_dims.is_empty(), rhs_dims.is_empty()) {
        (true, false) => Ok((builder.fill(lhs, rhs_dims, span)?, rhs)),
        (false, true) => Ok((lhs, builder.fill(rhs, lhs_dims, span)?)),
        _ => Ok((lhs, rhs)),
    }
}

/// A GALEC comparison: a Real NaN operand makes it false and raises NAN.
fn lower_compare<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    scope: &MethodScope<'_>,
    env: &mut Env<'p>,
    operator: SolveCompareOperator,
    lhs: ProgramRegister<'p>,
    rhs: ProgramRegister<'p>,
    span: Span,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    let result = builder.compare(operator, lhs, rhs, span)?;
    let real = matches!(
        builder.value_type_of(lhs, span)?.element_type(),
        SolveScalarType::Real { .. }
    );
    if !real {
        return Ok(result);
    }
    let lhs_nan = builder.compare(SolveCompareOperator::NotEqual, lhs, lhs, span)?;
    let rhs_nan = builder.compare(SolveCompareOperator::NotEqual, rhs, rhs, span)?;
    let nan = builder.binary(SolveBinaryOperator::Or, lhs_nan, rhs_nan, span)?;
    raise(builder, scope, env, NAN_SIGNAL, nan, span)?;
    if operator == SolveCompareOperator::NotEqual {
        // IEEE `!=` is true on NaN; GALEC `<>` is false.
        let ordered = builder.unary(SolveUnaryOperator::Not, nan, span)?;
        return builder.binary(SolveBinaryOperator::And, result, ordered, span);
    }
    Ok(result)
}
