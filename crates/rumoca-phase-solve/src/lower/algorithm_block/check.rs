//! Pre-pass: refuse unsupported constructs and lifecycle violations.

use super::*;

pub(super) fn check_statements(
    scope: &MethodScope<'_>,
    statements: &[Spanned<Statement>],
) -> Result<(), AlgorithmBlockLowerError> {
    for statement in statements {
        check_statement(scope, &statement.node, statement.span)?;
    }
    Ok(())
}

pub(super) fn check_statement(
    scope: &MethodScope<'_>,
    statement: &Statement,
    span: Span,
) -> Result<(), AlgorithmBlockLowerError> {
    match statement {
        Statement::Assignment { target, value } => {
            let (target, subscripts) = scope.resolve_place(target)?;
            scope.check_store(target, span)?;
            check_subscripts(scope, target, subscripts, span)?;
            if matches!(value, Expression::Array(_)) {
                if !subscripts.is_empty() {
                    return Err(unsupported("array literal assigned to an element", span));
                }
                return check_array_literal(scope, value, &scope.variable(target).dims, span);
            }
            check_expression(scope, value, span)
        }
        Statement::For(for_loop) => check_for(scope, for_loop, span),
        Statement::If(if_statement) => {
            for branch in &if_statement.branches {
                match &branch.condition {
                    Condition::Expression(condition) => check_expression(scope, condition, span)?,
                    Condition::SignalCheck(_) => {
                        return Err(unsupported("signal check", branch.span));
                    }
                }
                check_statements(scope, &branch.body)?;
            }
            if let Some(else_body) = &if_statement.else_body {
                check_statements(scope, else_body)?;
            }
            Ok(())
        }
        Statement::Limit(targets) => {
            for target in targets {
                match target {
                    LimitTarget::SelfState => {}
                    LimitTarget::Reference(reference) => {
                        let target = scope.resolve(reference)?;
                        scope.check_store(target, span)?;
                    }
                }
            }
            Ok(())
        }
        Statement::MultiAssignment { targets, call } => {
            check_call_statement(scope, targets, call, span)
        }
        Statement::Call(call) => check_call_statement(scope, &[], call, span),
        Statement::Signal(signals) => {
            for signal in signals {
                predefined_signal(signal.as_str())
                    .ok_or_else(|| unsupported("user error signal", span))?;
            }
            Ok(())
        }
    }
}

pub(super) fn check_expression(
    scope: &MethodScope<'_>,
    expression: &Expression,
    span: Span,
) -> Result<(), AlgorithmBlockLowerError> {
    match expression {
        Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => Ok(()),
        Expression::Ref(reference) | Expression::Neg(reference) => {
            let (id, subscripts) = scope.resolve_place(reference)?;
            check_subscripts(scope, id, subscripts, span)
        }
        Expression::Size { .. } => static_int(scope, expression)
            .map(|_| ())
            .ok_or_else(|| unsupported("size of a non-array or a non-literal axis", span)),
        Expression::Paren(inner) | Expression::Not(inner) => check_expression(scope, inner, span),
        Expression::Binary { op, lhs, rhs } => {
            check_expression(scope, lhs, span)?;
            check_expression(scope, rhs, span)?;
            // GALEC arithmetic does not mix Real and Integer operands (the
            // projection converts with `real()`); the evaluator rejects it.
            let arithmetic = matches!(
                op,
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Pow
            );
            let element = |side| expression_type(scope, side).map(|ty| ty.element_type());
            if arithmetic
                && let (Some(lhs), Some(rhs)) = (element(lhs), element(rhs))
                && lhs != rhs
            {
                return Err(unsupported("mixed Real and Integer arithmetic", span));
            }
            Ok(())
        }
        Expression::If(if_expression) => {
            for (condition, value) in &if_expression.branches {
                check_expression(scope, condition, span)?;
                check_expression(scope, value, span)?;
            }
            check_expression(scope, &if_expression.else_value, span)
        }
        Expression::Call(call) if scope.scope.functions.is_user(call) => {
            check_user_call(scope, call, 1, span)
        }
        Expression::Call(call) => {
            let builtin =
                Builtin::of(call).ok_or_else(|| unsupported("built-in function", span))?;
            if call.arguments.len() != builtin.arity() {
                return Err(unsupported("built-in function arity", span));
            }
            if let Some((_, rank)) = lifted(call.function.lexeme()) {
                let argument_rank = call
                    .arguments
                    .first()
                    .and_then(|argument| expression_type(scope, argument))
                    .map(|value_type| value_type.dimensions().len());
                if argument_rank != Some(rank) {
                    return Err(unsupported("lifted built-in argument rank", span));
                }
            }
            if builtin == Builtin::ToInteger
                && integer_conversion_limits(scope.scope.arithmetic).is_none()
            {
                return Err(unsupported(
                    "integer() outside a two's-complement domain",
                    span,
                ));
            }
            let elementwise = lifted(call.function.lexeme()).is_some();
            for argument in &call.arguments {
                check_expression(scope, argument, span)?;
                let matches = expression_type(scope, argument)
                    .is_some_and(|value_type| builtin_admits(builtin, elementwise, value_type));
                if !matches {
                    return Err(unsupported("built-in argument type", span));
                }
            }
            Ok(())
        }
        Expression::Array(_) => Err(unsupported("array literal outside an assignment", span)),
    }
}

/// The scalar built-ins this slice refines (rumoca-eval-galec builtins.rs
/// is the reference). Only `integer` raises a signal; only the Integer
/// divisions fault.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum Builtin {
    Unary(SolveUnaryOperator),
    Atan2,
    /// GALEC `min`/`max` (`if a < b then a else b`, resp. `>`) and
    /// `imin`/`imax`: a raw compare and select, which differs from Solve
    /// `Min`/`Max` on NaN and signed zeros.
    Extremum(SolveCompareOperator, Domain),
    IsNaN,
    Constant(f64),
    /// `real(i)`.
    ToReal,
    /// `integer(x)`: truncation; NaN raises NAN, out of the domain raises
    /// OVERFLOW, and both return zero.
    ToInteger,
    /// `divisionTowardsZero` / `remainderTowardsZero`.
    IntegerDivision(SolveBinaryOperator),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Domain {
    Real,
    Integer,
}

impl Domain {
    /// A scalar of this domain.
    fn admits(self, value_type: &SolveValueType) -> bool {
        value_type.dimensions().is_empty()
            && matches!(
                (self, value_type.element_type()),
                (Self::Real, SolveScalarType::Real { .. })
                    | (Self::Integer, SolveScalarType::Integer(_))
            )
    }
}

impl Builtin {
    pub(super) fn named(name: &str) -> Option<Self> {
        use SolveUnaryOperator as U;
        Some(match name {
            "roundDown" => Self::Unary(U::Floor),
            "roundUp" => Self::Unary(U::Ceiling),
            "sign" => Self::Unary(U::Sign),
            "absolute" => Self::Unary(U::Abs),
            "sqrt" => Self::Unary(U::Sqrt),
            "exp" => Self::Unary(U::Exp),
            "ln" => Self::Unary(U::Log),
            "lg" => Self::Unary(U::Log10),
            "sin" => Self::Unary(U::Sin),
            "cos" => Self::Unary(U::Cos),
            "tan" => Self::Unary(U::Tan),
            "asin" => Self::Unary(U::Asin),
            "acos" => Self::Unary(U::Acos),
            "atan" => Self::Unary(U::Atan),
            "sinh" => Self::Unary(U::Sinh),
            "cosh" => Self::Unary(U::Cosh),
            "tanh" => Self::Unary(U::Tanh),
            "atan2" => Self::Atan2,
            "min" => Self::Extremum(SolveCompareOperator::Less, Domain::Real),
            "max" => Self::Extremum(SolveCompareOperator::Greater, Domain::Real),
            "imin" => Self::Extremum(SolveCompareOperator::Less, Domain::Integer),
            "imax" => Self::Extremum(SolveCompareOperator::Greater, Domain::Integer),
            "isNaN" => Self::IsNaN,
            "pi" => Self::Constant(std::f64::consts::PI),
            "euler" => Self::Constant(std::f64::consts::E),
            "real" => Self::ToReal,
            "integer" => Self::ToInteger,
            "divisionTowardsZero" => Self::IntegerDivision(SolveBinaryOperator::IntegerQuotient),
            "remainderTowardsZero" => Self::IntegerDivision(SolveBinaryOperator::IntegerRemainder),
            _ => return None,
        })
    }

    pub(super) fn of(call: &FunctionCall) -> Option<Self> {
        let name = call.function.lexeme();
        Self::named(name).or_else(|| lifted(name).map(|(builtin, _)| builtin))
    }

    pub(super) const fn arity(self) -> usize {
        match self {
            Self::Constant(_) => 0,
            Self::Unary(_) | Self::IsNaN | Self::ToReal | Self::ToInteger => 1,
            Self::Atan2 | Self::Extremum(..) | Self::IntegerDivision(_) => 2,
        }
    }

    pub(super) const fn parameter(self) -> Domain {
        match self {
            Self::Extremum(_, domain) => domain,
            Self::ToReal | Self::IntegerDivision(_) => Domain::Integer,
            _ => Domain::Real,
        }
    }

    pub(super) fn result(self, arithmetic: SolveArithmeticProfile) -> SolveScalarType {
        match self {
            Self::IsNaN => SolveScalarType::Boolean,
            Self::Extremum(_, Domain::Integer) | Self::ToInteger | Self::IntegerDivision(_) => {
                SolveScalarType::Integer(arithmetic.integer_domain())
            }
            _ => SolveScalarType::Real {
                format: arithmetic.real_format(),
            },
        }
    }
}

/// Whether a built-in admits an argument: a lifted call's rank is checked
/// separately, so only its elements must be what the scalar built-in admits.
fn builtin_admits(builtin: Builtin, elementwise: bool, value_type: SolveValueType) -> bool {
    let admitted = if elementwise {
        SolveValueType::scalar(value_type.element_type())
    } else {
        value_type
    };
    builtin.parameter().admits(&admitted)
}

/// A lifted unary built-in (`sin1D`, `absolute2D`, ...): the scalar function
/// applied to every element of an argument of that rank.
fn lifted(name: &str) -> Option<(Builtin, usize)> {
    let (base, rank) = name
        .strip_suffix("1D")
        .map(|base| (base, 1))
        .or_else(|| name.strip_suffix("2D").map(|base| (base, 2)))?;
    match Builtin::named(base)? {
        builtin @ Builtin::Unary(_) => Some((builtin, rank)),
        _ => None,
    }
}

/// `integer()`'s domain test as `[low, high)` in Real: exact powers of two
/// for a two's-complement domain, in every Real format.
pub(super) fn integer_conversion_limits(arithmetic: SolveArithmeticProfile) -> Option<(f64, f64)> {
    let domain = arithmetic.integer_domain();
    let span = domain.maximum().checked_add(1)?;
    (span.count_ones() == 1 && domain.minimum().checked_neg() == Some(span))
        .then(|| (domain.minimum() as f64, span as f64))
}

/// An element reference names every axis of an array variable.
pub(super) fn check_subscripts(
    scope: &MethodScope<'_>,
    id: VarId,
    subscripts: &[Expression],
    span: Span,
) -> Result<(), AlgorithmBlockLowerError> {
    if subscripts.is_empty() {
        return Ok(());
    }
    if matches!(id, VarId::Loop(_)) || subscripts.len() != scope.variable(id).dims.len() {
        return Err(unsupported("partial or scalar subscript", span));
    }
    for subscript in subscripts {
        check_expression(scope, subscript, span)?;
    }
    Ok(())
}

/// A nested literal whose shape is exactly `dims`.
fn check_array_literal(
    scope: &MethodScope<'_>,
    literal: &Expression,
    dims: &[u32],
    span: Span,
) -> Result<(), AlgorithmBlockLowerError> {
    let Some((&extent, rest)) = dims.split_first() else {
        return check_expression(scope, literal, span);
    };
    let Expression::Array(items) = literal else {
        return Err(unsupported("array literal of the wrong shape", span));
    };
    if items.len() != extent as usize {
        return Err(unsupported("array literal of the wrong shape", span));
    }
    for item in items {
        check_array_literal(scope, item, rest, span)?;
    }
    Ok(())
}

/// A `for` loop over a static range with an iterator.
fn check_for(
    scope: &MethodScope<'_>,
    for_loop: &ForLoop,
    span: Span,
) -> Result<(), AlgorithmBlockLowerError> {
    let iterator = for_loop
        .iterator
        .as_ref()
        .ok_or_else(|| unsupported("for loop without an iterator", span))?;
    loop_range(scope, for_loop)
        .ok_or_else(|| unsupported("for loop with a non-static range", span))?;
    scope.with_loop(iterator.lexeme(), |_| {
        check_statements(scope, &for_loop.body)
    })
}

/// The static `(start, stop, step)` of a loop, with a nonzero step.
pub(super) fn loop_range(scope: &MethodScope<'_>, for_loop: &ForLoop) -> Option<(i64, i64, i64)> {
    let start = static_int(scope, &for_loop.start)?;
    let stop = static_int(scope, &for_loop.stop)?;
    let step = match &for_loop.step {
        Some(step) => static_int(scope, step)?,
        None => 1,
    };
    (step != 0).then_some((start, stop, step))
}

/// An Integer known at construction: literals, `size` of a declared array
/// axis, and their sums, differences and products.
pub(super) fn static_int(scope: &MethodScope<'_>, expression: &Expression) -> Option<i64> {
    match expression {
        Expression::Integer(value) => Some(*value),
        Expression::Paren(inner) => static_int(scope, inner),
        Expression::Size { array, dimension } => {
            let id = scope.resolve(array).ok()?;
            if matches!(id, VarId::Loop(_)) {
                return None;
            }
            let axis = usize::try_from(static_int(scope, dimension)?.checked_sub(1)?).ok()?;
            scope
                .variable(id)
                .dims
                .get(axis)
                .map(|&extent| i64::from(extent))
        }
        Expression::Binary { op, lhs, rhs } => {
            let (lhs, rhs) = (static_int(scope, lhs)?, static_int(scope, rhs)?);
            match op {
                BinaryOp::Add => lhs.checked_add(rhs),
                BinaryOp::Sub => lhs.checked_sub(rhs),
                BinaryOp::Mul => lhs.checked_mul(rhs),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Whether evaluating `expression` can fault. An if-expression lowers to an
/// eager `select`, which is exact only when neither branch can fault.
pub(super) fn may_fault(expression: &Expression) -> bool {
    match expression {
        Expression::Bool(_)
        | Expression::Integer(_)
        | Expression::Real(_)
        | Expression::Size { .. } => false,
        // An element read faults outside the array.
        Expression::Ref(reference) | Expression::Neg(reference) => reference_subscripts(reference),
        Expression::Paren(inner) | Expression::Not(inner) => may_fault(inner),
        Expression::Binary { op, lhs, rhs } => {
            matches!(op, BinaryOp::Div | BinaryOp::Pow) || may_fault(lhs) || may_fault(rhs)
        }
        Expression::If(if_expression) => {
            if_expression
                .branches
                .iter()
                .any(|(condition, value)| may_fault(condition) || may_fault(value))
                || may_fault(&if_expression.else_value)
        }
        Expression::Call(call) => {
            matches!(Builtin::of(call), Some(Builtin::IntegerDivision(_)) | None)
                || call.arguments.iter().any(may_fault)
        }
        Expression::Array(_) => true,
    }
}
