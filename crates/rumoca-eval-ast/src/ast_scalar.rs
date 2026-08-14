//! Shared scalar interpreter for AST expressions.
//!
//! Syntax dispatch belongs here. Compiler phases provide lookup, function-call,
//! coercion, and diagnostic policy through [`AstScalarContext`].

use rumoca_core::{OpBinary, OpUnary, Span};
use rumoca_ir_ast::{ComponentReference, Expression, TerminalType};

pub(crate) trait AstScalarContext {
    /// Maximum combined expression/function recursion depth for phases that
    /// evaluate model-controlled bindings. Typechecking historically accepts
    /// arbitrary parsed expression depth, so its adapter keeps the default.
    fn expression_depth_limit(&self) -> Option<usize> {
        None
    }

    fn lookup_integer(&self, expr: &Expression, scope: &str, depth: usize) -> Option<i64>;

    fn lookup_real(&self, _expr: &Expression, _scope: &str, _depth: usize) -> Option<f64> {
        None
    }

    fn lookup_boolean(&self, expr: &Expression, scope: &str, depth: usize) -> Option<bool>;

    fn call_integer(
        &self,
        function: &ComponentReference,
        args: &[Expression],
        scope: &str,
        depth: usize,
        span: Span,
    ) -> Option<i64>;

    fn call_boolean(
        &self,
        _function: &ComponentReference,
        _args: &[Expression],
        _scope: &str,
        _depth: usize,
        _span: Span,
    ) -> Option<bool> {
        None
    }

    fn call_real(
        &self,
        _function: &ComponentReference,
        _args: &[Expression],
        _scope: &str,
        _depth: usize,
        _span: Span,
    ) -> Option<f64> {
        None
    }

    fn enum_equal(
        &self,
        _lhs: &Expression,
        _rhs: &Expression,
        _scope: &str,
        _depth: usize,
    ) -> Option<bool> {
        None
    }

    fn coerce_integral_real(&self, _value: f64, _span: Span) -> Option<i64> {
        None
    }

    fn integer_binary(&self, op: &OpBinary, lhs: i64, rhs: i64, span: Span) -> Option<i64>;

    fn negate_integer(&self, value: i64, _span: Span) -> Option<i64> {
        value.checked_neg()
    }

    fn boolean_expression_allowed(&self, _expr: &Expression) -> bool {
        true
    }
}

pub(crate) fn eval_real<C: AstScalarContext>(
    expr: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<f64> {
    if ctx
        .expression_depth_limit()
        .is_some_and(|limit| depth > limit)
    {
        return None;
    }
    let child_depth = depth.checked_add(1)?;
    let recurse = |expr| eval_real(expr, ctx, scope, child_depth);
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token,
            ..
        } => token.text.parse().ok(),
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse::<i64>().ok().map(|value| value as f64),
        Expression::ComponentReference(_)
        | Expression::FieldAccess { .. }
        | Expression::ArrayIndex { .. } => ctx.lookup_real(expr, scope, child_depth),
        Expression::Unary { op, rhs, .. } => {
            let value = recurse(rhs)?;
            match op {
                OpUnary::Plus | OpUnary::DotPlus | OpUnary::Empty => Some(value),
                OpUnary::Minus | OpUnary::DotMinus => Some(-value),
                OpUnary::Not => None,
            }
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = recurse(lhs)?;
            let rhs = recurse(rhs)?;
            match op {
                OpBinary::Add | OpBinary::AddElem => Some(lhs + rhs),
                OpBinary::Sub | OpBinary::SubElem => Some(lhs - rhs),
                OpBinary::Mul | OpBinary::MulElem => Some(lhs * rhs),
                OpBinary::Div | OpBinary::DivElem => (rhs != 0.0).then_some(lhs / rhs),
                OpBinary::Exp | OpBinary::ExpElem => Some(lhs.powf(rhs)),
                _ => None,
            }
        }
        Expression::Parenthesized { inner, .. } => recurse(inner),
        Expression::FunctionCall {
            comp,
            args,
            is_partial_application: false,
            span,
        } => ctx.call_real(comp, args, scope, child_depth, *span),
        Expression::FunctionCall {
            is_partial_application: true,
            ..
        } => None,
        Expression::If {
            branches,
            else_branch,
            ..
        } => eval_real_if(branches, else_branch, ctx, scope, child_depth),
        _ => None,
    }
}

fn eval_real_if<C: AstScalarContext>(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<f64> {
    for (index, (condition, value)) in branches.iter().enumerate() {
        match eval_boolean(condition, ctx, scope, depth) {
            Some(true) => return eval_real(value, ctx, scope, depth),
            Some(false) => {}
            None => {
                let common = eval_real(value, ctx, scope, depth)?;
                if !remaining_real_outcomes_match(
                    &branches[index + 1..],
                    common,
                    ctx,
                    scope,
                    depth,
                )? {
                    return None;
                }
                return same_real(eval_real(else_branch, ctx, scope, depth)?, common)
                    .then_some(common);
            }
        }
    }
    eval_real(else_branch, ctx, scope, depth)
}

fn remaining_real_outcomes_match<C: AstScalarContext>(
    branches: &[(Expression, Expression)],
    common: f64,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<bool> {
    for (_, value) in branches {
        if !same_real(eval_real(value, ctx, scope, depth)?, common) {
            return Some(false);
        }
    }
    Some(true)
}

fn same_real(lhs: f64, rhs: f64) -> bool {
    lhs == rhs || lhs.to_bits() == rhs.to_bits()
}

pub(crate) fn eval_integer<C: AstScalarContext>(
    expr: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<i64> {
    if ctx
        .expression_depth_limit()
        .is_some_and(|limit| depth > limit)
    {
        return None;
    }
    let child_depth = depth.checked_add(1)?;
    let recurse = |expr| eval_integer(expr, ctx, scope, child_depth);
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse().ok(),
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token,
            span,
        } => {
            let value = token.text.parse::<f64>().ok()?;
            (value.is_finite() && value.fract() == 0.0)
                .then(|| ctx.coerce_integral_real(value, *span))
                .flatten()
        }
        Expression::ComponentReference(_)
        | Expression::FieldAccess { .. }
        | Expression::ArrayIndex { .. } => ctx.lookup_integer(expr, scope, child_depth),
        Expression::Unary { op, rhs, span } => {
            let value = recurse(rhs)?;
            match op {
                OpUnary::Plus | OpUnary::DotPlus | OpUnary::Empty => Some(value),
                OpUnary::Minus | OpUnary::DotMinus => ctx.negate_integer(value, *span),
                OpUnary::Not => None,
            }
        }
        Expression::Binary { op, lhs, rhs, span } => {
            ctx.integer_binary(op, recurse(lhs)?, recurse(rhs)?, *span)
        }
        Expression::Parenthesized { inner, .. } => recurse(inner),
        Expression::FunctionCall {
            comp,
            args,
            is_partial_application: false,
            span,
        } => ctx.call_integer(comp, args, scope, child_depth, *span),
        Expression::FunctionCall {
            is_partial_application: true,
            ..
        } => None,
        Expression::If {
            branches,
            else_branch,
            ..
        } => eval_integer_if(branches, else_branch, ctx, scope, child_depth),
        _ => None,
    }
}

fn eval_integer_if<C: AstScalarContext>(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<i64> {
    for (index, (condition, value)) in branches.iter().enumerate() {
        match eval_boolean(condition, ctx, scope, depth) {
            Some(true) => return eval_integer(value, ctx, scope, depth),
            Some(false) => {}
            None => {
                let common = eval_integer(value, ctx, scope, depth)?;
                if !remaining_integer_outcomes_match(
                    &branches[index + 1..],
                    common,
                    ctx,
                    scope,
                    depth,
                ) {
                    return None;
                }
                return (eval_integer(else_branch, ctx, scope, depth) == Some(common))
                    .then_some(common);
            }
        }
    }
    eval_integer(else_branch, ctx, scope, depth)
}

fn remaining_integer_outcomes_match<C: AstScalarContext>(
    branches: &[(Expression, Expression)],
    common: i64,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> bool {
    branches
        .iter()
        .all(|(_, value)| eval_integer(value, ctx, scope, depth) == Some(common))
}

pub(crate) fn eval_boolean<C: AstScalarContext>(
    expr: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<bool> {
    if ctx
        .expression_depth_limit()
        .is_some_and(|limit| depth > limit)
        || !ctx.boolean_expression_allowed(expr)
    {
        return None;
    }
    let child_depth = depth.checked_add(1)?;
    let recurse = |expr| eval_boolean(expr, ctx, scope, child_depth);
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::Bool,
            token,
            ..
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        Expression::ComponentReference(_)
        | Expression::FieldAccess { .. }
        | Expression::ArrayIndex { .. } => ctx.lookup_boolean(expr, scope, child_depth),
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => recurse(rhs).map(|value| !value),
        Expression::Unary { .. } => None,
        Expression::Binary { op, lhs, rhs, .. } => {
            eval_boolean_binary(op, lhs, rhs, ctx, scope, child_depth)
        }
        Expression::Parenthesized { inner, .. } => recurse(inner),
        Expression::FunctionCall {
            comp,
            args,
            is_partial_application: false,
            span,
        } => ctx.call_boolean(comp, args, scope, child_depth, *span),
        Expression::FunctionCall {
            is_partial_application: true,
            ..
        } => None,
        Expression::If {
            branches,
            else_branch,
            ..
        } => eval_boolean_if(branches, else_branch, ctx, scope, child_depth),
        _ => None,
    }
}

fn eval_boolean_binary<C: AstScalarContext>(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<bool> {
    match op {
        OpBinary::And => match eval_boolean(lhs, ctx, scope, depth) {
            Some(false) => Some(false),
            Some(true) => eval_boolean(rhs, ctx, scope, depth),
            None => (eval_boolean(rhs, ctx, scope, depth) == Some(false)).then_some(false),
        },
        OpBinary::Or => match eval_boolean(lhs, ctx, scope, depth) {
            Some(true) => Some(true),
            Some(false) => eval_boolean(rhs, ctx, scope, depth),
            None => (eval_boolean(rhs, ctx, scope, depth) == Some(true)).then_some(true),
        },
        OpBinary::Eq | OpBinary::Neq => {
            let equal = scalar_equal(lhs, rhs, ctx, scope, depth)?;
            Some(if matches!(op, OpBinary::Eq) {
                equal
            } else {
                !equal
            })
        }
        OpBinary::Lt | OpBinary::Le | OpBinary::Gt | OpBinary::Ge => {
            compare_numeric(op, lhs, rhs, ctx, scope, depth)
        }
        _ => None,
    }
}

fn scalar_equal<C: AstScalarContext>(
    lhs: &Expression,
    rhs: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<bool> {
    if let (Some(lhs), Some(rhs)) = (
        eval_integer(lhs, ctx, scope, depth),
        eval_integer(rhs, ctx, scope, depth),
    ) {
        return Some(lhs == rhs);
    }
    if let (Some(lhs), Some(rhs)) = (
        eval_real(lhs, ctx, scope, depth),
        eval_real(rhs, ctx, scope, depth),
    ) {
        return Some(lhs == rhs);
    }
    if let Some(equal) = ctx.enum_equal(lhs, rhs, scope, depth) {
        return Some(equal);
    }
    Some(eval_boolean(lhs, ctx, scope, depth)? == eval_boolean(rhs, ctx, scope, depth)?)
}

fn compare_numeric<C: AstScalarContext>(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<bool> {
    if let (Some(lhs), Some(rhs)) = (
        eval_integer(lhs, ctx, scope, depth),
        eval_integer(rhs, ctx, scope, depth),
    ) {
        return Some(compare_ordered(op, lhs, rhs));
    }
    let lhs = eval_real(lhs, ctx, scope, depth)?;
    let rhs = eval_real(rhs, ctx, scope, depth)?;
    Some(compare_ordered(op, lhs, rhs))
}

fn compare_ordered<T: PartialOrd>(op: &OpBinary, lhs: T, rhs: T) -> bool {
    match op {
        OpBinary::Lt => lhs < rhs,
        OpBinary::Le => lhs <= rhs,
        OpBinary::Gt => lhs > rhs,
        OpBinary::Ge => lhs >= rhs,
        _ => false,
    }
}

fn eval_boolean_if<C: AstScalarContext>(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<bool> {
    for (index, (condition, value)) in branches.iter().enumerate() {
        match eval_boolean(condition, ctx, scope, depth) {
            Some(true) => return eval_boolean(value, ctx, scope, depth),
            Some(false) => {}
            None => {
                let common = eval_boolean(value, ctx, scope, depth)?;
                if !remaining_boolean_outcomes_match(
                    &branches[index + 1..],
                    common,
                    ctx,
                    scope,
                    depth,
                ) {
                    return None;
                }
                return (eval_boolean(else_branch, ctx, scope, depth) == Some(common))
                    .then_some(common);
            }
        }
    }
    eval_boolean(else_branch, ctx, scope, depth)
}

fn remaining_boolean_outcomes_match<C: AstScalarContext>(
    branches: &[(Expression, Expression)],
    common: bool,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> bool {
    branches
        .iter()
        .all(|(_, value)| eval_boolean(value, ctx, scope, depth) == Some(common))
}
