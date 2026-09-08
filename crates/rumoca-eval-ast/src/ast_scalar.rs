//! Shared scalar interpreter for AST expressions.
//!
//! Syntax dispatch belongs here. Compiler phases provide lookup, function-call,
//! coercion, and diagnostic policy through [`AstScalarContext`].

use rumoca_core::{OpBinary, OpUnary, Span};
use rumoca_ir_ast::{ComponentReference, Expression, TerminalType};

pub trait AstScalarContext {
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

pub fn eval_real<C: AstScalarContext>(
    expr: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<f64> {
    if rumoca_ir_ast::expression_required_value_violation(expr).is_some() {
        return None;
    }
    eval_real_inner(expr, ctx, scope, depth)
}

fn eval_real_inner<C: AstScalarContext>(
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
    let recurse = |expr| eval_real_inner(expr, ctx, scope, child_depth);
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token,
            ..
        } => token
            .text
            .parse::<f64>()
            .ok()
            // A literal like 1e400 parses to Inf; a non-finite value must
            // refuse to fold before it can feed a comparison.
            .filter(|value| value.is_finite()),
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse::<i64>().ok().map(|value| value as f64),
        Expression::ComponentReference(_)
        | Expression::FieldAccess { .. }
        | Expression::ArrayIndex { .. } => ctx.lookup_real(expr, scope, child_depth),
        Expression::Unary { op, rhs, .. } => match op {
            OpUnary::Plus | OpUnary::DotPlus => recurse(rhs),
            OpUnary::Minus | OpUnary::DotMinus => recurse(rhs).map(|value| -value),
            OpUnary::Not | OpUnary::Empty => None,
        },
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = recurse(lhs)?;
            let rhs = recurse(rhs)?;
            // A structural fold must never manufacture a value from an
            // undefined operation: a non-finite result (overflow, 0-divisor
            // already rejected) refuses to fold rather than flowing onward
            // as Inf/NaN into a comparison.
            match op {
                OpBinary::Add | OpBinary::AddElem => Some(lhs + rhs),
                OpBinary::Sub | OpBinary::SubElem => Some(lhs - rhs),
                OpBinary::Mul | OpBinary::MulElem => Some(lhs * rhs),
                OpBinary::Div | OpBinary::DivElem => (rhs != 0.0).then_some(lhs / rhs),
                OpBinary::Exp | OpBinary::ExpElem => Some(lhs.powf(rhs)),
                _ => None,
            }
            .filter(|value| value.is_finite())
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
        match eval_boolean_inner(condition, ctx, scope, depth) {
            Some(true) => return eval_real_inner(value, ctx, scope, depth),
            Some(false) => {}
            None => {
                let common = eval_real_inner(value, ctx, scope, depth)?;
                if !remaining_real_outcomes_match(
                    &branches[index + 1..],
                    common,
                    ctx,
                    scope,
                    depth,
                )? {
                    return None;
                }
                return same_real(eval_real_inner(else_branch, ctx, scope, depth)?, common)
                    .then_some(common);
            }
        }
    }
    eval_real_inner(else_branch, ctx, scope, depth)
}

fn remaining_real_outcomes_match<C: AstScalarContext>(
    branches: &[(Expression, Expression)],
    common: f64,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<bool> {
    for (_, value) in branches {
        if !same_real(eval_real_inner(value, ctx, scope, depth)?, common) {
            return Some(false);
        }
    }
    Some(true)
}

fn same_real(lhs: f64, rhs: f64) -> bool {
    lhs == rhs || lhs.to_bits() == rhs.to_bits()
}

pub fn eval_integer<C: AstScalarContext>(
    expr: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<i64> {
    if rumoca_ir_ast::expression_required_value_violation(expr).is_some() {
        return None;
    }
    eval_integer_inner(expr, ctx, scope, depth)
}

fn eval_integer_inner<C: AstScalarContext>(
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
    let recurse = |expr| eval_integer_inner(expr, ctx, scope, child_depth);
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
        Expression::Unary { op, rhs, span } => match op {
            OpUnary::Plus | OpUnary::DotPlus => recurse(rhs),
            OpUnary::Minus | OpUnary::DotMinus => ctx.negate_integer(recurse(rhs)?, *span),
            OpUnary::Not | OpUnary::Empty => None,
        },
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
        match eval_boolean_inner(condition, ctx, scope, depth) {
            Some(true) => return eval_integer_inner(value, ctx, scope, depth),
            Some(false) => {}
            None => {
                let common = eval_integer_inner(value, ctx, scope, depth)?;
                if !remaining_integer_outcomes_match(
                    &branches[index + 1..],
                    common,
                    ctx,
                    scope,
                    depth,
                ) {
                    return None;
                }
                return (eval_integer_inner(else_branch, ctx, scope, depth) == Some(common))
                    .then_some(common);
            }
        }
    }
    eval_integer_inner(else_branch, ctx, scope, depth)
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
        .all(|(_, value)| eval_integer_inner(value, ctx, scope, depth) == Some(common))
}

pub fn eval_boolean<C: AstScalarContext>(
    expr: &Expression,
    ctx: &C,
    scope: &str,
    depth: usize,
) -> Option<bool> {
    if rumoca_ir_ast::expression_required_value_violation(expr).is_some() {
        return None;
    }
    eval_boolean_inner(expr, ctx, scope, depth)
}

fn eval_boolean_inner<C: AstScalarContext>(
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
    let recurse = |expr| eval_boolean_inner(expr, ctx, scope, child_depth);
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
        OpBinary::And => match eval_boolean_inner(lhs, ctx, scope, depth) {
            Some(false) => Some(false),
            Some(true) => eval_boolean_inner(rhs, ctx, scope, depth),
            None => (eval_boolean_inner(rhs, ctx, scope, depth) == Some(false)).then_some(false),
        },
        OpBinary::Or => match eval_boolean_inner(lhs, ctx, scope, depth) {
            Some(true) => Some(true),
            Some(false) => eval_boolean_inner(rhs, ctx, scope, depth),
            None => (eval_boolean_inner(rhs, ctx, scope, depth) == Some(true)).then_some(true),
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
        eval_integer_inner(lhs, ctx, scope, depth),
        eval_integer_inner(rhs, ctx, scope, depth),
    ) {
        return Some(lhs == rhs);
    }
    if let (Some(lhs), Some(rhs)) = (
        eval_real_inner(lhs, ctx, scope, depth),
        eval_real_inner(rhs, ctx, scope, depth),
    ) {
        return Some(lhs == rhs);
    }
    if let Some(equal) = ctx.enum_equal(lhs, rhs, scope, depth) {
        return Some(equal);
    }
    Some(eval_boolean_inner(lhs, ctx, scope, depth)? == eval_boolean_inner(rhs, ctx, scope, depth)?)
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
        eval_integer_inner(lhs, ctx, scope, depth),
        eval_integer_inner(rhs, ctx, scope, depth),
    ) {
        return Some(compare_ordered(op, lhs, rhs));
    }
    let lhs = eval_real_inner(lhs, ctx, scope, depth)?;
    let rhs = eval_real_inner(rhs, ctx, scope, depth)?;
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
        match eval_boolean_inner(condition, ctx, scope, depth) {
            Some(true) => return eval_boolean_inner(value, ctx, scope, depth),
            Some(false) => {}
            None => {
                let common = eval_boolean_inner(value, ctx, scope, depth)?;
                if !remaining_boolean_outcomes_match(
                    &branches[index + 1..],
                    common,
                    ctx,
                    scope,
                    depth,
                ) {
                    return None;
                }
                return (eval_boolean_inner(else_branch, ctx, scope, depth) == Some(common))
                    .then_some(common);
            }
        }
    }
    eval_boolean_inner(else_branch, ctx, scope, depth)
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
        .all(|(_, value)| eval_boolean_inner(value, ctx, scope, depth) == Some(common))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_ast::{ComponentRefPart, Subscript};
    use std::sync::Arc;

    /// A context that overrides none of the trait's defaults: no real or
    /// enum lookups, no real/boolean calls, no integral-real coercion. What
    /// it CAN answer is integer arithmetic, which is the floor every phase
    /// context provides.
    struct IntegerOnly;

    impl AstScalarContext for IntegerOnly {
        fn lookup_integer(&self, _expr: &Expression, _scope: &str, _depth: usize) -> Option<i64> {
            None
        }

        fn lookup_boolean(&self, _expr: &Expression, _scope: &str, _depth: usize) -> Option<bool> {
            None
        }

        fn call_integer(
            &self,
            _function: &ComponentReference,
            _args: &[Expression],
            _scope: &str,
            _depth: usize,
            _span: Span,
        ) -> Option<i64> {
            None
        }

        fn integer_binary(&self, op: &OpBinary, lhs: i64, rhs: i64, _span: Span) -> Option<i64> {
            match op {
                OpBinary::Add => lhs.checked_add(rhs),
                OpBinary::Sub => lhs.checked_sub(rhs),
                OpBinary::Mul => lhs.checked_mul(rhs),
                _ => None,
            }
        }
    }

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text),
            location: Default::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn real_literal(text: &str) -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token: token(text),
            span: Span::DUMMY,
        }
    }

    fn int_literal(value: i64) -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: token(&value.to_string()),
            span: Span::DUMMY,
        }
    }

    fn bool_literal(value: bool) -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::Bool,
            token: token(if value { "true" } else { "false" }),
            span: Span::DUMMY,
        }
    }

    fn recovery_expression() -> Expression {
        Expression::Empty { span: Span::DUMMY }
    }

    fn comp_ref(name: &str) -> Expression {
        Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: token(name),
                subs: None::<Vec<Subscript>>,
                def_id: None,
            }],
            span: Span::DUMMY,
            qualified_display_name: None,
        })
    }

    fn compare(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op,
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
            span: Span::DUMMY,
        }
    }

    /// The default `lookup_real` answers `None`, so a context that never
    /// opted into real lookups must refuse to fold a named real rather than
    /// inventing a value for it. Literal arithmetic keeps folding: refusal is
    /// scoped to the lookup, not to the real domain.
    #[test]
    fn a_context_without_real_lookups_refuses_named_reals_but_folds_literals() {
        let ctx = IntegerOnly;
        assert_eq!(eval_real(&comp_ref("r"), &ctx, "scope", 0), None);
        assert_eq!(
            eval_real(
                &compare(OpBinary::Add, real_literal("1.5"), int_literal(2)),
                &ctx,
                "scope",
                0
            ),
            Some(3.5)
        );
        assert_eq!(
            eval_boolean(
                &compare(OpBinary::Lt, real_literal("1.5"), comp_ref("r")),
                &ctx,
                "scope",
                0
            ),
            None,
            "a comparison against an unavailable lookup must not fold"
        );
    }

    /// A mixed integer/real comparison falls through the integer attempt to
    /// the ordered real comparison, and each strictness variant answers for
    /// itself: `<=` on equal values is not `<`.
    #[test]
    fn mixed_comparisons_fold_through_the_ordered_real_path() {
        let ctx = IntegerOnly;
        let cases = [
            (OpBinary::Lt, "1.5", 2, Some(true)),
            (OpBinary::Gt, "1.5", 2, Some(false)),
            (OpBinary::Le, "2.0", 2, Some(true)),
            (OpBinary::Lt, "2.0", 2, Some(false)),
            (OpBinary::Ge, "2.0", 2, Some(true)),
        ];
        for (op, lhs, rhs, expected) in cases {
            let folded = eval_boolean(
                &compare(op.clone(), real_literal(lhs), int_literal(rhs)),
                &ctx,
                "scope",
                0,
            );
            assert_eq!(folded, expected, "{lhs} {op:?} {rhs}");
        }
    }

    #[test]
    fn recovery_unary_operator_never_folds_as_unary_plus() {
        let ctx = IntegerOnly;
        let integer = Expression::Unary {
            op: OpUnary::Empty,
            rhs: Arc::new(int_literal(7)),
            span: Span::DUMMY,
        };
        let real = Expression::Unary {
            op: OpUnary::Empty,
            rhs: Arc::new(real_literal("7.5")),
            span: Span::DUMMY,
        };

        assert_eq!(eval_integer(&integer, &ctx, "scope", 0), None);
        assert_eq!(eval_real(&real, &ctx, "scope", 0), None);
    }

    #[test]
    fn recovery_control_flow_never_launders_an_equal_or_short_circuit_result() {
        let ctx = IntegerOnly;
        let integer_if = Expression::If {
            branches: vec![(recovery_expression(), int_literal(1))],
            else_branch: Arc::new(int_literal(1)),
            span: Span::DUMMY,
        };
        let real_if = Expression::If {
            branches: vec![(recovery_expression(), real_literal("1.0"))],
            else_branch: Arc::new(real_literal("1.0")),
            span: Span::DUMMY,
        };
        let boolean_if = Expression::If {
            branches: vec![(recovery_expression(), bool_literal(true))],
            else_branch: Arc::new(bool_literal(true)),
            span: Span::DUMMY,
        };
        let and = compare(OpBinary::And, recovery_expression(), bool_literal(false));
        let or = compare(OpBinary::Or, recovery_expression(), bool_literal(true));

        assert_eq!(eval_integer(&integer_if, &ctx, "scope", 0), None);
        assert_eq!(eval_real(&real_if, &ctx, "scope", 0), None);
        assert_eq!(eval_boolean(&boolean_if, &ctx, "scope", 0), None);
        assert_eq!(eval_boolean(&and, &ctx, "scope", 0), None);
        assert_eq!(eval_boolean(&or, &ctx, "scope", 0), None);
    }

    #[test]
    fn context_only_syntax_never_launders_an_equal_or_short_circuit_result() {
        let ctx = IntegerOnly;
        for invalid in [
            compare(OpBinary::Assign, int_literal(1), int_literal(2)),
            Expression::Terminal {
                terminal_type: TerminalType::End,
                token: token("end"),
                span: Span::DUMMY,
            },
        ] {
            let integer_if = Expression::If {
                branches: vec![(invalid.clone(), int_literal(3))],
                else_branch: Arc::new(int_literal(3)),
                span: Span::DUMMY,
            };
            let real_if = Expression::If {
                branches: vec![(invalid.clone(), real_literal("3.0"))],
                else_branch: Arc::new(real_literal("3.0")),
                span: Span::DUMMY,
            };
            let and = compare(OpBinary::And, invalid.clone(), bool_literal(false));
            let or = compare(OpBinary::Or, invalid, bool_literal(true));

            assert_eq!(eval_integer(&integer_if, &ctx, "scope", 0), None);
            assert_eq!(eval_real(&real_if, &ctx, "scope", 0), None);
            assert_eq!(eval_boolean(&and, &ctx, "scope", 0), None);
            assert_eq!(eval_boolean(&or, &ctx, "scope", 0), None);
        }
    }
}
