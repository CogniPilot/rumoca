//! Builders for the expression precedence cascade (WI-3).
//!
//! The whole cascade collapses to one AST type ([`crate::ast::Expression`]): the
//! left-associative classes fold left, `power_expr` folds right, and the printer
//! re-parenthesizes cross-class mixes (trap T6), so a permissive cascade is safe
//! (§3.3). Every builder is `TryFrom<&Generated> for AstType` with
//! `type Error = anyhow::Error` (see [`crate::parse::refs`] for the rationale);
//! typed rejections are bridged with `GalecParseError::into_anyhow()`.

use crate::ast::{BinaryOp, Expression};
use crate::parse::errors::GalecParseError;
use crate::parse::generated::galec_grammar_trait as g;
use crate::parse::refs::{computed_dimensions_to_vec, state_reference_tail_parts};

// ---------------------------------------------------------------------------
// Precedence cascade — left folds
// ---------------------------------------------------------------------------

/// `expression : logical_or_expr` — the cascade entry; nothing to fold.
impl TryFrom<&g::Expression> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::Expression) -> Result<Self, Self::Error> {
        Ok(ast.logical_or_expr.clone())
    }
}

/// `logical_or_expr : logical_and_expr { 'or' logical_and_expr }` — left fold.
impl TryFrom<&g::LogicalOrExpr> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::LogicalOrExpr) -> Result<Self, Self::Error> {
        let mut acc = ast.logical_and_expr.clone();
        for tail in &ast.logical_or_expr_list {
            acc = Expression::binary(BinaryOp::Or, acc, tail.logical_and_expr.clone());
        }
        Ok(acc)
    }
}

/// `logical_and_expr : equality_expr { 'and' equality_expr }` — left fold.
impl TryFrom<&g::LogicalAndExpr> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::LogicalAndExpr) -> Result<Self, Self::Error> {
        let mut acc = ast.equality_expr.clone();
        for tail in &ast.logical_and_expr_list {
            acc = Expression::binary(BinaryOp::And, acc, tail.equality_expr.clone());
        }
        Ok(acc)
    }
}

/// `equality_expr : relational_expr { equality_operator relational_expr }`.
impl TryFrom<&g::EqualityExpr> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::EqualityExpr) -> Result<Self, Self::Error> {
        let mut acc = ast.relational_expr.clone();
        for tail in &ast.equality_expr_list {
            let op = match &tail.equality_operator {
                g::EqualityOperator::EquEqu(_) => BinaryOp::Eq,
                g::EqualityOperator::LTGT(_) => BinaryOp::Ne,
            };
            acc = Expression::binary(op, acc, tail.relational_expr.clone());
        }
        Ok(acc)
    }
}

/// `relational_expr : additive_expr { relational_operator additive_expr }`.
impl TryFrom<&g::RelationalExpr> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::RelationalExpr) -> Result<Self, Self::Error> {
        let mut acc = ast.additive_expr.clone();
        for tail in &ast.relational_expr_list {
            let op = match &tail.relational_operator {
                g::RelationalOperator::LTEqu(_) => BinaryOp::Le,
                g::RelationalOperator::GTEqu(_) => BinaryOp::Ge,
                g::RelationalOperator::LT(_) => BinaryOp::Lt,
                g::RelationalOperator::GT(_) => BinaryOp::Gt,
            };
            acc = Expression::binary(op, acc, tail.additive_expr.clone());
        }
        Ok(acc)
    }
}

/// `additive_expr : multiplicative_expr { add_operator multiplicative_expr }`.
impl TryFrom<&g::AdditiveExpr> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::AdditiveExpr) -> Result<Self, Self::Error> {
        let mut acc = ast.multiplicative_expr.clone();
        for tail in &ast.additive_expr_list {
            let op = match &tail.add_operator {
                g::AddOperator::Plus(_) => BinaryOp::Add,
                g::AddOperator::Minus(_) => BinaryOp::Sub,
            };
            acc = Expression::binary(op, acc, tail.multiplicative_expr.clone());
        }
        Ok(acc)
    }
}

/// `multiplicative_expr : power_expr { mul_operator power_expr }`.
impl TryFrom<&g::MultiplicativeExpr> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::MultiplicativeExpr) -> Result<Self, Self::Error> {
        let mut acc = ast.power_expr.clone();
        for tail in &ast.multiplicative_expr_list {
            let op = match &tail.mul_operator {
                g::MulOperator::Star(_) => BinaryOp::Mul,
                g::MulOperator::Slash(_) => BinaryOp::Div,
            };
            acc = Expression::binary(op, acc, tail.power_expr.clone());
        }
        Ok(acc)
    }
}

// ---------------------------------------------------------------------------
// Precedence cascade — right fold (power)
// ---------------------------------------------------------------------------

/// `power_expr : unary_expr { '^' unary_expr }` — `^` is right-associative
/// (`a^b^c == a^(b^c)`), so fold from the right.
impl TryFrom<&g::PowerExpr> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::PowerExpr) -> Result<Self, Self::Error> {
        let mut operands = Vec::with_capacity(1 + ast.power_expr_list.len());
        operands.push(ast.unary_expr.clone());
        for tail in &ast.power_expr_list {
            operands.push(tail.unary_expr.clone());
        }
        // `unary_expr` is mandatory, so `operands` is never empty.
        let mut acc = operands
            .pop()
            .expect("power_expr always has at least one unary_expr operand");
        while let Some(lhs) = operands.pop() {
            acc = Expression::binary(BinaryOp::Pow, lhs, acc);
        }
        Ok(acc)
    }
}

// ---------------------------------------------------------------------------
// Unary & primary
// ---------------------------------------------------------------------------

/// `unary_expr : neg | not_expr | primary`.
///
/// `neg` is grammar-limited to a bare reference (trap T4); `not_expr` wraps a
/// self-delimiting `parenthesized_or_if` operand (trap T12).
impl TryFrom<&g::UnaryExpr> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::UnaryExpr) -> Result<Self, Self::Error> {
        Ok(match ast {
            g::UnaryExpr::Neg(u) => Expression::Neg(u.neg.reference.clone()),
            g::UnaryExpr::NotExpr(u) => {
                Expression::Not(Box::new(u.not_expr.parenthesized_or_if.clone()))
            }
            g::UnaryExpr::Primary(u) => u.primary.as_ref().clone(),
        })
    }
}

/// `primary : constant | dimension_query | reference_or_call
///           | parenthesized_or_if | multi_dimension_constructor`.
impl TryFrom<&g::Primary> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::Primary) -> Result<Self, Self::Error> {
        Ok(match ast {
            g::Primary::Constant(p) => constant_to_expr(&p.constant)?,
            g::Primary::DimensionQuery(p) => {
                let query = &p.dimension_query;
                Expression::Size {
                    array: query.reference.clone(),
                    dimension: Box::new(query.expression.clone()),
                }
            }
            g::Primary::ReferenceOrCall(p) => reference_or_call_to_expr(&p.reference_or_call),
            // Already `Paren`/`If` (converted via the `parenthesized_or_if` builder).
            g::Primary::ParenthesizedOrIf(p) => p.parenthesized_or_if.clone(),
            g::Primary::MultiDimensionConstructor(p) => {
                Expression::Array(mdc_to_vec(&p.multi_dimension_constructor))
            }
        })
    }
}

/// `parenthesized_or_if : '(' ( if_expression | expression ) ')'`.
///
/// The `if_expression` alternative is self-parenthesizing ([`Expression::If`]
/// prints its own `(…)`); the plain-`expression` alternative becomes an explicit
/// [`Expression::Paren`].
impl TryFrom<&g::ParenthesizedOrIf> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::ParenthesizedOrIf) -> Result<Self, Self::Error> {
        Ok(match &ast.parenthesized_or_if_group {
            g::ParenthesizedOrIfGroup::IfExpression(group) => {
                Expression::If(group.if_expression.clone())
            }
            g::ParenthesizedOrIfGroup::Expression(group) => {
                Expression::Paren(Box::new(group.expression.clone()))
            }
        })
    }
}

// ---------------------------------------------------------------------------
// if-expression & function call
// ---------------------------------------------------------------------------

/// `if_expression : 'if' e 'then' e { 'elseif' e 'then' e } 'else' e`.
impl TryFrom<&g::IfExpression> for crate::ast::IfExpression {
    type Error = anyhow::Error;

    fn try_from(ast: &g::IfExpression) -> Result<Self, Self::Error> {
        let mut branches = Vec::with_capacity(1 + ast.if_expression_list.len());
        branches.push((ast.expression.clone(), ast.expression0.clone()));
        for branch in &ast.if_expression_list {
            branches.push((branch.expression.clone(), branch.expression0.clone()));
        }
        Ok(Self {
            branches,
            else_value: Box::new(ast.expression1.clone()),
        })
    }
}

/// `function_call : name call_args`.
impl TryFrom<&g::FunctionCall> for crate::ast::FunctionCall {
    type Error = anyhow::Error;

    fn try_from(ast: &g::FunctionCall) -> Result<Self, Self::Error> {
        Ok(Self {
            function: ast.name.clone(),
            arguments: call_args_to_vec(&ast.call_args),
        })
    }
}

// ---------------------------------------------------------------------------
// Helpers over non-`%nt_type` sub-productions (consumed inline, not via TryFrom)
// ---------------------------------------------------------------------------

/// `constant : 'true' | 'false' | integer | real` → the literal expression.
///
/// The lexeme grammar already enforces trap-T7 spelling; the parsed `i64`/`f64`
/// value is stored (not the raw text) so equal literals compare equal. A value
/// the lexer accepted but Rust cannot parse (e.g. an out-of-range integer) is a
/// typed syntax error, never a silent default.
fn constant_to_expr(constant: &g::Constant) -> anyhow::Result<Expression> {
    Ok(match constant {
        g::Constant::True(_) => Expression::Bool(true),
        g::Constant::False(_) => Expression::Bool(false),
        g::Constant::Integer(c) => {
            let text = c.integer.integer.text();
            let value = c.integer.integer.as_i64().map_err(|err| {
                GalecParseError::syntax(format!("invalid integer literal `{text}`: {err}"))
                    .into_anyhow()
            })?;
            Expression::Integer(value)
        }
        g::Constant::Real(c) => {
            let text = c.real.real.text();
            let value = c.real.real.as_f64().map_err(|err| {
                GalecParseError::syntax(format!("invalid real literal `{text}`: {err}"))
                    .into_anyhow()
            })?;
            Expression::Real(value)
        }
    })
}

/// `reference_or_call : state_reference | local_ref_or_call` → a reference
/// expression or a call expression (the `local_ref_or_call` alternative folds
/// the `name (…)` vs `name[…]` ambiguity).
fn reference_or_call_to_expr(roc: &g::ReferenceOrCall) -> Expression {
    match roc {
        g::ReferenceOrCall::StateReference(r) => Expression::Ref(crate::ast::Reference::State(
            state_reference_tail_parts(&r.state_reference.state_reference_tail),
        )),
        g::ReferenceOrCall::LocalRefOrCall(r) => local_ref_or_call_to_expr(&r.local_ref_or_call),
    }
}

/// `local_ref_or_call : name [ call_args | computed_dimensions ]`.
fn local_ref_or_call_to_expr(local: &g::LocalRefOrCall) -> Expression {
    let name = local.name.clone();
    match &local.local_ref_or_call_opt {
        None => Expression::Ref(crate::ast::Reference::Local(crate::ast::RefPart {
            name,
            subscripts: Vec::new(),
        })),
        Some(opt) => match &opt.local_ref_or_call_opt_group {
            g::LocalRefOrCallOptGroup::CallArgs(group) => {
                Expression::Call(crate::ast::FunctionCall {
                    function: name,
                    arguments: call_args_to_vec(&group.call_args),
                })
            }
            g::LocalRefOrCallOptGroup::ComputedDimensions(group) => {
                Expression::Ref(crate::ast::Reference::Local(crate::ast::RefPart {
                    name,
                    subscripts: computed_dimensions_to_vec(&group.computed_dimensions),
                }))
            }
        },
    }
}

/// `call_args : '(' [ expression { ',' expression } ] ')'` → the argument list
/// (possibly empty). Shared with the statement builders (WI-4).
pub(crate) fn call_args_to_vec(args: &g::CallArgs) -> Vec<Expression> {
    match &args.call_args_opt {
        None => Vec::new(),
        Some(opt) => {
            let mut out = Vec::with_capacity(1 + opt.call_args_opt_list.len());
            out.push(opt.expression.clone());
            for extra in &opt.call_args_opt_list {
                out.push(extra.expression.clone());
            }
            out
        }
    }
}

/// `multi_dimension_constructor : '{' expression { ',' expression } '}'` → the
/// (non-empty) element list.
fn mdc_to_vec(mdc: &g::MultiDimensionConstructor) -> Vec<Expression> {
    let mut out = Vec::with_capacity(1 + mdc.multi_dimension_constructor_list.len());
    out.push(mdc.expression.clone());
    for extra in &mdc.multi_dimension_constructor_list {
        out.push(extra.expression.clone());
    }
    out
}
