use std::sync::Arc;

use super::{
    infer_dims_from_expr, try_eval_const_boolean_with_scope, try_eval_const_real_with_scope,
};
use crate::Context;
use rumoca_ir_ast as ast;

fn token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: Arc::from(text),
        ..rumoca_core::Token::default()
    }
}

fn bool_expr(value: bool) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::Bool,
        token: token(if value { "true" } else { "false" }),
        span: rumoca_core::Span::DUMMY,
    }
}

fn real_expr(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedReal,
        token: token(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn int_expr(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: token(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn unknown_bool_ref(name: &str) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: token(name),
            subs: None,
        }],
        span: rumoca_core::Span::DUMMY,
        def_id: None,
    })
}

fn real_binary(
    op: rumoca_core::OpBinary,
    lhs: ast::Expression,
    rhs: ast::Expression,
) -> ast::Expression {
    ast::Expression::Binary {
        op,
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn comp_ref(path: &str) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: crate::path_utils::split_path_with_indices(path)
            .into_iter()
            .map(|part| ast::ComponentRefPart {
                ident: token(part),
                subs: None,
            })
            .collect(),
        span: rumoca_core::Span::DUMMY,
        def_id: None,
    }
}

fn call_expr(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: comp_ref(name),
        args,
        span: rumoca_core::Span::DUMMY,
    }
}

fn bool_binary(
    op: rumoca_core::OpBinary,
    lhs: ast::Expression,
    rhs: ast::Expression,
) -> ast::Expression {
    ast::Expression::Binary {
        op,
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn if_expr(
    cond: ast::Expression,
    then_expr: ast::Expression,
    else_expr: ast::Expression,
) -> ast::Expression {
    ast::Expression::If {
        branches: vec![(cond, then_expr)],
        else_branch: Arc::new(else_expr),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn infers_constant_range_binding_dimensions() {
    let expr = ast::Expression::Range {
        start: Arc::new(int_expr("1")),
        step: Some(Arc::new(int_expr("2"))),
        end: Arc::new(int_expr("5")),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(
        infer_dims_from_expr(&expr, &Context::default(), ""),
        Some(vec![3])
    );
}

#[test]
fn boolean_constant_folding_short_circuits_unknown_and_rhs() {
    let expr = bool_binary(
        rumoca_core::OpBinary::And,
        bool_expr(false),
        unknown_bool_ref("unknownFlag"),
    );
    assert_eq!(
        try_eval_const_boolean_with_scope(&expr, &Context::default(), ""),
        Some(false)
    );
}

#[test]
fn boolean_constant_folding_short_circuits_unknown_or_rhs() {
    let expr = bool_binary(
        rumoca_core::OpBinary::Or,
        bool_expr(true),
        unknown_bool_ref("unknownFlag"),
    );
    assert_eq!(
        try_eval_const_boolean_with_scope(&expr, &Context::default(), ""),
        Some(true)
    );
}

#[test]
fn boolean_constant_folding_handles_if_expressions() {
    let expr = if_expr(bool_expr(false), bool_expr(false), bool_expr(true));
    assert_eq!(
        try_eval_const_boolean_with_scope(&expr, &Context::default(), ""),
        Some(true)
    );
}

#[test]
fn relational_constant_folding_compares_real_values() {
    let expr = real_binary(
        rumoca_core::OpBinary::Lt,
        real_expr("-1.5"),
        real_expr("-1.0"),
    );
    assert_eq!(
        try_eval_const_boolean_with_scope(&expr, &Context::default(), ""),
        Some(true)
    );
}

#[test]
fn real_constant_folding_evaluates_modelica_math_calls() {
    let expr = real_binary(
        rumoca_core::OpBinary::Mul,
        int_expr("2"),
        call_expr("Modelica.Math.asin", vec![real_expr("1.0")]),
    );

    let value = try_eval_const_real_with_scope(&expr, &Context::default(), "")
        .expect("expression should evaluate");
    assert!((value - std::f64::consts::PI).abs() < f64::EPSILON);
}
