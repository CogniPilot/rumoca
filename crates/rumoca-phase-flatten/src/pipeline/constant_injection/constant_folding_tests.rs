use std::sync::Arc;

use super::{
    extract_named_record_constructor_fields, infer_dims_from_expr,
    try_eval_const_boolean_with_scope,
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

fn component_ref(path: &str, def_id: Option<rumoca_core::DefId>) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: path
            .split('.')
            .map(|part| ast::ComponentRefPart {
                ident: token(part),
                subs: None,
            })
            .collect(),
        span: rumoca_core::Span::DUMMY,
        def_id,
    }
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
fn named_record_constructor_extraction_preserves_target_identity() {
    let record_def_id = rumoca_core::DefId::new(42);
    let expr = ast::Expression::ClassModification {
        target: component_ref(
            "Utilities.ParameterRecords.MachineData",
            Some(record_def_id),
        ),
        modifications: vec![ast::Expression::Modification {
            target: component_ref("PRef", None),
            value: Arc::new(real_expr("1000.0")),
            span: rumoca_core::Span::DUMMY,
        }],
        each_flags: vec![false],
        final_flags: vec![false],
        redeclare_flags: vec![false],
        span: rumoca_core::Span::DUMMY,
    };

    let (constructor, fields) = extract_named_record_constructor_fields(&expr)
        .expect("named record constructor should be extracted");

    assert_eq!(constructor.target_def_id(), Some(record_def_id));
    assert_eq!(
        constructor.as_str(),
        "Utilities.ParameterRecords.MachineData"
    );
    assert_eq!(fields.len(), 1);
}
