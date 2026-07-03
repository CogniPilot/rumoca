//! GAL-019 expression printing rules: unary-minus rewriting (T4),
//! cross-precedence-class parenthesization without re-association (T6),
//! parenthesized `not` and self-parenthesized if-expressions (T12), and
//! quoted-identifier printing (T13).

use rumoca_galec::ast::{
    BinaryOp, Expression, FunctionCall, IfExpression, Name, RefPart, Reference,
};
use rumoca_galec::{GalecError, print_expression};

fn lref(name: &str) -> Expression {
    Expression::Ref(Reference::local(Name::ident(name)))
}

fn sref(name: &str) -> Expression {
    Expression::Ref(Reference::state(Name::ident(name)))
}

fn bin(op: BinaryOp, lhs: Expression, rhs: Expression) -> Expression {
    Expression::binary(op, lhs, rhs)
}

fn printed(expression: &Expression) -> String {
    print_expression(expression).expect("printable expression")
}

#[test]
fn unary_minus_prints_only_over_references() {
    assert_eq!(
        printed(&Expression::Neg(Reference::local(Name::ident("b")))),
        "-b"
    );
    assert_eq!(
        printed(&Expression::Neg(Reference::state(Name::ident("x")))),
        "-self.x"
    );
}

#[test]
fn negated_real_rewrites_non_references() {
    // A reference stays unary minus.
    assert_eq!(printed(&Expression::negated_real(lref("b"))), "-b");
    // A literal folds.
    assert_eq!(
        printed(&Expression::negated_real(Expression::Real(2.5))),
        "-2.5"
    );
    // Anything else becomes `0.0 - (expr)` (never `-(expr)`).
    let negated_power =
        Expression::negated_real(bin(BinaryOp::Pow, lref("b"), Expression::Integer(2)));
    assert_eq!(printed(&negated_power), "0.0 - (b ^ 2)");
    let negated_call = Expression::negated_real(Expression::Call(FunctionCall {
        function: Name::ident("sqrt"),
        arguments: vec![lref("x")],
    }));
    assert_eq!(printed(&negated_call), "0.0 - (sqrt(x))");
}

#[test]
fn negated_integer_uses_integer_zero() {
    let negated =
        Expression::negated_integer(bin(BinaryOp::Add, lref("i"), Expression::Integer(1)));
    assert_eq!(printed(&negated), "0 - (i + 1)");
    assert_eq!(
        printed(&Expression::negated_integer(Expression::Integer(7))),
        "-7"
    );
}

#[test]
fn negated_integer_min_takes_the_binary_form() {
    // `-i64::MIN` has no i64 representation; the fold must not overflow and
    // instead emit the `0 - (…)` rewrite.
    let negated = Expression::negated_integer(Expression::Integer(i64::MIN));
    assert_eq!(printed(&negated), format!("0 - ({})", i64::MIN));
}

#[test]
fn cross_class_mixes_are_parenthesized() {
    // `a^2*b` is invalid GALEC; the printer must emit `(a ^ 2) * b`.
    let power_times = bin(
        BinaryOp::Mul,
        bin(BinaryOp::Pow, lref("a"), Expression::Integer(2)),
        lref("b"),
    );
    assert_eq!(printed(&power_times), "(a ^ 2) * b");

    let or_in_and = bin(
        BinaryOp::And,
        bin(BinaryOp::Or, lref("a"), lref("b")),
        lref("c"),
    );
    assert_eq!(printed(&or_in_and), "(a or b) and c");

    let relational_in_and = bin(
        BinaryOp::And,
        bin(BinaryOp::Lt, lref("a"), lref("b")),
        bin(BinaryOp::Gt, lref("c"), lref("d")),
    );
    assert_eq!(printed(&relational_in_and), "(a < b) and (c > d)");

    let mul_in_add = bin(
        BinaryOp::Add,
        bin(BinaryOp::Mul, lref("k"), lref("e")),
        sref("offset"),
    );
    assert_eq!(printed(&mul_in_add), "(k * e) + self.offset");
}

#[test]
fn same_class_chains_preserve_ast_order() {
    // Left-nested `+` prints bare; the spelling means `(a + b) + c`.
    let left_nested = bin(
        BinaryOp::Add,
        bin(BinaryOp::Add, lref("a"), lref("b")),
        lref("c"),
    );
    assert_eq!(printed(&left_nested), "a + b + c");
    // Right-nested `+` must keep its parentheses — no re-association.
    let right_nested = bin(
        BinaryOp::Add,
        lref("a"),
        bin(BinaryOp::Add, lref("b"), lref("c")),
    );
    assert_eq!(printed(&right_nested), "a + (b + c)");
    // `^` is right-associative: right-nested prints bare, left-nested parens.
    let pow_right = bin(
        BinaryOp::Pow,
        lref("a"),
        bin(BinaryOp::Pow, lref("b"), lref("c")),
    );
    assert_eq!(printed(&pow_right), "a ^ b ^ c");
    let pow_left = bin(
        BinaryOp::Pow,
        bin(BinaryOp::Pow, lref("a"), lref("b")),
        lref("c"),
    );
    assert_eq!(printed(&pow_left), "(a ^ b) ^ c");
}

#[test]
fn unary_operands_and_negative_literals_parenthesized_in_binaries() {
    let neg_base = bin(
        BinaryOp::Pow,
        Expression::Neg(Reference::local(Name::ident("a"))),
        Expression::Integer(2),
    );
    assert_eq!(printed(&neg_base), "(-a) ^ 2");
    let minus_negative_literal = bin(BinaryOp::Sub, lref("a"), Expression::Real(-50.0));
    assert_eq!(printed(&minus_negative_literal), "a - (-50.0)");
    let not_in_and = bin(
        BinaryOp::And,
        Expression::Not(Box::new(lref("a"))),
        lref("b"),
    );
    assert_eq!(printed(&not_in_and), "(not (a)) and b");
}

#[test]
fn not_is_always_parenthesized() {
    let not_or = Expression::Not(Box::new(bin(BinaryOp::Or, lref("b1"), lref("b2"))));
    assert_eq!(printed(&not_or), "not (b1 or b2)");
    // An if-expression argument is already self-parenthesized.
    let not_if = Expression::Not(Box::new(Expression::If(IfExpression {
        branches: vec![(lref("c"), Expression::Bool(true))],
        else_value: Box::new(Expression::Bool(false)),
    })));
    assert_eq!(printed(&not_if), "not (if c then true else false)");
}

#[test]
fn if_expressions_self_parenthesize_with_mandatory_else() {
    let limiter = Expression::If(IfExpression {
        branches: vec![
            (
                bin(BinaryOp::Gt, lref("u"), Expression::Real(50.0)),
                Expression::Real(50.0),
            ),
            (
                bin(BinaryOp::Lt, lref("u"), Expression::Real(-50.0)),
                Expression::Real(-50.0),
            ),
        ],
        else_value: Box::new(lref("u")),
    });
    assert_eq!(
        printed(&limiter),
        "(if u > 50.0 then 50.0 elseif u < (-50.0) then -50.0 else u)"
    );
}

#[test]
fn if_expression_needs_at_least_one_branch() {
    let empty = Expression::If(IfExpression {
        branches: vec![],
        else_value: Box::new(lref("u")),
    });
    let error = print_expression(&empty).expect_err("no branches must fail");
    assert_eq!(error.code(), "EG009");
}

#[test]
fn quoted_identifiers_print_with_quotes() {
    let previous = Expression::Ref(Reference::state(Name::quoted("previous(feedback.y)")));
    assert_eq!(printed(&previous), "self.'previous(feedback.y)'");
    // Quoted part with subscripts and a nested plain part.
    let nested = Expression::Ref(Reference::State(vec![
        RefPart {
            name: Name::quoted("shaft[2].gear"),
            subscripts: vec![Expression::Integer(1)],
        },
        RefPart::plain(Name::ident("w")),
    ]));
    assert_eq!(printed(&nested), "self.'shaft[2].gear'[1].w");
    // A local quoted identifier (scalarized method local).
    let local = Expression::Ref(Reference::local(Name::quoted("derivative(PID.I.x)")));
    assert_eq!(printed(&local), "'derivative(PID.I.x)'");
}

#[test]
fn malformed_quoted_identifiers_rejected() {
    for content in ["", "a b", "it's", "a\tb", "a\nb"] {
        let expression = Expression::Ref(Reference::local(Name::quoted(content)));
        let error = print_expression(&expression).expect_err("malformed quoted must fail");
        assert!(
            matches!(error, GalecError::MalformedQuotedIdentifier { .. }),
            "wrong error for {content:?}: {error}"
        );
        assert_eq!(error.code(), "EG003");
    }
}

#[test]
fn size_query_and_array_constructors() {
    let size = Expression::Size {
        array: Reference::state(Name::ident("table")),
        dimension: Box::new(Expression::Integer(1)),
    };
    assert_eq!(printed(&size), "size(self.table, 1)");
    // Row-major nested constructor.
    let matrix = Expression::Array(vec![
        Expression::Array(vec![Expression::Real(1.0), Expression::Real(2.0)]),
        Expression::Array(vec![Expression::Real(3.0), Expression::Real(4.0)]),
    ]);
    assert_eq!(printed(&matrix), "{{1.0, 2.0}, {3.0, 4.0}}");
    let empty = Expression::Array(vec![]);
    let error = print_expression(&empty).expect_err("empty constructor must fail");
    assert_eq!(error.code(), "EG007");
}
