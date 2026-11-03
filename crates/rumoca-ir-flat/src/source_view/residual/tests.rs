use super::*;
use rumoca_core::{Literal, SourceId};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("source_residual.mo"), 3, 17)
}

fn literal(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: span(),
    }
}

fn equation(op: OpBinary) -> Equation {
    Equation::new(
        Expression::Binary {
            op,
            lhs: Box::new(literal(-0.0)),
            rhs: Box::new(literal(f64::NAN)),
            span: span(),
        },
        span(),
        EquationOrigin::ComponentEquation {
            component: "residual".into(),
        },
    )
}

#[test]
fn subtraction_and_elementwise_subtraction_keep_exact_original_operands() {
    for operator in [OpBinary::Sub, OpBinary::SubElem] {
        let source = equation(operator);
        let presentation = source.source_presentation();
        let SourceEquationSides::Equality { left, right } = presentation.sides() else {
            panic!("canonical subtraction equality");
        };
        let Expression::Binary { lhs, rhs, .. } = &source.residual else {
            unreachable!()
        };
        assert!(std::ptr::eq(left, lhs.as_ref()));
        assert!(std::ptr::eq(right, rhs.as_ref()));
        assert_eq!(presentation.span(), source.span);
        assert!(std::ptr::eq(presentation.origin(), &source.origin));
        let Expression::Literal {
            value: Literal::Real(value),
            ..
        } = left
        else {
            panic!("left")
        };
        assert_eq!(value.to_bits(), (-0.0f64).to_bits());
        let Expression::Literal {
            value: Literal::Real(value),
            ..
        } = right
        else {
            panic!("right")
        };
        assert_eq!(value.to_bits(), f64::NAN.to_bits());
    }
}

#[test]
fn other_operators_keep_the_complete_residual_and_do_not_rewrite_children() {
    for operator in [OpBinary::Add, OpBinary::Mul, OpBinary::Eq] {
        let source = equation(operator);
        let presentation = source.source_presentation();
        let SourceEquationSides::ZeroResidual(residual) = presentation.sides() else {
            panic!("no subtraction split");
        };
        assert!(std::ptr::eq(residual, &source.residual));
    }
}

#[test]
fn a_nonbinary_residual_is_borrowed_without_inventing_an_ast_zero() {
    let source = Equation::new(
        literal(2.0),
        span(),
        EquationOrigin::ComponentEquation {
            component: "literal".into(),
        },
    );
    let view = source.source_presentation();
    let SourceEquationSides::ZeroResidual(residual) = view.sides() else {
        panic!("zero residual view")
    };
    assert!(std::ptr::eq(residual, &source.residual));
}

#[test]
fn only_the_outer_subtraction_is_presented_as_equality() {
    let mut source = equation(OpBinary::Sub);
    if let Expression::Binary { lhs, .. } = &mut source.residual {
        **lhs = equation(OpBinary::Sub).residual;
    }
    let view = source.source_presentation();
    let SourceEquationSides::Equality { left, .. } = view.sides() else {
        panic!("outer equality")
    };
    assert!(matches!(
        left,
        Expression::Binary {
            op: OpBinary::Sub,
            ..
        }
    ));
}
