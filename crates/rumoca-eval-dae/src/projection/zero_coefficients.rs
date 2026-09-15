//! Invariant-zero incidence proofs over the unchanged tensor expression owner.

use super::*;

#[derive(Default)]
pub(super) struct ZeroCoefficients<'dae> {
    values: HashMap<(dae::ExprId<'dae>, usize), bool>,
}

impl<'dae> ZeroCoefficients<'dae> {
    pub(super) fn omits_coordinate(
        &mut self,
        view: dae::DaeView<'dae>,
        coordinate: dae::ExprId<'dae>,
        coefficient: dae::ExprId<'dae>,
        scalar: usize,
    ) -> bool {
        let node = view.expression(coordinate).unwrap();
        node.value_type().scalar_type() == dae::ScalarType::Real
            && matches!(
                node.operation(),
                dae::ExpressionOperation::Coordinate(
                    dae::CoordinateView::Algebraic(_)
                        | dae::CoordinateView::State(_)
                        | dae::CoordinateView::Derivative(_)
                )
            )
            && self.prove(view, coefficient, scalar, &mut Vec::new())
    }

    fn prove(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        active: &mut Vec<dae::ExprId<'dae>>,
    ) -> bool {
        if let Some(&zero) = self.values.get(&(expression, scalar)) {
            return zero;
        }
        if active.contains(&expression) {
            return false;
        }
        active.push(expression);
        let zero = self.prove_operation(view, expression, scalar, active);
        active.pop();
        self.values.insert((expression, scalar), zero);
        zero
    }

    fn prove_operation(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        active: &mut Vec<dae::ExprId<'dae>>,
    ) -> bool {
        let node = view.expression(expression).unwrap();
        match node.operation() {
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Integer(0) | dae::DaeLiteral::Real(0.0),
            ) => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let variable = view.variable(parameter.into()).unwrap();
                !variable.is_tunable()
                    && variable.fixed() != Some(false)
                    && variable
                        .binding()
                        .is_some_and(|binding| self.prove(view, binding, scalar, active))
            }
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => self.prove(view, operand, scalar, active),
            dae::ExpressionOperation::Array(elements) => {
                let (element, scalar) = scalar_selection::array_scalar(view, elements, scalar);
                self.prove(view, element, scalar, active)
            }
            dae::ExpressionOperation::Builtin {
                builtin: builtin @ (dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2),
                arguments,
            } => {
                let (element, scalar) = scalar_selection::concatenation_scalar(
                    view,
                    arguments,
                    usize::from(builtin == dae::PureBuiltin::PromotedCat2),
                    node.value_type().dimensions(),
                    scalar,
                );
                self.prove(view, element, scalar, active)
            }
            _ => false,
        }
    }
}
