//! Exact symbolic differentiation over the rebuilt expression graph.
//!
//! Differentiation emits into the same construction as the rebuild, so a
//! derivative and the value it was taken from share subexpressions rather than
//! duplicating them. [`Derivative::Zero`] is a real algebraic zero, not a
//! literal, which keeps a structurally vanishing term out of the rebuilt graph
//! entirely instead of leaving `0 * x` for a later pass to notice.
//!
//! Every `unreachable!` below is discharged by a preflight in
//! [`constraints`](super::constraints): only an expression that
//! `is_differentiable` or `can_differentiate_order` already accepted ever
//! reaches these arms.

use rumoca_ir_dae as dae;

use super::equalities::{EqualityAnchor, EqualitySign};
use super::expressions::ExpressionRebuilder;
use super::variables::TargetVariable;

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(super) fn differentiate(
        &mut self,
        source_id: dae::ExprId<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        self.differentiate_order(source_id, 1, provenance)
    }

    pub(super) fn differentiate_order(
        &mut self,
        source_id: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let source = self
            .source
            .expression(source_id)
            .expect("differentiable expression identity resolves");
        match source.operation() {
            dae::ExpressionOperation::Literal(_) => Ok(Derivative::Zero),
            dae::ExpressionOperation::Coordinate(coordinate) => match coordinate {
                dae::CoordinateView::Parameter(_) => Ok(Derivative::Zero),
                dae::CoordinateView::Time if order == 1 => self
                    .target
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(1.0))
                    .map(Derivative::Expression),
                dae::CoordinateView::Time => Ok(Derivative::Zero),
                dae::CoordinateView::State(state) => {
                    self.differentiate_state(state, order, provenance)
                }
                // An algebraic the system proves equal to a class anchor
                // differentiates as that anchor: the equality holds for all
                // time, so its derivative holds too.
                dae::CoordinateView::Algebraic(algebraic) => {
                    self.differentiate_equality_class(algebraic, order, provenance)
                }
                // A derivative coordinate the system defines outright carries
                // its own definition forward one order at a time.
                dae::CoordinateView::Derivative(state) => {
                    let definition = self.facts.derivative_definitions[state.index() as usize]
                        .expect("differentiability preflight proved this derivative defined");
                    let definition = self
                        .source
                        .expression_id(definition as usize)
                        .expect("explicit derivative definition resolves");
                    self.differentiate_order(definition, order, provenance)
                }
                _ => unreachable!("differentiability preflight rejects this coordinate"),
            },
            dae::ExpressionOperation::Unary { operator, operand } => {
                let derivative = self.differentiate_order(operand, order, provenance)?;
                match (operator, derivative) {
                    (_, Derivative::Zero) => Ok(Derivative::Zero),
                    (dae::UnaryOperator::Plus, derivative) => Ok(derivative),
                    (dae::UnaryOperator::Negate, Derivative::Expression(operand)) => self
                        .target
                        .at(provenance)
                        .unary(dae::UnaryOperator::Negate, operand)
                        .map(Derivative::Expression),
                    (dae::UnaryOperator::Not, _) => {
                        unreachable!("differentiability preflight rejects Boolean negation")
                    }
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.differentiate_binary(operator, lhs, rhs, order, provenance)
            }
            _ => unreachable!("differentiability preflight rejects this operation"),
        }
    }

    /// Differentiate an algebraic through the anchor its equality class proves
    /// it equal to, negating when the class proves the opposite sign.
    fn differentiate_equality_class(
        &mut self,
        algebraic: dae::AlgebraicId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let Some((anchor, sign)) = self.facts.equalities.anchor_of(algebraic.index()) else {
            unreachable!("differentiability preflight rejects an unanchored algebraic")
        };
        let EqualityAnchor::State(anchor) = anchor else {
            // A class pinned to a time-invariant value has derivative zero.
            return Ok(Derivative::Zero);
        };
        let dae::VariableIdentity::State(anchor) = self
            .source
            .variable_id(anchor as usize)
            .and_then(|id| self.source.variable(id))
            .expect("equality anchor declaration resolves")
            .identity()
        else {
            unreachable!("an equality anchor state keeps its state role")
        };
        match (sign, self.differentiate_state(anchor, order, provenance)?) {
            (EqualitySign::Same, derivative)
            | (EqualitySign::Opposite, derivative @ Derivative::Zero) => Ok(derivative),
            (EqualitySign::Opposite, Derivative::Expression(anchor)) => self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Negate, anchor)
                .map(Derivative::Expression),
        }
    }

    fn differentiate_state(
        &mut self,
        source_state: dae::StateId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        if let Some(definition) = self.facts.derivative_definitions[source_state.index() as usize] {
            let definition = self
                .source
                .expression_id(definition as usize)
                .expect("explicit derivative definition resolves");
            if order > 1 {
                return self.differentiate_order(definition, order - 1, provenance);
            }
            let definition = self.rebuild(definition)?;
            return self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Plus, definition)
                .map(Derivative::Expression);
        }
        let TargetVariable::State(state) = self.variables[source_state.index() as usize].identity
        else {
            unreachable!("candidate RHS cannot refer to the demoted state")
        };
        self.target
            .at(provenance)
            .coordinate(dae::CoordinateInput::Derivative(state))
            .map(Derivative::Expression)
    }

    fn differentiate_binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'source>,
        rhs: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let lhs_derivative = self.differentiate_order(lhs, order, provenance)?;
        let rhs_derivative = self.differentiate_order(rhs, order, provenance)?;
        match operator {
            dae::BinaryOperator::Add | dae::BinaryOperator::Subtract => {
                self.combine_sum(operator, lhs_derivative, rhs_derivative, provenance)
            }
            dae::BinaryOperator::Multiply if order == 1 => {
                let lhs_value = self.rebuild(lhs)?;
                let rhs_value = self.rebuild(rhs)?;
                let left = self.multiply(lhs_derivative, rhs_value, provenance)?;
                let right = self.multiply(rhs_derivative, lhs_value, provenance)?;
                self.combine_sum(dae::BinaryOperator::Add, left, right, provenance)
            }
            dae::BinaryOperator::Multiply if order == 2 => self.differentiate_second_product(
                lhs,
                rhs,
                lhs_derivative,
                rhs_derivative,
                provenance,
            ),
            dae::BinaryOperator::Divide if order == 1 => {
                let lhs_value = self.rebuild(lhs)?;
                let rhs_value = self.rebuild(rhs)?;
                let left = self.multiply(lhs_derivative, rhs_value, provenance)?;
                let right = self.multiply(rhs_derivative, lhs_value, provenance)?;
                let numerator =
                    self.combine_sum(dae::BinaryOperator::Subtract, left, right, provenance)?;
                let Derivative::Expression(numerator) = numerator else {
                    return Ok(Derivative::Zero);
                };
                let denominator = self.target.at(provenance).binary(
                    dae::BinaryOperator::Multiply,
                    rhs_value,
                    rhs_value,
                )?;
                self.target
                    .at(provenance)
                    .binary(dae::BinaryOperator::Divide, numerator, denominator)
                    .map(Derivative::Expression)
            }
            _ => unreachable!("differentiability preflight rejects this binary operator"),
        }
    }

    fn differentiate_second_product(
        &mut self,
        lhs: dae::ExprId<'source>,
        rhs: dae::ExprId<'source>,
        lhs_second: Derivative<'target>,
        rhs_second: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let lhs_value = self.rebuild(lhs)?;
        let rhs_value = self.rebuild(rhs)?;
        let lhs_first = self.differentiate_order(lhs, 1, provenance)?;
        let rhs_first = self.differentiate_order(rhs, 1, provenance)?;
        let left = self.multiply(lhs_second, rhs_value, provenance)?;
        let right = self.multiply(rhs_second, lhs_value, provenance)?;
        let middle = self.multiply_derivatives(lhs_first, rhs_first, provenance)?;
        let outer = self.combine_sum(dae::BinaryOperator::Add, left, right, provenance)?;
        self.combine_sum(dae::BinaryOperator::Add, outer, middle, provenance)
    }

    fn multiply_derivatives(
        &mut self,
        lhs: Derivative<'target>,
        rhs: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let (Derivative::Expression(lhs), Derivative::Expression(rhs)) = (lhs, rhs) else {
            return Ok(Derivative::Zero);
        };
        let product = self
            .target
            .at(provenance)
            .binary(dae::BinaryOperator::Multiply, lhs, rhs)?;
        let two = self
            .target
            .at(provenance)
            .literal(dae::DaeLiteral::Real(2.0))?;
        self.target
            .at(provenance)
            .binary(dae::BinaryOperator::Multiply, two, product)
            .map(Derivative::Expression)
    }

    pub(super) fn materialize_derivative(
        &mut self,
        derivative: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        match derivative {
            Derivative::Zero => self
                .target
                .at(provenance)
                .literal(dae::DaeLiteral::Real(0.0)),
            Derivative::Expression(expression) => self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Plus, expression),
        }
    }

    fn multiply(
        &mut self,
        derivative: Derivative<'target>,
        value: dae::ExprId<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let Derivative::Expression(derivative) = derivative else {
            return Ok(Derivative::Zero);
        };
        self.target
            .at(provenance)
            .binary(dae::BinaryOperator::Multiply, derivative, value)
            .map(Derivative::Expression)
    }

    fn combine_sum(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: Derivative<'target>,
        rhs: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        match (lhs, rhs) {
            (Derivative::Zero, Derivative::Zero) => Ok(Derivative::Zero),
            (Derivative::Expression(expression), Derivative::Zero) => {
                Ok(Derivative::Expression(expression))
            }
            (Derivative::Zero, Derivative::Expression(expression))
                if operator == dae::BinaryOperator::Add =>
            {
                Ok(Derivative::Expression(expression))
            }
            (Derivative::Zero, Derivative::Expression(expression)) => self
                .target
                .at(provenance)
                .unary(dae::UnaryOperator::Negate, expression)
                .map(Derivative::Expression),
            (Derivative::Expression(lhs), Derivative::Expression(rhs)) => self
                .target
                .at(provenance)
                .binary(operator, lhs, rhs)
                .map(Derivative::Expression),
        }
    }
}

pub(super) enum Derivative<'dae> {
    Zero,
    Expression(dae::ExprId<'dae>),
}
