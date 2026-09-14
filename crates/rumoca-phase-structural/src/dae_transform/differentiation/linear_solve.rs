//! Derivatives of a checked aggregate solve on its nonsingular matrix domain.

use super::*;

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(super) fn differentiate_linear_solve(
        &mut self,
        arguments: dae::ExpressionOperands<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        assert!((1..=2).contains(&order));
        let function = self.linear_solve_function(arguments);
        let matrix = arguments.get(0).expect("checked solve matrix");
        let rhs = arguments.get(1).expect("checked solve right-hand side");
        let matrix_value = self.differentiation_value(matrix, provenance)?;
        let rhs_value = self.differentiation_value(rhs, provenance)?;
        let value = self
            .target
            .at(provenance)
            .call(function, 0, [matrix_value, rhs_value])?;
        let mut remainder =
            self.linear_solve_derivative_remainder(matrix, rhs, value, order, provenance)?;
        if order == 2 {
            let matrix_first = self.differentiate_order(matrix, 1, provenance)?;
            if matches!(matrix_first, Derivative::Expression(_)) {
                let first_rhs =
                    self.linear_solve_derivative_remainder(matrix, rhs, value, 1, provenance)?;
                let first =
                    self.solve_derivative_rhs(function, matrix_value, first_rhs, provenance)?;
                let mixed = self.multiply(matrix_first, first, provenance)?;
                let mixed = self.twice(mixed, provenance)?;
                remainder =
                    self.combine_sum(dae::BinaryOperator::Subtract, remainder, mixed, provenance)?;
            }
        }
        self.solve_derivative_rhs(function, matrix_value, remainder, provenance)
    }

    pub(super) fn linear_solve_function(
        &self,
        arguments: dae::ExpressionOperands<'source>,
    ) -> dae::FunctionId<'target> {
        let rhs = self.source.expression(arguments.get(1).unwrap()).unwrap();
        self.auxiliary_functions
            .linear_solve(rhs.value_type().dimensions()[0])
    }

    fn linear_solve_derivative_remainder(
        &mut self,
        matrix: dae::ExprId<'source>,
        rhs: dae::ExprId<'source>,
        value: dae::ExprId<'target>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let matrix_derivative = self.differentiate_order(matrix, order, provenance)?;
        let rhs_derivative = self.differentiate_order(rhs, order, provenance)?;
        let product =
            self.multiply(matrix_derivative, Derivative::Expression(value), provenance)?;
        self.combine_sum(
            dae::BinaryOperator::Subtract,
            rhs_derivative,
            product,
            provenance,
        )
    }

    fn solve_derivative_rhs(
        &mut self,
        function: dae::FunctionId<'target>,
        matrix: dae::ExprId<'target>,
        rhs: Derivative<'target>,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let Derivative::Expression(rhs) = rhs else {
            return Ok(Derivative::Zero);
        };
        self.target
            .at(provenance)
            .call(function, 0, [matrix, rhs])
            .map(Derivative::Expression)
    }
}
