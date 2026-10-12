//! DAE-C34: retained factors and pivots, followed by triangular solves.

use super::*;

impl<'dae> LapackBuilder<'_, '_, 'dae> {
    pub(super) fn lu_factorization(
        &mut self,
        matrix: dae::ExprId<'dae>,
    ) -> Result<(dae::ExprId<'dae>, dae::ExprId<'dae>, dae::ExprId<'dae>), dae::DaeConstructionError>
    {
        let dimensions = self.expressions.value_type(matrix, self.provenance)?;
        let (height, width) = (
            dimensions.dimensions()[0] as usize,
            dimensions.dimensions()[1] as usize,
        );
        let mut rows = (1..=height)
            .map(|row| self.at_index(matrix, row))
            .collect::<Result<Vec<_>, _>>()?;
        let mut pivots = Vec::with_capacity(height.min(width));
        let mut status = self.integer(0)?;
        let zero = self.literal(dae::DaeLiteral::Real(0.0))?;
        for step in 0..height.min(width) {
            let (magnitudes, largest) = self.column_magnitudes(&rows[step..], step + 1)?;
            let is_zero = self.op(dae::BinaryOperator::Equal, largest, zero)?;
            let unset = self.is_integer(status, 0)?;
            let first_zero = self.op(dae::BinaryOperator::And, unset, is_zero)?;
            let number = self.integer(step + 1)?;
            status = self.conditional(first_zero, number, status)?;
            let taken = self.first_largest(&magnitudes, largest)?;
            let mut selected = number;
            for (offset, condition) in taken.iter().enumerate().skip(1) {
                let row = self.integer(step + offset + 1)?;
                selected = self.conditional(*condition, row, selected)?;
            }
            pivots.push(selected);
            self.swap_selected_row(&mut rows[step..], &taken)?;
            self.store_lu_multipliers(&mut rows[step..], step + 1, width, is_zero)?;
        }
        let factors = self.expressions.at(self.provenance).array(rows)?;
        let pivots = self.expressions.at(self.provenance).array(pivots)?;
        Ok((factors, pivots, status))
    }

    fn store_lu_multipliers(
        &mut self,
        rows: &mut [dae::ExprId<'dae>],
        column: usize,
        width: usize,
        is_zero: dae::ExprId<'dae>,
    ) -> Result<(), dae::DaeConstructionError> {
        let pivot_row = rows[0];
        let pivot = self.at_index(pivot_row, column)?;
        let one = self.literal(dae::DaeLiteral::Real(1.0))?;
        let denominator = self.conditional(is_zero, one, pivot)?;
        let selected = self.integer(column)?;
        let trailing = if column < width {
            let first = self.integer(column + 1)?;
            let last = self.integer(width)?;
            let expression = self
                .expressions
                .at(self.provenance)
                .range(first, None, last)?;
            Some(dae::Subscript::Slice {
                expression,
                provenance: self.provenance,
            })
        } else {
            None
        };
        for row in rows.iter_mut().skip(1) {
            let leading = self.at_index(*row, column)?;
            let factor = self.op(dae::BinaryOperator::Divide, leading, denominator)?;
            if let Some(trailing) = trailing {
                let upper = self
                    .expressions
                    .at(self.provenance)
                    .index(pivot_row, [trailing])?;
                let remaining = self
                    .expressions
                    .at(self.provenance)
                    .index(*row, [trailing])?;
                let scaled = self.op(dae::BinaryOperator::Multiply, factor, upper)?;
                let updated = self.op(dae::BinaryOperator::Subtract, remaining, scaled)?;
                *row =
                    self.expressions
                        .at(self.provenance)
                        .array_update(*row, updated, [trailing])?;
            }
            *row = self.expressions.at(self.provenance).array_update(
                *row,
                factor,
                [dae::Subscript::Value {
                    expression: selected,
                    provenance: self.provenance,
                }],
            )?;
        }
        Ok(())
    }

    pub(super) fn lu_inverse(
        &mut self,
        factors: dae::ExprId<'dae>,
        pivots: dae::ExprId<'dae>,
    ) -> Result<(dae::ExprId<'dae>, dae::ExprId<'dae>, dae::ExprId<'dae>), dae::DaeConstructionError>
    {
        let n = self.extent(factors)?;
        let (coefficients, status) = self.lu_coefficients(factors, n)?;
        let valid = self.valid_pivots(pivots, n)?;
        let nonsingular = self.is_integer(status, 0)?;
        let singular = self
            .expressions
            .at(self.provenance)
            .unary(dae::UnaryOperator::Not, nonsingular)?;
        let valid = self.op(dae::BinaryOperator::Or, singular, valid)?;
        let identity = self.identity(n)?;
        let permutation = self.apply_pivots(identity, pivots, n)?;
        let mut solution = (1..=n)
            .map(|row| self.at_index(permutation, row))
            .collect::<Result<Vec<_>, _>>()?;
        self.forward_unit_lower(&coefficients, &mut solution)?;
        self.back_substitute(&coefficients, n, &mut solution)?;
        let inverse = self.expressions.at(self.provenance).array(solution)?;
        let inverse = self.conditional(nonsingular, inverse, factors)?;
        Ok((inverse, status, valid))
    }

    fn lu_coefficients(
        &mut self,
        factors: dae::ExprId<'dae>,
        n: usize,
    ) -> Result<(Vec<Vec<dae::ExprId<'dae>>>, dae::ExprId<'dae>), dae::DaeConstructionError> {
        let zero = self.literal(dae::DaeLiteral::Real(0.0))?;
        let mut coefficients = Vec::with_capacity(n);
        let mut status = self.integer(0)?;
        for row in 1..=n {
            let vector = self.at_index(factors, row)?;
            let cells = (1..=n)
                .map(|column| self.at_index(vector, column))
                .collect::<Result<Vec<_>, _>>()?;
            let is_zero = self.op(dae::BinaryOperator::Equal, cells[row - 1], zero)?;
            let unset = self.is_integer(status, 0)?;
            let first = self.op(dae::BinaryOperator::And, unset, is_zero)?;
            let number = self.integer(row)?;
            status = self.conditional(first, number, status)?;
            coefficients.push(cells);
        }
        Ok((coefficients, status))
    }

    fn forward_unit_lower(
        &mut self,
        coefficients: &[Vec<dae::ExprId<'dae>>],
        solution: &mut [dae::ExprId<'dae>],
    ) -> Result<(), dae::DaeConstructionError> {
        for pivot in 0..solution.len() {
            let settled = solution[pivot];
            for row in pivot + 1..solution.len() {
                let product = self.op(
                    dae::BinaryOperator::Multiply,
                    settled,
                    coefficients[row][pivot],
                )?;
                solution[row] = self.op(dae::BinaryOperator::Subtract, solution[row], product)?;
            }
        }
        Ok(())
    }

    fn valid_pivots(
        &mut self,
        pivots: dae::ExprId<'dae>,
        n: usize,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let one = self.integer(1)?;
        let last = self.integer(n)?;
        let mut valid = self.literal(dae::DaeLiteral::Boolean(true))?;
        for step in 1..n {
            let pivot = self.at_index(pivots, step)?;
            let low = self.op(dae::BinaryOperator::GreaterEqual, pivot, one)?;
            let high = self.op(dae::BinaryOperator::LessEqual, pivot, last)?;
            let in_range = self.op(dae::BinaryOperator::And, low, high)?;
            valid = self.op(dae::BinaryOperator::And, valid, in_range)?;
        }
        Ok(valid)
    }

    fn apply_pivots(
        &mut self,
        basis: dae::ExprId<'dae>,
        pivots: dae::ExprId<'dae>,
        n: usize,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let mut values = (1..=n)
            .map(|i| self.at_index(basis, i))
            .collect::<Result<Vec<_>, _>>()?;
        for step in 0..n.saturating_sub(1) {
            let pivot = self.at_index(pivots, step + 1)?;
            let old = values[step];
            let mut selected = old;
            for (offset, value) in values.iter().enumerate() {
                let at = self.is_integer(pivot, (offset + 1) as i64)?;
                selected = self.conditional(at, *value, selected)?;
            }
            for (offset, value) in values
                .iter_mut()
                .enumerate()
                .filter(|(offset, _)| *offset != step)
            {
                let at = self.is_integer(pivot, (offset + 1) as i64)?;
                *value = self.conditional(at, old, *value)?;
            }
            values[step] = selected;
        }
        self.expressions.at(self.provenance).array(values)
    }
}
