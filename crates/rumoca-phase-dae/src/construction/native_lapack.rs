//! Bodies of the LAPACK drivers the checked DAE defines (see
//! `analysis::function_native_lapack`), as straight-line arithmetic over the
//! translation-time extents of their operands.

use super::*;

mod least_squares;

/// Define the body of a function whose external clause is a native LAPACK
/// driver.
pub(super) fn lower_native_lapack<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    body: &mut dae::FunctionBody<'dae>,
    plan: &NativeLapackPlan,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let read = |construction: &mut dae::DaeConstruction<'dae>, name: &VarName| match coordinates
        [name]
    {
        Coordinate::FunctionValue(value) => {
            construction.functions(|functions| functions.read(body, value, provenance))
        }
        Coordinate::FunctionParameter(parameter) => construction
            .expressions(|expressions| expressions.at(provenance).function_parameter(parameter)),
        _ => unreachable!("analysis proves LAPACK operands are function values"),
    };
    let assignments = match plan {
        NativeLapackPlan::LinearSolve {
            matrix,
            solution,
            info,
        } => {
            let matrix_value = read(construction, matrix)?;
            let rhs = read(construction, solution)?;
            let (status, solved) = construction.expressions(|expressions| {
                LapackBuilder {
                    expressions,
                    provenance,
                }
                .linear_solve(matrix_value, rhs)
            })?;
            vec![(solution, solved), (info, status)]
        }
        NativeLapackPlan::LeastSquares {
            matrix,
            solution,
            rcond,
            rank,
            info,
        } => {
            let matrix_value = read(construction, matrix)?;
            let rhs = read(construction, solution)?;
            let rcond_value = read(construction, rcond)?;
            let (solved, effective_rank, status) = construction.expressions(|expressions| {
                let mut builder = LapackBuilder {
                    expressions,
                    provenance,
                };
                let (solved, effective_rank) =
                    builder.least_squares(matrix_value, rhs, rcond_value)?;
                // Every argument is proven legal, so the driver reports 0.
                Ok::<_, dae::DaeConstructionError>((solved, effective_rank, builder.integer(0)?))
            })?;
            vec![(solution, solved), (rank, effective_rank), (info, status)]
        }
    };
    for (name, value) in assignments {
        let target = function_value_coordinate(coordinates, name);
        construction.functions(|functions| functions.assign(body, target, value, provenance))?;
    }
    Ok(())
}

/// Straight-line scalar and row arithmetic over operands of translation-time
/// extent.
struct LapackBuilder<'scope, 'storage, 'dae> {
    expressions: &'scope mut dae::Expressions<'storage, 'dae>,
    provenance: dae::DaeProvenance,
}

impl<'dae> LapackBuilder<'_, '_, 'dae> {
    /// LAPACK `dgesv` with one right-hand side: `info` is the first step `k`
    /// at which elimination with partial pivoting meets an exactly zero
    /// pivot column, or 0. When `info = 0` the solution is the linear solve
    /// of the matrix argument; otherwise it keeps the right-hand side, as
    /// dgesv leaves `B` unchanged, and the caller decides what a singular
    /// matrix means.
    fn linear_solve(
        &mut self,
        matrix: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
    ) -> Result<(dae::ExprId<'dae>, dae::ExprId<'dae>), dae::DaeConstructionError> {
        let status = self.zero_pivot_step(matrix)?;
        let solvable = self.is_integer(status, 0)?;
        let identity = self.identity(self.extent(matrix)?)?;
        let regular = self.conditional(solvable, matrix, identity)?;
        let solved = self
            .expressions
            .at(self.provenance)
            .builtin(dae::PureBuiltin::LinearSolve, [regular, rhs])?;
        Ok((status, solved))
    }

    // Straight-line elimination with partial pivoting over a matrix of
    // translation-time extent `n`, kept as `n` row vectors: step `k` takes
    // the first row of largest magnitude in column `k` (LAPACK `idamax`),
    // swaps it into row `k`, and eliminates column `k` below it. A zero
    // largest magnitude is a zero pivot: the step eliminates nothing (its
    // multipliers are zero) and the first such step is the dgesv `info`.
    fn extent(&self, matrix: dae::ExprId<'dae>) -> Result<usize, dae::DaeConstructionError> {
        let ty = self.expressions.value_type(matrix, self.provenance)?;
        Ok(ty.dimensions()[0] as usize)
    }

    fn literal(
        &mut self,
        value: dae::DaeLiteral,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.expressions.at(self.provenance).literal(value)
    }

    fn integer(&mut self, value: usize) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.literal(dae::DaeLiteral::Integer(value as i64))
    }

    fn is_integer(
        &mut self,
        lhs: dae::ExprId<'dae>,
        value: i64,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let rhs = self.literal(dae::DaeLiteral::Integer(value))?;
        self.op(dae::BinaryOperator::Equal, lhs, rhs)
    }

    fn op(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.expressions
            .at(self.provenance)
            .binary(operator, lhs, rhs)
    }

    fn conditional(
        &mut self,
        condition: dae::ExprId<'dae>,
        then: dae::ExprId<'dae>,
        otherwise: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.expressions
            .at(self.provenance)
            .conditional([(condition, then)], otherwise)
    }

    /// `base[index]` for a translation-time 1-based `index`, with the
    /// remaining axes whole.
    fn at_index(
        &mut self,
        base: dae::ExprId<'dae>,
        index: usize,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let expression = self.integer(index)?;
        let provenance = self.provenance;
        let rank = self
            .expressions
            .value_type(base, provenance)?
            .dimensions()
            .len();
        let subscripts = std::iter::once(dae::Subscript::Value {
            expression,
            provenance,
        })
        .chain((1..rank).map(|_| dae::Subscript::Whole { provenance }));
        self.expressions.at(provenance).index(base, subscripts)
    }

    fn identity(&mut self, n: usize) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let rows = (1..=n)
            .map(|row| {
                let elements = (1..=n)
                    .map(|column| {
                        self.literal(dae::DaeLiteral::Real(f64::from(u8::from(row == column))))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                self.expressions.at(self.provenance).array(elements)
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.expressions.at(self.provenance).array(rows)
    }

    /// The first elimination step whose pivot column is exactly zero, or 0.
    fn zero_pivot_step(
        &mut self,
        matrix: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let n = self.extent(matrix)?;
        let mut rows = (1..=n)
            .map(|row| self.at_index(matrix, row))
            .collect::<Result<Vec<_>, _>>()?;
        let mut status = self.integer(0)?;
        let zero = self.literal(dae::DaeLiteral::Real(0.0))?;
        for step in 0..n {
            let (magnitudes, largest) = self.column_magnitudes(&rows[step..], step + 1)?;
            let is_zero = self.op(dae::BinaryOperator::Equal, largest, zero)?;
            let unset = self.is_integer(status, 0)?;
            let first_zero = self.op(dae::BinaryOperator::And, unset, is_zero)?;
            let step_number = self.integer(step + 1)?;
            status = self.conditional(first_zero, step_number, status)?;
            if step + 1 < n {
                let taken = self.first_largest(&magnitudes, largest)?;
                self.eliminate(&mut rows[step..], step + 1, &taken, is_zero)?;
            }
        }
        Ok(status)
    }

    /// The magnitudes of column `column` over `rows` and their largest.
    fn column_magnitudes(
        &mut self,
        rows: &[dae::ExprId<'dae>],
        column: usize,
    ) -> Result<(Vec<dae::ExprId<'dae>>, dae::ExprId<'dae>), dae::DaeConstructionError> {
        let magnitudes = rows
            .iter()
            .map(|row| {
                let element = self.at_index(*row, column)?;
                self.expressions
                    .at(self.provenance)
                    .builtin(dae::PureBuiltin::Abs, [element])
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut largest = magnitudes[0];
        for magnitude in &magnitudes[1..] {
            largest = self
                .expressions
                .at(self.provenance)
                .builtin(dae::PureBuiltin::Max, [largest, *magnitude])?;
        }
        Ok((magnitudes, largest))
    }

    /// Whether each row is the first of largest magnitude (LAPACK `idamax`).
    fn first_largest(
        &mut self,
        magnitudes: &[dae::ExprId<'dae>],
        largest: dae::ExprId<'dae>,
    ) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
        let mut taken = Vec::with_capacity(magnitudes.len());
        let mut earlier: Option<dae::ExprId<'dae>> = None;
        for magnitude in magnitudes {
            let select = self.op(dae::BinaryOperator::Equal, *magnitude, largest)?;
            let (first, seen) = match earlier {
                None => (select, select),
                Some(earlier) => {
                    let not_earlier = self
                        .expressions
                        .at(self.provenance)
                        .unary(dae::UnaryOperator::Not, earlier)?;
                    let first = self.op(dae::BinaryOperator::And, select, not_earlier)?;
                    (first, self.op(dae::BinaryOperator::Or, earlier, select)?)
                }
            };
            earlier = Some(seen);
            taken.push(first);
        }
        Ok(taken)
    }

    /// Swap the taken row into `rows[0]` and eliminate column `column` from
    /// the rows below it. A zero pivot column eliminates nothing: its
    /// leading entries are zero, so dividing them by 1 gives zero multipliers.
    fn eliminate(
        &mut self,
        rows: &mut [dae::ExprId<'dae>],
        column: usize,
        taken: &[dae::ExprId<'dae>],
        is_zero: dae::ExprId<'dae>,
    ) -> Result<(), dae::DaeConstructionError> {
        let pivot_row = self.expressions.at(self.provenance).conditional(
            taken[1..].iter().copied().zip(rows[1..].iter().copied()),
            rows[0],
        )?;
        let pivot = self.at_index(pivot_row, column)?;
        let one = self.literal(dae::DaeLiteral::Real(1.0))?;
        let denominator = self.conditional(is_zero, one, pivot)?;
        for offset in 1..rows.len() {
            // The old pivot row moves to the taken row's place.
            let swapped = self.conditional(taken[offset], rows[0], rows[offset])?;
            let leading = self.at_index(swapped, column)?;
            let factor = self.op(dae::BinaryOperator::Divide, leading, denominator)?;
            let scaled = self.op(dae::BinaryOperator::Multiply, factor, pivot_row)?;
            rows[offset] = self.op(dae::BinaryOperator::Subtract, swapped, scaled)?;
        }
        rows[0] = pivot_row;
        Ok(())
    }
}
