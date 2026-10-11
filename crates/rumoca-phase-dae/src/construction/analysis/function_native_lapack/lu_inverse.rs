use super::*;

impl LapackInterface<'_> {
    pub(super) fn lu_factorization(self, args: &[Expression]) -> Option<NativeLapackPlan> {
        let [m, n, matrix, lda, pivots, info] = args else {
            return None;
        };
        let [m, n, matrix, lda, pivots, info] =
            [m, n, matrix, lda, pivots, info].map(plain_reference);
        let (matrix, pivots, info) = (matrix?, pivots?, info?);
        let (rows, columns) = self.initialized_real_matrix_output(&matrix)?;
        let valid = self.function.pure
            && self.function.purity_declared
            && self.function.outputs.len() == 3
            && matrix != pivots
            && matrix != info
            && pivots != info
            && self.integer_vector(self.output(&pivots)?, &pivots, rows.min(columns))
            && self.integer_scalar_output(&info)
            && self.integer_extent(m, rows, rows)
            && self.integer_extent(n, columns, columns)
            && self.integer_extent(lda, rows, rows);
        valid.then_some(NativeLapackPlan::LuFactorization {
            matrix,
            pivots,
            info,
        })
    }

    pub(super) fn lu_inverse(self, args: &[Expression]) -> Option<NativeLapackPlan> {
        let [n, matrix, lda, pivots, work, lwork, info] = args else {
            return None;
        };
        let [n, matrix, lda, pivots, work, lwork, info] =
            [n, matrix, lda, pivots, work, lwork, info].map(plain_reference);
        let (matrix, pivots, work, info) = (matrix?, pivots?, work?, info?);
        let (rows, columns) = self.initialized_real_matrix_output(&matrix)?;
        let work_declaration = self.local(&work)?;
        let workspace = self.vector_local(&work)?;
        let valid = self.function.pure
            && self.function.purity_declared
            && rows == columns
            && self.function.outputs.len() == 2
            && matrix != info
            && self.integer_scalar_output(&info)
            && self.integer_vector(self.input(&pivots)?, &pivots, rows)
            && effective_function_scalar_type(self.context.flat, work_declaration)
                == Some(dae::ScalarType::Real)
            && self.integer_extent(n, rows, rows)
            && self.integer_extent(lda, rows, rows)
            && self.integer_extent(lwork, rows, workspace);
        valid.then_some(NativeLapackPlan::LuInverse {
            matrix,
            pivots,
            info,
        })
    }

    fn initialized_real_matrix_output(self, name: &VarName) -> Option<(i64, i64)> {
        let declaration = self.output(name)?;
        let source = plain_reference(declaration.default.as_ref()?)?;
        let input = self.input(&source)?;
        if effective_function_scalar_type(self.context.flat, declaration)
            != Some(dae::ScalarType::Real)
            || effective_function_scalar_type(self.context.flat, input)
                != Some(dae::ScalarType::Real)
            || self.context.shapes.get(name) != self.context.shapes.get(&source)
        {
            return None;
        }
        match self.context.shapes.get(name)?.as_slice() {
            [rows, columns] if *rows > 0 && *columns > 0 => {
                Some((i64::from(*rows), i64::from(*columns)))
            }
            _ => None,
        }
    }

    fn integer_vector(
        self,
        declaration: &rumoca_core::FunctionParam,
        name: &VarName,
        extent: i64,
    ) -> bool {
        effective_function_scalar_type(self.context.flat, declaration)
            == Some(dae::ScalarType::Integer)
            && matches!(self.context.shapes.get(name).map(Vec::as_slice), Some([size]) if i64::from(*size) == extent)
    }

    fn integer_extent(self, name: Option<VarName>, minimum: i64, maximum: i64) -> bool {
        let Some(name) = name else { return false };
        self.local(&name).is_some_and(|declaration| {
            self.is_scalar(&name)
                && effective_function_scalar_type(self.context.flat, declaration)
                    == Some(dae::ScalarType::Integer)
                && self.proven(Some(name.clone()), |value| {
                    minimum <= value && value <= maximum
                })
        })
    }
}
