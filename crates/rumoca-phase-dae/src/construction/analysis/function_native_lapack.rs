//! LAPACK routines whose foreign bodies the checked DAE defines.
//!
//! MLS §12.9 gives an external function the semantics of its foreign body,
//! called after the function's protected and output components are
//! initialized from their declaration equations (§12.4.4, §12.9.1). The
//! Solve runtime executes only programs the checked DAE defines, so a
//! foreign body has no executable meaning there unless the DAE defines it.
//! Two FORTRAN 77 LAPACK drivers are defined:
//!
//! - `dgesv` with one right-hand side (SPEC_0040 DAE-C26). For a square
//!   `n`-by-`n` matrix `A` with leading dimension `max(1, n)` and one
//!   right-hand side `B` of extent `n`, `dgesv` factors `A` with partial
//!   pivoting and sets `info` to the first step whose pivot is exactly zero,
//!   leaving `B` unchanged, or sets `info = 0` and overwrites `B` with the
//!   solution of `A*X = B`.
//! - `dgelsy` with one right-hand side (SPEC_0040 DAE-C29). For an
//!   `m`-by-`n` matrix `A` and a right-hand side `B` of extent at least
//!   `max(m, n)`, `dgelsy` overwrites `B` with the minimum-norm solution of
//!   the least squares problem through a QR factorization with column
//!   pivoting, an incremental condition estimate of the effective rank
//!   against `rcond`, and a complete orthogonal factorization of the
//!   leading rank rows; it returns that `rank` and `info = 0`.
//!
//! The interface is proven from the declaration, never from spelling alone:
//! the FORTRAN 77 entry point, plain arguments, a proven matrix, a vector
//! output initialized to the right-hand side, Integer status outputs, and
//! translation-time dimension and workspace arguments that the driver
//! accepts. Anything else keeps the ordinary external interface analysis.

use super::*;

/// The LAPACK driver a function's external clause names, with the function
/// values that carry its operands.
pub(in crate::construction) enum NativeLapackPlan {
    /// `dgesv` with one right-hand side.
    LinearSolve {
        matrix: VarName,
        solution: VarName,
        info: VarName,
    },
    /// `dgelsy` with one right-hand side.
    LeastSquares {
        matrix: VarName,
        solution: VarName,
        rcond: VarName,
        rank: VarName,
        info: VarName,
    },
}

pub(super) fn native_lapack_plan(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Option<FunctionPlan> {
    let external = function.external.as_ref()?;
    let language = dae::ExternalLanguage::from_declared(&external.language)?;
    if language != dae::ExternalLanguage::Fortran77 || external.output_name.is_some() {
        return None;
    }
    let interface = LapackInterface { function, context };
    let plan = match external.function_name.as_deref()? {
        "dgesv" => interface.linear_solve(&external.args)?,
        "dgelsy" => interface.least_squares(&external.args)?,
        _ => return None,
    };
    Some(FunctionPlan::NativeLapack(plan))
}

#[derive(Clone, Copy)]
struct LapackInterface<'a> {
    function: &'a rumoca_core::Function,
    context: FunctionValidationContext<'a>,
}

impl<'a> LapackInterface<'a> {
    fn linear_solve(self, args: &[Expression]) -> Option<NativeLapackPlan> {
        let [n, nrhs, matrix, lda, pivots, solution, ldb, info] = args else {
            return None;
        };
        let [n, nrhs, matrix, lda, pivots, solution, ldb, info] =
            [n, nrhs, matrix, lda, pivots, solution, ldb, info].map(plain_reference);
        let (matrix, solution, info, pivots) = (matrix?, solution?, info?, pivots?);
        let (rows, columns) = self.matrix_extent(&matrix)?;
        let leading = rows.max(1);
        let interface_holds = rows == columns
            && self.function.outputs.len() == 2
            && solution != info
            && self.initialized_vector_output(&solution)? == rows
            && self.integer_scalar_output(&info)
            && self.local(&pivots).is_some()
            && self.proven(n, |value| value == rows)
            && self.proven(nrhs, |value| value == 1)
            && self.proven(lda, |value| value >= leading)
            && self.proven(ldb, |value| value >= leading);
        interface_holds.then_some(NativeLapackPlan::LinearSolve {
            matrix,
            solution,
            info,
        })
    }

    fn least_squares(self, args: &[Expression]) -> Option<NativeLapackPlan> {
        let [
            m,
            n,
            nrhs,
            matrix,
            lda,
            solution,
            ldb,
            pivots,
            rcond,
            rank,
            work,
            lwork,
            info,
        ] = args
        else {
            return None;
        };
        let [
            m,
            n,
            nrhs,
            matrix,
            lda,
            solution,
            ldb,
            pivots,
            rcond,
            rank,
            work,
            lwork,
            info,
        ] = [
            m, n, nrhs, matrix, lda, solution, ldb, pivots, rcond, rank, work, lwork, info,
        ]
        .map(plain_reference);
        let (matrix, solution, pivots, rcond, rank, work, info) =
            (matrix?, solution?, pivots?, rcond?, rank?, work?, info?);
        let (rows, columns) = self.matrix_extent(&matrix)?;
        let extent = rows.max(columns);
        let smallest = rows.min(columns);
        let workspace = (smallest + 3 * columns + 1).max(2 * smallest + 1);
        let storage = self.initialized_vector_output(&solution)?;
        let work_extent = self.vector_local(&work)?;
        let interface_holds = self.function.outputs.len() == 3
            && solution != rank
            && solution != info
            && rank != info
            && storage >= extent
            && self.integer_scalar_output(&rank)
            && self.integer_scalar_output(&info)
            && self.unpivoted(&pivots, columns)
            && self.real_scalar_operand(&rcond)
            && self.proven(m, |value| value == rows)
            && self.proven(n, |value| value == columns)
            && self.proven(nrhs, |value| value == 1)
            && self.proven(lda, |value| value >= rows.max(1))
            && self.proven(ldb, |value| value >= extent.max(1) && value <= storage)
            && self.proven(lwork, |value| value >= workspace && value <= work_extent);
        interface_holds.then_some(NativeLapackPlan::LeastSquares {
            matrix,
            solution,
            rcond,
            rank,
            info,
        })
    }

    /// The extents of a nonempty matrix operand that holds a value before the
    /// foreign call: an input, or a protected local with a declaration
    /// equation.
    fn matrix_extent(self, matrix: &VarName) -> Option<(i64, i64)> {
        let initialized = self.input(matrix).is_some()
            || self
                .local(matrix)
                .is_some_and(|local| local.default.is_some());
        match self.context.shapes.get(matrix)?.as_slice() {
            [rows, columns] if initialized && *rows > 0 && *columns > 0 => {
                Some((i64::from(*rows), i64::from(*columns)))
            }
            _ => None,
        }
    }

    /// The extent of a vector output whose declaration equation initializes
    /// it to the right-hand side.
    fn initialized_vector_output(self, name: &VarName) -> Option<i64> {
        self.output(name)?.default.as_ref()?;
        match self.context.shapes.get(name)?.as_slice() {
            [extent] => Some(i64::from(*extent)),
            _ => None,
        }
    }

    fn vector_local(self, name: &VarName) -> Option<i64> {
        self.local(name)?;
        match self.context.shapes.get(name)?.as_slice() {
            [extent] => Some(i64::from(*extent)),
            _ => None,
        }
    }

    fn integer_scalar_output(self, name: &VarName) -> bool {
        self.output(name).is_some_and(|output| {
            self.is_scalar(name)
                && effective_function_scalar_type(self.context.flat, output)
                    == Some(dae::ScalarType::Integer)
        })
    }

    /// A Real scalar input, or a protected local with a declaration equation.
    fn real_scalar_operand(self, name: &VarName) -> bool {
        let value = self
            .input(name)
            .or_else(|| self.local(name).filter(|local| local.default.is_some()));
        value.is_some_and(|value| {
            self.is_scalar(name)
                && effective_function_scalar_type(self.context.flat, value)
                    == Some(dae::ScalarType::Real)
        })
    }

    /// A pivot local of `columns` entries whose declaration equation is
    /// `zeros(...)`, so every column is free to move (`JPVT(i) = 0`).
    fn unpivoted(self, name: &VarName, columns: i64) -> bool {
        let zeros = self.local(name).is_some_and(|local| {
            matches!(
                local.default,
                Some(Expression::BuiltinCall {
                    function: BuiltinFunction::Zeros,
                    ..
                })
            )
        });
        zeros && self.vector_local(name) == Some(columns)
    }

    fn is_scalar(self, name: &VarName) -> bool {
        self.context.shapes.get(name).is_some_and(Vec::is_empty)
    }

    fn proven(self, name: Option<VarName>, accept: impl Fn(i64) -> bool) -> bool {
        name.and_then(|name| self.translation_integer(&name))
            .is_some_and(accept)
    }

    /// The translation-time value of an Integer argument: a protected local
    /// whose declaration equation is a proven extent expression.
    fn translation_integer(self, name: &VarName) -> Option<i64> {
        evaluate_shape_integer(self.local(name)?.default.as_ref()?, self.context.shapes).ok()
    }

    fn input(self, name: &VarName) -> Option<&'a rumoca_core::FunctionParam> {
        find_value(&self.function.inputs, name)
    }

    fn output(self, name: &VarName) -> Option<&'a rumoca_core::FunctionParam> {
        find_value(&self.function.outputs, name)
    }

    fn local(self, name: &VarName) -> Option<&'a rumoca_core::FunctionParam> {
        find_value(&self.function.locals, name)
    }
}

fn find_value<'a>(
    values: &'a [rumoca_core::FunctionParam],
    name: &VarName,
) -> Option<&'a rumoca_core::FunctionParam> {
    values.iter().find(|value| value.name == name.as_str())
}

fn plain_reference(expression: &Expression) -> Option<VarName> {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.var_name().clone()),
        _ => None,
    }
}
