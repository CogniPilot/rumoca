//! LAPACK `dgesv` with one right-hand side as a checked linear solve.
//!
//! MLS §12.9 gives an external function the semantics of its foreign body,
//! called after the function's protected and output components are
//! initialized from their declaration equations (§12.4.4, §12.9.1). The
//! Solve runtime executes only programs the checked DAE defines, so a
//! foreign body has no executable meaning there unless the DAE defines it;
//! LAPACK `dgesv` with one right-hand side is defined here. For a square
//! `n`-by-`n` matrix `A` in column-major storage with leading dimension
//! `max(1, n)` and one right-hand side `B` of extent `n`, `dgesv` factors `A`
//! with partial pivoting and sets `info` to the first step whose pivot is
//! exactly zero, leaving `B` unchanged, or sets `info = 0` and overwrites `B`
//! with the solution of `A*X = B`. The body computes the same `info` by
//! elimination with partial pivoting over the translation-time extent and,
//! when it is 0, the solution with the DAE `LinearSolve` builtin; a caller
//! receives `info` and decides what a singular matrix means.
//!
//! The interface is proven from the declaration, never from spelling: the
//! FORTRAN 77 entry point `dgesv`, eight plain arguments, a proven square
//! matrix, a vector output initialized to the right-hand side, an Integer
//! `info` output, and translation-time values `n`, `nrhs = 1`,
//! `lda >= max(1, n)`, `ldb >= max(1, n)`. Anything else keeps the ordinary
//! external interface analysis.

use super::*;

pub(super) fn native_linear_solve_plan(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Option<FunctionPlan> {
    let external = function.external.as_ref()?;
    let language = dae::ExternalLanguage::from_declared(&external.language)?;
    if language != dae::ExternalLanguage::Fortran77
        || external.function_name.as_deref() != Some("dgesv")
        || external.output_name.is_some()
    {
        return None;
    }
    let [n, nrhs, matrix, lda, pivots, solution, ldb, info] = external.args.as_slice() else {
        return None;
    };
    let [n, nrhs, matrix, lda, pivots, solution, ldb, info] =
        [n, nrhs, matrix, lda, pivots, solution, ldb, info].map(plain_reference);
    let (matrix, solution, info, pivots) = (matrix?, solution?, info?, pivots?);
    let rows = match context.shapes.get(&matrix)?.as_slice() {
        [rows, columns] if rows == columns && *rows > 0 => i64::from(*rows),
        _ => return None,
    };
    let output = |name: &VarName| {
        function
            .outputs
            .iter()
            .find(|output| output.name == name.as_str())
    };
    let solution_output = output(&solution)?;
    let info_output = output(&info)?;
    let solution_is_rhs = solution_output.default.is_some()
        && context.shapes.get(&solution)?.as_slice() == [u32::try_from(rows).ok()?];
    let info_is_integer = context.shapes.get(&info)?.is_empty()
        && effective_function_scalar_type(context.flat, info_output)
            == Some(dae::ScalarType::Integer);
    let pivots_are_local = function
        .locals
        .iter()
        .any(|local| local.name == pivots.as_str());
    let matrix_is_initialized = function
        .inputs
        .iter()
        .any(|input| input.name == matrix.as_str())
        || function
            .locals
            .iter()
            .any(|local| local.name == matrix.as_str() && local.default.is_some());
    let leading = rows.max(1);
    let proven = |name: Option<VarName>, accept: &dyn Fn(i64) -> bool| {
        name.and_then(|name| translation_integer(function, context, &name))
            .is_some_and(accept)
    };
    let interface_holds = function.outputs.len() == 2
        && solution != info
        && solution_is_rhs
        && info_is_integer
        && pivots_are_local
        && matrix_is_initialized
        && proven(n, &|value| value == rows)
        && proven(nrhs, &|value| value == 1)
        && proven(lda, &|value| value >= leading)
        && proven(ldb, &|value| value >= leading);
    interface_holds.then_some(FunctionPlan::NativeLinearSolve {
        matrix,
        solution,
        info,
    })
}

fn plain_reference(expression: &Expression) -> Option<VarName> {
    match expression {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.var_name().clone()),
        _ => None,
    }
}

/// The translation-time value of an Integer argument: a protected local whose
/// declaration equation is a proven extent expression, or a literal.
fn translation_integer(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
    name: &VarName,
) -> Option<i64> {
    let local = function
        .locals
        .iter()
        .find(|local| local.name == name.as_str())?;
    evaluate_shape_integer(local.default.as_ref()?, context.shapes).ok()
}
