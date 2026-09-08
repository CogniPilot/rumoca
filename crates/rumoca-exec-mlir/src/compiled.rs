use crate::error::{MlirAbiArgument, MlirError};
use rumoca_ir_solve::SolveModel;
use tempfile::TempDir;

// MLIR lowers each memref<?xf64> arg to 5 LLVM params:
//   (alloc_ptr, aligned_ptr, offset: i64, size: i64, stride: i64)
// @eval_derivative(y, p, t, out):  5 + 5 + 1 + 5 = 16 LLVM params.
type EvalFn = unsafe extern "C" fn(
    *const f64,
    *const f64,
    i64,
    i64,
    i64, // y
    *const f64,
    *const f64,
    i64,
    i64,
    i64, // p
    f64, // t
    *mut f64,
    *mut f64,
    i64,
    i64,
    i64, // out
);

// @eval_jacobian_v(y, p, seed, t, out): 5 + 5 + 5 + 1 + 5 = 21 LLVM params.
type JvpFn = unsafe extern "C" fn(
    *const f64,
    *const f64,
    i64,
    i64,
    i64, // y
    *const f64,
    *const f64,
    i64,
    i64,
    i64, // p
    *const f64,
    *const f64,
    i64,
    i64,
    i64, // seed
    f64, // t
    *mut f64,
    *mut f64,
    i64,
    i64,
    i64, // out
);

/// Exact slice cardinalities of one MLIR residual artifact.
///
/// This contract is projected once from the sealed [`SolveModel`] used to
/// render the artifact and is retained beside the loaded function pointers.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MlirResidualAbi {
    y: usize,
    p: usize,
    derivative_output: usize,
    implicit_output: usize,
    seed: usize,
}

impl MlirResidualAbi {
    pub(crate) fn from_model(model: &SolveModel) -> Self {
        let y = model.initial_y().len();
        Self {
            y,
            p: model.parameters().len(),
            derivative_output: model.state_scalar_count(),
            implicit_output: model.problem().continuous().implicit_row_targets().len(),
            seed: y,
        }
    }

    #[cfg(test)]
    pub(crate) const fn fixture(y: usize, derivative_output: usize) -> Self {
        Self {
            y,
            p: 0,
            derivative_output,
            implicit_output: 0,
            seed: y,
        }
    }

    #[must_use]
    pub const fn y_count(self) -> usize {
        self.y
    }

    #[must_use]
    pub const fn parameter_count(self) -> usize {
        self.p
    }

    #[must_use]
    pub const fn derivative_output_count(self) -> usize {
        self.derivative_output
    }

    #[must_use]
    pub const fn implicit_output_count(self) -> usize {
        self.implicit_output
    }

    #[must_use]
    pub const fn seed_count(self) -> usize {
        self.seed
    }
}

/// A compiled residual function loaded from an MLIR-generated shared library.
///
/// The underlying `.so` is kept alive in a `TempDir` that lives as long as
/// this struct. On Linux the kernel keeps the mapping alive even after the
/// file is deleted, but we hold the `TempDir` as a belt-and-suspenders
/// measure and to enable inspection in debug builds.
pub struct CompiledMlirResidual {
    _lib: libloading::Library,
    _tmpdir: TempDir,
    eval_fn: EvalFn,
    /// Optional implicit residual function (`@eval_implicit_rhs`).
    implicit_fn: Option<EvalFn>,
    /// Optional Jacobian-vector product function (`@eval_jacobian_v`).
    jvp_fn: Option<JvpFn>,
    abi: MlirResidualAbi,
}

impl CompiledMlirResidual {
    pub(crate) fn new(
        lib: libloading::Library,
        tmpdir: TempDir,
        eval_fn: EvalFn,
        implicit_fn: Option<EvalFn>,
        jvp_fn: Option<JvpFn>,
        abi: MlirResidualAbi,
    ) -> Self {
        Self {
            _lib: lib,
            _tmpdir: tmpdir,
            eval_fn,
            implicit_fn,
            jvp_fn,
            abi,
        }
    }

    /// Evaluate the compiled derivative RHS: `out[i] = f_i(y, p, t)`.
    pub fn call(&self, y: &[f64], p: &[f64], t: f64, out: &mut [f64]) -> Result<(), MlirError> {
        let checked = CheckedEvalCall::new(
            "eval_derivative",
            self.abi,
            self.abi.derivative_output,
            y,
            p,
            out,
        )?;
        unsafe { invoke_eval(self.eval_fn, checked, t) };
        Ok(())
    }

    /// Evaluate the implicit residual: `out[i] = g_i(y, p, t)`.
    ///
    /// Returns `None` if the library was compiled without an `@eval_implicit_rhs` function
    /// (i.e. `implicit_rhs` was empty).
    pub fn call_implicit_rhs(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Option<Result<(), MlirError>> {
        let f = self.implicit_fn?;
        let checked = match CheckedEvalCall::new(
            "eval_implicit_rhs",
            self.abi,
            self.abi.implicit_output,
            y,
            p,
            out,
        ) {
            Ok(checked) => checked,
            Err(error) => return Some(Err(error)),
        };
        unsafe { invoke_eval(f, checked, t) };
        Some(Ok(()))
    }

    /// Evaluate the Jacobian-vector product: `out[i] = sum_j (dg_i/dy_j) * seed[j]`.
    ///
    /// Returns `None` if the library was compiled without an `@eval_jacobian_v` function.
    pub fn call_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        seed: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Option<Result<(), MlirError>> {
        let f = self.jvp_fn?;
        let checked = match CheckedJvpCall::new(self.abi, y, p, seed, out) {
            Ok(checked) => checked,
            Err(error) => return Some(Err(error)),
        };
        unsafe { invoke_jvp(f, checked, t) };
        Some(Ok(()))
    }

    pub fn rows(&self) -> usize {
        self.abi.derivative_output
    }

    pub fn implicit_rows(&self) -> usize {
        self.abi.implicit_output
    }

    #[must_use]
    pub const fn abi(&self) -> MlirResidualAbi {
        self.abi
    }
}

pub(crate) fn validate_abi_len(
    function: &'static str,
    argument: MlirAbiArgument,
    expected: usize,
    actual: usize,
) -> Result<(), MlirError> {
    if actual != expected {
        return Err(MlirError::AbiLengthMismatch {
            function,
            argument,
            expected,
            actual,
        });
    }
    Ok(())
}

#[derive(Debug)]
struct CheckedEvalCall<'a> {
    y: &'a [f64],
    p: &'a [f64],
    out: &'a mut [f64],
}

impl<'a> CheckedEvalCall<'a> {
    fn new(
        function: &'static str,
        abi: MlirResidualAbi,
        output_count: usize,
        y: &'a [f64],
        p: &'a [f64],
        out: &'a mut [f64],
    ) -> Result<Self, MlirError> {
        validate_abi_len(function, MlirAbiArgument::Y, abi.y, y.len())?;
        validate_abi_len(function, MlirAbiArgument::P, abi.p, p.len())?;
        validate_abi_len(function, MlirAbiArgument::Output, output_count, out.len())?;
        Ok(Self { y, p, out })
    }
}

#[derive(Debug)]
struct CheckedJvpCall<'a> {
    y: &'a [f64],
    p: &'a [f64],
    seed: &'a [f64],
    out: &'a mut [f64],
}

impl<'a> CheckedJvpCall<'a> {
    fn new(
        abi: MlirResidualAbi,
        y: &'a [f64],
        p: &'a [f64],
        seed: &'a [f64],
        out: &'a mut [f64],
    ) -> Result<Self, MlirError> {
        const FUNCTION: &str = "eval_jacobian_v";
        validate_abi_len(FUNCTION, MlirAbiArgument::Y, abi.y, y.len())?;
        validate_abi_len(FUNCTION, MlirAbiArgument::P, abi.p, p.len())?;
        validate_abi_len(FUNCTION, MlirAbiArgument::Seed, abi.seed, seed.len())?;
        validate_abi_len(
            FUNCTION,
            MlirAbiArgument::Output,
            abi.implicit_output,
            out.len(),
        )?;
        Ok(Self { y, p, seed, out })
    }
}

unsafe fn invoke_eval(f: EvalFn, checked: CheckedEvalCall<'_>, t: f64) {
    let CheckedEvalCall { y, p, out } = checked;
    unsafe {
        f(
            y.as_ptr(),
            y.as_ptr(),
            0,
            y.len() as i64,
            1,
            p.as_ptr(),
            p.as_ptr(),
            0,
            p.len() as i64,
            1,
            t,
            out.as_mut_ptr(),
            out.as_mut_ptr(),
            0,
            out.len() as i64,
            1,
        );
    }
}

unsafe fn invoke_jvp(f: JvpFn, checked: CheckedJvpCall<'_>, t: f64) {
    let CheckedJvpCall { y, p, seed, out } = checked;
    unsafe {
        f(
            y.as_ptr(),
            y.as_ptr(),
            0,
            y.len() as i64,
            1,
            p.as_ptr(),
            p.as_ptr(),
            0,
            p.len() as i64,
            1,
            seed.as_ptr(),
            seed.as_ptr(),
            0,
            seed.len() as i64,
            1,
            t,
            out.as_mut_ptr(),
            out.as_mut_ptr(),
            0,
            out.len() as i64,
            1,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::{CheckedEvalCall, CheckedJvpCall, MlirResidualAbi};
    use crate::error::{MlirAbiArgument, MlirError};

    const ABI: MlirResidualAbi = MlirResidualAbi {
        y: 3,
        p: 2,
        derivative_output: 1,
        implicit_output: 2,
        seed: 3,
    };

    fn assert_abi_error(
        error: MlirError,
        function: &'static str,
        argument: MlirAbiArgument,
        expected: usize,
        actual: usize,
    ) {
        assert!(matches!(
            error,
            MlirError::AbiLengthMismatch {
                function: actual_function,
                argument: actual_argument,
                expected: actual_expected,
                actual: actual_actual,
            } if actual_function == function
                && actual_argument == argument
                && actual_expected == expected
                && actual_actual == actual
        ));
    }

    #[test]
    fn checked_eval_view_accepts_the_exact_retained_abi() {
        let mut out = [0.0];
        CheckedEvalCall::new("eval_derivative", ABI, 1, &[0.0; 3], &[0.0; 2], &mut out)
            .expect("exact Y/P/output cardinalities issue a checked FFI view");
    }

    #[test]
    fn checked_eval_view_refuses_missing_and_surplus_y_p_and_output() {
        for (argument, expected, actual, y_count, p_count, out_count) in [
            (MlirAbiArgument::Y, 3, 2, 2, 2, 1),
            (MlirAbiArgument::Y, 3, 4, 4, 2, 1),
            (MlirAbiArgument::P, 2, 1, 3, 1, 1),
            (MlirAbiArgument::P, 2, 3, 3, 3, 1),
            (MlirAbiArgument::Output, 1, 0, 3, 2, 0),
            (MlirAbiArgument::Output, 1, 2, 3, 2, 2),
        ] {
            let y = vec![0.0; y_count];
            let p = vec![0.0; p_count];
            let mut out = vec![0.0; out_count];
            let error = CheckedEvalCall::new("eval_derivative", ABI, 1, &y, &p, &mut out)
                .expect_err("a mismatched slice cannot reach the unsafe FFI boundary");
            assert_abi_error(error, "eval_derivative", argument, expected, actual);
        }
    }

    #[test]
    fn checked_jvp_view_requires_one_seed_per_y_scalar() {
        for seed_count in [2, 4] {
            let mut out = [0.0; 2];
            let seed = vec![0.0; seed_count];
            let error = CheckedJvpCall::new(ABI, &[0.0; 3], &[0.0; 2], &seed, &mut out)
                .expect_err("JVP seed cardinality must equal the retained Y cardinality");
            assert_abi_error(
                error,
                "eval_jacobian_v",
                MlirAbiArgument::Seed,
                3,
                seed_count,
            );
        }
    }
}
