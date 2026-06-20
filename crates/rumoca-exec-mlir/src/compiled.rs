use crate::error::MlirError;
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
    rows: usize,
    /// Row count for the implicit residual (may differ from `rows`).
    implicit_rows: usize,
}

impl CompiledMlirResidual {
    pub(crate) fn new(
        lib: libloading::Library,
        tmpdir: TempDir,
        eval_fn: EvalFn,
        implicit_fn: Option<EvalFn>,
        jvp_fn: Option<JvpFn>,
        rows: usize,
        implicit_rows: usize,
    ) -> Self {
        Self {
            _lib: lib,
            _tmpdir: tmpdir,
            eval_fn,
            implicit_fn,
            jvp_fn,
            rows,
            implicit_rows,
        }
    }

    /// Evaluate the compiled derivative RHS: `out[i] = f_i(y, p, t)`.
    pub fn call(&self, y: &[f64], p: &[f64], t: f64, out: &mut [f64]) -> Result<(), MlirError> {
        validate_output_len("eval_derivative", self.rows, out.len())?;
        unsafe {
            (self.eval_fn)(
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
        if let Err(err) = validate_output_len("eval_implicit_rhs", self.implicit_rows, out.len()) {
            return Some(Err(err));
        }
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
        if let Err(err) = validate_output_len("eval_jacobian_v", self.implicit_rows, out.len()) {
            return Some(Err(err));
        }
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
        Some(Ok(()))
    }

    pub fn rows(&self) -> usize {
        self.rows
    }

    pub fn implicit_rows(&self) -> usize {
        self.implicit_rows
    }

    pub fn has_implicit_rhs(&self) -> bool {
        self.implicit_fn.is_some()
    }

    pub fn has_jacobian_v(&self) -> bool {
        self.jvp_fn.is_some()
    }
}

fn validate_output_len(
    function: &'static str,
    expected: usize,
    actual: usize,
) -> Result<(), MlirError> {
    if actual != expected {
        return Err(MlirError::OutputLength {
            function,
            expected,
            actual,
        });
    }
    Ok(())
}
