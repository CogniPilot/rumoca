//! LAPACK `dgesv` with one right-hand side as a checked linear solve
//! (MLS 3.7 §12.9, SPEC_0040 DAE-C26).
//!
//! `Modelica.Math.Matrices.solve` calls `LAPACK.dgesv_vec`, an external
//! FORTRAN 77 body that receives protected locals initialized from the
//! function's inputs. The Solve runtime owns no foreign code, so the call
//! executes as the linear solve `dgesv` computes, reporting a singular matrix
//! through `info`.

use std::path::PathBuf;

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

fn msl_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    root.is_dir().then_some(root)
}

const SOURCE: &str = r#"
model MatricesSolve
  Real x[3] = Modelica.Math.Matrices.solve([2, 1, 0; 1, 3, 1; 0, 1, 4 + time], {1, 2, 3});
end MatricesSolve;
"#;

#[test]
fn matrices_solve_executes_its_lapack_linear_solve() {
    let Some(root) = msl_root() else {
        eprintln!("skipping: the MSL is not available");
        return;
    };
    let compiled = Compiler::new()
        .model("MatricesSolve")
        .source_root(root.to_string_lossy().as_ref())
        .compile_str(SOURCE, "MatricesSolve.mo")
        .unwrap_or_else(|error| panic!("MatricesSolve compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("MatricesSolve simulates");
    let column = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        &result.data[index]
    };
    let (x1, x2, x3) = (column("x[1]"), column("x[2]"), column("x[3]"));
    for (sample, time) in result.times.iter().enumerate() {
        let (a, b, c) = (x1[sample], x2[sample], x3[sample]);
        assert!((2.0 * a + b - 1.0).abs() < 1e-12, "row 1 at {time}");
        assert!((a + 3.0 * b + c - 2.0).abs() < 1e-12, "row 2 at {time}");
        assert!(
            (b + (4.0 + time) * c - 3.0).abs() < 1e-12,
            "row 3 at {time}"
        );
    }
}

/// `dgesv` reports a singular matrix through `info` (the first zero pivot
/// step of elimination with partial pivoting) and leaves the right-hand side
/// unchanged, so a caller that handles `info > 0` itself keeps working.
const SINGULAR_SOURCE: &str = r#"
model SingularInfo
  pure function dgesv_vec
    input Real A[:, size(A, 1)];
    input Real b[size(A, 1)];
    output Real x[size(A, 1)] = b;
    output Integer info;
  protected
    Integer n = size(A, 1);
    Integer nrhs = 1;
    Real Awork[size(A, 1), size(A, 1)] = A;
    Integer lda = max(1, size(A, 1));
    Integer ldb = max(1, size(b, 1));
    Integer ipiv[size(A, 1)];
  external "FORTRAN 77" dgesv(n, nrhs, Awork, lda, ipiv, x, ldb, info);
  end dgesv_vec;
  function solveOrZero
    input Real A[:, size(A, 1)];
    input Real b[size(A, 1)];
    output Real x[size(A, 1)];
  protected
    Integer info;
  algorithm
    (x, info) := dgesv_vec(A, b);
    if info > 0 then
      x := zeros(size(A, 1));
    end if;
  end solveOrZero;
  Real x[3];
  Integer info;
  Real y[3] = solveOrZero([1, 2, 0; 2, 4, 0; 0, 0, 1]*(1 + time), {1, 2, 3});
  Real z[2] = solveOrZero([1 + time, 1; 1, 2], {1, 1});
equation
  (x, info) = dgesv_vec([1, 2, 0; 2, 4, 0; 0, 0, 1], {1, 2, 3});
end SingularInfo;
"#;

#[test]
fn a_singular_matrix_reports_its_zero_pivot_through_info() {
    let compiled = Compiler::new()
        .model("SingularInfo")
        .compile_str(SINGULAR_SOURCE, "SingularInfo.mo")
        .unwrap_or_else(|error| panic!("SingularInfo compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("SingularInfo simulates");
    let column = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        &result.data[index]
    };
    for (sample, time) in result.times.iter().enumerate() {
        // LAPACK dgesv: the second pivot column is zero after the first
        // step, so info = 2 and B is returned unchanged.
        assert_eq!(column("info")[sample], 2.0, "info at {time}");
        for (name, expected) in [("x[1]", 1.0), ("x[2]", 2.0), ("x[3]", 3.0)] {
            assert_eq!(column(name)[sample], expected, "{name} at {time}");
        }
        for name in ["y[1]", "y[2]", "y[3]"] {
            assert_eq!(column(name)[sample], 0.0, "{name} at {time}");
        }
        let (a, b) = (column("z[1]")[sample], column("z[2]")[sample]);
        assert!(
            ((1.0 + time) * a + b - 1.0).abs() < 1e-12,
            "z row 1 at {time}"
        );
        assert!((a + 2.0 * b - 1.0).abs() < 1e-12, "z row 2 at {time}");
    }
}
