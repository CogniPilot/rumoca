//! LAPACK `dgesv` with one right-hand side as a checked linear solve
//! (MLS 3.7 §12.9, SPEC_0040 DAE-C26).
//!
//! `Modelica.Math.Matrices.solve` calls `LAPACK.dgesv_vec`, an external
//! FORTRAN 77 body that receives protected locals initialized from the
//! function's inputs. The Solve runtime owns no foreign code, so the call
//! executes as the linear solve `dgesv` computes.

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
