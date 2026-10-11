//! MSL's inverse uses declaration-initialized LAPACK LU and inverse bodies
//! (MLS §12.4.4, §12.9). The foreign symbol alone cannot prove this interface.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae};

// The ordered interfaces and declaration bindings are those of pinned MSL
// 4.1.0 Math/package.mo; no foreign library is linked by these tests.
const FUNCTIONS: &str = r#"
  pure function dgetrf
    input Real A[:, :];
    output Real LU[size(A, 1), size(A, 2)] = A;
    output Integer pivots[min(size(A, 1), size(A, 2))];
    output Integer info;
  protected
    Integer m = size(A, 1);
    Integer n = size(A, 2);
    Integer lda = max(1, size(A, 1));
  external "FORTRAN 77" dgetrf(m, n, LU, lda, pivots, info);
  end dgetrf;

  pure function dgetri
    input Real LU[:, size(LU, 1)];
    input Integer pivots[size(LU, 1)];
    output Real inv[size(LU, 1), size(LU, 2)] = LU;
    output Integer info;
  protected
    Integer n = size(LU, 1);
    Integer lda = max(1, size(LU, 1));
    Integer lwork = max(1, min(10, size(LU, 1))*size(LU, 1));
    Real work[max(1, min(10, size(LU, 1))*size(LU, 1))];
  external "FORTRAN 77" dgetri(n, inv, lda, pivots, work, lwork, info);
  end dgetri;
"#;

fn run(declarations: &str, equations: &str) -> SimResult {
    let source =
        format!("model LUProbe\n{FUNCTIONS}\n{declarations}\nequation\n{equations}\nend LUProbe;");
    let compiled = Compiler::new()
        .model("LUProbe")
        .compile_str(&source, "native_lu_inverse.mo")
        .unwrap_or_else(|error| panic!("the declared LAPACK interface compiles: {error}"));
    simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("checked LAPACK bodies execute")
}

fn column<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result.names.iter().position(|n| n == name).expect(name);
    &result.data[index]
}

#[test]
fn lu_preserves_multipliers_pivot_order_and_first_singular_step() {
    let result = run(
        "Real LU[2,2]; Integer pivots[2]; Integer info; Real singular[2,2]; Integer sp[2]; Integer si;",
        "(LU,pivots,info) = dgetrf([2,1;4,3]*(1+time));\n(singular,sp,si) = dgetrf([0,1;0,2]);",
    );
    for (sample, time) in result.times.iter().enumerate() {
        let scale = 1.0 + time;
        for (name, expected) in [
            ("LU[1,1]", 4.0 * scale),
            ("LU[1,2]", 3.0 * scale),
            ("LU[2,1]", 0.5),
            ("LU[2,2]", -0.5 * scale),
            ("pivots[1]", 2.0),
            ("pivots[2]", 2.0),
            ("info", 0.0),
            ("si", 1.0),
        ] {
            assert!(
                (column(&result, name)[sample] - expected).abs() < 1e-10,
                "{name} at {time}"
            );
        }
    }
}

#[test]
fn inverse_consumes_the_issued_lu_and_pivot_tuple() {
    let result = run(
        "Real LU[2,2]; Integer pivots[2]; Integer info; Real inverse[2,2]; Integer inverseInfo;",
        "(LU,pivots,info) = dgetrf([2,1;4,3]*(1+time));\n(inverse,inverseInfo) = dgetri(LU,pivots);",
    );
    for (sample, time) in result.times.iter().enumerate() {
        for (name, numerator) in [
            ("inverse[1,1]", 1.5),
            ("inverse[1,2]", -0.5),
            ("inverse[2,1]", -2.0),
            ("inverse[2,2]", 1.0),
        ] {
            assert!(
                (column(&result, name)[sample] - numerator / (1.0 + time)).abs() < 1e-10,
                "{name} at {time}"
            );
        }
        assert_eq!(column(&result, "info")[sample], 0.0);
        assert_eq!(column(&result, "inverseInfo")[sample], 0.0);
    }
}
