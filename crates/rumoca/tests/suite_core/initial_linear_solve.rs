use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimulationSession};

const SOURCE: &str = r#"
model InitialLinearSolve
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
  function solve
    input Real A[:, size(A, 1)];
    input Real b[size(A, 1)];
    output Real x[size(A, 1)];
  protected
    Integer info;
  algorithm
    (x, info) := dgesv_vec(A, b);
    assert(info == 0, "matrix must be nonsingular");
  end solve;
  parameter Real p[2] = solve([0,2;1,3], {4,7});
  Real x(start=p[1], fixed=true);
  Real y(start=p[2], fixed=true);
equation
  der(x) = 0;
  der(y) = 0;
end InitialLinearSolve;
"#;

#[test]
fn parameter_linear_solve_initializes_dependent_states() {
    let compiled = Compiler::new()
        .model("InitialLinearSolve")
        .compile_str(SOURCE, "InitialLinearSolve.mo")
        .unwrap();
    let session = SimulationSession::new(&compiled.dae, SimOptions::default()).unwrap();
    assert_eq!(session.time(), 0.0);
    assert_eq!(session.get("x").unwrap(), Some(1.0));
    assert_eq!(session.get("y").unwrap(), Some(2.0));
}
