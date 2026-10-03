//! LAPACK `dgelsy` with one right-hand side as a checked least squares
//! solve (MLS 3.7 §12.9, SPEC_0040 DAE-C29).
//!
//! `Modelica.Math.Matrices.leastSquares` calls `LAPACK.dgelsy_vec`, an
//! external FORTRAN 77 body; `Modelica.Math.Polynomials.fitting` and the
//! table-based media fit their coefficients through it. The Solve runtime
//! owns no foreign code, so the call executes as the minimum-norm least
//! squares solution `dgelsy` computes, with its effective rank.

use std::path::PathBuf;

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

fn msl_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    root.is_dir().then_some(root)
}

fn simulate(source: &str, model: &str, root: Option<&PathBuf>) -> SimResult {
    let mut compiler = Compiler::new().model(model);
    if let Some(root) = root {
        compiler = compiler.source_root(root.to_string_lossy().as_ref());
    }
    let compiled = compiler
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error}"))
}

fn column<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} is recorded"));
    &result.data[index]
}

const FIT_SOURCE: &str = r#"
model LeastSquaresFit
  Real x[2] = Modelica.Math.Matrices.leastSquares([1, 0; 1, 1; 1, 2; 1, 3], {1, 3 + time, 5, 8});
end LeastSquaresFit;
"#;

/// An overdetermined full-rank system: the solution satisfies the normal
/// equations `A**T (A x - b) = 0`.
#[test]
fn matrices_least_squares_executes_its_lapack_driver() {
    let Some(root) = msl_root() else {
        eprintln!("skipping: the MSL is not available");
        return;
    };
    let result = simulate(FIT_SOURCE, "LeastSquaresFit", Some(&root));
    let (x1, x2) = (column(&result, "x[1]"), column(&result, "x[2]"));
    for (sample, time) in result.times.iter().enumerate() {
        let (a, b) = (x1[sample], x2[sample]);
        let residual = [
            a - 1.0,
            a + b - 3.0 - time,
            a + 2.0 * b - 5.0,
            a + 3.0 * b - 8.0,
        ];
        let first: f64 = residual.iter().sum();
        let second: f64 = residual
            .iter()
            .enumerate()
            .map(|(row, value)| row as f64 * value)
            .sum();
        assert!(first.abs() < 1e-12, "normal equation 1 at {time}");
        assert!(second.abs() < 1e-12, "normal equation 2 at {time}");
    }
}

/// The MSL `dgelsy_vec` interface, without the MSL.
const DRIVER_SOURCE: &str = r#"
model RankCases
  pure function dgelsy_vec
    input Real A[:, :];
    input Real b[size(A, 1)];
    input Real rcond = 0.0;
    output Real x[max(size(A, 1), size(A, 2))] = cat(1, b, zeros(max(nrow, ncol) - nrow));
    output Integer info;
    output Integer rank;
  protected
    Integer nrow = size(A, 1);
    Integer ncol = size(A, 2);
    Integer nrhs = 1;
    Integer nx = max(nrow, ncol);
    Integer lwork = max(min(nrow, ncol) + 3*ncol + 1, 2*min(nrow, ncol) + 1);
    Real work[max(min(size(A, 1), size(A, 2)) + 3*size(A, 2) + 1, 2*min(size(A, 1), size(A, 2)) + 1)];
    Real Awork[size(A, 1), size(A, 2)] = A;
    Integer jpvt[size(A, 2)] = zeros(ncol);
  external "FORTRAN 77" dgelsy(nrow, ncol, nrhs, Awork, nrow, x, nx, jpvt, rcond, rank, work, lwork, info);
  end dgelsy_vec;
  Real deficient[4];
  Integer deficientRank;
  Real wide[3];
  Integer wideRank;
  Real zero[2];
  Integer zeroRank;
  Real near[3];
  Integer nearRank;
  Integer info1;
  Integer info2;
  Integer info3;
  Integer info4;
equation
  (deficient, info1, deficientRank) = dgelsy_vec([1, 2, 3; 2, 4, 6; 1, 0, 1; 0, 1, 1], {1, 2, 3, 4}*(1 + time), 1e-10);
  (wide, info2, wideRank) = dgelsy_vec([1, 2, 3; 4, 5, 6], {1, 2}*(1 + time), 1e-10);
  (zero, info3, zeroRank) = dgelsy_vec([0, 0; 0, 0]*time, {1, 2}, 1e-10);
  (near, info4, nearRank) = dgelsy_vec([1, 1; 1, 1 + 1e-12; 1, 1], {1, 2, 3}*(1 + time), 1e-6);
end RankCases;
"#;

/// Rank-deficient, underdetermined, zero, and numerically rank-one matrices
/// give the effective rank and minimum-norm solution of LAPACK 3 `dgelsy`
/// (reference values computed with the reference driver).
#[test]
fn dgelsy_gives_the_effective_rank_and_minimum_norm_solution() {
    let result = simulate(DRIVER_SOURCE, "RankCases", None);
    let cases: [(&str, &[f64], f64); 4] = [
        ("deficient", &[2.0 / 3.0, -10.0 / 39.0, 16.0 / 39.0], 2.0),
        ("wide", &[-1.0 / 18.0, 1.0 / 9.0, 5.0 / 18.0], 2.0),
        ("zero", &[0.0, 0.0], 0.0),
        ("near", &[0.999_999_999_999_666_8, 1.0], 1.0),
    ];
    for (sample, time) in result.times.iter().enumerate() {
        for (name, expected, rank) in cases {
            let scale = if name == "zero" { 1.0 } else { 1.0 + time };
            for (index, value) in expected.iter().enumerate() {
                let actual = column(&result, &format!("{name}[{}]", index + 1))[sample];
                assert!(
                    (actual - value * scale).abs() < 1e-9,
                    "{name}[{}] at {time}: {actual}, expected {}",
                    index + 1,
                    value * scale
                );
            }
            assert_eq!(
                column(&result, &format!("{name}Rank"))[sample],
                rank,
                "{name} rank at {time}"
            );
        }
        for index in 1..=4 {
            assert_eq!(column(&result, &format!("info{index}"))[sample], 0.0);
        }
    }
}

/// A pivot vector that fixes columns (`JPVT(i) /= 0`) is not the free
/// pivoting the definition covers, so the foreign body stays refused.
#[test]
fn fixed_pivot_columns_keep_the_external_refused() {
    let source = DRIVER_SOURCE.replace(
        "jpvt[size(A, 2)] = zeros(ncol)",
        "jpvt[size(A, 2)] = ones(ncol)",
    );
    let compiled = Compiler::new()
        .model("RankCases")
        .compile_str(&source, "RankCases.mo");
    let refused = match compiled {
        Err(_) => true,
        Ok(compiled) => {
            simulate_dae_with_diagnostics(&compiled.dae, &SimOptions::default()).is_err()
        }
    };
    assert!(refused, "dgelsy with fixed pivot columns must be refused");
}
