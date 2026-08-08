//! End-to-end quadrotor Kalman-filter coverage for the GALEC export tracks
//! (SPEC_0034 GAL-027/028/029/030 — the estimator scope those rules were
//! written for): the `examples/models/QuadrotorAltitudeKF.mo` fixture
//! exercises matrix products (including the chained `A*P*transpose(A)`),
//! `identity`, a computed `initial equation`, and the
//! `Matrices.solve` → `solveLinearEquations` mapping with its
//! `SOLVE_LINEAR_EQUATIONS_FAILED` escape.
//!
//! The test is hermetic: `Modelica.Math.Matrices.solve` maps **by name**
//! (D13) before any body lookup, so a one-function MSL stub package stands
//! in for the real MSL — no library download.
//!
//! Checks: both embedded exports compile (`cc -Wall -Werror`,
//! `rustc -D warnings`), run 25 ticks on identical inputs, agree with each
//! other **exactly** (same evaluation order, both IEEE double, no
//! re-association — GAL-027/T6 end to end), and agree with an
//! independently-written reference filter to 1e-9.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use tempfile::tempdir;

#[path = "galec_cli_support/cc.rs"]
mod cc_support;

use cc_support::cc;

const MODEL: &str = "QuadrotorAltitudeKF";
const TICKS: usize = 25;

/// Hermetic stand-in for the MSL: only the resolved NAME matters — the
/// GALEC projection intercepts `Modelica.Math.Matrices.solve` before any
/// inlining (D13), so the body is never consumed.
const MSL_STUB: &str = "\
package Modelica \"Hermetic MSL stub (D13: Matrices.solve maps by name)\"
  package Math
    package Matrices
      function solve
        input Real A[:, :];
        input Real b[:];
        output Real x[size(b, 1)];
      algorithm
        x := b;
      end solve;
    end Matrices;
  end Math;
end Modelica;
";

const C_DRIVER: &str = "\
#include <stdio.h>
#include \"QuadrotorAltitudeKF.h\"

int main(void) {
    QuadrotorAltitudeKFState s;
    uint32_t status = QuadrotorAltitudeKF_startup(&s);
    for (int k = 0; k < 25; ++k) {
        s.u = 0.3 - 0.01 * (double)k;
        s.z_meas = 0.05 * (double)k;
        s.vz_meas = 0.05;
        status |= QuadrotorAltitudeKF_dostep(&s);
        printf(\"%d,%.17g,%.17g,%.17g,%.17g\\n\", k, s.z_hat, s.vz_hat, s.P[0][0], s.P[1][1]);
    }
    return status == 0u ? 0 : 2;
}
";

const RUST_DRIVER: &str = "\
fn main() {
    let mut s = QuadrotorAltitudeKF::QuadrotorAltitudeKFState::default();
    let mut failed = s.startup().is_err();
    for k in 0..25usize {
        s.u = 0.3 - 0.01 * (k as f64);
        s.z_meas = 0.05 * (k as f64);
        s.vz_meas = 0.05;
        failed |= s.do_step().is_err();
        println!(\"{k},{:.17e},{:.17e},{:.17e},{:.17e}\", s.z_hat, s.vz_hat, s.P[0][0], s.P[1][1]);
    }
    if failed {
        std::process::exit(2);
    }
}
";

fn fixture_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../examples/models")
        .join(format!("{MODEL}.mo"))
}

/// `rumoca compile <fixture> --target <target> -o <out>` with the stub
/// package on the source path.
fn compile_with_stub(target: &str, stub_root: &Path, out_dir: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(fixture_path())
        .arg("--source-root")
        .arg(stub_root)
        .arg("--target")
        .arg(target)
        .arg("-o")
        .arg(out_dir)
        .output()
        .unwrap_or_else(|error| panic!("run rumoca compile --target {target}: {error}"))
}

fn assert_success(output: &Output, what: &str) {
    assert!(
        output.status.success(),
        "{what} failed (status {:?}).\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Parse `k,z_hat,vz_hat,P11,P22` CSV lines.
fn parse_ticks(stdout: &[u8], what: &str) -> Vec<[f64; 4]> {
    let text = String::from_utf8_lossy(stdout).replace("\r\n", "\n");
    let ticks: Vec<[f64; 4]> = text
        .lines()
        .enumerate()
        .map(|(index, line)| {
            let fields: Vec<&str> = line.split(',').collect();
            assert_eq!(fields.len(), 5, "{what} line {index}: `{line}`");
            assert_eq!(
                fields[0].parse::<usize>().ok(),
                Some(index),
                "{what} dropped or duplicated a tick at line {index}"
            );
            [1, 2, 3, 4].map(|field| {
                fields[field]
                    .parse::<f64>()
                    .unwrap_or_else(|error| panic!("{what} line {index}: {error}"))
            })
        })
        .collect();
    assert_eq!(ticks.len(), TICKS, "{what} tick count");
    ticks
}

/// Independently-written reference filter (same math, its own code): the
/// fixture's parameters and update equations, with the same partial-pivot
/// solve the generated helpers implement.
fn reference_ticks() -> Vec<[f64; 4]> {
    let t = 0.02f64;
    let (q_accel, r_alt, r_vel, p0) = (0.05f64, 0.04f64, 0.09f64, 1.0f64);
    let a = [[1.0, t], [0.0, 1.0]];
    let b = [t * t / 2.0, t];
    let q = [[q_accel * t * t, 0.0], [0.0, q_accel]];
    let r = [[r_alt, 0.0], [0.0, r_vel]];
    let mut x = [0.0f64, 0.0];
    let mut p = [[p0, 0.0], [0.0, p0]];
    let mut out = Vec::with_capacity(TICKS);
    for k in 0..TICKS {
        let u = 0.3 - 0.01 * (k as f64);
        let (zm, vm) = (0.05 * (k as f64), 0.05);
        let x_pred = [
            a[0][0] * x[0] + a[0][1] * x[1] + b[0] * u,
            a[1][0] * x[0] + a[1][1] * x[1] + b[1] * u,
        ];
        // P_pred = A*P*A' + Q.
        let mut ap = [[0.0f64; 2]; 2];
        for i in 0..2 {
            for j in 0..2 {
                ap[i][j] = a[i][0] * p[0][j] + a[i][1] * p[1][j];
            }
        }
        let mut p_pred = [[0.0f64; 2]; 2];
        for i in 0..2 {
            for j in 0..2 {
                p_pred[i][j] = ap[i][0] * a[j][0] + ap[i][1] * a[j][1] + q[i][j];
            }
        }
        let s = [
            [p_pred[0][0] + r[0][0], p_pred[0][1] + r[0][1]],
            [p_pred[1][0] + r[1][0], p_pred[1][1] + r[1][1]],
        ];
        let k_row1 = solve2(s, [p_pred[0][0], p_pred[1][0]]);
        let k_row2 = solve2(s, [p_pred[0][1], p_pred[1][1]]);
        let (innov_z, innov_v) = (zm - x_pred[0], vm - x_pred[1]);
        x = [
            x_pred[0] + k_row1[0] * innov_z + k_row1[1] * innov_v,
            x_pred[1] + k_row2[0] * innov_z + k_row2[1] * innov_v,
        ];
        let ik = [[1.0 - k_row1[0], -k_row1[1]], [-k_row2[0], 1.0 - k_row2[1]]];
        let mut p_next = [[0.0f64; 2]; 2];
        for i in 0..2 {
            for j in 0..2 {
                p_next[i][j] = ik[i][0] * p_pred[0][j] + ik[i][1] * p_pred[1][j];
            }
        }
        p = p_next;
        out.push([x[0], x[1], p[0][0], p[1][1]]);
    }
    out
}

/// 2x2 `a*x = b` by Gaussian elimination with partial pivoting (the same
/// scheme the generated helpers use).
fn solve2(mut a: [[f64; 2]; 2], mut b: [f64; 2]) -> [f64; 2] {
    if a[1][0].abs() > a[0][0].abs() {
        a.swap(0, 1);
        b.swap(0, 1);
    }
    let factor = a[1][0] / a[0][0];
    let a11 = a[1][1] - factor * a[0][1];
    let b1 = b[1] - factor * b[0];
    let x1 = b1 / a11;
    [(b[0] - a[0][1] * x1) / a[0][0], x1]
}

#[test]
fn kalman_filter_exports_agree_across_c_rust_and_reference() {
    let dir = tempdir().expect("tempdir");
    let stub_root = dir.path().join("msl-stub");
    fs::create_dir_all(&stub_root).expect("mkdir stub");
    fs::write(stub_root.join("Modelica.mo"), MSL_STUB).expect("write stub");

    // --- C track ---
    let c_out = dir.path().join("c");
    assert_success(
        &compile_with_stub("embedded-c-galec", &stub_root, &c_out),
        "compile --target embedded-c-galec",
    );
    let c_source = c_out.join(format!("{MODEL}.c"));
    fs::write(c_out.join("main.c"), C_DRIVER).expect("write C driver");
    let c_program = c_out.join("kf");
    let compile = cc()
        .arg("-Wall")
        .arg("-Werror")
        .arg("-o")
        .arg(&c_program)
        .arg(c_out.join("main.c"))
        .arg(&c_source)
        .arg("-lm")
        .output()
        .expect("run cc");
    assert_success(&compile, "cc -Wall -Werror");
    let c_run = Command::new(&c_program).output().expect("run C block");
    assert_success(&c_run, "C driver (0 = no signal escaped)");
    let c_ticks = parse_ticks(&c_run.stdout, "C output");

    // --- Rust track ---
    let rust_out = dir.path().join("rust");
    assert_success(
        &compile_with_stub("embedded-rust-galec", &stub_root, &rust_out),
        "compile --target embedded-rust-galec",
    );
    let rlib_dir = rust_out.join("rlib");
    let rlib = Command::new("rustc")
        .arg("--edition")
        .arg("2021")
        .arg("--crate-type")
        .arg("rlib")
        .arg("-D")
        .arg("warnings")
        .arg(rust_out.join(format!("{MODEL}.rs")))
        .arg("--out-dir")
        .arg(&rlib_dir)
        .output()
        .expect("run rustc (rlib)");
    assert_success(&rlib, "rustc -D warnings (rlib)");
    fs::write(rust_out.join("main.rs"), RUST_DRIVER).expect("write Rust driver");
    let rust_program = rust_out.join("kf");
    let link = Command::new("rustc")
        .arg("--edition")
        .arg("2021")
        .arg(rust_out.join("main.rs"))
        .arg("--extern")
        .arg(format!(
            "{MODEL}={}",
            rlib_dir.join(format!("lib{MODEL}.rlib")).display()
        ))
        .arg("-o")
        .arg(&rust_program)
        .output()
        .expect("run rustc (driver)");
    assert_success(&link, "rustc (driver)");
    let rust_run = Command::new(&rust_program)
        .output()
        .expect("run Rust block");
    assert_success(&rust_run, "Rust driver (Ok = no signal escaped)");
    let rust_ticks = parse_ticks(&rust_run.stdout, "Rust output");

    // --- Equivalence ---
    // C and Rust render the same evaluation order from the same GALEC AST
    // (no re-association, GAL-027/T6), so IEEE-754 doubles agree exactly.
    for (tick, (c, rust)) in c_ticks.iter().zip(&rust_ticks).enumerate() {
        for (field, (cv, rv)) in c.iter().zip(rust).enumerate() {
            assert!(
                cv == rv,
                "C/Rust divergence at tick {tick} field {field}: {cv} vs {rv}"
            );
        }
    }
    // Both agree with the independently-written reference filter.
    for (tick, (got, want)) in c_ticks.iter().zip(reference_ticks()).enumerate() {
        for (field, (gv, wv)) in got.iter().zip(want).enumerate() {
            assert!(
                (gv - wv).abs() <= 1e-9 * wv.abs().max(1.0),
                "reference divergence at tick {tick} field {field}: {gv} vs {wv}"
            );
        }
    }
}

/// The Algorithm Code eFMU export of the same fixture declares the
/// GAL-029 escape on DoStep and carries per-method Signals in the manifest.
#[test]
fn kalman_filter_algorithm_code_declares_the_solve_escape() {
    let dir = tempdir().expect("tempdir");
    let stub_root = dir.path().join("msl-stub");
    fs::create_dir_all(&stub_root).expect("mkdir stub");
    fs::write(stub_root.join("Modelica.mo"), MSL_STUB).expect("write stub");

    let out = dir.path().join("efmu");
    assert_success(
        &compile_with_stub("galec", &stub_root, &out),
        "compile --target galec",
    );
    let alg = fs::read_to_string(
        out.join(MODEL)
            .join("AlgorithmCode")
            .join(format!("{MODEL}.alg")),
    )
    .expect("read .alg");
    assert!(
        alg.contains("signals SOLVE_LINEAR_EQUATIONS_FAILED;"),
        "DoStep must declare the solve escape (GAL-029):\n{alg}"
    );
    let manifest = fs::read_to_string(out.join(MODEL).join("AlgorithmCode").join("manifest.xml"))
        .expect("read manifest");
    assert!(
        manifest.contains("<Signal value=\"SOLVE_LINEAR_EQUATIONS_FAILED\"/>"),
        "manifest must carry the DoStep Signals element:\n{manifest}"
    );
}
