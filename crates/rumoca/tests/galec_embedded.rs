//! End-to-end test for `rumoca compile --target embedded-c-galec` using the
//! new `rumoca-galec` crate pipeline (analyze → transform → render).
//!
//! Invokes the real binary, then compiles the emitted C with `cc -Wall -Werror`
//! and runs a simple driver to verify the discrete dynamics.

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use tempfile::tempdir;

fn cc() -> Command {
    let probe = Command::new("cc").arg("--version").output();
    assert!(
        probe.is_ok_and(|output| output.status.success()),
        "`cc` must be installed: the generated-C compile check is mandatory"
    );
    Command::new("cc")
}

fn run_compile_target(file: &Path, target: &str, out_dir: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(file)
        .arg("--target")
        .arg(target)
        .arg("-o")
        .arg(out_dir)
        .output()
        .unwrap_or_else(|e| panic!("run rumoca compile --target {target}: {e}"))
}

const MODEL: &str = "GalecCounter";

const DISCRETE_FIXTURE: &str = "\
model GalecCounter
  constant Real samplePeriod = 0.001;
  parameter Real gain = 2.0;
  discrete Integer count(start = 0);
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    y = gain * count;
  end when;
end GalecCounter;
";

// Driver calls startup, recalibrate, then 3 dostep ticks.
// With gain = 2.0 and count starting at 0:
//   step 1: count = 1, y = 2.0 * 1 = 2.0
//   step 2: count = 2, y = 2.0 * 2 = 4.0
//   step 3: count = 3, y = 2.0 * 3 = 6.0
const DRIVER_MAIN: &str = r#"
#include <stdio.h>
#include "GalecCounter.h"

int main(void) {
    GalecCounterState state;
    GalecCounter_startup(&state);
    GalecCounter_recalibrate(&state);
    for (int step = 0; step < 3; ++step) {
        GalecCounter_dostep(&state);
        printf("%.1f\n", state.y);
    }
    return 0;
}
"#;

fn build_sources(work_dir: &Path, target: &str, out_dir: &Path) -> String {
    let file = work_dir.join(format!("{MODEL}.mo"));
    fs::write(&file, DISCRETE_FIXTURE).expect("write fixture");
    let output = run_compile_target(&file, target, out_dir);
    assert!(
        output.status.success(),
        "`compile --target {target}` failed.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stderr).into_owned()
}

#[test]
fn emitted_c_compiles_and_reproduces_discrete_dynamics() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    build_sources(dir.path(), "embedded-c-galec", &out_dir);

    let header = out_dir.join(format!("{MODEL}.h"));
    let source = out_dir.join(format!("{MODEL}.c"));
    assert!(header.is_file(), "missing {}", header.display());
    assert!(source.is_file(), "missing {}", source.display());

    let driver = out_dir.join("main.c");
    fs::write(&driver, DRIVER_MAIN).expect("write driver");
    let program = out_dir.join("smoke");
    let compile = cc()
        .arg("-Wall")
        .arg("-Werror")
        .arg("-o")
        .arg(&program)
        .arg(&driver)
        .arg(&source)
        .arg("-lm")
        .output()
        .expect("run cc");
    assert!(
        compile.status.success(),
        "cc -Wall -Werror failed.\nstderr:\n{}\nheader:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&compile.stderr),
        fs::read_to_string(&header).unwrap_or_default(),
        fs::read_to_string(&source).unwrap_or_default()
    );

    let run = Command::new(&program)
        .output()
        .expect("run generated block");
    assert!(
        run.status.success(),
        "generated block driver exited with {:?}",
        run.status.code()
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout).replace("\r\n", "\n"),
        "2.0\n4.0\n6.0\n",
        "three dostep ticks: y = gain * count with gain=2, count increments by 1"
    );
}

// Rust driver: compile the generated lib, link a small main that exercises the
// three methods, and assert the same 2.0/4.0/6.0 output as the C driver.
const RS_DRIVER_MAIN: &str = r#"
extern crate galec_counter;
use galec_counter::GalecCounterState;
fn main() {
    let mut state = GalecCounterState::new();
    state.startup();
    state.recalibrate();
    for _ in 0..3 {
        state.do_step();
        println!("{:.1}", state.y);
    }
}
"#;

#[test]
fn emitted_rust_compiles_and_reproduces_discrete_dynamics() {
    let rustc_ok = Command::new("rustc")
        .arg("--version")
        .output()
        .is_ok_and(|o| o.status.success());
    if !rustc_ok {
        eprintln!("skipping embedded-rust-galec compile check: rustc not found");
        return;
    }

    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    build_sources(dir.path(), "embedded-rust-galec", &out_dir);

    let rs_file = out_dir.join(format!("{MODEL}.rs"));
    assert!(rs_file.is_file(), "missing {}", rs_file.display());

    // Verify key structural elements in the output.
    let content = fs::read_to_string(&rs_file).expect("read generated Rust");
    assert!(
        content.contains("pub struct GalecCounterState"),
        "struct must be present"
    );
    assert!(
        content.contains("pub fn startup"),
        "startup method must be present"
    );
    assert!(
        content.contains("pub fn do_step"),
        "do_step method must be present"
    );
    assert!(
        content.contains("count_prev"),
        "previous value slot must be present"
    );

    // Compile as a library.
    let rlib = out_dir.join("libgalec_counter.rlib");
    let lib_compile = Command::new("rustc")
        .arg("--edition")
        .arg("2021")
        .arg("--crate-type")
        .arg("lib")
        .arg("--crate-name")
        .arg("galec_counter")
        .arg(&rs_file)
        .arg("-o")
        .arg(&rlib)
        .output()
        .expect("run rustc (lib)");
    assert!(
        lib_compile.status.success(),
        "rustc (lib) failed for generated Rust.\nstderr:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&lib_compile.stderr),
        &content
    );

    // Compile the driver binary linking against the generated lib.
    let driver_src = out_dir.join("driver.rs");
    fs::write(&driver_src, RS_DRIVER_MAIN).expect("write Rust driver");
    let driver_bin = out_dir.join("rust_smoke");
    let bin_compile = Command::new("rustc")
        .arg("--edition")
        .arg("2021")
        .arg("--crate-type")
        .arg("bin")
        .arg("--extern")
        .arg(format!("galec_counter={}", rlib.display()))
        .arg(&driver_src)
        .arg("-o")
        .arg(&driver_bin)
        .output()
        .expect("run rustc (bin)");
    assert!(
        bin_compile.status.success(),
        "rustc (bin) failed for Rust driver.\nstderr:\n{}",
        String::from_utf8_lossy(&bin_compile.stderr)
    );

    // Run and check output.
    let run = Command::new(&driver_bin).output().expect("run Rust driver");
    assert!(
        run.status.success(),
        "Rust driver exited with {:?}",
        run.status.code()
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout).replace("\r\n", "\n"),
        "2.0\n4.0\n6.0\n",
        "three do_step ticks: y = gain * count with gain=2, count increments by 1"
    );
}

const KALMAN_FIXTURE: &str = "\
model KalmanFilter1D
  constant Real samplePeriod = 0.01;
  parameter Real Q = 0.1;
  parameter Real R = 1.0;
  discrete input Real z;
  discrete output Real x_hat(start = 0.0);
  discrete Real P(start = 1.0);
  discrete Real P_pred(start = 1.0);
  discrete Real K(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    P_pred = pre(P) + Q;
    K = P_pred / (P_pred + R);
    x_hat = pre(x_hat) + K * (z - pre(x_hat));
    P = (1.0 - K) * P_pred;
  end when;
end KalmanFilter1D;
";

fn generate_kalman_target(file: &Path, target: &str, out_dir: &Path) {
    let output = run_compile_target(file, target, out_dir);
    assert!(
        output.status.success(),
        "compile --target {target} failed for KalmanFilter1D.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn compile_kalman_rust(rs_file: &Path, out_dir: &Path) {
    let content = fs::read_to_string(rs_file).expect("read generated Rust");
    let rlib = out_dir.join("libkalman.rlib");
    let compile = Command::new("rustc")
        .arg("--edition")
        .arg("2021")
        .arg("--crate-type")
        .arg("lib")
        .arg("--crate-name")
        .arg("kalman")
        .arg(rs_file)
        .arg("-o")
        .arg(&rlib)
        .output()
        .expect("run rustc");
    assert!(
        compile.status.success(),
        "rustc failed for KalmanFilter1D.rs:\nstderr:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&compile.stderr),
        &content
    );

    assert!(content.contains("pub struct KalmanFilter1DState"));
    assert!(content.contains("pub fn do_step"));
    assert!(content.contains("#![no_std]"));
    assert!(content.contains("pub P: f64"));
    assert!(content.contains("pub K: f64"));
    assert!(content.contains("pub x_hat: f64"));
}

fn compile_kalman_c(header: &Path, source: &Path, out_dir: &Path) {
    assert!(header.is_file(), "missing {}", header.display());
    let object = out_dir.join("KalmanFilter1D.o");
    let compile = cc()
        .arg("-Wall")
        .arg("-Werror")
        .arg("-c")
        .arg(source)
        .arg("-o")
        .arg(&object)
        .output()
        .expect("run cc");
    assert!(
        compile.status.success(),
        "cc -Wall -Werror failed for generated KalmanFilter1D.c.\nstderr:\n{}\nheader:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&compile.stderr),
        fs::read_to_string(header).unwrap_or_default(),
        fs::read_to_string(source).unwrap_or_default()
    );
}

#[test]
fn kalman_filter_generates_alg_rust_and_c() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("KalmanFilter1D.mo");
    fs::write(&file, KALMAN_FIXTURE).expect("write kalman fixture");

    let alg_out = dir.path().join("alg");
    generate_kalman_target(&file, "galec", &alg_out);
    let alg_file = alg_out
        .join("KalmanFilter1D")
        .join("AlgorithmCode")
        .join("KalmanFilter1D.alg");
    let alg = fs::read_to_string(&alg_file).expect("read generated GALEC");
    assert!(alg.contains("block KalmanFilter1D"));
    assert!(alg.contains("method DoStep"));
    assert!(alg.contains("end KalmanFilter1D;"));

    let rust_out = dir.path().join("rust");
    generate_kalman_target(&file, "embedded-rust-galec", &rust_out);
    let rs_file = rust_out.join("KalmanFilter1D.rs");
    compile_kalman_rust(&rs_file, &rust_out);

    let c_out = dir.path().join("c");
    generate_kalman_target(&file, "embedded-c-galec", &c_out);
    compile_kalman_c(
        &c_out.join("KalmanFilter1D.h"),
        &c_out.join("KalmanFilter1D.c"),
        &c_out,
    );
}

#[test]
fn continuous_model_is_rejected() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("Continuous.mo");
    fs::write(
        &file,
        "model Continuous\n  Real x(start=1.0);\nequation\n  der(x) = -x;\nend Continuous;\n",
    )
    .expect("write fixture");
    let out_dir = dir.path().join("out");

    let output = run_compile_target(&file, "embedded-c-galec", &out_dir);
    assert!(
        !output.status.success(),
        "`compile --target embedded-c-galec` must fail for a continuous model"
    );
    assert!(
        !out_dir.exists(),
        "capability rejection must happen before the output directory is created"
    );
}
