//! End-to-end CLI coverage for `rumoca compile --target embedded-rust-galec`
//! (SPEC_0034 GAL-011/GAL-012/GAL-030/D16).
//!
//! Invokes the real binary so the whole chain is exercised: CLI dispatch →
//! generic capability gate → GALEC projection → language-neutral block
//! context (`template_ir`) → the walking Rust template. The emitted source
//! is compiled with `rustc --edition 2021 -D warnings` both standalone
//! (`--crate-type lib`, proving the `#![no_std]` file is a self-contained
//! crate root) and as an rlib LINKED against a generated driver whose
//! execution checks the discrete dynamics tick for tick (GAL-012:
//! generated code is compile-checked, never skip-and-mark-covered).
//! `rustc` is the toolchain building this very test, so it is always
//! present — a spawn failure is a hard failure, never a skip.
//!
//! This target is a non-eFMI track (GAL-030): a GALEC-derived embedded
//! Rust export that must self-describe as NOT an eFMI Production Code
//! container (Rust is outside the Beta-1 ProductionCode schema) — pinned
//! in both the emitted file and the CLI completion message.

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use tempfile::tempdir;

#[path = "galec_cli_support/cli.rs"]
mod cli_support;

use cli_support::{run_compile_target, strip_ansi, write_fixture};

/// Fixed-sample discrete fixture (mirrors `cli_target_embedded_c_galec.rs`
/// so the two tracks pin identical dynamics).
const DISCRETE_FIXTURE: &str = "\
model EmbeddedRustGalecSmoke
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain * (pre(y) + 1.0);
  end when;
end EmbeddedRustGalecSmoke;
";

const MODEL: &str = "EmbeddedRustGalecSmoke";

/// Continuous model the capability gate must reject (GAL-006).
const CONTINUOUS_FIXTURE: &str = "\
model EmbeddedRustGalecContinuous
  Real x(start = 1.0);
  parameter Real k = 2.0;
equation
  der(x) = -k * x;
end EmbeddedRustGalecContinuous;
";

/// Driver exercising the generated block: startup, recalibrate, then three
/// do_step ticks of `y = gain * (pre(y) + 1)` with `gain = 2`, `y0 = 0`
/// (expected 2, 6, 14).
const DRIVER_MAIN: &str = "\
fn main() {
    let mut state = EmbeddedRustGalecSmoke::EmbeddedRustGalecSmokeState::default();
    state.startup();
    state.recalibrate();
    for _ in 0..3 {
        state.do_step();
        println!(\"{:.1}\", state.y);
    }
}
";

fn run_compile_embedded_rust_galec(file: &Path, out_dir: &Path) -> Output {
    run_compile_target(file, "embedded-rust-galec", out_dir)
}

/// Compile the discrete fixture into `out_dir`, failing loudly on any CLI
/// error, and return the CLI stderr for message assertions.
fn build_source(work_dir: &Path, out_dir: &Path) -> String {
    let file = write_fixture(work_dir, MODEL, DISCRETE_FIXTURE);
    let output = run_compile_embedded_rust_galec(&file, out_dir);
    assert!(
        output.status.success(),
        "`compile --target embedded-rust-galec` failed (status {:?}).\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stderr).into_owned()
}

fn rustc() -> Command {
    Command::new("rustc")
}

/// The emitted source compiles standalone under `-D warnings` as a
/// `#![no_std]` lib crate root, links against a generated driver as an
/// rlib, and the executed block reproduces the discrete dynamics tick for
/// tick.
#[test]
fn emitted_rust_compiles_links_and_reproduces_the_discrete_dynamics() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    build_source(dir.path(), &out_dir);

    let source = out_dir.join(format!("{MODEL}.rs"));
    assert!(source.is_file(), "missing {}", source.display());

    // Standalone: the file is a self-contained `#![no_std]` crate root
    // with zero dependencies — `-D warnings` keeps it lint-clean.
    let standalone = rustc()
        .arg("--edition")
        .arg("2021")
        .arg("--crate-type")
        .arg("lib")
        .arg("-D")
        .arg("warnings")
        .arg(&source)
        .arg("--out-dir")
        .arg(out_dir.join("standalone"))
        .output()
        .expect("run rustc (standalone lib)");
    assert!(
        standalone.status.success(),
        "rustc -D warnings failed.\nstderr:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&standalone.stderr),
        fs::read_to_string(&source).unwrap_or_default()
    );

    // Linked: rlib + driver, then run the block.
    let rlib_dir = out_dir.join("rlib");
    let rlib = rustc()
        .arg("--edition")
        .arg("2021")
        .arg("--crate-type")
        .arg("rlib")
        .arg("-D")
        .arg("warnings")
        .arg(&source)
        .arg("--out-dir")
        .arg(&rlib_dir)
        .output()
        .expect("run rustc (rlib)");
    assert!(
        rlib.status.success(),
        "rustc rlib build failed.\nstderr:\n{}",
        String::from_utf8_lossy(&rlib.stderr)
    );
    let driver = out_dir.join("main.rs");
    fs::write(&driver, DRIVER_MAIN).expect("write driver");
    let program = out_dir.join("smoke");
    let link = rustc()
        .arg("--edition")
        .arg("2021")
        .arg(&driver)
        .arg("--extern")
        .arg(format!(
            "{MODEL}={}",
            rlib_dir.join(format!("lib{MODEL}.rlib")).display()
        ))
        .arg("-o")
        .arg(&program)
        .output()
        .expect("run rustc (driver)");
    assert!(
        link.status.success(),
        "driver link failed.\nstderr:\n{}",
        String::from_utf8_lossy(&link.stderr)
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
        // Normalize CRLF: Windows text-mode stdio emits `\n` as `\r\n`.
        String::from_utf8_lossy(&run.stdout).replace("\r\n", "\n"),
        "2.0\n6.0\n14.0\n",
        "three do_step ticks of y := gain * (previous(y) + 1) with gain = 2"
    );
}

/// GAL-030 honesty: the emitted file and the CLI completion message both
/// self-describe as NOT an eFMI Production Code container.
#[test]
fn export_self_describes_as_not_an_efmi_production_code_container() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let stderr = build_source(dir.path(), &out_dir);

    let source = fs::read_to_string(out_dir.join(format!("{MODEL}.rs"))).expect("read source");
    assert!(
        source.contains("NOT an eFMI Production Code container"),
        "source must carry the GAL-030 self-description:\n{source}"
    );
    assert!(
        source.contains("#![no_std]"),
        "source must be a no_std crate root (D15):\n{source}"
    );
    assert!(
        strip_ansi(&stderr).contains("NOT an eFMI Production Code"),
        "completion message must carry the GAL-030 self-description, got:\n{stderr}"
    );
}

#[test]
fn continuous_model_is_rejected_by_the_capability_gate() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(
        dir.path(),
        "EmbeddedRustGalecContinuous",
        CONTINUOUS_FIXTURE,
    );
    let out_dir = dir.path().join("out");

    let output = run_compile_embedded_rust_galec(&file, &out_dir);
    assert!(
        !output.status.success(),
        "`compile --target embedded-rust-galec` must fail for a continuous model.\nstdout:\n{}",
        String::from_utf8_lossy(&output.stdout)
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("unsupported-feature:continuous_states"),
        "expected the generic capability diagnostic (GAL-006), got stderr:\n{stderr}"
    );
    assert!(
        !out_dir.exists(),
        "capability rejection must happen before the output directory is created"
    );
}

#[test]
fn targets_listing_includes_embedded_rust_galec() {
    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("targets")
        .output()
        .expect("run rumoca targets");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success(),
        "`rumoca targets` failed.\nstdout:\n{stdout}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        stdout.contains("embedded-rust-galec"),
        "targets listing must include embedded-rust-galec:\n{stdout}"
    );
}
