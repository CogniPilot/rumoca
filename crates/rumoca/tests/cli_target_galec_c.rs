//! End-to-end CLI coverage for `rumoca compile --target galec-c`
//! (SPEC_0034 GAL-011/GAL-012/GAL-024).
//!
//! Invokes the real binary so the whole chain is exercised: CLI dispatch →
//! generic capability gate → GALEC projection → C mangler/printer → typed
//! template context → thin C templates. The emitted sources are then
//! compiled with `cc -Wall -Werror` and LINKED against a generated driver
//! (`-lm`), and the driver is executed to check the discrete dynamics —
//! the roadmap-mandated compile check plus a behavioral check on top
//! (GAL-012: generated C is compile-checked, never skip-and-mark-covered).
//! Like the `galec` suite's `xmllint` requirement, a missing `cc` is a
//! hard failure, never a skip.
//!
//! This target is the non-eFMI track of GAL-024: a GALEC-derived embedded
//! C export that must self-describe as NOT an eFMI Production Code
//! container — the tests pin that honesty in both the emitted header and
//! the CLI completion message.

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use tempfile::tempdir;

#[path = "galec_cli_support/cc.rs"]
mod cc_support;
#[path = "galec_cli_support/cli.rs"]
mod cli_support;

use cc_support::cc;
use cli_support::{run_compile_target, strip_ansi, write_fixture};

/// Fixed-sample discrete fixture: a parameter, a `pre()` state, an output,
/// and one `when sample(...)` clock — the shape the GALEC projection
/// admits (mirrors `cli_target_galec.rs`).
const DISCRETE_FIXTURE: &str = "\
model EmbeddedGalecSmoke
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain * (pre(y) + 1.0);
  end when;
end EmbeddedGalecSmoke;
";

const MODEL: &str = "EmbeddedGalecSmoke";

/// Continuous model the capability gate must reject (GAL-006).
const CONTINUOUS_FIXTURE: &str = "\
model EmbeddedGalecContinuous
  Real x(start = 1.0);
  parameter Real k = 2.0;
equation
  der(x) = -k * x;
end EmbeddedGalecContinuous;
";

/// Driver exercising the generated block: startup, recalibrate, then three
/// dostep ticks of `y = gain * (pre(y) + 1)` with `gain = 2`, `y0 = 0`
/// (expected 2, 6, 14).
const DRIVER_MAIN: &str = "\
#include <stdio.h>
#include \"EmbeddedGalecSmoke.h\"

int main(void) {
    EFMI_STATE_TYPE(EmbeddedGalecSmoke) state;
    EFMI_INIT(EmbeddedGalecSmoke, &state);
    EFMI_RECALIBRATE(EmbeddedGalecSmoke, &state);
    for (int step = 0; step < 3; ++step) {
        EFMI_STEP(EmbeddedGalecSmoke, &state);
        printf(\"%.1f\\n\", state.y);
    }
    return 0;
}
";

fn run_compile_galec_c(file: &Path, out_dir: &Path) -> Output {
    run_compile_target(file, "galec-c", out_dir)
}

/// Compile the discrete fixture into `out_dir`, failing loudly on any CLI
/// error, and return the CLI stderr for message assertions.
fn build_sources(work_dir: &Path, out_dir: &Path) -> String {
    let file = write_fixture(work_dir, MODEL, DISCRETE_FIXTURE);
    let output = run_compile_galec_c(&file, out_dir);
    assert!(
        output.status.success(),
        "`compile --target galec-c` failed (status {:?}).\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stderr).into_owned()
}

/// The emitted sources compile under `-Wall -Werror`, link against libm
/// with a real driver, and the executed block reproduces the discrete
/// dynamics tick for tick.
#[test]
fn emitted_c_compiles_links_and_reproduces_the_discrete_dynamics() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    build_sources(dir.path(), &out_dir);

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
        // Normalize CRLF: Windows text-mode stdio emits `\n` as `\r\n`.
        String::from_utf8_lossy(&run.stdout).replace("\r\n", "\n"),
        "2.0\n6.0\n14.0\n",
        "three dostep ticks of y := gain * (previous(y) + 1) with gain = 2"
    );
}

/// GAL-024 honesty: the emitted header and the CLI completion message both
/// self-describe as NOT an eFMI Production Code container.
#[test]
fn export_self_describes_as_not_an_efmi_production_code_container() {
    let dir = tempdir().expect("tempdir");
    let out_dir = dir.path().join("out");
    let stderr = build_sources(dir.path(), &out_dir);

    let header = fs::read_to_string(out_dir.join(format!("{MODEL}.h"))).expect("read header");
    assert!(
        header.contains("NOT an eFMI Production Code container"),
        "header must carry the GAL-024 self-description:\n{header}"
    );
    for macro_name in [
        "EFMI_STATE_TYPE",
        "EFMI_INIT",
        "EFMI_RECALIBRATE",
        "EFMI_STEP",
    ] {
        assert!(
            header.contains(macro_name),
            "header must expose {macro_name}:\n{header}"
        );
    }
    assert!(
        strip_ansi(&stderr).contains("NOT an eFMI Production Code"),
        "completion message must carry the GAL-024 self-description, got:\n{stderr}"
    );
}

#[test]
fn continuous_model_is_rejected_by_the_capability_gate() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), "EmbeddedGalecContinuous", CONTINUOUS_FIXTURE);
    let out_dir = dir.path().join("out");

    let output = run_compile_galec_c(&file, &out_dir);
    assert!(
        !output.status.success(),
        "`compile --target galec-c` must fail for a continuous model.\nstdout:\n{}",
        String::from_utf8_lossy(&output.stdout)
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("unsupported-feature:continuous_states"),
        "expected the generic capability diagnostic (GAL-006), got stderr:\n{stderr}"
    );
    // The gate runs before any rendering: nothing may be written on rejection.
    assert!(
        !out_dir.exists(),
        "capability rejection must happen before the output directory is created"
    );
}

#[test]
fn targets_listing_includes_galec_c() {
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
        stdout.contains("galec-c"),
        "`rumoca targets` must list the galec-c target:\n{stdout}"
    );
}
