//! Acceptance-surface coverage for `rumoca compile --target <dir>`.
//!
//! A code-gen target that consumes DAE-derived IR carries its whole
//! admissibility proof in the manifest's `[capabilities]` table: the
//! compact-family gate, the residual-algebraic gate, the Phase-DAE
//! temporal-operator invariant, and the sealed `SolveProblem` construction
//! check itself all run only through it. A manifest that omits the table
//! therefore states nothing about which models it can render, and rendering it
//! anyway publishes bytes as if they had been checked.
//!
//! Every refusal below is paired with a positive control that differs only by
//! the table, so neither can pass vacuously.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::{TempDir, tempdir};

const FIXTURE: &str = "\
model AcceptanceFixture
  Real x(start = 1);
equation
  der(x) = -x;
end AcceptanceFixture;
";

/// The stable tag every undeclared-capabilities refusal carries, whichever IR
/// and whichever guard raised it.
const UNDECLARED_CODE: &str = "unsupported-feature:target-capabilities-undeclared";

/// The output file the fixture target renders. Its content template touches no
/// IR field, so the render itself cannot fail: whatever refuses a case below is
/// the acceptance gate, never a broken template.
const RENDERED_FILE: &str = "AcceptanceFixture_acceptance.txt";

/// Columns a target of `ir` may legally declare (`structured_equation_families`
/// is a DAE-only column) set permissive, so a positive control fails only if
/// the model itself is inadmissible.
fn permissive_capabilities(ir: &str) -> String {
    let mut table = String::from(
        "[capabilities]\n\
         continuous_states = true\n\
         residual_equations = true\n\
         external_functions = true\n\
         external_tables = true\n\
         random = true\n\
         initialization = true\n\
         events = true\n\
         runtime_events = true\n\
         clocks = true\n\
         dynamic_ranges = true\n\
         dynamic_derivative_subscripts = true\n",
    );
    if ir == "dae" {
        table.push_str("structured_equation_families = true\n");
    }
    table
}

struct Fixture {
    _dir: TempDir,
    model_file: PathBuf,
    target_dir: PathBuf,
    output_dir: PathBuf,
}

/// Write the model, a directory target consuming `ir`, and pick an output path.
///
/// `capabilities` is the manifest's `[capabilities]` table, or the empty string
/// for a manifest that declares none.
fn fixture(ir: &str, capabilities: &str) -> Fixture {
    let dir = tempdir().expect("tempdir");
    let model_file = dir.path().join("AcceptanceFixture.mo");
    fs::write(&model_file, FIXTURE).expect("write model fixture");

    let target_dir = dir.path().join("acceptance_target");
    fs::create_dir_all(&target_dir).expect("create target directory");
    fs::write(
        target_dir.join("acceptance.txt.jinja"),
        "{{ model_name }}\n",
    )
    .expect("write target template");
    fs::write(
        target_dir.join("target.toml"),
        format!(
            "version = 1\n\
             ir = \"{ir}\"\n\
             name = \"acceptance-fixture\"\n\
             description = \"acceptance-surface fixture\"\n\
             \n\
             {capabilities}\
             \n\
             [[files]]\n\
             path = \"{{{{ model_name }}}}_acceptance.txt\"\n\
             template = \"acceptance.txt.jinja\"\n"
        ),
    )
    .expect("write target manifest");

    let output_dir = dir.path().join("out");
    Fixture {
        _dir: dir,
        model_file,
        target_dir,
        output_dir,
    }
}

fn compile_target(fixture: &Fixture) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(&fixture.model_file)
        .arg("--target")
        .arg(&fixture.target_dir)
        .arg("--output")
        .arg(&fixture.output_dir)
        .output()
        .expect("run rumoca compile --target")
}

/// Assert the target renders its file: the control that keeps each refusal
/// below from passing for the wrong reason.
fn assert_target_renders(ir: &str) {
    let fixture = fixture(ir, &permissive_capabilities(ir));
    let output = compile_target(&fixture);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "a declared `{ir}` target must render (status {:?}):\n{stderr}",
        output.status.code()
    );
    assert!(
        fixture.output_dir.join(RENDERED_FILE).is_file(),
        "a declared `{ir}` target must write {RENDERED_FILE}:\n{stderr}"
    );
}

/// Assert the target is refused with the acceptance code, before any byte of
/// the artifact reaches the filesystem.
fn assert_target_refused(ir: &str) {
    let fixture = fixture(ir, "");
    let output = compile_target(&fixture);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        !output.status.success(),
        "an undeclared `{ir}` target must not render:\n{stdout}"
    );
    assert!(
        stderr.contains(UNDECLARED_CODE),
        "the refusal must carry `{UNDECLARED_CODE}`:\n{stderr}"
    );
    assert!(
        !fixture.output_dir.exists(),
        "a refused `{ir}` target must leave no output directory at {}",
        fixture.output_dir.display()
    );
    assert!(
        !stdout.contains("AcceptanceFixture"),
        "a refused `{ir}` target must not publish rendered content:\n{stdout}"
    );
}

/// The `solve` family was the gap this suite closes: `parse_target_manifest`
/// already refused an undeclared `dae`/`fmi`/`algorithm-code` manifest, and the
/// LSP's own Solve render path already demanded the table, but the CLI and
/// library target paths rendered an undeclared `solve` target without ever
/// reaching `validate_solve_target_capabilities`.
#[test]
fn solve_target_without_a_capabilities_table_is_refused() {
    assert_target_refused("solve");
}

#[test]
fn solve_target_with_a_capabilities_table_renders() {
    assert_target_renders("solve");
}

#[test]
fn dae_target_without_a_capabilities_table_is_refused() {
    assert_target_refused("dae");
}

#[test]
fn dae_target_with_a_capabilities_table_renders() {
    assert_target_renders("dae");
}

#[test]
fn flat_target_needs_no_capabilities_table() {
    // Flat templates carry no capability obligation: the Flat render context
    // proves its own contract (`rumoca::codegen::EC007`). Refusing them for a
    // missing table would be refusal with no rule behind it.
    let fixture = fixture("flat", "");
    let output = compile_target(&fixture);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "a flat target has no capability obligation (status {:?}):\n{stderr}",
        output.status.code()
    );
    assert!(
        fixture.output_dir.join(RENDERED_FILE).is_file(),
        "a flat target must render {RENDERED_FILE}:\n{stderr}"
    );
}

/// The lossy Modelica-family targets are removed rather than retained as
/// always-failing registered products.
#[test]
fn removed_flat_modelica_targets_are_unknown_and_emit_nothing() {
    let dir = tempdir().expect("tempdir");
    let model_file = dir.path().join("AcceptanceFixture.mo");
    fs::write(&model_file, FIXTURE).expect("write model fixture");
    for target in ["flat-modelica", "base-modelica"] {
        let out_dir = dir.path().join(target);
        let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
            .arg("compile")
            .arg(&model_file)
            .arg("--target")
            .arg(target)
            .arg("--output")
            .arg(&out_dir)
            .output()
            .expect("run rumoca compile --target");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            !output.status.success(),
            "removed target `{target}` must fail (status {:?})",
            output.status.code()
        );
        assert!(
            stderr.to_ascii_lowercase().contains("unknown target"),
            "removed target `{target}` must be rejected before rendering:\n{stderr}"
        );
        assert!(
            !rendered_any_file(&out_dir),
            "removed target `{target}` must not write an artifact:\n{stderr}"
        );
    }
}

fn rendered_any_file(dir: &Path) -> bool {
    fs::read_dir(dir)
        .map(|entries| entries.flatten().any(|entry| entry.path().is_file()))
        .unwrap_or(false)
}
