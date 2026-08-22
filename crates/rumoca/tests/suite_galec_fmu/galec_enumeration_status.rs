//! Modelica enumerations through the GALEC projection to embedded C.
//!
//! Blocker 1 — `EGT017` — is FIXED. `crates/rumoca-phase-galec/src/lower.rs`'s
//! `fn scalar_type` now maps `dae::ScalarType::Enumeration` to
//! `gast::ScalarType::Integer`. Everything downstream was already
//! enumeration-ready: `lower/expression_helpers.rs` lowers
//! `DaeLiteral::Enumeration(n)` to `gast::Expression::Integer(n)`, and
//! `rumoca-eval-dae`'s numeric evaluator folds enumeration literals to their
//! ordinal.
//!
//! KNOWN GAP: the projected ordinal is an *unbounded* Integer. The DAE erases
//! enumeration cardinality (`dae::ScalarType::Enumeration` is a unit variant
//! and `dae::ValueType` carries no literal count), so the declaration gets no
//! `min`/`max` and the implicit `limit self` boundary saturation has nothing
//! to clamp against. Nothing constrains the ordinal to its enumeration's
//! range. Tracked separately.
//!
//! Blocker 2 — `ED008` (`crates/rumoca-phase-dae`) — is FIXED, and the
//! characterization test that pinned it has been flipped to the positive
//! assertion it was written to become.
//!
//! The cause was a role-map asymmetry inside DAE construction, not anything
//! enumeration-specific downstream. `analyze_model_roles` produces two maps:
//! `variables` (the coordinate plan) and `expressions` (the coordinate plan
//! plus the enumeration literals of `Model::enum_literal_ordinals` and the
//! record aggregates). `validate_model_expressions` and
//! `validate_model_algorithm` already validated reads against the *expression*
//! map, but `validate_when_chains` received only the coordinate map, so an
//! `E.lit` read inside a `when` equation had no planned role and
//! `validate_reference` rejected it as an unresolved Flat reference. The
//! identical literal in a plain equation resolved, and — because a `when`
//! written in an *algorithm* section already used the expression map — the same
//! model spelled algorithmically also resolved.
//!
//! `validate_when_chains` now takes both maps: coordinate questions (may this
//! target be assigned, is this a state, does this name own a clock, is this
//! `previous(...)` operand clocked) still read the coordinate plan, and every
//! expression *read* reads the expression plan. The when scope also now carries
//! the enumeration-literal catalog, so a name that merely spells like a
//! cataloged literal without its enumeration's declaration identity is still
//! rejected — the same "identity, never spelling" rule the model-equation scope
//! applies.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use tempfile::tempdir;

/// An enumeration-typed **parameter** with no enumeration literal in any
/// equation. This is the narrowest model that reaches the GALEC projection,
/// so it isolates blocker 1 from blocker 2.
const ENUM_PARAMETER_FIXTURE: &str = "\
type Mode = enumeration(Idle, Armed, Active);

model GalecEnumParameter
  constant Real samplePeriod = 0.1;
  parameter Mode mode = Mode.Armed;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = pre(y) + 1.0;
  end when;
end GalecEnumParameter;
";

/// An enumeration literal assigned inside a `when` equation — the narrowest
/// shape that used to raise `ED008`.
const ENUM_IN_WHEN_FIXTURE: &str = "\
type Mode = enumeration(Idle, Armed, Active);

model GalecEnumWhen
  constant Real samplePeriod = 0.1;
  discrete Mode mode(start = Mode.Idle);
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    mode = Mode.Armed;
    y = pre(y) + 1.0;
  end when;
end GalecEnumWhen;
";

/// The shape a real mode-logic block has: a package-qualified enumeration
/// compared in a branch guard and assigned in the branch body, all inside one
/// `when`. Every enumeration read here is an expression read that the
/// coordinate role plan alone could not resolve.
const ENUM_MODE_LOGIC_FIXTURE: &str = "\
package Flight
  type Mode = enumeration(Idle, Armed, Active);

  model GalecEnumModeLogic
    constant Real samplePeriod = 0.1;
    input Real command;
    discrete Mode mode(start = Flight.Mode.Idle);
    discrete output Real throttle(start = 0.0);
  equation
    when sample(0.0, samplePeriod) then
      if pre(mode) == Flight.Mode.Idle and command > 0.5 then
        mode = Flight.Mode.Armed;
        throttle = 0.0;
      elseif pre(mode) == Flight.Mode.Armed and command > 1.5 then
        mode = Flight.Mode.Active;
        throttle = 0.25;
      else
        mode = pre(mode);
        throttle = pre(throttle);
      end if;
    end when;
  end GalecEnumModeLogic;
end Flight;
";

fn write_fixture(dir: &Path, model: &str, source: &str) -> PathBuf {
    let file = dir.join(format!("{model}.mo"));
    std::fs::write(&file, source).expect("write fixture");
    file
}

fn compile_embedded_c_galec(file: &Path, out_dir: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(file)
        .arg("--target")
        .arg("embedded-c-galec")
        .arg("-o")
        .arg(out_dir)
        .output()
        .expect("run rumoca compile --target embedded-c-galec")
}

/// Drop ANSI SGR escapes so assertions see plain diagnostic text.
fn strip_ansi(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars();
    while let Some(ch) = chars.next() {
        if ch != '\u{1b}' {
            out.push(ch);
            continue;
        }
        for escape in chars.by_ref() {
            if escape.is_ascii_alphabetic() {
                break;
            }
        }
    }
    out
}

/// Blocker 1, now FIXED: `lower.rs`'s `scalar_type` maps `Enumeration` to
/// `gast::ScalarType::Integer` (MLS §4.9.5 — an enumeration value is its
/// 1-based ordinal), so an enumeration-typed variable projects to embedded C
/// as an `int32_t`.
#[test]
fn enumeration_variable_projects_to_an_integer() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), "GalecEnumParameter", ENUM_PARAMETER_FIXTURE);
    let output = compile_embedded_c_galec(&file, dir.path());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "enumeration variables must project to GALEC, got:\n{stderr}"
    );
    assert!(
        !stderr.contains("EGT017"),
        "enumeration variables must no longer raise EGT017, got:\n{stderr}"
    );

    let header = std::fs::read_to_string(dir.path().join("GalecEnumParameter.h"))
        .expect("the projection emits a C header");
    assert!(
        header.contains("int32_t mode"),
        "the enumeration must be declared as an int32_t ordinal:\n{header}"
    );
}

/// Blocker 2, now FIXED (this is the flipped characterization test). An
/// enumeration literal assigned inside a `when` equation resolves to its 1-based
/// ordinal (MLS §4.9.5) and reaches embedded C.
///
/// The ordinal is asserted, not just the absence of `ED008`: `Mode.Armed` is the
/// **second** literal of `enumeration(Idle, Armed, Active)`, so a projection that
/// resolved the reference but folded the wrong ordinal — 0-based, or the
/// declaration order of some other enumeration — would still be wrong code.
#[test]
fn enumeration_literal_in_when_equation_resolves_to_its_ordinal() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), "GalecEnumWhen", ENUM_IN_WHEN_FIXTURE);
    let output = compile_embedded_c_galec(&file, dir.path());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "an enumeration literal in a `when` equation must reach GALEC, got:\n{stderr}"
    );
    assert!(
        !stderr.contains("[ED008]"),
        "the when-equation enumeration literal must no longer raise ED008, got:\n{stderr}"
    );

    let header = std::fs::read_to_string(dir.path().join("GalecEnumWhen.h"))
        .expect("the projection emits a C header");
    assert!(
        header.contains("int32_t mode"),
        "the enumeration coordinate must be declared as an int32_t ordinal:\n{header}"
    );

    let source = std::fs::read_to_string(dir.path().join("GalecEnumWhen.c"))
        .expect("the projection emits a C translation unit");
    assert!(
        source.contains("self->mode = 2;"),
        "`Mode.Armed` is the second literal, so the when body must assign ordinal 2:\n{source}"
    );
    assert!(
        source.contains("self->mode = 1;"),
        "`start = Mode.Idle` is the first literal, so startup must assign ordinal 1:\n{source}"
    );
}

/// The firmware-shaped case: a package-qualified enumeration *compared* in a
/// branch guard and *assigned* in the branch body, inside one `when`. This is
/// what `ED008` actually blocked — a fix that only admitted a bare assignment
/// would still leave mode logic unwritable.
#[test]
fn enumeration_mode_logic_in_a_when_equation_reaches_embedded_c() {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), "GalecEnumModeLogic", ENUM_MODE_LOGIC_FIXTURE);
    let output = compile_embedded_c_galec(&file, dir.path());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "enumeration mode logic in a `when` equation must reach GALEC, got:\n{stderr}"
    );

    let source = std::fs::read_to_string(dir.path().join("Flight_GalecEnumModeLogic.c"))
        .expect("the projection emits a C translation unit");
    // Guards compare the ordinal of the literal they name, bodies assign it.
    assert!(
        source.contains("self->previous_mode == 1"),
        "the `pre(mode) == Flight.Mode.Idle` guard must compare ordinal 1:\n{source}"
    );
    assert!(
        source.contains("self->previous_mode == 2"),
        "the `pre(mode) == Flight.Mode.Armed` guard must compare ordinal 2:\n{source}"
    );
    assert!(
        source.contains("self->mode = 2;"),
        "the armed branch must assign ordinal 2:\n{source}"
    );
    assert!(
        source.contains("self->mode = 3;"),
        "the active branch must assign ordinal 3:\n{source}"
    );
}
