//! End-to-end coverage for the two rules that govern a Modelica function call
//! as it crosses into GALEC: what an actual argument is converted to, and what
//! happens to a call that `Startup` is not allowed to make.
//!
//! # Argument conversion (MLS §10.6.13, §6.7)
//!
//! Modelica converts Integer to Real implicitly wherever a Real is expected,
//! element by element when the expected type is an array. GALEC has no
//! implicit conversion at all (SPEC_0042 trap T5), so the projection has to
//! apply `real(...)` itself, and it has to decide the element type from the
//! *formal* rather than from the actual. `identity(4)` reaching a `Real[:, :]`
//! formal is the flight case: without the aggregate conversion the projected
//! block refuses its own output with EG017.
//!
//! The conversion is directional. A Real actual reaching an Integer formal is
//! not a Modelica conversion and stays refused, which the negative controls
//! here pin at the type checker.
//!
//! # A call `Startup` may not make (SPEC_0034 GAL-017)
//!
//! `Startup` may call builtins only, and every emitted function must be
//! transitively reachable from `DoStep`. A dependent parameter bound to a
//! Modelica function call therefore has no emission as a call. When every
//! input the binding reads is fixed while the code is generated, the call has
//! one value and is emitted as that value; when an input is tunable the
//! binding is refused, because freezing it would make `Recalibrate` ignore the
//! tuning it exists to apply.
//!
//! These tests drive the real binary against `embedded-c-galec` so the
//! assertions are about bytes that reach a target, not about an intermediate
//! the shipped path might not carry.

use std::fs;
use std::path::Path;

use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};
use tempfile::tempdir;

use super::cli_support::{diagnostic_contains, run_compile_target, strip_ansi, write_fixture};

/// Target whose emitted C is read back by the accepting tests.
const TARGET: &str = "embedded-c-galec";

/// An Integer parameter array reaching a `Real[3]` formal.
const INTEGER_ARRAY_ARGUMENT: &str = "\
model GalecIntegerArrayArgument
  function sumReal
    input Real values[3];
    output Real total;
  algorithm
    total := values[1] + values[2] + values[3];
  end sumReal;
  constant Real samplePeriod = 0.1;
  parameter Integer counts[3] = {1, 2, 3};
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = sumReal(counts) + pre(y);
  end when;
end GalecIntegerArrayArgument;
";

/// A scalar Integer parameter reaching a `Real` formal.
const INTEGER_SCALAR_ARGUMENT: &str = "\
model GalecIntegerScalarArgument
  function scaleReal
    input Real factor;
    output Real value;
  algorithm
    value := 2.0 * factor;
  end scaleReal;
  constant Real samplePeriod = 0.1;
  parameter Integer count = 3;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = scaleReal(count) + pre(y);
  end when;
end GalecIntegerScalarArgument;
";

/// A Real parameter array reaching an `Integer[3]` formal: not a Modelica
/// conversion, so it must stay refused.
const REAL_ARRAY_ARGUMENT: &str = "\
model GalecRealArrayArgument
  function sumInteger
    input Integer values[3];
    output Integer total;
  algorithm
    total := values[1] + values[2] + values[3];
  end sumInteger;
  constant Real samplePeriod = 0.1;
  parameter Real amounts[3] = {1.5, 2.5, 3.5};
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = sumInteger(amounts) + pre(y);
  end when;
end GalecRealArrayArgument;
";

/// A Real parameter reaching an `Integer` formal.
const REAL_SCALAR_ARGUMENT: &str = "\
model GalecRealScalarArgument
  function twice
    input Integer n;
    output Integer value;
  algorithm
    value := 2 * n;
  end twice;
  constant Real samplePeriod = 0.1;
  parameter Real amount = 1.5;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = twice(amount) + pre(y);
  end when;
end GalecRealScalarArgument;
";

/// A dependent parameter bound to a call whose only input is evaluated at
/// translation time. `inverse2` is the closed-form inverse of a 2x2 matrix, so
/// the expected folded value is arithmetic anyone can redo by hand: for
/// `[[gain, 1], [1, gain]]` with `gain = 3`, the determinant is 8 and the
/// inverse is `[[0.375, -0.125], [-0.125, 0.375]]`.
const FROZEN_FOLD: &str = "\
model GalecFrozenFold
  function inverse2
    input Real gain;
    output Real inverse[2, 2];
  protected
    Real determinant;
  algorithm
    determinant := gain * gain - 1.0;
    assert(abs(determinant) > 1e-12, \"The 2x2 system must be invertible\");
    inverse := {{gain / determinant, -1.0 / determinant},
                {-1.0 / determinant, gain / determinant}};
  end inverse2;
  constant Real samplePeriod = 0.1;
  parameter Real gain = 3.0 annotation(Evaluate = true);
  parameter Real inverse[2, 2] = inverse2(gain);
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = inverse[1, 1] + pre(y);
  end when;
end GalecFrozenFold;
";

/// The same binding with a tunable `gain`: the fold would strand the tuning,
/// so the projection refuses.
const TUNABLE_FOLD: &str = "\
model GalecTunableFold
  function scaleVector
    input Real gain;
    output Real scaled[3];
  algorithm
    scaled := {gain, 2.0 * gain, 3.0 * gain};
  end scaleVector;
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  parameter Real scaled[3] = scaleVector(gain);
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = scaled[1] + pre(y);
  end when;
end GalecTunableFold;
";

/// A frozen binding whose value is far above the folding cap.
const OVERSIZED_FOLD: &str = "\
model GalecOversizedFold
  function rampVector
    input Real gain;
    input Integer count;
    output Real ramp[count];
  algorithm
    for index in 1:count loop
      ramp[index] := gain * index;
    end for;
  end rampVector;
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0 annotation(Evaluate = true);
  parameter Real ramp[2048] = rampVector(gain, 2048);
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = ramp[1] + pre(y);
  end when;
end GalecOversizedFold;
";

/// Compile one fixture and return the emitted model translation unit.
fn compile_to_c(model: &str, source: &str) -> String {
    let work = tempdir().expect("temp dir");
    let fixture = write_fixture(work.path(), model, source);
    let out = work.path().join("out");
    let output = run_compile_target(&fixture, TARGET, &out);
    assert!(
        output.status.success(),
        "{model} must project:\n{}",
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );
    read_model_unit(&out, model)
}

/// Read `<Model>.c` out of an `embedded-c-galec` output directory.
fn read_model_unit(out: &Path, model: &str) -> String {
    fs::read_to_string(out.join(format!("{model}.c")))
        .unwrap_or_else(|error| panic!("read emitted {model}.c: {error}"))
}

/// Compile one fixture that must be refused and return the plain diagnostics.
fn refusal_of(model: &str, source: &str) -> String {
    let work = tempdir().expect("temp dir");
    let fixture = write_fixture(work.path(), model, source);
    let out = work.path().join("out");
    let output = run_compile_target(&fixture, TARGET, &out);
    assert!(
        !output.status.success(),
        "{model} must be refused, but the projection accepted it"
    );
    String::from_utf8_lossy(&output.stderr).into_owned()
}

/// MLS §6.7 converts an Integer array to a Real array element by element, so
/// the emitted C must convert each element rather than hand the Integer
/// storage over.
#[test]
fn an_integer_array_argument_converts_elementwise_for_a_real_formal() {
    let unit = compile_to_c("GalecIntegerArrayArgument", INTEGER_ARRAY_ARGUMENT);
    assert!(
        unit.contains("sumReal("),
        "the call must survive as a call:\n{unit}"
    );
    assert!(
        unit.contains("(double)") || unit.contains("(float)"),
        "each Integer element must be converted before the call:\n{unit}"
    );
}

/// The scalar boundary takes its target from the formal for the same reason
/// the aggregate one does, so it is pinned by the same rule rather than by a
/// second mechanism.
#[test]
fn an_integer_scalar_argument_converts_for_a_real_formal() {
    let unit = compile_to_c("GalecIntegerScalarArgument", INTEGER_SCALAR_ARGUMENT);
    assert!(
        unit.contains("scaleReal("),
        "the call must survive as a call:\n{unit}"
    );
}

/// Negative control: Real to Integer is not a Modelica conversion in either
/// shape, and both stay refused.
#[test]
fn a_real_argument_still_refuses_an_integer_formal() {
    for (model, source) in [
        ("GalecRealArrayArgument", REAL_ARRAY_ARGUMENT),
        ("GalecRealScalarArgument", REAL_SCALAR_ARGUMENT),
    ] {
        let stderr = refusal_of(model, source);
        assert!(
            diagnostic_contains(&stderr, "expected `Integer`, found `Real`"),
            "{model} must refuse the narrowing argument:\n{}",
            strip_ansi(&stderr)
        );
    }
}

/// A call `Startup` may not make, with every input fixed while the code is
/// generated, reaches the target as its value. The expected numbers are the
/// closed-form 2x2 inverse, so the assertion is independent of how the
/// projection evaluated the function.
#[test]
fn a_frozen_dependent_parameter_call_folds_to_its_value() {
    let unit = compile_to_c("GalecFrozenFold", FROZEN_FOLD);
    assert!(
        !unit.contains("inverse2("),
        "the folded call must not survive into the emitted C:\n{unit}"
    );
    for expected in ["0.375", "-0.125"] {
        assert!(
            unit.contains(expected),
            "folded inverse must carry {expected}:\n{unit}"
        );
    }
}

/// A tunable input is refused rather than frozen, and the refusal names the
/// value the author can act on.
#[test]
fn a_tunable_input_refuses_the_fold_and_names_its_root() {
    let stderr = refusal_of("GalecTunableFold", TUNABLE_FOLD);
    let plain = strip_ansi(&stderr);
    assert!(plain.contains("EGT023"), "{plain}");
    assert!(
        diagnostic_contains(&stderr, "it reads `gain`, which is a tunable parameter"),
        "{plain}"
    );
    assert!(
        diagnostic_contains(&stderr, "annotation(Evaluate = true)"),
        "the refusal must state the remedy:\n{plain}"
    );
}

/// The folded call is absent from every emitted artifact, so the package
/// records which variable was folded and which function it was folded from.
/// Without that record nothing downstream could say where the literal came
/// from, and no generated file may carry the answer as a comment.
#[test]
fn a_folded_parameter_is_recorded_in_the_package() {
    let work = tempdir().expect("temp dir");
    let fixture = write_fixture(work.path(), "GalecFrozenFold", FROZEN_FOLD);
    let compiled = rumoca::Compiler::new()
        .model("GalecFrozenFold")
        .compile_path(&fixture)
        .expect("fixture compiles");
    let package = lower_to_algorithm_code(
        &GalecInput::new(&compiled.dae, "GalecFrozenFold"),
        &GalecOptions::default(),
    )
    .expect("fixture projects");
    let folded = package.constant_folded_parameters();
    assert_eq!(folded.len(), 1, "{folded:?}");
    assert_eq!(folded[0].variable, "inverse");
    assert_eq!(folded[0].folded_from, "inverse2");
    assert_eq!(folded[0].scalars, 4);
}

/// The fold is capped, so a frozen but very large value is refused instead of
/// being written twice into the generated code.
#[test]
fn an_oversized_folded_value_is_refused() {
    let stderr = refusal_of("GalecOversizedFold", OVERSIZED_FOLD);
    let plain = strip_ansi(&stderr);
    assert!(plain.contains("EGT023"), "{plain}");
    assert!(
        diagnostic_contains(&stderr, "2048 scalars"),
        "the refusal must state the measured size:\n{plain}"
    );
}

/// A record-typed parameter carrying the Evaluate mark freezes its members
/// too (MLS §18.6: the whole component is evaluated during translation), so
/// a dependent parameter reading through the record folds exactly like one
/// reading a scalar Evaluate parameter. This is the flight controller's
/// allocation shape: the rotor geometry record is frozen and the wrench map
/// folds to literals.
const RECORD_FROZEN_FOLD: &str = "\
model GalecRecordFrozenFold
  record Geometry
    Real arm = 0.25;
    Real lever = arm / 2.0;
  end Geometry;
  function leverGain
    input Real lever;
    output Real gain[2];
  algorithm
    gain := {1.0 / (4.0 * lever), -1.0 / (4.0 * lever)};
  end leverGain;
  constant Real samplePeriod = 0.1;
  parameter Geometry geometry = Geometry() annotation(Evaluate = true);
  parameter Real gain[2] = leverGain(geometry.lever);
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain[1] + pre(y);
  end when;
end GalecRecordFrozenFold;
";

#[test]
fn an_evaluate_record_parameter_freezes_its_members_for_the_fold() {
    let unit = compile_to_c("GalecRecordFrozenFold", RECORD_FROZEN_FOLD);
    assert!(
        !unit.contains("leverGain("),
        "the folded call must not survive into the emitted C:\n{unit}"
    );
    assert!(
        unit.contains("2.0") || unit.contains("2.0f") || unit.contains("2.00"),
        "folded gain 1/(4*0.125) = 2 must appear:\n{unit}"
    );
}
