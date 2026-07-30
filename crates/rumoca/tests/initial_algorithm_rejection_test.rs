//! Regression: an `initial algorithm` has no canonical DAE owner yet, and the
//! rejection must name that missing owner.
//!
//! Before the fix, function-shape discovery ran first and read the statement
//! form `assert(...)` inside the initial algorithm as a call to an unregistered
//! function, so the MSL `Electrical.Analog.Sources.DCPowerSupply` pattern
//! reported `ED008 unresolved Flat reference 'assert'` — a consequence of the
//! absent owner rather than the owner itself.

use rumoca::Compiler;

const INITIAL_ALGORITHM_MODEL: &str = r#"
model InitialAlgorithmAssert
  parameter Real v0 = 60.0;
  parameter Real i0 = 50.0;
  parameter Real limit(fixed=false);
  Real x(start = 0, fixed = true);
initial algorithm
  assert(v0 > 0, "v0 must be positive");
  limit := v0 / i0;
equation
  der(x) = limit;
end InitialAlgorithmAssert;
"#;

#[test]
fn initial_algorithm_is_rejected_at_its_own_owner() {
    let error = Compiler::new()
        .model("InitialAlgorithmAssert")
        .compile_str(INITIAL_ALGORITHM_MODEL, "initial_algorithm_assert.mo")
        .expect_err("an initial algorithm has no canonical DAE owner");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("ED013") && rendered.contains("unsupported initial algorithm"),
        "the rejection must name the initial-algorithm owner, got: {rendered}"
    );
    assert!(
        !rendered.contains("unresolved Flat reference"),
        "a statement-form builtin inside the rejected section must not be \
         reported as an unresolved callee, got: {rendered}"
    );
}
