//! Regression: an enumeration compact range `E.first:E.last` (MLS §10.4.1) is
//! an enumeration-valued array, not an Integer compact range.
//!
//! Before the fix, canonical DAE construction required both compact-range
//! bounds to be Integer literals and rejected the MSL Digital pattern
//! `constant Logic LogicValues[:] = L.'U':L.'-'` with
//! `ED019 unsupported Flat semantic owner 'range start'`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const ENUMERATION_RANGE_MODEL: &str = r#"
model EnumerationCompactRange
  type Level = enumeration(low, mid, high, top);
  constant Level levels[:] = Level.mid:Level.top;
  Real y;
equation
  y = if levels[2] == Level.high then 3.0 else 0.0;
end EnumerationCompactRange;
"#;

#[test]
fn enumeration_compact_range_lowers_to_its_enumeration_values() {
    let compiled = Compiler::new()
        .model("EnumerationCompactRange")
        .compile_str(ENUMERATION_RANGE_MODEL, "enumeration_compact_range.mo")
        .expect("an enumeration compact range has a checked DAE owner");
    let wire = serde_json::to_string(&compiled.dae).expect("the enumeration array serializes");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("the wire reconstructs the enumeration array");
    let simulation = simulate_dae(&decoded, &SimOptions::default())
        .expect("the enumeration array lowers to a Solve value");
    let y = simulation
        .names
        .iter()
        .position(|name| name == "y")
        .expect("result y is visible");
    // levels = {mid, high, top}, so levels[2] is `high` with ordinal 3.
    assert!(
        simulation.data[y]
            .iter()
            .all(|value| (*value - 3.0).abs() <= 1.0e-12),
        "levels[2] must be the third literal of Level"
    );
}

/// MLS §4.9.5.2: `Integer(e)` is the ordinal of an enumeration value. The MSL
/// Digital `Integer(pre(x))` pattern reads that ordinal back out of an
/// enumeration coordinate, so the conversion must accept an Enumeration operand
/// rather than only a numeric one.
#[test]
fn integer_conversion_reads_an_enumeration_ordinal() {
    let compiled = Compiler::new()
        .model("EnumerationOrdinal")
        .compile_str(
            r#"
model EnumerationOrdinal
  type Level = enumeration(low, mid, high, top);
  constant Level levels[:] = Level.mid:Level.top;
  Real y;
equation
  y = Integer(levels[2]);
end EnumerationOrdinal;
"#,
            "enumeration_ordinal.mo",
        )
        .expect("Integer() of an enumeration value has a checked DAE owner");
    let simulation = simulate_dae(&compiled.dae, &SimOptions::default())
        .expect("the enumeration ordinal lowers to a Solve value");
    let y = simulation
        .names
        .iter()
        .position(|name| name == "y")
        .expect("result y is visible");
    assert!(
        simulation.data[y]
            .iter()
            .all(|value| (*value - 3.0).abs() <= 1.0e-12),
        "Integer(levels[2]) must be the ordinal of the third literal of Level"
    );
}

#[test]
fn compact_range_over_two_enumeration_types_is_rejected() {
    let error = Compiler::new()
        .model("MixedEnumerationRange")
        .compile_str(
            r#"
model MixedEnumerationRange
  type Low = enumeration(a, b, c);
  type High = enumeration(d, e, f);
  constant Low mixed[3] = Low.a:High.f;
  Real y;
equation
  y = if mixed[1] == Low.a then 1.0 else 0.0;
end MixedEnumerationRange;
"#,
            "mixed_enumeration_range.mo",
        )
        .expect_err("bounds declared by two enumeration types have no checked DAE owner");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("enumeration range"),
        "the rejection must name the enumeration-range owner, got: {rendered}"
    );
}
