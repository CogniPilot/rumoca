//! Compiler-defined bodies of cataloged foreign entry points (MLS 3.7 §12.9,
//! SPEC_0040 DAE-C30).
//!
//! The MSL xorshift generators are pure external C functions. The Solve
//! runtime owns no foreign code, so a declaration proven against its catalog
//! row executes that row's body, and any other external interface is refused.

use std::path::PathBuf;

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

fn msl_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    root.is_dir().then_some(root)
}

/// The exposed-state part of `Modelica.Math.Random.Examples.GenerateRandomNumbers`.
const MSL_SOURCE: &str = r#"
model XorshiftDraws
  parameter Integer localSeed = 614657;
  parameter Integer globalSeed = 30020;
  output Real r64;
  output Real r128;
  output Real r1024;
protected
  discrete Integer state64[2](each start = 0, each fixed = true);
  discrete Integer state128[4](each start = 0, each fixed = true);
  discrete Integer state1024[33](each start = 0, each fixed = true);
algorithm
  when initial() then
    state64 := Modelica.Math.Random.Generators.Xorshift64star.initialState(localSeed, globalSeed);
    state128 := Modelica.Math.Random.Generators.Xorshift128plus.initialState(localSeed, globalSeed);
    state1024 := Modelica.Math.Random.Generators.Xorshift1024star.initialState(localSeed, globalSeed);
    r64 := 0;
    r128 := 0;
    r1024 := 0;
  elsewhen sample(0, 0.05) then
    (r64, state64) := Modelica.Math.Random.Generators.Xorshift64star.random(pre(state64));
    (r128, state128) := Modelica.Math.Random.Generators.Xorshift128plus.random(pre(state128));
    (r1024, state1024) := Modelica.Math.Random.Generators.Xorshift1024star.random(pre(state1024));
  end when;
end XorshiftDraws;
"#;

/// The draws at `t = 0, 0.05, 0.1` as OpenModelica's run of the MSL 4.1 C
/// sources reports them (16 significant digits).
const REFERENCE: [(&str, [f64; 3]); 3] = [
    (
        "r64",
        [0.3135925178776172, 0.5110345034226015, 0.04244575388960031],
    ),
    (
        "r128",
        [0.4611540572500742, 0.03170147695200792, 0.9192264762816789],
    ),
    (
        "r1024",
        [0.3115592868634632, 0.05995209958305187, 0.9468004399728742],
    ),
];

#[test]
fn msl_xorshift_generators_reproduce_the_c_sources() {
    let Some(root) = msl_root() else {
        eprintln!("skipping: the MSL is not available");
        return;
    };
    let compiled = Compiler::new()
        .model("XorshiftDraws")
        .source_root(root.to_string_lossy().as_ref())
        .compile_str(MSL_SOURCE, "XorshiftDraws.mo")
        .unwrap_or_else(|error| panic!("XorshiftDraws compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.12,
            ..SimOptions::default()
        },
    )
    .expect("XorshiftDraws simulates");
    for (name, expected) in REFERENCE {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .map(|index| &result.data[index])
            .unwrap_or_else(|| panic!("{name} is recorded"));
        for (draw, expected) in expected.iter().enumerate() {
            let time = 0.05 * draw as f64 + 0.01;
            let sample = result
                .times
                .iter()
                .position(|candidate| *candidate >= time)
                .unwrap_or_else(|| panic!("a sample after {time}"));
            assert_eq!(
                format!("{:.15e}", column[sample]),
                format!("{expected:.15e}"),
                "{name} draw {draw}"
            );
        }
    }
}

fn generator_source(prefix: &str, extent: usize) -> String {
    format!(
        r#"
model Draw
  {prefix} function xorshift
    input Integer stateIn[{extent}];
    output Real result;
    output Integer stateOut[{extent}];
  external "C" ModelicaRandom_xorshift64star(stateIn, stateOut, result);
  end xorshift;
  discrete Real r(start = 0, fixed = true);
  discrete Integer s[{extent}](each start = 1, each fixed = true);
algorithm
  when sample(0, 0.1) then
    (r, s) := xorshift(pre(s));
  end when;
end Draw;
"#
    )
}

/// The declaration MSL writes executes the catalog body.
#[test]
fn a_pure_declaration_of_a_cataloged_entry_point_executes() {
    let compiled = Compiler::new()
        .model("Draw")
        .compile_str(&generator_source("pure", 2), "Draw.mo")
        .unwrap_or_else(|error| panic!("Draw compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.05,
            ..SimOptions::default()
        },
    )
    .expect("Draw simulates");
    let index = result.names.iter().position(|name| name == "r").unwrap();
    let last = *result.data[index].last().unwrap();
    assert!(
        last > 0.0 && last <= 1.0,
        "a unit-interval draw, got {last}"
    );
}

/// An impure declaration or an interface outside the catalog row is not the
/// row's body, so it stays a foreign body the Solve runtime refuses.
#[test]
fn an_unproven_interface_keeps_its_foreign_body() {
    for source in [generator_source("impure", 2), generator_source("pure", 3)] {
        let executed = Compiler::new()
            .model("Draw")
            .compile_str(&source, "Draw.mo")
            .is_ok_and(|compiled| {
                simulate_dae_with_diagnostics(
                    &compiled.dae,
                    &SimOptions {
                        t_end: 0.05,
                        ..SimOptions::default()
                    },
                )
                .is_ok()
            });
        assert!(!executed, "a foreign body never executes:\n{source}");
    }
}

/// A parameter binding evaluates the native body at initialization with the
/// same definitional evaluator the event algorithm uses.
#[test]
fn a_parameter_binding_evaluates_the_native_body() {
    let source = generator_source("pure", 2).replace(
        "  discrete Real r(",
        "  parameter Real p = xorshift({1, 1});\n  parameter Integer s0[2] = {1, 1};\n  Real q = p;\n  Real v = xorshift(s0);\n  discrete Real r(",
    );
    let compiled = Compiler::new()
        .model("Draw")
        .compile_str(&source, "Draw.mo")
        .unwrap_or_else(|error| panic!("Draw compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.05,
            ..SimOptions::default()
        },
    )
    .expect("Draw simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|candidate| candidate == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} is recorded"))]
    };
    let drawn = *column("r").last().unwrap();
    assert_eq!(column("q")[0].to_bits(), drawn.to_bits());
    assert_eq!(column("v")[0].to_bits(), drawn.to_bits());
}
