//! Hidden foreign library state threaded through explicit values (MLS 3.7
//! §12.3, §12.9; SPEC_0040 FLAT-C05).
//!
//! `ModelicaRandom.c` keeps its impure xorshift1024* generator in static
//! storage: `setInternalState` writes it and `impureRandom` advances it. The
//! state becomes one discrete Integer vector that the parameter binding
//! initializing it gives its start value and the one algorithm section
//! drawing from it updates, so each event draws exactly once.

use std::path::PathBuf;

use rumoca::Compiler;
use rumoca_core::native_body::{ForeignStateCell, NativeBody, NativeScalar};
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const PACKAGE: &str = r#"
  impure function setInternalState
    input Integer[33] rngState;
    input Integer id;
  external "C" ModelicaRandom_setInternalState_xorshift1024star(rngState, size(rngState, 1), id);
  end setInternalState;
  function initialize
    input Integer seed;
    output Integer id;
  protected
    Integer rngState[33];
  algorithm
    rngState := fill(seed, 33);
    id := 7;
    setInternalState(rngState, id);
  end initialize;
  impure function impureRandom
    input Integer id;
    output Real y;
  external "C" y = ModelicaRandom_impureRandom_xorshift1024star(id);
  end impureRandom;
  impure function impureRandomInteger
    input Integer id;
    input Integer imin;
    input Integer imax;
    output Integer y;
  protected
    Real r;
  algorithm
    r := impureRandom(id = id);
    y := min(imax, integer(r * (imax - imin + 1)) + imin);
  end impureRandomInteger;
"#;

fn model(name: &str, declarations: &str, body: &str) -> String {
    format!("model {name}\n{PACKAGE}\n{declarations}\n{body}\nend {name};\n")
}

fn draws_source() -> String {
    model(
        "ImpureDraws",
        "  parameter Integer id = initialize(5);\n  discrete Real r(start = 0, fixed = true);\n  discrete Integer i(start = 0, fixed = true);",
        "algorithm\n  when sample(0, 0.1) then\n    r := impureRandom(id);\n    i := impureRandomInteger(id, 1, 10);\n  end when;",
    )
}

fn simulate(source: &str, name: &str, t_end: f64) -> Result<rumoca_sim::SimResult, String> {
    let compiled = Compiler::new()
        .model(name)
        .compile_str(source, &format!("{name}.mo"))
        .map_err(|error| format!("{error:?}"))?;
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end,
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error:?}"))
}

fn column<'a>(result: &'a rumoca_sim::SimResult, name: &str) -> &'a [f64] {
    let index = result.names.iter().position(|candidate| candidate == name);
    &result.data[index.unwrap_or_else(|| panic!("{name} is recorded"))]
}

/// The value at the first output point after `time`.
fn after(result: &rumoca_sim::SimResult, name: &str, time: f64) -> f64 {
    let sample = result
        .times
        .iter()
        .position(|candidate| *candidate > time)
        .unwrap_or_else(|| panic!("an output point after {time}"));
    column(result, name)[sample]
}

/// The draws the C library makes after `initialize(5)`, from the catalog's
/// definitional evaluator.
fn expected_draws(count: usize) -> Vec<f64> {
    let integers = |values: &[i64]| -> Vec<NativeScalar> {
        values.iter().copied().map(NativeScalar::Integer).collect()
    };
    let set = NativeBody::Xorshift1024StarSetState
        .evaluate(&[
            &integers(&[5; 33]),
            &integers(&[33]),
            &integers(&[7]),
            &integers(&ForeignStateCell::Xorshift1024Star.initial_value()),
        ])
        .unwrap();
    let mut cell = set[0].clone();
    (0..count)
        .map(|_| {
            let outputs = NativeBody::Xorshift1024StarImpureDraw
                .evaluate(&[&integers(&[7]), &cell])
                .unwrap();
            cell = outputs[1].clone();
            match outputs[0][0] {
                NativeScalar::Real(value) => value,
                NativeScalar::Integer(_) => panic!("a Real draw"),
            }
        })
        .collect()
}

#[test]
fn impure_draws_advance_the_library_state_once_per_event() {
    let result = simulate(&draws_source(), "ImpureDraws", 0.15).expect("ImpureDraws simulates");
    let draws = expected_draws(4);
    let integer = |draw: f64| (1 + (draw * 10.0).trunc() as i64).min(10) as f64;
    for (event, time) in [0.0, 0.1].into_iter().enumerate() {
        assert_eq!(
            after(&result, "r", time).to_bits(),
            draws[2 * event].to_bits(),
            "r at {time}"
        );
        assert_eq!(
            after(&result, "i", time),
            integer(draws[2 * event + 1]),
            "i at {time}"
        );
    }
}

/// MSL `Modelica.Math.Random.Examples.GenerateRandomNumbers` draws the
/// values OpenModelica's run of the MSL 4.1 C sources reports.
#[test]
fn msl_impure_random_reproduces_the_c_sources() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    if !root.is_dir() {
        eprintln!("skipping: the MSL is not available");
        return;
    }
    let model = "Modelica.Math.Random.Examples.GenerateRandomNumbers";
    let compiled = Compiler::new()
        .model(model)
        .source_root(root.to_string_lossy().as_ref())
        .compile_str(
            "package RandomProbe import Modelica; end RandomProbe;",
            "foreign_library_state.mo",
        )
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.12,
            ..SimOptions::default()
        },
    )
    .expect("GenerateRandomNumbers simulates");
    for (time, r, i) in [
        (0.0, 0.7397046101410745, -944.0),
        (0.05, 0.6555718770098982, -153.0),
        (0.1, 0.78737376016686, 1725.0),
    ] {
        assert_eq!(
            format!("{:.15e}", after(&result, "rImpure", time)),
            format!("{r:.15e}"),
            "rImpure at {time}"
        );
        assert_eq!(after(&result, "iImpure", time), i, "iImpure at {time}");
    }
}

/// The foreign body's own check: drawing with an id the library was not
/// initialized with fails instead of producing a value.
#[test]
fn a_draw_without_initialization_fails() {
    let source = model(
        "Uninitialized",
        "  parameter Integer id = initialize(5);\n  discrete Real r(start = 0, fixed = true);",
        "algorithm\n  when sample(0, 0.1) then\n    r := impureRandom(id + 1);\n  end when;",
    );
    let error = simulate(&source, "Uninitialized", 0.05).unwrap_err();
    assert!(error.contains("not initialized"), "{error}");
}

fn refusal(source: &str, name: &str) -> String {
    let error = Compiler::new()
        .model(name)
        .compile_str(source, &format!("{name}.mo"))
        .err()
        .unwrap_or_else(|| panic!("{name} is refused"));
    format!("{error:?}")
}

/// Every position where the order of library-state accesses is unspecified
/// is refused (EF036).
#[test]
fn unordered_library_state_accesses_are_refused() {
    let declarations = "  parameter Integer id = initialize(5);\n  discrete Real r(start = 0, fixed = true);\n  discrete Real s(start = 0, fixed = true);";
    let cases = [
        (
            "TwoSections",
            "algorithm\n  when sample(0, 0.1) then\n    r := impureRandom(id);\n  end when;\nalgorithm\n  when sample(0, 0.1) then\n    s := impureRandom(id);\n  end when;",
            "two algorithm sections",
        ),
        (
            "InExpression",
            "algorithm\n  when sample(0, 0.1) then\n    r := 2 * impureRandom(id);\n  end when;",
            "inside an expression",
        ),
        (
            "InWhenEquation",
            "equation\n  when sample(0, 0.1) then\n    r = impureRandom(id);\n  end when;",
            "EF036",
        ),
    ];
    for (name, body, expected) in cases {
        let error = refusal(&model(name, declarations, body), name);
        assert!(
            error.contains("EF036") || error.contains(expected),
            "{name}: {error}"
        );
        assert!(error.contains(expected), "{name}: {error}");
    }
    let twice = model(
        "TwoInitializers",
        "  parameter Integer id = initialize(5);\n  parameter Integer other = initialize(6);\n  discrete Real r(start = 0, fixed = true);",
        "algorithm\n  when sample(0, 0.1) then\n    r := impureRandom(id);\n  end when;",
    );
    let error = refusal(&twice, "TwoInitializers");
    assert!(error.contains("two parameter bindings"), "{error}");
}

/// A reaching function is treated as impure whatever its written prefix
/// (MLS 3.7 §12.3, applied recursively), and its specialization is pure, so
/// the call-context proof is made before threading: a draw outside a `when`
/// statement, directly or through a wrapper without explicit purity, and a
/// draw from a body declared `pure` are refused.
#[test]
fn impure_call_contexts_are_proven_before_threading() {
    let wrappers = "  function wrapped\n    input Integer id;\n    output Real y;\n  algorithm\n    y := impureRandom(id);\n  end wrapped;\n  pure function declaredPure\n    input Integer id;\n    output Real y;\n  algorithm\n    y := impureRandom(id);\n  end declaredPure;";
    let declarations = format!(
        "{wrappers}\n  parameter Integer id = initialize(5);\n  discrete Real r(start = 0, fixed = true);"
    );
    let cases = [
        (
            "Direct",
            "algorithm\n  r := impureRandom(id);",
            "outside a `when` statement",
        ),
        (
            "InIf",
            "algorithm\n  if time > 0.5 then\n    r := impureRandom(id);\n  end if;",
            "outside a `when` statement",
        ),
        (
            "Wrapped",
            "algorithm\n  r := wrapped(id);",
            "outside a `when` statement",
        ),
        (
            "DeclaredPure",
            "algorithm\n  when sample(0, 0.1) then\n    r := declaredPure(id);\n  end when;",
            "declared `pure`",
        ),
    ];
    for (name, body, expected) in cases {
        let error = refusal(&model(name, &declarations, body), name);
        assert!(error.contains("EF036"), "{name}: {error}");
        assert!(error.contains(expected), "{name}: {error}");
    }
    let admitted = model(
        "WrappedInWhen",
        &declarations,
        "algorithm\n  when sample(0, 0.1) then\n    r := wrapped(id);\n  end when;",
    );
    let result = simulate(&admitted, "WrappedInWhen", 0.05).expect("a wrapped draw in a when");
    assert_eq!(
        after(&result, "r", 0.0).to_bits(),
        expected_draws(1)[0].to_bits()
    );
}
