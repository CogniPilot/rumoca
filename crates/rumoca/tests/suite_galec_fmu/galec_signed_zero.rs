//! Real negation of an expression keeps the IEEE sign of zero.
//!
//! GALEC unary minus binds only to references (SPEC_0042 T4), so the
//! projection rewrites `-(expr)`. `(-1.0) * (expr)` maps `+0.0` to `-0.0` as
//! Modelica negation does; `0.0 - (expr)` would yield `+0.0`.

use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};

const MODEL: &str = "SignedZero";

const SOURCE: &str = r#"
block SignedZero
  input Real x;
  output Real negative(start = 1.0, fixed = true);
  output Real product(start = 1.0, fixed = true);
algorithm
  when sample(0.0, 0.1) then
    negative := -sqrt(x);
    product := (-1.0)*sqrt(x);
  end when;
end SignedZero;
"#;

fn real_state(evaluator: &Evaluator<'_>, name: &str) -> f64 {
    match evaluator
        .state(name)
        .unwrap_or_else(|error| panic!("oracle state `{name}`: {error}"))
    {
        Value::Real(value) => *value,
        other => panic!("`{name}` must hold a Real, got {other:?}"),
    }
}

#[test]
fn negating_an_expression_at_positive_zero_yields_negative_zero() {
    let dir = tempfile::tempdir().expect("tempdir");
    let fixture = super::cli_support::write_fixture(dir.path(), MODEL, SOURCE);
    let compiled = rumoca::Compiler::new()
        .model(MODEL)
        .compile_path(&fixture)
        .expect("the fixture compiles");
    let package = lower_to_algorithm_code(
        &GalecInput::new(&compiled.dae, MODEL),
        &GalecOptions::default(),
    )
    .unwrap_or_else(|errors| panic!("GALEC projection accepts {MODEL}: {errors:?}"));
    let mut evaluator = Evaluator::new(package.checked_block(), IntegerDomain::signed_32())
        .unwrap_or_else(|error| panic!("the checked block builds an oracle: {error}"));
    evaluator.startup().expect("Startup");
    evaluator.recalibrate().expect("Recalibrate");
    for (input, expected) in [(0.0_f64, -0.0_f64), (4.0, -2.0)] {
        evaluator
            .set_state("x", Value::Real(input))
            .expect("the input is a block state");
        evaluator.do_step().expect("DoStep");
        for name in ["negative", "product"] {
            assert_eq!(
                real_state(&evaluator, name).to_bits(),
                expected.to_bits(),
                "`{name}` at x = {input}"
            );
        }
    }
}
