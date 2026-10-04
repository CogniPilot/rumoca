//! Integer-typed `sign` and `abs` through the GALEC projection.
//!
//! MLS 3.7 §3.7.1 types `sign(v)` Integer for either operand type, and
//! `abs(i)` Integer for an Integer operand, while GALEC `sign` and `absolute`
//! are Real-only (SPEC_0042 T8). The projection converts the exact Real
//! result, so the checked block assigns an Integer to each Integer
//! declaration and the GALEC oracle computes the Modelica values.

use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};

const MODEL: &str = "IntegerSign";

const SOURCE: &str = r#"
model IntegerSign
  discrete Real u(start = -2.0, fixed = true);
  discrete Integer s(start = 0, fixed = true);
  discrete Integer m(start = 0, fixed = true);
equation
  when sample(0.0, 0.1) then
    u = pre(u) + 1.0;
    s = sign(u);
    m = abs(pre(s) - 1);
  end when;
end IntegerSign;
"#;

fn integer_state(evaluator: &Evaluator<'_>, name: &str) -> i64 {
    match evaluator
        .state(name)
        .unwrap_or_else(|error| panic!("oracle state `{name}`: {error}"))
    {
        Value::Integer(value) => *value,
        other => panic!("`{name}` must hold an Integer, got {other:?}"),
    }
}

#[test]
fn integer_sign_and_abs_project_to_integer_values() {
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
    let mut ticks = Vec::new();
    for _ in 0..5 {
        evaluator.do_step().expect("DoStep");
        ticks.push((
            integer_state(&evaluator, "s"),
            integer_state(&evaluator, "m"),
        ));
    }
    assert_eq!(ticks, vec![(-1, 1), (0, 2), (1, 1), (1, 0), (1, 0)]);
}
