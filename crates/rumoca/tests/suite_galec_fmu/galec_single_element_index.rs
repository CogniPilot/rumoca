//! A runtime index into an axis of extent one selects its only element.
//!
//! A bounded selection over a single candidate has no branch to test: it is
//! the candidate itself (the value a longer selection gives as its
//! fallback). It once lowered to an if-expression with no branches, which
//! the Algorithm Code checker refuses (EG009), so a two-waypoint plan (one
//! segment) could not be exported while a three-waypoint plan could.

use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};

const MODEL: &str = "SingleElementDynamicIndex";

const SOURCE: &str = r#"
model SingleElementDynamicIndex
  function pick
    input Real a[1];
    input Integer i;
    output Real y;
  algorithm
    y := a[i];
  end pick;
  discrete Real y(start = 0.0, fixed = true);
  discrete Integer i(start = 1, fixed = true);
equation
  when sample(0.0, 0.1) then
    i = max(1, min(1, pre(i)));
    y = pick({pre(y) + 1.0}, pre(i));
  end when;
end SingleElementDynamicIndex;
"#;

#[test]
fn a_runtime_index_into_one_element_selects_it() {
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
    for step in 1..=3 {
        evaluator.do_step().expect("DoStep");
        let Value::Real(y) = evaluator.state("y").expect("y is a block state") else {
            panic!("y must hold a Real");
        };
        assert_eq!(*y, f64::from(step), "y after step {step}");
    }
}
