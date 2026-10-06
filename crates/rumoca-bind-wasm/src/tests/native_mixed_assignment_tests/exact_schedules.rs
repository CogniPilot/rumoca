//! Every issued exact assignment sequence of a small plant executes in WASM
//! with the canonical interpreter's bits, immutable parameters and guards.
mod execution;
mod fixtures;
mod inventory;

use super::*;
use rumoca_ir_solve as solve;

const SOURCE: &str = r#"
model ExactPlant
  parameter Real mass = 2.0;
  parameter Real k = 3.0;
  parameter Real c = 0.4;
  Real x(start = 1.0, fixed = true);
  Real v(start = 0.0, fixed = true);
  Real spring;
  Real damper;
  Real force;
  Real a;
equation
  spring = -k * x;
  damper = -c * v * abs(v);
  force = spring + damper + 0.1 * sin(time);
  a = force / mass;
  der(x) = v;
  der(v) = a;
end ExactPlant;
"#;

#[test]
fn exact_schedules_match_canonical_bits_for_every_issued_dispatch() {
    let _lock = session_test_guard();
    let baseline = inspect(SOURCE);
    let edited = inspect(&SOURCE.replacen("mass = 2.0", "mass = 2.4", 1));
    for report in [&baseline, &edited] {
        assert!(report.admitted > 0, "no issued exact sequence admitted");
        assert_eq!(
            report.failures, 0,
            "admitted sequences diverge from canonical bits"
        );
    }
    assert_eq!(baseline.outputs.len(), edited.outputs.len());
    assert_ne!(
        baseline.outputs, edited.outputs,
        "the parameter edit reaches admitted native outputs"
    );
}

fn inspect(source: &str) -> inventory::Report {
    let mut report = None;
    crate::native_assignment_api::with_prepared_native_model(
        source,
        "ExactPlant",
        |model, _, _| {
            report = Some(inventory::inspect(model));
            Ok(String::new())
        },
    )
    .unwrap();
    report.expect("the prepared plant was inspected")
}
