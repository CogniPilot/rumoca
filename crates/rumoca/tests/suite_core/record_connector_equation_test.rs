//! Regression for whole-record routing through short connector aliases.
//!
//! A connector such as `connector SampleInput = input Sample` retains the
//! canonical `Sample` record identity.  Its equation is one aggregate owner;
//! DAE construction derives one tensor-preserving equation for each declared
//! field instead of requiring source code to copy array elements in loops.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODEL: &str = r#"
within;

record Sample
  Real vector[2];
  Real bias;
end Sample;

connector SampleInput = input Sample;
connector SampleOutput = output Sample;

block Source
  SampleOutput signal;
equation
  signal.vector = {2.0, 3.0};
  signal.bias = 4.0;
end Source;

block Sink
  SampleInput signal;
  output Real value;
equation
  value = sum(signal.vector) + signal.bias;
end Sink;

model RecordConnectorEquation
  Source source;
  Sink sink;
  Real x(start=0.0, fixed=true);
equation
  sink.signal = source.signal;
  der(x) = sink.value;
end RecordConnectorEquation;
"#;

#[test]
fn whole_record_connector_equation_preserves_array_fields() {
    let compiler = Compiler::new().model("RecordConnectorEquation");
    let flat = compiler
        .compile_str_flat(MODEL, "record_connector_equation.mo")
        .expect("short record connectors should compile to Flat");
    for name in ["source.signal", "sink.signal"] {
        assert!(
            flat.record_instances
                .contains_key(&rumoca_core::VarName::new(name)),
            "{name} must retain its canonical record identity"
        );
    }

    let compiled = compiler
        .compile_str(MODEL, "record_connector_equation.mo")
        .expect("whole-record connector equality should compile to DAE");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("record-routed model should evaluate at t=0");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    let derivative = probe
        .report
        .derivatives
        .iter()
        .find(|slot| slot.name == "der(x)")
        .expect("missing der(x)")
        .value;
    assert_eq!(derivative, 9.0);
}
