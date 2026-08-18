//! A loop-local snapshot must not be replaced by a dependency that a later
//! indexed assignment mutates.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODEL: &str = r#"
within;
function preserveSnapshot
  input Real initialValues[2];
  output Real observed;
protected
  Real values[2];
  Real snapshot;
algorithm
  values := initialValues;
  snapshot := 0.0;
  for pass in 1:1 loop
    snapshot := values[1];
    if snapshot > 0.0 then
      for index in 1:2 loop
        values[index] := values[index] + 10.0;
      end for;
    end if;
  end for;
  observed := snapshot;
end preserveSnapshot;

model ObserveLoopSnapshot
  Real observed;
equation
  observed = preserveSnapshot({1.0, 2.0});
end ObserveLoopSnapshot;
"#;

#[test]
fn indexed_mutation_does_not_replace_an_earlier_snapshot_with_a_live_read() {
    let compiled = Compiler::new()
        .model("ObserveLoopSnapshot")
        .compile_str(MODEL, "ObserveLoopSnapshot.mo")
        .expect("the invariant guard should move inside the compact nested loop");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the checked snapshot DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let observed = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "observed")
        .expect("observed remains an algebraic result");
    assert_eq!(observed.value, 1.0);
}
