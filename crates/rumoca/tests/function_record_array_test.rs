//! Function-local arrays of records retain one compact typed owner.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODEL: &str = r#"
record Candidate
  Real length;
  Boolean feasible;
end Candidate;

function candidate
  input Real length;
  output Candidate result;
algorithm
  result.length := length;
  result.feasible := true;
end candidate;

function choose
  output Real selected;
protected
  Candidate candidates[2];
  Real weights[2];
algorithm
  candidates[1] := candidate(2.0);
  candidates[2] := candidate(1.0);
  weights := fill(1.0, 2);
  selected := min(
    candidates[1].length * weights[1],
    candidates[2].length * weights[2]);
end choose;

model FunctionRecordArray
  Real result;
equation
  result = choose();
end FunctionRecordArray;
"#;

#[test]
fn complete_record_element_writes_form_one_array_and_evaluate() {
    let compiled = Compiler::new()
        .model("FunctionRecordArray")
        .compile_str(MODEL, "FunctionRecordArray.mo")
        .expect("record arrays and fill extents should construct a checked DAE");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("checked record-array function should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let result = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "result")
        .expect("result remains an observable algebraic");
    assert!((result.value - 1.0).abs() < 1.0e-12, "{:?}", result.value);
}
