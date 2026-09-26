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

/// A structural reconstruction (here the folding of a `final` parameter)
/// replays function-local record arrays with their extents, and the function
/// still evaluates at run time.
#[test]
fn a_reconstructed_record_array_keeps_its_extents() {
    let model = MODEL
        .replace("function choose\n  output", "function choose\n  input Real scale;\n  output")
        .replace("fill(1.0, 2)", "fill(scale, 2)")
        .replace(
            "  Real result;\nequation\n  result = choose();",
            "  final parameter Real k = 2;\n  Real result;\n  Real z;\nequation\n  result = choose(1 + time);\n  z = k * time;",
        );
    assert!(model.contains("choose(1 + time)") && model.contains("fill(scale, 2)"));
    let compiled = Compiler::new()
        .model("FunctionRecordArray")
        .compile_str(&model, "FunctionRecordArray.mo")
        .expect("the scaled record-array function constructs");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the reconstructed record-array function evaluates");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let result = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "result")
        .expect("result remains an observable algebraic");
    assert!((result.value - 1.0).abs() < 1.0e-12, "{:?}", result.value);
}
