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

// MLS §§10.1, 12.4: a record-array component is a value when passed to a
// function, including when two instances share the same source declaration.
#[test]
fn whole_record_array_arguments_select_their_own_instances() {
    let source = r#"
record Item
  Real value;
end Item;
function total
  input Item items[:];
  output Real result;
algorithm
  result := 0;
  for i in 1:size(items, 1) loop
    result := result + items[i].value;
  end for;
end total;
model Part
  parameter Real scale;
  parameter Item items[2](value = {scale, 2 * scale});
  Real result = total(items);
end Part;
model WholeRecordArray
  Part left(scale = 1);
  Part right(scale = 10);
  Real x = left.result + right.result;
end WholeRecordArray;
"#;
    let compiled = Compiler::new()
        .model("WholeRecordArray")
        .compile_str(source, "WholeRecordArray.mo")
        .expect("whole record arrays should retain checked element owners");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("record array arguments should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let result = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "x")
        .expect("combined result");
    assert_eq!(result.value, 33.0);
}

#[test]
fn record_array_modifier_selects_rows_of_array_expression() {
    let source = r#"
record Item
  Real position[3];
end Item;
model ProjectArrayModifier
  parameter Real mounts[2,3] = {{2,4,6},{8,10,12}};
  parameter Item items[2](position = mounts / 2);
  Real result = items[1].position[2] + items[2].position[3];
end ProjectArrayModifier;
"#;
    let compiled = Compiler::new()
        .model("ProjectArrayModifier")
        .compile_str(source, "ProjectArrayModifier.mo")
        .expect("non-each expression modifiers must select one row per record");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("projected row values should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let result = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "result")
        .expect("result");
    assert_eq!(result.value, 8.0);
}

#[test]
fn concatenated_record_arrays_preserve_vector_fields_and_empty_operands() {
    let source = r#"
record Part
  Real mass = 1;
  Real position[3] = {0,0,0};
end Part;
function total
  input Part parts[:];
  output Real result;
algorithm
  result := 0;
  for i in 1:size(parts,1) loop
    result := result + parts[i].mass + sum(parts[i].position);
  end for;
end total;
model ConcatenatedRecords
  parameter Part head;
  parameter Part items[2](each mass = 2, position = {{2,4,6},{8,10,12}});
  parameter Part empty[0];
  Real result = total(cat(1, {head}, items, empty));
end ConcatenatedRecords;
"#;
    let compiled = Compiler::new()
        .model("ConcatenatedRecords")
        .compile_str(source, "ConcatenatedRecords.mo")
        .expect("record concatenation should compile");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("record concatenation should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let result = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "result")
        .expect("result");
    assert_eq!(result.value, 47.0);
}

#[test]
fn comprehension_in_structured_equation_has_its_own_domain_plan() {
    let source = r#"
model NestedReduction
  parameter Integer n = 2;
  Real values[n,3];
  Real sums[3];
equation
  values = {{1,2,3},{4,5,6}};
  for j in 1:3 loop
    sums[j] = sum(values[i,j] for i in 1:n);
  end for;
end NestedReduction;
"#;
    let compiled = Compiler::new()
        .model("NestedReduction")
        .compile_str(source, "NestedReduction.mo")
        .expect("nested reduction should compile");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("nested reduction should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    for (index, expected) in [5.0, 7.0, 9.0].iter().enumerate() {
        let name = format!("sums[{}]", index + 1);
        let result = probe
            .report
            .solver_y
            .iter()
            .find(|slot| slot.name == name)
            .expect("sum");
        assert!((result.value - expected).abs() < 1e-10);
    }
}
