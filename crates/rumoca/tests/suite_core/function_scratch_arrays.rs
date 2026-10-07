//! MLS 3.7 sections 11.2.2, 11.2.6 and 12.4.4: a protected scratch array that
//! a loop fills element by element is defined for the reads that follow it,
//! on each outer iteration and under the selection that guards it.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const SOURCE: &str = r#"
function RepeatedScratch
  input Real values[:];
  output Real result[size(values,1)];
protected
  Real scratch[size(values,1)];
algorithm
  for i in 1:size(values,1) loop
    for j in 1:size(values,1) loop
      scratch[j] := values[j]-values[i];
    end for;
    result[i] := sum(scratch);
  end for;
end RepeatedScratch;

function ConditionalRepeatedScratch
  input Real values[:];
  input Boolean enabled;
  output Real result[size(values,1)];
protected
  Real scratch[size(values,1)];
algorithm
  result := zeros(size(values,1));
  if enabled then
    for i in 1:size(values,1) loop
      for j in 1:size(values,1) loop
        scratch[j] := values[j]-values[i];
      end for;
      if values[i] > 0 then
        result[i] := sum(scratch);
      end if;
    end for;
  end if;
end ConditionalRepeatedScratch;

function ConditionalScratch
  input Real values[:];
  input Boolean enabled;
  output Real result[size(values,1)];
protected
  Real scratch[size(values,1)];
  Real center;
algorithm
  result := zeros(size(values,1));
  if enabled then
    for i in 1:size(values,1) loop
      scratch[i] := 2*values[i];
    end for;
    for i in 1:size(values,1) loop
      center := scratch[i];
      if center > 0 then
        result[i] := center;
      end if;
    end for;
  end if;
end ConditionalScratch;

model Repeated
  Real result[3] = RepeatedScratch({-1, 2, 3});
end Repeated;

model ConditionalRepeated
  Real result[3] = ConditionalRepeatedScratch({-1, 2, 3}, time < 0.5);
end ConditionalRepeated;

model Conditional
  Real result[3] = ConditionalScratch({-1, 2, 3}, time < 0.5);
end Conditional;
"#;

fn result_at(model: &str, time: f64) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], time)
        .unwrap_or_else(|error| panic!("{model} evaluates: {error:?}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    (1..=3)
        .map(|index| {
            let name = format!("result[{index}]");
            probe
                .report
                .solver_y
                .iter()
                .find(|slot| slot.name.replace(' ', "") == name)
                .unwrap_or_else(|| panic!("solver value {name}"))
                .value
        })
        .collect()
}

#[test]
fn an_inner_loop_redefines_the_scratch_on_each_outer_iteration() {
    assert_eq!(result_at("Repeated", 0.0), [7.0, -2.0, -5.0]);
}

#[test]
fn a_selected_outer_loop_keeps_the_inner_scratch_definitions() {
    assert_eq!(result_at("ConditionalRepeated", 0.0), [0.0, -2.0, -5.0]);
    assert_eq!(result_at("ConditionalRepeated", 1.0), [0.0; 3]);
}

#[test]
fn a_selected_sequence_reads_the_scratch_its_earlier_loop_filled() {
    assert_eq!(result_at("Conditional", 0.0), [0.0, 4.0, 6.0]);
    assert_eq!(result_at("Conditional", 1.0), [0.0; 3]);
}
