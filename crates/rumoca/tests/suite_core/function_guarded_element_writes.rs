//! MLS 3.6 §11.2.2: a `for` body runs once per range element in order, so an
//! element written by an earlier iteration keeps its value through later
//! iterations that write other elements. An array that no statement writes
//! before the loop is carried by the compact transition from its seed; a
//! guarded element write inside the body must update that carried value,
//! never a fresh seed, or only the last iteration's writes would survive.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODELS: &str = r#"
function guardedBlocks
  output Real y[4];
algorithm
  for i in 1:2 loop
    for ii in 1:4 loop
      if ii <= 2 * i and ii > 2 * (i - 1) then
        y[ii] := 10 * i + ii;
      end if;
    end for;
  end for;
end guardedBlocks;

model GuardedBlocks
  output Real y[4];
equation
  y = guardedBlocks();
end GuardedBlocks;

function guardedHalves
  output Real y[4];
algorithm
  for ii in 1:4 loop
    if ii <= 2 then
      y[ii] := ii;
    end if;
    if ii > 2 then
      y[ii] := -ii;
    end if;
  end for;
end guardedHalves;

model GuardedHalves
  output Real y[4];
equation
  y = guardedHalves();
end GuardedHalves;
"#;

fn read(model: &str) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "GuardedElementWrites.mo")
        .unwrap_or_else(|error| panic!("{model} should compile: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("{model} should evaluate: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    (1..=4)
        .map(|index| {
            let name = format!("y[{index}]");
            probe
                .report
                .solver_y
                .iter()
                .find(|slot| slot.name.replace(' ', "") == name)
                .unwrap_or_else(|| panic!("{model} has no {name}"))
                .value
        })
        .collect()
}

#[test]
fn guarded_element_writes_keep_earlier_iterations() {
    assert_eq!(read("GuardedBlocks"), vec![11.0, 12.0, 23.0, 24.0]);
    assert_eq!(read("GuardedHalves"), vec![1.0, 2.0, -3.0, -4.0]);
}
