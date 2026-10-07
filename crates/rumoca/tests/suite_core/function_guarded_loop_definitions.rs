//! MLS 3.7 sections 11.2.2 and 11.2.6: a loop selected by one loop-invariant
//! runtime condition that writes several arrays element by element defines
//! each of them where it runs, so a read after the loop in the same selected
//! sequence sees every element.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

fn source(write_range: &str) -> String {
    format!(
        r#"
package V
  constant Integer n = 4;
  function Total
    input Real enabled[n];
    input Boolean requested;
    output Real y;
  protected
    Real mask[n]; Real scaled[n];
  algorithm
    y := 0;
    if requested then
      for slot in {write_range} loop
        mask[slot] := if enabled[slot] > 0.5 then 1.0 else 0.0;
        scaled[slot] := 2.0 * mask[slot];
      end for;
      y := sum(mask) + sum(scaled);
    end if;
  end Total;
end V;
model Guarded
  Real y = V.Total({{1, 0, 1, 1}}, true);
end Guarded;
"#
    )
}

#[test]
fn a_guarded_loop_with_several_array_targets_defines_each_of_them() {
    let compiled = Compiler::new()
        .model("Guarded")
        .compile_str(&source("1:n"), "Guarded.mo")
        .expect("the guarded loop defines both arrays before they are read");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the guarded loop DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let y = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name.replace(' ', "") == "y")
        .expect("solver value y")
        .value;
    assert_eq!(y, 3.0 + 6.0);
}

#[test]
fn a_guarded_loop_that_leaves_elements_undefined_is_refused() {
    let error = Compiler::new()
        .model("Guarded")
        .compile_str(&source("1:n-1"), "Guarded.mo")
        .expect_err("an element the guarded loop never writes is read");
    assert!(
        error.to_string().contains("do not all have a definition"),
        "{error}"
    );
}
