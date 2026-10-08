//! MLS 3.7 sections 11.2.2, 11.2.6 and 12.4.4: when every branch of an
//! if-else inside a loop writes the same rows of an array, each row is
//! written whichever branch a binder value takes, so a read after the loop
//! under the guard that selected it sees every row. A branch set that is not
//! exhaustive, or whose branches write different elements, leaves rows
//! undefined and stays refused.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

fn source(first_write: &str, second_write: &str, with_else: bool) -> String {
    let otherwise = if with_else {
        format!("          else\n            {second_write}\n")
    } else {
        String::new()
    };
    format!(
        r#"
package Rows
  function Total
    input Integer a;
    output Real y;
  protected
    Real rel[2,3];
    Integer nodes[2];
  algorithm
    y := 0;
    if a >= 0 then
      nodes := {{a, 2}};
      for node in 1:2 loop
        if nodes[node] == 1 then
          {first_write}
{otherwise}        end if;
      end for;
      y := rel[1,1] + rel[2,3];
    end if;
  end Total;
end Rows;
model RowsProbe
  input Integer a = 1;
  output Real result;
equation
  result = Rows.Total(a);
end RowsProbe;
"#
    )
}

fn compile(source: &str) -> Result<rumoca::CompilationResult, String> {
    Compiler::new()
        .model("RowsProbe")
        .compile_str(source, "Rows.mo")
        .map_err(|error| error.to_string())
}

const ZEROS: &str = "rel[node,:] := zeros(3);";
const SCALED: &str = "rel[node,:] := {1.0,2.0,3.0}*nodes[node];";

#[test]
fn rows_written_by_every_branch_are_defined_under_the_selecting_guard() {
    let compiled = compile(&source(ZEROS, SCALED, true))
        .unwrap_or_else(|error| panic!("both branches write every row: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let result = probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "result")
        .expect("result is a solver output")
        .value;
    // a = 1: row 1 is zeros, row 2 is {1, 2, 3} * 2; rel[1,1] + rel[2,3].
    assert_eq!(result, 0.0 + 6.0);
}

#[test]
fn rows_written_by_only_some_branches_stay_refused() {
    let error = compile(&source(ZEROS, SCALED, false))
        .map(|_| ())
        .expect_err("row 2 is never written when no else branch exists");
    assert!(error.contains("only some branches"), "{error}");
}

#[test]
fn branches_writing_different_elements_stay_refused() {
    let error = compile(&source(ZEROS, "rel[node,1:2] := {1.0,2.0};", true))
        .map(|_| ())
        .expect_err("element [2,3] is written by neither branch");
    assert!(error.contains("do not all have a definition"), "{error}");
}
