//! MLS 3.7 sections 10.4.1 and 11.2.2: a slice `a:b` has a fixed extent when
//! `b - a` is one constant on every value of the conditions its bounds select
//! on, even where each bound alone is piecewise in the loop binder. The
//! slice is read through one compact index map at run time.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

fn compile(source: &str) -> Result<rumoca::CompilationResult, String> {
    Compiler::new()
        .model("Probe")
        .compile_str(source, "Piecewise.mo")
        .map_err(|error| error.to_string())
}

fn result(compiled: &rumoca::CompilationResult) -> f64 {
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "result")
        .expect("result is a solver output")
        .value
}

fn two_blocks(end_arms: &str) -> String {
    format!(
        r#"
package P
  function Blocks
    output Real y;
  protected
    Real H[6,21];
    Integer offset;
    Integer stateOffset;
  algorithm
    H := zeros(6,21);
    for node in 1:2 loop
      offset := 3*(node-1);
      stateOffset := if node == 1 then 0 else 15;
      H[offset+1:offset+3, stateOffset+1:stateOffset+3] := identity(3);
      H[offset+1:offset+3, stateOffset+(if node == 1 then 7 else 4):stateOffset+({end_arms})] := 2*identity(3);
    end for;
    y := H[2,2] + 10*H[2,8] + 100*H[5,17] + 1000*H[5,20];
  end Blocks;
end P;
model Probe
  output Real result = P.Blocks();
end Probe;
"#
    )
}

#[test]
fn bounds_piecewise_in_the_binder_with_one_distance_define_a_fixed_slice() {
    let compiled = compile(&two_blocks("if node == 1 then 9 else 6"))
        .unwrap_or_else(|error| panic!("both subdomains have extent 3: {error}"));
    // Node 1: identity at columns 1:3, 2*identity at 7:9 (H[2,8] = 2).
    // Node 2: identity at columns 16:18 (H[5,17] = 1), 2*identity at 19:21 (H[5,20] = 2).
    assert_eq!(result(&compiled), 1.0 + 20.0 + 100.0 + 2000.0);
}

#[test]
fn a_distance_that_differs_between_subdomains_is_refused() {
    let error = compile(&two_blocks("if node == 1 then 9 else 7"))
        .map(|_| ())
        .expect_err("extent 3 on node 1 and 4 on node 2 has no fixed shape");
    assert!(error.contains("range"), "{error}");
}

fn three_way(second_end: &str) -> String {
    format!(
        r#"
package P
  function Rows
    output Real y;
  protected
    Real H[3,9];
  algorithm
    H := zeros(3,9);
    for node in 1:3 loop
      H[node, (if node == 1 then 1 elseif node == 2 then 4 else 7):(if node == 1 then 3 elseif node == 2 then {second_end} else 9)] := {{1.0,2.0,3.0}}*node;
    end for;
    y := H[1,3] + 10*H[2,5] + 100*H[3,9];
  end Rows;
end P;
model Probe
  output Real result = P.Rows();
end Probe;
"#
    )
}

#[test]
fn an_elseif_chain_of_bounds_splits_into_three_subdomains() {
    let compiled =
        compile(&three_way("6")).unwrap_or_else(|error| panic!("every arm has extent 3: {error}"));
    // Row 1 holds {1,2,3} at 1:3, row 2 {2,4,6} at 4:6, row 3 {3,6,9} at 7:9.
    assert_eq!(result(&compiled), 3.0 + 40.0 + 900.0);
}

#[test]
fn one_arm_with_another_extent_in_the_chain_is_refused() {
    let error = compile(&three_way("7"))
        .map(|_| ())
        .expect_err("the middle arm has extent 4");
    assert!(error.contains("range"), "{error}");
}
