//! MLS 12.4.4: a function local has no implicit start value, so a read of a
//! value some path leaves undefined is refused, whether the path skips a
//! branch of a conditional or reaches the read on the first pass of a loop.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

/// A function over `(a, v, k)` whose algorithm is `body`, called with the
/// given inputs.
fn source(body: &str, (a, v, k): (bool, bool, i64)) -> String {
    format!(
        r#"
package P
  function F
    input Boolean a; input Boolean v; input Integer k;
    output Real y;
  protected
    Boolean g; Boolean h; Boolean u; Real x;
  algorithm
{body}
  end F;
end P;
model Probe
  input Boolean a = {a}; input Boolean v = {v}; input Integer k = {k};
  output Real result = P.F(a,v,k);
end Probe;
"#
    )
}

fn compile(body: &str, inputs: (bool, bool, i64)) -> Result<f64, String> {
    let compiled = Compiler::new()
        .model("Probe")
        .compile_str(&source(body, inputs), "P.mo")
        .map_err(|error| error.to_string())?;
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .map_err(|error| error.to_string())?;
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    Ok(probe
        .report
        .solver_y
        .iter()
        .find(|slot| slot.name == "result")
        .expect("result is a solver output")
        .value)
}

fn refused(body: &str, inputs: (bool, bool, i64)) {
    let error = compile(body, inputs).expect_err("the read of an undefined value is refused");
    assert!(
        error.contains("only some branches")
            || error.contains("do not all have a definition")
            || error.contains("on the first pass of a loop"),
        "{error}"
    );
}

const CHAIN: &str = "    y := 0;
    u := k > 1;
    h := k > 5;
    g := if a then v elseif u then h else false;";

fn chain(write: &str) -> String {
    format!("{CHAIN}\n    {write}\n    if g then y := x; end if;")
}

#[test]
fn a_multi_arm_chain_proves_no_operand_pair() {
    for write in [
        "if u then x := 1; end if;",
        "if h then x := 1; end if;",
        "if a then x := 1; end if;",
    ] {
        refused(&chain(write), (true, true, 0));
    }
}

#[test]
fn a_single_branch_conjunction_still_proves_its_operands() {
    let body = "    y := 0;
    g := if a then v else false;
    if v then x := 3; end if;
    if g then y := x; end if;";
    assert_eq!(compile(body, (true, true, 0)), Ok(3.0));
}

#[test]
fn a_chain_whose_every_true_branch_proves_the_guard_of_the_value_is_accepted() {
    // `g` holds on `a and v` or on `not a and u and v`: both prove `v`, the
    // condition x is defined under.
    let body = "    y := 0;
    u := k > 1;
    g := if a then v elseif u then v else false;
    if v then x := 4; end if;
    if g then y := x; end if;";
    assert_eq!(compile(body, (true, true, 0)), Ok(4.0));
    assert_eq!(compile(body, (false, true, 2)), Ok(4.0));
    assert_eq!(compile(body, (false, true, 0)), Ok(0.0));
    assert_eq!(compile(body, (true, false, 0)), Ok(0.0));
}

fn looped(body: &str) -> String {
    format!("    y := 0;\n{body}")
}

#[test]
fn a_read_before_the_first_write_of_a_loop_is_refused() {
    let cases = [
        "    for i in 1:3 loop
      y := y + x;
      if k > 1 then x := i; end if;
    end for;",
        "    u := true;
    for i in 1:3 loop
      if u then y := y + x; end if;
      x := i;
    end for;",
        "    u := true;
    for i in 1:3 loop
      if u then y := y + x; end if;
      if k > 1 then x := i; end if;
    end for;",
        "    u := true;
    for i in 1:3 loop
      if u then y := y + x; end if;
      if k > 1 then x := i; u := true; end if;
    end for;",
        "    for i in 1:3 loop
      if v then y := y + x; end if;
      if k > 1 then x := i; end if;
    end for;",
        "    u := false;
    for i in 1:3 loop
      if u then y := y + x; end if;
      if k > 1 then u := true; end if;
      if k > 2 then x := i; end if;
    end for;",
        "    u := true;
    for i in 1:3 loop
      if u then y := y + x; end if;
      if k > 1 then x := i; u := true; end if;
    end for;",
    ];
    for body in cases {
        let error = compile(&looped(body), (true, true, 2))
            .expect_err("a read before the first write of the loop is refused");
        assert!(error.contains("on the first pass of a loop"), "{error}");
    }
}

#[test]
fn a_loop_value_defined_before_each_read_is_accepted() {
    let before = "    x := 5;
    for i in 1:3 loop
      y := y + x;
      if k > 1 then x := i; end if;
    end for;";
    assert_eq!(compile(&looped(before), (true, true, 2)), Ok(8.0));
    let top = "    for i in 1:3 loop
      x := i;
      y := y + x;
    end for;";
    assert_eq!(compile(&looped(top), (true, true, 2)), Ok(6.0));
    let first_pass_skipped = "    for i in 1:3 loop
      if i > 1 then y := y + x; end if;
      x := i;
    end for;";
    assert_eq!(
        compile(&looped(first_pass_skipped), (true, true, 2)),
        Ok(3.0)
    );
}
