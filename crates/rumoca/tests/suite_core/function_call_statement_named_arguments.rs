//! Named arguments of a function call statement bind by input name (MLS
//! §12.4.1), exactly like the expression form of the call.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

fn source(call: &str) -> String {
    format!(
        r#"
within;
package P
  function Split
    input Real a;
    input Real b[2];
    input Real c = 100.0;
    output Real sum;
    output Real weighted[2];
    input Real d = 1000.0;
  algorithm
    sum := a + b[1] + b[2] + c + d;
    weighted := {{a * b[1], 10 * a * b[2]}};
  end Split;
  function Outer
    input Real x;
    output Real y[3];
  protected
    Real s;
    Real w[2];
  algorithm
    {call}
    y := {{s, w[1], w[2]}};
  end Outer;
end P;

model ObserveNamedStatement
  Real y[3] = P.Outer(2.0);
end ObserveNamedStatement;
"#
    )
}

#[test]
fn named_statement_arguments_bind_by_input_name() {
    let compiled = Compiler::new()
        .model("ObserveNamedStatement")
        .compile_str(
            &source("(s, w) := P.Split(d = 3.0, b = {5.0, 7.0}, a = x);"),
            "ObserveNamedStatement.mo",
        )
        .expect("named statement arguments bind by input name");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("the call statement DAE should evaluate");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let value = |name: &str| {
        probe
            .report
            .solver_y
            .iter()
            .find(|slot| slot.name.replace(' ', "") == name)
            .unwrap_or_else(|| panic!("missing solver value {name}"))
            .value
    };
    // a = 2, b = {5, 7}, c defaults to 100, d = 3.
    assert_eq!(value("y[1]"), 117.0);
    assert_eq!(value("y[2]"), 10.0);
    assert_eq!(value("y[3]"), 140.0);
}

#[test]
fn unknown_named_statement_argument_is_refused() {
    let error = Compiler::new()
        .model("ObserveNamedStatement")
        .compile_str(
            &source("(s, w) := P.Split(a = x, b = {5.0, 7.0}, e = 1.0);"),
            "ObserveNamedStatement.mo",
        )
        .expect_err("a named argument that names no input is an error");
    assert!(
        error.to_string().contains('e') && error.to_string().contains("named argument"),
        "unexpected diagnostic: {error}"
    );
}
