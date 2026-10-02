//! Redeclare values that name a replaceable alias of the enclosing class
//! (MLS 3.7 §7.3).
//!
//! In `Inner a(redeclare package Medium = MA)`, `MA` denotes the class the
//! enclosing occurrence selected for it, which an outer redeclaration may have
//! replaced. `Modelica.Fluid` heat exchangers pass `Medium_1` and `Medium_2`
//! to their pipes this way, and the pipes forward `Medium` to their heat
//! transfer models; a call `Medium.f(...)` there must select the function of
//! the package the outermost redeclaration chose, not the alias's default.

use rumoca::Compiler;

const SOURCE: &str = r#"
package Base
  replaceable partial function f
    input Real x;
    output Real y;
  end f;
end Base;

package P1
  extends Base;
  redeclare function extends f
  algorithm
    y := 1;
  end f;
end P1;

package P2
  extends Base;
  redeclare function extends f
  algorithm
    y := 2;
  end f;
end P2;

model Transfer
  replaceable package Medium = P1;
  Real y = Medium.f(time);
end Transfer;

model Pipe
  replaceable package Medium = P1;
  Transfer transfer(redeclare package Medium = Medium);
  Real y = Medium.f(time);
end Pipe;

model Exchanger
  replaceable package MediumA = P1;
  replaceable package MediumB = P1;
  Pipe a(redeclare package Medium = MediumA);
  Pipe b(redeclare package Medium = MediumB);
end Exchanger;

model Plant
  Exchanger exchanger(redeclare package MediumA = P2, redeclare package MediumB = P2);
end Plant;
"#;

#[test]
fn redeclare_value_aliases_select_the_outer_redeclaration() {
    let compiled = Compiler::new()
        .model("Plant")
        .compile_str(SOURCE, "RedeclareValueScopeSelection.mo")
        .unwrap_or_else(|error| panic!("Plant compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.1,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("Plant simulates: {error:?}"));
    for name in [
        "exchanger.a.y",
        "exchanger.b.y",
        "exchanger.a.transfer.y",
        "exchanger.b.transfer.y",
    ] {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} in {:?}", result.names));
        assert_eq!(result.data[column][0], 2.0, "{name}");
    }
}
