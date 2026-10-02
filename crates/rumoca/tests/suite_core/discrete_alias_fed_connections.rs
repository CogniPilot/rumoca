//! A discrete alias `a = b` whose written side already has a definition
//! (a binding modification) defines `b` (MLS 3.7 Appendix B). When `b` is the
//! output of a connection set that no other producer reaches, the alias makes
//! `b` the set's producer, so each connection equation defines the side
//! farther from it. `Modelica.StateGraph.Interfaces.CompositeStepState` states
//! `suspend = subgraphStatePort.suspend` with `suspend` bound by the composite
//! step, and every step's `outerStatePort` joins that port's connection set.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
connector PortOut
  output Boolean suspend;
end PortOut;
connector PortIn
  input Boolean suspend;
end PortIn;
model Root
  output Boolean suspend = false;
  PortOut port;
equation
  suspend = port.suspend;
end Root;
model Step
  PortIn port;
  Boolean active = not port.suspend;
end Step;
model Chain
  Root root(suspend = time > 0.5);
  Step s1;
  Step s2;
  Step s3;
equation
  connect(s1.port, root.port);
  connect(root.port, s2.port);
  connect(s2.port, s3.port);
end Chain;
"#;

#[test]
fn an_alias_fed_connection_set_defines_every_member_once() {
    let compiled = Compiler::new()
        .model("Chain")
        .compile_str(SOURCE, "Chain.mo")
        .unwrap_or_else(|error| panic!("Chain compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("Chain simulates: {error}"));
    let at = |name: &str, sample: usize| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        result.data[index][sample]
    };
    let last = result.times.len() - 1;
    for step in ["s1", "s2", "s3"] {
        assert_eq!(
            at(&format!("{step}.active"), 0),
            1.0,
            "{step} starts active"
        );
        assert_eq!(
            at(&format!("{step}.active"), last),
            0.0,
            "{step} is suspended"
        );
    }
}
