//! A connector whose flow can only enter its own component never supplies its
//! stream connection set (MLS 3.7 §15.2, SPEC_0022 STRM-013).
//!
//! `tank.port.m_flow` has `min = 0`, as the `Modelica.Fluid` AST_BatchPlant
//! tank top ports do, so it never delivers fluid to `pipe.b` and drops out of
//! `inStream(pipe.b.h_outflow)`. With no supplying peer left, the operator is
//! the connector's own `h_outflow`, as for an unconnected connector, so
//! `pipe.a.h_outflow = pipe.b.h_outflow + 10 = 300`. OpenModelica gives the
//! same values.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model StreamMin
  connector Port
    flow Real m_flow;
    stream Real h_outflow;
    Real p;
  end Port;
  model Tank "fluid only enters through its port"
    Port port(m_flow(min = 0));
  equation
    port.h_outflow = 100;
    port.p = 1;
  end Tank;
  model Source
    Port port;
  equation
    port.m_flow = -2;
    port.h_outflow = 300;
  end Source;
  model Pipe
    Port a;
    Port b;
  equation
    a.m_flow + b.m_flow = 0;
    a.p = b.p;
    a.h_outflow = inStream(b.h_outflow) + 10;
    b.h_outflow = inStream(a.h_outflow) - 10;
  end Pipe;
  Source src;
  Pipe pipe;
  Tank tank;
equation
  connect(src.port, pipe.a);
  connect(pipe.b, tank.port);
end StreamMin;
"#;

#[test]
fn a_receive_only_connector_drops_out_of_the_mixing_enthalpy() {
    let compiled = Compiler::new()
        .model("StreamMin")
        .compile_str(SOURCE, "StreamMin.mo")
        .unwrap_or_else(|error| panic!("StreamMin compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("StreamMin simulates: {error}"));
    let last = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        *result.data[index].last().expect("samples")
    };
    assert_eq!(last("pipe.b.h_outflow"), 290.0);
    assert_eq!(last("pipe.a.h_outflow"), 300.0);
}
