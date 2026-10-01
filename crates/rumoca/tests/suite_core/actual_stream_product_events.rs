//! MLS §15.3: `port.m_flow*actualStream(port.h_outflow)` is continuous, since
//! actualStream switches exactly where its own port's flow is zero, so the
//! product is lowered as `smooth(0, ..)` and owns no event. A standalone
//! actualStream keeps its flow-reversal event. Near a zero-flow equilibrium
//! (`Modelica.Fluid.Examples.Tanks.ThreeTanks`) the event on the product fired
//! at every round-off sign change of the flow and stalled the integration.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, lower_dae_for_simulation, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package S
  connector Port
    Real p;
    flow Real m_flow;
    stream Real h_outflow;
  end Port;
  model Source
    Port port;
    parameter Real h = 1;
  equation
    port.h_outflow = h;
    port.m_flow = -sin(time);
  end Source;
  model Volume
    Port port;
    Real H_flow;
    Real h_seen;
  equation
    port.p = 1;
    port.h_outflow = 2;
    H_flow = port.m_flow*actualStream(port.h_outflow);
    h_seen = actualStream(port.h_outflow);
  end Volume;
  model Top
    Source source;
    Volume volume;
  equation
    connect(source.port, volume.port);
  end Top;
end S;
"#;

#[test]
fn a_flow_weighted_actual_stream_owns_no_event() {
    let compiled = Compiler::new()
        .model("S.Top")
        .compile_str(SOURCE, "ActualStreamProduct.mo")
        .unwrap_or_else(|error| panic!("S.Top compiles: {error:?}"));
    let roots = lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .expect("S.Top lowers")
        .problem
        .events
        .root_conditions
        .output_count();
    assert_eq!(roots, 1, "only the standalone actualStream owns an event");

    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 10.0,
            ..Default::default()
        },
    )
    .expect("S.Top simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    for (index, time) in result.times.iter().enumerate() {
        let m_flow = time.sin();
        let upstream = if m_flow > 1e-9 {
            1.0
        } else if m_flow < -1e-9 {
            2.0
        } else {
            continue;
        };
        let h_flow = column("volume.H_flow")[index];
        assert!(
            (h_flow - m_flow * upstream).abs() < 1e-6,
            "H_flow({time}) = {h_flow}"
        );
        assert_eq!(column("volume.h_seen")[index], upstream, "h_seen({time})");
    }
}

/// The same product over a connector array: each element reads its own
/// flow, and no element owns an event.
const ARRAY_SOURCE: &str = r#"
package S2
  connector Port
    Real p;
    flow Real m_flow;
    stream Real h_outflow;
  end Port;
  model Source
    Port port;
    parameter Real h = 1;
    parameter Real k = 1;
  equation
    port.h_outflow = h;
    port.m_flow = -k*sin(time);
  end Source;
  model Volume
    Port ports[2];
    Real H_flow[2];
  equation
    for i in 1:2 loop
      ports[i].p = 1;
      ports[i].h_outflow = 2;
      H_flow[i] = ports[i].m_flow*actualStream(ports[i].h_outflow);
    end for;
  end Volume;
  model Top
    Source a;
    Source b(k = 2);
    Volume volume;
  equation
    connect(a.port, volume.ports[1]);
    connect(b.port, volume.ports[2]);
  end Top;
end S2;
"#;

#[test]
fn a_flow_weighted_actual_stream_over_a_connector_array_owns_no_event() {
    let compiled = Compiler::new()
        .model("S2.Top")
        .compile_str(ARRAY_SOURCE, "ActualStreamArrayProduct.mo")
        .unwrap_or_else(|error| panic!("S2.Top compiles: {error:?}"));
    let roots = lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .expect("S2.Top lowers")
        .problem
        .events
        .root_conditions
        .output_count();
    assert_eq!(roots, 0, "no flow-weighted actualStream owns an event");

    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 10.0,
            ..Default::default()
        },
    )
    .expect("S2.Top simulates");
    for (element, gain) in [(1, 1.0), (2, 2.0)] {
        let name = format!("volume.H_flow[{element}]");
        let index = result.names.iter().position(|n| *n == name);
        let column = &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))];
        for (sample, time) in result.times.iter().enumerate() {
            let m_flow = gain * time.sin();
            let upstream = if m_flow > 0.0 { 1.0 } else { 2.0 };
            let h_flow = column[sample];
            assert!(
                (h_flow - m_flow * upstream).abs() < 1e-6,
                "{name}({time}) = {h_flow}"
            );
        }
    }
}
