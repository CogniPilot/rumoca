//! MLS 3.7 §7.3: a call spelled through a package alias selects the package
//! that alias denotes in the calling instance. In
//! `Modelica.Fluid.Examples.HeatExchanger.BaseClasses.BasicHX` the pipe's
//! medium is `Medium_1`, whose default (`StandardWater`) is replaced by the
//! example's medium, and the flow model receives it through
//! `redeclare final package Medium = Medium`. The flow model's
//! `Medium.pressure(state)` must select the example's medium through its own
//! alias, not through the enclosing `Medium_1`, which keeps the call spelled
//! through the lexical alias whose state record has no fields.

use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

use super::msl_sim_regression::require_msl_compiler;

const SOURCE: &str = "package SelectedMedium
  model HX
    replaceable package Medium_1 = Modelica.Media.Water.StandardWater
      constrainedby Modelica.Media.Interfaces.PartialMedium;
    Modelica.Fluid.Pipes.DynamicPipe pipe_1(redeclare package Medium = Medium_1,
      length = 10, diameter = 0.05, nNodes = 2);
  end HX;
  model Top
    replaceable package Medium = Modelica.Media.Water.ConstantPropertyLiquidWater;
    inner Modelica.Fluid.System system(
      energyDynamics = Modelica.Fluid.Types.Dynamics.FixedInitial);
    Modelica.Fluid.Sources.Boundary_pT a(redeclare package Medium = Medium,
      p = 1.1e5, T = 300, nPorts = 1);
    Modelica.Fluid.Sources.Boundary_pT b(redeclare package Medium = Medium,
      p = 1e5, T = 300, nPorts = 1);
    HX hex(redeclare package Medium_1 = Medium);
  equation
    connect(a.ports[1], hex.pipe_1.port_a);
    connect(hex.pipe_1.port_b, b.ports[1]);
  end Top;
end SelectedMedium;
";

#[test]
fn a_flow_model_selects_the_medium_its_own_alias_denotes() {
    let compiled = require_msl_compiler()
        .model("SelectedMedium.Top")
        .compile_str(SOURCE, "SelectedMedium.mo")
        .unwrap_or_else(|error| panic!("{error}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("{error}"));
    let index = result
        .names
        .iter()
        .position(|name| name == "hex.pipe_1.flowModel.Fs_p[1]")
        .unwrap_or_else(|| panic!("Fs_p in {:?}", result.names));
    // Constant-density water: the pressure difference across the flow segment
    // is finite and the run completes.
    assert!(result.data[index].iter().all(|value| value.is_finite()));
}
