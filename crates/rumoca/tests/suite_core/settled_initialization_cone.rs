//! MLS §8.6 initialization through the settled view of a state without a start.
//!
//! `Modelica.Thermal.FluidHeatFlow.Examples.TwoTanks` gives each tank's mass
//! and enthalpy no start, so the initialization begins at `m = H = 0`, where
//! the enthalpy row `H = m*h` leaves `h` undetermined. The first
//! initialization block solves `m` from the fixed level alone; its Jacobian
//! reads only the level block, so the singular enthalpy block must not be
//! linearized there. Once `m` is settled, the temperature row solves `H`
//! through a regular enthalpy block.

use std::path::PathBuf;

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const MODEL: &str = "Modelica.Thermal.FluidHeatFlow.Examples.TwoTanks";

fn msl_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    root.exists().then_some(root)
}

#[test]
fn a_tank_without_a_mass_start_initializes_from_its_level_and_temperature() {
    let Some(root) = msl_root() else {
        return;
    };
    let compiled = Compiler::new()
        .model(MODEL)
        .source_root(root.to_str().expect("MSL root path is UTF-8"))
        .compile_str(
            "package TwoTanksProbe import Modelica; end TwoTanksProbe;",
            "settled_initialization_cone.mo",
        )
        .expect("TwoTanks compiles");
    let options = SimOptions {
        t_end: 0.1,
        ..Default::default()
    };
    let result = match simulate_dae_with_diagnostics(&compiled.dae, &options) {
        Ok(result) => result,
        Err(error) => panic!("TwoTanks initializes and simulates: {error}"),
    };
    for (name, expected) in [
        ("level1", 0.9),
        ("T1", 313.15),
        ("level2", 0.1),
        ("T2", 293.15),
    ] {
        let Some(index) = result.names.iter().position(|column| column == name) else {
            panic!("TwoTanks records {name}");
        };
        let initial = result.data[index][0];
        assert!(
            (initial - expected).abs() <= 1e-9 * expected,
            "{name} starts at {initial}, expected {expected}"
        );
    }
}

/// `Modelica.Thermal.FluidHeatFlow.Components.OpenTank`, written flat: the
/// states `m` and `H` have no start, and the fixed level and temperature are
/// algebraics read through the settled view. At the seed `m = H = 0` the
/// enthalpy `h = H/m` is undefined; it lies outside the read cone of the level
/// row, which projects `m` first, so its evaluation reconstructs only the level
/// block and never divides by the zero mass.
const OPEN_TANK: &str = r"
model OpenTankLevelInit
  parameter Real rho = 995.6;
  parameter Real area = 1.0;
  parameter Real cp = 4177.0;
  Real m;
  Real H;
  Real h;
  Real level(start = 0.5, fixed = true);
  Real T(start = 313.15, fixed = true);
equation
  m = rho*area*level;
  der(m) = 0;
  H = m*h;
  der(H) = 0;
  T = h/cp;
end OpenTankLevelInit;
";

#[test]
fn a_block_outside_the_level_rows_cone_is_not_reconstructed_at_the_seed() {
    let compiled = Compiler::new()
        .model("OpenTankLevelInit")
        .compile_str(OPEN_TANK, "OpenTankLevelInit.mo")
        .expect("the fixture compiles");
    let options = SimOptions {
        t_end: 0.1,
        ..Default::default()
    };
    let result = match simulate_dae_with_diagnostics(&compiled.dae, &options) {
        Ok(result) => result,
        Err(error) => panic!("the open tank initializes and simulates: {error}"),
    };
    for (name, expected) in [
        ("level", 0.5),
        ("T", 313.15),
        ("m", 497.8),
        ("H", 497.8 * 4177.0 * 313.15),
    ] {
        let Some(index) = result.names.iter().position(|column| column == name) else {
            panic!("the open tank records {name}");
        };
        let initial = result.data[index][0];
        assert!(
            (initial - expected).abs() <= 1e-9 * expected,
            "{name} starts at {initial}, expected {expected}"
        );
    }
}
