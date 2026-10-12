//! The unchanged MSL viscosity path includes a mapped constant initializer.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const PROBE: &str = r#"
model ViscosityProbe
  package Medium = Modelica.Media.Incompressible.Examples.Glycol47;
  Medium.ThermodynamicState state = Medium.setState_pT(1.013e5, Medium.T_min + time);
  output Real y = Medium.dynamicViscosity(state);
end ViscosityProbe;
"#;

// Independent 70-digit least-squares fit of the nine authored Glycol47
// log(viscosity) samples against {1/T^2, 1/T, 1}, evaluated at 243.15 + time.
const VISCOSITY: [f64; 3] = [
    0.166_255_011_621_964_1,
    0.166_177_411_256_905_95,
    0.166_099_853_269_561_6,
];

#[test]
fn msl_glycol_viscosity_uses_its_checked_coefficient_interface() {
    let root = super::native_linear_solve::msl_root().expect("the required MSL fixture exists");
    let original = root.join("Modelica 4.1.0/Media/Incompressible.mo");
    for (model, observable) in [
        ("Modelica.Media.Incompressible.Examples.TestGlycol", "eta"),
        ("ViscosityProbe", "y"),
    ] {
        let compiler = Compiler::new()
            .model(model)
            .source_root(root.to_string_lossy().as_ref());
        let compiled = if model == "ViscosityProbe" {
            compiler.compile_str_dae(PROBE, "ViscosityProbe.mo")
        } else {
            compiler.compile_file_dae(original.to_string_lossy().as_ref())
        }
        .unwrap_or_else(|error| panic!("{model} compiles: {error}"));
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.01,
                dt: Some(0.005),
                ..SimOptions::default()
            },
        )
        .unwrap_or_else(|error| panic!("{model} simulates: {error}"));
        assert_eq!(result.times, [0.0, 0.005, 0.01]);
        for column in &result.data {
            assert_eq!(column.len(), result.times.len());
            assert!(column.iter().all(|value| value.is_finite()));
        }
        let column = result
            .names
            .iter()
            .position(|name| name == observable)
            .unwrap_or_else(|| panic!("{model} records {observable}"));
        for ((time, actual), expected) in
            result.times.iter().zip(&result.data[column]).zip(VISCOSITY)
        {
            assert!(
                (actual - expected).abs() < 1e-10,
                "{model}: {observable} at {time}: {actual} != {expected}"
            );
        }
    }
}
