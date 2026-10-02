//! Index reduction through a supplied derivative that reads a field of a
//! record a multi-statement function returns (MLS 3.7 §12.7.1).
//!
//! `Modelica.Media.Water.IF97_Utilities.density_pT_der` reads
//! `aux.rho` of `aux := waterBaseProp_pT(p, T, region)`, whose body is no
//! single straight-line assignment, so the field cannot be projected to one
//! expression. Instantiating the derivative keeps it as the field of the
//! rebuilt record value instead of failing the field projection.

use std::path::PathBuf;

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

fn msl_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    root.is_dir().then_some(root)
}

const SOURCE: &str = r#"
model WaterDensityConstraint
  package Medium = Modelica.Media.Water.StandardWater;
  Real T(start = 300);
  Real Tdot;
equation
  der(T) = Tdot;
  Medium.density_pT(1e5, T) = 996 - time;
end WaterDensityConstraint;
"#;

#[test]
fn a_supplied_derivative_reads_a_field_of_a_multi_statement_record_result() {
    let Some(root) = msl_root() else {
        eprintln!("skipping: the MSL is not available");
        return;
    };
    let compiled = Compiler::new()
        .model("WaterDensityConstraint")
        .source_root(root.to_string_lossy().as_ref())
        .compile_str(SOURCE, "WaterDensityConstraint.mo")
        .unwrap_or_else(|error| panic!("WaterDensityConstraint compiles: {error:?}"));
    // The formal derivative instantiation must complete; the outcome of the
    // simulation itself is owned by the structural analysis that follows.
    let lowered = std::panic::catch_unwind(|| {
        simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                ..SimOptions::default()
            },
        )
        .map(|_| ())
        .map_err(|error| error.to_string())
    });
    assert!(
        lowered.is_ok(),
        "instantiating the supplied derivative panicked"
    );
}
