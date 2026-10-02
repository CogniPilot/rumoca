//! Independent state selection settles each lower differential stage at a
//! trial point (SPEC_0053). A constraint that balances large terms, such as the
//! energy `Us[i] = ms[i]*mediums[i].u` of a water volume (about 5e8 J), keeps a
//! residual at its own rounding (about 1e-6) and cannot reach an absolute
//! 1e-10; the stage is settled once each residual is within the rounding of its
//! linearized term magnitudes. A stage with no real solution still fails.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

#[test]
fn a_large_magnitude_energy_constraint_settles_at_its_rounding() {
    let source = include_str!("../fixtures/index_reduction/WaterVolumes.mo");
    let compiled = Compiler::new()
        .model("WaterVolumes")
        .compile_str(source, "WaterVolumes.mo")
        .expect("WaterVolumes compiles");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("WaterVolumes simulates: {error}"));
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    for (row, &time) in result.times.iter().enumerate() {
        // Constant volume mass forces every mass flow to equal m[1] = sin(t),
        // and dT1/dt = sin(t)*(293.15 - T1)/4975.
        assert!((column("m[3]")[row] - time.sin()).abs() < 1e-6);
        let expected = 293.15 + 6.85 * (-(1.0 - time.cos()) / 4975.0).exp();
        let actual = column("mediums[1].T")[row];
        assert!(
            // The cooling over one second is about 4e-4 K; resolve it to 1 %.
            (actual - expected).abs() < 5e-6,
            "T1({time}) = {actual}, expected {expected}"
        );
    }
}

#[test]
fn a_stage_without_a_real_solution_is_still_rejected() {
    let source = include_str!("../fixtures/index_reduction/ImaginaryConstraint.mo");
    let compiled = Compiler::new()
        .model("ImaginaryConstraint")
        .compile_str(source, "ImaginaryConstraint.mo")
        .expect("ImaginaryConstraint compiles");
    let error = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect_err("x*x = -1 has no real trial point");
    assert!(
        error
            .to_string()
            .contains("trial constraints did not converge"),
        "{error}"
    );
}
