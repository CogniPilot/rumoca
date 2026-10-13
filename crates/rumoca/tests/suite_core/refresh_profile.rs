//! A stateless model whose algebraic projection is one ranged declaration run
//! is issued compact refresh owners only for the retained native owner; the
//! scalar runtime that simulation uses is issued per-scalar owners.

use rumoca::Compiler;
use rumoca_sim::{
    SimOptions, lower_correlated_for_native_execution_with_overrides,
    lower_correlated_for_simulation_with_overrides,
};

const DECLARATION_RUN: &str = r#"
model DeclarationRun
  parameter Real p[3] = {1, 2, 3};
  Real y[3] = p;
end DeclarationRun;
"#;

fn owners_are_compact(native: bool) -> bool {
    let compiled = Compiler::new()
        .model("DeclarationRun")
        .compile_str(DECLARATION_RUN, "refresh_profile.mo")
        .expect("declaration run compiles");
    let options = SimOptions::default();
    let lowered = if native {
        lower_correlated_for_native_execution_with_overrides(&compiled.dae, &options)
    } else {
        lower_correlated_for_simulation_with_overrides(&compiled.dae, &options)
    }
    .expect("declaration run lowers");
    let compact = lowered
        .model()
        .problem
        .continuous
        .refresh_owners
        .has_compact_source_recipe();
    assert_eq!(
        compact,
        lowered
            .model()
            .artifacts
            .continuous
            .structural
            .checked_assignment()
            .is_some(),
        "checked assignment artifacts follow the compact owners",
    );
    compact
}

#[test]
fn simulation_lowering_issues_scalar_refresh_owners() {
    assert!(!owners_are_compact(false));
}

#[test]
fn native_lowering_issues_compact_refresh_owners() {
    assert!(owners_are_compact(true));
}

#[test]
fn a_declaration_run_simulates_on_the_scalar_runtime() {
    let compiled = Compiler::new()
        .model("DeclarationRun")
        .compile_str(DECLARATION_RUN, "refresh_profile.mo")
        .expect("declaration run compiles");
    let result = rumoca_sim::simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("declaration run simulates");
    let index = result.names.iter().position(|n| n == "y[2]").unwrap();
    assert!(result.data[index].iter().all(|value| *value == 2.0));
}
