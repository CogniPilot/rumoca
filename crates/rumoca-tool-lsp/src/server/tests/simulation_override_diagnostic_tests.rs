//! SPEC_0008 code identity for the LSP's parameter-override simulation path:
//! a code identifies the defect, not the reporting surface.

use super::*;

const DECAY_SOURCE: &str = "model Decay\n  parameter Real k = 1;\n  Real x(start=1);\nequation\n  der(x) = -k*x;\nend Decay;\n";

async fn compiled_decay_model(
    server: &ModelicaLanguageServer,
    temp: &Path,
) -> crate::server::simulation_jobs::SimulationCompileResult {
    let focus = temp.join("Decay.mo");
    std::fs::write(&focus, DECAY_SOURCE).expect("write focus");
    {
        let mut session = server.session.write().await;
        session.update_document(&focus.to_string_lossy(), DECAY_SOURCE);
    }
    server
        .compile_model_for_simulation("Decay", &focus.to_string_lossy())
        .await
        .expect("Decay compiles for simulation")
}

fn override_sim_options() -> SimOptions {
    SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    }
}

#[test]
fn unknown_parameter_override_reports_override_rejection_code() {
    run_async_test(async {
        let temp = new_temp_dir("unknown-parameter-override-code");
        let service = new_test_service();
        let server = service.inner();
        let compiled = compiled_decay_model(server, &temp).await;

        let error = simulate_dae_with_parameter_overrides(
            &compiled.dae,
            &override_sim_options(),
            &[("notAParam".to_string(), 1.0)],
        )
        .expect_err("an override naming a missing parameter must be rejected");

        // EX003 (override rejected), never EX001 ("numeric solver reported a
        // failure while integrating") — the solver never ran.
        assert_eq!(
            error.diagnostic_code(),
            "EX003",
            "unexpected diagnostic for override rejection: {error}"
        );
        assert!(
            error.to_string().contains("notAParam"),
            "override rejection must name the offending parameter: {error}"
        );
    });
}

#[test]
fn known_parameter_override_still_simulates() {
    run_async_test(async {
        let temp = new_temp_dir("known-parameter-override-simulates");
        let service = new_test_service();
        let server = service.inner();
        let compiled = compiled_decay_model(server, &temp).await;

        let sim = simulate_dae_with_parameter_overrides(
            &compiled.dae,
            &override_sim_options(),
            &[("k".to_string(), 2.0)],
        )
        .expect("a tunable parameter override must simulate");

        assert!(!sim.times.is_empty(), "override run must produce samples");
    });
}
