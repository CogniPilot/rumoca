//! MLS §8.5 treats `when` bodies with implicit `noEvent`; body `if` guards
//! read the values solved at that instant, while the activation retains its edge.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};
use rumoca_solver::SimExecutionPolicy;

const SOURCE: &str =
    include_str!("../../../../docs/verification/pr427-workshop-ci/sampled-extrema-reproduction.mo");

#[test]
fn a_sampled_body_condition_does_not_create_a_crossing_root() {
    let compiled = Compiler::new()
        .model("SampledExtremaReproduction")
        .compile_str(SOURCE, "SampledExtremaReproduction.mo")
        .unwrap();
    compiled
        .dae
        .inspect(|view| assert_eq!(view.root_count(), 0));
}

#[test]
fn a_sampled_body_condition_reads_the_same_event_extremum() {
    assert_sampled_timestamps(SOURCE);
}

#[test]
fn explicit_noevent_keeps_the_same_sampled_body_values() {
    let source = SOURCE.replace(
        "if y_max > pre(y_max) then",
        "if noEvent(y_max > pre(y_max)) then",
    );
    assert_sampled_timestamps(&source);
}

fn assert_sampled_timestamps(source: &str) {
    let compiled = Compiler::new()
        .model("SampledExtremaReproduction")
        .compile_str(source, "SampledExtremaReproduction.mo")
        .unwrap();
    for execution_policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let result = simulate_dae(
            &compiled.dae,
            &SimOptions {
                t_end: 0.3,
                dt: Some(0.05),
                execution_policy,
                ..Default::default()
            },
        )
        .unwrap();
        for name in ["y_max", "t_max"] {
            let column = result.names.iter().position(|value| value == name).unwrap();
            for (time, expected) in [(0.05, 0.0), (0.15, 0.1), (0.25, 0.2), (0.3, 0.3)] {
                let row = result
                    .times
                    .iter()
                    .rposition(|value| *value <= time + 1e-10)
                    .unwrap();
                let actual = result.data[column][row];
                assert!(
                    (actual - expected).abs() < 1e-10,
                    "{execution_policy:?}/{name} at {time}: {actual}, expected {expected}"
                );
            }
        }
    }
}
