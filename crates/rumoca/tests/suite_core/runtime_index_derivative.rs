//! MLS 10.5: a runtime index on a derivative equation's value side stays dynamic.

use rumoca::Compiler;

fn source(equation: &str, next: usize) -> String {
    format!(
        r#"model IndexedDerivative
  Real x[2](each start=0,each fixed=true);
  Real values[2,2]=[time,3*time;2*time,4*time];
  Integer k(start=1,fixed=true);
equation
  {equation};
  when time>0.5 then k={next}; end when;
end IndexedDerivative;"#
    )
}

fn simulate(source: &str) -> Result<rumoca_sim::SimResult, rumoca_sim::SimError> {
    let dae = Compiler::new()
        .model("IndexedDerivative")
        .compile_str(source, "indexed_derivative.mo")
        .unwrap()
        .dae;
    rumoca_sim::simulate_dae(
        &dae,
        &rumoca_sim::SimOptions {
            t_end: 1.0,
            dt: Some(0.01),
            rtol: 1e-9,
            atol: 1e-10,
            ..Default::default()
        },
    )
}

#[test]
fn a_runtime_index_is_not_a_compile_time_derivative_projection() {
    for equation in [
        "der(x)={values[k,1],values[k,2]}",
        "{values[k,1],values[k,2]}=der(x)",
    ] {
        let result = simulate(&source(equation, 2)).unwrap();
        assert_eq!(result.times.last().copied(), Some(1.0));
        for (name, initial_rate) in [("x[1]", 1.0), ("x[2]", 3.0)] {
            let channel = result.names.iter().position(|n| n == name).unwrap();
            for (&actual, &time) in result.data[channel].iter().zip(&result.times) {
                let expected =
                    initial_rate * time * time / 2.0 + (time * time - 0.25).max(0.0) / 2.0;
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{equation}: {name} at {time}: {actual}, expected {expected}"
                );
            }
        }
    }
}

#[test]
fn an_invalid_runtime_derivative_index_cannot_complete() {
    let error = simulate(&source("der(x)={values[k,1],values[k,2]}", 3)).unwrap_err();
    assert!(
        matches!(error.kind(), rumoca_sim::SimError::ModelExchangeSession(_)),
        "{error}"
    );
    assert!(
        error.to_string().contains("non-finite derivative"),
        "{error}"
    );
}
