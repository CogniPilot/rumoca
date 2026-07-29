use rumoca::Compiler;
use rumoca_ir_dae::{DaeGeneration, DaeProvenanceOrigin, VariableRole};
use rumoca_sim::{SimOptions, simulate_dae};

const CONTINUOUS_ALGORITHM: &str = r#"
model ContinuousAlgorithm
  Real x(start = 1, fixed = true);
  Real y;
algorithm
  if x > 0 then
    y := 2 * x;
  else
    y := -x;
  end if;
equation
  der(x) = -y;
end ContinuousAlgorithm;
"#;

#[test]
fn continuous_model_algorithm_remains_an_algebraic_equation() {
    let compiled = Compiler::new()
        .model("ContinuousAlgorithm")
        .compile_str(CONTINUOUS_ALGORITHM, "continuous_algorithm.mo")
        .expect("continuous algorithm should construct checked DAE");

    compiled.dae.inspect(|view| {
        let y = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "y")
            .map(|(_, variable)| variable)
            .expect("algorithm target y remains in the checked catalog");
        assert_eq!(y.role(), VariableRole::Algebraic);
        let algorithm = (0..view.continuous_equation_count())
            .filter_map(|index| view.continuous_equation(index))
            .find(|equation| {
                equation.provenance().origin()
                    == DaeProvenanceOrigin::Generated(DaeGeneration::AlgorithmEquation)
            });
        assert!(
            algorithm
                .is_some_and(|equation| { view.source_text(equation.provenance()) == Some("x") }),
            "generated algorithm equation retains the responsible condition occurrence"
        );
    });

    let wire = serde_json::to_string(&compiled.dae)
        .expect("continuous algorithm should serialize through wire-v11");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("wire-v11 should reconstruct the checked owners");
    let simulation = simulate_dae(&decoded, &SimOptions::default())
        .expect("decoded algorithm equation should lower to computable Solve IR");
    let x = simulation
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state x is visible");
    let y = simulation
        .names
        .iter()
        .position(|name| name == "y")
        .expect("algorithm target y is visible");
    let x_final = simulation.data[x]
        .last()
        .copied()
        .expect("state trace is non-empty");
    let y_final = simulation.data[y]
        .last()
        .copied()
        .expect("algebraic trace is non-empty");
    assert!((x_final - (-2.0_f64).exp()).abs() <= 5.0e-4);
    assert!((y_final - 2.0 * x_final).abs() <= 5.0e-8);
}

#[test]
fn model_algorithm_read_before_definition_fails_in_dae_analysis() {
    let error = Compiler::new()
        .model("InvalidAlgorithmMemory")
        .compile_str(
            r#"
model InvalidAlgorithmMemory
  Real y;
algorithm
  y := y + 1;
end InvalidAlgorithmMemory;
"#,
            "invalid_algorithm_memory.mo",
        )
        .expect_err("an algorithm must not acquire implicit continuous memory");
    let message = error.to_string();
    assert!(
        message.contains("unsupported model algorithm")
            && message.contains("read before definition"),
        "the responsible algorithm owner should reject missing initialization: {message}"
    );
}

#[test]
fn mixed_continuous_event_algorithm_fails_before_construction() {
    let error = Compiler::new()
        .model("MixedAlgorithm")
        .compile_str(
            r#"
model MixedAlgorithm
  Real x;
  discrete Real z;
algorithm
  x := 1;
  when time > 0.5 then
    z := 1;
  end when;
end MixedAlgorithm;
"#,
            "mixed_algorithm.mo",
        )
        .expect_err("mixed partitions need an explicit checked atomic owner");
    let message = error.to_string();
    assert!(
        message.contains("mixed continuous/event algorithm")
            && message.contains("checked atomic owner"),
        "mixed ownership must fail at the source algorithm: {message}"
    );
}
