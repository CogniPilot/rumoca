use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const GUARDED_RETURN: &str = r#"
function magnitude
  input Real x;
  output Real y;
algorithm
  if x > 0 then
    y := x;
    return;
  end if;
  y := -x;
end magnitude;

model GuardedReturn
  Real x(start = -0.5, fixed = true);
  Real y;
equation
  der(x) = 1;
  y = magnitude(x);
end GuardedReturn;
"#;

#[test]
fn guarded_return_round_trips_and_remains_computable() {
    let compiled = Compiler::new()
        .model("GuardedReturn")
        .compile_str(GUARDED_RETURN, "guarded_return.mo")
        .expect("proved guarded return should construct checked DAE");
    let wire =
        serde_json::to_string(&compiled.dae).expect("guarded return should serialize through v11");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("wire-v11 reconstructs conditional result ownership");
    let simulation = simulate_dae(&decoded, &SimOptions::default())
        .expect("guarded return should lower to computable Solve IR");
    let x = simulation
        .names
        .iter()
        .position(|name| name == "x")
        .expect("state x is visible");
    let y = simulation
        .names
        .iter()
        .position(|name| name == "y")
        .expect("function result y is visible");
    for (x, y) in simulation.data[x].iter().zip(&simulation.data[y]) {
        assert!((y - x.abs()).abs() <= 1.0e-8, "magnitude({x}) = {y}");
    }
}

#[test]
fn partial_return_definition_fails_at_the_function_owner() {
    let error = Compiler::new()
        .model("InvalidReturn")
        .compile_str(
            r#"
function partial_output
  input Real x;
  output Real y;
algorithm
  if x > 0 then
    return;
  end if;
  y := -x;
end partial_output;
model InvalidReturn
  Real y;
equation
  y = partial_output(1);
end InvalidReturn;
"#,
            "invalid_return.mo",
        )
        .expect_err("return without a total output definition must fail closed");
    let message = error.to_string();
    assert!(
        message.contains("function return") && message.contains("define every output"),
        "the return owner should explain its missing definition: {message}"
    );
}
