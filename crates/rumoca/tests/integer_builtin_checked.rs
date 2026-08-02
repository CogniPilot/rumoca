use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

#[test]
fn integer_builtin_floors_through_wire_and_solve() {
    let compiled = Compiler::new()
        .model("IntegerBuiltin")
        .compile_str(
            r#"
model IntegerBuiltin
  parameter Real source = -1.8;
  parameter Integer converted = integer(source);
  Real y;
equation
  y = converted;
end IntegerBuiltin;
"#,
            "integer_builtin.mo",
        )
        .expect("integer() should have one typed checked DAE operation");
    let wire = serde_json::to_string(&compiled.dae).expect("integer() should serialize");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("wire-v11 should reconstruct integer()");
    let simulation = simulate_dae(&decoded, &SimOptions::default())
        .expect("integer() should lower to the Solve floor operation");
    let y = simulation
        .names
        .iter()
        .position(|name| name == "y")
        .expect("result y is visible");
    assert!(
        simulation.data[y]
            .iter()
            .all(|value| (*value - (-2.0)).abs() <= 1.0e-12)
    );
}
