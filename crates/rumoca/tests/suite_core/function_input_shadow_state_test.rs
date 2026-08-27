use rumoca_compile::compile::{Session, SessionConfig};
use rumoca_sim::{SimOptions, simulate_dae};

#[test]
fn function_input_shadowing_is_preserved_through_checked_dae_and_solve() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "function.mo",
            r#"
function f
  input Real x;
  output Real y;
algorithm
  y := x + 1;
end f;
model UsesFunction
  Real x;
equation
  x = f(time);
end UsesFunction;
"#,
        )
        .expect("fixture parses");
    let compiled = session
        .compile_model("UsesFunction")
        .expect("function body should lower through its checked semantic owner");
    compiled
        .dae
        .inspect(|view| assert_eq!(view.function_count(), 1));

    let simulation = simulate_dae(&compiled.dae, &SimOptions::default())
        .expect("function model should simulate");
    let index = simulation
        .names
        .iter()
        .position(|name| name == "x")
        .expect("model x remains visible");
    let value = simulation.data[index]
        .last()
        .copied()
        .expect("simulation trace is non-empty");
    assert!(
        (value - 2.0).abs() <= 1.0e-10,
        "function-local input x must shadow model x, yielding f(time)=2 at t=1; got {value}"
    );
}
