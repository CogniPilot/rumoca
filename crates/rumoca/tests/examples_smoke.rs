use rumoca::Compiler;
use rumoca_sim::SimOptions;

#[test]
fn representative_scalar_model_compiles_and_simulates_through_checked_ir() {
    let source = "model Decay Real x(start=1); equation der(x)=-x; end Decay;";
    let result = Compiler::new()
        .model("Decay")
        .compile_str(source, "decay.mo")
        .expect("checked compiler pipeline succeeds");
    let simulation = rumoca_sim::simulate_dae(
        &result.dae,
        &SimOptions {
            t_end: 0.1,
            ..SimOptions::default()
        },
    )
    .expect("checked Solve IR is computable");

    assert!(!simulation.times.is_empty());
    assert!(
        simulation
            .data
            .iter()
            .flatten()
            .all(|value| value.is_finite())
    );
}
