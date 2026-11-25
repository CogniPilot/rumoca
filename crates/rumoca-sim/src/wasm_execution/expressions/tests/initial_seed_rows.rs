//! The discrete rows initialization seeds (initial_seed_rows.rs) run as
//! compiled lifecycle programs, which check their inputs against the issued
//! layout: the seed pass hands them the solver vector, not an empty one.
use super::*;

const SOURCE: &str = r#"
model Selected
  parameter Real p[4] = {1, 2, 3, 4};
  input Integer k(start = 3);
  Integer k2;
  Real x(start = 0, fixed = true);
  output Real y;
equation
  der(x) = 1;
  k2 = k;
  y = p[k2];
end Selected;
"#;

#[test]
fn the_portable_backend_seeds_an_input_fixed_discrete_row_at_initialization() {
    let mut session = rumoca_compile::Session::new(Default::default());
    session.update_document("Selected.mo", SOURCE);
    let compiled = session.compile_model("Selected").unwrap();
    let options = rumoca_solver::SimOptions {
        initial_inputs: vec![("k".into(), 3.0)],
        ..Default::default()
    };
    let lowered =
        crate::lower_correlated_for_simulation_with_overrides(&compiled.dae, &options).unwrap();
    let model = lowered.model();
    let backend = Rc::new(WasmExecutionBackend::new(
        model.problem.layout.clone(),
        Rc::new(model.pure_calls.clone()),
    ));
    let runtime =
        rumoca_solver::SolveRuntime::new_with_execution_backend(model, Some(backend)).unwrap();
    assert!(
        !model.problem.initial_seed_discrete_rows().is_empty(),
        "the fixture has a seeded row"
    );
    let mut y = model.initial_y.to_vec();
    let mut p = model.parameters.to_vec();
    runtime
        .settle_initialization_system(&mut y, &mut p, 0.0, 1e-9, 32)
        .expect("initialization settles through the portable backend");
    // `k2 = k` holds before `y = p[k2]` is read: the seeded target carries 3.
    let seed = model.problem.initial_seed_discrete_rows()[0];
    assert_eq!(p[seed.target], 3.0);
}
