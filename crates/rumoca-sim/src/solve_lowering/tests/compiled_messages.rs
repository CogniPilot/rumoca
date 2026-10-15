use super::*;

#[test]
fn native_numeric_warning_message_matches_explicit_reference_on_a_cold_activation() {
    let dae = compile(
        r#"
        model NumericWarning
          Real x(start=2, fixed=true);
        equation
          der(x) = 0;
          assert(time < 0.05,
            "x=" + String(x * 2, minimumLength=integer(x + 6),
              leftJustified=time < 0, significantDigits=integer(x + 1)),
            AssertionLevel.warning);
        end NumericWarning;
    "#,
        "NumericWarning",
    );
    let model = lower_dae_for_simulation(&dae, &SimOptions::default()).unwrap();
    assert!(
        model.problem.events.actions[0]
            .assertion_projection
            .is_none()
    );
    let backend = crate::native_execution::backend(&model.pure_calls).unwrap();
    let native =
        rumoca_solver::SolveRuntime::new_with_execution_backend(&model, Some(backend)).unwrap();
    let reference = rumoca_solver::SolveRuntime::new(&model).unwrap();
    for runtime in [&native, &reference] {
        runtime
            .observe_warnings(&model.initial_y, &model.parameters, 0.0)
            .unwrap();
        assert!(runtime.diagnostics().is_empty());
        runtime
            .observe_warnings(&model.initial_y, &model.parameters, 0.1)
            .unwrap();
        assert_eq!(runtime.diagnostics()[0].message, "x=       4");
        runtime
            .observe_warnings(&model.initial_y, &model.parameters, 0.2)
            .unwrap();
        assert_eq!(
            runtime.diagnostics().len(),
            1,
            "the warning remains once per site"
        );
    }
    assert_eq!(native.diagnostics(), reference.diagnostics());
}
