use super::*;

#[test]
fn test_project_runtime_unfixes_state_dependency_chain() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.states.insert(
        dae::VarName::new("v"),
        dae::Variable::new(dae::VarName::new("v")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );

    // 0 = der(x) - v
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        var("v"),
    )));
    // 0 = der(v) - z
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("v")],
        },
        var("z"),
    )));
    // 0 = x - time  (directly assigned state)
    dae.f_x.push(eq_from(binop(
        dae::OpBinary::Sub(Default::default()),
        var("x"),
        var("time"),
    )));

    let timeout = crate::TimeoutBudget::new(None);
    let projected = project_algebraics_with_fixed_states_at_time(
        &dae,
        &[0.0, 0.0, 0.0],
        2,
        2.0,
        1e-9,
        &timeout,
    )
    .expect("runtime projection should not error")
    .expect("runtime projection should converge");

    assert!((projected[0] - 2.0).abs() < 1e-9);
    assert!(projected[1].abs() < 1e-9);
    assert!(projected[2].abs() < 1e-9);
}
