//! Input batches are atomic component writes with one history correlation.

use super::*;

#[test]
fn unchanged_inputs_preserve_history_even_before_the_first_host_write() {
    let model = branch_component_with_input();
    let mut retained = retained_branch_component(&model);
    let mut session = faulty_session(
        &mut retained,
        PluginFault::FailsEveryInitializeAfterTheFirst,
    );
    session
        .set_inputs(&[])
        .expect("an empty live batch is valid");
    for _ in 0..3 {
        session
            .set_input("u", 1.0)
            .expect("the component already has this input");
    }
    assert_eq!(session.visible_values().unwrap()["u"], 1.0);
    session
        .set_input("u", 2.0)
        .expect_err("a changed input still restarts the plugin");
    assert!(
        session.visible_values().is_err(),
        "failed correlation ends the session"
    );
}

#[test]
fn a_whole_input_batch_restarts_once_and_invalid_batches_change_nothing() {
    let mut model = branch_component_with_input();
    model.problem.solve_layout.compiled_parameter_len = 2;
    model
        .problem
        .solve_layout
        .input_scalar_names
        .push("v".to_string());
    model.parameters.push(3.0);
    let model = refresh_owned(model);
    let mut retained = retained_branch_component(&model);
    let plugin = FaultyPlugin::new(PluginFault::Healthy);
    let initializations = Rc::clone(&plugin.initializations);
    let mut session = plugin_session(&mut retained, plugin);
    assert_eq!(initializations.get(), 1);

    session.set_inputs(&[("u", 2.0), ("v", 4.0)]).unwrap();
    assert_eq!(
        initializations.get(),
        2,
        "two changed inputs need one restart"
    );
    session.set_inputs(&[("u", 2.0), ("v", 4.0)]).unwrap();
    assert_eq!(
        initializations.get(),
        2,
        "an unchanged batch preserves history"
    );
    for invalid in [
        [("u", 9.0), ("missing", 5.0)],
        [("u", 9.0), ("v", f64::NAN)],
        [("u", 9.0), ("v", f64::INFINITY)],
        [("u", 9.0), ("v", f64::NEG_INFINITY)],
    ] {
        assert!(session.set_inputs(&invalid).is_err());
        let values = session
            .visible_values()
            .expect("a rejected batch leaves the session live");
        assert_eq!((values["u"], values["v"]), (2.0, 4.0));
        assert_eq!(initializations.get(), 2);
    }
    session
        .set_inputs(&[("u", 5.0), ("u", 6.0), ("v", 0.0)])
        .unwrap();
    assert_eq!(session.visible_values().unwrap()["u"], 6.0);
    assert_eq!(
        initializations.get(),
        3,
        "duplicate names commit in source order once"
    );
    session.set_input("v", -0.0).unwrap();
    assert_eq!(initializations.get(), 4, "signed zero changes input bits");
    assert_eq!(
        session.visible_values().unwrap()["v"].to_bits(),
        (-0.0_f64).to_bits()
    );
    assert_eq!(
        session.verification_component_point(),
        session.verification_session_point()
    );
}
