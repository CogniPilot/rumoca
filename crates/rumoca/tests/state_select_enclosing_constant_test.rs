use rumoca_compile::compile::{Session, SessionConfig, VariableRole};

#[test]
fn state_selection_is_reflected_in_checked_role_construction() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "state.mo",
            "model S Real x(stateSelect=StateSelect.always); equation der(x)=1; end S;",
        )
        .expect("fixture parses");
    let result = session.compile_model("S").expect("fixture compiles");
    result.dae.inspect(|view| {
        let state = view
            .variables()
            .find(|(_, variable)| variable.role() == VariableRole::State)
            .map(|(_, variable)| variable)
            .expect("derivative target is a checked state");
        assert_eq!(state.name().as_str(), "x");
    });
}
