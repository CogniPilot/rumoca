use rumoca_compile::compile::{Session, SessionConfig, VariableRole};

fn compile(source: &str, model: &str) -> rumoca_compile::compile::CompilationResult {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("tiered.mo", source)
        .expect("fixture parses");
    session
        .compile_model(model)
        .expect("fixture compiles through checked ToDAE")
}

#[test]
fn scalar_ode_is_balanced_and_retains_exact_source_provenance() {
    let result = compile(
        "model Decay parameter Real k=2; Real x(start=1); equation der(x)=-k*x; end Decay;",
        "Decay",
    );
    assert!(result.balance_detail.is_balanced());
    result.dae.inspect(|view| {
        assert_eq!(
            view.variables()
                .filter(|(_, variable)| variable.role() == VariableRole::State)
                .count(),
            1
        );
        assert_eq!(view.continuous_owner_count(), 1);
        assert!((0..view.expression_count()).any(|index| {
            let expression = view
                .expression(
                    view.expression_id(index)
                        .expect("dense expression identity"),
                )
                .expect("branded expression resolves");
            view.source_text(expression.provenance()) == Some("k")
        }));
    });
}

#[test]
fn array_declarations_remain_compact_checked_variables() {
    let result = compile(
        "model ArrayModel Real x[3]; equation x={1,2,3}; end ArrayModel;",
        "ArrayModel",
    );
    result.dae.inspect(|view| {
        let x = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "x")
            .map(|(_, variable)| variable)
            .expect("array variable retained");
        assert_eq!(x.value_type().dimensions(), &[3]);
        assert_eq!(x.scalar_count(), 3);
        assert_eq!(view.continuous_owner_count(), 1);
    });
}

#[test]
fn undefined_reference_fails_before_checked_dae_construction() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "undefined.mo",
            "model Undefined Real x; equation x=missing; end Undefined;",
        )
        .expect("fixture parses");
    let error = session
        .compile_model("Undefined")
        .expect_err("undefined reference cannot inhabit checked DAE");
    assert!(error.to_string().contains("unresolved"));
}
