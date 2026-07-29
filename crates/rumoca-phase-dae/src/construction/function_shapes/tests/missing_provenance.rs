use super::*;

#[test]
fn missing_call_provenance_cannot_mint_a_shape_certificate() {
    let function = rumoca_core::Function::new("f", Span::DUMMY);
    let key = FunctionSpecializationKey {
        function: VarName::new("f"),
        inputs: Vec::new(),
    };

    assert!(matches!(
        resolve_certificate(&function, key, Span::DUMMY, &ShapeEnvironment::new()),
        Err(ToDaeError::MissingProvenance { owner })
            if owner == "function specialization call"
    ));
}
