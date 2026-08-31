//! A collected Flat function exposes exactly one source declaration, which it
//! carries as its exposure identity. The `63_7xx` band names the declaration
//! this module writes.

use super::*;

#[test]
fn missing_call_provenance_cannot_mint_a_shape_certificate() {
    let function = rumoca_core::Function::new("f", rumoca_core::DefId::new(63_701), Span::DUMMY);
    let key = FunctionSpecializationKey {
        function: VarName::new("f"),
        inputs: Vec::new(),
        input_values: Vec::new(),
    };

    let flat = flat::Model::default();
    assert!(matches!(
        resolve_certificate(&flat, &function, key, Span::DUMMY, &ShapeEnvironment::default()),
        Err(ToDaeError::MissingProvenance { owner })
            if owner == "function specialization call"
    ));
}
