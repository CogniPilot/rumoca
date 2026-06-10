//! Test-only fixture helpers shared across the phase test modules.

/// Attaches the structured component reference that flatten output always
/// carries, derived from the fixture's flat name. Hand-built flat models
/// must mirror that contract or DAE conversion rejects them.
pub(crate) fn with_component_ref(
    mut variable: rumoca_ir_flat::Variable,
) -> rumoca_ir_flat::Variable {
    variable.component_ref =
        rumoca_core::component_reference_from_flat_name(&variable.name, rumoca_core::Span::DUMMY);
    variable
}
