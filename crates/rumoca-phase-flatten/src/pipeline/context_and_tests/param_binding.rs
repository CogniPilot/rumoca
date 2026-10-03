//! The parameter binding view shared by the flatten context's evaluation
//! passes, plus the binding-shape predicates those passes classify with.

use super::*;

#[derive(Clone, Copy)]
pub(super) struct ParamBinding<'a> {
    pub(super) name: &'a str,
    pub(super) binding: &'a Expression,
    pub(super) may_be_record_alias: bool,
    pub(super) binding_from_modification: bool,
    /// The declared type is String or the declaration is an array, so the
    /// binding value has no scalar inventory.
    pub(super) aggregate: bool,
}

pub(super) fn is_array_literal_binding(binding: &Expression) -> bool {
    matches!(binding, Expression::Array { .. })
}

/// True when `binding` is a bare component reference rather than an
/// enumeration literal path (MLS 3.7 §4.8.5) or a composed expression.
pub(super) fn is_plain_component_reference(binding: &Expression) -> bool {
    matches!(
        binding,
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() && !looks_like_enum_literal_path(name.as_str())
    )
}

/// True when `var` is declared with the predefined String type.
pub(super) fn is_string_variable(flat: &Model, var: &flat::Variable) -> bool {
    flat.effective_types
        .get(&var.type_id)
        .is_some_and(|effective| effective.canonical_type() == flat.predefined_types.string)
}
