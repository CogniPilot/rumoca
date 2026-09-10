//! The parameter binding view shared by the flatten context's evaluation
//! passes, plus the binding-shape predicates those passes classify with.

use super::*;

#[derive(Clone, Copy)]
pub(super) struct ParamBinding<'a> {
    pub(super) name: &'a str,
    pub(super) binding: &'a Expression,
    pub(super) may_be_record_alias: bool,
    pub(super) binding_from_modification: bool,
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
