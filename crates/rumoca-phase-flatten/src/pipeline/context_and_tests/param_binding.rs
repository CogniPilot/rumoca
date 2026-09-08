//! The parameter binding view shared by the flatten context's evaluation
//! passes, plus the binding-shape predicates those passes classify with.

use super::*;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum ParamPrimitive {
    Real,
    Integer,
    Boolean,
    Enumeration,
    Other,
    /// Test and recovery fixtures predating finalized effective-type metadata.
    Unknown,
}

impl ParamPrimitive {
    pub(super) fn from_variable(flat: &Model, variable: &flat::Variable) -> Self {
        let canonical_type = flat
            .effective_types
            .get(&variable.type_id)
            .map(rumoca_core::EffectiveType::canonical_type)
            .unwrap_or(variable.type_id);
        let predefined = flat.predefined_types;
        if !predefined.is_complete() || canonical_type.is_unknown() {
            Self::Unknown
        } else if canonical_type == predefined.real {
            Self::Real
        } else if canonical_type == predefined.integer {
            Self::Integer
        } else if canonical_type == predefined.boolean {
            Self::Boolean
        } else if flat.enumeration_type_roots.contains(&canonical_type)
            || flat.enumeration_types.contains(&variable.type_id)
        {
            Self::Enumeration
        } else {
            Self::Other
        }
    }

    pub(super) fn may_evaluate_as_real(self) -> bool {
        matches!(self, Self::Real)
    }

    pub(super) fn may_evaluate_as_integer(self) -> bool {
        matches!(self, Self::Integer)
    }

    pub(super) fn may_evaluate_as_boolean(self) -> bool {
        matches!(self, Self::Boolean)
    }

    pub(super) fn may_evaluate_as_enumeration(self) -> bool {
        matches!(self, Self::Enumeration)
    }
}

#[derive(Clone, Copy)]
pub(super) struct ParamBinding<'a> {
    pub(super) name: &'a str,
    pub(super) identity: rumoca_eval_flat::constant::ResolvedOccurrenceKey,
    pub(super) binding: &'a Expression,
    pub(super) primitive: ParamPrimitive,
    pub(super) may_be_record_alias: bool,
    pub(super) binding_from_modification: bool,
}

pub(super) fn is_array_literal_binding(binding: &Expression) -> bool {
    matches!(binding, Expression::Array { .. })
}
