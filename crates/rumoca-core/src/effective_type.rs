//! Concrete type identity shared by typed instances and Flat IR.

use serde::{Deserialize, Deserializer, Serialize};

use crate::TypeId;

#[cfg(test)]
mod tests;

/// The resolved type of one concrete component shape.
///
/// `nominal_type` retains aliases, redeclarations, records, and operator-record
/// identity. `canonical_type` is the root used only for compatibility checks.
/// The complete fixed shape is part of effective identity per SPEC_0007.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct EffectiveType {
    nominal_type: TypeId,
    canonical_type: TypeId,
    dimensions: Box<[i64]>,
}

impl EffectiveType {
    /// Construct a resolved effective type.
    pub fn new(
        nominal_type: TypeId,
        canonical_type: TypeId,
        dimensions: impl Into<Box<[i64]>>,
    ) -> Result<Self, EffectiveTypeError> {
        if nominal_type.is_unknown() {
            return Err(EffectiveTypeError::UnknownNominalType);
        }
        if canonical_type.is_unknown() {
            return Err(EffectiveTypeError::UnknownCanonicalType);
        }
        let dimensions = dimensions.into();
        if dimensions.iter().any(|extent| *extent < 0) {
            return Err(EffectiveTypeError::NegativeExtent);
        }
        Ok(Self {
            nominal_type,
            canonical_type,
            dimensions,
        })
    }

    pub fn nominal_type(&self) -> TypeId {
        self.nominal_type
    }

    pub fn canonical_type(&self) -> TypeId {
        self.canonical_type
    }

    pub fn dimensions(&self) -> &[i64] {
        &self.dimensions
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EffectiveTypeError {
    UnknownNominalType,
    UnknownCanonicalType,
    NegativeExtent,
}

impl std::fmt::Display for EffectiveTypeError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            Self::UnknownNominalType => "the nominal type is unresolved",
            Self::UnknownCanonicalType => "the canonical type is unresolved",
            Self::NegativeExtent => "an effective type dimension is negative",
        };
        formatter.write_str(message)
    }
}

impl std::error::Error for EffectiveTypeError {}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct EffectiveTypeWire {
    nominal_type: TypeId,
    canonical_type: TypeId,
    dimensions: Box<[i64]>,
}

impl<'de> Deserialize<'de> for EffectiveType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let wire = EffectiveTypeWire::deserialize(deserializer)?;
        Self::new(wire.nominal_type, wire.canonical_type, wire.dimensions)
            .map_err(serde::de::Error::custom)
    }
}
