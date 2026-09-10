//! The one current wire shape for [`Reference`], plus the identity contract
//! decoding replays.
//!
//! SPEC 0036 serialization rules apply here: the wire record is private, denies
//! unknown fields, states every field explicitly (including the ones that are
//! absent as `null`), and is admitted only through checked construction.
//!
//! There is deliberately no compatibility arm. The bare-name spelling this type
//! used to emit for undecorated references, and the shape-guessing decoder that
//! accepted it, are gone: a payload that never carried structure, an occurrence
//! identity, or a resolved function target can no longer be silently completed
//! with "none of those" at decode time. Omission is not ambiguity — it is a
//! decode error.

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use super::{ComponentReference, InstanceId, Reference, ResolvedFunctionReference, VarName};

/// The complete current reference record.
///
/// Private because the wire is not the IR. Every field is required: serde
/// reports an absent key as `missing field ...` rather than defaulting it.
///
/// The optional fields carry `required` explicitly. Serde's derive otherwise
/// completes an absent `Option` field with `None`, which is precisely the
/// missing-field acceptance this record must not have: "the producer did not
/// write an occurrence identity" and "the reference has no occurrence
/// identity" are different claims, and only the second one may be spelled.
#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ReferenceWire {
    name: VarName,
    #[serde(deserialize_with = "required")]
    component_ref: Option<ComponentReference>,
    #[serde(deserialize_with = "required")]
    resolved_function: Option<ResolvedFunctionReference>,
    #[serde(deserialize_with = "required")]
    instance_id: Option<InstanceId>,
    generated: bool,
}

/// Decode an optional wire field that must still be written out.
///
/// Naming a `deserialize_with` is what suppresses serde's implicit
/// `None`-for-absent completion; the value itself decodes normally, so an
/// explicit `null` is accepted and an omitted key is a `missing field` error.
fn required<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer)
}

/// Identity contract a [`Reference`] record must satisfy to be constructed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceContractError {
    /// A reference names something; the empty spelling names nothing.
    EmptyName,
    /// [`InstanceId::UNSET`] is the reserved "no allocated occurrence" value.
    ///
    /// Instantiation allocates occurrence identities from one and the Flat
    /// model shape contract rejects an unset identity on any variable or
    /// record instance, so a reference may not carry the sentinel as though it
    /// named a concrete occurrence.
    UnsetInstanceIdentity,
}

impl std::fmt::Display for ReferenceContractError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyName => formatter.write_str("reference requires a nonempty name"),
            Self::UnsetInstanceIdentity => formatter.write_str(
                "reference carries the reserved unset occurrence identity InstanceId(0)",
            ),
        }
    }
}

impl std::error::Error for ReferenceContractError {}

impl Reference {
    /// Construct a reference from a complete identity record.
    ///
    /// This is the checked entry point wire decoding replays, so a decoded
    /// reference is admitted on exactly the terms an in-process one is.
    pub fn construct(
        name: VarName,
        component_ref: Option<ComponentReference>,
        resolved_function: Option<ResolvedFunctionReference>,
        instance_id: Option<InstanceId>,
        generated: bool,
    ) -> Result<Self, ReferenceContractError> {
        if name.as_str().is_empty() {
            return Err(ReferenceContractError::EmptyName);
        }
        if instance_id.is_some_and(InstanceId::is_unset) {
            return Err(ReferenceContractError::UnsetInstanceIdentity);
        }
        Ok(Self {
            name,
            component_ref,
            resolved_function,
            instance_id,
            generated,
        })
    }
}

impl Serialize for Reference {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        ReferenceWire {
            name: self.name.clone(),
            component_ref: self.component_ref.clone(),
            resolved_function: self.resolved_function,
            instance_id: self.instance_id,
            generated: self.generated,
        }
        .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Reference {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let wire = ReferenceWire::deserialize(deserializer)?;
        Self::construct(
            wire.name,
            wire.component_ref,
            wire.resolved_function,
            wire.instance_id,
            wire.generated,
        )
        .map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests;
