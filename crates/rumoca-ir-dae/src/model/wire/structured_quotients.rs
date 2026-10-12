//! Checked positional projection of structured quotient event boundaries.
use super::*;
use serde::ser::SerializeSeq;

pub(super) fn verify_surface(
    storage: &FrozenStorage,
    quotient: u32,
    generated: [u32; 6],
    domain: u32,
    root: u32,
) -> Result<(), String> {
    let root = storage
        .structured_roots
        .get(root as usize)
        .ok_or("structured quotient root is absent")?;
    let arena = &storage.expressions;
    if arena
        .binder_domains
        .get(quotient as usize)
        .copied()
        .flatten()
        != Some(domain)
        || arena
            .binder_domains
            .get(generated[5] as usize)
            .copied()
            .flatten()
            != Some(domain)
        || arena
            .function_scopes
            .get(quotient as usize)
            .copied()
            .flatten()
            .is_some()
        || root.domain != domain
        || root.expression != generated[5]
        || root.activation
            != arena.model_arm_scopes[quotient as usize]
                .map(|arm| storage.model_arms[arm as usize].guard)
        || arena.provenance.get(generated[5] as usize) != Some(&root.provenance)
    {
        return Err(
            "structured quotient boundary does not match its issued domain and batch".into(),
        );
    }
    Ok(())
}

pub(super) struct StructuredRootsOutput<'storage> {
    pub(super) roots: &'storage [StructuredRootEntry],
    pub(super) markers: &'storage rustc_hash::FxHashMap<u32, u32>,
}

impl Serialize for StructuredRootsOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut sequence = serializer.serialize_seq(Some(self.roots.len()))?;
        for (index, entry) in self.roots.iter().enumerate() {
            sequence.serialize_element(&StructuredRootOutput(
                entry,
                self.markers.get(&(index as u32)).copied(),
            ))?;
        }
        sequence.end()
    }
}

struct StructuredRootOutput<'storage>(&'storage StructuredRootEntry, Option<u32>);
impl Serialize for StructuredRootOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if let Some(owner) = self.1 {
            let mut state = serializer.serialize_struct_variant(
                "StructuredRootEntryWire",
                1,
                "quotient_owner",
                1,
            )?;
            state.serialize_field("owner", &owner)?;
            state.end()
        } else {
            let mut state =
                serializer.serialize_struct_variant("StructuredRootEntryWire", 0, "root", 3)?;
            state.serialize_field("domain", &self.0.domain)?;
            state.serialize_field("expression", &self.0.expression)?;
            state.serialize_field("provenance", &self.0.provenance)?;
            state.end()
        }
    }
}
