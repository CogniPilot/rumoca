//! Flattened artifact-identity template-name grammar.
//!
//! A canonical logical artifact key `k` is exposed to a template only as the
//! reserved top-level scalar `__rumoca_artifact_identity_v1_k`. The versioned
//! prefix and the restricted key grammar make this mapping injective and keep
//! identity authority out of map-shaped template values. This grammar is owned
//! entirely by the compiler: the passive codegen phase names no identity.

/// Reserved top-level namespace for one target-declared artifact identity.
pub(in crate::codegen_target) const ARTIFACT_IDENTITY_TEMPLATE_NAME_PREFIX: &str =
    "__rumoca_artifact_identity_v1_";

/// Map one canonical logical artifact key to its exact template scalar name.
pub(in crate::codegen_target) fn artifact_identity_template_name(key: &str) -> Option<String> {
    is_canonical_artifact_identity_key(key)
        .then(|| format!("{ARTIFACT_IDENTITY_TEMPLATE_NAME_PREFIX}{key}"))
}

/// Recover a canonical logical key from an exact flattened template name.
pub(in crate::codegen_target) fn artifact_identity_template_key(name: &str) -> Option<&str> {
    let key = name.strip_prefix(ARTIFACT_IDENTITY_TEMPLATE_NAME_PREFIX)?;
    is_canonical_artifact_identity_key(key).then_some(key)
}

/// Whether a top-level name occupies the reserved identity namespace.
pub(in crate::codegen_target) fn is_artifact_identity_template_namespace(name: &str) -> bool {
    name.starts_with(ARTIFACT_IDENTITY_TEMPLATE_NAME_PREFIX)
}

fn is_canonical_artifact_identity_key(key: &str) -> bool {
    let mut bytes = key.bytes();
    matches!(bytes.next(), Some(b'a'..=b'z' | b'_'))
        && bytes.all(|byte| matches!(byte, b'a'..=b'z' | b'0'..=b'9' | b'_'))
}
