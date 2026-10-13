//! Compile-time source provenance for CLI and binding artifacts.

include!(concat!(env!("OUT_DIR"), "/build_identity.rs"));

/// The identity of the source state this crate was built from.
///
/// `None` when the build could not determine it. Callers comparing two
/// artifacts MUST treat `None` as "cannot verify" and skip the comparison; two
/// `None`s are not evidence that the builds agree.
///
/// Pairs with the package version: the version says which release, this says
/// which build of it. Two artifacts reporting the same version and different
/// identities were built from different source states.
#[must_use]
pub fn build_identity() -> Option<&'static str> {
    BUILD_IDENTITY
}

/// The short parents of the built commit when it is a merge, first parent
/// first; empty for an ordinary commit and `None` when git cannot say.
///
/// A pull-request build compiles a synthetic merge of the request head into
/// its base. [`build_identity`] is that merge commit, which exists only on the
/// hosting service; the parents name the head and base it combines, so a
/// report can state both what was built and which head it came from.
#[must_use]
pub fn build_merge_parents() -> Option<&'static [&'static str]> {
    BUILD_MERGE_PARENTS
}

#[cfg(test)]
mod build_identity_tests;
