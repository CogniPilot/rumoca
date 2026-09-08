//! Rendered bytes that retain their exact construction-issued member witness.

use rumoca_core::TargetInvocationBrand;

use crate::{
    AlgorithmCodeArtifactLayoutMember, AlgorithmCodeTemplateFile, ProductionArtifactLayoutMember,
};

/// Non-authoritative rendered target text.
///
/// This carrier holds only the bytes a template produced. It names no output
/// path, no member identity, no artifact session, and no artifact identity, and
/// it retains no target-invocation brand. Binding these bytes to a checked
/// path, package member, or completed artifact is the exclusive responsibility
/// of the private fold in `rumoca-compile`; nothing this crate hands out can
/// perform that binding.
#[derive(Debug)]
pub struct UntrustedRenderedText(String);

impl UntrustedRenderedText {
    #[must_use]
    pub(crate) fn new(content: String) -> Self {
        Self(content)
    }

    /// Borrow the rendered bytes.
    #[must_use]
    pub fn content(&self) -> &str {
        &self.0
    }

    /// Consume the carrier, yielding the rendered bytes.
    #[must_use]
    pub fn into_content(self) -> String {
        self.0
    }
}

/// Rendered source-only Algorithm Code bytes retaining the exact checked file.
#[derive(Debug)]
pub struct RenderedAlgorithmCodeSourceFile<'inv, 'member, 'body> {
    _brand: TargetInvocationBrand<'inv>,
    _file: &'member AlgorithmCodeTemplateFile<'inv, 'body>,
    content: String,
}

impl<'inv, 'member, 'body> RenderedAlgorithmCodeSourceFile<'inv, 'member, 'body> {
    pub(crate) fn construct(
        brand: TargetInvocationBrand<'inv>,
        file: &'member AlgorithmCodeTemplateFile<'inv, 'body>,
        content: String,
    ) -> Self {
        Self {
            _brand: brand,
            _file: file,
            content,
        }
    }

    #[must_use]
    pub fn content(&self) -> &str {
        &self.content
    }

    #[must_use]
    pub fn into_completion_content(self) -> String {
        self.content
    }
}

/// Rendered standalone Algorithm Code bytes bound to one exact package member.
///
/// There is no path/content decomposition API: checksum and package assembly
/// must consume the retained member witness together with these bytes.
#[derive(Debug)]
pub struct RenderedPackagedAlgorithmCodeFile<'inv, 'member> {
    _brand: TargetInvocationBrand<'inv>,
    member: &'member AlgorithmCodeArtifactLayoutMember,
    content: String,
}

impl<'inv, 'member> RenderedPackagedAlgorithmCodeFile<'inv, 'member> {
    pub(crate) fn construct(
        brand: TargetInvocationBrand<'inv>,
        member: &'member AlgorithmCodeArtifactLayoutMember,
        content: String,
    ) -> Self {
        Self {
            _brand: brand,
            member,
            content,
        }
    }

    #[must_use]
    pub const fn member(&self) -> &'member AlgorithmCodeArtifactLayoutMember {
        self.member
    }

    #[must_use]
    pub fn content(&self) -> &str {
        &self.content
    }

    /// Transfer the rendered bytes into the compiler-owned exact-member
    /// completion. The caller must simultaneously retain the checked target
    /// step; architecture gates confine production use to that composition.
    #[must_use]
    pub fn into_completion_content(self) -> String {
        self.content
    }
}

/// Rendered Algorithm Code-side bytes bound to one exact correlated member.
#[derive(Debug)]
pub struct RenderedCorrelatedAlgorithmCodeFile<'inv, 'member> {
    _brand: TargetInvocationBrand<'inv>,
    member: &'member ProductionArtifactLayoutMember,
    content: String,
}

impl<'inv, 'member> RenderedCorrelatedAlgorithmCodeFile<'inv, 'member> {
    pub(crate) fn construct(
        brand: TargetInvocationBrand<'inv>,
        member: &'member ProductionArtifactLayoutMember,
        content: String,
    ) -> Self {
        Self {
            _brand: brand,
            member,
            content,
        }
    }

    #[must_use]
    pub const fn member(&self) -> &'member ProductionArtifactLayoutMember {
        self.member
    }

    #[must_use]
    pub fn content(&self) -> &str {
        &self.content
    }

    #[must_use]
    pub fn into_completion_content(self) -> String {
        self.content
    }
}

/// Rendered Production Code bytes bound to one exact correlated member.
#[derive(Debug)]
pub struct RenderedProductionCodeFile<'inv, 'member> {
    _brand: TargetInvocationBrand<'inv>,
    member: &'member ProductionArtifactLayoutMember,
    content: String,
}

impl<'inv, 'member> RenderedProductionCodeFile<'inv, 'member> {
    pub(crate) fn construct(
        brand: TargetInvocationBrand<'inv>,
        member: &'member ProductionArtifactLayoutMember,
        content: String,
    ) -> Self {
        Self {
            _brand: brand,
            member,
            content,
        }
    }

    #[must_use]
    pub const fn member(&self) -> &'member ProductionArtifactLayoutMember {
        self.member
    }

    #[must_use]
    pub fn content(&self) -> &str {
        &self.content
    }

    #[must_use]
    pub fn into_completion_content(self) -> String {
        self.content
    }
}
