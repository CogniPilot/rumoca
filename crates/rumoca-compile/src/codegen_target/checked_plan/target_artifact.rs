//! Closed target rendering and publication.
//!
//! This module is the only layer allowed to combine a strict compilation,
//! checked target declarations, generation-session facts, semantic renderers,
//! and the checksum web.  Public callers receive only completed bytes.

mod rendering;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use std::marker::PhantomData;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result, bail};
use rumoca_core::{TargetInvocationBrand, with_target_invocation_brand};
use rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile;
use rumoca_phase_galec::{GalecInput, GalecOptions};
use time::OffsetDateTime;
use time::format_description::well_known::Rfc3339;
use uuid::Uuid;

use super::{
    AlgorithmCodeSourceRenderPlanStep, CheckedAlgorithmCodeSourceFold,
    CheckedArtifactIdentityDependencies, CheckedDirectPackageFold, CheckedDirectPackageMemberPlan,
    CheckedDirectUnpackagedFold, CheckedPackagedAlgorithmCodeFold,
    CheckedPackagedAlgorithmCodeMemberPlan, CheckedSolveAlgorithmFold,
    CheckedSolveAlgorithmMemberPlan, CheckedTargetArtifactStem, CheckedTargetBundle,
    CheckedTargetMetadata, CheckedTargetPackageAsset, CheckedTargetPackagePath,
    CheckedTargetPackageProductPlan, CheckedTargetRenderPlan, CheckedUnpackagedTargetProductPlan,
    ClosedRenderPlanStep, DirectPackageRenderPlanStep, DirectTargetTemplate,
    DirectUnpackagedRenderPlanStep, PackagedAlgorithmCodeRenderPlanStep,
    SolveAlgorithmRenderPlanStep, TargetAlgorithmCodeArithmetic,
    validate_dae_render_capability_contract, validate_solve_capability_contract,
};
use crate::session::{CanonicalModelIdentity, StrictCompilation};

/// Exact UTC-second generation instant supplied to one artifact build.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArtifactGenerationInstant(Box<str>);

impl ArtifactGenerationInstant {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::str::FromStr for ArtifactGenerationInstant {
    type Err = ArtifactSessionInputError;

    fn from_str(source: &str) -> std::result::Result<Self, Self::Err> {
        let parsed = OffsetDateTime::parse(source, &Rfc3339)
            .map_err(|_| ArtifactSessionInputError::NonCanonicalGenerationInstant)?;
        let canonical = parsed
            .format(&Rfc3339)
            .map_err(|_| ArtifactSessionInputError::NonCanonicalGenerationInstant)?;
        if source.len() != 20
            || parsed.offset() != time::UtcOffset::UTC
            || parsed.nanosecond() != 0
            || canonical != source
        {
            return Err(ArtifactSessionInputError::NonCanonicalGenerationInstant);
        }
        Ok(Self(source.into()))
    }
}

/// Canonical UUID namespace seed supplied to one artifact build.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArtifactIdentitySeed {
    spelling: Box<str>,
    uuid: Uuid,
}

impl ArtifactIdentitySeed {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.spelling
    }
}

impl std::str::FromStr for ArtifactIdentitySeed {
    type Err = ArtifactSessionInputError;

    fn from_str(source: &str) -> std::result::Result<Self, Self::Err> {
        let uuid = Uuid::parse_str(source)
            .map_err(|_| ArtifactSessionInputError::NonCanonicalIdentitySeed)?;
        if uuid.hyphenated().to_string() != source {
            return Err(ArtifactSessionInputError::NonCanonicalIdentitySeed);
        }
        Ok(Self {
            spelling: source.into(),
            uuid,
        })
    }
}

/// Complete explicit presentation identity/time input for one artifact build.
///
/// There is intentionally no `Default`, ambient-clock constructor, or random
/// seed constructor in the artifact path.
#[derive(Debug, PartialEq, Eq)]
pub struct ArtifactSessionInput {
    generation_instant: ArtifactGenerationInstant,
    identity_seed: ArtifactIdentitySeed,
}

impl ArtifactSessionInput {
    #[must_use]
    pub const fn construct(
        generation_instant: ArtifactGenerationInstant,
        identity_seed: ArtifactIdentitySeed,
    ) -> Self {
        Self {
            generation_instant,
            identity_seed,
        }
    }

    #[must_use]
    pub const fn generation_instant(&self) -> &ArtifactGenerationInstant {
        &self.generation_instant
    }

    #[must_use]
    pub const fn identity_seed(&self) -> &ArtifactIdentitySeed {
        &self.identity_seed
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArtifactSessionInputError {
    NonCanonicalGenerationInstant,
    NonCanonicalIdentitySeed,
}

impl fmt::Display for ArtifactSessionInputError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NonCanonicalGenerationInstant => formatter.write_str(
                "artifact generation instant must use exact YYYY-MM-DDTHH:MM:SSZ UTC-second form",
            ),
            Self::NonCanonicalIdentitySeed => {
                formatter.write_str("artifact identity seed must be a lowercase hyphenated UUID")
            }
        }
    }
}

impl std::error::Error for ArtifactSessionInputError {}

#[derive(Debug)]
struct ArtifactSession<'inv> {
    generated_at: Box<str>,
    generation_tool: String,
    identities: BTreeMap<String, String>,
    model_qualified_name: String,
    artifact_stem: Arc<CheckedTargetArtifactStem>,
    brand: TargetInvocationBrand<'inv>,
}

impl<'inv> ArtifactSession<'inv> {
    fn construct(
        brand: TargetInvocationBrand<'inv>,
        input: ArtifactSessionInput,
        target: &CheckedTargetMetadata,
        model_identity: &CanonicalModelIdentity,
    ) -> Result<Self> {
        const IDENTITY_ALGORITHM_VERSION: &str = "rumoca-artifact-identity-v1";

        let ArtifactSessionInput {
            generation_instant,
            identity_seed,
        } = input;
        let mut identities = BTreeMap::new();
        let mut issued_values = BTreeSet::new();
        for identity_key in target.artifact_identity_keys() {
            let name = length_framed_identity_name(
                IDENTITY_ALGORITHM_VERSION,
                target.artifact_identity_scope(),
                model_identity,
                identity_key,
            );
            let identity = Uuid::new_v5(&identity_seed.uuid, &name)
                .hyphenated()
                .to_string();
            if !issued_values.insert(identity.clone()) {
                bail!(
                    "UUIDv5 artifact identity collision in checked target scope '{}'",
                    target.artifact_identity_scope().value()
                );
            }
            identities.insert(identity_key.clone(), identity);
        }
        Ok(Self {
            generated_at: generation_instant.0,
            generation_tool: format!("rumoca {}", env!("CARGO_PKG_VERSION")),
            identities,
            model_qualified_name: model_identity.components().join("."),
            artifact_stem: Arc::clone(model_identity.artifact_stem()),
            brand,
        })
    }

    fn scoped_artifact_identities(
        &self,
        dependencies: &CheckedArtifactIdentityDependencies,
    ) -> BTreeMap<String, String> {
        dependencies
            .keys()
            .iter()
            .map(|key| (key.clone(), self.identities[key].clone()))
            .collect()
    }

    /// Bind the exact target-aware facts for one file into the neutral phase
    /// carrier: `model_name`, `model_qualified_name`, and the immutable
    /// `artifact` projection, plus one flattened text scalar per scoped
    /// identity. The identity grammar is owned
    /// here; the passive phase names no identity.
    ///
    /// `model_name` is the artifact stem: it is interpolated into output paths
    /// and file names, so it carries the checked stem grammar rather than the
    /// source spelling. `model_qualified_name` is the dotted class name the
    /// model was compiled under. They are separate facts and must not be
    /// substituted for one another: FMI `modelDescription/@modelName` is
    /// defined as the name of the model in the originating modeling
    /// environment, so it takes `model_qualified_name`, while a path segment
    /// takes `model_name`.
    fn template_bindings(
        &self,
        identities: &BTreeMap<String, String>,
        checksums: &BTreeMap<String, String>,
    ) -> Result<rumoca_phase_codegen::TemplateBindings<'inv>> {
        use rumoca_phase_codegen::TemplateBindingValue;

        let stem = self.artifact_stem.as_str().to_owned();
        let mut bindings = rumoca_phase_codegen::TemplateBindings::construct(self.brand);
        bindings
            .bind("model_name", TemplateBindingValue::Text(stem.clone()))
            .context("bind model_name")?;
        bindings
            .bind(
                "model_qualified_name",
                TemplateBindingValue::Text(self.model_qualified_name.clone()),
            )
            .context("bind model_qualified_name")?;

        let artifact = BTreeMap::from([
            (
                "generated_at".to_owned(),
                TemplateBindingValue::Text(self.generated_at.to_string()),
            ),
            (
                "generation_tool".to_owned(),
                TemplateBindingValue::Text(self.generation_tool.clone()),
            ),
            ("artifact_stem".to_owned(), TemplateBindingValue::Text(stem)),
            (
                "checksums".to_owned(),
                TemplateBindingValue::Object(
                    checksums
                        .iter()
                        .map(|(key, value)| {
                            (key.clone(), TemplateBindingValue::Text(value.clone()))
                        })
                        .collect(),
                ),
            ),
        ]);
        bindings
            .bind("artifact", TemplateBindingValue::Object(artifact))
            .context("bind artifact projection")?;

        for (key, value) in identities {
            let name = super::super::artifact_identity_name::artifact_identity_template_name(key)
                .ok_or_else(|| {
                anyhow::anyhow!("artifact identity key '{key}' has no canonical flattened name")
            })?;
            bindings
                .bind(name, TemplateBindingValue::Text(value.clone()))
                .with_context(|| format!("bind artifact identity '{key}'"))?;
        }
        Ok(bindings)
    }
}

const _: () = assert!(usize::BITS <= u64::BITS);

fn length_framed_identity_name(
    algorithm_version: &str,
    target_scope: &super::TargetArtifactIdentityScope,
    model_identity: &CanonicalModelIdentity,
    identity_key: &str,
) -> Vec<u8> {
    let mut name = Vec::new();
    for frame in [
        algorithm_version.as_bytes(),
        target_scope.kind().as_str().as_bytes(),
        target_scope.value().as_bytes(),
    ] {
        push_identity_frame(&mut name, frame);
    }
    name.extend_from_slice(&(model_identity.components().len() as u64).to_be_bytes());
    for component in model_identity.components() {
        push_identity_frame(&mut name, component.as_bytes());
    }
    push_identity_frame(
        &mut name,
        model_identity.artifact_stem().as_str().as_bytes(),
    );
    push_identity_frame(&mut name, identity_key.as_bytes());
    name
}

fn push_identity_frame(target: &mut Vec<u8>, frame: &[u8]) {
    target.extend_from_slice(&(frame.len() as u64).to_be_bytes());
    target.extend_from_slice(frame);
}

#[derive(Debug)]
struct CompletedUnpackagedMember {
    path: String,
    content: String,
    mode: Option<u32>,
}

struct CompletedPackageContent(String);

/// One completed rendered file for in-memory consumers.
#[derive(Debug)]
pub struct CompletedRenderedFile {
    path: String,
    content: String,
    mode: Option<u32>,
}

impl CompletedRenderedFile {
    #[must_use]
    pub fn path(&self) -> &str {
        &self.path
    }

    #[must_use]
    pub fn content(&self) -> &str {
        &self.content
    }

    #[must_use]
    pub const fn mode(&self) -> Option<u32> {
        self.mode
    }
}

#[derive(Debug)]
struct CompletedPresentation {
    label: Box<str>,
    description: Option<Box<str>>,
    completion_message: rumoca_phase_codegen::PreparedCompletionMessage,
}

impl CompletedPresentation {
    fn construct(metadata: &CheckedTargetMetadata, session: &ArtifactSession) -> Result<Self> {
        Ok(Self {
            label: metadata.label().into(),
            description: metadata.description().map(Into::into),
            completion_message: rumoca_phase_codegen::PreparedCompletionMessage::construct(
                metadata.completion_message(),
                session.artifact_stem.as_str(),
                metadata.label(),
            )?,
        })
    }
}

/// Completed target bytes. No semantic product, template, file declaration,
/// checksum capability, or renderer survives this boundary.
pub enum CompletedTargetArtifact {
    Packaged(CompletedPackage),
    Unpackaged(CompletedUnpackaged),
}

impl CompletedTargetArtifact {
    #[must_use]
    pub fn label(&self) -> &str {
        match self {
            Self::Packaged(package) => package.label(),
            Self::Unpackaged(files) => files.label(),
        }
    }

    #[must_use]
    pub fn description(&self) -> Option<&str> {
        match self {
            Self::Packaged(package) => package.description(),
            Self::Unpackaged(files) => files.description(),
        }
    }

    /// Post-closure in-memory projection. Packaged assets remain private; the
    /// returned files are the rendered-file subsequence of the sole mixed plan.
    #[must_use]
    pub fn into_rendered_files(self) -> Vec<CompletedRenderedFile> {
        match self {
            Self::Packaged(package) => package.into_rendered_files(),
            Self::Unpackaged(files) => files.into_rendered_files(),
        }
    }

    #[cfg(feature = "fmu-packaging")]
    pub fn publish(self, host_output_root: &Path) -> Result<PublishedTargetArtifact> {
        match self {
            Self::Packaged(package) => package.publish(host_output_root),
            Self::Unpackaged(files) => files.publish(host_output_root),
        }
    }
}

struct CompletedMixedPlan {
    members: Box<[CompletedPackageMember]>,
}

enum CompletedPackageMember {
    File {
        path: Box<str>,
        content: CompletedPackageContent,
    },
    Asset {
        path: Arc<CheckedTargetPackagePath>,
        bytes: Arc<[u8]>,
    },
}

impl CompletedPackageMember {
    fn path(&self) -> &str {
        match self {
            Self::File { path, .. } => path,
            Self::Asset { path, .. } => path.as_str(),
        }
    }

    fn bytes(&self) -> &[u8] {
        match self {
            Self::File { content, .. } => content.0.as_bytes(),
            Self::Asset { bytes, .. } => bytes,
        }
    }
}

/// Completed package retaining the one exact target-issued mixed member order.
pub struct CompletedPackage {
    presentation: CompletedPresentation,
    root: PathBuf,
    archive: Option<PathBuf>,
    required_files: Box<[Box<str>]>,
    members: CompletedMixedPlan,
}

impl CompletedPackage {
    #[must_use]
    pub fn label(&self) -> &str {
        &self.presentation.label
    }

    #[must_use]
    pub fn description(&self) -> Option<&str> {
        self.presentation.description.as_deref()
    }

    /// Target-relative package root retained by the checked artifact plan.
    #[must_use]
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Optional target-relative archive path retained by the checked artifact plan.
    #[must_use]
    pub fn archive(&self) -> Option<&Path> {
        self.archive.as_deref()
    }

    #[must_use]
    pub fn required_files(&self) -> impl ExactSizeIterator<Item = &str> {
        self.required_files.iter().map(AsRef::as_ref)
    }

    #[must_use]
    pub fn members(&self) -> impl ExactSizeIterator<Item = CompletedArtifactMemberRef<'_>> {
        self.members
            .members
            .iter()
            .map(|member| CompletedArtifactMemberRef {
                path: member.path(),
                bytes: member.bytes(),
            })
    }

    #[must_use]
    pub fn into_rendered_files(self) -> Vec<CompletedRenderedFile> {
        self.members
            .members
            .into_vec()
            .into_iter()
            .filter_map(|member| match member {
                CompletedPackageMember::File { path, content } => Some(CompletedRenderedFile {
                    path: path.into_string(),
                    content: content.0,
                    mode: None,
                }),
                CompletedPackageMember::Asset { .. } => None,
            })
            .collect()
    }
}

/// Borrowed path and exact bytes from a completed package. File/asset origin is
/// intentionally erased because publication needs neither semantic identity.
#[derive(Clone, Copy)]
pub struct CompletedArtifactMemberRef<'a> {
    path: &'a str,
    bytes: &'a [u8],
}

impl<'a> CompletedArtifactMemberRef<'a> {
    #[must_use]
    pub const fn path(self) -> &'a str {
        self.path
    }

    #[must_use]
    pub const fn bytes(self) -> &'a [u8] {
        self.bytes
    }
}

/// Completed non-package target files in exact target render order.
pub struct CompletedUnpackaged {
    presentation: CompletedPresentation,
    members: Box<[CompletedUnpackagedMember]>,
}

impl CompletedUnpackaged {
    #[must_use]
    pub fn label(&self) -> &str {
        &self.presentation.label
    }

    #[must_use]
    pub fn description(&self) -> Option<&str> {
        self.presentation.description.as_deref()
    }

    #[must_use]
    pub fn files(&self) -> impl ExactSizeIterator<Item = CompletedRenderedFileRef<'_>> {
        self.members.iter().map(|member| CompletedRenderedFileRef {
            path: &member.path,
            content: &member.content,
            mode: member.mode,
        })
    }

    #[must_use]
    pub fn into_rendered_files(self) -> Vec<CompletedRenderedFile> {
        self.members
            .into_vec()
            .into_iter()
            .map(|member| CompletedRenderedFile {
                path: member.path,
                content: member.content,
                mode: member.mode,
            })
            .collect()
    }

    /// Transactionally replace exactly this target's current output files.
    /// Unrelated siblings beneath `host_output_root` are preserved.
    pub fn publish(self, host_output_root: &Path) -> Result<PublishedTargetArtifact> {
        publication::publish_unpacked(self, host_output_root)
    }
}

#[derive(Clone, Copy)]
pub struct CompletedRenderedFileRef<'a> {
    path: &'a str,
    content: &'a str,
    mode: Option<u32>,
}

impl<'a> CompletedRenderedFileRef<'a> {
    #[must_use]
    pub const fn path(self) -> &'a str {
        self.path
    }

    #[must_use]
    pub const fn content(self) -> &'a str {
        self.content
    }

    #[must_use]
    pub const fn mode(self) -> Option<u32> {
        self.mode
    }
}

#[derive(Debug)]
pub struct PublishedTargetArtifact {
    root: PathBuf,
    archive: Option<PathBuf>,
}

impl PublishedTargetArtifact {
    #[must_use]
    pub fn root(&self) -> &Path {
        &self.root
    }

    #[must_use]
    pub fn archive(&self) -> Option<&Path> {
        self.archive.as_deref()
    }
}

// Publication is intentionally implemented below the completed-artifact
// boundary. It receives no semantic renderer or target declaration.
mod publication;

#[cfg(test)]
mod identity_tests {
    use super::*;
    use crate::session::Session;

    macro_rules! assert_not_implemented {
        ($ty:ty, $bound:path) => {
            const _: fn() = || {
                trait AmbiguousIfImplemented<Marker> {
                    fn probe() {}
                }
                impl<T> AmbiguousIfImplemented<()> for T {}
                struct Implements;
                impl<T: $bound> AmbiguousIfImplemented<Implements> for T {}
                let _ = <$ty as AmbiguousIfImplemented<_>>::probe;
            };
        };
    }

    assert_not_implemented!(ArtifactSession<'static>, ::serde::Serialize);

    #[test]
    fn compiler_proves_artifact_session_has_no_serialization_authority() {}

    fn strict_compilation(source: &str, model: &str) -> StrictCompilation {
        let mut session = Session::default();
        session
            .add_document("artifact-identity.mo", source)
            .expect("parse artifact identity fixture");
        session
            .compile_model_strict(model)
            .unwrap_or_else(|report| panic!("strict artifact identity fixture: {report:#?}"))
    }

    fn components(values: &[&str]) -> Box<[Box<str>]> {
        values
            .iter()
            .map(|value| Box::<str>::from(*value))
            .collect()
    }

    #[test]
    fn canonical_stem_frames_component_boundaries_and_utf8_bytes() {
        let nested = CheckedTargetArtifactStem::from_model_components(&components(&["A", "B"]));
        let flat = CheckedTargetArtifactStem::from_model_components(&components(&["A_B"]));
        let left = CheckedTargetArtifactStem::from_model_components(&components(&["ab", "c"]));
        let right = CheckedTargetArtifactStem::from_model_components(&components(&["a", "bc"]));
        let quoted = CheckedTargetArtifactStem::from_model_components(&components(&["'A.B'"]));
        let unicode = CheckedTargetArtifactStem::from_model_components(&components(&["Δ"]));

        assert_eq!(nested.as_str(), "rm1_1_41_1_42");
        assert_eq!(flat.as_str(), "rm1_3_415f42");
        assert_ne!(nested.as_str(), flat.as_str());
        assert_ne!(left.as_str(), right.as_str());
        assert_eq!(quoted.as_str(), "rm1_5_27412e4227");
        assert_eq!(unicode.as_str(), "rm1_2_ce94");
    }

    #[test]
    fn direct_and_hashed_stem_namespaces_are_disjoint_and_bounded() {
        let direct = CheckedTargetArtifactStem::from_model_components(&components(&["short"]));
        let long_component = "x".repeat(128);
        let hashed =
            CheckedTargetArtifactStem::from_model_components(&components(&[&long_component]));

        assert!(direct.as_str().starts_with("rm1_"));
        assert_eq!(direct.as_str().as_bytes()[3], b'_');
        assert!(hashed.as_str().starts_with("rm1h_"));
        assert_eq!(hashed.as_str().as_bytes()[3], b'h');
        assert!(hashed.as_str().len() <= 120);
        assert_eq!(hashed.as_str().len(), 118);
    }

    #[test]
    fn uuid_preimage_separately_frames_exact_model_identity_and_stem() {
        let source = r#"
package A
  model B
  end B;
end A;
model A_B
end A_B;
model Other
end Other;
"#;
        let nested = strict_compilation(source, "A.B");
        let flat = strict_compilation(source, "A_B");
        let other = strict_compilation(source, "Other");
        let scope = super::super::TargetArtifactIdentityScope {
            kind: super::super::TargetArtifactIdentityScopeKind::BuiltinRegistryKey,
            value: "identity-test".into(),
        };
        let seed =
            Uuid::parse_str("9a7e81c1-19d4-4d8b-8a18-ea6380ce5d01").expect("fixed identity seed");
        let issue = |compilation: &StrictCompilation| {
            let preimage = length_framed_identity_name(
                "rumoca-artifact-identity-v1",
                &scope,
                compilation.canonical_model_identity(),
                "component",
            );
            Uuid::new_v5(&seed, &preimage)
        };

        assert_ne!(issue(&nested), issue(&flat));
        assert_ne!(issue(&nested), issue(&other));
        assert_ne!(issue(&flat), issue(&other));
    }

    #[test]
    fn template_bindings_expose_only_the_scoped_identity_and_the_four_artifact_facts() {
        let compilation = strict_compilation(
            "model Model\n  Real x(start = 0.0, fixed = true);\nequation\n  der(x) = 1.0;\nend Model;\n",
            "Model",
        );
        with_target_invocation_brand(|brand| {
            let catalog = vec!["alpha".to_owned(), "beta".to_owned()];
            let identities = BTreeMap::from([
                ("alpha".to_owned(), "alpha-identity-value".to_owned()),
                ("beta".to_owned(), "beta-identity-value".to_owned()),
            ]);
            let session = ArtifactSession {
                generated_at: "1970-01-01T00:00:00Z".into(),
                generation_tool: "rumoca-test".to_owned(),
                identities,
                model_qualified_name: "Model".to_owned(),
                artifact_stem: Arc::new(CheckedTargetArtifactStem::from_model_components(
                    &components(&["Model"]),
                )),
                brand,
            };

            // The file depends on exactly one of the two catalog identities.
            let dependencies = CheckedArtifactIdentityDependencies::construct(
                vec!["alpha".to_owned()],
                &catalog,
                "path",
            )
            .expect("a single declared dependency names a catalog identity");
            let scoped = session.scoped_artifact_identities(&dependencies);
            assert_eq!(
                scoped.keys().cloned().collect::<Vec<_>>(),
                vec!["alpha".to_owned()],
                "scoping yields exactly the declared identity subset"
            );
            assert!(
                !scoped.values().any(|value| value == "beta-identity-value"),
                "the undeclared identity's value never enters the scoped subset"
            );

            let checksums = BTreeMap::from([("source".to_owned(), "digest".to_owned())]);
            let stem = session.artifact_stem.as_str().to_owned();
            let bindings = session
                .template_bindings(&scoped, &checksums)
                .expect("scoped facts bind under the neutral carrier");

            let prepared =
                rumoca_phase_codegen::prepare_ast_rendering(compilation.resolved().inner());
            let rendered = rumoca_phase_codegen::render_ast_template_content(
                "{{ model_name }}|{{ __rumoca_artifact_identity_v1_alpha }}|{% for key in artifact %}{{ key }},{% endfor %}",
                &prepared,
                &bindings,
            )
            .expect("scoped identity and the artifact projection render")
            .into_content();
            assert_eq!(
                rendered,
                format!(
                    "{stem}|alpha-identity-value|artifact_stem,checksums,generated_at,generation_tool,"
                ),
                "the bound names are exactly model_name, the one scoped identity scalar, and the four-key artifact object"
            );
            assert!(
                !rendered.contains("beta-identity-value"),
                "no value anywhere in the render tree equals the unscoped identity's value"
            );

            let undeclared = rumoca_phase_codegen::render_ast_template_content(
                "{{ __rumoca_artifact_identity_v1_beta }}",
                &prepared,
                &bindings,
            )
            .expect_err(
                "the undeclared identity scalar is physically absent under strict-undefined",
            );
            assert!(
                undeclared.to_string().to_lowercase().contains("undefined"),
                "unexpected diagnostic for the absent scalar: {undeclared:#}"
            );
        });
    }
}
