mod artifact_identity_name;
mod checked_plan;
mod descriptors;
mod feature_analysis;
mod filesystem;
mod profiles;
mod target_sources;
mod validation;

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io::Read as _;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

use anyhow::{Context, Result, bail};
use rumoca_ir_dae as dae;
use rumoca_phase_codegen::templates;
use serde::{Deserialize, Deserializer, Serialize, de::Error as _};
use sha2::{Digest as _, Sha256};

pub(crate) use checked_plan::CheckedTargetArtifactStem;
pub use checked_plan::{
    ArtifactGenerationInstant, ArtifactIdentitySeed, ArtifactSessionInput,
    ArtifactSessionInputError, CheckedTargetBundle, CompletedArtifactMemberRef, CompletedPackage,
    CompletedRenderedFile, CompletedRenderedFileRef, CompletedTargetArtifact, CompletedUnpackaged,
    PublishedTargetArtifact, TargetArtifactIdentityScope, TargetArtifactIdentityScopeKind,
    TargetBundle,
};
use checked_plan::{CheckedTargetCapabilityContract, TargetBundleSource};
use descriptors::{
    CapabilityTarget, parse_target_manifest_construction, phase_artifact_kind,
    phase_semantic_context, validate_dae_render_capability_contract,
    validate_solve_capability_contract,
};
pub use descriptors::{
    builtin_target_compatibility_matrix, builtin_target_descriptors,
    builtin_target_descriptors_requiring,
};
#[cfg(test)]
use descriptors::{validate_dae_capabilities, validate_solve_capabilities};
use feature_analysis::{
    dae_has_clocks, dae_has_dynamic_derivative_subscripts, dae_has_dynamic_ranges, dae_has_events,
    dae_has_external_functions, dae_has_initialization, dae_has_runtime_events, dae_uses_random,
    solve_requires_residual_equations,
};
#[cfg(test)]
use filesystem::MAX_TARGET_INPUT_FILE_BYTES;
pub(crate) use filesystem::safe_target_join;
use filesystem::{TargetSnapshotBudget, read_regular_file_bounded, read_utf8_regular_file_bounded};
use profiles::TargetSolveExecutableProfiles;
#[cfg(test)]
use target_sources::target_asset_relative_path;
use target_sources::{
    TargetAssetFile, borrowed_asset_files, close_asset_member_order, collect_target_assets,
    target_template_source_for,
};
use validation::{
    construct_artifact_identity_catalog, construct_render_authority,
    ensure_target_has_rendered_files, target_file_role_name, unsupported_feature,
    unsupported_feature_at, validate_product_member_roles, validate_target_file_contract,
    validate_target_manifest,
};

/// Checked target manifest retained only inside target construction.
#[derive(Debug)]
pub(crate) struct TargetManifest {
    version: u32,
    arithmetic: TargetArithmeticSelection,
    required_product: TargetRequiredProduct,
    name: Option<String>,
    description: Option<String>,
    execution_mode: Option<String>,
    deployment_class: Option<String>,
    readiness_level: Option<u8>,
    package: Option<TargetPackage>,
    completion_message: Option<String>,
    capabilities: Option<TargetCapabilities>,
    solve_executable: TargetSolveExecutableSelection,
    files: Vec<TargetFile>,
    /// Canonically sorted logical artifact identities proved to use the exact
    /// dot-addressable grammar and to be unique by the construction pass that
    /// issues `render_plan`.
    artifact_identity_keys: Box<[String]>,
    /// Declared target-relative asset trees copied verbatim into the product.
    assets: Vec<AssetBundle>,
}

/// Complete normalized numeric profile fixed by an Algorithm Code target.
#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct TargetAlgorithmCodeArithmetic {
    pub source_real: rumoca_ir_galec::package::AlgorithmCodeRealFormat,
    pub source_integer: rumoca_ir_galec::package::AlgorithmCodeIntegerFormat,
    pub real_matrix_multiply: rumoca_core::RealMatrixMultiplySemantics,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TargetArithmeticSelection {
    NotApplicable,
    AlgorithmCode(TargetAlgorithmCodeArithmetic),
}

#[derive(Debug)]
enum TargetSolveExecutableSelection {
    NotApplicable,
    Prepared(rumoca_phase_codegen::SolveAlgorithmProductionProfile),
}

/// Untrusted TOML shape. It never escapes deserialization: conditional
/// arithmetic applicability is closed before [`TargetManifest`] exists.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TargetManifestDraft {
    version: u32,
    arithmetic: Option<TargetAlgorithmCodeArithmetic>,
    name: Option<String>,
    description: Option<String>,
    execution_mode: Option<String>,
    deployment_class: Option<String>,
    readiness_level: Option<u8>,
    package: Option<TargetPackage>,
    completion_message: Option<String>,
    capabilities: Option<TargetCapabilities>,
    solve_executable: Option<TargetSolveExecutableProfiles>,
    #[serde(default)]
    files: Vec<TargetFileDraft>,
    #[serde(default)]
    assets: Vec<AssetBundle>,
}

impl<'de> Deserialize<'de> for TargetManifest {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let draft = TargetManifestDraft::deserialize(deserializer)?;
        Self::from_draft(draft)
            .map(|construction| construction.manifest)
            .map_err(D::Error::custom)
    }
}

impl TargetManifest {
    fn from_draft(mut draft: TargetManifestDraft) -> Result<TargetManifestConstruction> {
        let artifact_identity_keys = construct_artifact_identity_catalog(&draft.files)?;
        let files = std::mem::take(&mut draft.files)
            .into_iter()
            .map(|file| {
                TargetFile::from_draft(file, draft.name.as_deref(), &artifact_identity_keys)
            })
            .collect::<Result<Vec<_>>>()?;
        let required_product = TargetRequiredProduct::from_declarations(&files)?;
        let arithmetic = match (
            required_product.carries_algorithm_code_package(),
            draft.arithmetic,
        ) {
            (true, Some(arithmetic)) => TargetArithmeticSelection::AlgorithmCode(arithmetic),
            (true, None) => {
                bail!("Algorithm Code target manifest must declare an [arithmetic] table")
            }
            (false, Some(_)) => bail!(
                "target manifest whose checked product carries no AlgorithmCodePackage must not declare an [arithmetic] table"
            ),
            (false, None) => TargetArithmeticSelection::NotApplicable,
        };
        let solve_executable = match (required_product, draft.solve_executable, arithmetic) {
            (
                TargetRequiredProduct::SolveAlgorithmProduct,
                Some(profiles),
                TargetArithmeticSelection::AlgorithmCode(arithmetic),
            ) => TargetSolveExecutableSelection::Prepared(
                profiles.into_production_profile(arithmetic)?,
            ),
            (TargetRequiredProduct::SolveAlgorithmProduct, None, _) => bail!(
                "SolveAlgorithmProduct target manifest must declare a complete [solve_executable] profile"
            ),
            (
                TargetRequiredProduct::SolveAlgorithmProduct,
                Some(_),
                TargetArithmeticSelection::NotApplicable,
            ) => bail!(
                "SolveAlgorithmProduct target manifest lost its required Algorithm Code numeric profile"
            ),
            (_, Some(_), _) => bail!(
                "target manifest whose product is not SolveAlgorithmProduct must not declare [solve_executable]"
            ),
            (_, None, _) => TargetSolveExecutableSelection::NotApplicable,
        };
        let validated_members = validate_product_member_roles(
            required_product,
            &files,
            &draft.assets,
            draft.package.as_ref(),
        )?;
        let render_authority = construct_render_authority(
            &files,
            validated_members.render_members,
            draft.package.as_ref(),
        )?;
        let manifest = Self {
            version: draft.version,
            arithmetic,
            required_product,
            name: draft.name,
            description: draft.description,
            execution_mode: draft.execution_mode,
            deployment_class: draft.deployment_class,
            readiness_level: draft.readiness_level,
            package: draft.package,
            completion_message: draft.completion_message,
            capabilities: draft.capabilities,
            solve_executable,
            files,
            artifact_identity_keys,
            assets: draft.assets,
        };
        validate_target_manifest(&manifest)?;
        Ok(TargetManifestConstruction {
            manifest,
            render_authority,
        })
    }

    /// The explicit Algorithm Code numeric profile carried by this target kind.
    /// `None` means the checked target kind is not Algorithm Code, never that
    /// an Algorithm Code declaration was omitted.
    #[must_use]
    pub(crate) const fn algorithm_code_arithmetic(&self) -> Option<TargetAlgorithmCodeArithmetic> {
        match self.arithmetic {
            TargetArithmeticSelection::AlgorithmCode(arithmetic) => Some(arithmetic),
            TargetArithmeticSelection::NotApplicable => None,
        }
    }

    /// Construction-issued semantic product required by all per-file views in
    /// this manifest.
    ///
    /// Orchestration consumes this product directly. It must not infer it from
    /// target identity or output suffixes.
    #[must_use]
    pub(crate) fn required_product(&self) -> TargetRequiredProduct {
        self.required_product
    }

    #[must_use]
    pub(crate) const fn solve_algorithm_production_profile(
        &self,
    ) -> Option<rumoca_phase_codegen::SolveAlgorithmProductionProfile> {
        match self.solve_executable {
            TargetSolveExecutableSelection::Prepared(profile) => Some(profile),
            TargetSolveExecutableSelection::NotApplicable => None,
        }
    }

    #[must_use]
    pub(crate) fn files(&self) -> &[TargetFile] {
        &self.files
    }

    fn canonical_artifact_identity_digest(&self) -> Result<String> {
        let arithmetic = match &self.arithmetic {
            TargetArithmeticSelection::NotApplicable => None,
            TargetArithmeticSelection::AlgorithmCode(profile) => Some(profile),
        };
        let solve_executable = match self.solve_executable {
            TargetSolveExecutableSelection::NotApplicable => None,
            TargetSolveExecutableSelection::Prepared(profile) => {
                Some(CanonicalSolveExecutableIdentity {
                    maximum_method_automatic_payload_bytes: profile
                        .maximum_method_automatic_payload_bytes(),
                    failure_transport: match profile.failure_transport() {
                        rumoca_phase_codegen::SolveAlgorithmProductionFailureTransport::ReturnedStatusI32 => {
                            "returned-status-i32"
                        }
                    },
                })
            }
        };
        let canonical = CanonicalTargetManifestIdentity {
            version: self.version,
            arithmetic,
            required_product: self.required_product,
            execution_mode: self.execution_mode.as_deref(),
            deployment_class: self.deployment_class.as_deref(),
            readiness_level: self.readiness_level,
            package: self.package.as_ref(),
            capabilities: self.capabilities.as_ref(),
            solve_executable,
            files: &self.files,
            assets: &self.assets,
        };
        let bytes = serde_json::to_vec(&canonical)
            .context("Serialize canonical checked target manifest identity")?;
        Ok(blake3::hash(&bytes).to_hex().to_string())
    }
}

/// Canonical target facts that can affect artifact construction or admission.
/// Human-facing `name`, `description`, and completion text are deliberately
/// absent: presentation is not target identity.
#[derive(Serialize)]
struct CanonicalTargetManifestIdentity<'a> {
    version: u32,
    arithmetic: Option<&'a TargetAlgorithmCodeArithmetic>,
    required_product: TargetRequiredProduct,
    execution_mode: Option<&'a str>,
    deployment_class: Option<&'a str>,
    readiness_level: Option<u8>,
    package: Option<&'a TargetPackage>,
    capabilities: Option<&'a TargetCapabilities>,
    solve_executable: Option<CanonicalSolveExecutableIdentity<'static>>,
    files: &'a [TargetFile],
    assets: &'a [AssetBundle],
}

#[derive(Serialize)]
struct CanonicalSolveExecutableIdentity<'a> {
    maximum_method_automatic_payload_bytes: u32,
    failure_transport: &'a str,
}

/// Product membership and the complete per-declaration rendering authority
/// are issued by one construction pass. The latter is consumed directly into
/// the checksum plan, so no parallel role array survives checked manifest
/// construction.
struct ValidatedTargetProductMembers {
    render_members: Box<[TargetPreparedMemberPlan]>,
}

/// Private result of parsing and closing target semantics before target-bundle
/// bytes are snapshotted. The render authority is consumed by
/// [`TargetBundle::check`]; public manifest inspection receives metadata only.
struct TargetManifestConstruction {
    manifest: TargetManifest,
    render_authority: TargetDeclaredRenderAuthority,
}

/// The one declared render authority. Packaged targets own one mixed sequence;
/// non-package targets own only their file sequence.
enum TargetDeclaredRenderAuthority {
    Unpackaged(Box<[TargetDeclaredRenderPlanStep]>),
    Packaged(Box<[TargetDeclaredPackageMemberPlan]>),
}

enum TargetDeclaredPackageMemberPlan {
    File(TargetDeclaredRenderPlanStep),
    Asset {
        source: String,
        relative_path: String,
    },
}

enum PendingTargetRenderAuthority {
    Unpackaged(Box<[PendingTargetRenderStep]>),
    Packaged(Box<[PendingTargetPackageMemberPlan]>),
}

struct PendingTargetRenderStep {
    file: TargetDeclaredFileSpec,
    file_id: Option<Box<str>>,
    checksum_needs: Box<[ChecksumNeed]>,
    prepared_member: TargetPreparedMemberPlan,
}

enum PendingTargetPackageMemberPlan {
    File(PendingTargetRenderStep),
    Asset {
        source: String,
        relative_path: String,
    },
}

enum TargetSnapshottedRenderAuthority {
    Unpackaged(Box<[TargetRenderPlanStep]>),
    Packaged(Box<[TargetSnapshottedPackageMemberPlan]>),
}

enum TargetSnapshottedPackageMemberPlan {
    File(TargetRenderPlanStep),
    Asset {
        source: String,
        relative_path: String,
    },
}

/// Closed preparation route for one exact manifest file declaration.
///
/// This value is private and can be issued only while the product's complete
/// role family is being checked. It is consumed into the product-specific
/// target member sequence before rendering begins.
#[derive(Debug)]
enum TargetPreparedMemberPlan {
    Direct,
    AlgorithmCodeSource {
        output_path_template: rumoca_phase_codegen::AlgorithmCodeSourceOutputPathTemplate,
    },
    PackagedAlgorithmCode {
        role: rumoca_phase_codegen::AlgorithmCodeArtifactRole,
    },
    CorrelatedAlgorithmCode {
        role: rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole,
    },
    ProductionCode {
        role: rumoca_phase_codegen::ProductionCodeFileRole,
    },
}

/// Closed identity of one rendered artifact's byte-level format.
///
/// This is an admission field, not a filename inference. Construction checks
/// that the declared output path has this kind's one legal suffix, so a target
/// cannot label a C/H product as generic text to bypass its semantic-context
/// gate (SPEC_0034 GAL-043).
#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TargetArtifactKind {
    AlgorithmCode,
    CHeader,
    CSource,
    CudaSource,
    Json,
    Markdown,
    MlirSource,
    ModelicaSource,
    PythonSource,
    RustSource,
    Text,
    Toml,
    WgslSource,
    Xml,
}

/// Closed role of one member in a correlated Algorithm/Production Code eFMU.
///
/// This is independent of byte format and IR authority: those remain owned by
/// [`TargetArtifactKind`] and [`TargetSemanticView`]. The role lets manifest
/// construction issue one complete, typed package-member layout without
/// guessing from a path, suffix, template name, or target identity.
#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "kebab-case")]
enum TargetProductMemberRole {
    PackageManifest,
    AlgorithmCodeManifest,
    AlgorithmCodeSource,
    ProductionManifest,
    ProductionHeader,
    ProductionSource,
    Schema,
}

/// Construction-issued role of one rendered correlated-product member.
///
/// Unlike the source-manifest [`TargetProductMemberRole`], this vocabulary
/// cannot represent the asset-only `Schema` role. Manifest construction also
/// proves the role's artifact-kind and semantic-view relation before issuing
/// this value, so rendering never rechecks or narrows a raw declaration role.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TargetProductFileRole {
    PackageManifest,
    AlgorithmCodeManifest,
    AlgorithmCodeSource,
    ProductionManifest,
    ProductionHeader,
    ProductionSource,
}

/// Construction-issued rendered-member role for a standalone packaged
/// Algorithm Code product. Production roles are absent at the type level, so
/// orchestration can map this vocabulary exhaustively without an impossible
/// fallback arm.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TargetAlgorithmCodeFileRole {
    PackageManifest,
    AlgorithmCodeManifest,
    AlgorithmCodeSource,
}

impl TargetArtifactKind {
    const fn suffix(self) -> &'static str {
        match self {
            Self::AlgorithmCode => "alg",
            Self::CHeader => "h",
            Self::CSource => "c",
            Self::CudaSource => "cu",
            Self::Json => "json",
            Self::Markdown => "md",
            Self::MlirSource => "mlir",
            Self::ModelicaSource => "mo",
            Self::PythonSource => "py",
            Self::RustSource => "rs",
            Self::Text => "txt",
            Self::Toml => "toml",
            Self::WgslSource => "wgsl",
            Self::Xml => "xml",
        }
    }

    const fn admits_context(self, context: TargetSemanticContext) -> bool {
        match context {
            TargetSemanticContext::Galec => matches!(self, Self::AlgorithmCode | Self::Xml),
            TargetSemanticContext::Solve => !matches!(self, Self::AlgorithmCode),
            TargetSemanticContext::Ast
            | TargetSemanticContext::Flat
            | TargetSemanticContext::Dae => !matches!(
                self,
                Self::AlgorithmCode | Self::CHeader | Self::CSource | Self::CudaSource
            ),
        }
    }
}

/// Closed semantic authority made available while rendering one file.
///
/// The vocabulary is exactly the five `rumoca-ir-*` crate names. Roots and
/// product flavors inside an IR crate never become manifest contexts.
macro_rules! define_target_semantic_contexts {
    ($( $variant:ident => $wire_name:literal ),+ $(,)?) => {
        #[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
        pub enum TargetSemanticContext {
            $(#[serde(rename = $wire_name)] $variant),+
        }

        impl TargetSemanticContext {
            /// Complete context vocabulary, generated from the same list as
            /// the enum so architecture tests cannot observe a partial list.
            pub const ALL: &'static [Self] = &[$(Self::$variant),+];

            #[must_use]
            pub const fn as_str(self) -> &'static str {
                match self {
                    $(Self::$variant => $wire_name),+
                }
            }
        }
    };
}

define_target_semantic_contexts! {
    Ast => "ast",
    Flat => "flat",
    Dae => "dae",
    Galec => "galec",
    Solve => "solve",
}

/// Closed checked IR root or view supplied while rendering one file.
///
/// A missing `[[files]].view` is resolved explicitly from the file's semantic
/// context by [`TargetSemanticView::canonical_for_context`]. This type has no
/// `Default`: adding a context or a view requires extending the closed
/// construction relation.
macro_rules! define_target_semantic_views {
    ($( $variant:ident => ($wire_name:literal, $context:ident) ),+ $(,)?) => {
        #[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
        pub enum TargetSemanticView {
            $(#[serde(rename = $wire_name)] $variant),+
        }

        impl TargetSemanticView {
            /// Complete view vocabulary, generated from the same list as the
            /// enum so architecture tests can prove every member's IR owner.
            pub const ALL: &'static [Self] = &[$(Self::$variant),+];

            #[must_use]
            pub const fn as_str(self) -> &'static str {
                match self {
                    $(Self::$variant => $wire_name),+
                }
            }

            #[must_use]
            pub const fn semantic_context(self) -> TargetSemanticContext {
                match self {
                    $(Self::$variant => TargetSemanticContext::$context),+
                }
            }
        }
    };
}

define_target_semantic_views! {
    ClassTree => ("class-tree", Ast),
    FlatModel => ("flat-model", Flat),
    Dae => ("dae", Dae),
    AlgorithmCodePackage => ("algorithm-code-package", Galec),
    SolveModel => ("solve-model", Solve),
    FmiComponent => ("fmi-component", Solve),
    SolveAlgorithmBlock => ("solve-algorithm-block", Solve),
}

impl TargetSemanticView {
    const fn canonical_for_context(context: TargetSemanticContext) -> Self {
        match context {
            TargetSemanticContext::Ast => Self::ClassTree,
            TargetSemanticContext::Flat => Self::FlatModel,
            TargetSemanticContext::Dae => Self::Dae,
            TargetSemanticContext::Galec => Self::AlgorithmCodePackage,
            TargetSemanticContext::Solve => Self::SolveModel,
        }
    }
}

/// Closed semantic product required by the per-file contexts in one target.
///
/// `SolveAlgorithmProduct` is one correlated product retaining both its
/// `AlgorithmCodePackage` and `SolveAlgorithmBlock`; it is never a request to
/// construct or pair those views independently.
macro_rules! define_target_required_products {
    ($( $variant:ident => $wire_name:literal ),+ $(,)?) => {
        #[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq)]
        pub enum TargetRequiredProduct {
            $(#[serde(rename = $wire_name)] $variant),+
        }

        impl TargetRequiredProduct {
            #[must_use]
            pub const fn as_str(self) -> &'static str {
                match self {
                    $(Self::$variant => $wire_name),+
                }
            }
        }
    };
}

define_target_required_products! {
    Ast => "ast",
    Flat => "flat",
    Dae => "dae",
    AlgorithmCodePackage => "algorithm-code-package",
    SolveModel => "solve-model",
    FmiComponent => "fmi-component",
    SolveAlgorithmProduct => "solve-algorithm-product",
}

impl TargetRequiredProduct {
    fn from_declarations(files: &[TargetFile]) -> Result<Self> {
        let views = files
            .iter()
            .map(|file| file.semantic_view)
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        if files
            .iter()
            .any(|file| file.semantic_context == TargetSemanticContext::Galec)
        {
            let source_count = files
                .iter()
                .filter(|file| file.artifact_kind == TargetArtifactKind::AlgorithmCode)
                .count();
            if source_count != 1 {
                bail!(
                    "a 'galec' semantic context must declare exactly one artifact_kind = 'algorithm-code' source file; found {source_count}"
                );
            }
        }
        if views.is_empty() {
            bail!("target.toml must contain at least one file entry");
        }
        if let [view] = views.as_slice() {
            return match view {
                TargetSemanticView::ClassTree => Ok(Self::Ast),
                TargetSemanticView::FlatModel => Ok(Self::Flat),
                TargetSemanticView::Dae => Ok(Self::Dae),
                TargetSemanticView::AlgorithmCodePackage => Ok(Self::AlgorithmCodePackage),
                TargetSemanticView::SolveModel => Ok(Self::SolveModel),
                TargetSemanticView::FmiComponent => Ok(Self::FmiComponent),
                TargetSemanticView::SolveAlgorithmBlock => bail!(
                    "a 'solve-algorithm-block' view is only admitted as the correlated Production Code view of one SolveAlgorithmProduct"
                ),
            };
        }
        if views.as_slice()
            == [
                TargetSemanticView::AlgorithmCodePackage,
                TargetSemanticView::SolveAlgorithmBlock,
            ]
        {
            return Ok(Self::SolveAlgorithmProduct);
        }
        bail!(
            "target files declare incompatible checked IR views; independently provisioned roots cannot share one target"
        )
    }

    const fn carries_algorithm_code_package(self) -> bool {
        match self {
            Self::AlgorithmCodePackage | Self::SolveAlgorithmProduct => true,
            Self::Ast | Self::Flat | Self::Dae | Self::SolveModel | Self::FmiComponent => false,
        }
    }

    const fn carries_solve_tensor_program(self) -> bool {
        match self {
            Self::SolveModel | Self::FmiComponent | Self::SolveAlgorithmProduct => true,
            Self::Ast | Self::Flat | Self::Dae | Self::AlgorithmCodePackage => false,
        }
    }

    const fn requires_declared_capabilities(self) -> bool {
        match self {
            Self::Dae
            | Self::AlgorithmCodePackage
            | Self::SolveModel
            | Self::FmiComponent
            | Self::SolveAlgorithmProduct => true,
            Self::Ast | Self::Flat => false,
        }
    }

    const fn admits_exact_algebraic_assignment_capability(self) -> bool {
        match self {
            Self::SolveModel | Self::FmiComponent => true,
            Self::Ast
            | Self::Flat
            | Self::Dae
            | Self::AlgorithmCodePackage
            | Self::SolveAlgorithmProduct => false,
        }
    }
}

/// Target-declared package layout. Rendered artifact authority remains owned
/// by each checked `[[files]].semantic_context` declaration.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct TargetPackage {
    root: String,
    #[serde(default)]
    required_files: Vec<String>,
    archive: Option<TargetArchive>,
    /// Sole declared package-member order. Every rendered file and expanded
    /// asset member must occur exactly once in this closed sum.
    members: Vec<TargetPackageMember>,
}

/// One exact member reference in the package's target-issued order.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "kebab-case", deny_unknown_fields)]
enum TargetPackageMember {
    File { file: String },
    Asset { source: String, path: String },
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct TargetArchive {
    path: String,
    format: TargetArchiveFormat,
    root: TargetArchiveRoot,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
enum TargetArchiveFormat {
    Zip,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
enum TargetArchiveRoot {
    Flat,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct TargetCapabilities {
    pub continuous_states: Option<bool>,
    pub residual_equations: Option<bool>,
    /// The target directly executes construction-issued exact algebraic
    /// assignment schedules when no residual projection remains.
    pub exact_algebraic_assignments: Option<bool>,
    /// The target consumes compact DAE `structured_equations` as the
    /// authoritative body instead of blindly iterating placeholder scalar rows.
    pub structured_equation_families: Option<bool>,
    /// Whether a Solve-derived target admits the construction-issued scalar
    /// lowering alternative for tensor operations. This declaration is total:
    /// omission is not permission to scalarize.
    pub scalar_fallback: bool,
    pub external_functions: Option<bool>,
    pub random: Option<bool>,
    pub initialization: Option<bool>,
    pub events: Option<bool>,
    pub runtime_events: Option<bool>,
    pub forward_ad: Option<bool>,
    pub reverse_ad: Option<bool>,
    pub dynamic_control_flow: Option<bool>,
    pub host_callbacks: Option<bool>,
    pub clocks: Option<bool>,
    pub dynamic_ranges: Option<bool>,
    pub dynamic_derivative_subscripts: Option<bool>,
    pub tensor: Option<TensorCapabilities>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct TensorCapabilities {
    pub matmul: Option<TensorCapability>,
    pub linsolve: Option<TensorCapability>,
    pub elementwise: Option<TensorCapability>,
    pub stencil: Option<TensorCapability>,
    pub reductions: Option<TensorCapability>,
    pub layout: Option<TensorLayoutCapability>,
    pub supports_dynamic_shapes: Option<bool>,
    pub sparse: Option<bool>,
    pub dtypes: Option<Vec<String>>,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TensorCapability {
    Native,
    Scalar,
    Unsupported,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TensorLayoutCapability {
    RowMajor,
}

/// Checked rendered-file declaration retained only inside target construction.
#[derive(Debug, Serialize)]
pub(crate) struct TargetFile {
    /// Closed byte-level artifact identity; its output suffix is checked once
    /// when the manifest is constructed.
    artifact_kind: TargetArtifactKind,
    /// The sole semantic authority this file may observe while rendering.
    semantic_context: TargetSemanticContext,
    /// Checked root/view inside `semantic_context`, resolved during manifest
    /// construction even when the source declaration omits `view`.
    semantic_view: TargetSemanticView,
    product_role: Option<TargetProductMemberRole>,
    path: String,
    template: String,
    /// Exact registered built-in owner of this complete template's bytes.
    /// Absence means the declaring target owns a local template file.
    template_shared_from: Option<String>,
    mode: Option<String>,
    /// Construction-parsed Unix permission bits for unpackaged publication.
    /// The declaration spelling remains only for canonical manifest identity.
    mode_bits: Option<u32>,
    /// Stable logical identity of this rendered file within the target
    /// (contract §4a). Only files a checksum edge points at (`of = <id>`)
    /// need one; the identity is keyed off `id`, never the templated `path`,
    /// so it is stable across path interpolation (`{{ model_name }}`).
    id: Option<String>,
    /// Exact artifact identity keys this file may observe while rendering.
    /// Manifest construction proves every key was issued by this target's
    /// complete logical file-ID catalog before any template plan exists.
    required_artifact_identities: CheckedArtifactIdentityDependencies,
    /// Checksum edges this file consumes: for each entry, the SHA-1 of the
    /// producer file `of` is exposed to this file's templates under the
    /// context key `as` (contract §4a). The declaration is co-located with
    /// the template that interpolates the key, so under strict-undefined
    /// minijinja a template referencing `{{ <as> }}` without a matching
    /// entry fails loudly at render — declaration and use cannot drift.
    checksums: Vec<ChecksumNeed>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TargetFileDraft {
    artifact_kind: TargetArtifactKind,
    semantic_context: TargetSemanticContext,
    view: Option<TargetSemanticView>,
    product_role: Option<TargetProductMemberRole>,
    path: String,
    template: String,
    template_shared_from: Option<String>,
    mode: Option<String>,
    id: Option<String>,
    #[serde(default)]
    required_artifact_identities: Vec<String>,
    #[serde(default)]
    checksums: Vec<ChecksumNeed>,
}

impl TargetFile {
    fn from_draft(
        draft: TargetFileDraft,
        target_name: Option<&str>,
        artifact_identity_keys: &[String],
    ) -> Result<Self> {
        validate_target_file_contract(draft.artifact_kind, draft.semantic_context, &draft.path)?;
        let semantic_view = draft
            .view
            .unwrap_or_else(|| TargetSemanticView::canonical_for_context(draft.semantic_context));
        if semantic_view.semantic_context() != draft.semantic_context {
            bail!(
                "[[files]] path '{}' declares view = '{}' outside semantic_context = '{}'",
                draft.path,
                semantic_view.as_str(),
                draft.semantic_context.as_str()
            );
        }
        if let Some(owner) = draft.template_shared_from.as_deref() {
            validate_borrowed_template(
                target_name,
                owner,
                &draft.template,
                draft.artifact_kind,
                draft.semantic_context,
                semantic_view,
            )?;
        }
        let mode_bits = draft
            .mode
            .as_deref()
            .map(|mode| {
                u32::from_str_radix(mode.trim_start_matches("0o"), 8)
                    .with_context(|| format!("Parse target file mode '{mode}'"))
            })
            .transpose()?;
        let required_artifact_identities = CheckedArtifactIdentityDependencies::construct(
            draft.required_artifact_identities,
            artifact_identity_keys,
            &draft.path,
        )?;
        Ok(Self {
            artifact_kind: draft.artifact_kind,
            semantic_context: draft.semantic_context,
            semantic_view,
            product_role: draft.product_role,
            path: draft.path,
            template: draft.template,
            template_shared_from: draft.template_shared_from,
            mode: draft.mode,
            mode_bits,
            id: draft.id,
            required_artifact_identities,
            checksums: draft.checksums,
        })
    }

    #[must_use]
    pub(crate) fn template(&self) -> &str {
        &self.template
    }

    #[must_use]
    pub(crate) fn template_shared_from(&self) -> Option<&str> {
        self.template_shared_from.as_deref()
    }
}

/// Construction-checked artifact identities visible to one template file.
///
/// This carrier is neither deserializable nor publicly constructible. Its
/// keys are canonicalized and proven members of the target-wide issued
/// identity catalog before the render authority is created.
#[derive(Debug, Clone, Serialize)]
#[serde(transparent)]
struct CheckedArtifactIdentityDependencies(Box<[String]>);

impl CheckedArtifactIdentityDependencies {
    fn construct(
        declared: Vec<String>,
        artifact_identity_keys: &[String],
        file_path: &str,
    ) -> Result<Self> {
        let mut keys = BTreeSet::new();
        for key in declared {
            if key.trim().is_empty() {
                bail!(
                    "[[files]].required_artifact_identities must not contain an empty key (file '{file_path}')"
                );
            }
            if !keys.insert(key.clone()) {
                bail!(
                    "[[files]].required_artifact_identities contains duplicate key '{key}' (file '{file_path}')"
                );
            }
            if artifact_identity_keys.binary_search(&key).is_err() {
                bail!(
                    "[[files]].required_artifact_identities key '{key}' on file '{file_path}' names no target-issued [[files]] id"
                );
            }
        }
        Ok(Self(
            keys.into_iter().collect::<Vec<_>>().into_boxed_slice(),
        ))
    }

    fn keys(&self) -> &[String] {
        &self.0
    }
}

fn validate_borrowed_template(
    target_name: Option<&str>,
    owner: &str,
    template: &str,
    artifact_kind: TargetArtifactKind,
    semantic_context: TargetSemanticContext,
    semantic_view: TargetSemanticView,
) -> Result<()> {
    if owner.trim() != owner || owner.is_empty() || owner.chars().any(char::is_control) {
        bail!("[[files]].template_shared_from must be one exact nonempty built-in target name");
    }
    if target_name == Some(owner) {
        bail!("[[files]] template '{template}' cannot borrow from its declaring target '{owner}'");
    }
    let builtin = templates::builtin_target(owner).with_context(|| {
        format!("[[files]] template '{template}' borrows from unknown built-in target '{owner}'")
    })?;
    if builtin.template_source(template).is_none() {
        bail!(
            "[[files]] template '{template}' borrows from target '{owner}', which owns no such template"
        );
    }
    let owner_draft: TargetManifestDraft = toml::from_str(builtin.manifest)
        .with_context(|| format!("parse registered template owner target manifest '{owner}'"))?;
    let mut matches = owner_draft
        .files
        .into_iter()
        .filter(|file| file.template == template);
    let owner_file = matches.next().with_context(|| {
        format!(
            "registered target '{owner}' bundles template '{template}' without one owning [[files]] declaration"
        )
    })?;
    if matches.next().is_some() {
        bail!("registered target '{owner}' declares template '{template}' more than once");
    }
    if owner_file.template_shared_from.is_some() {
        bail!(
            "[[files]] template '{template}' must name its canonical owner, not borrowing target '{owner}'"
        );
    }
    let owner_view = owner_file
        .view
        .unwrap_or_else(|| TargetSemanticView::canonical_for_context(owner_file.semantic_context));
    if (
        owner_file.artifact_kind,
        owner_file.semantic_context,
        owner_view,
    ) != (artifact_kind, semantic_context, semantic_view)
    {
        bail!(
            "[[files]] template '{template}' cannot borrow from target '{owner}': artifact/context/view declarations differ"
        );
    }
    Ok(())
}

/// One consumer-declared checksum edge: "embed the producer `of`'s SHA-1
/// under my context key `as`" (contract §4a). Manifest construction resolves
/// the directed edge `of -> this`, proves the DAG, and issues its render plan;
/// packaging receives no string edge to resolve or graph to sort.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ChecksumNeed {
    /// The producer file's `id` whose exact rendered bytes are hashed.
    of: String,
    /// Hash algorithm selected by the target format.
    algorithm: ChecksumAlgorithm,
    /// The context key this file's templates read the producer's SHA-1 from.
    /// `as` is a Rust keyword, so the field is renamed for the struct.
    #[serde(rename = "as")]
    as_key: String,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum ChecksumAlgorithm {
    Sha1,
}

/// One construction-resolved checksum input for a consumer file.
///
/// The producer is an immutable earlier render position, not the source
/// manifest's string id. This value has no public constructor or deserializer.
#[derive(Debug)]
enum TargetResolvedChecksumBinding {
    Sha1 {
        producer: TargetFileResultId,
        as_key: String,
    },
}

impl TargetResolvedChecksumBinding {
    fn erase_after_resolution(self) {
        match self {
            Self::Sha1 { .. } => {}
        }
    }
}

/// Opaque identity of one file result in the package plan's private dense
/// file arena.
///
/// The token's representation and constructor stay private to manifest
/// construction. In particular, packaging cannot convert arbitrary integers
/// into producer authority or perform a bounds/missing-producer check.
#[derive(Debug, Clone, Copy)]
struct TargetFileResultId(usize);

impl TargetFileResultId {
    fn resolve<'a, T>(&self, rendered_prefix: &'a [T]) -> &'a T {
        &rendered_prefix[self.0]
    }
}

#[derive(Debug)]
struct TargetDeclaredRenderPlanStep {
    file: TargetDeclaredFileSpec,
    incoming_checksums: Vec<TargetResolvedChecksumBinding>,
    prepared_member: TargetPreparedMemberPlan,
}

#[derive(Debug)]
struct TargetRenderPlanStep {
    file: TargetSnapshottedFile,
    incoming_checksums: Vec<TargetResolvedChecksumBinding>,
    prepared_member: TargetPreparedMemberPlan,
}

#[derive(Debug)]
struct TargetDeclaredFileSpec {
    artifact_kind: TargetArtifactKind,
    semantic_context: TargetSemanticContext,
    path: Box<str>,
    template: Box<str>,
    template_shared_from: Option<Box<str>>,
    mode_bits: Option<u32>,
    role_name: &'static str,
    artifact_identity_dependencies: CheckedArtifactIdentityDependencies,
}

impl TargetDeclaredFileSpec {
    fn from_file(file: &TargetFile) -> Self {
        Self {
            artifact_kind: file.artifact_kind,
            semantic_context: file.semantic_context,
            path: file.path.clone().into_boxed_str(),
            template: file.template.clone().into_boxed_str(),
            template_shared_from: file.template_shared_from.as_deref().map(Box::<str>::from),
            mode_bits: file.mode_bits,
            role_name: target_file_role_name(file),
            artifact_identity_dependencies: file.required_artifact_identities.clone(),
        }
    }
}

#[derive(Debug)]
struct TargetSnapshottedFile {
    declaration: TargetDeclaredFileSpec,
    template_body: Box<str>,
}

/// A declared asset tree copied from `source` under the target directory to
/// `dest` under the package root.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct AssetBundle {
    source: String,
    dest: String,
    product_role: Option<TargetProductMemberRole>,
    /// The built-in target that owns these bytes, when this target borrows a
    /// bundle instead of vendoring its own copy of it.
    ///
    /// The borrower keeps no directory of its own: the files arrive from the
    /// owner under the identical relative paths, so nothing downstream can
    /// tell a borrowed bundle from an owned one. For a built-in target the
    /// codegen build script grafts them in at bundling time and embeds the
    /// bytes once; for a directory target [`TargetBundle::asset_files`] reads
    /// them out of the owner's embedded bundle. Both spellings of the same
    /// target therefore emit the same bytes.
    shared_from: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BuiltinTargetDescriptor {
    pub id: String,
    pub label: String,
    pub description: Option<String>,
    pub required_product: TargetRequiredProduct,
    pub capabilities: Option<TargetCapabilities>,
    pub file_plans: Vec<TargetFileDescriptor>,
}

/// Passive discovery facts for one checked built-in target output.
///
/// This value carries no template body, declaration identity, package-member
/// role, checksum edge, or render authority.
#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TargetFileDescriptor {
    pub path: String,
    pub semantic_context: TargetSemanticContext,
    pub semantic_view: TargetSemanticView,
}

struct TargetDescriptorFacts {
    label: String,
    description: Option<String>,
    required_product: TargetRequiredProduct,
    capabilities: Option<TargetCapabilities>,
    execution_mode: Option<String>,
    deployment_class: Option<String>,
    readiness_level: Option<u8>,
    file_plans: Vec<TargetFileDescriptor>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum TargetFeatureSupport {
    Native,
    Scalar,
    Unsupported,
    Unknown,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TargetCompatibilityEntry {
    pub id: String,
    pub label: String,
    pub required_product: TargetRequiredProduct,
    pub execution_mode: Option<String>,
    pub deployment_class: Option<String>,
    pub readiness_level: Option<u8>,
    pub scalar_programs: TargetFeatureSupport,
    pub matmul: TargetFeatureSupport,
    pub linsolve: TargetFeatureSupport,
    pub elementwise: TargetFeatureSupport,
    pub stencil: TargetFeatureSupport,
    pub reductions: TargetFeatureSupport,
    pub supports_dynamic_shapes: Option<bool>,
    pub sparse: TargetFeatureSupport,
    pub dtypes: Vec<String>,
    pub events: TargetFeatureSupport,
    pub runtime_events: TargetFeatureSupport,
    pub forward_ad: TargetFeatureSupport,
    pub reverse_ad: TargetFeatureSupport,
    pub dynamic_control_flow: TargetFeatureSupport,
    pub host_callbacks: TargetFeatureSupport,
}
