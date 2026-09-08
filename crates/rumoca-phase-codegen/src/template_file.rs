//! Checked per-file template plans.
//!
//! Manifest parsing lives above this crate, but a renderer must not accept the
//! parser's raw strings.  Orchestration maps its closed declaration vocabulary
//! into the phase-owned enums below and consumes the strings into one of the
//! context-specific plans.  After that construction step, neither the output
//! path template nor the body template can be extracted or relabelled.

use std::fmt;
use std::sync::Arc;

use crate::views::{
    AlgorithmCodeArtifactLayoutMember, AlgorithmCodeArtifactRole, PreparedAlgorithmCodePackage,
    PreparedSolveAlgorithmProduction, ProductionArtifactLayoutMember,
};
use rumoca_core::TargetInvocationBrand;

#[cfg(test)]
mod tests;

/// Byte-level identity declared independently for one rendered artifact.
///
/// This is deliberately not deserializable.  The manifest layer owns parsing
/// and must map its already-checked declaration exhaustively at orchestration.
macro_rules! define_template_artifact_kinds {
    ($( $(#[$attribute:meta])* $variant:ident => $wire_name:literal ),+ $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum TemplateArtifactKind {
            $($(#[$attribute])* $variant),+
        }

        impl TemplateArtifactKind {
            /// Complete byte-level artifact vocabulary.
            pub const ALL: &'static [Self] = &[$(Self::$variant),+];

            /// Stable diagnostic spelling.
            #[must_use]
            pub const fn as_str(self) -> &'static str {
                match self {
                    $(Self::$variant => $wire_name),+
                }
            }
        }
    };
}

define_template_artifact_kinds! {
    /// eFMI Algorithm Code source (`.alg`).
    AlgorithmCode => "algorithm-code",
    /// C declaration artifact.
    CHeader => "c-header",
    /// C implementation artifact.
    CSource => "c-source",
    /// CUDA C implementation artifact.
    CudaSource => "cuda-source",
    /// JSON metadata or inspection artifact.
    Json => "json",
    /// Markdown documentation artifact.
    Markdown => "markdown",
    /// MLIR source artifact.
    MlirSource => "mlir-source",
    /// Modelica source artifact.
    ModelicaSource => "modelica-source",
    /// Python source artifact.
    PythonSource => "python-source",
    /// Rust source artifact.
    RustSource => "rust-source",
    /// Plain-text artifact.
    Text => "text",
    /// TOML metadata artifact.
    Toml => "toml",
    /// WGSL source artifact.
    WgslSource => "wgsl-source",
    /// XML metadata artifact.
    Xml => "xml",
}

/// Proven-valid IR crate family supplied to one template file.
///
/// Product names, root flavours, and output formats are intentionally absent.
/// The plan's Rust type selects the exact checked root or view within this
/// family, so neither an eFMU nor an FMI component becomes a context spelling.
macro_rules! define_template_semantic_contexts {
    ($( $(#[$attribute:meta])* $variant:ident => $wire_name:literal ),+ $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum TemplateSemanticContext {
            $($(#[$attribute])* $variant),+
        }

        impl TemplateSemanticContext {
            /// Complete semantic-context vocabulary.
            pub const ALL: &'static [Self] = &[$(Self::$variant),+];

            /// Stable diagnostic spelling.
            #[must_use]
            pub const fn as_str(self) -> &'static str {
                match self {
                    $(Self::$variant => $wire_name),+
                }
            }
        }
    };
}

define_template_semantic_contexts! {
    /// `rumoca-ir-ast`.
    Ast => "ast",
    /// `rumoca-ir-flat`.
    Flat => "flat",
    /// `rumoca-ir-dae`.
    Dae => "dae",
    /// `rumoca-ir-galec`.
    Galec => "galec",
    /// `rumoca-ir-solve`.
    Solve => "solve",
}

/// Failure to construct one context-specific template-file plan.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TemplateFilePlanError {
    /// The constructor was given a different checked IR crate family.
    SemanticContextMismatch {
        /// Context required by the requested plan type.
        expected: TemplateSemanticContext,
        /// Context declared for this file.
        actual: TemplateSemanticContext,
    },
    /// This byte-level artifact is not legal for the checked IR crate family.
    ArtifactForbiddenForContext {
        /// Independently declared byte-level artifact kind.
        artifact_kind: TemplateArtifactKind,
        /// Independently declared semantic authority.
        semantic_context: TemplateSemanticContext,
    },
    /// The checked declaration resolved to no body template.
    EmptyBodyTemplate,
    /// A sealed Production Code role was paired with a different byte artifact.
    ProductionCodeFileRoleMismatch {
        role: ProductionCodeFileRole,
        artifact_kind: TemplateArtifactKind,
    },
    /// A correlated Algorithm Code role was paired with another byte kind.
    CorrelatedAlgorithmCodeArtifactRoleMismatch {
        role: CorrelatedAlgorithmCodeArtifactRole,
        artifact_kind: TemplateArtifactKind,
    },
    /// A packaged Algorithm Code role was paired with another byte kind.
    PackagedAlgorithmCodeArtifactRoleMismatch {
        role: AlgorithmCodeArtifactRole,
        artifact_kind: TemplateArtifactKind,
    },
}

/// Failure to construct the complete output-path grammar admitted by generic
/// source-only Algorithm Code rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlgorithmCodeSourceOutputPathError {
    /// The path is not one portable `.alg` basename.
    NonPortableBasename,
}

impl fmt::Display for AlgorithmCodeSourceOutputPathError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NonPortableBasename => formatter.write_str(
                "source-only Algorithm Code output must be one static portable `.alg` basename",
            ),
        }
    }
}

impl std::error::Error for AlgorithmCodeSourceOutputPathError {}

/// Checked static output basename for generic source-only Algorithm Code.
///
/// Package member paths contain directories and therefore cannot inhabit this
/// type. The type is deliberately non-`Clone`, non-`Default`, and
/// non-deserializable; manifest construction issues it once and file plans
/// borrow that proof.
#[derive(Debug, PartialEq, Eq)]
pub struct AlgorithmCodeSourceOutputPathTemplate(Box<str>);

impl AlgorithmCodeSourceOutputPathTemplate {
    pub fn construct(path: Box<str>) -> Result<Self, AlgorithmCodeSourceOutputPathError> {
        let stem = path.strip_suffix(".alg");
        let portable = path
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'_' | b'-'));
        let valid_stem = stem.is_some_and(|stem| {
            !stem.is_empty()
                && stem != "."
                && stem != ".."
                && !stem.starts_with('.')
                && !stem.ends_with('.')
                && !is_windows_reserved_basename(stem)
        });
        if !portable || !valid_stem {
            return Err(AlgorithmCodeSourceOutputPathError::NonPortableBasename);
        }
        Ok(Self(path))
    }

    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

fn is_windows_reserved_basename(stem: &str) -> bool {
    let device = stem.find('.').map_or(stem, |extension| &stem[..extension]);
    ["CON", "PRN", "AUX", "NUL"]
        .iter()
        .any(|reserved| device.eq_ignore_ascii_case(reserved))
        || (device.len() == 4
            && (device[..3].eq_ignore_ascii_case("COM") || device[..3].eq_ignore_ascii_case("LPT"))
            && matches!(device.as_bytes()[3], b'1'..=b'9'))
}

impl fmt::Display for TemplateFilePlanError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SemanticContextMismatch { expected, actual } => write!(
                formatter,
                "template file requires semantic context '{}' but declaration carries '{}'",
                expected.as_str(),
                actual.as_str()
            ),
            Self::ArtifactForbiddenForContext {
                artifact_kind,
                semantic_context,
            } => write!(
                formatter,
                "artifact kind '{}' is forbidden for semantic context '{}'",
                artifact_kind.as_str(),
                semantic_context.as_str()
            ),
            Self::EmptyBodyTemplate => formatter.write_str("template file body must not be empty"),
            Self::ProductionCodeFileRoleMismatch {
                role,
                artifact_kind,
            } => write!(
                formatter,
                "Production Code role {role:?} disagrees with artifact kind '{}'",
                artifact_kind.as_str()
            ),
            Self::CorrelatedAlgorithmCodeArtifactRoleMismatch {
                role,
                artifact_kind,
            } => write!(
                formatter,
                "correlated Algorithm Code role {role:?} disagrees with artifact kind '{}'",
                artifact_kind.as_str()
            ),
            Self::PackagedAlgorithmCodeArtifactRoleMismatch {
                role,
                artifact_kind,
            } => write!(
                formatter,
                "packaged Algorithm Code role {role:?} disagrees with artifact kind '{}'",
                artifact_kind.as_str()
            ),
        }
    }
}

impl std::error::Error for TemplateFilePlanError {}

/// Close a source-only Algorithm Code declaration into its checked carrier.
pub fn algorithm_code_template_spec(
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    output_path: AlgorithmCodeSourceOutputPathTemplate,
    body_template: Box<str>,
) -> Result<AlgorithmCodeTemplateSpec, TemplateFilePlanError> {
    AlgorithmCodeTemplateSpec::construct(
        artifact_kind,
        semantic_context,
        output_path,
        body_template,
    )
}

/// Close one packaged Algorithm Code declaration into its role carrier.
pub fn packaged_algorithm_code_template_spec(
    role: AlgorithmCodeArtifactRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: Box<str>,
) -> Result<PackagedAlgorithmCodeTemplateSpec, TemplateFilePlanError> {
    PackagedAlgorithmCodeTemplateSpec::construct(
        role,
        artifact_kind,
        semantic_context,
        body_template,
    )
}

/// Close one correlated Algorithm Code declaration into its role carrier.
pub fn correlated_algorithm_code_template_spec(
    role: CorrelatedAlgorithmCodeArtifactRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: Box<str>,
) -> Result<CorrelatedAlgorithmCodeTemplateSpec, TemplateFilePlanError> {
    CorrelatedAlgorithmCodeTemplateSpec::construct(
        role,
        artifact_kind,
        semantic_context,
        body_template,
    )
}

/// Close one Production Code declaration into its role carrier.
pub fn solve_algorithm_template_spec(
    role: ProductionCodeFileRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: Box<str>,
) -> Result<SolveAlgorithmTemplateSpec, TemplateFilePlanError> {
    SolveAlgorithmTemplateSpec::construct(role, artifact_kind, semantic_context, body_template)
}

/// One opaque file that may be rendered from an `AlgorithmCodePackage`.
///
/// The plan is non-`Clone`, non-`Default`, non-serializable, and
/// non-deserializable. Its body is consumed at construction while its path
/// borrows the manifest-issued basename proof; neither can be replaced or
/// relabelled after the plan exists.
///
/// ```compile_fail
/// use rumoca_core::with_target_invocation_brand;
/// use rumoca_phase_codegen::{
///     AlgorithmCodeSourceOutputPathTemplate, AlgorithmCodeTemplateFile,
///     TemplateArtifactKind, TemplateSemanticContext,
/// };
/// with_target_invocation_brand(|brand| {
///     let output = AlgorithmCodeSourceOutputPathTemplate::construct("model.alg".into()).unwrap();
///     let file = AlgorithmCodeTemplateFile::construct(
///         brand,
///         TemplateArtifactKind::AlgorithmCode,
///         TemplateSemanticContext::Galec,
///         &output,
///         "algorithm Checked",
///     ).unwrap();
///     let _copied_source = file.body_template();
/// });
/// ```
///
/// ```compile_fail
/// use rumoca_phase_codegen::AlgorithmCodeTemplateFile;
/// fn duplicate(file: AlgorithmCodeTemplateFile<'_, '_>) {
///     let _second_authority = file.clone();
/// }
/// ```
///
/// ```compile_fail
/// use rumoca_phase_codegen::AlgorithmCodeTemplateFile;
/// fn require_default<T: Default>() {}
/// require_default::<AlgorithmCodeTemplateFile<'static, 'static>>();
/// ```
///
/// ```compile_fail
/// use rumoca_phase_codegen::AlgorithmCodeTemplateFile;
/// fn require_deserialize<T: for<'de> serde::Deserialize<'de>>() {}
/// require_deserialize::<AlgorithmCodeTemplateFile<'static, 'static>>();
/// ```
pub struct AlgorithmCodeTemplateFile<'inv, 'path> {
    _brand: TargetInvocationBrand<'inv>,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    output_path: &'path AlgorithmCodeSourceOutputPathTemplate,
    body_template: Arc<str>,
}

#[derive(Debug)]
pub struct AlgorithmCodeTemplateSpec {
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    output_path: AlgorithmCodeSourceOutputPathTemplate,
    body_template: Arc<str>,
}

impl AlgorithmCodeTemplateSpec {
    pub(crate) fn construct(
        artifact_kind: TemplateArtifactKind,
        semantic_context: TemplateSemanticContext,
        output_path: AlgorithmCodeSourceOutputPathTemplate,
        body_template: Box<str>,
    ) -> Result<Self, TemplateFilePlanError> {
        validate_algorithm_code_source_file(artifact_kind, semantic_context, &body_template)?;
        Ok(Self {
            artifact_kind,
            semantic_context,
            output_path,
            body_template: Arc::from(body_template),
        })
    }

    #[must_use]
    pub fn bind<'inv>(
        &self,
        brand: TargetInvocationBrand<'inv>,
    ) -> AlgorithmCodeTemplateFile<'inv, '_> {
        AlgorithmCodeTemplateFile {
            _brand: brand,
            artifact_kind: self.artifact_kind,
            semantic_context: self.semantic_context,
            output_path: &self.output_path,
            body_template: Arc::clone(&self.body_template),
        }
    }
}

fn validate_algorithm_code_source_file(
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: &str,
) -> Result<(), TemplateFilePlanError> {
    const EXPECTED: TemplateSemanticContext = TemplateSemanticContext::Galec;
    if semantic_context != EXPECTED {
        return Err(TemplateFilePlanError::SemanticContextMismatch {
            expected: EXPECTED,
            actual: semantic_context,
        });
    }
    if artifact_kind != TemplateArtifactKind::AlgorithmCode {
        return Err(TemplateFilePlanError::ArtifactForbiddenForContext {
            artifact_kind,
            semantic_context,
        });
    }
    if body_template.trim().is_empty() {
        return Err(TemplateFilePlanError::EmptyBodyTemplate);
    }
    Ok(())
}

impl fmt::Debug for AlgorithmCodeTemplateFile<'_, '_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("AlgorithmCodeTemplateFile")
            .field("artifact_kind", &self.artifact_kind)
            .field("semantic_context", &self.semantic_context)
            .finish_non_exhaustive()
    }
}

impl<'inv, 'path> AlgorithmCodeTemplateFile<'inv, 'path> {
    /// Consume one independently-declared file into the Algorithm Code render
    /// authority. Only `.alg` source is admitted. Package XML requires a
    /// role-bound [`PackagedAlgorithmCodeTemplateFile`], so this generic path
    /// cannot construct a manifest relation.
    #[cfg(test)]
    pub(crate) fn construct(
        brand: TargetInvocationBrand<'inv>,
        artifact_kind: TemplateArtifactKind,
        semantic_context: TemplateSemanticContext,
        output_path: &'path AlgorithmCodeSourceOutputPathTemplate,
        body_template: impl Into<Box<str>>,
    ) -> Result<Self, TemplateFilePlanError> {
        let body_template: Box<str> = body_template.into();
        validate_algorithm_code_source_file(artifact_kind, semantic_context, &body_template)?;
        Ok(Self {
            _brand: brand,
            artifact_kind,
            semantic_context,
            output_path,
            body_template: Arc::from(body_template),
        })
    }

    /// Independently declared byte-level artifact kind.
    #[must_use]
    pub const fn artifact_kind(&self) -> TemplateArtifactKind {
        self.artifact_kind
    }

    /// IR crate family containing the exact root selected by this plan type.
    #[must_use]
    pub const fn semantic_context(&self) -> TemplateSemanticContext {
        self.semantic_context
    }

    pub(crate) fn output_path(&self) -> &AlgorithmCodeSourceOutputPathTemplate {
        self.output_path
    }

    pub(crate) fn body_template(&self) -> &str {
        &self.body_template
    }
}

/// One no-path declaration in a standalone packaged Algorithm Code product.
///
/// This type can represent Algorithm Code source or package XML bytes but no
/// output path. Binding to [`PreparedAlgorithmCodePackage`] selects the exact
/// total layout member for its closed role.
///
/// ```compile_fail
/// use rumoca_phase_codegen::PackagedAlgorithmCodeTemplateFile;
/// fn require_default<T: Default>() {}
/// require_default::<PackagedAlgorithmCodeTemplateFile<'static>>();
/// ```
///
/// ```compile_fail
/// use rumoca_phase_codegen::PackagedAlgorithmCodeTemplateFile;
/// fn require_deserialize<T: for<'de> serde::Deserialize<'de>>() {}
/// require_deserialize::<PackagedAlgorithmCodeTemplateFile<'static>>();
/// ```
#[derive(Debug)]
pub struct PackagedAlgorithmCodeTemplateFile<'inv> {
    _brand: TargetInvocationBrand<'inv>,
    role: AlgorithmCodeArtifactRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: Arc<str>,
}

#[derive(Debug)]
pub struct PackagedAlgorithmCodeTemplateSpec {
    role: AlgorithmCodeArtifactRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: Arc<str>,
}

impl PackagedAlgorithmCodeTemplateSpec {
    pub(crate) fn construct(
        role: AlgorithmCodeArtifactRole,
        artifact_kind: TemplateArtifactKind,
        semantic_context: TemplateSemanticContext,
        body_template: Box<str>,
    ) -> Result<Self, TemplateFilePlanError> {
        validate_packaged_algorithm_code_file(
            role,
            artifact_kind,
            semantic_context,
            &body_template,
        )?;
        Ok(Self {
            role,
            artifact_kind,
            semantic_context,
            body_template: Arc::from(body_template),
        })
    }

    #[must_use]
    pub fn bind<'inv>(
        &self,
        brand: TargetInvocationBrand<'inv>,
    ) -> PackagedAlgorithmCodeTemplateFile<'inv> {
        PackagedAlgorithmCodeTemplateFile {
            _brand: brand,
            role: self.role,
            artifact_kind: self.artifact_kind,
            semantic_context: self.semantic_context,
            body_template: Arc::clone(&self.body_template),
        }
    }
}

fn validate_packaged_algorithm_code_file(
    role: AlgorithmCodeArtifactRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: &str,
) -> Result<(), TemplateFilePlanError> {
    const EXPECTED: TemplateSemanticContext = TemplateSemanticContext::Galec;
    if semantic_context != EXPECTED {
        return Err(TemplateFilePlanError::SemanticContextMismatch {
            expected: EXPECTED,
            actual: semantic_context,
        });
    }
    if !matches!(
        (role, artifact_kind),
        (
            AlgorithmCodeArtifactRole::PackageManifest
                | AlgorithmCodeArtifactRole::AlgorithmCodeManifest,
            TemplateArtifactKind::Xml,
        ) | (
            AlgorithmCodeArtifactRole::AlgorithmCodeSource,
            TemplateArtifactKind::AlgorithmCode,
        )
    ) {
        return Err(
            TemplateFilePlanError::PackagedAlgorithmCodeArtifactRoleMismatch {
                role,
                artifact_kind,
            },
        );
    }
    if body_template.trim().is_empty() {
        return Err(TemplateFilePlanError::EmptyBodyTemplate);
    }
    Ok(())
}

impl<'inv> PackagedAlgorithmCodeTemplateFile<'inv> {
    #[cfg(test)]
    pub(crate) fn construct(
        brand: TargetInvocationBrand<'inv>,
        role: AlgorithmCodeArtifactRole,
        artifact_kind: TemplateArtifactKind,
        semantic_context: TemplateSemanticContext,
        body_template: impl Into<Box<str>>,
    ) -> Result<Self, TemplateFilePlanError> {
        let body_template: Box<str> = body_template.into();
        validate_packaged_algorithm_code_file(
            role,
            artifact_kind,
            semantic_context,
            &body_template,
        )?;
        Ok(Self {
            _brand: brand,
            role,
            artifact_kind,
            semantic_context,
            body_template: Arc::from(body_template),
        })
    }

    #[must_use]
    pub const fn artifact_kind(&self) -> TemplateArtifactKind {
        self.artifact_kind
    }

    #[must_use]
    pub const fn semantic_context(&self) -> TemplateSemanticContext {
        self.semantic_context
    }

    #[must_use]
    pub const fn role(&self) -> AlgorithmCodeArtifactRole {
        self.role
    }
}

/// Render authority joining one no-path declaration to one exact member of a
/// prepared standalone Algorithm Code package.
pub struct PreparedPackagedAlgorithmCodeTemplateFile<'inv, 'package, 'file> {
    package: &'package PreparedAlgorithmCodePackage<'inv>,
    file: &'file PackagedAlgorithmCodeTemplateFile<'inv>,
    member: &'package AlgorithmCodeArtifactLayoutMember,
}

impl fmt::Debug for PreparedPackagedAlgorithmCodeTemplateFile<'_, '_, '_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("PreparedPackagedAlgorithmCodeTemplateFile")
            .field("artifact_kind", &self.file.artifact_kind())
            .field("semantic_context", &self.file.semantic_context())
            .field("role", &self.file.role())
            .field("member_path", &self.member.member_path().as_str())
            .finish_non_exhaustive()
    }
}

impl<'inv, 'package, 'file> PreparedPackagedAlgorithmCodeTemplateFile<'inv, 'package, 'file> {
    #[must_use]
    pub const fn artifact_kind(&self) -> TemplateArtifactKind {
        self.file.artifact_kind()
    }

    #[must_use]
    pub const fn semantic_context(&self) -> TemplateSemanticContext {
        self.file.semantic_context()
    }

    #[must_use]
    pub const fn role(&self) -> AlgorithmCodeArtifactRole {
        self.file.role()
    }

    pub(crate) const fn package(&self) -> &'package PreparedAlgorithmCodePackage<'inv> {
        self.package
    }

    pub(crate) const fn member(&self) -> &'package AlgorithmCodeArtifactLayoutMember {
        self.member
    }

    pub(crate) fn body_template(&self) -> &'file str {
        &self.file.body_template
    }
}

/// Bind one no-path declaration to its exact retained package member.
///
/// Role and artifact kind are narrowed exhaustively here. No output path is
/// accepted, compared, or revalidated downstream.
///
/// ```compile_fail
/// use rumoca_phase_codegen::{
///     PackagedAlgorithmCodeTemplateFile, PreparedAlgorithmCodePackage,
///     prepare_packaged_algorithm_code_template_file,
/// };
/// fn mix_origins<'left, 'right>(
///     package: &PreparedAlgorithmCodePackage<'left>,
///     file: &PackagedAlgorithmCodeTemplateFile<'right>,
/// ) {
///     let _ = prepare_packaged_algorithm_code_template_file(package, file);
/// }
/// ```
#[must_use]
pub fn prepare_packaged_algorithm_code_template_file<'inv, 'package, 'file>(
    package: &'package PreparedAlgorithmCodePackage<'inv>,
    file: &'file PackagedAlgorithmCodeTemplateFile<'inv>,
) -> PreparedPackagedAlgorithmCodeTemplateFile<'inv, 'package, 'file> {
    let member = match file.role() {
        AlgorithmCodeArtifactRole::PackageManifest => package.artifact_layout().package_manifest(),
        AlgorithmCodeArtifactRole::AlgorithmCodeManifest => {
            package.artifact_layout().algorithm_code_manifest()
        }
        AlgorithmCodeArtifactRole::AlgorithmCodeSource => {
            package.artifact_layout().algorithm_code_source()
        }
    };
    PreparedPackagedAlgorithmCodeTemplateFile {
        package,
        file,
        member,
    }
}

/// Closed Algorithm Code side of the correlated Algorithm/Production family.
///
/// Unlike the complete package-layout role vocabulary, this type cannot name a
/// Production Code or schema member.  Orchestration must choose one of these
/// three roles before a GALEC-context file can borrow path authority from the
/// prepared Solve product.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CorrelatedAlgorithmCodeArtifactRole {
    PackageManifest,
    AlgorithmCodeManifest,
    AlgorithmCodeSource,
}

impl CorrelatedAlgorithmCodeArtifactRole {
    /// Complete correlated Algorithm Code file-role vocabulary.
    pub const ALL: &'static [Self] = &[
        Self::PackageManifest,
        Self::AlgorithmCodeManifest,
        Self::AlgorithmCodeSource,
    ];
}

/// One Algorithm Code-side declaration in a correlated package.
///
/// This type deliberately has no output-path input or accessor. Whole-target
/// construction has already consumed each declared role/path into
/// [`ProductionArtifactLayout`]; binding selects that total member directly
/// instead of validating or copying the path a second time.
///
/// ```compile_fail
/// use rumoca_phase_codegen::CorrelatedAlgorithmCodeTemplateFile;
/// fn require_default<T: Default>() {}
/// require_default::<CorrelatedAlgorithmCodeTemplateFile<'static>>();
/// ```
///
/// ```compile_fail
/// use rumoca_phase_codegen::CorrelatedAlgorithmCodeTemplateFile;
/// fn require_deserialize<T: for<'de> serde::Deserialize<'de>>() {}
/// require_deserialize::<CorrelatedAlgorithmCodeTemplateFile<'static>>();
/// ```
///
/// ```compile_fail
/// use rumoca_core::with_target_invocation_brand;
/// use rumoca_phase_codegen::{
///     CorrelatedAlgorithmCodeArtifactRole, CorrelatedAlgorithmCodeTemplateFile,
///     TemplateArtifactKind, TemplateSemanticContext,
/// };
/// with_target_invocation_brand(|brand| {
///     let file = CorrelatedAlgorithmCodeTemplateFile::construct(
///         brand,
///         CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeSource,
///         TemplateArtifactKind::AlgorithmCode,
///         TemplateSemanticContext::Galec,
///         "body",
///     ).unwrap();
///     let _ = file.output_path_template();
/// });
/// ```
#[derive(Debug)]
pub struct CorrelatedAlgorithmCodeTemplateFile<'inv> {
    _brand: TargetInvocationBrand<'inv>,
    role: CorrelatedAlgorithmCodeArtifactRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: Arc<str>,
}

#[derive(Debug)]
pub struct CorrelatedAlgorithmCodeTemplateSpec {
    role: CorrelatedAlgorithmCodeArtifactRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: Arc<str>,
}

impl CorrelatedAlgorithmCodeTemplateSpec {
    pub(crate) fn construct(
        role: CorrelatedAlgorithmCodeArtifactRole,
        artifact_kind: TemplateArtifactKind,
        semantic_context: TemplateSemanticContext,
        body_template: Box<str>,
    ) -> Result<Self, TemplateFilePlanError> {
        validate_correlated_algorithm_code_file(
            role,
            artifact_kind,
            semantic_context,
            &body_template,
        )?;
        Ok(Self {
            role,
            artifact_kind,
            semantic_context,
            body_template: Arc::from(body_template),
        })
    }

    #[must_use]
    pub fn bind<'inv>(
        &self,
        brand: TargetInvocationBrand<'inv>,
    ) -> CorrelatedAlgorithmCodeTemplateFile<'inv> {
        CorrelatedAlgorithmCodeTemplateFile {
            _brand: brand,
            role: self.role,
            artifact_kind: self.artifact_kind,
            semantic_context: self.semantic_context,
            body_template: Arc::clone(&self.body_template),
        }
    }
}

fn validate_correlated_algorithm_code_file(
    role: CorrelatedAlgorithmCodeArtifactRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: &str,
) -> Result<(), TemplateFilePlanError> {
    const EXPECTED: TemplateSemanticContext = TemplateSemanticContext::Galec;
    if semantic_context != EXPECTED {
        return Err(TemplateFilePlanError::SemanticContextMismatch {
            expected: EXPECTED,
            actual: semantic_context,
        });
    }
    if !matches!(
        (role, artifact_kind),
        (
            CorrelatedAlgorithmCodeArtifactRole::PackageManifest
                | CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeManifest,
            TemplateArtifactKind::Xml,
        ) | (
            CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeSource,
            TemplateArtifactKind::AlgorithmCode,
        )
    ) {
        return Err(
            TemplateFilePlanError::CorrelatedAlgorithmCodeArtifactRoleMismatch {
                role,
                artifact_kind,
            },
        );
    }
    if body_template.trim().is_empty() {
        return Err(TemplateFilePlanError::EmptyBodyTemplate);
    }
    Ok(())
}

impl<'inv> CorrelatedAlgorithmCodeTemplateFile<'inv> {
    #[cfg(test)]
    pub(crate) fn construct(
        brand: TargetInvocationBrand<'inv>,
        role: CorrelatedAlgorithmCodeArtifactRole,
        artifact_kind: TemplateArtifactKind,
        semantic_context: TemplateSemanticContext,
        body_template: impl Into<Box<str>>,
    ) -> Result<Self, TemplateFilePlanError> {
        let body_template: Box<str> = body_template.into();
        validate_correlated_algorithm_code_file(
            role,
            artifact_kind,
            semantic_context,
            &body_template,
        )?;
        Ok(Self {
            _brand: brand,
            role,
            artifact_kind,
            semantic_context,
            body_template: Arc::from(body_template),
        })
    }

    #[must_use]
    pub const fn artifact_kind(&self) -> TemplateArtifactKind {
        self.artifact_kind
    }

    #[must_use]
    pub const fn semantic_context(&self) -> TemplateSemanticContext {
        self.semantic_context
    }

    #[must_use]
    pub const fn role(&self) -> CorrelatedAlgorithmCodeArtifactRole {
        self.role
    }
}

/// Render authority joining one GALEC-context file to the exact correlated
/// package member retained by a prepared Solve Algorithm Production product.
///
/// A renderer obtains the output path only from [`member`](Self::member), so
/// model-name interpolation or later relabelling cannot move correlated bytes
/// outside the checked package layout.
pub struct PreparedCorrelatedAlgorithmCodeTemplateFile<'inv, 'production, 'file> {
    production: &'production PreparedSolveAlgorithmProduction<'inv>,
    file: &'file CorrelatedAlgorithmCodeTemplateFile<'inv>,
    member: &'production ProductionArtifactLayoutMember,
}

impl fmt::Debug for PreparedCorrelatedAlgorithmCodeTemplateFile<'_, '_, '_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("PreparedCorrelatedAlgorithmCodeTemplateFile")
            .field("artifact_kind", &self.file.artifact_kind())
            .field("semantic_context", &self.file.semantic_context())
            .field("role", &self.file.role())
            .field("member_path", &self.member.member_path().as_str())
            .finish_non_exhaustive()
    }
}

impl<'inv, 'production, 'file>
    PreparedCorrelatedAlgorithmCodeTemplateFile<'inv, 'production, 'file>
{
    #[must_use]
    pub const fn artifact_kind(&self) -> TemplateArtifactKind {
        self.file.artifact_kind()
    }

    #[must_use]
    pub const fn semantic_context(&self) -> TemplateSemanticContext {
        self.file.semantic_context()
    }

    #[must_use]
    pub const fn role(&self) -> CorrelatedAlgorithmCodeArtifactRole {
        self.file.role()
    }

    pub(crate) const fn production(&self) -> &'production PreparedSolveAlgorithmProduction<'inv> {
        self.production
    }

    pub(crate) const fn member(&self) -> &'production ProductionArtifactLayoutMember {
        self.member
    }

    pub(crate) fn body_template(&self) -> &'file str {
        &self.file.body_template
    }
}

/// Bind one GALEC-context declaration to the matching member in the exact
/// correlated Solve product.
///
/// Correlated paths are static checked package facts and are never accepted
/// from this per-file declaration, rendered from `model_name`, or compared a
/// second time.
///
/// ```compile_fail
/// use rumoca_phase_codegen::{
///     CorrelatedAlgorithmCodeTemplateFile, PreparedSolveAlgorithmProduction,
///     prepare_correlated_algorithm_code_template_file,
/// };
/// fn mix_origins<'left, 'right>(
///     production: &PreparedSolveAlgorithmProduction<'left>,
///     file: &CorrelatedAlgorithmCodeTemplateFile<'right>,
/// ) {
///     let _ = prepare_correlated_algorithm_code_template_file(production, file);
/// }
/// ```
#[must_use]
pub fn prepare_correlated_algorithm_code_template_file<'inv, 'production, 'file>(
    production: &'production PreparedSolveAlgorithmProduction<'inv>,
    file: &'file CorrelatedAlgorithmCodeTemplateFile<'inv>,
) -> PreparedCorrelatedAlgorithmCodeTemplateFile<'inv, 'production, 'file> {
    let member = match file.role() {
        CorrelatedAlgorithmCodeArtifactRole::PackageManifest => production
            .presentation()
            .artifact_layout()
            .package_manifest(),
        CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeManifest => production
            .presentation()
            .artifact_layout()
            .algorithm_code_manifest(),
        CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeSource => production
            .presentation()
            .artifact_layout()
            .algorithm_code_source(),
    };
    PreparedCorrelatedAlgorithmCodeTemplateFile {
        production,
        file,
        member,
    }
}

/// Closed Production Code member vocabulary. Algorithm Code, package-root,
/// and schema members are absent at the type level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProductionCodeFileRole {
    Manifest,
    Header,
    Source,
}

impl ProductionCodeFileRole {
    pub const ALL: &'static [Self] = &[Self::Manifest, Self::Header, Self::Source];
}

/// One opaque file declared for the checked `SolveAlgorithmBlock` view.
///
/// Constructing this declaration does not authorize rendering.  It must still
/// be consumed together with [`PreparedSolveAlgorithmProduction`] by
/// [`prepare_solve_algorithm_template_file`].
///
/// ```compile_fail
/// use rumoca_phase_codegen::SolveAlgorithmTemplateFile;
/// fn require_default<T: Default>() {}
/// require_default::<SolveAlgorithmTemplateFile<'static>>();
/// ```
///
/// ```compile_fail
/// use rumoca_phase_codegen::SolveAlgorithmTemplateFile;
/// fn require_deserialize<T: for<'de> serde::Deserialize<'de>>() {}
/// require_deserialize::<SolveAlgorithmTemplateFile<'static>>();
/// ```
#[derive(Debug)]
pub struct SolveAlgorithmTemplateFile<'inv> {
    _brand: TargetInvocationBrand<'inv>,
    role: ProductionCodeFileRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: Arc<str>,
}

/// Construction-checked, invocation-independent Production Code template.
///
/// Target-bundle construction owns the only fallible admission. Binding the
/// retained specification to a target invocation is therefore infallible and
/// cannot change its role, artifact kind, context, or bytes.
#[derive(Debug)]
pub struct SolveAlgorithmTemplateSpec {
    role: ProductionCodeFileRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: Arc<str>,
}

impl SolveAlgorithmTemplateSpec {
    pub(crate) fn construct(
        role: ProductionCodeFileRole,
        artifact_kind: TemplateArtifactKind,
        semantic_context: TemplateSemanticContext,
        body_template: Box<str>,
    ) -> Result<Self, TemplateFilePlanError> {
        validate_solve_algorithm_file(role, artifact_kind, semantic_context, &body_template)?;
        Ok(Self {
            role,
            artifact_kind,
            semantic_context,
            body_template: Arc::from(body_template),
        })
    }

    #[must_use]
    pub fn bind<'inv>(
        &self,
        brand: TargetInvocationBrand<'inv>,
    ) -> SolveAlgorithmTemplateFile<'inv> {
        SolveAlgorithmTemplateFile {
            _brand: brand,
            role: self.role,
            artifact_kind: self.artifact_kind,
            semantic_context: self.semantic_context,
            body_template: Arc::clone(&self.body_template),
        }
    }
}

fn validate_solve_algorithm_file(
    role: ProductionCodeFileRole,
    artifact_kind: TemplateArtifactKind,
    semantic_context: TemplateSemanticContext,
    body_template: &str,
) -> Result<(), TemplateFilePlanError> {
    const EXPECTED: TemplateSemanticContext = TemplateSemanticContext::Solve;
    if semantic_context != EXPECTED {
        return Err(TemplateFilePlanError::SemanticContextMismatch {
            expected: EXPECTED,
            actual: semantic_context,
        });
    }
    if !matches!(
        (role, artifact_kind),
        (ProductionCodeFileRole::Manifest, TemplateArtifactKind::Xml)
            | (
                ProductionCodeFileRole::Header,
                TemplateArtifactKind::CHeader,
            )
            | (
                ProductionCodeFileRole::Source,
                TemplateArtifactKind::CSource,
            )
    ) {
        return Err(TemplateFilePlanError::ProductionCodeFileRoleMismatch {
            role,
            artifact_kind,
        });
    }
    if body_template.trim().is_empty() {
        return Err(TemplateFilePlanError::EmptyBodyTemplate);
    }
    Ok(())
}

impl<'inv> SolveAlgorithmTemplateFile<'inv> {
    /// Consume one independently-declared Production Code file.
    ///
    /// C/H and Production Code XML metadata are the only currently specified
    /// artifacts.  Algorithm Code source is categorically refused.
    #[cfg(test)]
    pub(crate) fn construct(
        brand: TargetInvocationBrand<'inv>,
        role: ProductionCodeFileRole,
        artifact_kind: TemplateArtifactKind,
        semantic_context: TemplateSemanticContext,
        body_template: impl Into<Box<str>>,
    ) -> Result<Self, TemplateFilePlanError> {
        let body_template: Box<str> = body_template.into();
        validate_solve_algorithm_file(role, artifact_kind, semantic_context, &body_template)?;
        Ok(Self {
            _brand: brand,
            role,
            artifact_kind,
            semantic_context,
            body_template: Arc::from(body_template),
        })
    }

    /// Independently declared byte-level artifact kind.
    #[must_use]
    pub const fn artifact_kind(&self) -> TemplateArtifactKind {
        self.artifact_kind
    }

    /// IR crate family containing the exact root selected by this plan type.
    #[must_use]
    pub const fn semantic_context(&self) -> TemplateSemanticContext {
        self.semantic_context
    }

    #[must_use]
    pub const fn role(&self) -> ProductionCodeFileRole {
        self.role
    }
}

/// Render authority joining one file to a fully prepared Solve-owned
/// Production Code owner.
pub struct PreparedSolveAlgorithmTemplateFile<'inv, 'production, 'file> {
    production: &'production PreparedSolveAlgorithmProduction<'inv>,
    file: &'file SolveAlgorithmTemplateFile<'inv>,
    member: &'production ProductionArtifactLayoutMember,
}

impl fmt::Debug for PreparedSolveAlgorithmTemplateFile<'_, '_, '_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("PreparedSolveAlgorithmTemplateFile")
            .field("artifact_kind", &self.file.artifact_kind())
            .field("semantic_context", &self.file.semantic_context())
            .field("role", &self.file.role())
            .field("member_path", &self.member.member_path().as_str())
            .finish_non_exhaustive()
    }
}

impl<'inv, 'production, 'file> PreparedSolveAlgorithmTemplateFile<'inv, 'production, 'file> {
    /// Independently declared byte-level artifact kind.
    #[must_use]
    pub const fn artifact_kind(&self) -> TemplateArtifactKind {
        self.file.artifact_kind()
    }

    /// IR crate family containing the exact root selected by this plan type.
    #[must_use]
    pub const fn semantic_context(&self) -> TemplateSemanticContext {
        self.file.semantic_context()
    }

    #[must_use]
    pub const fn role(&self) -> ProductionCodeFileRole {
        self.file.role()
    }

    pub(crate) const fn production(&self) -> &'production PreparedSolveAlgorithmProduction<'inv> {
        self.production
    }

    pub(crate) const fn member(&self) -> &'production ProductionArtifactLayoutMember {
        self.member
    }

    pub(crate) fn body_template(&self) -> &'file str {
        &self.file.body_template
    }
}

/// Bind one Solve-owned file to the fully prepared Production Code authority.
///
/// The file is borrowed from the construction-issued declaration sequence;
/// its retained role selects the exact member without a parallel role array.
///
/// ```compile_fail
/// use rumoca_phase_codegen::{
///     PreparedSolveAlgorithmProduction, SolveAlgorithmTemplateFile,
///     prepare_solve_algorithm_template_file,
/// };
/// fn mix_origins<'left, 'right>(
///     production: &PreparedSolveAlgorithmProduction<'left>,
///     file: &SolveAlgorithmTemplateFile<'right>,
/// ) {
///     let _ = prepare_solve_algorithm_template_file(production, file);
/// }
/// ```
#[must_use]
pub fn prepare_solve_algorithm_template_file<'inv, 'production, 'file>(
    production: &'production PreparedSolveAlgorithmProduction<'inv>,
    file: &'file SolveAlgorithmTemplateFile<'inv>,
) -> PreparedSolveAlgorithmTemplateFile<'inv, 'production, 'file> {
    let member = match file.role() {
        ProductionCodeFileRole::Header => production
            .presentation()
            .artifact_layout()
            .production_header(),
        ProductionCodeFileRole::Source => production
            .presentation()
            .artifact_layout()
            .production_source(),
        ProductionCodeFileRole::Manifest => production
            .presentation()
            .artifact_layout()
            .production_manifest(),
    };
    PreparedSolveAlgorithmTemplateFile {
        production,
        file,
        member,
    }
}

// Byte-level artifact vocabulary and the checked per-file plans above are the
// only exports of this module: every target-aware fact (artifact stem,
// identity, or checksum) now enters a render exclusively through the neutral
// `crate::TemplateBindings` carrier, owned by the compiler.
