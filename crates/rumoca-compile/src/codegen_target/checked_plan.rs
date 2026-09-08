mod bundle;
mod target_artifact;
#[cfg(test)]
mod tests;

use super::*;

pub use target_artifact::PublishedTargetArtifact;
pub use target_artifact::{
    ArtifactGenerationInstant, ArtifactIdentitySeed, ArtifactSessionInput,
    ArtifactSessionInputError, CompletedArtifactMemberRef, CompletedPackage, CompletedRenderedFile,
    CompletedRenderedFileRef, CompletedTargetArtifact, CompletedUnpackaged,
};

pub struct TargetBundle {
    pub(super) source: TargetBundleSource,
}

pub(super) enum TargetBundleSource {
    Builtin {
        target: &'static templates::BuiltinTarget,
    },
    Directory {
        dir: PathBuf,
        manifest: String,
    },
    /// One complete in-memory target input. Manifest, template, and asset
    /// inventories travel together into [`TargetBundle::check`]; none of the
    /// maps is an independently usable render authority.
    InMemory {
        label: String,
        manifest: String,
        templates: BTreeMap<String, String>,
        assets: BTreeMap<String, BTreeMap<String, Vec<u8>>>,
    },
}

/// One target bundle joined to the manifest parsed and checked from those
/// exact bytes.
///
/// The fields are private and every directory-backed template and asset byte
/// is snapshotted during [`TargetBundle::check`]. No API accepts a separate
/// file declaration, asset declaration, or manifest, so later filesystem
/// mutation and foreign-member pairing cannot affect this authority.
///
/// External crates cannot split two checked targets and recombine their
/// metadata and render plans because both fields are private:
///
/// ```compile_fail
/// use rumoca_compile::codegen::targets::CheckedTargetBundle;
///
/// fn splice(first: CheckedTargetBundle, second: CheckedTargetBundle) {
///     let CheckedTargetBundle { metadata, .. } = first;
///     let CheckedTargetBundle { render_authority, .. } = second;
///     let _foreign = CheckedTargetBundle { metadata, render_authority };
/// }
/// ```
///
/// The old public preparation issuer is intentionally absent:
///
/// ```compile_fail
/// use rumoca_phase_codegen::issue_strict_target_preparation_authority;
///
/// let _forged = issue_strict_target_preparation_authority();
/// ```
pub struct CheckedTargetBundle {
    metadata: CheckedTargetMetadata,
    render_authority: CheckedTargetRenderAuthority,
}

/// Immutable non-member target facts shared after the affine render authority
/// is consumed. File declarations, templates, member paths, and asset bytes
/// are deliberately absent from this public surface.
struct CheckedTargetMetadata {
    label: Box<str>,
    description: Option<Box<str>>,
    completion_message: Option<Box<str>>,
    artifact_identity_scope: TargetArtifactIdentityScope,
    artifact_identity_keys: Box<[String]>,
}

#[derive(Debug)]
pub(super) struct CheckedTargetCapabilityContract {
    pub(super) label: Box<str>,
    pub(super) required_product: TargetRequiredProduct,
    pub(super) capabilities: TargetCapabilities,
}

impl std::fmt::Debug for CheckedTargetBundle {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("CheckedTargetBundle")
            .field("label", &self.metadata.label)
            .field(
                "is_packaged",
                &matches!(
                    self.render_authority,
                    CheckedTargetRenderAuthority::Packaged { .. }
                ),
            )
            .finish_non_exhaustive()
    }
}

impl CheckedTargetRenderAuthority {
    #[must_use]
    fn into_plan(self) -> CheckedTargetRenderPlan {
        match self {
            CheckedTargetRenderAuthority::Packaged {
                policy,
                product_layout,
            } => CheckedTargetRenderPlan::Packaged(CheckedTargetPackagePlan {
                policy,
                product_layout,
            }),
            CheckedTargetRenderAuthority::Unpackaged { product } => {
                CheckedTargetRenderPlan::Unpackaged(CheckedUnpackagedTargetPlan { product })
            }
        }
    }
}

impl CheckedTargetMetadata {
    #[must_use]
    fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }

    #[must_use]
    fn completion_message(&self) -> Option<&str> {
        self.completion_message.as_deref()
    }

    #[must_use]
    fn label(&self) -> &str {
        &self.label
    }

    #[must_use]
    fn artifact_identity_keys(&self) -> &[String] {
        &self.artifact_identity_keys
    }

    #[must_use]
    fn artifact_identity_scope(&self) -> &TargetArtifactIdentityScope {
        &self.artifact_identity_scope
    }
}

/// Exhaustive borrowed render authority for one checked target bundle.
enum CheckedTargetRenderPlan {
    Packaged(CheckedTargetPackagePlan),
    Unpackaged(CheckedUnpackagedTargetPlan),
}

/// Borrowed authority for one checked non-package target's file sequence.
/// It has no public constructor and cannot be obtained from a packaged target.
struct CheckedUnpackagedTargetPlan {
    product: CheckedUnpackagedTargetProduct,
}

impl CheckedUnpackagedTargetPlan {
    fn into_product_plan(self) -> Result<CheckedUnpackagedTargetProductPlan> {
        Ok(match self.product {
            CheckedUnpackagedTargetProduct::AlgorithmCode {
                capability_contract,
                arithmetic,
                steps,
            } => CheckedUnpackagedTargetProductPlan::AlgorithmCode(
                CheckedUnpackagedAlgorithmCodePlan {
                    capability_contract,
                    arithmetic,
                    fold: CheckedAlgorithmCodeSourceFold { steps },
                },
            ),
            CheckedUnpackagedTargetProduct::Other { product, steps } => {
                CheckedUnpackagedTargetProductPlan::Other(product.into_unpacked_plan(steps)?)
            }
        })
    }
}

enum CheckedUnpackagedTargetProductPlan {
    AlgorithmCode(CheckedUnpackagedAlgorithmCodePlan),
    Other(CheckedOtherUnpackagedTargetPlan),
}

struct CheckedUnpackagedAlgorithmCodePlan {
    capability_contract: CheckedTargetCapabilityContract,
    arithmetic: TargetAlgorithmCodeArithmetic,
    fold: CheckedAlgorithmCodeSourceFold,
}

impl CheckedUnpackagedAlgorithmCodePlan {
    #[must_use]
    fn into_parts(
        self,
    ) -> (
        CheckedTargetCapabilityContract,
        TargetAlgorithmCodeArithmetic,
        CheckedAlgorithmCodeSourceFold,
    ) {
        (self.capability_contract, self.arithmetic, self.fold)
    }
}

enum CheckedOtherUnpackagedTargetPlan {
    Ast(CheckedDirectUnpackagedFold<DirectTargetTemplate>),
    Flat(CheckedDirectUnpackagedFold<DirectTargetTemplate>),
    Dae {
        capability_contract: CheckedTargetCapabilityContract,
        fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
    },
    SolveModel {
        capability_contract: CheckedTargetCapabilityContract,
        fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
    },
    FmiComponent {
        capability_contract: CheckedTargetCapabilityContract,
        fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
    },
}

struct CheckedAlgorithmCodeSourceFold {
    steps: Box<[Box<AlgorithmCodeSourceRenderPlanStep>]>,
}

struct CheckedDirectUnpackagedFold<T> {
    steps: Box<[Box<DirectUnpackagedRenderPlanStep<T>>]>,
}

struct CheckedDirectPackageClosure<T> {
    fold_members: Box<[CheckedDirectPackageMemberPlan<T>]>,
    package_paths: Box<[Arc<CheckedTargetPackagePath>]>,
}

enum CheckedTargetRenderAuthority {
    Unpackaged {
        product: CheckedUnpackagedTargetProduct,
    },
    Packaged {
        policy: CheckedTargetPackagePolicy,
        product_layout: CheckedTargetPackageProductLayout,
    },
}

enum CheckedTargetPackageMemberPlan {
    File {
        step: TargetRenderPlanStep,
        package_path: Arc<CheckedTargetPackagePath>,
    },
    Asset(CheckedTargetPackageAsset),
}

struct CheckedTargetPackageAsset {
    package_path: Arc<CheckedTargetPackagePath>,
    source: Arc<str>,
    relative_path: Arc<str>,
    position: usize,
    bytes: Arc<[u8]>,
}

enum CheckedTargetPackageProductLayout {
    AlgorithmCode {
        capability_contract: CheckedTargetCapabilityContract,
        arithmetic: TargetAlgorithmCodeArithmetic,
        artifact_layout: CheckedAlgorithmCodeLayout,
        fold_members: Box<[CheckedPackagedAlgorithmCodeMemberPlan]>,
    },
    SolveAlgorithm {
        capability_contract: CheckedTargetCapabilityContract,
        arithmetic: TargetAlgorithmCodeArithmetic,
        production_profile: rumoca_phase_codegen::SolveAlgorithmProductionProfile,
        layout: CheckedSolveAlgorithmLayout,
        fold_members: Box<[CheckedSolveAlgorithmMemberPlan]>,
    },
    Ast {
        fold_members: Box<[CheckedDirectPackageMemberPlan<DirectTargetTemplate>]>,
    },
    Flat {
        fold_members: Box<[CheckedDirectPackageMemberPlan<DirectTargetTemplate>]>,
    },
    Dae {
        capability_contract: CheckedTargetCapabilityContract,
        fold_members: Box<[CheckedDirectPackageMemberPlan<DirectTargetTemplate>]>,
    },
    SolveModel {
        capability_contract: CheckedTargetCapabilityContract,
        fold_members: Box<[CheckedDirectPackageMemberPlan<DirectTargetTemplate>]>,
    },
    FmiComponent {
        capability_contract: CheckedTargetCapabilityContract,
        fold_members: Box<[CheckedDirectPackageMemberPlan<DirectTargetTemplate>]>,
    },
}

struct CheckedTargetPackageProductClosure {
    product_layout: CheckedTargetPackageProductLayout,
    package_paths: Box<[Arc<CheckedTargetPackagePath>]>,
}

enum CheckedUnpackagedTargetProduct {
    AlgorithmCode {
        capability_contract: CheckedTargetCapabilityContract,
        arithmetic: TargetAlgorithmCodeArithmetic,
        steps: Box<[Box<AlgorithmCodeSourceRenderPlanStep>]>,
    },
    Other {
        product: CheckedDirectTargetProduct,
        steps: Box<[TargetRenderPlanStep]>,
    },
}

#[derive(Debug)]
enum CheckedDirectTargetProduct {
    Ast,
    Flat,
    Dae(CheckedTargetCapabilityContract),
    SolveModel(CheckedTargetCapabilityContract),
    FmiComponent(CheckedTargetCapabilityContract),
}

impl CheckedDirectTargetProduct {
    fn construct(
        product: TargetRequiredProduct,
        capability_contract: Option<CheckedTargetCapabilityContract>,
    ) -> Result<Self> {
        match product {
            TargetRequiredProduct::Ast if capability_contract.is_none() => Ok(Self::Ast),
            TargetRequiredProduct::Flat if capability_contract.is_none() => Ok(Self::Flat),
            TargetRequiredProduct::Dae => {
                Ok(Self::Dae(capability_contract.context(
                    "checked DAE target lost its construction-issued capability contract",
                )?))
            }
            TargetRequiredProduct::SolveModel => {
                Ok(Self::SolveModel(capability_contract.context(
                    "checked SolveModel target lost its construction-issued capability contract",
                )?))
            }
            TargetRequiredProduct::FmiComponent => {
                Ok(Self::FmiComponent(capability_contract.context(
                    "checked FMI target lost its construction-issued capability contract",
                )?))
            }
            TargetRequiredProduct::Ast | TargetRequiredProduct::Flat => {
                bail!("non-DAE target received an inapplicable capability contract")
            }
            TargetRequiredProduct::AlgorithmCodePackage
            | TargetRequiredProduct::SolveAlgorithmProduct => {
                bail!("Algorithm Code product cannot inhabit the direct target product sum")
            }
        }
    }

    fn into_unpacked_plan(
        self,
        steps: Box<[TargetRenderPlanStep]>,
    ) -> Result<CheckedOtherUnpackagedTargetPlan> {
        use rumoca_phase_codegen::TemplateSemanticContext as Context;
        Ok(match self {
            Self::Ast => CheckedOtherUnpackagedTargetPlan::Ast(CheckedDirectUnpackagedFold {
                steps: close_direct_steps(steps, Context::Ast)?,
            }),
            Self::Flat => CheckedOtherUnpackagedTargetPlan::Flat(CheckedDirectUnpackagedFold {
                steps: close_direct_steps(steps, Context::Flat)?,
            }),
            Self::Dae(capability_contract) => CheckedOtherUnpackagedTargetPlan::Dae {
                capability_contract,
                fold: CheckedDirectUnpackagedFold {
                    steps: close_direct_steps(steps, Context::Dae)?,
                },
            },
            Self::SolveModel(capability_contract) => CheckedOtherUnpackagedTargetPlan::SolveModel {
                capability_contract,
                fold: CheckedDirectUnpackagedFold {
                    steps: close_direct_steps(steps, Context::Solve)?,
                },
            },
            Self::FmiComponent(capability_contract) => {
                CheckedOtherUnpackagedTargetPlan::FmiComponent {
                    capability_contract,
                    fold: CheckedDirectUnpackagedFold {
                        steps: close_direct_steps(steps, Context::Solve)?,
                    },
                }
            }
        })
    }

    fn into_package_closure(
        self,
        members: Box<[CheckedTargetPackageMemberPlan]>,
    ) -> Result<CheckedTargetPackageProductClosure> {
        use rumoca_phase_codegen::TemplateSemanticContext as Context;
        match self {
            Self::Ast => {
                let closure = close_direct_package_members(members, Context::Ast)?;
                Ok(CheckedTargetPackageProductClosure {
                    product_layout: CheckedTargetPackageProductLayout::Ast {
                        fold_members: closure.fold_members,
                    },
                    package_paths: closure.package_paths,
                })
            }
            Self::Flat => {
                let closure = close_direct_package_members(members, Context::Flat)?;
                Ok(CheckedTargetPackageProductClosure {
                    product_layout: CheckedTargetPackageProductLayout::Flat {
                        fold_members: closure.fold_members,
                    },
                    package_paths: closure.package_paths,
                })
            }
            Self::Dae(capability_contract) => {
                let closure = close_direct_package_members(members, Context::Dae)?;
                Ok(CheckedTargetPackageProductClosure {
                    product_layout: CheckedTargetPackageProductLayout::Dae {
                        capability_contract,
                        fold_members: closure.fold_members,
                    },
                    package_paths: closure.package_paths,
                })
            }
            Self::SolveModel(capability_contract) => {
                let closure = close_direct_package_members(members, Context::Solve)?;
                Ok(CheckedTargetPackageProductClosure {
                    product_layout: CheckedTargetPackageProductLayout::SolveModel {
                        capability_contract,
                        fold_members: closure.fold_members,
                    },
                    package_paths: closure.package_paths,
                })
            }
            Self::FmiComponent(capability_contract) => {
                let closure = close_direct_package_members(members, Context::Solve)?;
                Ok(CheckedTargetPackageProductClosure {
                    product_layout: CheckedTargetPackageProductLayout::FmiComponent {
                        capability_contract,
                        fold_members: closure.fold_members,
                    },
                    package_paths: closure.package_paths,
                })
            }
        }
    }
}

/// Checked body and output-path templates for one direct target member.
///
/// The semantic context of the enclosing product fixes which checked semantic
/// root the body is rendered against; this carrier holds only the checked
/// template text. It carries no rendered path, member identity, or artifact
/// session: those target-aware facts are owned by the completion fold, and the
/// phase renderer sees only the body text.
#[derive(Debug)]
struct DirectTargetTemplate {
    body: Box<str>,
    output_path: Box<str>,
}

impl DirectTargetTemplate {
    fn construct(
        context: rumoca_phase_codegen::TemplateSemanticContext,
        artifact_kind: rumoca_phase_codegen::TemplateArtifactKind,
        output_path: Box<str>,
        body: Box<str>,
    ) -> Result<Self> {
        if output_path.trim().is_empty() {
            bail!("direct target template output path must not be empty");
        }
        if body.trim().is_empty() {
            bail!("direct target template body must not be empty");
        }
        if !direct_artifact_is_admitted(artifact_kind, context) {
            bail!(
                "artifact kind '{}' is forbidden for semantic context '{}'",
                artifact_kind.as_str(),
                context.as_str()
            );
        }
        Ok(Self { body, output_path })
    }

    fn body(&self) -> &str {
        &self.body
    }

    fn output_path(&self) -> &str {
        &self.output_path
    }
}

/// Whether one byte-level artifact kind is legal for a direct semantic context.
///
/// Algorithm Code source is never a direct member (it has its own package
/// production path). The C artifact family is admitted only under the Solve
/// context, which owns the executable Solve-model and FMI-component roots.
const fn direct_artifact_is_admitted(
    artifact: rumoca_phase_codegen::TemplateArtifactKind,
    context: rumoca_phase_codegen::TemplateSemanticContext,
) -> bool {
    use rumoca_phase_codegen::TemplateArtifactKind as Kind;
    use rumoca_phase_codegen::TemplateSemanticContext as Context;
    match artifact {
        Kind::AlgorithmCode => false,
        Kind::CHeader | Kind::CSource | Kind::CudaSource => matches!(context, Context::Solve),
        Kind::Json
        | Kind::Markdown
        | Kind::MlirSource
        | Kind::ModelicaSource
        | Kind::PythonSource
        | Kind::RustSource
        | Kind::Text
        | Kind::Toml
        | Kind::WgslSource
        | Kind::Xml => true,
    }
}

fn close_direct_steps(
    steps: Box<[TargetRenderPlanStep]>,
    context: rumoca_phase_codegen::TemplateSemanticContext,
) -> Result<Box<[Box<DirectUnpackagedRenderPlanStep<DirectTargetTemplate>>]>> {
    steps
        .into_vec()
        .into_iter()
        .map(|step| {
            let (core, prepared_member, file) = ClosedRenderPlanStep::from_step(step);
            let TargetPreparedMemberPlan::Direct = prepared_member else {
                bail!("direct target contains a non-direct render member");
            };
            let mode = file.declaration.mode_bits;
            let template = DirectTargetTemplate::construct(
                context,
                phase_artifact_kind(file.declaration.artifact_kind),
                file.declaration.path,
                file.template_body,
            )?;
            Ok(Box::new(DirectUnpackagedRenderPlanStep {
                core,
                template,
                mode,
            }))
        })
        .collect::<Result<Vec<_>>>()
        .map(Vec::into_boxed_slice)
}

fn close_direct_package_members(
    members: Box<[CheckedTargetPackageMemberPlan]>,
    context: rumoca_phase_codegen::TemplateSemanticContext,
) -> Result<CheckedDirectPackageClosure<DirectTargetTemplate>> {
    let mut fold_members = Vec::with_capacity(members.len());
    let mut package_paths = Vec::with_capacity(members.len());
    for member in members.into_vec() {
        match member {
            CheckedTargetPackageMemberPlan::File { step, package_path } => {
                package_paths.push(Arc::clone(&package_path));
                let (core, prepared_member, file) = ClosedRenderPlanStep::from_step(step);
                let TargetPreparedMemberPlan::Direct = prepared_member else {
                    bail!("direct package product contains a non-direct render member");
                };
                let template = DirectTargetTemplate::construct(
                    context,
                    phase_artifact_kind(file.declaration.artifact_kind),
                    file.declaration.path,
                    file.template_body,
                )?;
                fold_members.push(CheckedDirectPackageMemberPlan::File {
                    step: Box::new(DirectPackageRenderPlanStep { core, template }),
                    package_path,
                });
            }
            CheckedTargetPackageMemberPlan::Asset(asset) => {
                package_paths.push(Arc::clone(&asset.package_path));
                fold_members.push(CheckedDirectPackageMemberPlan::Asset(asset));
            }
        }
    }
    Ok(CheckedDirectPackageClosure {
        fold_members: fold_members.into_boxed_slice(),
        package_paths: package_paths.into_boxed_slice(),
    })
}

#[derive(Debug)]
struct ClosedRenderPlanStep {
    incoming_checksums: Vec<TargetResolvedChecksumBinding>,
    artifact_identity_dependencies: CheckedArtifactIdentityDependencies,
}

impl ClosedRenderPlanStep {
    fn from_step(
        step: TargetRenderPlanStep,
    ) -> (Self, TargetPreparedMemberPlan, TargetSnapshottedFile) {
        (
            Self {
                incoming_checksums: step.incoming_checksums,
                artifact_identity_dependencies: step
                    .file
                    .declaration
                    .artifact_identity_dependencies
                    .clone(),
            },
            step.prepared_member,
            step.file,
        )
    }

    fn erase_after_checksum_resolution(self) {
        for binding in self.incoming_checksums {
            binding.erase_after_resolution();
        }
        let _ = self.artifact_identity_dependencies;
    }

    fn artifact_identity_dependencies(&self) -> &CheckedArtifactIdentityDependencies {
        &self.artifact_identity_dependencies
    }
}

#[derive(Debug)]
struct DirectPackageRenderPlanStep<T> {
    core: ClosedRenderPlanStep,
    template: T,
}

#[derive(Debug)]
struct DirectUnpackagedRenderPlanStep<T> {
    core: ClosedRenderPlanStep,
    template: T,
    mode: Option<u32>,
}

#[derive(Debug)]
struct AlgorithmCodeSourceRenderPlanStep {
    core: ClosedRenderPlanStep,
    template: rumoca_phase_codegen::AlgorithmCodeTemplateSpec,
    output_path: String,
    mode: Option<u32>,
}

#[derive(Debug)]
struct PackagedAlgorithmCodeRenderPlanStep {
    core: ClosedRenderPlanStep,
    template: rumoca_phase_codegen::PackagedAlgorithmCodeTemplateSpec,
}

#[derive(Debug)]
enum SolveAlgorithmRenderPlanStep {
    CorrelatedAlgorithmCode {
        core: ClosedRenderPlanStep,
        template: rumoca_phase_codegen::CorrelatedAlgorithmCodeTemplateSpec,
    },
    ProductionCode {
        core: ClosedRenderPlanStep,
        template: rumoca_phase_codegen::SolveAlgorithmTemplateSpec,
    },
}

impl SolveAlgorithmRenderPlanStep {
    fn core(&self) -> &ClosedRenderPlanStep {
        match self {
            Self::CorrelatedAlgorithmCode { core, .. } | Self::ProductionCode { core, .. } => core,
        }
    }
}

enum CheckedPackagedAlgorithmCodeMemberPlan {
    File {
        step: Box<PackagedAlgorithmCodeRenderPlanStep>,
    },
    Asset(CheckedTargetPackageAsset),
}

enum CheckedSolveAlgorithmMemberPlan {
    File {
        step: Box<SolveAlgorithmRenderPlanStep>,
    },
    Asset(CheckedTargetPackageAsset),
}

enum CheckedDirectPackageMemberPlan<T> {
    File {
        step: Box<DirectPackageRenderPlanStep<T>>,
        package_path: Arc<CheckedTargetPackagePath>,
    },
    Asset(CheckedTargetPackageAsset),
}

enum CheckedAlgorithmCodeLayoutMemberPlan {
    AlgorithmCode {
        role: rumoca_phase_codegen::AlgorithmCodeArtifactRole,
        path: Arc<CheckedTargetPackagePath>,
    },
}

enum CheckedSolveAlgorithmLayoutMemberPlan {
    CorrelatedAlgorithmCode {
        role: rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole,
        path: Arc<CheckedTargetPackagePath>,
    },
    ProductionCode {
        role: rumoca_phase_codegen::ProductionCodeFileRole,
        path: Arc<CheckedTargetPackagePath>,
    },
    Schema {
        path: Arc<CheckedTargetPackagePath>,
    },
}

struct CheckedPackageProductLayoutRequest {
    product: TargetRequiredProduct,
    members: Box<[CheckedTargetPackageMemberPlan]>,
    arithmetic: Option<TargetAlgorithmCodeArithmetic>,
    production_profile: Option<rumoca_phase_codegen::SolveAlgorithmProductionProfile>,
    capability_contract: Option<CheckedTargetCapabilityContract>,
}

struct CheckedAlgorithmCodePackageLayoutBuilder {
    seen_schema: bool,
    layout: Vec<CheckedAlgorithmCodeLayoutMemberPlan>,
    fold_members: Vec<CheckedPackagedAlgorithmCodeMemberPlan>,
    package_paths: Vec<Arc<CheckedTargetPackagePath>>,
}

impl CheckedAlgorithmCodePackageLayoutBuilder {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            seen_schema: false,
            layout: Vec::with_capacity(capacity),
            fold_members: Vec::with_capacity(capacity),
            package_paths: Vec::with_capacity(capacity),
        }
    }

    fn admit_member(&mut self, member: CheckedTargetPackageMemberPlan) -> Result<()> {
        match member {
            CheckedTargetPackageMemberPlan::File { step, package_path } => {
                self.package_paths.push(Arc::clone(&package_path));
                if self.seen_schema {
                    bail!(
                        "packaged AlgorithmCodePackage [[package.members]] must place every schema member in one trailing suffix; rendered file '{}' follows a schema",
                        package_path.as_path().display()
                    );
                }
                let (core, prepared_member, file) = ClosedRenderPlanStep::from_step(step);
                let TargetPreparedMemberPlan::PackagedAlgorithmCode { role } = prepared_member
                else {
                    bail!(
                        "packaged AlgorithmCodePackage member '{}' does not carry an Algorithm Code artifact role",
                        package_path.as_path().display()
                    );
                };
                let template = rumoca_phase_codegen::packaged_algorithm_code_template_spec(
                    role,
                    phase_artifact_kind(file.declaration.artifact_kind),
                    phase_semantic_context(file.declaration.semantic_context),
                    file.template_body,
                )?;
                self.fold_members
                    .push(CheckedPackagedAlgorithmCodeMemberPlan::File {
                        step: Box::new(PackagedAlgorithmCodeRenderPlanStep { core, template }),
                    });
                self.layout
                    .push(CheckedAlgorithmCodeLayoutMemberPlan::AlgorithmCode {
                        role,
                        path: package_path,
                    });
            }
            CheckedTargetPackageMemberPlan::Asset(asset) => {
                self.seen_schema = true;
                self.package_paths.push(Arc::clone(&asset.package_path));
                self.fold_members
                    .push(CheckedPackagedAlgorithmCodeMemberPlan::Asset(asset));
            }
        }
        Ok(())
    }

    fn close(
        self,
        capability_contract: CheckedTargetCapabilityContract,
        arithmetic: TargetAlgorithmCodeArithmetic,
    ) -> Result<CheckedTargetPackageProductClosure> {
        if !self.seen_schema {
            bail!("packaged AlgorithmCodePackage layout must end in at least one schema member");
        }
        let spec = rumoca_phase_codegen::AlgorithmCodeArtifactLayoutSpec::construct(
            self.layout
                .iter()
                .map(|member| match member {
                    CheckedAlgorithmCodeLayoutMemberPlan::AlgorithmCode { role, path } => {
                        (*role, Box::<str>::from(path.as_str()))
                    }
                })
                .collect(),
        )
        .context("construct complete packaged Algorithm Code member layout")?;
        Ok(CheckedTargetPackageProductClosure {
            product_layout: CheckedTargetPackageProductLayout::AlgorithmCode {
                capability_contract,
                arithmetic,
                artifact_layout: CheckedAlgorithmCodeLayout { spec },
                fold_members: self.fold_members.into_boxed_slice(),
            },
            package_paths: self.package_paths.into_boxed_slice(),
        })
    }
}

struct CheckedSolveAlgorithmPackageLayoutBuilder {
    seen_schema: bool,
    layout: Vec<CheckedSolveAlgorithmLayoutMemberPlan>,
    fold_members: Vec<CheckedSolveAlgorithmMemberPlan>,
    package_paths: Vec<Arc<CheckedTargetPackagePath>>,
}

impl CheckedSolveAlgorithmPackageLayoutBuilder {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            seen_schema: false,
            layout: Vec::with_capacity(capacity),
            fold_members: Vec::with_capacity(capacity),
            package_paths: Vec::with_capacity(capacity),
        }
    }

    fn admit_member(&mut self, member: CheckedTargetPackageMemberPlan) -> Result<()> {
        match member {
            CheckedTargetPackageMemberPlan::File { step, package_path } => {
                self.package_paths.push(Arc::clone(&package_path));
                if self.seen_schema {
                    bail!(
                        "SolveAlgorithmProduct [[package.members]] must place every schema member in one trailing suffix; rendered file '{}' follows a schema",
                        package_path.as_path().display()
                    );
                }
                let (core, prepared_member, file) = ClosedRenderPlanStep::from_step(step);
                self.admit_rendered_member(core, prepared_member, file, package_path)?;
            }
            CheckedTargetPackageMemberPlan::Asset(asset) => {
                self.seen_schema = true;
                self.package_paths.push(Arc::clone(&asset.package_path));
                self.layout
                    .push(CheckedSolveAlgorithmLayoutMemberPlan::Schema {
                        path: Arc::clone(&asset.package_path),
                    });
                self.fold_members
                    .push(CheckedSolveAlgorithmMemberPlan::Asset(asset));
            }
        }
        Ok(())
    }

    fn admit_rendered_member(
        &mut self,
        core: ClosedRenderPlanStep,
        prepared_member: TargetPreparedMemberPlan,
        file: TargetSnapshottedFile,
        package_path: Arc<CheckedTargetPackagePath>,
    ) -> Result<()> {
        match prepared_member {
            TargetPreparedMemberPlan::CorrelatedAlgorithmCode { role } => {
                self.layout.push(
                    CheckedSolveAlgorithmLayoutMemberPlan::CorrelatedAlgorithmCode {
                        role,
                        path: Arc::clone(&package_path),
                    },
                );
                let template = rumoca_phase_codegen::correlated_algorithm_code_template_spec(
                    role,
                    phase_artifact_kind(file.declaration.artifact_kind),
                    phase_semantic_context(file.declaration.semantic_context),
                    file.template_body,
                )?;
                self.fold_members
                    .push(CheckedSolveAlgorithmMemberPlan::File {
                        step: Box::new(SolveAlgorithmRenderPlanStep::CorrelatedAlgorithmCode {
                            core,
                            template,
                        }),
                    });
            }
            TargetPreparedMemberPlan::ProductionCode { role } => {
                self.layout
                    .push(CheckedSolveAlgorithmLayoutMemberPlan::ProductionCode {
                        role,
                        path: Arc::clone(&package_path),
                    });
                let template = rumoca_phase_codegen::solve_algorithm_template_spec(
                    role,
                    phase_artifact_kind(file.declaration.artifact_kind),
                    phase_semantic_context(file.declaration.semantic_context),
                    file.template_body,
                )?;
                self.fold_members
                    .push(CheckedSolveAlgorithmMemberPlan::File {
                        step: Box::new(SolveAlgorithmRenderPlanStep::ProductionCode {
                            core,
                            template,
                        }),
                    });
            }
            TargetPreparedMemberPlan::Direct
            | TargetPreparedMemberPlan::AlgorithmCodeSource { .. }
            | TargetPreparedMemberPlan::PackagedAlgorithmCode { .. } => {
                bail!(
                    "SolveAlgorithmProduct package member '{}' does not carry a correlated Algorithm Code or Production Code role",
                    package_path.as_path().display()
                );
            }
        }
        Ok(())
    }

    fn close(
        self,
        capability_contract: CheckedTargetCapabilityContract,
        arithmetic: TargetAlgorithmCodeArithmetic,
        production_profile: rumoca_phase_codegen::SolveAlgorithmProductionProfile,
    ) -> Result<CheckedTargetPackageProductClosure> {
        if !self.seen_schema {
            bail!("SolveAlgorithmProduct package layout must end in at least one schema member");
        }
        let layout = CheckedSolveAlgorithmLayout::construct_spec(&self.layout)?;
        Ok(CheckedTargetPackageProductClosure {
            product_layout: CheckedTargetPackageProductLayout::SolveAlgorithm {
                capability_contract,
                arithmetic,
                production_profile,
                layout,
                fold_members: self.fold_members.into_boxed_slice(),
            },
            package_paths: self.package_paths.into_boxed_slice(),
        })
    }
}

fn construct_checked_package_product_layout(
    request: CheckedPackageProductLayoutRequest,
) -> Result<CheckedTargetPackageProductClosure> {
    let CheckedPackageProductLayoutRequest {
        product,
        members,
        arithmetic,
        production_profile,
        capability_contract,
    } = request;
    if product == TargetRequiredProduct::AlgorithmCodePackage {
        return construct_checked_algorithm_code_package_layout(
            members,
            arithmetic,
            production_profile,
            capability_contract,
        );
    }
    if product == TargetRequiredProduct::SolveAlgorithmProduct {
        return construct_checked_solve_algorithm_package_layout(
            members,
            arithmetic,
            production_profile,
            capability_contract,
        );
    }
    if arithmetic.is_some() || production_profile.is_some() {
        bail!("non-Algorithm-Code package product received Algorithm Code profile facts");
    }
    let direct_product = CheckedDirectTargetProduct::construct(product, capability_contract)?;
    direct_product.into_package_closure(members)
}

fn construct_checked_algorithm_code_package_layout(
    members: Box<[CheckedTargetPackageMemberPlan]>,
    arithmetic: Option<TargetAlgorithmCodeArithmetic>,
    production_profile: Option<rumoca_phase_codegen::SolveAlgorithmProductionProfile>,
    capability_contract: Option<CheckedTargetCapabilityContract>,
) -> Result<CheckedTargetPackageProductClosure> {
    let arithmetic = arithmetic.context(
        "packaged AlgorithmCodePackage lost its construction-issued arithmetic contract",
    )?;
    if production_profile.is_some() {
        bail!("packaged AlgorithmCodePackage received an inapplicable Production profile");
    }
    let capability_contract = capability_contract.context(
        "packaged AlgorithmCodePackage lost its construction-issued capability contract",
    )?;
    let mut builder = CheckedAlgorithmCodePackageLayoutBuilder::with_capacity(members.len());
    for member in members.into_vec() {
        builder.admit_member(member)?;
    }
    builder.close(capability_contract, arithmetic)
}

fn construct_checked_solve_algorithm_package_layout(
    members: Box<[CheckedTargetPackageMemberPlan]>,
    arithmetic: Option<TargetAlgorithmCodeArithmetic>,
    production_profile: Option<rumoca_phase_codegen::SolveAlgorithmProductionProfile>,
    capability_contract: Option<CheckedTargetCapabilityContract>,
) -> Result<CheckedTargetPackageProductClosure> {
    let arithmetic = arithmetic.context(
        "SolveAlgorithmProduct lost its construction-issued Algorithm Code arithmetic contract",
    )?;
    let production_profile = production_profile
        .context("SolveAlgorithmProduct lost its construction-issued Production profile")?;
    let capability_contract = capability_contract
        .context("SolveAlgorithmProduct lost its construction-issued capability contract")?;
    let mut builder = CheckedSolveAlgorithmPackageLayoutBuilder::with_capacity(members.len());
    for member in members.into_vec() {
        builder.admit_member(member)?;
    }
    builder.close(capability_contract, arithmetic, production_profile)
}

struct CloseCheckedRenderAuthorityRequest<'a> {
    authority: TargetSnapshottedRenderAuthority,
    assets: &'a [CheckedTargetAssetBundle],
    package: Option<&'a TargetPackage>,
    product: TargetRequiredProduct,
    arithmetic: Option<TargetAlgorithmCodeArithmetic>,
    production_profile: Option<rumoca_phase_codegen::SolveAlgorithmProductionProfile>,
    capability_contract: Option<CheckedTargetCapabilityContract>,
}

fn close_checked_render_authority(
    request: CloseCheckedRenderAuthorityRequest<'_>,
) -> Result<CheckedTargetRenderAuthority> {
    let CloseCheckedRenderAuthorityRequest {
        authority,
        assets,
        package,
        product,
        arithmetic,
        production_profile,
        capability_contract,
    } = request;
    match authority {
        TargetSnapshottedRenderAuthority::Unpackaged(steps) => {
            if !assets.is_empty() {
                bail!(
                    "[[assets]] requires [package] with an explicit [[package.members]] entry for every expanded asset member"
                );
            }
            let product = match product {
                TargetRequiredProduct::AlgorithmCodePackage => {
                    close_unpacked_algorithm_code_product(
                        steps,
                        arithmetic,
                        production_profile,
                        capability_contract,
                    )?
                }
                _ => {
                    if arithmetic.is_some() || production_profile.is_some() {
                        bail!(
                            "unpackaged non-Algorithm-Code product received Algorithm Code profile facts"
                        );
                    }
                    let direct_product =
                        CheckedDirectTargetProduct::construct(product, capability_contract)?;
                    CheckedUnpackagedTargetProduct::Other {
                        product: direct_product,
                        steps,
                    }
                }
            };
            Ok(CheckedTargetRenderAuthority::Unpackaged { product })
        }
        TargetSnapshottedRenderAuthority::Packaged(members) => {
            let package = package.context(
                "target construction issued package members without their package policy",
            )?;
            let members = close_checked_package_members(members, assets)?;
            let closed_product =
                construct_checked_package_product_layout(CheckedPackageProductLayoutRequest {
                    product,
                    members,
                    arithmetic,
                    production_profile,
                    capability_contract,
                })?;
            let policy =
                CheckedTargetPackagePolicy::construct(package, &closed_product.package_paths)?;
            Ok(CheckedTargetRenderAuthority::Packaged {
                policy,
                product_layout: closed_product.product_layout,
            })
        }
    }
}

fn close_unpacked_algorithm_code_product(
    steps: Box<[TargetRenderPlanStep]>,
    arithmetic: Option<TargetAlgorithmCodeArithmetic>,
    production_profile: Option<rumoca_phase_codegen::SolveAlgorithmProductionProfile>,
    capability_contract: Option<CheckedTargetCapabilityContract>,
) -> Result<CheckedUnpackagedTargetProduct> {
    let arithmetic = arithmetic.context(
        "unpackaged AlgorithmCodePackage lost its construction-issued arithmetic contract",
    )?;
    if production_profile.is_some() {
        bail!("unpackaged AlgorithmCodePackage received an inapplicable Production profile");
    }
    let capability_contract = capability_contract.context(
        "unpackaged AlgorithmCodePackage lost its construction-issued capability contract",
    )?;
    Ok(CheckedUnpackagedTargetProduct::AlgorithmCode {
        capability_contract,
        arithmetic,
        steps: close_algorithm_code_source_steps(steps)?,
    })
}

fn close_algorithm_code_source_steps(
    steps: Box<[TargetRenderPlanStep]>,
) -> Result<Box<[Box<AlgorithmCodeSourceRenderPlanStep>]>> {
    steps
        .into_vec()
        .into_iter()
        .map(close_algorithm_code_source_step)
        .collect::<Result<Vec<_>>>()
        .map(Vec::into_boxed_slice)
}

fn close_algorithm_code_source_step(
    step: TargetRenderPlanStep,
) -> Result<Box<AlgorithmCodeSourceRenderPlanStep>> {
    let (core, prepared_member, file) = ClosedRenderPlanStep::from_step(step);
    let TargetPreparedMemberPlan::AlgorithmCodeSource {
        output_path_template,
    } = prepared_member
    else {
        bail!("source-only Algorithm Code product contains a non-source render member");
    };
    let mode = file.declaration.mode_bits;
    let output_path = output_path_template.as_str().to_owned();
    let template = rumoca_phase_codegen::algorithm_code_template_spec(
        phase_artifact_kind(file.declaration.artifact_kind),
        phase_semantic_context(file.declaration.semantic_context),
        output_path_template,
        file.template_body,
    )?;
    Ok(Box::new(AlgorithmCodeSourceRenderPlanStep {
        core,
        template,
        output_path,
        mode,
    }))
}

fn close_checked_package_members(
    members: Box<[TargetSnapshottedPackageMemberPlan]>,
    assets: &[CheckedTargetAssetBundle],
) -> Result<Box<[CheckedTargetPackageMemberPlan]>> {
    let bundles_by_source = index_checked_asset_bundles(assets)?;
    let mut admitted_assets = BTreeSet::new();
    let mut admitted_package_paths = BTreeMap::new();
    let mut checked = Vec::with_capacity(members.len());
    for (position, member) in members.into_vec().into_iter().enumerate() {
        checked.push(close_checked_package_member(
            position,
            member,
            &bundles_by_source,
            &mut admitted_assets,
            &mut admitted_package_paths,
        )?);
    }
    validate_all_asset_members_admitted(assets, &admitted_assets)?;
    Ok(checked.into_boxed_slice())
}

fn index_checked_asset_bundles(
    assets: &[CheckedTargetAssetBundle],
) -> Result<BTreeMap<&str, &CheckedTargetAssetBundle>> {
    let mut bundles_by_source = BTreeMap::new();
    for bundle in assets {
        if bundles_by_source
            .insert(bundle.source.as_ref(), bundle)
            .is_some()
        {
            bail!(
                "[[assets]] source '{}' is declared more than once; [[package.members]] asset references require one exact owner",
                bundle.source
            );
        }
    }
    Ok(bundles_by_source)
}

fn close_checked_package_member<'a>(
    position: usize,
    member: TargetSnapshottedPackageMemberPlan,
    bundles_by_source: &BTreeMap<&'a str, &'a CheckedTargetAssetBundle>,
    admitted_assets: &mut BTreeSet<(&'a str, &'a str)>,
    admitted_package_paths: &mut BTreeMap<String, usize>,
) -> Result<CheckedTargetPackageMemberPlan> {
    match member {
        TargetSnapshottedPackageMemberPlan::File(step) => {
            let path = step.file.declaration.path.as_ref();
            let components = validate_portable_member_path(path).with_context(|| {
                format!(
                    "[[package.members]] position {} references file with invalid static path '{}'",
                    position + 1,
                    path
                )
            })?;
            let normalized = components.join("/");
            admit_package_path(admitted_package_paths, Path::new(&normalized), position)?;
            Ok(CheckedTargetPackageMemberPlan::File {
                step,
                package_path: Arc::new(CheckedTargetPackagePath(normalized.into_boxed_str())),
            })
        }
        TargetSnapshottedPackageMemberPlan::Asset {
            source,
            relative_path,
        } => close_checked_asset_package_member(
            position,
            source,
            relative_path,
            bundles_by_source,
            admitted_assets,
            admitted_package_paths,
        ),
    }
}

fn close_checked_asset_package_member<'a>(
    position: usize,
    source: String,
    relative_path: String,
    bundles_by_source: &BTreeMap<&'a str, &'a CheckedTargetAssetBundle>,
    admitted_assets: &mut BTreeSet<(&'a str, &'a str)>,
    admitted_package_paths: &mut BTreeMap<String, usize>,
) -> Result<CheckedTargetPackageMemberPlan> {
    validate_portable_member_path(&relative_path).with_context(|| {
        format!(
            "[[package.members]] position {} has invalid asset path '{}'",
            position + 1,
            relative_path
        )
    })?;
    let bundle = bundles_by_source
        .get(source.as_str())
        .copied()
        .ok_or_else(|| {
            anyhow::anyhow!(
                "[[package.members]] position {} references unknown [[assets]].source '{}'",
                position + 1,
                source
            )
        })?;
    let member = bundle
        .members
        .iter()
        .find(|member| member.relative_path.as_ref() == relative_path)
        .ok_or_else(|| {
            anyhow::anyhow!(
                "[[package.members]] position {} references unknown member path '{}' in [[assets]].source '{}'",
                position + 1,
                relative_path,
                source
            )
        })?;
    let identity = (bundle.source.as_ref(), member.relative_path.as_ref());
    if !admitted_assets.insert(identity) {
        bail!(
            "[[package.members]] position {} repeats asset source '{}' path '{}'; every expanded asset member must occur exactly once",
            position + 1,
            source,
            relative_path
        );
    }
    let package_path = checked_asset_package_path(&bundle.destination, &relative_path)?;
    admit_package_path(admitted_package_paths, package_path.as_path(), position)?;
    Ok(CheckedTargetPackageMemberPlan::Asset(
        CheckedTargetPackageAsset {
            package_path: Arc::new(package_path),
            source: Arc::clone(&bundle.source),
            relative_path: Arc::clone(&member.relative_path),
            position,
            bytes: Arc::clone(&member.bytes),
        },
    ))
}

fn validate_all_asset_members_admitted(
    assets: &[CheckedTargetAssetBundle],
    admitted_assets: &BTreeSet<(&str, &str)>,
) -> Result<()> {
    let mut missing = Vec::new();
    for bundle in assets {
        for member in &bundle.members {
            if !admitted_assets.contains(&(bundle.source.as_ref(), member.relative_path.as_ref())) {
                missing.push(format!(
                    "source '{}' path '{}'",
                    bundle.source, member.relative_path
                ));
            }
        }
    }
    if !missing.is_empty() {
        bail!(
            "[[package.members]] omits expanded asset members {}; every expanded asset member must occur exactly once",
            missing.join(", ")
        );
    }
    Ok(())
}

fn checked_asset_package_path(
    destination: &str,
    relative_path: &str,
) -> Result<CheckedTargetPackagePath> {
    let mut components = validate_portable_directory_path(destination)?;
    components.extend(validate_portable_member_path(relative_path)?);
    if components.is_empty() {
        bail!("package asset path must identify a member");
    }
    Ok(CheckedTargetPackagePath(
        components.join("/").into_boxed_str(),
    ))
}

fn validate_portable_directory_path(path: &str) -> Result<Vec<&str>> {
    let path = path.strip_suffix('/').unwrap_or(path);
    if path.is_empty() {
        return Ok(Vec::new());
    }
    validate_portable_member_path(path)
}

fn validate_portable_member_path(path: &str) -> Result<Vec<&str>> {
    if path.is_empty() || path.starts_with('/') || path.contains('\\') || !path.is_ascii() {
        bail!(
            "package member path '{}' must be a nonempty relative ASCII path using '/' separators",
            path
        );
    }
    let mut components = Vec::new();
    for component in path.split('/') {
        if component.is_empty()
            || component == "."
            || component == ".."
            || component.contains(':')
            || component.bytes().any(|byte| byte.is_ascii_control())
        {
            bail!(
                "package member path '{}' contains an empty, traversal, drive-like, or control-bearing component",
                path
            );
        }
        components.push(component);
    }
    Ok(components)
}

fn admit_package_path(
    admitted: &mut BTreeMap<String, usize>,
    path: &Path,
    position: usize,
) -> Result<()> {
    let path = path
        .to_str()
        .with_context(|| format!("package member path '{}' must be UTF-8", path.display()))?;
    let collision_key = path.to_ascii_lowercase();
    for (existing, previous) in admitted.iter() {
        let existing_is_ancestor = collision_key
            .strip_prefix(existing)
            .is_some_and(|suffix| suffix.starts_with('/'));
        let new_is_ancestor = existing
            .strip_prefix(&collision_key)
            .is_some_and(|suffix| suffix.starts_with('/'));
        if existing_is_ancestor || new_is_ancestor {
            bail!(
                "[[package.members]] positions {} and {} have an impossible portable file-tree relation between '{}' and '{}' (one path is an ancestor of the other)",
                previous + 1,
                position + 1,
                existing,
                path
            );
        }
    }
    if let Some(previous) = admitted.insert(collision_key, position) {
        bail!(
            "[[package.members]] positions {} and {} have colliding normalized package path '{}'",
            previous + 1,
            position + 1,
            path
        );
    }
    Ok(())
}

/// Affine packaged authority before its checked product refinement is opened.
/// Consuming [`Self::into_product_plan`] transfers the product layout, package
/// policy, and sole member fold into siblings, so renderer preparation never
/// needs a self-reference into the fold it later consumes.
struct CheckedTargetPackagePlan {
    policy: CheckedTargetPackagePolicy,
    product_layout: CheckedTargetPackageProductLayout,
}

impl CheckedTargetPackagePlan {
    #[must_use]
    fn into_product_plan(self) -> CheckedTargetPackageProductPlan {
        match self.product_layout {
            CheckedTargetPackageProductLayout::AlgorithmCode {
                capability_contract,
                arithmetic,
                artifact_layout,
                fold_members,
            } => CheckedTargetPackageProductPlan::AlgorithmCode(CheckedAlgorithmCodePackagePlan {
                policy: self.policy,
                capability_contract,
                arithmetic,
                artifact_layout,
                fold: CheckedPackagedAlgorithmCodeFold {
                    members: fold_members,
                },
            }),
            CheckedTargetPackageProductLayout::SolveAlgorithm {
                capability_contract,
                arithmetic,
                production_profile,
                layout,
                fold_members,
            } => {
                CheckedTargetPackageProductPlan::SolveAlgorithm(CheckedSolveAlgorithmPackagePlan {
                    policy: self.policy,
                    capability_contract,
                    arithmetic,
                    production_profile,
                    layout,
                    fold: CheckedSolveAlgorithmFold {
                        members: fold_members,
                    },
                })
            }
            CheckedTargetPackageProductLayout::Ast { fold_members } => {
                CheckedTargetPackageProductPlan::Other(CheckedOtherTargetPackagePlan::Ast {
                    policy: self.policy,
                    fold: CheckedDirectPackageFold {
                        members: fold_members,
                    },
                })
            }
            CheckedTargetPackageProductLayout::Flat { fold_members } => {
                CheckedTargetPackageProductPlan::Other(CheckedOtherTargetPackagePlan::Flat {
                    policy: self.policy,
                    fold: CheckedDirectPackageFold {
                        members: fold_members,
                    },
                })
            }
            CheckedTargetPackageProductLayout::Dae {
                capability_contract,
                fold_members,
            } => CheckedTargetPackageProductPlan::Other(CheckedOtherTargetPackagePlan::Dae {
                policy: self.policy,
                capability_contract,
                fold: CheckedDirectPackageFold {
                    members: fold_members,
                },
            }),
            CheckedTargetPackageProductLayout::SolveModel {
                capability_contract,
                fold_members,
            } => {
                CheckedTargetPackageProductPlan::Other(CheckedOtherTargetPackagePlan::SolveModel {
                    policy: self.policy,
                    capability_contract,
                    fold: CheckedDirectPackageFold {
                        members: fold_members,
                    },
                })
            }
            CheckedTargetPackageProductLayout::FmiComponent {
                capability_contract,
                fold_members,
            } => CheckedTargetPackageProductPlan::Other(
                CheckedOtherTargetPackagePlan::FmiComponent {
                    policy: self.policy,
                    capability_contract,
                    fold: CheckedDirectPackageFold {
                        members: fold_members,
                    },
                },
            ),
        }
    }
}

/// Exhaustive owned package-product operation. Matching this sum is the sole
/// applicability decision; every alternative retains exactly one fold.
enum CheckedTargetPackageProductPlan {
    AlgorithmCode(CheckedAlgorithmCodePackagePlan),
    SolveAlgorithm(CheckedSolveAlgorithmPackagePlan),
    Other(CheckedOtherTargetPackagePlan),
}

/// Owned packaged Algorithm Code operation.
struct CheckedAlgorithmCodePackagePlan {
    policy: CheckedTargetPackagePolicy,
    capability_contract: CheckedTargetCapabilityContract,
    arithmetic: TargetAlgorithmCodeArithmetic,
    artifact_layout: CheckedAlgorithmCodeLayout,
    fold: CheckedPackagedAlgorithmCodeFold,
}

impl CheckedAlgorithmCodePackagePlan {
    /// Transfer all authorities once. The semantic renderer may borrow the
    /// artifact layout while the independent fold is consumed.
    #[must_use]
    fn into_parts(
        self,
    ) -> (
        CheckedTargetPackagePolicy,
        CheckedTargetCapabilityContract,
        TargetAlgorithmCodeArithmetic,
        CheckedAlgorithmCodeLayout,
        CheckedPackagedAlgorithmCodeFold,
    ) {
        (
            self.policy,
            self.capability_contract,
            self.arithmetic,
            self.artifact_layout,
            self.fold,
        )
    }
}

/// Owned correlated Solve Algorithm operation.
struct CheckedSolveAlgorithmPackagePlan {
    policy: CheckedTargetPackagePolicy,
    capability_contract: CheckedTargetCapabilityContract,
    arithmetic: TargetAlgorithmCodeArithmetic,
    production_profile: rumoca_phase_codegen::SolveAlgorithmProductionProfile,
    layout: CheckedSolveAlgorithmLayout,
    fold: CheckedSolveAlgorithmFold,
}

impl CheckedSolveAlgorithmPackagePlan {
    #[must_use]
    fn into_parts(
        self,
    ) -> (
        CheckedTargetPackagePolicy,
        CheckedTargetCapabilityContract,
        TargetAlgorithmCodeArithmetic,
        rumoca_phase_codegen::SolveAlgorithmProductionProfile,
        CheckedSolveAlgorithmLayout,
        CheckedSolveAlgorithmFold,
    ) {
        (
            self.policy,
            self.capability_contract,
            self.arithmetic,
            self.production_profile,
            self.layout,
            self.fold,
        )
    }
}

/// Owned packaged operation for products without a narrower layout contract.
enum CheckedOtherTargetPackagePlan {
    Ast {
        policy: CheckedTargetPackagePolicy,
        fold: CheckedDirectPackageFold<DirectTargetTemplate>,
    },
    Flat {
        policy: CheckedTargetPackagePolicy,
        fold: CheckedDirectPackageFold<DirectTargetTemplate>,
    },
    Dae {
        policy: CheckedTargetPackagePolicy,
        capability_contract: CheckedTargetCapabilityContract,
        fold: CheckedDirectPackageFold<DirectTargetTemplate>,
    },
    SolveModel {
        policy: CheckedTargetPackagePolicy,
        capability_contract: CheckedTargetCapabilityContract,
        fold: CheckedDirectPackageFold<DirectTargetTemplate>,
    },
    FmiComponent {
        policy: CheckedTargetPackagePolicy,
        capability_contract: CheckedTargetCapabilityContract,
        fold: CheckedDirectPackageFold<DirectTargetTemplate>,
    },
}

struct CheckedPackagedAlgorithmCodeFold {
    members: Box<[CheckedPackagedAlgorithmCodeMemberPlan]>,
}

struct CheckedSolveAlgorithmFold {
    members: Box<[CheckedSolveAlgorithmMemberPlan]>,
}

struct CheckedDirectPackageFold<T> {
    members: Box<[CheckedDirectPackageMemberPlan<T>]>,
}

fn resolved_checksum_map(
    bindings: &[TargetResolvedChecksumBinding],
    digests: &[String],
) -> BTreeMap<String, String> {
    bindings
        .iter()
        .map(|binding| match binding {
            TargetResolvedChecksumBinding::Sha1 { producer, as_key } => {
                (as_key.clone(), producer.resolve(digests).clone())
            }
        })
        .collect()
}

fn sha1_hex(bytes: &[u8]) -> String {
    use sha1::Digest as _;
    format!("{:x}", sha1::Sha1::digest(bytes))
}

/// Complete packaged Algorithm Code/schema layout in the exact mixed package
/// order. This role/path-only witness has no template, body, render step, or
/// completion authority; those remain solely inside the consumed fold.
struct CheckedAlgorithmCodeLayout {
    spec: rumoca_phase_codegen::AlgorithmCodeArtifactLayoutSpec,
}

impl CheckedAlgorithmCodeLayout {
    #[cfg(test)]
    #[must_use]
    fn members(&self) -> impl ExactSizeIterator<Item = CheckedAlgorithmCodeLayoutMember<'_>> {
        self.spec.members().iter().map(|member| {
            let role = member.role();
            let path = member.member_path();
            CheckedAlgorithmCodeLayoutMember::AlgorithmCode { role, path }
        })
    }

    fn into_layout(self) -> rumoca_phase_codegen::AlgorithmCodeArtifactLayout {
        self.spec.into_layout()
    }
}

/// One member of a checked packaged Algorithm Code product layout. Generic,
/// correlated, and Production Code roles cannot inhabit this sum.
#[derive(Clone, Copy)]
#[cfg(test)]
enum CheckedAlgorithmCodeLayoutMember<'a> {
    AlgorithmCode {
        role: rumoca_phase_codegen::AlgorithmCodeArtifactRole,
        path: &'a rumoca_phase_codegen::AlgorithmCodePortableMemberPath,
    },
}

/// Complete correlated Algorithm Code/Production Code/schema layout in the
/// exact package-member order. Construction proves schemas form its trailing
/// suffix.
struct CheckedSolveAlgorithmLayout {
    spec: rumoca_phase_codegen::ProductionArtifactLayoutSpec,
}

impl CheckedSolveAlgorithmLayout {
    fn construct_spec(members: &[CheckedSolveAlgorithmLayoutMemberPlan]) -> Result<Self> {
        let drafts = members
            .iter()
            .map(|member| match member {
                CheckedSolveAlgorithmLayoutMemberPlan::CorrelatedAlgorithmCode { role, path } => {
                    let role = match role {
                        rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::PackageManifest => rumoca_phase_codegen::ProductionArtifactRole::PackageManifest,
                        rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeManifest => rumoca_phase_codegen::ProductionArtifactRole::AlgorithmCodeManifest,
                        rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeSource => rumoca_phase_codegen::ProductionArtifactRole::AlgorithmCodeSource,
                    };
                    (role, Box::<str>::from(path.as_str()))
                }
                CheckedSolveAlgorithmLayoutMemberPlan::ProductionCode { role, path } => {
                    let role = match role {
                        rumoca_phase_codegen::ProductionCodeFileRole::Manifest => rumoca_phase_codegen::ProductionArtifactRole::ProductionManifest,
                        rumoca_phase_codegen::ProductionCodeFileRole::Header => rumoca_phase_codegen::ProductionArtifactRole::ProductionHeader,
                        rumoca_phase_codegen::ProductionCodeFileRole::Source => rumoca_phase_codegen::ProductionArtifactRole::ProductionSource,
                    };
                    (role, Box::<str>::from(path.as_str()))
                }
                CheckedSolveAlgorithmLayoutMemberPlan::Schema { path } => (
                    rumoca_phase_codegen::ProductionArtifactRole::Schema,
                    Box::<str>::from(path.as_str()),
                ),
            })
            .collect();
        let spec = rumoca_phase_codegen::ProductionArtifactLayoutSpec::construct(drafts)
            .context("construct complete Solve Algorithm Production member layout")?;
        Ok(Self { spec })
    }

    #[cfg(test)]
    #[must_use]
    fn members(&self) -> impl ExactSizeIterator<Item = CheckedSolveAlgorithmLayoutMember<'_>> {
        self.spec.members().iter().map(|member| match member.role() {
            rumoca_phase_codegen::ProductionArtifactRole::PackageManifest => {
                CheckedSolveAlgorithmLayoutMember::CorrelatedAlgorithmCode {
                    role: rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::PackageManifest,
                    path: member.member_path(),
                }
            }
            rumoca_phase_codegen::ProductionArtifactRole::AlgorithmCodeManifest => {
                CheckedSolveAlgorithmLayoutMember::CorrelatedAlgorithmCode {
                    role: rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeManifest,
                    path: member.member_path(),
                }
            }
            rumoca_phase_codegen::ProductionArtifactRole::AlgorithmCodeSource => {
                CheckedSolveAlgorithmLayoutMember::CorrelatedAlgorithmCode {
                    role: rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeSource,
                    path: member.member_path(),
                }
            }
            rumoca_phase_codegen::ProductionArtifactRole::ProductionManifest => {
                CheckedSolveAlgorithmLayoutMember::ProductionCode {
                    role: rumoca_phase_codegen::ProductionCodeFileRole::Manifest,
                    path: member.member_path(),
                }
            }
            rumoca_phase_codegen::ProductionArtifactRole::ProductionHeader => {
                CheckedSolveAlgorithmLayoutMember::ProductionCode {
                    role: rumoca_phase_codegen::ProductionCodeFileRole::Header,
                    path: member.member_path(),
                }
            }
            rumoca_phase_codegen::ProductionArtifactRole::ProductionSource => {
                CheckedSolveAlgorithmLayoutMember::ProductionCode {
                    role: rumoca_phase_codegen::ProductionCodeFileRole::Source,
                    path: member.member_path(),
                }
            }
            rumoca_phase_codegen::ProductionArtifactRole::Schema => {
                CheckedSolveAlgorithmLayoutMember::Schema {
                    path: member.member_path(),
                }
            }
        })
    }

    fn into_layout(self) -> rumoca_phase_codegen::ProductionArtifactLayout {
        self.spec.into_layout()
    }
}

/// One narrowed correlated-product layout member. Other target preparation
/// routes cannot inhabit this sum.
#[derive(Clone, Copy)]
#[cfg(test)]
enum CheckedSolveAlgorithmLayoutMember<'a> {
    CorrelatedAlgorithmCode {
        role: rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole,
        path: &'a rumoca_phase_codegen::ProductionPortableMemberPath,
    },
    ProductionCode {
        role: rumoca_phase_codegen::ProductionCodeFileRole,
        path: &'a rumoca_phase_codegen::ProductionPortableMemberPath,
    },
    Schema {
        path: &'a rumoca_phase_codegen::ProductionPortableMemberPath,
    },
}

#[cfg(test)]
impl<'a> CheckedSolveAlgorithmLayoutMember<'a> {
    #[must_use]
    const fn production_artifact_role(self) -> rumoca_phase_codegen::ProductionArtifactRole {
        match self {
            Self::CorrelatedAlgorithmCode { role, .. } => match role {
                rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::PackageManifest => {
                    rumoca_phase_codegen::ProductionArtifactRole::PackageManifest
                }
                rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeManifest => {
                    rumoca_phase_codegen::ProductionArtifactRole::AlgorithmCodeManifest
                }
                rumoca_phase_codegen::CorrelatedAlgorithmCodeArtifactRole::AlgorithmCodeSource => {
                    rumoca_phase_codegen::ProductionArtifactRole::AlgorithmCodeSource
                }
            },
            Self::ProductionCode { role, .. } => match role {
                rumoca_phase_codegen::ProductionCodeFileRole::Manifest => {
                    rumoca_phase_codegen::ProductionArtifactRole::ProductionManifest
                }
                rumoca_phase_codegen::ProductionCodeFileRole::Header => {
                    rumoca_phase_codegen::ProductionArtifactRole::ProductionHeader
                }
                rumoca_phase_codegen::ProductionCodeFileRole::Source => {
                    rumoca_phase_codegen::ProductionArtifactRole::ProductionSource
                }
            },
            Self::Schema { .. } => rumoca_phase_codegen::ProductionArtifactRole::Schema,
        }
    }
}

/// A package-relative path normalized and collision-checked exactly once.
#[derive(Debug)]
pub(crate) struct CheckedTargetPackagePath(Box<str>);

impl CheckedTargetPackagePath {
    #[must_use]
    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }

    #[must_use]
    pub(crate) fn as_path(&self) -> &Path {
        Path::new(self.0.as_ref())
    }
}

/// Opaque checked package publication policy. Raw TOML schema values never
/// escape manifest construction.
#[derive(Debug)]
struct CheckedTargetPackagePolicy {
    root: CheckedTargetOutputPathTemplate,
    required_files: Box<[CheckedTargetPackagePath]>,
    archive: Option<CheckedTargetArchivePolicy>,
}

impl CheckedTargetPackagePolicy {
    fn construct(
        package: &TargetPackage,
        package_paths: &[Arc<CheckedTargetPackagePath>],
    ) -> Result<Self> {
        let root = CheckedTargetOutputPathTemplate::construct(&package.root)
            .context("construct checked [package] root path template")?;
        let archive = package
            .archive
            .as_ref()
            .map(|archive| {
                let TargetArchiveFormat::Zip = archive.format;
                let TargetArchiveRoot::Flat = archive.root;
                CheckedTargetOutputPathTemplate::construct(&archive.path)
                    .context("construct checked [package.archive] path template")
                    .map(|path| CheckedTargetArchivePolicy { path })
            })
            .transpose()?;

        let admitted_paths = package_paths
            .iter()
            .map(|path| path.as_path())
            .collect::<BTreeSet<_>>();
        let mut required_files = Vec::with_capacity(package.required_files.len());
        let mut unique_required = BTreeSet::new();
        for required in &package.required_files {
            let components = validate_portable_member_path(required).with_context(|| {
                format!("[package] required file '{}' is not portable", required)
            })?;
            let path = components.iter().collect::<PathBuf>();
            if !unique_required.insert(path.clone()) {
                bail!(
                    "[package] required file '{}' is declared more than once",
                    required
                );
            }
            if !admitted_paths.contains(path.as_path()) {
                bail!(
                    "[package] required file '{}' is not an exact checked [[package.members]] path",
                    required
                );
            }
            required_files.push(CheckedTargetPackagePath(required.clone().into_boxed_str()));
        }
        Ok(Self {
            root,
            required_files: required_files.into_boxed_slice(),
            archive,
        })
    }

    #[must_use]
    const fn root(&self) -> &CheckedTargetOutputPathTemplate {
        &self.root
    }

    #[must_use]
    fn required_files(&self) -> impl ExactSizeIterator<Item = &CheckedTargetPackagePath> {
        self.required_files.iter()
    }

    #[must_use]
    const fn archive(&self) -> Option<&CheckedTargetArchivePolicy> {
        self.archive.as_ref()
    }
}

/// Checked flat ZIP archive policy. The Beta targets admit no alternate
/// archive format or root convention.
#[derive(Debug)]
pub(crate) struct CheckedTargetArchivePolicy {
    path: CheckedTargetOutputPathTemplate,
}

impl CheckedTargetArchivePolicy {
    #[must_use]
    pub(crate) const fn path(&self) -> &CheckedTargetOutputPathTemplate {
        &self.path
    }
}

/// Closed package-root/archive path presentation. The sole dynamic segment is
/// the invocation's already-issued artifact stem.
#[derive(Debug)]
pub(crate) struct CheckedTargetOutputPathTemplate {
    form: CheckedTargetOutputPathTemplateForm,
}

#[derive(Debug)]
enum CheckedTargetOutputPathTemplateForm {
    Static(CheckedTargetPackagePath),
    ArtifactStem { suffix: Box<str> },
}

mod artifact_presentation;
pub(crate) use artifact_presentation::CheckedTargetArtifactStem;

/// One immutable snapshotted target asset bundle.
#[derive(Debug)]
struct CheckedTargetAssetBundle {
    source: Arc<str>,
    destination: Box<str>,
    members: Box<[CheckedTargetAssetMember]>,
}

/// One immutable member read from a checked asset bundle.
#[derive(Debug)]
struct CheckedTargetAssetMember {
    relative_path: Arc<str>,
    bytes: Arc<[u8]>,
}

/// Construction-issued stable scope for target-local artifact identities.
///
/// Built-ins use their unique registry key. Directory targets use a digest of
/// the checked, normalized manifest facts rather than raw TOML bytes or the
/// optional human-facing display name.
#[derive(Debug, PartialEq, Eq)]
pub struct TargetArtifactIdentityScope {
    kind: TargetArtifactIdentityScopeKind,
    value: Box<str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetArtifactIdentityScopeKind {
    BuiltinRegistryKey,
    CanonicalManifestDigest,
}

impl TargetArtifactIdentityScopeKind {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::BuiltinRegistryKey => "builtin-registry-key",
            Self::CanonicalManifestDigest => "canonical-manifest-blake3",
        }
    }
}

impl TargetArtifactIdentityScope {
    #[must_use]
    pub const fn kind(&self) -> TargetArtifactIdentityScopeKind {
        self.kind
    }

    #[must_use]
    pub fn value(&self) -> &str {
        &self.value
    }
}
