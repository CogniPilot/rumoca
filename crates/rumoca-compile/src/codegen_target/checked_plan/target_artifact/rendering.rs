use super::super::{CheckedOtherTargetPackagePlan, CheckedOtherUnpackagedTargetPlan};
use super::*;
use crate::codegen_target::descriptors::validate_fmi_capability_contract;

struct MemberBrand<'member> {
    _invariant: PhantomData<fn(&'member mut ()) -> &'member mut ()>,
}

struct MemberScope;

impl<'member> MemberBrand<'member> {
    fn mint(_scope: &'member mut MemberScope) -> Self {
        Self {
            _invariant: PhantomData,
        }
    }

    fn erase_at_fold(self) {}
}

struct ClosedPackageFile {
    member: CompletedPackageMember,
    digest: String,
}

struct ClosedUnpackagedFile {
    member: CompletedUnpackagedMember,
    digest: String,
}

trait PackageMemberCloseOperation<'operation, 'inv> {
    fn close<'member>(
        self,
        brand: MemberBrand<'member>,
    ) -> Result<PackageMemberCompletion<'member, 'operation, 'inv>>;
}

trait UnpackagedMemberCloseOperation {
    fn close<'member>(
        self,
        brand: MemberBrand<'member>,
    ) -> Result<UnpackagedMemberCompletion<'member>>;
}

fn close_package_member<'operation, 'inv>(
    operation: impl PackageMemberCloseOperation<'operation, 'inv>,
) -> Result<ClosedPackageFile> {
    let mut scope = MemberScope;
    let completion = operation.close(MemberBrand::mint(&mut scope))?;
    Ok(completion.erase())
}

fn close_unpackaged_member(
    operation: impl UnpackagedMemberCloseOperation,
) -> Result<ClosedUnpackagedFile> {
    let mut scope = MemberScope;
    let completion = operation.close(MemberBrand::mint(&mut scope))?;
    Ok(completion.erase())
}

impl CheckedTargetPackageAsset {
    fn close(self) -> (Arc<CheckedTargetPackagePath>, Arc<[u8]>) {
        let Self {
            package_path,
            source,
            relative_path,
            position,
            bytes,
        } = self;
        drop((source, relative_path, position));
        (package_path, bytes)
    }
}

fn completed_asset(asset: CheckedTargetPackageAsset) -> CompletedPackageMember {
    let (package_path, bytes) = asset.close();
    CompletedPackageMember::Asset {
        path: package_path,
        bytes,
    }
}

struct PackagedAlgorithmCodeIssuer<'member, 'operation, 'inv> {
    step: Box<PackagedAlgorithmCodeRenderPlanStep>,
    package: &'operation rumoca_phase_codegen::PreparedAlgorithmCodePackage<'inv>,
    session: &'operation ArtifactSession<'inv>,
    brand: MemberBrand<'member>,
}

struct PackagedAlgorithmCodeCompletion<'member, 'inv, 'package> {
    step: Box<PackagedAlgorithmCodeRenderPlanStep>,
    rendered: rumoca_phase_codegen::RenderedPackagedAlgorithmCodeFile<'inv, 'package>,
    brand: MemberBrand<'member>,
}

impl PackagedAlgorithmCodeCompletion<'_, '_, '_> {
    fn erase(self) -> ClosedPackageFile {
        let digest = super::super::sha1_hex(self.rendered.content().as_bytes());
        let path: Box<str> = self.rendered.member().member_path().as_str().into();
        let content = self.rendered.into_completion_content();
        let _step = self.step;
        let _brand = self.brand;
        ClosedPackageFile {
            member: CompletedPackageMember::File {
                path,
                content: CompletedPackageContent(content),
            },
            digest,
        }
    }
}

impl<'member, 'operation, 'inv> PackagedAlgorithmCodeIssuer<'member, 'operation, 'inv> {
    fn close_rendered(
        self,
        checksums: &BTreeMap<String, String>,
    ) -> Result<PackagedAlgorithmCodeCompletion<'member, 'inv, 'operation>> {
        let identities = self
            .session
            .scoped_artifact_identities(self.step.core.artifact_identity_dependencies());
        let template = self.step.template.bind(self.session.brand);
        let template = rumoca_phase_codegen::prepare_packaged_algorithm_code_template_file(
            self.package,
            &template,
        );
        let rendered = rumoca_phase_codegen::render_packaged_algorithm_code_file(
            template,
            &self.session.template_bindings(&identities, checksums)?,
        )?;
        Ok(PackagedAlgorithmCodeCompletion {
            step: self.step,
            rendered,
            brand: self.brand,
        })
    }
}

struct PackagedAlgorithmCodeOperation<'operation, 'inv> {
    step: Box<PackagedAlgorithmCodeRenderPlanStep>,
    package: &'operation rumoca_phase_codegen::PreparedAlgorithmCodePackage<'inv>,
    session: &'operation ArtifactSession<'inv>,
    checksums: BTreeMap<String, String>,
}

impl<'operation, 'inv> PackageMemberCloseOperation<'operation, 'inv>
    for PackagedAlgorithmCodeOperation<'operation, 'inv>
{
    fn close<'member>(
        self,
        brand: MemberBrand<'member>,
    ) -> Result<PackageMemberCompletion<'member, 'operation, 'inv>> {
        PackagedAlgorithmCodeIssuer {
            step: self.step,
            package: self.package,
            session: self.session,
            brand,
        }
        .close_rendered(&self.checksums)
        .map(PackageMemberCompletion::AlgorithmCode)
    }
}

struct SolveAlgorithmIssuer<'member, 'operation, 'inv> {
    step: Box<SolveAlgorithmRenderPlanStep>,
    production: &'operation rumoca_phase_codegen::PreparedSolveAlgorithmProduction<'inv>,
    session: &'operation ArtifactSession<'inv>,
    brand: MemberBrand<'member>,
}

enum SolveRenderedFile<'inv, 'production> {
    AlgorithmCode(rumoca_phase_codegen::RenderedCorrelatedAlgorithmCodeFile<'inv, 'production>),
    Production(rumoca_phase_codegen::RenderedProductionCodeFile<'inv, 'production>),
}

struct SolveAlgorithmCompletion<'member, 'inv, 'production> {
    step: Box<SolveAlgorithmRenderPlanStep>,
    rendered: SolveRenderedFile<'inv, 'production>,
    brand: MemberBrand<'member>,
}

impl SolveAlgorithmCompletion<'_, '_, '_> {
    fn erase(self) -> ClosedPackageFile {
        let (path, content, digest) = match self.rendered {
            SolveRenderedFile::AlgorithmCode(rendered) => {
                let digest = super::super::sha1_hex(rendered.content().as_bytes());
                let path: Box<str> = rendered.member().member_path().as_str().into();
                (path, rendered.into_completion_content(), digest)
            }
            SolveRenderedFile::Production(rendered) => {
                let digest = super::super::sha1_hex(rendered.content().as_bytes());
                let path: Box<str> = rendered.member().member_path().as_str().into();
                (path, rendered.into_completion_content(), digest)
            }
        };
        let _step = self.step;
        let _brand = self.brand;
        ClosedPackageFile {
            member: CompletedPackageMember::File {
                path,
                content: CompletedPackageContent(content),
            },
            digest,
        }
    }
}

impl<'member, 'operation, 'inv> SolveAlgorithmIssuer<'member, 'operation, 'inv> {
    fn close_rendered(
        self,
        checksums: &BTreeMap<String, String>,
    ) -> Result<SolveAlgorithmCompletion<'member, 'inv, 'operation>> {
        let identities = self
            .session
            .scoped_artifact_identities(self.step.core().artifact_identity_dependencies());
        let rendered = match self.step.as_ref() {
            SolveAlgorithmRenderPlanStep::CorrelatedAlgorithmCode { template, .. } => {
                let template = template.bind(self.session.brand);
                let template =
                    rumoca_phase_codegen::prepare_correlated_algorithm_code_template_file(
                        self.production,
                        &template,
                    );
                SolveRenderedFile::AlgorithmCode(
                    rumoca_phase_codegen::render_correlated_algorithm_code_file(
                        template,
                        &self.session.template_bindings(&identities, checksums)?,
                    )?,
                )
            }
            SolveAlgorithmRenderPlanStep::ProductionCode { template, .. } => {
                let template = template.bind(self.session.brand);
                let template = rumoca_phase_codegen::prepare_solve_algorithm_template_file(
                    self.production,
                    &template,
                );
                SolveRenderedFile::Production(
                    rumoca_phase_codegen::render_solve_algorithm_production_file(
                        template,
                        &self.session.template_bindings(&identities, checksums)?,
                    )?,
                )
            }
        };
        Ok(SolveAlgorithmCompletion {
            step: self.step,
            rendered,
            brand: self.brand,
        })
    }
}

struct SolveAlgorithmOperation<'operation, 'inv> {
    step: Box<SolveAlgorithmRenderPlanStep>,
    production: &'operation rumoca_phase_codegen::PreparedSolveAlgorithmProduction<'inv>,
    session: &'operation ArtifactSession<'inv>,
    checksums: BTreeMap<String, String>,
}

impl<'operation, 'inv> PackageMemberCloseOperation<'operation, 'inv>
    for SolveAlgorithmOperation<'operation, 'inv>
{
    fn close<'member>(
        self,
        brand: MemberBrand<'member>,
    ) -> Result<PackageMemberCompletion<'member, 'operation, 'inv>> {
        SolveAlgorithmIssuer {
            step: self.step,
            production: self.production,
            session: self.session,
            brand,
        }
        .close_rendered(&self.checksums)
        .map(PackageMemberCompletion::SolveAlgorithm)
    }
}

struct AstDirectProduct(rumoca_phase_codegen::PreparedAstRendering);
struct FlatDirectProduct(rumoca_phase_codegen::PreparedFlatRendering);
struct DaeDirectProduct(rumoca_phase_codegen::PreparedDaeRendering);
struct SolveDirectProduct(rumoca_phase_codegen::PreparedSolveModelRendering);
struct FmiDirectProduct(rumoca_phase_codegen::PreparedFmiComponentRendering);

impl AstDirectProduct {
    fn render_content(
        &self,
        body: &str,
        artifact: &rumoca_phase_codegen::TemplateBindings<'_>,
    ) -> Result<rumoca_phase_codegen::UntrustedRenderedText> {
        rumoca_phase_codegen::render_ast_template_content(body, &self.0, artifact)
            .map_err(Into::into)
    }
}
impl FlatDirectProduct {
    fn render_content(
        &self,
        body: &str,
        artifact: &rumoca_phase_codegen::TemplateBindings<'_>,
    ) -> Result<rumoca_phase_codegen::UntrustedRenderedText> {
        rumoca_phase_codegen::render_flat_template_content(&self.0, body, artifact)
            .map_err(Into::into)
    }
}

impl DaeDirectProduct {
    fn render_content(
        &self,
        body: &str,
        artifact: &rumoca_phase_codegen::TemplateBindings<'_>,
    ) -> Result<rumoca_phase_codegen::UntrustedRenderedText> {
        rumoca_phase_codegen::render_dae_template_content(&self.0, body, artifact)
            .map_err(Into::into)
    }
}

impl SolveDirectProduct {
    fn render_content(
        &self,
        body: &str,
        artifact: &rumoca_phase_codegen::TemplateBindings<'_>,
    ) -> Result<rumoca_phase_codegen::UntrustedRenderedText> {
        self.0
            .render_solve_model_content(body, artifact)
            .map_err(Into::into)
    }
}

impl FmiDirectProduct {
    fn render_content(
        &self,
        body: &str,
        artifact: &rumoca_phase_codegen::TemplateBindings<'_>,
    ) -> Result<rumoca_phase_codegen::UntrustedRenderedText> {
        self.0
            .render_fmi_component_content(body, artifact)
            .map_err(Into::into)
    }
}

macro_rules! define_direct_package_close {
    (
        $issuer:ident,
        $completion:ident,
        $operation:ident,
        $product:ty,
        $template:ty,
        $rendered:ty,
        $variant:ident
    ) => {
        struct $issuer<'member, 'operation, 'inv> {
            step: Box<DirectPackageRenderPlanStep<$template>>,
            path: Arc<CheckedTargetPackagePath>,
            product: &'operation $product,
            session: &'operation ArtifactSession<'inv>,
            brand: MemberBrand<'member>,
        }

        struct $completion<'member> {
            core: ClosedRenderPlanStep,
            path: Arc<CheckedTargetPackagePath>,
            rendered: $rendered,
            digest: String,
            brand: MemberBrand<'member>,
        }

        impl $completion<'_> {
            fn erase(self) -> ClosedPackageFile {
                let Self {
                    core,
                    path,
                    rendered,
                    digest,
                    brand,
                } = self;
                core.erase_after_checksum_resolution();
                brand.erase_at_fold();
                ClosedPackageFile {
                    member: CompletedPackageMember::File {
                        path: path.as_str().into(),
                        content: CompletedPackageContent(rendered.into_content()),
                    },
                    digest,
                }
            }
        }

        impl<'member, 'operation, 'inv> $issuer<'member, 'operation, 'inv> {
            fn close_rendered(
                self,
                checksums: &BTreeMap<String, String>,
            ) -> Result<$completion<'member>> {
                let DirectPackageRenderPlanStep { core, template } = *self.step;
                let identities = self
                    .session
                    .scoped_artifact_identities(core.artifact_identity_dependencies());
                let rendered = self.product.render_content(
                    template.body(),
                    &self.session.template_bindings(&identities, checksums)?,
                )?;
                let digest = super::super::sha1_hex(rendered.content().as_bytes());
                Ok($completion {
                    core,
                    path: self.path,
                    rendered,
                    digest,
                    brand: self.brand,
                })
            }
        }

        struct $operation<'operation, 'inv> {
            step: Box<DirectPackageRenderPlanStep<$template>>,
            path: Arc<CheckedTargetPackagePath>,
            product: &'operation $product,
            session: &'operation ArtifactSession<'inv>,
            checksums: BTreeMap<String, String>,
        }

        impl<'operation, 'inv> PackageMemberCloseOperation<'operation, 'inv>
            for $operation<'operation, 'inv>
        {
            fn close<'member>(
                self,
                brand: MemberBrand<'member>,
            ) -> Result<PackageMemberCompletion<'member, 'operation, 'inv>> {
                $issuer {
                    step: self.step,
                    path: self.path,
                    product: self.product,
                    session: self.session,
                    brand,
                }
                .close_rendered(&self.checksums)
                .map(PackageMemberCompletion::$variant)
            }
        }
    };
}

define_direct_package_close!(
    AstPackageIssuer,
    AstPackageCompletion,
    AstPackageOperation,
    AstDirectProduct,
    DirectTargetTemplate,
    rumoca_phase_codegen::UntrustedRenderedText,
    DirectAst
);
define_direct_package_close!(
    FlatPackageIssuer,
    FlatPackageCompletion,
    FlatPackageOperation,
    FlatDirectProduct,
    DirectTargetTemplate,
    rumoca_phase_codegen::UntrustedRenderedText,
    DirectFlat
);
define_direct_package_close!(
    DaePackageIssuer,
    DaePackageCompletion,
    DaePackageOperation,
    DaeDirectProduct,
    DirectTargetTemplate,
    rumoca_phase_codegen::UntrustedRenderedText,
    DirectDae
);
define_direct_package_close!(
    SolveDirectPackageIssuer,
    SolveDirectPackageCompletion,
    SolveDirectPackageOperation,
    SolveDirectProduct,
    DirectTargetTemplate,
    rumoca_phase_codegen::UntrustedRenderedText,
    DirectSolve
);
define_direct_package_close!(
    FmiPackageIssuer,
    FmiPackageCompletion,
    FmiPackageOperation,
    FmiDirectProduct,
    DirectTargetTemplate,
    rumoca_phase_codegen::UntrustedRenderedText,
    DirectFmi
);

enum PackageMemberCompletion<'member, 'operation, 'inv> {
    AlgorithmCode(PackagedAlgorithmCodeCompletion<'member, 'inv, 'operation>),
    SolveAlgorithm(SolveAlgorithmCompletion<'member, 'inv, 'operation>),
    DirectAst(AstPackageCompletion<'member>),
    DirectFlat(FlatPackageCompletion<'member>),
    DirectDae(DaePackageCompletion<'member>),
    DirectSolve(SolveDirectPackageCompletion<'member>),
    DirectFmi(FmiPackageCompletion<'member>),
}

impl PackageMemberCompletion<'_, '_, '_> {
    fn erase(self) -> ClosedPackageFile {
        match self {
            Self::AlgorithmCode(completion) => completion.erase(),
            Self::SolveAlgorithm(completion) => completion.erase(),
            Self::DirectAst(completion) => completion.erase(),
            Self::DirectFlat(completion) => completion.erase(),
            Self::DirectDae(completion) => completion.erase(),
            Self::DirectSolve(completion) => completion.erase(),
            Self::DirectFmi(completion) => completion.erase(),
        }
    }
}

struct AlgorithmCodeSourceIssuer<'member, 'operation, 'inv> {
    step: Box<AlgorithmCodeSourceRenderPlanStep>,
    renderer: &'operation rumoca_phase_codegen::AlgorithmCodeTemplateRenderer<'inv>,
    session: &'operation ArtifactSession<'inv>,
    brand: MemberBrand<'member>,
}

struct AlgorithmCodeSourceCompletion<'member> {
    step: Box<AlgorithmCodeSourceRenderPlanStep>,
    content: String,
    digest: String,
    brand: MemberBrand<'member>,
}

impl AlgorithmCodeSourceCompletion<'_> {
    fn erase(self) -> ClosedUnpackagedFile {
        let AlgorithmCodeSourceRenderPlanStep {
            core,
            template: _,
            output_path,
            mode,
        } = *self.step;
        core.erase_after_checksum_resolution();
        let _brand = self.brand;
        ClosedUnpackagedFile {
            member: CompletedUnpackagedMember {
                path: output_path,
                content: self.content,
                mode,
            },
            digest: self.digest,
        }
    }
}

impl<'member, 'operation, 'inv> AlgorithmCodeSourceIssuer<'member, 'operation, 'inv> {
    fn close_rendered(
        self,
        checksums: &BTreeMap<String, String>,
    ) -> Result<AlgorithmCodeSourceCompletion<'member>> {
        let identities = self
            .session
            .scoped_artifact_identities(self.step.core.artifact_identity_dependencies());
        let template = self.step.template.bind(self.session.brand);
        let rendered = self.renderer.render_file(
            &template,
            &self.session.template_bindings(&identities, checksums)?,
        )?;
        let digest = super::super::sha1_hex(rendered.content().as_bytes());
        let content = rendered.into_completion_content();
        Ok(AlgorithmCodeSourceCompletion {
            step: self.step,
            content,
            digest,
            brand: self.brand,
        })
    }
}

struct AlgorithmCodeSourceOperation<'operation, 'inv> {
    step: Box<AlgorithmCodeSourceRenderPlanStep>,
    renderer: &'operation rumoca_phase_codegen::AlgorithmCodeTemplateRenderer<'inv>,
    session: &'operation ArtifactSession<'inv>,
    checksums: BTreeMap<String, String>,
}

impl<'operation, 'inv> UnpackagedMemberCloseOperation
    for AlgorithmCodeSourceOperation<'operation, 'inv>
{
    fn close<'member>(
        self,
        brand: MemberBrand<'member>,
    ) -> Result<UnpackagedMemberCompletion<'member>> {
        AlgorithmCodeSourceIssuer {
            step: self.step,
            renderer: self.renderer,
            session: self.session,
            brand,
        }
        .close_rendered(&self.checksums)
        .map(UnpackagedMemberCompletion::AlgorithmCode)
    }
}

macro_rules! define_direct_unpacked_close {
    (
        $issuer:ident,
        $completion:ident,
        $operation:ident,
        $product:ty,
        $variant:ident
    ) => {
        struct $issuer<'member, 'operation, 'inv> {
            step: Box<DirectUnpackagedRenderPlanStep<DirectTargetTemplate>>,
            product: &'operation $product,
            session: &'operation ArtifactSession<'inv>,
            brand: MemberBrand<'member>,
        }

        struct $completion<'member> {
            core: ClosedRenderPlanStep,
            mode: Option<u32>,
            path: String,
            content: rumoca_phase_codegen::UntrustedRenderedText,
            digest: String,
            brand: MemberBrand<'member>,
        }

        impl $completion<'_> {
            fn erase(self) -> ClosedUnpackagedFile {
                let Self {
                    core,
                    mode,
                    path,
                    content,
                    digest,
                    brand,
                } = self;
                core.erase_after_checksum_resolution();
                brand.erase_at_fold();
                ClosedUnpackagedFile {
                    member: CompletedUnpackagedMember {
                        path,
                        content: content.into_content(),
                        mode,
                    },
                    digest,
                }
            }
        }

        impl<'member, 'operation, 'inv> $issuer<'member, 'operation, 'inv> {
            fn close_rendered(
                self,
                checksums: &BTreeMap<String, String>,
            ) -> Result<$completion<'member>> {
                let DirectUnpackagedRenderPlanStep {
                    core,
                    template,
                    mode,
                } = *self.step;
                let identities = self
                    .session
                    .scoped_artifact_identities(core.artifact_identity_dependencies());
                let artifact = self.session.template_bindings(&identities, checksums)?;
                let content = self.product.render_content(template.body(), &artifact)?;
                let path = rumoca_phase_codegen::render_output_path(
                    template.output_path(),
                    self.session.artifact_stem.as_str(),
                )?
                .into_content();
                let digest = super::super::sha1_hex(content.content().as_bytes());
                Ok($completion {
                    core,
                    mode,
                    path,
                    content,
                    digest,
                    brand: self.brand,
                })
            }
        }

        struct $operation<'operation, 'inv> {
            step: Box<DirectUnpackagedRenderPlanStep<DirectTargetTemplate>>,
            product: &'operation $product,
            session: &'operation ArtifactSession<'inv>,
            checksums: BTreeMap<String, String>,
        }

        impl<'operation, 'inv> UnpackagedMemberCloseOperation for $operation<'operation, 'inv> {
            fn close<'member>(
                self,
                brand: MemberBrand<'member>,
            ) -> Result<UnpackagedMemberCompletion<'member>> {
                $issuer {
                    step: self.step,
                    product: self.product,
                    session: self.session,
                    brand,
                }
                .close_rendered(&self.checksums)
                .map(UnpackagedMemberCompletion::$variant)
            }
        }
    };
}

define_direct_unpacked_close!(
    AstUnpackagedIssuer,
    AstUnpackagedCompletion,
    AstUnpackagedOperation,
    AstDirectProduct,
    DirectAst
);
define_direct_unpacked_close!(
    FlatUnpackagedIssuer,
    FlatUnpackagedCompletion,
    FlatUnpackagedOperation,
    FlatDirectProduct,
    DirectFlat
);
define_direct_unpacked_close!(
    DaeUnpackagedIssuer,
    DaeUnpackagedCompletion,
    DaeUnpackagedOperation,
    DaeDirectProduct,
    DirectDae
);
define_direct_unpacked_close!(
    SolveDirectUnpackagedIssuer,
    SolveDirectUnpackagedCompletion,
    SolveDirectUnpackagedOperation,
    SolveDirectProduct,
    DirectSolve
);
define_direct_unpacked_close!(
    FmiUnpackagedIssuer,
    FmiUnpackagedCompletion,
    FmiUnpackagedOperation,
    FmiDirectProduct,
    DirectFmi
);

enum UnpackagedMemberCompletion<'member> {
    AlgorithmCode(AlgorithmCodeSourceCompletion<'member>),
    DirectAst(AstUnpackagedCompletion<'member>),
    DirectFlat(FlatUnpackagedCompletion<'member>),
    DirectDae(DaeUnpackagedCompletion<'member>),
    DirectSolve(SolveDirectUnpackagedCompletion<'member>),
    DirectFmi(FmiUnpackagedCompletion<'member>),
}

impl UnpackagedMemberCompletion<'_> {
    fn erase(self) -> ClosedUnpackagedFile {
        match self {
            Self::AlgorithmCode(completion) => completion.erase(),
            Self::DirectAst(completion) => completion.erase(),
            Self::DirectFlat(completion) => completion.erase(),
            Self::DirectDae(completion) => completion.erase(),
            Self::DirectSolve(completion) => completion.erase(),
            Self::DirectFmi(completion) => completion.erase(),
        }
    }
}

fn checksum_map(
    bindings: &[super::super::TargetResolvedChecksumBinding],
    digests: &[String],
) -> BTreeMap<String, String> {
    super::super::resolved_checksum_map(bindings, digests)
}

/// Affine admitted Algorithm Code lowering authority.
///
/// Minting one consumes a checked Algorithm-Code-bearing plan's capability
/// contract and validates it against one exact DAE before any artifact
/// session, identity, or completion presentation exists. The bound target
/// identity, DAE, model name, and arithmetic profile leave only through the
/// consuming [`Self::lower`], so Algorithm Code lowering cannot be invoked
/// with independently selected parts.
#[must_use]
struct AdmittedAlgorithmCodeLowering<'compilation> {
    dae: &'compilation rumoca_ir_dae::Dae,
    model_name: &'compilation str,
    target_label: Box<str>,
    profile: AlgorithmCodeArithmeticProfile,
}

/// Prepare the direct Solve-model product from already-destructured parts.
///
/// Non-plan inputs only, and returns the precise `SolveDirectProduct` rather
/// than any widened union.
fn prepare_solve_direct(
    compilation: &StrictCompilation,
    capability_contract: super::super::CheckedTargetCapabilityContract,
) -> Result<SolveDirectProduct> {
    let model = admit_solve_model_direct(compilation, capability_contract)?;
    Ok(SolveDirectProduct(
        rumoca_phase_codegen::PreparedSolveModelRendering::prepare(model)?,
    ))
}

/// Prepare the direct FMI component product from already-destructured parts.
///
/// Keeps the capability-contract validation between issue and preparation, in
/// the original order, and returns the precise `FmiDirectProduct`.
fn prepare_fmi_direct(
    compilation: &StrictCompilation,
    capability_contract: &super::super::CheckedTargetCapabilityContract,
) -> Result<FmiDirectProduct> {
    let event_free = lower_event_free_fmi_view(compilation, capability_contract)?;
    let admitted = rumoca_phase_codegen::AdmittedFmiRenderingInput::issue(event_free)?;
    validate_fmi_capability_contract(&admitted, capability_contract)?;
    Ok(FmiDirectProduct(
        rumoca_phase_codegen::PreparedFmiComponentRendering::from_admitted(admitted)?,
    ))
}

/// Admit and lower one algorithm-code product from already-destructured parts.
///
/// Takes only non-plan values, so no render or product plan crosses this
/// boundary; the plan stays opened and consumed inside the sole constructor.
fn admit_algorithm_code_product<'inv>(
    compilation: &StrictCompilation,
    contract: super::super::CheckedTargetCapabilityContract,
    arithmetic: TargetAlgorithmCodeArithmetic,
    brand: TargetInvocationBrand<'inv>,
) -> Result<rumoca_ir_galec::TracedAlgorithmCodeProduct<'inv>> {
    AdmittedAlgorithmCodeLowering::admit(compilation, contract, arithmetic)?.lower(brand)
}

impl<'compilation> AdmittedAlgorithmCodeLowering<'compilation> {
    fn admit(
        compilation: &'compilation StrictCompilation,
        contract: super::super::CheckedTargetCapabilityContract,
        arithmetic: TargetAlgorithmCodeArithmetic,
    ) -> Result<Self> {
        let dae = compilation.result().dae.as_ref();
        validate_dae_render_capability_contract(dae, &contract)?;
        Ok(Self {
            dae,
            model_name: compilation.model_name(),
            target_label: contract.label,
            profile: AlgorithmCodeArithmeticProfile::construct(
                arithmetic.source_real,
                arithmetic.source_integer,
                arithmetic.real_matrix_multiply,
            ),
        })
    }

    fn lower<'inv>(
        self,
        brand: TargetInvocationBrand<'inv>,
    ) -> Result<rumoca_ir_galec::TracedAlgorithmCodeProduct<'inv>> {
        let input = GalecInput::new(self.dae, self.model_name);
        let options = GalecOptions::new(self.profile);
        rumoca_phase_galec::lower_to_algorithm_code(brand, &input, &options).map_err(|errors| {
            anyhow::Error::new(rumoca_phase_galec::GalecTargetErrors::from(errors)).context(
                format!("GALEC projection rejected target '{}'", self.target_label),
            )
        })
    }
}

/// Prepare the sole Flat presentation from the compilation admitted by this
/// target invocation.
fn prepare_flat_direct(compilation: &StrictCompilation) -> Result<FlatDirectProduct> {
    rumoca_phase_codegen::prepare_flat_rendering(&compilation.result().flat)
        .map(FlatDirectProduct)
        .map_err(Into::into)
}

/// Validate and prepare the sole DAE presentation from the same compilation.
fn prepare_dae_direct(
    compilation: &StrictCompilation,
    contract: &super::super::CheckedTargetCapabilityContract,
) -> Result<DaeDirectProduct> {
    let dae = compilation.result().dae.as_ref();
    validate_dae_render_capability_contract(dae, contract)?;
    rumoca_phase_codegen::prepare_dae_rendering(dae)
        .map(DaeDirectProduct)
        .map_err(Into::into)
}

/// Admit one direct Solve-model product: validate the capability contract
/// against the exact DAE and the lowered Solve problem, before any artifact
/// session exists.
fn admit_solve_model_direct(
    compilation: &StrictCompilation,
    contract: super::super::CheckedTargetCapabilityContract,
) -> Result<rumoca_ir_solve::SolveModel> {
    let dae = compilation.result().dae.as_ref();
    validate_dae_render_capability_contract(dae, &contract)?;
    let lowered = rumoca_phase_solve::lower_solve_model(dae, &HashMap::new(), |_| {})
        .context("Construct checked SolveModel for target")?;
    let model = lowered.into_model();
    validate_solve_capability_contract(model.problem(), &contract)?;
    Ok(model)
}

/// Admit the DAE half of one direct FMI product: validate the capability
/// contract against the exact DAE and lower the checked event-free component
/// view. The FMI template-domain admission and its capability match follow
/// inside the bundle mint, before any artifact session exists.
fn lower_event_free_fmi_view(
    compilation: &StrictCompilation,
    contract: &super::super::CheckedTargetCapabilityContract,
) -> Result<rumoca_ir_solve::fmi::FmiEventFreeCodegenView> {
    let dae = compilation.result().dae.as_ref();
    validate_dae_render_capability_contract(dae, contract)?;
    let component = lower_fmi_component(dae)?;
    component
        .into_codegen_view()
        .try_event_free()
        .context("Prepare checked event-free FMI component view")
}

/// Admit the packaged Algorithm Code operation from the already-admitted
/// product and the destructured non-plan parts.
fn admit_packaged_algorithm_code<'inv>(
    product: rumoca_ir_galec::TracedAlgorithmCodeProduct<'inv>,
    policy: super::super::CheckedTargetPackagePolicy,
    artifact_layout: super::super::CheckedAlgorithmCodeLayout,
    fold: CheckedPackagedAlgorithmCodeFold,
) -> AdmittedPackagedOperation<'inv> {
    let package = rumoca_phase_codegen::prepare_algorithm_code_package(
        product,
        artifact_layout.into_layout(),
    );
    AdmittedPackagedOperation::AlgorithmCode {
        policy,
        package: Box::new(package),
        fold,
    }
}

/// Admit the packaged Solve Algorithm operation from the already-admitted
/// product and the destructured non-plan parts.
fn admit_packaged_solve_algorithm<'inv>(
    product: rumoca_ir_galec::TracedAlgorithmCodeProduct<'inv>,
    policy: super::super::CheckedTargetPackagePolicy,
    production_profile: rumoca_phase_codegen::SolveAlgorithmProductionProfile,
    layout: super::super::CheckedSolveAlgorithmLayout,
    fold: CheckedSolveAlgorithmFold,
) -> Result<AdmittedPackagedOperation<'inv>> {
    let solve = rumoca_phase_solve::lower_solve_algorithm_product(product)?;
    let production = rumoca_phase_codegen::prepare_solve_algorithm_production(
        solve,
        production_profile,
        layout.into_layout(),
    )?;
    Ok(AdmittedPackagedOperation::SolveAlgorithm {
        policy,
        production: Box::new(production),
        fold,
    })
}

/// Admit the packaged AST operation. AST presentation is infallible, so this
/// helper is total and carries no `Result`.
fn admit_packaged_ast<'inv>(
    compilation: &StrictCompilation,
    policy: super::super::CheckedTargetPackagePolicy,
    fold: CheckedDirectPackageFold<DirectTargetTemplate>,
) -> AdmittedPackagedOperation<'inv> {
    AdmittedPackagedOperation::Ast {
        product: AstDirectProduct(rumoca_phase_codegen::prepare_ast_rendering(
            compilation.resolved().inner(),
        )),
        policy,
        fold,
    }
}

/// Admit the packaged flat-model operation from destructured non-plan parts.
fn admit_packaged_flat<'inv>(
    compilation: &StrictCompilation,
    policy: super::super::CheckedTargetPackagePolicy,
    fold: CheckedDirectPackageFold<DirectTargetTemplate>,
) -> Result<AdmittedPackagedOperation<'inv>> {
    Ok(AdmittedPackagedOperation::Flat {
        policy,
        product: prepare_flat_direct(compilation)?,
        fold,
    })
}

/// Admit the packaged DAE operation from destructured non-plan parts.
fn admit_packaged_dae<'inv>(
    compilation: &StrictCompilation,
    policy: super::super::CheckedTargetPackagePolicy,
    capability_contract: super::super::CheckedTargetCapabilityContract,
    fold: CheckedDirectPackageFold<DirectTargetTemplate>,
) -> Result<AdmittedPackagedOperation<'inv>> {
    Ok(AdmittedPackagedOperation::Dae {
        policy,
        product: prepare_dae_direct(compilation, &capability_contract)?,
        fold,
    })
}

/// Admit the packaged Solve-model operation from destructured non-plan parts.
fn admit_packaged_solve_model<'inv>(
    compilation: &StrictCompilation,
    policy: super::super::CheckedTargetPackagePolicy,
    capability_contract: super::super::CheckedTargetCapabilityContract,
    fold: CheckedDirectPackageFold<DirectTargetTemplate>,
) -> Result<AdmittedPackagedOperation<'inv>> {
    Ok(AdmittedPackagedOperation::SolveModel {
        policy,
        product: prepare_solve_direct(compilation, capability_contract)?,
        fold,
    })
}

/// Admit the packaged FMI-component operation from destructured non-plan parts.
fn admit_packaged_fmi_component<'inv>(
    compilation: &StrictCompilation,
    policy: super::super::CheckedTargetPackagePolicy,
    capability_contract: super::super::CheckedTargetCapabilityContract,
    fold: CheckedDirectPackageFold<DirectTargetTemplate>,
) -> Result<AdmittedPackagedOperation<'inv>> {
    Ok(AdmittedPackagedOperation::FmiComponent {
        policy,
        product: prepare_fmi_direct(compilation, &capability_contract)?,
        fold,
    })
}

/// Admit the unpackaged Algorithm Code operation from the already-admitted
/// product and the destructured non-plan parts.
fn admit_unpackaged_algorithm_code<'inv>(
    product: &rumoca_ir_galec::TracedAlgorithmCodeProduct<'inv>,
    fold: CheckedAlgorithmCodeSourceFold,
) -> Result<AdmittedUnpackagedOperation<'inv>> {
    let renderer = rumoca_phase_codegen::AlgorithmCodeTemplateRenderer::new(product)?;
    Ok(AdmittedUnpackagedOperation::AlgorithmCode { renderer, fold })
}

/// Admit the unpackaged AST operation. AST presentation is infallible, so this
/// helper is total and carries no `Result`.
fn admit_unpackaged_ast<'inv>(
    compilation: &StrictCompilation,
    fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
) -> AdmittedUnpackagedOperation<'inv> {
    AdmittedUnpackagedOperation::Ast {
        product: AstDirectProduct(rumoca_phase_codegen::prepare_ast_rendering(
            compilation.resolved().inner(),
        )),
        fold,
    }
}

/// Admit the unpackaged flat-model operation from destructured non-plan parts.
fn admit_unpackaged_flat<'inv>(
    compilation: &StrictCompilation,
    fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
) -> Result<AdmittedUnpackagedOperation<'inv>> {
    Ok(AdmittedUnpackagedOperation::Flat {
        product: prepare_flat_direct(compilation)?,
        fold,
    })
}

/// Admit the unpackaged DAE operation from destructured non-plan parts.
fn admit_unpackaged_dae<'inv>(
    compilation: &StrictCompilation,
    capability_contract: super::super::CheckedTargetCapabilityContract,
    fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
) -> Result<AdmittedUnpackagedOperation<'inv>> {
    Ok(AdmittedUnpackagedOperation::Dae {
        product: prepare_dae_direct(compilation, &capability_contract)?,
        fold,
    })
}

/// Admit the unpackaged Solve-model operation from destructured non-plan parts.
fn admit_unpackaged_solve_model<'inv>(
    compilation: &StrictCompilation,
    capability_contract: super::super::CheckedTargetCapabilityContract,
    fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
) -> Result<AdmittedUnpackagedOperation<'inv>> {
    Ok(AdmittedUnpackagedOperation::SolveModel {
        product: prepare_solve_direct(compilation, capability_contract)?,
        fold,
    })
}

/// Admit the unpackaged FMI-component operation from destructured non-plan parts.
fn admit_unpackaged_fmi_component<'inv>(
    compilation: &StrictCompilation,
    capability_contract: super::super::CheckedTargetCapabilityContract,
    fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
) -> Result<AdmittedUnpackagedOperation<'inv>> {
    Ok(AdmittedUnpackagedOperation::FmiComponent {
        product: prepare_fmi_direct(compilation, &capability_contract)?,
        fold,
    })
}

/// Exhaustive admitted packaged operation. Every fallible product family
/// (Algorithm Code, Solve Algorithm, direct Flat, DAE, Solve model, and
/// FMI component) retains the complete branded product prepared at
/// admission; rendering a variant performs only template byte production
/// over that product. Direct variants retain only their prepared
/// presentations, so post-admission rendering cannot name a compilation or
/// raw semantic root.
#[must_use]
enum AdmittedPackagedOperation<'inv> {
    AlgorithmCode {
        policy: super::super::CheckedTargetPackagePolicy,
        package: Box<rumoca_phase_codegen::PreparedAlgorithmCodePackage<'inv>>,
        fold: CheckedPackagedAlgorithmCodeFold,
    },
    SolveAlgorithm {
        policy: super::super::CheckedTargetPackagePolicy,
        production: Box<rumoca_phase_codegen::PreparedSolveAlgorithmProduction<'inv>>,
        fold: CheckedSolveAlgorithmFold,
    },
    Ast {
        product: AstDirectProduct,
        policy: super::super::CheckedTargetPackagePolicy,
        fold: CheckedDirectPackageFold<DirectTargetTemplate>,
    },
    Flat {
        policy: super::super::CheckedTargetPackagePolicy,
        product: FlatDirectProduct,
        fold: CheckedDirectPackageFold<DirectTargetTemplate>,
    },
    Dae {
        policy: super::super::CheckedTargetPackagePolicy,
        product: DaeDirectProduct,
        fold: CheckedDirectPackageFold<DirectTargetTemplate>,
    },
    SolveModel {
        policy: super::super::CheckedTargetPackagePolicy,
        product: SolveDirectProduct,
        fold: CheckedDirectPackageFold<DirectTargetTemplate>,
    },
    FmiComponent {
        policy: super::super::CheckedTargetPackagePolicy,
        product: FmiDirectProduct,
        fold: CheckedDirectPackageFold<DirectTargetTemplate>,
    },
}

impl<'inv> AdmittedPackagedOperation<'inv> {
    fn render(
        self,
        session: &ArtifactSession<'inv>,
    ) -> Result<(super::super::CheckedTargetPackagePolicy, CompletedMixedPlan)> {
        match self {
            Self::AlgorithmCode {
                policy,
                package,
                fold,
            } => Ok((
                policy,
                render_packaged_algorithm_code(fold, &package, session)?,
            )),
            Self::SolveAlgorithm {
                policy,
                production,
                fold,
            } => Ok((policy, render_solve_algorithm(fold, &production, session)?)),
            Self::Ast {
                product,
                policy,
                fold,
            } => Ok((policy, render_ast_package(fold, &product, session)?)),
            Self::Flat {
                policy,
                product,
                fold,
            } => Ok((policy, render_flat_package(fold, &product, session)?)),
            Self::Dae {
                policy,
                product,
                fold,
            } => Ok((policy, render_dae_package(fold, &product, session)?)),
            Self::SolveModel {
                policy,
                product,
                fold,
            } => Ok((
                policy,
                render_solve_direct_package(fold, &product, session)?,
            )),
            Self::FmiComponent {
                policy,
                product,
                fold,
            } => Ok((policy, render_fmi_package(fold, &product, session)?)),
        }
    }
}

/// Exhaustive admitted unpackaged operation, with the same complete
/// product-admission discipline as the packaged sum.
#[must_use]
enum AdmittedUnpackagedOperation<'inv> {
    AlgorithmCode {
        renderer: rumoca_phase_codegen::AlgorithmCodeTemplateRenderer<'inv>,
        fold: CheckedAlgorithmCodeSourceFold,
    },
    Ast {
        product: AstDirectProduct,
        fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
    },
    Flat {
        product: FlatDirectProduct,
        fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
    },
    Dae {
        product: DaeDirectProduct,
        fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
    },
    SolveModel {
        product: SolveDirectProduct,
        fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
    },
    FmiComponent {
        product: FmiDirectProduct,
        fold: CheckedDirectUnpackagedFold<DirectTargetTemplate>,
    },
}

impl<'inv> AdmittedUnpackagedOperation<'inv> {
    fn render(self, session: &ArtifactSession<'inv>) -> Result<Box<[CompletedUnpackagedMember]>> {
        match self {
            Self::AlgorithmCode { renderer, fold } => {
                render_algorithm_code_source(fold, &renderer, session)
            }
            Self::Ast { product, fold } => render_ast_unpacked(fold, &product, session),
            Self::Flat { product, fold } => render_flat_unpacked(fold, &product, session),
            Self::Dae { product, fold } => render_dae_unpacked(fold, &product, session),
            Self::SolveModel { product, fold } => {
                render_solve_direct_unpacked(fold, &product, session)
            }
            Self::FmiComponent { product, fold } => render_fmi_unpacked(fold, &product, session),
        }
    }
}

/// Exhaustive admitted target operation. Every capability check, semantic
/// lowering, and fallible presentation preparation has already run before
/// an artifact session can exist.
#[must_use]
enum AdmittedTargetOperation<'inv> {
    Packaged(AdmittedPackagedOperation<'inv>),
    Unpackaged(AdmittedUnpackagedOperation<'inv>),
}

/// One affine prepared emission owning the exact checked target metadata,
/// artifact session, and prepared operation admitted together.
///
/// The sole private constructor consumes one checked target and one session
/// input against one strict compilation. No raw compilation or IR root is
/// retained after construction, and the sole route out consumes this value.
#[must_use]
struct PreparedTargetEmission<'inv> {
    metadata: CheckedTargetMetadata,
    session: ArtifactSession<'inv>,
    operation: AdmittedTargetOperation<'inv>,
}

impl<'inv> PreparedTargetEmission<'inv> {
    /// Consume one complete checked target. Every capability-bearing product
    /// family validates its contract against the exact DAE, and every
    /// fallible semantic lowering and presentation preparation runs here,
    /// before any artifact session, identity, or completion presentation can
    /// exist.
    fn construct(
        brand: TargetInvocationBrand<'inv>,
        compilation: &StrictCompilation,
        target: CheckedTargetBundle,
        input: ArtifactSessionInput,
    ) -> Result<Self> {
        let CheckedTargetBundle {
            metadata,
            render_authority,
        } = target;
        let plan = render_authority.into_plan();
        let operation = match plan {
            CheckedTargetRenderPlan::Packaged(plan) => {
                AdmittedTargetOperation::Packaged(match plan.into_product_plan() {
                    CheckedTargetPackageProductPlan::AlgorithmCode(plan) => {
                        let (policy, contract, arithmetic, artifact_layout, fold) =
                            plan.into_parts();
                        let product =
                            admit_algorithm_code_product(compilation, contract, arithmetic, brand)?;
                        admit_packaged_algorithm_code(product, policy, artifact_layout, fold)
                    }
                    CheckedTargetPackageProductPlan::SolveAlgorithm(plan) => {
                        let (policy, contract, arithmetic, production_profile, layout, fold) =
                            plan.into_parts();
                        let product =
                            admit_algorithm_code_product(compilation, contract, arithmetic, brand)?;
                        admit_packaged_solve_algorithm(
                            product,
                            policy,
                            production_profile,
                            layout,
                            fold,
                        )?
                    }
                    CheckedTargetPackageProductPlan::Other(plan) => match plan {
                        CheckedOtherTargetPackagePlan::Ast { policy, fold } => {
                            admit_packaged_ast(compilation, policy, fold)
                        }
                        CheckedOtherTargetPackagePlan::Flat { policy, fold } => {
                            admit_packaged_flat(compilation, policy, fold)?
                        }
                        CheckedOtherTargetPackagePlan::Dae {
                            policy,
                            capability_contract,
                            fold,
                        } => admit_packaged_dae(compilation, policy, capability_contract, fold)?,
                        CheckedOtherTargetPackagePlan::SolveModel {
                            policy,
                            capability_contract: contract,
                            fold,
                        } => admit_packaged_solve_model(compilation, policy, contract, fold)?,
                        CheckedOtherTargetPackagePlan::FmiComponent {
                            policy,
                            capability_contract: contract,
                            fold,
                        } => admit_packaged_fmi_component(compilation, policy, contract, fold)?,
                    },
                })
            }
            CheckedTargetRenderPlan::Unpackaged(plan) => {
                AdmittedTargetOperation::Unpackaged(match plan.into_product_plan()? {
                    CheckedUnpackagedTargetProductPlan::AlgorithmCode(plan) => {
                        let (contract, arithmetic, fold) = plan.into_parts();
                        let product =
                            admit_algorithm_code_product(compilation, contract, arithmetic, brand)?;
                        admit_unpackaged_algorithm_code(&product, fold)?
                    }
                    CheckedUnpackagedTargetProductPlan::Other(plan) => match plan {
                        CheckedOtherUnpackagedTargetPlan::Ast(fold) => {
                            admit_unpackaged_ast(compilation, fold)
                        }
                        CheckedOtherUnpackagedTargetPlan::Flat(fold) => {
                            admit_unpackaged_flat(compilation, fold)?
                        }
                        CheckedOtherUnpackagedTargetPlan::Dae {
                            capability_contract,
                            fold,
                        } => admit_unpackaged_dae(compilation, capability_contract, fold)?,
                        CheckedOtherUnpackagedTargetPlan::SolveModel {
                            capability_contract,
                            fold,
                        } => admit_unpackaged_solve_model(compilation, capability_contract, fold)?,
                        CheckedOtherUnpackagedTargetPlan::FmiComponent {
                            capability_contract,
                            fold,
                        } => {
                            admit_unpackaged_fmi_component(compilation, capability_contract, fold)?
                        }
                    },
                })
            }
        };
        let session = ArtifactSession::construct(
            brand,
            input,
            &metadata,
            compilation.canonical_model_identity(),
        )?;
        Ok(Self {
            metadata,
            session,
            operation,
        })
    }

    /// Consume the sole prepared emission route. The session, metadata, and
    /// prepared operation came from one constructor invocation, so only byte
    /// rendering and completed-artifact closure remain here.
    fn emit(self) -> Result<CompletedTargetArtifact> {
        let Self {
            metadata,
            session,
            operation,
        } = self;
        let presentation = CompletedPresentation::construct(&metadata, &session)?;
        match operation {
            AdmittedTargetOperation::Packaged(operation) => {
                let (policy, members) = operation.render(&session)?;
                let root = policy.root().render(&session.artifact_stem);
                let archive = policy
                    .archive()
                    .map(|archive| archive.path().render(&session.artifact_stem));
                let required_files = policy
                    .required_files()
                    .map(|path| Box::<str>::from(path.as_str()))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                Ok(CompletedTargetArtifact::Packaged(CompletedPackage {
                    presentation,
                    root,
                    archive,
                    required_files,
                    members,
                }))
            }
            AdmittedTargetOperation::Unpackaged(operation) => {
                let members = operation.render(&session)?;
                validate_completed_unpacked_paths(&members)?;
                Ok(CompletedTargetArtifact::Unpackaged(CompletedUnpackaged {
                    presentation,
                    members,
                }))
            }
        }
    }
}

impl StrictCompilation {
    /// Render one complete checked target under one explicit artifact session.
    /// This is the only public semantic-to-byte target operation.
    pub fn render_target(
        &self,
        target: CheckedTargetBundle,
        input: ArtifactSessionInput,
    ) -> Result<CompletedTargetArtifact> {
        with_target_invocation_brand(|brand| {
            PreparedTargetEmission::construct(brand, self, target, input)?.emit()
        })
    }
}

fn render_packaged_algorithm_code<'inv>(
    fold: CheckedPackagedAlgorithmCodeFold,
    package: &rumoca_phase_codegen::PreparedAlgorithmCodePackage<'inv>,
    session: &ArtifactSession<'inv>,
) -> Result<CompletedMixedPlan> {
    let CheckedPackagedAlgorithmCodeFold { members } = fold;
    let mut digests = Vec::new();
    let mut completed = Vec::with_capacity(members.len());
    for member in members.into_vec() {
        match member {
            CheckedPackagedAlgorithmCodeMemberPlan::File { step } => {
                let checksums = checksum_map(&step.core.incoming_checksums, &digests);
                let closed = close_package_member(PackagedAlgorithmCodeOperation {
                    step,
                    package,
                    session,
                    checksums,
                })?;
                completed.push(closed.member);
                digests.push(closed.digest);
            }
            CheckedPackagedAlgorithmCodeMemberPlan::Asset(asset) => {
                completed.push(completed_asset(asset));
            }
        }
    }
    Ok(CompletedMixedPlan {
        members: completed.into_boxed_slice(),
    })
}

fn render_solve_algorithm<'inv>(
    fold: CheckedSolveAlgorithmFold,
    production: &rumoca_phase_codegen::PreparedSolveAlgorithmProduction<'inv>,
    session: &ArtifactSession<'inv>,
) -> Result<CompletedMixedPlan> {
    let CheckedSolveAlgorithmFold { members } = fold;
    let mut digests = Vec::new();
    let mut completed = Vec::with_capacity(members.len());
    for member in members.into_vec() {
        match member {
            CheckedSolveAlgorithmMemberPlan::File { step } => {
                let checksums = checksum_map(&step.core().incoming_checksums, &digests);
                let closed = close_package_member(SolveAlgorithmOperation {
                    step,
                    production,
                    session,
                    checksums,
                })?;
                completed.push(closed.member);
                digests.push(closed.digest);
            }
            CheckedSolveAlgorithmMemberPlan::Asset(asset) => {
                completed.push(completed_asset(asset));
            }
        }
    }
    Ok(CompletedMixedPlan {
        members: completed.into_boxed_slice(),
    })
}

macro_rules! define_direct_package_renderer {
    ($function:ident, $template:ty, $product:ty, $operation:ident) => {
        fn $function<'inv>(
            fold: CheckedDirectPackageFold<$template>,
            product: &$product,
            session: &ArtifactSession<'inv>,
        ) -> Result<CompletedMixedPlan> {
            let CheckedDirectPackageFold { members } = fold;
            let mut digests = Vec::new();
            let mut completed = Vec::with_capacity(members.len());
            for member in members.into_vec() {
                match member {
                    CheckedDirectPackageMemberPlan::File { step, package_path } => {
                        let checksums = checksum_map(&step.core.incoming_checksums, &digests);
                        let closed = close_package_member($operation {
                            step,
                            path: package_path,
                            product,
                            session,
                            checksums,
                        })?;
                        completed.push(closed.member);
                        digests.push(closed.digest);
                    }
                    CheckedDirectPackageMemberPlan::Asset(asset) => {
                        completed.push(completed_asset(asset));
                    }
                }
            }
            Ok(CompletedMixedPlan {
                members: completed.into_boxed_slice(),
            })
        }
    };
}

define_direct_package_renderer!(
    render_ast_package,
    DirectTargetTemplate,
    AstDirectProduct,
    AstPackageOperation
);
define_direct_package_renderer!(
    render_flat_package,
    DirectTargetTemplate,
    FlatDirectProduct,
    FlatPackageOperation
);
define_direct_package_renderer!(
    render_dae_package,
    DirectTargetTemplate,
    DaeDirectProduct,
    DaePackageOperation
);
define_direct_package_renderer!(
    render_solve_direct_package,
    DirectTargetTemplate,
    SolveDirectProduct,
    SolveDirectPackageOperation
);
define_direct_package_renderer!(
    render_fmi_package,
    DirectTargetTemplate,
    FmiDirectProduct,
    FmiPackageOperation
);

fn render_algorithm_code_source<'inv>(
    fold: CheckedAlgorithmCodeSourceFold,
    renderer: &rumoca_phase_codegen::AlgorithmCodeTemplateRenderer<'inv>,
    session: &ArtifactSession<'inv>,
) -> Result<Box<[CompletedUnpackagedMember]>> {
    let CheckedAlgorithmCodeSourceFold { steps } = fold;
    let mut digests = Vec::new();
    let mut completed = Vec::with_capacity(steps.len());
    for step in steps.into_vec() {
        let checksums = checksum_map(&step.core.incoming_checksums, &digests);
        let closed = close_unpackaged_member(AlgorithmCodeSourceOperation {
            step,
            renderer,
            session,
            checksums,
        })?;
        digests.push(closed.digest);
        completed.push(closed.member);
    }
    Ok(completed.into_boxed_slice())
}

macro_rules! define_direct_unpacked_renderer {
    ($function:ident, $template:ty, $product:ty, $operation:ident) => {
        fn $function<'inv>(
            fold: CheckedDirectUnpackagedFold<$template>,
            product: &$product,
            session: &ArtifactSession<'inv>,
        ) -> Result<Box<[CompletedUnpackagedMember]>> {
            let CheckedDirectUnpackagedFold { steps } = fold;
            let mut digests = Vec::new();
            let mut completed = Vec::with_capacity(steps.len());
            for step in steps.into_vec() {
                let checksums = checksum_map(&step.core.incoming_checksums, &digests);
                let closed = close_unpackaged_member($operation {
                    step,
                    product,
                    session,
                    checksums,
                })?;
                digests.push(closed.digest);
                completed.push(closed.member);
            }
            Ok(completed.into_boxed_slice())
        }
    };
}

define_direct_unpacked_renderer!(
    render_ast_unpacked,
    DirectTargetTemplate,
    AstDirectProduct,
    AstUnpackagedOperation
);
define_direct_unpacked_renderer!(
    render_flat_unpacked,
    DirectTargetTemplate,
    FlatDirectProduct,
    FlatUnpackagedOperation
);
define_direct_unpacked_renderer!(
    render_dae_unpacked,
    DirectTargetTemplate,
    DaeDirectProduct,
    DaeUnpackagedOperation
);
define_direct_unpacked_renderer!(
    render_solve_direct_unpacked,
    DirectTargetTemplate,
    SolveDirectProduct,
    SolveDirectUnpackagedOperation
);
define_direct_unpacked_renderer!(
    render_fmi_unpacked,
    DirectTargetTemplate,
    FmiDirectProduct,
    FmiUnpackagedOperation
);

fn validate_completed_unpacked_paths(plan: &[CompletedUnpackagedMember]) -> Result<()> {
    let mut admitted = BTreeMap::new();
    for (position, member) in plan.iter().enumerate() {
        let components = super::super::validate_portable_member_path(&member.path)
            .with_context(|| format!("rendered target path '{}' is not portable", member.path))?;
        let normalized = components.join("/");
        if normalized != member.path {
            bail!(
                "rendered target path '{}' is not in canonical portable form",
                member.path
            );
        }
        super::super::admit_package_path(&mut admitted, Path::new(&member.path), position)?;
    }
    Ok(())
}

fn lower_fmi_component(model: &rumoca_ir_dae::Dae) -> Result<rumoca_ir_solve::fmi::FmiComponent> {
    let seeds = host_driven_input_seeds(model)?;
    let lowered = rumoca_phase_solve::lower_solve_model(model, &seeds, |_| {})?;
    rumoca_phase_solve::fmi::finish_fmi_component(lowered).map_err(Into::into)
}

fn host_driven_input_seeds(model: &rumoca_ir_dae::Dae) -> Result<HashMap<String, f64>> {
    model.inspect(|view| {
        let mut seeds = HashMap::new();
        for (_, variable) in view
            .variables()
            .filter(|(_, variable)| variable.role() == rumoca_ir_dae::VariableRole::Input)
        {
            seed_host_driven_input(view, variable, &mut seeds)?;
        }
        Ok(seeds)
    })
}

fn seed_host_driven_input<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    variable: rumoca_ir_dae::VariableView<'dae>,
    seeds: &mut HashMap<String, f64>,
) -> Result<()> {
    if variable.binding().is_some() || variable.start().is_none() {
        return Ok(());
    }
    let values = rumoca_phase_solve::host_driven_input_start_values(view, variable)?
        .expect("checked start-bearing input yields a value vector");
    for scalar in 0..variable.scalar_count() {
        let name = variable.scalar_name(scalar).ok_or_else(|| {
            anyhow::anyhow!(
                "checked input '{}' has no scalar name at ordinal {scalar}",
                variable.name()
            )
        })?;
        let value = match values.as_slice() {
            [single] => *single,
            many => *many.get(scalar).ok_or_else(|| {
                anyhow::anyhow!(
                    "start value for input '{}' contains {} scalars; expected {}",
                    variable.name(),
                    many.len(),
                    variable.scalar_count()
                )
            })?,
        };
        seeds.insert(name, value);
    }
    Ok(())
}
