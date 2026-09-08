//! Sealed preparation boundary for Solve-owned Production Code input.

use std::fmt;

use rumoca_core::TargetInvocationBrand;
use rumoca_ir_galec::package::{
    AlgorithmCodeDeclarationClass, AlgorithmCodePackage, AlgorithmCodeRealFormat,
};
use rumoca_ir_solve::{
    SolveAlgorithmBlock, SolveAlgorithmDeclaration, SolveAlgorithmErrorEffects,
    SolveAlgorithmMethod, SolveAlgorithmMethodKind, SolveAlgorithmProduct,
    SolveDeclarationInitialization, SolveDeclarationStartValue, SolveOperation,
    SolveProgramOperationRun, SolveScalarType, SolveStorageClass, SolveValue, SolveValueKind,
    SolveValueType,
};
use serde::Serialize;

mod artifact_layout;
mod presentation;
#[cfg(test)]
mod tests;

pub use artifact_layout::{
    ProductionArtifactLayout, ProductionArtifactLayoutError, ProductionArtifactLayoutMember,
    ProductionArtifactLayoutSpec, ProductionArtifactRepresentationFile, ProductionArtifactRole,
    ProductionHeaderInclude, ProductionPortableMemberPath, ProductionRepresentationFileName,
    ProductionRepresentationFilePath, ProductionRepresentationName,
};
pub use presentation::{
    ProductionClockPresentation, ProductionDeclarationPresentation, ProductionLifecycleStorageAbi,
    ProductionLogicalDataAnchorConvention, ProductionLogicalDataPresentation,
    ProductionManifestName, ProductionManifestPresentation, ProductionMethodPresentation,
    ProductionPresentationIdentifier, ProductionPresentationPlan, ProductionRegisterPresentation,
    ProductionSuccessStatusPresentation,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveAlgorithmProductionRequirement {
    DeclarationInitialization,
    LifecycleActionGraph,
    ProgramStorageBinding,
    CallOwnerAbiAndWorkingMemory,
    ErrorSignalEffects,
    ProductionMethodAbi,
    TypedOperationEmissionPlan,
    ProductionPresentationPlan,
}

impl SolveAlgorithmProductionRequirement {
    #[must_use]
    pub const fn id(self) -> &'static str {
        match self {
            Self::DeclarationInitialization => "declaration-initialization",
            Self::LifecycleActionGraph => "lifecycle-action-graph",
            Self::ProgramStorageBinding => "program-storage-binding",
            Self::CallOwnerAbiAndWorkingMemory => "call-owner-abi-and-working-memory",
            Self::ErrorSignalEffects => "error-signal-effects",
            Self::ProductionMethodAbi => "production-method-abi",
            Self::TypedOperationEmissionPlan => "typed-operation-emission-plan",
            Self::ProductionPresentationPlan => "production-presentation-plan",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveAlgorithmProductionPreparationError {
    requirement: SolveAlgorithmProductionRequirement,
    method: Option<SolveAlgorithmMethodKind>,
    operation: Option<u32>,
}

impl SolveAlgorithmProductionPreparationError {
    #[must_use]
    pub const fn requirement(&self) -> SolveAlgorithmProductionRequirement {
        self.requirement
    }

    #[must_use]
    pub const fn method(&self) -> Option<SolveAlgorithmMethodKind> {
        self.method
    }

    #[must_use]
    pub const fn operation(&self) -> Option<u32> {
        self.operation
    }
}

impl fmt::Display for SolveAlgorithmProductionPreparationError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "unsupported-feature:solve-algorithm-production:{}",
            self.requirement.id()
        )?;
        if let Some(method) = self.method {
            write!(formatter, " method={method:?}")?;
        }
        if let Some(operation) = self.operation {
            write!(formatter, " operation={operation}")?;
        }
        Ok(())
    }
}

impl std::error::Error for SolveAlgorithmProductionPreparationError {}

/// Checked non-numeric target constraints for the currently implemented
/// freestanding eFMI Production C subset.
///
/// Numeric meaning is absent deliberately: the source package owns the sole
/// complete numeric profile, and preparation derives lexical C ABI facts from
/// that profile. This profile must never acquire a Real, Integer, matrix, or
/// rounding selector.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveAlgorithmProductionProfile {
    maximum_method_automatic_payload_bytes: u32,
    failure_transport: SolveAlgorithmProductionFailureTransport,
    code_container: ProductionCodeContainerProfile,
}

/// Closed C-side transport for Solve typed failures and observable status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveAlgorithmProductionFailureTransport {
    ReturnedStatusI32,
}

/// Closed target language admitted by the initial Production Code profile.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProductionCodeLanguage {
    C,
}

/// Closed language standard admitted by the initial Production Code profile.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProductionCodeLanguageStandard {
    C99,
}

/// Closed platform vocabulary admitted by the initial Production Code profile.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProductionCodePlatform {
    Legacy,
}

/// Construction-sealed CodeContainer profile consumed by Production XML.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct ProductionCodeContainerProfile {
    language: ProductionCodeLanguage,
    standard: ProductionCodeLanguageStandard,
    platform: ProductionCodePlatform,
}

impl ProductionCodeContainerProfile {
    #[must_use]
    pub const fn language(self) -> ProductionCodeLanguage {
        self.language
    }

    #[must_use]
    pub const fn standard(self) -> ProductionCodeLanguageStandard {
        self.standard
    }

    #[must_use]
    pub const fn platform(self) -> ProductionCodePlatform {
        self.platform
    }
}

impl SolveAlgorithmProductionProfile {
    pub fn freestanding_c99_loop_returned_status(
        maximum_method_automatic_payload_bytes: u32,
    ) -> Result<Self, SolveAlgorithmProductionPreparationError> {
        if maximum_method_automatic_payload_bytes == 0 {
            return Err(preparation_error(
                SolveAlgorithmProductionRequirement::CallOwnerAbiAndWorkingMemory,
                None,
                None,
            ));
        }
        Ok(Self {
            maximum_method_automatic_payload_bytes,
            failure_transport: SolveAlgorithmProductionFailureTransport::ReturnedStatusI32,
            code_container: ProductionCodeContainerProfile {
                language: ProductionCodeLanguage::C,
                standard: ProductionCodeLanguageStandard::C99,
                platform: ProductionCodePlatform::Legacy,
            },
        })
    }

    #[must_use]
    pub const fn maximum_method_automatic_payload_bytes(self) -> u32 {
        self.maximum_method_automatic_payload_bytes
    }

    #[must_use]
    pub const fn failure_transport(self) -> SolveAlgorithmProductionFailureTransport {
        self.failure_transport
    }

    #[must_use]
    pub const fn code_container(self) -> ProductionCodeContainerProfile {
        self.code_container
    }
}

/// Closed lexical mapping from one package-selected Modelica `Real`
/// specialization to the eFMI Production C ABI.
///
/// Every field is derived together before rendering. Templates receive these
/// final tokens and cannot branch on, infer, or override numeric semantics.
/// This mapping is not a SPEC_0047 §4.31 toolchain receipt: authenticated
/// Production-C correctness additionally needs checked `FLT_EVAL_METHOD == 0`,
/// no excess precision, disabled contraction, the floating-environment/status
/// relation, and trap behavior for the exact selected compiler and flags.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub(crate) struct ProductionRealAbi {
    c_scalar_type: &'static str,
    literal_suffix: &'static str,
    byte_width: u32,
    efmi_datatype: &'static str,
    float_precision: &'static str,
}

impl ProductionRealAbi {
    const fn from_package_format(format: AlgorithmCodeRealFormat) -> Self {
        match format {
            AlgorithmCodeRealFormat::Binary32 => Self {
                c_scalar_type: "float",
                literal_suffix: "F",
                byte_width: 4,
                efmi_datatype: "efmiFloat32",
                float_precision: "32-bit",
            },
            AlgorithmCodeRealFormat::Binary64 => Self {
                c_scalar_type: "double",
                literal_suffix: "",
                byte_width: 8,
                efmi_datatype: "efmiFloat64",
                float_precision: "64-bit",
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PreparedSolveAlgorithmScalarType {
    Real32,
    Real64,
    Integer32,
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(transparent)]
pub struct PreparedSolveAlgorithmRealLiteral(Box<str>);

impl PreparedSolveAlgorithmRealLiteral {
    fn from_f32_bits(bits: u32, abi: ProductionRealAbi) -> Option<Self> {
        let value = f32::from_bits(bits);
        if !value.is_finite() {
            return None;
        }
        let decimal = decimal_with_fraction(value.to_string());
        if decimal.parse::<f32>().ok()?.to_bits() != bits {
            return None;
        }
        Some(Self(
            format!("{decimal}{}", abi.literal_suffix).into_boxed_str(),
        ))
    }

    fn from_f64_bits(bits: u64, abi: ProductionRealAbi) -> Option<Self> {
        let value = f64::from_bits(bits);
        if !value.is_finite() {
            return None;
        }
        let decimal = decimal_with_fraction(value.to_string());
        if decimal.parse::<f64>().ok()?.to_bits() != bits {
            return None;
        }
        Some(Self(
            format!("{decimal}{}", abi.literal_suffix).into_boxed_str(),
        ))
    }

    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PreparedSolveAlgorithmLiteral {
    Real {
        token: PreparedSolveAlgorithmRealLiteral,
    },
    Integer32Minimum,
    Integer32 {
        value: i32,
    },
    Boolean {
        value: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", content = "literal", rename_all = "snake_case")]
pub enum PreparedSolveAlgorithmInitialization {
    External {
        suggested_value: PreparedSolveAlgorithmInitializationValue,
    },
    Internal(PreparedSolveAlgorithmInitializationValue),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum PreparedSolveAlgorithmInitializationValue {
    Scalar(PreparedSolveAlgorithmLiteral),
    UniformTensorFill(PreparedSolveAlgorithmLiteral),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct PreparedSolveAlgorithmDeclaration {
    presentation: ProductionDeclarationPresentation,
    role: PreparedSolveAlgorithmDeclarationRole,
    storage_owner: PreparedSolveAlgorithmStorageOwner,
    scalar_type: PreparedSolveAlgorithmScalarType,
    type_identifier: ProductionPresentationIdentifier,
    dimensions: Box<[PreparedSolveAlgorithmDimension]>,
    initialization: PreparedSolveAlgorithmInitialization,
}

/// One prepared zero-based dimension identity and its exact extent.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PreparedSolveAlgorithmDimension {
    number: u32,
    size: u32,
    initialization_index: ProductionPresentationIdentifier,
}

impl PreparedSolveAlgorithmDimension {
    #[must_use]
    pub const fn number(&self) -> u32 {
        self.number
    }

    #[must_use]
    pub const fn size(&self) -> u32 {
        self.size
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PreparedSolveAlgorithmDeclarationRole {
    Input,
    Output,
    TunableParameter,
    DependentParameter,
    Constant,
    PersistentState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PreparedSolveAlgorithmStorageOwner {
    Input,
    Output,
    TunableParameter,
    CalculatedParameter,
    Constant,
    PersistentState,
}

impl PreparedSolveAlgorithmDeclaration {
    #[must_use]
    pub const fn presentation(&self) -> &ProductionDeclarationPresentation {
        &self.presentation
    }

    #[must_use]
    pub const fn role(&self) -> PreparedSolveAlgorithmDeclarationRole {
        self.role
    }

    #[must_use]
    pub const fn scalar_type(&self) -> PreparedSolveAlgorithmScalarType {
        self.scalar_type
    }

    #[must_use]
    pub const fn initialization(&self) -> &PreparedSolveAlgorithmInitialization {
        &self.initialization
    }

    #[must_use]
    pub fn dimensions(&self) -> &[PreparedSolveAlgorithmDimension] {
        &self.dimensions
    }
}

#[derive(Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PreparedSolveAlgorithmOperation {
    InitializeScalar {
        declaration: ProductionPresentationIdentifier,
        value: PreparedSolveAlgorithmLiteral,
    },
    InitializeUniformTensor {
        declaration: ProductionPresentationIdentifier,
        value: PreparedSolveAlgorithmLiteral,
        dimensions: Box<[PreparedSolveAlgorithmDimension]>,
    },
    Constant {
        destination: ProductionPresentationIdentifier,
        value: PreparedSolveAlgorithmLiteral,
    },
    Store {
        declaration: ProductionPresentationIdentifier,
        source: ProductionPresentationIdentifier,
    },
}

#[derive(Debug, Serialize)]
pub struct PreparedSolveAlgorithmRegister {
    identifier: ProductionPresentationIdentifier,
    scalar_type: PreparedSolveAlgorithmScalarType,
    type_identifier: ProductionPresentationIdentifier,
}

#[derive(Debug, Serialize)]
pub struct PreparedSolveAlgorithmMethod {
    #[serde(skip)]
    kind: SolveAlgorithmMethodKind,
    presentation: ProductionMethodPresentation,
    status: PreparedSolveAlgorithmMethodStatus,
    self_parameter_number: u32,
    self_parameter_type: ProductionPresentationIdentifier,
    return_type: ProductionPresentationIdentifier,
    error_status_component: ProductionPresentationIdentifier,
    registers: Box<[PreparedSolveAlgorithmRegister]>,
    operations: Box<[PreparedSolveAlgorithmOperation]>,
}

/// Fully selected target presentation of one Solve lifecycle method's status
/// result. The initial subset admits no raisable operation, but still obeys
/// the requested returned-status ABI rather than silently emitting `void`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PreparedSolveAlgorithmMethodStatus {
    InfallibleReturnedStatusI32,
}

impl PreparedSolveAlgorithmMethod {
    #[must_use]
    pub const fn presentation(&self) -> &ProductionMethodPresentation {
        &self.presentation
    }

    #[must_use]
    pub const fn kind(&self) -> SolveAlgorithmMethodKind {
        self.kind
    }

    #[must_use]
    pub const fn status(&self) -> PreparedSolveAlgorithmMethodStatus {
        self.status
    }

    #[must_use]
    pub fn registers(&self) -> &[PreparedSolveAlgorithmRegister] {
        &self.registers
    }

    #[must_use]
    pub fn operations(&self) -> &[PreparedSolveAlgorithmOperation] {
        &self.operations
    }
}

/// Sole owning authority for Algorithm Code and Production Code co-emission.
/// Preparation consumes the correlated product and proves the admitted
/// presentation subset exactly once.
#[derive(Debug)]
pub struct PreparedSolveAlgorithmProduction<'inv> {
    product: SolveAlgorithmProduct<'inv>,
    profile: SolveAlgorithmProductionProfile,
    real_abi: ProductionRealAbi,
    presentation: ProductionPresentationPlan,
    declarations: Box<[PreparedSolveAlgorithmDeclaration]>,
    methods: [PreparedSolveAlgorithmMethod; 3],
}

impl<'inv> PreparedSolveAlgorithmProduction<'inv> {
    pub(crate) const fn brand(&self) -> TargetInvocationBrand<'inv> {
        self.product.traced_algorithm_code().brand()
    }

    #[must_use]
    pub const fn algorithm_code(&self) -> &AlgorithmCodePackage {
        self.product.algorithm_code()
    }

    pub(crate) const fn traced_algorithm_code(
        &self,
    ) -> &rumoca_ir_galec::TracedAlgorithmCodeProduct<'inv> {
        self.product.traced_algorithm_code()
    }

    #[must_use]
    pub const fn block(&self) -> &SolveAlgorithmBlock {
        self.product.solve_algorithm_block()
    }

    #[must_use]
    pub const fn profile(&self) -> SolveAlgorithmProductionProfile {
        self.profile
    }

    #[must_use]
    pub const fn code_container(&self) -> ProductionCodeContainerProfile {
        self.profile.code_container()
    }

    #[must_use]
    pub(crate) const fn real_abi(&self) -> ProductionRealAbi {
        self.real_abi
    }

    #[must_use]
    pub const fn presentation(&self) -> &ProductionPresentationPlan {
        &self.presentation
    }

    #[must_use]
    pub fn declarations(&self) -> &[PreparedSolveAlgorithmDeclaration] {
        &self.declarations
    }

    #[must_use]
    pub fn method(&self, kind: SolveAlgorithmMethodKind) -> &PreparedSolveAlgorithmMethod {
        &self.methods[method_index(kind)]
    }

    #[must_use]
    pub const fn methods(&self) -> &[PreparedSolveAlgorithmMethod; 3] {
        &self.methods
    }
}

pub fn prepare_solve_algorithm_production<'inv>(
    product: SolveAlgorithmProduct<'inv>,
    profile: SolveAlgorithmProductionProfile,
    artifact_layout: ProductionArtifactLayout,
) -> Result<PreparedSolveAlgorithmProduction<'inv>, SolveAlgorithmProductionPreparationError> {
    let block = product.solve_algorithm_block();
    let real_abi = ProductionRealAbi::from_package_format(
        product.algorithm_code().arithmetic_profile().source_real(),
    );
    if !block.call_transfers().entries().is_empty() {
        return Err(preparation_error(
            SolveAlgorithmProductionRequirement::CallOwnerAbiAndWorkingMemory,
            None,
            None,
        ));
    }
    let presentation = prepare_presentation(&product, block, artifact_layout)?;
    let declarations = block
        .declarations()
        .iter()
        .enumerate()
        .map(|(index, declaration)| {
            prepare_declaration(
                declaration,
                &presentation.declarations()[index],
                &presentation,
                real_abi,
            )
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_boxed_slice();
    let methods = prepare_methods(block, &presentation, profile, real_abi, &declarations)?;
    Ok(PreparedSolveAlgorithmProduction {
        product,
        profile,
        real_abi,
        presentation,
        declarations,
        methods,
    })
}

fn prepare_presentation(
    product: &SolveAlgorithmProduct<'_>,
    block: &SolveAlgorithmBlock,
    artifact_layout: ProductionArtifactLayout,
) -> Result<ProductionPresentationPlan, SolveAlgorithmProductionPreparationError> {
    let declaration_facts = block
        .declarations()
        .iter()
        .map(|declaration| {
            declaration
                .block_index()
                .map(rumoca_ir_galec::package::AlgorithmCodeBlockDeclarationIndex::get)
                .map(|block_index| (block_index, declaration.dimensions().len()))
                .ok_or_else(|| {
                    preparation_error(
                        SolveAlgorithmProductionRequirement::ProductionPresentationPlan,
                        None,
                        None,
                    )
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    ProductionPresentationPlan::construct(
        product.traced_algorithm_code().semantic_model(),
        &declaration_facts,
        product.algorithm_code().clock_variable_ordinal(),
        [
            block
                .method(SolveAlgorithmMethodKind::Startup)
                .program()
                .register_types()
                .len(),
            block
                .method(SolveAlgorithmMethodKind::Recalibrate)
                .program()
                .register_types()
                .len(),
            block
                .method(SolveAlgorithmMethodKind::DoStep)
                .program()
                .register_types()
                .len(),
        ],
        [
            block.method(SolveAlgorithmMethodKind::Startup).abi(),
            block.method(SolveAlgorithmMethodKind::Recalibrate).abi(),
            block.method(SolveAlgorithmMethodKind::DoStep).abi(),
        ],
        artifact_layout,
    )
}

fn prepare_declaration(
    declaration: &SolveAlgorithmDeclaration,
    presentation: &ProductionDeclarationPresentation,
    plan: &ProductionPresentationPlan,
    real_abi: ProductionRealAbi,
) -> Result<PreparedSolveAlgorithmDeclaration, SolveAlgorithmProductionPreparationError> {
    let scalar_type = prepare_element_type(declaration.value_type(), None)?;
    let initialization = match declaration.initialization() {
        SolveDeclarationInitialization::External {
            suggested_value, ..
        } => PreparedSolveAlgorithmInitialization::External {
            suggested_value: prepare_initialization_value(suggested_value, None, real_abi)?,
        },
        SolveDeclarationInitialization::Internal { value, .. } => {
            PreparedSolveAlgorithmInitialization::Internal(prepare_initialization_value(
                value, None, real_abi,
            )?)
        }
    };
    Ok(PreparedSolveAlgorithmDeclaration {
        presentation: presentation.clone(),
        role: prepare_declaration_role(declaration.source_class())?,
        storage_owner: prepare_storage_owner(declaration.storage())?,
        scalar_type,
        type_identifier: scalar_type_identifier(plan, scalar_type).clone(),
        dimensions: declaration
            .dimensions()
            .iter()
            .enumerate()
            .map(|(number, dimension)| {
                Ok(PreparedSolveAlgorithmDimension {
                    number: u32::try_from(number).map_err(|_| {
                        preparation_error(
                            SolveAlgorithmProductionRequirement::TypedOperationEmissionPlan,
                            None,
                            None,
                        )
                    })?,
                    size: dimension.extent(),
                    initialization_index: presentation.initialization_indices()[number].clone(),
                })
            })
            .collect::<Result<Vec<_>, SolveAlgorithmProductionPreparationError>>()?
            .into_boxed_slice(),
        initialization,
    })
}

struct MethodPreparationRequest<'a> {
    method: &'a SolveAlgorithmMethod,
    presentation: &'a ProductionMethodPresentation,
    plan: &'a ProductionPresentationPlan,
    profile: SolveAlgorithmProductionProfile,
    real_abi: ProductionRealAbi,
    declarations: &'a [PreparedSolveAlgorithmDeclaration],
}

fn prepare_methods(
    block: &SolveAlgorithmBlock,
    plan: &ProductionPresentationPlan,
    profile: SolveAlgorithmProductionProfile,
    real_abi: ProductionRealAbi,
    declarations: &[PreparedSolveAlgorithmDeclaration],
) -> Result<[PreparedSolveAlgorithmMethod; 3], SolveAlgorithmProductionPreparationError> {
    let prepare = |kind| {
        prepare_method(MethodPreparationRequest {
            method: block.method(kind),
            presentation: plan.method(kind),
            plan,
            profile,
            real_abi,
            declarations,
        })
    };
    Ok([
        prepare(SolveAlgorithmMethodKind::Startup)?,
        prepare(SolveAlgorithmMethodKind::Recalibrate)?,
        prepare(SolveAlgorithmMethodKind::DoStep)?,
    ])
}

fn prepare_method(
    request: MethodPreparationRequest<'_>,
) -> Result<PreparedSolveAlgorithmMethod, SolveAlgorithmProductionPreparationError> {
    match request.method.error_effects() {
        SolveAlgorithmErrorEffects::ResetOnly => {}
    }
    let status = match (
        request.plan.logical_data().lifecycle_abi(),
        request.profile.failure_transport(),
    ) {
        (
            ProductionLifecycleStorageAbi::SharedStorageOwnerParameterFreeInfallible,
            SolveAlgorithmProductionFailureTransport::ReturnedStatusI32,
        ) => PreparedSolveAlgorithmMethodStatus::InfallibleReturnedStatusI32,
    };
    let register_types = request
        .method
        .program()
        .register_types()
        .iter()
        .map(|value_type| prepare_scalar_type(value_type, Some(request.method.kind())))
        .collect::<Result<Vec<_>, _>>()?
        .into_boxed_slice();
    check_method_automatic_payload(&request, &register_types)?;
    let operations = prepare_method_operations(&request)?;
    let registers = prepare_method_registers(&request, &register_types)?;
    Ok(PreparedSolveAlgorithmMethod {
        kind: request.method.kind(),
        presentation: request.presentation.clone(),
        status,
        self_parameter_number: 0,
        self_parameter_type: request.plan.storage_type().clone(),
        return_type: request.plan.status_type().clone(),
        error_status_component: request.plan.error_status_component().clone(),
        registers,
        operations,
    })
}

fn check_method_automatic_payload(
    request: &MethodPreparationRequest<'_>,
    register_types: &[PreparedSolveAlgorithmScalarType],
) -> Result<(), SolveAlgorithmProductionPreparationError> {
    let register_payload_bytes = register_types.iter().try_fold(0u32, |total, scalar| {
        total.checked_add(scalar_payload_bytes(*scalar, request.real_abi))
    });
    let local_payload_bytes =
        method_local_payload_bytes(request.method, request.declarations, request.real_abi)?;
    let automatic_payload_bytes =
        register_payload_bytes.and_then(|registers| registers.checked_add(local_payload_bytes));
    if automatic_payload_bytes
        .is_none_or(|bytes| bytes > request.profile.maximum_method_automatic_payload_bytes())
    {
        return Err(preparation_error(
            SolveAlgorithmProductionRequirement::CallOwnerAbiAndWorkingMemory,
            Some(request.method.kind()),
            None,
        ));
    }
    Ok(())
}

fn prepare_method_operations(
    request: &MethodPreparationRequest<'_>,
) -> Result<Box<[PreparedSolveAlgorithmOperation]>, SolveAlgorithmProductionPreparationError> {
    let mut operations = Vec::new();
    for action in request.method.actions() {
        match action.kind() {
            rumoca_ir_solve::SolveAlgorithmActionKind::StartupInitialize {
                declaration,
                value,
                ..
            } if request.method.kind() == SolveAlgorithmMethodKind::Startup => {
                operations.push(prepare_startup_initialization(
                    request,
                    *declaration,
                    value,
                )?);
            }
            rumoca_ir_solve::SolveAlgorithmActionKind::AssignScalarLiteral {
                program_operations: owned,
                ..
            } => {
                operations.extend(prepare_owned_operations(request, *owned)?);
            }
            rumoca_ir_solve::SolveAlgorithmActionKind::StartupInitialize { .. } => {
                return Err(preparation_error(
                    SolveAlgorithmProductionRequirement::LifecycleActionGraph,
                    Some(request.method.kind()),
                    None,
                ));
            }
        }
    }
    Ok(operations.into_boxed_slice())
}

fn prepare_startup_initialization(
    request: &MethodPreparationRequest<'_>,
    declaration: u32,
    value: &SolveDeclarationStartValue,
) -> Result<PreparedSolveAlgorithmOperation, SolveAlgorithmProductionPreparationError> {
    let declaration = usize::try_from(declaration)
        .ok()
        .and_then(|index| request.declarations.get(index))
        .ok_or_else(|| {
            preparation_error(
                SolveAlgorithmProductionRequirement::LifecycleActionGraph,
                Some(request.method.kind()),
                None,
            )
        })?;
    let declaration_identifier = declaration.presentation().component().clone();
    let value = prepare_initialization_value(value, Some(request.method.kind()), request.real_abi)?;
    Ok(match value {
        PreparedSolveAlgorithmInitializationValue::Scalar(value) => {
            PreparedSolveAlgorithmOperation::InitializeScalar {
                declaration: declaration_identifier,
                value,
            }
        }
        PreparedSolveAlgorithmInitializationValue::UniformTensorFill(value) => {
            PreparedSolveAlgorithmOperation::InitializeUniformTensor {
                declaration: declaration_identifier,
                value,
                dimensions: declaration.dimensions().to_vec().into_boxed_slice(),
            }
        }
    })
}

fn prepare_owned_operations(
    request: &MethodPreparationRequest<'_>,
    owned: SolveProgramOperationRun,
) -> Result<Vec<PreparedSolveAlgorithmOperation>, SolveAlgorithmProductionPreparationError> {
    let first = usize::try_from(owned.first()).map_err(|_| {
        preparation_error(
            SolveAlgorithmProductionRequirement::TypedOperationEmissionPlan,
            Some(request.method.kind()),
            None,
        )
    })?;
    let end = usize::try_from(owned.end()).map_err(|_| {
        preparation_error(
            SolveAlgorithmProductionRequirement::TypedOperationEmissionPlan,
            Some(request.method.kind()),
            None,
        )
    })?;
    let owned_operations = request
        .method
        .program()
        .operations()
        .get(first..end)
        .ok_or_else(|| {
            preparation_error(
                SolveAlgorithmProductionRequirement::TypedOperationEmissionPlan,
                Some(request.method.kind()),
                None,
            )
        })?;
    owned_operations
        .iter()
        .enumerate()
        .map(|(offset, operation)| {
            prepare_operation(
                request.method,
                first + offset,
                operation.operation(),
                request.presentation,
                request.declarations,
                request.real_abi,
            )
        })
        .collect()
}

fn prepare_method_registers(
    request: &MethodPreparationRequest<'_>,
    register_types: &[PreparedSolveAlgorithmScalarType],
) -> Result<Box<[PreparedSolveAlgorithmRegister]>, SolveAlgorithmProductionPreparationError> {
    let registers = register_types
        .iter()
        .copied()
        .enumerate()
        .map(|(index, scalar_type)| {
            let presentation = request.presentation.registers().get(index).ok_or_else(|| {
                preparation_error(
                    SolveAlgorithmProductionRequirement::TypedOperationEmissionPlan,
                    Some(request.method.kind()),
                    None,
                )
            })?;
            Ok(PreparedSolveAlgorithmRegister {
                identifier: presentation.identifier().clone(),
                scalar_type,
                type_identifier: scalar_type_identifier(request.plan, scalar_type).clone(),
            })
        })
        .collect::<Result<Vec<_>, SolveAlgorithmProductionPreparationError>>()?
        .into_boxed_slice();
    Ok(registers)
}

fn prepare_scalar_type(
    value_type: &SolveValueType,
    method: Option<SolveAlgorithmMethodKind>,
) -> Result<PreparedSolveAlgorithmScalarType, SolveAlgorithmProductionPreparationError> {
    if !value_type.dimensions().is_empty() {
        return Err(preparation_error(
            SolveAlgorithmProductionRequirement::TypedOperationEmissionPlan,
            method,
            None,
        ));
    }
    prepare_element_type(value_type, method)
}

fn prepare_element_type(
    value_type: &SolveValueType,
    method: Option<SolveAlgorithmMethodKind>,
) -> Result<PreparedSolveAlgorithmScalarType, SolveAlgorithmProductionPreparationError> {
    match value_type.element_type() {
        SolveScalarType::Real {
            format: rumoca_ir_solve::SolveRealFormat::Binary32,
        } => Ok(PreparedSolveAlgorithmScalarType::Real32),
        SolveScalarType::Real {
            format: rumoca_ir_solve::SolveRealFormat::Binary64,
        } => Ok(PreparedSolveAlgorithmScalarType::Real64),
        SolveScalarType::Integer(domain) if domain == rumoca_ir_solve::SolveIntegerDomain::I32 => {
            Ok(PreparedSolveAlgorithmScalarType::Integer32)
        }
        SolveScalarType::Boolean => Ok(PreparedSolveAlgorithmScalarType::Boolean),
        _ => Err(preparation_error(
            SolveAlgorithmProductionRequirement::TypedOperationEmissionPlan,
            method,
            None,
        )),
    }
}

fn prepare_initialization_value(
    value: &SolveDeclarationStartValue,
    method: Option<SolveAlgorithmMethodKind>,
    real_abi: ProductionRealAbi,
) -> Result<PreparedSolveAlgorithmInitializationValue, SolveAlgorithmProductionPreparationError> {
    match value {
        SolveDeclarationStartValue::Scalar(value) => prepare_literal(value, method, None, real_abi)
            .map(PreparedSolveAlgorithmInitializationValue::Scalar),
        SolveDeclarationStartValue::UniformTensorFill(value) => {
            prepare_literal(value, method, None, real_abi)
                .map(PreparedSolveAlgorithmInitializationValue::UniformTensorFill)
        }
    }
}

fn prepare_literal(
    value: &SolveValue,
    method: Option<SolveAlgorithmMethodKind>,
    operation: Option<u32>,
    real_abi: ProductionRealAbi,
) -> Result<PreparedSolveAlgorithmLiteral, SolveAlgorithmProductionPreparationError> {
    match value.kind() {
        SolveValueKind::Real32(bits) => {
            PreparedSolveAlgorithmRealLiteral::from_f32_bits(bits, real_abi)
                .map(|token| PreparedSolveAlgorithmLiteral::Real { token })
                .ok_or_else(|| {
                    preparation_error(
                        SolveAlgorithmProductionRequirement::TypedOperationEmissionPlan,
                        method,
                        operation,
                    )
                })
        }
        SolveValueKind::Integer(value) => match i32::try_from(value) {
            Ok(i32::MIN) => Ok(PreparedSolveAlgorithmLiteral::Integer32Minimum),
            Ok(value) => Ok(PreparedSolveAlgorithmLiteral::Integer32 { value }),
            Err(_) => Err(preparation_error(
                SolveAlgorithmProductionRequirement::TypedOperationEmissionPlan,
                method,
                operation,
            )),
        },
        SolveValueKind::Boolean(value) => Ok(PreparedSolveAlgorithmLiteral::Boolean { value }),
        SolveValueKind::Real64(bits) => {
            PreparedSolveAlgorithmRealLiteral::from_f64_bits(bits, real_abi)
                .map(|token| PreparedSolveAlgorithmLiteral::Real { token })
                .ok_or_else(|| {
                    preparation_error(
                        SolveAlgorithmProductionRequirement::TypedOperationEmissionPlan,
                        method,
                        operation,
                    )
                })
        }
    }
}

fn prepare_operation(
    method: &SolveAlgorithmMethod,
    index: usize,
    operation: &SolveOperation,
    presentation: &ProductionMethodPresentation,
    declarations: &[PreparedSolveAlgorithmDeclaration],
    real_abi: ProductionRealAbi,
) -> Result<PreparedSolveAlgorithmOperation, SolveAlgorithmProductionPreparationError> {
    let prepared = match operation {
        SolveOperation::Constant { destination, value } => {
            let destination = presentation
                .registers()
                .get(destination.index())
                .ok_or_else(|| operation_error(method.kind(), index))?;
            PreparedSolveAlgorithmOperation::Constant {
                destination: destination.identifier().clone(),
                value: prepare_literal(
                    value,
                    Some(method.kind()),
                    operation_index(index),
                    real_abi,
                )?,
            }
        }
        SolveOperation::Store { slot, source } => {
            let declaration = storage_declaration(method, *slot, index)?;
            let declaration = usize::try_from(declaration)
                .ok()
                .and_then(|declaration| declarations.get(declaration))
                .ok_or_else(|| operation_error(method.kind(), index))?;
            let source = presentation
                .registers()
                .get(source.index())
                .ok_or_else(|| operation_error(method.kind(), index))?;
            PreparedSolveAlgorithmOperation::Store {
                declaration: declaration.presentation().component().clone(),
                source: source.identifier().clone(),
            }
        }
        SolveOperation::Convert { .. }
        | SolveOperation::Load { .. }
        | SolveOperation::Unary { .. }
        | SolveOperation::Binary { .. }
        | SolveOperation::Compare { .. }
        | SolveOperation::Select { .. }
        | SolveOperation::Conditional { .. }
        | SolveOperation::Map { .. }
        | SolveOperation::Fold { .. }
        | SolveOperation::Scale { .. }
        | SolveOperation::BroadcastBinary { .. }
        | SolveOperation::Transpose { .. }
        | SolveOperation::MatrixMultiply { .. }
        | SolveOperation::Cross { .. }
        | SolveOperation::Reduce { .. }
        | SolveOperation::Identity { .. }
        | SolveOperation::Diagonal { .. }
        | SolveOperation::Concatenate { .. }
        | SolveOperation::Fill { .. }
        | SolveOperation::ConstructAggregate { .. }
        | SolveOperation::ProjectElement { .. }
        | SolveOperation::ProjectElementDynamic { .. }
        | SolveOperation::ProjectSlice { .. }
        | SolveOperation::ProjectView { .. }
        | SolveOperation::SelectElement { .. }
        | SolveOperation::UpdateElement { .. }
        | SolveOperation::UpdateSlice { .. }
        | SolveOperation::UpdateView { .. }
        | SolveOperation::Call { .. } => return Err(operation_error(method.kind(), index)),
    };
    Ok(prepared)
}

fn prepare_declaration_role(
    class: AlgorithmCodeDeclarationClass,
) -> Result<PreparedSolveAlgorithmDeclarationRole, SolveAlgorithmProductionPreparationError> {
    match class {
        AlgorithmCodeDeclarationClass::Input => Ok(PreparedSolveAlgorithmDeclarationRole::Input),
        AlgorithmCodeDeclarationClass::Output => Ok(PreparedSolveAlgorithmDeclarationRole::Output),
        AlgorithmCodeDeclarationClass::TunableParameter => {
            Ok(PreparedSolveAlgorithmDeclarationRole::TunableParameter)
        }
        AlgorithmCodeDeclarationClass::DependentParameter => {
            Ok(PreparedSolveAlgorithmDeclarationRole::DependentParameter)
        }
        AlgorithmCodeDeclarationClass::Constant => {
            Ok(PreparedSolveAlgorithmDeclarationRole::Constant)
        }
        AlgorithmCodeDeclarationClass::PersistentState => {
            Ok(PreparedSolveAlgorithmDeclarationRole::PersistentState)
        }
        AlgorithmCodeDeclarationClass::CompartmentDependentParameter
        | AlgorithmCodeDeclarationClass::CompartmentConstant
        | AlgorithmCodeDeclarationClass::CompartmentPersistentState
        | AlgorithmCodeDeclarationClass::MethodLocal
        | AlgorithmCodeDeclarationClass::FunctionInput
        | AlgorithmCodeDeclarationClass::FunctionOutput
        | AlgorithmCodeDeclarationClass::FunctionLocal => Err(preparation_error(
            SolveAlgorithmProductionRequirement::ProductionPresentationPlan,
            None,
            None,
        )),
    }
}

fn prepare_storage_owner(
    storage: SolveStorageClass,
) -> Result<PreparedSolveAlgorithmStorageOwner, SolveAlgorithmProductionPreparationError> {
    match storage {
        SolveStorageClass::Input => Ok(PreparedSolveAlgorithmStorageOwner::Input),
        SolveStorageClass::Output => Ok(PreparedSolveAlgorithmStorageOwner::Output),
        SolveStorageClass::TunableParameter => {
            Ok(PreparedSolveAlgorithmStorageOwner::TunableParameter)
        }
        SolveStorageClass::CalculatedParameter => {
            Ok(PreparedSolveAlgorithmStorageOwner::CalculatedParameter)
        }
        SolveStorageClass::Constant => Ok(PreparedSolveAlgorithmStorageOwner::Constant),
        SolveStorageClass::PersistentState => {
            Ok(PreparedSolveAlgorithmStorageOwner::PersistentState)
        }
        SolveStorageClass::PreviousState
        | SolveStorageClass::MethodLocal
        | SolveStorageClass::SignalStatus => Err(preparation_error(
            SolveAlgorithmProductionRequirement::ProductionPresentationPlan,
            None,
            None,
        )),
    }
}

const fn scalar_type_identifier(
    plan: &ProductionPresentationPlan,
    scalar_type: PreparedSolveAlgorithmScalarType,
) -> &ProductionPresentationIdentifier {
    match scalar_type {
        PreparedSolveAlgorithmScalarType::Real32 | PreparedSolveAlgorithmScalarType::Real64 => {
            plan.real_type()
        }
        PreparedSolveAlgorithmScalarType::Integer32 => plan.integer32_type(),
        PreparedSolveAlgorithmScalarType::Boolean => plan.boolean_type(),
    }
}

fn storage_declaration(
    method: &SolveAlgorithmMethod,
    slot: rumoca_ir_solve::SolveSlotId,
    index: usize,
) -> Result<u32, SolveAlgorithmProductionPreparationError> {
    method
        .storage_bindings()
        .iter()
        .copied()
        .find(|binding| binding.slot() == slot)
        .map(rumoca_ir_solve::SolveProgramStorageBinding::declaration)
        .ok_or_else(|| {
            preparation_error(
                SolveAlgorithmProductionRequirement::ProgramStorageBinding,
                Some(method.kind()),
                operation_index(index),
            )
        })
}

fn method_local_payload_bytes(
    method: &SolveAlgorithmMethod,
    declarations: &[PreparedSolveAlgorithmDeclaration],
    real_abi: ProductionRealAbi,
) -> Result<u32, SolveAlgorithmProductionPreparationError> {
    method
        .program()
        .slots()
        .iter()
        .enumerate()
        .filter(|(_, slot)| slot.storage() == SolveStorageClass::MethodLocal)
        .try_fold(0u32, |total, (index, _)| {
            let binding = method.storage_bindings().get(index).ok_or_else(|| {
                preparation_error(
                    SolveAlgorithmProductionRequirement::ProgramStorageBinding,
                    Some(method.kind()),
                    None,
                )
            })?;
            let declaration = usize::try_from(binding.declaration())
                .ok()
                .and_then(|index| declarations.get(index))
                .ok_or_else(|| {
                    preparation_error(
                        SolveAlgorithmProductionRequirement::ProgramStorageBinding,
                        Some(method.kind()),
                        None,
                    )
                })?;
            total
                .checked_add(scalar_payload_bytes(declaration.scalar_type(), real_abi))
                .ok_or_else(|| {
                    preparation_error(
                        SolveAlgorithmProductionRequirement::CallOwnerAbiAndWorkingMemory,
                        Some(method.kind()),
                        None,
                    )
                })
        })
}

const fn scalar_payload_bytes(
    scalar: PreparedSolveAlgorithmScalarType,
    real_abi: ProductionRealAbi,
) -> u32 {
    match scalar {
        PreparedSolveAlgorithmScalarType::Real32 | PreparedSolveAlgorithmScalarType::Real64 => {
            real_abi.byte_width
        }
        PreparedSolveAlgorithmScalarType::Integer32 => 4,
        PreparedSolveAlgorithmScalarType::Boolean => 1,
    }
}

fn decimal_with_fraction(mut decimal: String) -> String {
    if !decimal.contains('.') && !decimal.contains('e') && !decimal.contains('E') {
        decimal.push_str(".0");
    }
    decimal
}

const fn operation_error(
    method: SolveAlgorithmMethodKind,
    index: usize,
) -> SolveAlgorithmProductionPreparationError {
    preparation_error(
        SolveAlgorithmProductionRequirement::TypedOperationEmissionPlan,
        Some(method),
        operation_index(index),
    )
}

const fn operation_index(index: usize) -> Option<u32> {
    if index <= u32::MAX as usize {
        Some(index as u32)
    } else {
        None
    }
}

const fn preparation_error(
    requirement: SolveAlgorithmProductionRequirement,
    method: Option<SolveAlgorithmMethodKind>,
    operation: Option<u32>,
) -> SolveAlgorithmProductionPreparationError {
    SolveAlgorithmProductionPreparationError {
        requirement,
        method,
        operation,
    }
}

const fn method_index(kind: SolveAlgorithmMethodKind) -> usize {
    match kind {
        SolveAlgorithmMethodKind::Startup => 0,
        SolveAlgorithmMethodKind::Recalibrate => 1,
        SolveAlgorithmMethodKind::DoStep => 2,
    }
}
