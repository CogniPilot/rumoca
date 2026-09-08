//! Construction-only identifier assignment for the correlated H/C/PC family.

use std::collections::BTreeSet;

use rumoca_ir_galec::AlgorithmCodeSemanticModelIdentity;
use rumoca_ir_solve::{SolveAlgorithmMethodAbi, SolveAlgorithmMethodKind};
use serde::Serialize;

#[cfg(test)]
use super::ProductionArtifactRole;
use super::{
    ProductionArtifactLayout, SolveAlgorithmProductionPreparationError,
    SolveAlgorithmProductionRequirement, preparation_error,
};

/// One identifier admitted to the correlated Production artifact family.
///
/// The field and constructor are private: outside code can serialize or read
/// an assigned identifier, but cannot manufacture an unchecked spelling.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(transparent)]
pub struct ProductionPresentationIdentifier(Box<str>);

impl ProductionPresentationIdentifier {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    fn construct(value: String) -> Result<Self, SolveAlgorithmProductionPreparationError> {
        if !is_identifier(&value) || is_reserved_identifier(&value) || is_c_keyword(&value) {
            return Err(presentation_error());
        }
        Ok(Self(value.into_boxed_str()))
    }
}

/// Exact eFMI manifest name correlated to the checked Algorithm Code block.
///
/// XML escaping remains template-owned; construction rejects empty names and
/// control characters so this value cannot become a markup or file fragment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(transparent)]
pub struct ProductionManifestName(Box<str>);

impl ProductionManifestName {
    fn from_semantic_model(
        semantic_model: &AlgorithmCodeSemanticModelIdentity,
    ) -> Result<Self, SolveAlgorithmProductionPreparationError> {
        let source = semantic_model.as_str();
        if source.is_empty() || source.chars().any(char::is_control) {
            return Err(presentation_error());
        }
        Ok(Self(source.into()))
    }

    #[cfg(test)]
    fn for_test(source: &str) -> Result<Self, SolveAlgorithmProductionPreparationError> {
        if source.is_empty() || source.chars().any(char::is_control) {
            return Err(presentation_error());
        }
        Ok(Self(source.into()))
    }

    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Identifier assignments for one manifest-visible Algorithm Code variable.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ProductionDeclarationPresentation {
    algorithm_code_ordinal: u32,
    algorithm_code_identity: ProductionPresentationIdentifier,
    component: ProductionPresentationIdentifier,
    initialization_indices: Box<[ProductionPresentationIdentifier]>,
}

impl ProductionDeclarationPresentation {
    #[must_use]
    pub const fn algorithm_code_identity(&self) -> &ProductionPresentationIdentifier {
        &self.algorithm_code_identity
    }

    #[must_use]
    pub const fn component(&self) -> &ProductionPresentationIdentifier {
        &self.component
    }

    #[must_use]
    pub fn initialization_indices(&self) -> &[ProductionPresentationIdentifier] {
        &self.initialization_indices
    }
}

/// Identifier assignments for one typed automatic register.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ProductionRegisterPresentation {
    identifier: ProductionPresentationIdentifier,
}

impl ProductionRegisterPresentation {
    #[must_use]
    pub const fn identifier(&self) -> &ProductionPresentationIdentifier {
        &self.identifier
    }
}

/// Identifier assignments for one correlated lifecycle method.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ProductionMethodPresentation {
    #[serde(skip)]
    kind: SolveAlgorithmMethodKind,
    algorithm_code_identity: ProductionPresentationIdentifier,
    function: ProductionPresentationIdentifier,
    self_parameter: ProductionPresentationIdentifier,
    return_parameter: ProductionPresentationIdentifier,
    registers: Box<[ProductionRegisterPresentation]>,
}

/// Exact checked Algorithm Code clock-variable identity and its Production
/// storage component correlation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ProductionClockPresentation {
    algorithm_code_ordinal: u32,
    algorithm_code_identity: ProductionPresentationIdentifier,
    component: ProductionPresentationIdentifier,
}

impl ProductionClockPresentation {
    #[must_use]
    pub const fn algorithm_code_identity(&self) -> &ProductionPresentationIdentifier {
        &self.algorithm_code_identity
    }

    #[must_use]
    pub const fn component(&self) -> &ProductionPresentationIdentifier {
        &self.component
    }
}

/// Construction-retained proof used to select one LogicalData formal without
/// asking XML templates to infer a canonical lifecycle owner.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ProductionLogicalDataPresentation {
    parameter: ProductionPresentationIdentifier,
    storage_type: ProductionPresentationIdentifier,
    lifecycle_self_parameters: [ProductionPresentationIdentifier; 3],
    convention: ProductionLogicalDataAnchorConvention,
    lifecycle_abi: ProductionLifecycleStorageAbi,
}

impl ProductionLogicalDataPresentation {
    #[must_use]
    pub const fn parameter(&self) -> &ProductionPresentationIdentifier {
        &self.parameter
    }

    #[must_use]
    pub const fn storage_type(&self) -> &ProductionPresentationIdentifier {
        &self.storage_type
    }

    #[must_use]
    pub fn lifecycle_self_parameters(&self) -> &[ProductionPresentationIdentifier; 3] {
        &self.lifecycle_self_parameters
    }

    #[must_use]
    pub const fn convention(&self) -> ProductionLogicalDataAnchorConvention {
        self.convention
    }

    #[must_use]
    pub const fn lifecycle_abi(&self) -> ProductionLifecycleStorageAbi {
        self.lifecycle_abi
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProductionLogicalDataAnchorConvention {
    DoStepSelfAfterUniformLifecycleStorageAbi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProductionLifecycleStorageAbi {
    SharedStorageOwnerParameterFreeInfallible,
}

/// XML-local identifiers issued by the same global presentation authority as
/// every AC and C identifier in the correlated artifact family.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ProductionManifestPresentation {
    algorithm_code_reference: ProductionPresentationIdentifier,
    algorithm_code_file: ProductionPresentationIdentifier,
    algorithm_code_clock: ProductionPresentationIdentifier,
    header_file: ProductionPresentationIdentifier,
    source_file: ProductionPresentationIdentifier,
    header_code_file: ProductionPresentationIdentifier,
    source_code_file: ProductionPresentationIdentifier,
    real_target_type: ProductionPresentationIdentifier,
    integer32_target_type: ProductionPresentationIdentifier,
    unsigned_integer32_target_type: ProductionPresentationIdentifier,
    boolean_target_type: ProductionPresentationIdentifier,
}

/// One globally-issued success-status object and its exact signed-i32 value.
/// H, C, and Production Code XML consume this single fact.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ProductionSuccessStatusPresentation {
    identifier: ProductionPresentationIdentifier,
    signed_i32: i32,
}

impl ProductionSuccessStatusPresentation {
    #[must_use]
    pub const fn identifier(&self) -> &ProductionPresentationIdentifier {
        &self.identifier
    }
}

impl ProductionManifestPresentation {
    #[must_use]
    pub const fn algorithm_code_file(&self) -> &ProductionPresentationIdentifier {
        &self.algorithm_code_file
    }

    #[must_use]
    pub const fn algorithm_code_clock(&self) -> &ProductionPresentationIdentifier {
        &self.algorithm_code_clock
    }

    #[must_use]
    pub const fn header_file(&self) -> &ProductionPresentationIdentifier {
        &self.header_file
    }

    #[must_use]
    pub const fn source_file(&self) -> &ProductionPresentationIdentifier {
        &self.source_file
    }
}

impl ProductionMethodPresentation {
    #[must_use]
    pub const fn kind(&self) -> SolveAlgorithmMethodKind {
        self.kind
    }

    #[must_use]
    pub const fn algorithm_code_identity(&self) -> &ProductionPresentationIdentifier {
        &self.algorithm_code_identity
    }

    #[must_use]
    pub const fn function(&self) -> &ProductionPresentationIdentifier {
        &self.function
    }

    #[must_use]
    pub const fn self_parameter(&self) -> &ProductionPresentationIdentifier {
        &self.self_parameter
    }

    #[must_use]
    pub const fn return_parameter(&self) -> &ProductionPresentationIdentifier {
        &self.return_parameter
    }

    #[must_use]
    pub fn registers(&self) -> &[ProductionRegisterPresentation] {
        &self.registers
    }
}

/// Sole naming authority shared by H, C, Algorithm Code metadata, and
/// Production Code XML.
///
/// Text facts use purpose-specific identifier, manifest-name, and portable
/// member-path newtypes. Target syntax, declarations, expressions, statements,
/// and arbitrary fragments cannot enter this plan.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ProductionPresentationPlan {
    model: ProductionPresentationIdentifier,
    manifest_name: ProductionManifestName,
    artifact_layout: ProductionArtifactLayout,
    header_guard: ProductionPresentationIdentifier,
    real_type: ProductionPresentationIdentifier,
    integer32_type: ProductionPresentationIdentifier,
    unsigned_integer32_type: ProductionPresentationIdentifier,
    boolean_type: ProductionPresentationIdentifier,
    status_type: ProductionPresentationIdentifier,
    success_status: ProductionSuccessStatusPresentation,
    storage_type: ProductionPresentationIdentifier,
    error_status_algorithm_code_identity: ProductionPresentationIdentifier,
    error_status_component: ProductionPresentationIdentifier,
    logical_data: ProductionLogicalDataPresentation,
    clock: ProductionClockPresentation,
    manifest: ProductionManifestPresentation,
    declarations: Box<[ProductionDeclarationPresentation]>,
    methods: [ProductionMethodPresentation; 3],
}

struct ProductionGlobalPresentation {
    header_guard: ProductionPresentationIdentifier,
    real_type: ProductionPresentationIdentifier,
    integer32_type: ProductionPresentationIdentifier,
    unsigned_integer32_type: ProductionPresentationIdentifier,
    boolean_type: ProductionPresentationIdentifier,
    status_type: ProductionPresentationIdentifier,
    success_status: ProductionSuccessStatusPresentation,
    storage_type: ProductionPresentationIdentifier,
    error_status_algorithm_code_identity: ProductionPresentationIdentifier,
    error_status_component: ProductionPresentationIdentifier,
}

impl ProductionPresentationPlan {
    pub(super) fn construct(
        semantic_model: &AlgorithmCodeSemanticModelIdentity,
        declaration_facts: &[(u32, usize)],
        clock_variable_ordinal: usize,
        method_register_counts: [usize; 3],
        lifecycle_abis: [SolveAlgorithmMethodAbi; 3],
        artifact_layout: ProductionArtifactLayout,
    ) -> Result<Self, SolveAlgorithmProductionPreparationError> {
        let manifest_name = ProductionManifestName::from_semantic_model(semantic_model)?;
        Self::construct_with_manifest_name(
            manifest_name,
            declaration_facts,
            clock_variable_ordinal,
            method_register_counts,
            lifecycle_abis,
            artifact_layout,
        )
    }

    #[cfg(test)]
    fn construct_for_test(
        source_model_name: &str,
        declaration_facts: &[(u32, usize)],
        clock_variable_ordinal: usize,
        method_register_counts: [usize; 3],
        lifecycle_abis: [SolveAlgorithmMethodAbi; 3],
        artifact_layout: ProductionArtifactLayout,
    ) -> Result<Self, SolveAlgorithmProductionPreparationError> {
        let manifest_name = ProductionManifestName::for_test(source_model_name)?;
        Self::construct_with_manifest_name(
            manifest_name,
            declaration_facts,
            clock_variable_ordinal,
            method_register_counts,
            lifecycle_abis,
            artifact_layout,
        )
    }

    fn construct_with_manifest_name(
        manifest_name: ProductionManifestName,
        declaration_facts: &[(u32, usize)],
        clock_variable_ordinal: usize,
        method_register_counts: [usize; 3],
        lifecycle_abis: [SolveAlgorithmMethodAbi; 3],
        artifact_layout: ProductionArtifactLayout,
    ) -> Result<Self, SolveAlgorithmProductionPreparationError> {
        let model = fixed_identifier("rumoca_model")?;
        let model_name = model.as_str();
        let mut definitions = BTreeSet::new();
        admit_definition(&mut definitions, &model)?;

        let global = global_presentation(&mut definitions, model_name)?;
        let manifest = manifest_presentation(&mut definitions)?;
        let declarations =
            declaration_presentations(&mut definitions, model_name, declaration_facts)?;

        let methods = [
            method_presentation(
                &mut definitions,
                model_name,
                SolveAlgorithmMethodKind::Startup,
                method_register_counts[0],
            )?,
            method_presentation(
                &mut definitions,
                model_name,
                SolveAlgorithmMethodKind::Recalibrate,
                method_register_counts[1],
            )?,
            method_presentation(
                &mut definitions,
                model_name,
                SolveAlgorithmMethodKind::DoStep,
                method_register_counts[2],
            )?,
        ];
        let lifecycle_abi = match lifecycle_abis {
            [
                SolveAlgorithmMethodAbi::ParameterFreeInfallible,
                SolveAlgorithmMethodAbi::ParameterFreeInfallible,
                SolveAlgorithmMethodAbi::ParameterFreeInfallible,
            ] => ProductionLifecycleStorageAbi::SharedStorageOwnerParameterFreeInfallible,
        };
        let logical_data = ProductionLogicalDataPresentation {
            parameter: methods[2].self_parameter.clone(),
            storage_type: global.storage_type.clone(),
            lifecycle_self_parameters: methods
                .each_ref()
                .map(|method| method.self_parameter.clone()),
            convention:
                ProductionLogicalDataAnchorConvention::DoStepSelfAfterUniformLifecycleStorageAbi,
            lifecycle_abi,
        };
        let clock_ordinal =
            u32::try_from(clock_variable_ordinal).map_err(|_| presentation_error())?;
        let clock_declaration = declarations
            .iter()
            .find(|declaration| declaration.algorithm_code_ordinal == clock_ordinal)
            .ok_or_else(presentation_error)?;
        let clock = ProductionClockPresentation {
            algorithm_code_ordinal: clock_ordinal,
            algorithm_code_identity: clock_declaration.algorithm_code_identity.clone(),
            component: clock_declaration.component.clone(),
        };
        prove_significant_identifier_distinction(&definitions, &methods)?;

        Ok(Self {
            model,
            manifest_name,
            artifact_layout,
            header_guard: global.header_guard,
            real_type: global.real_type,
            integer32_type: global.integer32_type,
            unsigned_integer32_type: global.unsigned_integer32_type,
            boolean_type: global.boolean_type,
            status_type: global.status_type,
            success_status: global.success_status,
            storage_type: global.storage_type,
            error_status_algorithm_code_identity: global.error_status_algorithm_code_identity,
            error_status_component: global.error_status_component,
            logical_data,
            clock,
            manifest,
            declarations,
            methods,
        })
    }

    #[must_use]
    pub const fn model(&self) -> &ProductionPresentationIdentifier {
        &self.model
    }

    #[must_use]
    pub const fn manifest_name(&self) -> &ProductionManifestName {
        &self.manifest_name
    }

    #[must_use]
    pub const fn artifact_layout(&self) -> &ProductionArtifactLayout {
        &self.artifact_layout
    }

    #[must_use]
    pub const fn real_type(&self) -> &ProductionPresentationIdentifier {
        &self.real_type
    }

    #[must_use]
    pub const fn integer32_type(&self) -> &ProductionPresentationIdentifier {
        &self.integer32_type
    }

    #[must_use]
    pub const fn boolean_type(&self) -> &ProductionPresentationIdentifier {
        &self.boolean_type
    }

    #[must_use]
    pub const fn status_type(&self) -> &ProductionPresentationIdentifier {
        &self.status_type
    }

    #[must_use]
    pub const fn success_status(&self) -> &ProductionSuccessStatusPresentation {
        &self.success_status
    }

    #[must_use]
    pub const fn storage_type(&self) -> &ProductionPresentationIdentifier {
        &self.storage_type
    }

    #[must_use]
    pub const fn error_status_component(&self) -> &ProductionPresentationIdentifier {
        &self.error_status_component
    }

    #[must_use]
    pub const fn logical_data(&self) -> &ProductionLogicalDataPresentation {
        &self.logical_data
    }

    #[must_use]
    pub const fn clock(&self) -> &ProductionClockPresentation {
        &self.clock
    }

    #[must_use]
    pub const fn manifest(&self) -> &ProductionManifestPresentation {
        &self.manifest
    }

    #[must_use]
    pub fn declarations(&self) -> &[ProductionDeclarationPresentation] {
        &self.declarations
    }

    #[must_use]
    pub fn method(&self, kind: SolveAlgorithmMethodKind) -> &ProductionMethodPresentation {
        &self.methods[method_index(kind)]
    }
}

fn global_presentation(
    definitions: &mut BTreeSet<ProductionPresentationIdentifier>,
    model: &str,
) -> Result<ProductionGlobalPresentation, SolveAlgorithmProductionPreparationError> {
    let presentation = ProductionGlobalPresentation {
        header_guard: derived_identifier(model, "production_h")?,
        real_type: derived_identifier(model, "real")?,
        integer32_type: derived_identifier(model, "integer32")?,
        unsigned_integer32_type: derived_identifier(model, "unsigned_integer32")?,
        boolean_type: derived_identifier(model, "boolean")?,
        status_type: derived_identifier(model, "status")?,
        success_status: ProductionSuccessStatusPresentation {
            identifier: derived_identifier(model, "status_ok")?,
            signed_i32: 0,
        },
        storage_type: derived_identifier(model, "storage")?,
        error_status_algorithm_code_identity: fixed_identifier("ESS")?,
        error_status_component: derived_identifier(model, "error_signal_status")?,
    };
    for identifier in [
        &presentation.header_guard,
        &presentation.real_type,
        &presentation.integer32_type,
        &presentation.unsigned_integer32_type,
        &presentation.boolean_type,
        &presentation.status_type,
        &presentation.success_status.identifier,
        &presentation.storage_type,
        &presentation.error_status_algorithm_code_identity,
        &presentation.error_status_component,
    ] {
        admit_definition(definitions, identifier)?;
    }
    Ok(presentation)
}

fn declaration_presentations(
    definitions: &mut BTreeSet<ProductionPresentationIdentifier>,
    model: &str,
    declaration_facts: &[(u32, usize)],
) -> Result<Box<[ProductionDeclarationPresentation]>, SolveAlgorithmProductionPreparationError> {
    declaration_facts
        .iter()
        .copied()
        .map(|(block_index, dimension_count)| {
            let ordinal = block_index.checked_add(1).ok_or_else(presentation_error)?;
            let algorithm_code_identity = indexed_identifier("V", ordinal)?;
            let component = indexed_derived_identifier(model, "declaration", ordinal)?;
            admit_definition(definitions, &algorithm_code_identity)?;
            admit_definition(definitions, &component)?;
            let initialization_indices = (0..dimension_count)
                .map(|axis| {
                    let axis = u32::try_from(axis)
                        .ok()
                        .and_then(|axis| axis.checked_add(1))
                        .ok_or_else(presentation_error)?;
                    let identifier = indexed_derived_identifier(
                        model,
                        &format!("declaration_{ordinal}_index"),
                        axis,
                    )?;
                    admit_definition(definitions, &identifier)?;
                    Ok(identifier)
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_boxed_slice();
            Ok(ProductionDeclarationPresentation {
                algorithm_code_ordinal: ordinal,
                algorithm_code_identity,
                component,
                initialization_indices,
            })
        })
        .collect::<Result<Vec<_>, _>>()
        .map(Vec::into_boxed_slice)
}

fn method_presentation(
    definitions: &mut BTreeSet<ProductionPresentationIdentifier>,
    model: &str,
    kind: SolveAlgorithmMethodKind,
    register_count: usize,
) -> Result<ProductionMethodPresentation, SolveAlgorithmProductionPreparationError> {
    let suffix = method_suffix(kind);
    let algorithm_code_identity = fixed_identifier(match kind {
        SolveAlgorithmMethodKind::Startup => "BM_STARTUP",
        SolveAlgorithmMethodKind::Recalibrate => "BM_RECALIBRATE",
        SolveAlgorithmMethodKind::DoStep => "BM_DOSTEP",
    })?;
    let function = derived_identifier(model, suffix)?;
    let self_parameter = derived_identifier(model, &format!("{suffix}_self"))?;
    let return_parameter = derived_identifier(model, &format!("{suffix}_return"))?;
    for identifier in [
        &algorithm_code_identity,
        &function,
        &self_parameter,
        &return_parameter,
    ] {
        admit_definition(definitions, identifier)?;
    }
    let registers = (0..register_count)
        .map(|index| {
            let ordinal = u32::try_from(index)
                .ok()
                .and_then(|value| value.checked_add(1))
                .ok_or_else(presentation_error)?;
            let identifier =
                indexed_derived_identifier(model, &format!("{suffix}_register"), ordinal)?;
            admit_definition(definitions, &identifier)?;
            Ok(ProductionRegisterPresentation { identifier })
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_boxed_slice();
    Ok(ProductionMethodPresentation {
        kind,
        algorithm_code_identity,
        function,
        self_parameter,
        return_parameter,
        registers,
    })
}

fn manifest_presentation(
    definitions: &mut BTreeSet<ProductionPresentationIdentifier>,
) -> Result<ProductionManifestPresentation, SolveAlgorithmProductionPreparationError> {
    let presentation = ProductionManifestPresentation {
        algorithm_code_reference: fixed_identifier("MR_ALGORITHM_CODE")?,
        algorithm_code_file: fixed_identifier("F_ALGORITHM_CODE")?,
        algorithm_code_clock: fixed_identifier("CLK")?,
        header_file: fixed_identifier("F_PRODUCTION_HEADER")?,
        source_file: fixed_identifier("F_PRODUCTION_SOURCE")?,
        header_code_file: fixed_identifier("CF_PRODUCTION_HEADER")?,
        source_code_file: fixed_identifier("CF_PRODUCTION_SOURCE")?,
        real_target_type: fixed_identifier("TT_REAL")?,
        integer32_target_type: fixed_identifier("TT_INTEGER32")?,
        unsigned_integer32_target_type: fixed_identifier("TT_UNSIGNED_INTEGER32")?,
        boolean_target_type: fixed_identifier("TT_BOOLEAN")?,
    };
    for identifier in [
        &presentation.algorithm_code_reference,
        &presentation.algorithm_code_file,
        &presentation.algorithm_code_clock,
        &presentation.header_file,
        &presentation.source_file,
        &presentation.header_code_file,
        &presentation.source_code_file,
        &presentation.real_target_type,
        &presentation.integer32_target_type,
        &presentation.unsigned_integer32_target_type,
        &presentation.boolean_target_type,
    ] {
        admit_definition(definitions, identifier)?;
    }
    Ok(presentation)
}

fn fixed_identifier(
    value: &str,
) -> Result<ProductionPresentationIdentifier, SolveAlgorithmProductionPreparationError> {
    ProductionPresentationIdentifier::construct(value.to_owned())
}

fn derived_identifier(
    model: &str,
    suffix: &str,
) -> Result<ProductionPresentationIdentifier, SolveAlgorithmProductionPreparationError> {
    ProductionPresentationIdentifier::construct(format!("rumoca_{suffix}_{model}"))
}

fn indexed_identifier(
    prefix: &str,
    ordinal: u32,
) -> Result<ProductionPresentationIdentifier, SolveAlgorithmProductionPreparationError> {
    ProductionPresentationIdentifier::construct(format!("{prefix}{ordinal}"))
}

fn indexed_derived_identifier(
    model: &str,
    suffix: &str,
    ordinal: u32,
) -> Result<ProductionPresentationIdentifier, SolveAlgorithmProductionPreparationError> {
    ProductionPresentationIdentifier::construct(format!("rumoca_{suffix}_{ordinal}_{model}"))
}

fn admit_definition(
    definitions: &mut BTreeSet<ProductionPresentationIdentifier>,
    identifier: &ProductionPresentationIdentifier,
) -> Result<(), SolveAlgorithmProductionPreparationError> {
    if !definitions.insert(identifier.clone()) {
        return Err(presentation_error());
    }
    Ok(())
}

fn prove_significant_identifier_distinction(
    definitions: &BTreeSet<ProductionPresentationIdentifier>,
    methods: &[ProductionMethodPresentation; 3],
) -> Result<(), SolveAlgorithmProductionPreparationError> {
    let mut internal_prefixes = BTreeSet::new();
    for identifier in definitions {
        if identifier.as_str().len() > 63 {
            return Err(presentation_error());
        }
        let prefix = significant_prefix(identifier.as_str(), 63);
        if !internal_prefixes.insert(prefix) {
            return Err(presentation_error());
        }
    }
    let mut external_prefixes = BTreeSet::new();
    for method in methods {
        if method.function.as_str().len() > 31 {
            return Err(presentation_error());
        }
        let prefix = significant_prefix(method.function.as_str(), 31);
        if !external_prefixes.insert(prefix) {
            return Err(presentation_error());
        }
    }
    Ok(())
}

fn significant_prefix(identifier: &str, significant_characters: usize) -> &str {
    &identifier[..identifier.len().min(significant_characters)]
}

fn is_identifier(value: &str) -> bool {
    let mut bytes = value.bytes();
    bytes.next().is_some_and(is_identifier_start) && bytes.all(is_identifier_continue)
}

const fn is_identifier_start(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphabetic()
}

const fn is_identifier_continue(byte: u8) -> bool {
    is_identifier_start(byte) || byte.is_ascii_digit()
}

fn is_reserved_identifier(value: &str) -> bool {
    value.starts_with('_')
}

fn is_c_keyword(value: &str) -> bool {
    matches!(
        value,
        "auto"
            | "break"
            | "case"
            | "char"
            | "const"
            | "continue"
            | "default"
            | "do"
            | "double"
            | "else"
            | "enum"
            | "extern"
            | "float"
            | "for"
            | "goto"
            | "if"
            | "inline"
            | "int"
            | "long"
            | "register"
            | "restrict"
            | "return"
            | "short"
            | "signed"
            | "sizeof"
            | "static"
            | "struct"
            | "switch"
            | "typedef"
            | "union"
            | "unsigned"
            | "void"
            | "volatile"
            | "while"
            | "_Alignas"
            | "_Alignof"
            | "_Atomic"
            | "_Bool"
            | "_Complex"
            | "_Generic"
            | "_Imaginary"
            | "_Noreturn"
            | "_Static_assert"
            | "_Thread_local"
    )
}

const fn method_suffix(kind: SolveAlgorithmMethodKind) -> &'static str {
    match kind {
        SolveAlgorithmMethodKind::Startup => "startup",
        SolveAlgorithmMethodKind::Recalibrate => "recalibrate",
        SolveAlgorithmMethodKind::DoStep => "do_step",
    }
}

const fn method_index(kind: SolveAlgorithmMethodKind) -> usize {
    match kind {
        SolveAlgorithmMethodKind::Startup => 0,
        SolveAlgorithmMethodKind::Recalibrate => 1,
        SolveAlgorithmMethodKind::DoStep => 2,
    }
}

const fn presentation_error() -> SolveAlgorithmProductionPreparationError {
    preparation_error(
        SolveAlgorithmProductionRequirement::ProductionPresentationPlan,
        None,
        None,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn layout() -> ProductionArtifactLayout {
        ProductionArtifactLayout::construct([
            (ProductionArtifactRole::PackageManifest, "__content.xml"),
            (
                ProductionArtifactRole::AlgorithmCodeManifest,
                "AlgorithmCode/manifest.xml",
            ),
            (
                ProductionArtifactRole::AlgorithmCodeSource,
                "AlgorithmCode/model.alg",
            ),
            (
                ProductionArtifactRole::ProductionManifest,
                "ProductionCode/manifest.xml",
            ),
            (
                ProductionArtifactRole::ProductionHeader,
                "ProductionCode/sources/production.h",
            ),
            (
                ProductionArtifactRole::ProductionSource,
                "ProductionCode/sources/production.c",
            ),
            (
                ProductionArtifactRole::Schema,
                "schemas/ProductionCode/schema.xsd",
            ),
        ])
        .expect("the test artifact layout is checked")
    }

    #[test]
    fn unchecked_or_reserved_identifiers_are_refused_at_construction() {
        for value in ["", "1bad", "bad-name", "_Hidden", "__hidden", "while"] {
            assert!(ProductionPresentationIdentifier::construct(value.to_owned()).is_err());
        }
        assert!(ProductionPresentationIdentifier::construct("safe_name9".to_owned()).is_ok());
    }

    #[test]
    fn repeated_derivation_is_byte_identical_and_globally_unique() {
        let abis = [SolveAlgorithmMethodAbi::ParameterFreeInfallible; 3];
        let left = ProductionPresentationPlan::construct_for_test(
            "A.'quoted model'",
            &[(0, 1), (1, 2)],
            1,
            [2, 0, 1],
            abis,
            layout(),
        )
        .expect("the presentation family is constructible");
        let right = ProductionPresentationPlan::construct_for_test(
            "A.'quoted model'",
            &[(0, 1), (1, 2)],
            1,
            [2, 0, 1],
            abis,
            layout(),
        )
        .expect("the same presentation family is constructible");
        assert_eq!(left, right);
        assert_eq!(left.clock().algorithm_code_identity().as_str(), "V1");
        assert_eq!(
            left.logical_data().convention(),
            ProductionLogicalDataAnchorConvention::DoStepSelfAfterUniformLifecycleStorageAbi
        );
        assert_eq!(
            left.logical_data().lifecycle_abi(),
            ProductionLifecycleStorageAbi::SharedStorageOwnerParameterFreeInfallible
        );
        assert_eq!(left.logical_data().lifecycle_self_parameters().len(), 3);

        assert!(
            ProductionPresentationPlan::construct_for_test(
                "collision",
                &[(0, 0), (0, 0)],
                1,
                [0, 0, 0],
                abis,
                layout(),
            )
            .is_err(),
            "two definitions cannot receive one identifier"
        );
    }

    #[test]
    fn source_name_length_cannot_escape_c_identifier_translation_limits() {
        let source_name = "m".repeat(4_096);
        let plan = ProductionPresentationPlan::construct_for_test(
            &source_name,
            &[(0, 0)],
            1,
            [1, 1, 1],
            [SolveAlgorithmMethodAbi::ParameterFreeInfallible; 3],
            layout(),
        )
        .expect("source identity is separate from bounded C identifiers");
        assert_eq!(plan.manifest_name().as_str(), source_name);
        assert_eq!(plan.model().as_str(), "rumoca_model");
        for kind in [
            SolveAlgorithmMethodKind::Startup,
            SolveAlgorithmMethodKind::Recalibrate,
            SolveAlgorithmMethodKind::DoStep,
        ] {
            assert!(plan.method(kind).function().as_str().len() <= 31);
        }
    }
}
