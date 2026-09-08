//! Target-neutral read-only template views over proven-valid IR.

pub(crate) mod algorithm_code;
mod algorithm_code_artifact_layout;
mod solve_algorithm_production;
mod source_trace;

pub use algorithm_code_artifact_layout::{
    AlgorithmCodeArtifactLayout, AlgorithmCodeArtifactLayoutError,
    AlgorithmCodeArtifactLayoutMember, AlgorithmCodeArtifactLayoutSpec, AlgorithmCodeArtifactRole,
    AlgorithmCodePortableMemberPath, AlgorithmCodeRepresentationFile, PreparedAlgorithmCodePackage,
    prepare_algorithm_code_package,
};

pub use solve_algorithm_production::{
    PreparedSolveAlgorithmDeclaration, PreparedSolveAlgorithmDeclarationRole,
    PreparedSolveAlgorithmDimension, PreparedSolveAlgorithmInitialization,
    PreparedSolveAlgorithmInitializationValue, PreparedSolveAlgorithmLiteral,
    PreparedSolveAlgorithmMethod, PreparedSolveAlgorithmMethodStatus,
    PreparedSolveAlgorithmOperation, PreparedSolveAlgorithmProduction,
    PreparedSolveAlgorithmRealLiteral, PreparedSolveAlgorithmRegister,
    PreparedSolveAlgorithmScalarType, PreparedSolveAlgorithmStorageOwner, ProductionArtifactLayout,
    ProductionArtifactLayoutError, ProductionArtifactLayoutMember, ProductionArtifactLayoutSpec,
    ProductionArtifactRepresentationFile, ProductionArtifactRole, ProductionClockPresentation,
    ProductionCodeContainerProfile, ProductionCodeLanguage, ProductionCodeLanguageStandard,
    ProductionCodePlatform, ProductionDeclarationPresentation, ProductionHeaderInclude,
    ProductionLifecycleStorageAbi, ProductionLogicalDataAnchorConvention,
    ProductionLogicalDataPresentation, ProductionManifestName, ProductionManifestPresentation,
    ProductionMethodPresentation, ProductionPortableMemberPath, ProductionPresentationIdentifier,
    ProductionPresentationPlan, ProductionRegisterPresentation, ProductionRepresentationFileName,
    ProductionRepresentationFilePath, ProductionRepresentationName,
    ProductionSuccessStatusPresentation, SolveAlgorithmProductionFailureTransport,
    SolveAlgorithmProductionPreparationError, SolveAlgorithmProductionProfile,
    SolveAlgorithmProductionRequirement, prepare_solve_algorithm_production,
};

pub(crate) use solve_algorithm_production::ProductionRealAbi;
