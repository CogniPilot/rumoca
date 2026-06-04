pub mod admissibility;
pub mod codegen;
pub mod ir;
pub mod lower;

pub use ir::{
    GalecAlgorithmPackage, GalecBlock, GalecDecl, GalecDeclRole, GalecExpr, GalecInterface,
    GalecManifest, GalecMethod, GalecMethodKind, GalecModel, GalecSampleTime, GalecStmt, GalecType,
    GalecVariable, GalecVariableRole,
};

pub use admissibility::{
    GalecAdmissibilityError, GalecAdmissibilityReport, GalecAdmissibleDae, GalecProfile,
    check_galec_admissible,
};

pub use codegen::{GalecCodegenError, GalecTemplateContext, render_galec, template_context};
pub use lower::{GalecLowerError, lower_to_galec};
