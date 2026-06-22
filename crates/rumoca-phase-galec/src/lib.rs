pub mod admissibility;
pub mod codegen;
pub mod codegen_c;
pub mod ir;
pub mod lower;
pub mod prepare;

pub use ir::{
    GalecAlgorithmPackage, GalecBlock, GalecDecl, GalecDeclRole, GalecExpr, GalecInterface,
    GalecManifest, GalecMethod, GalecMethodKind, GalecModel, GalecSampleTime, GalecStmt, GalecType,
    GalecVariable, GalecVariableRole,
};

pub use admissibility::{
    GalecAdmissibilityError, GalecAdmissibilityReport, GalecAdmissibleDae, GalecProfile,
    check_galec_admissible, check_galec_statements_admissible,
};

pub use codegen::{GalecCodegenError, GalecTemplateContext, render_galec, template_context};
pub use codegen_c::{GalecCContext, GalecCMethods, galec_c_template_context};
pub use lower::{
    GalecLowerError, lower_statement_to_galec, lower_statements_to_galec_block, lower_to_galec,
};
pub use prepare::{GalecPrepareError, prepare_for_galec};
