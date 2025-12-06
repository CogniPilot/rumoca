use thiserror::Error;

/// Errors that can occur during IR (Intermediate Representation) operations
#[derive(Error, Debug)]
pub enum IrError {
    #[error("Connection equations should be expanded during flattening")]
    UnexpandedConnectionEquation,

    #[error("Invalid reinit function call: expected 2 arguments, got {0}")]
    InvalidReinitArgCount(usize),

    #[error("First argument of reinit must be a component reference, got expression of type: {0}")]
    InvalidReinitFirstArg(String),

    #[error("Main class not found in stored definition")]
    MainClassNotFound,

    #[error("Model name is required. Use --model <CLASS_NAME> to specify which class to simulate.")]
    ModelNameRequired,

    #[error("Class '{0}' not found for extend clause")]
    ExtendClassNotFound(String),

    #[error("Component class '{0}' not found")]
    ComponentClassNotFound(String),

    #[error("Import failed: class '{0}' not found. Did you forget to include the library?")]
    ImportClassNotFound(String),

    #[error("Unsupported feature: {feature}")]
    UnsupportedFeature { feature: String },

    #[error("Invalid der() function call: {0}")]
    InvalidDerCall(String),
}
