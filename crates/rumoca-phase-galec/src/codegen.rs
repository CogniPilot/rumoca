use core::result::Result;

use serde::Serialize;

use crate::ir::{GalecAlgorithmPackage, GalecModel};

#[derive(Debug, Clone, Serialize)]
pub struct GalecTemplateContext {
    pub package: GalecAlgorithmPackage,
}

pub fn template_context(model: &GalecModel) -> GalecTemplateContext {
    GalecTemplateContext {
        package: model.algorithm_package(),
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GalecCodegenError {
    #[error("GALEC rendering failed: {0}")]
    Render(String),
}

pub fn render_galec(_model: &GalecModel) -> Result<String, GalecCodegenError> {
    Err(GalecCodegenError::Render(
        "GALEC text rendering is not implemented; use template_context for Jinja rendering"
            .to_string(),
    ))
}
