use std::collections::HashSet;

use serde_json::Value;
use rumoca_ir_dae::Dae;
use rumoca_phase_galec::{
    GalecProfile, check_galec_admissible, codegen_c, lower_to_galec, prepare_for_galec,
    render_galec as render_galec_model,
};

#[derive(Debug, thiserror::Error)]
pub enum GalecPipelineError {
    #[error("model is not GALEC-admissible:\n{}", .violations.join("\n"))]
    Admissibility { violations: Vec<String> },
    #[error("GALEC lowering failed: {0}")]
    Lowering(String),
    #[error("GALEC rendering failed: {0}")]
    Rendering(String),
    #[error("GALEC preparation failed: {0}")]
    Preparation(String),
}

/// Run the DAE -> GALEC admissibility -> GALEC IR -> Algorithm Code pipeline.
pub fn render_galec(dae: &Dae, model_name: &str) -> Result<String, GalecPipelineError> {
    let prepared = prepare_for_galec(dae)
        .map_err(|error| GalecPipelineError::Preparation(error.to_string()))?;
    let admissible = check_galec_admissible(&prepared, GalecProfile::Efmi10).map_err(|error| {
        let mut seen = HashSet::new();
        GalecPipelineError::Admissibility {
            violations: error
                .report
                .violations
                .into_iter()
                .map(|violation| violation.message)
                .filter(|message| seen.insert(message.clone()))
                .map(|message| format!("- {message}"))
                .collect(),
        }
    })?;
    let model = lower_to_galec(admissible, model_name)
        .map_err(|error| GalecPipelineError::Lowering(error.to_string()))?;
    render_galec_model(&model).map_err(|error| GalecPipelineError::Rendering(error.to_string()))
}

/// Run the DAE -> GALEC pipeline and produce a C template context as JSON Value.
pub fn render_galec_c_template_context(
    dae: &Dae,
    model_name: &str,
) -> Result<Value, GalecPipelineError> {
    let prepared = prepare_for_galec(dae)
        .map_err(|error| GalecPipelineError::Preparation(error.to_string()))?;
    let admissible = check_galec_admissible(&prepared, GalecProfile::Efmi10).map_err(|error| {
        let mut seen = HashSet::new();
        GalecPipelineError::Admissibility {
            violations: error
                .report
                .violations
                .into_iter()
                .map(|violation| violation.message)
                .filter(|message| seen.insert(message.clone()))
                .map(|message| format!("- {message}"))
                .collect(),
        }
    })?;
    let galec_model = lower_to_galec(admissible, model_name)
        .map_err(|error| GalecPipelineError::Lowering(error.to_string()))?;
    let ctx = codegen_c::galec_c_template_context(&galec_model);
    serde_json::to_value(ctx).map_err(|e| GalecPipelineError::Preparation(e.to_string()))
}
