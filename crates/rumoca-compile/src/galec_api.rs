// Minimal GALEC render facade shared by the LSP and WASM addon.
//
// The CLI eFMU container path (packaging identity, manifest checksums) stays
// in the rumoca crate; this module only provides identity-free source text.

use rumoca_ir_dae as dae;
use thiserror::Error;

/// The three GALEC codegen target names (all `ir = "dae"`).
pub const GALEC_TARGET: &str = "galec";
pub const GALEC_PRODUCTION_TARGET: &str = "galec-production";
pub const EMBEDDED_C_GALEC_TARGET: &str = "embedded-c-galec";

/// Whether `target` is one of the GALEC codegen targets.
#[must_use]
pub fn is_galec_target(target: &str) -> bool {
    matches!(
        target,
        GALEC_TARGET | GALEC_PRODUCTION_TARGET | EMBEDDED_C_GALEC_TARGET
    )
}

/// Rendered GALEC sources (`.alg` + optional `.h`/`.c` for the C tracks).
/// `c_header` and `c_source` are empty for the Algorithm-Code-only `galec` target.
#[derive(Debug, Clone)]
pub struct GalecSources {
    pub alg: String,
    pub c_header: String,
    pub c_source: String,
}

/// Errors from [`render_galec_sources`].
#[derive(Debug, Error)]
pub enum GalecRenderError {
    #[error("'{0}' is not a GALEC codegen target")]
    UnknownTarget(String),
    #[error("GALEC analysis failed: {0}")]
    Analysis(String),
    #[error("GALEC transformation failed: {0}")]
    Transform(String),
    #[error("GALEC render failed: {0}")]
    Render(String),
}

/// Project a compiled DAE to identity-free GALEC source text.
///
/// Folds hidden component outputs before analysis (same as the CLI path).
pub fn render_galec_sources(
    dae: &dae::Dae,
    model_name: &str,
    target: &str,
) -> Result<GalecSources, GalecRenderError> {
    if !is_galec_target(target) {
        return Err(GalecRenderError::UnknownTarget(target.to_string()));
    }
    let mut folded = dae.clone();
    rumoca_phase_dae::fold_hidden_component_outputs_for_projection(&mut folded);

    let analysis = rumoca_galec::analysis::analyze(&folded)
        .map_err(|errs| GalecRenderError::Analysis(format!("{errs:?}")))?;

    let galec = rumoca_galec::transformation::transform(
        rumoca_galec::transformation::TransformationInput {
            dae: &folded,
            analysis,
            model_name: model_name.to_string(),
        },
    )
    .map_err(|e| GalecRenderError::Transform(e.to_string()))?;

    let alg = rumoca_galec::render::render(&galec)
        .map_err(|e| GalecRenderError::Render(e.to_string()))?;

    let (c_header, c_source) = if target == GALEC_TARGET {
        (String::new(), String::new())
    } else {
        let h = rumoca_galec::render::render_c_header(&galec)
            .map_err(|e| GalecRenderError::Render(e.to_string()))?;
        let c = rumoca_galec::render::render_c_source(&galec)
            .map_err(|e| GalecRenderError::Render(e.to_string()))?;
        (h, c)
    };

    Ok(GalecSources {
        alg,
        c_header,
        c_source,
    })
}
