//! Provenance-aware failures raised by checked-DAE templates.

use minijinja::value::ViaDeserialize;
use minijinja::{Environment, Value};
use rumoca_ir_dae as dae;
use serde::Deserialize;

use crate::errors::CodegenError;

use super::RenderResult;

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct TemplateProvenance {
    #[serde(rename = "origin")]
    _origin: dae::DaeProvenanceOrigin,
    span: rumoca_core::Span,
}

#[derive(Debug)]
struct TemplateSemanticError {
    message: String,
    span: rumoca_core::Span,
}

impl std::fmt::Display for TemplateSemanticError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl std::error::Error for TemplateSemanticError {}

pub(super) fn render_error(dae_model: &dae::Dae, error: minijinja::Error) -> CodegenError {
    let Some(semantic_error) = semantic_error(&error) else {
        return error.into();
    };
    let span = semantic_error.span;
    let Some((source_name, source)) = dae_model.source_map().get_source(span.source) else {
        return error.into();
    };
    if span.start.0 > span.end.0 || span.end.0 > source.len() {
        return error.into();
    }
    CodegenError::template_render_at(semantic_error.message.clone(), source_name, source, span)
}

fn semantic_error(error: &minijinja::Error) -> Option<&TemplateSemanticError> {
    let mut source = std::error::Error::source(error);
    while let Some(current) = source {
        if let Some(semantic_error) = current.downcast_ref::<TemplateSemanticError>() {
            return Some(semantic_error);
        }
        source = current.source();
    }
    None
}

/// Fail template rendering at an exact checked-IR provenance span.
///
/// This preserves the target-neutral capability message while diagnostics
/// resolve the provenance against the DAE's canonical source map.
fn fail_at_function(
    message: Value,
    ViaDeserialize(provenance): ViaDeserialize<TemplateProvenance>,
) -> RenderResult {
    let message = template_message(message);
    Err(
        minijinja::Error::new(minijinja::ErrorKind::InvalidOperation, message.clone()).with_source(
            TemplateSemanticError {
                message,
                span: provenance.span,
            },
        ),
    )
}

pub(super) fn register(environment: &mut Environment<'static>) {
    environment.add_function("fail_at", fail_at_function);
}

pub(super) fn template_message(message: Value) -> String {
    message
        .as_str()
        .map(str::to_owned)
        .unwrap_or_else(|| message.to_string())
}
