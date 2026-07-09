use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;
use serde_json::Value;

pub use rumoca_phase_codegen::CodegenError;

pub fn dae_to_template_json(dae_model: &dae::Dae) -> Result<Value, CodegenError> {
    rumoca_phase_codegen::dae_template_json(dae_model)
}

pub fn dae_for_solve_template_context(dae_model: &dae::Dae) -> Result<dae::Dae, CodegenError> {
    let mut prepared = dae_model.clone();
    rumoca_phase_structural::scalarize::scalarize_equations(&mut prepared).map_err(|err| {
        CodegenError::template(format!("solve-template DAE scalarization: {err}"))
    })?;
    rumoca_phase_dae::prepare_dae_for_codegen(&prepared)
        .map(|prepared| prepared.into_dae())
        .map_err(|err| CodegenError::template(format!("solve-template DAE preparation: {err}")))
}

pub fn dae_for_fmi_model_description_context(
    dae_model: &dae::Dae,
) -> Result<dae::Dae, CodegenError> {
    let prepared = dae_for_solve_template_context(dae_model)?;
    rumoca_phase_dae::prepare_dae_for_fmi_model_description(&prepared)
        .map(|prepared| prepared.into_dae())
        .map_err(|err| {
            CodegenError::dae_preparation_failed(
                format!("FMI modelDescription DAE preparation: {err}"),
                err.source_span(),
            )
        })
}

pub fn render_dae_template(dae_model: &dae::Dae, template: &str) -> Result<String, CodegenError> {
    rumoca_phase_codegen::render_template(dae_model, template)
}

pub fn render_dae_template_with_json(
    dae_json: &Value,
    template: &str,
) -> Result<String, CodegenError> {
    rumoca_phase_codegen::render_template_with_dae_json(dae_json, template)
}

pub fn render_dae_template_with_json_and_name(
    dae_json: &Value,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    rumoca_phase_codegen::render_template_with_dae_json_and_name(dae_json, template, model_name)
}

pub fn render_dae_template_with_name(
    dae_model: &dae::Dae,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    rumoca_phase_codegen::render_template_with_name(dae_model, template, model_name)
}

pub fn render_flat_template_with_name(
    flat_model: &flat::Model,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    rumoca_phase_codegen::render_flat_template_with_name(flat_model, template, model_name)
}

pub fn render_ast_template_with_name(
    ast_tree: &ast::ClassTree,
    template: &str,
    model_name: &str,
) -> Result<String, CodegenError> {
    rumoca_phase_codegen::render_ast_template_with_name(ast_tree, template, model_name)
}

pub use rumoca_phase_codegen::{SolveTemplateRenderer, render_solve_template_with_name};

/// Built-in target metadata, re-exported from the codegen crate.
pub mod templates {
    pub use rumoca_phase_codegen::templates::{
        BUILTIN_TARGETS, BuiltinTarget, BuiltinTargetTemplate, builtin_target, builtin_targets,
        builtin_template_source,
    };
}
