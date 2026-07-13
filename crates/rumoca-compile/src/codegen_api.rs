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
    rumoca_phase_dae::project_dae_for_fmi_metadata(dae_model)
        .map(|prepared| {
            let mut prepared = prepared.into_dae();
            retain_fmi_model_description_metadata(&mut prepared);
            prepared
        })
        .map_err(|err| {
            CodegenError::dae_preparation_failed(
                format!("FMI modelDescription DAE preparation: {err}"),
                err.source_span(),
            )
        })
}

fn retain_fmi_model_description_metadata(prepared: &mut dae::Dae) {
    // modelDescription consumes variable attributes plus event/clock capability
    // metadata. Numerical equations, function bodies, source ancestry, and
    // runtime-only clock lookup tables make the generic DAE JSON much larger
    // but cannot affect the XML document.
    prepared.continuous.equations.clear();
    prepared.continuous.structured_equations.clear();
    prepared.initialization.equations.clear();
    prepared.initialization.structured_equations.clear();
    prepared.discrete.real_updates.clear();
    prepared.discrete.valued_updates.clear();
    prepared.conditions.equations.clear();
    prepared.clocks.constructor_exprs.clear();
    prepared.clocks.intervals.clear();
    prepared.clocks.timings.clear();
    prepared.symbols.functions.clear();
    prepared.symbols.enum_literal_ordinals.clear();
    prepared.metadata.variable_starts.clear();
    prepared.metadata.symbol_ancestry.clear();
}

pub fn dae_for_fmi_implementation_context(dae_model: &dae::Dae) -> Result<dae::Dae, CodegenError> {
    let mut prepared = rumoca_phase_dae::prepare_dae_for_fmi_model_description(dae_model)
        .map(|prepared| prepared.into_dae())
        .map_err(|err| {
            CodegenError::dae_preparation_failed(
                format!("FMI implementation default-value preparation: {err}"),
                err.source_span(),
            )
        })?;
    // FMI implementation kernels are rendered from fully lowered Solve IR,
    // whose LinearOp programs cannot retain Modelica user-function calls. The
    // attached DAE supplies only variable layout/defaults here. Preparing a
    // second scalarized codegen DAE would clone the complete expression graph
    // even though no DAE equation is part of the native kernel.
    prepared.symbols.functions.clear();
    prepared.symbols.enum_literal_ordinals.clear();
    prepared.initialization.equations.clear();
    prepared.initialization.structured_equations.clear();
    prepared.discrete.real_updates.clear();
    prepared.discrete.valued_updates.clear();
    prepared.clocks.constructor_exprs.clear();
    prepared.clocks.intervals.clear();
    prepared.clocks.timings.clear();
    prepared.metadata.variable_starts.clear();
    Ok(prepared)
}

pub fn dae_for_fmi_native_implementation_context(
    dae_model: &dae::Dae,
) -> Result<dae::Dae, CodegenError> {
    let mut prepared = rumoca_phase_dae::project_dae_for_fmi_metadata(dae_model)
        .map(|prepared| prepared.into_dae())
        .map_err(|err| {
            CodegenError::dae_preparation_failed(
                format!("FMI native implementation metadata preparation: {err}"),
                err.source_span(),
            )
        })?;
    // A complete native projection computes all algebraics from Solve IR. Keep
    // variables and condition/event metadata, but do not serialize the unused
    // continuous DAE equation graph into the C template context.
    prepared.continuous.equations.clear();
    prepared.continuous.structured_equations.clear();
    prepared.clocks.constructor_exprs.clear();
    prepared.clocks.intervals.clear();
    prepared.clocks.timings.clear();
    Ok(prepared)
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

pub use rumoca_phase_codegen::{
    SolveTemplateRenderer, fmi3_native_projection_available, render_solve_template_with_name,
};

/// Built-in target metadata, re-exported from the codegen crate.
pub mod templates {
    pub use rumoca_phase_codegen::templates::{
        BUILTIN_TARGETS, BuiltinTarget, BuiltinTargetTemplate, builtin_target, builtin_targets,
        builtin_template_source,
    };
}
