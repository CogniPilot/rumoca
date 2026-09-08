use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use serde_json::Value;

pub use rumoca_phase_codegen::CodegenError;

pub fn dae_to_template_json(dae_model: &dae::Dae) -> Result<Value, CodegenError> {
    rumoca_phase_codegen::dae_template_json(dae_model)
}

pub fn render_casadi_execution_model(
    model: solve::SolveModel,
    model_name: &str,
) -> Result<String, CodegenError> {
    rumoca_phase_codegen::render_casadi_execution_model(model, model_name)
}
