use rumoca_ir_dae as dae;
use serde_json::Value;

pub use rumoca_phase_solve as solve;
pub use rumoca_sim::{
    AVAILABLE_BACKENDS, SimBackend, SimError, SimOptions, SimResult, SimSolverMode,
    SimVariableMeta, available_backends, dae_balance, dae_balance_detail, dae_is_balanced,
    prepare_dae_for_template_codegen, prepare_dae_for_template_codegen_with_backend,
    runtime_defined_continuous_unknown_names, runtime_defined_unknown_names, simulate_dae,
    simulate_dae_with_backend,
};

pub fn dae_to_template_json(dae_model: &dae::Dae) -> Value {
    rumoca_phase_codegen::dae_template_json(dae_model)
}

pub fn render_dae_template(dae_model: &dae::Dae, template: &str) -> Result<String, String> {
    rumoca_phase_codegen::render_template(dae_model, template).map_err(|error| error.to_string())
}

pub fn render_dae_template_with_json(dae_json: &Value, template: &str) -> Result<String, String> {
    rumoca_phase_codegen::render_template_with_dae_json(dae_json, template)
        .map_err(|error| error.to_string())
}

pub fn builtin_template_for_target(target: &str) -> Option<&'static str> {
    match target {
        "casadi" => Some(rumoca_phase_codegen::templates::CASADI_SX),
        "cyecca" => Some(rumoca_phase_codegen::templates::CYECCA),
        "julia" => Some(rumoca_phase_codegen::templates::JULIA_MTK),
        "c" => Some(rumoca_phase_codegen::templates::C_CODE),
        "jax" => Some(rumoca_phase_codegen::templates::JAX),
        "onnx" => Some(rumoca_phase_codegen::templates::ONNX),
        _ => None,
    }
}

pub fn render_dae_template_for_target(dae_json: &Value, target: &str) -> Result<String, String> {
    let template =
        builtin_template_for_target(target).ok_or_else(|| format!("Unknown target: {target}"))?;
    render_dae_template_with_json(dae_json, template)
}
