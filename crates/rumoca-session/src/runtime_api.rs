use rumoca_ir_dae as dae;
use serde_json::Value;

pub use rumoca_phase_solve as solve;
pub use rumoca_sim::{
    SimOptions, SimResult, SimSolverMode, SimVariableMeta, compiled_layout_binding_debug,
    compiled_layout_related_bindings_debug, dae_balance, dae_balance_detail, dae_is_balanced,
    runtime_defined_continuous_unknown_names, runtime_defined_unknown_names,
};
pub use rumoca_sim_diffsol::{
    AVAILABLE_BACKENDS, PreparedSimulation, SimBackend, SimError, SimStepper, StepperOptions,
    StepperState, available_backends, build_simulation, prepare_dae_for_template_codegen,
    run_prepared_simulation, simulate_dae,
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

pub fn render_dae_template_with_name(
    dae_model: &dae::Dae,
    template: &str,
    model_name: &str,
) -> Result<String, String> {
    rumoca_phase_codegen::render_template_with_name(dae_model, template, model_name)
        .map_err(|error| error.to_string())
}

/// Built-in FMI 2.0 template sources, re-exported from the codegen crate.
pub mod fmi2_templates {
    pub use rumoca_phase_codegen::templates::{
        FMI2_MODEL, FMI2_MODEL_DESCRIPTION, FMI2_TEST_DRIVER,
    };
}

/// Built-in FMI 3.0 template sources, re-exported from the codegen crate.
pub mod fmi3_templates {
    pub use rumoca_phase_codegen::templates::{
        FMI3_MODEL, FMI3_MODEL_DESCRIPTION, FMI3_TEST_DRIVER,
    };
}

/// Built-in embedded C template sources, re-exported from the codegen crate.
pub mod embedded_c_templates {
    pub use rumoca_phase_codegen::templates::{EMBEDDED_C_H, EMBEDDED_C_IMPL};
}

/// All built-in template sources, re-exported from the codegen crate.
pub mod templates {
    pub use rumoca_phase_codegen::templates::*;
}
