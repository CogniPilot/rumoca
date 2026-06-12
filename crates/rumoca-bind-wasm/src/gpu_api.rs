//! WebGPU execution preparation: renders the `wgsl-solve` target and packs
//! everything a browser-side integrator needs into one JSON payload.

use wasm_bindgen::prelude::*;

use crate::simulation_api::build_simulation_options;
use crate::{compile_requested_model, qualify_input_model_name, with_singleton_session};
use rumoca_compile::codegen::render_solve_template_with_name;
use rumoca_compile::codegen::targets::{TargetBundle, TargetTemplateSource};

/// Prepare a model for WebGPU execution.
///
/// Compiles `source`, lowers it to the Solve IR with simulation defaults
/// (honoring the model's `experiment` annotation like `simulate_model`),
/// renders the `wgsl-solve` builtin target, and returns a JSON object:
///
/// ```json
/// {
///   "wgsl": "...compute shader source...",
///   "layout": { ...the wgsl-solve layout manifest... },
///   "y0": [..], "p0": [..],
///   "n_states": 3,
///   "t_start": 0.0, "t_end": 1.0, "dt": 0.01
/// }
/// ```
///
/// v1 semantics: the host integrates the first `n_states` entries of `y`;
/// the remaining (algebraic) slots and all parameters - including relation
/// memory - stay frozen at their prepared initial values. The layout's
/// `runtime_event_roots` count lets hosts warn when that matters.
#[wasm_bindgen]
pub fn prepare_gpu_simulation(source: &str, model_name: &str) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let requested_model = qualify_input_model_name(session, model_name);
        let result = compile_requested_model(session, &requested_model)?;

        let (opts, _solver_label) = build_simulation_options(&result, 0.0, 0.0, "");
        let solve_model = rumoca_sim::lower_dae_for_simulation(&result.dae, &opts)
            .map_err(|e| JsValue::from_str(&format!("Solve lowering failed: {e}")))?;

        let bundle = TargetBundle::builtin("wgsl-solve")
            .ok_or_else(|| JsValue::from_str("wgsl-solve builtin target is missing"))?;
        let render = |template_name: &str| -> Result<String, JsValue> {
            let template = bundle
                .template_source(template_name)
                .map_err(|e| JsValue::from_str(&format!("{e}")))?;
            render_solve_template_with_name(
                &solve_model.problem,
                &solve_model.artifacts,
                template.as_ref(),
                model_name,
            )
            .map_err(|e| JsValue::from_str(&format!("wgsl-solve render failed: {e}")))
        };
        let wgsl = render("model_solve.wgsl.jinja")?;
        let layout_text = render("model_layout.json.jinja")?;
        let layout: serde_json::Value = serde_json::from_str(&layout_text)
            .map_err(|e| JsValue::from_str(&format!("wgsl-solve layout is not JSON: {e}")))?;

        let response = serde_json::json!({
            "wgsl": wgsl,
            "layout": layout,
            "y0": solve_model.initial_y,
            "p0": solve_model.parameters,
            "n_states": solve_model.state_scalar_count(),
            "t_start": opts.t_start,
            "t_end": opts.t_end,
            "dt": opts.dt,
        });
        serde_json::to_string(&response).map_err(|e| JsValue::from_str(&format!("JSON error: {e}")))
    })
}
