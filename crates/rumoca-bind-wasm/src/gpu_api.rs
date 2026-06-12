//! WebGPU execution preparation: renders the `wgsl-solve` target and packs
//! everything a browser-side integrator needs into one JSON payload.

use std::cell::RefCell;

use wasm_bindgen::prelude::*;

use crate::simulation_api::build_simulation_options;
use crate::{compile_requested_model, qualify_input_model_name, with_singleton_session};
use rumoca_compile::codegen::SolveTemplateRenderer;
use rumoca_compile::codegen::targets::{TargetBundle, TargetTemplateSource};

/// Lowered model retained from the last `prepare_gpu_simulation` so
/// parameter updates can re-settle vectors without re-lowering.
struct GpuPrepCache {
    source_key: u64,
    model_name: String,
    t_start: f64,
    solve_model: rumoca_ir_solve::SolveModel,
}

thread_local! {
    static GPU_PREP_CACHE: RefCell<Option<GpuPrepCache>> = const { RefCell::new(None) };
}

fn source_key(source: &str, model_name: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::hash::DefaultHasher::new();
    source.hash(&mut hasher);
    model_name.hash(&mut hasher);
    hasher.finish()
}

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
        // One shared context: building it serializes the full solve problem.
        let renderer =
            SolveTemplateRenderer::new(&solve_model.problem, &solve_model.artifacts, model_name)
                .map_err(|e| JsValue::from_str(&format!("wgsl-solve context failed: {e}")))?;
        let render = |template_name: &str| -> Result<String, JsValue> {
            let template = bundle
                .template_source(template_name)
                .map_err(|e| JsValue::from_str(&format!("{e}")))?;
            renderer
                .render(template.as_ref())
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
        let text = serde_json::to_string(&response)
            .map_err(|e| JsValue::from_str(&format!("JSON error: {e}")))?;
        GPU_PREP_CACHE.with(|cache| {
            *cache.borrow_mut() = Some(GpuPrepCache {
                source_key: source_key(source, model_name),
                model_name: model_name.to_string(),
                t_start: opts.t_start,
                solve_model,
            });
        });
        Ok(text)
    })
}

/// Re-settle the prepared vectors of the last `prepare_gpu_simulation` for
/// new parameter values, without re-lowering the model. `overrides_json`
/// is a `{ "name": value }` object naming scalar parameters. Returns
/// `{ "y0": [...], "p0": [...] }`.
#[wasm_bindgen]
pub fn update_gpu_parameters(
    source: &str,
    model_name: &str,
    overrides_json: &str,
) -> Result<String, JsValue> {
    let overrides: std::collections::BTreeMap<String, f64> =
        serde_json::from_str(overrides_json)
            .map_err(|e| JsValue::from_str(&format!("overrides must be {{name: value}}: {e}")))?;
    let overrides: Vec<(String, f64)> = overrides.into_iter().collect();
    GPU_PREP_CACHE.with(|cache| {
        let cache = cache.borrow();
        let Some(prep) = cache.as_ref() else {
            return Err(JsValue::from_str(
                "no prepared GPU model in this session; run prepare_gpu_simulation first",
            ));
        };
        if prep.source_key != source_key(source, model_name) || prep.model_name != model_name {
            return Err(JsValue::from_str(
                "the prepared GPU model does not match this source; run prepare_gpu_simulation again",
            ));
        }
        let (y0, p0) =
            rumoca_sim::refresh_prepared_vectors(&prep.solve_model, prep.t_start, &overrides)
                .map_err(|e| JsValue::from_str(&format!("parameter update failed: {e}")))?;
        serde_json::to_string(&serde_json::json!({ "y0": y0, "p0": p0 }))
            .map_err(|e| JsValue::from_str(&format!("JSON error: {e}")))
    })
}
