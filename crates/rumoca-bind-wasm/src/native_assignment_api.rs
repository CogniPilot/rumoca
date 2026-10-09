//! Portable CPU WASM preparation from construction-issued native value stages.

use crate::{WasmError, compile_requested_model, qualify_input_model_name, with_singleton_session};
use sha2::{Digest, Sha256};
use wasm_bindgen::prelude::*;

/// Compile a stateless native-family assignment pipeline to portable CPU WASM.
/// Hosts write input P slots before each invocation, execute stages in the issued
/// order, and copy each complete stage output into its exact target Y range.
/// No equation solving, kernel isolation, or schedule discovery occurs in a host.
#[wasm_bindgen]
pub fn prepare_native_assignments(source: &str, model_name: &str) -> Result<String, WasmError> {
    with_prepared_native_model(source, model_name, model_artifact)
}

pub(crate) fn with_prepared_native_model(
    source: &str,
    model_name: &str,
    artifact: impl FnOnce(&rumoca_ir_solve::SolveModel, &str, &str) -> Result<String, WasmError>,
) -> Result<String, WasmError> {
    with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let requested = qualify_input_model_name(session, model_name);
        let compilation = compile_requested_model(session, &requested)?;
        let lowered =
            rumoca_sim::lower_dae_for_native_preparation(&compilation.dae, &Default::default())
                .map_err(|error| {
                    WasmError::new(format!("native assignment lowering failed: {error}"))
                })?;
        artifact(&lowered, source, model_name)
    })
}

pub(crate) fn checked_native_schedule(
    model: &rumoca_ir_solve::SolveModel,
) -> Result<&rumoca_ir_solve::NativeRefreshAssignmentSchedule, WasmError> {
    // The issued schedule owns every semantic admission decision (states,
    // events, history, clocks, initialization, typed outputs); only the
    // external table bindings live on the model rather than the problem.
    let problem = &model.problem;
    if !model.external_tables.is_empty() {
        return Err(WasmError::new(
            "native evaluation has no external table storage",
        ));
    }
    problem
        .continuous
        .refresh_owners
        .native_assignment_schedule()
        .ok_or_else(|| {
            WasmError::new(format!(
                "compiler did not issue a complete native direct-assignment schedule: {}",
                problem
                    .continuous
                    .refresh_owners
                    .native_assignment_refusal()
                    .map_or_else(
                        || "missing construction evidence".into(),
                        ToString::to_string
                    ),
            ))
        })
}

fn model_artifact(
    model: &rumoca_ir_solve::SolveModel,
    source: &str,
    model_name: &str,
) -> Result<String, WasmError> {
    let problem = &model.problem;
    let schedule = checked_native_schedule(model)?;
    let carries_calls = crate::native_program_api::any_stage_operation(schedule, |operation| {
        matches!(operation, rumoca_ir_solve::LinearOp::PureCall { .. })
    });
    if !schedule.derived_outputs().is_empty() || !schedule.input_lanes().is_empty() || carries_calls
    {
        return Err(WasmError::new(
            "typed lanes and pure calls belong to the native program ABI, not the separate-stage copy ABI: its stage modules return no status and link no call table",
        ));
    }
    let y_count = problem.layout.y_scalars();
    let p_count = problem.layout.p_scalars();
    let max_output = schedule
        .stages()
        .iter()
        .map(|stage| stage.target_span().len())
        .max()
        .unwrap_or(0);
    let seed_count = y_count
        .checked_add(p_count)
        .ok_or_else(|| WasmError::new("native layout size overflows"))?;
    let memory_bytes = seed_count
        .checked_mul(2)
        .and_then(|size| size.checked_add(max_output))
        .and_then(|size| size.checked_mul(8))
        .ok_or_else(|| WasmError::new("native memory size overflows"))?;
    if memory_bytes > 64 * 1024 * 1024 {
        return Err(WasmError::new(
            "native assignment memory exceeds the 64 MiB profile",
        ));
    }
    let stages = schedule
        .stages()
        .iter()
        .map(|stage| stage_artifact(stage, &problem.layout))
        .collect::<Result<Vec<_>, WasmError>>()?;
    let response = serde_json::json!({
        "profile": "native-direct-assignments-f64-v1",
        "model_name": model_name,
        "source_sha256": format!("{:x}", Sha256::digest(source.as_bytes())),
        "compiler": crate::compiler_provenance(),
        "solve_schema_version": rumoca_ir_solve::SOLVE_SCHEMA_VERSION,
        "abi": { "export": "eval_residual", "arguments": ["yPtr:i32", "pPtr:i32", "time:f64", "seedPtr:i32", "outPtr:i32"],
            "memory_import": "env.memory", "memory_shared": false,
            "memory_pages": memory_bytes.div_ceil(65536).max(1),
            "y_offset": 0, "p_offset": y_count * 8, "seed_offset": seed_count * 8,
            "output_offset": seed_count * 16, "output_capacity": max_output,
            "y_count": y_count, "p_count": p_count, "seed_count": seed_count },
        "var_layout": problem.layout,
        "input_names": problem.solve_layout.input_scalar_names(),
        "parameters": model.parameters,
        "stages": stages,
    });
    serde_json::to_string(&response)
        .map_err(|error| WasmError::new(format!("native assignment JSON failed: {error}")))
}

fn stage_artifact(
    stage: &rumoca_ir_solve::NativeRefreshAssignmentStage,
    layout: &rumoca_ir_solve::VarLayout,
) -> Result<serde_json::Value, WasmError> {
    let range = stage.target_range().ok_or_else(|| WasmError::new(
        "the separate-stage copy ABI requires dense targets; use the native single-program ABI for strided targets",
    ))?;
    let bytes =
        rumoca_exec_wasm::compile_expression_compute_block_wasm_bytes(stage.value_kernel(), layout)
            .map_err(|error| {
                WasmError::new(format!(
                    "native assignment WASM capability rejected: {error}"
                ))
            })?;
    if stage
        .value_kernel()
        .output_count("native value kernel")
        .map_err(|error| WasmError::new(error.to_string()))?
        != range.len()
    {
        return Err(WasmError::new(
            "native assignment output cardinality differs from its owned targets",
        ));
    }
    Ok(serde_json::json!({
        "source": crate::native_program_api::stage_source(stage), "target_start": range.start,
        "target_count": range.len(),
        "module_sha256": format!("{:x}", Sha256::digest(&bytes)), "module_bytes": bytes,
    }))
}
