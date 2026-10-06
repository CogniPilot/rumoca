//! Single-call direct assignments from the compiler's certified native schedule.

use crate::{GIT_COMMIT, WasmError, native_assignment_api};
use sha2::{Digest, Sha256};
use wasm_bindgen::prelude::*;

/// Compile one stateless assignment module with shared Y/P storage.
/// The host writes P and calls `eval_assignments(yPtr, pPtr, time, 0, 0)` once.
/// All source-issued stage ordering and direct target writes live in the module.
#[wasm_bindgen]
pub fn prepare_native_program(source: &str, model_name: &str) -> Result<String, WasmError> {
    prepare_native_program_impl(source, model_name)
}

pub(crate) fn prepare_native_program_impl(
    source: &str,
    model_name: &str,
) -> Result<String, WasmError> {
    native_assignment_api::with_prepared_native_model(source, model_name, model_artifact)
}

pub(crate) fn model_artifact(
    model: &rumoca_ir_solve::SolveModel,
    source: &str,
    model_name: &str,
) -> Result<String, WasmError> {
    let problem = &model.problem;
    let schedule = native_assignment_api::checked_native_schedule(model)?;
    let y_count = problem.layout.y_scalars();
    let p_count = problem.layout.p_scalars();
    let storage_bytes = y_count
        .checked_add(p_count)
        .and_then(|count| count.checked_mul(8))
        .ok_or_else(|| WasmError::new("native program memory size overflows"))?;
    if storage_bytes > 64 * 1024 * 1024 {
        return Err(WasmError::new(
            "native program memory exceeds the 64 MiB profile",
        ));
    }
    let (bytes, profile, mut abi_extra, math_imports, faults) = compile_program(model, schedule)?;
    let scratch_bytes = abi_extra["scratch_bytes"].as_u64().unwrap_or(0) as usize;
    let memory_bytes = storage_bytes
        .checked_add(scratch_bytes)
        .filter(|&size| size <= 64 * 1024 * 1024)
        .ok_or_else(|| WasmError::new("native program memory exceeds the 64 MiB profile"))?;
    if scratch_bytes != 0 {
        abi_extra["scratch_offset"] = serde_json::json!(storage_bytes);
    }
    if bytes.len() > 64 * 1024 * 1024 {
        return Err(WasmError::new(
            "native program module exceeds the 64 MiB profile",
        ));
    }
    let issued_schedule = schedule
        .stages()
        .iter()
        .map(|stage| {
            serde_json::json!({
                "source_node": stage.source_node(),
                "target_start": stage.target_span().start,
                "target_count": stage.target_count(),
                "target_stride": stage.target_stride(),
                "target_block_width": stage.target_block_width(),
            })
        })
        .collect::<Vec<_>>();
    let mut response = serde_json::json!({
        "profile": profile,
        "model_name": model_name,
        "source_sha256": format!("{:x}", Sha256::digest(source.as_bytes())),
        "compiler": { "version": env!("CARGO_PKG_VERSION"), "git_commit": GIT_COMMIT },
        "solve_schema_version": rumoca_ir_solve::SOLVE_SCHEMA_VERSION,
        "module_sha256": format!("{:x}", Sha256::digest(&bytes)), "module_bytes": bytes,
        "abi": { "export": "eval_assignments",
            "arguments": ["yPtr:i32", "pPtr:i32", "time:f64", "reservedSeedPtr:i32", "reservedOutputPtr:i32"],
            "memory_import": "env.memory", "memory_shared": false,
            "memory_pages": memory_bytes.div_ceil(65536).max(1),
            "y_offset": 0, "p_offset": y_count * 8,
            "y_count": y_count, "p_count": p_count, "reserved_pointer_value": 0 },
        "var_layout": problem.layout,
        "input_names": problem.solve_layout.input_scalar_names(),
        "parameters": model.parameters,
        "issued_schedule": issued_schedule,
    });
    let abi = response["abi"]
        .as_object_mut()
        .expect("artifact ABI object");
    abi.extend(abi_extra.as_object().expect("ABI extensions").clone());
    if profile == "native-direct-program-f64-v3" {
        response["math_imports"] = math_imports;
        response["faults"] = faults;
    }
    serde_json::to_string(&response)
        .map_err(|error| WasmError::new(format!("native program JSON failed: {error}")))
}

type ProgramArtifact = (
    Vec<u8>,
    &'static str,
    serde_json::Value,
    serde_json::Value,
    serde_json::Value,
);

fn compile_program(
    model: &rumoca_ir_solve::SolveModel,
    schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule,
) -> Result<ProgramArtifact, WasmError> {
    let checked_entry = requires_checked_entry(schedule);
    if !checked_entry {
        let bytes = rumoca_exec_wasm::compile_native_assignment_schedule_wasm_bytes(
            schedule,
            &model.problem.layout,
        )
        .map_err(|e| WasmError::new(format!("native program WASM capability rejected: {e}")))?;
        return Ok((
            bytes,
            "native-direct-program-f64-v2",
            serde_json::json!({}),
            serde_json::Value::Null,
            serde_json::Value::Null,
        ));
    }
    let compiled = rumoca_exec_wasm::compile_native_assignment_schedule_with_calls_wasm(
        schedule,
        &model.problem.layout,
        &model.pure_calls,
    )
    .map_err(|e| WasmError::new(format!("native program WASM capability rejected: {e}")))?;
    let mut faults = vec![
        serde_json::json!({"status":1,"kind":"InvalidBuffer"}),
        serde_json::json!({"status":2,"kind":"InvalidInput"}),
    ];
    faults.extend(compiled.gather_faults().iter().map(|fault| {
        serde_json::json!({
            "status":fault.status, "kind":format!("{:?}",fault.kind),
            "kernel":fault.kernel, "program":fault.program, "operation":fault.operation,
            "region_path":fault.region_path, "opcode":"LoadIndexedRegister",
            "provenance":{"source":fault.provenance.source.0.to_string(),
                "start":fault.provenance.start,"end":fault.provenance.end},
        })
    }));
    faults.extend(compiled.faults().iter().map(|fault| {
        serde_json::json!({
            "status":fault.status, "kind":format!("{:?}",fault.kind), "owner":fault.owner.index(),
            "operation":fault.operation, "region_path":fault.region_path, "opcode":fault.opcode,
            "provenance":{"source":fault.provenance.source.0.to_string(),
                "start":fault.provenance.start,"end":fault.provenance.end},
        })
    }));
    Ok((
        compiled.module_bytes().to_vec(),
        "native-direct-program-f64-v3",
        serde_json::json!({
            "arguments":["yPtr:i32","pPtr:i32","time:f64","scratchPtr:i32","reservedZero:i32"],
            "result":"status:i32", "success_status":0, "scratch_bytes":compiled.scratch_bytes(),
            "transactional_y":true,"p_readonly":true,
        }),
        serde_json::json!(compiled.math_imports()),
        serde_json::json!(faults),
    ))
}

fn requires_checked_entry(schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule) -> bool {
    use rumoca_ir_solve::SolveVisitor;
    struct Calls(bool);
    impl SolveVisitor for Calls {
        type Error = std::convert::Infallible;
        fn visit_linear_op(
            &mut self,
            _: rumoca_ir_solve::LinearOpSliceKind,
            _: usize,
            operation: &rumoca_ir_solve::LinearOp,
        ) -> Result<(), Self::Error> {
            self.0 |= matches!(
                operation,
                rumoca_ir_solve::LinearOp::PureCall { .. }
                    | rumoca_ir_solve::LinearOp::LoadIndexedRegister { .. }
                    | rumoca_ir_solve::LinearOp::FunctionConditional { .. }
            );
            Ok(())
        }
    }
    let mut calls = Calls(false);
    for stage in schedule.stages() {
        let Ok(()) = calls.visit_compute_block(stage.value_kernel());
    }
    calls.0
}
