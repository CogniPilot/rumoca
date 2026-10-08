//! Single-call direct assignments from the compiler's certified native schedule.

use crate::{WasmError, native_assignment_api};
use sha2::{Digest, Sha256};
use wasm_bindgen::prelude::*;

/// Compile one stateless assignment module with shared Y/P storage.
/// The host writes P and calls `eval_assignments` once. Integer and Boolean
/// inputs are written to the typed input lanes listed in `input_lanes` (i64,
/// u8) at the start of the typed lane buffer, never to P. Derived discrete
/// outputs (Integer, Boolean, discrete Real) are published through the typed
/// output lanes listed in `derived_outputs`, after the input lanes; read
/// Integer lanes with [`read_native_integer_lane`] or a `BigInt64Array`.
/// All source-issued stage ordering and direct target writes live in the module.
#[wasm_bindgen]
pub fn prepare_native_program(source: &str, model_name: &str) -> Result<String, WasmError> {
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
    let input_lane_bytes = schedule.input_lane_bytes();
    let lane_bytes = schedule.lane_bytes();
    let typed_lanes_offset = storage_bytes
        .checked_add(scratch_bytes)
        .and_then(|end| end.checked_next_multiple_of(8))
        .ok_or_else(|| WasmError::new("native program memory exceeds the 64 MiB profile"))?;
    let lanes_offset = typed_lanes_offset
        .checked_add(input_lane_bytes)
        .ok_or_else(|| WasmError::new("native program memory exceeds the 64 MiB profile"))?;
    let memory_bytes = lanes_offset
        .checked_add(lane_bytes)
        .filter(|&size| size <= 64 * 1024 * 1024)
        .ok_or_else(|| WasmError::new("native program memory exceeds the 64 MiB profile"))?;
    if scratch_bytes != 0 {
        abi_extra["scratch_offset"] = serde_json::json!(storage_bytes);
    }
    if input_lane_bytes + lane_bytes != 0 {
        abi_extra["typed_lanes_offset"] = serde_json::json!(typed_lanes_offset);
    }
    if input_lane_bytes != 0 {
        abi_extra["input_lanes_offset"] = serde_json::json!(typed_lanes_offset);
        abi_extra["input_lanes_bytes"] = serde_json::json!(input_lane_bytes);
    }
    if lane_bytes != 0 {
        abi_extra["output_lanes_offset"] = serde_json::json!(lanes_offset);
        abi_extra["output_lanes_bytes"] = serde_json::json!(lane_bytes);
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
                "source": stage_source(stage),
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
        "compiler": crate::compiler_provenance(),
        "solve_schema_version": rumoca_ir_solve::SOLVE_SCHEMA_VERSION,
        "module_sha256": format!("{:x}", Sha256::digest(&bytes)), "module_bytes": bytes,
        "abi": { "export": "eval_assignments",
            "arguments": ["yPtr:i32", "pPtr:i32", "time:f64", "reservedSeedPtr:i32", "reservedOutputPtr:i32"],
            "memory_import": "env.memory", "memory_shared": false,
            "memory_pages": memory_bytes.div_ceil(65536).max(1),
            "y_offset": 0, "p_offset": y_count * 8,
            "y_count": y_count, "p_count": p_count, "reserved_pointer_value": 0 },
        "var_layout": host_layout(problem, schedule)?,
        "derived_outputs": derived_outputs(problem, schedule),
        "input_lanes": input_lanes(problem, schedule),
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
            "arguments":["yPtr:i32","pPtr:i32","time:f64","scratchPtr:i32",
                match (schedule.input_lane_bytes(), schedule.lane_bytes()) {
                    (0, 0) => "reservedZero:i32",
                    (0, _) => "outputLanesPtr:i32",
                    _ => "typedLanesPtr:i32",
                }],
            "result":"status:i32", "success_status":0, "scratch_bytes":compiled.scratch_bytes(),
            "transactional_y":true,"p_readonly":true,
        }),
        serde_json::json!(compiled.math_imports()),
        serde_json::json!(faults),
    ))
}

pub(crate) fn requires_checked_entry(
    schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule,
) -> bool {
    !schedule.derived_outputs().is_empty()
        || !schedule.input_lanes().is_empty()
        || any_stage_operation(schedule, |operation| {
            matches!(
                operation,
                rumoca_ir_solve::LinearOp::PureCall { .. }
                    | rumoca_ir_solve::LinearOp::LoadIndexedRegister { .. }
                    | rumoca_ir_solve::LinearOp::FunctionConditional { .. }
            )
        })
}

/// Whether any stage kernel of `schedule` holds an operation `matches`.
pub(crate) fn any_stage_operation(
    schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule,
    matches: fn(&rumoca_ir_solve::LinearOp) -> bool,
) -> bool {
    use rumoca_ir_solve::SolveVisitor;
    struct Found(bool, fn(&rumoca_ir_solve::LinearOp) -> bool);
    impl SolveVisitor for Found {
        type Error = std::convert::Infallible;
        fn visit_linear_op(
            &mut self,
            _: rumoca_ir_solve::LinearOpSliceKind,
            _: usize,
            operation: &rumoca_ir_solve::LinearOp,
        ) -> Result<(), Self::Error> {
            self.0 |= (self.1)(operation);
            Ok(())
        }
    }
    let mut found = Found(false, matches);
    for stage in schedule.stages() {
        let Ok(()) = found.visit_compute_block(stage.value_kernel());
    }
    found.0
}

/// The canonical Solve owner of one issued stage.
pub(crate) fn stage_source(
    stage: &rumoca_ir_solve::NativeRefreshAssignmentStage,
) -> serde_json::Value {
    match stage.source() {
        rumoca_ir_solve::NativeStageSource::Continuous { node } => {
            serde_json::json!({ "continuous_node": node })
        }
        rumoca_ir_solve::NativeStageSource::Discrete { row } => {
            serde_json::json!({ "discrete_row": row })
        }
    }
}

/// Every derived-discrete output a host reads from the typed output lanes:
/// its scalar name, storage representation (f64, i64 or u8) and byte offset
/// from the start of the output lanes.
fn derived_outputs(
    problem: &rumoca_ir_solve::SolveProblem,
    schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule,
) -> serde_json::Value {
    let names = lane_slot_names(problem, schedule);
    schedule
        .derived_outputs()
        .iter()
        .map(|output| {
            serde_json::json!({
                "name": names.get(&output.p_index()),
                "representation": output.lane().as_str(),
                "byte_offset": output.lane_offset(),
            })
        })
        .collect()
}

/// Every Integer or Boolean input a host writes to the typed input lanes
/// (SPEC_0040 SOLVE-C69): its scalar name, representation (i64 or u8), byte
/// offset from the start of the typed lane buffer, and the Solve P slot
/// (whose `parameters` entry is its declared start value) it replaces.
fn input_lanes(
    problem: &rumoca_ir_solve::SolveProblem,
    schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule,
) -> serde_json::Value {
    let names = lane_slot_names(problem, schedule);
    schedule
        .input_lanes()
        .iter()
        .map(|input| {
            serde_json::json!({
                "name": names.get(&input.p_index()),
                "representation": input.lane().as_str(),
                "byte_offset": input.lane_offset(),
                "p_index": input.p_index(),
            })
        })
        .collect()
}

/// The Solve P slots a host reaches through typed lanes instead of P: every
/// derived output and every typed input.
fn lane_slots(
    schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule,
) -> std::collections::BTreeSet<usize> {
    schedule
        .derived_outputs()
        .iter()
        .map(|output| output.p_index())
        .chain(schedule.input_lanes().iter().map(|input| input.p_index()))
        .collect()
}

/// The scalar binding name of each typed-lane P slot: an array element's own
/// name rather than its array base.
fn lane_slot_names(
    problem: &rumoca_ir_solve::SolveProblem,
    schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule,
) -> std::collections::BTreeMap<usize, String> {
    let slots = lane_slots(schedule);
    let mut names = std::collections::BTreeMap::<usize, String>::new();
    for (name, slot) in problem.layout.bindings() {
        let rumoca_ir_solve::ScalarSlot::P { index, .. } = *slot else {
            continue;
        };
        if !slots.contains(&index) {
            continue;
        }
        let name = name.as_str();
        let better = names.get(&index).is_none_or(|current| {
            (name.contains('['), name.len()) > (current.contains('['), current.len())
        });
        if better {
            names.insert(index, name.to_owned());
        }
    }
    names
}

/// The host-visible layout: the problem layout without any binding that
/// covers a typed-lane P slot (a derived output, which the program never reads
/// or publishes, or a typed input, which the host writes to its lane).
fn host_layout(
    problem: &rumoca_ir_solve::SolveProblem,
    schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule,
) -> Result<serde_json::Value, WasmError> {
    let mut layout = serde_json::to_value(&problem.layout)
        .map_err(|error| WasmError::new(format!("native layout JSON failed: {error}")))?;
    let slots = lane_slots(schedule);
    if slots.is_empty() {
        return Ok(layout);
    }
    let mut removed = Vec::new();
    for (name, slot) in problem.layout.bindings() {
        if let rumoca_ir_solve::ScalarSlot::P { index, .. } = *slot
            && slots.contains(&index)
        {
            removed.push(name.as_str().to_owned());
        }
    }
    for table in ["bindings", "shapes", "shape_spans"] {
        if let Some(entries) = layout[table].as_object_mut() {
            entries.retain(|name, _| !removed.contains(name));
        }
    }
    Ok(layout)
}

/// Read one Integer output lane, the little-endian i64 at `byte_offset` of the
/// published output lanes: a JavaScript number when its value is exactly
/// representable as one, otherwise a BigInt, so no host read rounds it.
///
/// Coverage exemption: the JavaScript values exist only under a JavaScript
/// host, so no workspace test can call this; the decoding it publishes is
/// [`integer_lane`], which the tests cover.
#[cfg_attr(coverage_nightly, coverage(off))]
#[wasm_bindgen]
pub fn read_native_integer_lane(lanes: &[u8], byte_offset: usize) -> Result<JsValue, WasmError> {
    Ok(match integer_lane(lanes, byte_offset)? {
        IntegerLane::Number(value) => JsValue::from_f64(value),
        IntegerLane::BigInt(value) => js_sys::BigInt::from(value).into(),
    })
}

#[derive(Debug, PartialEq)]
pub(crate) enum IntegerLane {
    Number(f64),
    BigInt(i64),
}

pub(crate) fn integer_lane(lanes: &[u8], byte_offset: usize) -> Result<IntegerLane, WasmError> {
    let bytes = byte_offset
        .checked_add(8)
        .and_then(|end| lanes.get(byte_offset..end))
        .ok_or_else(|| WasmError::new("Integer output lane lies outside the published lanes"))?;
    let value = i64::from_le_bytes(bytes.try_into().expect("an 8-byte lane"));
    // Every integer of magnitude at most 2^53 is exactly a Binary64 value.
    Ok(if value.unsigned_abs() <= 1 << 53 {
        IntegerLane::Number(value as f64)
    } else {
        IntegerLane::BigInt(value)
    })
}
