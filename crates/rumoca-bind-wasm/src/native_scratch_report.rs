//! JSON form of the native whole-program scratch report.
use rumoca_exec_wasm::{ScratchCall, ScratchFrame, ScratchOwner, ScratchRegion, ScratchReport};
use serde_json::{Value, json};

/// Per-owner frame, region and call-site high-water marks plus the program
/// components, under `abi.scratch_report`.
pub(crate) fn scratch_report_json(report: &ScratchReport) -> Value {
    json!({
        "total_bytes": report.total_bytes,
        "work_y_bytes": report.work_y_bytes,
        "call_input_bytes": report.call_input_bytes,
        "call_output_bytes": report.call_output_bytes,
        "call_scratch_bytes": report.call_scratch_bytes,
        "memo_bytes": report.memo_bytes,
        "typed_lane_bytes": report.typed_lane_bytes,
        "p_copy_bytes": report.p_copy_bytes,
        "unshared_call_scratch_bytes": report.unshared_call_scratch_bytes,
        "widest_owner": report.widest_owner,
        "owners": report.owners.iter().map(owner_json).collect::<Vec<_>>(),
    })
}

fn owner_json(owner: &ScratchOwner) -> Value {
    json!({
        "owner": owner.owner,
        "provenance": {
            "source": owner.provenance.source.0.to_string(),
            "start": owner.provenance.start,
            "end": owner.provenance.end,
        },
        "frame": frame_json(&owner.frame),
    })
}

fn frame_json(frame: &ScratchFrame) -> Value {
    json!({
        "base_bytes": frame.base_bytes,
        "input_bytes": frame.input_bytes,
        "output_bytes": frame.output_bytes,
        "high_water_bytes": frame.high_water_bytes,
        "unshared_bytes": frame.unshared_bytes,
        "slot_bytes": frame.slot_bytes,
        "register_bytes": frame.register_bytes,
        "register_count": frame.register_count,
        "largest_register_bytes": frame.largest_register_bytes,
        "regions": frame.regions.iter().map(region_json).collect::<Vec<_>>(),
        "calls": frame.calls.iter().map(call_json).collect::<Vec<_>>(),
    })
}

fn region_json(region: &ScratchRegion) -> Value {
    json!({
        "operation": region.operation,
        "role": region.role,
        "frame": frame_json(&region.frame),
    })
}

fn call_json(call: &ScratchCall) -> Value {
    json!({
        "operation": call.operation,
        "owner": call.owner,
        "input_bytes": call.input_bytes,
        "output_bytes": call.output_bytes,
        "scratch_bytes": call.scratch_bytes,
        "offset_bytes": call.offset_bytes,
    })
}
