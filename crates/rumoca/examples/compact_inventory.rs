//! Bounded owner census; this does not execute or package an FMI-LS FMU.
use std::{env, fs, time::Instant};

use rumoca_ir_solve::{ComputeNode, ProjectionStage, ScalarProgramBlock};
use serde_json::{Value, json};

fn projection_inventory(
    component: &rumoca_ir_solve::fmi::FmiComponent,
) -> Result<Value, Box<dyn std::error::Error>> {
    let continuous = &component.problem().continuous;
    let blocks = &continuous.algebraic_projection_plan.blocks;
    let stages: Vec<_> = blocks
        .stages()
        .iter()
        .map(|stage| match stage {
            ProjectionStage::Explicit(block) => json!({
                "kind": "Explicit", "rows": block.rows.len(),
                "targets": block.y_indices.len(),
            }),
            ProjectionStage::SingletonDeclarationRun {
                source,
                first_row,
                declaration,
                storage,
            } => json!({
                "kind": "SingletonDeclarationRun", "node": source.node(),
                "program": source.program(), "first_row": first_row,
                "declaration": declaration, "scalar_count": storage.scalar_count,
            }),
        })
        .collect();
    Ok(json!({
        "stored_stages": stages, "logical_blocks": blocks.len(),
        "implicit_rows": continuous.implicit_rhs.len()?,
        "explicit_refresh_rows": continuous.refresh_owners.algebraic().rows.len(),
        "targets_share_projection_owner": continuous.implicit_row_targets
            .shares_projection_owner(blocks),
    }))
}

fn source_inventory(component: &rumoca_ir_solve::fmi::FmiComponent) -> Value {
    let nodes: Vec<_> = component
        .problem()
        .continuous
        .implicit_rhs
        .nodes
        .iter()
        .enumerate()
        .map(|(index, node)| match node {
            ComputeNode::ScalarPrograms(block) => {
                let programs: Vec<_> = block
                    .programs()
                    .iter()
                    .map(|program| {
                        json!({
                            "operations": program.len(),
                            "outputs": ScalarProgramBlock::program_output_count(program),
                        })
                    })
                    .collect();
                json!({"node": index, "kind": "ScalarPrograms", "programs": programs})
            }
            _ => json!({"node": index, "kind": "tensor"}),
        })
        .collect();
    json!({"nodes": nodes})
}

fn native_inventory(component: &rumoca_ir_solve::fmi::FmiComponent) -> Value {
    let owners = &component.problem().continuous.refresh_owners;
    let Some(schedule) = owners.native_assignment_schedule() else {
        return json!({"issued": false,
            "refusal": format!("{:?}", owners.native_assignment_refusal())});
    };
    let stages: Vec<_> = schedule
        .stages()
        .iter()
        .map(|stage| {
            json!({
                "source": format!("{:?}", stage.source()),
                "target_count": stage.target_count(), "target_stride": stage.target_stride(),
                "target_block_width": stage.target_block_width(),
                "kernel_nodes": stage.value_kernel().nodes.len(),
            })
        })
        .collect();
    json!({"issued": true, "stored_stages": stages,
        "typed_input_runs": schedule.input_runs().len(),
        "typed_input_bytes": schedule.input_lane_bytes(),
        "derived_output_descriptors": schedule.derived_outputs().len(),
        "typed_lane_bytes": schedule.lane_bytes()})
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = env::args().collect();
    let [_, source_path, model, output] = args.as_slice() else {
        return Err("usage: compact_inventory SOURCE MODEL OUTPUT_JSON".into());
    };
    let source = fs::read_to_string(source_path)?;
    let started = Instant::now();
    let _observer = rumoca_compile::compile::install_compile_phase_observer(move |phase, event| {
        eprintln!(
            "PROBE phase={phase:?} event={event:?} elapsed_seconds={}",
            started.elapsed().as_secs_f64()
        );
    });
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(&source, source_path)
        .map_err(|error| {
            if let rumoca::CompilerError::CompileDiagnosticsError { failures, .. } = &error {
                eprintln!("PROBE failures={failures:?}");
            }
            error.to_string()
        })?;
    let frontend_seconds = started.elapsed().as_secs_f64();
    eprintln!("PROBE frontend_seconds={frontend_seconds}");
    let lowering = Instant::now();
    let component = rumoca_sim::lower_fmi_component(&compiled.dae)?;
    let checked_component_seconds = lowering.elapsed().as_secs_f64();
    eprintln!("PROBE checked_component_seconds={checked_component_seconds}");
    let report = json!({"scope": "owner census; no FMU packaging or execution",
        "model": model, "source_bytes": source.len(),
        "frontend_seconds": frontend_seconds,
        "checked_component_seconds": checked_component_seconds,
        "source": source_inventory(&component), "projection": projection_inventory(&component)?,
        "native": native_inventory(&component)});
    fs::write(output, serde_json::to_vec_pretty(&report)?)?;
    eprintln!(
        "PROBE inventory_written total_seconds={}",
        started.elapsed().as_secs_f64()
    );
    Ok(())
}
