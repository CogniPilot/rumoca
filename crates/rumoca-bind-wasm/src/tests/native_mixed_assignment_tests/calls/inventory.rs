//! Test-only capture precedes native issuance so refusals retain checked evidence.
mod tests;

use crate::WasmError;
use rumoca_ir_solve as solve;
use serde::Serialize;
use serde_json::{Value, json};
use std::path::Path;

#[derive(Serialize)]
struct Inventory<'a> {
    problem: &'a solve::SolveProblem,
    pure_calls: &'a solve::SolvePureCallTable,
    parameters: &'a [f64],
}

pub(super) fn capture(model: &solve::SolveModel) -> Result<String, WasmError> {
    let path = std::env::var_os("RUMOCA_NATIVE_SOURCE_SOLVE_ARTIFACT");
    let inventory = save(model, path.as_deref().map(Path::new))?;
    for (index, node) in model
        .problem
        .continuous
        .implicit_rhs
        .nodes
        .iter()
        .enumerate()
    {
        print_source(index, node, &model.pure_calls)?;
    }
    Ok(inventory)
}

fn save(model: &solve::SolveModel, path: Option<&Path>) -> Result<String, WasmError> {
    let inventory = Inventory {
        problem: &model.problem,
        pure_calls: &model.pure_calls,
        parameters: &model.parameters,
    };
    let text = serde_json::to_string(&inventory)
        .map_err(|error| WasmError::new(format!("native inventory JSON failed: {error}")))?;
    if let Some(path) = path {
        std::fs::write(path, &text)
            .map_err(|error| WasmError::new(format!("native inventory write failed: {error}")))?;
    }
    Ok(text)
}

fn source_span(span: rumoca_core::Span) -> Value {
    // Human-readable diagnostics retain all SourceId bits across JS JSON readers.
    // The separate canonical Solve payload preserves its existing exact wire form.
    json!({"source":span.source.0.to_string(),"start":span.start.0,"end":span.end.0})
}

fn print_source(
    index: usize,
    node: &solve::ComputeNode,
    table: &solve::SolvePureCallTable,
) -> Result<(), WasmError> {
    match node {
        solve::ComputeNode::ScalarPrograms(block) => print_scalar(index, block, table),
        solve::ComputeNode::Map { base_ops, .. }
        | solve::ComputeNode::AffineStencil { base_ops, .. } => {
            eprintln!("NATIVE_FAMILY_SOURCE {}", family_summary(index, node));
            print_operations(index, base_ops, table)
        }
        _ => Ok(()),
    }
}

fn family_summary(index: usize, node: &solve::ComputeNode) -> Value {
    let (kind, domain, output_map, load_strides, const_strides, span) = match node {
        solve::ComputeNode::Map {
            domain,
            output_map,
            load_strides,
            const_strides,
            span,
            ..
        } => ("Map", domain, output_map, load_strides, const_strides, span),
        solve::ComputeNode::AffineStencil {
            domain,
            output_map,
            load_strides,
            const_strides,
            span,
            ..
        } => (
            "AffineStencil",
            domain,
            output_map,
            load_strides,
            const_strides,
            span,
        ),
        _ => unreachable!("family summary only accepts the matched compact family"),
    };
    json!({"source_node":index,"kind":kind,"domain":domain,"output_map":output_map,
        "load_strides":load_strides,"const_strides":const_strides,"span":source_span(*span)})
}

fn print_scalar(
    node: usize,
    block: &solve::ScalarProgramBlock,
    table: &solve::SolvePureCallTable,
) -> Result<(), WasmError> {
    for (index, ops) in block.programs().iter().enumerate() {
        eprintln!(
            "NATIVE_CALL_SOURCE program={index} outputs={} ops={} node={node}",
            solve::ScalarProgramBlock::program_output_count(ops),
            ops.len()
        );
        print_operations(node, ops, table)?;
    }
    Ok(())
}

fn print_operations(
    node: usize,
    operations: &[solve::LinearOp],
    table: &solve::SolvePureCallTable,
) -> Result<(), WasmError> {
    for (position, operation) in operations.iter().enumerate() {
        eprintln!(
            "NATIVE_CALL_OPCODE {} node={node} position={position}",
            operation.kind_name()
        );
        if let solve::LinearOp::PureCall {
            dst_start,
            input_starts,
            site,
        } = operation
        {
            let owner = table.owner(site.owner()).ok_or_else(|| {
                WasmError::new("native inventory call has no construction-issued owner")
            })?;
            eprintln!(
                "NATIVE_ISSUED_CALL {}",
                json!({"source_node":node,"operation":position,
                "owner":site.owner().index(),"dst_start":dst_start,"input_starts":input_starts,
                "inputs":site.inputs(),"outputs":site.outputs(),
                "provenance":source_span(owner.provenance())})
            );
        }
    }
    Ok(())
}
