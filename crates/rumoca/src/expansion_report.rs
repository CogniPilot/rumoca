//! Versioned JSON for opt-in native emission diagnostics, not runtime samples.

use anyhow::{Context, Result};
use rumoca_sim::native_expansion::{
    ExpansionEntry, ExpansionReport, ExpansionSource, ForcedExpansion,
};
use serde_json::{Value, json};

fn entry(entry: &ExpansionEntry) -> Value {
    let source = match &entry.source {
        ExpansionSource::Typed { program, operation } => {
            json!({ "typed_program": program, "operation": operation })
        }
        ExpansionSource::Row(operation) => {
            let constant_bits = if let rumoca_ir_solve::LinearOp::Const { value, .. } = operation {
                Some(format!("{:016x}", value.to_bits()))
            } else {
                None
            };
            json!({ "row_operation": operation, "constant_f64_bits": constant_bits })
        }
        ExpansionSource::AssignmentOutputs(row) => json!({ "assignment_row": row }),
        ExpansionSource::FunctionFold(program) => json!({ "row_fold": program }),
    };
    let forced = entry.forced.map(|reason| match reason {
        ForcedExpansion::StaticUnroll { limit } => json!({ "static_unroll_limit": limit }),
    });
    json!({
        "source": source, "phase": entry.phase, "call_table": entry.call_table, "kind": entry.kind, "parent_entry": entry.parent,
        "span": entry.span.map(|span| json!({"source": span.source.0,
            "start": span.start.0, "end": span.end.0})),
        "elements": entry.elements, "clif_instructions_allocated_excluding_children": entry.instructions,
        "loop_capacity_bounds": entry.loop_bounds, "chosen_path": entry.path,
        "fused_source_operations": entry.fused_operations, "forced": forced,
        "lowering_succeeded": entry.succeeded,
        "legacy_single_shape_suspect": entry.is_suspect(),
        "legacy_unforced_suspect": entry.is_unforced_suspect(),
        "copies": entry.copies.iter().map(|copy| json!({
            "cells": copy.cells, "method": copy.method,
            "loop_capacity_bounds": copy.loop_bounds,
            "capacity_weighted_cells_upper_bound": copy.capacity_cells(),
        })).collect::<Vec<_>>(),
    })
}

fn document(report: &ExpansionReport, invocation_error: Option<&str>) -> Value {
    json!({
        "schema": "rumoca-native-expansion-v2",
        "scope": "calling-thread CLIF emission; machine-code compilation is excluded",
        "units": "allocated CLIF instructions before optimization and logical copied cells, not time or dynamic counts",
        "classification": "legacy single-shape heuristic, not a scaling proof",
        "instrumented_preparation_timings": true,
        "floor": rumoca_sim::native_expansion::EXPANSION_FLOOR,
        "collection_complete": report.complete,
        "nonempty": !report.entries.is_empty(),
        "invocation_error": invocation_error,
        "unattributed_copy_sites": report.unattributed_copy_sites,
        "pure_call_tables": report.tables,
        "program_tables": report.program_tables,
        "programs": report.programs,
        "entries": report.entries.iter().map(entry).collect::<Vec<_>>(),
    })
}

pub(crate) fn write(
    path: &str,
    report: &ExpansionReport,
    invocation_error: Option<&str>,
) -> Result<()> {
    std::fs::write(
        path,
        serde_json::to_vec_pretty(&document(report, invocation_error))?,
    )
    .with_context(|| format!("writing native expansion report to {path}"))?;
    eprintln!(
        "native emission report: {} source operations, {} raw suspects, complete={} -> {path}",
        report.entries.len(),
        report.suspects().count(),
        report.complete
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_sim::native_expansion::{ExpansionCopy, record_expansion_report};

    #[test]
    fn missing_failed_and_empty_reports_cannot_read_as_success() {
        let report = ExpansionReport::default();
        let value = document(&report, Some("compile refused"));
        assert_eq!(value["collection_complete"], false);
        assert_eq!(value["nonempty"], false);
        assert_eq!(value["invocation_error"], "compile refused");
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("failed.json");
        write(path.to_str().unwrap(), &report, Some("compile refused")).unwrap();
        let saved: Value = serde_json::from_slice(&std::fs::read(path).unwrap()).unwrap();
        assert_eq!(saved, value);
        let absent_parent = directory.path().join("absent/report.json");
        assert!(write(absent_parent.to_str().unwrap(), &report, None).is_err());
        let ((), empty) = record_expansion_report(|| {});
        assert_eq!(document(&empty, None)["nonempty"], false);
    }

    #[test]
    fn static_counts_and_capacity_upper_bounds_remain_distinct() {
        let value = entry(&ExpansionEntry {
            source: ExpansionSource::AssignmentOutputs(Vec::new()),
            phase: "definition",
            call_table: None,
            kind: "AssignmentOutputs",
            parent: None,
            span: None,
            elements: 128,
            instructions: 12,
            loop_bounds: vec![64],
            forced: None,
            succeeded: true,
            path: "ordinary",
            fused_operations: Vec::new(),
            copies: vec![ExpansionCopy {
                cells: 128,
                method: "memmove",
                loop_bounds: vec![64],
            }],
        });
        assert_eq!(value["clif_instructions_allocated_excluding_children"], 12);
        assert_eq!(value["copies"][0]["cells"], 128);
        assert_eq!(
            value["copies"][0]["capacity_weighted_cells_upper_bound"],
            8192
        );
        assert_eq!(value["legacy_unforced_suspect"], true);
    }
}
