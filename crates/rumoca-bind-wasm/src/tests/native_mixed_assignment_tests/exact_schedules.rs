//! Opt-in actual full plant issuance, complete-sequence execution and export.
mod execution;
mod fixtures;
mod inventory;

use super::*;
use rumoca_ir_solve as solve;
use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use std::{collections::BTreeSet, path::Path, time::Instant};

#[test]
#[ignore = "requires original complete LabQuadrotor through RUMOCA_EXACT_SOURCE_FIXTURE"]
fn native_exact_physics_full_source_diagnostic() {
    let _lock = session_test_guard();
    let source =
        std::fs::read_to_string(std::env::var("RUMOCA_EXACT_SOURCE_FIXTURE").unwrap()).unwrap();
    assert_eq!(
        digest(source.as_bytes()),
        "945690a6db098b680ad360edb51ab8dd8da4fcf6215d2900d4be05d8ac898658",
        "unchanged full original plant source required"
    );
    let directory = std::env::var("RUMOCA_EXACT_OUTPUT_DIRECTORY").unwrap();
    let directory = Path::new(&directory);
    std::fs::create_dir_all(directory).unwrap();
    let needle = "parameter Real mass = 2.0;";
    assert_eq!(
        source.matches(needle).count(),
        1,
        "original lab wrapper required"
    );
    let edited = source.replacen(needle, "parameter Real mass = 2.4;", 1);
    let binary = std::fs::read(std::env::current_exe().unwrap()).unwrap();
    let producer_source = std::env::var("RUMOCA_EXACT_PRODUCER_SOURCE_SHA256")
        .expect("exact frozen source manifest SHA required");
    assert!(
        producer_source.len() == 64 && producer_source.bytes().all(|b| b.is_ascii_hexdigit()),
        "producer source manifest must be an exact SHA256 digest"
    );
    let producer = json!({"binary_sha256":digest(&binary),
        "source_sha256":producer_source,
        "revision":std::env::var("RUMOCA_EXACT_PRODUCER_REVISION").expect("exact producer revision required")});
    std::fs::write(
        directory.join("producer.json"),
        serde_json::to_vec_pretty(&producer).unwrap(),
    )
    .unwrap();
    let mut reports = Vec::new();
    for (variant, text) in [("baseline", &source), ("mass-2.4", &edited)] {
        let path = directory.join(variant);
        std::fs::create_dir_all(&path).unwrap();
        std::fs::write(path.join("source.mo"), text).unwrap();
        let start = Instant::now();
        let result = crate::native_assignment_api::with_prepared_native_model(
            text,
            "LabQuadrotor",
            |model, source, _| {
                let report = inventory::inspect(model, source, &path);
                Ok(serde_json::to_string(&report).unwrap())
            },
        )
        .unwrap();
        let mut report: Value = serde_json::from_str(&result).unwrap();
        report["preparation_and_execution_ms"] = json!(start.elapsed().as_secs_f64() * 1000.);
        std::fs::write(
            path.join("report.json"),
            serde_json::to_vec_pretty(&report).unwrap(),
        )
        .unwrap();
        reports.push(report);
    }
    let changed = native_source_edit_observed(&reports[0], &reports[1]);
    let report = json!({"producer":producer,"variants":reports,"native_source_edit_observed":changed,
        "scope":"Full original source and every issued dispatch; refused sequences remain refused, never replaced by admitted-only execution"});
    std::fs::write(
        directory.join("report.json"),
        serde_json::to_vec_pretty(&report).unwrap(),
    )
    .unwrap();
    for variant in report["variants"].as_array().unwrap() {
        assert!(
            variant["admitted_sequences"].as_u64().unwrap() > 0,
            "no actual source sequence admitted"
        );
        assert_eq!(variant["numerical_failures"], 0);
    }
    assert!(
        changed,
        "admitted native outputs did not yet demonstrate the actual mass source edit"
    );
}

fn digest(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

fn native_source_edit_observed(baseline: &Value, edited: &Value) -> bool {
    baseline["sequences"]
        .as_array()
        .unwrap()
        .iter()
        .zip(edited["sequences"].as_array().unwrap())
        .any(|(a, b)| {
            a["admitted"] == true
                && b["admitted"] == true
                && a["cases"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .zip(b["cases"].as_array().unwrap())
                    .any(|(x, y)| x["actual_y_sha256"] != y["actual_y_sha256"])
        })
}
