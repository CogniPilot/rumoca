use super::super::{RuntimeRatioStats, TraceAccuracyStats};
use super::{CohortEvidence, CohortSnapshot, cohort_comparison};
use serde::Deserialize;
use serde_json::Value;
use std::path::Path;

/// One side of a shared case, read as the resolver reads a baseline.
#[derive(Deserialize)]
struct CaseSnapshot {
    trace_accuracy_stats: TraceAccuracyStats,
    runtime_ratio_stats: RuntimeRatioStats,
    #[serde(flatten)]
    evidence: CohortEvidence,
}

impl CaseSnapshot {
    fn view(&self) -> CohortSnapshot<'_> {
        CohortSnapshot {
            evidence: &self.evidence,
            trace: &self.trace_accuracy_stats,
            runtime: &self.runtime_ratio_stats,
        }
    }
}

fn shared_cases() -> Value {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../.github/scripts/msl-baseline-cohort-cases.json");
    serde_json::from_slice(&std::fs::read(path).unwrap()).unwrap()
}

/// RFC 7396 JSON merge patch: objects merge, null deletes, anything else
/// replaces.
fn merge_patch(target: &mut Value, patch: &Value) {
    let Some(patch) = patch.as_object() else {
        *target = patch.clone();
        return;
    };
    if !target.is_object() {
        *target = Value::Object(serde_json::Map::new());
    }
    let target = target.as_object_mut().unwrap();
    for (key, value) in patch {
        if value.is_null() {
            target.remove(key);
        } else {
            merge_patch(target.entry(key.clone()).or_insert(Value::Null), value);
        }
    }
}

fn case_snapshot(cases: &Value, side: &Value) -> CaseSnapshot {
    let mut snapshot = cases["base"].clone();
    let patches = side
        .as_array()
        .cloned()
        .unwrap_or_else(|| vec![side.clone()]);
    for patch in &patches {
        let patch = patch.as_str().map_or(patch, |name| &cases[name]);
        merge_patch(&mut snapshot, patch);
    }
    serde_json::from_value(snapshot).unwrap()
}

fn labels(lines: &[String]) -> Vec<String> {
    lines
        .iter()
        .map(|line| {
            let label = line.split(':').next().unwrap();
            label.split(" over ").next().unwrap().to_string()
        })
        .collect()
}

fn expected(case: &Value, key: &str) -> Vec<String> {
    case[key]
        .as_array()
        .unwrap()
        .iter()
        .map(|label| label.as_str().unwrap().to_string())
        .collect()
}

#[test]
fn resolver_and_ratchet_reach_the_shared_cohort_verdicts() {
    let cases = shared_cases();
    let all = cases["cases"].as_array().unwrap();
    assert!(all.len() >= 10, "shared cases went missing");
    for case in all {
        let name = case["name"].as_str().unwrap();
        let reference = case_snapshot(&cases, &case["reference"]);
        let candidate = case_snapshot(&cases, &case["candidate"]);
        let result = cohort_comparison(reference.view(), candidate.view());
        if let Some(error) = case["error"].as_str() {
            let message = format!("{:#}", result.expect_err(name));
            assert!(message.contains(error), "{name}: {message}");
            continue;
        }
        let verdict = result.unwrap_or_else(|error| panic!("{name}: {error:#}"));
        assert_eq!(
            labels(&verdict.regressions),
            expected(case, "regressions"),
            "{name}"
        );
        assert_eq!(
            labels(&verdict.improvements),
            expected(case, "improvements"),
            "{name}"
        );
    }
}
