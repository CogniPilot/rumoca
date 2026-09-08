use std::collections::BTreeMap;
use std::num::NonZeroUsize;

use anyhow::{Context as _, Result, ensure};
use serde::de::{Error as _, IgnoredAny};
use serde::ser::{SerializeMap as _, SerializeSeq as _};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use super::CandidateOnlyRegistryAdmission;

const AGGREGATE_REPORT_SCHEMA_VERSION: u32 = 2;

/// The immutable schema-2 report for a registry containing candidates only.
///
/// Its admission-bearing fields have no setters or mutable accessors. The sole
/// constructor consumes the checked registry admission capability and fixes
/// every numerator field to its only valid candidate-only value.
#[derive(Serialize)]
#[serde(transparent)]
pub struct CandidateOnlyAggregateReport {
    wire: AggregateReportWire,
}

/// Checked denominator/history facts for aggregate construction. Nonzero
/// denominators are represented by type, and the only fallible arithmetic
/// normalization occurs before the final report constructor consumes this
/// product.
pub struct AggregateDenominatorHistory {
    current: NonZeroUsize,
    previous: Option<NonZeroUsize>,
    delta: Option<i64>,
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct AggregateReportWire {
    aggregate_schema_version: u32,
    #[serde(
        serialize_with = "serialize_empty_sequence",
        deserialize_with = "deserialize_empty_sequence"
    )]
    admitted_models: (),
    #[serde(
        serialize_with = "serialize_empty_map",
        deserialize_with = "deserialize_empty_map"
    )]
    per_model_covered_lines: (),
    #[serde(
        serialize_with = "serialize_empty_map",
        deserialize_with = "deserialize_empty_map"
    )]
    marginal_covered_lines: (),
    #[serde(
        serialize_with = "serialize_zero_usize",
        deserialize_with = "deserialize_zero_usize"
    )]
    union_covered_lines: (),
    workspace_instrumentable_production_lines: NonZeroUsize,
    previous_workspace_instrumentable_production_lines: Option<NonZeroUsize>,
    denominator_line_delta: Option<i64>,
    #[serde(
        serialize_with = "serialize_zero_f64",
        deserialize_with = "deserialize_zero_f64"
    )]
    golden_line_coverage_percent: (),
}

pub fn checked_aggregate_denominator_history(
    current: NonZeroUsize,
    previous: Option<NonZeroUsize>,
) -> Result<AggregateDenominatorHistory> {
    let delta = previous
        .map(|previous| signed_line_delta(current, previous))
        .transpose()?;
    Ok(AggregateDenominatorHistory {
        current,
        previous,
        delta,
    })
}

pub fn candidate_only_aggregate_report(
    _admission: CandidateOnlyRegistryAdmission,
    history: AggregateDenominatorHistory,
) -> CandidateOnlyAggregateReport {
    CandidateOnlyAggregateReport {
        wire: AggregateReportWire {
            aggregate_schema_version: AGGREGATE_REPORT_SCHEMA_VERSION,
            admitted_models: (),
            per_model_covered_lines: (),
            marginal_covered_lines: (),
            union_covered_lines: (),
            workspace_instrumentable_production_lines: history.current,
            previous_workspace_instrumentable_production_lines: history.previous,
            denominator_line_delta: history.delta,
            golden_line_coverage_percent: (),
        },
    }
}

/// Read only the denominator history accepted from a prior candidate-only
/// report. Tampered admission fields are rejected even though they cannot
/// influence the new report.
pub fn parse_candidate_only_aggregate_denominator(
    source: &str,
    location: &str,
) -> Result<NonZeroUsize> {
    let report: AggregateReportWire =
        serde_json::from_str(source).with_context(|| format!("failed to parse {location}"))?;
    ensure!(
        report.aggregate_schema_version == AGGREGATE_REPORT_SCHEMA_VERSION,
        "prior aggregate report uses schema {}; expected exactly schema {AGGREGATE_REPORT_SCHEMA_VERSION}",
        report.aggregate_schema_version
    );
    Ok(report.workspace_instrumentable_production_lines)
}

impl CandidateOnlyAggregateReport {
    pub fn admitted_model_count(&self) -> usize {
        0
    }

    pub fn union_covered_lines(&self) -> usize {
        0
    }

    pub fn workspace_instrumentable_production_lines(&self) -> NonZeroUsize {
        self.wire.workspace_instrumentable_production_lines
    }

    pub fn previous_workspace_instrumentable_production_lines(&self) -> Option<NonZeroUsize> {
        self.wire.previous_workspace_instrumentable_production_lines
    }

    pub fn denominator_line_delta(&self) -> Option<i64> {
        self.wire.denominator_line_delta
    }

    pub fn golden_line_coverage_percent(&self) -> f64 {
        0.0
    }
}

fn signed_line_delta(current: NonZeroUsize, previous: NonZeroUsize) -> Result<i64> {
    let current = i64::try_from(current.get()).context("current denominator does not fit i64")?;
    let previous =
        i64::try_from(previous.get()).context("previous denominator does not fit i64")?;
    Ok(current - previous)
}

fn serialize_empty_sequence<S>(_: &(), serializer: S) -> std::result::Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_seq(Some(0))?.end()
}

fn deserialize_empty_sequence<'de, D>(deserializer: D) -> std::result::Result<(), D::Error>
where
    D: Deserializer<'de>,
{
    let values = Vec::<IgnoredAny>::deserialize(deserializer)?;
    if values.is_empty() {
        Ok(())
    } else {
        Err(D::Error::custom(
            "candidate-only admitted-model sequence is not empty",
        ))
    }
}

fn serialize_empty_map<S>(_: &(), serializer: S) -> std::result::Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_map(Some(0))?.end()
}

fn deserialize_empty_map<'de, D>(deserializer: D) -> std::result::Result<(), D::Error>
where
    D: Deserializer<'de>,
{
    let values = BTreeMap::<String, IgnoredAny>::deserialize(deserializer)?;
    if values.is_empty() {
        Ok(())
    } else {
        Err(D::Error::custom(
            "candidate-only per-model coverage map is not empty",
        ))
    }
}

fn serialize_zero_usize<S>(_: &(), serializer: S) -> std::result::Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_u64(0)
}

fn deserialize_zero_usize<'de, D>(deserializer: D) -> std::result::Result<(), D::Error>
where
    D: Deserializer<'de>,
{
    match usize::deserialize(deserializer)? {
        0 => Ok(()),
        _ => Err(D::Error::custom(
            "candidate-only aggregate numerator is not zero",
        )),
    }
}

fn serialize_zero_f64<S>(_: &(), serializer: S) -> std::result::Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_f64(0.0)
}

fn deserialize_zero_f64<'de, D>(deserializer: D) -> std::result::Result<(), D::Error>
where
    D: Deserializer<'de>,
{
    match f64::deserialize(deserializer)? {
        0.0 => Ok(()),
        _ => Err(D::Error::custom(
            "candidate-only aggregate coverage percent is not zero",
        )),
    }
}
