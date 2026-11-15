//! Complete current-version FMI wire over the checked Solve replay owner.

use rumoca_ir_solve::fmi::{CoSimulationStepPlan, FmiComponent, FmiVariableInput};
use serde::{Deserialize, Serialize};

/// The canonical model writer refused a component's retained checked root.
#[derive(Debug)]
pub enum FmiWireError {
    Wire(crate::model_wire::SolveModelWireError),
}

impl std::fmt::Display for FmiWireError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Wire(error) => write!(formatter, "FMI component wire is invalid: {error}"),
        }
    }
}

impl std::error::Error for FmiWireError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Wire(error) => Some(error),
        }
    }
}

/// Current FMI component wire schema; every other version is rejected.
///
/// 2 carries each variable's declared `input`/`output` prefix where it differs
/// from the exported causality. 3 replays into a component whose
/// `RootLocationPlan` refines state-event brackets by its Illinois minmax
/// method rather than bisection and applies every domain change within one
/// location tolerance after the located left limit together
/// (`RootTieBreak::ToleranceWindow`, SPEC_0044 ME-EVENT-004), so a replayed
/// wire locates and groups its events as the compiler that wrote it does.
/// 5 retains declaration-shaped scalar names and source-issued numeric runs;
/// replay checks their logical extents against the same Solve storage catalog.
/// 6 replaces Float64 starts with one exact typed declaration-start authority.
/// 7 retains distinct effective input defaults and exact supplied/fallback evidence.
/// A component whose Co-Simulation step rule is not the standard one also
/// carries it (`co_simulation`); the field is absent otherwise, so a standard
/// component's wire is unchanged and replays into the standard rule.
pub const FMI_COMPONENT_SCHEMA_VERSION: u16 = 7;

/// Borrowed, canonical construction inputs for one correlated FMI component.
#[derive(Serialize)]
pub struct FmiComponentWireRef<'model> {
    schema_version: u16,
    solve_model: crate::model_wire::SolveModelWireRef<'model>,
    variables: Vec<FmiVariableInput>,
    #[serde(skip_serializing_if = "Option::is_none")]
    co_simulation: Option<CoSimulationStepPlan>,
}

/// Encode an already checked retained component without pairing independent
/// metadata and model arguments. Replay uses the existing current-wire reader.
pub fn retained_fmi_component_wire(
    root: &rumoca_ir_solve::RetainedAssertionRoot,
) -> Result<FmiComponentWireRef<'_>, FmiWireError> {
    let solve_model =
        crate::model_wire::solve_model_wire(root.model()).map_err(FmiWireError::Wire)?;
    let plan = *root.co_simulation();
    Ok(FmiComponentWireRef {
        schema_version: FMI_COMPONENT_SCHEMA_VERSION,
        solve_model,
        variables: root.fmi_declaration_inputs().collect(),
        co_simulation: (plan != CoSimulationStepPlan::STANDARD).then_some(plan),
    })
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FmiComponentWire {
    schema_version: u16,
    #[serde(deserialize_with = "crate::model_wire::deserialize_solve_model")]
    solve_model: rumoca_ir_solve::SolveModel,
    variables: Vec<FmiVariableInput>,
    #[serde(default)]
    co_simulation: Option<CoSimulationStepPlan>,
}

/// Replay the canonical FMI wire through the same checked component
/// construction used by fresh lowering.
pub fn deserialize_fmi_component<'de, D>(deserializer: D) -> Result<FmiComponent, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let wire = FmiComponentWire::deserialize(deserializer)?;
    if wire.schema_version != FMI_COMPONENT_SCHEMA_VERSION {
        return Err(serde::de::Error::custom(format!(
            "unsupported FMI component schema {}; expected {}",
            wire.schema_version, FMI_COMPONENT_SCHEMA_VERSION
        )));
    }
    let plan = match wire.co_simulation {
        None => CoSimulationStepPlan::STANDARD,
        Some(plan) if plan == CoSimulationStepPlan::STANDARD => {
            return Err(serde::de::Error::custom(
                "the standard Co-Simulation step plan is written by omission",
            ));
        }
        Some(plan) => plan,
    };
    FmiComponent::construct(wire.solve_model, wire.variables)
        .map_err(serde::de::Error::custom)?
        .with_co_simulation(plan)
        .map_err(serde::de::Error::custom)
}

/// Bytes of each wire shown from the first difference on.
const DIFFERENCE_WINDOW: usize = 120;
/// Innermost path segments shown; deeper prefixes are counted, not printed.
const DIFFERENCE_PATH_SEGMENTS: usize = 12;

/// Where two canonical wires first differ, without parsing either: when both
/// come from this module's serializer, so their first differing byte is the first
/// difference in document order. The JSON path to it is read from the saved
/// prefix by a flat scan (expression trees are deeper than any stack allows
/// recursion for), and both wires are shown as bounded byte windows.
pub fn first_wire_difference(saved: &[u8], packaged: &[u8]) -> String {
    let offset = saved
        .iter()
        .zip(packaged)
        .position(|(a, b)| a != b)
        .unwrap_or_else(|| saved.len().min(packaged.len()));
    let segments = json_path_at(&saved[..offset]);
    let skipped = segments.len().saturating_sub(DIFFERENCE_PATH_SEGMENTS);
    let tail = segments[skipped..].join("/");
    let path = if skipped == 0 {
        format!("/{tail}")
    } else {
        format!("/...({skipped} more levels)/{tail}")
    };
    format!(
        "at {path}: saved {} packaged {}",
        byte_window(saved, offset),
        byte_window(packaged, offset)
    )
}

fn byte_window(bytes: &[u8], from: usize) -> String {
    let end = from.saturating_add(DIFFERENCE_WINDOW).min(bytes.len());
    let text = String::from_utf8_lossy(&bytes[from.min(end)..end]);
    if end < bytes.len() {
        format!("{text}...")
    } else {
        text.into_owned()
    }
}

/// The object keys and array indices open at the end of `prefix`, a prefix of
/// one compact JSON document, outermost first. Strings are skipped with their
/// escapes, so brackets and commas inside them do not count.
fn json_path_at(prefix: &[u8]) -> Vec<String> {
    enum Open {
        Object(Option<String>),
        Array(usize),
    }
    let mut open = Vec::new();
    let mut at = 0;
    while at < prefix.len() {
        match prefix[at] {
            b'{' => open.push(Open::Object(None)),
            b'[' => open.push(Open::Array(0)),
            b'}' | b']' => {
                open.pop();
            }
            b',' => {
                if let Some(Open::Array(index)) = open.last_mut() {
                    *index += 1;
                }
            }
            b'"' => {
                let start = at + 1;
                at = start;
                while at < prefix.len() && prefix[at] != b'"' {
                    at += if prefix[at] == b'\\' { 2 } else { 1 };
                }
                let end = at.min(prefix.len());
                if prefix.get(at + 1) == Some(&b':')
                    && let Some(Open::Object(key)) = open.last_mut()
                {
                    *key = Some(String::from_utf8_lossy(&prefix[start..end]).into_owned());
                }
            }
            _ => {}
        }
        at += 1;
    }
    open.into_iter()
        .map(|entry| match entry {
            Open::Object(key) => key.unwrap_or_default(),
            Open::Array(index) => index.to_string(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model_wire::SolveModelWireError;
    use std::error::Error;

    #[test]
    fn a_component_wire_refusal_names_and_chains_the_model_wire_error() {
        let error = FmiWireError::Wire(SolveModelWireError::UnsupportedMassMatrix);
        let inner = SolveModelWireError::UnsupportedMassMatrix.to_string();
        assert_eq!(
            error.to_string(),
            format!("FMI component wire is invalid: {inner}")
        );
        assert_eq!(error.source().unwrap().to_string(), inner);
    }

    #[test]
    fn the_first_differing_path_is_named_in_document_order() {
        let saved = br#"{"a":1,"b":{"c":[1,2,{"d":"x"}],"e":5}}"#;
        let packaged = br#"{"a":1,"b":{"c":[1,2,{"d":"y"}],"e":6}}"#;
        assert_eq!(
            first_wire_difference(saved, packaged),
            r#"at /b/c/2/d: saved x"}],"e":5}} packaged y"}],"e":6}}"#
        );
        assert_eq!(
            first_wire_difference(
                br#"{"k":"a,]\"b","v":[1,2]}"#,
                br#"{"k":"a,]\"b","v":[1,3]}"#
            ),
            "at /v/1: saved 2]} packaged 3]}"
        );
        assert_eq!(
            first_wire_difference(b"[1,2]", b"[1,2,3]"),
            "at /1: saved ] packaged ,3]"
        );
    }

    /// A difference at the bottom of a nesting far deeper than any thread
    /// stack is found without recursion, and the message stays bounded.
    #[test]
    fn a_difference_under_deep_nesting_is_reported_without_recursion_and_bounded() {
        const DEPTH: usize = 200_000;
        let nested = |leaf: &str| format!("{}{leaf}{}", "[".repeat(DEPTH), "]".repeat(DEPTH));
        let report = first_wire_difference(nested("1").as_bytes(), nested("2").as_bytes());
        assert!(
            report.starts_with("at /...(199988 more levels)/0/0/0/0/0/0/0/0/0/0/0/0: saved 1]"),
            "{}",
            &report[..report.len().min(200)]
        );
        assert!(report.len() < 400, "{}", report.len());
    }
}
