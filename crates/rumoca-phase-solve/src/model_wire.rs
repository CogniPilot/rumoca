//! Checked replay for the canonical solver model wire.
//!
//! The wire contains only construction inputs. Executable differentiation and
//! structural artifacts are rebuilt through the same phase-owned derivation
//! used by fresh lowering before a [`solve::SolveModel`] is exposed.

use rumoca_ir_solve as solve;
use serde::{Deserialize, Serialize};

/// Require a current-wire optional key while preserving explicit `null` as
/// `None`. Naming a field deserializer suppresses Serde's implicit
/// absent-`Option` completion.
fn required_option<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer)
}

/// Current canonical `SolveModel` wire schema.
///
/// The model schema is distinct from `SOLVE_SCHEMA_VERSION`, which versions
/// the nested canonical `SolveProblem`.
pub const SOLVE_MODEL_SCHEMA_VERSION: u16 = 5;

/// Borrowed canonical construction inputs for one solver model.
///
/// Derived executable, differentiation, mass-matrix, and structural artifacts
/// are deliberately absent. Checked replay regenerates them through this
/// phase's sole derivation path.
#[derive(Debug, Serialize)]
pub struct SolveModelWireRef<'model> {
    schema_version: u16,
    problem: &'model solve::SolveProblem,
    pure_calls: &'model solve::SolvePureCallTable,
    initial_y: &'model [f64],
    solver_nominals: &'model [f64],
    parameters: &'model [f64],
    visible_value_rows: &'model solve::ScalarProgramBlock,
    variable_catalog: SolveVariableCatalogWireRef<'model>,
}

/// Borrow the canonical, artifact-free wire view of a constructed model.
///
/// The current canonical problem is explicit and therefore replays an identity
/// mass matrix. A caller-selected non-identity artifact fails closed instead
/// of being omitted and silently changing the replayed equations.
pub fn solve_model_wire(
    model: &solve::SolveModel,
) -> Result<SolveModelWireRef<'_>, SolveModelWireError> {
    if !matches!(
        model.artifacts().continuous().mass_matrix,
        solve::MassMatrix::Identity
    ) {
        return Err(SolveModelWireError::UnsupportedMassMatrix);
    }
    Ok(SolveModelWireRef {
        schema_version: SOLVE_MODEL_SCHEMA_VERSION,
        problem: model.problem(),
        pure_calls: model.pure_calls(),
        initial_y: model.initial_y(),
        solver_nominals: model.solver_nominals(),
        parameters: model.parameters(),
        visible_value_rows: model.visible_value_rows(),
        variable_catalog: variable_catalog_wire(model.variable_catalog()),
    })
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SolveModelWire {
    schema_version: u16,
    problem: solve::SolveProblem,
    pure_calls: solve::SolvePureCallTable,
    initial_y: Vec<f64>,
    solver_nominals: Vec<f64>,
    parameters: Vec<f64>,
    visible_value_rows: solve::ScalarProgramBlock,
    variable_catalog: SolveVariableCatalogWire,
}

/// Replay one canonical solver model from an arbitrary Serde input.
///
/// Use this function with `#[serde(deserialize_with = "...")]` for a model
/// nested inside a transport envelope. `SolveModel` itself intentionally does
/// not implement `Deserialize`: only this phase owns the mechanical artifact
/// derivation needed to construct the executable root.
pub fn deserialize_solve_model<'de, D>(deserializer: D) -> Result<solve::SolveModel, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let wire = SolveModelWire::deserialize(deserializer)?;
    replay_solve_model(wire).map_err(serde::de::Error::custom)
}

fn replay_solve_model(wire: SolveModelWire) -> Result<solve::SolveModel, SolveModelWireError> {
    if wire.schema_version != SOLVE_MODEL_SCHEMA_VERSION {
        return Err(SolveModelWireError::SchemaVersion {
            actual: wire.schema_version,
            expected: SOLVE_MODEL_SCHEMA_VERSION,
        });
    }
    let variable_entries = replay_variable_catalog_entries(wire.variable_catalog);
    let artifacts =
        super::artifacts::lower_solve_artifacts(&wire.problem, solve::MassMatrix::Identity)
            .map_err(|error| SolveModelWireError::ArtifactDerivation(error.to_string()))?;
    solve::SolveModel::construct(
        wire.problem,
        wire.pure_calls,
        artifacts,
        solve::SolveModelRuntimeInputs {
            initial_y: wire.initial_y,
            solver_nominals: wire.solver_nominals,
            parameters: wire.parameters,
        },
        wire.visible_value_rows,
        variable_entries,
    )
    .map_err(SolveModelWireError::Construction)
}

#[derive(Debug, Serialize)]
struct SolveVariableCatalogWireRef<'catalog> {
    entries: Vec<SolveVariableCatalogEntryWireRef<'catalog>>,
}

#[derive(Debug, Serialize)]
struct SolveVariableCatalogEntryWireRef<'entry> {
    source_occurrence: rumoca_core::SourceOccurrenceId,
    name: &'entry str,
    dimensions: &'entry [u32],
    scalar_names: &'entry [String],
    provenance: rumoca_core::Span,
    causality: solve::SolveVariableCausality,
    variability: solve::SolveVariableVariability,
    tunable: bool,
    unit: Option<&'entry str>,
    description: Option<&'entry str>,
    fixed: rumoca_core::Fixity,
    start: Option<&'entry [f64]>,
    minimum: Option<&'entry [f64]>,
    maximum: Option<&'entry [f64]>,
    nominal: Option<&'entry [f64]>,
}

fn variable_catalog_wire(catalog: &solve::SolveVariableCatalog) -> SolveVariableCatalogWireRef<'_> {
    SolveVariableCatalogWireRef {
        entries: catalog
            .entries()
            .iter()
            .map(|entry| SolveVariableCatalogEntryWireRef {
                source_occurrence: entry.source_occurrence(),
                name: entry.name(),
                dimensions: entry.dimensions(),
                scalar_names: entry.scalar_names(),
                provenance: entry.provenance(),
                causality: entry.causality(),
                variability: entry.variability(),
                tunable: entry.is_tunable(),
                unit: entry.unit(),
                description: entry.description(),
                fixed: entry.fixed(),
                start: entry.start(),
                minimum: entry.minimum(),
                maximum: entry.maximum(),
                nominal: entry.nominal(),
            })
            .collect(),
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SolveVariableCatalogWire {
    entries: Vec<SolveVariableCatalogEntryWire>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SolveVariableCatalogEntryWire {
    source_occurrence: rumoca_core::SourceOccurrenceId,
    name: String,
    dimensions: Vec<u32>,
    scalar_names: Vec<String>,
    provenance: rumoca_core::Span,
    causality: solve::SolveVariableCausality,
    variability: solve::SolveVariableVariability,
    tunable: bool,
    #[serde(deserialize_with = "required_option")]
    unit: Option<String>,
    #[serde(deserialize_with = "required_option")]
    description: Option<String>,
    fixed: rumoca_core::Fixity,
    #[serde(deserialize_with = "required_option")]
    start: Option<Vec<f64>>,
    #[serde(deserialize_with = "required_option")]
    minimum: Option<Vec<f64>>,
    #[serde(deserialize_with = "required_option")]
    maximum: Option<Vec<f64>>,
    #[serde(deserialize_with = "required_option")]
    nominal: Option<Vec<f64>>,
}

fn replay_variable_catalog_entries(
    wire: SolveVariableCatalogWire,
) -> Vec<solve::SolveVariableCatalogSourceEntry> {
    wire.entries
        .into_iter()
        .map(|entry| {
            let source = solve::SolveVariableSource::new(
                entry.source_occurrence,
                entry.name,
                entry.dimensions,
                entry.scalar_names,
                entry.provenance,
            );
            let attributes = solve::SolveVariableSourceAttributes::new(
                entry.causality,
                entry.variability,
                entry.tunable,
                entry.unit,
                entry.description,
                entry.fixed,
            );
            let values = solve::SolveVariableEvaluatedValues::new(
                entry.start,
                entry.minimum,
                entry.maximum,
                entry.nominal,
            );
            (source, attributes, values)
        })
        .collect()
}

#[derive(Debug)]
pub enum SolveModelWireError {
    SchemaVersion { actual: u16, expected: u16 },
    UnsupportedMassMatrix,
    ArtifactDerivation(String),
    Construction(solve::SolveModelConstructionError),
}

impl std::fmt::Display for SolveModelWireError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SchemaVersion { actual, expected } => write!(
                formatter,
                "unsupported SolveModel schema_version {actual}; expected {expected}"
            ),
            Self::UnsupportedMassMatrix => formatter.write_str(
                "canonical SolveModel wire cannot encode a caller-selected non-identity mass matrix",
            ),
            Self::ArtifactDerivation(error) => {
                write!(
                    formatter,
                    "failed to reconstruct SolveModel artifacts: {error}"
                )
            }
            Self::Construction(error) => {
                write!(formatter, "replayed SolveModel is invalid: {error}")
            }
        }
    }
}

impl std::error::Error for SolveModelWireError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn variable_catalog_wire_requires_every_optional_key_but_accepts_null() {
        let complete = serde_json::json!({
            "source_occurrence": 1,
            "name": "x",
            "dimensions": [],
            "scalar_names": ["x"],
            "provenance": rumoca_core::Span::DUMMY,
            "causality": "local",
            "variability": "continuous",
            "tunable": false,
            "unit": null,
            "description": null,
            "fixed": false,
            "start": null,
            "minimum": null,
            "maximum": null,
            "nominal": null,
        });

        serde_json::from_value::<SolveVariableCatalogEntryWire>(complete.clone())
            .expect("explicit null is valid current-wire absence");

        for field in [
            "unit",
            "description",
            "fixed",
            "start",
            "minimum",
            "maximum",
            "nominal",
        ] {
            let mut missing = complete.clone();
            missing
                .as_object_mut()
                .expect("catalog entry is an object")
                .remove(field)
                .expect("fixture contains every required optional key");
            assert!(
                serde_json::from_value::<SolveVariableCatalogEntryWire>(missing).is_err(),
                "current Solve variable-catalog wire must reject omitted `{field}`"
            );
        }

        let mut null_fixed = complete;
        null_fixed
            .as_object_mut()
            .expect("catalog entry is an object")
            .insert("fixed".to_string(), serde_json::Value::Null);
        assert!(
            serde_json::from_value::<SolveVariableCatalogEntryWire>(null_fixed).is_err(),
            "the Solve variable-catalog wire carries a total `fixed`; null is not a value"
        );
    }

    #[test]
    fn variable_catalog_wire_requires_a_nonzero_source_occurrence() {
        let complete = serde_json::json!({
            "source_occurrence": 1,
            "name": "x",
            "dimensions": [],
            "scalar_names": ["x"],
            "provenance": rumoca_core::Span::DUMMY,
            "causality": "local",
            "variability": "continuous",
            "tunable": false,
            "unit": null,
            "description": null,
            "fixed": false,
            "start": null,
            "minimum": null,
            "maximum": null,
            "nominal": null,
        });

        let mut missing = complete.clone();
        missing
            .as_object_mut()
            .expect("catalog entry is an object")
            .remove("source_occurrence")
            .expect("fixture carries source occurrence");
        assert!(serde_json::from_value::<SolveVariableCatalogEntryWire>(missing).is_err());

        let mut zero = complete;
        zero["source_occurrence"] = serde_json::json!(0);
        assert!(serde_json::from_value::<SolveVariableCatalogEntryWire>(zero).is_err());
    }
}
