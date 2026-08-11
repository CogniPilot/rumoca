//! Checked replay for the canonical solver model wire.
//!
//! The wire contains only construction inputs. Executable differentiation and
//! structural artifacts are rebuilt through the same phase-owned derivation
//! used by fresh lowering before a [`solve::SolveModel`] is exposed.

use rumoca_ir_solve as solve;
use serde::{Deserialize, Serialize};

/// Current canonical `SolveModel` wire schema.
///
/// The model schema is distinct from `SOLVE_SCHEMA_VERSION`, which versions
/// the nested canonical `SolveProblem`.
pub const SOLVE_MODEL_SCHEMA_VERSION: u16 = 1;

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
    external_tables: &'model solve::ExternalTables,
    visible_names: &'model [String],
    visible_value_rows: &'model solve::ScalarProgramBlock,
    variable_meta: &'model [solve::SolveVariableMeta],
}

/// Borrow the canonical, artifact-free wire view of a constructed model.
///
/// The current canonical problem is explicit and therefore replays an identity
/// mass matrix. A caller-selected non-identity artifact fails closed instead
/// of being omitted and silently changing the replayed equations.
pub fn solve_model_wire(
    model: &solve::SolveModel,
) -> Result<SolveModelWireRef<'_>, SolveModelWireError> {
    model
        .validate()
        .map_err(|error| SolveModelWireError::Root(error.to_string()))?;
    if !matches!(
        model.artifacts.continuous.mass_matrix,
        solve::MassMatrix::Identity
    ) {
        return Err(SolveModelWireError::UnsupportedMassMatrix);
    }
    validate_correlations(CorrelationView {
        problem: &model.problem,
        initial_y: &model.initial_y,
        solver_nominals: &model.solver_nominals,
        parameters: &model.parameters,
        visible_names: &model.visible_names,
        visible_value_rows: &model.visible_value_rows,
        variable_meta: &model.variable_meta,
    })?;
    Ok(SolveModelWireRef {
        schema_version: SOLVE_MODEL_SCHEMA_VERSION,
        problem: &model.problem,
        pure_calls: &model.pure_calls,
        initial_y: &model.initial_y,
        solver_nominals: &model.solver_nominals,
        parameters: &model.parameters,
        external_tables: &model.external_tables,
        visible_names: &model.visible_names,
        visible_value_rows: &model.visible_value_rows,
        variable_meta: &model.variable_meta,
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
    external_tables: solve::ExternalTables,
    visible_names: Vec<String>,
    visible_value_rows: solve::ScalarProgramBlock,
    variable_meta: Vec<solve::SolveVariableMeta>,
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
    validate_correlations(CorrelationView {
        problem: &wire.problem,
        initial_y: &wire.initial_y,
        solver_nominals: &wire.solver_nominals,
        parameters: &wire.parameters,
        visible_names: &wire.visible_names,
        visible_value_rows: &wire.visible_value_rows,
        variable_meta: &wire.variable_meta,
    })?;
    let artifacts =
        super::artifacts::lower_solve_artifacts(&wire.problem, solve::MassMatrix::Identity)
            .map_err(|error| SolveModelWireError::ArtifactDerivation(error.to_string()))?;
    let model = solve::SolveModel {
        problem: wire.problem,
        pure_calls: wire.pure_calls,
        artifacts,
        initial_y: wire.initial_y,
        solver_nominals: wire.solver_nominals,
        parameters: wire.parameters,
        external_tables: wire.external_tables,
        visible_names: wire.visible_names,
        visible_value_rows: wire.visible_value_rows,
        variable_meta: wire.variable_meta,
    };
    model
        .validate()
        .map_err(|error| SolveModelWireError::Root(error.to_string()))?;
    Ok(model)
}

struct CorrelationView<'model> {
    problem: &'model solve::SolveProblem,
    initial_y: &'model [f64],
    solver_nominals: &'model [f64],
    parameters: &'model [f64],
    visible_names: &'model [String],
    visible_value_rows: &'model solve::ScalarProgramBlock,
    variable_meta: &'model [solve::SolveVariableMeta],
}

fn validate_correlations(view: CorrelationView<'_>) -> Result<(), SolveModelWireError> {
    require_vector_length(
        "initial_y",
        view.problem.layout.y_scalars(),
        view.initial_y.len(),
    )?;
    require_vector_length(
        "solver_nominals",
        view.problem.layout.y_scalars(),
        view.solver_nominals.len(),
    )?;
    require_vector_length(
        "parameters",
        view.problem.layout.p_scalars(),
        view.parameters.len(),
    )?;
    require_vector_length(
        "visible_value_rows",
        view.visible_names.len(),
        view.visible_value_rows.row_count(),
    )?;
    require_vector_length(
        "visible_value_outputs",
        view.visible_names.len(),
        view.visible_value_rows.output_count(),
    )?;
    require_vector_length(
        "variable_meta",
        view.visible_names.len(),
        view.variable_meta.len(),
    )?;
    if !view
        .visible_value_rows
        .uses_local_contiguous_output_indices()
    {
        return Err(SolveModelWireError::VisibleOutputIndices);
    }
    for (index, (name, metadata)) in view
        .visible_names
        .iter()
        .zip(view.variable_meta)
        .enumerate()
    {
        if metadata.name != *name {
            return Err(SolveModelWireError::VariableMetaName { index });
        }
    }
    Ok(())
}

fn require_vector_length(
    field: &'static str,
    expected: usize,
    actual: usize,
) -> Result<(), SolveModelWireError> {
    if actual == expected {
        return Ok(());
    }
    Err(SolveModelWireError::VectorLength {
        field,
        expected,
        actual,
    })
}

#[derive(Debug)]
pub enum SolveModelWireError {
    SchemaVersion {
        actual: u16,
        expected: u16,
    },
    VectorLength {
        field: &'static str,
        expected: usize,
        actual: usize,
    },
    VisibleOutputIndices,
    VariableMetaName {
        index: usize,
    },
    UnsupportedMassMatrix,
    ArtifactDerivation(String),
    Root(String),
}

impl std::fmt::Display for SolveModelWireError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SchemaVersion { actual, expected } => write!(
                formatter,
                "unsupported SolveModel schema_version {actual}; expected {expected}"
            ),
            Self::VectorLength {
                field,
                expected,
                actual,
            } => write!(
                formatter,
                "SolveModel {field} contains {actual} entries, expected {expected}"
            ),
            Self::VisibleOutputIndices => formatter.write_str(
                "SolveModel visible-value rows do not use one dense local output per visible name",
            ),
            Self::VariableMetaName { index } => write!(
                formatter,
                "SolveModel variable metadata at index {index} does not name the matching visible value"
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
            Self::Root(error) => write!(formatter, "replayed SolveModel is invalid: {error}"),
        }
    }
}

impl std::error::Error for SolveModelWireError {}
