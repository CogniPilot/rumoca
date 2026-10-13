//! Checked replay for the canonical solver model wire.
//!
//! The wire contains only construction inputs. Executable differentiation and
//! structural artifacts are rebuilt through the same evaluation-owned derivation
//! used by fresh lowering before a [`solve::SolveModel`] is exposed.

use rumoca_ir_solve as solve;
use serde::{Deserialize, Serialize};

/// Current canonical `SolveModel` wire schema.
///
/// The model schema is distinct from `SOLVE_SCHEMA_VERSION`, which versions
/// the nested canonical `SolveProblem`.
pub const SOLVE_MODEL_SCHEMA_VERSION: u16 = 5;

/// Borrowed canonical construction inputs for one solver model.
///
/// Derived executable, differentiation, mass-matrix, and structural artifacts
/// are deliberately absent. Checked replay regenerates them through the
/// evaluation owner's sole derivation path.
#[derive(Debug, Serialize)]
pub struct SolveModelWireRef<'model> {
    schema_version: u16,
    pure_calls: &'model solve::SolvePureCallTable,
    problem: &'model solve::SolveProblem,
    initial_y: &'model solve::SolveInitialValues,
    solver_nominals: &'model [f64],
    parameters: &'model solve::SolveInitialValues,
    external_tables: &'model solve::ExternalTables,
    visible_projection: &'model [solve::VisibleProjectionInput],
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
        visible_projection: model.visible_projection.inputs(),
    })?;
    Ok(SolveModelWireRef {
        schema_version: SOLVE_MODEL_SCHEMA_VERSION,
        pure_calls: &model.pure_calls,
        problem: &model.problem,
        initial_y: &model.initial_y,
        solver_nominals: &model.solver_nominals,
        parameters: &model.parameters,
        external_tables: &model.external_tables,
        visible_projection: model.visible_projection.inputs(),
    })
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SolveModelWire {
    schema_version: u16,
    /// Read before `problem`: sites in the problem resolve against it.
    pure_calls: solve::ScopedSolvePureCallTable,
    problem: solve::SolveProblem,
    initial_y: solve::SolveInitialValues,
    solver_nominals: Vec<f64>,
    parameters: solve::SolveInitialValues,
    external_tables: solve::ExternalTables,
    visible_projection: Vec<solve::VisibleProjectionInput>,
}

/// Replay one canonical solver model from an arbitrary Serde input.
///
/// Use this function with `#[serde(deserialize_with = "...")]` for a model
/// nested inside a transport envelope. `SolveModel` itself intentionally does
/// not implement `Deserialize`: only the evaluation owner owns the mechanical artifact
/// derivation needed to construct the executable root.
pub fn deserialize_solve_model<'de, D>(deserializer: D) -> Result<solve::SolveModel, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let wire = SolveModelWire::deserialize(deserializer)?;
    replay_solve_model(wire).map_err(serde::de::Error::custom)
}

fn replay_solve_model(wire: SolveModelWire) -> Result<solve::SolveModel, SolveModelWireError> {
    let SolveModelWire {
        schema_version,
        pure_calls,
        mut problem,
        initial_y,
        solver_nominals,
        parameters,
        external_tables,
        visible_projection,
    } = wire;
    let pure_calls = pure_calls.into_table();
    if schema_version != SOLVE_MODEL_SCHEMA_VERSION {
        return Err(SolveModelWireError::SchemaVersion {
            actual: schema_version,
            expected: SOLVE_MODEL_SCHEMA_VERSION,
        });
    }
    validate_correlations(CorrelationView {
        problem: &problem,
        initial_y: &initial_y,
        solver_nominals: &solver_nominals,
        parameters: &parameters,
        visible_projection: &visible_projection,
    })?;
    let visible_projection = solve::VisibleProjection::construct(&problem, visible_projection)
        .map_err(|error| SolveModelWireError::Root(error.to_string()))?;
    solve::validate_problem_pure_call_sites(&problem, &pure_calls)
        .map_err(|error| SolveModelWireError::Root(error.to_string()))?;
    solve::issue_native_assignment_schedule_with_calls(&mut problem, &pure_calls);
    let artifacts = super::artifacts::lower_solve_artifacts_with_calls(
        &problem,
        &pure_calls,
        solve::MassMatrix::Identity,
    )
    .map_err(SolveModelWireError::ArtifactDerivation)?;
    // Chart artifacts are derived data absent from the wire: rebuild each alternate
    // reduced chart's executable image with the same assembly used at construction.
    super::artifacts::replay_reduced_chart_artifacts(&mut problem)
        .map_err(SolveModelWireError::ArtifactDerivation)?;
    let model = solve::SolveModel {
        problem,
        pure_calls,
        artifacts,
        initial_y,
        solver_nominals,
        parameters,
        external_tables,
        visible_projection,
    };
    model
        .validate()
        .map_err(|error| SolveModelWireError::Root(error.to_string()))?;
    Ok(model)
}

struct CorrelationView<'model> {
    problem: &'model solve::SolveProblem,
    initial_y: &'model solve::SolveInitialValues,
    solver_nominals: &'model [f64],
    parameters: &'model solve::SolveInitialValues,
    visible_projection: &'model [solve::VisibleProjectionInput],
}

fn validate_correlations(view: CorrelationView<'_>) -> Result<(), SolveModelWireError> {
    for values in [view.initial_y, view.parameters] {
        values
            .require_finite()
            .map_err(|error| SolveModelWireError::Root(error.to_string()))?;
    }
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
    solve::VisibleProjection::check_inputs(view.problem, view.visible_projection)
        .map_err(|error| SolveModelWireError::Root(error.to_string()))?;
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
    UnsupportedMassMatrix,
    ArtifactDerivation(crate::ad::SolveDerivationError),
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

impl std::error::Error for SolveModelWireError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::ArtifactDerivation(error) => Some(error),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error;

    #[test]
    fn only_a_failed_artifact_derivation_chains_a_source_error() {
        let derivation = SolveModelWireError::ArtifactDerivation(
            crate::ad::SolveDerivationError::unspanned_contract("derivation failed"),
        );
        assert!(
            derivation
                .source()
                .unwrap()
                .to_string()
                .contains("derivation failed")
        );
        assert!(
            derivation
                .to_string()
                .contains("failed to reconstruct SolveModel artifacts")
        );
        assert!(
            SolveModelWireError::UnsupportedMassMatrix
                .source()
                .is_none()
        );
        assert!(SolveModelWireError::Root("bad".into()).source().is_none());
    }
}
