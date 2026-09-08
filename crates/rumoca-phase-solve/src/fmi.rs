//! Completed Solve lowering into the checked FMI description of the problem.

use rumoca_core::Span;
use rumoca_ir_solve::fmi::{FmiComponent, FmiComponentError};
use serde::{Deserialize, Serialize};

#[derive(Debug, thiserror::Error)]
pub enum FmiLoweringError {
    #[error(transparent)]
    Component(#[from] FmiComponentError),
    #[error("FMI component wire is invalid: {0}")]
    Wire(String),
}

impl FmiLoweringError {
    #[must_use]
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Component(error) => error.span(),
            Self::Wire(_) => None,
        }
    }
}

pub const FMI_COMPONENT_SCHEMA_VERSION: u16 = 2;

/// Borrowed, canonical construction inputs for one correlated FMI component.
#[derive(Serialize)]
pub struct FmiComponentWireRef<'model> {
    schema_version: u16,
    solve_model: crate::SolveModelWireRef<'model>,
}

/// Encode the still-correlated phase result without exposing either half as
/// an independently selectable runtime input.
pub fn fmi_component_wire<'wire>(
    lowered: &'wire crate::LoweredSolveModel<'_>,
) -> Result<FmiComponentWireRef<'wire>, FmiLoweringError> {
    let solve_model = crate::solve_model_wire(lowered.model())
        .map_err(|error| FmiLoweringError::Wire(error.to_string()))?;
    Ok(FmiComponentWireRef {
        schema_version: FMI_COMPONENT_SCHEMA_VERSION,
        solve_model,
    })
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FmiComponentWire {
    schema_version: u16,
    #[serde(deserialize_with = "crate::deserialize_solve_model")]
    solve_model: rumoca_ir_solve::SolveModel,
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
    FmiComponent::construct(wire.solve_model).map_err(serde::de::Error::custom)
}

/// Consume one still-correlated complete Solve lowering into its checked FMI
/// component.
///
/// The component consumes only the completed Solve root and the sealed catalog
/// it retains; there is no second DAE inspection or metadata evaluation.
pub fn finish_fmi_component(
    lowered: crate::LoweredSolveModel<'_>,
) -> Result<FmiComponent, FmiLoweringError> {
    let solve = lowered.into_model();
    FmiComponent::construct(solve).map_err(Into::into)
}
