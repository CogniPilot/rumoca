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
        Some(plan) => plan.checked().map_err(serde::de::Error::custom)?,
    };
    FmiComponent::construct(wire.solve_model, wire.variables)
        .map(|component| component.with_co_simulation(plan))
        .map_err(serde::de::Error::custom)
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
}
