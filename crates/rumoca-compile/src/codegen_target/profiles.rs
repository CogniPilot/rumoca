//! Closed manifest profiles for Solve-executable products.

use anyhow::{Result, bail};
use serde::Deserialize;

use super::TargetAlgorithmCodeArithmetic;

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct TargetSolveExecutableProfiles {
    value_capabilities: TargetValueCapabilityProfile,
    operation_effect_capabilities: TargetOperationEffectCapabilityProfile,
    environment: TargetExecutionEnvironmentProfile,
    emission: TargetEmissionPolicy,
}

impl TargetSolveExecutableProfiles {
    fn validate(&self) -> Result<()> {
        self.value_capabilities.validate()?;
        self.operation_effect_capabilities.validate()?;
        self.environment.validate()?;
        self.emission.validate()
    }

    pub(super) fn into_production_profile(
        self,
        arithmetic: TargetAlgorithmCodeArithmetic,
    ) -> Result<rumoca_phase_codegen::SolveAlgorithmProductionProfile> {
        self.validate()?;
        if arithmetic.source_integer != rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I32 {
            bail!(
                "unsupported Solve Algorithm source Integer representation; the current Production C refinement admits only source_integer = 'i32'"
            );
        }
        rumoca_phase_codegen::SolveAlgorithmProductionProfile::freestanding_c99_loop_returned_status(
            self.environment.max_automatic_payload_bytes,
        )
        .map_err(Into::into)
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct TargetValueCapabilityProfile {
    boolean_scalar: bool,
    boolean_tensor: bool,
    signed_integer_scalar: bool,
    signed_integer_tensor: bool,
    unsigned_integer_scalar: bool,
    unsigned_integer_tensor: bool,
    real_scalar: bool,
    real_tensor: bool,
    nested_record_arrays: bool,
    empty_fields: bool,
    empty_values: bool,
    enum_brands: bool,
    opaque_value_handles: bool,
}

impl TargetValueCapabilityProfile {
    fn validate(&self) -> Result<()> {
        if !self.boolean_scalar || !self.signed_integer_scalar || !self.real_scalar {
            bail!(
                "Solve Algorithm Production C requires scalar Boolean, signed Integer, and Real value capabilities"
            );
        }
        if self.unsigned_integer_scalar
            || self.unsigned_integer_tensor
            || self.nested_record_arrays
            || self.empty_fields
            || self.empty_values
            || self.enum_brands
            || self.opaque_value_handles
        {
            bail!("unsupported value family in Solve Algorithm Production C profile");
        }
        if self.boolean_tensor || self.signed_integer_tensor || self.real_tensor {
            bail!("the current Production C preparation profile admits scalar values only");
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct TargetOperationEffectCapabilityProfile {
    admitted: Vec<TargetOperationEffectCapability>,
}

impl TargetOperationEffectCapabilityProfile {
    fn validate(&self) -> Result<()> {
        require_canonical_capabilities(&self.admitted)?;
        for required in [
            TargetOperationEffectCapability::Constant,
            TargetOperationEffectCapability::Store,
            TargetOperationEffectCapability::DeclarationInitialization,
            TargetOperationEffectCapability::LifecycleMethod,
            TargetOperationEffectCapability::ErrorSignalReset,
        ] {
            if !self.admitted.contains(&required) {
                bail!(
                    "Solve Algorithm Production C operation/effect profile omits required capability '{required:?}'"
                );
            }
        }
        Ok(())
    }
}

fn require_canonical_capabilities(values: &[TargetOperationEffectCapability]) -> Result<()> {
    if values.windows(2).any(|pair| pair[0] >= pair[1]) {
        bail!(
            "solve_executable.operation_effect_capabilities.admitted must be canonically sorted and deduplicated"
        );
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "kebab-case")]
enum TargetOperationEffectCapability {
    Constant,
    DeclarationInitialization,
    ErrorSignalReset,
    LifecycleMethod,
    Store,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct TargetExecutionEnvironmentProfile {
    environment: TargetEnvironmentKind,
    allocation: TargetAllocationPolicy,
    recursion: TargetRecursionPolicy,
    max_automatic_payload_bytes: u32,
    failure: TargetFailurePolicy,
    runtime_math: Vec<TargetRuntimeMath>,
    concurrency: TargetConcurrencyModel,
    atomics: TargetAtomicModel,
    admitted_library_contracts: Vec<String>,
    isa_features: Vec<String>,
}

impl TargetExecutionEnvironmentProfile {
    fn validate(&self) -> Result<()> {
        if self.environment != TargetEnvironmentKind::Freestanding
            || self.allocation != TargetAllocationPolicy::Forbidden
            || self.recursion != TargetRecursionPolicy::Forbidden
            || self.failure != TargetFailurePolicy::ReturnedStatus
            || self.concurrency != TargetConcurrencyModel::SingleThreaded
            || self.atomics != TargetAtomicModel::None
        {
            bail!("unsupported Solve Algorithm Production C execution environment profile");
        }
        if !self.runtime_math.is_empty()
            || !self.admitted_library_contracts.is_empty()
            || !self.isa_features.is_empty()
        {
            bail!(
                "the current scalar-literal Production C slice admits no runtime math, library contract, or ISA feature"
            );
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
enum TargetEnvironmentKind {
    Hosted,
    Freestanding,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
enum TargetAllocationPolicy {
    Forbidden,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
enum TargetRecursionPolicy {
    Forbidden,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
enum TargetFailurePolicy {
    ReturnedStatus,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "kebab-case")]
enum TargetRuntimeMath {
    Sqrt,
    Sin,
    Cos,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
enum TargetConcurrencyModel {
    SingleThreaded,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
enum TargetAtomicModel {
    None,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
enum TargetEmissionPolicy {
    Loop,
}

impl TargetEmissionPolicy {
    fn validate(&self) -> Result<()> {
        match self {
            Self::Loop => Ok(()),
        }
    }
}
