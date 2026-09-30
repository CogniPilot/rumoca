//! Checked correlated guarded updates over compact mutable-storage ranges.

use super::*;
use std::sync::Arc;

/// One compact mutable-storage destination for a guarded assignment result.
#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub struct GuardedAssignmentTargetRange {
    base: ScalarSlot,
    count: usize,
}

impl GuardedAssignmentTargetRange {
    pub const fn base(self) -> ScalarSlot {
        self.base
    }

    pub const fn count(self) -> usize {
        self.count
    }
}

/// One checked correlated guarded update.
///
/// `program` produces the concatenation of `target_ranges` in source order.
/// The compact ranges, rather than a per-coordinate target vector, are the
/// authoritative simultaneous-assignment relation.
#[derive(Clone, Debug, Serialize)]
pub struct GuardedAssignmentProgram {
    program: Arc<[LinearOp]>,
    span: Span,
    target_ranges: Box<[GuardedAssignmentTargetRange]>,
    #[serde(skip)]
    output_count: usize,
    #[serde(skip)]
    register_count: usize,
    role: DiscreteRowRole,
    pre_mode: DiscreteEventPreMode,
    observation_refresh: bool,
    integrator_history_effect: IntegratorHistoryEffect,
    clock_owner: Option<PeriodicClockId>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct GuardedAssignmentProgramWire {
    program: Vec<LinearOp>,
    span: Span,
    target_ranges: Box<[GuardedAssignmentTargetRangeWire]>,
    role: DiscreteRowRole,
    pre_mode: DiscreteEventPreMode,
    observation_refresh: bool,
    integrator_history_effect: IntegratorHistoryEffect,
    clock_owner: Option<PeriodicClockId>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct GuardedAssignmentTargetRangeWire {
    base: ScalarSlot,
    count: usize,
}

impl<'de> Deserialize<'de> for GuardedAssignmentProgram {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = GuardedAssignmentProgramWire::deserialize(deserializer)?;
        let provenance = wire
            .span
            .require_provenance("GuardedAssignmentProgram")
            .map_err(serde::de::Error::custom)?;
        Self::checked(
            wire.program,
            provenance,
            wire.target_ranges
                .iter()
                .map(|range| (range.base, range.count)),
            wire.role,
            wire.pre_mode,
            wire.observation_refresh,
            wire.integrator_history_effect,
            wire.clock_owner,
        )
        .map_err(serde::de::Error::custom)
    }
}

impl GuardedAssignmentProgram {
    // SPEC_0021: Exception - validated boundary keeps proof-relevant inputs explicit.
    #[allow(clippy::too_many_arguments)]
    pub fn checked(
        program: Vec<LinearOp>,
        provenance: ProvenanceSpan,
        target_ranges: impl IntoIterator<Item = (ScalarSlot, usize)>,
        role: DiscreteRowRole,
        pre_mode: DiscreteEventPreMode,
        observation_refresh: bool,
        integrator_history_effect: IntegratorHistoryEffect,
        clock_owner: Option<PeriodicClockId>,
    ) -> Result<Self, SolveProblemShapeContractError> {
        let span = provenance.span();
        let target_ranges = target_ranges
            .into_iter()
            .map(|(base, count)| GuardedAssignmentTargetRange { base, count })
            .collect::<Box<[_]>>();
        validate_guarded_assignment_targets(&target_ranges, span)?;
        let expected_outputs = target_ranges.iter().try_fold(0usize, |total, range| {
            total.checked_add(range.count).ok_or(
                SolveProblemShapeContractError::GuardedAssignmentProgram {
                    program_index: 0,
                    detail: "target result width overflows",
                    span: Some(span),
                },
            )
        })?;
        let actual_outputs = ScalarProgramBlock::program_output_count(&program);
        if actual_outputs != expected_outputs {
            return Err(SolveProblemShapeContractError::GuardedAssignmentProgram {
                program_index: 0,
                detail: "program output width does not equal its compact target ranges",
                span: Some(span),
            });
        }
        crate::validate_function_conditional_owners(
            "GuardedAssignmentProgram",
            0,
            std::slice::from_ref(&program),
            &[span],
        )?;
        let register_count = crate::derive_scalar_program_register_counts(
            "GuardedAssignmentProgram",
            0,
            std::slice::from_ref(&program),
            &[span],
        )?[0];
        Ok(Self {
            program: program.into(),
            span,
            target_ranges,
            output_count: expected_outputs,
            register_count,
            role,
            pre_mode,
            observation_refresh,
            integrator_history_effect,
            clock_owner,
        })
    }

    pub fn program(&self) -> &[LinearOp] {
        &self.program
    }

    pub fn shared_program(&self) -> Arc<[LinearOp]> {
        Arc::clone(&self.program)
    }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub fn target_ranges(&self) -> &[GuardedAssignmentTargetRange] {
        &self.target_ranges
    }

    pub const fn role(&self) -> DiscreteRowRole {
        self.role
    }

    pub const fn pre_mode(&self) -> DiscreteEventPreMode {
        self.pre_mode
    }

    pub const fn observation_refresh(&self) -> bool {
        self.observation_refresh
    }

    pub const fn integrator_history_effect(&self) -> IntegratorHistoryEffect {
        self.integrator_history_effect
    }

    pub const fn clock_owner(&self) -> Option<PeriodicClockId> {
        self.clock_owner
    }

    pub const fn output_count(&self) -> usize {
        self.output_count
    }

    /// Exact register capacity proved with this compact owner.
    pub const fn register_count(&self) -> usize {
        self.register_count
    }
}

fn validate_guarded_assignment_targets(
    target_ranges: &[GuardedAssignmentTargetRange],
    span: Span,
) -> Result<(), SolveProblemShapeContractError> {
    if target_ranges.is_empty() {
        return Err(SolveProblemShapeContractError::GuardedAssignmentProgram {
            program_index: 0,
            detail: "target-range catalog is empty",
            span: Some(span),
        });
    }
    let mut covered = Vec::<(u8, usize, usize)>::new();
    for range in target_ranges {
        if range.count == 0 {
            return Err(SolveProblemShapeContractError::GuardedAssignmentProgram {
                program_index: 0,
                detail: "target range is empty",
                span: Some(span),
            });
        }
        let (storage, start) = match range.base {
            ScalarSlot::Y { index, .. } => (0_u8, index),
            ScalarSlot::P { index, .. } => (1_u8, index),
            ScalarSlot::Time | ScalarSlot::Constant(_) => {
                return Err(SolveProblemShapeContractError::GuardedAssignmentProgram {
                    program_index: 0,
                    detail: "target range is not mutable Y/P storage or overflows",
                    span: Some(span),
                });
            }
        };
        let end = start.checked_add(range.count).ok_or(
            SolveProblemShapeContractError::GuardedAssignmentProgram {
                program_index: 0,
                detail: "target range is not mutable Y/P storage or overflows",
                span: Some(span),
            },
        )?;
        if covered
            .iter()
            .any(|&(other_storage, other_start, other_end)| {
                storage == other_storage && start < other_end && other_start < end
            })
        {
            return Err(SolveProblemShapeContractError::GuardedAssignmentProgram {
                program_index: 0,
                detail: "target ranges overlap",
                span: Some(span),
            });
        }
        covered.push((storage, start, end));
    }
    Ok(())
}
