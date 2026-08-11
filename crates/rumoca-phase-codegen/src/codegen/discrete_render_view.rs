use rumoca_ir_solve as solve;

use crate::errors::CodegenError;

/// Final template-render view of compact discrete owners.
///
/// This type is private to code generation so no compiler, evaluator, or
/// runtime-planning phase can acquire a scalar target catalog. Coordinate
/// spelling exists only for target templates whose ABI requires it.
pub(super) struct DiscreteRenderView {
    pub(super) rhs: solve::ScalarProgramBlock,
    pub(super) targets: Vec<solve::ScalarSlot>,
    pub(super) pre_modes: Vec<solve::DiscreteEventPreMode>,
    pub(super) observation_refresh: Vec<bool>,
}

impl DiscreteRenderView {
    pub(super) fn checked(system: &solve::DiscreteSolveSystem) -> Result<Self, CodegenError> {
        let canonical_count = system.rhs.len();
        for (name, actual) in [
            ("targets", system.update_targets.len()),
            ("pre modes", system.pre_modes.len()),
            ("observation policies", system.observation_refresh.len()),
        ] {
            if actual != canonical_count {
                return Err(CodegenError::template(format!(
                    "canonical scalar discrete {name} count differs from RHS"
                )));
            }
        }
        let mut programs = system.rhs.programs().to_vec();
        let mut spans = system.rhs.program_spans().to_vec();
        let mut targets = system.update_targets.clone();
        let mut pre_modes = system.pre_modes.clone();
        let mut observation_refresh = system.observation_refresh.clone();
        for owner in &system.guarded_assignments {
            programs.push(owner.program().to_vec());
            spans.push(owner.span());
            for range in owner.target_ranges() {
                for offset in 0..range.count() {
                    targets.push(render_target_at(range.base(), offset)?);
                    pre_modes.push(owner.pre_mode());
                    observation_refresh.push(owner.observation_refresh());
                }
            }
        }
        let rhs = solve::ScalarProgramBlock::with_program_spans(programs, spans)
            .map_err(|error| CodegenError::template(error.to_string()))?;
        if rhs.len() != targets.len() {
            return Err(CodegenError::template(
                "rendered discrete program and target counts differ",
            ));
        }
        Ok(Self {
            rhs,
            targets,
            pre_modes,
            observation_refresh,
        })
    }
}

fn render_target_at(
    base: solve::ScalarSlot,
    offset: usize,
) -> Result<solve::ScalarSlot, CodegenError> {
    match base {
        solve::ScalarSlot::Y { index, .. } => index
            .checked_add(offset)
            .map(solve::scalar_slot_y)
            .ok_or_else(|| CodegenError::template("rendered guarded Y target overflowed")),
        solve::ScalarSlot::P { index, .. } => index
            .checked_add(offset)
            .map(solve::scalar_slot_p)
            .ok_or_else(|| CodegenError::template("rendered guarded P target overflowed")),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => Err(CodegenError::template(
            "rendered guarded target is not mutable storage",
        )),
    }
}
