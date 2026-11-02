//! Source-correlated total tangents for a retained FMI component.
//!
//! The component remains borrowed by the plan: callers cannot construct a plan
//! from unrelated metadata and numerical programs. Each issued shared-value
//! segment retains its simultaneous target commit boundary. Consumers evaluate
//! its tangent program into private scratch, validate all outputs, then commit
//! those targets before the next segment. This preparation owner alone makes
//! no public FMI capability or lifecycle/API eligibility claim.

mod initialization;
mod post_initialization;

use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_solve::{self as solve, fmi::FmiCCodegenView};

use crate::{ad, to_scalar_program_block};

/// Direction-normalized consistency check for a retained source residual.
/// `scale` is the largest absolute Jacobian-column times seed contribution
/// at the same settled point. It has the residual tangent's units. There is
/// no unit-valued fallback: a small contradictory equation must still refuse.
/// Generated C mirrors this guard with the same projection-policy tolerance.
#[must_use]
pub fn consistency_row_converged(residual: f64, scale: f64) -> bool {
    if !residual.is_finite() || !scale.is_finite() || scale < 0.0 {
        return false;
    }
    if residual == 0.0 {
        return true;
    }
    scale > 0.0 && residual.abs() <= crate::projection_policy::ALGEBRAIC_REFRESH_TOLERANCE * scale
}

/// Why this preparation did not issue a complete tangent plan.
#[derive(Debug)]
pub enum FmiTangentPlanError {
    /// The primal profile does not issue the complete derivative interface.
    IncompleteProfile(String),
    /// The complete algebraic residual/coordinate inventory is unavailable.
    ImplicitInventory(String),
    /// Initialization needs a correlated owner not issued by this plan.
    InitializationInventory(String),
    /// Alternate charts or a state manifold need their correlated sensitivity.
    AlternateCoordinates,
    /// Checked owner/program capacities do not agree.
    Contract(String),
    /// Mechanical differentiation could not issue the required program.
    Derivation(ad::SolveDerivationError),
}

impl std::fmt::Display for FmiTangentPlanError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IncompleteProfile(reason) => {
                write!(formatter, "FMI derivative profile: {reason}")
            }
            Self::ImplicitInventory(reason) => {
                write!(formatter, "FMI implicit tangent inventory: {reason}")
            }
            Self::InitializationInventory(reason) => {
                write!(formatter, "FMI initialization tangent inventory: {reason}")
            }
            Self::AlternateCoordinates => {
                formatter.write_str("FMI total sensitivity needs an alternate-coordinate owner")
            }
            Self::Contract(message) => write!(formatter, "FMI tangent plan contract: {message}"),
            Self::Derivation(error) => std::fmt::Display::fmt(error, formatter),
        }
    }
}

impl std::error::Error for FmiTangentPlanError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Derivation(error) => Some(error),
            _ => None,
        }
    }
}

/// One simultaneous tangent assignment segment, in issued refresh order.
pub struct FmiTangentSegment {
    program: solve::ScalarProgramBlock,
    targets: Box<[usize]>,
}

impl FmiTangentSegment {
    /// Outputs occupy local scratch positions, in exactly `targets()` order.
    #[must_use]
    pub fn program(&self) -> &solve::ScalarProgramBlock {
        &self.program
    }

    /// Solver-Y seed positions committed together after successful evaluation.
    #[must_use]
    pub fn targets(&self) -> &[usize] {
        &self.targets
    }
}

/// One complete, square public-refresh relation at the settled primal point.
///
/// `program()` computes the full Y|P|time directional derivatives of the actual
/// implicit residual and public-observation value programs. `rows()` selects
/// their outputs. `subtract_targets()` distinguishes residual equations from
/// value assignments: only a value row subtracts its target seed, once.
/// `targets()` names solver-Y and observation-P unknowns in full Y|P order.
/// A consumer solves R_z dz = -R_known dknown, then commits the entire dz.
/// The local R_z is never exposed as a public state/input/output Jacobian.
pub struct FmiImplicitTangentSystem {
    program: solve::ScalarProgramBlock,
    rows: Box<[usize]>,
    targets: Box<[usize]>,
    subtract_targets: Box<[usize]>,
}

/// The numerical relation's lifecycle scope, not public API admission.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FmiTangentMode {
    ContinuousOrStep,
    Initialization,
    EventOrStep,
}

/// Derivatives of retained initialization checks with no solved target.
/// A consumer must validate these after solving the tangent relation; ignoring
/// them would accept a seed direction that violates the source initialization system.
pub struct FmiTangentConsistency {
    program: solve::ScalarProgramBlock,
    rows: Box<[usize]>,
}

impl FmiTangentConsistency {
    #[must_use]
    pub fn program(&self) -> &solve::ScalarProgramBlock {
        &self.program
    }

    #[must_use]
    pub fn rows(&self) -> &[usize] {
        &self.rows
    }
}

impl FmiImplicitTangentSystem {
    #[must_use]
    pub fn program(&self) -> &solve::ScalarProgramBlock {
        &self.program
    }

    #[must_use]
    pub fn rows(&self) -> &[usize] {
        &self.rows
    }

    #[must_use]
    pub fn targets(&self) -> &[usize] {
        &self.targets
    }

    /// Zero for an existing residual; otherwise one plus its value target's
    /// Y|P seed index. This avoids subtracting an implicit target twice.
    #[must_use]
    pub fn subtract_targets(&self) -> &[usize] {
        &self.subtract_targets
    }
}

/// Derived executable programs of this retained component's exact profile.
///
/// This is a preparation view, not a second canonical Solve root. Exact
/// schedules keep their shared-value segments; a projection profile retains the
/// complete square algebraic residual relation. Alternate-coordinate sensitivity
/// is refused until its owner exists, never replaced by zero derivatives.
pub struct FmiTangentPlan<'component> {
    component: &'component FmiCCodegenView,
    mode: FmiTangentMode,
    assignments: Vec<FmiTangentSegment>,
    implicit: Option<FmiImplicitTangentSystem>,
    rhs: solve::ScalarProgramBlock,
    seed_len: usize,
    consistency: Option<FmiTangentConsistency>,
}

/// Complete derivative coverage for the three equation sets of one C component.
///
/// Construction either issues every mode together or returns a typed refusal.
/// A renderer cannot advertise a continuous-only or initialization-only plan.
pub struct FmiDerivativePlans<'component> {
    continuous: FmiTangentPlan<'component>,
    initialization: FmiTangentPlan<'component>,
    post_initialization: FmiTangentPlan<'component>,
}

impl<'component> FmiDerivativePlans<'component> {
    /// Derive the complete profile once from the same retained component.
    pub fn derive(component: &'component FmiCCodegenView) -> Result<Self, FmiTangentPlanError> {
        Ok(Self {
            continuous: FmiTangentPlan::derive(component)?,
            initialization: FmiTangentPlan::derive_initialization(component)?,
            post_initialization: FmiTangentPlan::derive_event_or_step(component)?,
        })
    }

    #[must_use]
    pub fn continuous(&self) -> &FmiTangentPlan<'component> {
        &self.continuous
    }

    #[must_use]
    pub fn initialization(&self) -> &FmiTangentPlan<'component> {
        &self.initialization
    }

    #[must_use]
    pub fn post_initialization(&self) -> &FmiTangentPlan<'component> {
        &self.post_initialization
    }
}

impl<'component> FmiTangentPlan<'component> {
    pub fn derive(component: &'component FmiCCodegenView) -> Result<Self, FmiTangentPlanError> {
        let problem = component.problem();
        let continuous = &problem.continuous;
        if !continuous.reduced_chart_set.charts.is_empty()
            || !continuous.manifold_projection_plan.is_empty()
        {
            return Err(FmiTangentPlanError::AlternateCoordinates);
        }
        let seed_len = problem
            .layout
            .y_scalars()
            .checked_add(problem.layout.p_scalars())
            .and_then(|length| length.checked_add(1))
            .ok_or_else(|| FmiTangentPlanError::Contract("seed extent overflow".into()))?;
        let (assignments, implicit) = if component.public_observation_rows().iter().any(|&row| row)
        {
            (Vec::new(), Some(derive_implicit(component)?))
        } else {
            match derive_assignments(component)? {
                Some(assignments) => {
                    require_algebraic_coverage(component, &assignments)?;
                    (assignments, None)
                }
                None => (Vec::new(), Some(derive_implicit(component)?)),
            }
        };
        let rhs = derive_rhs(component)?;
        Ok(Self {
            component,
            mode: FmiTangentMode::ContinuousOrStep,
            assignments,
            implicit,
            rhs,
            seed_len,
            consistency: None,
        })
    }

    /// Issue the complete initialization relation from the same checked
    /// component. Its private initializer and consistency consumer must both
    /// execute at the same settled point. Profiles that
    /// require unimplemented event-initialization sensitivity refuse here.
    pub fn derive_initialization(
        component: &'component FmiCCodegenView,
    ) -> Result<Self, FmiTangentPlanError> {
        initialization::derive(component)
    }

    /// Issue the post-initialization parameter/algebraic relation for Event
    /// or Step Mode. Initial equations and initial-state assignments are not
    /// refreshed in these modes. The separate C consumer must bind its exact
    /// mode/VR permissions; preparation alone grants no capability. Scalar-event
    /// transaction sensitivity remains a distinct required owner.
    pub fn derive_event_or_step(
        component: &'component FmiCCodegenView,
    ) -> Result<Self, FmiTangentPlanError> {
        post_initialization::derive(component)
    }

    #[must_use]
    pub fn mode(&self) -> FmiTangentMode {
        self.mode
    }

    #[must_use]
    pub fn consistency(&self) -> Option<&FmiTangentConsistency> {
        self.consistency.as_ref()
    }

    #[must_use]
    pub fn component(&self) -> &'component FmiCCodegenView {
        self.component
    }

    #[must_use]
    pub fn assignments(&self) -> &[FmiTangentSegment] {
        &self.assignments
    }

    /// The complete algebraic residual relation, when refresh needs projection.
    #[must_use]
    pub fn implicit(&self) -> Option<&FmiImplicitTangentSystem> {
        self.implicit.as_ref()
    }

    /// State-derivative outputs keep their checked root-state destinations.
    #[must_use]
    pub fn rhs(&self) -> &solve::ScalarProgramBlock {
        &self.rhs
    }

    /// The distinct time direction follows both bounded storage columns.
    #[must_use]
    pub fn time_seed_index(&self) -> usize {
        self.seed_len - 1
    }

    /// Full seed storage is `[solver-Y | parameter | time]`.
    #[must_use]
    pub fn seed_len(&self) -> usize {
        self.seed_len
    }
}

fn derive_rhs(
    component: &FmiCCodegenView,
) -> Result<solve::ScalarProgramBlock, FmiTangentPlanError> {
    let problem = component.problem();
    let primal = to_scalar_program_block(&problem.continuous.derivative_rhs)
        .map_err(|error| FmiTangentPlanError::Derivation(error.into()))?;
    require_rhs_coverage(problem.solve_layout.state_scalar_count(), &primal)?;
    ad::lower_scalar_program_block_full_time_jvp(
        &primal,
        problem.layout.y_scalars(),
        problem.layout.p_scalars(),
    )
    .map_err(FmiTangentPlanError::Derivation)
}

fn derive_assignments(
    component: &FmiCCodegenView,
) -> Result<Option<Vec<FmiTangentSegment>>, FmiTangentPlanError> {
    let problem = component.problem();
    let continuous = &problem.continuous;
    let owners = &continuous.refresh_owners;
    let plan = owners.algebraic();
    let dispatch = plan.stage_schedule(&component.artifacts().continuous.structural);
    let steps = owners
        .staged_refresh_steps(plan, dispatch)
        .map_err(|error| FmiTangentPlanError::Contract(format!("refresh dispatch: {error:?}")))?;
    if steps
        .iter()
        .any(|step| !matches!(step, solve::StagedRefreshStep::Assignments(_)))
    {
        return Ok(None);
    }
    let mut derived = Vec::new();
    for step in steps {
        let solve::StagedRefreshStep::Assignments(schedule) = step else {
            return Ok(None);
        };
        let shared = schedule
            .shared_segments(&continuous.implicit_rhs, owners)
            .map_err(|error| FmiTangentPlanError::Contract(error.to_string()))?;
        for (segment, &span) in shared.segments().segments().iter().zip(shared.spans()) {
            if segment.targets().iter().any(|&target| {
                target < problem.solve_layout.state_scalar_count()
                    || target >= problem.layout.y_scalars()
            }) {
                return Err(FmiTangentPlanError::Contract(
                    "assignment target outside solver storage".into(),
                ));
            }
            let primal = solve::ScalarProgramBlock::with_output_indices(
                vec![segment.ops().to_vec()],
                vec![span],
                (0..segment.targets().len()).collect(),
            )
            .map_err(|error| FmiTangentPlanError::Contract(error.to_string()))?;
            let program = ad::lower_scalar_program_block_full_time_jvp(
                &primal,
                problem.layout.y_scalars(),
                problem.layout.p_scalars(),
            )
            .map_err(FmiTangentPlanError::Derivation)?;
            derived.push(FmiTangentSegment {
                program,
                targets: segment.targets().to_vec().into_boxed_slice(),
            });
        }
    }
    Ok(Some(derived))
}

fn require_algebraic_coverage(
    component: &FmiCCodegenView,
    assignments: &[FmiTangentSegment],
) -> Result<(), FmiTangentPlanError> {
    let targets = algebraic_target_range(&component.problem().solve_layout)?;
    let settled = assignments
        .iter()
        .flat_map(|segment| segment.targets.iter().copied())
        .collect::<BTreeSet<_>>();
    if !targets.into_iter().all(|index| settled.contains(&index)) {
        return Err(FmiTangentPlanError::Contract(
            "algebraic tangent inventory is incomplete".into(),
        ));
    }
    Ok(())
}

/// Output declarations occupy their own trailing Y run, but their values are
/// still dependent algebraic coordinates in a total derivative relation.
fn algebraic_target_range(
    layout: &solve::SolveLayout,
) -> Result<std::ops::Range<usize>, FmiTangentPlanError> {
    let first = layout.state_scalar_count();
    let count = layout
        .algebraic_scalar_count()
        .checked_add(layout.output_scalar_count)
        .ok_or_else(|| FmiTangentPlanError::Contract("algebraic extent overflow".into()))?;
    let end = first
        .checked_add(count)
        .ok_or_else(|| FmiTangentPlanError::Contract("algebraic extent overflow".into()))?;
    Ok(first..end)
}

fn require_rhs_coverage(
    states: usize,
    primal: &solve::ScalarProgramBlock,
) -> Result<(), FmiTangentPlanError> {
    let indices = primal.output_indices();
    if indices.len() != states
        || indices.iter().copied().collect::<BTreeSet<_>>() != (0..states).collect()
    {
        return Err(FmiTangentPlanError::IncompleteProfile(
            "state derivative tangent inventory is incomplete or duplicated".into(),
        ));
    }
    Ok(())
}

fn encode_value_target(target: usize) -> Result<usize, FmiTangentPlanError> {
    target
        .checked_add(1)
        .ok_or_else(|| FmiTangentPlanError::Contract("tangent target extent overflow".into()))
}

/// Read the complete simultaneous relation, not just one staged block. Later
/// blocks may depend on earlier ones; solving the full relation prevents a
/// block-local residual Jacobian being mistaken for a total public derivative.
fn algebraic_inventory(
    component: &FmiCCodegenView,
) -> Result<(Vec<usize>, Vec<usize>), FmiTangentPlanError> {
    let problem = component.problem();
    // Refresh optimization can isolate some residual rows as assignments.
    // The implicit tangent relation still needs those equations, including
    // initialization and post-initialization consumers. Read the complete
    // correlated row/target owner, not its remaining simultaneous subset.
    let inventory = &problem.continuous.implicit_row_targets;
    let mut rows = Vec::new();
    let mut targets = Vec::new();
    for row in 0..inventory.len() {
        match inventory.target_at(row) {
            Some(solve::ScalarSlot::Y { index, .. }) => {
                rows.push(row);
                targets.push(index);
            }
            None => {}
            _ => {
                return Err(FmiTangentPlanError::ImplicitInventory(
                    "algebraic residual target is not solver storage".into(),
                ));
            }
        }
    }
    let required = algebraic_target_range(&problem.solve_layout)?;
    let count = required.len();
    if rows.len() != count
        || targets.len() != count
        || rows.iter().copied().collect::<BTreeSet<_>>().len() != count
        || targets.iter().copied().collect::<BTreeSet<_>>() != required.collect()
    {
        return Err(FmiTangentPlanError::ImplicitInventory(
            "relation is not a complete square algebraic inventory".into(),
        ));
    }
    Ok((rows, targets))
}

fn derive_implicit(
    component: &FmiCCodegenView,
) -> Result<FmiImplicitTangentSystem, FmiTangentPlanError> {
    let problem = component.problem();
    let (rows, mut targets) = algebraic_inventory(component)?;
    let algebraic = to_scalar_program_block(&problem.continuous.implicit_rhs)
        .map_err(|error| FmiTangentPlanError::Derivation(error.into()))?;
    let (mut selected, mut positions) = select_residual_programs(&algebraic, &rows)?;
    let mut subtract_targets = vec![0; targets.len()];
    let (observation_rows, observation_targets) = observation_inventory(component)?;
    if !observation_rows.is_empty() {
        let (observations, local) =
            select_residual_programs(&problem.discrete.rhs, &observation_rows)?;
        let offset = selected.output_count();
        positions.extend(
            local
                .into_iter()
                .map(|row| {
                    offset.checked_add(row).ok_or_else(|| {
                        FmiTangentPlanError::Contract("observation output extent overflow".into())
                    })
                })
                .collect::<Result<Vec<_>, _>>()?,
        );
        for &target in &observation_targets {
            subtract_targets.push(encode_value_target(target)?);
        }
        targets.extend(observation_targets);
        selected = join_program_blocks(&selected, &observations)?;
    }
    if targets.is_empty() || targets.iter().copied().collect::<BTreeSet<_>>().len() != targets.len()
    {
        return Err(FmiTangentPlanError::ImplicitInventory(
            "empty or duplicated public-refresh targets".into(),
        ));
    }
    let program = ad::lower_scalar_program_block_full_time_jvp(
        &selected,
        problem.layout.y_scalars(),
        problem.layout.p_scalars(),
    )
    .map_err(FmiTangentPlanError::Derivation)?;
    Ok(FmiImplicitTangentSystem {
        program,
        rows: positions.into_boxed_slice(),
        targets: targets.into_boxed_slice(),
        subtract_targets: subtract_targets.into_boxed_slice(),
    })
}

/// Borrow only the exact observation rows authorized by the C profile. Event
/// actions, pre/history bindings and clock updates are held at the settled
/// point, not silently differentiated as if they were public value refreshes.
fn observation_inventory(
    component: &FmiCCodegenView,
) -> Result<(Vec<usize>, Vec<usize>), FmiTangentPlanError> {
    let problem = component.problem();
    let observed = component.public_observation_rows();
    if observed.is_empty() {
        return Ok((Vec::new(), Vec::new()));
    }
    let discrete = &problem.discrete;
    if observed.len() != discrete.update_targets.len()
        || observed.len() != discrete.rhs.output_count()
    {
        return Err(FmiTangentPlanError::Contract(
            "public observation inventory differs from its source rows".into(),
        ));
    }
    let mut rows = Vec::new();
    let mut targets = Vec::new();
    for (row, &selected) in observed.iter().enumerate() {
        if !selected {
            continue;
        }
        let solve::ScalarSlot::P { index, .. } = discrete.update_targets[row] else {
            return Err(FmiTangentPlanError::Contract(
                "public observation target is not parameter storage".into(),
            ));
        };
        if index >= problem.layout.p_scalars() {
            return Err(FmiTangentPlanError::Contract(
                "public observation target exceeds parameter storage".into(),
            ));
        }
        rows.push(row);
        targets.push(
            problem
                .layout
                .y_scalars()
                .checked_add(index)
                .ok_or_else(|| {
                    FmiTangentPlanError::Contract("observation seed extent overflow".into())
                })?,
        );
    }
    if targets.iter().copied().collect::<BTreeSet<_>>().len() != targets.len() {
        return Err(FmiTangentPlanError::Contract(
            "public observation targets are not unique".into(),
        ));
    }
    Ok((rows, targets))
}

/// Concatenate complete shared programs and their honest spans. Both selected
/// blocks use contiguous private output identities; remap the second block
/// once so its local stores cannot overwrite the first block's outputs.
fn join_program_blocks(
    left: &solve::ScalarProgramBlock,
    right: &solve::ScalarProgramBlock,
) -> Result<solve::ScalarProgramBlock, FmiTangentPlanError> {
    let count = left
        .output_count()
        .checked_add(right.output_count())
        .ok_or_else(|| {
            FmiTangentPlanError::Contract("public-refresh output extent overflow".into())
        })?;
    let mut programs = left.programs().to_vec();
    programs.extend_from_slice(right.programs());
    let spans = [left, right]
        .into_iter()
        .flat_map(|block| {
            (0..block.programs().len()).map(move |index| {
                block.program_span(index).ok_or_else(|| {
                    FmiTangentPlanError::Contract(
                        "public-refresh program has no source span".into(),
                    )
                })
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    solve::ScalarProgramBlock::with_output_indices(programs, spans, (0..count).collect())
        .map_err(|error| FmiTangentPlanError::Contract(error.to_string()))
}

/// Keep complete shared-value programs and source spans, then remap their
/// stores to private contiguous scratch. Rows choose their precise local slot;
/// no source expression is reconstructed or differentiated a second way.
fn select_residual_programs(
    source: &solve::ScalarProgramBlock,
    rows: &[usize],
) -> Result<(solve::ScalarProgramBlock, Vec<usize>), FmiTangentPlanError> {
    let selected = rows
        .iter()
        .map(|&row| {
            source.program_index_for_output(row).ok_or_else(|| {
                FmiTangentPlanError::ImplicitInventory(format!("missing residual row {row}"))
            })
        })
        .collect::<Result<BTreeSet<_>, _>>()?;
    let mut ops = Vec::new();
    let mut spans = Vec::new();
    let mut destinations = Vec::new();
    let mut positions = BTreeMap::new();
    for index in selected {
        let range = source.program_output_ordinals(index).ok_or_else(|| {
            FmiTangentPlanError::ImplicitInventory("missing program output range".into())
        })?;
        for ordinal in range {
            let row = source.output_index_at(ordinal).ok_or_else(|| {
                FmiTangentPlanError::ImplicitInventory("missing program output identity".into())
            })?;
            let position = destinations.len();
            if positions.insert(row, position).is_some() {
                return Err(FmiTangentPlanError::ImplicitInventory(
                    "duplicate residual output".into(),
                ));
            }
            destinations.push(position);
        }
        ops.push(source.programs()[index].clone());
        spans.push(source.program_span(index).ok_or_else(|| {
            FmiTangentPlanError::ImplicitInventory("missing residual source span".into())
        })?);
    }
    let selected = solve::ScalarProgramBlock::with_output_indices(ops, spans, destinations)
        .map_err(|error| FmiTangentPlanError::Contract(error.to_string()))?;
    let rows = rows
        .iter()
        .map(|row| {
            positions.get(row).copied().ok_or_else(|| {
                FmiTangentPlanError::ImplicitInventory(
                    "selected residual row was not emitted".into(),
                )
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok((selected, rows))
}

#[cfg(test)]
mod tests {
    use super::*;
    use solve::{BinaryOp, LinearOp};

    #[test]
    fn tangent_plan_errors_preserve_the_typed_derivation_cause() {
        use std::error::Error;

        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fmi-derivation-error.mo"),
            2,
            9,
        );
        let error = FmiTangentPlanError::Derivation(ad::SolveDerivationError::contract(
            "unowned result",
            span,
        ));
        let cause = error
            .source()
            .expect("a derivation failure must remain inspectable through the error chain")
            .downcast_ref::<ad::SolveDerivationError>()
            .expect("the source must retain its concrete derivation-error type");
        assert_eq!(cause.source_span(), Some(span));
        assert_eq!(error.to_string(), cause.to_string());
        assert!(cause.source().is_none());
        for error in [
            FmiTangentPlanError::IncompleteProfile("profile".into()),
            FmiTangentPlanError::ImplicitInventory("implicit".into()),
            FmiTangentPlanError::InitializationInventory("initialization".into()),
            FmiTangentPlanError::AlternateCoordinates,
            FmiTangentPlanError::Contract("contract".into()),
        ] {
            assert!(error.source().is_none());
        }
    }

    #[test]
    fn consistency_uses_its_direction_scale_without_a_unit_floor() {
        assert!(consistency_row_converged(0.0, 0.0));
        assert!(consistency_row_converged(1e-13, 1.0));
        assert!(consistency_row_converged(1e-25, 1e-12));
        assert!(!consistency_row_converged(1e-12, 1e-12));
        assert!(!consistency_row_converged(1e-12, 0.0));
        assert!(!consistency_row_converged(1e-320, 1e-320));
        assert!(!consistency_row_converged(f64::NAN, 1.0));
        assert!(!consistency_row_converged(0.0, f64::INFINITY));
        assert!(!consistency_row_converged(0.0, -1.0));
    }

    fn shared_rows() -> solve::ScalarProgramBlock {
        let mut sources = rumoca_core::SourceMap::new();
        let id = sources.add("fmi-residual-selection.mo", "x; u; x*u;");
        let span = rumoca_core::Span::from_offsets(id, 0, 10);
        solve::ScalarProgramBlock::with_output_indices(
            vec![
                vec![
                    LinearOp::LoadY { dst: 0, index: 0 },
                    LinearOp::LoadP { dst: 1, index: 0 },
                    LinearOp::StoreOutput { src: 0 },
                    LinearOp::StoreOutput { src: 1 },
                ],
                vec![
                    LinearOp::LoadY { dst: 0, index: 0 },
                    LinearOp::LoadP { dst: 1, index: 0 },
                    LinearOp::Binary {
                        dst: 2,
                        op: BinaryOp::Mul,
                        lhs: 0,
                        rhs: 1,
                    },
                    LinearOp::StoreOutput { src: 2 },
                ],
            ],
            vec![span, span],
            vec![7, 9, 8],
        )
        .unwrap()
    }

    #[test]
    fn reordered_residuals_keep_shared_program_outputs_and_total_parameter_seeds() {
        let (selected, rows) = select_residual_programs(&shared_rows(), &[8, 7]).unwrap();
        assert_eq!(rows, [2, 0]);
        assert_eq!(selected.programs().len(), 2);
        // Differentiate the actual selected block, including its shared extra
        // output, then gather by the issued positions (not by row number).
        let tangent = ad::lower_scalar_program_block_full_jvp(&selected, 1).unwrap();
        let mut output = [0.0; 3];
        crate::eval_scalar_program_block(
            &tangent,
            &[3.0],
            &[2.0],
            0.0,
            Some(&[5.0, 7.0]),
            &mut output,
        )
        .unwrap();
        assert_eq!(output, [5.0, 7.0, 31.0]);
        assert_eq!([output[rows[0]], output[rows[1]]], [31.0, 5.0]);
    }

    #[test]
    fn missing_canonical_residual_is_refused_instead_of_fabricated_zero() {
        assert!(matches!(
            select_residual_programs(&shared_rows(), &[8, 88]),
            Err(FmiTangentPlanError::ImplicitInventory(_))
        ));
    }

    #[test]
    fn observation_values_join_residuals_without_freezing_parameter_targets() {
        let mut sources = rumoca_core::SourceMap::new();
        let id = sources.add("observed-coupling.mo", "z+q-x-u; x+2*z;");
        let algebraic_span = rumoca_core::Span::from_offsets(id, 0, 7);
        let observation_span = rumoca_core::Span::from_offsets(id, 9, 14);
        let residual = solve::ScalarProgramBlock::with_output_indices(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::LoadY { dst: 1, index: 1 },
                LinearOp::LoadP { dst: 2, index: 0 },
                LinearOp::LoadP { dst: 3, index: 1 },
                LinearOp::Binary {
                    dst: 4,
                    op: BinaryOp::Add,
                    lhs: 1,
                    rhs: 3,
                },
                LinearOp::Binary {
                    dst: 5,
                    op: BinaryOp::Sub,
                    lhs: 4,
                    rhs: 0,
                },
                LinearOp::Binary {
                    dst: 6,
                    op: BinaryOp::Sub,
                    lhs: 5,
                    rhs: 2,
                },
                LinearOp::StoreOutput { src: 6 },
            ]],
            vec![algebraic_span],
            vec![0],
        )
        .unwrap();
        let observation = solve::ScalarProgramBlock::with_output_indices(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::LoadY { dst: 1, index: 1 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Add,
                    lhs: 1,
                    rhs: 1,
                },
                LinearOp::Binary {
                    dst: 3,
                    op: BinaryOp::Add,
                    lhs: 0,
                    rhs: 2,
                },
                LinearOp::StoreOutput { src: 3 },
            ]],
            vec![observation_span],
            vec![0],
        )
        .unwrap();
        let joined = join_program_blocks(&residual, &observation).unwrap();
        assert_eq!(joined.output_indices(), [0, 1]);
        assert_eq!(joined.program_span(0), Some(algebraic_span));
        assert_eq!(joined.program_span(1), Some(observation_span));
        let tangent = ad::lower_scalar_program_block_full_jvp(&joined, 2).unwrap();
        let evaluate = |seed: [f64; 4]| {
            let mut values = [0.0; 2];
            crate::eval_scalar_program_block(
                &tangent,
                &[2.0, 1.0],
                &[3.0, 4.0],
                0.0,
                Some(&seed),
                &mut values,
            )
            .unwrap();
            // First row is already a residual. Only q := x+2*z subtracts dq.
            [values[0], values[1] - seed[3]]
        };
        assert_eq!(evaluate([5.0, 0.0, 7.0, 0.0]), [-12.0, 5.0]);
        assert_eq!(evaluate([0.0, 1.0, 0.0, 0.0]), [1.0, 2.0]);
        assert_eq!(evaluate([0.0, 0.0, 0.0, 1.0]), [1.0, -1.0]);
        // Solving [[1,1],[2,-1]] * [dz,dq] = [12,-5] gives
        // dz=7/3, dq=29/3. Treating q as frozen instead gives dz=12.
    }

    #[test]
    fn observation_only_relation_needs_no_algebraic_residual_output() {
        let (empty, empty_rows) = select_residual_programs(&shared_rows(), &[]).unwrap();
        assert!(empty_rows.is_empty());
        let (observations, rows) = select_residual_programs(&shared_rows(), &[9]).unwrap();
        let joined = join_program_blocks(&empty, &observations).unwrap();
        assert_eq!(joined.programs(), observations.programs());
        assert_eq!(joined.output_indices(), observations.output_indices());
        assert_eq!(joined.program_span(0), observations.program_span(0));
        assert_eq!(rows, [1]);
    }
}
