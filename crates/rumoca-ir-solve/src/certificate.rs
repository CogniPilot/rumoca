//! Recomputable certificates for post-commit discrete-event settling.

use std::collections::BTreeSet;

use rumoca_core::Span;

use crate::{
    DiscreteEventPreMode, DiscreteRowRole, LinearOp, PeriodicClockId, RootRelationRefreshRole,
    RuntimeAssignmentRole, ScalarProgramBlock, ScalarSlot, SolveLayout, SolveProblem,
    SolveProblemShapeContractError, SolveVariableStorageRun,
};

/// The exact scalar B.1c rows projected for root-driven runtime refresh.
///
/// This value has no wire representation and can only be issued by
/// [`derive_runtime_assignment_projection`]. The retained source-output rows
/// are mandatory witnesses carried by [`crate::DiscreteSolveSystem`].
pub struct RuntimeAssignmentProjection {
    rhs: ScalarProgramBlock,
    targets: Vec<ScalarSlot>,
    source_rows: Vec<usize>,
    roles: Vec<RuntimeAssignmentRole>,
    post_commit_rhs: ScalarProgramBlock,
    post_commit_targets: Vec<ScalarSlot>,
    post_commit_runtime_rows: Vec<usize>,
    applicability: RuntimeAssignmentApplicability,
}

impl RuntimeAssignmentProjection {
    #[must_use]
    pub const fn rhs(&self) -> &ScalarProgramBlock {
        &self.rhs
    }

    #[must_use]
    pub fn targets(&self) -> &[ScalarSlot] {
        &self.targets
    }

    #[must_use]
    pub fn source_rows(&self) -> &[usize] {
        &self.source_rows
    }

    #[must_use]
    pub fn roles(&self) -> &[RuntimeAssignmentRole] {
        &self.roles
    }

    #[must_use]
    pub const fn post_commit_rhs(&self) -> &ScalarProgramBlock {
        &self.post_commit_rhs
    }

    #[must_use]
    pub fn post_commit_targets(&self) -> &[ScalarSlot] {
        &self.post_commit_targets
    }

    #[must_use]
    pub fn post_commit_runtime_rows(&self) -> &[usize] {
        &self.post_commit_runtime_rows
    }

    /// Consume this proof together with the completed discrete aggregate it
    /// authorizes. Applicability is checked once when the Solve root joins the
    /// prepared aggregate; root construction does not repeat reachability.
    pub fn prepare(
        self,
        system: crate::DiscreteSolveSystem,
        mut events: crate::SolveEventPartition,
    ) -> Result<PreparedDiscreteSolveSystem, SolveProblemShapeContractError> {
        validate_projection_copy(&system, &self)?;
        validate_completed_runtime_certificate(&system, &self)?;
        if self.applicability.root_relation_memory_targets != events.root_relation_memory_targets {
            return Err(runtime_projection_error(
                0,
                "prepared projection is joined to different root-target facts",
                system.rhs.first_source_span(),
            ));
        }
        events.root_relation_refresh_roles = derive_root_relation_refresh_roles(
            &events.root_conditions,
            &system.runtime_assignment_rhs,
            &system.runtime_assignment_targets,
            self.applicability.state_scalar_count,
            self.applicability.solver_scalar_count,
        )?;
        Ok(PreparedDiscreteSolveSystem {
            system,
            events,
            applicability: self.applicability,
        })
    }
}

fn validate_completed_runtime_certificate(
    system: &crate::DiscreteSolveSystem,
    expected: &RuntimeAssignmentProjection,
) -> Result<(), SolveProblemShapeContractError> {
    validate_exact_slice(
        "discrete.runtime_assignment_roles",
        &system.runtime_assignment_roles,
        &expected.roles,
        system.runtime_assignment_rhs.first_source_span(),
    )?;
    validate_exact_slice(
        "discrete.post_commit_assignment_runtime_rows",
        &system.post_commit_assignment_runtime_rows,
        &expected.post_commit_runtime_rows,
        system.post_commit_assignment_rhs.first_source_span(),
    )?;
    validate_exact_slice(
        "discrete.post_commit_assignment_targets",
        &system.post_commit_assignment_targets,
        &expected.post_commit_targets,
        system.post_commit_assignment_rhs.first_source_span(),
    )?;
    if scalar_program_blocks_equal(
        &system.post_commit_assignment_rhs,
        &expected.post_commit_rhs,
    ) {
        Ok(())
    } else {
        Err(SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.post_commit_assignment_rhs",
            row: 0,
            detail: "programs, spans, or output identities differ from the issued projection",
            span: system.post_commit_assignment_rhs.first_source_span(),
        })
    }
}

fn validate_exact_slice<T: PartialEq>(
    context: &'static str,
    actual: &[T],
    expected: &[T],
    span: Option<Span>,
) -> Result<(), SolveProblemShapeContractError> {
    if actual == expected {
        return Ok(());
    }
    let row = actual
        .iter()
        .zip(expected)
        .position(|(actual, expected)| actual != expected)
        .unwrap_or_else(|| actual.len().min(expected.len()));
    Err(SolveProblemShapeContractError::DiscreteCertificate {
        context,
        row,
        detail: "value, completeness, or owner order differs from the issued projection",
        span,
    })
}

struct RuntimeAssignmentApplicability {
    variable_storage_runs: Vec<SolveVariableStorageRun>,
    source_rhs: ScalarProgramBlock,
    source_targets: Vec<ScalarSlot>,
    source_roles: Vec<DiscreteRowRole>,
    source_pre_modes: Vec<DiscreteEventPreMode>,
    source_clock_owners: Vec<Option<PeriodicClockId>>,
    root_relation_memory_targets: Vec<Option<ScalarSlot>>,
    relation_memory_parameter_indices: Vec<usize>,
    state_scalar_count: usize,
    solver_scalar_count: usize,
}

/// Opaque non-wire proof that one completed discrete aggregate carries the
/// sole runtime projection issued for its exact source facts.
pub struct PreparedDiscreteSolveSystem {
    system: crate::DiscreteSolveSystem,
    events: crate::SolveEventPartition,
    applicability: RuntimeAssignmentApplicability,
}

impl PreparedDiscreteSolveSystem {
    pub(crate) fn into_system_for(
        self,
        layout: &SolveLayout,
    ) -> Result<
        (crate::DiscreteSolveSystem, crate::SolveEventPartition),
        SolveProblemShapeContractError,
    > {
        let facts = &self.applicability;
        let exact_source = facts.variable_storage_runs == layout.variable_storage_runs
            && facts.state_scalar_count == layout.state_scalar_count
            && facts.solver_scalar_count == layout.solver_scalar_count()
            && scalar_program_blocks_equal(&facts.source_rhs, &self.system.rhs)
            && facts.source_targets == self.system.update_targets
            && facts.source_roles == self.system.row_roles
            && facts.source_pre_modes == self.system.pre_modes
            && facts.source_clock_owners == self.system.clock_owners
            && facts.relation_memory_parameter_indices == layout.relation_memory_parameter_indices;
        if !exact_source {
            return Err(runtime_projection_error(
                0,
                "prepared projection is joined to different source facts",
                self.system.rhs.first_source_span(),
            ));
        }
        Ok((self.system, self.events))
    }
}

fn validate_count(
    context: &'static str,
    expected: usize,
    actual: usize,
) -> Result<(), SolveProblemShapeContractError> {
    if expected == actual {
        Ok(())
    } else {
        Err(SolveProblemShapeContractError::ScalarProgramCountMismatch {
            context,
            expected,
            actual,
            span: None,
        })
    }
}

fn validate_target(
    context: &'static str,
    row: usize,
    target: ScalarSlot,
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let (storage, index, extent) = match target {
        ScalarSlot::Y { index } => ("Y", index, problem.layout.y_scalars()),
        ScalarSlot::P { index } => ("P", index, problem.layout.p_scalars()),
        ScalarSlot::Time | ScalarSlot::Constant(_) => {
            return Err(SolveProblemShapeContractError::DiscreteCertificate {
                context,
                row,
                detail: "certificate target is not a writable Y/P slot",
                span: None,
            });
        }
    };
    if index < extent {
        Ok(())
    } else {
        Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
            context,
            storage,
            index,
            extent,
            span: None,
        })
    }
}

pub(crate) fn validate_discrete_certificate_shape(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let system = &problem.discrete;
    validate_count(
        "discrete.runtime_assignment_targets",
        system.runtime_assignment_rhs.len(),
        system.runtime_assignment_targets.len(),
    )?;
    validate_count(
        "discrete.post_commit_assignment_targets",
        system.post_commit_assignment_rhs.len(),
        system.post_commit_assignment_targets.len(),
    )?;
    validate_count(
        "events.root_relation_memory_targets",
        problem.events.root_conditions.len(),
        problem.events.root_relation_memory_targets.len(),
    )?;
    for (row, target) in system
        .runtime_assignment_targets
        .iter()
        .copied()
        .enumerate()
    {
        validate_target("discrete.runtime_assignment_targets", row, target, problem)?;
    }
    let mut relation_memory_seen = BTreeSet::new();
    for (row, &index) in problem
        .solve_layout
        .relation_memory_parameter_indices
        .iter()
        .enumerate()
    {
        validate_target(
            "solve_layout.relation_memory_parameter_indices",
            row,
            crate::scalar_slot_p(index),
            problem,
        )?;
        if !relation_memory_seen.insert(index) {
            return Err(SolveProblemShapeContractError::DiscreteCertificate {
                context: "solve_layout.relation_memory_parameter_indices",
                row,
                detail: "relation-memory P index is duplicated",
                span: None,
            });
        }
    }
    validate_count(
        "discrete.runtime_assignment_roles",
        system.runtime_assignment_rhs.len(),
        system.runtime_assignment_roles.len(),
    )?;
    validate_count(
        "discrete.runtime_assignment_rhs.single_output_rows",
        system.runtime_assignment_rhs.len(),
        system.runtime_assignment_rhs.row_count(),
    )?;
    validate_count(
        "discrete.post_commit_assignment_runtime_rows",
        system.post_commit_assignment_rhs.len(),
        system.post_commit_assignment_runtime_rows.len(),
    )?;
    validate_count(
        "discrete.post_commit_assignment_rhs.single_output_rows",
        system.post_commit_assignment_rhs.len(),
        system.post_commit_assignment_rhs.row_count(),
    )?;
    Ok(())
}

pub(crate) fn validate_root_certificate_shape(
    problem: &SolveProblem,
    validate_semantic_certificates: bool,
) -> Result<(), SolveProblemShapeContractError> {
    let events = &problem.events;
    for (row, target) in events
        .root_relation_memory_targets
        .iter()
        .copied()
        .enumerate()
    {
        let Some(target @ ScalarSlot::P { .. }) = target else {
            if target.is_none() {
                continue;
            }
            return Err(SolveProblemShapeContractError::DiscreteCertificate {
                context: "events.root_relation_memory_targets",
                row,
                detail: "root relation-memory target is not a P slot",
                span: events.root_conditions.span_for_output(row),
            });
        };
        validate_target("events.root_relation_memory_targets", row, target, problem)?;
    }
    validate_count(
        "events.root_relation_refresh_roles",
        events.root_conditions.len(),
        events.root_relation_refresh_roles.len(),
    )?;
    validate_count(
        "events.root_conditions.dense_outputs",
        events.root_conditions.len(),
        events.root_conditions.stored_output_count(),
    )?;
    if validate_semantic_certificates {
        validate_root_relation_refresh_certificates(problem)?;
    }
    Ok(())
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum DependencySlot {
    Y(usize),
    P(usize),
}

fn dependency_slot(slot: ScalarSlot) -> Option<DependencySlot> {
    match slot {
        ScalarSlot::Y { index } => Some(DependencySlot::Y(index)),
        ScalarSlot::P { index } => Some(DependencySlot::P(index)),
        ScalarSlot::Time | ScalarSlot::Constant(_) => None,
    }
}

#[derive(Clone)]
struct ProgramFacts {
    dependencies: BTreeSet<DependencySlot>,
    contains_relation: bool,
}

impl ProgramFacts {
    fn extend(&mut self, nested: Self) {
        self.dependencies.extend(nested.dependencies);
        self.contains_relation |= nested.contains_relation;
    }
}

fn insert_dependency_range(
    dependencies: &mut BTreeSet<DependencySlot>,
    input: crate::TensorInputKind,
    start: usize,
    count: usize,
) -> Option<()> {
    let end = start.checked_add(count)?;
    dependencies.extend((start..end).map(|index| match input {
        crate::TensorInputKind::Y => DependencySlot::Y(index),
        crate::TensorInputKind::P => DependencySlot::P(index),
    }));
    Some(())
}

fn program_facts(program: &[LinearOp]) -> Option<ProgramFacts> {
    let mut facts = ProgramFacts {
        dependencies: BTreeSet::new(),
        contains_relation: false,
    };
    for op in program {
        match op {
            LinearOp::LoadY { index, .. } => {
                facts.dependencies.insert(DependencySlot::Y(*index));
            }
            LinearOp::LoadP { index, .. } => {
                facts.dependencies.insert(DependencySlot::P(*index));
            }
            LinearOp::TensorLoad {
                input,
                input_start,
                count,
                ..
            } => {
                insert_dependency_range(&mut facts.dependencies, *input, *input_start, *count)?;
            }
            LinearOp::LoadSeed { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. } => return None,
            LinearOp::FunctionFold { program, .. }
            | LinearOp::GuardedFunctionFold { program, .. } => {
                facts.extend(program_facts(program.update())?);
            }
            LinearOp::FunctionConditional { program, .. } => {
                for arm in program.arms() {
                    facts.extend(program_facts(arm.condition())?);
                    facts.extend(program_facts(arm.result())?);
                }
                facts.extend(program_facts(program.fallback())?);
            }
            LinearOp::StoreOutputFunctionFold { program, .. } => {
                facts.extend(program_facts(program.update())?);
            }
            LinearOp::Compare { .. } => {
                facts.contains_relation = true;
            }
            LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadFoldCarried { .. }
            | LinearOp::LoadFoldIndex { .. }
            | LinearOp::LoadFoldCapture { .. }
            | LinearOp::LoadFunctionConditionalCapture { .. }
            | LinearOp::LoadFunctionConditionalCaptureRange { .. }
            | LinearOp::LoadIndexedRegister { .. }
            | LinearOp::LoadIndexedFoldCarried { .. }
            | LinearOp::LoadIndexedFoldCapture { .. }
            | LinearOp::Move { .. }
            | LinearOp::LinearSolveComponent { .. }
            | LinearOp::DotProduct { .. }
            | LinearOp::MatrixMultiply { .. }
            | LinearOp::TensorBinary { .. }
            | LinearOp::TensorCross { .. }
            | LinearOp::TensorTranspose { .. }
            | LinearOp::TensorConcatenate { .. }
            | LinearOp::TensorUpdate { .. }
            | LinearOp::TensorFill { .. }
            | LinearOp::TensorIdentity { .. }
            | LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Select { .. }
            | LinearOp::PureCall { .. }
            | LinearOp::PureCallDirectional { .. }
            | LinearOp::StoreOutputFoldTensorUpdate { .. }
            | LinearOp::StoreOutputRange { .. }
            | LinearOp::StoreOutput { .. } => {}
        }
    }
    Some(facts)
}

fn program_dependencies(program: &[LinearOp]) -> Option<BTreeSet<DependencySlot>> {
    Some(program_facts(program)?.dependencies)
}

fn checked_dependencies(
    rhs: &ScalarProgramBlock,
    context: &'static str,
) -> Result<Vec<BTreeSet<DependencySlot>>, SolveProblemShapeContractError> {
    rhs.programs()
        .iter()
        .enumerate()
        .map(|(row, program)| {
            program_dependencies(program).ok_or(
                SolveProblemShapeContractError::DiscreteCertificate {
                    context,
                    row,
                    detail: "dependency program is not certifiable",
                    span: rhs.program_span(row),
                },
            )
        })
        .collect()
}

fn checked_program_facts(
    rhs: &ScalarProgramBlock,
    context: &'static str,
) -> Result<Vec<ProgramFacts>, SolveProblemShapeContractError> {
    rhs.programs()
        .iter()
        .enumerate()
        .map(|(row, program)| {
            program_facts(program).ok_or(SolveProblemShapeContractError::DiscreteCertificate {
                context,
                row,
                detail: "dependency program is not certifiable",
                span: rhs.program_span(row),
            })
        })
        .collect()
}

fn runtime_projection_error(
    row: usize,
    detail: &'static str,
    span: Option<rumoca_core::Span>,
) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::DiscreteCertificate {
        context: "discrete.runtime_assignment_source_rows",
        row,
        detail,
        span,
    }
}

fn source_row_is_runtime_eligible(
    layout: &SolveLayout,
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    row_roles: &[DiscreteRowRole],
    pre_modes: &[DiscreteEventPreMode],
    clock_owners: &[Option<PeriodicClockId>],
    source_row: usize,
) -> Result<bool, SolveProblemShapeContractError> {
    if rhs.program_index_for_output(source_row).is_none() {
        return Ok(false);
    }
    if row_roles.get(source_row) != Some(&DiscreteRowRole::Equation)
        || pre_modes.get(source_row) != Some(&DiscreteEventPreMode::FollowCurrent)
        || clock_owners.get(source_row) != Some(&None)
    {
        return Ok(false);
    }
    let Some(target) = targets.get(source_row).copied() else {
        return Err(runtime_projection_error(
            source_row,
            "source row has no aligned update target",
            rhs.span_for_output(source_row),
        ));
    };
    let variable = crate::storage_variable_for_slot(layout, target).map_err(|detail| {
        runtime_projection_error(source_row, detail, rhs.span_for_output(source_row))
    })?;
    let eligible = variable.is_some_and(|variable| {
        matches!(target, ScalarSlot::P { .. })
            && layout.variable_storage_runs[variable]
                .event_iteration_kind()
                .is_some()
    });
    Ok(eligible)
}

fn validate_runtime_projection_source_columns(
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    row_roles: &[DiscreteRowRole],
    pre_modes: &[DiscreteEventPreMode],
    clock_owners: &[Option<PeriodicClockId>],
) -> Result<(), SolveProblemShapeContractError> {
    for (context, actual) in [
        ("discrete.update_targets", targets.len()),
        ("discrete.row_roles", row_roles.len()),
        ("discrete.pre_modes", pre_modes.len()),
        ("discrete.clock_owners", clock_owners.len()),
    ] {
        validate_count(context, rhs.len(), actual)?;
    }
    Ok(())
}

type RuntimeProjectionCandidate = (usize, usize, BTreeSet<DependencySlot>);

fn runtime_projection_candidates(
    layout: &SolveLayout,
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    row_roles: &[DiscreteRowRole],
    pre_modes: &[DiscreteEventPreMode],
    clock_owners: &[Option<PeriodicClockId>],
) -> Result<Vec<RuntimeProjectionCandidate>, SolveProblemShapeContractError> {
    let mut candidates = Vec::new();
    for &source_row in rhs.output_indices() {
        if !source_row_is_runtime_eligible(
            layout,
            rhs,
            targets,
            row_roles,
            pre_modes,
            clock_owners,
            source_row,
        )? {
            continue;
        }
        let program_index = rhs.program_index_for_output(source_row).ok_or_else(|| {
            runtime_projection_error(
                source_row,
                "eligible source output has no program owner",
                rhs.span_for_output(source_row),
            )
        })?;
        let dependencies = program_dependencies(
            rhs.program(program_index)
                .expect("program identity was resolved above"),
        )
        .ok_or_else(|| {
            runtime_projection_error(
                source_row,
                "eligible source dependency program is not certifiable",
                rhs.program_span(program_index),
            )
        })?;
        candidates.push((source_row, program_index, dependencies));
    }
    Ok(candidates)
}

fn select_root_reachable_candidates(
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    candidates: &[RuntimeProjectionCandidate],
    root_relation_memory_targets: &[Option<ScalarSlot>],
) -> Result<Vec<bool>, SolveProblemShapeContractError> {
    let mut reachable = root_relation_memory_targets
        .iter()
        .flatten()
        .copied()
        .filter_map(dependency_slot)
        .collect::<BTreeSet<_>>();
    let mut selected = vec![false; candidates.len()];
    loop {
        let mut changed = false;
        for (candidate, selected) in candidates.iter().zip(&mut selected) {
            if *selected || candidate.2.is_disjoint(&reachable) {
                continue;
            }
            if rhs.stored_output_count_for_program(candidate.1) != Some(1) {
                return Err(runtime_projection_error(
                    candidate.0,
                    "root-reachable multi-output scalar source needs an explicit projection capability",
                    rhs.program_span(candidate.1),
                ));
            }
            let Some(target) = dependency_slot(targets[candidate.0]) else {
                return Err(runtime_projection_error(
                    candidate.0,
                    "eligible source target is not Y/P-backed",
                    rhs.program_span(candidate.1),
                ));
            };
            *selected = true;
            changed |= reachable.insert(target);
        }
        if !changed {
            return Ok(selected);
        }
    }
}

fn copy_selected_runtime_rows(
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    candidates: Vec<RuntimeProjectionCandidate>,
    selected: Vec<bool>,
) -> Result<(ScalarProgramBlock, Vec<ScalarSlot>, Vec<usize>), SolveProblemShapeContractError> {
    let selected = candidates
        .into_iter()
        .zip(selected)
        .filter_map(|(candidate, selected)| selected.then_some(candidate))
        .collect::<Vec<_>>();
    let programs = selected
        .iter()
        .map(|(_, program_index, _)| {
            rhs.program(*program_index)
                .expect("selected program identity remains valid")
                .to_vec()
        })
        .collect::<Vec<_>>();
    let spans = selected
        .iter()
        .map(|(_, program_index, _)| {
            rhs.program_span(*program_index)
                .expect("checked source programs carry provenance")
        })
        .collect::<Vec<_>>();
    let source_rows = selected
        .iter()
        .map(|(source_row, _, _)| *source_row)
        .collect::<Vec<_>>();
    let projected_targets = source_rows
        .iter()
        .map(|source_row| targets[*source_row])
        .collect::<Vec<_>>();
    Ok((
        ScalarProgramBlock::with_program_spans(programs, spans)?,
        projected_targets,
        source_rows,
    ))
}

fn copy_runtime_rows(
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    rows: &[usize],
) -> Result<(ScalarProgramBlock, Vec<ScalarSlot>), SolveProblemShapeContractError> {
    let programs = rows
        .iter()
        .map(|row| {
            rhs.program(*row)
                .expect("selected runtime row remains valid")
                .to_vec()
        })
        .collect::<Vec<_>>();
    let spans = rows
        .iter()
        .map(|row| {
            rhs.program_span(*row)
                .expect("selected runtime row carries provenance")
        })
        .collect::<Vec<_>>();
    let targets = rows.iter().map(|row| targets[*row]).collect();
    Ok((
        ScalarProgramBlock::with_program_spans(programs, spans)?,
        targets,
    ))
}

/// Derive the sole runtime-assignment projection from authoritative scalar
/// B.1c source rows.
///
/// Eligible rows are exactly unconditional, unclocked `FollowCurrent`
/// equation outputs owned by P-backed typed discrete storage. Starting from
/// root-relation-memory targets, the derivation closes transitively over those
/// rows and preserves their source output order. Programs, spans, and targets
/// are copied together; callers cannot supply a parallel selection policy.
pub fn derive_runtime_assignment_projection(
    layout: &SolveLayout,
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    row_roles: &[DiscreteRowRole],
    pre_modes: &[DiscreteEventPreMode],
    clock_owners: &[Option<PeriodicClockId>],
    root_relation_memory_targets: &[Option<ScalarSlot>],
) -> Result<RuntimeAssignmentProjection, SolveProblemShapeContractError> {
    validate_runtime_projection_source_columns(rhs, targets, row_roles, pre_modes, clock_owners)?;

    let candidates =
        runtime_projection_candidates(layout, rhs, targets, row_roles, pre_modes, clock_owners)?;
    let selected =
        select_root_reachable_candidates(rhs, targets, &candidates, root_relation_memory_targets)?;

    let (projected_rhs, projected_targets, source_rows) =
        copy_selected_runtime_rows(rhs, targets, candidates, selected)?;
    let roles = derive_runtime_assignment_roles(
        &projected_rhs,
        &projected_targets,
        &layout.relation_memory_parameter_indices,
    )?;
    let reachable = derive_root_reachable_runtime_rows(
        &projected_rhs,
        &projected_targets,
        root_relation_memory_targets,
        &roles,
    )?;
    let post_commit_runtime_rows = roles
        .iter()
        .zip(reachable)
        .enumerate()
        .filter_map(|(row, (role, reachable))| {
            (*role == RuntimeAssignmentRole::RelationFree && reachable).then_some(row)
        })
        .collect::<Vec<_>>();
    let (post_commit_rhs, post_commit_targets) = copy_runtime_rows(
        &projected_rhs,
        &projected_targets,
        &post_commit_runtime_rows,
    )?;
    Ok(RuntimeAssignmentProjection {
        rhs: projected_rhs,
        targets: projected_targets,
        source_rows,
        roles,
        post_commit_rhs,
        post_commit_targets,
        post_commit_runtime_rows,
        applicability: RuntimeAssignmentApplicability {
            variable_storage_runs: layout.variable_storage_runs.clone(),
            source_rhs: rhs.clone(),
            source_targets: targets.to_vec(),
            source_roles: row_roles.to_vec(),
            source_pre_modes: pre_modes.to_vec(),
            source_clock_owners: clock_owners.to_vec(),
            root_relation_memory_targets: root_relation_memory_targets.to_vec(),
            relation_memory_parameter_indices: layout.relation_memory_parameter_indices.clone(),
            state_scalar_count: layout.state_scalar_count,
            solver_scalar_count: layout.solver_scalar_count(),
        },
    })
}

/// Derive the row-aligned relation role of runtime B.1c assignment owners.
pub fn derive_runtime_assignment_roles(
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    relation_memory_parameter_indices: &[usize],
) -> Result<Vec<RuntimeAssignmentRole>, SolveProblemShapeContractError> {
    validate_count(
        "discrete.runtime_assignment_targets",
        rhs.len(),
        targets.len(),
    )?;
    validate_count(
        "discrete.runtime_assignment_rhs.single_output_rows",
        rhs.len(),
        rhs.row_count(),
    )?;
    let relation_memory = relation_memory_parameter_indices
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    let facts = checked_program_facts(rhs, "discrete.runtime_assignment_roles")?;
    let mut roles = rhs
        .programs()
        .iter()
        .zip(&facts)
        .zip(targets.iter().copied())
        .map(|((_program, facts), target)| {
            let writes_relation_memory =
                matches!(target, ScalarSlot::P { index } if relation_memory.contains(&index));
            if writes_relation_memory || facts.contains_relation {
                RuntimeAssignmentRole::RelationEvaluating
            } else {
                RuntimeAssignmentRole::RelationFree
            }
        })
        .collect::<Vec<_>>();

    loop {
        let relation_evaluating_targets = roles
            .iter()
            .zip(targets.iter().copied())
            .filter_map(|(role, target)| {
                (*role == RuntimeAssignmentRole::RelationEvaluating)
                    .then(|| dependency_slot(target))
                    .flatten()
            })
            .collect::<BTreeSet<_>>();
        let mut changed = false;
        for (role, facts) in roles.iter_mut().zip(&facts) {
            if *role == RuntimeAssignmentRole::RelationFree
                && !facts.dependencies.is_disjoint(&relation_evaluating_targets)
            {
                *role = RuntimeAssignmentRole::RelationEvaluating;
                changed = true;
            }
        }
        if !changed {
            return Ok(roles);
        }
    }
}

/// Prove the root-memory-reachable closure represented by a runtime plan.
pub fn derive_root_reachable_runtime_rows(
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    root_relation_memory_targets: &[Option<ScalarSlot>],
    roles: &[RuntimeAssignmentRole],
) -> Result<Vec<bool>, SolveProblemShapeContractError> {
    derive_reachable_runtime_rows(rhs, targets, root_relation_memory_targets, roles, false)
}

fn derive_reachable_runtime_rows(
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    root_relation_memory_targets: &[Option<ScalarSlot>],
    roles: &[RuntimeAssignmentRole],
    traverse_relation_evaluating: bool,
) -> Result<Vec<bool>, SolveProblemShapeContractError> {
    validate_count(
        "discrete.runtime_assignment_targets",
        rhs.len(),
        targets.len(),
    )?;
    validate_count("discrete.runtime_assignment_roles", rhs.len(), roles.len())?;
    validate_count(
        "discrete.runtime_assignment_rhs.single_output_rows",
        rhs.len(),
        rhs.row_count(),
    )?;
    let dependencies = checked_dependencies(rhs, "discrete.runtime_assignment_rhs")?;
    let mut reachable = root_relation_memory_targets
        .iter()
        .flatten()
        .copied()
        .filter_map(dependency_slot)
        .collect::<BTreeSet<_>>();
    let mut selected = vec![false; dependencies.len()];
    loop {
        let mut changed = false;
        for (row, dependencies) in dependencies.iter().enumerate() {
            if selected[row]
                || (!traverse_relation_evaluating
                    && roles.get(row) == Some(&RuntimeAssignmentRole::RelationEvaluating))
                || dependencies.is_disjoint(&reachable)
            {
                continue;
            }
            let Some(target) = targets.get(row).copied().and_then(dependency_slot) else {
                return Err(SolveProblemShapeContractError::DiscreteCertificate {
                    context: "discrete.runtime_assignment_targets",
                    row,
                    detail: "root-reachable assignment has no Y/P target",
                    span: rhs.program_span(row),
                });
            };
            selected[row] = true;
            changed |= reachable.insert(target);
        }
        if !changed {
            return Ok(selected);
        }
    }
}

/// Derive root rows whose typed dependency closure reaches algebraic/output Y.
pub fn derive_root_relation_refresh_roles(
    roots: &ScalarProgramBlock,
    runtime_rhs: &ScalarProgramBlock,
    runtime_targets: &[ScalarSlot],
    state_scalar_count: usize,
    solver_scalar_count: usize,
) -> Result<Vec<RootRelationRefreshRole>, SolveProblemShapeContractError> {
    validate_count(
        "discrete.runtime_assignment_targets",
        runtime_rhs.len(),
        runtime_targets.len(),
    )?;
    validate_count(
        "discrete.runtime_assignment_rhs.single_output_rows",
        runtime_rhs.len(),
        runtime_rhs.row_count(),
    )?;
    let mut algebraic = (state_scalar_count..solver_scalar_count)
        .map(DependencySlot::Y)
        .collect::<BTreeSet<_>>();
    let dependencies = checked_dependencies(runtime_rhs, "discrete.runtime_assignment_rhs")?;
    let assignments = dependencies
        .into_iter()
        .zip(runtime_targets.iter().copied())
        .enumerate()
        .map(|(row, (dependencies, target))| {
            let target = dependency_slot(target).ok_or(
                SolveProblemShapeContractError::DiscreteCertificate {
                    context: "discrete.runtime_assignment_targets",
                    row,
                    detail: "runtime assignment has no Y/P target",
                    span: runtime_rhs.program_span(row),
                },
            )?;
            Ok((target, dependencies))
        })
        .collect::<Result<Vec<_>, SolveProblemShapeContractError>>()?;
    loop {
        let mut changed = false;
        for (target, dependencies) in &assignments {
            if !dependencies.is_disjoint(&algebraic) {
                changed |= algebraic.insert(*target);
            }
        }
        if !changed {
            break;
        }
    }
    let dependencies = checked_dependencies(roots, "events.root_relation_refresh_roles")?;
    let mut roles = Vec::with_capacity(roots.stored_output_count());
    for (program_index, dependencies) in dependencies.into_iter().enumerate() {
        let role = if dependencies.is_disjoint(&algebraic) {
            RootRelationRefreshRole::Frozen
        } else {
            RootRelationRefreshRole::AlgebraicDependent
        };
        roles.extend(std::iter::repeat_n(
            role,
            roots.stored_output_count_for_program(program_index).ok_or(
                SolveProblemShapeContractError::DiscreteCertificate {
                    context: "events.root_relation_refresh_roles",
                    row: program_index,
                    detail: "missing retained scalar program output width",
                    span: roots.program_span(program_index),
                },
            )?,
        ));
    }
    Ok(roles)
}

pub(crate) fn validate_runtime_assignment_source_projection(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let system = &problem.discrete;
    validate_count(
        "discrete.runtime_assignment_source_rows",
        system.runtime_assignment_rhs.len(),
        system.runtime_assignment_source_rows.len(),
    )?;
    let expected = derive_runtime_assignment_projection(
        &problem.solve_layout,
        &system.rhs,
        &system.update_targets,
        &system.row_roles,
        &system.pre_modes,
        &system.clock_owners,
        &problem.events.root_relation_memory_targets,
    )?;
    validate_projection_copy(system, &expected)?;
    validate_completed_runtime_certificate(system, &expected)
}

fn scalar_program_blocks_equal(lhs: &ScalarProgramBlock, rhs: &ScalarProgramBlock) -> bool {
    lhs.programs().len() == rhs.programs().len()
        && lhs
            .programs()
            .iter()
            .zip(rhs.programs())
            .all(|(lhs, rhs)| scalar_programs_bitwise_equal(lhs, rhs))
        && lhs.program_spans() == rhs.program_spans()
        && lhs.output_indices() == rhs.output_indices()
}

/// Compare complete instruction values through the owning recursive bitwise
/// authority, not Rust's numerical `PartialEq`.
fn scalar_programs_bitwise_equal(lhs: &[LinearOp], rhs: &[LinearOp]) -> bool {
    LinearOp::slice_bitwise_eq(lhs, rhs)
}

fn validate_projection_copy(
    system: &crate::DiscreteSolveSystem,
    expected: &RuntimeAssignmentProjection,
) -> Result<(), SolveProblemShapeContractError> {
    if system.runtime_assignment_source_rows != expected.source_rows {
        let row = system
            .runtime_assignment_source_rows
            .iter()
            .zip(&expected.source_rows)
            .position(|(actual, expected)| actual != expected)
            .unwrap_or_else(|| {
                system
                    .runtime_assignment_source_rows
                    .len()
                    .min(expected.source_rows.len())
            });
        return Err(runtime_projection_error(
            row,
            "witnesses are not the exact owner-ordered root-reachable source projection",
            system.runtime_assignment_rhs.program_span(row),
        ));
    }
    if !scalar_program_blocks_equal(&system.runtime_assignment_rhs, &expected.rhs) {
        return Err(runtime_projection_error(
            0,
            "runtime programs, spans, or output identities are not the exact source copies",
            system.runtime_assignment_rhs.first_source_span(),
        ));
    }
    if system.runtime_assignment_targets != expected.targets {
        let row = system
            .runtime_assignment_targets
            .iter()
            .zip(&expected.targets)
            .position(|(actual, expected)| actual != expected)
            .unwrap_or_else(|| {
                system
                    .runtime_assignment_targets
                    .len()
                    .min(expected.targets.len())
            });
        return Err(runtime_projection_error(
            row,
            "runtime targets are not the exact source copies",
            system.runtime_assignment_rhs.program_span(row),
        ));
    }
    Ok(())
}

pub(crate) fn validate_root_relation_refresh_certificates(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let events = &problem.events;
    let derived_roles = derive_root_relation_refresh_roles(
        &events.root_conditions,
        &problem.discrete.runtime_assignment_rhs,
        &problem.discrete.runtime_assignment_targets,
        problem.solve_layout.state_scalar_count(),
        problem.solve_layout.solver_scalar_count(),
    )?;
    for (row, (actual, expected)) in events
        .root_relation_refresh_roles
        .iter()
        .zip(derived_roles)
        .enumerate()
    {
        if *actual != expected {
            return Err(SolveProblemShapeContractError::DiscreteCertificate {
                context: "events.root_relation_refresh_roles",
                row,
                detail: "role disagrees with independently derived algebraic dependency",
                span: events.root_conditions.span_for_output(row),
            });
        }
    }
    Ok(())
}
