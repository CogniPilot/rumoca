//! SOLVE-C62 recursive owner groups of one pure-call table.
//!
//! A group reserves the ids of every member before any member body exists,
//! so member bodies may call one another. Construction proves the members
//! form one recursive SCC, derives their dependency summaries as a least
//! fixed point, and bounds the stack one chain of active member invocations
//! may occupy by the execution profile's declared depth limit.

use super::dependency::{self, SolveCallDependency};
use super::{
    SolvePureCallIdentity, SolvePureCallInterface, SolvePureCallOutput, SolvePureCallOutputKind,
    SolvePureCallOwner, SolvePureCallOwnerId, SolvePureCallTableView, next_owner_id,
    require_owner_interface, validate_owner_body,
};
use crate::typed_program::program::{SolveOperation, SolveProgramConstructionError, TypedProgram};
use crate::typed_program::types::{SolveArithmeticProfile, SolveValueType};
use rumoca_core::Span;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Recursion and stack limits declared by one execution environment profile
/// (SPEC_0047 §4.25).
///
/// `depth_limit` bounds the active member invocations of one recursive group
/// along a call chain; `frame_cell_budget` bounds that depth times the largest
/// member frame, in typed scalar cells.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SolveRecursionProfile {
    depth_limit: u32,
    frame_cell_budget: u64,
}

impl SolveRecursionProfile {
    /// The hosted simulation profile.
    pub const HOSTED: Self = Self {
        depth_limit: 64,
        frame_cell_budget: 1 << 16,
    };

    #[must_use]
    pub const fn depth_limit(self) -> u32 {
        self.depth_limit
    }

    #[must_use]
    pub const fn frame_cell_budget(self) -> u64 {
        self.frame_cell_budget
    }
}

/// One constructed recursive owner group: the contiguous owner ids it
/// reserved, the depth limit its execution applies, and its largest member
/// frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SolveRecursiveGroup {
    start: u32,
    end: u32,
    depth_limit: u32,
    frame_cells: u64,
}

impl SolveRecursiveGroup {
    #[must_use]
    pub const fn contains(&self, owner: SolvePureCallOwnerId) -> bool {
        self.start <= owner.index() && owner.index() < self.end
    }

    /// The largest number of member invocations one call chain may hold
    /// active at once.
    #[must_use]
    pub const fn depth_limit(&self) -> u32 {
        self.depth_limit
    }

    /// The largest member frame in typed scalar cells, nested regions included.
    #[must_use]
    pub const fn frame_cells(&self) -> u64 {
        self.frame_cells
    }

    pub(super) const fn start(&self) -> u32 {
        self.start
    }

    pub(super) const fn end(&self) -> u32 {
        self.end
    }
}

/// The typed interface of one group member, known before its body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SolveRecursiveMember {
    identity: SolvePureCallIdentity,
    inputs: Vec<SolveValueType>,
    outputs: Vec<SolvePureCallOutput>,
    provenance: Span,
}

impl SolveRecursiveMember {
    #[must_use]
    pub const fn new(
        identity: SolvePureCallIdentity,
        inputs: Vec<SolveValueType>,
        outputs: Vec<SolvePureCallOutput>,
        provenance: Span,
    ) -> Self {
        Self {
            identity,
            inputs,
            outputs,
            provenance,
        }
    }

    pub(super) fn interface(&self) -> (Vec<SolveValueType>, Vec<SolvePureCallOutput>, Span) {
        (self.inputs.clone(), self.outputs.clone(), self.provenance)
    }
}

/// A reserved member visible to member bodies while the group is built.
#[derive(Debug)]
pub(in crate::typed_program) struct PendingOwner {
    id: SolvePureCallOwnerId,
    inputs: Arc<[SolveValueType]>,
    outputs: Arc<[SolvePureCallOutput]>,
    dependencies: Box<[Box<[SolveCallDependency]>]>,
}

impl PendingOwner {
    pub(super) const fn id(&self) -> SolvePureCallOwnerId {
        self.id
    }

    pub(super) fn interface(&self) -> SolvePureCallInterface<'_> {
        SolvePureCallInterface {
            id: self.id,
            inputs: &self.inputs,
            outputs: &self.outputs,
            dependencies: &self.dependencies,
            projections: None,
            affinity: None,
        }
    }
}

/// Construct one group after `owners`, building member `i`'s body with
/// `body(i, ids, view)`. Shared by the table builder and wire replay.
pub(super) fn construct_group(
    owners: &mut Vec<SolvePureCallOwner>,
    groups: &mut Vec<SolveRecursiveGroup>,
    arithmetic: SolveArithmeticProfile,
    profile: SolveRecursionProfile,
    members: Vec<SolveRecursiveMember>,
    mut body: impl FnMut(
        usize,
        &[SolvePureCallOwnerId],
        SolvePureCallTableView<'_>,
    ) -> Result<TypedProgram, SolveProgramConstructionError>,
) -> Result<SolveRecursiveGroup, SolveProgramConstructionError> {
    let provenance = members
        .first()
        .map(|member| member.provenance)
        .ok_or(SolveProgramConstructionError::MissingProvenance)?;
    let mut pending = Vec::with_capacity(members.len());
    for (ordinal, member) in members.iter().enumerate() {
        require_owner_interface(
            arithmetic,
            &member.inputs,
            &member.outputs,
            member.provenance,
        )?;
        if member
            .outputs
            .iter()
            .any(|output| output.kind() != SolvePureCallOutputKind::Result)
        {
            return Err(SolveProgramConstructionError::RecursiveAssertion {
                provenance: member.provenance,
            });
        }
        let duplicate = owners.iter().any(|owner| owner.identity == member.identity)
            || members[..ordinal]
                .iter()
                .any(|prior| prior.identity == member.identity);
        if duplicate {
            return Err(SolveProgramConstructionError::DuplicateCallIdentity {
                provenance: member.provenance,
            });
        }
        let id = next_owner_id(owners.len() + ordinal, member.provenance)?;
        pending.push(PendingOwner {
            id,
            inputs: member.inputs.clone().into(),
            outputs: member.outputs.clone().into(),
            dependencies: vec![Box::default(); member.outputs.len()].into(),
        });
    }
    let ids = pending.iter().map(PendingOwner::id).collect::<Vec<_>>();
    let bodies = (0..members.len())
        .map(|ordinal| {
            body(
                ordinal,
                &ids,
                SolvePureCallTableView::with_pending(owners, &pending),
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    require_one_recursive_component(&bodies, &ids, provenance)?;
    let coordinates = bodies
        .iter()
        .zip(&members)
        .map(|(body, member)| {
            validate_owner_body(body, &member.inputs, &member.outputs, member.provenance)
        })
        .collect::<Result<Vec<_>, _>>()?;
    least_dependencies(owners, &mut pending, &bodies)?;
    let frame_cells = bodies.iter().map(program_cells).max().unwrap_or(0);
    let stack_cells = u64::from(profile.depth_limit()).checked_mul(frame_cells);
    if stack_cells.is_none_or(|cells| cells > profile.frame_cell_budget()) {
        return Err(SolveProgramConstructionError::RecursionFrameBound { provenance });
    }
    let start = ids[0].index();
    for (((pending, member), body), input_coordinate) in pending
        .into_iter()
        .zip(members)
        .zip(bodies)
        .zip(coordinates)
    {
        owners.push(SolvePureCallOwner {
            input_coordinate,
            dependencies: Arc::from(pending.dependencies),
            projections: None,
            affinity: None,
            id: pending.id,
            identity: member.identity,
            inputs: pending.inputs,
            outputs: pending.outputs,
            body,
            directional: None,
            provenance: member.provenance,
        });
    }
    let group = SolveRecursiveGroup {
        start,
        end: start + ids.len() as u32,
        depth_limit: profile.depth_limit(),
        frame_cells,
    };
    groups.push(group);
    Ok(group)
}

/// The member call graph must be exactly one SCC containing a cycle; a set
/// of members that could be issued in order is not a recursive group.
fn require_one_recursive_component(
    bodies: &[TypedProgram],
    ids: &[SolvePureCallOwnerId],
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let start = ids[0].index();
    let edges = bodies
        .iter()
        .map(|body| {
            let mut callees = Vec::new();
            visit_calls(body, &mut |owner| {
                if let Some(member) = owner.index().checked_sub(start)
                    && (member as usize) < ids.len()
                {
                    callees.push(member as usize);
                }
            });
            callees
        })
        .collect::<Vec<_>>();
    let components = rumoca_core::dependency_first_sccs(&edges)
        .map_err(|_| SolveProgramConstructionError::InvalidRecursiveGroup { provenance })?;
    if matches!(components.as_slice(), [component] if component.recursive) {
        Ok(())
    } else {
        Err(SolveProgramConstructionError::InvalidRecursiveGroup { provenance })
    }
}

/// Least fixed point of the member summaries. Each iteration widens every
/// dependency to its whole input, so the lattice is the finite set of input
/// subsets per output and the iteration terminates.
fn least_dependencies(
    owners: &[SolvePureCallOwner],
    pending: &mut [PendingOwner],
    bodies: &[TypedProgram],
) -> Result<(), SolveProgramConstructionError> {
    loop {
        let mut next = Vec::with_capacity(bodies.len());
        for (body, member) in bodies.iter().zip(pending.iter()) {
            let view = SolvePureCallTableView::with_pending(owners, pending);
            let derived =
                dependency::derive(body, member.inputs.len(), member.outputs.len(), view)?;
            next.push(dependency::widen(derived));
        }
        let mut changed = false;
        for (member, dependencies) in pending.iter_mut().zip(next) {
            if member.dependencies != dependencies {
                member.dependencies = dependencies;
                changed = true;
            }
        }
        if !changed {
            return Ok(());
        }
    }
}

/// Visit every owner a program calls, nested regions included.
pub(super) fn visit_calls(program: &TypedProgram, visit: &mut impl FnMut(SolvePureCallOwnerId)) {
    for operation in program.operations() {
        match operation.operation() {
            SolveOperation::Call { owner, .. } => visit(*owner),
            SolveOperation::Conditional {
                if_true, if_false, ..
            } => {
                visit_calls(if_true.body(), visit);
                visit_calls(if_false.body(), visit);
            }
            SolveOperation::Map { body, .. } => visit_calls(body.body(), visit),
            SolveOperation::Fold { transition, .. } => visit_calls(transition.body(), visit),
            _ => {}
        }
    }
}

/// Typed scalar cells one invocation of `program` holds: its slots and
/// registers plus every nested region's.
fn program_cells(program: &TypedProgram) -> u64 {
    let own = program
        .slots()
        .iter()
        .map(|slot| u64::from(slot.value_type().scalar_count()))
        .chain(
            program
                .register_types()
                .iter()
                .map(|value_type| u64::from(value_type.scalar_count())),
        )
        .sum::<u64>();
    let nested = program
        .operations()
        .iter()
        .map(|operation| match operation.operation() {
            SolveOperation::Conditional {
                if_true, if_false, ..
            } => program_cells(if_true.body()) + program_cells(if_false.body()),
            SolveOperation::Map { body, .. } => program_cells(body.body()),
            SolveOperation::Fold { transition, .. } => program_cells(transition.body()),
            _ => 0,
        })
        .sum::<u64>();
    own + nested
}
