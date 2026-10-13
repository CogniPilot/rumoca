//! One target helper per reachable, construction-issued pure-call owner.
#[cfg(test)]
mod tests;
use super::{
    TypedCallCompileError,
    layout::{Callees, Demand, FramePlan, InputConvention, Liveness, visit_calls},
};
use crate::emit::MathImport;
use rumoca_ir_solve as solve;
use std::collections::{BTreeMap, BTreeSet};

pub(super) struct LinkedOwners {
    pub plans: Vec<Option<FramePlan>>,
    /// The same bodies with top-level scalar registers in wasm locals, for an
    /// owner emitted as one function. Callers size frames from `plans`, which
    /// never needs less scratch than these.
    pub promoted: Vec<Option<FramePlan>>,
    /// Indexed by unit: an owner built for all its outputs is unit `owner`, an
    /// owner built for fewer follows the owners (SOLVE-C95).
    pub functions: Vec<Option<u32>>,
    /// The owner each unit is built from.
    pub unit_owner: Vec<u32>,
    /// The unit of each owner built for fewer than all of its outputs.
    pub unit_index: BTreeMap<(u32, Demand), usize>,
    /// The outputs of each owner that are built whatever a site reads.
    pub always: Vec<Box<[bool]>>,
    /// The units in the order they are built and numbered.
    pub order: Vec<usize>,
    /// The unit each root is called as, by owner.
    pub root_units: BTreeMap<u32, usize>,
    pub math_imports: Vec<MathImport>,
    pub math_indices: BTreeMap<MathImport, u32>,
}

impl LinkedOwners {
    pub(super) fn construct(
        table: &solve::SolvePureCallTable,
        root: solve::SolvePureCallOwnerId,
    ) -> Result<Self, TypedCallCompileError> {
        Self::construct_roots(table, &[root])
    }

    pub(super) fn construct_roots(
        table: &solve::SolvePureCallTable,
        roots: &[solve::SolvePureCallOwnerId],
    ) -> Result<Self, TypedCallCompileError> {
        Self::construct_admitted(
            table,
            &all_outputs(roots),
            None,
            (InputConvention::Packed, InputConvention::Packed, false),
        )
    }

    /// The native call program: its roots, which its entry glue calls, take a
    /// large Binary64 Real value by reference; every helper they reach receives
    /// every large input by reference.
    pub(super) fn construct_program(
        table: &solve::SolvePureCallTable,
        roots: &[(solve::SolvePureCallOwnerId, Demand)],
    ) -> Result<Self, TypedCallCompileError> {
        Self::construct_admitted(
            table,
            roots,
            None,
            (InputConvention::Stage, InputConvention::ByReference, true),
        )
    }

    /// Only this immutable projection supplies observation admission authority.
    pub(super) fn construct_checked(
        invocation: &solve::CheckedAssertionInvocation<'_>,
    ) -> Result<Self, TypedCallCompileError> {
        Self::construct_admitted(
            invocation.table(),
            &all_outputs(&[invocation.owner().id()]),
            Some(invocation),
            (InputConvention::Packed, InputConvention::Packed, false),
        )
    }

    fn construct_admitted(
        table: &solve::SolvePureCallTable,
        roots: &[(solve::SolvePureCallOwnerId, Demand)],
        invocation: Option<&solve::CheckedAssertionInvocation<'_>>,
        (roots_convention, helpers, by_demand): (InputConvention, InputConvention, bool),
    ) -> Result<Self, TypedCallCompileError> {
        let mut reachable = vec![false; table.owners().len()];
        let mut pending = roots.iter().map(|(root, _)| *root).collect::<Vec<_>>();
        let mut math_imports = BTreeSet::new();
        while let Some(id) = pending.pop() {
            let owner = table.owner(id).ok_or(TypedCallCompileError::SiteMismatch)?;
            // SOLVE-C62 groups need depth-carrying frames and source-bound
            // stack failures. The native typed-call ABI does not execute them.
            if table.recursive_group(id).is_some() {
                return Err(TypedCallCompileError::UnsupportedRecursiveGroup {
                    owner: id,
                    provenance: owner.provenance(),
                });
            }
            if reachable[id.index() as usize] {
                continue;
            }
            reachable[id.index() as usize] = true;
            if invocation.is_none() {
                check_outputs(owner)?;
            } else if owner
                .outputs()
                .iter()
                .any(|output| output.assertion_level().is_some())
                && owner.assertion_flow().is_none()
            {
                return Err(TypedCallCompileError::SiteMismatch);
            }
            super::math::collect(owner.body(), &mut math_imports);
            collect_calls(owner.body(), &mut pending);
        }
        let math_imports = math_imports.into_iter().collect::<Vec<_>>();
        let math_indices = math_imports
            .iter()
            .enumerate()
            .map(|(i, import)| (*import, i as u32))
            .collect();
        let next =
            u32::try_from(math_imports.len()).map_err(|_| TypedCallCompileError::SizeLimit)?;
        let always = table
            .owners()
            .iter()
            .map(|owner| always_built(owner, by_demand))
            .collect::<Vec<_>>();
        let roots = roots
            .iter()
            .map(|(root, demand)| {
                (
                    root.index(),
                    Callees::built_for(&always, *root, demand.clone()),
                )
            })
            .collect::<Vec<_>>();
        let units = admitted_units(table, &roots, &reachable, &always);
        let mut linked = Self::with_units(table.owners().len(), &units, always);
        for (owner, demand) in &roots {
            let unit = linked.unit_of(*owner, demand)?;
            linked.root_units.insert(*owner, unit);
        }
        linked.math_imports = math_imports;
        linked.math_indices = math_indices;
        // Admission above excludes SOLVE-C62 groups. The remaining checked
        // calls reference only previously issued owners (SOLVE-C52), so
        // building the units by owner index builds every callee first.
        let mut next = next;
        for (owner, demand) in &units {
            let owner_ref = &table.owners()[*owner as usize];
            let unit = linked.unit_of(*owner, demand)?;
            let convention = if roots.contains(&(*owner, demand.clone())) {
                roots_convention
            } else {
                helpers
            };
            let callees = Callees::new(&linked.plans, &linked.unit_index, &linked.always);
            let plan = Self::construct_owner_plan(
                owner_ref,
                invocation.is_some(),
                (convention, demand),
                &callees,
            )?;
            let promoted = match invocation {
                None => Some(promoted_plan(
                    owner_ref,
                    &callees,
                    (convention, demand),
                    &plan,
                )?),
                Some(_) => None,
            };
            linked.plans[unit] = Some(plan);
            linked.promoted[unit] = promoted;
            linked.functions[unit] = Some(next);
            linked.order.push(unit);
            next = next
                .checked_add(1)
                .ok_or(TypedCallCompileError::SizeLimit)?;
        }
        Ok(linked)
    }

    /// The tables of `units`, none of them built yet. The unit of an owner built
    /// for all of its outputs is the owner's index; any other follows the owners.
    fn with_units(
        owners: usize,
        units: &BTreeSet<(u32, Demand)>,
        always: Vec<Box<[bool]>>,
    ) -> Self {
        let mut unit_index = BTreeMap::new();
        let mut unit_owner = (0..owners as u32).collect::<Vec<_>>();
        for (owner, demand) in units.iter().filter(|(_, demand)| !demand.is_all()) {
            unit_index.insert((*owner, demand.clone()), unit_owner.len());
            unit_owner.push(*owner);
        }
        let count = unit_owner.len();
        Self {
            plans: (0..count).map(|_| None).collect(),
            promoted: (0..count).map(|_| None).collect(),
            functions: vec![None; count],
            math_imports: Vec::new(),
            math_indices: BTreeMap::new(),
            unit_owner,
            unit_index,
            always,
            order: Vec::new(),
            root_units: BTreeMap::new(),
        }
    }

    fn unit_of(&self, owner: u32, demand: &Demand) -> Result<usize, TypedCallCompileError> {
        if demand.is_all() {
            return Ok(owner as usize);
        }
        self.unit_index
            .get(&(owner, demand.clone()))
            .copied()
            .ok_or(TypedCallCompileError::SiteMismatch)
    }

    fn construct_owner_plan(
        owner: &solve::SolvePureCallOwner,
        invocation: bool,
        (convention, demand): (InputConvention, &Demand),
        callees: &Callees<'_>,
    ) -> Result<FramePlan, TypedCallCompileError> {
        let plan = if invocation && owner.assertion_flow().is_some() {
            let observations = super::observations::ObservationPlan::construct(owner, callees)?;
            FramePlan::construct_with_observations(
                owner.body(),
                callees,
                observations,
                (InputConvention::Packed, demand),
                false,
            )?
        } else {
            FramePlan::construct_with(owner.body(), callees, convention, demand)?
        };
        Ok(plan)
    }

    /// The first function index after every owner's entry; continuations follow it.
    pub(super) fn first_free_function(&self) -> u32 {
        self.functions
            .iter()
            .flatten()
            .max()
            .map_or(self.math_imports.len() as u32, |last| last + 1)
    }

    /// The plan `unit` is emitted from when it is not outlined.
    pub(super) fn unit_emit_plan(&self, unit: usize) -> Result<&FramePlan, TypedCallCompileError> {
        match self.promoted.get(unit).and_then(Option::as_ref) {
            Some(promoted) => Ok(promoted),
            None => self.unit_plan(unit),
        }
    }

    /// The plan of the owner built for all of its outputs.
    pub(super) fn plan(
        &self,
        id: solve::SolvePureCallOwnerId,
    ) -> Result<&FramePlan, TypedCallCompileError> {
        self.unit_plan(id.index() as usize)
    }

    /// The unit a root is called as: built for the outputs its callers read.
    pub(super) fn root_unit(&self, owner: solve::SolvePureCallOwnerId) -> usize {
        self.root_units
            .get(&owner.index())
            .copied()
            .unwrap_or(owner.index() as usize)
    }

    pub(super) fn unit_plan(&self, unit: usize) -> Result<&FramePlan, TypedCallCompileError> {
        self.plans
            .get(unit)
            .and_then(Option::as_ref)
            .ok_or(TypedCallCompileError::SiteMismatch)
    }
}

/// The owner's body with top-level scalars in wasm locals. Its callers
/// reserved the frame of `plain`, so it may never need more.
fn promoted_plan(
    owner: &solve::SolvePureCallOwner,
    callees: &Callees<'_>,
    (convention, demand): (InputConvention, &Demand),
    plain: &FramePlan,
) -> Result<FramePlan, TypedCallCompileError> {
    let promoted = FramePlan::construct_promoted(owner.body(), callees, convention, demand)?;
    if promoted.scratch_bytes > plain.scratch_bytes
        || promoted.input_bytes != plain.input_bytes
        || promoted.output_bytes != plain.output_bytes
    {
        return Err(TypedCallCompileError::SizeLimit);
    }
    Ok(promoted)
}

/// Every root read in full.
fn all_outputs(
    roots: &[solve::SolvePureCallOwnerId],
) -> Vec<(solve::SolvePureCallOwnerId, Demand)> {
    roots.iter().map(|root| (*root, Demand::all())).collect()
}

/// The outputs of the owner that are built for every site: all of them without
/// demand, otherwise the assertion predicates and message values, which carry
/// the owner's failures.
fn always_built(owner: &solve::SolvePureCallOwner, by_demand: bool) -> Box<[bool]> {
    owner
        .outputs()
        .iter()
        .map(|output| !by_demand || output.kind() != solve::SolvePureCallOutputKind::Result)
        .collect()
}

/// The owners the program builds, each for the outputs its callers read.
///
/// A root is built for all of its outputs. The callee of a call a built owner
/// runs is built for the outputs of its destinations that the owner reads, or
/// for all of them when it has no output to leave unbuilt. Without demand every reachable
/// owner is built for all of its outputs.
fn admitted_units(
    table: &solve::SolvePureCallTable,
    roots: &[(u32, Demand)],
    reachable: &[bool],
    always: &[Box<[bool]>],
) -> BTreeSet<(u32, Demand)> {
    if always
        .iter()
        .all(|always| always.iter().all(|built| *built))
    {
        return (0..reachable.len() as u32)
            .filter(|owner| reachable[*owner as usize])
            .map(|owner| (owner, Demand::all()))
            .collect();
    }
    let mut units = roots.iter().cloned().collect::<BTreeSet<_>>();
    let mut pending = units.iter().cloned().collect::<Vec<_>>();
    while let Some((owner, demand)) = pending.pop() {
        let body = table.owners()[owner as usize].body();
        let liveness = Liveness::of(body, &demand);
        visit_calls(body, &liveness, &mut |callee, wanted| {
            let wanted = Callees::built_for(always, callee, wanted);
            if units.insert((callee.index(), wanted.clone())) {
                pending.push((callee.index(), wanted));
            }
        });
    }
    units
}

fn check_outputs(owner: &solve::SolvePureCallOwner) -> Result<(), TypedCallCompileError> {
    for (output, value) in owner.outputs().iter().enumerate() {
        if value.kind() == solve::SolvePureCallOutputKind::AssertionPredicate
            && !owner
                .assertion_flow()
                .is_some_and(|flow| flow.assigned_read_predicates().contains(&output))
        {
            return Err(TypedCallCompileError::UnsupportedOutputKind {
                owner: owner.id(),
                output,
                kind: value.kind(),
                provenance: owner.provenance(),
            });
        }
    }
    Ok(())
}

fn collect_calls(program: &solve::TypedProgram, pending: &mut Vec<solve::SolvePureCallOwnerId>) {
    for operation in program.operations() {
        match operation.operation() {
            solve::SolveOperation::CheckAssertion {
                message: solve::SolveAssertionMessage::Captures { program },
                ..
            } => collect_calls(program.body(), pending),
            solve::SolveOperation::Call { owner, .. } => pending.push(*owner),
            solve::SolveOperation::AssignedConditional { program } => {
                collect_calls(program.if_true().body(), pending);
                collect_calls(program.if_false().body(), pending);
            }
            solve::SolveOperation::Conditional {
                if_true, if_false, ..
            } => {
                collect_calls(if_true.body(), pending);
                collect_calls(if_false.body(), pending);
            }
            solve::SolveOperation::Map { body, .. } => collect_calls(body.body(), pending),
            solve::SolveOperation::Fold {
                transition,
                continuation,
                ..
            } => {
                collect_calls(transition.body(), pending);
                if let Some(predicate) = continuation {
                    collect_calls(predicate.body(), pending);
                }
            }
            _ => {}
        }
    }
}
