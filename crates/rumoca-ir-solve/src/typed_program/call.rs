pub(super) mod dependency;
mod view;

use rumoca_core::Span;
use serde::{Deserialize, Deserializer, Serialize};
use std::num::NonZeroU64;
use std::sync::Arc;

use dependency::SolveCallDependency;
use dependency::affinity::{self, Affinity};
use dependency::value_projection::{self, ValueProjections};
pub(super) use view::SolvePureCallTableView;

/// Complete runtime input coordinate of its issuing pure-call owner.
///
/// Pure-call construction admits only input/output/method-local slots and the
/// closed typed-operation vocabulary. Region construction enforces the same
/// slot restriction; calls name earlier checked owners. Thus every result,
/// assertion predicate, and numerical failure is determined by these input
/// cells at the owner's fixed arithmetic profile. No ambient runtime state is
/// readable. Directional owners include all tangent cells in their coordinate.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolvePureCallInputCoordinate {
    input_cells: u64,
    output_cells: u64,
}

impl SolvePureCallInputCoordinate {
    fn construct(
        inputs: &[SolveValueType],
        outputs: &[SolvePureCallOutput],
        provenance: Span,
    ) -> Result<Self, SolveProgramConstructionError> {
        let input_cells = inputs
            .iter()
            .try_fold(0u64, |count, value| {
                count.checked_add(u64::from(value.scalar_count()))
            })
            .ok_or(SolveProgramConstructionError::IdentityOverflow { provenance })?;
        let output_cells = outputs
            .iter()
            .try_fold(0u64, |count, output| {
                count.checked_add(u64::from(output.value_type().scalar_count()))
            })
            .ok_or(SolveProgramConstructionError::IdentityOverflow { provenance })?;
        Ok(Self {
            input_cells,
            output_cells,
        })
    }

    #[must_use]
    pub const fn input_cells(self) -> u64 {
        self.input_cells
    }

    #[must_use]
    pub const fn output_cells(self) -> u64 {
        self.output_cells
    }
}

use super::program::wire::{TypedProgramWire, replay_program};
use super::program::{
    ProgramSlot, SolveOperation, SolveProgramConstructionError, SolveSlotAccess, SolveStorageClass,
    TypedProgram, TypedProgramBuilder,
};
use super::types::{SolveArithmeticProfile, SolveScalarType, SolveValueType};

fn shared_slice_eq<T: Eq>(left: &Arc<[T]>, right: &Arc<[T]>) -> bool {
    // Eq is reflexive; PartialEq alone would not justify shared-storage equality.
    Arc::ptr_eq(left, right) || left == right
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SolvePureCallOwnerId(u32);

impl SolvePureCallOwnerId {
    pub(super) const fn from_index(index: u32) -> Self {
        Self(index)
    }

    #[must_use]
    pub const fn index(self) -> u32 {
        self.0
    }
}

/// Compiler-issued identity of one exact DAE call occurrence and semantic
/// context. It is opaque to Solve consumers: equality is meaningful, its
/// numeric representation is not.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SolvePureCallIdentity(NonZeroU64);

impl SolvePureCallIdentity {
    #[must_use]
    pub const fn issued(value: NonZeroU64) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SolvePureCallOutputKind {
    Result,
    AssertionPredicate,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SolvePureCallOutput {
    value_type: SolveValueType,
    kind: SolvePureCallOutputKind,
}

impl SolvePureCallOutput {
    #[must_use]
    pub fn result(value_type: SolveValueType) -> Self {
        Self {
            value_type,
            kind: SolvePureCallOutputKind::Result,
        }
    }

    #[must_use]
    pub fn assertion_predicate() -> Self {
        Self {
            value_type: SolveValueType::scalar(SolveScalarType::Boolean),
            kind: SolvePureCallOutputKind::AssertionPredicate,
        }
    }

    #[must_use]
    pub const fn value_type(&self) -> &SolveValueType {
        &self.value_type
    }

    #[must_use]
    pub const fn kind(&self) -> SolvePureCallOutputKind {
        self.kind
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct SolvePureCallInterface<'owner> {
    pub(super) id: SolvePureCallOwnerId,
    pub(super) inputs: &'owner [SolveValueType],
    pub(super) outputs: &'owner [SolvePureCallOutput],
    pub(super) dependencies: &'owner [Box<[SolveCallDependency]>],
    pub(super) projections: Option<&'owner ValueProjections>,
    pub(super) affinity: Option<&'owner Affinity>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SolvePureCallDirectionalOwner {
    input_coordinate: SolvePureCallInputCoordinate,
    dependencies: Arc<[Box<[SolveCallDependency]>]>,
    projections: Option<Arc<ValueProjections>>,
    affinity: Option<Arc<Affinity>>,
    inputs: Arc<[SolveValueType]>,
    outputs: Arc<[SolvePureCallOutput]>,
    body: TypedProgram,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolvePureCallOwner {
    #[serde(skip)]
    input_coordinate: SolvePureCallInputCoordinate,
    #[serde(skip)]
    dependencies: Arc<[Box<[SolveCallDependency]>]>,
    #[serde(skip)]
    projections: Option<Arc<ValueProjections>>,
    #[serde(skip)]
    affinity: Option<Arc<Affinity>>,
    id: SolvePureCallOwnerId,
    identity: SolvePureCallIdentity,
    inputs: Arc<[SolveValueType]>,
    outputs: Arc<[SolvePureCallOutput]>,
    body: TypedProgram,
    #[serde(skip)]
    directional: Option<SolvePureCallDirectionalOwner>,
    provenance: Span,
}

/// Checked compact interface for directional evaluation of one issued owner.
///
/// Real aggregate inputs/results are represented by adjacent primal and
/// tangent typed values. Integer and Boolean values remain primal-only.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SolvePureCallDirectionalSite {
    dependencies: Arc<[Box<[SolveCallDependency]>]>,
    projections: Option<Arc<ValueProjections>>,
    affinity: Option<Arc<Affinity>>,
    owner: SolvePureCallOwnerId,
    inputs: Arc<[SolveValueType]>,
    outputs: Arc<[SolvePureCallOutput]>,
}

/// Checked compact interface carried by one scalar-program invocation of an
/// issued model-level pure-call owner.
///
/// The interface contains no body and no scalar-coordinate catalog. One input
/// range is supplied per typed input leaf; each range width is derived from
/// its value type. Wire replay of the enclosing model additionally proves that
/// this interface exactly matches `owner` in its sole pure-call table.
/// Issued sites share immutable interface storage with that owner. Equality
/// remains value-based for independently reconstructed sites.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SolvePureCallSite {
    dependencies: Arc<[Box<[SolveCallDependency]>]>,
    projections: Option<Arc<ValueProjections>>,
    affinity: Option<Arc<Affinity>>,
    owner: SolvePureCallOwnerId,
    inputs: Arc<[SolveValueType]>,
    outputs: Arc<[SolvePureCallOutput]>,
    directional: Option<Box<SolvePureCallDirectionalSite>>,
}

impl SolvePureCallSite {
    pub(crate) fn output_degrees(
        &self,
        inputs: &[crate::affinity::Degree],
    ) -> Option<Vec<crate::affinity::Degree>> {
        if inputs.len() != self.inputs.len() {
            return None;
        }
        self.affinity.as_ref()?.output_degrees(inputs)
    }

    pub(crate) fn projected_input_coordinate(&self, output: usize) -> Option<(usize, usize)> {
        self.projections
            .as_ref()?
            .input_coordinate(output, &self.inputs, &self.outputs)
    }

    /// Compact input-coordinate dependencies for each ordered output leaf.
    /// The enclosing owner table checks this summary against its issued body.
    #[must_use]
    pub fn output_dependencies(&self) -> &[Box<[SolveCallDependency]>] {
        &self.dependencies
    }

    #[must_use]
    pub const fn owner(&self) -> SolvePureCallOwnerId {
        self.owner
    }

    #[must_use]
    pub fn inputs(&self) -> &[SolveValueType] {
        &self.inputs
    }

    #[must_use]
    pub fn outputs(&self) -> &[SolvePureCallOutput] {
        &self.outputs
    }

    #[must_use]
    pub fn output_scalar_count(&self) -> Option<usize> {
        output_scalar_count(&self.outputs)
    }

    #[must_use]
    pub fn directional(&self) -> Option<&SolvePureCallDirectionalSite> {
        self.directional.as_deref()
    }
}

impl SolvePureCallDirectionalSite {
    pub(crate) fn output_degrees(
        &self,
        inputs: &[crate::affinity::Degree],
    ) -> Option<Vec<crate::affinity::Degree>> {
        if inputs.len() != self.inputs.len() {
            return None;
        }
        self.affinity.as_ref()?.output_degrees(inputs)
    }

    pub(crate) fn projected_input_coordinate(&self, output: usize) -> Option<(usize, usize)> {
        self.projections
            .as_ref()?
            .input_coordinate(output, &self.inputs, &self.outputs)
    }

    /// Compact input-coordinate dependencies for each ordered output leaf.
    /// The enclosing owner table checks this summary against its issued body.
    #[must_use]
    pub fn output_dependencies(&self) -> &[Box<[SolveCallDependency]>] {
        &self.dependencies
    }

    #[must_use]
    pub const fn owner(&self) -> SolvePureCallOwnerId {
        self.owner
    }

    #[must_use]
    pub fn inputs(&self) -> &[SolveValueType] {
        &self.inputs
    }

    #[must_use]
    pub fn outputs(&self) -> &[SolvePureCallOutput] {
        &self.outputs
    }

    #[must_use]
    pub fn output_scalar_count(&self) -> Option<usize> {
        output_scalar_count(&self.outputs)
    }
}

impl SolvePureCallDirectionalOwner {
    #[must_use]
    pub const fn input_coordinate(&self) -> SolvePureCallInputCoordinate {
        self.input_coordinate
    }

    pub(super) fn new(
        inputs: Vec<SolveValueType>,
        outputs: Vec<SolvePureCallOutput>,
        body: TypedProgram,
        dependencies: Box<[Box<[SolveCallDependency]>]>,
        projections: Option<ValueProjections>,
        affinity: Option<Affinity>,
        provenance: Span,
    ) -> Result<Self, SolveProgramConstructionError> {
        let input_coordinate = validate_owner_body(&body, &inputs, &outputs, provenance)?;
        Ok(Self {
            input_coordinate,
            dependencies: dependencies.into(),
            projections: projections.map(Arc::new),
            affinity: affinity.map(Arc::new),
            inputs: inputs.into(),
            outputs: outputs.into(),
            body,
        })
    }

    #[must_use]
    pub fn inputs(&self) -> &[SolveValueType] {
        &self.inputs
    }

    #[must_use]
    pub fn outputs(&self) -> &[SolvePureCallOutput] {
        &self.outputs
    }

    #[must_use]
    pub const fn body(&self) -> &TypedProgram {
        &self.body
    }

    fn interface(&self, id: SolvePureCallOwnerId) -> SolvePureCallInterface<'_> {
        SolvePureCallInterface {
            dependencies: &self.dependencies,
            projections: self.projections.as_deref(),
            affinity: self.affinity.as_deref(),
            id,
            inputs: &self.inputs,
            outputs: &self.outputs,
        }
    }

    fn call_site(&self, owner: SolvePureCallOwnerId) -> SolvePureCallDirectionalSite {
        SolvePureCallDirectionalSite {
            dependencies: self.dependencies.clone(),
            projections: self.projections.clone(),
            affinity: self.affinity.clone(),
            owner,
            inputs: self.inputs.clone(),
            outputs: self.outputs.clone(),
        }
    }

    fn matches_site(
        &self,
        owner: SolvePureCallOwnerId,
        site: &SolvePureCallDirectionalSite,
    ) -> bool {
        owner == site.owner
            && shared_slice_eq(&self.dependencies, &site.dependencies)
            && self.projections == site.projections
            && self.affinity == site.affinity
            && shared_slice_eq(&self.inputs, &site.inputs)
            && shared_slice_eq(&self.outputs, &site.outputs)
    }
}

impl SolvePureCallOwner {
    #[must_use]
    pub const fn input_coordinate(&self) -> SolvePureCallInputCoordinate {
        self.input_coordinate
    }
    #[must_use]
    pub const fn id(&self) -> SolvePureCallOwnerId {
        self.id
    }

    #[must_use]
    pub const fn identity(&self) -> SolvePureCallIdentity {
        self.identity
    }

    #[must_use]
    pub fn inputs(&self) -> &[SolveValueType] {
        &self.inputs
    }

    #[must_use]
    pub fn outputs(&self) -> &[SolvePureCallOutput] {
        &self.outputs
    }

    #[must_use]
    pub const fn body(&self) -> &TypedProgram {
        &self.body
    }

    #[must_use]
    pub const fn directional(&self) -> Option<&SolvePureCallDirectionalOwner> {
        self.directional.as_ref()
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.provenance
    }

    pub(super) fn interface(&self) -> SolvePureCallInterface<'_> {
        SolvePureCallInterface {
            id: self.id,
            inputs: &self.inputs,
            outputs: &self.outputs,
            dependencies: &self.dependencies,
            projections: self.projections.as_deref(),
            affinity: self.affinity.as_deref(),
        }
    }

    #[must_use]
    pub fn call_site(&self) -> SolvePureCallSite {
        SolvePureCallSite {
            dependencies: self.dependencies.clone(),
            projections: self.projections.clone(),
            affinity: self.affinity.clone(),
            owner: self.id,
            inputs: self.inputs.clone(),
            outputs: self.outputs.clone(),
            directional: self
                .directional
                .as_ref()
                .map(|directional| Box::new(directional.call_site(self.id))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolvePureCallTable {
    arithmetic: SolveArithmeticProfile,
    owners: Box<[SolvePureCallOwner]>,
}

impl Default for SolvePureCallTable {
    fn default() -> Self {
        Self {
            arithmetic: SolveArithmeticProfile::construct(
                super::types::SolveRealFormat::Binary64,
                super::types::SolveIntegerDomain::FULL,
            ),
            owners: Box::new([]),
        }
    }
}

impl SolvePureCallTable {
    pub fn construct(
        arithmetic: SolveArithmeticProfile,
        build: impl FnOnce(&mut SolvePureCallTableBuilder) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        let mut builder = Self::builder(arithmetic);
        build(&mut builder)?;
        Ok(builder.finish())
    }

    #[must_use]
    pub fn builder(arithmetic: SolveArithmeticProfile) -> SolvePureCallTableBuilder {
        SolvePureCallTableBuilder {
            arithmetic,
            owners: Vec::new(),
        }
    }

    #[must_use]
    pub const fn owners(&self) -> &[SolvePureCallOwner] {
        &self.owners
    }

    #[must_use]
    pub fn owner(&self, id: SolvePureCallOwnerId) -> Option<&SolvePureCallOwner> {
        self.owners
            .get(id.index() as usize)
            .filter(|owner| owner.id == id)
    }

    #[must_use]
    pub const fn arithmetic(&self) -> SolveArithmeticProfile {
        self.arithmetic
    }

    #[must_use]
    pub fn matches_site(&self, site: &SolvePureCallSite) -> bool {
        self.owner(site.owner).is_some_and(|owner| {
            shared_slice_eq(&owner.dependencies, &site.dependencies)
                && owner.projections == site.projections
                && owner.affinity == site.affinity
                && shared_slice_eq(&owner.inputs, &site.inputs)
                && shared_slice_eq(&owner.outputs, &site.outputs)
                && match (owner.directional.as_ref(), site.directional.as_deref()) {
                    (None, None) => true,
                    (Some(directional), Some(site)) => directional.matches_site(owner.id, site),
                    _ => false,
                }
        })
    }

    #[must_use]
    pub fn matches_directional_site(&self, site: &SolvePureCallDirectionalSite) -> bool {
        self.owner(site.owner).is_some_and(|owner| {
            owner
                .directional
                .as_ref()
                .is_some_and(|directional| directional.matches_site(owner.id, site))
        })
    }
}

pub struct SolvePureCallTableBuilder {
    arithmetic: SolveArithmeticProfile,
    owners: Vec<SolvePureCallOwner>,
}

impl SolvePureCallTableBuilder {
    #[must_use]
    pub fn finish(self) -> SolvePureCallTable {
        SolvePureCallTable {
            arithmetic: self.arithmetic,
            owners: self.owners.into_boxed_slice(),
        }
    }

    pub fn add_owner(
        &mut self,
        identity: SolvePureCallIdentity,
        inputs: Vec<SolveValueType>,
        outputs: Vec<SolvePureCallOutput>,
        provenance: Span,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
            &[ProgramSlot<'program>],
            &[ProgramSlot<'program>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<SolvePureCallOwnerId, SolveProgramConstructionError> {
        require_owner_interface(self.arithmetic, &inputs, &outputs, provenance)?;
        if self.owners.iter().any(|owner| owner.identity == identity) {
            return Err(SolveProgramConstructionError::DuplicateCallIdentity { provenance });
        }
        let id = next_owner_id(self.owners.len(), provenance)?;
        let interfaces = SolvePureCallTableView::primal(&self.owners);
        let body = TypedProgram::construct_with_calls(self.arithmetic, interfaces, |builder| {
            let input_slots = inputs
                .iter()
                .cloned()
                .map(|value_type| {
                    builder.declare_slot(
                        value_type,
                        SolveStorageClass::Input,
                        SolveSlotAccess::ReadOnly,
                        provenance,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            let output_slots = outputs
                .iter()
                .map(|output| {
                    builder.declare_slot(
                        output.value_type.clone(),
                        SolveStorageClass::Output,
                        SolveSlotAccess::ReadWrite,
                        provenance,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            build(builder, &input_slots, &output_slots)
        })?;
        let input_coordinate = validate_owner_body(&body, &inputs, &outputs, provenance)?;
        let directional_interfaces = SolvePureCallTableView::directional(&self.owners);
        let directional =
            body.derive_directional_owner(&inputs, &outputs, directional_interfaces, provenance)?;
        let dependencies = dependency::derive(&body, inputs.len(), outputs.len(), interfaces)?;
        let projections = derive_value_projections(&body, inputs.len(), &outputs, &self.owners);
        let affinity = derive_affinity(&body, inputs.len(), outputs.len(), &self.owners);
        self.owners.push(SolvePureCallOwner {
            input_coordinate,
            dependencies: dependencies.into(),
            projections: projections.map(Arc::new),
            affinity: affinity.map(Arc::new),
            id,
            identity,
            inputs: inputs.into(),
            outputs: outputs.into(),
            body,
            directional,
            provenance,
        });
        Ok(id)
    }

    #[must_use]
    pub fn call_site(&self, id: SolvePureCallOwnerId) -> Option<SolvePureCallSite> {
        self.owners
            .get(id.index() as usize)
            .filter(|owner| owner.id == id)
            .map(SolvePureCallOwner::call_site)
    }
}

/// The identity the next owner receives: the table's current length, which
/// must fit the owner index width. Kept out of the generic construction path
/// so the overflow branch exists once rather than per instantiation.
fn next_owner_id(
    owner_count: usize,
    provenance: Span,
) -> Result<SolvePureCallOwnerId, SolveProgramConstructionError> {
    let owner_index = u32::try_from(owner_count)
        .map_err(|_| SolveProgramConstructionError::IdentityOverflow { provenance })?;
    Ok(SolvePureCallOwnerId::from_index(owner_index))
}

fn derive_affinity(
    body: &TypedProgram,
    inputs: usize,
    outputs: usize,
    owners: &[SolvePureCallOwner],
) -> Option<Affinity> {
    affinity::derive(
        body,
        inputs,
        outputs,
        SolvePureCallTableView::primal(owners),
    )
}

fn derive_value_projections(
    body: &TypedProgram,
    input_count: usize,
    outputs: &[SolvePureCallOutput],
    owners: &[SolvePureCallOwner],
) -> Option<ValueProjections> {
    value_projection::derive(
        body,
        input_count,
        outputs,
        SolvePureCallTableView::primal(owners),
    )
}

fn output_scalar_count(outputs: &[SolvePureCallOutput]) -> Option<usize> {
    outputs.iter().try_fold(0usize, |count, output| {
        count.checked_add(output.value_type.scalar_count() as usize)
    })
}

fn require_owner_interface(
    arithmetic: SolveArithmeticProfile,
    inputs: &[SolveValueType],
    outputs: &[SolvePureCallOutput],
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    if provenance.is_dummy() {
        return Err(SolveProgramConstructionError::MissingProvenance);
    }
    if outputs.is_empty() {
        return Err(SolveProgramConstructionError::EmptyCallOutput { provenance });
    }
    if inputs
        .iter()
        .chain(outputs.iter().map(SolvePureCallOutput::value_type))
        .any(|value_type| !value_type.belongs_to(arithmetic))
    {
        return Err(SolveProgramConstructionError::ProfileMismatch { provenance });
    }
    if outputs.iter().any(|output| {
        output.kind == SolvePureCallOutputKind::AssertionPredicate
            && (output.value_type.element_type() != SolveScalarType::Boolean
                || !output.value_type.dimensions().is_empty())
    }) {
        return Err(SolveProgramConstructionError::InvalidCallOutput { provenance });
    }
    Ok(())
}

fn validate_owner_body(
    body: &TypedProgram,
    inputs: &[SolveValueType],
    outputs: &[SolvePureCallOutput],
    provenance: Span,
) -> Result<SolvePureCallInputCoordinate, SolveProgramConstructionError> {
    let input_count = inputs.len();
    let output_count = outputs.len();
    let interface_count = input_count
        .checked_add(output_count)
        .ok_or(SolveProgramConstructionError::IdentityOverflow { provenance })?;
    if body.slots().len() < interface_count
        || body.slots()[..input_count]
            .iter()
            .zip(inputs)
            .any(|(slot, expected)| {
                slot.storage() != SolveStorageClass::Input || slot.value_type() != expected
            })
        || body.slots()[input_count..interface_count]
            .iter()
            .zip(outputs)
            .any(|(slot, expected)| {
                slot.storage() != SolveStorageClass::Output
                    || slot.value_type() != expected.value_type()
            })
        || body.slots()[interface_count..]
            .iter()
            .any(|slot| slot.storage() != SolveStorageClass::MethodLocal)
    {
        return Err(SolveProgramConstructionError::InvalidCallInterface { provenance });
    }
    let mut stores = vec![0usize; output_count];
    for operation in body.operations() {
        match operation.operation() {
            SolveOperation::Load { slot, .. }
                if (input_count..interface_count).contains(&slot.index()) =>
            {
                return Err(SolveProgramConstructionError::IncompleteCallOutput { provenance });
            }
            SolveOperation::Store { slot, .. } => {
                if let Some(output) = slot
                    .index()
                    .checked_sub(input_count)
                    .filter(|output| *output < output_count)
                {
                    stores[output] += 1;
                }
            }
            _ => {}
        }
    }
    if stores.iter().any(|count| *count != 1) {
        return Err(SolveProgramConstructionError::IncompleteCallOutput { provenance });
    }
    SolvePureCallInputCoordinate::construct(inputs, outputs, provenance)
}

impl<'de> Deserialize<'de> for SolvePureCallTable {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Wire {
            arithmetic: SolveArithmeticProfile,
            owners: Vec<OwnerWire>,
        }

        #[derive(Deserialize)]
        struct OwnerWire {
            id: SolvePureCallOwnerId,
            identity: SolvePureCallIdentity,
            inputs: Box<[SolveValueType]>,
            outputs: Box<[SolvePureCallOutput]>,
            body: TypedProgramWire,
            provenance: Span,
        }

        let wire = Wire::deserialize(deserializer)?;
        let mut owners: Vec<SolvePureCallOwner> = Vec::with_capacity(wire.owners.len());
        for (index, owner) in wire.owners.into_iter().enumerate() {
            if owner.id.index() as usize != index {
                return Err(serde::de::Error::custom(
                    SolveProgramConstructionError::WireMismatch,
                ));
            }
            if owners.iter().any(|prior| prior.identity == owner.identity) {
                return Err(serde::de::Error::custom(
                    SolveProgramConstructionError::WireMismatch,
                ));
            }
            require_owner_interface(
                wire.arithmetic,
                &owner.inputs,
                &owner.outputs,
                owner.provenance,
            )
            .map_err(serde::de::Error::custom)?;
            let interfaces = SolvePureCallTableView::primal(&owners);
            let body = replay_program(&owner.body, interfaces).map_err(serde::de::Error::custom)?;
            let input_coordinate =
                validate_owner_body(&body, &owner.inputs, &owner.outputs, owner.provenance)
                    .map_err(serde::de::Error::custom)?;
            let directional_interfaces = SolvePureCallTableView::directional(&owners);
            let directional = body
                .derive_directional_owner(
                    &owner.inputs,
                    &owner.outputs,
                    directional_interfaces,
                    owner.provenance,
                )
                .map_err(serde::de::Error::custom)?;
            let dependencies =
                dependency::derive(&body, owner.inputs.len(), owner.outputs.len(), interfaces)
                    .map_err(serde::de::Error::custom)?;
            let projections =
                derive_value_projections(&body, owner.inputs.len(), &owner.outputs, &owners);
            let affinity = derive_affinity(&body, owner.inputs.len(), owner.outputs.len(), &owners);
            owners.push(SolvePureCallOwner {
                input_coordinate,
                dependencies: dependencies.into(),
                projections: projections.map(Arc::new),
                affinity: affinity.map(Arc::new),
                id: owner.id,
                identity: owner.identity,
                inputs: owner.inputs.into(),
                outputs: owner.outputs.into(),
                body,
                directional,
                provenance: owner.provenance,
            });
        }
        Ok(Self {
            arithmetic: wire.arithmetic,
            owners: owners.into_boxed_slice(),
        })
    }
}

#[cfg(test)]
mod tests {
    mod affinity;
    mod dependencies;
    mod value_projections;
    mod views;

    use super::*;
    use crate::{SolveIntegerDomain, SolveRealFormat, SolveValue};
    use rumoca_core::SourceId;

    fn span(start: usize) -> Span {
        Span::from_offsets(
            SourceId::from_source_name("pure_call_owner.mo"),
            start,
            start + 1,
        )
    }

    fn profile() -> SolveArithmeticProfile {
        SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
        )
    }

    fn vector_type() -> SolveValueType {
        SolveValueType::tensor(SolveScalarType::real(profile()), vec![3]).unwrap()
    }

    fn identity(value: u64) -> SolvePureCallIdentity {
        SolvePureCallIdentity::issued(NonZeroU64::new(value).unwrap())
    }

    fn add_passthrough_owner(
        table: &mut SolvePureCallTableBuilder,
        call_identity: SolvePureCallIdentity,
        vector: &SolveValueType,
        provenance: Span,
    ) -> Result<SolvePureCallOwnerId, SolveProgramConstructionError> {
        table.add_owner(
            call_identity,
            vec![vector.clone()],
            vec![SolvePureCallOutput::result(vector.clone())],
            provenance,
            |builder, inputs, outputs| {
                let value = builder.load(inputs[0], provenance)?;
                builder.store(outputs[0], value, provenance)
            },
        )
    }

    #[test]
    fn aggregate_result_and_assertion_share_one_pure_call_owner() {
        let vector = vector_type();
        let table = SolvePureCallTable::construct(profile(), |table| {
            table.add_owner(
                identity(1),
                vec![vector.clone()],
                vec![
                    SolvePureCallOutput::result(vector.clone()),
                    SolvePureCallOutput::assertion_predicate(),
                ],
                span(0),
                |builder, inputs, outputs| {
                    let value = builder.load(inputs[0], span(1))?;
                    let safe = builder.constant(SolveValue::boolean(true), span(2))?;
                    builder.store(outputs[0], value, span(3))?;
                    builder.store(outputs[1], safe, span(4))
                },
            )?;
            Ok(())
        })
        .unwrap();

        let [owner] = table.owners() else {
            panic!("one owner must be issued");
        };
        assert_eq!(owner.inputs(), std::slice::from_ref(&vector));
        assert_eq!(owner.outputs().len(), 2);
        assert_eq!(
            owner.outputs()[1].kind(),
            SolvePureCallOutputKind::AssertionPredicate
        );
        assert_eq!(owner.body().operations().len(), 4);
        assert!(matches!(
            owner.body().operations()[0].operation(),
            SolveOperation::Load { .. }
        ));
    }

    #[test]
    fn nested_owner_is_one_atomic_multi_result_call() {
        let vector = vector_type();
        let table = SolvePureCallTable::construct(profile(), |table| {
            let inner = table.add_owner(
                identity(1),
                vec![vector.clone()],
                vec![
                    SolvePureCallOutput::result(vector.clone()),
                    SolvePureCallOutput::assertion_predicate(),
                ],
                span(0),
                |builder, inputs, outputs| {
                    let value = builder.load(inputs[0], span(1))?;
                    let safe = builder.constant(SolveValue::boolean(true), span(2))?;
                    builder.store(outputs[0], value, span(3))?;
                    builder.store(outputs[1], safe, span(4))
                },
            )?;
            table.add_owner(
                identity(2),
                vec![vector.clone()],
                vec![
                    SolvePureCallOutput::result(vector.clone()),
                    SolvePureCallOutput::assertion_predicate(),
                ],
                span(5),
                |builder, inputs, outputs| {
                    let argument = builder.load(inputs[0], span(6))?;
                    let results = builder.call(inner, &[argument], span(7))?;
                    builder.store(outputs[0], results[0], span(8))?;
                    builder.store(outputs[1], results[1], span(9))
                },
            )?;
            Ok(())
        })
        .unwrap();

        let outer = &table.owners()[1];
        let calls = outer
            .body()
            .operations()
            .iter()
            .filter(|operation| matches!(operation.operation(), SolveOperation::Call { .. }))
            .count();
        assert_eq!(calls, 1);
        let SolveOperation::Call {
            owner,
            arguments,
            destinations,
        } = outer.body().operations()[1].operation()
        else {
            panic!("outer owner must call its issued inner owner");
        };
        assert_eq!(*owner, table.owners()[0].id());
        assert_eq!(arguments.len(), 1);
        assert_eq!(destinations.len(), 2);

        let json = serde_json::to_string(&table).expect("call table serializes");
        let replayed: SolvePureCallTable =
            serde_json::from_str(&json).expect("call table wire replays with issued interfaces");
        assert_eq!(replayed, table);
        for owner in replayed.owners() {
            let primal = owner.input_coordinate();
            assert_eq!(primal.input_cells(), u64::from(vector.scalar_count()));
            assert_eq!(primal.output_cells(), u64::from(vector.scalar_count()) + 1);
            let directional = owner.directional().unwrap().input_coordinate();
            assert_eq!(
                directional.input_cells(),
                2 * u64::from(vector.scalar_count())
            );
            assert_eq!(
                directional.output_cells(),
                2 * u64::from(vector.scalar_count()) + 1
            );
        }
    }

    #[test]
    fn complete_input_coordinates_keep_large_tensors_compact() {
        let vector =
            SolveValueType::tensor(SolveScalarType::real(profile()), vec![u32::MAX]).unwrap();
        let table = SolvePureCallTable::construct(profile(), |table| {
            table.add_owner(
                identity(1),
                vec![vector.clone(), vector.clone()],
                vec![SolvePureCallOutput::result(vector)],
                span(0),
                |builder, inputs, outputs| {
                    let value = builder.load(inputs[0], span(1))?;
                    builder.store(outputs[0], value, span(2))
                },
            )?;
            Ok(())
        })
        .unwrap();
        let owner = &table.owners()[0];
        assert_eq!(owner.body().operations().len(), 2);
        assert_eq!(
            owner.input_coordinate().input_cells(),
            2 * u64::from(u32::MAX)
        );
        assert_eq!(
            owner
                .directional()
                .unwrap()
                .input_coordinate()
                .input_cells(),
            4 * u64::from(u32::MAX)
        );
    }

    #[test]
    fn missing_assertion_output_fails_owner_construction() {
        let vector = vector_type();
        let error = SolvePureCallTable::construct(profile(), |table| {
            table.add_owner(
                identity(1),
                vec![vector.clone()],
                vec![
                    SolvePureCallOutput::result(vector.clone()),
                    SolvePureCallOutput::assertion_predicate(),
                ],
                span(0),
                |builder, inputs, outputs| {
                    let value = builder.load(inputs[0], span(1))?;
                    builder.store(outputs[0], value, span(2))
                },
            )?;
            Ok(())
        })
        .unwrap_err();
        assert_eq!(
            error,
            SolveProgramConstructionError::IncompleteCallOutput {
                provenance: span(0)
            }
        );
    }

    #[test]
    fn duplicate_semantic_identity_is_rejected_before_body_construction() {
        let vector = vector_type();
        let error = SolvePureCallTable::construct(profile(), |table| {
            add_passthrough_owner(table, identity(1), &vector, span(0))?;
            add_passthrough_owner(table, identity(1), &vector, span(10))?;
            Ok(())
        })
        .unwrap_err();
        assert_eq!(
            error,
            SolveProgramConstructionError::DuplicateCallIdentity {
                provenance: span(10)
            }
        );
    }

    #[test]
    fn call_output_cannot_be_read_before_its_single_definition() {
        let vector = vector_type();
        let error = SolvePureCallTable::construct(profile(), |table| {
            table.add_owner(
                identity(1),
                vec![vector.clone()],
                vec![SolvePureCallOutput::result(vector.clone())],
                span(0),
                |builder, _inputs, outputs| {
                    let stale = builder.load(outputs[0], span(1))?;
                    builder.store(outputs[0], stale, span(2))
                },
            )?;
            Ok(())
        })
        .unwrap_err();
        assert_eq!(
            error,
            SolveProgramConstructionError::UninitializedSlot {
                provenance: span(1)
            }
        );
    }

    #[test]
    fn pure_call_requires_every_capture_to_be_an_explicit_input() {
        let vector = vector_type();
        let error = SolvePureCallTable::construct(profile(), |table| {
            table.add_owner(
                identity(1),
                vec![vector.clone()],
                vec![SolvePureCallOutput::result(vector.clone())],
                span(0),
                |builder, inputs, outputs| {
                    builder.declare_slot(
                        vector.clone(),
                        SolveStorageClass::PersistentState,
                        SolveSlotAccess::ReadWrite,
                        span(1),
                    )?;
                    let value = builder.load(inputs[0], span(2))?;
                    builder.store(outputs[0], value, span(3))
                },
            )?;
            Ok(())
        })
        .unwrap_err();
        assert_eq!(
            error,
            SolveProgramConstructionError::InvalidCallInterface {
                provenance: span(0)
            }
        );
    }
}
