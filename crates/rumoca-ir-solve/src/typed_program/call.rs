use rumoca_core::Span;
use serde::{Deserialize, Deserializer, Serialize};
use std::num::NonZeroU64;

use super::program::wire::{TypedProgramWire, replay_program};
use super::program::{
    ProgramSlot, SolveOperation, SolveProgramConstructionError, SolveSlotAccess, SolveStorageClass,
    TypedProgram, TypedProgramBuilder,
};
use super::types::{SolveArithmeticProfile, SolveScalarType, SolveValueType};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct SolvePureCallInterface {
    pub(super) id: SolvePureCallOwnerId,
    pub(super) inputs: Box<[SolveValueType]>,
    pub(super) outputs: Box<[SolvePureCallOutput]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct SolvePureCallDirectionalInterface {
    pub(super) id: SolvePureCallOwnerId,
    pub(super) inputs: Box<[SolveValueType]>,
    pub(super) outputs: Box<[SolvePureCallOutput]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SolvePureCallDirectionalOwner {
    inputs: Box<[SolveValueType]>,
    outputs: Box<[SolvePureCallOutput]>,
    body: TypedProgram,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolvePureCallOwner {
    id: SolvePureCallOwnerId,
    identity: SolvePureCallIdentity,
    inputs: Box<[SolveValueType]>,
    outputs: Box<[SolvePureCallOutput]>,
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
    owner: SolvePureCallOwnerId,
    inputs: Box<[SolveValueType]>,
    outputs: Box<[SolvePureCallOutput]>,
}

/// Checked compact interface carried by one scalar-program invocation of an
/// issued model-level pure-call owner.
///
/// The interface contains no body and no scalar-coordinate catalog. One input
/// range is supplied per typed input leaf; each range width is derived from
/// its value type. Wire replay of the enclosing model additionally proves that
/// this interface exactly matches `owner` in its sole pure-call table.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SolvePureCallSite {
    owner: SolvePureCallOwnerId,
    inputs: Box<[SolveValueType]>,
    outputs: Box<[SolvePureCallOutput]>,
    directional: Option<Box<SolvePureCallDirectionalSite>>,
}

impl SolvePureCallSite {
    #[must_use]
    pub const fn owner(&self) -> SolvePureCallOwnerId {
        self.owner
    }

    #[must_use]
    pub const fn inputs(&self) -> &[SolveValueType] {
        &self.inputs
    }

    #[must_use]
    pub const fn outputs(&self) -> &[SolvePureCallOutput] {
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
    #[must_use]
    pub const fn owner(&self) -> SolvePureCallOwnerId {
        self.owner
    }

    #[must_use]
    pub const fn inputs(&self) -> &[SolveValueType] {
        &self.inputs
    }

    #[must_use]
    pub const fn outputs(&self) -> &[SolvePureCallOutput] {
        &self.outputs
    }

    #[must_use]
    pub fn output_scalar_count(&self) -> Option<usize> {
        output_scalar_count(&self.outputs)
    }
}

impl SolvePureCallDirectionalOwner {
    pub(super) fn new(
        inputs: Vec<SolveValueType>,
        outputs: Vec<SolvePureCallOutput>,
        body: TypedProgram,
    ) -> Self {
        Self {
            inputs: inputs.into_boxed_slice(),
            outputs: outputs.into_boxed_slice(),
            body,
        }
    }

    #[must_use]
    pub const fn inputs(&self) -> &[SolveValueType] {
        &self.inputs
    }

    #[must_use]
    pub const fn outputs(&self) -> &[SolvePureCallOutput] {
        &self.outputs
    }

    #[must_use]
    pub const fn body(&self) -> &TypedProgram {
        &self.body
    }

    fn interface(&self, id: SolvePureCallOwnerId) -> SolvePureCallDirectionalInterface {
        SolvePureCallDirectionalInterface {
            id,
            inputs: self.inputs.clone(),
            outputs: self.outputs.clone(),
        }
    }

    fn call_site(&self, owner: SolvePureCallOwnerId) -> SolvePureCallDirectionalSite {
        SolvePureCallDirectionalSite {
            owner,
            inputs: self.inputs.clone(),
            outputs: self.outputs.clone(),
        }
    }
}

impl SolvePureCallOwner {
    #[must_use]
    pub const fn id(&self) -> SolvePureCallOwnerId {
        self.id
    }

    #[must_use]
    pub const fn identity(&self) -> SolvePureCallIdentity {
        self.identity
    }

    #[must_use]
    pub const fn inputs(&self) -> &[SolveValueType] {
        &self.inputs
    }

    #[must_use]
    pub const fn outputs(&self) -> &[SolvePureCallOutput] {
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

    pub(super) fn interface(&self) -> SolvePureCallInterface {
        SolvePureCallInterface {
            id: self.id,
            inputs: self.inputs.clone(),
            outputs: self.outputs.clone(),
        }
    }

    #[must_use]
    pub fn call_site(&self) -> SolvePureCallSite {
        SolvePureCallSite {
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
        let integer_domain = super::types::SolveIntegerDomain::construct(i64::MIN, i64::MAX)
            .expect("the full i64 domain is nonempty");
        Self {
            arithmetic: SolveArithmeticProfile::construct(
                super::types::SolveRealFormat::Binary64,
                super::types::SolveRoundingMode::NearestTiesToEven,
                integer_domain,
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
            owner.inputs == site.inputs
                && owner.outputs == site.outputs
                && owner
                    .directional
                    .as_ref()
                    .map(|directional| (directional.inputs.as_ref(), directional.outputs.as_ref()))
                    == site.directional.as_deref().map(|directional| {
                        (directional.inputs.as_ref(), directional.outputs.as_ref())
                    })
        })
    }

    #[must_use]
    pub fn matches_directional_site(&self, site: &SolvePureCallDirectionalSite) -> bool {
        self.owner(site.owner).is_some_and(|owner| {
            owner.directional.as_ref().is_some_and(|directional| {
                directional.inputs == site.inputs && directional.outputs == site.outputs
            })
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
        let owner_index = u32::try_from(self.owners.len())
            .map_err(|_| SolveProgramConstructionError::IdentityOverflow { provenance })?;
        let id = SolvePureCallOwnerId::from_index(owner_index);
        let interfaces = self
            .owners
            .iter()
            .map(SolvePureCallOwner::interface)
            .collect::<Vec<_>>();
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
        validate_owner_body(&body, &inputs, &outputs, provenance)?;
        let directional_interfaces = self
            .owners
            .iter()
            .map(|owner| {
                owner
                    .directional
                    .as_ref()
                    .map(|directional| directional.interface(owner.id))
            })
            .collect::<Vec<_>>();
        let directional =
            body.derive_directional_owner(&inputs, &outputs, directional_interfaces, provenance)?;
        self.owners.push(SolvePureCallOwner {
            id,
            identity,
            inputs: inputs.into_boxed_slice(),
            outputs: outputs.into_boxed_slice(),
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
) -> Result<(), SolveProgramConstructionError> {
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
    Ok(())
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
            let interfaces = owners
                .iter()
                .map(SolvePureCallOwner::interface)
                .collect::<Vec<_>>();
            let body =
                replay_program(&owner.body, &interfaces).map_err(serde::de::Error::custom)?;
            validate_owner_body(&body, &owner.inputs, &owner.outputs, owner.provenance)
                .map_err(serde::de::Error::custom)?;
            let directional_interfaces = owners
                .iter()
                .map(|prior| {
                    prior
                        .directional
                        .as_ref()
                        .map(|directional| directional.interface(prior.id))
                })
                .collect::<Vec<_>>();
            let directional = body
                .derive_directional_owner(
                    &owner.inputs,
                    &owner.outputs,
                    directional_interfaces,
                    owner.provenance,
                )
                .map_err(serde::de::Error::custom)?;
            owners.push(SolvePureCallOwner {
                id: owner.id,
                identity: owner.identity,
                inputs: owner.inputs,
                outputs: owner.outputs,
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
    use super::*;
    use crate::{SolveIntegerDomain, SolveRealFormat, SolveRoundingMode, SolveValue};
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
            SolveRoundingMode::NearestTiesToEven,
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
