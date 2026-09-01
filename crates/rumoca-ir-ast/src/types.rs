//! Type system for the Class Tree (MLS §4).
//!
//! This module provides type definitions and the TypeTable for tracking
//! all types in the compilation unit.

use indexmap::IndexMap;
use rumoca_core::{DefId, TypeId};
use serde::{Deserialize, Serialize};

const PREDEFINED_TYPE_NAMES: [&str; 7] = [
    "Real",
    "Integer",
    "Boolean",
    "String",
    "Clock",
    "StateSelect",
    "AssertionLevel",
];
const PREDEFINED_TYPE_COUNT: usize = PREDEFINED_TYPE_NAMES.len();

/// MLS §4: Type definitions
///
/// The TypeTable stores all types encountered during compilation.
/// Types are referenced by TypeId throughout the compiler.
///
/// ```compile_fail
/// let _ = rumoca_ir_ast::TypeTable::default();
/// ```
#[derive(Debug, Clone, Serialize)]
pub struct TypeTable {
    /// All types indexed by TypeId.
    types: Vec<Type>,
    /// Map from type names to TypeIds for quick lookup.
    by_name: IndexMap<String, TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TypeDeclarationKind {
    Enumeration(Box<[String]>),
    Class(ClassKind),
    Alias { base: Option<DefId> },
}

#[derive(Debug)]
struct TypeDeclarationClaim {
    def_id: DefId,
    name: String,
    kind: TypeDeclarationKind,
}

#[derive(Debug)]
struct PredefinedTypeDeclarationClaim {
    def_id: DefId,
    name: &'static str,
    type_id: TypeId,
}

/// Exact declaration authority minted from one resolved [`crate::ClassTree`].
///
/// The fields are private so a caller cannot restate a declaration inventory
/// independently of the tree that owns it. The authority is consumed by the
/// sole public type-table append issuer.
#[derive(Debug)]
pub struct TypeDeclarationInventory {
    declarations: Box<[TypeDeclarationClaim]>,
    predefined: [PredefinedTypeDeclarationClaim; PREDEFINED_TYPE_COUNT],
}

impl TypeDeclarationInventory {
    pub(crate) fn new(
        declarations: Vec<(DefId, String, TypeDeclarationKind)>,
        predefined: Vec<(DefId, String, TypeId)>,
    ) -> Result<Self, TypeDeclarationInventoryError> {
        let mut claimed = IndexMap::new();
        if predefined.len() != PREDEFINED_TYPE_COUNT {
            return Err(TypeDeclarationInventoryError::structural(
                "predefined type prefix".to_string(),
                None,
                format!(
                    "expected exactly {PREDEFINED_TYPE_COUNT} declaration claims, found {}",
                    predefined.len()
                ),
            ));
        }
        let mut predefined_claims = Vec::with_capacity(PREDEFINED_TYPE_COUNT);
        for (index, (def_id, name, type_id)) in predefined.into_iter().enumerate() {
            let expected_name = PREDEFINED_TYPE_NAMES[index];
            let expected_type_id = TypeId::new(
                u32::try_from(index).expect("the fixed predefined prefix fits the TypeId domain"),
            );
            if name != expected_name || type_id != expected_type_id {
                return Err(TypeDeclarationInventoryError::structural(
                    expected_name.to_string(),
                    Some(def_id),
                    format!(
                        "expected canonical claim ({expected_name}, {expected_type_id:?}), found ({name}, {type_id:?})"
                    ),
                ));
            }
            claim_declaration_identity(&mut claimed, def_id, name)?;
            predefined_claims.push(PredefinedTypeDeclarationClaim {
                def_id,
                name: expected_name,
                type_id,
            });
        }
        let mut complete = Vec::with_capacity(declarations.len());
        for (def_id, name, kind) in declarations {
            claim_declaration_identity(&mut claimed, def_id, name.clone())?;
            complete.push(TypeDeclarationClaim { def_id, name, kind });
        }
        let predefined = predefined_claims
            .try_into()
            .expect("the exact predefined claim count was checked before construction");
        Ok(Self {
            declarations: complete.into_boxed_slice(),
            predefined,
        })
    }

    pub(crate) fn def_ids(&self) -> impl ExactSizeIterator<Item = DefId> + '_ {
        self.declarations.iter().map(|claim| claim.def_id)
    }

    /// Iterate over the exact source declarations admitted by this inventory.
    pub fn declarations(&self) -> impl ExactSizeIterator<Item = (DefId, &str)> {
        self.declarations
            .iter()
            .map(|claim| (claim.def_id, claim.name.as_str()))
    }

    /// Iterate over the canonical predefined associations admitted with the
    /// source declarations.
    pub fn predefined(&self) -> impl ExactSizeIterator<Item = (DefId, TypeId)> + '_ {
        self.predefined
            .iter()
            .map(|claim| (claim.def_id, claim.type_id))
    }

    fn len(&self) -> usize {
        self.declarations.len()
    }
}

fn claim_declaration_identity(
    claimed: &mut IndexMap<DefId, String>,
    def_id: DefId,
    name: String,
) -> Result<(), TypeDeclarationInventoryError> {
    if let Some(previous_name) = claimed.insert(def_id, name.clone()) {
        return Err(TypeDeclarationInventoryError::duplicate(
            def_id,
            previous_name,
            name,
        ));
    }
    Ok(())
}

/// A resolved declaration inventory could not prove one exact association.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDeclarationInventoryError {
    def_id: Option<DefId>,
    first_name: String,
    repeated_name: String,
    kind: TypeDeclarationInventoryErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeDeclarationInventoryErrorKind {
    Duplicate,
    Structural { reason: String },
}

impl TypeDeclarationInventoryError {
    fn duplicate(def_id: DefId, first_name: String, repeated_name: String) -> Self {
        Self {
            def_id: Some(def_id),
            first_name,
            repeated_name,
            kind: TypeDeclarationInventoryErrorKind::Duplicate,
        }
    }

    pub(crate) fn structural(name: String, def_id: Option<DefId>, reason: String) -> Self {
        Self {
            def_id,
            first_name: name.clone(),
            repeated_name: name,
            kind: TypeDeclarationInventoryErrorKind::Structural { reason },
        }
    }

    pub fn def_id(&self) -> Option<DefId> {
        self.def_id
    }

    pub fn first_name(&self) -> &str {
        &self.first_name
    }

    pub fn repeated_name(&self) -> &str {
        &self.repeated_name
    }
}

impl std::fmt::Display for TypeDeclarationInventoryError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeDeclarationInventoryErrorKind::Duplicate => write!(
                formatter,
                "declarations `{}` and `{}` both claim {:?}",
                self.first_name, self.repeated_name, self.def_id,
            ),
            TypeDeclarationInventoryErrorKind::Structural { reason } => write!(
                formatter,
                "declaration `{}` is not exactly represented by the resolved indexes: {reason}",
                self.repeated_name,
            ),
        }
    }
}

impl std::error::Error for TypeDeclarationInventoryError {}

/// Capacity failure while planning an atomic append to a [`TypeTable`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeTableCapacityError {
    current: usize,
    additional: usize,
}

impl std::fmt::Display for TypeTableCapacityError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "type identity space cannot append {} entries after {} existing entries",
            self.additional, self.current,
        )
    }
}

impl std::error::Error for TypeTableCapacityError {}

fn checked_type_append_end(
    current: usize,
    additional: usize,
) -> Result<usize, TypeTableCapacityError> {
    current
        .checked_add(additional)
        .filter(|end| *end <= TypeId::UNKNOWN.index() as usize)
        .ok_or(TypeTableCapacityError {
            current,
            additional,
        })
}

/// Opaque, table-borrowing plan for one exact-size atomic append.
///
/// Every planned identity is available before values are built, so recursive
/// or forward references can use exact IDs. `commit` constructs one value per
/// identity into detached storage and mutates the table only after every value
/// succeeds.
pub struct TypeTableAppendPlan<'table, Payload> {
    table: &'table mut TypeTable,
    entries: Box<[(TypeId, Payload)]>,
    declaration_inventory: Option<TypeDeclarationInventory>,
}

impl<Payload> TypeTableAppendPlan<'_, Payload> {
    pub fn entries(&self) -> impl ExactSizeIterator<Item = (TypeId, &Payload)> {
        self.entries
            .iter()
            .map(|(type_id, payload)| (*type_id, payload))
    }

    /// Consume the exact declaration inventory together with one payload per
    /// planned identity and publish the complete batch atomically.
    pub fn commit_declared<E>(
        self,
        mut construct: impl FnMut(TypeId, Payload) -> Result<(DefId, Type), E>,
    ) -> Result<(), TypeTableAppendError<E>> {
        let mut complete = Vec::with_capacity(self.entries.len());
        for (type_id, payload) in self.entries {
            let (declaration, ty) =
                construct(type_id, payload).map_err(TypeTableAppendError::Construction)?;
            complete.push((type_id, declaration, ty));
        }
        let Some(inventory) = self.declaration_inventory else {
            return Err(TypeTableAppendError::MissingDeclarationInventory);
        };
        self.table.publish_complete_declared(complete, inventory)
    }

    fn commit_unclaimed<E>(
        self,
        mut construct: impl FnMut(TypeId, Payload) -> Result<Type, E>,
    ) -> Result<(), TypeTableAppendError<E>> {
        let mut complete = Vec::with_capacity(self.entries.len());
        for (type_id, payload) in self.entries {
            let ty = construct(type_id, payload).map_err(TypeTableAppendError::Construction)?;
            complete.push((type_id, ty));
        }
        self.table.publish_complete_unclaimed(complete)
    }
}

impl TypeTable {
    fn publish_complete_unclaimed<E>(
        &mut self,
        complete: Vec<(TypeId, Type)>,
    ) -> Result<(), TypeTableAppendError<E>> {
        let published_len = self.types.len() + complete.len();
        for (type_id, ty) in &complete {
            if let Some(referenced) = invalid_type_reference(ty, published_len) {
                return Err(TypeTableAppendError::InvalidTypePayload {
                    type_id: *type_id,
                    referenced,
                });
            }
        }
        let mut complete_names = self.by_name.clone();
        for (type_id, ty) in &complete {
            let Some(name) = ty.name() else {
                continue;
            };
            if complete_names.insert(name.to_string(), *type_id).is_some() {
                return Err(TypeTableAppendError::DuplicateTypeName {
                    type_id: *type_id,
                    name: name.to_string(),
                });
            }
        }
        self.types.extend(complete.into_iter().map(|(_, ty)| ty));
        self.by_name = complete_names;
        Ok(())
    }

    fn publish_complete_declared<E>(
        &mut self,
        complete: Vec<(TypeId, DefId, Type)>,
        inventory: TypeDeclarationInventory,
    ) -> Result<(), TypeTableAppendError<E>> {
        if !self.has_exact_predefined_inventory() {
            return Err(TypeTableAppendError::ExistingUnclaimedPayload);
        }
        if inventory
            .predefined
            .iter()
            .any(|claim| self.lookup(claim.name) != Some(claim.type_id))
        {
            return Err(TypeTableAppendError::ExistingUnclaimedPayload);
        }
        if complete.len() != inventory.len() {
            return Err(TypeTableAppendError::DeclarationCount {
                expected: inventory.len(),
                actual: complete.len(),
            });
        }
        let mut claimed = IndexMap::new();
        for (type_id, declaration, _) in &complete {
            if let Some(previous) = claimed.insert(*declaration, *type_id) {
                return Err(TypeTableAppendError::DuplicateDeclaration {
                    type_id: *type_id,
                    declaration: *declaration,
                    previous,
                });
            }
        }
        let mut issued = IndexMap::new();
        issued.extend(
            inventory
                .predefined
                .iter()
                .map(|claim| (claim.def_id, claim.type_id)),
        );
        issued.extend(
            inventory
                .declarations
                .iter()
                .zip(&complete)
                .map(|(claim, (type_id, _, _))| (claim.def_id, *type_id)),
        );
        for ((type_id, declaration, ty), expected) in complete.iter().zip(&inventory.declarations) {
            if *declaration != expected.def_id || !expected.matches(ty, &issued) {
                return Err(TypeTableAppendError::DeclarationPayloadMismatch {
                    type_id: *type_id,
                    declaration: *declaration,
                    expected: expected.def_id,
                    expected_name: expected.name.clone(),
                });
            }
        }
        let published_len = self.types.len() + complete.len();
        for (type_id, _, ty) in &complete {
            if let Some(referenced) = invalid_type_reference(ty, published_len) {
                return Err(TypeTableAppendError::InvalidTypePayload {
                    type_id: *type_id,
                    referenced,
                });
            }
        }
        if let Some((type_id, referenced)) = alias_cycle_edge(self.types.len(), &complete) {
            let name = complete
                .iter()
                .find(|(candidate, _, _)| *candidate == type_id)
                .and_then(|(_, _, ty)| ty.name())
                .unwrap_or("<unnamed alias>")
                .to_string();
            return Err(TypeTableAppendError::AliasCycle {
                type_id,
                referenced,
                name,
            });
        }
        self.publish_complete_unclaimed(
            complete
                .into_iter()
                .map(|(type_id, _, ty)| (type_id, ty))
                .collect(),
        )
    }
}

impl TypeDeclarationClaim {
    fn matches(&self, ty: &Type, issued: &IndexMap<DefId, TypeId>) -> bool {
        if ty.name() != Some(self.name.as_str()) {
            return false;
        }
        match (&self.kind, ty) {
            (TypeDeclarationKind::Enumeration(literals), Type::Enumeration(enumeration)) => {
                enumeration.literals.as_slice() == literals.as_ref()
            }
            (TypeDeclarationKind::Alias { base: Some(base) }, Type::Alias(alias)) => {
                issued.get(base) == Some(&alias.aliased)
            }
            (TypeDeclarationKind::Class(kind), Type::Class(class)) => {
                class.def_id == self.def_id && class.kind == *kind
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeTableAppendError<E> {
    Construction(E),
    MissingDeclarationInventory,
    ExistingUnclaimedPayload,
    DeclarationCount {
        expected: usize,
        actual: usize,
    },
    DuplicateDeclaration {
        type_id: TypeId,
        declaration: DefId,
        previous: TypeId,
    },
    DeclarationPayloadMismatch {
        type_id: TypeId,
        declaration: DefId,
        expected: DefId,
        expected_name: String,
    },
    DuplicateTypeName {
        type_id: TypeId,
        name: String,
    },
    InvalidTypePayload {
        type_id: TypeId,
        referenced: Option<TypeId>,
    },
    AliasCycle {
        type_id: TypeId,
        referenced: TypeId,
        name: String,
    },
}

impl<E: std::fmt::Display> std::fmt::Display for TypeTableAppendError<E> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Construction(error) => error.fmt(formatter),
            Self::MissingDeclarationInventory => {
                write!(
                    formatter,
                    "type declaration append has no tree-issued inventory"
                )
            }
            Self::ExistingUnclaimedPayload => write!(
                formatter,
                "type declaration append requires the exact predefined inventory"
            ),
            Self::DeclarationCount { expected, actual } => write!(
                formatter,
                "type declaration inventory contains {expected} entries but the append contains {actual}"
            ),
            Self::DuplicateDeclaration {
                type_id,
                declaration,
                previous,
            } => write!(
                formatter,
                "type {type_id:?} and {previous:?} both claim declaration {declaration:?}"
            ),
            Self::DeclarationPayloadMismatch {
                type_id,
                declaration,
                expected,
                expected_name,
            } => write!(
                formatter,
                "type {type_id:?} claims declaration {declaration:?}, expected {expected:?} for `{expected_name}`"
            ),
            Self::DuplicateTypeName { type_id, name } => {
                write!(formatter, "type {type_id:?} repeats name `{name}`")
            }
            Self::InvalidTypePayload {
                type_id,
                referenced: Some(referenced),
            } => write!(
                formatter,
                "type {type_id:?} references unissued identity {referenced:?}"
            ),
            Self::InvalidTypePayload {
                type_id,
                referenced: None,
            } => write!(formatter, "type {type_id:?} has an incomplete payload"),
            Self::AliasCycle {
                type_id,
                referenced,
                name,
            } => write!(
                formatter,
                "alias `{name}` edge {type_id:?} -> {referenced:?} closes a type cycle"
            ),
        }
    }
}

fn alias_cycle_edge(
    predefined_len: usize,
    complete: &[(TypeId, DefId, Type)],
) -> Option<(TypeId, TypeId)> {
    let mut state = vec![0_u8; complete.len()];
    for start in 0..complete.len() {
        if state[start] != 0 {
            continue;
        }
        if let Some(edge) = alias_cycle_from(start, predefined_len, complete, &mut state) {
            return Some(edge);
        }
    }
    None
}

fn alias_cycle_from(
    start: usize,
    predefined_len: usize,
    complete: &[(TypeId, DefId, Type)],
    state: &mut [u8],
) -> Option<(TypeId, TypeId)> {
    let mut active = Vec::new();
    let mut current = start;
    loop {
        state[current] = 1;
        active.push(current);
        let Type::Alias(alias) = &complete[current].2 else {
            break;
        };
        let referenced = alias.aliased;
        let referenced_index = referenced.index() as usize;
        if referenced_index < predefined_len {
            break;
        }
        let next = referenced_index - predefined_len;
        if state[next] == 1 {
            return Some((complete[current].0, referenced));
        }
        if state[next] == 2 {
            break;
        }
        current = next;
    }
    for index in active {
        state[index] = 2;
    }
    None
}

fn invalid_type_reference(ty: &Type, published_len: usize) -> Option<Option<TypeId>> {
    let invalid = |referenced: TypeId| {
        (referenced.is_unknown() || referenced.index() as usize >= published_len)
            .then_some(Some(referenced))
    };
    match ty {
        Type::Array(array) => invalid(array.element),
        Type::Alias(alias) => invalid(alias.aliased),
        Type::Function(function) => function
            .inputs
            .iter()
            .chain(&function.outputs)
            .find_map(|(_, referenced)| invalid(*referenced)),
        Type::Unknown => Some(None),
        Type::Builtin(_) | Type::Class(_) | Type::Enumeration(_) => None,
    }
}

impl TypeTable {
    /// Create a type table containing the predefined Modelica types.
    pub fn with_predefined_types() -> Self {
        let mut table = Self::empty_for_checked_construction();
        let plan = table
            .plan_unclaimed_append(Self::builtin_types())
            .expect("the fixed predefined type inventory fits the TypeId domain");
        plan.commit_unclaimed(|_, ty| Ok::<_, std::convert::Infallible>(ty))
            .expect("the fixed predefined type inventory has unique names");
        table
    }

    fn empty_for_checked_construction() -> Self {
        Self {
            types: Vec::new(),
            by_name: IndexMap::new(),
        }
    }

    fn builtin_types() -> Vec<Type> {
        // MLS §4.9: Predefined types
        vec![
            Type::Builtin(BuiltinType::Real),
            Type::Builtin(BuiltinType::Integer),
            Type::Builtin(BuiltinType::Boolean),
            Type::Builtin(BuiltinType::String),
            Type::Builtin(BuiltinType::Clock),
            Type::Enumeration(EnumerationType {
                name: PREDEFINED_TYPE_NAMES[5].to_string(),
                literals: vec![
                    "never".to_string(),
                    "avoid".to_string(),
                    "default".to_string(),
                    "prefer".to_string(),
                    "always".to_string(),
                ],
            }),
            Type::Enumeration(EnumerationType {
                name: PREDEFINED_TYPE_NAMES[6].to_string(),
                literals: vec!["warning".to_string(), "error".to_string()],
            }),
        ]
    }

    pub(crate) fn canonical_predefined_entries()
    -> impl ExactSizeIterator<Item = (&'static str, TypeId)> {
        PREDEFINED_TYPE_NAMES
            .into_iter()
            .enumerate()
            .map(|(index, name)| {
                let index = u32::try_from(index)
                    .expect("the fixed predefined prefix fits the TypeId domain");
                (name, TypeId::new(index))
            })
    }

    /// Plan the complete resolved declaration inventory and append it once.
    pub fn plan_declaration_append<Payload>(
        &mut self,
        payloads: Vec<Payload>,
        inventory: TypeDeclarationInventory,
    ) -> Result<TypeTableAppendPlan<'_, Payload>, TypeTableCapacityError> {
        self.plan_append(payloads, Some(inventory))
    }

    fn plan_unclaimed_append<Payload>(
        &mut self,
        payloads: Vec<Payload>,
    ) -> Result<TypeTableAppendPlan<'_, Payload>, TypeTableCapacityError> {
        self.plan_append(payloads, None)
    }

    fn plan_append<Payload>(
        &mut self,
        payloads: Vec<Payload>,
        declaration_inventory: Option<TypeDeclarationInventory>,
    ) -> Result<TypeTableAppendPlan<'_, Payload>, TypeTableCapacityError> {
        let current = self.types.len();
        let additional = payloads.len();
        let end = checked_type_append_end(current, additional)?;
        let ids = (current..end)
            .map(|index| {
                u32::try_from(index)
                    .map(TypeId::new)
                    .map_err(|_| TypeTableCapacityError {
                        current,
                        additional,
                    })
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .zip(payloads)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Ok(TypeTableAppendPlan {
            table: self,
            entries: ids,
            declaration_inventory,
        })
    }

    /// Get a type by its TypeId.
    pub fn get(&self, id: TypeId) -> Option<&Type> {
        if id.is_unknown() {
            None
        } else {
            self.types.get(id.index() as usize)
        }
    }

    /// Iterate over every issued identity and its exact payload.
    pub fn entries(&self) -> impl ExactSizeIterator<Item = (TypeId, &Type)> {
        self.types.iter().enumerate().map(|(index, ty)| {
            let ordinal = u32::try_from(index)
                .expect("checked TypeTable construction keeps every index in the TypeId domain");
            (TypeId::new(ordinal), ty)
        })
    }

    /// Iterate over the canonical predefined type identity prefix.
    pub fn predefined_entries(&self) -> impl ExactSizeIterator<Item = (&'static str, TypeId)> + '_ {
        Self::canonical_predefined_entries().map(|(name, type_id)| {
            assert_eq!(
                self.lookup(name),
                Some(type_id),
                "checked TypeTable construction retains the canonical predefined identity"
            );
            (name, type_id)
        })
    }

    /// Look up a type by name.
    pub fn lookup(&self, name: &str) -> Option<TypeId> {
        self.by_name.get(name).copied()
    }

    /// Get the TypeId for Real.
    pub fn real(&self) -> TypeId {
        self.lookup("Real").expect("primitive Real not registered")
    }

    /// Get the TypeId for Integer.
    pub fn integer(&self) -> TypeId {
        self.lookup("Integer")
            .expect("primitive Integer not registered")
    }

    /// Get the TypeId for Boolean.
    pub fn boolean(&self) -> TypeId {
        self.lookup("Boolean")
            .expect("primitive Boolean not registered")
    }

    /// Get the TypeId for String.
    pub fn string(&self) -> TypeId {
        self.lookup("String")
            .expect("primitive String not registered")
    }

    /// Get the TypeId for Clock.
    pub fn clock(&self) -> TypeId {
        self.lookup("Clock")
            .expect("primitive Clock not registered")
    }

    /// Get the number of types in the table.
    pub fn len(&self) -> usize {
        self.types.len()
    }

    /// Check if the table is empty.
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct TypeTableWire {
    types: Vec<Type>,
    #[serde(deserialize_with = "deserialize_unique_type_name_index")]
    by_name: IndexMap<String, TypeId>,
}

fn deserialize_unique_type_name_index<'de, D>(
    deserializer: D,
) -> Result<IndexMap<String, TypeId>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::deserialize_unique_index_map(deserializer, "TypeTableWire.by_name")
}

#[derive(Debug)]
pub(crate) enum TypeTableReplayError {
    MissingPredefinedInventory,
    Capacity(TypeTableCapacityError),
    Append(TypeTableAppendError<std::convert::Infallible>),
    NameIndexMismatch,
}

impl std::fmt::Display for TypeTableReplayError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingPredefinedInventory => {
                write!(
                    formatter,
                    "TypeTable is missing its exact predefined type prefix"
                )
            }
            Self::Capacity(error) => error.fmt(formatter),
            Self::Append(error) => error.fmt(formatter),
            Self::NameIndexMismatch => write!(
                formatter,
                "TypeTable name index does not exactly match its checked payloads"
            ),
        }
    }
}

impl std::error::Error for TypeTableReplayError {}

impl TypeTable {
    pub(crate) fn replay_checked(
        mut wire: TypeTableWire,
        inventory: TypeDeclarationInventory,
    ) -> Result<Self, TypeTableReplayError> {
        let claimed_names = wire.by_name;
        if wire.types.len() < Self::builtin_types().len() {
            return Err(TypeTableReplayError::MissingPredefinedInventory);
        }
        let declared = wire.types.split_off(Self::builtin_types().len());
        let mut table = Self::empty_for_checked_construction();
        table
            .plan_unclaimed_append(wire.types)
            .map_err(TypeTableReplayError::Capacity)?
            .commit_unclaimed(|_, ty| Ok::<_, std::convert::Infallible>(ty))
            .map_err(TypeTableReplayError::Append)?;
        if !table.has_exact_predefined_inventory() {
            return Err(TypeTableReplayError::MissingPredefinedInventory);
        }
        let declaration_ids = inventory.def_ids().collect::<Vec<_>>();
        table
            .plan_declaration_append(
                declaration_ids.into_iter().zip(declared).collect(),
                inventory,
            )
            .map_err(TypeTableReplayError::Capacity)?
            .commit_declared(|_, (expected, ty)| {
                let claimed = match &ty {
                    Type::Class(class) => class.def_id,
                    _ => expected,
                };
                Ok::<_, std::convert::Infallible>((claimed, ty))
            })
            .map_err(TypeTableReplayError::Append)?;
        if table.by_name != claimed_names {
            return Err(TypeTableReplayError::NameIndexMismatch);
        }
        Ok(table)
    }

    fn has_exact_predefined_inventory(&self) -> bool {
        self.types.len() == Self::builtin_types().len()
            && self.by_name.len() == Self::builtin_types().len()
            && self.lookup("Real") == Some(TypeId::new(0))
            && self.lookup("Integer") == Some(TypeId::new(1))
            && self.lookup("Boolean") == Some(TypeId::new(2))
            && self.lookup("String") == Some(TypeId::new(3))
            && self.lookup("Clock") == Some(TypeId::new(4))
            && self.lookup("StateSelect") == Some(TypeId::new(5))
            && self.lookup("AssertionLevel") == Some(TypeId::new(6))
            && matches!(self.types.first(), Some(Type::Builtin(BuiltinType::Real)))
            && matches!(self.types.get(1), Some(Type::Builtin(BuiltinType::Integer)))
            && matches!(self.types.get(2), Some(Type::Builtin(BuiltinType::Boolean)))
            && matches!(self.types.get(3), Some(Type::Builtin(BuiltinType::String)))
            && matches!(self.types.get(4), Some(Type::Builtin(BuiltinType::Clock)))
            && matches!(
                self.types.get(5),
                Some(Type::Enumeration(enumeration))
                    if enumeration.name == "StateSelect"
                        && enumeration.literals
                            == ["never", "avoid", "default", "prefer", "always"]
            )
            && matches!(
                self.types.get(6),
                Some(Type::Enumeration(enumeration))
                    if enumeration.name == "AssertionLevel"
                        && enumeration.literals == ["warning", "error"]
            )
    }
}

/// A type in Modelica (MLS §4).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    /// Built-in primitive type (Real, Integer, Boolean, String).
    Builtin(BuiltinType),
    /// Class type (model, record, connector, etc.).
    Class(ClassType),
    /// Array type with element type and dimensions.
    Array(ArrayType),
    /// Enumeration type.
    Enumeration(EnumerationType),
    /// Type alias (type X = Y).
    Alias(TypeAlias),
    /// Function type.
    Function(FunctionType),
    /// Unknown/error type (used during type inference).
    Unknown,
}

impl Type {
    /// Get the name of this type, if it has one.
    pub fn name(&self) -> Option<&str> {
        match self {
            Type::Builtin(b) => Some(b.name()),
            Type::Class(c) => Some(&c.name),
            Type::Enumeration(e) => Some(&e.name),
            Type::Alias(a) => Some(&a.name),
            Type::Function(f) => Some(&f.name),
            Type::Array(_) | Type::Unknown => None,
        }
    }

    /// Check if this is a numeric type (Real or Integer).
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::Builtin(BuiltinType::Real) | Type::Builtin(BuiltinType::Integer)
        )
    }

    /// Check if this is a scalar type (not array).
    pub fn is_scalar(&self) -> bool {
        !matches!(self, Type::Array(_))
    }
}

/// MLS §4.9: Predefined types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BuiltinType {
    /// 64-bit floating point.
    Real,
    /// Machine integer.
    Integer,
    /// Boolean (true/false).
    Boolean,
    /// Unicode string.
    String,
    /// Clock domain type (MLS §16.9, Modelica 3.3+ synchronous features).
    Clock,
}

impl BuiltinType {
    /// Get the name of this builtin type.
    pub fn name(&self) -> &'static str {
        match self {
            BuiltinType::Real => "Real",
            BuiltinType::Integer => "Integer",
            BuiltinType::Boolean => "Boolean",
            BuiltinType::String => "String",
            BuiltinType::Clock => "Clock",
        }
    }
}

/// A class type (model, record, connector, etc.).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassType {
    /// The class name.
    pub name: String,
    /// Reference to the class definition.
    pub def_id: DefId,
    /// The kind of class (MLS §4.7).
    pub kind: ClassKind,
}

/// MLS §4.7: Specialized classes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ClassKind {
    /// General class.
    Class,
    /// Model (equations and components).
    Model,
    /// Block (causal with inputs/outputs).
    Block,
    /// Record (data structure).
    Record,
    /// Connector (for connections).
    Connector,
    /// Type alias.
    Type,
    /// Package (namespace).
    Package,
    /// Function.
    Function,
    /// Operator (for operator overloading).
    Operator,
}

/// An array type.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArrayType {
    /// Element type.
    pub element: TypeId,
    /// Array dimensions (None means unknown size, e.g., from `:` subscript).
    pub dims: Vec<Option<i64>>,
}

impl ArrayType {
    /// Create a new array type.
    pub fn new(element: TypeId, dims: Vec<Option<i64>>) -> Self {
        Self { element, dims }
    }

    /// Get the number of dimensions.
    pub fn ndims(&self) -> usize {
        self.dims.len()
    }
}

/// An enumeration type.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumerationType {
    /// The enumeration name.
    pub name: String,
    /// The literal values.
    pub literals: Vec<String>,
}

/// A type alias (type X = Y).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeAlias {
    /// The alias name.
    pub name: String,
    /// The underlying type.
    pub aliased: TypeId,
}

/// A function type.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionType {
    /// Function name.
    pub name: String,
    /// Input parameter types.
    pub inputs: Vec<(String, TypeId)>,
    /// Output parameter types.
    pub outputs: Vec<(String, TypeId)>,
}

/// MLS §6.4: Interface (type) of a class.
///
/// The interface comprises the information needed for subtype compatibility checking.
/// "The interface of a class is also the interface of the component having that class as type."
///
/// Two classes are compatible if their interfaces are compatible according to the
/// subtype rules in MLS §6.4-6.6.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Interface {
    /// Whether this is transitively non-replaceable (MLS §6.4).
    pub transitively_non_replaceable: bool,

    /// Whether this represents a class (vs component).
    pub is_class: bool,

    /// Specialized class kind (model, record, connector, function, etc.).
    pub class_kind: Option<ClassKind>,

    /// Component prefixes (flow, stream, variability, causality).
    pub prefixes: InterfacePrefixes,

    /// Array dimensions.
    pub dimensions: Vec<Option<i64>>,

    /// Whether this is a conditional component.
    pub conditional: bool,

    /// Whether this is final.
    pub is_final: bool,

    /// Whether this is inner/outer.
    pub inner: bool,
    pub outer: bool,

    /// Public named elements with their interfaces (recursive).
    pub elements: IndexMap<String, InterfaceElement>,

    /// If this is an operator record, the base class identity.
    pub operator_record_base: Option<DefId>,

    /// If derived from ExternalObject, the full name.
    pub external_object_name: Option<String>,

    /// If this is an enumeration, the literal names in order.
    pub enum_literals: Option<Vec<String>>,
}

/// Prefixes that affect interface compatibility (MLS §6.4).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct InterfacePrefixes {
    /// Flow prefix.
    pub flow: bool,
    /// Stream prefix.
    pub stream: bool,
    /// Variability (constant, parameter, discrete, continuous).
    pub variability: InterfaceVariability,
    /// Causality (input, output).
    pub causality: InterfaceCausality,
}

/// Variability levels for interface compatibility (MLS §4.5).
///
/// Per MLS §4.5: "constant < parameter < discrete < continuous"
/// This ordering is used for variability constraint checking.
/// A higher variability expression can depend on lower/equal variability expressions.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum InterfaceVariability {
    /// Constant (fixed at translation) - most restrictive.
    Constant,
    /// Parameter (fixed after initialization).
    Parameter,
    /// Discrete-time.
    Discrete,
    /// Continuous-time (default for Real) - least restrictive.
    #[default]
    Continuous,
}

/// Causality for interface compatibility (MLS §4.4.2).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum InterfaceCausality {
    /// No causality prefix.
    #[default]
    None,
    /// Input prefix.
    Input,
    /// Output prefix.
    Output,
}

/// An element in an interface (MLS §6.4).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InterfaceElement {
    /// Name of the element.
    pub name: String,
    /// Whether this is a class element (vs component).
    pub is_class: bool,
    /// The interface of this element.
    pub interface: Box<Interface>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::ser::SerializeMap;

    struct DuplicateNameIndex;

    impl Serialize for DuplicateNameIndex {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            let mut map = serializer.serialize_map(Some(2))?;
            map.serialize_entry("Real", &TypeId::new(4_294_967_294))?;
            map.serialize_entry("Real", &TypeId::new(0))?;
            map.end()
        }
    }

    #[derive(Serialize)]
    struct DuplicateTypeTableWire {
        types: Vec<Type>,
        by_name: DuplicateNameIndex,
    }

    #[test]
    fn type_table_wire_rejects_duplicate_raw_name_keys() {
        let forged = r#"{"types":[],"by_name":{"Real":4294967294,"Real":0}}"#;
        assert!(
            serde_json::from_str::<TypeTableWire>(forged).is_err(),
            "a forged first Real identity cannot disappear behind the canonical repeated key"
        );
    }

    #[test]
    fn type_table_wire_rejects_duplicate_binary_map_keys() {
        let forged = DuplicateTypeTableWire {
            types: Vec::new(),
            by_name: DuplicateNameIndex,
        };
        let bytes = bincode::serialize(&forged).expect("duplicate-key map wire serializes");
        assert!(
            bincode::deserialize::<TypeTableWire>(&bytes).is_err(),
            "map formats cannot collapse repeated TypeTable name keys"
        );
    }

    #[test]
    fn type_table_wire_retains_unique_name_order() {
        let wire = serde_json::from_str::<TypeTableWire>(
            r#"{"types":[],"by_name":{"Integer":1,"Real":0}}"#,
        )
        .expect("unique name keys decode");
        assert_eq!(
            wire.by_name.keys().map(String::as_str).collect::<Vec<_>>(),
            ["Integer", "Real"],
        );
    }

    #[test]
    fn declaration_inventory_requires_every_exact_predefined_claim() {
        let canonical = TypeTable::canonical_predefined_entries()
            .enumerate()
            .map(|(index, (name, type_id))| {
                let index = u32::try_from(index).expect("test prefix fits DefId");
                (DefId::new(index + 100), name.to_string(), type_id)
            })
            .collect::<Vec<_>>();
        TypeDeclarationInventory::new(Vec::new(), canonical.clone())
            .expect("all seven exact canonical claims mint one total product");

        for index in 0..canonical.len() {
            let name = canonical[index].1.clone();

            let mut deleted = canonical.clone();
            deleted.remove(index);
            assert!(
                TypeDeclarationInventory::new(Vec::new(), deleted).is_err(),
                "deleting `{name}` cannot mint the predefined product"
            );

            let mut substituted = canonical.clone();
            substituted[index].1 = format!("{name}Substituted");
            assert!(
                TypeDeclarationInventory::new(Vec::new(), substituted).is_err(),
                "substituting `{name}` cannot mint the predefined product"
            );

            let mut duplicated = canonical.clone();
            duplicated[index].0 = canonical[(index + 1) % canonical.len()].0;
            assert!(
                TypeDeclarationInventory::new(Vec::new(), duplicated).is_err(),
                "duplicating `{name}` identity cannot mint the predefined product"
            );
        }
    }

    #[test]
    fn atomic_append_exposes_forward_ids_and_publishes_complete_values() {
        let mut table = TypeTable::with_predefined_types();
        let real = table.real();
        let plan = table
            .plan_unclaimed_append(vec!["Forward", "Terminal"])
            .expect("two aliases fit in the type identity space");
        let ids = plan
            .entries()
            .map(|(type_id, _)| type_id)
            .collect::<Vec<_>>();
        plan.commit_unclaimed(|type_id, name| {
            let aliased = if type_id == ids[0] { ids[1] } else { real };
            Ok::<_, ()>(Type::Alias(TypeAlias {
                name: name.to_string(),
                aliased,
            }))
        })
        .expect("both detached aliases are complete");

        assert_eq!(table.lookup("Forward"), Some(ids[0]));
        assert_eq!(table.lookup("Terminal"), Some(ids[1]));
        assert!(matches!(
            table.get(ids[0]),
            Some(Type::Alias(alias)) if alias.aliased == ids[1]
        ));
    }

    #[test]
    fn atomic_append_failure_publishes_nothing() {
        let mut table = TypeTable::with_predefined_types();
        let before = table.len();
        let plan = table
            .plan_unclaimed_append(vec!["First", "Second"])
            .expect("two aliases fit in the type identity space");
        let ids = plan
            .entries()
            .map(|(type_id, _)| type_id)
            .collect::<Vec<_>>();
        let result = plan.commit_unclaimed(|type_id, name| {
            if type_id == ids[1] {
                return Err("second alias is malformed");
            }
            Ok(Type::Alias(TypeAlias {
                name: name.to_string(),
                aliased: ids[1],
            }))
        });

        assert_eq!(
            result,
            Err(TypeTableAppendError::Construction(
                "second alias is malformed"
            ))
        );
        assert_eq!(table.len(), before);
        assert_eq!(table.lookup("First"), None);
    }

    #[test]
    fn atomic_append_refuses_name_overwrite_before_publication() {
        let mut table = TypeTable::with_predefined_types();
        let before = table.len();
        let real = table.real();
        let plan = table
            .plan_unclaimed_append(vec!["Real"])
            .expect("one identity fits in the type identity space");
        let appended = plan
            .entries()
            .next()
            .map(|(type_id, _)| type_id)
            .expect("one payload produces one planned identity");
        let result = plan.commit_unclaimed(|_, name| {
            Ok::<_, ()>(Type::Alias(TypeAlias {
                name: name.to_string(),
                aliased: real,
            }))
        });

        assert_eq!(
            result,
            Err(TypeTableAppendError::DuplicateTypeName {
                type_id: appended,
                name: "Real".to_string(),
            })
        );
        assert_eq!(table.len(), before);
        assert_eq!(table.lookup("Real"), Some(real));
    }

    #[test]
    fn atomic_append_refuses_duplicate_new_names_before_publication() {
        let mut table = TypeTable::with_predefined_types();
        let before = table.len();
        let real = table.real();
        let plan = table
            .plan_unclaimed_append(vec!["Repeated", "Repeated"])
            .expect("two identities fit in the type identity space");
        let second = plan
            .entries()
            .nth(1)
            .map(|(type_id, _)| type_id)
            .expect("two payloads produce two planned identities");
        let result = plan.commit_unclaimed(|_, name| {
            Ok::<_, ()>(Type::Alias(TypeAlias {
                name: name.to_string(),
                aliased: real,
            }))
        });

        assert_eq!(
            result,
            Err(TypeTableAppendError::DuplicateTypeName {
                type_id: second,
                name: "Repeated".to_string(),
            })
        );
        assert_eq!(table.len(), before);
        assert_eq!(table.lookup("Repeated"), None);
    }

    #[test]
    fn atomic_append_checks_identity_capacity_without_truncation() {
        let error = checked_type_append_end(7, usize::MAX)
            .expect_err("usize overflow cannot produce a type append range");
        assert_eq!(error.current, 7);
        assert_eq!(error.additional, usize::MAX);
    }

    #[test]
    fn atomic_append_refuses_unknown_and_unissued_references() {
        let mut table = TypeTable::with_predefined_types();
        let before = table.len();
        let planned = table
            .plan_unclaimed_append(vec![TypeId::UNKNOWN])
            .expect("one payload identity fits");
        let type_id = planned
            .entries()
            .next()
            .expect("one payload has one identity")
            .0;
        let result = planned.commit_unclaimed(|_, aliased| {
            Ok::<_, ()>(Type::Alias(TypeAlias {
                name: "Broken".to_string(),
                aliased,
            }))
        });
        assert_eq!(
            result,
            Err(TypeTableAppendError::InvalidTypePayload {
                type_id,
                referenced: Some(TypeId::UNKNOWN),
            })
        );
        assert_eq!(table.len(), before);
        assert_eq!(table.lookup("Broken"), None);
    }

    #[test]
    fn type_table_wire_cannot_mint_empty_or_mismatched_state() {
        let tree = crate::ClassTree::new();
        let mut empty = serde_json::to_value(&tree).expect("valid tree serializes");
        empty["type_table"] = serde_json::json!({"types": [], "by_name": {}});
        assert!(serde_json::from_value::<crate::ClassTree>(empty).is_err());

        let mut mismatched = serde_json::to_value(&tree).expect("valid tree serializes");
        mismatched["type_table"]["by_name"] = serde_json::json!({});
        assert!(serde_json::from_value::<crate::ClassTree>(mismatched).is_err());
    }
}
