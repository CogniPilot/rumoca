//! This module defines the Abstract Syntax Tree (AST) and Intermediate Representation (IR)
//! structures for a custom language or model representation. It provides a comprehensive set
//! of data structures to represent various components, expressions, equations, and statements
//! in the language. The module also includes serialization and deserialization support via
//! `serde` and custom implementations of `Debug` and `Display` traits for better debugging
//! and formatting.
//!
//! # Key Structures
//!
//! - **Location**: Represents the location of a token or element in the source file, including
//!   line and column numbers.
//! - **Token**: Represents a lexical token with its text, location, type, and number.
//! - **Name**: Represents a hierarchical name composed of multiple tokens.
//! - **StoredDefinition**: Represents a collection of class definitions and an optional
//!   "within" clause.
//! - **Component**: Represents a component with its name, type, variability, causality,
//!   connection, description, and initial value.
//! - **ClassDef**: Represents a class definition with its name, components, equations,
//!   and algorithms.
//! - **ComponentReference**: Represents a reference to a component, including its parts and
//!   optional subscripts.
//! - **Equation**: Represents various types of equations, such as simple equations, connect
//!   equations, and conditional equations.
//! - **Expression**: Represents various types of expressions, including binary, unary,
//!   terminal, and function call expressions.
//! - **Statement**: Represents various types of statements, such as assignments, loops, and
//!   function calls.
//!
//! # Enums
//!
//! - **OpBinary**: Represents binary operators like addition, subtraction, multiplication, etc.
//! - **OpUnary**: Represents unary operators like negation and logical NOT.
//! - **TerminalType**: Represents the type of a terminal expression, such as real, integer,
//!   string, or boolean.
//! - **Variability**: Represents the variability of a component (e.g., constant, discrete,
//!   parameter).
//! - **Connection**: Represents the connection type of a component (e.g., flow, stream).
//! - **Causality**: Represents the causality of a component (e.g., input, output).
//!
//! This module is designed to be extensible and serves as the foundation for parsing,
//! analyzing, and generating code for the custom language or model representation.

mod external_object;
pub mod instance;
mod modelica;
mod nodes;
pub mod scope;
mod semantic_identity;
pub mod state_machines;
pub mod types;
pub mod visitor;

use indexmap::{IndexMap, IndexSet};
use rumoca_core::{
    BUILTIN_TYPES, Causality, ClassType, ComponentPath, DefId, Location, OpBinary, OpUnary,
    ScopeId, Span, StateSelect, Token, TypeId, Variability, visit_top_level_path_segments,
};
use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use std::hash::BuildHasher;
use std::sync::Arc;
use std::{fmt::Debug, fmt::Display};

pub use visitor::{
    ComponentReferenceContext, ExpressionContext, ExpressionTransformer, FunctionCallContext,
    NameContext, RequiredValueViolation, RequiredValueViolationKind, SubscriptContext,
    TypeNameContext, VisitScope, Visitor, collect_component_refs, contains_component_ref,
    contains_function_call, declaration_subscript_required_value_violation,
    equation_contains_required_recovery, equation_required_value_violation,
    expression_component_path, expression_contains_required_recovery,
    expression_required_value_violation, is_invocation_tuple_equation,
    modifier_required_value_violation, statement_contains_required_recovery,
    statement_required_value_violation, subscript_required_value_violation, walk_class_def_default,
    walk_component_default, walk_component_reference_default, walk_equation_default,
    walk_expression_default, walk_extend_default, walk_statement_default,
};

pub type AstIndexMap<K, V> = IndexMap<K, V, rustc_hash::FxBuildHasher>;

/// Decode a semantically optional current AST value while requiring its wire
/// key. Absence is represented by an explicit `null`, never by deleting the
/// field and asking serde to invent `None`.
pub(crate) fn deserialize_required_option<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: serde::Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer)
}

/// Decode one current-wire ordered map without allowing a repeated key to be
/// overwritten before checked root construction observes it.
pub(crate) fn deserialize_unique_index_map<'de, D, K, V, S>(
    deserializer: D,
    field: &'static str,
) -> Result<IndexMap<K, V, S>, D::Error>
where
    D: serde::Deserializer<'de>,
    K: Deserialize<'de> + Debug + Eq + std::hash::Hash,
    V: Deserialize<'de>,
    S: BuildHasher + Default,
{
    struct UniqueIndexMapVisitor<K, V, S>(&'static str, std::marker::PhantomData<(K, V, S)>);

    impl<'de, K, V, S> serde::de::Visitor<'de> for UniqueIndexMapVisitor<K, V, S>
    where
        K: Deserialize<'de> + Debug + Eq + std::hash::Hash,
        V: Deserialize<'de>,
        S: BuildHasher + Default,
    {
        type Value = IndexMap<K, V, S>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(formatter, "{} with unique keys", self.0)
        }

        fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::MapAccess<'de>,
        {
            // A wire-provided size hint is not construction authority. Growing
            // from an empty map prevents malformed binary input from turning a
            // forged length into an allocation before any key is replayed.
            let mut entries = IndexMap::with_hasher(S::default());
            while let Some((key, value)) = access.next_entry()? {
                insert_unique_wire_entry(&mut entries, key, value, self.0)?;
            }
            Ok(entries)
        }
    }

    deserializer.deserialize_map(UniqueIndexMapVisitor(field, std::marker::PhantomData))
}

fn insert_unique_wire_entry<K, V, S, E>(
    entries: &mut IndexMap<K, V, S>,
    key: K,
    value: V,
    field: &str,
) -> Result<(), E>
where
    K: Debug + Eq + std::hash::Hash,
    S: BuildHasher,
    E: serde::de::Error,
{
    match entries.entry(key) {
        indexmap::map::Entry::Vacant(entry) => {
            entry.insert(value);
            Ok(())
        }
        indexmap::map::Entry::Occupied(entry) => Err(E::custom(format_args!(
            "{field} contains duplicate key {:?}",
            entry.key(),
        ))),
    }
}

#[cfg(test)]
pub(crate) struct RepeatedMap<'a, K, V> {
    pub(crate) entries: &'a AstIndexMap<K, V>,
    pub(crate) repeated_key: &'a K,
    pub(crate) forged_value: &'a V,
    pub(crate) canonical_first: bool,
}

#[cfg(test)]
impl<K, V> Serialize for RepeatedMap<'_, K, V>
where
    K: Serialize + Eq + std::hash::Hash,
    V: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;

        let mut map = serializer.serialize_map(Some(self.entries.len() + 1))?;
        for (key, canonical_value) in self.entries {
            if key != self.repeated_key {
                map.serialize_entry(key, canonical_value)?;
                continue;
            }
            let (first, second) = if self.canonical_first {
                (canonical_value, self.forged_value)
            } else {
                (self.forged_value, canonical_value)
            };
            map.serialize_entry(key, first)?;
            map.serialize_entry(key, second)?;
        }
        map.end()
    }
}

fn deserialize_unique_def_map<'de, D>(
    deserializer: D,
) -> Result<AstIndexMap<DefId, String>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    deserialize_unique_index_map(deserializer, "ClassTreeWire.def_map")
}

fn deserialize_unique_name_map<'de, D>(
    deserializer: D,
) -> Result<AstIndexMap<String, DefId>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    deserialize_unique_index_map(deserializer, "ClassTreeWire.name_map")
}

fn deserialize_unique_scope_to_class<'de, D>(
    deserializer: D,
) -> Result<AstIndexMap<ScopeId, DefId>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    deserialize_unique_index_map(deserializer, "ClassTreeWire.scope_to_class")
}

pub use external_object::{
    ExternalObjectLifecycle, ExternalObjectLifecycleError, ExternalObjectLifecycleRole,
};
pub use nodes::*;
pub use semantic_identity::{
    classes_are_semantically_compatible, components_are_semantically_compatible,
};

// Re-export key types from submodules
pub use instance::{
    ClassInstanceData, ClassOverride, ClassOverrideMap, ConnectionOperatorCatalog,
    EffectiveTypePublicationError, EqualityConstraintCardinality,
    EqualityConstraintDeclarationIndex, EqualityConstraintEffectiveRecordIdentity,
    EqualityConstraintExposureError, EqualityConstraintOccurrenceError,
    EqualityConstraintOccurrenceExposure, EqualityConstraintPrototype,
    EqualityConstraintSelectionProof, EqualityConstraintSpecializationKey,
    ExternalObjectLifecycleCatalog, ExternalObjectLifecycleIdentity,
    FinalizedOverconstrainedCatalog, FinalizedOverconstrainedComponent,
    FinalizedOverconstrainedRecord, InstanceConnection, InstanceConnectionConstructionError,
    InstanceConnectionEndpoint, InstanceConnectionFamily, InstanceData, InstanceEquation,
    InstanceOverlay, InstanceOverlayInsertError, InstanceScalarConnection, InstanceStatement,
    InstancedTree, ModificationEnvironment, ModificationValue, QualifiedName,
    SemanticCatalogProjection,
};
pub use scope::{
    EffectiveImport, EffectiveImports, Import as ScopeImport, ImportBinding, ImportRefusal,
    InheritedMember, LookupOutcome, Scope, ScopeKind, ScopeTree, WildcardMember,
};
pub use state_machines::{State, StateMachine, StateMachineState, StateMachines, Transition};
pub use types::{
    ArrayType, BuiltinType, ClassKind, ClassType as TypeClassType, EnumerationType, FunctionType,
    Interface, InterfaceCausality, InterfaceElement, InterfacePrefixes, InterfaceVariability, Type,
    TypeAlias, TypeDeclarationInventory, TypeDeclarationInventoryError, TypeTable,
    TypeTableAppendError, TypeTableAppendPlan, TypeTableCapacityError,
};

/// MLS §5.6: Class Tree - represents the syntactic information from class definitions.
///
/// The ClassTree combines:
/// - The parsed class definitions (StoredDefinition)
/// - The type table (all types in the compilation unit)
/// - The scope tree (for name lookup)
/// - The def_map (DefId → qualified name for O(1) resolved definition lookup)
/// - The name_map (qualified name → DefId for O(1) resolved definition lookup)
///
/// This is the primary IR produced by parsing + semantic analysis.
#[derive(Debug, Clone, Serialize)]
pub struct ClassTree {
    /// The parsed class definitions.
    pub definitions: StoredDefinition,
    /// All types in the compilation unit.
    pub type_table: TypeTable,
    /// Scope tree for name lookup.
    pub scope_tree: ScopeTree,
    /// Map from DefId to qualified name (e.g., "Package.SubPackage.Model").
    /// Populated during the resolve phase for O(1) resolved definition lookup.
    pub def_map: AstIndexMap<DefId, String>,
    /// Inverse map from qualified name to DefId for O(1) resolved definition lookup.
    /// This includes non-class definitions such as components.
    /// Populated during the resolve phase alongside def_map.
    pub name_map: AstIndexMap<String, DefId>,
    /// Each class scope's declaring class. Populated during resolve so later
    /// phases walk enclosing classes through the scope tree instead of
    /// re-parsing qualified names.
    pub scope_to_class: AstIndexMap<ScopeId, DefId>,
    /// Source map for mapping file names to SourceIds.
    /// Populated during session build for multi-file diagnostics.
    pub source_map: rumoca_core::SourceMap,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ClassTreeWire {
    definitions: StoredDefinition,
    type_table: types::TypeTableWire,
    scope_tree: ScopeTree,
    #[serde(deserialize_with = "deserialize_unique_def_map")]
    def_map: AstIndexMap<DefId, String>,
    #[serde(deserialize_with = "deserialize_unique_name_map")]
    name_map: AstIndexMap<String, DefId>,
    #[serde(deserialize_with = "deserialize_unique_scope_to_class")]
    scope_to_class: AstIndexMap<ScopeId, DefId>,
    source_map: rumoca_core::SourceMap,
}

impl<'de> Deserialize<'de> for ClassTree {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = ClassTreeWire::deserialize(deserializer)?;
        let mut tree = Self {
            definitions: wire.definitions,
            type_table: TypeTable::with_predefined_types(),
            scope_tree: wire.scope_tree,
            def_map: wire.def_map,
            name_map: wire.name_map,
            scope_to_class: wire.scope_to_class,
            source_map: wire.source_map,
        };
        let inventory = tree
            .type_declaration_inventory()
            .map_err(serde::de::Error::custom)?;
        tree.type_table = TypeTable::replay_checked(wire.type_table, inventory)
            .map_err(serde::de::Error::custom)?;
        Ok(tree)
    }
}

fn ast_type_class_kind(class_type: &ClassType) -> ClassKind {
    match class_type {
        ClassType::Class => ClassKind::Class,
        ClassType::Model => ClassKind::Model,
        ClassType::Block => ClassKind::Block,
        ClassType::Record => ClassKind::Record,
        ClassType::Connector => ClassKind::Connector,
        ClassType::Type => ClassKind::Type,
        ClassType::Package => ClassKind::Package,
        ClassType::Function => ClassKind::Function,
        ClassType::Operator => ClassKind::Operator,
    }
}

fn type_declaration_kind(class: &ClassDef) -> types::TypeDeclarationKind {
    if !class.enum_literals.is_empty() {
        return types::TypeDeclarationKind::Enumeration(
            class
                .enum_literals
                .iter()
                .map(|literal| literal.ident.text.to_string())
                .collect(),
        );
    }
    if matches!(class.class_type, ClassType::Type) {
        return types::TypeDeclarationKind::Alias {
            base: class
                .extends
                .as_slice()
                .first()
                .filter(|_| class.extends.len() == 1)
                .and_then(|extend| extend.base_def_id),
        };
    }
    types::TypeDeclarationKind::Class(ast_type_class_kind(&class.class_type))
}

impl Default for ClassTree {
    fn default() -> Self {
        Self::new()
    }
}

impl ClassTree {
    /// Create a new empty class tree.
    pub fn new() -> Self {
        Self {
            definitions: StoredDefinition::default(),
            type_table: TypeTable::with_predefined_types(),
            scope_tree: ScopeTree::new(),
            def_map: AstIndexMap::default(),
            name_map: AstIndexMap::default(),
            scope_to_class: AstIndexMap::default(),
            source_map: rumoca_core::SourceMap::new(),
        }
    }

    /// Mint the exact class/enumeration/alias declaration inventory owned by
    /// this resolved class tree.
    pub fn type_declaration_inventory(
        &self,
    ) -> Result<TypeDeclarationInventory, TypeDeclarationInventoryError> {
        let mut expected_by_def = AstIndexMap::default();
        let mut expected_by_name = AstIndexMap::default();
        collect_predefined_declaration_indexes(self, &mut expected_by_def, &mut expected_by_name)?;
        let predefined_by_name = expected_by_name.clone();
        let mut structural = Vec::new();
        collect_class_declaration_indexes(
            &self.definitions.classes,
            None,
            &mut structural,
            &mut expected_by_def,
            &mut expected_by_name,
            &predefined_by_name,
        )?;
        prove_exact_declaration_indexes(self, &expected_by_def, &expected_by_name)?;

        let (mut aliases, mut declarations): (Vec<_>, Vec<_>) = structural
            .into_iter()
            .partition(|(_, _, kind)| matches!(kind, types::TypeDeclarationKind::Alias { .. }));
        declarations.append(&mut aliases);
        let predefined = collect_canonical_predefined_declarations(self)?;
        TypeDeclarationInventory::new(declarations, predefined)
    }

    /// Mint the effective import bindings of one scope (MLS §5.3.1, §13.2).
    ///
    /// This is the sole public producer of [`EffectiveImports`]: every name a
    /// scope can reach through its imports, decided by the one lookup
    /// authority. Shadowed imports are excluded; ambiguous ones are refused
    /// explicitly rather than dropped.
    pub fn effective_imports(&self, scope: ScopeId) -> EffectiveImports {
        self.scope_tree.effective_imports(scope)
    }

    /// The declared local name of a class, read from its own declaration.
    ///
    /// This is a structural identity-to-name projection: the name comes from
    /// the class's name token, never from tokenizing a rendered path.
    pub fn class_local_name(&self, def_id: DefId) -> Option<&str> {
        self.get_class_by_def_id(def_id)
            .map(|class| class.name.text.as_ref())
    }

    /// The declared local name of a member declaration of `container`.
    ///
    /// Covers both component members and nested classes; the name comes from
    /// the member's own declaration, never from tokenizing a rendered path.
    pub fn member_local_name(&self, container: DefId, member: DefId) -> Option<&str> {
        let class = self.get_class_by_def_id(container)?;
        class
            .components
            .values()
            .find(|component| component.def_id == Some(member))
            .map(|component| component.name.as_str())
            .or_else(|| {
                class
                    .classes
                    .values()
                    .find(|nested| nested.def_id == Some(member))
                    .map(|nested| nested.name.text.as_ref())
            })
    }

    /// Qualified names of the classes enclosing `scope` (innermost first),
    /// walked through the scope tree. This is the structured replacement for
    /// re-parsing a qualified name into its enclosing scopes.
    pub fn enclosing_class_names_from(&self, scope: ScopeId) -> impl Iterator<Item = &str> {
        std::iter::successors(Some(scope), |current| self.scope_tree.parent(*current))
            .filter_map(|current| self.scope_to_class.get(&current))
            .filter_map(|class_def_id| self.def_map.get(class_def_id))
            .map(String::as_str)
    }

    /// Qualified names of the classes strictly enclosing `qualified_name`
    /// (innermost first), walked through the scope tree.
    pub fn enclosing_class_names_of(&self, qualified_name: &str) -> impl Iterator<Item = &str> {
        self.get_class_by_qualified_name(qualified_name)
            .and_then(|class| class.scope_id)
            .and_then(|scope| self.scope_tree.parent(scope))
            .into_iter()
            .flat_map(|enclosing| self.enclosing_class_names_from(enclosing))
    }

    /// Create a class tree from a parsed StoredDefinition.
    pub fn from_parsed(definitions: StoredDefinition) -> Self {
        Self {
            definitions,
            type_table: TypeTable::with_predefined_types(),
            scope_tree: ScopeTree::new(),
            def_map: AstIndexMap::default(),
            name_map: AstIndexMap::default(),
            scope_to_class: AstIndexMap::default(),
            source_map: rumoca_core::SourceMap::new(),
        }
    }

    /// Look up a DefId by its qualified name (e.g., "Package.Model").
    ///
    /// This uses the name_map (populated during resolve phase) for O(1) lookup.
    /// Returns None if the name is not found.
    pub fn get_def_id_by_name(&self, name: &str) -> Option<DefId> {
        self.name_map.get(name).copied()
    }

    /// Look up a class definition by its DefId.
    ///
    /// For repeated lookups, build a `ClassDefIndex` once with
    /// `ClassDefIndex::from_tree` and query that instead.
    ///
    /// Returns None if the DefId is not in the map or the class cannot be found.
    pub fn get_class_by_def_id(&self, def_id: DefId) -> Option<&ClassDef> {
        let qualified_name = self.def_map.get(&def_id)?;
        self.get_class_by_qualified_name(qualified_name)
    }

    /// Look up a class definition by its qualified name (e.g., "Package.Model").
    ///
    /// Navigates the nested class structure following the dotted path.
    pub fn get_class_by_qualified_name(&self, qualified_name: &str) -> Option<&ClassDef> {
        let mut current: Option<&ClassDef> = None;
        let mut failed = false;
        visit_top_level_path_segments(qualified_name, |segment| {
            if failed {
                return;
            }
            current = match current {
                Some(class_def) => class_def.classes.get(segment),
                None => self.definitions.classes.get(segment),
            };
            failed = current.is_none();
        });

        current.filter(|_| !failed)
    }
}

/// Borrowed index from resolved class `DefId` to class definition.
///
/// `ClassTree` owns nested `ClassDef` values, so it cannot store references to
/// itself. Build this short-lived view once in hot semantic passes that already
/// carry resolved `DefId`s and need repeated class-body access.
pub struct ClassDefIndex<'tree> {
    classes: FxHashMap<DefId, &'tree ClassDef>,
    qualified_name_def_ids: FxHashMap<String, DefId>,
    qualified_names: FxHashMap<DefId, String>,
    parent_classes: FxHashMap<DefId, DefId>,
    local_names: FxHashMap<DefId, &'tree str>,
    predefined_def_ids: FxHashMap<&'static str, DefId>,
    builtin_def_ids: FxHashSet<DefId>,
    external_object_def_id: Option<DefId>,
    external_object_owner_def_ids: FxHashSet<DefId>,
}

impl<'tree> ClassDefIndex<'tree> {
    pub fn from_tree(tree: &'tree ClassTree) -> Self {
        let predefined_def_ids = BUILTIN_TYPES
            .iter()
            .filter_map(|&name| {
                tree.scope_tree
                    .predefined_member(&ComponentPath::from_flat_path(name))
                    .map(|def_id| (name, def_id))
            })
            .collect::<FxHashMap<_, _>>();
        let mut index = Self {
            classes: FxHashMap::default(),
            qualified_name_def_ids: FxHashMap::default(),
            qualified_names: FxHashMap::default(),
            parent_classes: FxHashMap::default(),
            local_names: FxHashMap::default(),
            builtin_def_ids: predefined_def_ids.values().copied().collect(),
            predefined_def_ids,
            external_object_def_id: tree
                .scope_tree
                .predefined_member(&ComponentPath::from_flat_path("ExternalObject")),
            external_object_owner_def_ids: FxHashSet::default(),
        };
        for class_def in tree.definitions.classes.values() {
            index.insert_class_tree(class_def, None, None);
        }
        for (qualified_name, def_id) in &tree.name_map {
            if index.classes.contains_key(def_id) {
                index
                    .qualified_name_def_ids
                    .insert(qualified_name.clone(), *def_id);
            }
        }
        for (def_id, qualified_name) in &tree.def_map {
            if index.classes.contains_key(def_id) {
                index
                    .qualified_name_def_ids
                    .insert(qualified_name.clone(), *def_id);
                index
                    .qualified_names
                    .entry(*def_id)
                    .or_insert_with(|| qualified_name.clone());
            }
        }
        if let Some(external_object_def_id) = index.external_object_def_id {
            index.external_object_owner_def_ids =
                external_object_descendants(&index.classes, external_object_def_id);
        }
        index
    }

    pub fn get(&self, def_id: DefId) -> Option<&'tree ClassDef> {
        self.classes.get(&def_id).copied()
    }

    pub fn def_ids(&self) -> impl Iterator<Item = DefId> + '_ {
        self.classes.keys().copied()
    }

    pub fn get_by_qualified_name(&self, qualified_name: &str) -> Option<&'tree ClassDef> {
        self.qualified_name_def_ids
            .get(qualified_name)
            .and_then(|def_id| self.get(*def_id))
    }

    pub fn def_id_by_qualified_name(&self, qualified_name: &str) -> Option<DefId> {
        self.qualified_name_def_ids.get(qualified_name).copied()
    }

    pub fn qualified_name(&self, def_id: DefId) -> Option<&str> {
        self.qualified_names.get(&def_id).map(String::as_str)
    }

    pub fn parent_def_id(&self, def_id: DefId) -> Option<DefId> {
        self.parent_classes.get(&def_id).copied()
    }

    pub fn local_name(&self, def_id: DefId) -> Option<&str> {
        self.local_names.get(&def_id).copied()
    }

    /// Return the resolved declaration identity of a predefined type.
    pub fn predefined_def_id(&self, name: &str) -> Option<DefId> {
        self.predefined_def_ids.get(name).copied()
    }

    pub fn def_ancestry(&self, def_id: DefId) -> Vec<DefId> {
        let mut chain = Vec::new();
        let mut current = Some(def_id);
        while let Some(id) = current {
            chain.push(id);
            current = self.parent_def_id(id);
        }
        chain.reverse();
        chain
    }

    /// Prove MLS §6.3.1 transitive non-replaceability for an exact class-name
    /// exposure path.
    ///
    /// Every written/restated path segment and every declaration in that
    /// segment's owning ancestry must be non-replaceable. A long class proves
    /// that fact directly; only a short class definition additionally depends
    /// on the class reference on the right-hand side of its alias. Missing
    /// identities, unresolved short aliases, and alias cycles cannot mint the
    /// proof.
    pub fn proves_transitively_non_replaceable_path(
        &self,
        path: impl IntoIterator<Item = DefId>,
    ) -> bool {
        let mut proven = FxHashMap::default();
        let mut active = FxHashSet::default();
        path.into_iter().all(|def_id| {
            prove_transitively_non_replaceable_reference(self, def_id, &mut proven, &mut active)
        })
    }

    fn insert_class_tree(
        &mut self,
        class_def: &'tree ClassDef,
        parent_def_id: Option<DefId>,
        parent_qualified_name: Option<&str>,
    ) {
        let qualified_name = match parent_qualified_name {
            Some(parent) if !parent.is_empty() => {
                format!("{parent}.{}", class_def.name.text.as_ref())
            }
            Some(_) | None => class_def.name.text.to_string(),
        };
        if let Some(def_id) = class_def.def_id {
            self.classes.insert(def_id, class_def);
            self.local_names
                .insert(def_id, class_def.name.text.as_ref());
            self.qualified_name_def_ids
                .entry(qualified_name.clone())
                .or_insert(def_id);
            self.qualified_names
                .entry(def_id)
                .or_insert_with(|| qualified_name.clone());
            if let Some(parent_def_id) = parent_def_id {
                self.parent_classes.insert(def_id, parent_def_id);
            }
        }
        let child_parent_def_id = class_def.def_id.or(parent_def_id);
        let child_parent_qualified_name = if class_def.def_id.is_some() {
            Some(qualified_name.as_str())
        } else {
            parent_qualified_name
        };
        if let Some(parent_def_id) = child_parent_def_id {
            self.parent_classes.extend(
                class_def
                    .components
                    .values()
                    .filter_map(|component| component.def_id.map(|def_id| (def_id, parent_def_id))),
            );
        }
        for (name, component) in &class_def.components {
            if let Some(component_def_id) = component.def_id {
                self.local_names.insert(component_def_id, name.as_str());
            }
        }
        for nested in class_def.classes.values() {
            self.insert_class_tree(nested, child_parent_def_id, child_parent_qualified_name);
        }
    }
}

fn collect_predefined_declaration_indexes(
    tree: &ClassTree,
    expected_by_def: &mut AstIndexMap<DefId, String>,
    expected_by_name: &mut AstIndexMap<String, DefId>,
) -> Result<(), TypeDeclarationInventoryError> {
    for (name, def_id) in tree.scope_tree.predefined_members() {
        insert_expected_declaration(
            name.to_string(),
            def_id,
            expected_by_def,
            expected_by_name,
            None,
        )?;
    }
    Ok(())
}

fn collect_canonical_predefined_declarations(
    tree: &ClassTree,
) -> Result<Vec<(DefId, String, TypeId)>, TypeDeclarationInventoryError> {
    let global = tree
        .scope_tree
        .get(tree.scope_tree.global())
        .ok_or_else(|| {
            TypeDeclarationInventoryError::structural(
                "predefined type prefix".to_string(),
                None,
                "the ScopeTree has no global scope".to_string(),
            )
        })?;
    if !matches!(global.kind, ScopeKind::Global) || global.parent.is_some() {
        return Err(TypeDeclarationInventoryError::structural(
            "predefined type prefix".to_string(),
            None,
            "the ScopeTree global slot is not the root global scope".to_string(),
        ));
    }

    TypeTable::canonical_predefined_entries()
        .map(|(name, type_id)| {
            let path = ComponentPath::from_flat_path(name);
            let def_id = tree.scope_tree.predefined_member(&path).ok_or_else(|| {
                TypeDeclarationInventoryError::structural(
                    name.to_string(),
                    None,
                    "the canonical predefined declaration claim is missing".to_string(),
                )
            })?;
            if tree.def_map.get(&def_id).map(String::as_str) != Some(name) {
                return Err(TypeDeclarationInventoryError::structural(
                    name.to_string(),
                    Some(def_id),
                    "the canonical predefined identity has no exact DefId-name claim".to_string(),
                ));
            }
            let visible = tree.name_map.get(name).copied().ok_or_else(|| {
                TypeDeclarationInventoryError::structural(
                    name.to_string(),
                    Some(def_id),
                    "the canonical predefined spelling has no effective name-map claim".to_string(),
                )
            })?;
            if global.members.get(&path).copied() != Some(visible) {
                return Err(TypeDeclarationInventoryError::structural(
                    name.to_string(),
                    Some(def_id),
                    "the global member and effective name-map claims disagree".to_string(),
                ));
            }
            Ok((def_id, name.to_string(), type_id))
        })
        .collect()
}

fn collect_class_declaration_indexes(
    classes: &AstIndexMap<String, ClassDef>,
    parent: Option<&str>,
    declarations: &mut Vec<(DefId, String, types::TypeDeclarationKind)>,
    expected_by_def: &mut AstIndexMap<DefId, String>,
    expected_by_name: &mut AstIndexMap<String, DefId>,
    predefined_by_name: &AstIndexMap<String, DefId>,
) -> Result<(), TypeDeclarationInventoryError> {
    for (stored_name, class) in classes {
        let qualified_name = qualify_declaration_name(parent, stored_name);
        prove_stored_declaration_name(stored_name, class.name.text.as_ref(), &qualified_name)?;
        let def_id = class.def_id.ok_or_else(|| {
            TypeDeclarationInventoryError::structural(
                qualified_name.clone(),
                None,
                "the class payload has no Resolve identity".to_string(),
            )
        })?;
        insert_expected_declaration(
            qualified_name.clone(),
            def_id,
            expected_by_def,
            expected_by_name,
            Some(predefined_by_name),
        )?;
        declarations.push((def_id, qualified_name.clone(), type_declaration_kind(class)));
        collect_component_declaration_indexes(
            class,
            &qualified_name,
            expected_by_def,
            expected_by_name,
            predefined_by_name,
        )?;
        collect_class_declaration_indexes(
            &class.classes,
            Some(&qualified_name),
            declarations,
            expected_by_def,
            expected_by_name,
            predefined_by_name,
        )?;
    }
    Ok(())
}

fn collect_component_declaration_indexes(
    class: &ClassDef,
    parent: &str,
    expected_by_def: &mut AstIndexMap<DefId, String>,
    expected_by_name: &mut AstIndexMap<String, DefId>,
    predefined_by_name: &AstIndexMap<String, DefId>,
) -> Result<(), TypeDeclarationInventoryError> {
    for (stored_name, component) in &class.components {
        let qualified_name = qualify_declaration_name(Some(parent), stored_name);
        prove_stored_declaration_name(stored_name, &component.name, &qualified_name)?;
        let def_id = component.def_id.ok_or_else(|| {
            TypeDeclarationInventoryError::structural(
                qualified_name.clone(),
                None,
                "the component payload has no Resolve identity".to_string(),
            )
        })?;
        insert_expected_declaration(
            qualified_name,
            def_id,
            expected_by_def,
            expected_by_name,
            Some(predefined_by_name),
        )?;
    }
    Ok(())
}

fn prove_stored_declaration_name(
    stored_name: &str,
    payload_name: &str,
    qualified_name: &str,
) -> Result<(), TypeDeclarationInventoryError> {
    if stored_name == payload_name {
        return Ok(());
    }
    Err(TypeDeclarationInventoryError::structural(
        qualified_name.to_string(),
        None,
        format!("stored name `{stored_name}` differs from payload name `{payload_name}`"),
    ))
}

fn qualify_declaration_name(parent: Option<&str>, local_name: &str) -> String {
    match parent {
        Some(parent) => format!("{parent}.{local_name}"),
        None => local_name.to_string(),
    }
}

fn insert_expected_declaration(
    name: String,
    def_id: DefId,
    expected_by_def: &mut AstIndexMap<DefId, String>,
    expected_by_name: &mut AstIndexMap<String, DefId>,
    predefined_by_name: Option<&AstIndexMap<String, DefId>>,
) -> Result<(), TypeDeclarationInventoryError> {
    if let Some(previous_name) = expected_by_def.get(&def_id) {
        return Err(TypeDeclarationInventoryError::structural(
            name,
            Some(def_id),
            format!("Resolve identity is already owned by `{previous_name}`"),
        ));
    }
    if let Some(&previous_def_id) = expected_by_name.get(&name) {
        let shadows_predefined = predefined_by_name
            .and_then(|predefined| predefined.get(&name))
            .is_some_and(|predefined| *predefined == previous_def_id);
        if !shadows_predefined {
            return Err(TypeDeclarationInventoryError::structural(
                name,
                Some(def_id),
                format!("qualified name is already owned by Resolve identity {previous_def_id:?}"),
            ));
        }
    }
    expected_by_def.insert(def_id, name.clone());
    expected_by_name.insert(name, def_id);
    Ok(())
}

fn prove_exact_declaration_indexes(
    tree: &ClassTree,
    expected_by_def: &AstIndexMap<DefId, String>,
    expected_by_name: &AstIndexMap<String, DefId>,
) -> Result<(), TypeDeclarationInventoryError> {
    for (&def_id, name) in expected_by_def {
        if tree.def_map.get(&def_id) != Some(name) {
            return Err(TypeDeclarationInventoryError::structural(
                name.clone(),
                Some(def_id),
                "the definition and DefId index are not an exact association".to_string(),
            ));
        }
    }
    for (name, &def_id) in expected_by_name {
        if tree.name_map.get(name) != Some(&def_id) {
            let actual = tree.name_map.get(name).copied();
            let actual_name = actual
                .and_then(|actual| tree.def_map.get(&actual))
                .map_or("a ghost declaration", String::as_str);
            return Err(TypeDeclarationInventoryError::structural(
                name.clone(),
                actual,
                format!(
                    "declarations `{actual_name}` and `{name}` both claim the name-map identity {actual:?}; expected {def_id:?}"
                ),
            ));
        }
    }
    for (&def_id, name) in &tree.def_map {
        if expected_by_def.get(&def_id) != Some(name) {
            return Err(TypeDeclarationInventoryError::structural(
                name.clone(),
                Some(def_id),
                "the DefId index contains no structural or predefined declaration".to_string(),
            ));
        }
    }
    for (name, &def_id) in &tree.name_map {
        if expected_by_name.get(name) != Some(&def_id) {
            return Err(TypeDeclarationInventoryError::structural(
                name.clone(),
                Some(def_id),
                "the name index contains no structural or predefined declaration".to_string(),
            ));
        }
    }
    Ok(())
}

fn prove_transitively_non_replaceable_reference(
    index: &ClassDefIndex<'_>,
    def_id: DefId,
    proven: &mut FxHashMap<DefId, bool>,
    active: &mut FxHashSet<DefId>,
) -> bool {
    index
        .def_ancestry(def_id)
        .into_iter()
        .all(|part| prove_transitively_non_replaceable_definition(index, part, proven, active))
}

fn prove_transitively_non_replaceable_definition(
    index: &ClassDefIndex<'_>,
    def_id: DefId,
    proven: &mut FxHashMap<DefId, bool>,
    active: &mut FxHashSet<DefId>,
) -> bool {
    if let Some(result) = proven.get(&def_id) {
        return *result;
    }
    let Some(class) = index.get(def_id) else {
        proven.insert(def_id, false);
        return false;
    };
    if class.is_replaceable {
        proven.insert(def_id, false);
        return false;
    }

    // `end_name_token` is the AST's source-form discriminator: long classes
    // have an `end Name`, while short definitions do not. MLS §6.3.1 makes
    // ordinary `extends` irrelevant to a long class's own non-replaceability;
    // recursively proving the base is required only for `class A = P.B` and
    // the other short alias forms represented by their single extends edge.
    let result = if class.end_name_token.is_some() || class.extends.is_empty() {
        true
    } else if class.extends.len() != 1 || !active.insert(def_id) {
        false
    } else {
        let result = class.extends[0].base_def_id.is_some_and(|base| {
            prove_transitively_non_replaceable_reference(index, base, proven, active)
        });
        active.remove(&def_id);
        result
    };
    proven.insert(def_id, result);
    result
}

#[cfg(test)]
mod transitive_nonreplaceability_tests {
    use super::*;
    use serde::ser::SerializeStruct;

    enum RepeatedClassTreeField<'a> {
        DefMap(RepeatedMap<'a, DefId, String>),
        NameMap(RepeatedMap<'a, String, DefId>),
        ScopeToClass(RepeatedMap<'a, ScopeId, DefId>),
    }

    struct RepeatedClassTreeWire<'a> {
        tree: &'a ClassTree,
        repeated: RepeatedClassTreeField<'a>,
    }

    impl Serialize for RepeatedClassTreeWire<'_> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            let mut wire = serializer.serialize_struct("ClassTreeWire", 7)?;
            wire.serialize_field("definitions", &self.tree.definitions)?;
            wire.serialize_field("type_table", &self.tree.type_table)?;
            wire.serialize_field("scope_tree", &self.tree.scope_tree)?;
            match &self.repeated {
                RepeatedClassTreeField::DefMap(repeated) => {
                    wire.serialize_field("def_map", repeated)?;
                    wire.serialize_field("name_map", &self.tree.name_map)?;
                    wire.serialize_field("scope_to_class", &self.tree.scope_to_class)?;
                }
                RepeatedClassTreeField::NameMap(repeated) => {
                    wire.serialize_field("def_map", &self.tree.def_map)?;
                    wire.serialize_field("name_map", repeated)?;
                    wire.serialize_field("scope_to_class", &self.tree.scope_to_class)?;
                }
                RepeatedClassTreeField::ScopeToClass(repeated) => {
                    wire.serialize_field("def_map", &self.tree.def_map)?;
                    wire.serialize_field("name_map", &self.tree.name_map)?;
                    wire.serialize_field("scope_to_class", repeated)?;
                }
            }
            wire.serialize_field("source_map", &self.tree.source_map)?;
            wire.end()
        }
    }

    fn complete_current_wire_tree() -> ClassTree {
        let mut tree = ClassTree::new();
        complete_predefined_claims(&mut tree);
        issue_declaration_types(&mut tree, Vec::new());
        tree
    }

    fn assert_repeated_class_tree_key_rejected(wire: &RepeatedClassTreeWire<'_>, field: &str) {
        let json = serde_json::to_vec(wire).expect("repeated-key JSON witness serializes");
        let json_error = serde_json::from_slice::<ClassTree>(&json)
            .expect_err("current JSON ClassTree wire must reject a repeated map key");
        assert!(
            json_error.to_string().contains(field),
            "JSON refusal must identify {field}: {json_error}",
        );

        let binary = bincode::serialize(wire).expect("repeated-key binary witness serializes");
        let binary_error = bincode::deserialize::<ClassTree>(&binary)
            .expect_err("current binary ClassTree wire must reject a repeated map key");
        assert!(
            binary_error.to_string().contains(field),
            "binary refusal must identify {field}: {binary_error}",
        );
    }

    #[test]
    fn current_class_tree_wire_rejects_deleted_semantic_maps() {
        let complete = serde_json::to_value(ClassTree::new()).expect("class tree serializes");
        for key in ["scope_to_class", "source_map"] {
            let mut missing = complete.clone();
            missing
                .as_object_mut()
                .expect("class tree wire is an object")
                .remove(key)
                .unwrap_or_else(|| panic!("class tree wire contains `{key}`"));
            assert!(
                serde_json::from_value::<ClassTree>(missing).is_err(),
                "deleted `{key}` must not invent compilation-unit semantics"
            );
        }
    }

    #[test]
    fn current_class_tree_wire_rejects_duplicate_raw_type_name_key() {
        let mut tree = ClassTree::new();
        complete_predefined_claims(&mut tree);
        issue_declaration_types(&mut tree, Vec::new());
        let canonical = serde_json::to_string(&tree).expect("current tree serializes");
        let forged = canonical.replacen(
            r#""by_name":{"Real":0"#,
            r#""by_name":{"Real":4294967294,"Real":0"#,
            1,
        );
        assert_ne!(
            forged, canonical,
            "the raw witness must inject a repeated Real key into TypeTable.by_name"
        );
        assert!(
            serde_json::from_str::<ClassTree>(&forged).is_err(),
            "a forged first Real identity must fail before canonical replay"
        );
    }

    #[test]
    fn current_class_tree_wire_rejects_repeated_def_map_key_in_either_order() {
        let tree = complete_current_wire_tree();
        let (&real, _) = tree
            .def_map
            .iter()
            .find(|(_, name)| name.as_str() == "Real")
            .expect("complete predefined declarations contain Real");
        let forged = "ForgedReal".to_string();
        for canonical_first in [true, false] {
            assert_repeated_class_tree_key_rejected(
                &RepeatedClassTreeWire {
                    tree: &tree,
                    repeated: RepeatedClassTreeField::DefMap(RepeatedMap {
                        entries: &tree.def_map,
                        repeated_key: &real,
                        forged_value: &forged,
                        canonical_first,
                    }),
                },
                "ClassTreeWire.def_map",
            );
        }
    }

    #[test]
    fn current_class_tree_wire_rejects_repeated_name_map_key_in_either_order() {
        let tree = complete_current_wire_tree();
        let real = "Real".to_string();
        let forged = DefId::new(u32::MAX - 1);
        for canonical_first in [true, false] {
            assert_repeated_class_tree_key_rejected(
                &RepeatedClassTreeWire {
                    tree: &tree,
                    repeated: RepeatedClassTreeField::NameMap(RepeatedMap {
                        entries: &tree.name_map,
                        repeated_key: &real,
                        forged_value: &forged,
                        canonical_first,
                    }),
                },
                "ClassTreeWire.name_map",
            );
        }
    }

    #[test]
    fn current_class_tree_wire_rejects_repeated_scope_owner_key_in_either_order() {
        let mut tree = complete_current_wire_tree();
        let real = tree.name_map["Real"];
        let integer = tree.name_map["Integer"];
        tree.scope_to_class.insert(ScopeId::GLOBAL, real);
        for canonical_first in [true, false] {
            assert_repeated_class_tree_key_rejected(
                &RepeatedClassTreeWire {
                    tree: &tree,
                    repeated: RepeatedClassTreeField::ScopeToClass(RepeatedMap {
                        entries: &tree.scope_to_class,
                        repeated_key: &ScopeId::GLOBAL,
                        forged_value: &integer,
                        canonical_first,
                    }),
                },
                "ClassTreeWire.scope_to_class",
            );
        }
    }

    #[test]
    fn current_class_tree_wire_roundtrips_json_and_bincode_exactly() {
        let mut tree = ClassTree::new();
        complete_predefined_claims(&mut tree);
        issue_declaration_types(&mut tree, Vec::new());

        let json = serde_json::to_vec(&tree).expect("current JSON wire serializes");
        let json_tree =
            serde_json::from_slice::<ClassTree>(&json).expect("current JSON wire replays");
        assert_eq!(
            serde_json::to_vec(&json_tree).expect("replayed JSON wire serializes"),
            json,
        );

        let binary = bincode::serialize(&tree).expect("current binary wire serializes");
        let binary_tree =
            bincode::deserialize::<ClassTree>(&binary).expect("current binary wire replays");
        assert_eq!(
            bincode::serialize(&binary_tree).expect("replayed binary wire serializes"),
            binary,
        );
    }

    #[test]
    fn current_class_tree_wire_rejects_coordinated_unclaimed_state_select_payload() {
        let mut tree = ClassTree::new();
        complete_predefined_claims(&mut tree);
        issue_declaration_types(&mut tree, Vec::new());
        let mut wire = valid_wire(&tree);
        let payload_table = wire["type_table"].clone();

        wire["scope_tree"]["predefined_members"]
            .as_object_mut()
            .expect("predefined members are an object")
            .remove("StateSelect")
            .expect("StateSelect has a predefined claim");
        wire["scope_tree"]["scopes"][0]["members"]
            .as_object_mut()
            .expect("global members are an object")
            .remove("StateSelect")
            .expect("StateSelect has a global claim");
        wire["name_map"]
            .as_object_mut()
            .expect("name map is an object")
            .remove("StateSelect")
            .expect("StateSelect has a name claim");
        remove_def_map_claim(&mut wire, "StateSelect");

        assert_eq!(
            wire["type_table"], payload_table,
            "the adversary leaves all seven payloads and their name index bit-exact"
        );
        assert_wire_rejected(
            wire,
            "coordinated removal of every StateSelect declaration claim while TypeId(5) survives",
        );
    }

    #[test]
    fn current_class_tree_wire_rejects_every_predefined_claim_adversary() {
        let mut tree = ClassTree::new();
        complete_predefined_claims(&mut tree);
        issue_declaration_types(&mut tree, Vec::new());
        let complete = valid_wire(&tree);
        let names = TypeTable::canonical_predefined_entries()
            .map(|(name, _)| name)
            .collect::<Vec<_>>();

        for (index, name) in names.iter().copied().enumerate() {
            let mut deleted = complete.clone();
            deleted["scope_tree"]["predefined_members"]
                .as_object_mut()
                .expect("predefined members are an object")
                .remove(name)
                .unwrap_or_else(|| panic!("{name} has a predefined claim"));
            assert_wire_rejected(deleted, &format!("deletion of predefined claim `{name}`"));

            let mut substituted = complete.clone();
            substituted["scope_tree"]["predefined_members"]
                .as_object_mut()
                .expect("predefined members are an object")
                .insert(
                    name.to_string(),
                    serde_json::to_value(DefId::new(u32::MAX - 1))
                        .expect("forged DefId serializes"),
                );
            assert_wire_rejected(
                substituted,
                &format!("substitution of predefined claim `{name}`"),
            );

            let duplicate_name = names[(index + 1) % names.len()];
            let duplicate_id = complete["scope_tree"]["predefined_members"][duplicate_name].clone();
            let mut duplicated = complete.clone();
            duplicated["scope_tree"]["predefined_members"]
                .as_object_mut()
                .expect("predefined members are an object")
                .insert(name.to_string(), duplicate_id);
            assert_wire_rejected(
                duplicated,
                &format!("duplicate predefined identity claim `{name}`/`{duplicate_name}`"),
            );
        }
    }

    #[test]
    fn current_class_tree_wire_rejects_two_class_names_claiming_one_def_id() {
        let first = DefId::new(10);
        let second = DefId::new(11);
        let mut tree = ClassTree::new();
        tree.definitions
            .classes
            .insert("First".to_string(), long_class("First", first));
        tree.definitions
            .classes
            .insert("Second".to_string(), long_class("Second", second));
        tree.def_map.insert(first, "First".to_string());
        tree.def_map.insert(second, "Second".to_string());
        tree.name_map.insert("First".to_string(), first);
        tree.name_map.insert("Second".to_string(), second);

        let mut wire = serde_json::to_value(&tree).expect("class tree serializes");
        let types = wire["type_table"]["types"]
            .as_array_mut()
            .expect("type table wire contains a type array");
        let first_type = TypeId::new(u32::try_from(types.len()).expect("test table fits TypeId"));
        types.push(
            serde_json::to_value(Type::Class(TypeClassType {
                name: "First".to_string(),
                def_id: first,
                kind: ClassKind::Class,
            }))
            .expect("class payload serializes"),
        );
        let second_type = TypeId::new(u32::try_from(types.len()).expect("test table fits TypeId"));
        types.push(
            serde_json::to_value(Type::Class(TypeClassType {
                name: "Second".to_string(),
                def_id: first,
                kind: ClassKind::Class,
            }))
            .expect("forged class payload serializes"),
        );
        let names = wire["type_table"]["by_name"]
            .as_object_mut()
            .expect("type table wire contains a name index");
        names.insert(
            "First".to_string(),
            serde_json::to_value(first_type).expect("TypeId serializes"),
        );
        names.insert(
            "Second".to_string(),
            serde_json::to_value(second_type).expect("TypeId serializes"),
        );

        let bytes = serde_json::to_vec(&wire).expect("forged current wire serializes");
        assert!(
            serde_json::from_slice::<ClassTree>(&bytes).is_err(),
            "two unique names cannot replay one Class DefId claim",
        );
    }

    #[test]
    fn current_class_tree_wire_rejects_component_class_qualified_name_collision() {
        let parent = DefId::new(10);
        let nested = DefId::new(11);
        let component = DefId::new(12);
        let mut parent_class = long_class("Parent", parent);
        parent_class
            .classes
            .insert("Claim".to_string(), long_class("Claim", nested));
        let mut tree = ClassTree::new();
        register_class(&mut tree, "Parent", parent_class);
        tree.def_map.insert(nested, "Parent.Claim".to_string());
        tree.name_map.insert("Parent.Claim".to_string(), nested);
        issue_declaration_types(
            &mut tree,
            vec![
                (
                    parent,
                    Type::Class(TypeClassType {
                        name: "Parent".to_string(),
                        def_id: parent,
                        kind: ClassKind::Model,
                    }),
                ),
                (
                    nested,
                    Type::Class(TypeClassType {
                        name: "Parent.Claim".to_string(),
                        def_id: nested,
                        kind: ClassKind::Model,
                    }),
                ),
            ],
        );
        valid_wire(&tree);

        let mut collision_component = Component::empty_with_span(Span::from_offsets(
            rumoca_core::SourceId::from_source_name("component-class-collision.mo"),
            0,
            1,
        ));
        collision_component.def_id = Some(component);
        collision_component.name = "Claim".to_string();
        tree.definitions
            .classes
            .get_mut("Parent")
            .expect("fixture has Parent")
            .components
            .insert("Claim".to_string(), collision_component);
        tree.def_map.insert(component, "Parent.Claim".to_string());
        assert_eq!(
            tree.name_map.get("Parent.Claim"),
            Some(&nested),
            "the lossy name projection retains the nested class claim"
        );

        let error = tree
            .type_declaration_inventory()
            .expect_err("one qualified name cannot identify a component and nested class");
        assert!(
            error
                .to_string()
                .contains("qualified name is already owned"),
            "the sole inventory issuer must explain the collision: {error}"
        );
        let wire = serde_json::to_value(&tree).expect("coordinated mutation serializes");
        assert_wire_rejected(
            wire,
            "distinct component and class DefIds claiming `Parent.Claim`",
        );
    }

    #[test]
    fn current_class_tree_wire_rejects_coordinated_alias_cycle() {
        let real = DefId::new(1);
        let first = DefId::new(10);
        let second = DefId::new(11);
        let mut tree = ClassTree::new();
        register_predefined(&mut tree, "Real", real);
        register_class(&mut tree, "First", type_alias_class("First", first, real));
        register_class(
            &mut tree,
            "Second",
            type_alias_class("Second", second, real),
        );
        issue_declaration_types(
            &mut tree,
            vec![
                (
                    first,
                    Type::Alias(TypeAlias {
                        name: "First".to_string(),
                        aliased: TypeId::new(0),
                    }),
                ),
                (
                    second,
                    Type::Alias(TypeAlias {
                        name: "Second".to_string(),
                        aliased: TypeId::new(0),
                    }),
                ),
            ],
        );
        let first_type = tree.type_table.lookup("First").expect("First is issued");
        let second_type = tree.type_table.lookup("Second").expect("Second is issued");
        let mut wire = valid_wire(&tree);
        wire["definitions"]["classes"]["First"]["extends"][0]["base_def_id"] =
            serde_json::to_value(second).expect("DefId serializes");
        wire["definitions"]["classes"]["Second"]["extends"][0]["base_def_id"] =
            serde_json::to_value(first).expect("DefId serializes");
        wire["type_table"]["types"][first_type.index() as usize] =
            serde_json::to_value(Type::Alias(TypeAlias {
                name: "First".to_string(),
                aliased: second_type,
            }))
            .expect("alias serializes");
        wire["type_table"]["types"][second_type.index() as usize] =
            serde_json::to_value(Type::Alias(TypeAlias {
                name: "Second".to_string(),
                aliased: first_type,
            }))
            .expect("alias serializes");

        assert_wire_rejected(wire, "a coordinated two-alias cycle");
    }

    #[test]
    fn current_class_tree_wire_replays_aliases_to_both_predefined_enumerations() {
        let state_select = DefId::new(5);
        let assertion_level = DefId::new(6);
        let select_alias = DefId::new(10);
        let level_alias = DefId::new(11);
        let mut tree = ClassTree::new();
        register_predefined(&mut tree, "StateSelect", state_select);
        register_predefined(&mut tree, "AssertionLevel", assertion_level);
        register_class(
            &mut tree,
            "SelectAlias",
            type_alias_class("SelectAlias", select_alias, state_select),
        );
        register_class(
            &mut tree,
            "LevelAlias",
            type_alias_class("LevelAlias", level_alias, assertion_level),
        );
        issue_declaration_types(
            &mut tree,
            vec![
                (
                    select_alias,
                    Type::Alias(TypeAlias {
                        name: "SelectAlias".to_string(),
                        aliased: TypeId::new(5),
                    }),
                ),
                (
                    level_alias,
                    Type::Alias(TypeAlias {
                        name: "LevelAlias".to_string(),
                        aliased: TypeId::new(6),
                    }),
                ),
            ],
        );

        let replayed = serde_json::from_value::<ClassTree>(valid_wire(&tree))
            .expect("checked current wire replays both predefined enumeration aliases");
        for (alias_name, predefined_name) in [
            ("SelectAlias", "StateSelect"),
            ("LevelAlias", "AssertionLevel"),
        ] {
            let alias_id = replayed
                .type_table
                .lookup(alias_name)
                .unwrap_or_else(|| panic!("{alias_name} is replayed"));
            let predefined_id = replayed
                .type_table
                .lookup(predefined_name)
                .unwrap_or_else(|| panic!("{predefined_name} is replayed"));
            assert!(
                matches!(replayed.type_table.get(alias_id), Some(Type::Alias(alias)) if alias.aliased == predefined_id),
                "{alias_name} must replay the exact {predefined_name} identity",
            );
        }
    }

    #[test]
    fn current_class_tree_wire_rejects_predefined_real_identity_reuse_for_every_type_kind() {
        let real = DefId::new(1);
        for (name, class, ty) in user_type_kinds(real) {
            let declaration = class.def_id.expect("test declaration has an identity");
            let mut tree = ClassTree::new();
            register_predefined(&mut tree, "Real", real);
            register_class(&mut tree, name, class);
            issue_declaration_types(&mut tree, vec![(declaration, ty)]);
            let mut wire = valid_wire(&tree);
            wire["definitions"]["classes"][name]["def_id"] =
                serde_json::to_value(real).expect("DefId serializes");

            assert_wire_rejected(
                wire,
                &format!("a {name} declaration reusing predefined Real's DefId"),
            );
        }
    }

    #[test]
    fn current_class_tree_wire_rejects_deleted_renamed_and_ghost_declaration_indexes() {
        let real = DefId::new(1);
        let declaration = DefId::new(10);
        let mut tree = ClassTree::new();
        register_predefined(&mut tree, "Real", real);
        register_class(&mut tree, "Owned", long_class("Owned", declaration));
        issue_declaration_types(
            &mut tree,
            vec![(
                declaration,
                Type::Class(TypeClassType {
                    name: "Owned".to_string(),
                    def_id: declaration,
                    kind: ClassKind::Model,
                }),
            )],
        );

        let mut deleted = valid_wire(&tree);
        deleted["type_table"]["types"]
            .as_array_mut()
            .expect("type payloads are an array")
            .pop();
        deleted["type_table"]["by_name"]
            .as_object_mut()
            .expect("type names are an object")
            .remove("Owned");
        deleted["name_map"]
            .as_object_mut()
            .expect("declaration names are an object")
            .remove("Owned");
        assert_wire_rejected(deleted, "a class payload and name binding deletion");

        let owned_type = tree.type_table.lookup("Owned").expect("Owned is issued");
        let mut renamed = valid_wire(&tree);
        let renamed_id = renamed["name_map"]
            .as_object_mut()
            .expect("declaration names are an object")
            .remove("Owned")
            .expect("Owned declaration is indexed");
        renamed["name_map"]
            .as_object_mut()
            .expect("declaration names are an object")
            .insert("Renamed".to_string(), renamed_id);
        let renamed_type_id = renamed["type_table"]["by_name"]
            .as_object_mut()
            .expect("type names are an object")
            .remove("Owned")
            .expect("Owned type is indexed");
        renamed["type_table"]["by_name"]
            .as_object_mut()
            .expect("type names are an object")
            .insert("Renamed".to_string(), renamed_type_id);
        renamed["type_table"]["types"][owned_type.index() as usize] =
            serde_json::to_value(Type::Class(TypeClassType {
                name: "Renamed".to_string(),
                def_id: declaration,
                kind: ClassKind::Model,
            }))
            .expect("renamed class type serializes");
        assert_wire_rejected(
            renamed,
            "a renamed index and payload without a declaration rename",
        );

        let mut ghost = valid_wire(&tree);
        ghost["name_map"]
            .as_object_mut()
            .expect("declaration names are an object")
            .insert(
                "Ghost".to_string(),
                serde_json::to_value(DefId::new(999)).expect("DefId serializes"),
            );
        assert_wire_rejected(ghost, "a ghost name-map identity");
    }

    fn valid_wire(tree: &ClassTree) -> serde_json::Value {
        let wire = serde_json::to_value(tree).expect("valid class tree serializes");
        serde_json::from_value::<ClassTree>(wire.clone()).expect("unmodified current wire replays");
        wire
    }

    fn assert_wire_rejected(wire: serde_json::Value, mutation: &str) {
        let bytes = serde_json::to_vec(&wire).expect("mutated current wire serializes");
        assert!(
            serde_json::from_slice::<ClassTree>(&bytes).is_err(),
            "current wire must reject {mutation}",
        );
    }

    fn remove_def_map_claim(wire: &mut serde_json::Value, name: &str) {
        let def_map = wire["def_map"]
            .as_object_mut()
            .expect("DefId map is an object");
        let key = def_map
            .iter()
            .find_map(|(key, value)| (value == name).then(|| key.clone()))
            .unwrap_or_else(|| panic!("{name} has a DefId claim"));
        def_map.remove(&key);
    }

    fn register_predefined(tree: &mut ClassTree, name: &str, def_id: DefId) {
        tree.scope_tree
            .add_predefined_member(ComponentPath::from_flat_path(name), def_id);
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }

    fn register_class(tree: &mut ClassTree, name: &str, class: ClassDef) {
        let def_id = class.def_id.expect("test class has an identity");
        tree.definitions.classes.insert(name.to_string(), class);
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }

    fn issue_declaration_types(tree: &mut ClassTree, payloads: Vec<(DefId, Type)>) {
        complete_predefined_claims(tree);
        let inventory = tree
            .type_declaration_inventory()
            .expect("test tree owns one exact declaration inventory");
        tree.type_table
            .plan_declaration_append(payloads, inventory)
            .expect("test declarations fit the TypeId domain")
            .commit_declared(|_, payload| Ok::<_, std::convert::Infallible>(payload))
            .expect("test payloads exactly match their declarations");
    }

    fn complete_predefined_claims(tree: &mut ClassTree) {
        let entries = TypeTable::canonical_predefined_entries().collect::<Vec<_>>();
        let mut next = 1_000_u32;
        for (name, _) in entries {
            let path = ComponentPath::from_flat_path(name);
            if tree.scope_tree.predefined_member(&path).is_some() {
                continue;
            }
            while tree.def_map.contains_key(&DefId::new(next)) {
                next = next.checked_add(1).expect("test DefId space is ample");
            }
            register_predefined(tree, name, DefId::new(next));
            next = next.checked_add(1).expect("test DefId space is ample");
        }
    }

    fn type_alias_class(name: &str, def_id: DefId, base_def_id: DefId) -> ClassDef {
        let mut class = short_alias(name, def_id, Some(base_def_id));
        class.class_type = ClassType::Type;
        class
    }

    fn user_type_kinds(real: DefId) -> Vec<(&'static str, ClassDef, Type)> {
        let class_id = DefId::new(10);
        let enum_id = DefId::new(11);
        let alias_id = DefId::new(12);
        let mut enumeration = long_class("EnumerationOwned", enum_id);
        enumeration.class_type = ClassType::Type;
        enumeration.enum_literals.push(EnumLiteral {
            ident: token("only"),
            ..EnumLiteral::default()
        });
        vec![
            (
                "ClassOwned",
                long_class("ClassOwned", class_id),
                Type::Class(TypeClassType {
                    name: "ClassOwned".to_string(),
                    def_id: class_id,
                    kind: ClassKind::Model,
                }),
            ),
            (
                "EnumerationOwned",
                enumeration,
                Type::Enumeration(EnumerationType {
                    name: "EnumerationOwned".to_string(),
                    literals: vec!["only".to_string()],
                }),
            ),
            (
                "AliasOwned",
                type_alias_class("AliasOwned", alias_id, real),
                Type::Alias(TypeAlias {
                    name: "AliasOwned".to_string(),
                    aliased: TypeId::new(0),
                }),
            ),
        ]
    }

    fn token(text: &str) -> Token {
        Token {
            text: Arc::from(text),
            ..Token::default()
        }
    }

    fn long_class(name: &str, def_id: DefId) -> ClassDef {
        let name = token(name);
        ClassDef {
            def_id: Some(def_id),
            name: name.clone(),
            end_name_token: Some(name),
            ..ClassDef::default()
        }
    }

    fn short_alias(name: &str, def_id: DefId, base_def_id: Option<DefId>) -> ClassDef {
        ClassDef {
            def_id: Some(def_id),
            name: token(name),
            extends: vec![Extend {
                base_name: Name::from_string("Base"),
                base_def_id,
                ..Extend::default()
            }],
            ..ClassDef::default()
        }
    }

    fn index(classes: impl IntoIterator<Item = (String, ClassDef)>) -> ClassTree {
        let mut tree = ClassTree::new();
        tree.definitions.classes.extend(classes);
        tree
    }

    #[test]
    fn long_class_extending_a_lexical_descendant_is_nonreplaceable() {
        let modelica_id = DefId::new(91_001);
        let icons_id = DefId::new(91_002);
        let package_id = DefId::new(91_003);
        let package = long_class("Package", package_id);
        let mut icons = long_class("Icons", icons_id);
        icons.classes.insert("Package".to_string(), package);
        let mut modelica = long_class("Modelica", modelica_id);
        modelica.extends.push(Extend {
            base_name: Name::from_string("Modelica.Icons.Package"),
            base_def_id: Some(package_id),
            ..Extend::default()
        });
        modelica.classes.insert("Icons".to_string(), icons);
        let tree = index([("Modelica".to_string(), modelica)]);
        let index = ClassDefIndex::from_tree(&tree);

        assert!(index.proves_transitively_non_replaceable_path([modelica_id]));
        assert!(index.proves_transitively_non_replaceable_path([package_id]));
    }

    #[test]
    fn short_alias_to_a_nonreplaceable_reference_is_nonreplaceable() {
        let base_id = DefId::new(91_011);
        let alias_id = DefId::new(91_012);
        let tree = index([
            ("Base".to_string(), long_class("Base", base_id)),
            (
                "Alias".to_string(),
                short_alias("Alias", alias_id, Some(base_id)),
            ),
        ]);
        let index = ClassDefIndex::from_tree(&tree);

        assert!(index.proves_transitively_non_replaceable_path([alias_id]));
    }

    #[test]
    fn short_alias_to_a_replaceable_reference_is_not_nonreplaceable() {
        let base_id = DefId::new(91_021);
        let alias_id = DefId::new(91_022);
        let mut base = long_class("Base", base_id);
        base.is_replaceable = true;
        let tree = index([
            ("Base".to_string(), base),
            (
                "Alias".to_string(),
                short_alias("Alias", alias_id, Some(base_id)),
            ),
        ]);
        let index = ClassDefIndex::from_tree(&tree);

        assert!(!index.proves_transitively_non_replaceable_path([alias_id]));
    }

    #[test]
    fn unresolved_short_alias_is_not_nonreplaceable() {
        let alias_id = DefId::new(91_031);
        let tree = index([("Alias".to_string(), short_alias("Alias", alias_id, None))]);
        let index = ClassDefIndex::from_tree(&tree);

        assert!(!index.proves_transitively_non_replaceable_path([alias_id]));
    }

    #[test]
    fn short_alias_cycle_is_not_nonreplaceable() {
        let a_id = DefId::new(91_041);
        let b_id = DefId::new(91_042);
        let tree = index([
            ("A".to_string(), short_alias("A", a_id, Some(b_id))),
            ("B".to_string(), short_alias("B", b_id, Some(a_id))),
        ]);
        let index = ClassDefIndex::from_tree(&tree);

        assert!(!index.proves_transitively_non_replaceable_path([a_id]));
        assert!(!index.proves_transitively_non_replaceable_path([b_id]));
    }

    #[test]
    fn replaceable_exposure_parent_is_not_nonreplaceable() {
        let package_id = DefId::new(91_051);
        let function_id = DefId::new(91_052);
        let function = long_class("f", function_id);
        let mut package = long_class("P", package_id);
        package.is_replaceable = true;
        package.classes.insert("f".to_string(), function);
        let tree = index([("P".to_string(), package)]);
        let index = ClassDefIndex::from_tree(&tree);

        assert!(!index.proves_transitively_non_replaceable_path([function_id]));
    }
}

fn external_object_descendants(
    classes: &FxHashMap<DefId, &ClassDef>,
    external_object_def_id: DefId,
) -> FxHashSet<DefId> {
    let mut derived_by_base: FxHashMap<DefId, Vec<DefId>> = FxHashMap::default();
    for (derived_def_id, class) in classes {
        for base_def_id in class.extends.iter().filter_map(|extend| extend.base_def_id) {
            derived_by_base
                .entry(base_def_id)
                .or_default()
                .push(*derived_def_id);
        }
    }

    let mut descendants = FxHashSet::default();
    let mut pending = vec![external_object_def_id];
    while let Some(base_def_id) = pending.pop() {
        let Some(derived_def_ids) = derived_by_base.get(&base_def_id) else {
            continue;
        };
        for derived_def_id in derived_def_ids {
            if descendants.insert(*derived_def_id) {
                pending.push(*derived_def_id);
            }
        }
    }
    descendants
}

/// A ClassTree that has been parsed but not yet resolved.
///
/// At this stage:
/// - Syntax is valid
/// - `def_id`, `scope_id`, `type_id` fields are all `None`
/// - The `scope_tree` only has the global scope
/// - The `type_table` only has built-in types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParsedTree(pub ClassTree);

impl ParsedTree {
    /// Create a new ParsedTree from a ClassTree.
    pub fn new(tree: ClassTree) -> Self {
        Self(tree)
    }

    /// Get a reference to the inner ClassTree.
    pub fn inner(&self) -> &ClassTree {
        &self.0
    }

    /// Consume and return the inner ClassTree.
    pub fn into_inner(self) -> ClassTree {
        self.0
    }
}

impl std::ops::Deref for ParsedTree {
    type Target = ClassTree;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ParsedTree {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// A ClassTree that has completed type checking.
///
/// At this stage:
/// - All `def_id` fields are populated
/// - All `scope_id` fields are populated
/// - All `type_id` fields are populated
/// - The `type_table` contains all types
/// - Type constraints have been validated
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedTree(pub ClassTree);

impl TypedTree {
    /// Create a new TypedTree from a ClassTree.
    /// This should only be called by the typecheck phase.
    pub fn new(tree: ClassTree) -> Self {
        Self(tree)
    }

    /// Get a reference to the inner ClassTree.
    pub fn inner(&self) -> &ClassTree {
        &self.0
    }

    /// Consume and return the inner ClassTree.
    pub fn into_inner(self) -> ClassTree {
        self.0
    }
}

impl std::ops::Deref for TypedTree {
    type Target = ClassTree;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for TypedTree {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
