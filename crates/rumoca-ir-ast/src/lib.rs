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

pub mod instance;
mod modelica;
mod nodes;
pub mod scope;
pub mod state_machines;
pub mod types;
pub mod visitor;

use indexmap::{IndexMap, IndexSet};
use rumoca_core::{
    Causality, ClassType, DefId, Location, OpBinary, OpUnary, ScopeId, Span, StateSelect, Token,
    TypeId, Variability, split_path_with_indices, visit_top_level_path_segments,
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::{fmt::Debug, fmt::Display};

pub use visitor::{
    ComponentReferenceContext, ExpressionContext, ExpressionTransformer, FunctionCallContext,
    NameContext, SubscriptContext, TypeNameContext, VisitScope, Visitor, collect_component_refs,
    contains_component_ref, contains_function_call, walk_class_def_default, walk_component_default,
    walk_component_reference_default, walk_equation_default, walk_expression_default,
    walk_extend_default, walk_statement_default,
};

pub type AstIndexMap<K, V> = IndexMap<K, V, rustc_hash::FxBuildHasher>;

pub use nodes::*;

// Re-export key types from submodules
pub use instance::{
    ClassInstanceData, ClassOverride, ClassOverrideMap, InstanceConnection, InstanceData,
    InstanceEquation, InstanceId, InstanceOverlay, InstanceStatement, InstancedTree,
    ModificationEnvironment, ModificationValue, QualifiedName,
};
pub use scope::{Import as ScopeImport, Scope, ScopeKind, ScopeTree};
pub use state_machines::{State, StateMachine, StateMachineState, StateMachines, Transition};
pub use types::{
    ArrayType, BuiltinType, ClassKind, ClassType as TypeClassType, EnumerationType, FunctionType,
    Interface, InterfaceCausality, InterfaceElement, InterfacePrefixes, InterfaceVariability, Type,
    TypeAlias, TypeTable,
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
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
    /// Source map for mapping file names to SourceIds.
    /// Populated during session build for multi-file diagnostics.
    #[serde(default)]
    pub source_map: rumoca_core::SourceMap,
}

impl ClassTree {
    /// Create a new empty class tree.
    pub fn new() -> Self {
        Self {
            definitions: StoredDefinition::default(),
            type_table: TypeTable::new(),
            scope_tree: ScopeTree::new(),
            def_map: AstIndexMap::default(),
            name_map: AstIndexMap::default(),
            source_map: rumoca_core::SourceMap::new(),
        }
    }

    /// Create a class tree from a parsed StoredDefinition.
    pub fn from_parsed(definitions: StoredDefinition) -> Self {
        Self {
            definitions,
            type_table: TypeTable::new(),
            scope_tree: ScopeTree::new(),
            def_map: AstIndexMap::default(),
            name_map: AstIndexMap::default(),
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
}

impl<'tree> ClassDefIndex<'tree> {
    pub fn from_tree(tree: &'tree ClassTree) -> Self {
        let mut index = Self {
            classes: FxHashMap::default(),
            qualified_name_def_ids: FxHashMap::default(),
            qualified_names: FxHashMap::default(),
            parent_classes: FxHashMap::default(),
            local_names: FxHashMap::default(),
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

    pub fn symbol_def_ids(&self) -> impl Iterator<Item = DefId> + '_ {
        self.local_names.keys().copied()
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

// =============================================================================
// Phase Wrappers - Newtype wrappers for type-safe phase transitions
// =============================================================================
//
// These wrappers enforce that the correct phase has been completed before
// proceeding. The underlying ClassTree is the same, but the wrappers provide
// compile-time guarantees about which fields have been populated.
//
// Standalone typecheck progression: ParsedTree -> ResolvedTree -> TypedTree.
// Production model compilation instantiates after resolve, then annotates the
// InstanceOverlay with post-instantiation type information before flattening.

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

/// A ClassTree that has completed name resolution.
///
/// At this stage:
/// - All `def_id` fields are populated
/// - All `scope_id` fields are populated
/// - The `scope_tree` is fully built
/// - `type_id` fields are still `None`
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedTree(pub ClassTree);

impl ResolvedTree {
    /// Create a new ResolvedTree from a ClassTree.
    /// This should only be called by the resolve phase.
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

impl std::ops::Deref for ResolvedTree {
    type Target = ClassTree;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ResolvedTree {
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
