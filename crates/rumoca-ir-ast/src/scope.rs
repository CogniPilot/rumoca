//! Scope tree for name lookup (MLS §5.3).
//!
//! This module provides the ScopeTree for tracking name visibility
//! and performing name lookup during semantic analysis.

use crate::AstIndexMap as IndexMap;
use rumoca_core::{ComponentPath, DefId, ScopeId};
use serde::{Deserialize, Serialize};

/// Effective visibility of one inherited name in a class scope.
///
/// Ambiguity is distinct from absence: lookup must stop at the class boundary
/// without selecting a declaration or falling through to an enclosing scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum InheritedMember {
    Unique(DefId),
    Ambiguous,
}

/// MLS §5.3: Scope tree for name lookup.
///
/// The scope tree tracks the hierarchical structure of scopes and
/// provides name lookup functionality.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ScopeTree {
    /// All scopes indexed by ScopeId.
    scopes: Vec<Scope>,
    /// Predefined names that remain visible at an encapsulated boundary.
    #[serde(default)]
    predefined_members: IndexMap<ComponentPath, DefId>,
}

impl ScopeTree {
    /// Create a new scope tree with a global scope.
    pub fn new() -> Self {
        let mut tree = Self::default();
        // Create the global scope
        tree.scopes.push(Scope {
            kind: ScopeKind::Global,
            parent: None,
            members: IndexMap::default(),
            imports: Vec::new(),
            inherited_members: IndexMap::default(),
        });
        tree
    }

    /// Get the global scope.
    pub fn global(&self) -> ScopeId {
        ScopeId::GLOBAL
    }

    /// Create a new child scope.
    pub fn create_scope(&mut self, parent: ScopeId, kind: ScopeKind) -> ScopeId {
        let id = ScopeId::new(self.scopes.len() as u32);
        self.scopes.push(Scope {
            kind,
            parent: Some(parent),
            members: IndexMap::default(),
            imports: Vec::new(),
            inherited_members: IndexMap::default(),
        });
        id
    }

    /// Get a scope by its ScopeId.
    pub fn get(&self, id: ScopeId) -> Option<&Scope> {
        self.scopes.get(id.index() as usize)
    }

    /// Get a mutable reference to a scope.
    pub fn get_mut(&mut self, id: ScopeId) -> Option<&mut Scope> {
        self.scopes.get_mut(id.index() as usize)
    }

    /// Add a member to a scope.
    pub fn add_member(&mut self, scope: ScopeId, name: ComponentPath, def_id: DefId) {
        if let Some(s) = self.get_mut(scope) {
            s.members.insert(name, def_id);
        }
    }

    /// Register a predefined name in the global scope and in the restricted
    /// fallback used at encapsulated boundaries (MLS §5.3.1).
    pub fn add_predefined_member(&mut self, name: ComponentPath, def_id: DefId) {
        self.predefined_members.insert(name.clone(), def_id);
        self.add_member(ScopeId::GLOBAL, name, def_id);
    }

    /// Add an import to a scope.
    pub fn add_import(&mut self, scope: ScopeId, import: Import) {
        if let Some(s) = self.get_mut(scope) {
            s.imports.push(import);
        }
    }

    /// Replace the effective inherited-member view for a class scope.
    ///
    /// A present `InheritedMember::Ambiguous` value records an ambiguous
    /// inherited name. Lookup stops at that scope rather than selecting an
    /// arbitrary base declaration or continuing to an unrelated enclosing
    /// declaration.
    pub fn set_inherited_members(
        &mut self,
        scope: ScopeId,
        members: IndexMap<ComponentPath, InheritedMember>,
    ) {
        if let Some(s) = self.get_mut(scope) {
            s.inherited_members = members;
        }
    }

    /// Look up a name in a scope, searching parent scopes if not found.
    ///
    /// MLS §5.3.1: Name lookup starts in the current scope and proceeds
    /// to enclosing scopes until the name is found or global scope is reached.
    pub fn lookup(&self, scope: ScopeId, name: &ComponentPath) -> Option<DefId> {
        let mut current = Some(scope);

        while let Some(scope_id) = current {
            let Some(s) = self.get(scope_id) else {
                break;
            };

            // Check direct members
            if let Some(&def_id) = s.members.get(name) {
                return Some(def_id);
            }

            // Check imports
            if let Some(def_id) = s.imports.iter().find_map(|import| import.resolves(name)) {
                return Some(def_id);
            }

            if let Some(inherited) = s.inherited_members.get(name) {
                return match inherited {
                    InheritedMember::Unique(def_id) => Some(*def_id),
                    InheritedMember::Ambiguous => None,
                };
            }

            // MLS §5.3.1: an encapsulated boundary exposes predefined names,
            // but neither enclosing scopes nor arbitrary top-level classes.
            if s.is_encapsulated() {
                return self.predefined_members.get(name).copied();
            }

            current = self.next_lookup_scope(scope_id, s);
        }

        None
    }

    /// Look up a name only in the given scope (no parent search).
    pub fn lookup_local(&self, scope: ScopeId, name: &ComponentPath) -> Option<DefId> {
        self.get(scope).and_then(|s| s.members.get(name).copied())
    }

    /// Whether `target` is declared directly in `scope`.
    ///
    /// This identity query lets post-resolution checks use `DefId` directly
    /// instead of reconstructing a member name from rendered source text.
    pub fn declares(&self, scope: ScopeId, target: DefId) -> bool {
        self.get(scope)
            .is_some_and(|scope| scope.members.values().any(|def_id| *def_id == target))
    }

    /// Whether `target` is the unique inherited declaration visible in `scope`.
    pub fn inherits_unique(&self, scope: ScopeId, target: DefId) -> bool {
        self.get(scope).is_some_and(|scope| {
            scope
                .inherited_members
                .values()
                .any(|member| *member == InheritedMember::Unique(target))
        })
    }

    /// Look up a name, optionally excluding a specific DefId from results.
    ///
    /// This is used for extends resolution where we don't want a class to find itself.
    /// If the lookup would return the excluded DefId, continue searching parent scopes.
    ///
    /// Pass `None` for `exclude` to perform a normal lookup without exclusion.
    pub fn lookup_excluding(
        &self,
        scope: ScopeId,
        name: &ComponentPath,
        exclude: Option<DefId>,
    ) -> Option<DefId> {
        let mut current = Some(scope);

        while let Some(scope_id) = current {
            let Some(s) = self.get(scope_id) else {
                break;
            };

            // Check direct members (skip if it matches the excluded DefId)
            let member_result = s
                .members
                .get(name)
                .copied()
                .filter(|&id| exclude.is_none_or(|ex| id != ex));
            if member_result.is_some() {
                return member_result;
            }

            // Check imports (skip if it matches the excluded DefId)
            let import_result = s
                .imports
                .iter()
                .find_map(|import| import.resolves(name))
                .filter(|&id| exclude.is_none_or(|ex| id != ex));
            if import_result.is_some() {
                return import_result;
            }

            if let Some(inherited) = s.inherited_members.get(name) {
                return match inherited {
                    InheritedMember::Unique(def_id) => {
                        Some(*def_id).filter(|&id| exclude.is_none_or(|ex| id != ex))
                    }
                    InheritedMember::Ambiguous => None,
                };
            }

            if s.is_encapsulated() {
                return self
                    .predefined_members
                    .get(name)
                    .copied()
                    .filter(|&id| exclude.is_none_or(|ex| id != ex));
            }

            current = self.next_lookup_scope(scope_id, s);
        }

        None
    }

    /// Get the parent scope.
    pub fn parent(&self, scope: ScopeId) -> Option<ScopeId> {
        self.get(scope).and_then(|s| s.parent)
    }

    /// Read the inherited-name state recorded directly on one scope.
    pub fn inherited_member(
        &self,
        scope: ScopeId,
        name: &ComponentPath,
    ) -> Option<InheritedMember> {
        self.get(scope)?.inherited_members.get(name).copied()
    }

    fn next_lookup_scope(&self, scope_id: ScopeId, scope: &Scope) -> Option<ScopeId> {
        debug_assert!(!scope.is_encapsulated() || scope_id == ScopeId::GLOBAL);
        scope.parent
    }

    /// Get the number of scopes.
    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    /// Check if the tree is empty.
    pub fn is_empty(&self) -> bool {
        self.scopes.is_empty()
    }
}

/// A scope in the scope tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Scope {
    /// The kind of scope.
    pub kind: ScopeKind,
    /// Parent scope (None for global scope).
    pub parent: Option<ScopeId>,
    /// Names defined in this scope.
    pub members: IndexMap<ComponentPath, DefId>,
    /// Imports in this scope (MLS §13.2).
    pub imports: Vec<Import>,
    /// Effective members contributed by resolved extends clauses.
    ///
    /// `InheritedMember::Ambiguous` records an ambiguous inherited name
    /// without choosing one base.
    #[serde(default)]
    pub inherited_members: IndexMap<ComponentPath, InheritedMember>,
}

impl Scope {
    /// Check if this scope is encapsulated (MLS §5.3.1).
    pub fn is_encapsulated(&self) -> bool {
        matches!(self.kind, ScopeKind::Encapsulated)
    }
}

/// The kind of scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ScopeKind {
    /// Global scope (top-level).
    Global,
    /// Package scope.
    Package,
    /// Class scope (model, record, connector, etc.).
    Class,
    /// Encapsulated class scope (lookup stops here except for Modelica.*).
    Encapsulated,
    /// Function scope.
    Function,
    /// For-loop scope.
    ForLoop,
}

/// MLS §13.2: Import clause.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Import {
    /// Qualified import: `import A.B.C;`
    /// Makes `C` available as `C`.
    Qualified {
        /// The full path being imported.
        path: Vec<String>,
        /// The definition being imported.
        def_id: DefId,
    },
    /// Renamed import: `import D = A.B.C;`
    /// Makes `C` available as `D`.
    Renamed {
        /// The alias name.
        alias: ComponentPath,
        /// The full path being imported.
        path: Vec<String>,
        /// The definition being imported.
        def_id: DefId,
    },
    /// Unqualified import: `import A.B.*;`
    /// Makes all public names from `A.B` available.
    Unqualified {
        /// The path to the package.
        path: Vec<String>,
        /// All names imported from the package.
        names: IndexMap<ComponentPath, DefId>,
    },
}

impl Import {
    /// Check if this import resolves the given name.
    pub fn resolves(&self, name: &ComponentPath) -> Option<DefId> {
        match self {
            Import::Qualified { path, def_id } => {
                // The last component of the path is the name
                if path.last().map(|s| s.as_str()) == Some(name.as_str()) {
                    Some(*def_id)
                } else {
                    None
                }
            }
            Import::Renamed { alias, def_id, .. } => {
                if alias == name {
                    Some(*def_id)
                } else {
                    None
                }
            }
            Import::Unqualified { names, .. } => names.get(name).copied(),
        }
    }
}
