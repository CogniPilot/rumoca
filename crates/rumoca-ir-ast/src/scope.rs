//! Scope tree for name lookup (MLS §5.3).
//!
//! This module provides the ScopeTree for tracking name visibility
//! and performing name lookup during semantic analysis.

use crate::AstIndexMap as IndexMap;
use rumoca_core::{ComponentPath, DefId, ScopeId};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

fn deserialize_unique_predefined_members<'de, D>(
    deserializer: D,
) -> Result<IndexMap<ComponentPath, DefId>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::deserialize_unique_index_map(deserializer, "ScopeTree.predefined_members")
}

fn deserialize_unique_scope_members<'de, D>(
    deserializer: D,
) -> Result<IndexMap<ComponentPath, DefId>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::deserialize_unique_index_map(deserializer, "Scope.members")
}

fn deserialize_unique_inherited_members<'de, D>(
    deserializer: D,
) -> Result<IndexMap<ComponentPath, InheritedMember>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::deserialize_unique_index_map(deserializer, "Scope.inherited_members")
}

fn deserialize_unique_wildcard_names<'de, D>(
    deserializer: D,
) -> Result<IndexMap<ComponentPath, WildcardMember>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    crate::deserialize_unique_index_map(deserializer, "Import::Wildcard.names")
}

/// Effective visibility of one inherited name in a class scope.
///
/// Ambiguity is distinct from absence: lookup must stop at the class boundary
/// without selecting a declaration or falling through to an enclosing scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum InheritedMember {
    Unique(DefId),
    Ambiguous,
}

/// Closed result of lexical or exact-member lookup.
///
/// Ambiguity is not absence: Resolve must diagnose the exact use and may not
/// continue into a lower-precedence import or enclosing scope.
#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LookupOutcome {
    Found(DefId),
    Absent,
    AmbiguousInherited,
    AmbiguousUnqualifiedImport,
}

/// One package member retained by a wildcard import.
///
/// Ambiguous inherited exports stay explicit so importing the package cannot
/// make the name disappear and fall through to an enclosing declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WildcardMember {
    Unique(DefId),
    AmbiguousInherited,
}

/// MLS §5.3: Scope tree for name lookup.
///
/// The scope tree tracks the hierarchical structure of scopes and
/// provides name lookup functionality.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScopeTree {
    /// All scopes indexed by ScopeId.
    scopes: Vec<Scope>,
    /// Predefined names that remain visible at an encapsulated boundary.
    #[serde(deserialize_with = "deserialize_unique_predefined_members")]
    predefined_members: IndexMap<ComponentPath, DefId>,
    /// Required current lookup/import wire contract.
    lookup_schema: LookupSchema,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
enum LookupSchema {
    Mls37DeclaredNamedWildcardIdentityPrefix,
}

/// The precedence tier that resolved one name, together with the winner.
///
/// `Scope::lookup` and the effective-import mint share this vocabulary so the
/// MLS §5.3.1 precedence order exists in exactly one place. Import winners
/// carry the identity path of the import clause that supplied them, so a
/// consumer can render a qualified reference without re-deriving any segment
/// from a spelling.
enum TieredOutcome<'a> {
    /// A declaration directly in the scope (or the predefined fallback at an
    /// encapsulated boundary).
    Declared(DefId),
    /// The unique inherited declaration visible in the scope.
    Inherited(DefId),
    /// A qualified, renamed, or selective import member.
    ImportedSingle {
        prefix: &'a [DefId],
        target: DefId,
    },
    /// A member supplied by an unqualified (wildcard) import.
    ImportedWildcard {
        prefix: &'a [DefId],
        target: DefId,
    },
    AmbiguousInherited,
    AmbiguousUnqualifiedImport,
    Absent,
}

impl TieredOutcome<'_> {
    fn to_lookup_outcome(&self) -> LookupOutcome {
        match self {
            TieredOutcome::Declared(definition) | TieredOutcome::Inherited(definition) => {
                LookupOutcome::Found(*definition)
            }
            TieredOutcome::ImportedSingle { target, .. }
            | TieredOutcome::ImportedWildcard { target, .. } => LookupOutcome::Found(*target),
            TieredOutcome::AmbiguousInherited => LookupOutcome::AmbiguousInherited,
            TieredOutcome::AmbiguousUnqualifiedImport => LookupOutcome::AmbiguousUnqualifiedImport,
            TieredOutcome::Absent => LookupOutcome::Absent,
        }
    }
}

/// Why one imported name refuses to bind (MLS §5.3.1).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportRefusal {
    /// Distinct declarations reach the name through more than one wildcard
    /// import.
    AmbiguousUnqualifiedImport,
    /// The name is ambiguous among inherited declarations, either in the
    /// importing scope or inside the imported package's export view.
    AmbiguousInherited,
}

/// One import-bound name with per-segment declaration identity.
///
/// `prefix` names the containing packages of the imported declaration and
/// `target` is the declaration itself, so a qualified reference rebuilt from
/// this binding carries an identity for every segment.
#[derive(Debug, PartialEq, Eq)]
pub struct ImportBinding {
    prefix: Arc<[DefId]>,
    target: DefId,
}

impl ImportBinding {
    /// Identities of the containing packages, outermost first.
    pub fn prefix(&self) -> &[DefId] {
        &self.prefix
    }

    /// Identity of the imported declaration.
    pub fn target(&self) -> DefId {
        self.target
    }

    /// Every segment identity of the rebuilt qualified reference, outermost
    /// first and ending with the imported declaration itself.
    pub fn segments(&self) -> impl Iterator<Item = DefId> + '_ {
        self.prefix
            .iter()
            .copied()
            .chain(std::iter::once(self.target))
    }
}

/// The lookup authority's verdict for one import-visible name.
#[derive(Debug, PartialEq, Eq)]
pub enum EffectiveImport {
    Bound(ImportBinding),
    Refused(ImportRefusal),
}

/// Every name a scope can reach through its imports, as decided by the one
/// lookup authority.
///
/// A name shadowed by a declared, inherited, or enclosing declaration is
/// absent from `bindings`: the import never takes effect, so no consumer can
/// select it. Construction is private to this module; `ClassTree::
/// effective_imports` is the sole public producer.
#[derive(Debug)]
pub struct EffectiveImports {
    scope: ScopeId,
    bindings: IndexMap<ComponentPath, EffectiveImport>,
}

impl EffectiveImports {
    /// The scope these bindings were minted for.
    pub fn scope(&self) -> ScopeId {
        self.scope
    }

    /// The verdict for one name, or `None` when the name is not supplied by
    /// an effective import in this scope.
    pub fn get(&self, name: &ComponentPath) -> Option<&EffectiveImport> {
        self.bindings.get(name)
    }

    /// Whether any import-visible name exists in this scope.
    pub fn is_empty(&self) -> bool {
        self.bindings.is_empty()
    }

    /// Every import-visible name and its verdict.
    pub fn iter(&self) -> impl Iterator<Item = (&ComponentPath, &EffectiveImport)> {
        self.bindings.iter()
    }

    /// Whether `name` is supplied by an effective import in this scope.
    pub fn mentions(&self, name: &ComponentPath) -> bool {
        self.bindings.contains_key(name)
    }
}

impl ScopeTree {
    /// Create a new scope tree with a global scope.
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope {
                kind: ScopeKind::Global,
                parent: None,
                members: IndexMap::default(),
                imports: Vec::new(),
                inherited_members: IndexMap::default(),
            }],
            predefined_members: IndexMap::default(),
            lookup_schema: LookupSchema::Mls37DeclaredNamedWildcardIdentityPrefix,
        }
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

    /// Return the exact declaration identity registered for a predefined name.
    ///
    /// Unlike ordinary global lookup, this query cannot be changed by a
    /// source declaration that shadows the same spelling.
    pub fn predefined_member(&self, name: &ComponentPath) -> Option<DefId> {
        self.predefined_members.get(name).copied()
    }

    pub(crate) fn predefined_members(
        &self,
    ) -> impl ExactSizeIterator<Item = (&ComponentPath, DefId)> {
        self.predefined_members
            .iter()
            .map(|(name, &def_id)| (name, def_id))
    }

    /// Replace every import visible directly in one source scope.
    ///
    /// Resolve constructs the complete import set for a class before publishing
    /// it. Replacing the set makes repeated inheritance/import fixed-point
    /// rounds idempotent: no consumer can observe duplicate or half-resolved
    /// import clauses.
    pub fn set_imports(&mut self, scope: ScopeId, imports: Vec<Import>) {
        if let Some(s) = self.get_mut(scope) {
            s.imports = imports;
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
    pub fn lookup(&self, scope: ScopeId, name: &ComponentPath) -> LookupOutcome {
        self.lookup_excluding(scope, name, None)
    }

    /// Look up a name, optionally excluding one declaration identity.
    ///
    /// Ordinary and excluding lookup deliberately share this implementation so
    /// MLS §5.3.1 precedence cannot drift between the two entry points.
    pub fn lookup_excluding(
        &self,
        scope: ScopeId,
        name: &ComponentPath,
        exclude: Option<DefId>,
    ) -> LookupOutcome {
        self.lookup_tiered(scope, name, exclude).to_lookup_outcome()
    }

    /// Walk the MLS §5.3.1 lookup chain, reporting the winning tier.
    ///
    /// This is the one implementation behind both ordinary lookup and the
    /// effective-import mint, so precedence cannot drift between them.
    fn lookup_tiered(
        &self,
        scope: ScopeId,
        name: &ComponentPath,
        exclude: Option<DefId>,
    ) -> TieredOutcome<'_> {
        let mut current = Some(scope);

        while let Some(scope_id) = current {
            let Some(s) = self.get(scope_id) else {
                break;
            };

            match s.lookup_tiered(name, exclude) {
                TieredOutcome::Absent => {}
                outcome => return outcome,
            }

            // MLS §5.3.1: an encapsulated boundary exposes predefined names,
            // but neither enclosing scopes nor arbitrary top-level classes.
            if s.is_encapsulated() {
                return self
                    .predefined_members
                    .get(name)
                    .copied()
                    .filter(|definition| Some(*definition) != exclude)
                    .map_or(TieredOutcome::Absent, TieredOutcome::Declared);
            }

            current = self.next_lookup_scope(scope_id, s);
        }

        TieredOutcome::Absent
    }

    /// Mint the effective import bindings of one scope.
    ///
    /// Candidate names are collected from every import clause along the
    /// lookup chain, erasing clause order structurally. Each name is then
    /// decided by the one lookup authority: an import-tier winner binds with
    /// its full identity path, ambiguity is refused explicitly, and a name
    /// whose lookup selects a declared, inherited, or enclosing declaration
    /// is excluded because the import never takes effect for it.
    pub(crate) fn effective_imports(&self, scope: ScopeId) -> EffectiveImports {
        let mut names: IndexMap<ComponentPath, ()> = IndexMap::default();
        let mut current = Some(scope);
        while let Some(scope_id) = current {
            let Some(s) = self.get(scope_id) else {
                break;
            };
            for import in &s.imports {
                import.collect_imported_names(&mut names);
            }
            if s.is_encapsulated() {
                break;
            }
            current = self.next_lookup_scope(scope_id, s);
        }

        let mut bindings: IndexMap<ComponentPath, EffectiveImport> = IndexMap::default();
        for (name, ()) in names {
            let verdict = match self.lookup_tiered(scope, &name, None) {
                TieredOutcome::ImportedSingle { prefix, target }
                | TieredOutcome::ImportedWildcard { prefix, target } => {
                    EffectiveImport::Bound(ImportBinding {
                        prefix: Arc::from(prefix),
                        target,
                    })
                }
                TieredOutcome::AmbiguousUnqualifiedImport => {
                    EffectiveImport::Refused(ImportRefusal::AmbiguousUnqualifiedImport)
                }
                TieredOutcome::AmbiguousInherited => {
                    EffectiveImport::Refused(ImportRefusal::AmbiguousInherited)
                }
                // The import is shadowed by a declaration (or nothing binds
                // the name at all): it never takes effect for this name.
                TieredOutcome::Declared(_)
                | TieredOutcome::Inherited(_)
                | TieredOutcome::Absent => {
                    continue;
                }
            };
            bindings.insert(name, verdict);
        }
        EffectiveImports { scope, bindings }
    }

    /// Look up a name only in the given scope (no parent search).
    pub fn lookup_local(&self, scope: ScopeId, name: &ComponentPath) -> Option<DefId> {
        self.get(scope).and_then(|s| s.members.get(name).copied())
    }

    /// Look up one member within exactly one class scope.
    ///
    /// Qualified-name traversal uses this operation after the container has
    /// already been resolved to a `DefId`. It must not walk to a parent scope:
    /// an absent `A.b` cannot resolve to an unrelated `b` enclosing `A`.
    pub fn lookup_member(&self, scope: ScopeId, name: &ComponentPath) -> LookupOutcome {
        let Some(scope) = self.get(scope) else {
            return LookupOutcome::Absent;
        };
        if let Some(def_id) = scope.members.get(name) {
            return LookupOutcome::Found(*def_id);
        }
        match scope.inherited_members.get(name) {
            Some(InheritedMember::Unique(def_id)) => LookupOutcome::Found(*def_id),
            Some(InheritedMember::Ambiguous) => LookupOutcome::AmbiguousInherited,
            None => LookupOutcome::Absent,
        }
    }

    /// Construct the effective direct-and-inherited member view used by one
    /// wildcard import.
    ///
    /// Direct declarations hide inherited declarations of the same name;
    /// ambiguous inherited names remain explicit refusal states. Imports are
    /// lexical conveniences and are not re-exported as package members
    /// (MLS §13.2.2).
    pub fn importable_members(&self, scope: ScopeId) -> IndexMap<ComponentPath, WildcardMember> {
        let Some(scope) = self.get(scope) else {
            return IndexMap::default();
        };
        let mut members = scope
            .inherited_members
            .iter()
            .map(|(name, member)| match member {
                InheritedMember::Unique(def_id) => (name.clone(), WildcardMember::Unique(*def_id)),
                InheritedMember::Ambiguous => (name.clone(), WildcardMember::AmbiguousInherited),
            })
            .collect::<IndexMap<_, _>>();
        members.extend(
            scope
                .members
                .iter()
                .map(|(name, def_id)| (name.clone(), WildcardMember::Unique(*def_id))),
        );
        members
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

impl Default for ScopeTree {
    fn default() -> Self {
        Self::new()
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
    #[serde(deserialize_with = "deserialize_unique_scope_members")]
    pub members: IndexMap<ComponentPath, DefId>,
    /// Imports in this scope (MLS §13.2).
    pub imports: Vec<Import>,
    /// Effective members contributed by resolved extends clauses.
    ///
    /// `InheritedMember::Ambiguous` records an ambiguous inherited name
    /// without choosing one base.
    #[serde(deserialize_with = "deserialize_unique_inherited_members")]
    pub inherited_members: IndexMap<ComponentPath, InheritedMember>,
}

impl Scope {
    /// Apply the MLS §5.3.1 precedence steps owned by exactly one scope.
    fn lookup_tiered(&self, name: &ComponentPath, exclude: Option<DefId>) -> TieredOutcome<'_> {
        if let Some(definition) = self
            .members
            .get(name)
            .copied()
            .filter(|definition| Some(*definition) != exclude)
        {
            return TieredOutcome::Declared(definition);
        }

        match self.inherited_members.get(name) {
            Some(InheritedMember::Unique(definition)) if Some(*definition) != exclude => {
                return TieredOutcome::Inherited(*definition);
            }
            Some(InheritedMember::Ambiguous) => {
                return TieredOutcome::AmbiguousInherited;
            }
            Some(InheritedMember::Unique(_)) | None => {}
        }

        if let Some((prefix, target)) = self
            .imports
            .iter()
            .filter_map(|import| import.resolves_single_definition(name))
            .find(|(_, definition)| Some(*definition) != exclude)
        {
            return TieredOutcome::ImportedSingle { prefix, target };
        }

        let mut found: Option<(&[DefId], DefId)> = None;
        for (prefix, member) in self
            .imports
            .iter()
            .filter_map(|import| import.resolves_wildcard(name))
        {
            match member {
                WildcardMember::Unique(definition) if Some(definition) == exclude => {}
                WildcardMember::Unique(definition) => match found {
                    None => found = Some((prefix, definition)),
                    // MLS §5.3.1 refuses a name "found in more than one
                    // package" through unqualified imports. Two wildcard
                    // routes to the same declaration select one definition,
                    // so no wrong selection is possible and the name binds.
                    Some((_, existing)) if existing == definition => {}
                    Some(_) => return TieredOutcome::AmbiguousUnqualifiedImport,
                },
                WildcardMember::AmbiguousInherited => {
                    return TieredOutcome::AmbiguousInherited;
                }
            }
        }
        found.map_or(TieredOutcome::Absent, |(prefix, target)| {
            TieredOutcome::ImportedWildcard { prefix, target }
        })
    }

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

#[cfg(test)]
mod wire_tests {
    use super::*;
    use crate::RepeatedMap;
    use serde::ser::{SerializeStruct, SerializeStructVariant};

    enum RepeatedScopeField<'a> {
        Members(RepeatedMap<'a, ComponentPath, DefId>),
        Inherited(RepeatedMap<'a, ComponentPath, InheritedMember>),
        Wildcard(&'a RepeatedWildcardImport<'a>),
    }

    struct RepeatedScope<'a> {
        scope: &'a Scope,
        repeated: RepeatedScopeField<'a>,
    }

    impl Serialize for RepeatedScope<'_> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            let mut scope = serializer.serialize_struct("Scope", 5)?;
            scope.serialize_field("kind", &self.scope.kind)?;
            scope.serialize_field("parent", &self.scope.parent)?;
            match &self.repeated {
                RepeatedScopeField::Members(repeated) => {
                    scope.serialize_field("members", repeated)?;
                    scope.serialize_field("imports", &self.scope.imports)?;
                    scope.serialize_field("inherited_members", &self.scope.inherited_members)?;
                }
                RepeatedScopeField::Inherited(repeated) => {
                    scope.serialize_field("members", &self.scope.members)?;
                    scope.serialize_field("imports", &self.scope.imports)?;
                    scope.serialize_field("inherited_members", repeated)?;
                }
                RepeatedScopeField::Wildcard(repeated) => {
                    scope.serialize_field("members", &self.scope.members)?;
                    scope.serialize_field("imports", std::slice::from_ref(*repeated))?;
                    scope.serialize_field("inherited_members", &self.scope.inherited_members)?;
                }
            }
            scope.end()
        }
    }

    struct RepeatedWildcardImport<'a> {
        prefix: &'a [DefId],
        names: RepeatedMap<'a, ComponentPath, WildcardMember>,
    }

    impl Serialize for RepeatedWildcardImport<'_> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            let mut import = serializer.serialize_struct_variant("Import", 1, "Wildcard", 2)?;
            import.serialize_field("prefix", self.prefix)?;
            import.serialize_field("names", &self.names)?;
            import.end()
        }
    }

    enum RepeatedScopeTreeField<'a> {
        Predefined(RepeatedMap<'a, ComponentPath, DefId>),
        Scope(&'a RepeatedScope<'a>),
    }

    struct RepeatedScopeTree<'a> {
        tree: &'a ScopeTree,
        repeated: RepeatedScopeTreeField<'a>,
    }

    impl Serialize for RepeatedScopeTree<'_> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            let mut tree = serializer.serialize_struct("ScopeTree", 3)?;
            match &self.repeated {
                RepeatedScopeTreeField::Predefined(repeated) => {
                    tree.serialize_field("scopes", &self.tree.scopes)?;
                    tree.serialize_field("predefined_members", repeated)?;
                }
                RepeatedScopeTreeField::Scope(repeated) => {
                    tree.serialize_field("scopes", std::slice::from_ref(*repeated))?;
                    tree.serialize_field("predefined_members", &self.tree.predefined_members)?;
                }
            }
            tree.serialize_field("lookup_schema", &self.tree.lookup_schema)?;
            tree.end()
        }
    }

    fn assert_repeated_scope_key_rejected<T: Serialize>(wire: &T, field: &str) {
        let json = serde_json::to_vec(wire).expect("repeated-key scope JSON serializes");
        let json_error = serde_json::from_slice::<ScopeTree>(&json)
            .expect_err("current JSON ScopeTree wire must reject a repeated map key");
        assert!(
            json_error.to_string().contains(field),
            "JSON refusal must identify {field}: {json_error}",
        );

        let binary = bincode::serialize(wire).expect("repeated-key scope binary serializes");
        let binary_error = bincode::deserialize::<ScopeTree>(&binary)
            .expect_err("current binary ScopeTree wire must reject a repeated map key");
        assert!(
            binary_error.to_string().contains(field),
            "binary refusal must identify {field}: {binary_error}",
        );
    }

    #[derive(Serialize)]
    struct LegacyScopeTree {
        scopes: Vec<LegacyScope>,
        predefined_members: IndexMap<ComponentPath, DefId>,
        lookup_schema: LegacyLookupSchema,
    }

    #[derive(Serialize)]
    enum LegacyLookupSchema {
        /// Pre-cutover schema whose imports carried rendered string paths.
        Mls37DeclaredNamedWildcard,
    }

    #[derive(Serialize)]
    struct LegacyScope {
        kind: ScopeKind,
        parent: Option<ScopeId>,
        members: IndexMap<ComponentPath, DefId>,
        imports: Vec<LegacyImport>,
        inherited_members: IndexMap<ComponentPath, InheritedMember>,
    }

    #[derive(Serialize)]
    enum LegacyImport {
        SingleDefinition {
            name: ComponentPath,
            path: Vec<String>,
            def_id: DefId,
        },
        Unqualified {
            path: Vec<String>,
            names: IndexMap<ComponentPath, DefId>,
        },
    }

    #[test]
    fn current_scope_wire_rejects_deleted_resolution_keys() {
        let complete = serde_json::to_value(ScopeTree::new()).expect("scope tree serializes");
        assert!(complete["predefined_members"].is_object());
        assert!(complete["scopes"][0]["inherited_members"].is_object());
        assert_eq!(
            complete["lookup_schema"],
            serde_json::json!("Mls37DeclaredNamedWildcardIdentityPrefix")
        );

        let mut missing_predefined = complete.clone();
        missing_predefined
            .as_object_mut()
            .expect("scope tree is an object")
            .remove("predefined_members")
            .expect("scope tree writes predefined members");
        assert!(serde_json::from_value::<ScopeTree>(missing_predefined).is_err());

        let mut missing_inherited = complete.clone();
        missing_inherited["scopes"][0]
            .as_object_mut()
            .expect("scope is an object")
            .remove("inherited_members")
            .expect("scope writes inherited members");
        assert!(serde_json::from_value::<ScopeTree>(missing_inherited).is_err());

        let mut missing_schema = complete;
        missing_schema
            .as_object_mut()
            .expect("scope tree is an object")
            .remove("lookup_schema")
            .expect("scope tree writes the lookup schema");
        assert!(serde_json::from_value::<ScopeTree>(missing_schema).is_err());
    }

    #[test]
    fn current_scope_wire_rejects_repeated_predefined_key_in_either_order() {
        let mut tree = ScopeTree::new();
        let name = ComponentPath::from_flat_path("Real");
        let canonical = DefId(7);
        let forged = DefId(8);
        tree.predefined_members.insert(name.clone(), canonical);
        for canonical_first in [true, false] {
            assert_repeated_scope_key_rejected(
                &RepeatedScopeTree {
                    tree: &tree,
                    repeated: RepeatedScopeTreeField::Predefined(RepeatedMap {
                        entries: &tree.predefined_members,
                        repeated_key: &name,
                        forged_value: &forged,
                        canonical_first,
                    }),
                },
                "ScopeTree.predefined_members",
            );
        }
    }

    #[test]
    fn current_scope_wire_rejects_repeated_declared_member_in_either_order() {
        let mut tree = ScopeTree::new();
        let name = ComponentPath::from_flat_path("x");
        let forged = DefId(8);
        tree.scopes[0].members.insert(name.clone(), DefId(7));
        for canonical_first in [true, false] {
            let repeated_scope = RepeatedScope {
                scope: &tree.scopes[0],
                repeated: RepeatedScopeField::Members(RepeatedMap {
                    entries: &tree.scopes[0].members,
                    repeated_key: &name,
                    forged_value: &forged,
                    canonical_first,
                }),
            };
            assert_repeated_scope_key_rejected(
                &RepeatedScopeTree {
                    tree: &tree,
                    repeated: RepeatedScopeTreeField::Scope(&repeated_scope),
                },
                "Scope.members",
            );
        }
    }

    #[test]
    fn current_scope_wire_rejects_repeated_inherited_member_in_either_order() {
        let mut tree = ScopeTree::new();
        let name = ComponentPath::from_flat_path("x");
        let forged = InheritedMember::Unique(DefId(8));
        tree.scopes[0]
            .inherited_members
            .insert(name.clone(), InheritedMember::Unique(DefId(7)));
        for canonical_first in [true, false] {
            let repeated_scope = RepeatedScope {
                scope: &tree.scopes[0],
                repeated: RepeatedScopeField::Inherited(RepeatedMap {
                    entries: &tree.scopes[0].inherited_members,
                    repeated_key: &name,
                    forged_value: &forged,
                    canonical_first,
                }),
            };
            assert_repeated_scope_key_rejected(
                &RepeatedScopeTree {
                    tree: &tree,
                    repeated: RepeatedScopeTreeField::Scope(&repeated_scope),
                },
                "Scope.inherited_members",
            );
        }
    }

    #[test]
    fn current_scope_wire_rejects_repeated_wildcard_name_in_either_order() {
        let mut tree = ScopeTree::new();
        let name = ComponentPath::from_flat_path("x");
        let forged = WildcardMember::Unique(DefId(8));
        tree.scopes[0].imports.push(Import::Wildcard {
            prefix: vec![DefId(6)],
            names: IndexMap::from_iter([(name.clone(), WildcardMember::Unique(DefId(7)))]),
        });
        let Import::Wildcard { prefix, names } = &tree.scopes[0].imports[0] else {
            panic!("test constructed a wildcard import");
        };
        for canonical_first in [true, false] {
            let repeated_import = RepeatedWildcardImport {
                prefix,
                names: RepeatedMap {
                    entries: names,
                    repeated_key: &name,
                    forged_value: &forged,
                    canonical_first,
                },
            };
            let repeated_scope = RepeatedScope {
                scope: &tree.scopes[0],
                repeated: RepeatedScopeField::Wildcard(&repeated_import),
            };
            assert_repeated_scope_key_rejected(
                &RepeatedScopeTree {
                    tree: &tree,
                    repeated: RepeatedScopeTreeField::Scope(&repeated_scope),
                },
                "Import::Wildcard.names",
            );
        }
    }

    #[test]
    fn current_scope_wire_preserves_unique_map_insertion_order() {
        let mut tree = ScopeTree::new();
        tree.predefined_members
            .insert(ComponentPath::from_flat_path("Second"), DefId(2));
        tree.predefined_members
            .insert(ComponentPath::from_flat_path("First"), DefId(1));
        tree.scopes[0]
            .members
            .insert(ComponentPath::from_flat_path("b"), DefId(4));
        tree.scopes[0]
            .members
            .insert(ComponentPath::from_flat_path("a"), DefId(3));

        let json = serde_json::to_vec(&tree).expect("current ScopeTree JSON serializes");
        let replayed = serde_json::from_slice::<ScopeTree>(&json)
            .expect("unique ordered ScopeTree JSON replays");
        assert_eq!(
            serde_json::to_vec(&replayed).expect("replayed ScopeTree JSON serializes"),
            json,
        );

        let binary = bincode::serialize(&tree).expect("current ScopeTree binary serializes");
        let replayed = bincode::deserialize::<ScopeTree>(&binary)
            .expect("unique ordered ScopeTree binary replays");
        assert_eq!(
            bincode::serialize(&replayed).expect("replayed ScopeTree binary serializes"),
            binary,
        );
    }

    #[test]
    fn pre_cutover_scope_import_wire_is_rejected() {
        let name = ComponentPath::from_flat_path("x");
        let legacy = LegacyScopeTree {
            scopes: vec![LegacyScope {
                kind: ScopeKind::Global,
                parent: None,
                members: IndexMap::default(),
                imports: vec![LegacyImport::Unqualified {
                    path: vec!["P".to_string()],
                    names: IndexMap::from_iter([(name, DefId(7))]),
                }],
                inherited_members: IndexMap::default(),
            }],
            predefined_members: IndexMap::default(),
            lookup_schema: LegacyLookupSchema::Mls37DeclaredNamedWildcard,
        };

        let json = serde_json::to_value(&legacy).expect("legacy scope JSON serializes");
        // The pre-cutover wire is rejected by its schema name.
        assert_eq!(
            json["lookup_schema"],
            serde_json::json!("Mls37DeclaredNamedWildcard")
        );
        assert!(serde_json::from_value::<ScopeTree>(json).is_err());

        let bytes = bincode::serialize(&legacy).expect("legacy scope bincode serializes");
        assert!(bincode::deserialize::<ScopeTree>(&bytes).is_err());
    }

    #[test]
    fn string_path_import_wire_is_rejected() {
        // A scope import that spells its source path as rendered strings is a
        // pre-cutover encoding: the current wire requires identity prefixes.
        let legacy = LegacyImport::SingleDefinition {
            name: ComponentPath::from_flat_path("x"),
            path: vec!["P".to_string(), "x".to_string()],
            def_id: DefId(7),
        };
        let json = serde_json::to_value(&legacy).expect("legacy import JSON serializes");
        assert!(json["SingleDefinition"]["path"].is_array());
        assert!(serde_json::from_value::<Import>(json).is_err());
    }
}

/// MLS §13.2: Import clause.
///
/// Every path segment carries the declaration identity Resolve proved for it,
/// so no consumer ever re-derives an identity from a rendered spelling.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Import {
    /// One qualified import name.
    ///
    /// This represents an ordinary qualified import, a renamed import, or one
    /// member of a selective import. MLS §13.2.2 gives all three the same
    /// lookup precedence.
    SingleDefinition {
        /// Name introduced into the importing scope.
        name: ComponentPath,
        /// Identities of the packages containing the imported definition,
        /// outermost first. Empty for a single-segment import.
        prefix: Vec<DefId>,
        /// The definition being imported.
        def_id: DefId,
    },
    /// Unqualified import: `import A.B.*;`
    ///
    /// Visibility filtering is not represented by this lookup-order view.
    Wildcard {
        /// Identities of the imported package path, outermost first and
        /// ending with the package itself.
        prefix: Vec<DefId>,
        /// All names imported from the package.
        #[serde(deserialize_with = "deserialize_unique_wildcard_names")]
        names: IndexMap<ComponentPath, WildcardMember>,
    },
}

impl Import {
    fn resolves_single_definition(&self, name: &ComponentPath) -> Option<(&[DefId], DefId)> {
        match self {
            Import::SingleDefinition {
                name: imported_name,
                prefix,
                def_id,
            } => {
                if imported_name == name {
                    Some((prefix.as_slice(), *def_id))
                } else {
                    None
                }
            }
            Import::Wildcard { .. } => None,
        }
    }

    /// Record every name this import clause can introduce into its scope.
    fn collect_imported_names(&self, names: &mut IndexMap<ComponentPath, ()>) {
        match self {
            Import::SingleDefinition { name, .. } => {
                names.entry(name.clone()).or_insert(());
            }
            Import::Wildcard { names: members, .. } => {
                for name in members.keys() {
                    names.entry(name.clone()).or_insert(());
                }
            }
        }
    }

    fn resolves_wildcard(&self, name: &ComponentPath) -> Option<(&[DefId], WildcardMember)> {
        match self {
            Import::Wildcard { prefix, names } => names
                .get(name)
                .copied()
                .map(|member| (prefix.as_slice(), member)),
            Import::SingleDefinition { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn name(value: &str) -> ComponentPath {
        ComponentPath::from_flat_path(value)
    }

    /// DefId of the fixture package `P` used by the import helpers.
    const PACKAGE_P: DefId = DefId(100);

    fn single(imported_name: &str, definition: DefId) -> Import {
        Import::SingleDefinition {
            name: name(imported_name),
            prefix: vec![PACKAGE_P],
            def_id: definition,
        }
    }

    fn wildcard(imported_name: &str, member: WildcardMember) -> Import {
        wildcard_from(PACKAGE_P, imported_name, member)
    }

    fn wildcard_from(package: DefId, imported_name: &str, member: WildcardMember) -> Import {
        Import::Wildcard {
            prefix: vec![package],
            names: IndexMap::from_iter([(name(imported_name), member)]),
        }
    }

    #[test]
    fn predefined_member_identity_is_not_replaced_by_global_shadowing() {
        let mut tree = ScopeTree::new();
        let name = ComponentPath::from_flat_path("ExternalObject");
        let predefined = DefId(1);
        let shadow = DefId(99);
        tree.add_predefined_member(name.clone(), predefined);
        tree.add_member(ScopeId::GLOBAL, name.clone(), shadow);

        assert_eq!(
            tree.lookup(ScopeId::GLOBAL, &name),
            LookupOutcome::Found(shadow)
        );
        assert_eq!(tree.predefined_member(&name), Some(predefined));
    }

    #[test]
    fn declared_and_inherited_names_precede_imports() {
        let mut tree = ScopeTree::new();
        let scope = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);
        let x = name("x");
        let direct = DefId(10);
        tree.add_member(scope, x.clone(), direct);
        tree.set_inherited_members(
            scope,
            IndexMap::from_iter([(x.clone(), InheritedMember::Ambiguous)]),
        );
        tree.set_imports(
            scope,
            vec![
                wildcard("x", WildcardMember::Unique(DefId(20))),
                single("x", DefId(30)),
            ],
        );
        assert_eq!(tree.lookup(scope, &x), LookupOutcome::Found(direct));

        let inherited = DefId(40);
        tree.get_mut(scope).expect("scope exists").members.clear();
        tree.set_inherited_members(
            scope,
            IndexMap::from_iter([(x.clone(), InheritedMember::Unique(inherited))]),
        );
        assert_eq!(tree.lookup(scope, &x), LookupOutcome::Found(inherited));
    }

    #[test]
    fn excluding_direct_reveals_inherited_state() {
        let mut tree = ScopeTree::new();
        let scope = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);
        let x = name("x");
        let direct = DefId(10);
        let inherited = DefId(20);
        tree.add_member(scope, x.clone(), direct);
        tree.set_inherited_members(
            scope,
            IndexMap::from_iter([(x.clone(), InheritedMember::Unique(inherited))]),
        );
        assert_eq!(
            tree.lookup_excluding(scope, &x, Some(direct)),
            LookupOutcome::Found(inherited)
        );

        tree.set_inherited_members(
            scope,
            IndexMap::from_iter([(x.clone(), InheritedMember::Ambiguous)]),
        );
        assert_eq!(
            tree.lookup_excluding(scope, &x, Some(direct)),
            LookupOutcome::AmbiguousInherited
        );
    }

    #[test]
    fn exclusion_continues_within_each_import_tier() {
        let x = name("x");
        let excluded = DefId(20);
        let remaining = DefId(30);
        let mut tree = ScopeTree::new();
        let scope = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);

        tree.set_imports(scope, vec![single("x", excluded), single("x", remaining)]);
        assert_eq!(
            tree.lookup_excluding(scope, &x, Some(excluded)),
            LookupOutcome::Found(remaining)
        );

        tree.set_imports(
            scope,
            vec![
                wildcard("x", WildcardMember::Unique(excluded)),
                wildcard("x", WildcardMember::Unique(remaining)),
            ],
        );
        assert_eq!(
            tree.lookup_excluding(scope, &x, Some(excluded)),
            LookupOutcome::Found(remaining)
        );
    }

    #[test]
    fn encapsulated_scope_uses_only_local_rules_and_predefined_fallback() {
        let x = name("x");
        let real = name("Real");
        let mut tree = ScopeTree::new();
        let enclosing = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);
        let encapsulated = tree.create_scope(enclosing, ScopeKind::Encapsulated);
        tree.add_member(enclosing, x.clone(), DefId(90));
        tree.add_predefined_member(real.clone(), DefId(1));

        assert_eq!(tree.lookup(encapsulated, &x), LookupOutcome::Absent);
        assert_eq!(
            tree.lookup(encapsulated, &real),
            LookupOutcome::Found(DefId(1))
        );
    }

    #[test]
    fn single_definition_import_precedes_wildcard_in_both_source_orders() {
        let x = name("x");
        for imports in [
            vec![
                wildcard("x", WildcardMember::Unique(DefId(20))),
                single("x", DefId(30)),
            ],
            vec![
                single("x", DefId(30)),
                wildcard("x", WildcardMember::Unique(DefId(20))),
            ],
        ] {
            let mut tree = ScopeTree::new();
            let scope = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);
            tree.set_imports(scope, imports);
            assert_eq!(tree.lookup(scope, &x), LookupOutcome::Found(DefId(30)));
        }
    }

    #[test]
    fn same_identity_via_two_wildcards_binds() {
        // MLS §5.3.1 refuses a name "found in more than one package" through
        // unqualified imports; two routes to one declaration select the same
        // definition, so no wrong selection is possible and the name binds.
        let mut tree = ScopeTree::new();
        let scope = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);
        let x = name("x");
        tree.set_imports(
            scope,
            vec![
                wildcard_from(DefId(100), "x", WildcardMember::Unique(DefId(20))),
                wildcard_from(DefId(101), "x", WildcardMember::Unique(DefId(20))),
            ],
        );
        assert_eq!(tree.lookup(scope, &x), LookupOutcome::Found(DefId(20)));

        let effective = tree.effective_imports(scope);
        let Some(EffectiveImport::Bound(binding)) = effective.get(&x) else {
            panic!("same-identity wildcard routes must bind, got {effective:?}");
        };
        assert_eq!(binding.target(), DefId(20));
        // The first clause's route names the rendered prefix deterministically.
        assert_eq!(binding.prefix(), &[DefId(100)]);
    }

    #[test]
    fn distinct_identities_via_two_wildcards_are_refused_not_dropped() {
        let mut tree = ScopeTree::new();
        let scope = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);
        let x = name("x");
        // An enclosing declaration must not be reachable through the refusal.
        tree.add_member(ScopeId::GLOBAL, x.clone(), DefId(90));
        tree.set_imports(
            scope,
            vec![
                wildcard_from(DefId(100), "x", WildcardMember::Unique(DefId(20))),
                wildcard_from(DefId(101), "x", WildcardMember::Unique(DefId(30))),
            ],
        );
        assert_eq!(
            tree.lookup(scope, &x),
            LookupOutcome::AmbiguousUnqualifiedImport
        );
        assert_eq!(
            tree.effective_imports(scope).get(&x),
            Some(&EffectiveImport::Refused(
                ImportRefusal::AmbiguousUnqualifiedImport
            ))
        );
    }

    #[test]
    fn declared_and_inherited_members_shadow_imports_in_effective_bindings() {
        let mut tree = ScopeTree::new();
        let scope = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);
        let x = name("x");
        tree.set_imports(scope, vec![single("x", DefId(30))]);
        tree.add_member(scope, x.clone(), DefId(10));
        assert_eq!(
            tree.effective_imports(scope).get(&x),
            None,
            "a declared member shadows the import, so the binding is excluded"
        );

        tree.get_mut(scope).expect("scope exists").members.clear();
        tree.set_inherited_members(
            scope,
            IndexMap::from_iter([(x.clone(), InheritedMember::Unique(DefId(40)))]),
        );
        assert_eq!(
            tree.effective_imports(scope).get(&x),
            None,
            "an inherited member shadows the import, so the binding is excluded"
        );
    }

    #[test]
    fn enclosing_declaration_between_scopes_shadows_outer_import() {
        // C ⊂ P ⊂ Q with Q importing X and P declaring X: MLS §5.3.1 walks
        // C, then P where the declaration wins, so Q's import never takes
        // effect for C. A per-scope tier fold that keeps the first import
        // seen would wrongly select Q's import.
        let mut tree = ScopeTree::new();
        let q = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Package);
        let p = tree.create_scope(q, ScopeKind::Package);
        let c = tree.create_scope(p, ScopeKind::Class);
        let x = name("X");
        tree.set_imports(q, vec![single("X", DefId(20))]);
        tree.add_member(p, x.clone(), DefId(10));

        assert_eq!(tree.lookup(c, &x), LookupOutcome::Found(DefId(10)));
        assert_eq!(
            tree.effective_imports(c).get(&x),
            None,
            "the enclosing declaration shadows the outer import"
        );
        // From inside Q itself the import is effective.
        let q_effective = tree.effective_imports(q);
        let Some(EffectiveImport::Bound(binding)) = q_effective.get(&x) else {
            panic!("Q's own import must bind");
        };
        assert_eq!(binding.target(), DefId(20));
    }

    #[test]
    fn encapsulation_barrier_hides_enclosing_imports() {
        let mut tree = ScopeTree::new();
        let enclosing = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);
        let encapsulated = tree.create_scope(enclosing, ScopeKind::Encapsulated);
        let x = name("x");
        tree.set_imports(enclosing, vec![single("x", DefId(30))]);

        assert_eq!(tree.lookup(encapsulated, &x), LookupOutcome::Absent);
        assert!(
            tree.effective_imports(encapsulated).is_empty(),
            "imports beyond the encapsulation barrier must not bind"
        );
    }

    #[test]
    fn imports_are_not_inherited_across_extends() {
        // MLS §13.2: imports are local to the class containing them. A base
        // class's imports reach a derived scope neither through the inherited
        // member view (importable_members already excludes them) nor through
        // the effective-import mint, whose candidates come only from the
        // lexical scope chain.
        let mut tree = ScopeTree::new();
        let base = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);
        let derived = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);
        let x = name("x");
        tree.set_imports(base, vec![single("x", DefId(30))]);
        // Model extends: the derived scope sees the base's importable members.
        let inherited = tree
            .importable_members(base)
            .into_iter()
            .map(|(name, member)| match member {
                WildcardMember::Unique(def_id) => (name, InheritedMember::Unique(def_id)),
                WildcardMember::AmbiguousInherited => (name, InheritedMember::Ambiguous),
            })
            .collect::<IndexMap<_, _>>();
        assert!(
            inherited.is_empty(),
            "importable members must not re-export the base's imports"
        );
        tree.set_inherited_members(derived, inherited);

        assert_eq!(tree.lookup(derived, &x), LookupOutcome::Absent);
        assert!(tree.effective_imports(derived).is_empty());
    }

    #[test]
    fn wildcard_preserves_inherited_ambiguity_before_parent_fallback() {
        let mut tree = ScopeTree::new();
        let package = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Package);
        let importer = tree.create_scope(ScopeId::GLOBAL, ScopeKind::Class);
        let x = name("x");
        tree.add_member(ScopeId::GLOBAL, x.clone(), DefId(90));
        tree.set_inherited_members(
            package,
            IndexMap::from_iter([(x.clone(), InheritedMember::Ambiguous)]),
        );
        tree.set_imports(
            importer,
            vec![Import::Wildcard {
                prefix: vec![PACKAGE_P],
                names: tree.importable_members(package),
            }],
        );
        assert_eq!(tree.lookup(importer, &x), LookupOutcome::AmbiguousInherited);
    }
}
