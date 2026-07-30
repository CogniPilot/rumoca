//! Name resolution phase for the Rumoca compiler.
//!
//! This phase walks the Class Tree (AST) and:
//! 1. Assigns DefIds to all definitions (classes, components)
//! 2. Builds the ScopeTree for name lookup
//! 3. Populates the def_id and scope_id fields
//!
//! The input is a `ParsedTree` and the output is a `ResolvedTree`.
//! Both wrap the same underlying `ClassTree`, but the newtype wrappers
//! provide compile-time guarantees about which phase has been completed.
//!
//! ## Module Organization
//!
//! The resolver is split into focused modules:
//! - `errors` - Error types for name resolution
//! - `registration` - Phase 1: DefId allocation and scope creation
//! - `extends` - Phase 2a: Import and extends resolution
//! - `inherited_scopes` - Phase 2b: Effective inherited scope entries
//! - `contents` - Phase 2c: Equation, statement, expression resolution
//! - `cycles` - Phase 3: Inheritance cycle detection
//! - `lookup` - Name lookup helpers
//! - [`validation`] - Post-resolution validation (unresolved symbol detection)

mod contents;
mod cycles;
mod errors;
mod extends;
mod inherited_scopes;
mod lookup;
mod path_utils;
mod registration;
pub mod semantic_checks;
mod traversal_adapter;
pub mod validation;

pub use errors::{ResolveError, ResolveResult};
pub use validation::{UnresolvedKind, UnresolvedSymbol, ValidationResult, validate_resolution};

use rumoca_core::{
    BUILTIN_FUNCTIONS, BUILTIN_TYPES, BUILTIN_VARIABLES, ComponentPath, DefId, Diagnostic,
    Diagnostics, PrimaryLabel, ScopeId, SourceMap, Span, maybe_elapsed_ms, maybe_start_timer,
};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

type ClassTree = ast::ClassTree;
type Location = rumoca_core::Location;
type ParsedTree = ast::ParsedTree;
type ScopeTree = ast::ScopeTree;
type StoredDefinition = ast::StoredDefinition;

/// A class tree that has completed name resolution without errors.
///
/// Only this crate can construct the proof. Downstream phases may inspect the
/// resolved tree or consume it into the next phase, but cannot mint or mutate
/// a resolved artifact.
///
/// ```compile_fail
/// fn forge(tree: rumoca_ir_ast::ClassTree) -> rumoca_phase_resolve::ResolvedTree {
///     rumoca_phase_resolve::ResolvedTree::new(tree)
/// }
/// ```
///
/// ```compile_fail
/// fn mutate(
///     resolved: &mut rumoca_phase_resolve::ResolvedTree,
///     source_map: rumoca_core::SourceMap,
/// ) {
///     resolved.source_map = source_map;
/// }
/// ```
#[derive(Debug, Clone)]
pub struct ResolvedTree(ClassTree);

impl ResolvedTree {
    fn new(tree: ClassTree) -> Self {
        Self(tree)
    }

    pub fn inner(&self) -> &ClassTree {
        &self.0
    }

    pub fn into_inner(self) -> ClassTree {
        self.0
    }
}

impl std::ops::Deref for ResolvedTree {
    type Target = ClassTree;

    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

const ER098_MISSING_SOURCE_CONTEXT: &str = "ER098";

/// Convert a Location to a Span for error reporting using the source map.
fn location_to_span(loc: &Location, source_map: &SourceMap) -> Option<Span> {
    if !location_has_valid_span(loc) {
        return None;
    }
    source_map.try_span(loc.source, loc.start as usize, loc.end as usize)
}

fn location_span_or_emit(
    diagnostics: &mut Diagnostics,
    loc: &Location,
    source_map: &SourceMap,
    context: &str,
) -> Option<Span> {
    let span = location_to_span(loc, source_map);
    if span.is_none() {
        diagnostics.emit(missing_source_context_diagnostic(context, loc, source_map));
    }
    span
}

fn missing_source_context_diagnostic(
    context: &str,
    loc: &Location,
    source_map: &SourceMap,
) -> Diagnostic {
    let source = source_display_name(loc.source, source_map);
    let reason = if !location_has_valid_span(loc) {
        format!("{context} in `{source}` is missing a non-empty source location")
    } else {
        format!("source file `{source}` for {context} was not found")
    };
    Diagnostic::global_error(
        ER098_MISSING_SOURCE_CONTEXT,
        format!("missing source context: {reason}"),
    )
}

/// The file name to print for a `SourceId`.
///
/// The registered path is used whenever the source map knows it. An
/// unregistered id — exactly the case that reaches the "was not found" branch —
/// still names its file through the stable placeholder form, so SPEC_0008
/// source provenance is never reduced to an anonymous placeholder.
fn source_display_name(source: rumoca_core::SourceId, source_map: &SourceMap) -> String {
    source_map
        .name(source)
        .map(str::to_string)
        .unwrap_or_else(|| rumoca_core::placeholder_source_name(source))
}

fn location_has_valid_span(loc: &Location) -> bool {
    loc.has_source()
        && loc.start_line > 0
        && loc.start_column > 0
        && loc.end_line > 0
        && loc.end_column > 0
}

/// Statistics collected during name resolution.
///
/// These stats help verify that resolution is working correctly by tracking
/// how different types of references were resolved.
#[derive(Debug, Clone, Default)]
pub struct ResolutionStats {
    /// Types fully resolved (type_def_id set to actual type's DefId)
    pub types_fully_resolved: usize,
    /// Types partially resolved (first part found in direct scope)
    pub types_partial_direct: usize,
    /// Types partially resolved (first part found via inheritance)
    pub types_partial_inherited: usize,
    /// Types that couldn't be resolved at all
    pub types_unresolved: usize,
    /// Details of unresolved types: (type_name, location)
    pub types_unresolved_details: Vec<(String, String)>,
    /// Extends clauses fully resolved
    pub extends_resolved: usize,
    /// Extends clauses resolved via inherited member lookup
    pub extends_inherited: usize,
    /// Extends clauses that couldn't be resolved
    pub extends_unresolved: usize,
    /// Component references resolved (first part found)
    pub comp_refs_resolved: usize,
    /// Component references unresolved
    pub comp_refs_unresolved: usize,
}

impl std::fmt::Display for ResolutionStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "=== Resolution Statistics ===")?;
        writeln!(f)?;
        writeln!(f, "Type References:")?;
        writeln!(f, "  Fully resolved:      {:>6}", self.types_fully_resolved)?;
        writeln!(f, "  Partial (direct):    {:>6}", self.types_partial_direct)?;
        writeln!(
            f,
            "  Partial (inherited): {:>6}",
            self.types_partial_inherited
        )?;
        writeln!(f, "  Unresolved:          {:>6}", self.types_unresolved)?;
        let total_types = self.types_fully_resolved
            + self.types_partial_direct
            + self.types_partial_inherited
            + self.types_unresolved;
        if total_types > 0 {
            let resolved = self.types_fully_resolved
                + self.types_partial_direct
                + self.types_partial_inherited;
            writeln!(
                f,
                "  Resolution rate:     {:>5.1}%",
                100.0 * resolved as f64 / total_types as f64
            )?;
        }
        if !self.types_unresolved_details.is_empty() {
            writeln!(f, "  Unresolved types:")?;
            for (type_name, location) in &self.types_unresolved_details {
                writeln!(f, "    - '{}' at {}", type_name, location)?;
            }
        }
        writeln!(f)?;
        writeln!(f, "Extends Clauses:")?;
        writeln!(f, "  Resolved:            {:>6}", self.extends_resolved)?;
        writeln!(f, "  Via inheritance:     {:>6}", self.extends_inherited)?;
        writeln!(f, "  Unresolved:          {:>6}", self.extends_unresolved)?;
        writeln!(f)?;
        writeln!(f, "Component References:")?;
        writeln!(f, "  Resolved:            {:>6}", self.comp_refs_resolved)?;
        writeln!(f, "  Unresolved:          {:>6}", self.comp_refs_unresolved)?;
        Ok(())
    }
}

/// Name resolution context.
pub struct Resolver {
    /// Counter for generating unique DefIds.
    next_def_id: u32,
    /// The scope tree being built.
    pub(crate) scope_tree: ScopeTree,
    /// Source map for file name → SourceId resolution in diagnostics.
    pub(crate) source_map: SourceMap,
    /// Map from DefId to qualified name (e.g., "Package.Model").
    /// Transferred to ClassTree.def_map after resolution for O(1) class lookup.
    pub(crate) def_names: IndexMap<DefId, String>,
    /// Inverse map from qualified name to DefId for O(1) lookup during resolution.
    pub(crate) name_to_def: IndexMap<String, DefId>,
    /// Map from class DefId to declared class type.
    pub(crate) class_types: IndexMap<DefId, rumoca_core::ClassType>,
    /// Map from package qualified name to its direct children.
    /// Used for O(1) unqualified import resolution instead of O(n) scan.
    pub(crate) package_children: IndexMap<String, IndexMap<String, DefId>>,
    /// Collected diagnostics.
    pub(crate) diagnostics: Diagnostics,
    /// Set of class DefIds currently being resolved for extends (for direct cycle detection).
    pub(crate) resolving_extends: std::collections::HashSet<DefId>,
    /// Inheritance edges collected during resolution: (class_def_id, base_def_id, location).
    /// Used for detecting indirect cycles in Phase 3.
    pub(crate) inheritance_edges: Vec<(DefId, DefId, Location)>,
    /// Index from class DefId to its base class DefIds for O(1) lookup.
    /// Built incrementally as extends are resolved.
    pub(crate) class_to_bases: IndexMap<DefId, Vec<DefId>>,
    /// Map class scope id -> class DefId for inherited lookups from nested scopes.
    pub(crate) scope_to_class_def: std::collections::HashMap<ScopeId, DefId>,
    /// Inverse of `scope_to_class_def`: each class declaration's own scope,
    /// so enclosing-class walks traverse the scope tree instead of re-parsing
    /// qualified names.
    pub(crate) class_def_scopes: std::collections::HashMap<DefId, ScopeId>,
    /// Resolved declared type of each component declaration.
    ///
    /// This lets full component-reference resolution cross component
    /// declarations by exact identity (`pin` -> `Pin` -> `v`) without
    /// recovering a type from rendered names.
    pub(crate) component_type_def_ids: std::collections::HashMap<DefId, DefId>,
    /// Declarations whose member set can change for a concrete instance.
    ///
    /// Resolve must not certify a qualified tail after crossing one of these
    /// declarations. Instantiation owns that proof after applying redeclares.
    pub(crate) dynamic_member_root_ids: std::collections::HashSet<DefId>,
    /// DefIds that can legitimately anchor partial type resolution (replaceable roots).
    pub(crate) partial_type_root_ids: std::collections::HashSet<DefId>,
    /// Exclusive upper bound for builtin DefIds. `DefId(0)` is root/global.
    builtin_count: u32,
    /// Statistics collected during resolution.
    pub(crate) stats: ResolutionStats,
    /// Timing from the most recent core resolve pass.
    last_core_timing: ResolveCoreTiming,
}

#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug, Clone, Copy, Default)]
struct ResolveCoreTiming {
    registration_ms: u128,
    extends_ms: u128,
    contents_ms: u128,
    cycle_check_ms: u128,
}

#[cfg(target_arch = "wasm32")]
#[derive(Debug, Clone, Copy, Default)]
struct ResolveCoreTiming;

#[cfg(not(target_arch = "wasm32"))]
#[derive(Debug, Clone, Copy)]
struct ResolveTimingSummary {
    registration_ms: u128,
    extends_ms: u128,
    contents_ms: u128,
    cycle_check_ms: u128,
    semantic_checks_ms: u128,
    validation_ms: u128,
    unresolved_emit_ms: u128,
    total_ms: u128,
    def_count: usize,
    class_count: usize,
}

#[cfg(not(target_arch = "wasm32"))]
fn count_declared_classes(def: &ast::StoredDefinition) -> usize {
    def.classes.values().map(count_class_and_nested).sum()
}

#[cfg(not(target_arch = "wasm32"))]
fn count_class_and_nested(class: &ast::ClassDef) -> usize {
    1 + class
        .classes
        .values()
        .map(count_class_and_nested)
        .sum::<usize>()
}

#[cfg(not(target_arch = "wasm32"))]
fn write_resolve_timing_summary(summary: &ResolveTimingSummary) {
    #[cfg(feature = "tracing")]
    tracing::debug!(
        target: "rumoca_phase_resolve::timing",
        registration_ms = summary.registration_ms,
        extends_ms = summary.extends_ms,
        contents_ms = summary.contents_ms,
        cycle_check_ms = summary.cycle_check_ms,
        semantic_checks_ms = summary.semantic_checks_ms,
        validation_ms = summary.validation_ms,
        unresolved_emit_ms = summary.unresolved_emit_ms,
        total_ms = summary.total_ms,
        def_count = summary.def_count,
        class_count = summary.class_count,
        "resolve timing summary"
    );

    #[cfg(not(feature = "tracing"))]
    let _ = (
        summary.registration_ms,
        summary.extends_ms,
        summary.contents_ms,
        summary.cycle_check_ms,
        summary.semantic_checks_ms,
        summary.validation_ms,
        summary.unresolved_emit_ms,
        summary.total_ms,
        summary.def_count,
        summary.class_count,
    );
}

impl Resolver {
    /// Create a new resolver with builtins pre-registered.
    pub fn new() -> Self {
        let mut resolver = Self {
            next_def_id: 1,
            scope_tree: ScopeTree::new(),
            source_map: SourceMap::default(),
            def_names: IndexMap::default(),
            name_to_def: IndexMap::default(),
            class_types: IndexMap::default(),
            package_children: IndexMap::default(),
            diagnostics: Diagnostics::new(),
            resolving_extends: std::collections::HashSet::new(),
            inheritance_edges: Vec::new(),
            class_to_bases: IndexMap::default(),
            scope_to_class_def: std::collections::HashMap::new(),
            class_def_scopes: std::collections::HashMap::new(),
            component_type_def_ids: std::collections::HashMap::new(),
            dynamic_member_root_ids: std::collections::HashSet::new(),
            partial_type_root_ids: std::collections::HashSet::new(),
            builtin_count: 0,
            stats: ResolutionStats::default(),
            last_core_timing: ResolveCoreTiming::default(),
        };
        resolver.register_builtins();
        resolver
    }

    /// Get the resolution statistics.
    pub fn stats(&self) -> &ResolutionStats {
        &self.stats
    }

    /// Register all builtin types, functions, and variables in the global scope.
    /// Builtins get DefIds 1..N, allowing O(1) builtin checks while reserving
    /// `DefId(0)` for root/global scope per SPEC_0001.
    fn register_builtins(&mut self) {
        // Chain all builtins, deduplicating (types appear in both BUILTIN_TYPES and BUILTIN_FUNCTIONS)
        let all_builtins = BUILTIN_TYPES
            .iter()
            .chain(BUILTIN_FUNCTIONS.iter())
            .chain(BUILTIN_VARIABLES.iter());

        for &name in all_builtins {
            if !self.name_to_def.contains_key(name) {
                let def_id = self.alloc_def_id(None, name);
                self.scope_tree
                    .add_predefined_member(ComponentPath::from_flat_path(name), def_id);
            }
        }
        for &(enum_name, literals) in rumoca_core::PREDEFINED_ENUM_LITERALS {
            for &literal in literals {
                let literal_id = self.alloc_def_id(Some(enum_name), literal);
                self.scope_tree.add_predefined_member(
                    ComponentPath::from_parts([enum_name, literal]),
                    literal_id,
                );
            }
        }

        // All DefIds allocated so far are builtins
        self.builtin_count = self.next_def_id;
    }

    /// Check if a DefId is a builtin (O(1) comparison).
    #[inline]
    pub fn is_builtin(&self, def_id: DefId) -> bool {
        def_id.index() > 0 && def_id.index() < self.builtin_count
    }

    /// Allocate a new DefId for `leaf` declared inside `enclosing` (None for
    /// top-level/global names) and register it in both lookup maps.
    ///
    /// The qualified name is composed here from the structured pair; callers
    /// never join-and-resplit paths. Also populates the package_children map
    /// for O(1) unqualified import resolution.
    pub(crate) fn alloc_def_id(&mut self, enclosing: Option<&str>, leaf: &str) -> DefId {
        let id = DefId::new(self.next_def_id);
        self.next_def_id += 1;

        let name = match enclosing {
            Some(enclosing) if !enclosing.is_empty() => {
                self.package_children
                    .entry(enclosing.to_string())
                    .or_default()
                    .insert(leaf.to_string(), id);
                format!("{enclosing}.{leaf}")
            }
            _ => leaf.to_string(),
        };

        // Insert into both maps: clone for first, move for second.
        self.name_to_def.insert(name.clone(), id);
        self.def_names.insert(id, name);

        id
    }

    /// Add an inheritance edge and update the class-to-bases index.
    ///
    /// This maintains both the edge list (for cycle detection) and the
    /// index (for O(1) base class lookup).
    pub(crate) fn add_inheritance_edge(
        &mut self,
        class_id: DefId,
        base_id: DefId,
        location: Location,
    ) {
        self.inheritance_edges.push((class_id, base_id, location));
        self.class_to_bases
            .entry(class_id)
            .or_default()
            .push(base_id);
    }

    /// Resolve names in a ClassTree.
    ///
    /// This is done in four phases:
    ///
    /// 1. Registration: Walk all classes and register DefIds, create scopes
    /// 2. Extends Resolution (two sub-phases):
    ///    - 2a: Resolve all extends clauses across entire tree first
    ///      (ensures inheritance edges are complete before nested class resolution)
    ///    - 2b: Resolve equations, statements, expressions
    /// 3. Cycle Detection: Check for circular inheritance across all classes
    ///
    /// This multi-phase approach ensures that:
    ///
    /// - All classes are registered before extends resolution
    /// - All inheritance edges are recorded before inherited member lookup
    /// - Indirect cycles (A extends B, B extends A) are detected
    pub fn resolve(&mut self, tree: &mut ClassTree) {
        let registration_start = maybe_start_timer();
        // Copy source map for use in diagnostics
        self.source_map = tree.source_map.clone();
        let global_scope = self.scope_tree.global();

        // Phase 1: Register all classes and their members
        self.register_stored_definition(&mut tree.definitions, global_scope, "");
        let registration_ms = maybe_elapsed_ms(registration_start);

        let extends_start = maybe_start_timer();
        // Phase 2a: Resolve all imports and extends clauses first
        // This ensures inheritance edges are complete for inherited member lookup
        self.resolve_extends_all(&mut tree.definitions, "");
        let extends_ms = maybe_elapsed_ms(extends_start);

        let cycle_check_start = maybe_start_timer();
        // Reject cycles before recursively constructing effective inherited
        // member views, then make inherited names participate in ordinary
        // scope lookup before contents are resolved.
        self.check_inheritance_cycles(&tree.definitions);
        self.populate_inherited_scope_members(&tree.definitions);
        let cycle_check_ms = maybe_elapsed_ms(cycle_check_start);

        let contents_start = maybe_start_timer();
        // Resolve every component's declared type before any expression. Full
        // component-reference resolution is consequently independent of class
        // declaration order.
        self.resolve_component_types_all(&mut tree.definitions, "");
        // Phase 2c: Resolve equations, statements, expressions.
        self.resolve_contents_all(&mut tree.definitions, global_scope, "");
        let contents_ms = maybe_elapsed_ms(contents_start);

        #[cfg(not(target_arch = "wasm32"))]
        {
            self.last_core_timing = ResolveCoreTiming {
                registration_ms,
                extends_ms,
                contents_ms,
                cycle_check_ms,
            };
        }
        #[cfg(target_arch = "wasm32")]
        {
            let _ = (registration_ms, extends_ms, contents_ms, cycle_check_ms);
            self.last_core_timing = ResolveCoreTiming;
        }

        // Transfer the built scope tree to the ClassTree
        tree.scope_tree = std::mem::take(&mut self.scope_tree);
        // Copy the lookup maps to the ClassTree for O(1) class lookup.
        // Keep resolver copies so post-resolution diagnostics can still use
        // inherited lookup helpers before returning.
        tree.def_map = self.def_names.clone();
        tree.name_map = self.name_to_def.clone();
        tree.scope_to_class = self
            .scope_to_class_def
            .iter()
            .map(|(k, v)| (*k, *v))
            .collect();
    }

    /// Check if resolution produced any errors.
    pub fn has_errors(&self) -> bool {
        self.diagnostics.has_errors()
    }

    /// Get the collected diagnostics.
    pub fn diagnostics(&self) -> &Diagnostics {
        &self.diagnostics
    }

    /// Take the diagnostics (consuming them).
    pub fn take_diagnostics(self) -> Diagnostics {
        self.diagnostics
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

/// Resolve names in a ParsedTree.
///
/// This is the main entry point for name resolution.
/// Takes a `ParsedTree` and returns a `ResolvedTree` with all DefIds
/// and ScopeIds populated.
///
/// Note: The resolve phase performs partial resolution for type paths (MLS §7.3)
/// but unresolved symbol references are treated as hard errors. Component
/// references and function calls must resolve their leading name in scope.
pub fn resolve(parsed: ParsedTree) -> Result<ResolvedTree, Diagnostics> {
    resolve_with_diagnostics(parsed)
        .map(ResolveSuccess::into_tree)
        .map_err(ResolveFailure::into_diagnostics)
}

/// A completed Resolve phase plus advisory diagnostics.
///
/// Construction is private to this crate: an error-bearing tree cannot receive
/// the [`ResolvedTree`] phase proof.
pub struct ResolveSuccess {
    tree: ResolvedTree,
    diagnostics: Diagnostics,
}

impl ResolveSuccess {
    pub fn into_tree(self) -> ResolvedTree {
        self.tree
    }

    pub fn into_parts(self) -> (ResolvedTree, Diagnostics) {
        (self.tree, self.diagnostics)
    }
}

/// An incomplete Resolve attempt for diagnostics and source-closure planning.
///
/// This type deliberately does not dereference to [`ResolvedTree`] and exposes
/// the incomplete class tree read-only. Semantic phases must resolve a selected
/// closure independently before they can receive a completed phase artifact.
pub struct ResolveFailure {
    tree: Box<ClassTree>,
    diagnostics: Box<Diagnostics>,
}

impl ResolveFailure {
    pub fn tree(&self) -> &ClassTree {
        &self.tree
    }

    pub fn diagnostics(&self) -> &Diagnostics {
        &self.diagnostics
    }

    pub fn into_parts(self) -> (ClassTree, Diagnostics) {
        (*self.tree, *self.diagnostics)
    }

    pub fn into_diagnostics(self) -> Diagnostics {
        *self.diagnostics
    }
}

/// Resolve names while preserving advisory diagnostics on success.
///
/// Any error returns a planning-only artifact rather than a [`ResolvedTree`].
pub fn resolve_with_diagnostics(parsed: ParsedTree) -> Result<ResolveSuccess, ResolveFailure> {
    complete_resolution(resolve_attempt(parsed))
}

struct ResolutionAttempt {
    tree: ClassTree,
    diagnostics: Diagnostics,
    stats: ResolutionStats,
}

fn resolve_attempt(parsed: ParsedTree) -> ResolutionAttempt {
    let total_start = maybe_start_timer();
    let mut tree = parsed.into_inner();
    let mut resolver = Resolver::new();
    resolver.resolve(&mut tree);

    // Run semantic checks on the AST.
    let semantic_checks_start = maybe_start_timer();
    for diag in semantic_checks::check_all_semantics(&tree.definitions, &tree.source_map) {
        resolver.diagnostics.emit(diag);
    }
    for diag in semantic_checks::check_resolved_semantics(&tree) {
        resolver.diagnostics.emit(diag);
    }
    let semantic_checks_ms = maybe_elapsed_ms(semantic_checks_start);

    // Validate unresolved symbols gathered by post-resolution visitor (MLS §5.3)
    let validation_start = maybe_start_timer();
    let validation = validation::validate_resolution(&tree);
    let validation_ms = maybe_elapsed_ms(validation_start);
    let unresolved_emit_start = maybe_start_timer();
    emit_unresolved_symbol_diagnostics(&mut resolver, &tree, &validation);
    let unresolved_emit_ms = maybe_elapsed_ms(unresolved_emit_start);

    #[cfg(target_arch = "wasm32")]
    let _ = (
        total_start,
        semantic_checks_ms,
        validation_ms,
        unresolved_emit_ms,
    );

    #[cfg(not(target_arch = "wasm32"))]
    write_resolve_timing_summary(&ResolveTimingSummary {
        registration_ms: resolver.last_core_timing.registration_ms,
        extends_ms: resolver.last_core_timing.extends_ms,
        contents_ms: resolver.last_core_timing.contents_ms,
        cycle_check_ms: resolver.last_core_timing.cycle_check_ms,
        semantic_checks_ms,
        validation_ms,
        unresolved_emit_ms,
        total_ms: maybe_elapsed_ms(total_start),
        def_count: tree.name_map.len(),
        class_count: count_declared_classes(&tree.definitions),
    });

    let stats = resolver.stats.clone();
    ResolutionAttempt {
        tree,
        diagnostics: resolver.take_diagnostics(),
        stats,
    }
}

fn complete_resolution(attempt: ResolutionAttempt) -> Result<ResolveSuccess, ResolveFailure> {
    if attempt.diagnostics.has_errors() {
        Err(ResolveFailure {
            tree: Box::new(attempt.tree),
            diagnostics: Box::new(attempt.diagnostics),
        })
    } else {
        Ok(ResolveSuccess {
            tree: ResolvedTree::new(attempt.tree),
            diagnostics: attempt.diagnostics,
        })
    }
}

/// Result of resolution with statistics.
pub struct ResolveWithStatsResult {
    /// The resolved tree (if successful).
    pub tree: Result<ResolvedTree, Diagnostics>,
    /// Statistics collected during resolution.
    pub stats: ResolutionStats,
}

/// Resolve names in a ParsedTree and return both the result and statistics.
///
/// This is useful for diagnosing resolution behavior - it always returns stats
/// even if resolution fails.
pub fn resolve_with_stats(parsed: ParsedTree) -> ResolveWithStatsResult {
    let attempt = resolve_attempt(parsed);
    let stats = attempt.stats.clone();
    let tree = match complete_resolution(attempt) {
        Ok(success) => Ok(success.into_tree()),
        Err(failure) => Err(failure.into_diagnostics()),
    };

    ResolveWithStatsResult { tree, stats }
}

/// Resolve names in a parsed StoredDefinition and return a ResolvedTree.
///
/// This is a convenience function that wraps a StoredDefinition in a ClassTree
/// and runs name resolution.
pub fn resolve_parsed(def: StoredDefinition) -> Result<ResolvedTree, Diagnostics> {
    let tree = ClassTree::from_parsed(def);
    let parsed = ParsedTree::new(tree);
    resolve(parsed)
}

/// Emit diagnostics for unresolved symbols discovered by validation.
///
/// MLS §5.3 name lookup failures are reported as resolve-phase diagnostics.
fn emit_unresolved_symbol_diagnostics(
    resolver: &mut Resolver,
    tree: &ClassTree,
    validation: &ValidationResult,
) {
    for unresolved in &validation.unresolved {
        if unresolved.kind == UnresolvedKind::TypeReference
            && unresolved.path.len() == 1
            && tree
                .scope_tree
                .inherited_member(unresolved.scope_id, &unresolved.path)
                == Some(rumoca_ir_ast::InheritedMember::Ambiguous)
        {
            // Conflicting inherited children are a partially flattened-class
            // error (MLS §5.6.1.4 / INST-037). Keep the structured ambiguity
            // for instantiation's EI010 diagnostic instead of misreporting it
            // as a static name-not-found error.
            continue;
        }
        let (kind, code) = match unresolved.kind {
            UnresolvedKind::TypeReference => ("type reference", "ER002"),
            UnresolvedKind::ExtendsBase => ("extends base class", "ER003"),
            UnresolvedKind::ComponentReference => ("component reference", "ER002"),
            UnresolvedKind::FunctionCall => ("function call", "ER002"),
        };

        let Some(span) = location_span_or_emit(
            &mut resolver.diagnostics,
            &unresolved.source_location,
            &resolver.source_map,
            kind,
        ) else {
            continue;
        };
        let primary_label = PrimaryLabel::new(span).with_message(format!("unresolved {kind}"));
        resolver.diagnostics.emit(rumoca_core::Diagnostic::error(
            code,
            format!("unresolved {kind}: '{}'", unresolved.name),
            primary_label,
        ));
    }
}

#[cfg(test)]
mod missing_source_context_tests {
    use super::{Location, missing_source_context_diagnostic};
    use rumoca_core::{SourceId, SourceMap};

    fn location(source: SourceId, start: u32, end: u32, line: u32) -> Location {
        Location {
            start_line: line,
            start_column: line,
            end_line: line,
            end_column: line,
            start,
            end,
            source,
        }
    }

    #[test]
    fn missing_source_context_names_the_registered_file() {
        let mut source_map = SourceMap::new();
        let source = source_map.add("pkg/Widget.mo", "model Widget end Widget;");
        // Line/column are zero, so the location is rejected before the span
        // lookup, but the file itself is registered and must be named.
        let loc = location(source, 0, 5, 0);
        let diag = missing_source_context_diagnostic("component reference", &loc, &source_map);
        assert!(
            diag.message.contains("`pkg/Widget.mo`"),
            "diagnostic must interpolate the real path, got: {}",
            diag.message
        );
    }

    #[test]
    fn missing_source_context_keeps_provenance_for_unregistered_sources() {
        let source_map = SourceMap::new();
        let source = SourceId::from_source_name("pkg/Missing.mo");
        let loc = location(source, 0, 5, 1);
        let diag = missing_source_context_diagnostic("type reference", &loc, &source_map);
        let placeholder = rumoca_core::placeholder_source_name(source);
        assert!(
            diag.message.contains(&placeholder),
            "diagnostic must name the source identity, got: {}",
            diag.message
        );
        assert_eq!(
            rumoca_core::source_id_for_name(&placeholder),
            source,
            "the printed identity must resolve back to the originating file"
        );
    }
}

#[cfg(test)]
mod tests;
