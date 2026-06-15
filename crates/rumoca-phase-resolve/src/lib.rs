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
//! - `contents` - Phase 2b: Equation, statement, expression resolution
//! - `cycles` - Phase 3: Inheritance cycle detection
//! - `lookup` - Name lookup helpers
//! - [`validation`] - Post-resolution validation (unresolved symbol detection)

mod contents;
mod cycles;
mod errors;
mod extends;
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
    DiagnosticSeverity, Diagnostics, PrimaryLabel, ScopeId, SourceMap, Span, maybe_elapsed_ms,
    maybe_start_timer,
};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

type ClassTree = ast::ClassTree;
type Location = rumoca_core::Location;
type ParsedTree = ast::ParsedTree;
type ResolvedTree = ast::ResolvedTree;
type ScopeTree = ast::ScopeTree;
type StoredDefinition = ast::StoredDefinition;

/// Resolution behavior options.
#[derive(Debug, Clone, Copy)]
pub struct ResolveOptions {
    /// Whether unresolved component references are treated as hard errors.
    pub unresolved_component_refs_are_errors: bool,
    /// Whether unresolved function calls are treated as hard errors.
    pub unresolved_function_calls_are_errors: bool,
    /// Whether ER070 (annotation Evaluate scope) is treated as a hard error.
    pub evaluate_scope_is_error: bool,
    /// Whether ER053 (single-assignment in when-equation) is treated as a hard error.
    pub when_single_assign_is_error: bool,
}

impl Default for ResolveOptions {
    fn default() -> Self {
        Self {
            unresolved_component_refs_are_errors: true,
            unresolved_function_calls_are_errors: true,
            evaluate_scope_is_error: true,
            when_single_assign_is_error: true,
        }
    }
}

fn apply_semantic_diagnostic_policy(mut diag: Diagnostic, options: ResolveOptions) -> Diagnostic {
    let code = diag.code.as_deref().unwrap_or_default();
    if code == "ER070" && !options.evaluate_scope_is_error {
        diag.severity = DiagnosticSeverity::Warning;
    }
    if code == "ER053" && !options.when_single_assign_is_error {
        diag.severity = DiagnosticSeverity::Warning;
    }
    diag
}

/// Convert a Location to a Span for error reporting using the source map.
fn location_to_span(loc: &Location, source_map: &SourceMap) -> Span {
    if !location_has_valid_span(loc) {
        return Span::DUMMY;
    }
    source_map.location_to_span(&loc.file_name, loc.start as usize, loc.end as usize)
}

fn location_has_valid_span(loc: &Location) -> bool {
    !loc.file_name.is_empty()
        && loc.end > loc.start
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
    /// Fully qualified class names whose scopes are encapsulated.
    pub(crate) encapsulated_class_names: std::collections::HashSet<String>,
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
            encapsulated_class_names: std::collections::HashSet::new(),
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
        let global = ScopeId::GLOBAL;

        // Chain all builtins, deduplicating (types appear in both BUILTIN_TYPES and BUILTIN_FUNCTIONS)
        let all_builtins = BUILTIN_TYPES
            .iter()
            .chain(BUILTIN_FUNCTIONS.iter())
            .chain(BUILTIN_VARIABLES.iter());

        for &name in all_builtins {
            if !self.name_to_def.contains_key(name) {
                let def_id = self.alloc_def_id(None, name);
                self.scope_tree
                    .add_member(global, ComponentPath::from_flat_path(name), def_id);
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

        let contents_start = maybe_start_timer();
        // Phase 2b: Resolve equations, statements, expressions
        self.resolve_contents_all(&mut tree.definitions, global_scope, "");
        let contents_ms = maybe_elapsed_ms(contents_start);

        let cycle_check_start = maybe_start_timer();
        // Phase 3: Check for circular inheritance (detects indirect cycles)
        self.check_inheritance_cycles(&tree.definitions);
        let cycle_check_ms = maybe_elapsed_ms(cycle_check_start);

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
    resolve_with_options(parsed, ResolveOptions::default())
}

/// Resolve names in a ParsedTree with custom unresolved-symbol policy.
pub fn resolve_with_options(
    parsed: ParsedTree,
    options: ResolveOptions,
) -> Result<ResolvedTree, Diagnostics> {
    let (resolved, diagnostics) = resolve_with_options_collect(parsed, options);
    if diagnostics.has_errors() {
        Err(diagnostics)
    } else {
        Ok(resolved)
    }
}

/// Resolve names and retain the resolved tree even when diagnostics were emitted.
///
/// This is used by strict target compilation, which needs a best-effort resolved
/// tree for reachability planning while separately deciding which diagnostics are
/// relevant to the requested target closure.
pub fn resolve_with_options_collect(
    parsed: ParsedTree,
    options: ResolveOptions,
) -> (ResolvedTree, Diagnostics) {
    let total_start = maybe_start_timer();
    let mut tree = parsed.into_inner();
    let mut resolver = Resolver::new();
    resolver.resolve(&mut tree);

    // Run semantic checks on the AST.
    let semantic_checks_start = maybe_start_timer();
    for diag in semantic_checks::check_all_semantics(&tree.definitions, &tree.source_map) {
        resolver
            .diagnostics
            .emit(apply_semantic_diagnostic_policy(diag, options));
    }
    let semantic_checks_ms = maybe_elapsed_ms(semantic_checks_start);

    // Validate unresolved symbols gathered by post-resolution visitor (MLS §5.3)
    let validation_start = maybe_start_timer();
    let validation = validation::validate_resolution(&tree);
    let validation_ms = maybe_elapsed_ms(validation_start);
    let unresolved_emit_start = maybe_start_timer();
    emit_unresolved_symbol_diagnostics(&mut resolver, &validation, options);
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

    (ResolvedTree::new(tree), resolver.take_diagnostics())
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
    let mut tree = parsed.into_inner();
    let mut resolver = Resolver::new();
    resolver.resolve(&mut tree);

    // Run semantic checks.
    for diag in semantic_checks::check_all_semantics(&tree.definitions, &tree.source_map) {
        resolver.diagnostics.emit(apply_semantic_diagnostic_policy(
            diag,
            ResolveOptions::default(),
        ));
    }

    // Validate unresolved symbols gathered by post-resolution visitor (MLS §5.3)
    let validation = validation::validate_resolution(&tree);
    emit_unresolved_symbol_diagnostics(&mut resolver, &validation, ResolveOptions::default());

    let stats = resolver.stats.clone();
    let result = if resolver.has_errors() {
        Err(resolver.take_diagnostics())
    } else {
        Ok(ResolvedTree::new(tree))
    };

    ResolveWithStatsResult {
        tree: result,
        stats,
    }
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
    validation: &ValidationResult,
    options: ResolveOptions,
) {
    for unresolved in &validation.unresolved {
        if unresolved.kind == UnresolvedKind::ComponentReference
            && has_inherited_match(resolver, &unresolved.scope_path, &unresolved.name)
        {
            continue;
        }

        let force_error = unresolved.kind == UnresolvedKind::ComponentReference
            && unresolved_is_within_encapsulated_scope(resolver, &unresolved.scope_path);
        let (kind, code, is_error) = match unresolved.kind {
            UnresolvedKind::TypeReference => ("type reference", "ER002", true),
            UnresolvedKind::ExtendsBase => ("extends base class", "ER003", true),
            UnresolvedKind::ComponentReference => (
                "component reference",
                "ER002",
                options.unresolved_component_refs_are_errors || force_error,
            ),
            UnresolvedKind::FunctionCall => (
                "function call",
                "ER002",
                options.unresolved_function_calls_are_errors,
            ),
        };

        let span = location_to_span(&unresolved.source_location, &resolver.source_map);
        let primary_label = PrimaryLabel::new(span).with_message(format!("unresolved {kind}"));
        let diag = if is_error {
            rumoca_core::Diagnostic::error(
                code,
                format!("unresolved {kind}: '{}'", unresolved.name),
                primary_label,
            )
        } else {
            rumoca_core::Diagnostic::warning(
                code,
                format!("unresolved {kind}: '{}'", unresolved.name),
                primary_label,
            )
        };
        resolver.diagnostics.emit(diag);
    }
}

/// Qualified names of the reference site's class and every enclosing class,
/// composed innermost-first from the structured scope segments.
fn enclosing_scope_names(scope_path: &[String]) -> impl Iterator<Item = String> + '_ {
    (1..=scope_path.len())
        .rev()
        .map(|end| scope_path[..end].join("."))
}

fn unresolved_is_within_encapsulated_scope(resolver: &Resolver, scope_path: &[String]) -> bool {
    enclosing_scope_names(scope_path).any(|path| resolver.encapsulated_class_names.contains(&path))
}

/// Check whether an unresolved simple name can be found in inherited members of
/// the current class or any enclosing class.
fn has_inherited_match(resolver: &Resolver, scope_path: &[String], name: &str) -> bool {
    enclosing_scope_names(scope_path)
        .any(|container| resolver.lookup_inherited_member(&container, name).is_some())
}

#[cfg(test)]
mod tests;
