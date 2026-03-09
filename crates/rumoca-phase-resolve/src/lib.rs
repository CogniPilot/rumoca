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
mod registration;
pub mod semantic_checks;
mod traversal_adapter;
pub mod validation;

pub use errors::{ResolveError, ResolveResult};
pub use validation::{UnresolvedKind, UnresolvedSymbol, ValidationResult, validate_resolution};

use indexmap::IndexMap;
use rumoca_core::{
    BUILTIN_FUNCTIONS, BUILTIN_TYPES, BUILTIN_VARIABLES, DefId, Diagnostics, PrimaryLabel, ScopeId,
    SourceMap, Span,
};
use rumoca_ir_ast as ast;

type ClassTree = ast::ClassTree;
type Location = rumoca_ir_core::Location;
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
}

impl Default for ResolveOptions {
    fn default() -> Self {
        Self {
            unresolved_component_refs_are_errors: true,
            unresolved_function_calls_are_errors: true,
        }
    }
}

/// Convert a Location to a Span for error reporting using the source map.
fn location_to_span(loc: &Location, source_map: &SourceMap) -> Span {
    assert!(
        location_has_valid_span(loc),
        "invalid AST location for span conversion: file='{}' start={} end={} start_line={} start_col={} end_line={} end_col={}",
        loc.file_name,
        loc.start,
        loc.end,
        loc.start_line,
        loc.start_column,
        loc.end_line,
        loc.end_column
    );
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
    /// DefIds that can legitimately anchor partial type resolution (replaceable roots).
    pub(crate) partial_type_root_ids: std::collections::HashSet<DefId>,
    /// Number of builtin DefIds (0..builtin_count are builtins).
    builtin_count: u32,
    /// Statistics collected during resolution.
    pub(crate) stats: ResolutionStats,
}

impl Resolver {
    /// Create a new resolver with builtins pre-registered.
    pub fn new() -> Self {
        let mut resolver = Self {
            next_def_id: 0,
            scope_tree: ScopeTree::new(),
            source_map: SourceMap::default(),
            def_names: IndexMap::new(),
            name_to_def: IndexMap::new(),
            package_children: IndexMap::new(),
            diagnostics: Diagnostics::new(),
            resolving_extends: std::collections::HashSet::new(),
            inheritance_edges: Vec::new(),
            class_to_bases: IndexMap::new(),
            scope_to_class_def: std::collections::HashMap::new(),
            partial_type_root_ids: std::collections::HashSet::new(),
            builtin_count: 0,
            stats: ResolutionStats::default(),
        };
        resolver.register_builtins();
        resolver
    }

    /// Get the resolution statistics.
    pub fn stats(&self) -> &ResolutionStats {
        &self.stats
    }

    /// Register all builtin types, functions, and variables in the global scope.
    /// Builtins get DefIds 0..N, allowing O(1) builtin check via `def_id < builtin_count`.
    fn register_builtins(&mut self) {
        let global = ScopeId::GLOBAL;

        // Chain all builtins, deduplicating (types appear in both BUILTIN_TYPES and BUILTIN_FUNCTIONS)
        let all_builtins = BUILTIN_TYPES
            .iter()
            .chain(BUILTIN_FUNCTIONS.iter())
            .chain(BUILTIN_VARIABLES.iter());

        for &name in all_builtins {
            if !self.name_to_def.contains_key(name) {
                let def_id = self.alloc_def_id(name.to_string());
                self.scope_tree.add_member(global, name.to_string(), def_id);
            }
        }

        // All DefIds allocated so far are builtins
        self.builtin_count = self.next_def_id;
    }

    /// Check if a DefId is a builtin (O(1) comparison).
    #[inline]
    pub fn is_builtin(&self, def_id: DefId) -> bool {
        def_id.index() < self.builtin_count
    }

    /// Allocate a new DefId and register it in both lookup maps.
    ///
    /// Also populates the package_children map for O(1) unqualified import resolution.
    ///
    /// Takes an owned `String` to avoid double allocation when callers already have
    /// a formatted string (e.g., from `format!()`).
    pub(crate) fn alloc_def_id(&mut self, name: String) -> DefId {
        let id = DefId::new(self.next_def_id);
        self.next_def_id += 1;

        // Register as child of parent package for O(1) unqualified import lookup.
        // Must do this before moving `name` into the maps.
        if let Some(dot_pos) = name.rfind('.') {
            let parent = &name[..dot_pos];
            let child_name = &name[dot_pos + 1..];
            self.package_children
                .entry(parent.to_string())
                .or_default()
                .insert(child_name.to_string(), id);
        }

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
        // Copy source map for use in diagnostics
        self.source_map = tree.source_map.clone();
        let global_scope = self.scope_tree.global();

        // Phase 1: Register all classes and their members
        self.register_stored_definition(&mut tree.definitions, global_scope, "");

        // Phase 2a: Resolve all imports and extends clauses first
        // This ensures inheritance edges are complete for inherited member lookup
        self.resolve_extends_all(&mut tree.definitions, "");

        // Phase 2b: Resolve equations, statements, expressions
        self.resolve_contents_all(&mut tree.definitions, global_scope, "");

        // Phase 3: Check for circular inheritance (detects indirect cycles)
        self.check_inheritance_cycles(&tree.definitions);

        // Transfer the built scope tree to the ClassTree
        tree.scope_tree = std::mem::take(&mut self.scope_tree);
        // Copy the lookup maps to the ClassTree for O(1) class lookup.
        // Keep resolver copies so post-resolution diagnostics can still use
        // inherited lookup helpers before returning.
        tree.def_map = self.def_names.clone();
        tree.name_map = self.name_to_def.clone();
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
    let mut tree = parsed.into_inner();
    let mut resolver = Resolver::new();
    resolver.resolve(&mut tree);

    // Run semantic checks on the AST.
    for diag in semantic_checks::check_all_semantics(&tree.definitions, &tree.source_map) {
        resolver.diagnostics.emit(diag);
    }

    // Validate unresolved symbols gathered by post-resolution visitor (MLS §5.3)
    let validation = validation::validate_resolution(&tree);
    emit_unresolved_symbol_diagnostics(&mut resolver, &validation, options);

    if resolver.has_errors() {
        Err(resolver.take_diagnostics())
    } else {
        Ok(ResolvedTree::new(tree))
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
    let mut tree = parsed.into_inner();
    let mut resolver = Resolver::new();
    resolver.resolve(&mut tree);

    // Run semantic checks.
    for diag in semantic_checks::check_all_semantics(&tree.definitions, &tree.source_map) {
        resolver.diagnostics.emit(diag);
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

        let (kind, code, is_error) = match unresolved.kind {
            UnresolvedKind::TypeReference => ("type reference", "ER002", true),
            UnresolvedKind::ExtendsBase => ("extends base class", "ER003", true),
            UnresolvedKind::ComponentReference => (
                "component reference",
                "ER002",
                options.unresolved_component_refs_are_errors,
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

/// Check whether an unresolved simple name can be found in inherited members of
/// the current class or any enclosing class.
fn has_inherited_match(resolver: &Resolver, location: &str, name: &str) -> bool {
    let mut container = location;
    loop {
        if resolver.lookup_inherited_member(container, name).is_some() {
            return true;
        }
        let Some(dot_pos) = container.rfind('.') else {
            break;
        };
        container = &container[..dot_pos];
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_phase_parse::parse_to_ast;

    fn find_comp_ref_def_id(expr: &rumoca_ir_ast::Expression) -> Option<DefId> {
        match expr {
            ast::Expression::ComponentReference(cr) => cr.def_id,
            ast::Expression::Binary { lhs, rhs, .. } => {
                find_comp_ref_def_id(lhs).or_else(|| find_comp_ref_def_id(rhs))
            }
            ast::Expression::Unary { rhs, .. } => find_comp_ref_def_id(rhs),
            ast::Expression::Range { start, step, end } => find_comp_ref_def_id(start)
                .or_else(|| step.as_ref().and_then(|s| find_comp_ref_def_id(s)))
                .or_else(|| find_comp_ref_def_id(end)),
            ast::Expression::FunctionCall { comp, args } => comp
                .def_id
                .or_else(|| args.iter().find_map(find_comp_ref_def_id)),
            ast::Expression::ClassModification {
                target,
                modifications,
            } => target
                .def_id
                .or_else(|| modifications.iter().find_map(find_comp_ref_def_id)),
            ast::Expression::NamedArgument { value, .. } => find_comp_ref_def_id(value),
            ast::Expression::Modification { target, value } => {
                target.def_id.or_else(|| find_comp_ref_def_id(value))
            }
            ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements } => {
                elements.iter().find_map(find_comp_ref_def_id)
            }
            ast::Expression::If {
                branches,
                else_branch,
            } => branches
                .iter()
                .find_map(|(cond, value)| {
                    find_comp_ref_def_id(cond).or_else(|| find_comp_ref_def_id(value))
                })
                .or_else(|| find_comp_ref_def_id(else_branch)),
            ast::Expression::Parenthesized { inner } => find_comp_ref_def_id(inner),
            ast::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => find_comp_ref_def_id(expr)
                .or_else(|| {
                    indices
                        .iter()
                        .find_map(|idx| find_comp_ref_def_id(&idx.range))
                })
                .or_else(|| filter.as_ref().and_then(|f| find_comp_ref_def_id(f))),
            ast::Expression::ArrayIndex { base, subscripts } => {
                find_comp_ref_def_id(base).or_else(|| {
                    subscripts.iter().find_map(|sub| match sub {
                        rumoca_ir_ast::Subscript::Expression(expr) => find_comp_ref_def_id(expr),
                        rumoca_ir_ast::Subscript::Range { .. } => None,
                        rumoca_ir_ast::Subscript::Empty => None,
                    })
                })
            }
            ast::Expression::FieldAccess { base, .. } => find_comp_ref_def_id(base),
            ast::Expression::Empty | ast::Expression::Terminal { .. } => None,
        }
    }

    #[test]
    fn test_empty_resolution() {
        let tree = ClassTree::new();
        let parsed = ParsedTree::new(tree);
        let result = resolve(parsed);
        assert!(result.is_ok());
    }

    #[test]
    fn test_component_reference_resolution() {
        let source = r#"
model Test
    Real x;
    Real y;
equation
    y = x + 1;
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(result.is_ok(), "resolution should succeed");

        let tree = result.unwrap().into_inner();
        let model = tree
            .definitions
            .classes
            .get("Test")
            .expect("Test should exist");

        // Components should have DefIds
        assert!(model.components.get("x").unwrap().def_id.is_some());
        assert!(model.components.get("y").unwrap().def_id.is_some());

        // Model should have a scope
        assert!(model.scope_id.is_some());
    }

    #[test]
    fn test_unresolved_component_reference_is_error() {
        let source = r#"
model Test
    Real y;
equation
    y = x + 1;
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(result.is_err(), "resolution should fail");

        let diags = result.expect_err("expected resolve diagnostics");
        assert!(diags.iter().any(|d| {
            d.message.contains("unresolved component reference")
                && d.code.as_deref() == Some("ER002")
        }));
    }

    #[test]
    fn test_unresolved_import_is_emitted_before_unresolved_type_reference() {
        let source = r#"
model Ball
    import Modelica.Blocks.Continuous.PID;
    PID pid();
end Ball;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(result.is_err(), "resolution should fail");

        let diags = result.expect_err("expected resolve diagnostics");
        let messages: Vec<_> = diags.iter().map(|d| d.message.as_str()).collect();

        let import_pos = messages
            .iter()
            .position(|msg| msg.contains("unresolved import") && msg.contains("PID"));
        let type_pos = messages
            .iter()
            .position(|msg| msg.contains("unresolved type reference") && msg.contains("PID"));

        assert!(
            import_pos.is_some(),
            "expected unresolved import diagnostic, got: {messages:?}"
        );
        assert!(
            type_pos.is_some(),
            "expected unresolved type reference diagnostic, got: {messages:?}"
        );
        assert!(
            import_pos.expect("import diagnostic index")
                < type_pos.expect("unresolved type diagnostic index"),
            "expected import diagnostic before unresolved type reference, got: {messages:?}"
        );
    }

    #[test]
    fn test_unresolved_diagnostics_include_source_labels() {
        let source = r#"
model Ball
    import Modelica.Blocks.Continuous.PID;
    PID pid();
equation
    der(x) = x;
end Ball;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(result.is_err(), "resolution should fail");

        let diags = result.expect_err("expected resolve diagnostics");
        let import = diags
            .iter()
            .find(|d| d.message.contains("unresolved import"))
            .expect("missing unresolved import diagnostic");
        let unresolved_type = diags
            .iter()
            .find(|d| d.message.contains("unresolved type reference"))
            .expect("missing unresolved type reference diagnostic");

        assert!(
            !import.labels.is_empty(),
            "unresolved import should include a source label"
        );
        assert!(
            !unresolved_type.labels.is_empty(),
            "unresolved type reference should include a source label"
        );
    }

    #[test]
    fn test_unresolved_selective_import_member_is_error() {
        let source = r#"
package P
  model A
  end A;
end P;

model M
  import P.{A, B};
end M;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(result.is_err(), "resolution should fail");

        let diags = result.expect_err("expected resolve diagnostics");
        let import = diags
            .iter()
            .find(|d| d.message.contains("unresolved import member") && d.message.contains("B"))
            .expect("missing unresolved selective import member diagnostic");

        assert_eq!(import.code.as_deref(), Some("ER002"));
        assert!(
            !import.labels.is_empty(),
            "unresolved selective import member should include source label"
        );
    }

    #[test]
    fn test_non_replaceable_partial_type_path_is_unresolved() {
        let source = r#"
model M
  package P
  end P;
  P.Missing x;
equation
  x = 0;
end M;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(
            result.is_err(),
            "resolution should fail for non-replaceable partial type path"
        );

        let diags = result.expect_err("expected resolve diagnostics");
        assert!(diags.iter().any(|d| {
            d.code.as_deref() == Some("ER002")
                && d.message.contains("unresolved type reference")
                && d.message.contains("P.Missing")
        }));
    }

    #[test]
    fn test_unresolved_function_call_is_error() {
        let source = r#"
model Test
    Real y;
equation
    y = unknownFunc(1.0);
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(result.is_err(), "resolution should fail");

        let diags = result.expect_err("expected resolve diagnostics");
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("unresolved function call")
                    && d.code.as_deref() == Some("ER002"))
        );
    }

    #[test]
    fn test_unresolved_function_call_can_be_lenient() {
        let source = r#"
model Test
    Real y;
equation
    y = unknownFunc(1.0);
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let parsed = ParsedTree::new(ClassTree::from_parsed(ast));
        let options = ResolveOptions {
            unresolved_component_refs_are_errors: false,
            unresolved_function_calls_are_errors: false,
        };
        let result = resolve_with_options(parsed, options);
        assert!(
            result.is_ok(),
            "lenient mode should not fail unresolved function calls"
        );
    }

    #[test]
    fn test_function_call_resolves_to_canonical_qualified_target() {
        let source = r#"
package Interfaces
  partial package PartialMedium
    replaceable function f
      input Real u;
      output Real y;
    algorithm
      y := u;
    end f;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
  redeclare function f
    input Real u;
    output Real y;
  algorithm
    y := u + 1;
  end f;
end TableBased;

model UsesMediumAlias
  package Medium = TableBased;
  Real y;
equation
  y = Medium.f(1.0);
end UsesMediumAlias;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let tree = resolve_parsed(ast)
            .expect("resolution should succeed")
            .into_inner();
        let model = tree
            .definitions
            .classes
            .get("UsesMediumAlias")
            .expect("UsesMediumAlias should exist");
        let rumoca_ir_ast::Equation::Simple { rhs, .. } = &model.equations[0] else {
            panic!("expected simple equation");
        };
        let rumoca_ir_ast::Expression::FunctionCall { comp, .. } = rhs else {
            panic!("expected function call on rhs");
        };
        let def_id = comp.def_id.expect("function call should have def_id");
        let resolved = tree
            .def_map
            .get(&def_id)
            .expect("resolved function def_id should exist in def_map");
        assert_eq!(
            resolved, "TableBased.f",
            "function call should resolve to canonical qualified function"
        );
        assert_eq!(
            comp.to_string(),
            "TableBased.f",
            "function call path should be canonicalized"
        );
    }

    #[test]
    fn test_inherited_medium_alias_function_call_is_canonicalized() {
        let source = r#"
package Interfaces
  partial package PartialMedium
    replaceable function density_pTX
      input Real p;
      input Real T;
      output Real d;
    algorithm
      d := p + T;
    end density_pTX;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
end TableBased;

model Base
  package Medium = TableBased;
end Base;

model Derived
  extends Base;
  Real d;
equation
  d = Medium.density_pTX(1.0, 2.0);
end Derived;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let tree = resolve_parsed(ast)
            .expect("resolution should succeed")
            .into_inner();
        let model = tree
            .definitions
            .classes
            .get("Derived")
            .expect("Derived should exist");
        let rumoca_ir_ast::Equation::Simple { rhs, .. } = &model.equations[0] else {
            panic!("expected simple equation");
        };
        let rumoca_ir_ast::Expression::FunctionCall { comp, .. } = rhs else {
            panic!("expected function call on rhs");
        };
        let def_id = comp
            .def_id
            .expect("inherited Medium call should have def_id");
        let resolved = tree
            .def_map
            .get(&def_id)
            .expect("resolved function def_id should exist in def_map");
        assert_eq!(
            resolved, "Interfaces.PartialMedium.density_pTX",
            "inherited alias function should resolve to concrete target"
        );
        assert_eq!(
            comp.to_string(),
            "Interfaces.PartialMedium.density_pTX",
            "function call path should be canonicalized"
        );
    }

    #[test]
    fn test_component_binding_function_call_is_canonicalized() {
        let source = r#"
package Interfaces
  partial package PartialMedium
    replaceable function f
      input Real u;
      output Real y;
    algorithm
      y := u;
    end f;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
  redeclare function f
    input Real u;
    output Real y;
  algorithm
    y := u + 2;
  end f;
end TableBased;

model UsesTableBasedState
  package Medium = TableBased;
  Real state = Medium.f(1.0);
end UsesTableBasedState;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let tree = resolve_parsed(ast)
            .expect("resolution should succeed")
            .into_inner();
        let model = tree
            .definitions
            .classes
            .get("UsesTableBasedState")
            .expect("UsesTableBasedState should exist");
        let state = model
            .components
            .get("state")
            .expect("state component should exist");

        let target =
            extract_call_target(&state.start).expect("binding should contain function call");
        let def_id = target
            .def_id
            .expect("binding function call should have def_id");
        let resolved = tree
            .def_map
            .get(&def_id)
            .expect("resolved function def_id should exist in def_map");
        assert_eq!(resolved, "TableBased.f");
        assert_eq!(target.to_string(), "TableBased.f");
    }

    fn extract_call_target(
        expr: &rumoca_ir_ast::Expression,
    ) -> Option<&rumoca_ir_ast::ComponentReference> {
        match expr {
            rumoca_ir_ast::Expression::FunctionCall { comp, .. } => Some(comp),
            rumoca_ir_ast::Expression::ClassModification { target, .. } => Some(target),
            _ => None,
        }
    }

    #[test]
    fn test_binding_call_with_redeclared_record_alias_is_canonicalized() {
        let source = r#"
package Common
  record BaseProps_Tpoly
    Real T;
    Real p;
  end BaseProps_Tpoly;
end Common;

package Interfaces
  partial package PartialMedium
    replaceable record ThermodynamicState
      Real x;
    end ThermodynamicState;

    replaceable function setState_pTX
      input Real p;
      input Real T;
      output ThermodynamicState state;
      external "C";
    end setState_pTX;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium(
    redeclare record ThermodynamicState = Common.BaseProps_Tpoly
  );

  redeclare function setState_pTX
    input Real p;
    input Real T;
    output ThermodynamicState state;
    external "C";
  end setState_pTX;
end TableBased;

model UsesTableBasedState
  package Medium = TableBased;
  Medium.ThermodynamicState state = Medium.setState_pTX(1, 2);
end UsesTableBasedState;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let tree = resolve_parsed(ast)
            .expect("resolution should succeed")
            .into_inner();
        let model = tree
            .definitions
            .classes
            .get("UsesTableBasedState")
            .expect("UsesTableBasedState should exist");
        let state = model
            .components
            .get("state")
            .expect("state component should exist");
        let target =
            extract_call_target(&state.start).expect("binding should contain function call");
        let def_id = target
            .def_id
            .expect("binding function call should have def_id");
        let resolved = tree
            .def_map
            .get(&def_id)
            .expect("resolved function def_id should exist in def_map");
        assert_eq!(resolved, "TableBased.setState_pTX");
        assert_eq!(target.to_string(), "TableBased.setState_pTX");
    }

    #[test]
    fn test_for_loop_scope() {
        let source = r#"
model Test
    Real x[3];
equation
    for i in 1:3 loop
        x[i] = i;
    end for;
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(result.is_ok(), "resolution should succeed");
    }

    #[test]
    fn test_for_equation_range_resolves() {
        let source = r#"
model Test
    parameter Integer n = 3;
    Real x[n];
equation
    for i in 1:n loop
        x[i] = i;
    end for;
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast).expect("resolution should succeed");
        let tree = result.into_inner();
        let model = tree
            .definitions
            .classes
            .get("Test")
            .expect("Test should exist");
        let rumoca_ir_ast::Equation::For { indices, .. } = &model.equations[0] else {
            panic!("expected for-equation");
        };
        let range_expr = &indices[0].range;
        assert!(
            find_comp_ref_def_id(range_expr).is_some(),
            "range expression should resolve component references"
        );
    }

    #[test]
    fn test_for_statement_range_resolves() {
        let source = r#"
model Test
    parameter Integer n = 3;
    Integer x;
algorithm
    for i in 1:n loop
        x := i;
    end for;
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast).expect("resolution should succeed");
        let tree = result.into_inner();
        let model = tree
            .definitions
            .classes
            .get("Test")
            .expect("Test should exist");
        let stmt = model.algorithms[0].first().expect("for statement");
        let rumoca_ir_ast::Statement::For { indices, .. } = stmt else {
            panic!("expected for-statement");
        };
        let range_expr = &indices[0].range;
        assert!(
            find_comp_ref_def_id(range_expr).is_some(),
            "range expression should resolve component references"
        );
    }

    #[test]
    fn test_while_condition_resolves() {
        let source = r#"
model Test
    Integer n = 3;
algorithm
    while n > 0 loop
        n := n - 1;
    end while;
end Test;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast).expect("resolution should succeed");
        let tree = result.into_inner();
        let model = tree
            .definitions
            .classes
            .get("Test")
            .expect("Test should exist");
        let stmt = model.algorithms[0].first().expect("while statement");
        let rumoca_ir_ast::Statement::While(block) = stmt else {
            panic!("expected while-statement");
        };
        assert!(
            find_comp_ref_def_id(&block.cond).is_some(),
            "while condition should resolve component references"
        );
    }

    #[test]
    fn test_nested_class_resolution() {
        let source = r#"
package TestPkg
    model Inner
        Real x;
    end Inner;
end TestPkg;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(result.is_ok(), "resolution should succeed");

        let tree = result.unwrap().into_inner();
        let pkg = tree
            .definitions
            .classes
            .get("TestPkg")
            .expect("TestPkg should exist");
        assert!(pkg.def_id.is_some());

        let inner = pkg.classes.get("Inner").expect("Inner should exist");
        assert!(inner.def_id.is_some());
    }

    // =========================================================================
    // Extends resolution tests (MLS §7)
    // =========================================================================

    #[test]
    fn test_simple_extends_resolution() {
        // Test that a simple extends clause resolves correctly
        let source = r#"
model Base
    Real x;
end Base;

model Derived
    extends Base;
    Real y;
end Derived;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(result.is_ok(), "resolution should succeed");

        let tree = result.unwrap().into_inner();

        // Verify base class exists and has a DefId
        let base = tree
            .definitions
            .classes
            .get("Base")
            .expect("Base should exist");
        assert!(base.def_id.is_some(), "Base should have DefId");

        // Verify derived class exists and extends has base_def_id set
        let derived = tree
            .definitions
            .classes
            .get("Derived")
            .expect("Derived should exist");
        assert_eq!(derived.extends.len(), 1, "Derived should have one extends");

        let extend = &derived.extends[0];
        assert!(
            extend.base_def_id.is_some(),
            "Extends should have base_def_id set"
        );
        assert_eq!(
            extend.base_def_id, base.def_id,
            "base_def_id should match Base's DefId"
        );
    }

    #[test]
    fn test_qualified_extends_resolution() {
        // Test that qualified extends (Package.Model) resolves correctly
        let source = r#"
package MyPkg
    model Base
        Real x;
    end Base;
end MyPkg;

model Derived
    extends MyPkg.Base;
    Real y;
end Derived;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(result.is_ok(), "resolution should succeed");

        let tree = result.unwrap().into_inner();

        // Get the base class's DefId
        let pkg = tree
            .definitions
            .classes
            .get("MyPkg")
            .expect("MyPkg should exist");
        let base = pkg.classes.get("Base").expect("Base should exist in MyPkg");
        assert!(base.def_id.is_some(), "Base should have DefId");

        // Verify derived class extends has correct base_def_id
        let derived = tree
            .definitions
            .classes
            .get("Derived")
            .expect("Derived should exist");
        assert_eq!(derived.extends.len(), 1);

        let extend = &derived.extends[0];
        assert!(
            extend.base_def_id.is_some(),
            "Extends should have base_def_id set"
        );
        assert_eq!(
            extend.base_def_id, base.def_id,
            "base_def_id should match MyPkg.Base's DefId"
        );
    }

    #[test]
    fn test_base_class_not_found() {
        // Test that extending a non-existent class produces an error
        let source = r#"
model Derived
    extends NonExistent;
    Real y;
end Derived;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);

        // Resolution should fail with base class not found error
        assert!(result.is_err(), "resolution should fail");
        let diagnostics = result.unwrap_err();
        assert!(diagnostics.has_errors(), "should have error diagnostics");

        // Check that the error message contains "base class not found"
        let has_base_not_found = diagnostics
            .iter()
            .any(|d| d.message.contains("base class not found"));
        assert!(has_base_not_found, "should have base class not found error");
    }

    #[test]
    fn test_circular_inheritance_direct() {
        // Test that direct self-reference (A extends A) is detected.
        // This produces "base class not found" because when we exclude the
        // current class from lookup (to support redeclare extends pattern),
        // we can't find any other class with that name.
        let source = r#"
model A
    extends A;
    Real x;
end A;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);

        // Resolution should fail with "base class not found" error
        assert!(result.is_err(), "resolution should fail");
        let diagnostics = result.unwrap_err();
        assert!(diagnostics.has_errors(), "should have error diagnostics");

        // Check that the error message indicates base not found
        let has_base_not_found = diagnostics
            .iter()
            .any(|d| d.message.contains("base class not found"));
        assert!(has_base_not_found, "should have base class not found error");
    }

    #[test]
    fn test_multiple_extends() {
        // Test that multiple extends clauses all resolve correctly
        let source = r#"
model Base1
    Real x;
end Base1;

model Base2
    Real y;
end Base2;

model Derived
    extends Base1;
    extends Base2;
    Real z;
end Derived;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);
        assert!(result.is_ok(), "resolution should succeed");

        let tree = result.unwrap().into_inner();
        let derived = tree
            .definitions
            .classes
            .get("Derived")
            .expect("Derived should exist");
        assert_eq!(derived.extends.len(), 2, "Derived should have two extends");

        // Both extends should have base_def_id set
        for extend in &derived.extends {
            assert!(
                extend.base_def_id.is_some(),
                "All extends should have base_def_id set"
            );
        }
    }

    #[test]
    fn test_circular_inheritance_indirect() {
        // Test that indirect circular inheritance (A extends B, B extends A) is detected
        let source = r#"
model A
    extends B;
    Real x;
end A;

model B
    extends A;
    Real y;
end B;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);

        // Resolution should fail with circular inheritance error
        assert!(result.is_err(), "resolution should fail for indirect cycle");
        let diagnostics = result.unwrap_err();
        assert!(diagnostics.has_errors(), "should have error diagnostics");

        // Check that the error message contains "circular"
        let has_circular = diagnostics.iter().any(|d| d.message.contains("circular"));
        assert!(
            has_circular,
            "should have circular inheritance error for indirect cycle"
        );
    }

    #[test]
    fn test_circular_inheritance_chain() {
        // Test that longer cycles (A extends B, B extends C, C extends A) are detected
        let source = r#"
model A
    extends B;
end A;

model B
    extends C;
end B;

model C
    extends A;
end C;
"#;
        let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
        let result = resolve_parsed(ast);

        // Resolution should fail with circular inheritance error
        assert!(result.is_err(), "resolution should fail for chain cycle");
        let diagnostics = result.unwrap_err();
        assert!(diagnostics.has_errors(), "should have error diagnostics");

        // Check that the error message contains "circular"
        let has_circular = diagnostics.iter().any(|d| d.message.contains("circular"));
        assert!(
            has_circular,
            "should have circular inheritance error for chain cycle"
        );
    }
}
