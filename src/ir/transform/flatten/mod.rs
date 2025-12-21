//! This module provides functionality to flatten a hierarchical intermediate representation (IR)
//! of a syntax tree into a flat representation. The primary purpose of this process is to
//! simplify the structure of the IR by expanding nested components and incorporating their
//! equations and subcomponents into a single flat class definition.
//!
//! The main function in this module is `flatten`, which takes a stored definition of the IR
//! and produces a flattened class definition. The process involves:
//!
//! - Identifying the main class and other class definitions from the provided IR.
//! - Iteratively expanding components in the main class that reference other class definitions.
//! - Propagating equations and subcomponents from referenced classes into the main class.
//! - Removing expanded components from the main class to ensure a flat structure.
//!
//! This module relies on `SymbolTable` for scope tracking and `SubCompNamer` for
//! renaming hierarchical component references during the flattening process.
//!
//! # Submodules
//! - `cache`: Cache control for flatten operations
//! - `class_dict`: Class dictionary building and lookup
//! - `connections`: Connect equation expansion
//! - `hash`: File hashing and dependency tracking
//! - `imports`: Import and package resolution
//! - `validation`: Subscript and cardinality validation
//!
//! # Dependencies
//! - `anyhow::Result`: For error handling.
//! - `indexmap::IndexMap`: To maintain the order of class definitions and components.
//!

mod cache;
mod class_dict;
mod connections;
mod expansion;
mod hash;
mod helpers;
mod imports;
mod validation;

pub use cache::{
    clear_all_caches, clear_caches, disable_cache, enable_cache, get_cache_stats, is_cache_enabled,
};
pub use hash::{FileDependencies, FlattenResult};

use class_dict::{get_or_build_class_dict, lookup_class};
use connections::expand_connect_equations;
use expansion::ExpansionContext;
use hash::{
    FILE_HASH_CACHE, build_dependency_graph, compute_def_hash, compute_dependency_levels,
    record_file_dep,
};
use imports::{
    build_import_aliases_for_class, collect_imported_packages_for_class,
    extract_extends_modifications, resolve_class_name_with_imports, validate_imports,
};
use validation::check_cardinality_array_connectors;

use crate::ir;
use crate::ir::analysis::reference_checker::collect_imported_packages;
use crate::ir::analysis::symbol_table::SymbolTable;
use crate::ir::ast::{Expression, Import, OpBinary};
use crate::ir::error::IrError;
use crate::ir::transform::constants::is_primitive_type;
use crate::ir::visitor::MutVisitor;
use anyhow::Result;
use indexmap::{IndexMap, IndexSet};
#[cfg(not(target_arch = "wasm32"))]
use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::RwLock;
use std::sync::{Arc, LazyLock};

/// Type alias for class dictionary with Arc-wrapped definitions for efficient sharing
pub type ClassDict = IndexMap<String, Arc<ir::ast::ClassDefinition>>;

// =============================================================================
// Global Caches (only used when CACHE_ENABLED is true)
// =============================================================================

/// Global cache for class dictionaries, keyed by content hash.
/// Only populated when caching is enabled via enable_cache().
static CLASS_DICT_CACHE: LazyLock<RwLock<HashMap<u64, Arc<ClassDict>>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Type alias for resolved class cache entry: (class, dependencies)
type ResolvedClassEntry = (Arc<ir::ast::ClassDefinition>, FileDependencies);

/// Type alias for resolved class cache key: (def_hash, class_path)
type ResolvedClassKey = (u64, String);

/// Global cache for resolved classes.
/// Only populated when caching is enabled via enable_cache().
static RESOLVED_CLASS_CACHE: LazyLock<RwLock<HashMap<ResolvedClassKey, ResolvedClassEntry>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Type alias for extends chain cache: (type_name, class_path) -> resolved_path
type ExtendsChainCache = HashMap<(String, String), Option<String>>;

/// Cache for extends chain type lookups.
/// This dramatically speeds up type resolution for deep inheritance hierarchies.
static EXTENDS_CHAIN_CACHE: LazyLock<RwLock<ExtendsChainCache>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Pre-warm the resolved class cache using parallel wavefront processing.
///
/// This resolves all classes in the StoredDefinition in dependency order,
/// with each level processed in parallel. This is optimal for bulk compilation
/// where many models will be flattened.
///
/// Returns the number of classes pre-warmed.
pub fn prewarm_class_cache(def: &ir::ast::StoredDefinition) -> usize {
    let def_hash = compute_def_hash(def);
    let class_dict = get_or_build_class_dict(def, def_hash);

    // Build dependency graph
    let deps = build_dependency_graph(&class_dict);

    // Compute levels for wavefront parallelism
    let levels = compute_dependency_levels(&class_dict, &deps);

    let mut total_prewarmed = 0;

    // Process each level (parallel on native, sequential on WASM)
    for level in &levels {
        #[cfg(not(target_arch = "wasm32"))]
        level.par_iter().for_each(|class_name| {
            if let Some(class_arc) = class_dict.get(class_name) {
                let _ = resolve_class(class_arc, class_name, &class_dict, def_hash);
            }
        });
        #[cfg(target_arch = "wasm32")]
        level.iter().for_each(|class_name| {
            if let Some(class_arc) = class_dict.get(class_name) {
                let _ = resolve_class(class_arc, class_name, &class_dict, def_hash);
            }
        });
        total_prewarmed += level.len();
    }

    total_prewarmed
}

/// Visitor that renames component references using a symbol table.
///
/// This visitor uses a `SymbolTable` to look up variable names and prepend
/// the appropriate scope prefix when the variable is not a global symbol.
#[derive(Debug, Clone)]
struct ScopeRenamer<'a> {
    /// Reference to the symbol table for lookups
    symbol_table: &'a SymbolTable,
    /// The component scope prefix to prepend
    scope_prefix: String,
    /// Additional global symbols specific to this component (e.g., imported packages)
    component_globals: std::collections::HashSet<String>,
}

impl<'a> ScopeRenamer<'a> {
    /// Create a ScopeRenamer with imports from the component's class hierarchy.
    /// This includes imports from the component class itself and all enclosing packages.
    fn with_class_imports(
        symbol_table: &'a SymbolTable,
        scope_prefix: &str,
        class_path: &str,
        class_dict: &ClassDict,
    ) -> Self {
        Self {
            symbol_table,
            scope_prefix: scope_prefix.to_string(),
            component_globals: collect_imported_packages_for_class(class_path, class_dict),
        }
    }

    fn is_global(&self, name: &str) -> bool {
        self.symbol_table.is_global(name) || self.component_globals.contains(name)
    }
}

impl MutVisitor for ScopeRenamer<'_> {
    fn exit_component_reference(&mut self, node: &mut ir::ast::ComponentReference) {
        // Check if the first part of the reference is a global symbol.
        // For a reference like "Modelica.Constants.pi", we should check if "Modelica" is global,
        // not the full "Modelica.Constants.pi" string.
        let first_part_is_global = node
            .parts
            .first()
            .map(|p| self.is_global(&p.ident.text))
            .unwrap_or(false);

        // Only prepend scope if the first part is not a global symbol
        if !first_part_is_global {
            node.parts.insert(
                0,
                ir::ast::ComponentRefPart {
                    ident: ir::ast::Token {
                        text: self.scope_prefix.clone(),
                        ..Default::default()
                    },
                    subs: None,
                },
            );
        }
    }
}

/// Recursively resolves a class definition by processing all extends clauses.
///
/// This function takes a class and resolves all inheritance by copying components
/// and equations from parent classes into the returned class definition.
///
/// # Arguments
///
/// * `class` - The class definition to resolve
/// * `current_class_path` - The fully qualified path of the current class (for scope lookup)
/// * `class_dict` - Dictionary of all available classes
/// * `def_hash` - Content hash of StoredDefinition for cache key stability
fn resolve_class(
    class: &ir::ast::ClassDefinition,
    current_class_path: &str,
    class_dict: &ClassDict,
    def_hash: u64,
) -> Result<(Arc<ir::ast::ClassDefinition>, FileDependencies)> {
    // Check in-memory cache first (only if caching is enabled)
    let cache_key = (def_hash, current_class_path.to_string());
    if is_cache_enabled()
        && let Some((resolved, deps)) = RESOLVED_CLASS_CACHE.read().unwrap().get(&cache_key)
    {
        return Ok((Arc::clone(resolved), deps.clone()));
    }

    // Cache miss or caching disabled - do full resolution
    // Use the internal function with empty visited set for cycle detection
    // Dependencies are tracked recursively in resolve_class_internal
    let mut visited = IndexSet::new();
    let mut deps = FileDependencies::new();
    let resolved = resolve_class_internal(
        class,
        current_class_path,
        class_dict,
        &mut visited,
        &mut deps,
    )?;

    // Wrap in Arc and cache in memory (only if caching is enabled)
    let resolved_arc = Arc::new(resolved);
    if is_cache_enabled() {
        RESOLVED_CLASS_CACHE
            .write()
            .unwrap()
            .insert(cache_key, (Arc::clone(&resolved_arc), deps.clone()));
    }

    Ok((resolved_arc, deps))
}

/// Internal implementation of resolve_class with cycle detection and dependency tracking.
fn resolve_class_internal(
    class: &ir::ast::ClassDefinition,
    current_class_path: &str,
    class_dict: &ClassDict,
    visited: &mut IndexSet<String>,
    deps: &mut FileDependencies,
) -> Result<ir::ast::ClassDefinition> {
    // Check for cycles
    if visited.contains(current_class_path) {
        // Already resolving this class - skip to avoid infinite recursion
        return Ok(class.clone());
    }
    visited.insert(current_class_path.to_string());

    // Record this class's file as a dependency
    record_file_dep(deps, &class.location.file_name);

    let mut resolved = class.clone();

    // Build import aliases for this class
    let import_aliases = build_import_aliases_for_class(current_class_path, class_dict);

    // Record dependencies from imports
    // Each imported class/package contributes a file dependency
    for import in &class.imports {
        let targets = match import {
            Import::Renamed { path, .. } | Import::Qualified { path, .. } => {
                vec![path.to_string()]
            }
            Import::Selective { path, names, .. } => names
                .iter()
                .map(|n| format!("{}.{}", path, n.text))
                .collect(),
            Import::Unqualified { path, .. } => {
                vec![path.to_string()]
            }
        };

        for target in targets {
            if let Some(imported_class) = class_dict.get(&target) {
                record_file_dep(deps, &imported_class.location.file_name);
            }
        }
    }

    // Process all extends clauses
    for extend in &class.extends {
        let parent_name = extend.comp.to_string();

        // Skip primitive types
        if is_primitive_type(&parent_name) {
            continue;
        }

        // Resolve the parent class name using enclosing scope search with import aliases
        let resolved_name = match resolve_class_name_with_imports(
            &parent_name,
            current_class_path,
            class_dict,
            &import_aliases,
        ) {
            Some(name) => name,
            None => continue, // Skip unresolved extends (might be external dependency)
        };

        // Skip if this would create a cycle
        if visited.contains(&resolved_name) {
            continue;
        }

        // Get the parent class
        let parent_class = match class_dict.get(&resolved_name) {
            Some(c) => c,
            None => continue, // Skip missing classes
        };

        // Recursively resolve the parent class first (using resolved name as new context)
        // This also collects dependencies from parent classes
        let resolved_parent =
            resolve_class_internal(parent_class, &resolved_name, class_dict, visited, deps)?;

        // Extract modifications from the extends clause (e.g., extends Foo(L=1e-3))
        let extends_mods = extract_extends_modifications(&extend.modifications);

        // Check for attempts to override final attributes in parent components
        // This handles extends A(x(start = 2.0)) where x has final start in parent
        for mod_expr in &extend.modifications {
            if let Expression::FunctionCall { comp, args } = mod_expr {
                let comp_name = comp.to_string();
                // Check if parent has this component
                if let Some(parent_comp) = resolved_parent.components.get(&comp_name) {
                    // Check each sub-modification
                    for arg in args {
                        if let Expression::Binary { op, lhs, .. } = arg
                            && matches!(op, OpBinary::Assign(_) | OpBinary::Eq(_))
                            && let Expression::ComponentReference(attr_ref) = &**lhs
                        {
                            let attr_name = attr_ref.to_string();
                            // Check if this attribute is final in the parent
                            if parent_comp.final_attributes.contains(&attr_name) {
                                anyhow::bail!(
                                    "Trying to override final element {} with modifier in extends clause for component '{}'",
                                    attr_name,
                                    comp_name
                                );
                            }
                        }
                    }
                }
            }
        }

        // Build import aliases for the parent class (for resolving type names)
        let parent_import_aliases = build_import_aliases_for_class(&resolved_name, class_dict);

        // Add parent's components (insert at the beginning to maintain proper order)
        for (comp_name, comp) in resolved_parent.components.iter().rev() {
            if !resolved.components.contains_key(comp_name) {
                let mut modified_comp = comp.clone();

                // Fully qualify the component's type name using the parent class's context
                // This is critical when inheriting components - e.g., SISO has "RealInput u"
                // and when SimpleIntegrator extends SISO, we need to resolve RealInput
                // in SISO's context (Interfaces package) to get "Interfaces.RealInput"
                let type_name = comp.type_name.to_string();
                if !is_primitive_type(&type_name)
                    && let Some(fq_name) = resolve_class_name_with_imports(
                        &type_name,
                        &resolved_name,
                        class_dict,
                        &parent_import_aliases,
                    )
                {
                    // Update the type name to the fully qualified version
                    modified_comp.type_name = ir::ast::Name {
                        name: fq_name
                            .split('.')
                            .map(|s| ir::ast::Token {
                                text: s.to_string(),
                                ..Default::default()
                            })
                            .collect(),
                    };
                }

                // Apply extends modifications to inherited components
                if let Some(mod_value) = extends_mods.get(comp_name) {
                    modified_comp.start = mod_value.clone();
                    modified_comp.start_is_modification = true;
                }

                resolved.components.insert(comp_name.clone(), modified_comp);
                resolved
                    .components
                    .move_index(resolved.components.len() - 1, 0);
            }
        }

        // Add parent's equations at the beginning
        let mut new_equations = resolved_parent.equations.clone();
        new_equations.append(&mut resolved.equations);
        resolved.equations = new_equations;

        // Merge parent's imports (add to end, child imports take precedence)
        // This ensures that when we flatten equations from parent classes,
        // references like "Modelica.Constants.pi" are recognized as global package references.
        for import in &resolved_parent.imports {
            if !resolved.imports.contains(import) {
                resolved.imports.push(import.clone());
            }
        }
    }

    // Apply causality from type definitions to components
    // e.g., if a component has type RealInput which is defined as "connector RealInput = input Real"
    // then the component should have Input causality
    apply_type_causality(&mut resolved, current_class_path, class_dict);

    Ok(resolved)
}

/// Apply causality from type definitions to components whose causality is Empty
/// This handles type aliases like "connector RealInput = input Real"
fn apply_type_causality(
    class: &mut ir::ast::ClassDefinition,
    current_class_path: &str,
    class_dict: &ClassDict,
) {
    use crate::ir::ast::Causality;

    // Build import aliases for this class
    let import_aliases = build_import_aliases_for_class(current_class_path, class_dict);

    for (_comp_name, comp) in class.components.iter_mut() {
        // Only apply if component's causality is empty (not explicitly set)
        if !matches!(comp.causality, Causality::Empty) {
            continue;
        }

        let type_name = comp.type_name.to_string();

        // Resolve the type name using enclosing scope search with import aliases
        let resolved_type_name = resolve_class_name_with_imports(
            &type_name,
            current_class_path,
            class_dict,
            &import_aliases,
        );

        if let Some(resolved_name) = resolved_type_name
            && let Some(type_class) = class_dict.get(&resolved_name)
        {
            // If the type has causality (from base_prefix), apply it to the component
            if !matches!(type_class.causality, Causality::Empty) {
                comp.causality = type_class.causality.clone();
            }
        }
    }
}

/// Flattens a hierarchical Modelica class definition into a single flat class.
///
/// This function takes a stored definition containing one or more class definitions
/// and produces a single flattened class where all hierarchical components have been
/// expanded into a flat namespace. The process involves:
///
/// - Extracting the main class (specified by name, or first in the definition if None)
/// - Processing extend clauses to inherit components and equations
/// - Expanding components that reference other classes by:
///   - Flattening nested component names with dots (e.g., `comp.subcomp` stays as `comp.subcomp`)
///   - Adding scoped prefixes to equation references
///   - Removing the parent component and adding all subcomponents directly
///
/// # Arguments
///
/// * `def` - A stored definition containing the class hierarchy to flatten
/// * `model_name` - Optional name of the main class to flatten. If None, uses the first class.
///
/// # Returns
///
/// * `Result<ClassDefinition>` - The flattened class definition on success
///
/// # Errors
///
/// Returns an error if:
/// - The main class is not found in the stored definition
/// - A referenced extend class is not found
///
/// # Example
///
/// Given a hierarchical class with subcomponents:
/// ```text
/// class Main
///   SubClass comp;
/// end Main;
///
/// class SubClass
///   Real x;
///   Real y;
/// end SubClass;
/// ```
///
/// This function produces a flat class:
/// ```text
/// class Main
///   Real comp.x;
///   Real comp.y;
/// end Main;
/// ```
///
/// # Package Support
///
/// This function also supports models inside packages. Use dotted paths
/// like "Package.Model" to reference nested models.
///
/// Flatten a model and return the flattened class definition.
pub fn flatten(
    def: &ir::ast::StoredDefinition,
    model_name: Option<&str>,
) -> Result<ir::ast::ClassDefinition> {
    let result = flatten_with_deps(def, model_name)?;
    Ok(result.class)
}

/// Flatten a model and return both the flattened class and its file dependencies.
///
/// The dependencies can be used for disk caching - if any dependency file has changed
/// (based on MD5 hash), the cached result is invalid.
pub fn flatten_with_deps(
    def: &ir::ast::StoredDefinition,
    model_name: Option<&str>,
) -> Result<FlattenResult> {
    // Compute content hash for cache key stability
    let def_hash = compute_def_hash(def);

    // Get or build cached class dictionary
    let class_dict = get_or_build_class_dict(def, def_hash);

    // Determine main class name - model name is required
    let main_class_name = model_name.ok_or(IrError::ModelNameRequired)?.to_string();

    // Get main class (supports dotted paths like "Package.Model")
    let main_class =
        lookup_class(def, &class_dict, &main_class_name).ok_or(IrError::MainClassNotFound)?;

    // Resolve the main class (process extends clauses recursively)
    // This also collects dependencies from all classes involved
    let (resolved_main, mut deps) =
        resolve_class(&main_class, &main_class_name, &class_dict, def_hash)?;

    // Validate all imports in the resolved class before proceeding
    validate_imports(&resolved_main.imports, &class_dict)?;

    // Create the flat class starting from resolved main
    // Clone the inner value from Arc since we need a mutable copy for flattening
    let mut fclass = (*resolved_main).clone();

    // Create symbol table for tracking variable scopes
    let mut symbol_table = SymbolTable::new();

    // Add imported package roots as global symbols so they don't get prefixed.
    // For example, if a component has "Modelica.Constants.pi", we don't want it
    // to become "sine.Modelica.Constants.pi" - we want to keep "Modelica" as global.
    let imported_packages = collect_imported_packages(&resolved_main.imports);
    for pkg in &imported_packages {
        symbol_table.add_global(pkg);
    }

    // Check for cardinality() calls with array connector arguments BEFORE expansion.
    // After expansion, nested references like a1.c are transformed and we lose the
    // ability to detect that a1 is an array component.
    let comp_shapes: std::collections::HashMap<String, Vec<usize>> = resolved_main
        .components
        .iter()
        .map(|(name, comp)| (name.clone(), comp.shape.clone()))
        .collect();
    check_cardinality_array_connectors(&fclass, &comp_shapes)?;

    // Create expansion context
    let mut ctx = ExpansionContext::new(&mut fclass, &class_dict, &symbol_table, def_hash);

    // Register top-level inner components before expansion
    ctx.register_inner_components(&resolved_main.components);

    // Collect component names that need expansion (to avoid borrow issues)
    // Include all non-primitive types - expand_component will error if type is not found
    let components_to_expand: Vec<(String, ir::ast::Component)> = resolved_main
        .components
        .iter()
        .filter(|(_, comp)| {
            // Skip primitive types, they don't need expansion
            !is_primitive_type(&comp.type_name.to_string())
        })
        .map(|(name, comp)| (name.clone(), comp.clone()))
        .collect();

    // Recursively expand each component that references a class (with inner/outer support)
    // Note: component expansion may use additional classes, but those dependencies
    // are already captured in resolve_class calls during expansion
    for (comp_name, comp) in &components_to_expand {
        ctx.expand_component(comp_name, comp, &main_class_name)?;
    }

    // Rewrite equations to redirect outer references to inner components
    ctx.apply_outer_renaming();

    // Extract pin_types and merge component dependencies
    let pin_types = ctx.pin_types;

    // Merge dependencies from component expansion into main deps
    for (file, hash) in ctx.deps.files {
        deps.record(&file, &hash);
    }

    // Expand connect equations into simple equations
    expand_connect_equations(&mut fclass, &class_dict, &pin_types)?;

    Ok(FlattenResult {
        class: fclass,
        dependencies: deps,
    })
}

#[cfg(test)]
mod tests;
