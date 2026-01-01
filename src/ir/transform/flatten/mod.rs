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

pub use class_dict::{
    build_combined_class_dict, clear_library_dict_cache, get_or_build_library_dict,
};
use class_dict::{get_or_build_class_dict, lookup_class};
use connections::expand_connect_equations;
use expansion::{ExpansionContext, evaluate_if_equations};
use hash::{
    FILE_HASH_CACHE, build_dependency_graph, compute_def_hash, compute_dependency_levels,
    record_file_dep,
};
use imports::{
    build_import_aliases_for_class, collect_imported_packages_for_class,
    extract_extends_modifications, extract_type_redeclarations, resolve_class_name_with_imports,
    validate_imports,
};
use validation::check_cardinality_array_connectors;

use crate::ir;
use crate::ir::analysis::reference_checker::collect_imported_packages;
use crate::ir::analysis::symbol_table::SymbolTable;
use crate::ir::ast::{ClassType, Expression, Import, OpBinary};
use crate::ir::error::IrError;
use crate::ir::transform::constants::is_primitive_type;
use crate::ir::visitor::{MutVisitable, MutVisitor};
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
    /// Stack of for-loop iteration variables that should not be renamed.
    /// Each entry is a set of variable names for one level of nested array comprehensions.
    for_loop_vars: Vec<std::collections::HashSet<String>>,
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
        let mut globals = collect_imported_packages_for_class(class_path, class_dict);

        // Also add top-level packages from the class dictionary as globals.
        // This ensures that fully qualified paths like "Modelica.Blocks.Interfaces.Adaptors.Functions.state2"
        // are not prefixed with the component scope, since "Modelica" is a known top-level package.
        for key in class_dict.keys() {
            if !key.contains('.') {
                // This is a top-level class/package
                globals.insert(key.clone());
            }
        }

        Self {
            symbol_table,
            scope_prefix: scope_prefix.to_string(),
            component_globals: globals,
            for_loop_vars: Vec::new(),
        }
    }

    fn is_global(&self, name: &str) -> bool {
        self.symbol_table.is_global(name) || self.component_globals.contains(name)
    }

    fn is_for_loop_var(&self, name: &str) -> bool {
        self.for_loop_vars.iter().any(|set| set.contains(name))
    }
}

impl MutVisitor for ScopeRenamer<'_> {
    fn enter_expression(&mut self, node: &mut ir::ast::Expression) {
        // When entering an array comprehension, register the iteration variables
        // BEFORE the inner expression is processed. This ensures that references
        // like `k` in `{u[k] for k in 1:n}` are not renamed to `comp.k`.
        if let ir::ast::Expression::ArrayComprehension { indices, .. } = node {
            let mut vars = std::collections::HashSet::new();
            for idx in indices.iter() {
                vars.insert(idx.ident.text.clone());
            }
            self.for_loop_vars.push(vars);
        }
    }

    fn exit_expression(&mut self, node: &mut ir::ast::Expression) {
        // Pop the for-loop variables when exiting the array comprehension
        if matches!(node, ir::ast::Expression::ArrayComprehension { .. }) {
            self.for_loop_vars.pop();
        }
    }

    fn enter_equation(&mut self, node: &mut ir::ast::Equation) {
        // When entering a for-loop equation, register the iteration variables
        // BEFORE the nested equations are processed.
        if let ir::ast::Equation::For { indices, .. } = node {
            let mut vars = std::collections::HashSet::new();
            for idx in indices.iter() {
                vars.insert(idx.ident.text.clone());
            }
            self.for_loop_vars.push(vars);
        }
    }

    fn exit_equation(&mut self, node: &mut ir::ast::Equation) {
        // Pop the for-loop variables when exiting the for-loop equation
        if matches!(node, ir::ast::Equation::For { .. }) {
            self.for_loop_vars.pop();
        }
    }

    fn enter_statement(&mut self, node: &mut ir::ast::Statement) {
        // When entering a for-loop statement, register the iteration variables
        // BEFORE the nested statements are processed.
        if let ir::ast::Statement::For { indices, .. } = node {
            let mut vars = std::collections::HashSet::new();
            for idx in indices.iter() {
                vars.insert(idx.ident.text.clone());
            }
            self.for_loop_vars.push(vars);
        }
    }

    fn exit_statement(&mut self, node: &mut ir::ast::Statement) {
        // Pop the for-loop variables when exiting the for-loop statement
        if matches!(node, ir::ast::Statement::For { .. }) {
            self.for_loop_vars.pop();
        }
    }

    fn exit_component_reference(&mut self, node: &mut ir::ast::ComponentReference) {
        // Check if the first part of the reference is a global symbol.
        // For a reference like "Modelica.Constants.pi", we should check if "Modelica" is global,
        // not the full "Modelica.Constants.pi" string.
        let first_part = node.parts.first().map(|p| p.ident.text.as_str());

        let first_part_is_global = first_part.map(|p| self.is_global(p)).unwrap_or(false);

        // Also check if this is a for-loop iteration variable
        let first_part_is_for_loop_var =
            first_part.map(|p| self.is_for_loop_var(p)).unwrap_or(false);

        // Only prepend scope if the first part is not a global symbol and not a for-loop variable
        if !first_part_is_global && !first_part_is_for_loop_var {
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

/// Find an inherited class in the parent package's extends chain.
///
/// This is used to resolve `redeclare model extends X` where X needs to be found
/// in the inherited packages, not in the local class. We skip the local class
/// (which would be a self-reference) and only search through the parent's extends.
fn find_inherited_class_in_parent_extends(
    class_name: &str,
    parent_package_path: &str,
    class_dict: &ClassDict,
    skip_path: &str, // The class path to skip (self-reference)
) -> Option<String> {
    // Get the parent package definition
    let parent_pkg = class_dict.get(parent_package_path)?;

    // Search through the parent package's extends chain
    for extend in &parent_pkg.extends {
        let extended_pkg_name = extend.comp.to_string();

        // Resolve the extended package name relative to the parent
        let resolved_pkg = imports::resolve_class_name_with_imports(
            &extended_pkg_name,
            parent_package_path,
            class_dict,
            &indexmap::IndexMap::new(),
        )?;

        // Look for the class in this extended package
        let candidate = format!("{}.{}", resolved_pkg, class_name);
        if class_dict.contains_key(&candidate) && candidate != skip_path {
            return Some(candidate);
        }

        // Recursively search the extended package's extends chain
        let mut visited = indexmap::IndexSet::new();
        if let Some(found) = imports::find_type_in_extends_chain(
            class_name,
            &resolved_pkg,
            class_dict,
            &mut visited,
        ) {
            if found != skip_path {
                return Some(found);
            }
        }
    }

    None
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

        // Handle self-reference case: when "redeclare model extends X" creates a class X
        // that tries to extend X, the resolution finds itself. For nested classes (like
        // BaseProperties inside a package), we need to find the inherited version from
        // the parent package's extends chain. For top-level classes, this is a circular error.
        let resolved_name = if resolved_name == current_class_path {
            let parts: Vec<&str> = current_class_path.split('.').collect();
            if parts.len() >= 2 {
                // Nested class (e.g., Extended.Base extends Base)
                // Search the parent package's extends chain for the inherited class
                let parent_package_path = parts[..parts.len() - 1].join(".");
                let class_simple_name = parts[parts.len() - 1];

                // Find the inherited class by directly searching the parent's extends chain
                // Skip the local class definition and only look in inherited packages
                if let Some(inherited_path) = find_inherited_class_in_parent_extends(
                    class_simple_name,
                    &parent_package_path,
                    class_dict,
                    current_class_path,
                ) {
                    inherited_path
                } else {
                    // Could not find inherited version, skip this extends
                    continue;
                }
            } else {
                // Top-level class extending itself (like "class A extends A") - this is always an error
                return Err(anyhow::anyhow!(
                    "Circular class inheritance detected: '{}' extends itself",
                    current_class_path
                ));
            }
        } else {
            resolved_name
        };

        // Check for circular inheritance
        if visited.contains(&resolved_name) {
            return Err(anyhow::anyhow!(
                "Circular class inheritance detected: '{}' extends '{}' which eventually extends back to '{}'",
                current_class_path,
                resolved_name,
                current_class_path
            ));
        }

        // Get the parent class
        let parent_class = match class_dict.get(&resolved_name) {
            Some(c) => c,
            None => continue, // Skip missing classes
        };

        // Check that the class types are compatible for extends
        // MLS §7.1.1: A model cannot extend a connector, function, etc.
        if !is_extends_compatible(&class.class_type, &parent_class.class_type) {
            return Err(anyhow::anyhow!(
                "{} '{}' cannot extend {} '{}'",
                class_type_name(&class.class_type),
                class.name.text,
                class_type_name(&parent_class.class_type),
                resolved_name
            ));
        }

        // Recursively resolve the parent class first (using resolved name as new context)
        // This also collects dependencies from parent classes
        let resolved_parent =
            resolve_class_internal(parent_class, &resolved_name, class_dict, visited, deps)?;

        // Extract modifications from the extends clause (e.g., extends Foo(L=1e-3))
        let extends_mods = extract_extends_modifications(&extend.modifications);

        // Extract type redeclarations from extends clause
        // e.g., extends VoltageSource(redeclare Modelica.Blocks.Sources.Sine signalSource(...))
        let (type_redeclarations, redecl_mods) = extract_type_redeclarations(&extend.modifications);

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

        // Check that all modifications target existing components in the parent class
        // MLS §7.2: "A modification shall refer to an element of the class"
        // Exception: Built-in attributes like start, min, max, fixed, unit are always valid
        const BUILTIN_ATTRIBUTES: &[&str] = &[
            "start",
            "min",
            "max",
            "fixed",
            "unit",
            "displayUnit",
            "quantity",
            "stateSelect",
            "nominal",
            "unbounded",
        ];
        for mod_name in extends_mods.keys() {
            if !resolved_parent.components.contains_key(mod_name)
                && !type_redeclarations.contains_key(mod_name)
                && !BUILTIN_ATTRIBUTES.contains(&mod_name.as_str())
            {
                anyhow::bail!(
                    "Modification '{}' in extends clause refers to non-existent element in class '{}'",
                    mod_name,
                    resolved_name
                );
            }
        }

        // Build import aliases for the parent class (for resolving type names)
        let mut parent_import_aliases = build_import_aliases_for_class(&resolved_name, class_dict);

        // Augment parent import aliases with package redeclarations from extends clause.
        // For example, if we have `extends PartialLumpedVolume(redeclare package Medium = WaterIF97_ph)`,
        // we need to add Medium -> WaterIF97_ph to the aliases so that inherited component types
        // like `Medium.BaseProperties` resolve to `WaterIF97_ph.BaseProperties` instead of
        // `PartialMedium.BaseProperties`.
        for (pkg_name, new_type) in &type_redeclarations {
            // Resolve the new type in the current class context
            let resolved_new_type = resolve_class_name_with_imports(
                new_type,
                current_class_path,
                class_dict,
                &import_aliases,
            )
            .unwrap_or_else(|| new_type.clone());
            parent_import_aliases.insert(pkg_name.clone(), resolved_new_type);
        }

        // First pass: Add parameter/constant components with modifications applied.
        // This ensures that when we check conditions in the second pass, we have access to
        // the modified parameter values (e.g., use_fder=false from extends clause).
        for (comp_name, comp) in resolved_parent.components.iter().rev() {
            if !resolved.components.contains_key(comp_name)
                && matches!(
                    comp.variability,
                    ir::ast::Variability::Parameter(_) | ir::ast::Variability::Constant(_)
                )
            {
                let mut modified_comp = comp.clone();

                // Resolve the component's type name using parent class's import aliases
                // This is critical for types like `parameter L y0` where L is an import alias
                // defined in the parent class (e.g., import L = Pkg.Types.Logic)
                let type_name = comp.type_name.to_string();
                if !is_primitive_type(&type_name)
                    && let Some(fq_name) = resolve_class_name_with_imports(
                        &type_name,
                        &resolved_name,
                        class_dict,
                        &parent_import_aliases,
                    )
                {
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

        // Second pass: Add non-parameter components, checking conditions against the
        // now-available parameter values.
        for (comp_name, comp) in resolved_parent.components.iter().rev() {
            if !resolved.components.contains_key(comp_name) {
                // Check conditional component: skip if condition evaluates to false.
                // For example: `RealOutput fder if use_fder` - if use_fder=false, skip.
                if let Some(ref condition) = comp.condition {
                    use crate::ir::transform::eval::eval_boolean;
                    if let Some(false) = eval_boolean(condition, &resolved.components) {
                        // Condition is false - skip this conditional component
                        continue;
                    }
                }

                let mut modified_comp = comp.clone();

                // Check if there's a type redeclaration for this component
                // e.g., extends VoltageSource(redeclare Sine signalSource(...))
                // changes signalSource from SignalSource (partial) to Sine (concrete)
                if let Some(new_type) = type_redeclarations.get(comp_name) {
                    // Resolve the new type in the context of the current class
                    // The new type may be a simple name or a qualified name
                    let resolved_new_type = resolve_class_name_with_imports(
                        new_type,
                        current_class_path,
                        class_dict,
                        &parent_import_aliases,
                    )
                    .unwrap_or_else(|| new_type.clone());

                    // Update the component's type to the new type
                    modified_comp.type_name = ir::ast::Name {
                        name: resolved_new_type
                            .split('.')
                            .map(|s| ir::ast::Token {
                                text: s.to_string(),
                                ..Default::default()
                            })
                            .collect(),
                    };

                    // Apply nested modifications from the redeclaration
                    if let Some(mods) = redecl_mods.get(comp_name) {
                        for mod_expr in mods {
                            if let Expression::Binary { op, lhs, rhs } = mod_expr
                                && matches!(op, OpBinary::Assign(_) | OpBinary::Eq(_))
                                && let Expression::ComponentReference(attr_ref) = &**lhs
                            {
                                let attr_name = attr_ref.to_string();
                                // Store the modification in the component's modifications
                                modified_comp
                                    .modifications
                                    .insert(attr_name, (**rhs).clone());
                            }
                        }
                    }
                } else {
                    // No type redeclaration - use original type name resolution
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
        // First, resolve function call paths in the parent's equations relative to the parent class
        // This handles cases like `Functions.state1(...)` where Functions is a sibling package
        // in the parent class's enclosing scope.
        let mut new_equations = resolved_parent.equations.clone();
        {
            let mut func_resolver = imports::FunctionCallResolver::new(
                &resolved_name,
                class_dict,
                &parent_import_aliases,
            );
            for eq in &mut new_equations {
                eq.accept_mut(&mut func_resolver);
            }
        }
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

    // Remove from visited set - this class is now fully resolved
    // This allows diamond inheritance to work (Base can be extended by both Left and Right)
    visited.shift_remove(current_class_path);

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
    let model_name_str = model_name.ok_or(IrError::ModelNameRequired)?;

    // Qualify model name with `within` clause if present and not already qualified
    let main_class_name = qualify_with_within(model_name_str, def);

    flatten_with_class_dict(def, &class_dict, &main_class_name, def_hash)
}

/// Flatten a model using pre-built library class dictionaries.
///
/// This is optimized for LSP use where libraries are loaded once and reused.
/// Instead of merging StoredDefinitions (which clones all class definitions),
/// this combines class dictionaries by cloning Arc references (cheap).
///
/// # Performance
///
/// This function achieves ~6-8ms compile times by:
/// 1. Reusing pre-built library class dictionaries (cached per library)
/// 2. Combining dictionaries by cloning Arc references instead of ClassDefinitions
/// 3. Only rebuilding the user's class dictionary on each compile
pub fn flatten_with_library_dicts(
    user_def: &ir::ast::StoredDefinition,
    library_dicts: &[Arc<ClassDict>],
    model_name: Option<&str>,
) -> Result<FlattenResult> {
    // Build combined class dictionary (reuses Arc refs from libraries)
    let class_dict = class_dict::build_combined_class_dict(user_def, library_dicts);

    // Compute hash of user definition for cache key
    // This ensures cache is invalidated when user code changes
    let def_hash = compute_def_hash(user_def);

    // Determine main class name - model name is required
    let model_name_str = model_name.ok_or(IrError::ModelNameRequired)?;

    // Qualify model name with `within` clause if present and not already qualified.
    // This is critical for files inside libraries (e.g., Continuous.mo with "within Modelica.Blocks;")
    // so that import resolution can walk up the package hierarchy to find inherited imports.
    let main_class_name = qualify_with_within(model_name_str, user_def);

    flatten_with_class_dict(user_def, &class_dict, &main_class_name, def_hash)
}

/// Qualify a model name with the `within` clause prefix if needed.
///
/// If the StoredDefinition has a `within` clause and the model name doesn't already
/// start with that prefix, prepend it. This ensures proper resolution of inherited
/// imports from parent packages.
///
/// # Examples
/// - `model_name="Integrator"`, `within="Modelica.Blocks.Continuous"` → `"Modelica.Blocks.Continuous.Integrator"`
/// - `model_name="Modelica.Blocks.Continuous.Integrator"`, `within="Modelica.Blocks.Continuous"` → unchanged
/// - `model_name="MyModel"`, no `within` → `"MyModel"`
fn qualify_with_within(model_name: &str, def: &ir::ast::StoredDefinition) -> String {
    if let Some(ref within) = def.within {
        let within_str = within.to_string();
        // Only prepend if model_name doesn't already start with the within prefix
        if !model_name.starts_with(&within_str) {
            return format!("{}.{}", within_str, model_name);
        }
    }
    model_name.to_string()
}

/// Internal flatten implementation that works with a pre-built class dictionary.
fn flatten_with_class_dict(
    def: &ir::ast::StoredDefinition,
    class_dict: &Arc<ClassDict>,
    main_class_name: &str,
    def_hash: u64,
) -> Result<FlattenResult> {
    // Determine main class name - model name is required
    let main_class_name = main_class_name.to_string();

    // Get main class (supports dotted paths like "Package.Model")
    let main_class =
        lookup_class(def, class_dict, &main_class_name).ok_or(IrError::MainClassNotFound)?;

    // Resolve the main class (process extends clauses recursively)
    // This also collects dependencies from all classes involved
    let (resolved_main, mut deps) =
        resolve_class(&main_class, &main_class_name, class_dict, def_hash)?;

    // Validate all imports in the resolved class before proceeding
    validate_imports(&resolved_main.imports, class_dict)?;

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
    let mut ctx = ExpansionContext::new(&mut fclass, class_dict, &symbol_table, def_hash);

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

    // Resolve import aliases in all expressions (equations, component bindings, etc.)
    // This handles patterns like `L.'0'` where L is an import alias for Logic enum.
    // Done after ctx is finished so we can borrow fclass mutably again.
    let import_aliases = imports::build_import_aliases_for_class(&main_class_name, class_dict);
    if !import_aliases.is_empty() {
        let mut alias_resolver = imports::ImportAliasResolver::new(&import_aliases);
        fclass.accept_mut(&mut alias_resolver);
    }

    // Expand connect equations into simple equations
    expand_connect_equations(&mut fclass, class_dict, &pin_types)?;

    // Evaluate if-equations with parameter conditions.
    // Per MLS §8, if-equations with parameter expression conditions can have different
    // equation counts in each branch. We evaluate these at flatten time using default
    // parameter values to select the appropriate branch.
    let equations = std::mem::take(&mut fclass.equations);
    fclass.equations = evaluate_if_equations(equations, &fclass.components);

    let initial_equations = std::mem::take(&mut fclass.initial_equations);
    fclass.initial_equations = evaluate_if_equations(initial_equations, &fclass.components);

    Ok(FlattenResult {
        class: fclass,
        dependencies: deps,
    })
}

// =============================================================================
// Extends Class Type Compatibility
// =============================================================================

/// Check if a class of type `extending` can extend a class of type `base`.
///
/// MLS §7.1.1: Restrictions on class inheritance.
fn is_extends_compatible(extending: &ClassType, base: &ClassType) -> bool {
    match (extending, base) {
        // Same types can always extend each other
        (a, b) if a == b => true,

        // Model and Block are compatible (both can have equations and components)
        (ClassType::Model, ClassType::Block) | (ClassType::Block, ClassType::Model) => true,

        // Model can extend Record (composition-like inheritance)
        (ClassType::Model, ClassType::Record) | (ClassType::Block, ClassType::Record) => true,

        // Connector can extend Record or Type (for type alias patterns in MSL)
        (ClassType::Connector, ClassType::Record) | (ClassType::Connector, ClassType::Type) => true,

        // Class is the most generic and can extend/be extended by model, block, record
        (ClassType::Class, ClassType::Model)
        | (ClassType::Class, ClassType::Block)
        | (ClassType::Class, ClassType::Record)
        | (ClassType::Model, ClassType::Class)
        | (ClassType::Block, ClassType::Class)
        | (ClassType::Record, ClassType::Class) => true,

        // Function can only extend function
        (ClassType::Function, ClassType::Function) => true,

        // Connector can only extend connector
        (ClassType::Connector, ClassType::Connector) => true,

        // Package can only extend package
        (ClassType::Package, ClassType::Package) => true,

        // Operator can only extend operator
        (ClassType::Operator, ClassType::Operator) => true,

        // All other combinations are incompatible
        _ => false,
    }
}

/// Get a human-readable name for a class type.
fn class_type_name(class_type: &ClassType) -> &'static str {
    match class_type {
        ClassType::Model => "model",
        ClassType::Block => "block",
        ClassType::Class => "class",
        ClassType::Connector => "connector",
        ClassType::Record => "record",
        ClassType::Function => "function",
        ClassType::Package => "package",
        ClassType::Type => "type",
        ClassType::Operator => "operator",
    }
}

#[cfg(test)]
mod tests;
