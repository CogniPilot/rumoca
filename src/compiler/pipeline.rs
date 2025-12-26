//! Core compilation pipeline.
//!
//! This module contains the main compilation pipeline that transforms
//! a Modelica AST into a DAE representation.

use super::function_collector::collect_all_functions;
use super::result::CompilationResult;
use crate::dae::balance::BalanceResult;
use crate::ir::analysis::type_checker::{
    check_algorithm_assignments_with_types, check_array_bounds, check_assert_arguments,
    check_break_return_context, check_builtin_attribute_modifiers, check_cardinality_arguments,
    check_cardinality_context, check_class_member_access, check_component_bindings,
    check_scalar_subscripts, check_start_modification_dimensions,
};
use crate::ir::analysis::var_validator::VarValidator;
use crate::ir::ast::{ClassType, StoredDefinition};
use crate::ir::structural::create_dae::create_dae;
use crate::ir::transform::array_comprehension::expand_array_comprehensions;
use crate::ir::transform::constant_substitutor::ConstantSubstitutor;
use crate::ir::transform::enum_substitutor::EnumSubstitutor;
use crate::ir::transform::equation_expander::expand_equations;
use crate::ir::transform::flatten::{
    ClassDict, FileDependencies, flatten, flatten_with_deps, flatten_with_library_dicts,
    is_cache_enabled,
};
use crate::ir::transform::function_inliner::FunctionInliner;
use crate::ir::transform::import_resolver::ImportResolver;
use crate::ir::transform::operator_expand::{
    build_operator_record_map, build_type_map, expand_complex_equations,
};
use crate::ir::transform::tuple_expander::expand_tuple_equations;
use crate::ir::visitor::MutVisitable;
use anyhow::Result;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, LazyLock, RwLock};

// Use web_time on WASM for Instant::now() polyfill
#[cfg(not(target_arch = "wasm32"))]
use std::time::Instant;
#[cfg(target_arch = "wasm32")]
use web_time::Instant;

// =============================================================================
// DAE Result Cache
// =============================================================================

/// Disk cache entry for a DAE result (only needed with cache feature)
#[cfg(feature = "cache")]
#[derive(serde::Serialize, serde::Deserialize)]
struct DaeCacheEntry {
    result: BalanceResult,
    dependencies: FileDependencies,
}

/// Global in-memory cache for DAE results.
/// For WASM, this is safe because compilation runs on worker threads (not main thread)
/// which support Atomics.wait.
static DAE_CACHE: LazyLock<RwLock<HashMap<String, (BalanceResult, FileDependencies)>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/// Compute cache key for DAE result based on model name and StoredDefinition
/// Cache format version - increment when transformation logic changes
/// to invalidate stale cached results.
///
/// History:
/// - v1: Initial cache format
/// - v2: Fixed Complex binding equation expansion (operator_expand, function_inliner)
const DAE_CACHE_VERSION: u32 = 2;

/// Rumoca version at compile time - used for automatic cache invalidation
const RUMOCA_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Git version at compile time - includes commit hash for automatic invalidation on code changes
const GIT_VERSION: &str = env!("RUMOCA_GIT_VERSION");

fn compute_dae_cache_key(model_name: &str, def: &StoredDefinition) -> String {
    use std::collections::hash_map::DefaultHasher;
    let mut hasher = DefaultHasher::new();
    // Include version info to invalidate cache on compiler updates
    DAE_CACHE_VERSION.hash(&mut hasher);
    RUMOCA_VERSION.hash(&mut hasher);
    GIT_VERSION.hash(&mut hasher);
    model_name.hash(&mut hasher);
    // Hash actual content of the definition including component names and equations
    for (name, class) in &def.class_list {
        hash_class_for_cache(name, class, &mut hasher);
    }
    format!("{:016x}", hasher.finish())
}

/// Hash the content of a class definition for cache key computation
fn hash_class_for_cache(
    name: &str,
    class: &crate::ir::ast::ClassDefinition,
    hasher: &mut impl std::hash::Hasher,
) {
    // Hash class name and type
    name.hash(hasher);
    std::mem::discriminant(&class.class_type).hash(hasher);

    // Hash all component names and their type names
    for (comp_name, comp) in &class.components {
        comp_name.hash(hasher);
        comp.type_name.to_string().hash(hasher);
        std::mem::discriminant(&comp.variability).hash(hasher);
        std::mem::discriminant(&comp.causality).hash(hasher);
        comp.shape.hash(hasher);
        // Hash start value - critical for parameter-dependent array sizing
        format!("{:?}", comp.start).hash(hasher);
    }

    // Hash extends clauses
    for ext in &class.extends {
        ext.comp.to_string().hash(hasher);
    }

    // Hash equations
    class.equations.len().hash(hasher);
    for eq in &class.equations {
        format!("{:?}", eq).hash(hasher);
    }

    // Recursively hash nested classes
    for (nested_name, nested_class) in &class.classes {
        hash_class_for_cache(nested_name, nested_class, hasher);
    }
}

// Disk cache functions (require filesystem access)
#[cfg(feature = "cache")]
mod disk_cache {
    use super::*;

    /// Get the DAE cache directory (~/.cache/rumoca/dae/)
    pub fn dae_cache_dir() -> Option<std::path::PathBuf> {
        dirs::cache_dir().map(|d| d.join("rumoca").join("dae"))
    }

    /// Check and update the cache version marker.
    /// Returns true if the cache is valid, false if it was cleared due to version mismatch.
    fn check_and_update_version_marker(cache_dir: &std::path::Path) -> bool {
        let version_file = cache_dir.join(".version");
        let current_version = format!(
            "{}:{}:{}",
            super::DAE_CACHE_VERSION,
            super::RUMOCA_VERSION,
            super::GIT_VERSION
        );

        if version_file.exists()
            && let Ok(stored_version) = std::fs::read_to_string(&version_file)
            && stored_version.trim() == current_version
        {
            return true;
        }

        // Version mismatch - clear the cache directory
        if cache_dir.exists() {
            let _ = std::fs::remove_dir_all(cache_dir);
        }

        // Recreate with new version marker
        if std::fs::create_dir_all(cache_dir).is_ok() {
            let _ = std::fs::write(&version_file, &current_version);
        }

        false
    }

    /// Try to load DAE result from disk cache
    pub fn load_dae_from_disk(cache_key: &str) -> Option<BalanceResult> {
        let cache_dir = dae_cache_dir()?;

        // Check version marker - clears cache if version changed
        if !check_and_update_version_marker(&cache_dir) {
            return None;
        }

        let cache_file = cache_dir.join(format!("{}.bin", cache_key));

        if !cache_file.exists() {
            return None;
        }

        let data = std::fs::read(&cache_file).ok()?;
        let entry: DaeCacheEntry = bincode::deserialize(&data).ok()?;

        // Validate dependencies
        if !entry.dependencies.is_valid() {
            let _ = std::fs::remove_file(&cache_file);
            return None;
        }

        Some(entry.result)
    }

    /// Save DAE result to disk cache
    pub fn save_dae_to_disk(
        cache_key: &str,
        result: &BalanceResult,
        dependencies: &FileDependencies,
    ) {
        let Some(cache_dir) = dae_cache_dir() else {
            return;
        };

        // Ensure version marker is up to date
        check_and_update_version_marker(&cache_dir);

        if std::fs::create_dir_all(&cache_dir).is_err() {
            return;
        }

        let cache_file = cache_dir.join(format!("{}.bin", cache_key));

        let entry = DaeCacheEntry {
            result: result.clone(),
            dependencies: dependencies.clone(),
        };

        if let Ok(data) = bincode::serialize(&entry) {
            let _ = std::fs::write(&cache_file, data);
        }
    }

    /// Clear the DAE disk cache
    pub fn clear_disk_cache() {
        if let Some(cache_dir) = dae_cache_dir() {
            let _ = std::fs::remove_dir_all(&cache_dir);
        }
    }
}

/// Clear the DAE cache (memory and disk)
pub fn clear_dae_cache() {
    DAE_CACHE.write().expect("DAE cache lock poisoned").clear();
    #[cfg(feature = "cache")]
    disk_cache::clear_disk_cache();
}

/// Run the compilation pipeline on a parsed AST.
///
/// This function takes a parsed StoredDefinition and performs:
/// 1. Flattening - resolve class hierarchy
/// 2. Import resolution - rewrite short names to fully qualified
/// 3. Constant substitution - replace Modelica.Constants with literal values
/// 4. Variable validation - check for undefined variables
/// 5. Function inlining - inline user-defined functions
/// 6. Tuple expansion - expand tuple equations
/// 7. DAE creation - create the final DAE representation
/// 8. Balance checking - verify equations match unknowns
pub fn compile_from_ast(
    def: StoredDefinition,
    model_name: Option<&str>,
    model_hash: String,
    parse_time: std::time::Duration,
    verbose: bool,
) -> Result<CompilationResult> {
    compile_from_ast_ref(&def, model_name, model_hash, parse_time, verbose)
}

/// Run the compilation pipeline on a reference to a parsed AST.
///
/// This is more efficient when compiling many models from the same AST
/// because it avoids cloning the StoredDefinition for each compilation.
/// The def is only cloned once at the end when storing in the result.
pub fn compile_from_ast_ref(
    def: &StoredDefinition,
    model_name: Option<&str>,
    model_hash: String,
    parse_time: std::time::Duration,
    verbose: bool,
) -> Result<CompilationResult> {
    // Flatten
    let flatten_start = Instant::now();
    let fclass_result = flatten(def, model_name);

    // Handle flatten errors - return raw error message (miette formatting at CLI only)
    let mut fclass = match fclass_result {
        Ok(fc) => fc,
        Err(e) => {
            return Err(e);
        }
    };
    let flatten_time = flatten_start.elapsed();

    if verbose {
        println!("Flattening took {} ms", flatten_time.as_millis());
        println!("Flattened class:\n{:#?}\n", fclass);
    }

    // Resolve imports - rewrite short function names to fully qualified names
    // This must happen before validation so imported names are recognized
    let mut import_resolver = ImportResolver::new(&fclass, def);
    fclass.accept_mut(&mut import_resolver);

    // Clone the expanded class after import resolution for semantic analysis
    // This is the ideal state for checking undefined/unused variables
    let expanded_class = fclass.clone();

    // Substitute Modelica.Constants with their literal values
    // This must happen after import resolution and before validation
    let mut const_substitutor = ConstantSubstitutor::new();
    fclass.accept_mut(&mut const_substitutor);

    // Substitute built-in enumeration values (StateSelect.prefer -> 4, etc.)
    let mut enum_substitutor = EnumSubstitutor::new();
    fclass.accept_mut(&mut enum_substitutor);

    // Collect all function names from the stored definition (including nested)
    let function_names = collect_all_functions(def);

    // Skip validation for packages and classes with nested functions
    // (function parameters aren't yet properly scoped)
    let has_nested_functions = fclass
        .classes
        .values()
        .any(|c| c.class_type == ClassType::Function);
    let should_validate = !matches!(fclass.class_type, ClassType::Package) && !has_nested_functions;

    if should_validate {
        // Collect peer class names for cross-class type references
        let peer_class_names: Vec<String> = def.class_list.keys().cloned().collect();

        // Validate variable references (passing function names and peer class names)
        let mut validator = VarValidator::with_context(&fclass, &function_names, &peer_class_names);
        fclass.accept_mut(&mut validator);

        if !validator.undefined_vars.is_empty() {
            // Return raw error (miette formatting at CLI only)
            let (var_name, context) = &validator.undefined_vars[0];
            return Err(anyhow::anyhow!(
                "Undefined variable '{}' in {}",
                var_name,
                context
            ));
        }

        // Check component binding types (e.g., Real x = "wrong" should fail)
        // Use expanded_class which is captured before enum substitution, so enum
        // literals like AssertionLevel.warning are still typed correctly
        let type_check_result = check_component_bindings(&expanded_class);
        if type_check_result.has_errors() {
            let first_error = &type_check_result.errors[0];
            return Err(anyhow::anyhow!("{}", first_error.message));
        }

        // Check for invalid assignments in algorithm sections
        // (e.g., assigning to constant, input, model, or package variables)
        // Build peer class types from file-level classes
        let peer_class_types: std::collections::HashMap<String, ClassType> = def
            .class_list
            .iter()
            .map(|(name, class)| (name.clone(), class.class_type.clone()))
            .collect();
        // Also build original component types from the target model (before flattening)
        // This is needed because composite components like `a1` of type `A` are expanded
        // to `a1.x` after flattening, but the algorithm still references `a1`
        let original_comp_types: std::collections::HashMap<String, String> = if let Some(name) =
            model_name
        {
            def.class_list
                .get(name)
                .map(|class| {
                    class
                        .components
                        .iter()
                        .map(|(comp_name, comp)| (comp_name.clone(), comp.type_name.to_string()))
                        .collect()
                })
                .unwrap_or_default()
        } else {
            std::collections::HashMap::new()
        };
        let assign_check_result = check_algorithm_assignments_with_types(
            &expanded_class,
            &peer_class_types,
            &original_comp_types,
        );
        if assign_check_result.has_errors() {
            let first_error = &assign_check_result.errors[0];
            return Err(anyhow::anyhow!("{}", first_error.message));
        }
        // Also check file-level functions for input assignment violations
        // Only check functions (not models/connectors) to avoid false positives
        for (_, file_class) in &def.class_list {
            if file_class.class_type == ClassType::Function {
                let file_assign_result = check_algorithm_assignments_with_types(
                    file_class,
                    &peer_class_types,
                    &std::collections::HashMap::new(),
                );
                if file_assign_result.has_errors() {
                    let first_error = &file_assign_result.errors[0];
                    return Err(anyhow::anyhow!("{}", first_error.message));
                }
            }
        }

        // Check assert() function argument types
        let assert_check_result = check_assert_arguments(&expanded_class);
        if assert_check_result.has_errors() {
            let first_error = &assert_check_result.errors[0];
            return Err(anyhow::anyhow!("{}", first_error.message));
        }

        // Check builtin attribute modifiers (e.g., Real x(y=1) is invalid)
        let modifier_check_result = check_builtin_attribute_modifiers(&expanded_class);
        if modifier_check_result.has_errors() {
            let first_error = &modifier_check_result.errors[0];
            return Err(anyhow::anyhow!("{}", first_error.message));
        }

        // Check start modification dimension mismatches
        // E.g., `Real x[3](start = x_start)` where x_start is a 2-element array
        let start_dim_result = check_start_modification_dimensions(&expanded_class);
        if start_dim_result.has_errors() {
            let first_error = &start_dim_result.errors[0];
            return Err(anyhow::anyhow!("{}", first_error.message));
        }

        // Check for break/return statements outside proper context
        // Check both the model and all file-level classes (including functions)
        let break_check_result = check_break_return_context(&expanded_class);
        if break_check_result.has_errors() {
            let first_error = &break_check_result.errors[0];
            return Err(anyhow::anyhow!("{}", first_error.message));
        }
        // Also check file-level functions
        for (_, class) in &def.class_list {
            let file_break_result = check_break_return_context(class);
            if file_break_result.has_errors() {
                let first_error = &file_break_result.errors[0];
                return Err(anyhow::anyhow!("{}", first_error.message));
            }
        }

        // Check for subscripting scalar variables (like time[2])
        let scalar_subscript_result = check_scalar_subscripts(&expanded_class);
        if scalar_subscript_result.has_errors() {
            let first_error = &scalar_subscript_result.errors[0];
            return Err(anyhow::anyhow!("{}", first_error.message));
        }

        // Check for cardinality() used outside valid contexts
        let cardinality_result = check_cardinality_context(&expanded_class);
        if cardinality_result.has_errors() {
            let first_error = &cardinality_result.errors[0];
            return Err(anyhow::anyhow!("{}", first_error.message));
        }

        // Check for invalid cardinality() arguments
        let cardinality_args_result = check_cardinality_arguments(&expanded_class);
        if cardinality_args_result.has_errors() {
            let first_error = &cardinality_args_result.errors[0];
            return Err(anyhow::anyhow!("{}", first_error.message));
        }

        // Check for class member access without instance (e.g., Boolean.x where Boolean is a class)
        let class_member_result = check_class_member_access(&expanded_class);
        if class_member_result.has_errors() {
            let first_error = &class_member_result.errors[0];
            return Err(anyhow::anyhow!("{}", first_error.message));
        }
    }

    // Inline user-defined function calls
    let mut inliner = FunctionInliner::from_class_list(&def.class_list);
    fclass.accept_mut(&mut inliner);
    drop(inliner); // Drop before cloning def

    // Expand tuple equations like (a, b) = (expr1, expr2) into separate equations
    expand_tuple_equations(&mut fclass);

    // Expand array comprehensions like {expr for i in 1:n} into explicit arrays
    expand_array_comprehensions(&mut fclass);

    // Expand structured equations to scalar form:
    // - For-loops expanded to individual equations
    // - Array equations expanded to element equations
    // - Binding equations converted to regular equations
    expand_equations(&mut fclass);

    // Check for array subscript out of bounds errors
    // This must happen after expand_equations so shapes are evaluated
    if should_validate {
        let bounds_check_result = check_array_bounds(&fclass);
        if bounds_check_result.has_errors() {
            let first_error = &bounds_check_result.errors[0];
            return Err(anyhow::anyhow!("{}", first_error.message));
        }
    }

    // Expand Complex arithmetic equations to scalar form:
    // - y = a + b  =>  y.re = a.re + b.re; y.im = a.im + b.im
    // - y = a * b  =>  y.re = a.re*b.re - a.im*b.im; y.im = a.re*b.im + a.im*b.re
    let operator_records = build_operator_record_map(&def.class_list);
    let type_map = build_type_map(&fclass, &def.class_list);

    if verbose {
        println!("=== Complex expansion debug ===");
        println!(
            "Operator records: {:?}",
            operator_records.keys().collect::<Vec<_>>()
        );
        println!(
            "Type map entries: {:?}",
            type_map.keys().collect::<Vec<_>>()
        );
    }

    let eq_count_before = fclass.equations.len();
    fclass.equations = expand_complex_equations(&fclass.equations, &type_map, &operator_records);

    if verbose {
        println!(
            "Equations: {} -> {}",
            eq_count_before,
            fclass.equations.len()
        );
        println!(
            "After function inlining, tuple expansion, array comprehension, and equation expansion:\n{:#?}\n",
            fclass
        );
    }

    // Create DAE
    let dae_start = Instant::now();
    let mut dae = create_dae(&mut fclass)?;
    dae.model_hash = model_hash.clone();
    let dae_time = dae_start.elapsed();

    if verbose {
        println!("DAE creation took {} ms", dae_time.as_millis());
        println!("DAE:\n{:#?}\n", dae);
    }

    // Check model balance
    let balance = dae.check_balance();

    if verbose {
        println!("{}", balance.status_message());
    }

    Ok(CompilationResult {
        dae,
        def: def.clone(), // Clone only at the end for result storage
        expanded_class,
        parse_time,
        flatten_time,
        dae_time,
        model_hash,
        balance,
    })
}

/// Run a lightweight compilation that only returns the balance check result.
///
/// This is much faster than full compilation when you only need to check
/// if a model is balanced, as it avoids cloning the StoredDefinition.
/// Results are cached to disk for fast lookups across compiler restarts.
pub fn check_balance_only(
    def: &StoredDefinition,
    model_name: Option<&str>,
) -> Result<crate::dae::balance::BalanceResult> {
    let model_name_str = model_name.unwrap_or("");

    let cache_key = compute_dae_cache_key(model_name_str, def);

    // Check in-memory cache first (only if caching is enabled)
    if is_cache_enabled() {
        let cache = DAE_CACHE.read().expect("DAE cache lock poisoned");
        if let Some((result, _deps)) = cache.get(&cache_key) {
            return Ok(result.clone());
        }
    }

    // Check disk cache second (native only - no filesystem in WASM, only if caching enabled)
    #[cfg(all(feature = "cache", not(target_arch = "wasm32")))]
    if is_cache_enabled()
        && let Some(result) = disk_cache::load_dae_from_disk(&cache_key)
    {
        // Populate in-memory cache
        let deps = FileDependencies::default(); // We validated deps during load
        DAE_CACHE
            .write()
            .expect("DAE cache lock poisoned")
            .insert(cache_key, (result.clone(), deps));
        return Ok(result);
    }

    // Cache miss - compute balance with dependency tracking
    let flatten_result = flatten_with_deps(def, model_name);

    let mut fclass = match flatten_result {
        Ok(fr) => fr,
        Err(e) => {
            return Err(anyhow::anyhow!("Flatten error: {}", e));
        }
    };

    // Skip import resolution and validation for speed - we just need balance

    // But we do need constant substitution for Modelica.Constants
    let mut const_substitutor = ConstantSubstitutor::new();
    fclass.class.accept_mut(&mut const_substitutor);

    // Substitute built-in enumeration values (StateSelect.prefer -> 4, etc.)
    let mut enum_substitutor = EnumSubstitutor::new();
    fclass.class.accept_mut(&mut enum_substitutor);

    // Inline user-defined function calls
    let mut inliner = FunctionInliner::from_class_list(&def.class_list);
    fclass.class.accept_mut(&mut inliner);
    drop(inliner);

    // Expand tuple equations
    expand_tuple_equations(&mut fclass.class);

    // Expand array comprehensions
    expand_array_comprehensions(&mut fclass.class);

    // Expand structured equations to scalar form
    expand_equations(&mut fclass.class);

    // Expand Complex arithmetic equations to scalar form
    let operator_records = build_operator_record_map(&def.class_list);
    let type_map = build_type_map(&fclass.class, &def.class_list);
    fclass.class.equations =
        expand_complex_equations(&fclass.class.equations, &type_map, &operator_records);

    // Create DAE
    let dae = create_dae(&mut fclass.class)?;

    // Check model balance
    let result = dae.check_balance();

    // Save to caches (only if caching is enabled)
    if is_cache_enabled() {
        #[cfg(all(feature = "cache", not(target_arch = "wasm32")))]
        disk_cache::save_dae_to_disk(&cache_key, &result, &fclass.dependencies);

        DAE_CACHE
            .write()
            .expect("DAE cache lock poisoned")
            .insert(cache_key, (result.clone(), fclass.dependencies));
    }

    Ok(result)
}

/// Run a lightweight compilation with pre-built library class dictionaries.
///
/// This is optimized for LSP use where libraries are loaded once and their
/// class dictionaries are cached. Instead of merging StoredDefinitions
/// (which clones all class definitions), this uses pre-built dictionaries
/// that can be combined cheaply by cloning Arc references.
///
/// # Performance
///
/// This function achieves ~6-8ms compile times after the initial library
/// dictionary build (~1 second). This is critical for responsive LSP
/// diagnostics during editing.
///
/// # Arguments
///
/// * `user_def` - The user's parsed Modelica source
/// * `library_dicts` - Pre-built class dictionaries for libraries (e.g., MSL)
/// * `model_name` - Name of the model to compile and check balance for
///
/// # Returns
///
/// A `BalanceResult` indicating whether the model has matching equations/unknowns
pub fn check_balance_with_library_dicts(
    user_def: &StoredDefinition,
    library_dicts: &[Arc<ClassDict>],
    model_name: Option<&str>,
) -> Result<crate::dae::balance::BalanceResult> {
    // Flatten using pre-built library dictionaries (avoids expensive merge)
    let flatten_result = flatten_with_library_dicts(user_def, library_dicts, model_name);

    let mut fclass = match flatten_result {
        Ok(fr) => fr,
        Err(e) => {
            return Err(anyhow::anyhow!("Flatten error: {}", e));
        }
    };

    // Skip import resolution and validation for speed - we just need balance

    // But we do need constant substitution for Modelica.Constants
    let mut const_substitutor = ConstantSubstitutor::new();
    fclass.class.accept_mut(&mut const_substitutor);

    // Substitute built-in enumeration values (StateSelect.prefer -> 4, etc.)
    let mut enum_substitutor = EnumSubstitutor::new();
    fclass.class.accept_mut(&mut enum_substitutor);

    // Inline user-defined function calls
    let mut inliner = FunctionInliner::from_class_list(&user_def.class_list);
    fclass.class.accept_mut(&mut inliner);
    drop(inliner);

    // Expand tuple equations
    expand_tuple_equations(&mut fclass.class);

    // Expand array comprehensions
    expand_array_comprehensions(&mut fclass.class);

    // Expand structured equations to scalar form
    expand_equations(&mut fclass.class);

    // Expand Complex arithmetic equations to scalar form
    let operator_records = build_operator_record_map(&user_def.class_list);
    let type_map = build_type_map(&fclass.class, &user_def.class_list);
    fclass.class.equations =
        expand_complex_equations(&fclass.class.equations, &type_map, &operator_records);

    // Create DAE
    let dae = create_dae(&mut fclass.class)?;

    // Check model balance
    Ok(dae.check_balance())
}
