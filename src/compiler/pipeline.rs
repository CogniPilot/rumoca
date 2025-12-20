//! Core compilation pipeline.
//!
//! This module contains the main compilation pipeline that transforms
//! a Modelica AST into a DAE representation.

use super::function_collector::collect_all_functions;
use super::result::CompilationResult;
use crate::dae::balance::BalanceResult;
use crate::ir::analysis::var_validator::VarValidator;
use crate::ir::ast::{ClassType, StoredDefinition};
use crate::ir::structural::create_dae::create_dae;
use crate::ir::transform::array_comprehension::expand_array_comprehensions;
use crate::ir::transform::constant_substitutor::ConstantSubstitutor;
use crate::ir::transform::enum_substitutor::EnumSubstitutor;
use crate::ir::transform::equation_expander::expand_equations;
use crate::ir::transform::flatten::{
    FileDependencies, flatten, flatten_with_deps, is_cache_enabled,
};
use crate::ir::transform::function_inliner::FunctionInliner;
use crate::ir::transform::import_resolver::ImportResolver;
use crate::ir::transform::tuple_expander::expand_tuple_equations;
use crate::ir::visitor::MutVisitable;
use anyhow::Result;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::{LazyLock, RwLock};

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
fn compute_dae_cache_key(model_name: &str, def: &StoredDefinition) -> String {
    use std::collections::hash_map::DefaultHasher;
    let mut hasher = DefaultHasher::new();
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

    /// Try to load DAE result from disk cache
    pub fn load_dae_from_disk(cache_key: &str) -> Option<BalanceResult> {
        let cache_dir = dae_cache_dir()?;
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
    DAE_CACHE.write().unwrap().clear();
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

    if verbose {
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
        let cache = DAE_CACHE.read().unwrap();
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
            .unwrap()
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
            .unwrap()
            .insert(cache_key, (result.clone(), fclass.dependencies));
    }

    Ok(result)
}
