//! High-level API for compiling Modelica models to DAE representations.
//!
//! This module provides a clean, ergonomic interface for using rumoca as a library.
//! The main entry point is the [`Compiler`] struct, which uses a builder pattern
//! for configuration.
//!
//! # Examples
//!
//! Basic usage:
//!
//! ```no_run
//! use rumoca::Compiler;
//!
//! let result = Compiler::new()
//!     .model("MyModel")
//!     .compile_file("model.mo")?;
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! With verbose output and template rendering:
//!
//! ```no_run
//! use rumoca::Compiler;
//!
//! let output = Compiler::new()
//!     .model("MyModel")
//!     .verbose(true)
//!     .compile_file("model.mo")?
//!     .render_template("template.j2")?;
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! Compiling from a string:
//!
//! ```no_run
//! use rumoca::Compiler;
//!
//! let modelica_code = r#"
//!     model Integrator
//!         Real x(start=0);
//!     equation
//!         der(x) = 1;
//!     end Integrator;
//! "#;
//!
//! let result = Compiler::new()
//!     .model("Integrator")
//!     .compile_str(modelica_code, "Integrator.mo")?;
//! # Ok::<(), anyhow::Error>(())
//! ```

pub mod cache;
mod error_handling;
mod function_collector;
pub mod pipeline;
mod result;

pub use error_handling::extract_parse_error;
pub use result::CompilationResult;

use crate::ir::ast::StoredDefinition;
use crate::modelica_grammar::ModelicaGrammar;
use crate::modelica_parser::parse;
use anyhow::{Context, Result};
use error_handling::create_syntax_error;
use indexmap::IndexSet;
#[cfg(not(target_arch = "wasm32"))]
use rayon::prelude::*;
use std::fs;
use std::path::{Path, PathBuf};

// Use web_time on WASM for Instant::now() polyfill
#[cfg(not(target_arch = "wasm32"))]
use std::time::Instant;
#[cfg(target_arch = "wasm32")]
use web_time::Instant;

// ============================================================================
// Standalone parsing functions (used by both compiler and LSP)
// ============================================================================

/// Parse Modelica source code and return the AST.
///
/// This is a low-level parsing function that returns `None` on parse errors.
/// For error details, use [`parse_source`] instead.
///
/// # Arguments
/// * `source` - The Modelica source code
/// * `file_name` - The file name (used for error messages and location tracking)
///
/// # Returns
/// `Some(StoredDefinition)` if parsing succeeded, `None` otherwise.
pub fn parse_source_simple(source: &str, file_name: &str) -> Option<StoredDefinition> {
    let mut grammar = ModelicaGrammar::new();
    if parse(source, file_name, &mut grammar).is_ok() {
        grammar.modelica
    } else {
        None
    }
}

/// Parse Modelica source code and return the AST with detailed errors.
///
/// # Arguments
/// * `source` - The Modelica source code
/// * `file_name` - The file name (used for error messages and location tracking)
///
/// # Returns
/// `Ok(StoredDefinition)` if parsing succeeded, `Err` with detailed error otherwise.
pub fn parse_source(source: &str, file_name: &str) -> Result<StoredDefinition> {
    let mut grammar = ModelicaGrammar::new();
    if let Err(e) = parse(source, file_name, &mut grammar) {
        let diagnostic = create_syntax_error(&e, source);
        let report = miette::Report::new(diagnostic);
        return Err(anyhow::anyhow!("{:?}", report));
    }

    grammar
        .modelica
        .ok_or_else(|| anyhow::anyhow!("Parser succeeded but produced no AST for {}", file_name))
}

/// Parse a Modelica file from disk, using disk cache if available.
///
/// This function checks the AST cache (`~/.cache/rumoca/ast/`) first.
/// If the file hasn't changed since the last parse, the cached AST is returned.
/// Otherwise, the file is parsed and the result is cached for future use.
///
/// # Arguments
/// * `path` - Path to the Modelica file
///
/// # Returns
/// `Some(StoredDefinition)` if parsing succeeded, `None` otherwise.
pub fn parse_file_cached(path: &Path) -> Option<StoredDefinition> {
    // Compute file hash for cache lookup
    let file_hash = cache::compute_file_hash(path).ok()?;

    // Try cache first
    if let Some(ast) = cache::load_cached_ast(path, &file_hash) {
        return Some(ast);
    }

    // Cache miss - read and parse the file
    let text = fs::read_to_string(path).ok()?;
    let path_str = path.to_string_lossy().to_string();

    let ast = parse_source_simple(&text, &path_str)?;

    // Store in cache for next time
    let _ = cache::store_cached_ast(path, &file_hash, &ast);

    Some(ast)
}

/// Parse a Modelica file from disk with detailed errors, using disk cache if available.
///
/// Like [`parse_file_cached`] but returns detailed error information on failure.
///
/// # Arguments
/// * `path` - Path to the Modelica file
///
/// # Returns
/// `Ok(StoredDefinition)` if parsing succeeded, `Err` with detailed error otherwise.
pub fn parse_file_cached_result(path: &Path) -> Result<StoredDefinition> {
    // Compute file hash for cache lookup
    let file_hash = cache::compute_file_hash(path)?;

    // Try cache first
    if let Some(ast) = cache::load_cached_ast(path, &file_hash) {
        return Ok(ast);
    }

    // Cache miss - read and parse the file
    let text = fs::read_to_string(path)
        .with_context(|| format!("Failed to read file: {}", path.display()))?;
    let path_str = path.to_string_lossy().to_string();

    let ast = parse_source(&text, &path_str)?;

    // Store in cache for next time
    let _ = cache::store_cached_ast(path, &file_hash, &ast);

    Ok(ast)
}

/// A high-level compiler for Modelica models.
///
/// This struct provides a builder-pattern interface for configuring and executing
/// the compilation pipeline from Modelica source code to DAE representation.
///
/// # Examples
///
/// ```no_run
/// use rumoca::Compiler;
///
/// let result = Compiler::new()
///     .model("MyModel")
///     .verbose(true)
///     .compile_file("model.mo")?;
/// # Ok::<(), anyhow::Error>(())
/// ```
#[derive(Debug, Clone)]
pub struct Compiler {
    verbose: bool,
    /// Main model/class name to simulate (required)
    model_name: Option<String>,
    /// Additional source files to include in compilation (deduplicated by canonical path)
    additional_files: IndexSet<PathBuf>,
    /// Explicit library paths (overrides MODELICAPATH env var if set)
    modelica_paths: Vec<std::path::PathBuf>,
    /// Number of threads for parallel parsing (None = 50% of cores)
    threads: Option<usize>,
    /// Enable AST caching for faster library loading (default: true)
    use_cache: bool,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            verbose: false,
            model_name: None,
            additional_files: IndexSet::new(),
            modelica_paths: Vec::new(),
            threads: None,   // Will use 50% of cores
            use_cache: true, // Enable caching by default
        }
    }
}

impl Compiler {
    /// Creates a new compiler with default settings.
    ///
    /// # Examples
    ///
    /// ```
    /// use rumoca::Compiler;
    ///
    /// let compiler = Compiler::new();
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Enables or disables verbose output during compilation.
    ///
    /// When enabled, the compiler will print timing information and intermediate
    /// representations to stdout.
    ///
    /// # Examples
    ///
    /// ```
    /// use rumoca::Compiler;
    ///
    /// let compiler = Compiler::new().verbose(true);
    /// ```
    pub fn verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Sets the main model/class name to simulate (required).
    ///
    /// According to the Modelica specification, the user must specify which
    /// class (of specialized class `model` or `block`) to simulate.
    ///
    /// # Examples
    ///
    /// ```
    /// use rumoca::Compiler;
    ///
    /// let compiler = Compiler::new().model("MyModel");
    /// ```
    pub fn model(mut self, name: &str) -> Self {
        self.model_name = Some(name.to_string());
        self
    }

    /// Sets explicit library search paths (overrides MODELICAPATH environment variable).
    ///
    /// This allows specifying library paths programmatically without relying on
    /// environment variables, which is useful for testing and reproducible builds.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("Modelica.Blocks.Examples.PID_Controller")
    ///     .modelica_path(&["/path/to/MSL", "/path/to/other/libs"])
    ///     .include_from_modelica_path("Modelica")?
    ///     .compile_file("model.mo")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn modelica_path(mut self, paths: &[&str]) -> Self {
        self.modelica_paths = paths.iter().map(std::path::PathBuf::from).collect();
        self
    }

    /// Sets the number of threads for parallel file parsing.
    ///
    /// By default, uses 50% of available CPU cores to leave resources for other tasks.
    /// Set to 1 for single-threaded parsing, or a higher number to use more cores.
    ///
    /// # Examples
    ///
    /// ```
    /// use rumoca::Compiler;
    ///
    /// // Use all available cores
    /// let compiler = Compiler::new().threads(num_cpus::get());
    ///
    /// // Use single-threaded parsing
    /// let compiler = Compiler::new().threads(1);
    /// ```
    pub fn threads(mut self, threads: usize) -> Self {
        self.threads = Some(threads.max(1));
        self
    }

    /// Returns the number of threads to use for parallel parsing.
    /// Defaults to (num_cpus - 1) to leave one core free for the system, minimum 1.
    ///
    /// If called from within an existing rayon thread pool (nested parallelism),
    /// returns 1 to avoid thread explosion.
    #[cfg(not(target_arch = "wasm32"))]
    fn get_thread_count(&self) -> usize {
        // If explicitly set, use that value
        if let Some(threads) = self.threads {
            return threads;
        }

        // Detect if we're already inside a rayon pool (nested parallelism)
        // rayon::current_num_threads() returns 1 when not in a pool
        let in_pool = rayon::current_num_threads() > 1;
        if in_pool {
            // We're inside a parallel context - use single thread to avoid explosion
            return 1;
        }

        // Default: use num_cpus - 1, minimum 1
        std::cmp::max(1, num_cpus::get().saturating_sub(1))
    }

    /// Enables or disables AST caching.
    ///
    /// When enabled (default), parsed ASTs are cached to `~/.cache/rumoca/ast/`
    /// to speed up subsequent compilations of the same files.
    ///
    /// # Examples
    ///
    /// ```
    /// use rumoca::Compiler;
    ///
    /// // Disable caching
    /// let compiler = Compiler::new().cache(false);
    /// ```
    pub fn cache(mut self, enable: bool) -> Self {
        self.use_cache = enable;
        self
    }

    /// Adds an additional source file to include in compilation.
    ///
    /// Use this to include library files, package definitions, or other
    /// dependencies that the main model requires. Files are automatically
    /// deduplicated by canonical path.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyModel")
    ///     .include("library/utils.mo")
    ///     .include("library/types.mo")
    ///     .compile_file("model.mo")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn include(mut self, path: &str) -> Self {
        let path_buf = PathBuf::from(path);
        // Canonicalize to detect duplicates (same file via different paths)
        let canonical = path_buf.canonicalize().unwrap_or(path_buf);
        self.additional_files.insert(canonical);
        self
    }

    /// Adds multiple source files to include in compilation.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyModel")
    ///     .include_all(&["lib1.mo", "lib2.mo"])
    ///     .compile_file("model.mo")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn include_all(mut self, paths: &[&str]) -> Self {
        for path in paths {
            self = self.include(path);
        }
        self
    }

    /// Includes a Modelica package directory in compilation.
    ///
    /// This method discovers all Modelica files in a package directory structure,
    /// following Modelica Spec 13.4 conventions:
    /// - Directories with `package.mo` are treated as packages
    /// - `package.order` files specify the order of nested entities
    /// - Single `.mo` files define classes
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyPackage.MyModel")
    ///     .include_package("path/to/MyPackage")?
    ///     .compile_file("model.mo")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn include_package(mut self, path: &str) -> Result<Self> {
        use crate::ir::transform::multi_file::discover_modelica_files;

        let package_path = std::path::Path::new(path);
        let files = discover_modelica_files(package_path)?;

        for file in files {
            // Canonicalize to detect duplicates
            let canonical = file.canonicalize().unwrap_or(file);
            self.additional_files.insert(canonical);
        }

        Ok(self)
    }

    /// Includes a package from MODELICAPATH by name.
    ///
    /// This method searches the library paths for a package with the given name
    /// and includes all its files. If explicit paths were set via `.modelica_path()`,
    /// those are used; otherwise, the MODELICAPATH environment variable is used.
    ///
    /// According to Modelica Spec 13.3, MODELICAPATH is an ordered list of library
    /// root directories, separated by `:` on Unix or `;` on Windows.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// // Using explicit paths (recommended for reproducible builds)
    /// let result = Compiler::new()
    ///     .model("Modelica.Mechanics.Rotational.Examples.First")
    ///     .modelica_path(&["/path/to/MSL"])
    ///     .include_from_modelica_path("Modelica")?
    ///     .compile_file("model.mo")?;
    ///
    /// // Or using MODELICAPATH environment variable
    /// let result = Compiler::new()
    ///     .model("Modelica.Mechanics.Rotational.Examples.First")
    ///     .include_from_modelica_path("Modelica")?
    ///     .compile_file("model.mo")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn include_from_modelica_path(self, package_name: &str) -> Result<Self> {
        use crate::ir::transform::multi_file::{find_package_in_paths, get_modelica_path};

        // Use explicit paths if set, otherwise fall back to env var
        let search_paths = if self.modelica_paths.is_empty() {
            get_modelica_path()
        } else {
            self.modelica_paths.clone()
        };

        let package_path = find_package_in_paths(package_name, &search_paths).ok_or_else(|| {
            anyhow::anyhow!(
                "Package '{}' not found in library paths: {:?}",
                package_name,
                search_paths
            )
        })?;

        self.include_package(&package_path.to_string_lossy())
    }

    /// Compiles a Modelica package directory directly.
    ///
    /// This method discovers all files in a package directory structure and
    /// compiles them together. The main model to simulate is specified via `.model()`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyPackage.MyModel")
    ///     .compile_package("path/to/MyPackage")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    #[cfg(not(target_arch = "wasm32"))]
    pub fn compile_package(&self, path: &str) -> Result<CompilationResult> {
        use crate::ir::transform::multi_file::discover_modelica_files;

        let package_path = std::path::Path::new(path);
        let files = discover_modelica_files(package_path)?;

        if files.is_empty() {
            anyhow::bail!("No Modelica files found in package: {}", path);
        }

        let file_strs: Vec<&str> = files.iter().map(|p| p.to_str().unwrap()).collect();
        self.compile_files(&file_strs)
    }

    /// Compiles a Modelica file to a DAE representation.
    ///
    /// This method performs the full compilation pipeline:
    /// 1. Reads the file from disk
    /// 2. Parses the Modelica code into an AST
    /// 3. Flattens the hierarchical class structure
    /// 4. Converts to DAE representation
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the Modelica file to compile
    ///
    /// # Returns
    ///
    /// A [`CompilationResult`] containing the DAE and metadata
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The file cannot be read
    /// - The Modelica code contains syntax errors
    /// - The model contains unsupported features (e.g., unexpanded connection equations)
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyModel")
    ///     .compile_file("model.mo")?;
    /// println!("Model has {} states", result.dae.x.len());
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    #[cfg(not(target_arch = "wasm32"))]
    pub fn compile_file(&self, path: &str) -> Result<CompilationResult> {
        use std::sync::atomic::{AtomicUsize, Ordering};

        // Configure thread pool
        let thread_count = self.get_thread_count();
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(thread_count)
            .build()
            .with_context(|| "Failed to create thread pool")?;

        let cache_hits = AtomicUsize::new(0);
        let cache_misses = AtomicUsize::new(0);

        if self.verbose {
            println!(
                "Parsing {} files using {} threads (cache: {})...",
                self.additional_files.len() + 1,
                thread_count,
                if self.use_cache {
                    "enabled"
                } else {
                    "disabled"
                }
            );
        }

        // Parse additional files in parallel (with caching)
        // Returns (path, definition, file_hash) for each file
        let additional_paths: Vec<_> = self.additional_files.iter().collect();
        let use_cache = self.use_cache;
        let parsed_additional: Result<Vec<_>> = pool.install(|| {
            additional_paths
                .par_iter()
                .map(|additional_path| {
                    let path_str = additional_path.to_string_lossy().to_string();

                    // Compute hash for this file (used for both AST cache and flat cache key)
                    let file_hash = cache::compute_file_hash(additional_path)
                        .unwrap_or_else(|_| "unknown".to_string());

                    // Try cache first
                    if use_cache
                        && let Some(cached_def) =
                            cache::load_cached_ast(additional_path, &file_hash)
                    {
                        cache_hits.fetch_add(1, Ordering::Relaxed);
                        return Ok((path_str, cached_def, file_hash));
                    }

                    // Cache miss - parse the file
                    cache_misses.fetch_add(1, Ordering::Relaxed);
                    let additional_source = fs::read_to_string(additional_path)
                        .with_context(|| format!("Failed to read file: {}", path_str))?;
                    let def = self.parse_source(&additional_source, &path_str)?;

                    // Store in cache
                    if use_cache {
                        let _ = cache::store_cached_ast(additional_path, &file_hash, &def);
                    }

                    Ok((path_str, def, file_hash))
                })
                .collect()
        });

        let additional_results = parsed_additional?;

        if self.verbose {
            println!(
                "Cache: {} hits, {} misses",
                cache_hits.load(Ordering::Relaxed),
                cache_misses.load(Ordering::Relaxed)
            );
        }

        // Parse main file (not cached - it's the user's code that changes frequently)
        let input =
            fs::read_to_string(path).with_context(|| format!("Failed to read file: {}", path))?;
        let main_hash = format!("{:x}", chksum_md5::hash(&input));

        let main_def = self.parse_source(&input, path)?;

        // Collect all definitions and hashes
        let mut all_definitions: Vec<(String, StoredDefinition)> = additional_results
            .iter()
            .map(|(p, d, _)| (p.clone(), d.clone()))
            .collect();
        all_definitions.push((path.to_string(), main_def));

        let mut all_hashes: Vec<String> =
            additional_results.into_iter().map(|(_, _, h)| h).collect();
        all_hashes.push(main_hash);

        // Compile with all definitions and hashes for flat class caching
        self.compile_definitions_with_hashes(all_definitions, &input, path, Some(all_hashes))
    }

    /// Compiles multiple Modelica files together.
    ///
    /// This method compiles multiple files, merging their class definitions
    /// before flattening. The main model to simulate is specified via `.model()`.
    ///
    /// # Arguments
    ///
    /// * `paths` - Paths to the Modelica files to compile
    ///
    /// # Returns
    ///
    /// A [`CompilationResult`] containing the DAE and metadata
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyPackage.MyModel")
    ///     .compile_files(&["library.mo", "model.mo"])?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    #[cfg(not(target_arch = "wasm32"))]
    pub fn compile_files(&self, paths: &[&str]) -> Result<CompilationResult> {
        if paths.is_empty() {
            anyhow::bail!("At least one file must be provided");
        }

        // Configure thread pool
        let thread_count = self.get_thread_count();
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(thread_count)
            .build()
            .with_context(|| "Failed to create thread pool")?;

        if self.verbose {
            println!(
                "Parsing {} files using {} threads...",
                paths.len(),
                thread_count
            );
        }

        // Parse all files in parallel
        let parsed_results: Result<Vec<_>> = pool.install(|| {
            paths
                .par_iter()
                .map(|path| {
                    let source = fs::read_to_string(path)
                        .with_context(|| format!("Failed to read file: {}", path))?;
                    let def = self.parse_source(&source, path)?;
                    Ok((path.to_string(), def, source))
                })
                .collect()
        });

        let results = parsed_results?;

        // Separate definitions and sources
        let all_definitions: Vec<_> = results
            .iter()
            .map(|(path, def, _)| (path.clone(), def.clone()))
            .collect();
        let main_source = &results.last().unwrap().2;
        let main_path = &results.last().unwrap().0;

        self.compile_definitions(all_definitions, main_source, main_path)
    }

    /// Parse a source file and return the StoredDefinition
    fn parse_source(&self, source: &str, file_name: &str) -> Result<StoredDefinition> {
        let mut grammar = ModelicaGrammar::new();
        if let Err(e) = parse(source, file_name, &mut grammar) {
            let diagnostic = create_syntax_error(&e, source);
            let report = miette::Report::new(diagnostic);
            return Err(anyhow::anyhow!("{:?}", report));
        }

        grammar.modelica.ok_or_else(|| {
            anyhow::anyhow!("Parser succeeded but produced no AST for {}", file_name)
        })
    }

    /// Compile from pre-parsed definitions (public for WASM caching)
    pub fn compile_definitions(
        &self,
        definitions: Vec<(String, StoredDefinition)>,
        main_source: &str,
        _main_file_name: &str,
    ) -> Result<CompilationResult> {
        self.compile_definitions_with_hashes(definitions, main_source, _main_file_name, None)
    }

    /// Compile from pre-parsed definitions with optional source hashes for flat class caching
    fn compile_definitions_with_hashes(
        &self,
        definitions: Vec<(String, StoredDefinition)>,
        main_source: &str,
        _main_file_name: &str,
        _source_hashes: Option<Vec<String>>,
    ) -> Result<CompilationResult> {
        use crate::ir::transform::multi_file::merge_stored_definitions;

        let start = Instant::now();

        // Merge all definitions
        let def = if definitions.len() == 1 {
            definitions.into_iter().next().unwrap().1
        } else {
            if self.verbose {
                println!("Merging {} files...", definitions.len());
            }
            merge_stored_definitions(definitions)?
        };

        let model_hash = format!("{:x}", chksum_md5::hash(main_source));
        let parse_time = start.elapsed();

        if self.verbose {
            println!("Parsing took {} ms", parse_time.as_millis());
            println!("AST:\n{:#?}\n", def);
        }

        // Run the compilation pipeline
        pipeline::compile_from_ast_ref(
            &def,
            self.model_name.as_deref(),
            model_hash,
            parse_time,
            self.verbose,
        )
    }

    /// Compiles Modelica source code from a string to a DAE representation.
    ///
    /// This method performs the full compilation pipeline on the provided source code.
    ///
    /// # Arguments
    ///
    /// * `source` - The Modelica source code to compile
    /// * `file_name` - A name to use for error reporting (can be anything)
    ///
    /// # Returns
    ///
    /// A [`CompilationResult`] containing the DAE and metadata
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The Modelica code contains syntax errors
    /// - The model contains unsupported features
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let code = "model Test\n  Real x;\nequation\n  der(x) = 1;\nend Test;";
    /// let result = Compiler::new()
    ///     .model("Test")
    ///     .compile_str(code, "test.mo")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn compile_str(&self, source: &str, file_name: &str) -> Result<CompilationResult> {
        // WASM: No filesystem access, so no additional files - just parse main source
        #[cfg(target_arch = "wasm32")]
        {
            let def = self.parse_source(source, file_name)?;
            let all_definitions = vec![(file_name.to_string(), def)];
            return self.compile_definitions(all_definitions, source, file_name);
        }

        // Native: Full parallel processing with thread pool
        #[cfg(not(target_arch = "wasm32"))]
        {
            use std::sync::atomic::{AtomicUsize, Ordering};

            // Configure thread pool
            let thread_count = self.get_thread_count();
            let pool = rayon::ThreadPoolBuilder::new()
                .num_threads(thread_count)
                .build()
                .with_context(|| "Failed to create thread pool")?;

            let cache_hits = AtomicUsize::new(0);
            let cache_misses = AtomicUsize::new(0);

            if self.verbose && !self.additional_files.is_empty() {
                println!(
                    "Parsing {} additional files using {} threads (cache: {})...",
                    self.additional_files.len(),
                    thread_count,
                    if self.use_cache {
                        "enabled"
                    } else {
                        "disabled"
                    }
                );
            }

            // Parse additional files in parallel (with caching)
            let additional_paths: Vec<_> = self.additional_files.iter().collect();
            let use_cache = self.use_cache;
            let parsed_additional: Result<Vec<_>> = pool.install(|| {
                additional_paths
                    .par_iter()
                    .map(|additional_path| {
                        let path_str = additional_path.to_string_lossy().to_string();

                        // Try cache first
                        if use_cache
                            && let Ok(hash) = cache::compute_file_hash(additional_path)
                            && let Some(cached_def) = cache::load_cached_ast(additional_path, &hash)
                        {
                            cache_hits.fetch_add(1, Ordering::Relaxed);
                            return Ok((path_str, cached_def));
                        }

                        // Cache miss - parse the file
                        cache_misses.fetch_add(1, Ordering::Relaxed);
                        let additional_source = fs::read_to_string(additional_path)
                            .with_context(|| format!("Failed to read file: {}", path_str))?;
                        let def = self.parse_source(&additional_source, &path_str)?;

                        // Store in cache
                        if use_cache && let Ok(hash) = cache::compute_file_hash(additional_path) {
                            let _ = cache::store_cached_ast(additional_path, &hash, &def);
                        }

                        Ok((path_str, def))
                    })
                    .collect()
            });

            let mut all_definitions = parsed_additional?;

            if self.verbose && !self.additional_files.is_empty() {
                println!(
                    "Cache: {} hits, {} misses",
                    cache_hits.load(Ordering::Relaxed),
                    cache_misses.load(Ordering::Relaxed)
                );
            }

            // Parse main source
            let def = self.parse_source(source, file_name)?;
            all_definitions.push((file_name.to_string(), def));

            // Compile with all definitions
            self.compile_definitions(all_definitions, source, file_name)
        }
    }

    /// Compiles from a pre-parsed StoredDefinition.
    ///
    /// This method is useful when you have already parsed the Modelica code
    /// and want to avoid re-parsing it. This is especially beneficial when
    /// compiling multiple models from the same source files (e.g., testing).
    ///
    /// # Arguments
    ///
    /// * `def` - The pre-parsed StoredDefinition
    /// * `source` - The original source code (for error reporting)
    ///
    /// # Returns
    ///
    /// A [`CompilationResult`] containing the DAE and metadata
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::{Compiler, ir::ast::StoredDefinition};
    /// use rumoca::modelica_grammar::ModelicaGrammar;
    /// use rumoca::modelica_parser::parse;
    ///
    /// // Parse once
    /// let source = "model Test\n  Real x;\nequation\n  der(x) = 1;\nend Test;";
    /// let mut grammar = ModelicaGrammar::new();
    /// parse(source, "test.mo", &mut grammar).unwrap();
    /// let def = grammar.modelica.unwrap();
    ///
    /// // Compile multiple times without re-parsing
    /// let result = Compiler::new()
    ///     .model("Test")
    ///     .compile_parsed(def.clone(), source)?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn compile_parsed(&self, def: StoredDefinition, source: &str) -> Result<CompilationResult> {
        let model_hash = format!("{:x}", chksum_md5::hash(source));
        pipeline::compile_from_ast(
            def,
            self.model_name.as_deref(),
            model_hash,
            std::time::Duration::ZERO, // No parse time for pre-parsed
            self.verbose,
        )
    }

    /// Compiles from a reference to a pre-parsed StoredDefinition.
    ///
    /// This is more efficient than `compile_parsed` when compiling many models
    /// from the same AST because it avoids cloning the StoredDefinition during
    /// the compilation process. The def is only cloned once at the end.
    pub fn compile_parsed_ref(
        &self,
        def: &StoredDefinition,
        source: &str,
    ) -> Result<CompilationResult> {
        let model_hash = format!("{:x}", chksum_md5::hash(source));
        pipeline::compile_from_ast_ref(
            def,
            self.model_name.as_deref(),
            model_hash,
            std::time::Duration::ZERO,
            self.verbose,
        )
    }

    /// Performs a lightweight balance check only, without full compilation.
    ///
    /// This is much faster than full compilation when you only need to check
    /// if a model is balanced. It skips the StoredDefinition cloning entirely.
    pub fn check_balance(
        &self,
        def: &StoredDefinition,
    ) -> Result<crate::dae::balance::BalanceResult> {
        pipeline::check_balance_only(def, self.model_name.as_deref())
    }

    /// Compile with additional library sources provided as strings.
    ///
    /// This is useful for WASM where filesystem access is not available.
    /// Library sources are parsed and merged with the main source before compilation.
    ///
    /// # Arguments
    ///
    /// * `source` - The main Modelica source code
    /// * `file_name` - Name for the main source file (for error messages)
    /// * `libraries` - Vector of (file_name, source_code) pairs for library files
    ///
    /// # Returns
    ///
    /// A [`CompilationResult`] containing the DAE and metadata
    pub fn compile_str_with_sources(
        &self,
        source: &str,
        file_name: &str,
        libraries: Vec<(&str, &str)>,
    ) -> Result<CompilationResult> {
        // Parse the main source
        let main_def = self.parse_source(source, file_name)?;

        // Parse all library sources
        let mut all_definitions = Vec::with_capacity(libraries.len() + 1);

        for (lib_name, lib_source) in libraries {
            let lib_def = self.parse_source(lib_source, lib_name)?;
            all_definitions.push((lib_name.to_string(), lib_def));
        }

        // Add main source last (so its classes take precedence)
        all_definitions.push((file_name.to_string(), main_def));

        self.compile_definitions(all_definitions, source, file_name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_default() {
        let compiler = Compiler::new();
        assert!(!compiler.verbose);
    }

    #[test]
    fn test_compiler_verbose() {
        let compiler = Compiler::new().verbose(true);
        assert!(compiler.verbose);
    }

    #[test]
    fn test_compile_simple_model() {
        let source = r#"
model Integrator
    Real x(start=0);
equation
    der(x) = 1;
end Integrator;
"#;

        let result = Compiler::new()
            .model("Integrator")
            .compile_str(source, "test.mo");
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

        let result = result.unwrap();
        assert!(!result.dae.x.is_empty(), "Should have state variables");
        assert_eq!(result.dae.x.len(), 1, "Should have exactly one state");
    }

    #[test]
    fn test_compile_requires_model_name() {
        let source = r#"
model Test
    Real x;
equation
    der(x) = 1;
end Test;
"#;

        let result = Compiler::new().compile_str(source, "test.mo");
        assert!(result.is_err(), "Should error when model name not provided");
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("Model name is required"),
            "Error should mention model name is required: {}",
            err_msg
        );
    }

    #[test]
    fn test_compilation_result_total_time() {
        let source = r#"
model Test
    Real x;
equation
    der(x) = 1;
end Test;
"#;

        let result = Compiler::new()
            .model("Test")
            .compile_str(source, "test.mo")
            .unwrap();
        let total = result.total_time();
        assert!(total > std::time::Duration::from_nanos(0));
        assert_eq!(
            total,
            result.parse_time + result.flatten_time + result.dae_time
        );
    }
}
