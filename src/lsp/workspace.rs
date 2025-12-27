//! Workspace state manager for multi-file LSP support.
//!
//! Provides:
//! - Tracking of all open documents and their parsed ASTs
//! - Package structure discovery and management
//! - Cross-file symbol lookup
//! - Dependency tracking between files

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use lsp_types::Uri;
use rayon::prelude::*;

use crate::dae::balance::BalanceResult;
use crate::ir::analysis::symbol_trait::SymbolInfo;
use crate::ir::analysis::type_inference::SymbolType;
use crate::ir::ast::{ClassDefinition, ClassType, Import, StoredDefinition};
use crate::ir::transform::multi_file::{
    discover_modelica_files, extract_package_name, get_modelica_path, is_modelica_package,
    should_ignore_directory,
};
use crate::ir::transform::scope_resolver::{ExternalSymbol, SymbolCategory, SymbolLookup};

use super::utils::{parse_document, parse_file_cached};

/// Information about a symbol in the workspace
#[derive(Debug, Clone)]
pub struct WorkspaceSymbol {
    /// Fully qualified name (e.g., "MyPackage.SubPackage.MyModel")
    pub qualified_name: String,
    /// The URI of the file containing this symbol
    pub uri: Uri,
    /// Line number (0-based)
    pub line: u32,
    /// Column number (0-based)
    pub column: u32,
    /// The kind of symbol
    pub kind: SymbolKind,
    /// Brief description or signature
    pub detail: Option<String>,
}

/// Result from loading library sources
#[derive(Debug, Default, Clone)]
pub struct LoadLibraryResult {
    pub parsed_count: usize,
    pub error_count: usize,
    pub library_names: Vec<String>,
    pub conflicts: Vec<String>,
    pub skipped_files: Vec<String>,
}

/// Kind of workspace symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Package,
    Model,
    Class,
    Block,
    Connector,
    Record,
    Type,
    Function,
    Operator,
    Component,
    Parameter,
    Constant,
}

impl From<&ClassType> for SymbolKind {
    fn from(ct: &ClassType) -> Self {
        match ct {
            ClassType::Package => SymbolKind::Package,
            ClassType::Model => SymbolKind::Model,
            ClassType::Class => SymbolKind::Class,
            ClassType::Block => SymbolKind::Block,
            ClassType::Connector => SymbolKind::Connector,
            ClassType::Record => SymbolKind::Record,
            ClassType::Type => SymbolKind::Type,
            ClassType::Function => SymbolKind::Function,
            ClassType::Operator => SymbolKind::Operator,
        }
    }
}

impl SymbolInfo for WorkspaceSymbol {
    fn name(&self) -> &str {
        // Return the simple name (last part of qualified name)
        self.qualified_name
            .rsplit('.')
            .next()
            .unwrap_or(&self.qualified_name)
    }

    fn qualified_name(&self) -> &str {
        &self.qualified_name
    }

    fn symbol_type(&self) -> SymbolType {
        // WorkspaceSymbol tracks class kinds, so return Class type for class-like symbols
        match self.kind {
            SymbolKind::Package
            | SymbolKind::Model
            | SymbolKind::Class
            | SymbolKind::Block
            | SymbolKind::Connector
            | SymbolKind::Record
            | SymbolKind::Type
            | SymbolKind::Function
            | SymbolKind::Operator => SymbolType::Class(self.qualified_name.clone()),
            SymbolKind::Component | SymbolKind::Parameter | SymbolKind::Constant => {
                SymbolType::Unknown
            }
        }
    }

    fn line(&self) -> u32 {
        // Convert from 0-based to 1-based
        self.line + 1
    }

    fn column(&self) -> u32 {
        // Convert from 0-based to 1-based
        self.column + 1
    }

    fn is_parameter(&self) -> bool {
        matches!(self.kind, SymbolKind::Parameter)
    }

    fn is_constant(&self) -> bool {
        matches!(self.kind, SymbolKind::Constant)
    }

    fn is_class(&self) -> bool {
        matches!(
            self.kind,
            SymbolKind::Package
                | SymbolKind::Model
                | SymbolKind::Class
                | SymbolKind::Block
                | SymbolKind::Connector
                | SymbolKind::Record
                | SymbolKind::Type
                | SymbolKind::Function
                | SymbolKind::Operator
        )
    }
}

/// Workspace state for multi-file support
pub struct WorkspaceState {
    /// All open documents and their content
    documents: HashMap<Uri, String>,
    /// Parsed ASTs for each document (last successful parse)
    parsed_asts: HashMap<Uri, StoredDefinition>,
    /// Global symbol index: qualified name -> symbol info
    symbol_index: HashMap<String, WorkspaceSymbol>,
    /// Reverse index: URI -> list of symbols defined in that file
    file_symbols: HashMap<Uri, Vec<String>>,
    /// Package roots discovered in workspace
    package_roots: Vec<PathBuf>,
    /// Workspace root folders
    workspace_roots: Vec<PathBuf>,
    /// Files that have been discovered but not opened
    discovered_files: HashSet<PathBuf>,
    /// Cache of last successfully parsed ASTs (kept even when current parse fails)
    /// This allows completions to work while the user is typing (causing syntax errors)
    cached_asts: HashMap<Uri, StoredDefinition>,
    /// Cache of balance check results per class name (computed during diagnostics)
    /// Key is (Uri, class_name) to support multiple classes per file
    balance_cache: HashMap<(Uri, String), BalanceResult>,
    /// In-memory cache of pre-merged library ASTs (e.g., MSL)
    /// Key is library name (e.g., "Modelica"), value is Arc-wrapped merged StoredDefinition
    /// Pre-merging at load time avoids expensive merging on every compile
    /// Using Arc avoids cloning the large library on every compile
    library_cache: HashMap<String, Arc<StoredDefinition>>,
    /// Debug mode flag for verbose logging
    debug: bool,
}

impl Default for WorkspaceState {
    fn default() -> Self {
        Self::new()
    }
}

impl WorkspaceState {
    /// Create a new workspace state
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
            parsed_asts: HashMap::new(),
            symbol_index: HashMap::new(),
            file_symbols: HashMap::new(),
            package_roots: Vec::new(),
            workspace_roots: Vec::new(),
            discovered_files: HashSet::new(),
            cached_asts: HashMap::new(),
            balance_cache: HashMap::new(),
            library_cache: HashMap::new(),
            debug: false,
        }
    }

    /// Add a document to the workspace (for WASM use).
    ///
    /// This parses the document and indexes its symbols.
    pub fn add_document(&mut self, uri: Uri, text: String) {
        self.documents.insert(uri.clone(), text.clone());
        self.reparse_document(&uri);
    }

    /// Enable or disable debug logging
    pub fn set_debug(&mut self, debug: bool) {
        self.debug = debug;
    }

    /// Log a debug message if debug mode is enabled
    fn debug_log(&self, msg: &str) {
        #[cfg(target_arch = "wasm32")]
        {
            web_sys::console::log_1(&msg.into());
        }
        #[cfg(not(target_arch = "wasm32"))]
        if self.debug {
            eprintln!("{}", msg);
        }
    }

    /// Set the cached balance result for a specific class in a document
    pub fn set_balance(&mut self, uri: Uri, class_name: String, balance: BalanceResult) {
        self.balance_cache.insert((uri, class_name), balance);
    }

    /// Get the cached balance result for a specific class in a document
    pub fn get_balance(&self, uri: &Uri, class_name: &str) -> Option<&BalanceResult> {
        self.balance_cache
            .get(&(uri.clone(), class_name.to_string()))
    }

    /// Clear all cached balance results for a document
    pub fn clear_balances(&mut self, uri: &Uri) {
        self.balance_cache.retain(|(u, _), _| u != uri);
    }

    /// Get cached pre-merged library AST by name
    /// Returns an Arc to avoid cloning the large library on every compile
    pub fn get_library(&self, name: &str) -> Option<Arc<StoredDefinition>> {
        self.library_cache.get(name).cloned()
    }

    /// Check if a library is cached
    pub fn has_library(&self, name: &str) -> bool {
        self.library_cache.contains_key(name)
    }

    /// Get the number of libraries loaded in the cache.
    pub fn library_count(&self) -> usize {
        self.library_cache.len()
    }

    /// Iterate over all loaded libraries.
    pub fn libraries(&self) -> impl Iterator<Item = (&String, &Arc<StoredDefinition>)> {
        self.library_cache.iter()
    }

    /// Load library sources and parse them.
    ///
    /// This is the primary entry point for WASM library loading. It:
    /// 1. Parses all source files (parallel on native, sequential on WASM)
    /// 2. Groups them by library name
    /// 3. Merges into complete packages
    /// 4. Indexes symbols for completion/hover
    ///
    /// Note: WASM uses sequential parsing because rayon's work-stealing
    /// doesn't work reliably with wasm-bindgen-rayon for this workload.
    ///
    /// # Arguments
    /// * `sources` - HashMap of file_path -> source_code
    ///
    /// # Returns
    /// LoadLibraryResult with parsed_count, error_count, library_names, conflicts, skipped_files
    pub fn load_library_sources(
        &mut self,
        sources: std::collections::HashMap<String, String>,
    ) -> LoadLibraryResult {
        let total = sources.len();
        self.debug_log(&format!("[workspace] Starting parse of {} files...", total));

        #[cfg(target_arch = "wasm32")]
        let start = web_time::Instant::now();
        #[cfg(not(target_arch = "wasm32"))]
        let start = std::time::Instant::now();

        // Group files by library name first to detect duplicates early
        let mut files_by_lib: std::collections::HashMap<String, Vec<(String, String)>> =
            std::collections::HashMap::new();
        for (file_name, source) in sources {
            // Extract library name from path (first component)
            // For "Modelica/Blocks/Continuous.mo" -> "Modelica"
            // For "Complex.mo" (single-file lib) -> "Complex"
            let first_part = file_name.split('/').next().unwrap_or(&file_name);
            let lib_name = first_part
                .strip_suffix(".mo")
                .unwrap_or(first_part)
                .to_string();
            files_by_lib
                .entry(lib_name)
                .or_default()
                .push((file_name, source));
        }

        // Check for already-loaded libraries and skip them
        let mut skipped_files: Vec<String> = Vec::new();
        let mut sources_to_parse: Vec<(String, String)> = Vec::new();

        for (lib_name, files) in files_by_lib {
            if self.library_cache.contains_key(&lib_name) {
                self.debug_log(&format!(
                    "[workspace] Skipping {} files for already-loaded library '{}'",
                    files.len(),
                    lib_name
                ));
                for (file_name, _) in files {
                    skipped_files.push(file_name);
                }
            } else {
                sources_to_parse.extend(files);
            }
        }

        let sources_vec = sources_to_parse;
        self.debug_log(&format!(
            "[workspace] Parsing {} files ({} skipped)",
            sources_vec.len(),
            skipped_files.len()
        ));

        // Use sequential parsing for WASM - rayon parallel has too much overhead
        // (coordination via SharedArrayBuffer + Web Workers is slower than native threads)
        #[cfg(target_arch = "wasm32")]
        let results: Vec<_> = {
            let total = sources_vec.len();
            web_sys::console::log_1(
                &format!("[WASM] load_libraries: parsing {} files...", total).into(),
            );

            let results: Vec<_> = sources_vec
                .iter()
                .enumerate()
                .map(|(i, (file_name, source))| {
                    // Log progress every 50 files
                    if i % 50 == 0 {
                        let percent = (i * 100) / total;
                        web_sys::console::log_1(
                            &format!(
                                "[WASM] load_libraries: parsing {}/{} ({}%)",
                                i, total, percent
                            )
                            .into(),
                        );
                    }
                    match crate::parse_source(source, file_name) {
                        Ok(def) => Ok((file_name.clone(), def)),
                        Err(e) => Err(format!("{}: {}", file_name, e)),
                    }
                })
                .collect();

            web_sys::console::log_1(
                &format!("[WASM] load_libraries: parsing {}/{} (100%)", total, total).into(),
            );

            results
        };

        #[cfg(not(target_arch = "wasm32"))]
        let results: Vec<_> = {
            self.debug_log(&format!(
                "[workspace] Using parallel parsing ({} files)...",
                sources_vec.len()
            ));
            sources_vec
                .into_par_iter()
                .map(
                    |(file_name, source)| match crate::parse_source(&source, &file_name) {
                        Ok(def) => Ok((file_name, def)),
                        Err(e) => Err(format!("{}: {}", file_name, e)),
                    },
                )
                .collect()
        };

        self.debug_log(&format!(
            "[workspace] Parse complete, {} results",
            results.len()
        ));

        // Separate successes and errors
        let mut parsed = Vec::with_capacity(results.len());
        let mut errors = Vec::new();

        for result in results {
            match result {
                Ok(pair) => parsed.push(pair),
                Err(e) => errors.push(e),
            }
        }

        let parse_time = start.elapsed();
        self.debug_log(&format!(
            "[workspace] Parsed {} files in {:.2}s ({} errors)",
            parsed.len(),
            parse_time.as_secs_f64(),
            errors.len()
        ));

        // Log first few errors
        for (i, err) in errors.iter().take(5).enumerate() {
            self.debug_log(&format!("[workspace] Parse error {}: {}", i + 1, err));
        }

        let parsed_count = parsed.len();
        let error_count = errors.len();

        self.debug_log("[workspace] Starting library merge...");

        // Load the parsed definitions
        let (library_names, conflicts) = self.load_library_definitions(parsed);

        self.debug_log(&format!(
            "[workspace] Done! {} libraries loaded: {:?}{}{}",
            library_names.len(),
            library_names,
            if conflicts.is_empty() {
                String::new()
            } else {
                format!(" ({} conflicts: {:?})", conflicts.len(), conflicts)
            },
            if skipped_files.is_empty() {
                String::new()
            } else {
                format!(" ({} files skipped)", skipped_files.len())
            }
        ));

        LoadLibraryResult {
            parsed_count,
            error_count,
            library_names,
            conflicts,
            skipped_files,
        }
    }

    /// Load pre-parsed library definitions into the workspace.
    ///
    /// This is the primary method for loading libraries from pre-parsed sources
    /// (e.g., from WASM where files are loaded via JavaScript).
    ///
    /// # Arguments
    /// * `definitions` - List of (filename, StoredDefinition) tuples
    ///
    /// # Returns
    /// Number of library packages successfully loaded
    ///
    /// # Example
    /// ```ignore
    /// let definitions = vec![
    ///     ("Modelica/Blocks/Continuous.mo".to_string(), ast1),
    ///     ("Modelica/Blocks/Math.mo".to_string(), ast2),
    /// ];
    /// workspace.load_library_definitions(definitions);
    /// ```
    /// Returns (library_names, conflicts) where conflicts are libraries that were replaced
    pub fn load_library_definitions(
        &mut self,
        definitions: Vec<(String, StoredDefinition)>,
    ) -> (Vec<String>, Vec<String>) {
        use crate::ir::transform::multi_file::merge_stored_definitions;
        use std::collections::HashMap as StdHashMap;

        // Group definitions by library name (root package from file path)
        // e.g., "Modelica/Blocks/Continuous.mo" -> "Modelica"
        let mut libs_by_name: StdHashMap<String, Vec<(String, StoredDefinition)>> =
            StdHashMap::new();

        for (file_name, def) in definitions.iter() {
            // Extract root package name from file path
            let lib_name = file_name.split('/').next().unwrap_or(file_name).to_string();
            libs_by_name
                .entry(lib_name)
                .or_default()
                .push((file_name.clone(), def.clone()));
        }

        // Log detected library names
        let lib_names: Vec<_> = libs_by_name.keys().collect();
        self.debug_log(&format!(
            "[workspace] Detected {} library packages: {:?}",
            lib_names.len(),
            lib_names
        ));

        let mut loaded_names: Vec<String> = Vec::new();
        let mut conflicts: Vec<String> = Vec::new();

        // Merge each library's definitions and store in library_cache
        for (lib_name, lib_defs) in libs_by_name {
            // Check for conflicts - warn if library already exists
            if self.library_cache.contains_key(&lib_name) {
                self.debug_log(&format!(
                    "[workspace] WARNING: Library '{}' already loaded, will be replaced",
                    lib_name
                ));
                conflicts.push(lib_name.clone());
            }

            match merge_stored_definitions(lib_defs) {
                Ok(merged) => {
                    self.library_cache
                        .insert(lib_name.clone(), Arc::new(merged));
                    loaded_names.push(lib_name.clone());
                    self.debug_log(&format!(
                        "[workspace] Loaded library '{}' into cache{}",
                        lib_name,
                        if conflicts.contains(&lib_name) {
                            " (replaced)"
                        } else {
                            ""
                        }
                    ));
                }
                Err(e) => {
                    self.debug_log(&format!(
                        "[workspace] Failed to merge library '{}': {}",
                        lib_name, e
                    ));
                }
            }
        }

        // Index symbols from all definitions for completion/hover
        for (i, (_, ast)) in definitions.iter().enumerate() {
            let uri: Uri = format!("memory:///library_{}.mo", i)
                .parse()
                .expect("valid URI");
            self.index_stored_definition(&uri, ast);
            self.parsed_asts.insert(uri.clone(), ast.clone());
            self.cached_asts.insert(uri, ast.clone());
        }

        // Log conflicts if any
        if !conflicts.is_empty() {
            self.debug_log(&format!(
                "[workspace] WARNING: {} library conflicts detected: {:?}",
                conflicts.len(),
                conflicts
            ));
        }

        (loaded_names, conflicts)
    }

    /// Get a pre-built class dictionary for a library.
    ///
    /// This is more efficient than `get_library()` for compilation because
    /// it returns the class dictionary directly, which can be combined
    /// with user code without cloning class definitions.
    ///
    /// The class dictionary is cached in `LIBRARY_DICT_CACHE` so subsequent
    /// calls return the cached dictionary in O(1) time.
    ///
    /// # Performance
    ///
    /// - First call: ~1 second (builds dictionary from ~50,000 MSL classes)
    /// - Subsequent calls: ~1ms (returns cached Arc reference)
    pub fn get_library_dict(
        &self,
        name: &str,
    ) -> Option<Arc<crate::ir::transform::flatten::ClassDict>> {
        use crate::ir::transform::flatten::get_or_build_library_dict;

        // Get the cached library StoredDefinition
        let lib = self.library_cache.get(name)?;

        // Build (or get cached) class dictionary from it
        Some(get_or_build_library_dict(name, lib))
    }

    /// Load a library into the in-memory cache
    ///
    /// This parses all files in the library package, merges them into a single
    /// StoredDefinition wrapped in Arc, and caches the result. Pre-merging at
    /// load time avoids expensive merging on every compile. Using Arc avoids
    /// cloning the large library on every access.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn load_library(&mut self, name: &str) -> Option<Arc<StoredDefinition>> {
        use crate::compiler::parse_file_cached;
        use crate::ir::transform::multi_file::{
            discover_modelica_files, find_package_in_paths, merge_stored_definitions,
        };

        // Already cached?
        if let Some(cached) = self.library_cache.get(name) {
            return Some(Arc::clone(cached));
        }

        // Find the package in our library paths
        let package_path = find_package_in_paths(name, &self.package_roots)?;

        self.debug_log(&format!(
            "[workspace] Loading library '{}' from {:?}",
            name, package_path
        ));

        let start = std::time::Instant::now();

        // Discover all files
        let files = discover_modelica_files(&package_path).ok()?;

        // Parse all files in parallel using rayon and disk cache
        use rayon::prelude::*;
        let definitions: Vec<(String, StoredDefinition)> = files
            .par_iter()
            .filter_map(|path| {
                let path_str = path.to_string_lossy().to_string();
                parse_file_cached(path).map(|ast| (path_str, ast))
            })
            .collect();

        let parse_time = start.elapsed();

        // Pre-merge all definitions into one StoredDefinition
        let merge_start = std::time::Instant::now();
        let merged = match merge_stored_definitions(definitions) {
            Ok(m) => m,
            Err(e) => {
                self.debug_log(&format!(
                    "[workspace] Failed to merge library '{}': {}",
                    name, e
                ));
                return None;
            }
        };
        let merge_time = merge_start.elapsed();

        self.debug_log(&format!(
            "[workspace] Loaded library '{}': parsed in {:?}, merged in {:?}",
            name, parse_time, merge_time
        ));

        let arc = Arc::new(merged);
        self.library_cache
            .insert(name.to_string(), Arc::clone(&arc));
        Some(arc)
    }

    /// Initialize workspace with root folders and optional additional library paths
    ///
    /// # Arguments
    /// * `workspace_folders` - Folders opened in the editor
    /// * `extra_library_paths` - Additional library paths (from settings, added to MODELICAPATH)
    ///
    /// # Cache Invalidation
    ///
    /// This clears both the local `library_cache` and the global `LIBRARY_DICT_CACHE`
    /// to ensure each workspace session starts fresh. This prevents stale library
    /// data from being used when switching between projects with different library versions.
    pub fn initialize(
        &mut self,
        workspace_folders: Vec<PathBuf>,
        extra_library_paths: Vec<PathBuf>,
    ) {
        // Clear caches to ensure fresh library loading for this workspace
        // This handles the case where different projects use different library versions
        self.library_cache.clear();
        crate::ir::transform::flatten::clear_library_dict_cache();

        self.debug_log(&format!(
            "[workspace] initialize() called with {} folders, {} extra library paths",
            workspace_folders.len(),
            extra_library_paths.len()
        ));
        self.workspace_roots = workspace_folders.clone();

        // Add extra library paths from settings (these take priority)
        if !extra_library_paths.is_empty() {
            self.debug_log(&format!(
                "[workspace] Extra library paths from settings: {:?}",
                extra_library_paths
            ));
            self.package_roots.extend(extra_library_paths);
        }

        // Add MODELICAPATH directories from environment
        let modelica_path = get_modelica_path();
        self.debug_log(&format!(
            "[workspace] MODELICAPATH has {} directories: {:?}",
            modelica_path.len(),
            modelica_path
        ));
        self.package_roots.extend(modelica_path);

        // Discover packages in workspace folders
        for folder in &workspace_folders {
            self.debug_log(&format!(
                "[workspace] Discovering packages in workspace: {:?}",
                folder
            ));
            let start = std::time::Instant::now();
            self.discover_packages_in_folder(folder);
            self.debug_log(&format!(
                "[workspace] Finished {:?} in {:?}",
                folder,
                start.elapsed()
            ));
        }

        // Also discover packages in library paths (MODELICAPATH + extra paths)
        // This enables autocompletion for library imports
        let library_paths: Vec<PathBuf> = self.package_roots.clone();
        for lib_path in &library_paths {
            if !workspace_folders.contains(lib_path) {
                self.debug_log(&format!(
                    "[workspace] Discovering packages in library path: {:?}",
                    lib_path
                ));
                let start = std::time::Instant::now();
                self.discover_packages_in_folder(lib_path);
                self.debug_log(&format!(
                    "[workspace] Finished {:?} in {:?}",
                    lib_path,
                    start.elapsed()
                ));
            }
        }

        self.debug_log(&format!(
            "[workspace] initialize() complete - {} files discovered",
            self.discovered_files.len()
        ));

        // Index all discovered files upfront for reliable hover/goto-definition
        self.index_all_discovered_files();
    }

    /// Index all discovered Modelica files upfront
    ///
    /// This provides immediate hover/goto-definition support without lazy loading
    /// Uses parallel parsing with rayon for faster initialization
    fn index_all_discovered_files(&mut self) {
        let start = std::time::Instant::now();
        let files: Vec<PathBuf> = self.discovered_files.iter().cloned().collect();
        let total = files.len();

        self.debug_log(&format!(
            "[workspace] Starting upfront indexing of {} files",
            total
        ));

        // Parse all files in parallel using the global rayon thread pool
        // (configured at startup to use num_cpus - 1 threads)
        // Uses disk cache for files that haven't changed since last parse
        let parsed_results: Vec<(PathBuf, Option<(String, StoredDefinition)>)> = files
            .par_iter()
            .map(|path| {
                // Use cached parsing - checks disk cache first, then parses if needed
                if let Some(ast) = parse_file_cached(path) {
                    // Read text for document storage (needed for LSP features)
                    let text = std::fs::read_to_string(path).unwrap_or_default();
                    (path.clone(), Some((text, ast)))
                } else {
                    (path.clone(), None)
                }
            })
            .collect();

        // Now merge results into workspace state (single-threaded to avoid lock contention)
        let mut indexed = 0;
        let mut failed = 0;

        for (path, result) in parsed_results {
            if let Some((text, ast)) = result {
                // Convert path to URI
                if let Some(uri) = path_to_uri(&path) {
                    // Store document and AST
                    self.documents.insert(uri.clone(), text);

                    // Index symbols from AST
                    self.remove_file_symbols(&uri);
                    self.index_stored_definition(&uri, &ast);
                    self.parsed_asts.insert(uri.clone(), ast.clone());
                    self.cached_asts.insert(uri, ast);

                    indexed += 1;
                } else {
                    failed += 1;
                }
            } else {
                failed += 1;
            }
        }

        self.debug_log(&format!(
            "[workspace] Upfront indexing complete in {:?}: {} indexed, {} failed, {} symbols",
            start.elapsed(),
            indexed,
            failed,
            self.symbol_index.len()
        ));
    }

    /// Discover Modelica packages in a folder
    fn discover_packages_in_folder(&mut self, folder: &Path) {
        // Skip ignored directories
        if should_ignore_directory(folder) {
            self.debug_log(&format!(
                "[workspace] Skipping ignored directory: {:?}",
                folder
            ));
            return;
        }

        if is_modelica_package(folder) {
            self.debug_log(&format!("[workspace] Found Modelica package: {:?}", folder));
            self.package_roots.push(folder.to_path_buf());

            // Register the package name as a symbol for import autocompletion
            // Use extract_package_name to handle versioned directory names (Modelica Spec 13.4)
            // e.g., "Modelica 4.1.0" should be registered as "Modelica"
            if let Some(folder_name) = folder.file_name().and_then(|n| n.to_str()) {
                let package_name = extract_package_name(folder_name);
                let package_mo = folder.join("package.mo");
                if let Some(path_str) = package_mo.to_str() {
                    let uri_str = format!("file://{}", path_str);
                    if let Ok(uri) = uri_str.parse::<Uri>() {
                        self.register_package_symbol(package_name, &uri);
                    }
                }
            }

            // Discover all files in this package
            if let Ok(files) = discover_modelica_files(folder) {
                self.debug_log(&format!(
                    "[workspace] Package {:?} contains {} files",
                    folder,
                    files.len()
                ));
                for file in files {
                    self.discovered_files.insert(file);
                }
            }
        } else if folder.is_dir() {
            // Look for packages in subdirectories
            if let Ok(entries) = std::fs::read_dir(folder) {
                let entries: Vec<_> = entries.flatten().collect();
                self.debug_log(&format!(
                    "[workspace] Scanning directory {:?} ({} entries)",
                    folder,
                    entries.len()
                ));
                for entry in entries {
                    let path = entry.path();
                    if path.is_dir() {
                        // Skip ignored directories
                        if !should_ignore_directory(&path) {
                            self.discover_packages_in_folder(&path);
                        }
                    } else if path.extension().is_some_and(|e| e == "mo") {
                        self.discovered_files.insert(path);
                    }
                }
            }
        }
    }

    /// Open a document (called when file is opened in editor)
    pub fn open_document(&mut self, uri: Uri, text: String) {
        self.documents.insert(uri.clone(), text.clone());
        self.reparse_document(&uri);
    }

    /// Update a document (called when file is changed)
    pub fn update_document(&mut self, uri: Uri, text: String) {
        self.documents.insert(uri.clone(), text.clone());
        self.reparse_document(&uri);
    }

    /// Close a document
    pub fn close_document(&mut self, uri: &Uri) {
        self.documents.remove(uri);
        self.remove_file_symbols(uri);
        self.parsed_asts.remove(uri);
    }

    /// Get document text
    pub fn get_document(&self, uri: &Uri) -> Option<&String> {
        self.documents.get(uri)
    }

    /// Get all documents
    pub fn documents(&self) -> &HashMap<Uri, String> {
        &self.documents
    }

    /// Reparse a document and update symbol index
    fn reparse_document(&mut self, uri: &Uri) {
        let text = match self.documents.get(uri) {
            Some(t) => t.clone(),
            None => return,
        };

        let path = uri.path().as_str();

        // Parse the document
        if let Some(ast) = parse_document(&text, path) {
            // Successful parse - remove old symbols and index new ones
            self.remove_file_symbols(uri);
            self.index_stored_definition(uri, &ast);
            self.parsed_asts.insert(uri.clone(), ast.clone());
            // Also update the cache with the successful parse
            self.cached_asts.insert(uri.clone(), ast);
        }
        // If parse fails, keep the cached AST for completion support
        // but don't update the symbol index (it stays as it was from last good parse)
    }

    /// Get the cached AST for a document (from last successful parse)
    /// This is useful for completions when the current document has syntax errors
    pub fn get_cached_ast(&self, uri: &Uri) -> Option<&StoredDefinition> {
        self.cached_asts.get(uri)
    }

    /// Set a cached AST for a URI (used for WASM fallback when parsing fails)
    pub fn set_cached_ast(&mut self, uri: Uri, ast: StoredDefinition) {
        self.cached_asts.insert(uri, ast);
    }

    /// Remove symbols from a file from the index
    fn remove_file_symbols(&mut self, uri: &Uri) {
        if let Some(symbols) = self.file_symbols.remove(uri) {
            for name in symbols {
                self.symbol_index.remove(&name);
            }
        }
    }

    /// Index symbols from a StoredDefinition
    fn index_stored_definition(&mut self, uri: &Uri, def: &StoredDefinition) {
        let mut file_symbols = Vec::new();

        // Get the within prefix if present
        let prefix = def
            .within
            .as_ref()
            .map(|n| n.to_string())
            .unwrap_or_default();

        for (class_name, class_def) in &def.class_list {
            let qualified_name = if prefix.is_empty() {
                class_name.clone()
            } else {
                format!("{}.{}", prefix, class_name)
            };

            self.index_class(uri, &qualified_name, class_def, &mut file_symbols);
        }

        self.file_symbols.insert(uri.clone(), file_symbols);
    }

    /// Register a package symbol for import autocompletion
    /// This is called during package discovery to make package names available
    /// without needing to fully parse all files
    fn register_package_symbol(&mut self, package_name: &str, uri: &Uri) {
        // Only add if not already present
        if self.symbol_index.contains_key(package_name) {
            return;
        }

        let symbol = WorkspaceSymbol {
            qualified_name: package_name.to_string(),
            uri: uri.clone(),
            line: 0,
            column: 0,
            kind: SymbolKind::Package,
            detail: Some("Package".to_string()),
        };

        self.debug_log(&format!(
            "[workspace] Registered package symbol: {}",
            package_name
        ));
        self.symbol_index.insert(package_name.to_string(), symbol);
    }

    /// Index a class and its nested contents
    fn index_class(
        &mut self,
        uri: &Uri,
        qualified_name: &str,
        class: &ClassDefinition,
        file_symbols: &mut Vec<String>,
    ) {
        // Index the class itself
        let symbol = WorkspaceSymbol {
            qualified_name: qualified_name.to_string(),
            uri: uri.clone(),
            line: class.name.location.start_line.saturating_sub(1),
            column: class.name.location.start_column.saturating_sub(1),
            kind: SymbolKind::from(&class.class_type),
            detail: Some(format!("{:?}", class.class_type)),
        };

        self.symbol_index.insert(qualified_name.to_string(), symbol);
        file_symbols.push(qualified_name.to_string());

        // Index components
        for (comp_name, comp) in &class.components {
            let comp_qualified = format!("{}.{}", qualified_name, comp_name);
            let kind = if matches!(comp.variability, crate::ir::ast::Variability::Parameter(_)) {
                SymbolKind::Parameter
            } else if matches!(comp.variability, crate::ir::ast::Variability::Constant(_)) {
                SymbolKind::Constant
            } else {
                SymbolKind::Component
            };

            let (line, col) = comp
                .type_name
                .name
                .first()
                .map(|t| {
                    (
                        t.location.start_line.saturating_sub(1),
                        t.location.start_column.saturating_sub(1),
                    )
                })
                .unwrap_or((0, 0));

            let symbol = WorkspaceSymbol {
                qualified_name: comp_qualified.clone(),
                uri: uri.clone(),
                line,
                column: col,
                kind,
                detail: Some(comp.type_name.to_string()),
            };

            self.symbol_index.insert(comp_qualified.clone(), symbol);
            file_symbols.push(comp_qualified);
        }

        // Recursively index nested classes
        for (nested_name, nested_class) in &class.classes {
            let nested_qualified = format!("{}.{}", qualified_name, nested_name);
            self.index_class(uri, &nested_qualified, nested_class, file_symbols);
        }
    }

    /// Look up a symbol by qualified name
    pub fn lookup_symbol(&self, qualified_name: &str) -> Option<&WorkspaceSymbol> {
        self.symbol_index.get(qualified_name)
    }

    /// Look up a symbol by simple name (searches all matching qualified names)
    pub fn lookup_by_simple_name(&self, name: &str) -> Vec<&WorkspaceSymbol> {
        self.symbol_index
            .iter()
            .filter(|(qn, _)| qn.rsplit('.').next() == Some(name) || *qn == name)
            .map(|(_, sym)| sym)
            .collect()
    }

    /// Find all symbols matching a query (for workspace symbol search)
    pub fn find_symbols(&self, query: &str) -> Vec<&WorkspaceSymbol> {
        let query_lower = query.to_lowercase();

        self.symbol_index
            .values()
            .filter(|sym| {
                let name_lower = sym.qualified_name.to_lowercase();
                // Match on simple name or qualified name
                let simple_name = sym.qualified_name.rsplit('.').next().unwrap_or("");
                let simple_lower = simple_name.to_lowercase();

                simple_lower.contains(&query_lower) || name_lower.contains(&query_lower)
            })
            .collect()
    }

    /// Get all symbols in a specific file
    pub fn get_file_symbols(&self, uri: &Uri) -> Vec<&WorkspaceSymbol> {
        self.file_symbols
            .get(uri)
            .map(|names| {
                names
                    .iter()
                    .filter_map(|n| self.symbol_index.get(n))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Resolve a type reference from a given context
    ///
    /// This handles:
    /// - Simple names (look up in current class, then imports, then global)
    /// - Qualified names (try with within prefix first, then direct lookup)
    pub fn resolve_type(
        &self,
        type_name: &str,
        context_uri: &Uri,
        context_class: Option<&str>,
    ) -> Option<&WorkspaceSymbol> {
        // Try looking up in the context class first (both simple and qualified names)
        if let Some(class_name) = context_class {
            let qualified = format!("{}.{}", class_name, type_name);
            if let Some(sym) = self.lookup_symbol(&qualified) {
                return Some(sym);
            }
        }

        // Try looking up with the file's within prefix
        // This handles cases like "Interfaces.DiscreteSISO" in a file with
        // "within Modelica.Blocks;" -> "Modelica.Blocks.Interfaces.DiscreteSISO"
        if let Some(ast) = self.parsed_asts.get(context_uri)
            && let Some(within) = &ast.within
        {
            let qualified = format!("{}.{}", within, type_name);
            if let Some(sym) = self.lookup_symbol(&qualified) {
                return Some(sym);
            }
        }

        // Try direct lookup (for fully qualified names or top-level symbols)
        if let Some(sym) = self.lookup_symbol(type_name) {
            return Some(sym);
        }

        None
    }

    /// Get the parsed AST for a symbol by its qualified name
    ///
    /// This looks up the symbol, finds its file URI, and returns the parsed AST for that file.
    pub fn get_parsed_ast_by_name(&self, qualified_name: &str) -> Option<&StoredDefinition> {
        let sym = self.lookup_symbol(qualified_name)?;
        self.parsed_asts.get(&sym.uri)
    }

    /// Get the parsed AST for a URI
    pub fn get_parsed_ast(&self, uri: &Uri) -> Option<&StoredDefinition> {
        self.parsed_asts.get(uri)
    }

    /// Get all package roots
    pub fn package_roots(&self) -> &[PathBuf] {
        &self.package_roots
    }

    /// Get all discovered files
    pub fn discovered_files(&self) -> &HashSet<PathBuf> {
        &self.discovered_files
    }

    /// Get the number of indexed symbols
    pub fn symbol_count(&self) -> usize {
        self.symbol_index.len()
    }

    /// Load a file from disk if not already open
    pub fn ensure_file_loaded(&mut self, path: &Path) -> Option<Uri> {
        // Convert path to URI
        let uri = path_to_uri(path)?;

        // If already loaded, return the URI
        if self.documents.contains_key(&uri) {
            return Some(uri);
        }

        // Read and parse the file
        let text = std::fs::read_to_string(path).ok()?;
        self.open_document(uri.clone(), text);

        Some(uri)
    }

    /// Lazily load and index a package's contents for completion
    /// This is called when the user types "PackageName." to get sub-packages
    pub fn ensure_package_indexed(&mut self, package_prefix: &str) {
        // Split prefix into components (e.g., "Modelica.Blocks" -> ["Modelica", "Blocks"])
        let components: Vec<&str> = package_prefix.split('.').collect();
        if components.is_empty() {
            return;
        }

        // Find the package root that matches the first component
        // Use extract_package_name to handle versioned directory names (Modelica Spec 13.4)
        let top_level = components[0];
        let mut package_path: Option<PathBuf> = None;

        for root in &self.package_roots {
            if let Some(name) = root.file_name().and_then(|n| n.to_str())
                && extract_package_name(name) == top_level
            {
                package_path = Some(root.clone());
                break;
            }
        }

        let Some(mut current_path) = package_path else {
            return;
        };

        // Navigate to nested package if needed (e.g., Modelica/Blocks)
        for component in components.iter().skip(1) {
            let dir_path = current_path.join(component);
            let file_path = current_path.join(format!("{}.mo", component));

            if dir_path.is_dir() {
                // Directory package structure
                current_path = dir_path;
            } else if file_path.exists() {
                // Single-file package (e.g., Units.mo instead of Units/package.mo)
                self.debug_log(&format!(
                    "[workspace] Loading single-file package: {} from {:?}",
                    package_prefix, file_path
                ));
                let _ = self.ensure_file_loaded(&file_path);
                return; // No more nested directories to navigate
            } else {
                return;
            }
        }

        // Load the package.mo file to index its contents
        let package_mo = current_path.join("package.mo");
        if package_mo.exists() {
            self.debug_log(&format!(
                "[workspace] Lazy-loading package: {} from {:?}",
                package_prefix, package_mo
            ));
            let _ = self.ensure_file_loaded(&package_mo);

            // Scan directory contents
            if let Ok(entries) = std::fs::read_dir(&current_path) {
                for entry in entries.flatten() {
                    let path = entry.path();

                    if path.is_dir() {
                        // Sub-packages (directories with package.mo)
                        if path.join("package.mo").exists()
                            && let Some(subpkg_name) = path.file_name().and_then(|n| n.to_str())
                        {
                            let qualified_name = format!("{}.{}", package_prefix, subpkg_name);
                            let subpkg_mo = path.join("package.mo");
                            if let Some(path_str) = subpkg_mo.to_str() {
                                let uri_str = format!("file://{}", path_str);
                                if let Ok(uri) = uri_str.parse::<Uri>() {
                                    self.register_package_symbol(&qualified_name, &uri);
                                }
                            }
                        }
                    } else if path.extension().is_some_and(|ext| ext == "mo") {
                        // Individual .mo files (classes within the package)
                        // Skip package.mo as it's already loaded
                        if let Some(file_name) = path.file_name().and_then(|n| n.to_str())
                            && file_name != "package.mo"
                            && file_name != "package.order"
                        {
                            self.debug_log(&format!(
                                "[workspace] Loading package member: {:?}",
                                path
                            ));
                            let _ = self.ensure_file_loaded(&path);
                        }
                    }
                }
            }
        }
    }

    /// Get imports from a file
    pub fn get_imports(&self, uri: &Uri) -> Vec<String> {
        self.parsed_asts
            .get(uri)
            .map(|ast| {
                let mut imports = Vec::new();
                for class in ast.class_list.values() {
                    collect_imports(class, &mut imports);
                }
                imports
            })
            .unwrap_or_default()
    }
}

/// Collect all imports from a class recursively
fn collect_imports(class: &ClassDefinition, imports: &mut Vec<String>) {
    for import in &class.imports {
        imports.push(import_to_string(import));
    }

    for nested in class.classes.values() {
        collect_imports(nested, imports);
    }
}

/// Convert an Import to a string representation
fn import_to_string(import: &Import) -> String {
    match import {
        Import::Qualified { path, .. } => path.to_string(),
        Import::Renamed { alias, path, .. } => format!("{} = {}", alias.text, path),
        Import::Unqualified { path, .. } => format!("{}.*", path),
        Import::Selective { path, names, .. } => {
            let names_str: Vec<&str> = names.iter().map(|t| t.text.as_str()).collect();
            format!("{}.{{{}}}", path, names_str.join(", "))
        }
    }
}

/// Convert a file path to a URI
fn path_to_uri(path: &Path) -> Option<Uri> {
    let abs_path = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir().ok()?.join(path)
    };

    let path_str = abs_path.to_str()?;
    let uri_str = format!("file://{}", path_str);
    uri_str.parse().ok()
}

// ============================================================================
// SymbolLookup trait implementation for WorkspaceState
// ============================================================================

impl From<SymbolKind> for SymbolCategory {
    fn from(kind: SymbolKind) -> Self {
        match kind {
            SymbolKind::Package => SymbolCategory::Package,
            SymbolKind::Model => SymbolCategory::Model,
            SymbolKind::Class => SymbolCategory::Class,
            SymbolKind::Block => SymbolCategory::Block,
            SymbolKind::Connector => SymbolCategory::Connector,
            SymbolKind::Record => SymbolCategory::Record,
            SymbolKind::Type => SymbolCategory::Type,
            SymbolKind::Function => SymbolCategory::Function,
            SymbolKind::Operator => SymbolCategory::Operator,
            SymbolKind::Component => SymbolCategory::Component,
            SymbolKind::Parameter => SymbolCategory::Parameter,
            SymbolKind::Constant => SymbolCategory::Constant,
        }
    }
}

// ============================================================================
// Compilation support
// ============================================================================

impl WorkspaceState {
    /// Compile a model using pre-built library dictionaries from the workspace.
    ///
    /// This is the primary compilation entry point for WASM and LSP contexts.
    /// It uses the workspace's pre-cached library dictionaries for efficient
    /// compilation without re-parsing or re-merging library files.
    ///
    /// # Arguments
    ///
    /// * `user_def` - The user's parsed model
    /// * `model_name` - Name of the model to compile
    ///
    /// # Returns
    ///
    /// A `CompilationResult` containing the DAE and timing information
    pub fn compile(
        &self,
        user_def: &StoredDefinition,
        model_name: &str,
    ) -> anyhow::Result<crate::CompilationResult> {
        // Collect import roots from the user's model
        let import_roots = collect_import_roots_from_def(user_def);

        // Get pre-built library dicts for required libraries
        let library_dicts: Vec<std::sync::Arc<crate::ir::transform::flatten::ClassDict>> =
            import_roots
                .iter()
                .filter_map(|pkg_name| self.get_library_dict(pkg_name))
                .collect();

        // Use the pipeline function that accepts pre-built library dicts
        crate::compiler::pipeline::compile_with_library_dicts(user_def, &library_dicts, model_name)
    }

    /// Check balance for a model using pre-built library dictionaries.
    ///
    /// This is a lightweight version that only returns balance information,
    /// skipping the full compilation pipeline. Used by diagnostics.
    ///
    /// # Arguments
    ///
    /// * `user_def` - The user's parsed model
    /// * `model_name` - Name of the model to check (None for first class)
    ///
    /// # Returns
    ///
    /// A `BalanceResult` indicating whether the model has matching equations/unknowns
    pub fn check_balance(
        &self,
        user_def: &StoredDefinition,
        model_name: Option<&str>,
    ) -> anyhow::Result<BalanceResult> {
        // Collect import roots from the user's model
        let import_roots = collect_import_roots_from_def(user_def);

        // Get pre-built library dicts for required libraries
        let library_dicts: Vec<std::sync::Arc<crate::ir::transform::flatten::ClassDict>> =
            import_roots
                .iter()
                .filter_map(|pkg_name| self.get_library_dict(pkg_name))
                .collect();

        // Use the pipeline function that accepts pre-built library dicts
        crate::compiler::pipeline::check_balance_with_library_dicts(
            user_def,
            &library_dicts,
            model_name,
        )
    }
}

/// Collect root package names from imports in a StoredDefinition.
///
/// This extracts the first component of each import path (e.g., "Modelica" from
/// "Modelica.Blocks.Continuous.PID") to determine which libraries need to be loaded.
pub fn collect_import_roots_from_def(def: &StoredDefinition) -> std::collections::HashSet<String> {
    let mut roots = std::collections::HashSet::new();
    for class in def.class_list.values() {
        collect_import_roots_from_class(class, &mut roots);
    }
    roots
}

/// Collect root package names from imports in a class (recursively).
fn collect_import_roots_from_class(
    class: &ClassDefinition,
    roots: &mut std::collections::HashSet<String>,
) {
    for import in &class.imports {
        let path = import.base_path();
        if let Some(first) = path.name.first() {
            roots.insert(first.text.clone());
        }
    }
    // Recurse into nested classes
    for nested in class.classes.values() {
        collect_import_roots_from_class(nested, roots);
    }
}

impl SymbolLookup for WorkspaceState {
    fn lookup_symbol(&self, name: &str) -> Option<ExternalSymbol> {
        #[cfg(target_arch = "wasm32")]
        web_sys::console::log_1(
            &format!(
                "[workspace] lookup_symbol('{}'), symbol_index has {} entries",
                name,
                self.symbol_index.len()
            )
            .into(),
        );

        let ws_sym = self.symbol_index.get(name)?;

        #[cfg(target_arch = "wasm32")]
        web_sys::console::log_1(&format!("[workspace] found symbol: {:?}", ws_sym.kind).into());

        Some(ExternalSymbol {
            qualified_name: ws_sym.qualified_name.clone(),
            location: ws_sym.uri.to_string(),
            line: ws_sym.line,
            column: ws_sym.column,
            kind: ws_sym.kind.into(),
            detail: ws_sym.detail.clone(),
        })
    }

    fn get_ast_for_symbol(&self, qualified_name: &str) -> Option<&StoredDefinition> {
        #[cfg(target_arch = "wasm32")]
        web_sys::console::log_1(
            &format!(
                "[workspace] get_ast_for_symbol('{}'), symbol_index has {} entries",
                qualified_name,
                self.symbol_index.len()
            )
            .into(),
        );

        let sym = self.symbol_index.get(qualified_name);

        #[cfg(target_arch = "wasm32")]
        {
            if sym.is_none() {
                // Log some sample symbols to help debug
                let samples: Vec<_> = self.symbol_index.keys().take(5).collect();
                web_sys::console::log_1(
                    &format!(
                        "[workspace] symbol '{}' not found, sample keys: {:?}",
                        qualified_name, samples
                    )
                    .into(),
                );
            }
        }

        let sym = sym?;
        self.parsed_asts.get(&sym.uri)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workspace_state_new() {
        let ws = WorkspaceState::new();
        assert!(ws.documents.is_empty());
        assert!(ws.symbol_index.is_empty());
    }

    #[test]
    fn test_open_close_document() {
        let mut ws = WorkspaceState::new();
        let uri: Uri = "file:///tmp/test.mo".parse().unwrap();

        ws.open_document(uri.clone(), "model Test end Test;".to_string());
        assert!(ws.get_document(&uri).is_some());

        ws.close_document(&uri);
        assert!(ws.get_document(&uri).is_none());
    }
}
