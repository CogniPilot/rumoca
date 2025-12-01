//! Workspace state manager for multi-file LSP support.
//!
//! Provides:
//! - Tracking of all open documents and their parsed ASTs
//! - Package structure discovery and management
//! - Cross-file symbol lookup
//! - Dependency tracking between files

// Allow mutable key type warning - Uri has interior mutability but we use it correctly
#![allow(clippy::mutable_key_type)]

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use lsp_types::Uri;

use crate::ir::ast::{ClassDefinition, ClassType, Import, StoredDefinition};
use crate::ir::multi_file::{discover_modelica_files, get_modelica_path, is_modelica_package};

use super::utils::parse_document;

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
        }
    }

    /// Initialize workspace with root folders
    pub fn initialize(&mut self, workspace_folders: Vec<PathBuf>) {
        self.workspace_roots = workspace_folders.clone();

        // Add MODELICAPATH directories
        self.package_roots.extend(get_modelica_path());

        // Discover packages in workspace folders
        for folder in workspace_folders {
            self.discover_packages_in_folder(&folder);
        }
    }

    /// Discover Modelica packages in a folder
    fn discover_packages_in_folder(&mut self, folder: &Path) {
        if is_modelica_package(folder) {
            self.package_roots.push(folder.to_path_buf());
            // Discover all files in this package
            if let Ok(files) = discover_modelica_files(folder) {
                for file in files {
                    self.discovered_files.insert(file);
                }
            }
        } else if folder.is_dir() {
            // Look for packages in subdirectories
            if let Ok(entries) = std::fs::read_dir(folder) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.is_dir() {
                        self.discover_packages_in_folder(&path);
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
    /// - Qualified names (direct lookup)
    pub fn resolve_type(
        &self,
        type_name: &str,
        context_uri: &Uri,
        context_class: Option<&str>,
    ) -> Option<&WorkspaceSymbol> {
        // If it's a qualified name, look it up directly
        if type_name.contains('.') {
            return self.lookup_symbol(type_name);
        }

        // Try looking up in the context class first
        if let Some(class_name) = context_class {
            let qualified = format!("{}.{}", class_name, type_name);
            if let Some(sym) = self.lookup_symbol(&qualified) {
                return Some(sym);
            }
        }

        // Try looking up with the file's within prefix
        if let Some(ast) = self.parsed_asts.get(context_uri) {
            if let Some(within) = &ast.within {
                let qualified = format!("{}.{}", within, type_name);
                if let Some(sym) = self.lookup_symbol(&qualified) {
                    return Some(sym);
                }
            }
        }

        // Try as a top-level symbol
        self.lookup_symbol(type_name)
    }

    /// Get all package roots
    pub fn package_roots(&self) -> &[PathBuf] {
        &self.package_roots
    }

    /// Get all discovered files
    pub fn discovered_files(&self) -> &HashSet<PathBuf> {
        &self.discovered_files
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
        Import::Qualified { path } => path.to_string(),
        Import::Renamed { alias, path } => format!("{} = {}", alias.text, path),
        Import::Unqualified { path } => format!("{}.*", path),
        Import::Selective { path, names } => {
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
