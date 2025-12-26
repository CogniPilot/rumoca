//! Diagnostics computation for Modelica files.
//!
//! Provides enhanced diagnostics including:
//! - Parse errors
//! - Compilation errors
//! - Undefined variable references
//! - Unused variable warnings
//! - Missing parameter default warnings
//! - Type mismatch detection
//! - Array dimension warnings
//!
//! This module uses canonical scope resolution functions from
//! `crate::ir::transform::scope_resolver` to avoid duplication.

mod helpers;
mod symbols;

use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;
use lsp_types::{Diagnostic, DiagnosticSeverity, Uri};
use rayon::prelude::*;

use crate::compiler::extract_parse_error;
use crate::dae::balance::{BalanceResult, BalanceStatus};
use crate::ir::analysis::reference_checker::check_class_references;
use crate::ir::analysis::symbol_table::SymbolTable;
use crate::ir::analysis::symbols::{DefinedSymbol, is_class_instance_type};
use crate::ir::analysis::type_inference::type_from_name;
use crate::ir::ast::{Causality, ClassDefinition, ClassType, Variability};
use crate::ir::transform::scope_resolver::collect_inherited_components;

use crate::lsp::WorkspaceState;

use crate::ir::analysis::type_checker;
use helpers::create_diagnostic;
use symbols::type_errors_to_diagnostics;

/// Compute diagnostics for a document
pub fn compute_diagnostics(
    uri: &Uri,
    text: &str,
    workspace: &mut WorkspaceState,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    let path = uri.path().as_str();
    if path.ends_with(".mo") {
        use crate::modelica_grammar::ModelicaGrammar;
        use crate::modelica_parser::parse;

        let mut grammar = ModelicaGrammar::new();
        match parse(text, path, &mut grammar) {
            Ok(_) => {
                // Parsing succeeded - compute new balances (old ones will be overwritten)
                // Note: We don't clear balances here to avoid race conditions with CodeLens
                // which might request balance info during computation

                if let Some(ref ast) = grammar.modelica {
                    // Compile each class using the full Compiler pipeline (with library access)
                    // This gives us both the flattened class (for semantic analysis) and balance
                    compile_and_analyze_classes(uri, text, path, ast, workspace, &mut diagnostics);
                }
            }
            Err(e) => {
                // Clear cached balance on parse error (model is invalid)
                workspace.clear_balances(uri);
                // Use compiler's error extraction for consistent error messages
                let (line, col, message) = extract_parse_error(&e, text);
                diagnostics.push(create_diagnostic(
                    line,
                    col,
                    message,
                    DiagnosticSeverity::ERROR,
                ));
            }
        }
    }

    diagnostics
}

/// Analyze a class for semantic issues
/// `peer_classes` contains all top-level classes in the file (for looking up peer functions)
fn analyze_class(
    class: &ClassDefinition,
    peer_classes: &IndexMap<String, ClassDefinition>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Use parent_scope for top-level, will be passed in for nested classes
    analyze_class_with_scope(class, peer_classes, diagnostics, &SymbolTable::new());
}

/// Analyze a class with a parent scope for nested class support
fn analyze_class_with_scope(
    class: &ClassDefinition,
    peer_classes: &IndexMap<String, ClassDefinition>,
    diagnostics: &mut Vec<Diagnostic>,
    parent_scope: &SymbolTable,
) {
    // Build set of defined symbols (for location info in diagnostics)
    let mut defined: HashMap<String, DefinedSymbol> = HashMap::new();

    // Clone parent scope to build current scope (includes builtins from SymbolTable::new())
    let mut scope = parent_scope.clone();

    // Add peer functions from the same file (top-level functions)
    for (peer_name, peer_class) in peer_classes {
        if matches!(peer_class.class_type, ClassType::Function) {
            // Extract the return type from output components
            let function_return = peer_class
                .components
                .values()
                .find(|c| matches!(c.causality, Causality::Output(_)))
                .map(|output| {
                    (
                        type_from_name(&output.type_name.to_string()),
                        output.shape.clone(),
                    )
                });

            defined.insert(
                peer_name.clone(),
                DefinedSymbol {
                    name: peer_name.clone(),
                    line: peer_class.name.location.start_line,
                    col: peer_class.name.location.start_column,
                    is_parameter: false,
                    is_constant: false,
                    is_class: true,
                    has_default: true,
                    declared_type: type_from_name(peer_name),
                    shape: vec![],
                    function_return,
                },
            );
            // Also add to scope for nested class resolution
            scope.add_global(peer_name);
        }
    }

    // Collect component declarations
    for (comp_name, comp) in class.iter_components() {
        let (name, symbol) = DefinedSymbol::from_component(comp_name, comp);
        defined.insert(name.clone(), symbol);
        // Add to scope using SymbolTable
        let is_parameter = matches!(comp.variability, Variability::Parameter(_));
        scope.add_symbol(
            comp_name,
            comp_name,
            &comp.type_name.to_string(),
            is_parameter,
        );
    }

    // Add inherited components from extends clauses (using canonical function)
    // Track inherited names so we can skip them in unused variable warnings
    let inherited = collect_inherited_components(class, peer_classes);
    let mut inherited_names: HashSet<String> = HashSet::new();
    for (comp_name, (comp, _base_name)) in inherited {
        // Don't override if already defined (derived class takes precedence)
        if !defined.contains_key(&comp_name) {
            let (name, symbol) = DefinedSymbol::from_component(&comp_name, comp);
            inherited_names.insert(name.clone());
            defined.insert(name.clone(), symbol);
            // Add to scope
            let is_parameter = matches!(comp.variability, Variability::Parameter(_));
            scope.add_symbol(
                &comp_name,
                &comp_name,
                &comp.type_name.to_string(),
                is_parameter,
            );
        }
    }

    // Add nested class names as defined (these are types, not variables)
    // For functions, extract the return type from output components
    for (nested_name, nested_class) in class.iter_classes() {
        let (name, symbol) = DefinedSymbol::from_class(nested_name, nested_class);
        defined.insert(name, symbol);
        // Add to scope as global
        scope.add_global(nested_name);
    }

    // Use unified reference checker for undefined variable detection
    let ref_result = check_class_references(class, &defined, &scope);

    // Convert reference errors to diagnostics
    for error in &ref_result.errors {
        diagnostics.push(create_diagnostic(
            error.line,
            error.col,
            error.message.clone(),
            DiagnosticSeverity::ERROR,
        ));
    }

    // Run type checking on equations (with class lookup for member access resolution)
    for eq in &class.equations {
        let type_result =
            type_checker::check_equation_with_classes(eq, &defined, Some(peer_classes));
        diagnostics.extend(type_errors_to_diagnostics(&type_result));
    }

    // Run type checking on initial equations
    for eq in &class.initial_equations {
        let type_result =
            type_checker::check_equation_with_classes(eq, &defined, Some(peer_classes));
        diagnostics.extend(type_errors_to_diagnostics(&type_result));
    }

    // Run type checking on algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            let type_result =
                type_checker::check_statement_with_classes(stmt, &defined, Some(peer_classes));
            diagnostics.extend(type_errors_to_diagnostics(&type_result));
        }
    }

    // Run type checking on initial algorithms
    for algo in &class.initial_algorithms {
        for stmt in algo {
            let type_result =
                type_checker::check_statement_with_classes(stmt, &defined, Some(peer_classes));
            diagnostics.extend(type_errors_to_diagnostics(&type_result));
        }
    }

    // Check for unused variables (warning)
    // Skip for records, connectors, and partial classes since their fields are accessed externally
    // or will be used when the partial class is extended
    if !class.partial && !matches!(class.class_type, ClassType::Record | ClassType::Connector) {
        for (name, sym) in &defined {
            if !ref_result.used_symbols.contains(name) && !name.starts_with('_') {
                // Skip parameters, constants, classes, class instances (submodels), and inherited components
                // - Class instances contribute to the system even without explicit references
                // - Inherited components are used in their base class's equations
                if !sym.is_parameter
                    && !sym.is_constant
                    && !sym.is_class
                    && !is_class_instance_type(&sym.type_name())
                    && !inherited_names.contains(name)
                {
                    diagnostics.push(create_diagnostic(
                        sym.line,
                        sym.col,
                        format!("Variable '{}' is declared but never used", name),
                        DiagnosticSeverity::WARNING,
                    ));
                }
            }
        }
    }

    // Check for parameters without default values (hint)
    for (name, sym) in &defined {
        if sym.is_parameter && !sym.has_default {
            diagnostics.push(create_diagnostic(
                sym.line,
                sym.col,
                format!(
                    "Parameter '{}' has no default value - consider adding one",
                    name
                ),
                DiagnosticSeverity::HINT,
            ));
        }
    }

    // Recursively analyze nested classes, passing current scope
    for nested_class in class.classes.values() {
        analyze_class_with_scope(nested_class, peer_classes, diagnostics, &scope);
    }
}

// Note: collect_inherited_components is now imported from canonical module

/// Collect root package names from imports in a class (recursively)
fn collect_import_roots(class: &ClassDefinition, roots: &mut std::collections::HashSet<String>) {
    for import in &class.imports {
        let path = import.base_path();
        if let Some(first) = path.name.first() {
            roots.insert(first.text.clone());
        }
    }
    // Recurse into nested classes
    for nested in class.classes.values() {
        collect_import_roots(nested, roots);
    }
}

/// Compile and analyze all classes in the document.
/// Semantic analysis runs on original AST classes (pre-flattening) to match source code.
/// Compilation is used for balance checking (post-flattening).
fn compile_and_analyze_classes(
    uri: &Uri,
    _text: &str,
    _path: &str,
    ast: &crate::ir::ast::StoredDefinition,
    workspace: &mut WorkspaceState,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // First, run semantic analysis on original AST classes (pre-flattening)
    // This checks for undefined/unused variables against what the user wrote
    for class in ast.class_list.values() {
        analyze_class(class, &ast.class_list, diagnostics);
    }

    // Note: Import validation is deferred to the compiler, which has access to
    // the full library cache. The workspace symbol index may not have all
    // library symbols indexed, leading to false positives.

    // Collect all class paths that need compilation for balance checking
    let mut class_paths: Vec<(String, bool, ClassType)> = Vec::new();
    for (class_name, class) in &ast.class_list {
        collect_balance_classes(class, class_name, &mut class_paths);
    }

    // Collect all root package names from imports across all classes
    let mut import_roots: std::collections::HashSet<String> = std::collections::HashSet::new();
    for class in ast.class_list.values() {
        collect_import_roots(class, &mut import_roots);
    }

    let uri_clone = uri.clone();

    // Load required libraries into workspace cache (single-threaded, before parallel section)
    // This is the key optimization - libraries are pre-merged once and reused
    for pkg_name in &import_roots {
        if !workspace.has_library(pkg_name) {
            workspace.load_library(pkg_name);
        }
    }

    // Collect pre-built library class dictionaries
    // This is much more efficient than collecting StoredDefinitions because:
    // 1. Class dictionaries contain Arc<ClassDefinition> entries that can be shared
    // 2. Building the combined dictionary only clones Arc references, not the classes
    let library_dicts: Vec<std::sync::Arc<crate::ir::transform::flatten::ClassDict>> = import_roots
        .iter()
        .filter_map(|pkg_name| workspace.get_library_dict(pkg_name))
        .collect();

    // Use the already-parsed AST directly instead of re-parsing
    let user_ast = Some(ast.clone());

    // Compile all classes in parallel for balance checking only
    let results: Vec<_> = class_paths
        .par_iter()
        .filter_map(|(class_path, is_partial, class_type)| {
            // Only compile models, blocks, classes, and connectors
            if !matches!(
                class_type,
                ClassType::Model | ClassType::Block | ClassType::Class | ClassType::Connector
            ) {
                return None;
            }

            // Get user's parsed AST (returns None if parse failed)
            let user_def = user_ast.as_ref()?;

            // Time the compilation using class dictionary method (avoids cloning libraries)
            let start = std::time::Instant::now();
            let compile_result = crate::compiler::pipeline::check_balance_with_library_dicts(
                user_def,
                &library_dicts,
                Some(class_path),
            );
            let compile_time_ms = start.elapsed().as_millis() as u64;

            match compile_result {
                Ok(mut balance) => {
                    balance.compile_time_ms = compile_time_ms;
                    let is_connector = matches!(class_type, ClassType::Connector);
                    if (*is_partial || is_connector) && !balance.is_balanced() {
                        balance.status = BalanceStatus::Partial;
                    }

                    Some((class_path.clone(), balance))
                }
                Err(e) => {
                    // Errors are now raw (no miette formatting), just use the message directly
                    let mut balance = BalanceResult::compile_error(e.to_string());
                    balance.compile_time_ms = compile_time_ms;
                    Some((class_path.clone(), balance))
                }
            }
        })
        .collect();

    // Merge balance results (single-threaded)
    for (class_path, balance) in results {
        workspace.set_balance(uri_clone.clone(), class_path, balance);
    }
}

/// Recursively collect all class paths that need balance computation
fn collect_balance_classes(
    class: &ClassDefinition,
    class_path: &str,
    result: &mut Vec<(String, bool, ClassType)>,
) {
    result.push((
        class_path.to_string(),
        class.partial,
        class.class_type.clone(),
    ));

    // Recursively collect nested classes
    for (nested_name, nested_class) in class.iter_classes() {
        let nested_path = format!("{}.{}", class_path, nested_name);
        collect_balance_classes(nested_class, &nested_path, result);
    }
}
