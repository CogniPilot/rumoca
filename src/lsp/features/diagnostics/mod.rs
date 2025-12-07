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

mod helpers;
mod symbols;

use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;
use lsp_types::{Diagnostic, DiagnosticSeverity, Uri};
use rayon::prelude::*;

use crate::compiler::extract_parse_error;
use crate::dae::balance::BalanceStatus;
use crate::ir::analysis::symbols::{DefinedSymbol, is_class_instance_type};
use crate::ir::ast::{Causality, ClassDefinition, ClassType};
use crate::ir::transform::constants::global_builtins;
use crate::ir::transform::flatten::flatten;

use crate::lsp::WorkspaceState;

use crate::ir::analysis::type_checker;
use helpers::create_diagnostic;
use symbols::{
    collect_equation_symbols, collect_statement_symbols, collect_used_symbols,
    type_errors_to_diagnostics,
};

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
                // Parsing succeeded - clear stale balance cache since document changed
                workspace.clear_balances(uri);

                // Run semantic analysis on each class (using flattening for inherited symbols)
                if let Some(ref ast) = grammar.modelica {
                    // Collect class names for parallel processing
                    let class_names: Vec<_> = ast.class_list.keys().cloned().collect();
                    let class_list = &ast.class_list;

                    // Parallelize flattening and analysis across classes
                    let class_diagnostics: Vec<Vec<Diagnostic>> = class_names
                        .par_iter()
                        .filter_map(|class_name| {
                            if let Ok(fclass) = flatten(ast, Some(class_name)) {
                                let mut diags = Vec::new();
                                analyze_class(&fclass, class_list, &mut diags);
                                Some(diags)
                            } else {
                                None
                            }
                        })
                        .collect();

                    // Merge all diagnostics
                    for diags in class_diagnostics {
                        diagnostics.extend(diags);
                    }

                    // Compute balance for each model/block/class and cache it
                    compute_balance_for_classes(uri, text, path, ast, workspace);
                }
            }
            Err(e) => {
                // Clear cached balance on parse error
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
    // Build set of defined symbols
    let mut defined: HashMap<String, DefinedSymbol> = HashMap::new();
    let mut used: HashSet<String> = HashSet::new();

    // Add global builtins
    let globals: HashSet<String> = global_builtins().into_iter().collect();

    // Add peer functions from the same file (top-level functions)
    for (peer_name, peer_class) in peer_classes {
        if matches!(peer_class.class_type, ClassType::Function) {
            // Extract the return type from output components
            let function_return = peer_class
                .components
                .values()
                .find(|c| matches!(c.causality, Causality::Output(_)))
                .map(|output| (output.type_name.to_string(), output.shape.clone()));

            defined.insert(
                peer_name.clone(),
                DefinedSymbol {
                    line: peer_class.name.location.start_line,
                    col: peer_class.name.location.start_column,
                    is_parameter: false,
                    is_constant: false,
                    is_class: true,
                    has_default: true,
                    type_name: peer_name.clone(),
                    shape: vec![],
                    function_return,
                },
            );
        }
    }

    // Collect component declarations
    for (comp_name, comp) in &class.components {
        let (name, symbol) = DefinedSymbol::from_component(comp_name, comp);
        defined.insert(name, symbol);

        // Check references in start expression
        collect_used_symbols(&comp.start, &mut used);
    }

    // Add nested class names as defined (these are types, not variables)
    // For functions, extract the return type from output components
    for (nested_name, nested_class) in &class.classes {
        let (name, symbol) = DefinedSymbol::from_class(nested_name, nested_class);
        defined.insert(name, symbol);
    }

    // Collect symbols used in equations and run type checking
    for eq in &class.equations {
        collect_equation_symbols(eq, &mut used, diagnostics, &defined, &globals);
        // Type check the equation using the shared type checker
        let type_result = type_checker::check_equation(eq, &defined);
        diagnostics.extend(type_errors_to_diagnostics(&type_result));
    }

    // Collect symbols used in initial equations and run type checking
    for eq in &class.initial_equations {
        collect_equation_symbols(eq, &mut used, diagnostics, &defined, &globals);
        let type_result = type_checker::check_equation(eq, &defined);
        diagnostics.extend(type_errors_to_diagnostics(&type_result));
    }

    // Collect symbols used in algorithms and run type checking
    for algo in &class.algorithms {
        for stmt in algo {
            collect_statement_symbols(stmt, &mut used, diagnostics, &defined, &globals);
            let type_result = type_checker::check_statement(stmt, &defined);
            diagnostics.extend(type_errors_to_diagnostics(&type_result));
        }
    }

    // Collect symbols used in initial algorithms and run type checking
    for algo in &class.initial_algorithms {
        for stmt in algo {
            collect_statement_symbols(stmt, &mut used, diagnostics, &defined, &globals);
            let type_result = type_checker::check_statement(stmt, &defined);
            diagnostics.extend(type_errors_to_diagnostics(&type_result));
        }
    }

    // Check for unused variables (warning)
    // Skip for records, connectors, and partial classes since their fields are accessed externally
    // or will be used when the partial class is extended
    if !class.partial && !matches!(class.class_type, ClassType::Record | ClassType::Connector) {
        for (name, sym) in &defined {
            if !used.contains(name) && !name.starts_with('_') {
                // Skip parameters, classes, and class instances (submodels)
                // Class instances contribute to the system even without explicit references
                if !sym.is_parameter && !sym.is_class && !is_class_instance_type(&sym.type_name) {
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

    // Recursively analyze nested classes
    for nested_class in class.classes.values() {
        analyze_class(nested_class, peer_classes, diagnostics);
    }
}

/// Compute balance for all model/block/class definitions and cache the results
fn compute_balance_for_classes(
    uri: &Uri,
    text: &str,
    path: &str,
    ast: &crate::ir::ast::StoredDefinition,
    workspace: &mut WorkspaceState,
) {
    // Collect all class paths that need balance computation
    let mut class_paths: Vec<(String, bool, ClassType)> = Vec::new();
    for (class_name, class) in &ast.class_list {
        collect_balance_classes(class, class_name, &mut class_paths);
    }

    // Compute balance in parallel for all classes
    let uri_clone = uri.clone();
    let text_owned = text.to_string();
    let path_owned = path.to_string();

    let balance_results: Vec<_> = class_paths
        .par_iter()
        .filter_map(|(class_path, is_partial, class_type)| {
            // Compute balance for models, blocks, classes, and connectors (not functions, records, etc.)
            if matches!(
                class_type,
                ClassType::Model | ClassType::Block | ClassType::Class | ClassType::Connector
            ) {
                // Try to compile and get balance - silently ignore errors
                // Note: Using threads(1) here since we're already parallelizing at the outer level
                if let Ok(result) = crate::Compiler::new()
                    .model(class_path)
                    .threads(1)
                    .compile_str(&text_owned, &path_owned)
                {
                    let mut balance = result.dae.check_balance();

                    // Mark as Partial if:
                    // - Class is explicitly declared with `partial` keyword, or
                    // - Class is a connector (connectors are partial by design)
                    let is_connector = matches!(class_type, ClassType::Connector);
                    if (*is_partial || is_connector) && !balance.is_balanced {
                        balance.status = BalanceStatus::Partial;
                    }

                    return Some((class_path.clone(), balance));
                }
            }
            None
        })
        .collect();

    // Merge results into workspace (single-threaded)
    for (class_path, balance) in balance_results {
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
    for (nested_name, nested_class) in &class.classes {
        let nested_path = format!("{}.{}", class_path, nested_name);
        collect_balance_classes(nested_class, &nested_path, result);
    }
}
