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

mod errors;
mod helpers;
mod symbols;
mod types;

use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;
use lsp_types::{Diagnostic, DiagnosticSeverity, Uri};

use crate::ir::ast::{Causality, ClassDefinition, ClassType, Expression, Variability};
use crate::ir::transform::constants::global_builtins;
use crate::ir::transform::flatten::flatten;

use crate::lsp::WorkspaceState;

use errors::extract_structured_error;
use helpers::create_diagnostic;
use symbols::{collect_equation_symbols, collect_statement_symbols, collect_used_symbols};
use types::{DefinedSymbol, is_class_instance_type};

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
                // Balance will be recomputed on-demand when user clicks "Analyze"
                workspace.clear_balances(uri);

                // Run semantic analysis on each class (using flattening for inherited symbols)
                if let Some(ref ast) = grammar.modelica {
                    for class_name in ast.class_list.keys() {
                        if let Ok(fclass) = flatten(ast, Some(class_name)) {
                            analyze_class(&fclass, &ast.class_list, &mut diagnostics);
                        }
                    }
                }
            }
            Err(e) => {
                // Clear cached balance on parse error
                workspace.clear_balances(uri);
                // Use structured error extraction when possible
                let (line, col, message) = extract_structured_error(&e, text);
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
        let line = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_line)
            .unwrap_or(1);
        let col = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_column)
            .unwrap_or(1);

        let has_start = !matches!(comp.start, Expression::Empty);
        let is_parameter = matches!(comp.variability, Variability::Parameter(_));
        let type_name = comp.type_name.to_string();

        defined.insert(
            comp_name.clone(),
            DefinedSymbol {
                line,
                col,
                is_parameter,
                is_class: false,
                has_default: has_start,
                type_name: type_name.clone(),
                shape: comp.shape.clone(),
                function_return: None,
            },
        );

        // Check references in start expression
        collect_used_symbols(&comp.start, &mut used);
    }

    // Add nested class names as defined (these are types, not variables)
    // For functions, extract the return type from output components
    for (nested_name, nested_class) in &class.classes {
        // Check if this is a function and extract its return type
        let function_return = if matches!(nested_class.class_type, ClassType::Function) {
            // Find the output component(s) - typically just one for the return value
            nested_class
                .components
                .values()
                .find(|c| matches!(c.causality, Causality::Output(_)))
                .map(|output| (output.type_name.to_string(), output.shape.clone()))
        } else {
            None
        };

        defined.insert(
            nested_name.clone(),
            DefinedSymbol {
                line: nested_class.name.location.start_line,
                col: nested_class.name.location.start_column,
                is_parameter: false,
                is_class: true,
                has_default: true,
                type_name: nested_name.clone(), // class type
                shape: vec![],
                function_return,
            },
        );
    }

    // Collect symbols used in equations
    for eq in &class.equations {
        collect_equation_symbols(eq, &mut used, diagnostics, &defined, &globals);
    }

    // Collect symbols used in initial equations
    for eq in &class.initial_equations {
        collect_equation_symbols(eq, &mut used, diagnostics, &defined, &globals);
    }

    // Collect symbols used in algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            collect_statement_symbols(stmt, &mut used, diagnostics, &defined, &globals);
        }
    }

    // Collect symbols used in initial algorithms
    for algo in &class.initial_algorithms {
        for stmt in algo {
            collect_statement_symbols(stmt, &mut used, diagnostics, &defined, &globals);
        }
    }

    // Check for unused variables (warning)
    // Skip for records and connectors since their fields are accessed externally
    if !matches!(class.class_type, ClassType::Record | ClassType::Connector) {
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
