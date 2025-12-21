//! Algorithm section assignment checking.

use std::collections::HashMap;

use crate::ir::ast::{Causality, ClassDefinition, ClassType, Location, Statement, Variability};

use crate::ir::analysis::type_inference::SymbolType;

use super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check for invalid assignments in algorithm sections with additional class type info.
///
/// This version accepts peer class types from the file level for looking up
/// component types that reference other classes in the same file.
/// It also accepts original component types for components that have been flattened.
pub fn check_algorithm_assignments_with_types(
    class: &ClassDefinition,
    peer_class_types: &HashMap<String, ClassType>,
    original_comp_types: &HashMap<String, String>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of component variability, causality, and type info
    let mut comp_info: HashMap<String, (Variability, Causality, Location)> = HashMap::new();
    for (name, comp) in &class.components {
        comp_info.insert(
            name.clone(),
            (
                comp.variability.clone(),
                comp.causality.clone(),
                comp.name_token.location.clone(),
            ),
        );
    }

    // Build a map of class names to their class types (Model, Package, etc.)
    // Start with peer classes (file-level classes), then add nested classes
    let mut class_types: HashMap<String, ClassType> = peer_class_types.clone();
    for (name, nested_class) in &class.classes {
        class_types.insert(name.clone(), nested_class.class_type.clone());
    }

    // Build a map of component types (for checking if assigned component is model/package)
    // Start with original types (before flattening), then add flattened types
    let mut comp_types: HashMap<String, String> = original_comp_types.clone();
    for (name, comp) in &class.components {
        comp_types.insert(name.clone(), comp.type_name.to_string());
    }

    // Check all algorithm statements (algorithms is Vec<Vec<Statement>>)
    for algorithm_block in &class.algorithms {
        for stmt in algorithm_block {
            check_statement_assignments(stmt, &comp_info, &class_types, &comp_types, &mut result);
        }
    }

    // Recursively check nested classes (especially functions)
    for (_name, nested_class) in &class.classes {
        let nested_result =
            check_algorithm_assignments_with_types(nested_class, peer_class_types, &HashMap::new());
        for error in nested_result.errors {
            result.add_error(error);
        }
    }

    result
}

/// Check a single statement for invalid assignments
fn check_statement_assignments(
    stmt: &Statement,
    comp_info: &HashMap<String, (Variability, Causality, Location)>,
    class_types: &HashMap<String, ClassType>,
    comp_types: &HashMap<String, String>,
    result: &mut TypeCheckResult,
) {
    match stmt {
        Statement::Assignment { comp, value: _ } => {
            // Get the first part of the component reference (the variable name)
            if let Some(first) = comp.parts.first() {
                let var_name = &first.ident.text;

                // Check if the component's type is a model or package (not allowed to assign)
                if let Some(type_name) = comp_types.get(var_name)
                    && let Some(class_type) = class_types.get(type_name)
                {
                    match class_type {
                        ClassType::Model => {
                            result.add_error(TypeError::new(
                                first.ident.location.clone(),
                                SymbolType::Unknown,
                                SymbolType::Unknown,
                                format!("Component '{}' may not be assigned to due to class specialization 'model'.", var_name),
                                TypeErrorSeverity::Error,
                            ));
                            return;
                        }
                        ClassType::Package => {
                            result.add_error(TypeError::new(
                                first.ident.location.clone(),
                                SymbolType::Unknown,
                                SymbolType::Unknown,
                                format!("Component '{}' may not be assigned to due to class specialization 'package'.", var_name),
                                TypeErrorSeverity::Error,
                            ));
                            return;
                        }
                        _ => {}
                    }
                }

                if let Some((variability, causality, _)) = comp_info.get(var_name) {
                    // Check for assignment to constant
                    if matches!(variability, Variability::Constant(_)) {
                        result.add_error(TypeError::new(
                            first.ident.location.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            format!("Trying to assign to constant component in {} := ...", comp),
                            TypeErrorSeverity::Error,
                        ));
                    }
                    // Check for assignment to parameter
                    else if matches!(variability, Variability::Parameter(_)) {
                        result.add_error(TypeError::new(
                            first.ident.location.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            format!("Trying to assign to parameter component in {} := ...", comp),
                            TypeErrorSeverity::Error,
                        ));
                    }
                    // Check for assignment to input (in non-function context)
                    else if matches!(causality, Causality::Input(_)) {
                        result.add_error(TypeError::new(
                            first.ident.location.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            format!("Trying to assign to input component {}", var_name),
                            TypeErrorSeverity::Error,
                        ));
                    }
                }
            }
        }
        Statement::For { equations, .. } => {
            for sub_stmt in equations {
                check_statement_assignments(sub_stmt, comp_info, class_types, comp_types, result);
            }
        }
        Statement::While(block) => {
            for sub_stmt in &block.stmts {
                check_statement_assignments(sub_stmt, comp_info, class_types, comp_types, result);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for sub_stmt in &block.stmts {
                    check_statement_assignments(
                        sub_stmt,
                        comp_info,
                        class_types,
                        comp_types,
                        result,
                    );
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    check_statement_assignments(
                        sub_stmt,
                        comp_info,
                        class_types,
                        comp_types,
                        result,
                    );
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                for sub_stmt in &block.stmts {
                    check_statement_assignments(
                        sub_stmt,
                        comp_info,
                        class_types,
                        comp_types,
                        result,
                    );
                }
            }
        }
        _ => {}
    }
}
