//! Variability validation checks.
//!
//! MLS §4.4, §4.5: Variability constraints

use std::collections::HashMap;

use crate::ir::ast::{ClassDefinition, Expression, Variability};

use crate::ir::analysis::type_inference::SymbolType;

use super::super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check that constants have binding expressions.
///
/// MLS §4.4: "A constant shall have a declaration equation."
/// This validates that all components with `constant` variability have a binding.
pub fn check_constant_bindings(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    for (name, comp) in &class.components {
        // Check if this is a constant (not parameter)
        if let Variability::Constant(_) = &comp.variability {
            // Check if it has a real binding (not empty, not a modification, not synthetic default)
            let has_real_binding =
                if matches!(comp.start, Expression::Empty) || comp.start_is_modification {
                    false
                } else {
                    // Check if this is a synthetic default value (location 0,0,"")
                    // Real bindings have actual source locations
                    if let Some(loc) = comp.start.get_location() {
                        !(loc.start_line == 0 && loc.start_column == 0 && loc.file_name.is_empty())
                    } else {
                        true // No location info - assume real binding
                    }
                };

            if !has_real_binding {
                result.add_error(TypeError::new(
                    comp.name_token.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Constant '{}' must have a declaration equation (binding expression)",
                        name
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }
    }

    result
}

/// Check for variability dependency violations.
///
/// MLS §4.5: Variability constraints on binding equations:
/// - A constant binding cannot depend on parameters or variables
/// - A parameter binding cannot depend on variables
pub fn check_variability_dependencies(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of component names to their variability
    let mut comp_variability: HashMap<String, &Variability> = HashMap::new();
    for (name, comp) in &class.components {
        comp_variability.insert(name.clone(), &comp.variability);
    }

    // Check each component's binding expression
    for (name, comp) in &class.components {
        // Skip components without real bindings
        if matches!(comp.start, Expression::Empty) {
            continue;
        }

        // Skip modifications (we only check declaration equations)
        if comp.start_is_modification {
            continue;
        }

        // Skip synthetic bindings
        if let Some(loc) = comp.start.get_location()
            && loc.start_line == 0
            && loc.start_column == 0
            && loc.file_name.is_empty()
        {
            continue;
        }

        // Collect all component references from the binding
        let mut refs: Vec<(String, crate::ir::ast::Location)> = Vec::new();
        collect_component_refs(&comp.start, &mut refs);

        // Check each referenced component
        for (ref_name, ref_loc) in refs {
            // Skip self-references and built-ins like 'time'
            if ref_name == *name || ref_name == "time" {
                continue;
            }

            // Get the variability of the referenced component
            if let Some(ref_variability) = comp_variability.get(&ref_name) {
                // Check variability constraints
                match &comp.variability {
                    Variability::Constant(_) => {
                        // Constant cannot depend on parameter or variable
                        match ref_variability {
                            Variability::Parameter(_) => {
                                result.add_error(TypeError::new(
                                    ref_loc,
                                    SymbolType::Unknown,
                                    SymbolType::Unknown,
                                    format!(
                                        "Constant '{}' cannot depend on parameter '{}'",
                                        name, ref_name
                                    ),
                                    TypeErrorSeverity::Error,
                                ));
                            }
                            Variability::Empty | Variability::Discrete(_) => {
                                result.add_error(TypeError::new(
                                    ref_loc,
                                    SymbolType::Unknown,
                                    SymbolType::Unknown,
                                    format!(
                                        "Constant '{}' cannot depend on variable '{}'",
                                        name, ref_name
                                    ),
                                    TypeErrorSeverity::Error,
                                ));
                            }
                            _ => {}
                        }
                    }
                    Variability::Parameter(_) => {
                        // Parameter cannot depend on variable
                        match ref_variability {
                            Variability::Empty | Variability::Discrete(_) => {
                                result.add_error(TypeError::new(
                                    ref_loc,
                                    SymbolType::Unknown,
                                    SymbolType::Unknown,
                                    format!(
                                        "Parameter '{}' cannot depend on variable '{}'",
                                        name, ref_name
                                    ),
                                    TypeErrorSeverity::Error,
                                ));
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    result
}

/// Collect all component references from an expression
fn collect_component_refs(expr: &Expression, refs: &mut Vec<(String, crate::ir::ast::Location)>) {
    match expr {
        Expression::ComponentReference(comp_ref) => {
            if let Some(first) = comp_ref.parts.first() {
                refs.push((first.ident.text.clone(), first.ident.location.clone()));
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_component_refs(lhs, refs);
            collect_component_refs(rhs, refs);
        }
        Expression::Unary { rhs, .. } => {
            collect_component_refs(rhs, refs);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_component_refs(arg, refs);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_component_refs(cond, refs);
                collect_component_refs(then_expr, refs);
            }
            collect_component_refs(else_branch, refs);
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                collect_component_refs(elem, refs);
            }
        }
        Expression::Parenthesized { inner } => {
            collect_component_refs(inner, refs);
        }
        Expression::ArrayComprehension { expr, .. } => {
            collect_component_refs(expr, refs);
        }
        _ => {}
    }
}
