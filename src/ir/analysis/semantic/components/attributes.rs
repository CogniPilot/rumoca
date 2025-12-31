//! Builtin attribute type checking.

use std::collections::HashMap;

use crate::ir::ast::{ClassDefinition, Expression, Variability};

use crate::ir::analysis::symbols::DefinedSymbol;
use crate::ir::analysis::type_inference::{SymbolType, infer_expression_type, type_from_name};

use super::super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Definition of valid attributes for built-in types.
/// Each attribute has a name and expected type.
struct BuiltinAttribute {
    name: &'static str,
    expected_type: SymbolType,
}

/// Get valid attributes for a built-in type name
fn get_builtin_attributes(type_name: &str) -> Option<&'static [BuiltinAttribute]> {
    static REAL_ATTRS: &[BuiltinAttribute] = &[
        BuiltinAttribute {
            name: "start",
            expected_type: SymbolType::Real,
        },
        BuiltinAttribute {
            name: "fixed",
            expected_type: SymbolType::Boolean,
        },
        BuiltinAttribute {
            name: "min",
            expected_type: SymbolType::Real,
        },
        BuiltinAttribute {
            name: "max",
            expected_type: SymbolType::Real,
        },
        BuiltinAttribute {
            name: "nominal",
            expected_type: SymbolType::Real,
        },
        BuiltinAttribute {
            name: "unit",
            expected_type: SymbolType::String,
        },
        BuiltinAttribute {
            name: "displayUnit",
            expected_type: SymbolType::String,
        },
        BuiltinAttribute {
            name: "quantity",
            expected_type: SymbolType::String,
        },
        BuiltinAttribute {
            name: "unbounded",
            expected_type: SymbolType::Boolean,
        },
        // stateSelect is handled specially as an enumeration
    ];

    static INTEGER_ATTRS: &[BuiltinAttribute] = &[
        BuiltinAttribute {
            name: "start",
            expected_type: SymbolType::Integer,
        },
        BuiltinAttribute {
            name: "fixed",
            expected_type: SymbolType::Boolean,
        },
        BuiltinAttribute {
            name: "min",
            expected_type: SymbolType::Integer,
        },
        BuiltinAttribute {
            name: "max",
            expected_type: SymbolType::Integer,
        },
        BuiltinAttribute {
            name: "quantity",
            expected_type: SymbolType::String,
        },
    ];

    static BOOLEAN_ATTRS: &[BuiltinAttribute] = &[
        BuiltinAttribute {
            name: "start",
            expected_type: SymbolType::Boolean,
        },
        BuiltinAttribute {
            name: "fixed",
            expected_type: SymbolType::Boolean,
        },
        BuiltinAttribute {
            name: "quantity",
            expected_type: SymbolType::String,
        },
    ];

    static STRING_ATTRS: &[BuiltinAttribute] = &[
        BuiltinAttribute {
            name: "start",
            expected_type: SymbolType::String,
        },
        BuiltinAttribute {
            name: "fixed",
            expected_type: SymbolType::Boolean,
        },
        BuiltinAttribute {
            name: "quantity",
            expected_type: SymbolType::String,
        },
    ];

    match type_name {
        "Real" => Some(REAL_ATTRS),
        "Integer" => Some(INTEGER_ATTRS),
        "Boolean" | "Bool" => Some(BOOLEAN_ATTRS),
        "String" => Some(STRING_ATTRS),
        _ => None,
    }
}

/// Check if an attribute name is valid for a built-in type
fn is_valid_attribute(type_name: &str, attr_name: &str) -> bool {
    if let Some(attrs) = get_builtin_attributes(type_name) {
        attrs.iter().any(|a| a.name == attr_name) || attr_name == "stateSelect"
    } else {
        true // Non-builtin types can have any attributes
    }
}

/// Get the expected type for an attribute of a built-in type
fn get_attribute_expected_type(type_name: &str, attr_name: &str) -> Option<SymbolType> {
    if let Some(attrs) = get_builtin_attributes(type_name) {
        attrs
            .iter()
            .find(|a| a.name == attr_name)
            .map(|a| a.expected_type.clone())
    } else {
        None
    }
}

/// Find a continuous (non-parameter, non-constant) variable in an expression.
/// Returns the variable name if found, None otherwise.
fn find_continuous_variable(
    expr: &Expression,
    defined: &HashMap<String, DefinedSymbol>,
) -> Option<String> {
    match expr {
        Expression::ComponentReference(comp) => {
            if let Some(first) = comp.parts.first() {
                let var_name = &first.ident.text;
                if let Some(symbol) = defined.get(var_name) {
                    // Continuous = not parameter and not constant
                    if !symbol.is_parameter && !symbol.is_constant {
                        return Some(var_name.clone());
                    }
                }
            }
            None
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                if let Some(var) = find_continuous_variable(arg, defined) {
                    return Some(var);
                }
            }
            None
        }
        Expression::Binary { lhs, rhs, .. } => find_continuous_variable(lhs, defined)
            .or_else(|| find_continuous_variable(rhs, defined)),
        Expression::Unary { rhs, .. } => find_continuous_variable(rhs, defined),
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                if let Some(var) = find_continuous_variable(cond, defined) {
                    return Some(var);
                }
                if let Some(var) = find_continuous_variable(then_expr, defined) {
                    return Some(var);
                }
            }
            find_continuous_variable(else_branch, defined)
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                if let Some(var) = find_continuous_variable(elem, defined) {
                    return Some(var);
                }
            }
            None
        }
        Expression::Parenthesized { inner } => find_continuous_variable(inner, defined),
        _ => None,
    }
}

/// Check builtin attribute modifiers for type validity.
///
/// This validates that:
/// - Modifier names are valid for the type (e.g., `x` is not valid for Real)
/// - Modifier values have the correct type (e.g., start should be Real, not String)
pub fn check_builtin_attribute_modifiers(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of defined symbols for type inference
    let mut defined: HashMap<String, DefinedSymbol> = HashMap::new();
    for (name, comp) in &class.components {
        let declared_type = type_from_name(&comp.type_name.to_string());
        defined.insert(
            name.clone(),
            DefinedSymbol {
                name: name.clone(),
                declared_type,
                line: comp.name_token.location.start_line,
                col: comp.name_token.location.start_column,
                is_parameter: matches!(comp.variability, Variability::Parameter(_)),
                is_constant: matches!(comp.variability, Variability::Constant(_)),
                is_class: false,
                has_default: !matches!(comp.start, Expression::Empty),
                shape: comp.shape.clone(),
                function_return: None,
            },
        );
    }

    // Check each component's modifications
    for (_comp_name, comp) in &class.components {
        let type_name = comp.type_name.to_string();

        // Skip non-builtin types
        if get_builtin_attributes(&type_name).is_none() {
            continue;
        }

        for (mod_name, mod_expr) in &comp.modifications {
            // Check if the modifier name is valid for this type
            if !is_valid_attribute(&type_name, mod_name) {
                if let Some(loc) = mod_expr.get_location() {
                    result.add_error(TypeError::new(
                        loc.clone(),
                        SymbolType::Unknown,
                        SymbolType::Unknown,
                        format!(
                            "Modified element {} not found in class {}",
                            mod_name, type_name
                        ),
                        TypeErrorSeverity::Error,
                    ));
                }
                continue;
            }

            // Check if the modifier value has the correct type
            if let Some(expected_type) = get_attribute_expected_type(&type_name, mod_name) {
                let actual_type = infer_expression_type(mod_expr, &defined);

                if !expected_type.is_compatible_with(&actual_type)
                    && !matches!(actual_type, SymbolType::Unknown)
                    && let Some(loc) = mod_expr.get_location()
                {
                    // Format message first, then move types to avoid cloning
                    let message = format!(
                        "Type mismatch in binding {} = ..., expected subtype of {}, got type {}",
                        mod_name, expected_type, actual_type
                    );
                    result.add_error(TypeError::new(
                        loc.clone(),
                        expected_type,
                        actual_type,
                        message,
                        TypeErrorSeverity::Error,
                    ));
                }
            }
        }

        // Check the start field if it's a modification (not a binding)
        // This validates `Real x(start = "fish")` - start should be Real, not String
        if comp.start_is_modification && !matches!(comp.start, Expression::Empty) {
            let scalar_start_type = get_attribute_expected_type(&type_name, "start");
            if let Some(scalar_type) = scalar_start_type {
                // Build expected type considering component's array shape
                // For `Real x[3](start = {1,2,3})`, expected is Real[3], not just Real
                let expected_type = if comp.shape.is_empty() {
                    scalar_type.clone()
                } else {
                    // Build array type from innermost to outermost dimension
                    let mut t = scalar_type.clone();
                    for &dim in comp.shape.iter().rev() {
                        t = SymbolType::Array(Box::new(t), Some(dim));
                    }
                    t
                };

                let actual_type = infer_expression_type(&comp.start, &defined);

                // For array components, also allow scalar start values (broadcast)
                // e.g., `Real x[3](each start = 1)` uses scalar start
                let is_compatible = expected_type.is_compatible_with(&actual_type)
                    || (!comp.shape.is_empty() && scalar_type.is_compatible_with(&actual_type));

                if !is_compatible
                    && !matches!(actual_type, SymbolType::Unknown)
                    && let Some(loc) = comp.start.get_location()
                {
                    // Format message first, then move types to avoid cloning
                    let message = format!(
                        "Type mismatch in binding start = ..., expected subtype of {}, got type {}",
                        expected_type, actual_type
                    );
                    result.add_error(TypeError::new(
                        loc.clone(),
                        expected_type,
                        actual_type,
                        message,
                        TypeErrorSeverity::Error,
                    ));
                }

                // Check variability: start should have at most parameter variability
                // e.g., `Real y(start = x)` where x is continuous should fail
                if let Some(continuous_var) = find_continuous_variable(&comp.start, &defined)
                    && let Some(loc) = comp.start.get_location()
                {
                    result.add_error(TypeError::new(
                        loc.clone(),
                        SymbolType::Unknown,
                        SymbolType::Unknown,
                        format!(
                            "Component start of variability parameter has binding '{}' of higher variability continuous.",
                            continuous_var
                        ),
                        TypeErrorSeverity::Error,
                    ));
                }
            }
        }
    }

    result
}
