//! Assert function argument type checking.

use std::collections::HashMap;

use crate::ir::ast::{ClassDefinition, Equation, Expression, Variability};

use crate::ir::analysis::symbols::DefinedSymbol;
use crate::ir::analysis::type_inference::{SymbolType, infer_expression_type, type_from_name};

use super::super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check assert() function argument types.
///
/// This validates that:
/// - First argument (condition) is Boolean
/// - Second argument (message) is String
/// - Third argument (level, optional) is AssertionLevel enum with parameter variability
pub fn check_assert_arguments(class: &ClassDefinition) -> TypeCheckResult {
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

    // Check all equations for assert calls
    for eq in &class.equations {
        check_equation_assert(eq, &defined, &mut result);
    }

    result
}

/// Recursively check equations for assert function calls
fn check_equation_assert(
    eq: &Equation,
    defined: &HashMap<String, DefinedSymbol>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::FunctionCall { comp, args } => {
            let name = comp.to_string();
            if name == "assert" && !args.is_empty() {
                // Check first argument is Boolean
                let first_arg = &args[0];
                let first_type = infer_expression_type(first_arg, defined);
                if !matches!(
                    first_type.base_type(),
                    SymbolType::Boolean | SymbolType::Unknown
                ) && let Some(loc) = first_arg.get_location()
                {
                    result.add_error(TypeError::new(
                        loc.clone(),
                        SymbolType::Boolean,
                        first_type,
                        "Type mismatch for positional argument 1 in assert(condition=...). Expected Boolean".to_string(),
                        TypeErrorSeverity::Error,
                    ));
                }

                // Check second argument is String (if present)
                if args.len() >= 2 {
                    let second_arg = &args[1];
                    let second_type = infer_expression_type(second_arg, defined);
                    if !matches!(
                        second_type.base_type(),
                        SymbolType::String | SymbolType::Unknown
                    ) && let Some(loc) = second_arg.get_location()
                    {
                        result.add_error(TypeError::new(
                            loc.clone(),
                            SymbolType::String,
                            second_type,
                            "Type mismatch for positional argument 2 in assert(message=...). Expected String".to_string(),
                            TypeErrorSeverity::Error,
                        ));
                    }
                }

                // Check third argument is AssertionLevel (if present)
                if args.len() >= 3 {
                    let third_arg = &args[2];
                    let third_type = infer_expression_type(third_arg, defined);
                    // AssertionLevel is an enumeration with values 'warning' and 'error'
                    let is_valid = match third_type.base_type() {
                        SymbolType::Enumeration(name) => name == "AssertionLevel",
                        SymbolType::Unknown => true,
                        _ => false,
                    };
                    if !is_valid && let Some(loc) = third_arg.get_location() {
                        result.add_error(TypeError::new(
                            loc.clone(),
                            SymbolType::Enumeration("AssertionLevel".to_string()),
                            third_type,
                            "Type mismatch for positional argument 3 in assert(level=...). Expected enumeration AssertionLevel(warning, error)".to_string(),
                            TypeErrorSeverity::Error,
                        ));
                    }

                    // Check variability: level argument must be a parameter expression
                    // If it's a component reference to a non-parameter variable, report error
                    if let Expression::ComponentReference(comp_ref) = third_arg
                        && let Some(first) = comp_ref.parts.first()
                    {
                        let var_name = &first.ident.text;
                        if let Some(sym) = defined.get(var_name)
                            && !sym.is_parameter
                            && !sym.is_constant
                            && let Some(loc) = third_arg.get_location()
                        {
                            result.add_error(TypeError::new(
                                loc.clone(),
                                SymbolType::Unknown,
                                SymbolType::Unknown,
                                format!(
                                    "Function argument level={} in call to assert has variability discrete which is not a parameter expression",
                                    var_name
                                ),
                                TypeErrorSeverity::Error,
                            ));
                        }
                    }
                }
            }
        }
        Equation::For { equations, .. } => {
            for sub_eq in equations {
                check_equation_assert(sub_eq, defined, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for sub_eq in &block.eqs {
                    check_equation_assert(sub_eq, defined, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for sub_eq in &block.eqs {
                    check_equation_assert(sub_eq, defined, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_equation_assert(sub_eq, defined, result);
                }
            }
        }
        _ => {}
    }
}
