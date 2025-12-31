//! Operator record restrictions.
//!
//! MLS §14.3: Operator record restrictions

use indexmap::IndexMap;

use crate::ir::ast::ClassDefinition;

use crate::ir::analysis::type_inference::SymbolType;

use super::super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check that operator records do not use extends (MLS §14.3).
///
/// MLS §14.3: "An operator record cannot extend another class."
/// This is because operator records have special semantics for overloaded operators
/// that don't compose well with inheritance.
pub fn check_operator_record_no_extends(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check operator records
    if !class.operator_record {
        return result;
    }

    // Check if the class has any extends clauses
    if !class.extends.is_empty() {
        for ext in &class.extends {
            result.add_error(TypeError::new(
                ext.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Operator record '{}' cannot extend other classes",
                    class.name.text
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}

/// Check that operator records cannot be extended (MLS §14.3).
///
/// MLS §14.3: "An operator record cannot be used as a base class."
/// This validation requires checking all classes in the stored definition to find
/// any that extend an operator record.
pub fn check_operator_record_cannot_be_extended(
    class: &ClassDefinition,
    all_classes: &IndexMap<String, ClassDefinition>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check if this class extends any operator records
    for ext in &class.extends {
        // Get the base class name
        let base_name = ext.comp.to_string();

        // Look up the base class
        if let Some(base_class) = all_classes.get(&base_name)
            && base_class.operator_record
        {
            result.add_error(TypeError::new(
                ext.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Cannot extend operator record '{}' - operator records cannot be used as base classes",
                    base_name
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}

/// Check that operator records cannot be partial (MLS §14.3).
///
/// MLS §14.3: "An operator record shall not be partial."
/// Operator records must be complete definitions with all required operators.
pub fn check_operator_record_no_partial(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check operator records
    if !class.operator_record {
        return result;
    }

    // Check if the class is partial
    if class.partial {
        result.add_error(TypeError::new(
            class.location.clone(),
            SymbolType::Unknown,
            SymbolType::Unknown,
            format!(
                "Operator record '{}' cannot be declared as partial",
                class.name.text
            ),
            TypeErrorSeverity::Error,
        ));
    }

    result
}
