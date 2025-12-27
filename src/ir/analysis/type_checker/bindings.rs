//! Component binding type checking.

use std::collections::HashMap;

use crate::ir::ast::{ClassDefinition, Expression};

use crate::ir::analysis::symbols::DefinedSymbol;
use crate::ir::analysis::type_inference::{infer_expression_type, type_from_name};

use super::expressions::validate_expression;
use super::types::{build_array_type, get_array_dimensions, has_inferred_dimensions};
use super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check component binding types in a flattened class.
///
/// This validates that binding expressions have types compatible with their
/// declared component types. For example, `Real x = "hello"` would be flagged
/// as a type mismatch since String is not compatible with Real.
pub fn check_component_bindings(class: &ClassDefinition) -> TypeCheckResult {
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
                is_parameter: false,
                is_constant: false,
                is_class: false,
                has_default: !matches!(comp.start, Expression::Empty),
                shape: comp.shape.clone(),
                function_return: None,
            },
        );
    }

    // Check each component's binding expression
    for (name, comp) in &class.components {
        // Skip if there's no binding (Empty expression)
        if matches!(comp.start, Expression::Empty) {
            continue;
        }

        // Skip if the binding is a modification (start=x) rather than a declaration binding (= x)
        if comp.start_is_modification {
            continue;
        }

        // Skip if the binding is a synthetic default (no real source location)
        // Default values from type definitions have location (0, 0, "")
        let is_synthetic = if let Some(loc) = comp.start.get_location() {
            loc.start_line == 0 && loc.start_column == 0 && loc.file_name.is_empty()
        } else {
            false // No location is OK - check the binding
        };
        if is_synthetic {
            continue;
        }

        // Validate the expression itself (check for invalid operator usage)
        let expr_result = validate_expression(&comp.start, &defined);
        result.merge(expr_result);

        // Build the full declared type including array dimensions
        let base_type = type_from_name(&comp.type_name.to_string());
        let declared_type = build_array_type(base_type.clone(), &comp.shape);

        // Infer binding expression type
        let binding_type = infer_expression_type(&comp.start, &defined);

        // Get base types for comparison
        let declared_base = declared_type.base_type().clone();
        let binding_base = binding_type.base_type().clone();

        let location = comp
            .start
            .get_location()
            .cloned()
            .unwrap_or_else(|| comp.name_token.location.clone());

        // Check for base type compatibility first
        // For user-defined types (Class), be lenient - they may be aliases for Real/Integer
        if !declared_base.is_compatible_with(&binding_base)
            && !matches!(
                binding_base,
                crate::ir::analysis::type_inference::SymbolType::Unknown
            )
            && !matches!(
                declared_base,
                crate::ir::analysis::type_inference::SymbolType::Class(_)
            )
        {
            // Format message first, then move types to avoid cloning
            let message = format!(
                "Type mismatch in binding {} = ..., expected subtype of {}, got type {}",
                name, declared_type, binding_type
            );
            result.add_error(TypeError::new(
                location,
                declared_type,
                binding_type,
                message,
                TypeErrorSeverity::Error,
            ));
            continue;
        }

        // Skip dimension checking if declared type uses inferred dimensions (`:`)
        // e.g., `parameter Real a[:] = {1, 2, 3}` is valid
        if has_inferred_dimensions(&comp.shape_expr) {
            continue;
        }

        // Check array dimension compatibility
        let declared_dims = get_array_dimensions(&declared_type);
        let binding_dims = get_array_dimensions(&binding_type);

        // Case 1: Scalar declared, array binding (BindingInvalidType2)
        // Real x = {1, 2, 3} - shape is [] but binding is an array
        if declared_dims.is_empty() && !binding_dims.is_empty() {
            // Format message first, then move types to avoid cloning
            let message = format!(
                "Type mismatch in binding '{}' = ..., expected array dimensions [], got {:?}",
                name, binding_dims
            );
            result.add_error(TypeError::new(
                location,
                declared_type,
                binding_type,
                message,
                TypeErrorSeverity::Error,
            ));
            continue;
        }

        // Case 2: Array declared with fixed dims, scalar binding without 'each' (BindingInvalidType4)
        // Skip if the binding is an array type (even with unknown dimensions, like ones(n))
        // Also skip for subcomponents (names with '.') - these inherit scalar bindings from
        // their class definitions and broadcasting is valid (e.g., A a[3] where A has Real x = 1.0)
        let is_array_binding = matches!(
            binding_type,
            crate::ir::analysis::type_inference::SymbolType::Array(_, _)
        );
        let is_subcomponent = name.contains('.');
        if !declared_dims.is_empty()
            && binding_dims.is_empty()
            && !matches!(
                binding_type,
                crate::ir::analysis::type_inference::SymbolType::Unknown
            )
            && !is_array_binding
            && !is_subcomponent
        {
            // Format message first, then move types to avoid cloning
            let message = format!(
                "Non-array modification for array component '{}', possibly due to missing 'each'",
                name
            );
            result.add_error(TypeError::new(
                location,
                declared_type,
                binding_type,
                message,
                TypeErrorSeverity::Error,
            ));
            continue;
        }

        // Case 3: Array dimensions don't match (BindingInvalidType3)
        if !declared_dims.is_empty() && !binding_dims.is_empty() && declared_dims != binding_dims {
            // Format message first, then move types to avoid cloning
            let message = format!(
                "Type mismatch in binding '{}' = ..., expected array dimensions {:?}, got {:?}",
                name, declared_dims, binding_dims
            );
            result.add_error(TypeError::new(
                location,
                declared_type,
                binding_type,
                message,
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}
