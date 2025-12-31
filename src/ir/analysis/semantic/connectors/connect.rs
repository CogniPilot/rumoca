//! Connect equation validation.
//!
//! MLS §9.3: Connect equations and connector compatibility

use std::collections::{HashMap, HashSet};

use crate::ir::ast::{
    ClassDefinition, ClassType, Component, ComponentReference, Connection, Equation, Expression,
    Subscript,
};

use crate::ir::analysis::type_inference::SymbolType;

use super::super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check for self-connections in connect equations.
///
/// MLS §9.3: A connector cannot be connected to itself.
/// This detects cases like `connect(c, c)` which are invalid.
pub fn check_self_connections(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check all equations for self-connections
    for eq in &class.equations {
        check_self_connection_in_equation(eq, &mut result);
    }

    // Check initial equations as well
    for eq in &class.initial_equations {
        check_self_connection_in_equation(eq, &mut result);
    }

    result
}

/// Recursively check for self-connections in an equation.
fn check_self_connection_in_equation(eq: &Equation, result: &mut TypeCheckResult) {
    match eq {
        Equation::Connect { lhs, rhs } => {
            // Check if lhs and rhs are the same component reference
            if component_refs_equal(lhs, rhs)
                && let Some(loc) = lhs.get_location()
            {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    "Self-connection detected: a connector cannot be connected to itself"
                        .to_string(),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_self_connection_in_equation(inner_eq, result);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    check_self_connection_in_equation(inner_eq, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_self_connection_in_equation(inner_eq, result);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner_eq in &block.eqs {
                    check_self_connection_in_equation(inner_eq, result);
                }
            }
        }
        _ => {}
    }
}

/// Check if two component references are equal (same path).
fn component_refs_equal(a: &ComponentReference, b: &ComponentReference) -> bool {
    if a.parts.len() != b.parts.len() {
        return false;
    }

    for (part_a, part_b) in a.parts.iter().zip(b.parts.iter()) {
        // Compare identifiers
        if part_a.ident.text != part_b.ident.text {
            return false;
        }

        // Compare subscripts
        match (&part_a.subs, &part_b.subs) {
            (None, None) => {}
            (Some(subs_a), Some(subs_b)) => {
                if subs_a.len() != subs_b.len() {
                    return false;
                }
                for (sub_a, sub_b) in subs_a.iter().zip(subs_b.iter()) {
                    if !subscripts_equal(sub_a, sub_b) {
                        return false;
                    }
                }
            }
            _ => return false,
        }
    }

    true
}

/// Check if two subscripts are equal.
fn subscripts_equal(a: &Subscript, b: &Subscript) -> bool {
    match (a, b) {
        (Subscript::Empty, Subscript::Empty) => true,
        (Subscript::Range { .. }, Subscript::Range { .. }) => true,
        (Subscript::Expression(expr_a), Subscript::Expression(expr_b)) => {
            expressions_structurally_equal(expr_a, expr_b)
        }
        _ => false,
    }
}

/// Check if two expressions are structurally equal (for subscript comparison).
fn expressions_structurally_equal(a: &Expression, b: &Expression) -> bool {
    match (a, b) {
        (Expression::Empty, Expression::Empty) => true,
        (
            Expression::Terminal {
                terminal_type: type_a,
                token: tok_a,
            },
            Expression::Terminal {
                terminal_type: type_b,
                token: tok_b,
            },
        ) => type_a == type_b && tok_a.text == tok_b.text,
        (Expression::ComponentReference(ref_a), Expression::ComponentReference(ref_b)) => {
            component_refs_equal(ref_a, ref_b)
        }
        (
            Expression::Binary {
                lhs: lhs_a,
                op: op_a,
                rhs: rhs_a,
            },
            Expression::Binary {
                lhs: lhs_b,
                op: op_b,
                rhs: rhs_b,
            },
        ) => {
            std::mem::discriminant(op_a) == std::mem::discriminant(op_b)
                && expressions_structurally_equal(lhs_a, lhs_b)
                && expressions_structurally_equal(rhs_a, rhs_b)
        }
        _ => false,
    }
}

/// Check that connect arguments are connectors (not regular variables).
///
/// MLS §9.1: Both arguments to connect must be connectors.
/// This validates that connect(a, b) where both a and b have connector types.
pub fn check_connect_types(
    class: &ClassDefinition,
    connector_types: &HashSet<String>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of component names to their type names
    let comp_types: HashMap<String, String> = class
        .components
        .iter()
        .map(|(name, comp)| (name.clone(), comp.type_name.to_string()))
        .collect();

    // Check all equations for connect type errors
    for eq in &class.equations {
        check_connect_types_in_equation(eq, &comp_types, connector_types, &mut result);
    }

    // Check initial equations as well
    for eq in &class.initial_equations {
        check_connect_types_in_equation(eq, &comp_types, connector_types, &mut result);
    }

    result
}

/// Recursively check connect types in an equation.
fn check_connect_types_in_equation(
    eq: &Equation,
    comp_types: &HashMap<String, String>,
    connector_types: &HashSet<String>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::Connect { lhs, rhs } => {
            // Only check direct connections (single-part paths like `connect(c, x)`)
            // For paths like `connect(a.p, b.p)`, the intermediate components might be
            // models/blocks with connector components, which is valid.
            // We only reject when connecting directly to a built-in type variable.

            // Check lhs - only if it's a single-part path (direct variable)
            if lhs.parts.len() == 1 {
                let lhs_name = &lhs.parts[0].ident.text;
                if let Some(lhs_type) = comp_types.get(lhs_name) {
                    // Check if it's a built-in type (definitely not a connector)
                    if is_builtin_type(lhs_type) {
                        if let Some(loc) = lhs.get_location() {
                            result.add_error(TypeError::new(
                                loc.clone(),
                                SymbolType::Unknown,
                                SymbolType::Unknown,
                                format!(
                                    "Connect argument '{}' has type '{}' which is not a connector",
                                    lhs_name, lhs_type
                                ),
                                TypeErrorSeverity::Error,
                            ));
                        }
                    }
                    // For non-builtin types, only error if we have full connector info
                    // and this is definitely a model/block/record (not connector)
                    else if !connector_types.is_empty() && !connector_types.contains(lhs_type) {
                        // Check if this type is known to be a model/block/record
                        // by checking our class list (we can't do this here, so skip)
                        // This check would require more context
                    }
                }
            }

            // Check rhs - only if it's a single-part path (direct variable)
            if rhs.parts.len() == 1 {
                let rhs_name = &rhs.parts[0].ident.text;
                if let Some(rhs_type) = comp_types.get(rhs_name) {
                    // Check if it's a built-in type (definitely not a connector)
                    if is_builtin_type(rhs_type)
                        && let Some(loc) = rhs.get_location()
                    {
                        result.add_error(TypeError::new(
                            loc.clone(),
                            SymbolType::Unknown,
                            SymbolType::Unknown,
                            format!(
                                "Connect argument '{}' has type '{}' which is not a connector",
                                rhs_name, rhs_type
                            ),
                            TypeErrorSeverity::Error,
                        ));
                    }
                }
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_connect_types_in_equation(inner_eq, comp_types, connector_types, result);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    check_connect_types_in_equation(inner_eq, comp_types, connector_types, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_connect_types_in_equation(inner_eq, comp_types, connector_types, result);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner_eq in &block.eqs {
                    check_connect_types_in_equation(inner_eq, comp_types, connector_types, result);
                }
            }
        }
        _ => {}
    }
}

/// Check if a type name is a built-in type (not a connector).
fn is_builtin_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "Real" | "Integer" | "Boolean" | "String" | "StateSelect" | "AssertionLevel"
    )
}

use indexmap::IndexMap;

/// Check for flow/non-flow mixing in connect equations.
///
/// MLS §9.3: Flow and non-flow variables cannot be connected directly.
/// This validates that connect(a.v, b.i) where v and i have different flow status is rejected.
pub fn check_flow_compatibility(
    class: &ClassDefinition,
    connector_classes: &IndexMap<String, ClassDefinition>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Build a map of component names to their type names
    let comp_types: HashMap<String, String> = class
        .components
        .iter()
        .map(|(name, comp)| (name.clone(), comp.type_name.to_string()))
        .collect();

    // Check all equations for flow compatibility errors
    for eq in &class.equations {
        check_flow_in_equation(eq, &comp_types, connector_classes, &mut result);
    }

    // Check initial equations as well
    for eq in &class.initial_equations {
        check_flow_in_equation(eq, &comp_types, connector_classes, &mut result);
    }

    result
}

/// Recursively check flow compatibility in an equation.
fn check_flow_in_equation(
    eq: &Equation,
    comp_types: &HashMap<String, String>,
    connector_classes: &IndexMap<String, ClassDefinition>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::Connect { lhs, rhs } => {
            // Case 1: Connecting individual fields (e.g., connect(p1.v, p2.i))
            // We need to check if both fields have the same flow status
            if lhs.parts.len() >= 2 && rhs.parts.len() >= 2 {
                // Get the connector type for lhs
                let lhs_base = &lhs.parts[0].ident.text;
                let lhs_field = &lhs.parts[1].ident.text;

                // Get the connector type for rhs
                let rhs_base = &rhs.parts[0].ident.text;
                let rhs_field = &rhs.parts[1].ident.text;

                // Look up the connector types
                if let (Some(lhs_type), Some(rhs_type)) =
                    (comp_types.get(lhs_base), comp_types.get(rhs_base))
                {
                    // Look up the connector class definitions
                    if let (Some(lhs_class), Some(rhs_class)) = (
                        connector_classes.get(lhs_type),
                        connector_classes.get(rhs_type),
                    ) {
                        // Get the flow status of each field
                        let lhs_is_flow = get_field_flow_status(lhs_class, lhs_field);
                        let rhs_is_flow = get_field_flow_status(rhs_class, rhs_field);

                        // Check for mixing
                        if lhs_is_flow != rhs_is_flow
                            && let Some(loc) = lhs.get_location()
                        {
                            result.add_error(TypeError::new(
                                loc.clone(),
                                SymbolType::Unknown,
                                SymbolType::Unknown,
                                format!(
                                    "Cannot connect {} field '{}' to {} field '{}'",
                                    if lhs_is_flow { "flow" } else { "non-flow" },
                                    lhs_field,
                                    if rhs_is_flow { "flow" } else { "non-flow" },
                                    rhs_field
                                ),
                                TypeErrorSeverity::Error,
                            ));
                        }
                    }
                }
            }

            // Case 2: Connecting whole connectors (e.g., connect(a, b))
            // Check that corresponding fields have the same flow status
            if lhs.parts.len() == 1 && rhs.parts.len() == 1 {
                let lhs_name = &lhs.parts[0].ident.text;
                let rhs_name = &rhs.parts[0].ident.text;

                if let (Some(lhs_type), Some(rhs_type)) =
                    (comp_types.get(lhs_name), comp_types.get(rhs_name))
                {
                    // Different connector types - check field compatibility
                    if lhs_type != rhs_type
                        && let (Some(lhs_class), Some(rhs_class)) = (
                            connector_classes.get(lhs_type),
                            connector_classes.get(rhs_type),
                        )
                    {
                        // Check each field in lhs_class against corresponding field in rhs_class
                        for (field_name, lhs_comp) in &lhs_class.components {
                            if let Some(rhs_comp) = rhs_class.components.get(field_name) {
                                let lhs_is_flow =
                                    matches!(lhs_comp.connection, Connection::Flow(_));
                                let rhs_is_flow =
                                    matches!(rhs_comp.connection, Connection::Flow(_));

                                if lhs_is_flow != rhs_is_flow {
                                    if let Some(loc) = lhs.get_location() {
                                        result.add_error(TypeError::new(
                                                loc.clone(),
                                                SymbolType::Unknown,
                                                SymbolType::Unknown,
                                                format!(
                                                    "Incompatible connectors: field '{}' is {} in '{}' but {} in '{}'",
                                                    field_name,
                                                    if lhs_is_flow { "flow" } else { "non-flow" },
                                                    lhs_type,
                                                    if rhs_is_flow { "flow" } else { "non-flow" },
                                                    rhs_type
                                                ),
                                                TypeErrorSeverity::Error,
                                            ));
                                    }
                                    break; // Report only first mismatch
                                }
                            }
                        }
                    }
                }
            }
        }
        Equation::For { equations, .. } => {
            for inner_eq in equations {
                check_flow_in_equation(inner_eq, comp_types, connector_classes, result);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner_eq in &block.eqs {
                    check_flow_in_equation(inner_eq, comp_types, connector_classes, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for inner_eq in else_eqs {
                    check_flow_in_equation(inner_eq, comp_types, connector_classes, result);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                for inner_eq in &block.eqs {
                    check_flow_in_equation(inner_eq, comp_types, connector_classes, result);
                }
            }
        }
        _ => {}
    }
}

/// Get the flow status of a field in a connector class.
fn get_field_flow_status(class: &ClassDefinition, field_name: &str) -> bool {
    if let Some(comp) = class.components.get(field_name) {
        matches!(comp.connection, Connection::Flow(_))
    } else {
        false
    }
}

/// Check that stream variables are only declared in connectors (MLS §15.1).
///
/// Stream variables can only be declared inside connector classes.
pub fn check_stream_only_in_connector(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check non-connector classes
    if class.class_type == ClassType::Connector {
        return result;
    }

    // Check all components for stream prefix
    for (name, comp) in &class.components {
        if matches!(comp.connection, Connection::Stream(_)) {
            result.add_error(TypeError::new(
                comp.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Stream variable '{}' can only be declared inside a connector, not in {}",
                    name,
                    match class.class_type {
                        ClassType::Model => "a model",
                        ClassType::Class => "a class",
                        ClassType::Block => "a block",
                        ClassType::Record => "a record",
                        ClassType::Type => "a type",
                        ClassType::Package => "a package",
                        ClassType::Function => "a function",
                        ClassType::Operator => "an operator",
                        ClassType::Connector => "a connector", // unreachable due to early return
                    }
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}

/// Check that stream variables have an associated flow variable (MLS §15.1).
///
/// A connector with stream variables must also have at least one flow variable.
pub fn check_stream_requires_flow(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check connectors
    if class.class_type != ClassType::Connector {
        return result;
    }

    // Check if there are any stream variables
    let stream_vars: Vec<(&String, &Component)> = class
        .components
        .iter()
        .filter(|(_, comp)| matches!(comp.connection, Connection::Stream(_)))
        .collect();

    if stream_vars.is_empty() {
        return result;
    }

    // Check if there is at least one flow variable
    let has_flow = class
        .components
        .iter()
        .any(|(_, comp)| matches!(comp.connection, Connection::Flow(_)));

    if !has_flow {
        // Report error on each stream variable
        for (name, comp) in stream_vars {
            result.add_error(TypeError::new(
                comp.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Stream variable '{}' requires an associated flow variable in the connector",
                    name
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}

/// Check that connectors do not contain parameter or constant components (MLS §9.1).
///
/// According to MLS §9.1, connector components must be one of:
/// - potential variables (no prefix)
/// - flow variables
/// - stream variables
/// - parameter/constant prefixes are NOT allowed
pub fn check_connector_no_parameter_constant(class: &ClassDefinition) -> TypeCheckResult {
    use crate::ir::ast::Variability;
    let mut result = TypeCheckResult::new();

    // Only check connectors
    if class.class_type != ClassType::Connector {
        return result;
    }

    // Check all components for parameter/constant variability
    for (name, comp) in &class.components {
        match &comp.variability {
            Variability::Parameter(_) => {
                result.add_error(TypeError::new(
                    comp.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Parameter '{}' is not allowed in connector '{}'. Connectors can only contain potential, flow, or stream variables.",
                        name, class.name.text
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
            Variability::Constant(_) => {
                result.add_error(TypeError::new(
                    comp.location.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Constant '{}' is not allowed in connector '{}'. Connectors can only contain potential, flow, or stream variables.",
                        name, class.name.text
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
            _ => {}
        }
    }

    result
}

/// Check that expandable connectors do not contain flow variables (MLS §9.1.3).
///
/// Flow variables are not allowed in expandable connectors because the connection
/// set membership is not known at compile time.
pub fn check_expandable_no_flow(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Only check expandable connectors
    if class.class_type != ClassType::Connector || !class.expandable {
        return result;
    }

    // Check all components for flow prefix
    for (name, comp) in &class.components {
        if matches!(comp.connection, Connection::Flow(_)) {
            result.add_error(TypeError::new(
                comp.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Flow variable '{}' is not allowed in expandable connector '{}'",
                    name, class.name.text
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}
