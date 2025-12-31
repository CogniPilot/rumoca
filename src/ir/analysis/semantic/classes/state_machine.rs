//! State machine restrictions.
//!
//! MLS §17: State machine semantics

use std::collections::HashMap;

use crate::ir::ast::{ClassDefinition, ClassType, Equation, Expression, OpBinary, TerminalType};

use crate::ir::analysis::type_inference::SymbolType;

use super::super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check that initialState is called at most once per state machine (MLS §17.1).
///
/// MLS §17.1: "initialState must be called exactly once in a state machine."
/// This validates that there is at most one initialState() call in the equations.
pub fn check_single_initial_state(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    let mut initial_state_calls: Vec<&Equation> = Vec::new();

    // Find all initialState() calls in equations
    for eq in &class.equations {
        if let Equation::FunctionCall { comp, .. } = eq {
            let func_name = comp.to_string();
            if func_name == "initialState" {
                initial_state_calls.push(eq);
            }
        }
    }

    // If there's more than one initialState call, report an error
    if initial_state_calls.len() > 1
        && let Some(Equation::FunctionCall { comp, .. }) = initial_state_calls.get(1)
        && let Some(loc) = comp.get_location()
    {
        result.add_error(TypeError::new(
            loc.clone(),
            SymbolType::Unknown,
            SymbolType::Unknown,
            format!(
                "Multiple initialState() calls in '{}' - only one initial state is allowed per state machine",
                class.name.text
            ),
            TypeErrorSeverity::Error,
        ));
    }

    result
}

/// Check that states passed to initialState/transition are blocks or models (MLS §17.1).
///
/// MLS §17.1: "A state must be a block or model."
/// This validates that arguments to state machine operators reference valid state types.
pub fn check_state_is_block_or_model(
    class: &ClassDefinition,
    all_classes: &indexmap::IndexMap<String, ClassDefinition>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Check equations for state machine operator calls
    for eq in &class.equations {
        if let Equation::FunctionCall { comp, args } = eq {
            let func_name = comp.to_string();

            // Check initialState(state) and transition(from, to, ...) calls
            if func_name == "initialState" || func_name == "transition" {
                // Get the state argument(s) to validate
                let state_args: Vec<&Expression> = if func_name == "initialState" {
                    args.iter().take(1).collect()
                } else {
                    // transition(from, to, condition, ...)
                    args.iter().take(2).collect()
                };

                for state_arg in state_args {
                    // The state argument should be a component reference
                    if let Expression::ComponentReference(state_ref) = state_arg {
                        let state_name = state_ref.to_string();

                        // Look up the component in the class
                        if let Some(component) = class.components.get(&state_name) {
                            // Get the type name and look up the class definition
                            let type_name = component.type_name.to_string();

                            // First check nested classes in the current class
                            let class_def = class
                                .classes
                                .get(&type_name)
                                .or_else(|| all_classes.get(&type_name));

                            if let Some(state_class) = class_def
                                && !matches!(
                                    state_class.class_type,
                                    ClassType::Block | ClassType::Model
                                )
                                && let Some(loc) = state_arg.get_location()
                            {
                                result.add_error(TypeError::new(
                                    loc.clone(),
                                    SymbolType::Unknown,
                                    SymbolType::Unknown,
                                    format!(
                                        "State '{}' in {}() must be a block or model, not a {}",
                                        state_name,
                                        func_name,
                                        format!("{:?}", state_class.class_type).to_lowercase()
                                    ),
                                    TypeErrorSeverity::Error,
                                ));
                            }
                        }
                    }
                }
            }
        }
    }

    result
}

/// Check that transitions from the same state have unique priorities (MLS §17.2).
///
/// MLS §17.2: "Transitions from a state must have unique priorities."
/// This validates that no two transitions from the same source state have the same priority.
pub fn check_transition_priority_unique(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    // Collect transitions: (source_state, priority, location)
    let mut transitions: Vec<(String, i64, &Equation)> = Vec::new();

    for eq in &class.equations {
        if let Equation::FunctionCall { comp, args } = eq {
            let func_name = comp.to_string();

            if func_name == "transition" && !args.is_empty() {
                // Get the source state (first argument)
                let source_state =
                    if let Some(Expression::ComponentReference(state_ref)) = args.first() {
                        state_ref.to_string()
                    } else {
                        continue;
                    };

                // Get priority from named argument (default is 1)
                let mut priority: i64 = 1;

                // Look for priority = N in the arguments (after the first 3 positional args)
                for arg in args.iter().skip(3) {
                    if let Expression::Binary { op, lhs, rhs } = arg
                        && matches!(op, OpBinary::Assign(_))
                        && let Expression::ComponentReference(name_ref) = lhs.as_ref()
                        && name_ref.to_string() == "priority"
                        && let Expression::Terminal {
                            terminal_type: TerminalType::UnsignedInteger,
                            token,
                        } = rhs.as_ref()
                        && let Ok(n) = token.text.parse::<i64>()
                    {
                        priority = n;
                    }
                }

                transitions.push((source_state, priority, eq));
            }
        }
    }

    // Check for duplicate priorities from the same source state
    let mut seen: HashMap<(String, i64), &Equation> = HashMap::new();

    for (source, priority, eq) in &transitions {
        let key = (source.clone(), *priority);
        if let Some(prev_eq) = seen.get(&key) {
            // Found duplicate priority - report if we can get the location
            if let Equation::FunctionCall { comp, .. } = eq
                && let Some(loc) = comp.get_location()
            {
                // Get the location of the previous transition for context
                let prev_loc = if let Equation::FunctionCall {
                    comp: prev_comp, ..
                } = prev_eq
                {
                    prev_comp
                        .get_location()
                        .map(|l| format!(" (previous at line {})", l.start_line))
                        .unwrap_or_default()
                } else {
                    String::new()
                };

                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Unknown,
                    SymbolType::Unknown,
                    format!(
                        "Duplicate transition priority {} from state '{}'{}",
                        priority, source, prev_loc
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        } else {
            seen.insert(key, eq);
        }
    }

    result
}
