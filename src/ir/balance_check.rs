//! Balance checking for Modelica models
//!
//! A balanced model has the same number of equations as unknowns.
//! This module provides utilities to count equations and unknowns
//! and check model balance.

use crate::dae::ast::Dae;
use crate::ir::ast::{ClassDefinition, Equation, Expression, Variability};
use crate::ir::visitor::{Visitable, Visitor};

/// Result of a balance check
#[derive(Debug, Clone, PartialEq)]
pub struct BalanceCheckResult {
    /// Number of equations in the model
    pub num_equations: usize,
    /// Number of unknown variables (states + algebraic + discrete)
    pub num_unknowns: usize,
    /// Number of state variables (variables that appear in der())
    pub num_states: usize,
    /// Number of algebraic variables
    pub num_algebraic: usize,
    /// Number of parameters
    pub num_parameters: usize,
    /// Number of inputs
    pub num_inputs: usize,
    /// Whether the model is balanced
    pub is_balanced: bool,
}

impl BalanceCheckResult {
    /// Get the difference between equations and unknowns
    pub fn difference(&self) -> i64 {
        self.num_equations as i64 - self.num_unknowns as i64
    }

    /// Get a human-readable description of the balance status
    pub fn status_message(&self) -> String {
        if self.is_balanced {
            format!(
                "Model is balanced: {} equations, {} unknowns ({} states, {} algebraic)",
                self.num_equations, self.num_unknowns, self.num_states, self.num_algebraic
            )
        } else {
            let diff = self.difference();
            if diff > 0 {
                format!(
                    "Model is over-determined: {} equations, {} unknowns ({} extra equations)",
                    self.num_equations,
                    self.num_unknowns,
                    diff.abs()
                )
            } else {
                format!(
                    "Model is under-determined: {} equations, {} unknowns ({} missing equations)",
                    self.num_equations,
                    self.num_unknowns,
                    diff.abs()
                )
            }
        }
    }
}

/// Check if a type name represents a primitive/built-in type
fn is_primitive_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "Real" | "Integer" | "Boolean" | "String" | "StateSelect" | "ExternalObject"
    )
}

/// Check balance of a flattened class definition
pub fn check_class_balance(class: &ClassDefinition) -> BalanceCheckResult {
    // Count equations
    let num_equations = count_equations(&class.equations);

    // Count variables by category
    let mut num_algebraic: usize = 0;
    let mut num_parameters: usize = 0;
    let mut num_inputs: usize = 0;

    for (_name, comp) in &class.components {
        let type_name = comp.type_name.to_string();

        // Skip class instances - they have their own balance
        // Only count primitive types (Real, Integer, Boolean, String)
        if !is_primitive_type(&type_name) {
            continue;
        }

        // Skip constants and parameters - they are known
        if matches!(
            comp.variability,
            Variability::Constant(_) | Variability::Parameter(_)
        ) {
            num_parameters += 1;
            continue;
        }

        // Skip inputs - they are known from outside
        if matches!(comp.causality, crate::ir::ast::Causality::Input(_)) {
            num_inputs += 1;
            continue;
        }

        // All other Real variables are unknowns
        // (states are a subset - identified by appearing in der())
        num_algebraic += 1;
    }

    // Find states by looking for der(x) calls in equations
    let states = find_state_variables(&class.equations);
    let num_states = states.len();

    // Algebraic = total unknowns - states
    // But we counted all non-parameter/input as algebraic above
    // So adjust: algebraic = (algebraic we counted) - states found
    // Actually, states are ALSO in the algebraic count, so:
    // total unknowns = algebraic (which already excludes parameters/inputs)
    // states is a subset of those unknowns
    let num_unknowns = num_algebraic;

    // For reporting, separate states from algebraic
    num_algebraic = num_algebraic.saturating_sub(num_states);

    BalanceCheckResult {
        num_equations,
        num_unknowns,
        num_states,
        num_algebraic,
        num_parameters,
        num_inputs,
        is_balanced: num_equations == num_unknowns,
    }
}

/// Check balance using the DAE structure (after full compilation)
pub fn check_dae_balance(dae: &Dae) -> BalanceCheckResult {
    // Unknowns from DAE: x (states) + y (algebraic) + z (discrete real) + m (discrete other)
    let num_states = dae.x.len();
    let num_algebraic = dae.y.len();
    let num_discrete = dae.z.len() + dae.m.len();
    let num_unknowns = num_states + num_algebraic + num_discrete;

    // Equations: fx (continuous) + fz (discrete update) + fm (discrete value)
    let num_equations = dae.fx.len();

    // Parameters and inputs (for reporting)
    let num_parameters = dae.p.len() + dae.cp.len();
    let num_inputs = dae.u.len();

    BalanceCheckResult {
        num_equations,
        num_unknowns,
        num_states,
        num_algebraic: num_algebraic + num_discrete,
        num_parameters,
        num_inputs,
        is_balanced: num_equations == num_unknowns,
    }
}

/// Count the number of equations, expanding if/for equations recursively
fn count_equations(equations: &[Equation]) -> usize {
    let mut count = 0;
    for eq in equations {
        count += count_single_equation(eq);
    }
    count
}

/// Count a single equation (may be composite like if/for)
fn count_single_equation(eq: &Equation) -> usize {
    match eq {
        Equation::Simple { .. } => 1,
        Equation::Connect { .. } => {
            // Connect equations expand to multiple equations
            // but they should already be expanded at this point
            1
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            // For balance checking, we assume all branches have the same count
            // and use the else branch count (which must exist for valid if-equations)
            // In Modelica, each branch must have the same number of equations
            if let Some(else_eqs) = else_block {
                count_equations(else_eqs)
            } else if let Some(first_block) = cond_blocks.first() {
                count_equations(&first_block.eqs)
            } else {
                0
            }
        }
        Equation::For { equations, .. } => {
            // For loops expand based on the range
            // For now, count the body equations (this is conservative)
            // TODO: Actually expand based on range size
            count_equations(equations)
        }
        Equation::When(branches) => {
            // When equations generate discrete equations
            // For continuous balance, these don't count
            // But they do generate fz/fm equations
            let _ = branches; // silence unused warning
            0
        }
        Equation::FunctionCall { .. } => {
            // Function call equations (like assert()) typically don't add to equation count
            0
        }
        Equation::Empty => 0,
    }
}

/// Visitor that finds variables that appear in der() calls
struct DerFinder {
    /// Found state variables
    states: Vec<String>,
}

impl DerFinder {
    fn new() -> Self {
        Self { states: Vec::new() }
    }
}

impl Visitor for DerFinder {
    fn enter_expression(&mut self, node: &Expression) {
        if let Expression::FunctionCall { comp, args } = node {
            if comp.to_string() == "der" && !args.is_empty() {
                if let Expression::ComponentReference(cref) = &args[0] {
                    let var_name = cref.to_string();
                    if !self.states.contains(&var_name) {
                        self.states.push(var_name);
                    }
                }
            }
        }
    }
}

/// Find variables that appear in der() calls
fn find_state_variables(equations: &[Equation]) -> Vec<String> {
    let mut finder = DerFinder::new();
    for eq in equations {
        eq.accept(&mut finder);
    }
    finder.states
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_balance_check_result_messages() {
        let balanced = BalanceCheckResult {
            num_equations: 3,
            num_unknowns: 3,
            num_states: 1,
            num_algebraic: 2,
            num_parameters: 1,
            num_inputs: 0,
            is_balanced: true,
        };
        assert!(balanced.status_message().contains("balanced"));

        let over = BalanceCheckResult {
            num_equations: 5,
            num_unknowns: 3,
            num_states: 1,
            num_algebraic: 2,
            num_parameters: 1,
            num_inputs: 0,
            is_balanced: false,
        };
        assert!(over.status_message().contains("over-determined"));
        assert_eq!(over.difference(), 2);

        let under = BalanceCheckResult {
            num_equations: 2,
            num_unknowns: 4,
            num_states: 2,
            num_algebraic: 2,
            num_parameters: 0,
            num_inputs: 0,
            is_balanced: false,
        };
        assert!(under.status_message().contains("under-determined"));
        assert_eq!(under.difference(), -2);
    }
}
