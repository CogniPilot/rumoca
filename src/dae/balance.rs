//! DAE balance checking
//!
//! Simple balance check: count equations vs unknowns from the DAE structure.
//!
//! Note: This assumes equations have been expanded to scalar form by the
//! equation_expander pass before DAE creation.

use super::ast::Dae;
use crate::ir::ast::{Component, Connection, Statement};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Balance status categories
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BalanceStatus {
    /// Fully determined: equations = unknowns
    Balanced,
    /// Under-determined by design: has external connectors that need connection equations
    Partial,
    /// Invalid: over-determined (too many equations) or under-determined without external connectors
    Unbalanced,
    /// Compilation failed - could not analyze balance
    CompileError(String),
}

/// Result of checking DAE balance
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BalanceResult {
    /// Number of equations
    pub num_equations: usize,
    /// Number of unknowns (states + algebraic + discrete)
    pub num_unknowns: usize,
    /// Number of state variables
    pub num_states: usize,
    /// Number of algebraic variables
    pub num_algebraic: usize,
    /// Number of parameters
    pub num_parameters: usize,
    /// Number of inputs
    pub num_inputs: usize,
    /// Number of external connector variables (flow variables that need connection equations)
    pub num_external_connectors: usize,
    /// Balance status category
    pub status: BalanceStatus,
}

impl BalanceResult {
    /// Create a BalanceResult representing a compilation error
    pub fn compile_error(message: String) -> Self {
        Self {
            num_equations: 0,
            num_unknowns: 0,
            num_states: 0,
            num_algebraic: 0,
            num_parameters: 0,
            num_inputs: 0,
            num_external_connectors: 0,
            status: BalanceStatus::CompileError(message),
        }
    }

    /// Check if the model is balanced (equations == unknowns)
    pub fn is_balanced(&self) -> bool {
        matches!(self.status, BalanceStatus::Balanced)
    }

    /// Get the difference between equations and unknowns
    pub fn difference(&self) -> i64 {
        self.num_equations as i64 - self.num_unknowns as i64
    }

    /// Get a human-readable status message
    pub fn status_message(&self) -> String {
        match &self.status {
            BalanceStatus::Balanced => "balanced".to_string(),
            BalanceStatus::Partial => {
                let diff = -self.difference();
                format!(
                    "partial (under by {}, {} external connectors)",
                    diff, self.num_external_connectors
                )
            }
            BalanceStatus::Unbalanced => {
                let diff = self.difference();
                if diff > 0 {
                    format!("unbalanced: over-determined by {}", diff)
                } else {
                    format!("unbalanced: under-determined by {}", -diff)
                }
            }
            BalanceStatus::CompileError(msg) => format!("compile error: {}", msg),
        }
    }
}

impl Dae {
    /// Check the balance of the DAE system
    ///
    /// Counts equations and unknowns directly from the DAE structure.
    /// Categorizes the result as:
    /// - Balanced: equations = unknowns
    /// - Partial: under-determined but has external connectors (by design)
    /// - Unbalanced: over-determined OR under-determined without external connectors
    ///
    /// Note: This assumes equations have been expanded to scalar form by the
    /// equation_expander pass. Each equation in fx/fz represents one scalar equation.
    /// Event equations in fr (from when blocks) are also counted by unique variable.
    pub fn check_balance(&self) -> BalanceResult {
        // Count scalar elements in each variable category
        let num_states = count_scalars(&self.x);
        let num_algebraic =
            count_scalars(&self.y) + count_scalars(&self.z) + count_scalars(&self.m);
        let num_unknowns = num_states + num_algebraic;

        // Count equations - after expansion, each equation in the Vec is one scalar equation
        // For event equations (fr), count unique variables assigned, not total assignments.
        // A when/elsewhen chain assigns to the same variable multiple times but is 1 equation.
        // Exclude state variables - reinit(state, expr) is an event action, not an equation.
        let num_event_equations = count_unique_event_variables(&self.fr, &self.x);
        let num_equations = self.fx.len() + self.fz.len() + num_event_equations;

        // Count parameters and inputs for reporting
        let num_parameters = count_scalars(&self.p) + count_scalars(&self.cp);
        let num_inputs = count_scalars(&self.u);

        // Count external connector variables (flow variables that need connection equations)
        // These are variables with Connection::Flow - they get their equations from connections
        let num_external_connectors = count_external_connectors(&self.y)
            + count_external_connectors(&self.z)
            + count_external_connectors(&self.m);

        // Determine balance status
        let diff = num_equations as i64 - num_unknowns as i64;

        let status = if diff == 0 {
            BalanceStatus::Balanced
        } else if diff > 0 {
            // Over-determined is always unbalanced (a bug)
            BalanceStatus::Unbalanced
        } else {
            // Under-determined: check if it's partial by design
            // A model is "partial" if it has external connectors that explain the missing equations
            if num_external_connectors > 0 {
                BalanceStatus::Partial
            } else {
                BalanceStatus::Unbalanced
            }
        };

        BalanceResult {
            num_equations,
            num_unknowns,
            num_states,
            num_algebraic,
            num_parameters,
            num_inputs,
            num_external_connectors,
            status,
        }
    }
}

/// Count scalar elements in a component map (accounting for array dimensions)
fn count_scalars(components: &IndexMap<String, Component>) -> usize {
    components
        .values()
        .map(|comp| {
            if comp.shape.is_empty() {
                1
            } else {
                comp.shape.iter().product()
            }
        })
        .sum()
}

/// Count external connector variables (flow variables that need connection equations)
///
/// Flow variables in connectors get their equations from connect() statements.
/// When a model is checked in isolation, these variables are "missing" equations
/// because they would be provided by the connections in a larger system.
fn count_external_connectors(components: &IndexMap<String, Component>) -> usize {
    components
        .values()
        .filter(|comp| matches!(comp.connection, Connection::Flow(_)))
        .map(|comp| {
            if comp.shape.is_empty() {
                1
            } else {
                comp.shape.iter().product()
            }
        })
        .sum()
}

/// Count unique variables assigned in event equations (fr).
///
/// In Modelica, a when/elsewhen chain assigning to the same variable counts as
/// one equation, not multiple. This function counts unique variable names.
///
/// Excludes state variables because `reinit(state, expr)` is an event action
/// that modifies a state at an event instant, not a defining equation.
/// The state already has its equation from `der(state) = ...`.
fn count_unique_event_variables(
    fr: &IndexMap<String, Statement>,
    states: &IndexMap<String, Component>,
) -> usize {
    let mut unique_vars: HashSet<String> = HashSet::new();
    for stmt in fr.values() {
        if let Statement::Assignment { comp, .. } = stmt {
            let var_name = comp.to_string();
            // Exclude state variables (reinit targets states, not discrete vars)
            if !states.contains_key(&var_name) {
                unique_vars.insert(var_name);
            }
        }
    }
    unique_vars.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_balance_result_messages() {
        // Balanced model
        let balanced = BalanceResult {
            num_equations: 3,
            num_unknowns: 3,
            num_states: 2,
            num_algebraic: 1,
            num_parameters: 0,
            num_inputs: 0,
            num_external_connectors: 0,
            status: BalanceStatus::Balanced,
        };
        assert!(balanced.status_message().contains("balanced"));
        assert_eq!(balanced.difference(), 0);
        assert_eq!(balanced.status, BalanceStatus::Balanced);
        assert!(balanced.is_balanced());

        // Over-determined (unbalanced - always a bug)
        let over = BalanceResult {
            num_equations: 5,
            num_unknowns: 3,
            num_states: 2,
            num_algebraic: 1,
            num_parameters: 0,
            num_inputs: 0,
            num_external_connectors: 0,
            status: BalanceStatus::Unbalanced,
        };
        assert!(over.status_message().contains("over-determined"));
        assert_eq!(over.difference(), 2);
        assert_eq!(over.status, BalanceStatus::Unbalanced);
        assert!(!over.is_balanced());

        // Under-determined without external connectors (unbalanced - a bug)
        let under_bug = BalanceResult {
            num_equations: 3,
            num_unknowns: 5,
            num_states: 3,
            num_algebraic: 2,
            num_parameters: 0,
            num_inputs: 0,
            num_external_connectors: 0,
            status: BalanceStatus::Unbalanced,
        };
        assert!(under_bug.status_message().contains("under-determined"));
        assert_eq!(under_bug.difference(), -2);
        assert_eq!(under_bug.status, BalanceStatus::Unbalanced);
        assert!(!under_bug.is_balanced());

        // Under-determined with external connectors (partial - by design)
        let partial = BalanceResult {
            num_equations: 3,
            num_unknowns: 5,
            num_states: 2,
            num_algebraic: 3,
            num_parameters: 0,
            num_inputs: 0,
            num_external_connectors: 2,
            status: BalanceStatus::Partial,
        };
        assert!(partial.status_message().contains("partial"));
        assert_eq!(partial.difference(), -2);
        assert_eq!(partial.status, BalanceStatus::Partial);
        assert!(!partial.is_balanced());
    }
}
