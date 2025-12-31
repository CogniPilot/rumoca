//! DAE balance checking
//!
//! Simple balance check: count equations vs unknowns from the DAE structure.
//!
//! Note: For loops are counted by evaluating their range when possible,
//! or by counting inner equations if the range cannot be determined.

use super::ast::Dae;
use crate::ir::ast::{Component, Connection, Equation, Expression, Statement, TerminalType};
use crate::ir::transform::eval::{eval_boolean, eval_integer};
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
#[derive(Debug, Clone, PartialEq, Deserialize)]
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
    /// Compilation time in milliseconds (for performance display)
    #[serde(default)]
    pub compile_time_ms: u64,
}

// Custom Serialize to include computed `is_balanced` field for WASM/JSON consumers
impl Serialize for BalanceResult {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("BalanceResult", 10)?;
        state.serialize_field("num_equations", &self.num_equations)?;
        state.serialize_field("num_unknowns", &self.num_unknowns)?;
        state.serialize_field("num_states", &self.num_states)?;
        state.serialize_field("num_algebraic", &self.num_algebraic)?;
        state.serialize_field("num_parameters", &self.num_parameters)?;
        state.serialize_field("num_inputs", &self.num_inputs)?;
        state.serialize_field("num_external_connectors", &self.num_external_connectors)?;
        state.serialize_field("status", &self.status)?;
        state.serialize_field("is_balanced", &self.is_balanced())?;
        state.serialize_field("compile_time_ms", &self.compile_time_ms)?;
        state.end()
    }
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
            compile_time_ms: 0,
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
    /// Note: For loops that couldn't be expanded are counted by evaluating their range
    /// from parameters when possible. Event equations from when blocks are counted
    /// by unique variable assignment.
    pub fn check_balance(&self) -> BalanceResult {
        // Count scalar elements in each variable category
        let num_states = count_scalars(&self.x);
        let num_algebraic =
            count_scalars(&self.y) + count_scalars(&self.z) + count_scalars(&self.m);
        let num_unknowns = num_states + num_algebraic;

        // Merge all parameters and condition variables for equation counting
        // Include: p (parameters), cp (computed parameters), c (condition variables)
        // The condition variables (c) contain evaluated boolean values like c0=false
        // that control which if-equation branch is active.
        let all_params: IndexMap<String, Component> = self
            .p
            .iter()
            .chain(self.cp.iter())
            .chain(self.c.iter())
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        // Merge all components for tuple LHS shape lookup
        // Tuple equations like (a, b) = f(x) need to know the shapes of a and b
        let all_components: IndexMap<String, Component> = self
            .p
            .iter()
            .chain(self.cp.iter())
            .chain(self.c.iter())
            .chain(self.x.iter())
            .chain(self.y.iter())
            .chain(self.z.iter())
            .chain(self.m.iter())
            .chain(self.u.iter())
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        // Count equations recursively, handling For loops
        let fx_count = count_equations(&self.fx, &all_params, &all_components);
        let fz_count = count_equations(&self.fz, &all_params, &all_components);

        // For event equations (fr), count unique variables assigned, not total assignments.
        // A when/elsewhen chain assigns to the same variable multiple times but is 1 equation.
        // Exclude state variables - reinit(state, expr) is an event action, not an equation.
        let num_event_equations = count_unique_event_variables(&self.fr, &self.x);
        let num_equations = fx_count + fz_count + num_event_equations;

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
            compile_time_ms: 0, // Set by caller if timing is tracked
        }
    }
}

/// Count equations recursively, handling For loops and If equations
fn count_equations(
    equations: &[Equation],
    params: &IndexMap<String, Component>,
    all_components: &IndexMap<String, Component>,
) -> usize {
    equations
        .iter()
        .map(|eq| count_single_equation(eq, params, all_components))
        .sum()
}

/// Count a single equation, recursing into For/If blocks
fn count_single_equation(
    eq: &Equation,
    params: &IndexMap<String, Component>,
    all_components: &IndexMap<String, Component>,
) -> usize {
    match eq {
        Equation::Simple { lhs, .. } => {
            // Only count scalar elements for tuple LHS - these are multi-output function calls
            // that aren't expanded during equation expansion. Other equations (including
            // expanded array equations) count as 1.
            if matches!(lhs, Expression::Tuple { .. }) {
                count_lhs_scalars(lhs, all_components)
            } else {
                1
            }
        }
        Equation::Connect { .. } | Equation::FunctionCall { .. } => 1,
        Equation::Empty => 0,

        Equation::For { indices, equations } => {
            // Try to evaluate the For loop range from parameters
            if let Some(range_size) = evaluate_for_range(indices, params) {
                // Inner equations * range size
                let inner_count = count_equations(equations, params, all_components);
                inner_count * range_size
            } else {
                // Can't evaluate range - just count inner equations
                // This is a conservative estimate
                count_equations(equations, params, all_components)
            }
        }

        Equation::If {
            cond_blocks,
            else_block,
        } => {
            // Try to evaluate conditions at compile time using known parameter values.
            // If a condition evaluates to true, count only that branch.
            // If all conditions evaluate to false, count only the else branch.
            // If conditions can't be evaluated, fall back to MAX rule.

            // First, try to find an active branch
            for block in cond_blocks {
                match eval_boolean(&block.cond, params) {
                    Some(true) => {
                        // This branch is active - count only its equations
                        return count_equations(&block.eqs, params, all_components);
                    }
                    Some(false) => {
                        // This condition is false, continue to next branch
                        continue;
                    }
                    None => {
                        // Condition can't be evaluated - fall back to MAX rule
                        break;
                    }
                }
            }

            // Check if all conditions evaluated to false (use else branch)
            let all_false = cond_blocks
                .iter()
                .all(|block| matches!(eval_boolean(&block.cond, params), Some(false)));

            if all_false {
                // All conditions are false, use else branch
                return else_block
                    .as_ref()
                    .map(|eqs| count_equations(eqs, params, all_components))
                    .unwrap_or(0);
            }

            // Conditions can't be fully evaluated.
            // Per MLS §8.3.3, all branches of an if-equation must have the same
            // number of equations (when conditions are not parameter-controlled).
            // Collect counts from all branches: if, all elseif, and else.
            let mut all_branch_counts: Vec<usize> = cond_blocks
                .iter()
                .map(|block| count_equations(&block.eqs, params, all_components))
                .collect();

            let else_count = else_block
                .as_ref()
                .map(|eqs| count_equations(eqs, params, all_components))
                .unwrap_or(0);
            all_branch_counts.push(else_count);

            // Check if all branches (if, elseif, else) have equal counts
            let first_count = all_branch_counts[0];
            let all_equal = all_branch_counts.iter().all(|&c| c == first_count);

            if all_equal {
                // All branches have equal counts - use that count
                first_count
            } else {
                // Branches have different counts - this is a parameter-controlled if-equation
                // Use MAX as conservative estimate
                all_branch_counts.into_iter().max().unwrap_or(0)
            }
        }

        Equation::When(blocks) => {
            // When equations - count equations in each block (they add up)
            blocks
                .iter()
                .map(|block| count_equations(&block.eqs, params, all_components))
                .sum()
        }
    }
}

/// Count scalar elements in an equation's LHS expression.
///
/// For tuple equations like `(a, b) = f(x)`, this counts the total scalar
/// elements in all tuple members. For scalar or array LHS, counts appropriately.
fn count_lhs_scalars(lhs: &Expression, components: &IndexMap<String, Component>) -> usize {
    match lhs {
        Expression::Tuple { elements } => {
            // Sum the scalar count of each tuple element
            elements
                .iter()
                .map(|elem| count_lhs_scalars(elem, components))
                .sum()
        }
        Expression::ComponentReference(comp_ref) => {
            // Look up the component to get its shape
            if let Some(first_part) = comp_ref.parts.first() {
                let name = &first_part.ident.text;

                // If component has subscripts, it's already a scalar access
                if first_part.subs.as_ref().is_some_and(|s| !s.is_empty()) {
                    return 1;
                }

                // First try exact match
                if let Some(comp) = components.get(name) {
                    if comp.shape.is_empty() {
                        return 1;
                    } else {
                        return comp.shape.iter().product();
                    }
                }

                // If exact match not found, look for scalar-expanded components
                // (e.g., seedState -> seedState[1], seedState[2], seedState[3])
                let prefix = format!("{}[", name);
                let scalar_count = components.keys().filter(|k| k.starts_with(&prefix)).count();
                if scalar_count > 0 {
                    return scalar_count;
                }

                // Unknown component - assume scalar
                1
            } else {
                1
            }
        }
        Expression::FunctionCall { comp, args } => {
            // Handle der(x) - count based on argument
            if let Some(first_part) = comp.parts.first()
                && first_part.ident.text == "der"
                && let Some(arg) = args.first()
            {
                return count_lhs_scalars(arg, components);
            }
            // Other function calls on LHS - assume scalar
            1
        }
        Expression::Array { elements, .. } => {
            // Array literal on LHS - count elements
            elements
                .iter()
                .map(|elem| count_lhs_scalars(elem, components))
                .sum()
        }
        _ => 1,
    }
}

/// Evaluate the size of a For loop range from its indices
fn evaluate_for_range(
    indices: &[crate::ir::ast::ForIndex],
    params: &IndexMap<String, Component>,
) -> Option<usize> {
    if indices.is_empty() {
        return Some(1);
    }

    // Evaluate the first index range
    let first = &indices[0];
    let range_size = evaluate_range_size(&first.range, params)?;

    if indices.len() == 1 {
        Some(range_size)
    } else {
        // Nested For loops - multiply ranges
        let rest_size = evaluate_for_range(&indices[1..], params)?;
        Some(range_size * rest_size)
    }
}

/// Evaluate the size of a range expression (end - start + 1) / step
fn evaluate_range_size(expr: &Expression, params: &IndexMap<String, Component>) -> Option<usize> {
    match expr {
        Expression::Range { start, step, end } => {
            let start_val = eval_integer(start, params)?;
            let end_val = eval_integer(end, params)?;
            let step_val = step
                .as_ref()
                .map(|s| eval_integer(s, params))
                .unwrap_or(Some(1))?;

            if step_val == 0 {
                return None;
            }

            // Count iterations
            let count = if step_val > 0 {
                if end_val >= start_val {
                    ((end_val - start_val) / step_val + 1) as usize
                } else {
                    0
                }
            } else if start_val >= end_val {
                ((start_val - end_val) / (-step_val) + 1) as usize
            } else {
                0
            };

            Some(count)
        }
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
        } => {
            // Single value means 1:value
            let n: usize = token.text.parse().ok()?;
            Some(n)
        }
        Expression::ComponentReference(_) => {
            // Could be a parameter reference like `n` meaning 1:n
            let val = eval_integer(expr, params)?;
            if val > 0 { Some(val as usize) } else { Some(0) }
        }
        _ => None,
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
            compile_time_ms: 0,
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
            compile_time_ms: 0,
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
            compile_time_ms: 0,
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
            compile_time_ms: 0,
        };
        assert!(partial.status_message().contains("partial"));
        assert_eq!(partial.difference(), -2);
        assert_eq!(partial.status, BalanceStatus::Partial);
        assert!(!partial.is_balanced());
    }
}
