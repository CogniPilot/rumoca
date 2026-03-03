//! Variable and equation classification for the ToDae phase.
//!
//! This module determines the role of each variable and equation
//! in the DAE system.

use std::collections::HashSet;

use rumoca_ir_ast as ast;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

// =============================================================================
// State Variable Detection
// =============================================================================

/// Find all state variables by scanning equations and bindings for der() calls.
///
/// Bindings must be scanned because they are converted to equations AFTER state
/// detection (in `convert_bindings_to_equations`). If `der(x)` only appears in
/// a binding expression (e.g., `body.frame_a.R = Frames.from_Q(body.Q,
/// Frames.Quaternions.angularVelocity2(body.Q, der(body.Q)))`), it would be
/// missed without this scan.
pub fn find_state_variables(flat: &flat::Model) -> HashSet<flat::VarName> {
    let mut states = HashSet::default();

    // Scan all equations for der() calls
    for eq in &flat.equations {
        eq.residual.collect_state_variables(&mut states);
    }

    // Also scan initial equations
    for eq in &flat.initial_equations {
        eq.residual.collect_state_variables(&mut states);
    }

    // Scan variable bindings for der() calls (MLS §4.4.1)
    // Bindings become equations during todae, but state detection runs first
    for var in flat.variables.values() {
        if let Some(binding) = &var.binding {
            binding.collect_state_variables(&mut states);
        }
    }

    states
}

// =============================================================================
// Variable Classification
// =============================================================================

/// Classify a variable based on its attributes.
///
/// Classification priority (MLS Appendix B - DAE representation):
/// 1. ast::Variability (constant/parameter) - fixed values, no equations needed
/// 2. ast::Causality (input) - external inputs, determined by connections
/// 3. ast::Variability (discrete) - discrete variables change only at events (MLS §4.5)
///    This includes Integer/Boolean outputs which are discrete by default
/// 4. State detection for continuous outputs - output variables with der() are states
/// 5. ast::Causality (output) - continuous outputs without der() are algebraic outputs
/// 6. State detection for other variables - appear in der()
/// 7. Default: algebraic
///
/// Key insight: Variables with input causality that appear in der() are still
/// inputs - der(u) computes the derivative of the input signal, it doesn't
/// make u a state. However, output variables that appear in der() ARE states
/// because the ODE equation determines their time evolution.
///
/// Integer and Boolean variables are discrete by default (MLS §4.5) and should
/// not be counted as continuous unknowns, even if they have output causality.
pub fn classify_variable(
    var: &flat::Variable,
    state_vars: &HashSet<flat::VarName>,
) -> dae::VariableKind {
    // Check variability for constant/parameter first - these are fixed values
    match &var.variability {
        ast::Variability::Constant(_) => return dae::VariableKind::Constant,
        ast::Variability::Parameter(_) => return dae::VariableKind::Parameter,
        _ => {}
    }

    // Check input causality - inputs stay inputs even if they appear in der()
    // (der(u) computes the derivative of the input, u is still determined externally)
    if matches!(&var.causality, ast::Causality::Input(_)) {
        return dae::VariableKind::Input;
    }

    // Check discrete variability BEFORE output causality (MLS §4.5)
    // Variables with explicit discrete variability prefix are discrete.
    if matches!(&var.variability, ast::Variability::Discrete(_)) {
        return dae::VariableKind::Discrete;
    }

    // MLS §4.5 / Appendix B: discrete-valued variables (Boolean/Integer/enum)
    // are event-driven unknowns and belong to the discrete partition even when
    // declared as outputs.
    if var.is_discrete_type {
        return dae::VariableKind::Discrete;
    }

    // For output variables: if they appear in der(), they're states
    // (the ODE equation defines their time evolution)
    if matches!(&var.causality, ast::Causality::Output(_)) {
        if state_vars.contains(&var.name) {
            return dae::VariableKind::State;
        }
        return dae::VariableKind::Output;
    }

    // For non-interface variables: if they appear in der(), they're states
    if state_vars.contains(&var.name) {
        return dae::VariableKind::State;
    }

    // Default: algebraic variable
    dae::VariableKind::Algebraic
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_var(name: &str) -> flat::Expression {
        flat::Expression::VarRef {
            name: flat::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn make_der(var_name: &str) -> flat::Expression {
        flat::Expression::BuiltinCall {
            function: flat::BuiltinFunction::Der,
            args: vec![make_var(var_name)],
        }
    }

    fn make_int(value: i64) -> flat::Expression {
        flat::Expression::Literal(flat::Literal::Integer(value))
    }

    #[test]
    fn test_empty_state_vars() {
        let flat = flat::Model::new();
        let states = find_state_variables(&flat);
        assert!(states.is_empty());
    }

    #[test]
    fn test_flat_expression_finds_state() {
        // Test: der(x) - 1 should find x as state
        let expr = flat::Expression::Binary {
            op: ast::OpBinary::Sub(Default::default()),
            lhs: Box::new(make_der("x")),
            rhs: Box::new(make_int(1)),
        };

        let mut states = HashSet::default();
        expr.collect_state_variables(&mut states);

        assert_eq!(states.len(), 1);
        assert!(states.contains(&flat::VarName::new("x")));
    }

    #[test]
    fn test_contains_der() {
        let expr_with_der = make_der("y");
        let expr_without_der = make_var("z");

        assert!(expr_with_der.contains_der());
        assert!(!expr_without_der.contains_der());
    }

    #[test]
    fn test_nested_der_detection() {
        // Test: sin(der(x)) should still find x
        let nested_der = flat::Expression::BuiltinCall {
            function: flat::BuiltinFunction::Sin,
            args: vec![make_der("nested_x")],
        };

        let mut states = HashSet::default();
        nested_der.collect_state_variables(&mut states);

        assert!(states.contains(&flat::VarName::new("nested_x")));
    }
}
