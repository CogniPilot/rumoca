//! Constant substitutor visitor
//!
//! This visitor substitutes Modelica.Constants references with their literal values.
//! It handles both short forms (pi) and fully qualified forms (Modelica.Constants.pi).
//!
//! Note: Single-letter constants like h, k, c, e are commonly used as variable names,
//! so they are only substituted when using the fully qualified Modelica.Constants.x form.

use crate::ir::ast::{Expression, TerminalType, Token};
use crate::ir::transform::constants::get_modelica_constant;
use crate::ir::visitor::MutVisitor;

/// Constants that are safe to substitute with short names.
/// These names are unlikely to be used as regular variables.
const SAFE_SHORT_NAMES: &[&str] = &[
    "pi",        // Mathematical pi - very specific
    "mu_0",      // Vacuum permeability - has underscore
    "epsilon_0", // Vacuum permittivity - has underscore
    "sigma",     // Stefan-Boltzmann - specific physics term
    "g_n",       // Gravity - has underscore
    "N_A",       // Avogadro - has underscore
    "D2R",       // Degree to radian - specific
    "R2D",       // Radian to degree - specific
    "gamma",     // Euler-Mascheroni constant
    "eps",       // Machine epsilon - common in numerical code
    "small",     // Smallest float
    "inf",       // Infinity
    "T_zero",    // Absolute zero - has underscore
];

/// Check if a short name is safe to substitute without qualification.
/// Single-letter names (e, c, h, k, q, G, F, R) are NOT safe as they're common variable names.
fn is_safe_short_name(name: &str) -> bool {
    SAFE_SHORT_NAMES.contains(&name)
}

/// Visitor that substitutes Modelica.Constants references with literal values
pub struct ConstantSubstitutor {
    /// Number of substitutions made
    pub substitution_count: usize,
}

impl ConstantSubstitutor {
    pub fn new() -> Self {
        Self {
            substitution_count: 0,
        }
    }
}

impl Default for ConstantSubstitutor {
    fn default() -> Self {
        Self::new()
    }
}

impl MutVisitor for ConstantSubstitutor {
    fn exit_expression(&mut self, expr: &mut Expression) {
        if let Expression::ComponentReference(comp_ref) = expr {
            let name = comp_ref.to_string();

            // Check if this is a fully qualified Modelica.Constants reference
            let is_qualified = name.starts_with("Modelica.Constants.");

            // For short names, only substitute if it's a safe name
            // (e.g., "pi" is safe, but "h" is not as it's a common variable name)
            let should_substitute = if is_qualified {
                true // Always substitute fully qualified names
            } else {
                is_safe_short_name(&name)
            };

            if should_substitute && let Some(value) = get_modelica_constant(&name) {
                // Replace with a literal value
                *expr = Expression::Terminal {
                    terminal_type: TerminalType::UnsignedReal,
                    token: Token {
                        text: format_float(value),
                        ..Default::default()
                    },
                };
                self.substitution_count += 1;
            }
        }
    }
}

/// Format a float value for Modelica syntax
/// Uses scientific notation for very large or small numbers
fn format_float(value: f64) -> String {
    if value == f64::MAX {
        // Modelica.Constants.inf
        "1.7976931348623157e308".to_string()
    } else if value == f64::MIN_POSITIVE {
        // Modelica.Constants.small
        "2.2250738585072014e-308".to_string()
    } else if value.abs() >= 1e10 || (value != 0.0 && value.abs() < 1e-4) {
        // Use scientific notation for very large or small numbers
        format!("{:e}", value)
    } else {
        // Use standard notation, ensuring it has a decimal point
        let s = format!("{}", value);
        if s.contains('.') || s.contains('e') {
            s
        } else {
            format!("{}.0", s)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{ComponentRefPart, ComponentReference};
    use crate::ir::visitor::MutVisitable;

    fn make_comp_ref(name: &str) -> Expression {
        let parts: Vec<ComponentRefPart> = name
            .split('.')
            .map(|p| ComponentRefPart {
                ident: Token {
                    text: p.to_string(),
                    ..Default::default()
                },
                subs: None,
            })
            .collect();

        Expression::ComponentReference(ComponentReference {
            local: false,
            parts,
        })
    }

    #[test]
    fn test_substitute_pi() {
        let mut expr = make_comp_ref("pi");
        let mut sub = ConstantSubstitutor::new();
        expr.accept_mut(&mut sub);

        if let Expression::Terminal { token, .. } = &expr {
            let value: f64 = token.text.parse().unwrap();
            assert!((value - std::f64::consts::PI).abs() < 1e-10);
        } else {
            panic!("Expected Terminal expression");
        }
        assert_eq!(sub.substitution_count, 1);
    }

    #[test]
    fn test_substitute_qualified_pi() {
        let mut expr = make_comp_ref("Modelica.Constants.pi");
        let mut sub = ConstantSubstitutor::new();
        expr.accept_mut(&mut sub);

        if let Expression::Terminal { token, .. } = &expr {
            let value: f64 = token.text.parse().unwrap();
            assert!((value - std::f64::consts::PI).abs() < 1e-10);
        } else {
            panic!("Expected Terminal expression");
        }
    }

    #[test]
    fn test_substitute_mu_0() {
        let mut expr = make_comp_ref("mu_0");
        let mut sub = ConstantSubstitutor::new();
        expr.accept_mut(&mut sub);

        if let Expression::Terminal { token, .. } = &expr {
            let value: f64 = token.text.parse().unwrap();
            assert!((value - 1.25663706212e-6).abs() < 1e-16);
        } else {
            panic!("Expected Terminal expression");
        }
    }

    #[test]
    fn test_no_substitute_unknown() {
        let mut expr = make_comp_ref("unknown_var");
        let mut sub = ConstantSubstitutor::new();
        expr.accept_mut(&mut sub);

        assert!(matches!(expr, Expression::ComponentReference(_)));
        assert_eq!(sub.substitution_count, 0);
    }

    #[test]
    fn test_no_substitute_ambiguous_short_name() {
        // "h" is Planck's constant, but it's a common variable name
        // so it should NOT be substituted without qualification
        let mut expr = make_comp_ref("h");
        let mut sub = ConstantSubstitutor::new();
        expr.accept_mut(&mut sub);

        // Should remain as component reference, not substituted
        assert!(matches!(expr, Expression::ComponentReference(_)));
        assert_eq!(sub.substitution_count, 0);
    }

    #[test]
    fn test_substitute_qualified_h() {
        // Fully qualified Modelica.Constants.h should be substituted
        let mut expr = make_comp_ref("Modelica.Constants.h");
        let mut sub = ConstantSubstitutor::new();
        expr.accept_mut(&mut sub);

        if let Expression::Terminal { token, .. } = &expr {
            let value: f64 = token.text.parse().unwrap();
            // Planck constant ~ 6.62607015e-34
            assert!((value - 6.62607015e-34).abs() < 1e-44);
        } else {
            panic!("Expected Terminal expression");
        }
        assert_eq!(sub.substitution_count, 1);
    }
}
