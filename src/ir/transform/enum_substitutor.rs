//! Enumeration substitutor visitor
//!
//! This visitor substitutes built-in enumeration references with their integer values.
//! For example, `StateSelect.prefer` becomes `4`.
//!
//! Supported enumerations:
//! - StateSelect: never(1), avoid(2), default(3), prefer(4), always(5)
//! - Init: NoInit(1), SteadyState(2), InitialState(3), InitialOutput(4)
//! - Dynamics: DynamicFreeInitial(1), FixedInitial(2), SteadyStateInitial(3), SteadyState(4)
//! - GravityTypes: NoGravity(1), UniformGravity(2), PointGravity(3)
//! - AssertionLevel: warning(1), error(2)
//! - AnalogFilter: CriticalDamping(1), Bessel(2), Butterworth(3), ChebyshevI(4)
//! - FilterType: LowPass(1), HighPass(2), BandPass(3), BandStop(4)
//! - SimpleController: P(1), PI(2), PD(3), PID(4)

use crate::ir::ast::{Expression, TerminalType, Token};
use crate::ir::transform::constants::get_enumeration_value;
use crate::ir::visitor::MutVisitor;

/// Visitor that substitutes enumeration references with integer literal values
pub struct EnumSubstitutor {
    /// Number of substitutions made
    pub substitution_count: usize,
}

impl EnumSubstitutor {
    pub fn new() -> Self {
        Self {
            substitution_count: 0,
        }
    }
}

impl Default for EnumSubstitutor {
    fn default() -> Self {
        Self::new()
    }
}

impl MutVisitor for EnumSubstitutor {
    fn exit_expression(&mut self, expr: &mut Expression) {
        if let Expression::ComponentReference(comp_ref) = expr {
            let name = comp_ref.to_string();

            // Try to look up as enumeration value
            if let Some(value) = get_enumeration_value(&name) {
                // Replace with an integer literal
                *expr = Expression::Terminal {
                    terminal_type: TerminalType::UnsignedInteger,
                    token: Token {
                        text: value.to_string(),
                        ..Default::default()
                    },
                };
                self.substitution_count += 1;
            }
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
    fn test_substitute_state_select_prefer() {
        let mut expr = make_comp_ref("StateSelect.prefer");
        let mut sub = EnumSubstitutor::new();
        expr.accept_mut(&mut sub);

        if let Expression::Terminal { token, .. } = &expr {
            assert_eq!(token.text, "4");
        } else {
            panic!("Expected Terminal expression");
        }
        assert_eq!(sub.substitution_count, 1);
    }

    #[test]
    fn test_substitute_state_select_never() {
        let mut expr = make_comp_ref("StateSelect.never");
        let mut sub = EnumSubstitutor::new();
        expr.accept_mut(&mut sub);

        if let Expression::Terminal { token, .. } = &expr {
            assert_eq!(token.text, "1");
        } else {
            panic!("Expected Terminal expression");
        }
    }

    #[test]
    fn test_substitute_init_no_init() {
        let mut expr = make_comp_ref("Init.NoInit");
        let mut sub = EnumSubstitutor::new();
        expr.accept_mut(&mut sub);

        if let Expression::Terminal { token, .. } = &expr {
            assert_eq!(token.text, "1");
        } else {
            panic!("Expected Terminal expression");
        }
    }

    #[test]
    fn test_substitute_dynamics_steady_state() {
        let mut expr = make_comp_ref("Dynamics.SteadyState");
        let mut sub = EnumSubstitutor::new();
        expr.accept_mut(&mut sub);

        if let Expression::Terminal { token, .. } = &expr {
            assert_eq!(token.text, "4");
        } else {
            panic!("Expected Terminal expression");
        }
    }

    #[test]
    fn test_substitute_gravity_types() {
        let mut expr = make_comp_ref("GravityTypes.UniformGravity");
        let mut sub = EnumSubstitutor::new();
        expr.accept_mut(&mut sub);

        if let Expression::Terminal { token, .. } = &expr {
            assert_eq!(token.text, "2");
        } else {
            panic!("Expected Terminal expression");
        }
    }

    #[test]
    fn test_no_substitute_unknown() {
        let mut expr = make_comp_ref("UnknownEnum.value");
        let mut sub = EnumSubstitutor::new();
        expr.accept_mut(&mut sub);

        assert!(matches!(expr, Expression::ComponentReference(_)));
        assert_eq!(sub.substitution_count, 0);
    }

    #[test]
    fn test_substitute_qualified_state_select() {
        // Fully qualified names should also work
        let mut expr = make_comp_ref("Modelica.Blocks.Types.StateSelect.prefer");
        let mut sub = EnumSubstitutor::new();
        expr.accept_mut(&mut sub);

        if let Expression::Terminal { token, .. } = &expr {
            assert_eq!(token.text, "4");
        } else {
            panic!("Expected Terminal expression");
        }
    }
}
