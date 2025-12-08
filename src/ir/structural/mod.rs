//! Block Lower Triangular (BLT) decomposition for equation ordering.
//!
//! This module implements BLT transformation to reorder equations so that:
//! 1. Each equation can be solved for one variable
//! 2. Variables are computed in dependency order
//! 3. Derivative equations (der(x) = ...) are in proper form
//!
//! The algorithm combines:
//! - **Hopcroft-Karp algorithm** for maximum bipartite matching (equation-variable assignment)
//! - **Tarjan's strongly connected components (SCC)** for topological ordering
//!
//! ## Submodules
//!
//! - `tearing` - Tearing algorithm for optimizing algebraic loop solving
//! - `differentiate` - Symbolic differentiation for index reduction
//! - `pantelides` - Pantelides algorithm for DAE index reduction
//!
//! ## Steps
//!
//! 1. Build bipartite graph: equations on one side, variables on the other
//! 2. Use Hopcroft-Karp to find maximum matching (O(E√V) complexity)
//! 3. Build dependency graph from matching
//! 4. Apply Tarjan's algorithm to find strongly connected components (SCCs)
//!    - SCCs represent blocks of mutually dependent equations (algebraic loops)
//! 5. Process equations in dependency order
//! 6. Normalize derivative equations: swap if der() on RHS but not on LHS
//!
//! ## References
//!
//! - Hopcroft, J. & Karp, R. (1973). "An n^(5/2) Algorithm for Maximum Matchings in Bipartite Graphs"
//! - Tarjan, R. (1972). "Depth-first search and linear graph algorithms"
//! - Pantelides, C. (1988). "The consistent initialization of differential-algebraic systems"
//! - Elmqvist, H. & Otter, M. (1994). "Methods for Tearing Systems of Equations"

mod causalize;
pub mod create_dae;
mod differentiate;
pub mod location;
mod matching;
mod pantelides;
mod scc;
mod tearing;

use crate::ir::ast::{ComponentReference, Equation, Expression};
use crate::ir::visitor::{Visitable, Visitor};
use causalize::{causalize_equation, check_if_needs_swap, normalize_derivative_equation};
use matching::find_maximum_matching;
use scc::tarjan_scc;
use std::collections::{HashMap, HashSet};

// Re-export public APIs
pub use causalize::has_der_call;
pub use differentiate::{differentiate_equation, differentiate_expression};
pub use pantelides::pantelides_index_reduction;
pub use tearing::{analyze_algebraic_loops, tear_algebraic_loop};

/// Visitor to find all variables referenced in an expression.
/// Excludes function names (like "der", "sin", etc.) from the variable list.
struct VariableFinder {
    variables: HashSet<String>,
    /// Track when entering a function call to skip the function name
    skip_next_cref: bool,
}

impl VariableFinder {
    fn new() -> Self {
        Self {
            variables: HashSet::new(),
            skip_next_cref: false,
        }
    }
}

impl Visitor for VariableFinder {
    fn enter_expression(&mut self, node: &Expression) {
        // When entering a function call, mark that we should skip the next component reference
        // (which is the function name, not a variable)
        if matches!(node, Expression::FunctionCall { .. }) {
            self.skip_next_cref = true;
        }
    }

    fn enter_component_reference(&mut self, comp: &ComponentReference) {
        // Skip function names, only collect actual variable references
        if self.skip_next_cref {
            self.skip_next_cref = false;
        } else {
            self.variables.insert(comp.to_string());
        }
    }
}

/// Visitor to find der() calls in an expression
struct DerivativeFinder {
    derivatives: Vec<String>,
}

impl DerivativeFinder {
    fn new() -> Self {
        Self {
            derivatives: Vec::new(),
        }
    }
}

impl Visitor for DerivativeFinder {
    fn enter_expression(&mut self, node: &Expression) {
        if let Expression::FunctionCall { comp, args } = node
            && comp.to_string() == "der"
            && !args.is_empty()
            && let Expression::ComponentReference(cref) = &args[0]
        {
            self.derivatives.push(cref.to_string());
        }
    }
}

/// Information about an equation in the BLT graph
#[derive(Debug, Clone)]
pub(crate) struct EquationInfo {
    pub equation: Equation,
    /// All variables that appear in this equation (both LHS and RHS)
    pub all_variables: HashSet<String>,
    /// Variable on LHS (if in form: var = expr or der(var) = expr)
    pub lhs_variable: Option<String>,
    /// True if this is a derivative equation: der(x) = expr
    pub is_derivative: bool,
    /// Matched variable (assigned by Hopcroft-Karp)
    pub matched_variable: Option<String>,
}

/// Information about an algebraic loop (SCC with size > 1)
///
/// Algebraic loops occur when equations are mutually dependent and must be
/// solved simultaneously. Tearing can reduce the computational cost by
/// selecting a subset of variables to iterate on.
#[derive(Debug, Clone)]
pub struct AlgebraicLoop {
    /// Indices of equations in this loop
    pub equation_indices: Vec<usize>,
    /// All variables involved in this loop
    pub variables: HashSet<String>,
    /// Tearing variables (if tearing was applied)
    /// These are guessed/iterated variables that break the loop
    pub tearing_variables: Vec<String>,
    /// Residual variables (solved sequentially from tearing variables)
    pub residual_variables: Vec<String>,
    /// Size of the loop (number of equations)
    pub size: usize,
}

/// A dummy derivative variable introduced by index reduction
///
/// When the Pantelides algorithm differentiates constraint equations,
/// it introduces new algebraic variables representing higher derivatives.
#[derive(Debug, Clone)]
pub struct DummyDerivative {
    /// Name of the dummy variable (e.g., "der_x" for der(x))
    pub name: String,
    /// The variable being differentiated
    pub base_variable: String,
    /// Order of differentiation (1 = first derivative, 2 = second, etc.)
    pub order: usize,
}

/// Result of structural analysis including index reduction information
///
/// This provides detailed information about DAE structure useful for:
/// - Detecting high-index DAEs that need index reduction
/// - Identifying algebraic loops that may need tearing
/// - Diagnosing structural singularities
#[derive(Debug, Clone, Default)]
pub struct StructuralAnalysis {
    /// The DAE index (0 = ODE, 1 = index-1 DAE, 2+ = high index)
    pub dae_index: usize,

    /// Equations that need to be differentiated for index reduction
    /// Maps equation index to number of times it needs differentiation
    pub equations_to_differentiate: HashMap<usize, usize>,

    /// Variables that are "dummy derivatives" introduced by index reduction
    /// These are new algebraic variables representing higher derivatives
    pub dummy_derivatives: Vec<DummyDerivative>,

    /// Information about each strongly connected component (algebraic loop)
    pub algebraic_loops: Vec<AlgebraicLoop>,

    /// Whether the system is structurally singular
    pub is_singular: bool,

    /// Diagnostic messages
    pub diagnostics: Vec<String>,
}

/// Result of BLT transformation including structural information
#[derive(Debug, Clone, Default)]
pub struct BltResult {
    /// Transformed equations in topological order
    pub equations: Vec<Equation>,
    /// Strongly connected components (algebraic loops have size > 1)
    pub sccs: Vec<Vec<usize>>,
    /// Matching: equation index -> matched variable name
    pub matching: HashMap<usize, String>,
    /// Whether a perfect matching was found
    pub is_complete_matching: bool,
    /// Algebraic loops with tearing information (SCCs with size > 1)
    pub algebraic_loops: Vec<AlgebraicLoop>,
}

/// Perform BLT transformation on a set of equations
///
/// This function:
/// 1. Parses equations to extract variable information
/// 2. Uses Hopcroft-Karp algorithm to find maximum matching between equations and variables
/// 3. Uses Tarjan's SCC algorithm for topological ordering
/// 4. Normalizes derivative equations (der(x) on LHS)
///
/// The `exclude_from_matching` parameter specifies variables that should not
/// be matched to equations (e.g., parameters, constants, time). This ensures that
/// equations like `R * i = v` are solved for the algebraic variable `i` rather
/// than the parameter `R`.
pub fn blt_transform(
    equations: Vec<Equation>,
    exclude_from_matching: &HashSet<String>,
) -> Vec<Equation> {
    blt_transform_with_info(equations, exclude_from_matching).equations
}

/// Perform BLT transformation and return detailed structural information
///
/// This function returns both the transformed equations and additional
/// structural information useful for:
/// - Index reduction (detecting high-index DAEs)
/// - Tearing (optimizing algebraic loop solving)
/// - Diagnostics (debugging structural issues)
pub fn blt_transform_with_info(
    equations: Vec<Equation>,
    exclude_from_matching: &HashSet<String>,
) -> BltResult {
    // Parse equations and extract variable information
    let mut eq_infos: Vec<EquationInfo> = Vec::new();
    let mut all_variables_set: HashSet<String> = HashSet::new();

    for eq in equations.iter() {
        if let Equation::Simple { lhs, rhs, .. } = eq {
            let mut info = EquationInfo {
                equation: eq.clone(),
                all_variables: HashSet::new(),
                lhs_variable: None,
                is_derivative: false,
                matched_variable: None,
            };

            // Find LHS variable and add to all_variables
            match lhs {
                Expression::ComponentReference(cref) => {
                    let var_name = cref.to_string();
                    info.lhs_variable = Some(var_name.clone());
                    info.all_variables.insert(var_name.clone());
                    all_variables_set.insert(var_name);
                }
                Expression::FunctionCall { comp, args } => {
                    if comp.to_string() == "der"
                        && !args.is_empty()
                        && let Expression::ComponentReference(cref) = &args[0]
                    {
                        let var_name = format!("der({})", cref);
                        info.lhs_variable = Some(var_name.clone());
                        info.all_variables.insert(var_name.clone());
                        all_variables_set.insert(var_name);
                        info.is_derivative = true;
                    }
                }
                _ => {
                    // For other LHS types, try to extract variables
                    let mut lhs_finder = VariableFinder::new();
                    lhs.accept(&mut lhs_finder);
                    for var in lhs_finder.variables {
                        info.all_variables.insert(var.clone());
                        all_variables_set.insert(var);
                    }
                }
            }

            // Find all variables in RHS
            let mut var_finder = VariableFinder::new();
            rhs.accept(&mut var_finder);
            for var in var_finder.variables {
                info.all_variables.insert(var.clone());
                all_variables_set.insert(var);
            }

            // Also check for der() calls in both LHS and RHS
            let mut der_finder = DerivativeFinder::new();
            lhs.accept(&mut der_finder);
            rhs.accept(&mut der_finder);
            for der_var in &der_finder.derivatives {
                let var_name = format!("der({})", der_var);
                info.all_variables.insert(var_name.clone());
                all_variables_set.insert(var_name);
            }

            eq_infos.push(info);
        } else {
            // Non-simple equations (If, When, etc.) - keep as-is
            eq_infos.push(EquationInfo {
                equation: eq.clone(),
                all_variables: HashSet::new(),
                lhs_variable: None,
                is_derivative: false,
                matched_variable: None,
            });
        }
    }

    // Convert variable set to sorted vector for consistent ordering
    // Exclude specified variables from the matching (e.g., parameters, constants, time)
    let all_variables: Vec<String> = {
        let mut vars: Vec<_> = all_variables_set
            .into_iter()
            .filter(|v| !exclude_from_matching.contains(v))
            .collect();
        vars.sort();
        vars
    };

    // Use Hopcroft-Karp to find maximum matching
    let matching = find_maximum_matching(&eq_infos, &all_variables, exclude_from_matching);

    // Update eq_infos with matched variables
    for (eq_idx, var_name) in &matching {
        eq_infos[*eq_idx].matched_variable = Some(var_name.clone());
    }

    // Build dependency graph and find ordering using Tarjan's SCC algorithm
    let tarjan_result = tarjan_scc(&eq_infos);

    // Reorder, normalize, and causalize equations
    let mut result_equations = Vec::new();
    for idx in &tarjan_result.ordered_indices {
        let info = &eq_infos[*idx];

        // Normalize derivative equations: if der(x) appears on RHS, swap sides
        if let Equation::Simple { lhs, rhs, .. } = &info.equation {
            let needs_swap = check_if_needs_swap(lhs, rhs);

            if needs_swap {
                // Swap LHS and RHS
                result_equations.push(Equation::Simple {
                    lhs: rhs.clone(),
                    rhs: lhs.clone(),
                });
            } else if let Some(normalized) = normalize_derivative_equation(lhs, rhs) {
                // Normalize derivative equations like C * der(x) = y to der(x) = y / C
                result_equations.push(normalized);
            } else {
                // Check if we need to causalize (solve for matched variable)
                if let Some(matched_var) = &info.matched_variable {
                    if let Some(causalized) = causalize_equation(lhs, rhs, matched_var) {
                        result_equations.push(causalized);
                    } else {
                        result_equations.push(info.equation.clone());
                    }
                } else {
                    result_equations.push(info.equation.clone());
                }
            }
        } else {
            result_equations.push(info.equation.clone());
        }
    }

    // Check if we have a complete matching
    let is_complete_matching = matching.len() == eq_infos.len();

    // Analyze algebraic loops (SCCs with size > 1) and apply tearing
    let algebraic_loops = tearing::analyze_algebraic_loops(&result_equations, &tarjan_result.sccs);

    BltResult {
        equations: result_equations,
        sccs: tarjan_result.sccs,
        matching,
        is_complete_matching,
        algebraic_loops,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{ComponentRefPart, OpBinary, OpUnary, TerminalType, Token};

    fn make_var(name: &str) -> Expression {
        Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: name.to_string(),
                    ..Default::default()
                },
                subs: None,
            }],
        })
    }

    fn make_der(var: Expression) -> Expression {
        Expression::FunctionCall {
            comp: ComponentReference {
                local: false,
                parts: vec![ComponentRefPart {
                    ident: Token {
                        text: "der".to_string(),
                        ..Default::default()
                    },
                    subs: None,
                }],
            },
            args: vec![var],
        }
    }

    fn make_zero() -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token: Token {
                text: "0".to_string(),
                ..Default::default()
            },
        }
    }

    #[test]
    fn test_swap_derivative_equation() {
        // v = der(h) should become der(h) = v
        let equations = vec![Equation::Simple {
            lhs: make_var("v"),
            rhs: make_der(make_var("h")),
        }];

        let result = blt_transform(equations, &HashSet::new());

        assert_eq!(result.len(), 1);
        if let Equation::Simple { lhs, rhs, .. } = &result[0] {
            assert!(has_der_call(lhs), "LHS should have der()");
            assert!(!has_der_call(rhs), "RHS should not have der()");
        } else {
            panic!("Expected Simple equation");
        }
    }

    #[test]
    fn test_blt_with_chain_dependencies() {
        // Test BLT ordering with chain: z = 1, y = z, x = y
        // Should be ordered as: z = 1, y = z, x = y

        let equations = vec![
            // x = y
            Equation::Simple {
                lhs: make_var("x"),
                rhs: make_var("y"),
            },
            // y = z
            Equation::Simple {
                lhs: make_var("y"),
                rhs: make_var("z"),
            },
            // z = 1
            Equation::Simple {
                lhs: make_var("z"),
                rhs: Expression::Terminal {
                    terminal_type: TerminalType::UnsignedInteger,
                    token: Token {
                        text: "1".to_string(),
                        ..Default::default()
                    },
                },
            },
        ];

        let result = blt_transform(equations, &HashSet::new());

        // After BLT, z=1 should come first, then y=z, then x=y
        assert_eq!(result.len(), 3);

        // Extract LHS variable names for checking order
        let order: Vec<String> = result
            .iter()
            .filter_map(|eq| {
                if let Equation::Simple {
                    lhs: Expression::ComponentReference(cref),
                    ..
                } = eq
                {
                    return Some(cref.to_string());
                }
                None
            })
            .collect();

        // z should come before y, y should come before x
        let z_pos = order.iter().position(|s| s == "z").unwrap();
        let y_pos = order.iter().position(|s| s == "y").unwrap();
        let x_pos = order.iter().position(|s| s == "x").unwrap();

        assert!(
            z_pos < y_pos,
            "z should be computed before y (z at {}, y at {})",
            z_pos,
            y_pos
        );
        assert!(
            y_pos < x_pos,
            "y should be computed before x (y at {}, x at {})",
            y_pos,
            x_pos
        );
    }

    #[test]
    fn test_blt_algebraic_loop_detection() {
        // Test that algebraic loops (SCCs) are kept together
        // x = y + 1
        // y = x + 1
        // These form an algebraic loop

        let one = Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: "1".to_string(),
                ..Default::default()
            },
        };

        let equations = vec![
            // x = y + 1
            Equation::Simple {
                lhs: make_var("x"),
                rhs: Expression::Binary {
                    lhs: Box::new(make_var("y")),
                    op: OpBinary::Add(Token::default()),
                    rhs: Box::new(one.clone()),
                },
            },
            // y = x + 1
            Equation::Simple {
                lhs: make_var("y"),
                rhs: Expression::Binary {
                    lhs: Box::new(make_var("x")),
                    op: OpBinary::Add(Token::default()),
                    rhs: Box::new(one),
                },
            },
        ];

        let result = blt_transform(equations, &HashSet::new());

        // Both equations should be present (algebraic loop)
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_causalize_already_causal() {
        // Test: a = b should return None (already in causal form for "a")
        let lhs = make_var("a");
        let rhs = make_var("b");

        let result = causalize_equation(&lhs, &rhs, "a");
        assert!(
            result.is_none(),
            "Should return None for already causal equation"
        );
    }

    #[test]
    fn test_causalize_sum_to_zero() {
        // Test: a + b = 0 with matching to "a" should become a = -b
        let lhs = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(make_var("a")),
            rhs: Box::new(make_var("b")),
        };
        let rhs = make_zero();

        let result = causalize_equation(&lhs, &rhs, "a");
        assert!(
            result.is_some(),
            "Should be able to causalize a + b = 0 for a"
        );

        if let Some(Equation::Simple { lhs, rhs }) = result {
            // LHS should be "a"
            if let Expression::ComponentReference(cref) = lhs {
                assert_eq!(cref.to_string(), "a");
            } else {
                panic!("LHS should be ComponentReference");
            }

            // RHS should be -b (Unary minus of b)
            if let Expression::Unary {
                op: OpUnary::Minus(_),
                rhs,
            } = rhs
            {
                if let Expression::ComponentReference(cref) = *rhs {
                    assert_eq!(cref.to_string(), "b");
                } else {
                    panic!("RHS of negation should be ComponentReference");
                }
            } else {
                panic!("RHS should be Unary negation, got: {:?}", rhs);
            }
        }
    }

    #[test]
    fn test_causalize_three_term_sum() {
        // Test: a + b + c = 0 with matching to "a" should become a = -(b + c)
        let inner = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(make_var("a")),
            rhs: Box::new(make_var("b")),
        };
        let lhs = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(inner),
            rhs: Box::new(make_var("c")),
        };
        let rhs = make_zero();

        let result = causalize_equation(&lhs, &rhs, "a");
        assert!(
            result.is_some(),
            "Should be able to causalize a + b + c = 0 for a"
        );

        if let Some(Equation::Simple { lhs, .. }) = result {
            // LHS should be "a"
            if let Expression::ComponentReference(cref) = lhs {
                assert_eq!(cref.to_string(), "a");
            } else {
                panic!("LHS should be ComponentReference");
            }
        }
    }

    #[test]
    fn test_causalize_zero_on_lhs() {
        // Test: 0 = a + b (alternate form) with matching to "a" should become a = -b
        let lhs = make_zero();
        let rhs = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(make_var("a")),
            rhs: Box::new(make_var("b")),
        };

        let result = causalize_equation(&lhs, &rhs, "a");
        assert!(
            result.is_some(),
            "Should be able to causalize 0 = a + b for a"
        );

        if let Some(Equation::Simple { lhs, rhs }) = result {
            // LHS should be "a"
            if let Expression::ComponentReference(cref) = lhs {
                assert_eq!(cref.to_string(), "a");
            } else {
                panic!("LHS should be ComponentReference");
            }

            // RHS should be -b (Unary minus of b)
            if let Expression::Unary {
                op: OpUnary::Minus(_),
                rhs,
            } = rhs
            {
                if let Expression::ComponentReference(cref) = *rhs {
                    assert_eq!(cref.to_string(), "b");
                } else {
                    panic!("RHS of negation should be ComponentReference");
                }
            } else {
                panic!("RHS should be Unary negation, got: {:?}", rhs);
            }
        }
    }

    #[test]
    fn test_kcl_style_equation() {
        // Simulate KCL equation from circuit: R2_n_i + L1_p_i = 0
        // This should be causalized to R2_n_i = -L1_p_i
        let equations = vec![Equation::Simple {
            lhs: Expression::Binary {
                op: OpBinary::Add(Token::default()),
                lhs: Box::new(make_var("R2_n_i")),
                rhs: Box::new(make_var("L1_p_i")),
            },
            rhs: make_zero(),
        }];

        let result = blt_transform(equations, &HashSet::new());
        assert_eq!(result.len(), 1);

        // The equation should now be causalized
        if let Equation::Simple { lhs, .. } = &result[0] {
            // LHS should be a simple variable reference, not a binary expression
            assert!(
                matches!(lhs, Expression::ComponentReference(_)),
                "LHS should be a simple variable after causalization, got: {:?}",
                lhs
            );
        } else {
            panic!("Expected Simple equation");
        }
    }

    #[test]
    fn test_kcl_style_equation_zero_on_lhs() {
        // Simulate KCL equation from circuit in the form: 0 = R2_n_i + L1_p_i
        // This should be causalized to one of the variables
        let equations = vec![Equation::Simple {
            lhs: make_zero(),
            rhs: Expression::Binary {
                op: OpBinary::Add(Token::default()),
                lhs: Box::new(make_var("R2_n_i")),
                rhs: Box::new(make_var("L1_p_i")),
            },
        }];

        let result = blt_transform(equations, &HashSet::new());
        assert_eq!(result.len(), 1);

        // The equation should now be causalized
        if let Equation::Simple { lhs, .. } = &result[0] {
            // LHS should be a simple variable reference, not zero
            assert!(
                matches!(lhs, Expression::ComponentReference(_)),
                "LHS should be a simple variable after causalization, got: {:?}",
                lhs
            );
        } else {
            panic!("Expected Simple equation");
        }
    }
}
