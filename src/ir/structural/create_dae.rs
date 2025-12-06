//! This module provides functionality for working with the `Dae` structure,
//! which is part of the Abstract Syntax Tree (AST) representation in the
//! Differential-Algebraic Equation (DAE) domain. It is used to model and
//! manipulate DAE-related constructs within the application.
use crate::dae::ast::Dae;
use crate::ir::analysis::condition_finder::ConditionFinder;
use crate::ir::analysis::state_finder::StateFinder;
use crate::ir::ast::{
    Causality, ClassDefinition, Component, Equation, Expression, Name, Statement, Token,
    Variability,
};
use crate::ir::error::IrError;
use crate::ir::transform::constants::BUILTIN_REINIT;
use crate::ir::visitor::MutVisitable;
use git_version::git_version;
use std::collections::HashSet;

use anyhow::Result;

const GIT_VERSION: &str = git_version!(
    args = ["--tags", "--always", "--dirty=-dirty"],
    fallback = "unknown"
);

/// Collect variable names that appear on the left-hand side of simple equations.
/// These variables have defining equations and should not be treated as external inputs
/// even if they have Input causality (e.g., signal connector inputs with connect equations).
fn collect_defined_variables(equations: &[Equation]) -> HashSet<String> {
    let mut defined = HashSet::new();
    collect_defined_variables_recursive(equations, &mut defined);
    defined
}

/// Recursively collect defined variables from equations, handling nested structures.
fn collect_defined_variables_recursive(equations: &[Equation], defined: &mut HashSet<String>) {
    for eq in equations {
        match eq {
            Equation::Simple {
                lhs: Expression::ComponentReference(cref),
                ..
            } => {
                defined.insert(cref.to_string());
            }
            Equation::Simple { .. } => {}
            Equation::For {
                equations: inner, ..
            } => {
                collect_defined_variables_recursive(inner, defined);
            }
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    collect_defined_variables_recursive(&block.eqs, defined);
                }
                if let Some(else_eqs) = else_block {
                    collect_defined_variables_recursive(else_eqs, defined);
                }
            }
            Equation::When(blocks) => {
                for block in blocks {
                    collect_defined_variables_recursive(&block.eqs, defined);
                }
            }
            _ => {}
        }
    }
}

/// Expand an array component into individual scalar components.
/// For example, `x[3]` becomes `x[1]`, `x[2]`, `x[3]`.
fn expand_array_component(comp: &Component) -> Vec<(String, Component)> {
    if comp.shape.is_empty() {
        // Scalar component - return as-is
        return vec![(comp.name.clone(), comp.clone())];
    }

    // Calculate total number of elements
    let total_elements: usize = comp.shape.iter().product();
    if total_elements == 0 {
        // Empty array (e.g., x[0]) - return nothing
        return vec![];
    }

    let mut result = Vec::with_capacity(total_elements);

    // Generate all index combinations
    let indices = generate_indices(&comp.shape);

    for idx in indices {
        // Create subscripted name like "x[1]" or "x[1,2]"
        let subscript_str = idx
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join(",");
        let scalar_name = format!("{}[{}]", comp.name, subscript_str);

        // Create a scalar component (empty shape)
        let mut scalar_comp = comp.clone();
        scalar_comp.name = scalar_name.clone();
        scalar_comp.shape = vec![]; // Now it's a scalar

        // If the start value is an array, extract the corresponding element
        if !matches!(comp.start, Expression::Empty) {
            scalar_comp.start = extract_array_element(&comp.start, &idx);
        }

        result.push((scalar_name, scalar_comp));
    }

    result
}

/// Generate all index combinations for a given shape.
/// For shape [2, 3], generates [[1,1], [1,2], [1,3], [2,1], [2,2], [2,3]]
fn generate_indices(shape: &[usize]) -> Vec<Vec<usize>> {
    if shape.is_empty() {
        return vec![vec![]];
    }

    let mut result = Vec::new();
    generate_indices_recursive(shape, 0, &mut vec![], &mut result);
    result
}

fn generate_indices_recursive(
    shape: &[usize],
    dim: usize,
    current: &mut Vec<usize>,
    result: &mut Vec<Vec<usize>>,
) {
    if dim >= shape.len() {
        result.push(current.clone());
        return;
    }

    for i in 1..=shape[dim] {
        current.push(i);
        generate_indices_recursive(shape, dim + 1, current, result);
        current.pop();
    }
}

/// Extract an element from an array expression given indices.
fn extract_array_element(expr: &Expression, indices: &[usize]) -> Expression {
    if indices.is_empty() {
        return expr.clone();
    }

    match expr {
        Expression::Array { elements } => {
            let idx = indices[0];
            if idx > 0 && idx <= elements.len() {
                if indices.len() == 1 {
                    elements[idx - 1].clone()
                } else {
                    extract_array_element(&elements[idx - 1], &indices[1..])
                }
            } else {
                expr.clone()
            }
        }
        _ => {
            // For non-array expressions, create a subscripted reference
            // This handles cases like `x_start` where start is a parameter
            expr.clone()
        }
    }
}

/// Creates a DAE (Differential-Algebraic Equation) representation from a flattened class definition.
///
/// This function transforms a flattened Modelica class into a structured DAE representation suitable
/// for numerical solving. It performs the following transformations:
///
/// - Identifies state variables (those appearing in `der()` calls) and their derivatives
/// - Classifies components by variability (parameters, constants, discrete, continuous)
/// - Classifies components by causality (inputs, outputs, algebraic variables)
/// - Finds and extracts conditions from when/if clauses
/// - Processes `reinit` statements in when clauses
/// - Creates previous value variables for discrete and state variables
/// - Collects all equations into the appropriate DAE categories
///
/// # Arguments
///
/// * `fclass` - A mutable reference to a flattened class definition (output from the `flatten` function)
///
/// # Returns
///
/// * `Result<Dae>` - The DAE representation on success, or an error if:
///   - Connection equations are not yet expanded (not implemented)
///   - Invalid reinit function calls are encountered
///
/// # Errors
///
/// Returns an error if connection equations are encountered (they should be expanded during
/// flattening but this feature is not yet implemented).
pub fn create_dae(fclass: &mut ClassDefinition) -> Result<Dae> {
    // create default Dae struct
    let mut dae = Dae {
        model_name: fclass.name.text.clone(),
        rumoca_version: env!("CARGO_PKG_VERSION").to_string(),
        git_version: GIT_VERSION.to_string(),
        t: Component {
            name: "t".to_string(),
            type_name: Name {
                name: vec![Token {
                    text: "Real".to_string(),
                    ..Default::default()
                }],
            },
            ..Default::default()
        },
        ..Default::default()
    };

    // run statefinder to find states and replace
    // derivative references
    let mut state_finder = StateFinder::default();
    fclass.accept_mut(&mut state_finder);

    // find conditions
    let mut condition_finder = ConditionFinder::default();
    fclass.accept_mut(&mut condition_finder);

    // Find variables that have defining equations (appear on LHS of simple equations)
    // These should be treated as algebraic variables, not inputs, even if they have Input causality
    let defined_variables = collect_defined_variables(&fclass.equations);

    // handle components - expand arrays to scalar components
    for (_, comp) in &fclass.components {
        // Expand array components to individual scalars
        let expanded = expand_array_component(comp);

        for (scalar_name, scalar_comp) in expanded {
            match scalar_comp.variability {
                Variability::Parameter(..) => {
                    dae.p.insert(scalar_name, scalar_comp);
                }
                Variability::Constant(..) => {
                    dae.cp.insert(scalar_name, scalar_comp);
                }
                Variability::Discrete(..) => {
                    dae.m.insert(scalar_name, scalar_comp);
                }
                Variability::Empty => {
                    // Check causality, but also check if the variable has a defining equation
                    match scalar_comp.causality {
                        Causality::Input(..) => {
                            // Input causality variables are only true "inputs" if they have no defining equation
                            // If they have a defining equation (e.g., from connect), they're algebraic variables
                            if defined_variables.contains(&scalar_name) {
                                // Has a defining equation - treat as algebraic variable
                                dae.y.insert(scalar_name, scalar_comp);
                            } else {
                                // No defining equation - true external input
                                dae.u.insert(scalar_name, scalar_comp);
                            }
                        }
                        Causality::Output(..) | Causality::Empty => {
                            // For outputs and regular variables, check if it's a state
                            // Check both the original array name and the scalar name for states
                            let base_name = comp.name.clone();
                            if state_finder.states.contains(&base_name)
                                || state_finder.states.contains(&scalar_name)
                            {
                                // Add state variable only - derivatives remain as der() calls in equations
                                dae.x.insert(scalar_name, scalar_comp);
                            } else {
                                dae.y.insert(scalar_name, scalar_comp);
                            }
                        }
                    }
                }
            }
        }
    }

    // handle conditions and relations
    dae.c = condition_finder.conditions.clone();
    dae.fc = condition_finder.expressions.clone();

    // Build set of variables to exclude from BLT matching
    // (parameters, constants, inputs, states, and "time" should not be solved for)
    // States are excluded because their values come from integration, not algebraic equations
    let mut exclude_from_matching: HashSet<String> = HashSet::new();
    for name in dae.p.keys() {
        exclude_from_matching.insert(name.clone());
    }
    for name in dae.cp.keys() {
        exclude_from_matching.insert(name.clone());
    }
    for name in dae.u.keys() {
        exclude_from_matching.insert(name.clone());
    }
    for name in dae.x.keys() {
        exclude_from_matching.insert(name.clone());
    }
    exclude_from_matching.insert("time".to_string());

    // Apply structural transformation to reorder and normalize equations
    let transformed_equations =
        crate::ir::structural::blt_transform(fclass.equations.clone(), &exclude_from_matching);

    // handle equations
    for eq in &transformed_equations {
        match &eq {
            Equation::Simple { .. } => {
                dae.fx.push(eq.clone());
            }
            Equation::If { .. } => {
                dae.fx.push(eq.clone());
            }
            Equation::For { .. } => {
                // For equations are passed through directly - they will be
                // either expanded by the backend or serialized as-is
                dae.fx.push(eq.clone());
            }
            Equation::Connect { .. } => {
                return Err(IrError::UnexpandedConnectionEquation.into());
            }
            Equation::When(blocks) => {
                for block in blocks {
                    for eq in &block.eqs {
                        match eq {
                            Equation::FunctionCall { comp, args } => {
                                let name = comp.to_string();
                                if name == BUILTIN_REINIT {
                                    let cond_name = match &block.cond {
                                        Expression::ComponentReference(cref) => cref.to_string(),
                                        other => {
                                            let loc = other
                                                .get_location()
                                                .map(|l| {
                                                    format!(
                                                        " at {}:{}:{}",
                                                        l.file_name, l.start_line, l.start_column
                                                    )
                                                })
                                                .unwrap_or_default();
                                            anyhow::bail!(
                                                "Unsupported condition type in 'when' block{}. \
                                                 Expected a component reference.",
                                                loc
                                            )
                                        }
                                    };
                                    if args.len() != 2 {
                                        return Err(
                                            IrError::InvalidReinitArgCount(args.len()).into()
                                        );
                                    }
                                    match &args[0] {
                                        Expression::ComponentReference(cref) => {
                                            dae.fr.insert(
                                                cond_name,
                                                Statement::Assignment {
                                                    comp: cref.clone(),
                                                    value: args[1].clone(),
                                                },
                                            );
                                        }
                                        _ => {
                                            return Err(IrError::InvalidReinitFirstArg(format!(
                                                "{:?}",
                                                args[0]
                                            ))
                                            .into());
                                        }
                                    }
                                }
                            }
                            Equation::Simple { lhs, rhs } => {
                                // Handle direct variable assignments in when blocks
                                // e.g., when trigger then y = expr; end when;
                                let cond_name = match &block.cond {
                                    Expression::ComponentReference(cref) => cref.to_string(),
                                    other => {
                                        let loc = other
                                            .get_location()
                                            .map(|l| {
                                                format!(
                                                    " at {}:{}:{}",
                                                    l.file_name, l.start_line, l.start_column
                                                )
                                            })
                                            .unwrap_or_default();
                                        anyhow::bail!(
                                            "Unsupported condition type in 'when' block{}. \
                                             Expected a component reference.",
                                            loc
                                        )
                                    }
                                };
                                // Convert lhs to ComponentReference for assignment
                                match lhs {
                                    Expression::ComponentReference(cref) => {
                                        dae.fr.insert(
                                            format!("{}_{}", cond_name, cref),
                                            Statement::Assignment {
                                                comp: cref.clone(),
                                                value: rhs.clone(),
                                            },
                                        );
                                    }
                                    Expression::Tuple { elements } => {
                                        // Handle tuple assignments like (a, b) = func()
                                        // Add as event update equation
                                        dae.fz.push(eq.clone());
                                        // Also add individual assignments for simple tuple elements
                                        for (i, elem) in elements.iter().enumerate() {
                                            if let Expression::ComponentReference(cref) = elem {
                                                // Create indexed access to RHS if it's a tuple result
                                                dae.fr.insert(
                                                    format!("{}_tuple_{}", cond_name, i),
                                                    Statement::Assignment {
                                                        comp: cref.clone(),
                                                        value: rhs.clone(), // Will be handled by backend
                                                    },
                                                );
                                            }
                                        }
                                    }
                                    _ => {
                                        // For other complex LHS patterns, add as event equation
                                        dae.fz.push(eq.clone());
                                    }
                                }
                            }
                            Equation::If { .. } | Equation::For { .. } => {
                                // Pass through if/for equations inside when blocks as event equations
                                dae.fz.push(eq.clone());
                            }
                            other => {
                                let loc = other
                                    .get_location()
                                    .map(|l| {
                                        format!(
                                            " at {}:{}:{}",
                                            l.file_name, l.start_line, l.start_column
                                        )
                                    })
                                    .unwrap_or_default();
                                anyhow::bail!(
                                    "Unsupported equation type in 'when' block{}. \
                                     Only assignments, 'reinit', 'if' and 'for' are currently supported.",
                                    loc
                                )
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Handle initial equations
    for eq in &fclass.initial_equations {
        match eq {
            Equation::Simple { .. } | Equation::For { .. } | Equation::If { .. } => {
                dae.fx_init.push(eq.clone());
            }
            _ => {
                // Other equation types in initial section are less common
                // but we'll pass them through
                dae.fx_init.push(eq.clone());
            }
        }
    }

    Ok(dae)
}
