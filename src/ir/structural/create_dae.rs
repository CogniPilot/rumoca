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
use crate::ir::transform::eval::eval_boolean;
use crate::ir::visitor::{MutVisitable, Visitable, Visitor};
use git_version::git_version;
use indexmap::IndexMap;
use std::collections::HashSet;

use anyhow::Result;

// =============================================================================
// Defined Variable Collector Visitor
// =============================================================================

/// Visitor that collects variable names from LHS of simple equations.
struct DefinedVariableCollector {
    defined: HashSet<String>,
}

impl DefinedVariableCollector {
    fn new() -> Self {
        Self {
            defined: HashSet::new(),
        }
    }

    fn into_defined(self) -> HashSet<String> {
        self.defined
    }
}

impl Visitor for DefinedVariableCollector {
    fn enter_equation(&mut self, node: &Equation) {
        if let Equation::Simple {
            lhs: Expression::ComponentReference(cref),
            ..
        } = node
        {
            self.defined.insert(cref.to_string());
        }
    }
}

const GIT_VERSION: &str = git_version!(
    args = ["--tags", "--always", "--dirty=-dirty"],
    fallback = "unknown"
);

/// Check if an equation assigns to a filtered component (on the LHS).
/// Used to filter out equations that define variables that were removed due to false conditions.
fn equation_assigns_to_filtered(eq: &Equation, filtered: &HashSet<String>) -> bool {
    match eq {
        Equation::Simple { lhs, .. } => {
            if let Expression::ComponentReference(cref) = lhs {
                // Get the base component name (first part of the reference)
                if let Some(first_part) = cref.parts.first() {
                    return filtered.contains(&first_part.ident.text);
                }
            }
            false
        }
        // For other equation types, don't filter (they may have multiple LHS or complex structure)
        _ => false,
    }
}

/// Recursively filter equations, removing those that assign to filtered components.
fn filter_equations(equations: Vec<Equation>, filtered: &HashSet<String>) -> Vec<Equation> {
    equations
        .into_iter()
        .filter(|eq| !equation_assigns_to_filtered(eq, filtered))
        .map(|eq| match eq {
            Equation::If {
                cond_blocks,
                else_block,
            } => Equation::If {
                cond_blocks: cond_blocks
                    .into_iter()
                    .map(|mut block| {
                        block.eqs = filter_equations(block.eqs, filtered);
                        block
                    })
                    .collect(),
                else_block: else_block.map(|eqs| filter_equations(eqs, filtered)),
            },
            Equation::For {
                indices,
                equations: inner,
            } => Equation::For {
                indices,
                equations: filter_equations(inner, filtered),
            },
            Equation::When(blocks) => Equation::When(
                blocks
                    .into_iter()
                    .map(|mut block| {
                        block.eqs = filter_equations(block.eqs, filtered);
                        block
                    })
                    .collect(),
            ),
            other => other,
        })
        .collect()
}

/// Collect variable names that appear on the left-hand side of simple equations.
/// These variables have defining equations and should not be treated as external inputs
/// even if they have Input causality (e.g., signal connector inputs with connect equations).
///
/// Uses the visitor pattern for clean, maintainable traversal.
fn collect_defined_variables(equations: &[Equation]) -> HashSet<String> {
    let mut collector = DefinedVariableCollector::new();
    for eq in equations {
        eq.accept(&mut collector);
    }
    collector.into_defined()
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
        Expression::Array { elements, .. } => {
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

    // First pass: Collect all parameters for condition evaluation
    // Parameters are needed to evaluate conditional component expressions like `if use_reset`
    let mut all_params: IndexMap<String, Component> = IndexMap::new();
    for (_, comp) in &fclass.components {
        if matches!(
            comp.variability,
            Variability::Parameter(..) | Variability::Constant(..)
        ) {
            let expanded = expand_array_component(comp);
            for (name, c) in expanded {
                all_params.insert(name, c);
            }
        }
    }

    // Track components that are filtered out due to false conditions
    // These will be used to filter equations that assign to them
    let mut filtered_components: HashSet<String> = HashSet::new();

    // handle components - expand arrays to scalar components
    for (_, comp) in &fclass.components {
        // Check conditional component: if condition evaluates to false, skip this component
        if let Some(ref cond_expr) = comp.condition {
            match eval_boolean(cond_expr, &all_params) {
                Some(false) => {
                    // Condition is false - skip this component entirely
                    // Track the component name so we can filter equations that assign to it
                    filtered_components.insert(comp.name.clone());
                    continue;
                }
                Some(true) => {
                    // Condition is true - include the component
                }
                None => {
                    // Condition can't be evaluated at compile time - include the component
                    // This is the conservative approach (same as before)
                }
            }
        }

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
                            // Determine if this is a top-level input or a sub-component input.
                            // Top-level inputs are declared directly in the model (no dot in base name).
                            // Sub-component inputs are from instantiated components (dot in base name).
                            //
                            // Per Modelica spec: "the input prefix defines that values for such a
                            // variable have to be provided from the simulation environment."
                            // This means top-level inputs are ALWAYS external inputs, even if they
                            // appear on the LHS of equations (like in DeMultiplex: [u] = [y1; y2]).
                            //
                            // Sub-component inputs that have connect equations become internal signals
                            // (algebraic variables) because they are connected within the model.
                            let base_name = &comp.name; // Original name before array expansion
                            let is_top_level = !base_name.contains('.');

                            if is_top_level {
                                // Top-level input - always external input per Modelica spec
                                dae.u.insert(scalar_name, scalar_comp);
                            } else if defined_variables.contains(&scalar_name) {
                                // Sub-component input with defining equation - algebraic variable
                                dae.y.insert(scalar_name, scalar_comp);
                            } else {
                                // Sub-component input without defining equation - external input
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

    // Filter out equations that assign to conditional components that were removed
    let filtered_equations = if filtered_components.is_empty() {
        transformed_equations
    } else {
        filter_equations(transformed_equations, &filtered_components)
    };

    // handle equations
    for eq in &filtered_equations {
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
                                        // Each tuple element gets its own assignment in fr.
                                        // Do NOT add to fz as that would double-count equations.
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
