//! Equation expansion pass
//!
//! This module expands structured equations into scalar form according to
//! the Modelica specification. After flattening, equations should be expanded:
//!
//! - For-equations are expanded to individual scalar equations
//! - Array equations are expanded to individual element equations
//! - Binding equations in declarations are converted to regular equations
//!
//! This makes balance checking trivial: just count the number of equations.

use crate::ir::ast::{
    ClassDefinition, Component, ComponentRefPart, ComponentReference, Equation, Expression,
    ForIndex, Statement, Subscript, TerminalType, Token,
};
use crate::ir::transform::eval::{eval_boolean, eval_integer, eval_real};
use crate::ir::visitor::{Visitable, Visitor};
use indexmap::IndexMap;
use std::collections::HashSet;

// =============================================================================
// Visitor-based Variable Finders
// =============================================================================

/// Visitor that finds variables inside der() calls (state variables).
///
/// This replaces manual recursion through equations and expressions with
/// the standard Visitor pattern for cleaner, more maintainable code.
struct DerVarFinder {
    states: HashSet<String>,
}

impl DerVarFinder {
    fn new() -> Self {
        Self {
            states: HashSet::new(),
        }
    }

    fn into_states(self) -> HashSet<String> {
        self.states
    }
}

impl Visitor for DerVarFinder {
    fn enter_expression(&mut self, node: &Expression) {
        let Expression::FunctionCall { comp, args } = node else {
            return;
        };

        // Check if this is a der() call and extract the variable name
        if comp.parts.first().is_none_or(|p| p.ident.text != "der") {
            return;
        }

        if let Some(name) = args.first().and_then(|arg| {
            if let Expression::ComponentReference(comp_ref) = arg {
                comp_ref.parts.first().map(|p| p.ident.text.clone())
            } else {
                None
            }
        }) {
            self.states.insert(name);
        }
    }
}

/// Visitor that finds variables assigned in statements.
///
/// This replaces manual recursion through statements with the standard
/// Visitor pattern.
struct AssignedVarFinder {
    assigned: HashSet<String>,
}

impl AssignedVarFinder {
    fn new() -> Self {
        Self {
            assigned: HashSet::new(),
        }
    }

    fn into_assigned(self) -> HashSet<String> {
        self.assigned
    }
}

impl Visitor for AssignedVarFinder {
    fn enter_statement(&mut self, node: &Statement) {
        match node {
            Statement::Assignment { comp, .. } => {
                // Get the base variable name (first part of component reference)
                if let Some(first_part) = comp.parts.first() {
                    self.assigned.insert(first_part.ident.text.clone());
                }
            }
            Statement::FunctionCall { outputs, .. } => {
                // Extract assigned variable names from output expressions
                // For `(a, b) := func(x)`, the outputs are [a, b]
                for name in outputs.iter().filter_map(|o| {
                    if let Expression::ComponentReference(comp_ref) = o {
                        comp_ref.parts.first().map(|p| p.ident.text.clone())
                    } else {
                        None
                    }
                }) {
                    self.assigned.insert(name);
                }
            }
            _ => {}
        }
    }
}

/// Expand all equations in a class definition to scalar form.
///
/// This includes:
/// - Evaluating computed parameters from initial equations
/// - Evaluating parameter-dependent array shapes
/// - Expanding for-loops to individual equations
/// - Expanding array equations to scalar equations
/// - Converting binding equations to regular equations
/// - Converting algorithm sections to equations
pub fn expand_equations(class: &mut ClassDefinition) {
    // First, evaluate computed parameters from initial equations
    // This handles cases like: na = integer((order+1)/2)
    evaluate_computed_parameters(&mut class.components, &class.initial_equations);

    // Then, evaluate any parameter-dependent array shapes
    evaluate_array_shapes(&mut class.components);

    // Expand structured equations first
    let mut expanded = Vec::new();
    for eq in &class.equations {
        expand_equation(eq, &class.components, &mut expanded);
    }

    // Convert algorithm sections to equations
    // Each algorithm section contributes one equation per unique variable assigned
    let algorithm_equations = convert_algorithms_to_equations(&class.algorithms, &class.components);
    expanded.extend(algorithm_equations);

    // Collect binding equations from components (needs expanded equations to find states)
    let binding_equations = extract_binding_equations(&class.components, &expanded);

    // Add binding equations
    expanded.extend(binding_equations);

    class.equations = expanded;

    // Also expand initial equations
    let mut expanded_init = Vec::new();
    for eq in &class.initial_equations {
        expand_equation(eq, &class.components, &mut expanded_init);
    }
    class.initial_equations = expanded_init;
}

/// Evaluate computed parameters from initial equations.
///
/// Some parameters have their values defined by binding equations in initial_equations,
/// like `na = integer((order+1)/2)`. This function tries to evaluate these expressions
/// using known parameter values and sets the parameter's start value.
fn evaluate_computed_parameters(
    components: &mut IndexMap<String, Component>,
    initial_equations: &[Equation],
) {
    // Iterate multiple times to handle transitive dependencies.
    // For example: na depends on order, and nx depends on na.
    const MAX_ITERATIONS: usize = 10;

    for _iteration in 0..MAX_ITERATIONS {
        // Collect updates first to avoid cloning the entire IndexMap
        let mut updates: Vec<(String, Expression)> = Vec::new();

        for eq in initial_equations {
            if let Equation::Simple {
                lhs: Expression::ComponentReference(comp_ref),
                rhs,
            } = eq
                && comp_ref.parts.iter().all(|p| p.subs.is_none())
            {
                let name = comp_ref
                    .parts
                    .iter()
                    .map(|p| p.ident.text.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                // Check if this component exists and has Empty start
                if let Some(comp) = components.get(&name)
                    && matches!(comp.start, Expression::Empty)
                {
                    // Try to evaluate the RHS as integer first
                    if let Some(val) = eval_integer(rhs, components) {
                        updates.push((
                            name,
                            Expression::Terminal {
                                terminal_type: TerminalType::UnsignedInteger,
                                token: Token {
                                    text: val.to_string(),
                                    ..Default::default()
                                },
                            },
                        ));
                    } else if let Some(val) = eval_real(rhs, components) {
                        updates.push((
                            name,
                            Expression::Terminal {
                                terminal_type: TerminalType::UnsignedReal,
                                token: Token {
                                    text: val.to_string(),
                                    ..Default::default()
                                },
                            },
                        ));
                    }
                }
            }
        }

        // Stop if no progress was made
        if updates.is_empty() {
            break;
        }

        // Apply updates
        for (name, value) in updates {
            if let Some(comp) = components.get_mut(&name) {
                comp.start = value;
            }
        }
    }
}

/// Evaluate parameter-dependent array shapes.
///
/// For components with `shape_expr` but empty `shape`, try to evaluate
/// the expressions to get the actual array dimensions.
fn evaluate_array_shapes(components: &mut IndexMap<String, Component>) {
    // Iterate multiple times to handle transitive dependencies.
    // For example: x_scaled[size(x, 1)] depends on x[size(a, 1) - 1]
    // which depends on a's size. We need to evaluate in dependency order.
    const MAX_ITERATIONS: usize = 10;

    for _iteration in 0..MAX_ITERATIONS {
        // Collect only the shape expressions that need evaluation (avoid cloning entire IndexMap)
        let to_evaluate: Vec<(String, Vec<Subscript>)> = components
            .iter()
            .filter(|(_, comp)| comp.shape.is_empty() && !comp.shape_expr.is_empty())
            .map(|(name, comp)| (name.clone(), comp.shape_expr.clone()))
            .collect();

        let mut updates: Vec<(String, Vec<usize>)> = Vec::new();

        for (name, shape_expr) in to_evaluate {
            let mut evaluated_shape = Vec::new();
            let mut all_evaluated = true;

            for sub in &shape_expr {
                match sub {
                    Subscript::Expression(expr) => {
                        // Pass all components (not just params) so size(array, dim) works
                        if let Some(val) = eval_integer(expr, components) {
                            // Allow val >= 0 (including 0 for empty arrays)
                            if val >= 0 {
                                evaluated_shape.push(val as usize);
                            } else {
                                all_evaluated = false;
                                break;
                            }
                        } else {
                            all_evaluated = false;
                            break;
                        }
                    }
                    Subscript::Range { .. } | Subscript::Empty => {
                        // Unbounded dimension or empty - can't evaluate statically
                        all_evaluated = false;
                        break;
                    }
                }
            }

            // Collect the update if all dimensions were evaluated
            if all_evaluated {
                updates.push((name, evaluated_shape));
            }
        }

        // Stop if no progress was made (all remaining shapes depend on runtime values)
        if updates.is_empty() {
            break;
        }

        // Apply updates
        for (name, shape) in updates {
            if let Some(comp) = components.get_mut(&name) {
                comp.shape = shape;
            }
        }
    }
}

/// Extract binding equations from component declarations.
///
/// A binding equation is `Real x = expr;` (as opposed to `Real x(start=expr);`).
/// These are identified by `start_is_modification == false` with a non-empty start expression.
///
/// Note: We need to check if the variable is differentiated (appears in der()) -
/// if so, it's a state variable and the binding is an initial value, not an equation.
fn extract_binding_equations(
    components: &IndexMap<String, Component>,
    equations: &[Equation],
) -> Vec<Equation> {
    // First, find all variables that appear in der() calls - these are states
    let states = find_differentiated_variables(equations);

    let mut binding_equations = Vec::new();

    for (name, comp) in components {
        // Skip if this is a modification (start=x) rather than a binding (= x)
        if comp.start_is_modification {
            continue;
        }

        // Skip if there's no start expression (empty binding)
        if matches!(comp.start, Expression::Empty) {
            continue;
        }

        // Skip if the start value is just a default numeric literal (0, 0.0)
        // These are not actual binding equations, just default values
        if is_default_value(&comp.start) {
            continue;
        }

        // Skip parameters and constants - they don't contribute equations
        if matches!(
            comp.variability,
            crate::ir::ast::Variability::Parameter(_) | crate::ir::ast::Variability::Constant(_)
        ) {
            continue;
        }

        // Skip inputs - they are provided externally
        if matches!(comp.causality, crate::ir::ast::Causality::Input(..)) {
            continue;
        }

        // Skip state variables - their binding is an initial condition, not an equation
        if states.contains(name) {
            continue;
        }

        // Create equation: name = start_expr
        let lhs = Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: name.clone(),
                    ..Default::default()
                },
                subs: None,
            }],
        });

        // If the component is an array, we need to expand to scalar equations
        if comp.shape.is_empty() {
            // Scalar binding equation
            binding_equations.push(Equation::Simple {
                lhs,
                rhs: comp.start.clone(),
            });
        } else {
            // Array binding equation - expand to scalars
            expand_array_binding(name, &comp.shape, &comp.start, &mut binding_equations);
        }
    }

    binding_equations
}

/// Convert algorithm sections to equations.
///
/// In Modelica, each algorithm section contributes one equation per unique
/// variable that is assigned. This function finds all assigned variables
/// and creates Simple equations for them.
///
/// For balance checking purposes, we create a simple placeholder equation
/// for each unique assigned variable. The actual algorithm semantics would
/// need more sophisticated handling for simulation.
///
/// IMPORTANT: For array variables, we create one equation per array element
/// to match the unknown count. The balance check counts scalar unknowns,
/// so we need scalar equations.
fn convert_algorithms_to_equations(
    algorithms: &[Vec<Statement>],
    components: &IndexMap<String, Component>,
) -> Vec<Equation> {
    let mut equations = Vec::new();

    for algorithm_section in algorithms {
        // Find all unique variables assigned in this algorithm section
        let assigned_vars = find_assigned_variables(algorithm_section);

        // Create equations per assigned variable (accounting for arrays)
        for var_name in assigned_vars {
            // Skip if it's an input (inputs don't need equations)
            if let Some(comp) = components.get(&var_name)
                && matches!(comp.causality, crate::ir::ast::Causality::Input(..))
            {
                continue;
            }

            // Check if this is an array variable
            let (num_elements, shape) = if let Some(comp) = components.get(&var_name) {
                if comp.shape.is_empty() {
                    (1, vec![]) // Scalar
                } else {
                    (comp.shape.iter().product(), comp.shape.clone()) // Product of all dimensions
                }
            } else {
                (1, vec![]) // Unknown component, assume scalar
            };

            // Create one equation per array element
            for i in 1..=num_elements {
                let comp_ref = if num_elements == 1 {
                    // Scalar variable - no subscript needed
                    ComponentReference {
                        local: false,
                        parts: vec![ComponentRefPart {
                            ident: Token {
                                text: var_name.clone(),
                                ..Default::default()
                            },
                            subs: None,
                        }],
                    }
                } else {
                    // Array variable - convert flat index to N-dimensional indices
                    let indices = flat_index_to_nd(&shape, i);
                    let subs: Vec<Subscript> = indices
                        .iter()
                        .map(|&idx| {
                            Subscript::Expression(Expression::Terminal {
                                terminal_type: TerminalType::UnsignedInteger,
                                token: Token {
                                    text: idx.to_string(),
                                    ..Default::default()
                                },
                            })
                        })
                        .collect();
                    ComponentReference {
                        local: false,
                        parts: vec![ComponentRefPart {
                            ident: Token {
                                text: var_name.clone(),
                                ..Default::default()
                            },
                            subs: Some(subs),
                        }],
                    }
                };

                let lhs = Expression::ComponentReference(comp_ref.clone());
                let rhs = Expression::ComponentReference(comp_ref);

                equations.push(Equation::Simple { lhs, rhs });
            }
        }
    }

    equations
}

/// Find all unique variable names assigned in an algorithm section.
///
/// Uses the visitor pattern for clean, maintainable traversal.
fn find_assigned_variables(statements: &[Statement]) -> HashSet<String> {
    let mut finder = AssignedVarFinder::new();
    for stmt in statements {
        stmt.accept(&mut finder);
    }
    finder.into_assigned()
}

/// Find all variables that appear inside der() calls (state variables).
///
/// Uses the visitor pattern for clean, maintainable traversal.
fn find_differentiated_variables(equations: &[Equation]) -> HashSet<String> {
    let mut finder = DerVarFinder::new();
    for eq in equations {
        eq.accept(&mut finder);
    }
    finder.into_states()
}

/// Check if an expression is a parser-generated default value that should not
/// be treated as a binding equation.
///
/// Returns true only if:
/// 1. The value is a default (0, 0.0, false), AND
/// 2. The token has no source location (empty file_name), indicating it was
///    generated by the parser as a default, not written explicitly in source.
///
/// This allows us to distinguish:
/// - `output Real y = 0.0;` (explicit binding → creates equation)
/// - `output Real y;` with parser default (no binding → no equation)
fn is_default_value(expr: &Expression) -> bool {
    match expr {
        Expression::Terminal {
            terminal_type,
            token,
        } => {
            // If the token has a source location (non-empty file_name), it's an
            // explicit value in the source code, not a parser default
            if !token.location.file_name.is_empty() {
                return false;
            }

            // Check if the value itself is a default
            match terminal_type {
                TerminalType::UnsignedInteger => token.text == "0",
                TerminalType::UnsignedReal => {
                    // Check for 0, 0.0, 0.0e0, etc.
                    if let Ok(val) = token.text.parse::<f64>() {
                        val == 0.0
                    } else {
                        false
                    }
                }
                TerminalType::Bool => token.text == "false",
                _ => false,
            }
        }
        _ => false,
    }
}

/// Expand an array binding equation to scalar equations.
fn expand_array_binding(
    name: &str,
    shape: &[usize],
    rhs: &Expression,
    equations: &mut Vec<Equation>,
) {
    // For now, handle 1D arrays
    if shape.len() == 1 {
        let size = shape[0];
        for i in 1..=size {
            let lhs = make_subscripted_ref(name, &[i]);

            // If RHS is an array literal, extract the corresponding element
            // Otherwise, subscript the RHS as well
            let rhs_elem = match rhs {
                Expression::Array { elements, .. } => {
                    if i <= elements.len() {
                        elements[i - 1].clone()
                    } else {
                        subscript_expr(rhs.clone(), &[i])
                    }
                }
                _ => subscript_expr(rhs.clone(), &[i]),
            };

            equations.push(Equation::Simple { lhs, rhs: rhs_elem });
        }
    } else {
        // Multi-dimensional arrays - create nested subscripts
        expand_array_binding_nd(name, shape, 0, &[], rhs, equations);
    }
}

/// Recursively expand multi-dimensional array binding.
fn expand_array_binding_nd(
    name: &str,
    shape: &[usize],
    dim: usize,
    indices: &[usize],
    rhs: &Expression,
    equations: &mut Vec<Equation>,
) {
    if dim >= shape.len() {
        // Base case: all dimensions indexed
        let lhs = make_subscripted_ref(name, indices);
        let rhs_elem = subscript_expr_nd(rhs.clone(), indices);
        equations.push(Equation::Simple { lhs, rhs: rhs_elem });
        return;
    }

    for i in 1..=shape[dim] {
        let mut new_indices = indices.to_vec();
        new_indices.push(i);
        expand_array_binding_nd(name, shape, dim + 1, &new_indices, rhs, equations);
    }
}

/// Expand a single equation, recursively handling For and If structures.
fn expand_equation(
    eq: &Equation,
    components: &IndexMap<String, Component>,
    out: &mut Vec<Equation>,
) {
    match eq {
        Equation::Empty => {}

        Equation::Simple { lhs, rhs } => {
            // Check if this is an array equation that needs expansion
            if let Some(size) = get_equation_array_size(lhs, components) {
                if size == 0 {
                    // Empty array equation (e.g., y[0] = u[0]) - no scalar equations
                    return;
                }
                if size > 1 {
                    expand_array_equation(lhs, rhs, size, components, out);
                    return;
                }
            }
            // Scalar equation - keep as is
            out.push(eq.clone());
        }

        Equation::For { indices, equations } => {
            // Expand for-loop to individual equations
            expand_for_equation(indices, equations, components, out);
        }

        Equation::If {
            cond_blocks,
            else_block,
        } => {
            // Try to evaluate conditions at compile time for parameter-based conditions
            let mut selected_branch: Option<&Vec<Equation>> = None;

            for block in cond_blocks {
                if let Some(val) = eval_boolean(&block.cond, components) {
                    if val {
                        selected_branch = Some(&block.eqs);
                        break;
                    }
                    // Condition is false, try next branch
                } else {
                    // Can't evaluate at compile time - keep the structure
                    let mut expanded_cond_blocks = Vec::new();
                    for block in cond_blocks {
                        let mut expanded_eqs = Vec::new();
                        for inner_eq in &block.eqs {
                            expand_equation(inner_eq, components, &mut expanded_eqs);
                        }
                        expanded_cond_blocks.push(crate::ir::ast::EquationBlock {
                            cond: block.cond.clone(),
                            eqs: expanded_eqs,
                        });
                    }

                    let expanded_else = else_block.as_ref().map(|eqs| {
                        let mut expanded = Vec::new();
                        for inner_eq in eqs {
                            expand_equation(inner_eq, components, &mut expanded);
                        }
                        expanded
                    });

                    out.push(Equation::If {
                        cond_blocks: expanded_cond_blocks,
                        else_block: expanded_else,
                    });
                    return;
                }
            }

            // If no true branch found, use else branch
            let eqs_to_expand = selected_branch.or(else_block.as_ref());

            if let Some(eqs) = eqs_to_expand {
                for inner_eq in eqs {
                    expand_equation(inner_eq, components, out);
                }
            }
        }

        Equation::When(blocks) => {
            // Expand equations inside when blocks
            let mut expanded_blocks = Vec::new();
            for block in blocks {
                let mut expanded_eqs = Vec::new();
                for inner_eq in &block.eqs {
                    expand_equation(inner_eq, components, &mut expanded_eqs);
                }
                expanded_blocks.push(crate::ir::ast::EquationBlock {
                    cond: block.cond.clone(),
                    eqs: expanded_eqs,
                });
            }
            out.push(Equation::When(expanded_blocks));
        }

        Equation::Connect { .. } | Equation::FunctionCall { .. } => {
            // Keep these as is
            out.push(eq.clone());
        }
    }
}

/// Expand a for-equation to individual scalar equations.
fn expand_for_equation(
    indices: &[ForIndex],
    equations: &[Equation],
    components: &IndexMap<String, Component>,
    out: &mut Vec<Equation>,
) {
    if indices.is_empty() {
        // No more indices to expand - expand the inner equations
        for eq in equations {
            expand_equation(eq, components, out);
        }
        return;
    }

    // Get the range for the first index
    let index = &indices[0];
    let range = get_iteration_range(&index.range, components);

    if let Some((start, end, step)) = range {
        // Expand the for-loop
        let index_name = &index.ident.text;
        let mut i = start;
        while (step > 0 && i <= end) || (step < 0 && i >= end) {
            // Substitute the index variable in the inner equations
            for eq in equations {
                let substituted = substitute_index(eq, index_name, i);
                // Recursively expand remaining indices
                expand_for_equation(&indices[1..], &[substituted], components, out);
            }
            i += step;
        }
    } else {
        // Couldn't determine the range (e.g., parameter-dependent)
        // Keep the for-equation as is, but expand inner equations
        let mut expanded_inner = Vec::new();
        for eq in equations {
            expand_equation(eq, components, &mut expanded_inner);
        }
        out.push(Equation::For {
            indices: indices.to_vec(),
            equations: expanded_inner,
        });
    }
}

/// Get the iteration range from a range expression.
/// Returns (start, end, step) if determinable, None otherwise.
fn get_iteration_range(
    expr: &Expression,
    components: &IndexMap<String, Component>,
) -> Option<(i64, i64, i64)> {
    match expr {
        Expression::Range { start, step, end } => {
            let start_val = eval_integer(start, components)?;
            let end_val = eval_integer(end, components)?;
            let step_val = step
                .as_ref()
                .map(|s| eval_integer(s, components))
                .unwrap_or(Some(1))?;
            Some((start_val, end_val, step_val))
        }
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
        } => {
            // Single value means 1:value
            let n: i64 = token.text.parse().ok()?;
            Some((1, n, 1))
        }
        Expression::ComponentReference(_) => {
            // Could be a parameter reference like `n`
            let val = eval_integer(expr, components)?;
            Some((1, val, 1))
        }
        _ => None,
    }
}

/// Substitute an index variable with a concrete value in an equation.
fn substitute_index(eq: &Equation, index_name: &str, value: i64) -> Equation {
    match eq {
        Equation::Simple { lhs, rhs } => Equation::Simple {
            lhs: substitute_in_expr(lhs, index_name, value),
            rhs: substitute_in_expr(rhs, index_name, value),
        },
        Equation::For { indices, equations } => {
            // Check if this introduces a shadowing variable
            let is_shadowed = indices.iter().any(|idx| idx.ident.text == index_name);
            if is_shadowed {
                eq.clone()
            } else {
                Equation::For {
                    indices: indices.clone(),
                    equations: equations
                        .iter()
                        .map(|e| substitute_index(e, index_name, value))
                        .collect(),
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => Equation::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|b| crate::ir::ast::EquationBlock {
                    cond: substitute_in_expr(&b.cond, index_name, value),
                    eqs: b
                        .eqs
                        .iter()
                        .map(|e| substitute_index(e, index_name, value))
                        .collect(),
                })
                .collect(),
            else_block: else_block.as_ref().map(|eqs| {
                eqs.iter()
                    .map(|e| substitute_index(e, index_name, value))
                    .collect()
            }),
        },
        Equation::When(blocks) => Equation::When(
            blocks
                .iter()
                .map(|b| crate::ir::ast::EquationBlock {
                    cond: substitute_in_expr(&b.cond, index_name, value),
                    eqs: b
                        .eqs
                        .iter()
                        .map(|e| substitute_index(e, index_name, value))
                        .collect(),
                })
                .collect(),
        ),
        _ => eq.clone(),
    }
}

/// Substitute an index variable in an expression.
fn substitute_in_expr(expr: &Expression, index_name: &str, value: i64) -> Expression {
    match expr {
        Expression::ComponentReference(comp_ref) => {
            // Check if this is just the index variable itself
            if comp_ref.parts.len() == 1
                && comp_ref.parts[0].subs.is_none()
                && comp_ref.parts[0].ident.text == index_name
            {
                // Replace with literal value
                return Expression::Terminal {
                    terminal_type: TerminalType::UnsignedInteger,
                    token: Token {
                        text: value.to_string(),
                        ..Default::default()
                    },
                };
            }

            // Substitute in subscripts
            let new_parts: Vec<ComponentRefPart> = comp_ref
                .parts
                .iter()
                .map(|part| ComponentRefPart {
                    ident: part.ident.clone(),
                    subs: part.subs.as_ref().map(|subs| {
                        subs.iter()
                            .map(|s| match s {
                                Subscript::Expression(e) => {
                                    Subscript::Expression(substitute_in_expr(e, index_name, value))
                                }
                                _ => s.clone(),
                            })
                            .collect()
                    }),
                })
                .collect();

            Expression::ComponentReference(ComponentReference {
                local: comp_ref.local,
                parts: new_parts,
            })
        }

        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(substitute_in_expr(lhs, index_name, value)),
            rhs: Box::new(substitute_in_expr(rhs, index_name, value)),
        },

        Expression::Unary { op, rhs } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(substitute_in_expr(rhs, index_name, value)),
        },

        Expression::FunctionCall { comp, args } => Expression::FunctionCall {
            comp: comp.clone(),
            args: args
                .iter()
                .map(|a| substitute_in_expr(a, index_name, value))
                .collect(),
        },

        Expression::Array {
            elements,
            is_matrix,
        } => Expression::Array {
            elements: elements
                .iter()
                .map(|e| substitute_in_expr(e, index_name, value))
                .collect(),
            is_matrix: *is_matrix,
        },

        Expression::If {
            branches,
            else_branch,
        } => Expression::If {
            branches: branches
                .iter()
                .map(|(cond, expr)| {
                    (
                        substitute_in_expr(cond, index_name, value),
                        substitute_in_expr(expr, index_name, value),
                    )
                })
                .collect(),
            else_branch: Box::new(substitute_in_expr(else_branch, index_name, value)),
        },

        Expression::Range { start, step, end } => Expression::Range {
            start: Box::new(substitute_in_expr(start, index_name, value)),
            step: step
                .as_ref()
                .map(|s| Box::new(substitute_in_expr(s, index_name, value))),
            end: Box::new(substitute_in_expr(end, index_name, value)),
        },

        Expression::Parenthesized { inner } => Expression::Parenthesized {
            inner: Box::new(substitute_in_expr(inner, index_name, value)),
        },

        _ => expr.clone(),
    }
}

/// Get the array size of an equation's LHS if it's an array variable.
fn get_equation_array_size(
    lhs: &Expression,
    components: &IndexMap<String, Component>,
) -> Option<usize> {
    match lhs {
        Expression::ComponentReference(comp_ref) => {
            if let Some(first_part) = comp_ref.parts.first() {
                // If already subscripted, it's a scalar access
                if first_part
                    .subs
                    .as_ref()
                    .map(|s| !s.is_empty())
                    .unwrap_or(false)
                {
                    return Some(1);
                }

                let name = &first_part.ident.text;
                if let Some(comp) = components.get(name) {
                    if comp.shape.is_empty() {
                        Some(1)
                    } else {
                        Some(comp.shape.iter().product())
                    }
                } else {
                    // Component not found directly - check if this is a flattened Complex type
                    // After flattening, Complex variable `y` becomes `y.re` and `y.im`
                    // Check if `y.re` exists to infer the array size
                    let re_name = format!("{}.re", name);
                    if let Some(re_comp) = components.get(&re_name) {
                        if re_comp.shape.is_empty() {
                            Some(1)
                        } else {
                            Some(re_comp.shape.iter().product())
                        }
                    } else {
                        Some(1)
                    }
                }
            } else {
                Some(1)
            }
        }
        Expression::FunctionCall { comp, args } => {
            // Handle der(x) - get size from argument
            if let Some(first_part) = comp.parts.first()
                && first_part.ident.text == "der"
                && let Some(arg) = args.first()
            {
                return get_equation_array_size(arg, components);
            }
            Some(1)
        }
        Expression::Array { elements, .. } => {
            // Array literal like [y] or [u1; u2]
            // Sum up the sizes of all elements (handles concatenation)
            let mut total = 0;
            for elem in elements {
                if let Some(size) = get_equation_array_size(elem, components) {
                    total += size;
                } else {
                    return None;
                }
            }
            Some(total)
        }
        _ => Some(1),
    }
}

/// Expand an array equation to scalar equations.
/// Takes components map for determining array sizes during flattening.
fn expand_array_equation(
    lhs: &Expression,
    rhs: &Expression,
    size: usize,
    components: &IndexMap<String, Component>,
    out: &mut Vec<Equation>,
) {
    // For 1D arrays, expand to individual equations using flat indexing
    for i in 1..=size {
        let lhs_elem = flatten_and_subscript(lhs, i, components);
        let rhs_elem = flatten_and_subscript(rhs, i, components);
        out.push(Equation::Simple {
            lhs: lhs_elem,
            rhs: rhs_elem,
        });
    }
}

/// Convert a 1-based flat index to multi-dimensional 1-based indices.
///
/// For example, for shape [2, 3] (2 rows, 3 cols):
/// - flat_index 1 -> [1, 1]
/// - flat_index 2 -> [1, 2]
/// - flat_index 3 -> [1, 3]
/// - flat_index 4 -> [2, 1]
/// - flat_index 5 -> [2, 2]
/// - flat_index 6 -> [2, 3]
///
/// Uses row-major (C-style) ordering where the last dimension varies fastest.
fn flat_index_to_nd(shape: &[usize], flat_index: usize) -> Vec<usize> {
    if shape.is_empty() {
        return vec![];
    }
    if shape.len() == 1 {
        return vec![flat_index];
    }

    // Convert to 0-based for calculation
    let mut remaining = flat_index - 1;
    let mut indices = Vec::with_capacity(shape.len());

    // Compute strides (product of dimensions to the right)
    let mut strides: Vec<usize> = Vec::with_capacity(shape.len());
    let mut stride = 1;
    for &dim in shape.iter().rev() {
        strides.push(stride);
        stride *= dim;
    }
    strides.reverse();

    // Extract each index
    for &s in &strides {
        indices.push(remaining / s + 1); // +1 for 1-based indexing
        remaining %= s;
    }

    indices
}

/// Create a subscripted component reference.
fn make_subscripted_ref(name: &str, indices: &[usize]) -> Expression {
    Expression::ComponentReference(ComponentReference {
        local: false,
        parts: vec![ComponentRefPart {
            ident: Token {
                text: name.to_string(),
                ..Default::default()
            },
            subs: Some(
                indices
                    .iter()
                    .map(|&i| {
                        Subscript::Expression(Expression::Terminal {
                            terminal_type: TerminalType::UnsignedInteger,
                            token: Token {
                                text: i.to_string(),
                                ..Default::default()
                            },
                        })
                    })
                    .collect(),
            ),
        }],
    })
}

/// Add subscripts to an expression.
/// For flat indexing, use subscript_expr_flat which handles concatenated arrays.
///
/// If the expression is a component reference with Range subscripts (`:` slicing),
/// the new indices replace the Range subscripts rather than replacing all subscripts.
/// For example: A[1, :] with indices [3] becomes A[1, 3].
fn subscript_expr(expr: Expression, indices: &[usize]) -> Expression {
    match expr {
        Expression::ComponentReference(mut comp_ref) => {
            // Add subscripts to the first part
            if let Some(first_part) = comp_ref.parts.first_mut() {
                // Check if existing subscripts contain Range subscripts
                let has_range_subs = first_part
                    .subs
                    .as_ref()
                    .is_some_and(|subs| subs.iter().any(|s| matches!(s, Subscript::Range { .. })));

                if has_range_subs {
                    // Replace Range subscripts with new indices
                    // For example: A[1, :] with indices [3] -> A[1, 3]
                    if let Some(existing_subs) = &mut first_part.subs {
                        let mut idx_iter = indices.iter();
                        for sub in existing_subs.iter_mut() {
                            if matches!(sub, Subscript::Range { .. })
                                && let Some(&i) = idx_iter.next()
                            {
                                *sub = Subscript::Expression(Expression::Terminal {
                                    terminal_type: TerminalType::UnsignedInteger,
                                    token: Token {
                                        text: i.to_string(),
                                        ..Default::default()
                                    },
                                });
                            }
                        }
                    }
                } else {
                    // No existing Range subscripts - replace all subscripts
                    let new_subs: Vec<Subscript> = indices
                        .iter()
                        .map(|&i| {
                            Subscript::Expression(Expression::Terminal {
                                terminal_type: TerminalType::UnsignedInteger,
                                token: Token {
                                    text: i.to_string(),
                                    ..Default::default()
                                },
                            })
                        })
                        .collect();

                    first_part.subs = Some(new_subs);
                }
            }
            Expression::ComponentReference(comp_ref)
        }
        Expression::FunctionCall { comp, args } => {
            // Handle der(x) -> der(x[i])
            if let Some(first_part) = comp.parts.first()
                && first_part.ident.text == "der"
                && args.len() == 1
            {
                return Expression::FunctionCall {
                    comp,
                    args: vec![subscript_expr(args[0].clone(), indices)],
                };
            }
            // For other functions, subscript the whole result
            Expression::FunctionCall { comp, args }
        }
        Expression::Unary { op, rhs } => Expression::Unary {
            op,
            rhs: Box::new(subscript_expr(*rhs, indices)),
        },
        Expression::Array { ref elements, .. } => {
            // For array literals, extract the element
            if elements.is_empty() {
                // Empty array - return unchanged
                expr
            } else if indices.len() == 1 && indices[0] > 0 && indices[0] <= elements.len() {
                elements[indices[0] - 1].clone()
            } else {
                subscript_expr(elements[0].clone(), indices)
            }
        }
        _ => expr,
    }
}

/// Flatten an array expression and get element at flat index.
/// For example, [[a, b], [c]] with index 2 returns b, index 3 returns c.
/// This handles matrix concatenation like [y] or [u1; u2].
fn flatten_and_subscript(
    expr: &Expression,
    flat_index: usize,
    components: &IndexMap<String, Component>,
) -> Expression {
    match expr {
        Expression::Array { elements, .. } => {
            // Track cumulative size as we go through elements
            let mut cumulative = 0;
            for elem in elements {
                let elem_size = get_equation_array_size(elem, components).unwrap_or(1);
                if flat_index <= cumulative + elem_size {
                    // The index falls within this element
                    let local_index = flat_index - cumulative;
                    if elem_size == 1 {
                        // Size 1 element - but we still need to check if it's a size-1 array
                        // that needs subscripting (e.g., y1[1] where n1=1)
                        if let Expression::ComponentReference(comp_ref) = elem
                            && let Some(first_part) = comp_ref.parts.first()
                        {
                            let name = &first_part.ident.text;
                            if let Some(comp) = components.get(name)
                                && !comp.shape.is_empty()
                            {
                                // It's an array (even if size 1) - subscript it
                                return subscript_expr(elem.clone(), &[1]);
                            }
                            // Check for flattened Complex type
                            let re_name = format!("{}.re", name);
                            if let Some(re_comp) = components.get(&re_name)
                                && !re_comp.shape.is_empty()
                            {
                                // It's a Complex array (even if size 1) - subscript it
                                return subscript_expr(elem.clone(), &[1]);
                            }
                        }
                        // Truly scalar element - return as-is
                        return elem.clone();
                    } else {
                        // Array element - recurse or subscript
                        return flatten_and_subscript(elem, local_index, components);
                    }
                }
                cumulative += elem_size;
            }
            // Fallback: shouldn't reach here if sizes are correct
            expr.clone()
        }
        Expression::ComponentReference(comp_ref) => {
            // Subscript the component reference
            if let Some(first_part) = comp_ref.parts.first() {
                let name = &first_part.ident.text;
                if let Some(comp) = components.get(name)
                    && !comp.shape.is_empty()
                {
                    // It's an array - convert flat index to multi-dimensional indices
                    let indices = flat_index_to_nd(&comp.shape, flat_index);
                    return subscript_expr(expr.clone(), &indices);
                }
                // Check if this is a flattened Complex type (y -> y.re, y.im)
                let re_name = format!("{}.re", name);
                if let Some(re_comp) = components.get(&re_name)
                    && !re_comp.shape.is_empty()
                {
                    // It's a Complex array - use the shape of y.re
                    let indices = flat_index_to_nd(&re_comp.shape, flat_index);
                    return subscript_expr(expr.clone(), &indices);
                }
            }
            // Scalar - return as-is
            expr.clone()
        }
        _ => expr.clone(),
    }
}

/// Add subscripts to an expression for multi-dimensional arrays.
fn subscript_expr_nd(expr: Expression, indices: &[usize]) -> Expression {
    subscript_expr(expr, indices)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_integer() {
        let components = IndexMap::new();

        // Test simple integer
        let expr = Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: "3".to_string(),
                ..Default::default()
            },
        };
        assert_eq!(eval_integer(&expr, &components), Some(3));

        // Test negative
        let expr = Expression::Unary {
            op: crate::ir::ast::OpUnary::Minus(Token::default()),
            rhs: Box::new(Expression::Terminal {
                terminal_type: TerminalType::UnsignedInteger,
                token: Token {
                    text: "5".to_string(),
                    ..Default::default()
                },
            }),
        };
        assert_eq!(eval_integer(&expr, &components), Some(-5));
    }

    #[test]
    fn test_get_iteration_range() {
        // Test 1:3
        let range = Expression::Range {
            start: Box::new(Expression::Terminal {
                terminal_type: TerminalType::UnsignedInteger,
                token: Token {
                    text: "1".to_string(),
                    ..Default::default()
                },
            }),
            step: None,
            end: Box::new(Expression::Terminal {
                terminal_type: TerminalType::UnsignedInteger,
                token: Token {
                    text: "3".to_string(),
                    ..Default::default()
                },
            }),
        };
        let components = IndexMap::new();
        assert_eq!(get_iteration_range(&range, &components), Some((1, 3, 1)));
    }
}
