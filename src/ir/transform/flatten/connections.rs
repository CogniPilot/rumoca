//! Connect equation expansion for flatten operations.
//!
//! This module handles Modelica connect() semantics:
//! - Equality equations for potential (non-flow) variables
//! - Sum-to-zero equations for flow variables
//! - Signal connector connections (RealInput/RealOutput)
//! - Nested connects in For/If/When structures

use crate::ir;
use crate::ir::ast::{
    ComponentRefPart, ComponentReference, Connection, Equation, Expression, OpBinary, TerminalType,
    Token,
};
use anyhow::Result;
use indexmap::{IndexMap, IndexSet};

use super::ClassDict;

// =============================================================================
// Component Reference Helpers
// =============================================================================

/// Creates a component reference from a flattened name like "R1.p.v"
/// Creates a ComponentReference from a flattened name, including any subscripts.
/// e.g., "a.c1.e[1]" becomes parts: [{ ident: "a.c1.e", subs: [1] }]
pub(super) fn make_comp_ref(name: &str) -> ComponentReference {
    // Check if the name has subscripts
    if let Some(bracket_start) = name.find('[') {
        // Extract base name and subscripts
        let base = &name[..bracket_start];
        let subs_str = &name[bracket_start..];

        // Parse the subscripts from the string like "[1]" or "[1, 2]"
        let subs = parse_subscript_string(subs_str);

        ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: base.to_string(),
                    ..Default::default()
                },
                subs: if subs.is_empty() { None } else { Some(subs) },
            }],
        }
    } else {
        ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: name.to_string(),
                    ..Default::default()
                },
                subs: None,
            }],
        }
    }
}

/// Parse a subscript string like "[1]" or "[1, 2]" into a vector of Subscripts
fn parse_subscript_string(s: &str) -> Vec<ir::ast::Subscript> {
    // Remove surrounding brackets
    let inner = s.trim_start_matches('[').trim_end_matches(']');
    if inner.is_empty() {
        return vec![];
    }

    inner
        .split(',')
        .map(|part| {
            let trimmed = part.trim();
            // Try to parse as integer
            if trimmed.parse::<i64>().is_ok() {
                ir::ast::Subscript::Expression(Expression::Terminal {
                    token: Token {
                        text: trimmed.to_string(),
                        ..Default::default()
                    },
                    terminal_type: ir::ast::TerminalType::UnsignedInteger,
                })
            } else {
                // If not an integer, treat as a variable reference
                ir::ast::Subscript::Expression(Expression::ComponentReference(ComponentReference {
                    local: false,
                    parts: vec![ComponentRefPart {
                        ident: Token {
                            text: trimmed.to_string(),
                            ..Default::default()
                        },
                        subs: None,
                    }],
                }))
            }
        })
        .collect()
}

// =============================================================================
// Equation Builders
// =============================================================================

/// Creates a simple equation: lhs = rhs
fn make_simple_eq(lhs: &str, rhs: &str) -> Equation {
    Equation::Simple {
        lhs: Expression::ComponentReference(make_comp_ref(lhs)),
        rhs: Expression::ComponentReference(make_comp_ref(rhs)),
    }
}

/// Creates an equation: lhs + rhs = 0
fn make_sum_eq(vars: &[String]) -> Equation {
    if vars.is_empty() {
        return Equation::Empty;
    }
    if vars.len() == 1 {
        // Single variable: var = 0
        return Equation::Simple {
            lhs: Expression::ComponentReference(make_comp_ref(&vars[0])),
            rhs: Expression::Terminal {
                token: Token {
                    text: "0".to_string(),
                    ..Default::default()
                },
                terminal_type: TerminalType::UnsignedReal,
            },
        };
    }

    // Build sum: var1 + var2 + ... = 0
    let mut sum = Expression::ComponentReference(make_comp_ref(&vars[0]));
    for var in vars.iter().skip(1) {
        sum = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(sum),
            rhs: Box::new(Expression::ComponentReference(make_comp_ref(var))),
        };
    }
    Equation::Simple {
        lhs: sum,
        rhs: Expression::Terminal {
            token: Token {
                text: "0".to_string(),
                ..Default::default()
            },
            terminal_type: TerminalType::UnsignedReal,
        },
    }
}

// =============================================================================
// Connect Detection
// =============================================================================

/// Check if an equation or its descendants contain any Connect equations.
pub(super) fn contains_connects(eq: &Equation) -> bool {
    match eq {
        Equation::Connect { .. } => true,
        Equation::For { equations, .. } => equations.iter().any(contains_connects),
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            cond_blocks
                .iter()
                .any(|b| b.eqs.iter().any(contains_connects))
                || else_block
                    .as_ref()
                    .is_some_and(|eqs| eqs.iter().any(contains_connects))
        }
        Equation::When(blocks) => blocks.iter().any(|b| b.eqs.iter().any(contains_connects)),
        _ => false,
    }
}

// =============================================================================
// Connect Extraction
// =============================================================================

/// Recursively extract connect equations from an equation, including nested For/If/When.
/// Returns the connect equations found and the equation with connect equations removed.
/// For For-equations containing connects, we DO NOT extract them - instead we return
/// the For equation to be processed separately with loop context preserved.
fn extract_connect_equations_recursive(
    eq: &Equation,
    connect_eqs: &mut Vec<(ComponentReference, ComponentReference)>,
) -> Option<Equation> {
    match eq {
        Equation::Connect { lhs, rhs } => {
            connect_eqs.push((lhs.clone(), rhs.clone()));
            None // Remove connect equation
        }
        Equation::For { indices, equations } => {
            // Check if this for loop contains any connects
            if equations.iter().any(contains_connects) {
                // Return the For equation as-is - it will be handled separately
                // to preserve the loop context for connect expansion
                Some(eq.clone())
            } else {
                // No connects inside - recursively process
                let mut filtered_eqs = Vec::new();
                for inner_eq in equations {
                    if let Some(filtered) =
                        extract_connect_equations_recursive(inner_eq, connect_eqs)
                    {
                        filtered_eqs.push(filtered);
                    }
                }
                if filtered_eqs.is_empty() {
                    None
                } else {
                    Some(Equation::For {
                        indices: indices.clone(),
                        equations: filtered_eqs,
                    })
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            // Check if any branch of this If equation contains connects
            let has_connects_in_if = cond_blocks
                .iter()
                .any(|b| b.eqs.iter().any(contains_connects))
                || else_block
                    .as_ref()
                    .is_some_and(|eqs| eqs.iter().any(contains_connects));

            if has_connects_in_if {
                // Return the If equation as-is - it will be handled separately
                // by expand_connects_in_equation to preserve the conditional structure.
                // This is important for patterns like:
                //   if cond then connect(a, b); else x = 0; end if;
                // where the connect should only apply when cond is true.
                Some(eq.clone())
            } else {
                // No connects inside - recursively process
                let mut new_cond_blocks = Vec::new();
                for block in cond_blocks {
                    let mut filtered_eqs = Vec::new();
                    for inner_eq in &block.eqs {
                        if let Some(filtered) =
                            extract_connect_equations_recursive(inner_eq, connect_eqs)
                        {
                            filtered_eqs.push(filtered);
                        }
                    }
                    new_cond_blocks.push(ir::ast::EquationBlock {
                        cond: block.cond.clone(),
                        eqs: filtered_eqs,
                    });
                }
                let new_else = else_block.as_ref().map(|eqs| {
                    let mut filtered = Vec::new();
                    for inner_eq in eqs {
                        if let Some(f) = extract_connect_equations_recursive(inner_eq, connect_eqs)
                        {
                            filtered.push(f);
                        }
                    }
                    filtered
                });
                Some(Equation::If {
                    cond_blocks: new_cond_blocks,
                    else_block: new_else,
                })
            }
        }
        Equation::When(blocks) => {
            let mut new_blocks = Vec::new();
            for block in blocks {
                let mut filtered_eqs = Vec::new();
                for inner_eq in &block.eqs {
                    if let Some(filtered) =
                        extract_connect_equations_recursive(inner_eq, connect_eqs)
                    {
                        filtered_eqs.push(filtered);
                    }
                }
                new_blocks.push(ir::ast::EquationBlock {
                    cond: block.cond.clone(),
                    eqs: filtered_eqs,
                });
            }
            Some(Equation::When(new_blocks))
        }
        _ => Some(eq.clone()),
    }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Strip array subscripts from a component name.
///
/// e.g., "a.c1[1]" -> "a.c1", "a.c1[1].e[2]" -> "a.c1.e"
fn strip_subscripts_from_name(name: &str) -> String {
    let mut result = String::new();
    let mut in_bracket = false;

    for c in name.chars() {
        if c == '[' {
            in_bracket = true;
        } else if c == ']' {
            in_bracket = false;
        } else if !in_bracket {
            result.push(c);
        }
    }

    result
}

/// Transform a pin reference with subscripts to a field reference.
/// e.g., "a.c1[1]" + "e" -> "a.c1.e[1]"
/// e.g., "cells[i, j].r" + "e" -> "cells.r.e[i, j]"
/// This moves subscripts from the connector level to the field level.
fn transform_pin_to_field(pin: &str, field: &str) -> String {
    // Check if the pin has subscripts
    if let Some(bracket_start) = pin.find('[') {
        // Find the matching closing bracket
        let mut bracket_depth = 0;
        let mut bracket_end = bracket_start;
        for (i, c) in pin[bracket_start..].char_indices() {
            match c {
                '[' => bracket_depth += 1,
                ']' => {
                    bracket_depth -= 1;
                    if bracket_depth == 0 {
                        bracket_end = bracket_start + i;
                        break;
                    }
                }
                _ => {}
            }
        }

        // Extract base, subscripts, and suffix
        let base = &pin[..bracket_start];
        let subscripts = &pin[bracket_start..=bracket_end];
        let suffix = &pin[bracket_end + 1..]; // Could be ".r" or empty

        // Build result: base + suffix + "." + field + subscripts
        // e.g., "cells" + ".r" + "." + "e" + "[i, j]" = "cells.r.e[i, j]"
        format!("{}{}.{}{}", base, suffix, field, subscripts)
    } else {
        format!("{}.{}", pin, field)
    }
}

// =============================================================================
// Connection Equation Generation
// =============================================================================

/// Generate equations for a set of connected pins based on connector class definition
fn generate_connection_equations(
    pins: &[&String],
    connector_class: &ir::ast::ClassDefinition,
    equations: &mut Vec<Equation>,
) {
    // For each variable in the connector
    for (var_name, var_comp) in &connector_class.components {
        let is_flow = matches!(var_comp.connection, Connection::Flow(_));

        if is_flow {
            // Flow variable: sum of all = 0
            let flow_vars: Vec<String> = pins
                .iter()
                .map(|pin| transform_pin_to_field(pin, var_name))
                .collect();
            equations.push(make_sum_eq(&flow_vars));
        } else {
            // Non-flow (potential) variable: all equal to first
            let first_var = transform_pin_to_field(pins[0], var_name);
            for pin in pins.iter().skip(1) {
                let other_var = transform_pin_to_field(pin, var_name);
                equations.push(make_simple_eq(&first_var, &other_var));
            }
        }
    }
}

/// Expand a single connect equation into equality and flow-sum equations.
fn expand_single_connect(
    lhs: &ComponentReference,
    rhs: &ComponentReference,
    class_dict: &ClassDict,
    pin_types: &IndexMap<String, String>,
) -> Vec<Equation> {
    let mut equations = Vec::new();

    let lhs_name = lhs.to_string();
    let rhs_name = rhs.to_string();

    // Get the connector type
    let lhs_base = strip_subscripts_from_name(&lhs_name);
    let connector_type = pin_types.get(&lhs_base);

    if let Some(connector_type) = connector_type
        && let Some(connector_class) = class_dict.get(connector_type)
        && !connector_class.components.is_empty()
    {
        // For each field in the connector
        for (var_name, var_comp) in &connector_class.components {
            let is_flow = matches!(var_comp.connection, Connection::Flow(_));
            let lhs_field = transform_pin_to_field(&lhs_name, var_name);
            let rhs_field = transform_pin_to_field(&rhs_name, var_name);

            if is_flow {
                // Flow variable: lhs + rhs = 0
                equations.push(make_sum_eq(&[lhs_field, rhs_field]));
            } else {
                // Non-flow (potential) variable: lhs = rhs
                equations.push(make_simple_eq(&lhs_field, &rhs_field));
            }
        }
    } else {
        // Signal connector or unknown - just set equal
        equations.push(make_simple_eq(&lhs_name, &rhs_name));
    }

    equations
}

/// Expand connect equations within a For/If/When structure, preserving the structure.
fn expand_connects_in_equation(
    eq: &Equation,
    class_dict: &ClassDict,
    pin_types: &IndexMap<String, String>,
) -> Vec<Equation> {
    match eq {
        Equation::Connect { lhs, rhs } => {
            // Transform connect to simple equations
            expand_single_connect(lhs, rhs, class_dict, pin_types)
        }
        Equation::For { indices, equations } => {
            // Recursively expand connects in inner equations
            let mut expanded_inner = Vec::new();
            for inner_eq in equations {
                expanded_inner.extend(expand_connects_in_equation(inner_eq, class_dict, pin_types));
            }
            if expanded_inner.is_empty() {
                vec![]
            } else {
                vec![Equation::For {
                    indices: indices.clone(),
                    equations: expanded_inner,
                }]
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            let new_cond_blocks: Vec<_> = cond_blocks
                .iter()
                .map(|block| {
                    let mut expanded_eqs = Vec::new();
                    for inner_eq in &block.eqs {
                        expanded_eqs
                            .extend(expand_connects_in_equation(inner_eq, class_dict, pin_types));
                    }
                    ir::ast::EquationBlock {
                        cond: block.cond.clone(),
                        eqs: expanded_eqs,
                    }
                })
                .collect();
            let new_else = else_block.as_ref().map(|eqs| {
                let mut expanded = Vec::new();
                for inner_eq in eqs {
                    expanded.extend(expand_connects_in_equation(inner_eq, class_dict, pin_types));
                }
                expanded
            });
            vec![Equation::If {
                cond_blocks: new_cond_blocks,
                else_block: new_else,
            }]
        }
        Equation::When(blocks) => {
            let new_blocks: Vec<_> = blocks
                .iter()
                .map(|block| {
                    let mut expanded_eqs = Vec::new();
                    for inner_eq in &block.eqs {
                        expanded_eqs
                            .extend(expand_connects_in_equation(inner_eq, class_dict, pin_types));
                    }
                    ir::ast::EquationBlock {
                        cond: block.cond.clone(),
                        eqs: expanded_eqs,
                    }
                })
                .collect();
            vec![Equation::When(new_blocks)]
        }
        _ => vec![eq.clone()],
    }
}

// =============================================================================
// Main Connect Expansion
// =============================================================================

/// Expand connect equations in a flattened class.
///
/// This function:
/// 1. Collects all connections (including from nested For/If/When) and builds a graph
/// 2. For each connection set, generates equality equations for non-flow vars
/// 3. For flow variables, generates a single sum=0 equation per connection set
///
/// - For flow variables: sum at each node = 0 (a.i + b.i + ... = 0)
pub(super) fn expand_connect_equations(
    fclass: &mut ir::ast::ClassDefinition,
    class_dict: &ClassDict,
    pin_types: &IndexMap<String, String>,
) -> Result<()> {
    // Use Union-Find to group connected pins
    let mut connection_sets: IndexMap<String, IndexSet<String>> = IndexMap::new();

    // Extract connect equations recursively from all equations (including nested structures)
    let mut connect_eqs: Vec<(ComponentReference, ComponentReference)> = Vec::new();
    let mut other_eqs: Vec<Equation> = Vec::new();

    for eq in &fclass.equations {
        if let Some(filtered_eq) = extract_connect_equations_recursive(eq, &mut connect_eqs) {
            other_eqs.push(filtered_eq);
        }
    }

    // Filter out connect equations to conditional components that don't exist.
    // When a connect references a conditional component (e.g., `connect(y1, fder)` where
    // `fder if use_fder` and `use_fder=false`), the connect should be silently skipped
    // per MLS §4.5.
    connect_eqs.retain(|(lhs, rhs)| {
        let lhs_name = lhs.to_string();
        let rhs_name = rhs.to_string();

        // Check if both endpoints exist as components or are prefixes of components
        let lhs_exists = fclass.components.contains_key(&lhs_name)
            || fclass
                .components
                .keys()
                .any(|k| k.starts_with(&format!("{}.", lhs_name)));
        let rhs_exists = fclass.components.contains_key(&rhs_name)
            || fclass
                .components
                .keys()
                .any(|k| k.starts_with(&format!("{}.", rhs_name)));

        lhs_exists && rhs_exists
    });

    // Check if there are any connects to process (either top-level or inside For loops)
    let has_for_connects = other_eqs.iter().any(contains_connects);
    if connect_eqs.is_empty() && !has_for_connects {
        // Even if there are no connects to process, we still need to update the equations
        // to remove any Connect equations that were filtered out (e.g., to non-existent
        // conditional components). The extraction process already removed them from other_eqs.
        fclass.equations = other_eqs;
        return Ok(());
    }

    // Build connection sets using a simple union-find approach
    // Each pin is represented as "component.subcomponent" (e.g., "R1.p")
    let mut parent: IndexMap<String, String> = IndexMap::new();

    fn find(parent: &mut IndexMap<String, String>, x: &str) -> String {
        if !parent.contains_key(x) {
            parent.insert(x.to_string(), x.to_string());
            return x.to_string();
        }
        let p = parent.get(x).expect("parent should exist").clone();
        if p != x {
            let root = find(parent, &p);
            parent.insert(x.to_string(), root.clone());
            return root;
        }
        p
    }

    fn union(parent: &mut IndexMap<String, String>, x: &str, y: &str) {
        let px = find(parent, x);
        let py = find(parent, y);
        if px != py {
            parent.insert(py, px);
        }
    }

    // Process connect equations to build union-find structure
    for (lhs, rhs) in &connect_eqs {
        let lhs_name = lhs.to_string();
        let rhs_name = rhs.to_string();
        union(&mut parent, &lhs_name, &rhs_name);
    }

    // Group all pins by their root
    for (lhs, rhs) in &connect_eqs {
        let lhs_name = lhs.to_string();
        let rhs_name = rhs.to_string();

        let root = find(&mut parent, &lhs_name);
        connection_sets
            .entry(root.clone())
            .or_default()
            .insert(lhs_name);

        let root = find(&mut parent, &rhs_name);
        connection_sets.entry(root).or_default().insert(rhs_name);
    }

    // For each connection set, generate equations
    let mut new_equations: Vec<Equation> = Vec::new();

    for (_root, pins) in &connection_sets {
        if pins.len() < 2 {
            continue;
        }

        let pins_vec: Vec<&String> = pins.iter().collect();

        // Get the connector type from the first pin using the pin_types map
        // Strip any array subscripts from the pin name for lookup
        // e.g., "a.c1[1]" -> "a.c1" to match how pin_types was populated
        let first_pin = pins_vec[0];
        let first_pin_base = strip_subscripts_from_name(first_pin);
        let mut generated = false;

        if let Some(connector_type) = pin_types.get(&first_pin_base)
            && let Some(connector_class) = class_dict.get(connector_type)
            && !connector_class.components.is_empty()
        {
            generate_connection_equations(&pins_vec, connector_class, &mut new_equations);
            generated = true;
        }

        // For signal connectors (type aliases like RealInput/RealOutput with no internal components),
        // generate equality equations for the connection.
        // For causal connectors: input = output (the input receives the value from the output)
        if !generated {
            // Separate pins into outputs (sources) and inputs (sinks)
            let mut output_pins: Vec<&String> = Vec::new();
            let mut input_pins: Vec<&String> = Vec::new();

            for pin in &pins_vec {
                if let Some(comp) = fclass.components.get(*pin) {
                    if matches!(comp.causality, ir::ast::Causality::Input(..)) {
                        input_pins.push(pin);
                    } else {
                        output_pins.push(pin);
                    }
                } else {
                    // Unknown pins are treated as outputs
                    output_pins.push(pin);
                }
            }

            // Find the source (output pin) - there should be exactly one
            // If no outputs, fall back to first pin as source (all-input connections)
            let source_pin = output_pins.first().or(pins_vec.first());

            if let Some(source) = source_pin {
                // For each input pin, generate: input = source
                // Skip if source == input (would create tautology like x = x)
                for input_pin in &input_pins {
                    if *input_pin != *source {
                        new_equations.push(make_simple_eq(input_pin, source));
                    }
                }

                // For additional outputs (if any), also set them equal to source
                for output_pin in output_pins.iter().skip(1) {
                    if *output_pin != *source {
                        new_equations.push(make_simple_eq(output_pin, source));
                    }
                }
            }
        }
    }

    // Process For equations that contain connects - expand connects in-place
    let mut final_eqs: Vec<Equation> = Vec::new();
    for eq in other_eqs {
        if contains_connects(&eq) {
            // Expand connects within this equation (For/If/When with connects)
            let expanded = expand_connects_in_equation(&eq, class_dict, pin_types);
            final_eqs.extend(expanded);
        } else {
            final_eqs.push(eq);
        }
    }

    // Replace equations
    fclass.equations = final_eqs;
    fclass.equations.extend(new_equations);

    Ok(())
}
