//! This module provides functionality to flatten a hierarchical intermediate representation (IR)
//! of a syntax tree into a flat representation. The primary purpose of this process is to
//! simplify the structure of the IR by expanding nested components and incorporating their
//! equations and subcomponents into a single flat class definition.
//!
//! The main function in this module is `flatten`, which takes a stored definition of the IR
//! and produces a flattened class definition. The process involves:
//!
//! - Identifying the main class and other class definitions from the provided IR.
//! - Iteratively expanding components in the main class that reference other class definitions.
//! - Propagating equations and subcomponents from referenced classes into the main class.
//! - Removing expanded components from the main class to ensure a flat structure.
//!
//! This module relies on `SymbolTable` for scope tracking and `SubCompNamer` for
//! renaming hierarchical component references during the flattening process.
//!
//! # Dependencies
//! - `anyhow::Result`: For error handling.
//! - `indexmap::IndexMap`: To maintain the order of class definitions and components.
//!

use crate::ir;
use crate::ir::ast::{
    ComponentRefPart, ComponentReference, Connection, Equation, Expression, OpBinary, TerminalType,
    Token,
};
use crate::ir::constants::is_primitive_type;
use crate::ir::error::IrError;
use crate::ir::visitor::{Visitable, Visitor};
use crate::ir::visitors::sub_comp_namer::SubCompNamer;
use crate::ir::visitors::symbol_table::SymbolTable;
use anyhow::Result;
use indexmap::{IndexMap, IndexSet};

/// Visitor that renames component references using a symbol table.
///
/// This visitor uses a `SymbolTable` to look up variable names and prepend
/// the appropriate scope prefix when the variable is not a global symbol.
#[derive(Debug, Clone)]
struct ScopeRenamer<'a> {
    /// Reference to the symbol table for lookups
    symbol_table: &'a SymbolTable,
    /// The component scope prefix to prepend
    scope_prefix: String,
}

impl<'a> ScopeRenamer<'a> {
    fn new(symbol_table: &'a SymbolTable, scope_prefix: &str) -> Self {
        Self {
            symbol_table,
            scope_prefix: scope_prefix.to_string(),
        }
    }
}

impl Visitor for ScopeRenamer<'_> {
    fn exit_component_reference(&mut self, node: &mut ir::ast::ComponentReference) {
        let name = node.to_string();
        // Only prepend scope if not a global symbol
        if !self.symbol_table.is_global(&name) {
            node.parts.insert(
                0,
                ir::ast::ComponentRefPart {
                    ident: ir::ast::Token {
                        text: self.scope_prefix.clone(),
                        ..Default::default()
                    },
                    subs: None,
                },
            );
        }
    }
}

/// Recursively resolves a class definition by processing all extends clauses.
///
/// This function takes a class and resolves all inheritance by copying components
/// and equations from parent classes into the returned class definition.
fn resolve_class(
    class: &ir::ast::ClassDefinition,
    class_dict: &IndexMap<String, ir::ast::ClassDefinition>,
) -> Result<ir::ast::ClassDefinition> {
    let mut resolved = class.clone();

    // Process all extends clauses
    for extend in &class.extends {
        let parent_name = extend.comp.to_string();

        // Skip primitive types
        if is_primitive_type(&parent_name) {
            continue;
        }

        // Get the parent class
        let parent_class = class_dict
            .get(&parent_name)
            .ok_or_else(|| IrError::ExtendClassNotFound(parent_name.clone()))?;

        // Recursively resolve the parent class first
        let resolved_parent = resolve_class(parent_class, class_dict)?;

        // Add parent's components (insert at the beginning to maintain proper order)
        for (comp_name, comp) in resolved_parent.components.iter().rev() {
            if !resolved.components.contains_key(comp_name) {
                resolved.components.insert(comp_name.clone(), comp.clone());
                resolved
                    .components
                    .move_index(resolved.components.len() - 1, 0);
            }
        }

        // Add parent's equations at the beginning
        let mut new_equations = resolved_parent.equations.clone();
        new_equations.append(&mut resolved.equations);
        resolved.equations = new_equations;
    }

    Ok(resolved)
}

/// Creates a component reference from a flattened name like "R1_p_v"
/// The name stays as a single identifier (no dots) since flattened names use underscores
fn make_comp_ref(name: &str) -> ComponentReference {
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

/// Expands connect equations into simple equations.
///
/// Connect equations in Modelica follow these rules:
/// - For non-flow variables: equality (a.v = b.v)
/// - For flow variables: sum at each node = 0 (a.i + b.i + ... = 0)
///
/// This function:
/// 1. Collects all connections and builds a graph of connected nodes
/// 2. For each connection set, generates equality equations for non-flow vars
/// 3. For flow variables, generates a single sum=0 equation per connection set
fn expand_connect_equations(
    fclass: &mut ir::ast::ClassDefinition,
    class_dict: &IndexMap<String, ir::ast::ClassDefinition>,
    pin_types: &IndexMap<String, String>,
) -> Result<()> {
    // Use Union-Find to group connected pins
    let mut connection_sets: IndexMap<String, IndexSet<String>> = IndexMap::new();

    // Extract connect equations and collect connected pins
    let mut connect_eqs: Vec<(ComponentReference, ComponentReference)> = Vec::new();
    let mut other_eqs: Vec<Equation> = Vec::new();

    for eq in &fclass.equations {
        match eq {
            Equation::Connect { lhs, rhs } => {
                connect_eqs.push((lhs.clone(), rhs.clone()));
            }
            _ => other_eqs.push(eq.clone()),
        }
    }

    // If no connect equations, nothing to do
    if connect_eqs.is_empty() {
        return Ok(());
    }

    // Build connection sets using a simple union-find approach
    // Each pin is represented as "component_subcomponent" (e.g., "R1_p")
    let mut parent: IndexMap<String, String> = IndexMap::new();

    fn find(parent: &mut IndexMap<String, String>, x: &str) -> String {
        if !parent.contains_key(x) {
            parent.insert(x.to_string(), x.to_string());
            return x.to_string();
        }
        let p = parent.get(x).unwrap().clone();
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
        let lhs_name = lhs.to_string().replace('.', "_");
        let rhs_name = rhs.to_string().replace('.', "_");
        union(&mut parent, &lhs_name, &rhs_name);
    }

    // Group all pins by their root
    for (lhs, rhs) in &connect_eqs {
        let lhs_name = lhs.to_string().replace('.', "_");
        let rhs_name = rhs.to_string().replace('.', "_");

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
        let first_pin = pins_vec[0];
        if let Some(connector_type) = pin_types.get(first_pin) {
            if let Some(connector_class) = class_dict.get(connector_type) {
                generate_connection_equations(&pins_vec, connector_class, &mut new_equations);
            }
        }
    }

    // Replace equations
    fclass.equations = other_eqs;
    fclass.equations.extend(new_equations);

    Ok(())
}

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
                .map(|pin| format!("{}_{}", pin, var_name))
                .collect();
            equations.push(make_sum_eq(&flow_vars));
        } else {
            // Non-flow (potential) variable: all equal to first
            let first_var = format!("{}_{}", pins[0], var_name);
            for pin in pins.iter().skip(1) {
                let other_var = format!("{}_{}", pin, var_name);
                equations.push(make_simple_eq(&first_var, &other_var));
            }
        }
    }
}

/// Recursively expands a single component, adding its subcomponents and equations to the flat class.
///
/// This function handles:
/// 1. Looking up the component's class definition
/// 2. Adding equations with properly scoped variable references
/// 3. Recursively expanding any nested components that are also class types
/// 4. Recording connector types for later connect equation expansion
///
/// Type aliases (classes that extend primitives but have no components, like `Voltage extends Real`)
/// are not expanded - the component is kept as-is since it acts like a primitive.
fn expand_component(
    fclass: &mut ir::ast::ClassDefinition,
    comp_name: &str,
    comp: &ir::ast::Component,
    class_dict: &IndexMap<String, ir::ast::ClassDefinition>,
    symbol_table: &SymbolTable,
    pin_types: &mut IndexMap<String, String>,
) -> Result<()> {
    let type_name = comp.type_name.to_string();

    // Skip if not a class type (primitive type)
    let comp_class_raw = match class_dict.get(&type_name) {
        Some(c) => c,
        None => return Ok(()), // Primitive type, nothing to expand
    };

    // Resolve the component class (handle its extends clauses)
    let comp_class = resolve_class(comp_class_raw, class_dict)?;

    // If the resolved class has no components, it's effectively a type alias (like Voltage = Real)
    // Don't remove the component, just add any equations it might have
    if comp_class.components.is_empty() {
        // Still add any equations from the type alias (though rare)
        let mut renamer = ScopeRenamer::new(symbol_table, comp_name);
        for eq in &comp_class.equations {
            let mut feq = eq.clone();
            feq.accept(&mut renamer);
            fclass.equations.push(feq);
        }
        return Ok(());
    }

    // Record the connector type for this component (before it gets expanded)
    // This is used later for connect equation expansion
    pin_types.insert(comp_name.to_string(), type_name.clone());

    // Create a scope renamer for this component
    let mut renamer = ScopeRenamer::new(symbol_table, comp_name);

    // Add equations from component class, with scoped variable references
    for eq in &comp_class.equations {
        let mut feq = eq.clone();
        feq.accept(&mut renamer);
        fclass.equations.push(feq);
    }

    // Expand comp.sub_comp names to use underscores in existing equations
    fclass.accept(&mut SubCompNamer {
        comp: comp_name.to_string(),
    });

    // Add subcomponents from component class to flat class
    // We need to collect them first to avoid borrow issues during recursion
    let subcomponents: Vec<(String, ir::ast::Component)> = comp_class
        .components
        .iter()
        .map(|(subcomp_name, subcomp)| {
            let mut scomp = subcomp.clone();
            let name = format!("{}_{}", comp_name, subcomp_name);
            scomp.name = name.clone();
            (name, scomp)
        })
        .collect();

    // Insert all subcomponents
    for (name, scomp) in &subcomponents {
        fclass.components.insert(name.clone(), scomp.clone());
    }

    // Remove the parent component from flat class (it's been expanded into subcomponents)
    fclass.components.swap_remove(comp_name);

    // Recursively expand any subcomponents that are also class types
    for (subcomp_name, subcomp) in &subcomponents {
        if class_dict.contains_key(&subcomp.type_name.to_string()) {
            expand_component(
                fclass,
                subcomp_name,
                subcomp,
                class_dict,
                symbol_table,
                pin_types,
            )?;
        }
    }

    Ok(())
}

/// Flattens a hierarchical Modelica class definition into a single flat class.
///
/// This function takes a stored definition containing one or more class definitions
/// and produces a single flattened class where all hierarchical components have been
/// expanded into a flat namespace. The process involves:
///
/// - Extracting the main class (specified by name, or first in the definition if None)
/// - Processing extend clauses to inherit components and equations
/// - Expanding components that reference other classes by:
///   - Flattening nested component names with underscores (e.g., `comp.subcomp` → `comp_subcomp`)
///   - Adding scoped prefixes to equation references
///   - Removing the parent component and adding all subcomponents directly
///
/// # Arguments
///
/// * `def` - A stored definition containing the class hierarchy to flatten
/// * `model_name` - Optional name of the main class to flatten. If None, uses the first class.
///
/// # Returns
///
/// * `Result<ClassDefinition>` - The flattened class definition on success
///
/// # Errors
///
/// Returns an error if:
/// - The main class is not found in the stored definition
/// - A referenced extend class is not found
///
/// # Example
///
/// Given a hierarchical class with subcomponents:
/// ```text
/// class Main
///   SubClass comp;
/// end Main;
///
/// class SubClass
///   Real x;
///   Real y;
/// end SubClass;
/// ```
///
/// This function produces a flat class:
/// ```text
/// class Main
///   Real comp_x;
///   Real comp_y;
/// end Main;
/// ```
///
/// # Package Support
///
/// This function also supports models inside packages. Use dotted paths
/// like "Package.Model" to reference nested models.
// Internal helper: Recursively builds a class dictionary with full path names.
//
// This function traverses the class hierarchy and adds all classes
// to the dictionary with their fully qualified names (e.g., "Package.SubPackage.Model").
fn build_class_dict(
    class: &ir::ast::ClassDefinition,
    prefix: &str,
    dict: &mut IndexMap<String, ir::ast::ClassDefinition>,
) {
    // Add the class itself with its full path
    let full_name = if prefix.is_empty() {
        class.name.text.clone()
    } else {
        format!("{}.{}", prefix, class.name.text)
    };
    dict.insert(full_name.clone(), class.clone());

    // Recursively add nested classes
    for (_name, nested_class) in &class.classes {
        build_class_dict(nested_class, &full_name, dict);
    }
}

// Internal helper: Looks up a class by path in the stored definition.
//
// Supports both simple names (e.g., "Model") and dotted paths (e.g., "Package.Model").
fn lookup_class(
    def: &ir::ast::StoredDefinition,
    class_dict: &IndexMap<String, ir::ast::ClassDefinition>,
    path: &str,
) -> Option<ir::ast::ClassDefinition> {
    // First try a direct lookup in the class dictionary
    if let Some(class) = class_dict.get(path) {
        return Some(class.clone());
    }

    // Try to navigate the path manually for backwards compatibility
    let parts: Vec<&str> = path.split('.').collect();
    if parts.is_empty() {
        return None;
    }

    // Start from the root class
    let root = def.class_list.get(parts[0])?;
    if parts.len() == 1 {
        return Some(root.clone());
    }

    // Navigate through nested classes
    let mut current = root;
    for part in &parts[1..] {
        current = current.classes.get(*part)?;
    }
    Some(current.clone())
}

pub fn flatten(
    def: &ir::ast::StoredDefinition,
    model_name: Option<&str>,
) -> Result<ir::ast::ClassDefinition> {
    // Build class dictionary from all class definitions (including nested classes)
    let mut class_dict = IndexMap::new();
    for (_class_name, class) in &def.class_list {
        build_class_dict(class, "", &mut class_dict);
    }

    // Determine main class name - model name is required
    let main_class_name = model_name.ok_or(IrError::ModelNameRequired)?.to_string();

    // Get main class (supports dotted paths like "Package.Model")
    let main_class =
        lookup_class(def, &class_dict, &main_class_name).ok_or(IrError::MainClassNotFound)?;

    // Resolve the main class (process extends clauses recursively)
    let resolved_main = resolve_class(&main_class, &class_dict)?;

    // Create the flat class starting from resolved main
    let mut fclass = resolved_main.clone();

    // Create symbol table for tracking variable scopes
    let symbol_table = SymbolTable::new();

    // Track connector types for connect equation expansion
    // Maps flattened pin names (e.g., "R1_p") to their connector type (e.g., "Pin")
    let mut pin_types: IndexMap<String, String> = IndexMap::new();

    // Collect component names that need expansion (to avoid borrow issues)
    let components_to_expand: Vec<(String, ir::ast::Component)> = resolved_main
        .components
        .iter()
        .filter(|(_, comp)| class_dict.contains_key(&comp.type_name.to_string()))
        .map(|(name, comp)| (name.clone(), comp.clone()))
        .collect();

    // Recursively expand each component that references a class
    for (comp_name, comp) in &components_to_expand {
        expand_component(
            &mut fclass,
            comp_name,
            comp,
            &class_dict,
            &symbol_table,
            &mut pin_types,
        )?;
    }

    // Expand connect equations into simple equations
    expand_connect_equations(&mut fclass, &class_dict, &pin_types)?;

    Ok(fclass)
}
