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
use crate::ir::analysis::symbol_table::SymbolTable;
use crate::ir::ast::{
    ClassDefinition, ComponentRefPart, ComponentReference, Connection, Equation, Expression,
    Import, OpBinary, TerminalType, Token,
};
use crate::ir::error::IrError;
use crate::ir::transform::constants::is_primitive_type;
use crate::ir::transform::sub_comp_namer::SubCompNamer;
use crate::ir::visitor::{MutVisitable, MutVisitor};
use anyhow::Result;
use indexmap::{IndexMap, IndexSet};

/// Extract name=value pairs from extends clause modifications.
///
/// Extends modifications are stored as a Vec<Expression> containing Binary expressions
/// like `L = 1e-3` which are `Binary { op: Eq, lhs: ComponentReference("L"), rhs: value }`.
///
/// This function extracts these into an IndexMap for easy lookup.
fn extract_extends_modifications(modifications: &[Expression]) -> IndexMap<String, Expression> {
    let mut result = IndexMap::new();

    for expr in modifications {
        if let Expression::Binary { op, lhs, rhs } = expr {
            if matches!(op, OpBinary::Eq(_)) {
                if let Expression::ComponentReference(comp_ref) = &**lhs {
                    let param_name = comp_ref.to_string();
                    result.insert(param_name, (**rhs).clone());
                }
            }
        }
    }

    result
}

/// Builds a map of import aliases from a class's imports.
///
/// For renamed imports like `import D = Modelica.Electrical.Digital;`,
/// this creates an entry mapping "D" -> "Modelica.Electrical.Digital".
///
/// For qualified imports like `import Modelica.Electrical.Digital;`,
/// this creates an entry mapping "Digital" -> "Modelica.Electrical.Digital".
fn build_import_aliases(imports: &[Import]) -> IndexMap<String, String> {
    let mut aliases = IndexMap::new();

    for import in imports {
        match import {
            Import::Renamed { alias, path, .. } => {
                // import D = A.B.C; => D -> A.B.C
                aliases.insert(alias.text.clone(), path.to_string());
            }
            Import::Qualified { path, .. } => {
                // import A.B.C; => C -> A.B.C
                if let Some(last) = path.name.last() {
                    aliases.insert(last.text.clone(), path.to_string());
                }
            }
            Import::Unqualified { .. } => {
                // import A.B.*; - handled differently (need to check all classes in A.B)
                // For now, skip as this requires checking the class dictionary
            }
            Import::Selective { path, names, .. } => {
                // import A.B.{C, D}; => C -> A.B.C, D -> A.B.D
                let base_path = path.to_string();
                for name in names {
                    aliases.insert(name.text.clone(), format!("{}.{}", base_path, name.text));
                }
            }
        }
    }

    aliases
}

/// Builds a combined import alias map from a class and all its enclosing scopes.
///
/// This collects imports from the class itself and all parent packages up to the root.
fn build_import_aliases_for_class(
    class_path: &str,
    class_dict: &IndexMap<String, ClassDefinition>,
) -> IndexMap<String, String> {
    let mut all_aliases = IndexMap::new();

    // Collect imports from each level of the class hierarchy (most specific first)
    let parts: Vec<&str> = class_path.split('.').collect();
    for i in (1..=parts.len()).rev() {
        let path = parts[..i].join(".");
        if let Some(class) = class_dict.get(&path) {
            let aliases = build_import_aliases(&class.imports);
            // Earlier (more specific) imports take precedence
            for (alias, target) in aliases {
                all_aliases.entry(alias).or_insert(target);
            }
        }
    }

    all_aliases
}

/// Applies import aliases to a type name.
///
/// If the first part of the name is an import alias, replace it with the full path.
/// For example, with alias "D" -> "Modelica.Electrical.Digital":
/// - "D.Basic.Nor" becomes "Modelica.Electrical.Digital.Basic.Nor"
/// - "Real" stays "Real" (no alias)
fn apply_import_aliases(name: &str, aliases: &IndexMap<String, String>) -> String {
    let parts: Vec<&str> = name.split('.').collect();
    if parts.is_empty() {
        return name.to_string();
    }

    let first = parts[0];
    if let Some(target) = aliases.get(first) {
        // Replace the first part with the full path
        if parts.len() == 1 {
            target.clone()
        } else {
            format!("{}.{}", target, parts[1..].join("."))
        }
    } else {
        name.to_string()
    }
}

/// Resolves a class name by searching the enclosing scope hierarchy.
///
/// This function implements Modelica's name lookup rules for extends clauses:
/// 1. First applies import aliases to resolve aliased names
/// 2. Then tries an exact match (for fully qualified names)
/// 3. Then tries prepending enclosing package prefixes from most specific to least
///
/// For example, if `current_class_path` is `Modelica.Blocks.Continuous.Derivative`
/// and `name` is `Interfaces.SISO`, it will try:
/// - `Interfaces.SISO` (exact match)
/// - `Modelica.Blocks.Continuous.Interfaces.SISO`
/// - `Modelica.Blocks.Interfaces.SISO` (found!)
/// - `Modelica.Interfaces.SISO`
/// - `Interfaces.SISO` (at root level)
fn resolve_class_name(
    name: &str,
    current_class_path: &str,
    class_dict: &IndexMap<String, ClassDefinition>,
) -> Option<String> {
    resolve_class_name_with_imports(name, current_class_path, class_dict, &IndexMap::new())
}

/// Resolves a class name with import alias support.
fn resolve_class_name_with_imports(
    name: &str,
    current_class_path: &str,
    class_dict: &IndexMap<String, ClassDefinition>,
    import_aliases: &IndexMap<String, String>,
) -> Option<String> {
    // 0. Apply import aliases first
    let resolved_name = apply_import_aliases(name, import_aliases);

    // 1. Try exact match first (handles fully qualified names)
    if class_dict.contains_key(&resolved_name) {
        return Some(resolved_name);
    }

    // 2. Try prepending enclosing package prefixes
    let parts: Vec<&str> = current_class_path.split('.').collect();
    for i in (0..parts.len()).rev() {
        let prefix = parts[..i].join(".");
        let candidate = if prefix.is_empty() {
            resolved_name.clone()
        } else {
            format!("{}.{}", prefix, resolved_name)
        };
        if class_dict.contains_key(&candidate) {
            return Some(candidate);
        }
    }

    // 3. If import alias resolution changed the name but still not found,
    //    try with the original name too
    if resolved_name != name {
        if class_dict.contains_key(name) {
            return Some(name.to_string());
        }
        for i in (0..parts.len()).rev() {
            let prefix = parts[..i].join(".");
            let candidate = if prefix.is_empty() {
                name.to_string()
            } else {
                format!("{}.{}", prefix, name)
            };
            if class_dict.contains_key(&candidate) {
                return Some(candidate);
            }
        }
    }

    None
}

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

impl MutVisitor for ScopeRenamer<'_> {
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
///
/// # Arguments
///
/// * `class` - The class definition to resolve
/// * `current_class_path` - The fully qualified path of the current class (for scope lookup)
/// * `class_dict` - Dictionary of all available classes
fn resolve_class(
    class: &ir::ast::ClassDefinition,
    current_class_path: &str,
    class_dict: &IndexMap<String, ir::ast::ClassDefinition>,
) -> Result<ir::ast::ClassDefinition> {
    // Use the internal function with empty visited set for cycle detection
    let mut visited = IndexSet::new();
    resolve_class_internal(class, current_class_path, class_dict, &mut visited)
}

/// Internal implementation of resolve_class with cycle detection.
fn resolve_class_internal(
    class: &ir::ast::ClassDefinition,
    current_class_path: &str,
    class_dict: &IndexMap<String, ir::ast::ClassDefinition>,
    visited: &mut IndexSet<String>,
) -> Result<ir::ast::ClassDefinition> {
    // Check for cycles
    if visited.contains(current_class_path) {
        // Already resolving this class - skip to avoid infinite recursion
        return Ok(class.clone());
    }
    visited.insert(current_class_path.to_string());

    let mut resolved = class.clone();

    // Build import aliases for this class
    let import_aliases = build_import_aliases_for_class(current_class_path, class_dict);

    // Process all extends clauses
    for extend in &class.extends {
        let parent_name = extend.comp.to_string();

        // Skip primitive types
        if is_primitive_type(&parent_name) {
            continue;
        }

        // Resolve the parent class name using enclosing scope search with import aliases
        let resolved_name = match resolve_class_name_with_imports(
            &parent_name,
            current_class_path,
            class_dict,
            &import_aliases,
        ) {
            Some(name) => name,
            None => continue, // Skip unresolved extends (might be external dependency)
        };

        // Skip if this would create a cycle
        if visited.contains(&resolved_name) {
            continue;
        }

        // Get the parent class
        let parent_class = match class_dict.get(&resolved_name) {
            Some(c) => c,
            None => continue, // Skip missing classes
        };

        // Recursively resolve the parent class first (using resolved name as new context)
        let resolved_parent =
            resolve_class_internal(parent_class, &resolved_name, class_dict, visited)?;

        // Extract modifications from the extends clause (e.g., extends Foo(L=1e-3))
        let extends_mods = extract_extends_modifications(&extend.modifications);

        // Add parent's components (insert at the beginning to maintain proper order)
        for (comp_name, comp) in resolved_parent.components.iter().rev() {
            if !resolved.components.contains_key(comp_name) {
                let mut modified_comp = comp.clone();

                // Apply extends modifications to inherited components
                if let Some(mod_value) = extends_mods.get(comp_name) {
                    modified_comp.start = mod_value.clone();
                    modified_comp.start_is_modification = true;
                }

                resolved.components.insert(comp_name.clone(), modified_comp);
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

    // Apply causality from type definitions to components
    // e.g., if a component has type RealInput which is defined as "connector RealInput = input Real"
    // then the component should have Input causality
    apply_type_causality(&mut resolved, current_class_path, class_dict);

    Ok(resolved)
}

/// Apply causality from type definitions to components whose causality is Empty
/// This handles type aliases like "connector RealInput = input Real"
fn apply_type_causality(
    class: &mut ir::ast::ClassDefinition,
    current_class_path: &str,
    class_dict: &IndexMap<String, ir::ast::ClassDefinition>,
) {
    use crate::ir::ast::Causality;

    // Build import aliases for this class
    let import_aliases = build_import_aliases_for_class(current_class_path, class_dict);

    for (_comp_name, comp) in class.components.iter_mut() {
        // Only apply if component's causality is empty (not explicitly set)
        if !matches!(comp.causality, Causality::Empty) {
            continue;
        }

        let type_name = comp.type_name.to_string();

        // Resolve the type name using enclosing scope search with import aliases
        let resolved_type_name = resolve_class_name_with_imports(
            &type_name,
            current_class_path,
            class_dict,
            &import_aliases,
        );

        if let Some(resolved_name) = resolved_type_name {
            if let Some(type_class) = class_dict.get(&resolved_name) {
                // If the type has causality (from base_prefix), apply it to the component
                if !matches!(type_class.causality, Causality::Empty) {
                    comp.causality = type_class.causality.clone();
                }
            }
        }
    }
}

/// Creates a component reference from a flattened name like "R1.p.v"
/// The name stays as a single identifier since flattened names use dots
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

/// Recursively extract connect equations from an equation, including nested For/If/When.
/// Returns the connect equations found and the equation with connect equations removed.
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
            let mut filtered_eqs = Vec::new();
            for inner_eq in equations {
                if let Some(filtered) = extract_connect_equations_recursive(inner_eq, connect_eqs) {
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
        Equation::If {
            cond_blocks,
            else_block,
        } => {
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
                    if let Some(f) = extract_connect_equations_recursive(inner_eq, connect_eqs) {
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

/// Expands connect equations into simple equations.
///
/// Connect equations in Modelica follow these rules:
/// - For non-flow variables: equality (a.v = b.v)
/// - For flow variables: sum at each node = 0 (a.i + b.i + ... = 0)
///
/// This function:
/// 1. Collects all connections (including from nested For/If/When) and builds a graph
/// 2. For each connection set, generates equality equations for non-flow vars
/// 3. For flow variables, generates a single sum=0 equation per connection set
fn expand_connect_equations(
    fclass: &mut ir::ast::ClassDefinition,
    class_dict: &IndexMap<String, ir::ast::ClassDefinition>,
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

    // If no connect equations, nothing to do
    if connect_eqs.is_empty() {
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
                .map(|pin| format!("{}.{}", pin, var_name))
                .collect();
            equations.push(make_sum_eq(&flow_vars));
        } else {
            // Non-flow (potential) variable: all equal to first
            let first_var = format!("{}.{}", pins[0], var_name);
            for pin in pins.iter().skip(1) {
                let other_var = format!("{}.{}", pin, var_name);
                equations.push(make_simple_eq(&first_var, &other_var));
            }
        }
    }
}

/// Track inner components for inner/outer resolution.
/// Maps (type_name, component_name) -> flattened component name
/// For example, ("World", "world") -> "world" means there's an inner World world at the top level
type InnerMap = IndexMap<(String, String), String>;

/// Visitor that renames outer component references to point to their inner counterparts.
/// For example, if "child.world" is outer and maps to inner "world",
/// then "child.world.g" gets renamed to "world.g".
#[derive(Debug, Clone, Default)]
struct OuterRenamer {
    /// Maps outer component path prefix to inner component path
    /// e.g., "child.world" -> "world"
    outer_to_inner: IndexMap<String, String>,
}

impl OuterRenamer {
    fn add_mapping(&mut self, outer_path: &str, inner_path: &str) {
        self.outer_to_inner
            .insert(outer_path.to_string(), inner_path.to_string());
    }
}

impl MutVisitor for OuterRenamer {
    fn exit_component_reference(&mut self, node: &mut ir::ast::ComponentReference) {
        let ref_name = node.to_string();

        // Check if this reference starts with an outer path that needs renaming
        for (outer_path, inner_path) in &self.outer_to_inner {
            if ref_name == *outer_path {
                // Exact match - replace entire reference
                node.parts = vec![ComponentRefPart {
                    ident: Token {
                        text: inner_path.clone(),
                        ..Default::default()
                    },
                    subs: None,
                }];
                return;
            } else if ref_name.starts_with(&format!("{}.", outer_path)) {
                // Reference to subcomponent of outer - replace prefix
                let suffix = &ref_name[outer_path.len()..]; // includes the leading "."
                let new_ref = format!("{}{}", inner_path, suffix);
                node.parts = vec![ComponentRefPart {
                    ident: Token {
                        text: new_ref,
                        ..Default::default()
                    },
                    subs: None,
                }];
                return;
            }
        }
    }
}

/// Context for component expansion during flattening.
/// Groups all the mutable state needed during recursive expansion.
struct ExpansionContext<'a> {
    /// The flattened class being built
    fclass: &'a mut ir::ast::ClassDefinition,
    /// Dictionary of all available classes
    class_dict: &'a IndexMap<String, ir::ast::ClassDefinition>,
    /// Symbol table for scope tracking
    symbol_table: &'a SymbolTable,
    /// Maps flattened pin names to their connector types
    pin_types: IndexMap<String, String>,
    /// Maps (type_name, component_name) -> inner component's flattened name
    inner_map: InnerMap,
    /// Tracks outer->inner mappings for equation rewriting
    outer_renamer: OuterRenamer,
}

impl<'a> ExpansionContext<'a> {
    fn new(
        fclass: &'a mut ir::ast::ClassDefinition,
        class_dict: &'a IndexMap<String, ir::ast::ClassDefinition>,
        symbol_table: &'a SymbolTable,
    ) -> Self {
        Self {
            fclass,
            class_dict,
            symbol_table,
            pin_types: IndexMap::new(),
            inner_map: IndexMap::new(),
            outer_renamer: OuterRenamer::default(),
        }
    }

    /// Register top-level inner components
    fn register_inner_components(&mut self, components: &IndexMap<String, ir::ast::Component>) {
        for (comp_name, comp) in components {
            if comp.inner {
                let key = (comp.type_name.to_string(), comp_name.clone());
                self.inner_map.insert(key, comp_name.clone());
            }
        }
    }

    /// Apply outer renaming to all equations
    fn apply_outer_renaming(&mut self) {
        self.fclass.accept_mut(&mut self.outer_renamer);
    }

    /// Expand a component recursively
    fn expand_component(
        &mut self,
        comp_name: &str,
        comp: &ir::ast::Component,
        current_class_path: &str,
    ) -> Result<()> {
        let type_name = comp.type_name.to_string();

        // Build import aliases for the current class path
        let import_aliases = build_import_aliases_for_class(current_class_path, self.class_dict);

        // Resolve the type name using enclosing scope search and import aliases
        let resolved_type_name = match resolve_class_name_with_imports(
            &type_name,
            current_class_path,
            self.class_dict,
            &import_aliases,
        ) {
            Some(name) => name,
            None => return Ok(()), // Primitive type or not found, nothing to expand
        };

        // Get the component class
        let comp_class_raw = match self.class_dict.get(&resolved_type_name) {
            Some(c) => c,
            None => return Ok(()), // Should not happen after resolve_class_name succeeded
        };

        // Resolve the component class (handle its extends clauses)
        let comp_class = resolve_class(comp_class_raw, &resolved_type_name, self.class_dict)?;

        // Record the connector type for this component BEFORE checking if it has sub-components.
        // This is critical for connectors like Pin that have only primitive types (Real v, Real i).
        // These connectors have no class-type sub-components but are still used in connect equations.
        self.pin_types
            .insert(comp_name.to_string(), resolved_type_name.clone());

        // If the resolved class has no components, it's effectively a type alias (like Voltage = Real)
        // or a "leaf" connector with only primitive types.
        // Don't remove the component, just add any equations and algorithms it might have.
        if comp_class.components.is_empty() {
            // Still add any equations from the type alias (though rare)
            let mut renamer = ScopeRenamer::new(self.symbol_table, comp_name);
            for eq in &comp_class.equations {
                let mut feq = eq.clone();
                feq.accept_mut(&mut renamer);
                self.fclass.equations.push(feq);
            }
            // Add algorithm sections from leaf component
            for algo_section in &comp_class.algorithms {
                let mut scoped_section = Vec::new();
                for stmt in algo_section {
                    let mut fstmt = stmt.clone();
                    fstmt.accept_mut(&mut renamer);
                    scoped_section.push(fstmt);
                }
                self.fclass.algorithms.push(scoped_section);
            }
            return Ok(());
        }

        // Create a scope renamer for this component
        let mut renamer = ScopeRenamer::new(self.symbol_table, comp_name);

        // Add equations from component class, with scoped variable references
        for eq in &comp_class.equations {
            let mut feq = eq.clone();
            feq.accept_mut(&mut renamer);
            self.fclass.equations.push(feq);
        }

        // Add algorithm sections from component class, with scoped variable references
        for algo_section in &comp_class.algorithms {
            let mut scoped_section = Vec::new();
            for stmt in algo_section {
                let mut fstmt = stmt.clone();
                fstmt.accept_mut(&mut renamer);
                scoped_section.push(fstmt);
            }
            self.fclass.algorithms.push(scoped_section);
        }

        // Expand comp.sub_comp names to use dots in existing equations
        self.fclass.accept_mut(&mut SubCompNamer {
            comp: comp_name.to_string(),
        });

        // Collect subcomponents, handling inner/outer
        let mut subcomponents: Vec<(String, ir::ast::Component)> = Vec::new();
        for (subcomp_name, subcomp) in &comp_class.components {
            // Handle outer components: they reference an inner component from enclosing scope
            if subcomp.outer {
                let subcomp_type = subcomp.type_name.to_string();
                // Look for matching inner component
                let key = (subcomp_type, subcomp_name.clone());
                if let Some(inner_name) = self.inner_map.get(&key) {
                    // Outer component resolves to inner - don't create a new variable
                    // Record the mapping for equation rewriting
                    let outer_path = format!("{}.{}", comp_name, subcomp_name);
                    self.outer_renamer.add_mapping(&outer_path, inner_name);
                    continue;
                }
                // No matching inner found - could be an error or external dependency
                // For now, create the component anyway
            }

            let mut scomp = subcomp.clone();
            let name = format!("{}.{}", comp_name, subcomp_name);
            scomp.name = name.clone();

            // If this is an inner component, register it
            if subcomp.inner {
                let key = (subcomp.type_name.to_string(), subcomp_name.clone());
                self.inner_map.insert(key, name.clone());
            }

            // Apply modifications from parent component
            if let Some(mod_expr) = comp.modifications.get(subcomp_name) {
                scomp.start = mod_expr.clone();
            }

            subcomponents.push((name, scomp));
        }

        // Insert all subcomponents
        for (name, scomp) in &subcomponents {
            self.fclass.components.insert(name.clone(), scomp.clone());
        }

        // Remove the parent component from flat class (it's been expanded into subcomponents)
        self.fclass.components.swap_remove(comp_name);

        // Recursively expand any subcomponents that are also class types
        // Build import aliases for the resolved component class for subcomponent resolution
        let subcomp_import_aliases =
            build_import_aliases_for_class(&resolved_type_name, self.class_dict);
        for (subcomp_name, subcomp) in &subcomponents {
            // Use resolved_type_name as context for resolving nested component types
            if resolve_class_name_with_imports(
                &subcomp.type_name.to_string(),
                &resolved_type_name,
                self.class_dict,
                &subcomp_import_aliases,
            )
            .is_some()
            {
                self.expand_component(subcomp_name, subcomp, &resolved_type_name)?;
            }
        }

        Ok(())
    }
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
///   - Flattening nested component names with dots (e.g., `comp.subcomp` stays as `comp.subcomp`)
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
///   Real comp.x;
///   Real comp.y;
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
    let resolved_main = resolve_class(&main_class, &main_class_name, &class_dict)?;

    // Create the flat class starting from resolved main
    let mut fclass = resolved_main.clone();

    // Create symbol table for tracking variable scopes
    let symbol_table = SymbolTable::new();

    // Create expansion context
    let mut ctx = ExpansionContext::new(&mut fclass, &class_dict, &symbol_table);

    // Register top-level inner components before expansion
    ctx.register_inner_components(&resolved_main.components);

    // Collect component names that need expansion (to avoid borrow issues)
    // Use scope-aware class resolution to determine which components are class types
    let components_to_expand: Vec<(String, ir::ast::Component)> = resolved_main
        .components
        .iter()
        .filter(|(_, comp)| {
            resolve_class_name(&comp.type_name.to_string(), &main_class_name, &class_dict).is_some()
        })
        .map(|(name, comp)| (name.clone(), comp.clone()))
        .collect();

    // Recursively expand each component that references a class (with inner/outer support)
    for (comp_name, comp) in &components_to_expand {
        ctx.expand_component(comp_name, comp, &main_class_name)?;
    }

    // Rewrite equations to redirect outer references to inner components
    ctx.apply_outer_renaming();

    // Extract pin_types for connect equation expansion
    let pin_types = ctx.pin_types;

    // Expand connect equations into simple equations
    expand_connect_equations(&mut fclass, &class_dict, &pin_types)?;

    Ok(fclass)
}
