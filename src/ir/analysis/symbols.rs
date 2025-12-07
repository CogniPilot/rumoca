//! Symbol collection and analysis for Modelica code.
//!
//! This module provides unified symbol collection and analysis functionality
//! used by linting, diagnostics, and semantic analysis.

use std::collections::{HashMap, HashSet};

use crate::ir::ast::{
    Causality, ClassDefinition, ClassType, Component, ComponentReference, Equation, Expression,
    Statement, Variability,
};

/// Information about a defined symbol for analysis.
///
/// This struct captures all relevant information about a declared symbol
/// (variable, parameter, constant, or nested class) for use in semantic analysis.
#[derive(Clone, Debug)]
pub struct DefinedSymbol {
    /// Source line number (1-based)
    pub line: u32,
    /// Source column number (1-based)
    pub col: u32,
    /// Whether this symbol is a parameter
    pub is_parameter: bool,
    /// Whether this symbol is a constant
    pub is_constant: bool,
    /// Whether this symbol is a class (type, function, etc.)
    pub is_class: bool,
    /// Whether this symbol has a default/start value
    pub has_default: bool,
    /// The type name (Real, Integer, Boolean, String, or user-defined)
    pub type_name: String,
    /// Array dimensions (empty for scalars)
    pub shape: Vec<usize>,
    /// For functions: the return type (output variable type and shape)
    /// None for non-functions
    pub function_return: Option<(String, Vec<usize>)>,
}

impl DefinedSymbol {
    /// Create a new symbol for a component declaration
    pub fn from_component(name: &str, comp: &Component) -> (String, Self) {
        let line = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_line)
            .unwrap_or(1);
        let col = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_column)
            .unwrap_or(1);

        let has_start = !matches!(comp.start, Expression::Empty);
        let is_parameter = matches!(comp.variability, Variability::Parameter(_));
        let is_constant = matches!(comp.variability, Variability::Constant(_));
        let type_name = comp.type_name.to_string();

        (
            name.to_string(),
            Self {
                line,
                col,
                is_parameter,
                is_constant,
                is_class: false,
                has_default: has_start,
                type_name,
                shape: comp.shape.clone(),
                function_return: None,
            },
        )
    }

    /// Create a new symbol for a nested class/function
    pub fn from_class(name: &str, class: &ClassDefinition) -> (String, Self) {
        // For functions, extract return type from output components
        let function_return = if matches!(class.class_type, ClassType::Function) {
            class
                .components
                .values()
                .find(|c| matches!(c.causality, Causality::Output(_)))
                .map(|output| (output.type_name.to_string(), output.shape.clone()))
        } else {
            None
        };

        (
            name.to_string(),
            Self {
                line: class.name.location.start_line,
                col: class.name.location.start_column,
                is_parameter: false,
                is_constant: false,
                is_class: true,
                has_default: true,
                type_name: name.to_string(),
                shape: vec![],
                function_return,
            },
        )
    }

    /// Create a symbol for a loop index variable
    pub fn loop_index(line: u32, col: u32) -> Self {
        Self {
            line,
            col,
            is_parameter: false,
            is_constant: false,
            is_class: false,
            has_default: true,
            type_name: "Integer".to_string(),
            shape: vec![],
            function_return: None,
        }
    }
}

/// Check if a type name represents a class instance (not a primitive type).
///
/// Returns `false` for built-in types like Real, Integer, Boolean, String,
/// and special types like StateSelect and ExternalObject.
pub fn is_class_instance_type(type_name: &str) -> bool {
    !matches!(
        type_name,
        "Real" | "Integer" | "Boolean" | "String" | "StateSelect" | "ExternalObject"
    )
}

/// Collect all defined symbols in a class.
///
/// This includes:
/// - Component declarations (variables, parameters, constants)
/// - Nested class definitions (functions, models, etc.)
pub fn collect_defined_symbols(class: &ClassDefinition) -> HashMap<String, DefinedSymbol> {
    let mut defined = HashMap::new();

    // Collect component declarations
    for (name, comp) in &class.components {
        let (sym_name, symbol) = DefinedSymbol::from_component(name, comp);
        defined.insert(sym_name, symbol);
    }

    // Collect nested class definitions
    for (name, nested_class) in &class.classes {
        let (sym_name, symbol) = DefinedSymbol::from_class(name, nested_class);
        defined.insert(sym_name, symbol);
    }

    defined
}

/// Collect all symbols used in a class (referenced in expressions, equations, etc.)
pub fn collect_used_symbols(class: &ClassDefinition) -> HashSet<String> {
    let mut used = HashSet::new();

    // From component start expressions
    for comp in class.components.values() {
        collect_expr_symbols(&comp.start, &mut used);
    }

    // From equations
    for eq in &class.equations {
        collect_equation_symbols(eq, &mut used);
    }

    // From initial equations
    for eq in &class.initial_equations {
        collect_equation_symbols(eq, &mut used);
    }

    // From algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            collect_statement_symbols(stmt, &mut used);
        }
    }

    // From initial algorithms
    for algo in &class.initial_algorithms {
        for stmt in algo {
            collect_statement_symbols(stmt, &mut used);
        }
    }

    used
}

/// Collect symbols from an expression
pub fn collect_expr_symbols(expr: &Expression, used: &mut HashSet<String>) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            collect_comp_ref_symbols(comp_ref, used);
        }
        Expression::Terminal { .. } => {}
        Expression::FunctionCall { comp, args } => {
            collect_comp_ref_symbols(comp, used);
            for arg in args {
                collect_expr_symbols(arg, used);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_expr_symbols(lhs, used);
            collect_expr_symbols(rhs, used);
        }
        Expression::Unary { rhs, .. } => {
            collect_expr_symbols(rhs, used);
        }
        Expression::Array { elements } => {
            for elem in elements {
                collect_expr_symbols(elem, used);
            }
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                collect_expr_symbols(elem, used);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_expr_symbols(cond, used);
                collect_expr_symbols(then_expr, used);
            }
            collect_expr_symbols(else_branch, used);
        }
        Expression::Range { start, step, end } => {
            collect_expr_symbols(start, used);
            if let Some(s) = step {
                collect_expr_symbols(s, used);
            }
            collect_expr_symbols(end, used);
        }
        Expression::Parenthesized { inner } => {
            collect_expr_symbols(inner, used);
        }
        Expression::ArrayComprehension { expr, indices } => {
            collect_expr_symbols(expr, used);
            for idx in indices {
                collect_expr_symbols(&idx.range, used);
            }
        }
    }
}

/// Collect symbols from an equation
pub fn collect_equation_symbols(eq: &Equation, used: &mut HashSet<String>) {
    match eq {
        Equation::Empty => {}
        Equation::Simple { lhs, rhs } => {
            collect_expr_symbols(lhs, used);
            collect_expr_symbols(rhs, used);
        }
        Equation::Connect { lhs, rhs } => {
            collect_comp_ref_symbols(lhs, used);
            collect_comp_ref_symbols(rhs, used);
        }
        Equation::For { indices, equations } => {
            for index in indices {
                collect_expr_symbols(&index.range, used);
            }
            for sub_eq in equations {
                collect_equation_symbols(sub_eq, used);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                collect_expr_symbols(&block.cond, used);
                for sub_eq in &block.eqs {
                    collect_equation_symbols(sub_eq, used);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_expr_symbols(&block.cond, used);
                for sub_eq in &block.eqs {
                    collect_equation_symbols(sub_eq, used);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    collect_equation_symbols(sub_eq, used);
                }
            }
        }
        Equation::FunctionCall { comp, args } => {
            collect_comp_ref_symbols(comp, used);
            for arg in args {
                collect_expr_symbols(arg, used);
            }
        }
    }
}

/// Collect symbols from a statement
pub fn collect_statement_symbols(stmt: &Statement, used: &mut HashSet<String>) {
    match stmt {
        Statement::Empty => {}
        Statement::Assignment { comp, value } => {
            collect_comp_ref_symbols(comp, used);
            collect_expr_symbols(value, used);
        }
        Statement::FunctionCall { comp, args } => {
            collect_comp_ref_symbols(comp, used);
            for arg in args {
                collect_expr_symbols(arg, used);
            }
        }
        Statement::For { indices, equations } => {
            for index in indices {
                collect_expr_symbols(&index.range, used);
            }
            for sub_stmt in equations {
                collect_statement_symbols(sub_stmt, used);
            }
        }
        Statement::While(block) => {
            collect_expr_symbols(&block.cond, used);
            for sub_stmt in &block.stmts {
                collect_statement_symbols(sub_stmt, used);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_expr_symbols(&block.cond, used);
                for sub_stmt in &block.stmts {
                    collect_statement_symbols(sub_stmt, used);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    collect_statement_symbols(sub_stmt, used);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                collect_expr_symbols(&block.cond, used);
                for sub_stmt in &block.stmts {
                    collect_statement_symbols(sub_stmt, used);
                }
            }
        }
        Statement::Return { .. } | Statement::Break { .. } => {}
    }
}

/// Collect the first identifier from a component reference
pub fn collect_comp_ref_symbols(comp_ref: &ComponentReference, used: &mut HashSet<String>) {
    if let Some(first) = comp_ref.parts.first() {
        used.insert(first.ident.text.clone());
    }
}
