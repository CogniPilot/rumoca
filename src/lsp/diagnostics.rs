//! Diagnostics computation for Modelica files.
//!
//! Provides enhanced diagnostics including:
//! - Parse errors
//! - Compilation errors
//! - Undefined variable references
//! - Unused variable warnings
//! - Missing parameter default warnings
//! - Type mismatch detection
//! - Array dimension warnings

use std::collections::{HashMap, HashSet};

use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Uri};
use parol_runtime::{ParolError, ParserError};

use crate::ir::ast::{
    ClassDefinition, ComponentReference, Equation, Expression, OpBinary, Statement, TerminalType,
    Variability,
};
use crate::ir::constants::global_builtins;
use crate::ir::flatten::flatten;

use super::WorkspaceState;

/// Compute diagnostics for a document
pub fn compute_diagnostics(
    uri: &Uri,
    text: &str,
    workspace: &mut WorkspaceState,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    let path = uri.path().as_str();
    if path.ends_with(".mo") {
        use crate::modelica_grammar::ModelicaGrammar;
        use crate::modelica_parser::parse;

        let mut grammar = ModelicaGrammar::new();
        match parse(text, path, &mut grammar) {
            Ok(_) => {
                // Parsing succeeded - clear stale balance cache since document changed
                // Balance will be recomputed on-demand when user clicks "Analyze"
                workspace.clear_balances(uri);

                // Run semantic analysis on each class (using flattening for inherited symbols)
                if let Some(ref ast) = grammar.modelica {
                    for class_name in ast.class_list.keys() {
                        if let Ok(fclass) = flatten(ast, Some(class_name)) {
                            analyze_class(&fclass, &mut diagnostics);
                        }
                    }
                }
            }
            Err(e) => {
                // Clear cached balance on parse error
                workspace.clear_balances(uri);
                // Use structured error extraction when possible
                let (line, col, message) = extract_structured_error(&e, text);
                diagnostics.push(create_diagnostic(
                    line,
                    col,
                    message,
                    DiagnosticSeverity::ERROR,
                ));
            }
        }
    }

    diagnostics
}

/// Analyze a class for semantic issues
fn analyze_class(class: &ClassDefinition, diagnostics: &mut Vec<Diagnostic>) {
    // Build set of defined symbols
    let mut defined: HashMap<String, DefinedSymbol> = HashMap::new();
    let mut used: HashSet<String> = HashSet::new();

    // Add global builtins
    let globals: HashSet<String> = global_builtins().into_iter().collect();

    // Collect component declarations
    for (comp_name, comp) in &class.components {
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
        let type_name = comp.type_name.to_string();

        defined.insert(
            comp_name.clone(),
            DefinedSymbol {
                line,
                col,
                name_len: comp_name.len() as u32,
                is_parameter,
                is_class: false,
                has_default: has_start,
                type_name: type_name.clone(),
                shape: comp.shape.clone(),
            },
        );

        // Check references in start expression
        collect_used_symbols(&comp.start, &mut used);
    }

    // Add nested class names as defined (these are types, not variables)
    for nested_name in class.classes.keys() {
        defined.insert(
            nested_name.clone(),
            DefinedSymbol {
                line: 1,
                col: 1,
                name_len: nested_name.len() as u32,
                is_parameter: false,
                is_class: true,
                has_default: true,
                type_name: nested_name.clone(), // class type
                shape: vec![],
            },
        );
    }

    // Collect symbols used in equations
    for eq in &class.equations {
        collect_equation_symbols(eq, &mut used, diagnostics, &defined, &globals);
    }

    // Collect symbols used in initial equations
    for eq in &class.initial_equations {
        collect_equation_symbols(eq, &mut used, diagnostics, &defined, &globals);
    }

    // Collect symbols used in algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            collect_statement_symbols(stmt, &mut used, diagnostics, &defined, &globals);
        }
    }

    // Collect symbols used in initial algorithms
    for algo in &class.initial_algorithms {
        for stmt in algo {
            collect_statement_symbols(stmt, &mut used, diagnostics, &defined, &globals);
        }
    }

    // Check for unused variables (warning)
    for (name, sym) in &defined {
        if !used.contains(name) && !name.starts_with('_') {
            // Skip parameters, classes, and class instances (submodels)
            // Class instances contribute to the system even without explicit references
            if !sym.is_parameter && !sym.is_class && !is_class_instance_type(&sym.type_name) {
                diagnostics.push(create_diagnostic(
                    sym.line,
                    sym.col,
                    format!("Variable '{}' is declared but never used", name),
                    DiagnosticSeverity::WARNING,
                ));
            }
        }
    }

    // Check for parameters without default values (hint)
    for (name, sym) in &defined {
        if sym.is_parameter && !sym.has_default {
            diagnostics.push(create_diagnostic(
                sym.line,
                sym.col,
                format!(
                    "Parameter '{}' has no default value - consider adding one",
                    name
                ),
                DiagnosticSeverity::HINT,
            ));
        }
    }

    // Recursively analyze nested classes
    for nested_class in class.classes.values() {
        analyze_class(nested_class, diagnostics);
    }
}

/// Information about a defined symbol
#[derive(Clone)]
struct DefinedSymbol {
    line: u32,
    col: u32,
    #[allow(dead_code)]
    name_len: u32,
    is_parameter: bool,
    is_class: bool,
    has_default: bool,
    /// The base type (Real, Integer, Boolean, String)
    type_name: String,
    /// Array dimensions (empty for scalars)
    shape: Vec<usize>,
}

/// Inferred type for an expression
#[derive(Clone, Debug, PartialEq)]
enum InferredType {
    Real,
    Integer,
    Boolean,
    String,
    Array(Box<InferredType>, Option<usize>), // element type, optional size
    Unknown,
}

impl InferredType {
    fn base_type(&self) -> &InferredType {
        match self {
            InferredType::Array(inner, _) => inner.base_type(),
            other => other,
        }
    }

    fn is_numeric(&self) -> bool {
        matches!(self.base_type(), InferredType::Real | InferredType::Integer)
    }

    fn is_compatible_with(&self, other: &InferredType) -> bool {
        match (self, other) {
            (InferredType::Unknown, _) | (_, InferredType::Unknown) => true,
            (InferredType::Real, InferredType::Real) => true,
            (InferredType::Integer, InferredType::Integer) => true,
            (InferredType::Boolean, InferredType::Boolean) => true,
            (InferredType::String, InferredType::String) => true,
            // Real and Integer are compatible (Integer can be promoted to Real)
            (InferredType::Real, InferredType::Integer)
            | (InferredType::Integer, InferredType::Real) => true,
            // Arrays are compatible if element types are compatible
            (InferredType::Array(t1, _), InferredType::Array(t2, _)) => t1.is_compatible_with(t2),
            // Scalar and array are not compatible
            _ => false,
        }
    }
}

impl std::fmt::Display for InferredType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InferredType::Real => write!(f, "Real"),
            InferredType::Integer => write!(f, "Integer"),
            InferredType::Boolean => write!(f, "Boolean"),
            InferredType::String => write!(f, "String"),
            InferredType::Array(inner, size) => {
                if let Some(s) = size {
                    write!(f, "{}[{}]", inner, s)
                } else {
                    write!(f, "{}[:]", inner)
                }
            }
            InferredType::Unknown => write!(f, "Unknown"),
        }
    }
}

fn type_from_name(name: &str) -> InferredType {
    match name {
        "Real" => InferredType::Real,
        "Integer" => InferredType::Integer,
        "Boolean" => InferredType::Boolean,
        "String" => InferredType::String,
        _ => InferredType::Unknown, // User-defined types
    }
}

/// Check if a type name represents a class instance (not a primitive type)
fn is_class_instance_type(type_name: &str) -> bool {
    !matches!(
        type_name,
        "Real" | "Integer" | "Boolean" | "String" | "StateSelect" | "ExternalObject"
    )
}

/// Infer the type of an expression
fn infer_expression_type(
    expr: &Expression,
    defined: &HashMap<String, DefinedSymbol>,
) -> InferredType {
    match expr {
        Expression::Empty => InferredType::Unknown,
        Expression::ComponentReference(comp_ref) => {
            if let Some(first) = comp_ref.parts.first() {
                if let Some(sym) = defined.get(&first.ident.text) {
                    let base = type_from_name(&sym.type_name);
                    if sym.shape.is_empty() {
                        base
                    } else {
                        // Build array type from innermost to outermost
                        let mut result = base;
                        for &dim in sym.shape.iter().rev() {
                            result = InferredType::Array(Box::new(result), Some(dim));
                        }
                        result
                    }
                } else {
                    // Check if it's 'time' (global Real)
                    if first.ident.text == "time" {
                        InferredType::Real
                    } else {
                        InferredType::Unknown
                    }
                }
            } else {
                InferredType::Unknown
            }
        }
        Expression::Terminal {
            terminal_type,
            token: _,
        } => match terminal_type {
            TerminalType::UnsignedInteger => InferredType::Integer,
            TerminalType::UnsignedReal => InferredType::Real,
            TerminalType::String => InferredType::String,
            TerminalType::Bool => InferredType::Boolean,
            _ => InferredType::Unknown,
        },
        Expression::FunctionCall { comp, args } => {
            // Infer return type based on function name
            if let Some(first) = comp.parts.first() {
                match first.ident.text.as_str() {
                    // Trigonometric and math functions return Real
                    "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "atan2" | "sinh"
                    | "cosh" | "tanh" | "exp" | "log" | "log10" | "sqrt" | "abs" | "sign"
                    | "floor" | "ceil" | "mod" | "rem" | "max" | "min" | "sum" | "product" => {
                        InferredType::Real
                    }
                    // der returns the same type as its argument (preserves array dimensions)
                    "der" => {
                        if let Some(arg) = args.first() {
                            infer_expression_type(arg, defined)
                        } else {
                            InferredType::Real
                        }
                    }
                    // Boolean functions
                    "initial" | "terminal" | "edge" | "change" | "sample" => InferredType::Boolean,
                    // Size returns Integer
                    "size" | "ndims" => InferredType::Integer,
                    // pre maintains type (simplified to Unknown here)
                    _ => InferredType::Unknown,
                }
            } else {
                InferredType::Unknown
            }
        }
        Expression::Binary { lhs, op, rhs } => {
            let lhs_type = infer_expression_type(lhs, defined);
            let rhs_type = infer_expression_type(rhs, defined);

            match op {
                // Comparison operators return Boolean
                OpBinary::Lt(_)
                | OpBinary::Le(_)
                | OpBinary::Gt(_)
                | OpBinary::Ge(_)
                | OpBinary::Eq(_)
                | OpBinary::Neq(_) => InferredType::Boolean,
                // Logical operators return Boolean
                OpBinary::And(_) | OpBinary::Or(_) => InferredType::Boolean,
                // Arithmetic operators: promote to Real if either side is Real
                OpBinary::Add(_)
                | OpBinary::Sub(_)
                | OpBinary::Mul(_)
                | OpBinary::Div(_)
                | OpBinary::Exp(_) => {
                    if matches!(lhs_type.base_type(), InferredType::Real)
                        || matches!(rhs_type.base_type(), InferredType::Real)
                    {
                        InferredType::Real
                    } else if matches!(lhs_type.base_type(), InferredType::Integer)
                        && matches!(rhs_type.base_type(), InferredType::Integer)
                    {
                        InferredType::Integer
                    } else {
                        InferredType::Unknown
                    }
                }
                _ => InferredType::Unknown,
            }
        }
        Expression::Unary { op: _, rhs } => infer_expression_type(rhs, defined),
        Expression::Array { elements } => {
            if let Some(first) = elements.first() {
                let elem_type = infer_expression_type(first, defined);
                InferredType::Array(Box::new(elem_type), Some(elements.len()))
            } else {
                InferredType::Unknown
            }
        }
        Expression::Tuple { elements: _ } => InferredType::Unknown,
        Expression::If {
            branches,
            else_branch,
        } => {
            // Type is the type of the branches (should all be the same)
            if let Some((_, then_expr)) = branches.first() {
                infer_expression_type(then_expr, defined)
            } else {
                infer_expression_type(else_branch, defined)
            }
        }
        Expression::Range { .. } => {
            // Range produces an array of integers or reals
            InferredType::Array(Box::new(InferredType::Integer), None)
        }
    }
}

/// Collect symbols used in an equation and check for undefined references
fn collect_equation_symbols(
    eq: &Equation,
    used: &mut HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
) {
    match eq {
        Equation::Empty => {}
        Equation::Simple { lhs, rhs } => {
            collect_and_check_expression(lhs, used, diagnostics, defined, globals);
            collect_and_check_expression(rhs, used, diagnostics, defined, globals);

            // Type check: lhs and rhs should be compatible
            let lhs_type = infer_expression_type(lhs, defined);
            let rhs_type = infer_expression_type(rhs, defined);

            if !lhs_type.is_compatible_with(&rhs_type) {
                // Get location from lhs expression
                if let Some(loc) = lhs.get_location() {
                    diagnostics.push(create_diagnostic(
                        loc.start_line,
                        loc.start_column,
                        format!(
                            "Type mismatch in equation: {} is not compatible with {}",
                            lhs_type, rhs_type
                        ),
                        DiagnosticSeverity::WARNING,
                    ));
                }
            }

            // Check for Boolean = numeric mismatch specifically
            if (matches!(lhs_type.base_type(), InferredType::Boolean) && rhs_type.is_numeric())
                || (lhs_type.is_numeric() && matches!(rhs_type.base_type(), InferredType::Boolean))
            {
                if let Some(loc) = lhs.get_location() {
                    diagnostics.push(create_diagnostic(
                        loc.start_line,
                        loc.start_column,
                        "Cannot mix Boolean and numeric types in equation".to_string(),
                        DiagnosticSeverity::ERROR,
                    ));
                }
            }
        }
        Equation::Connect { lhs, rhs } => {
            collect_and_check_component_ref(lhs, used, diagnostics, defined, globals);
            collect_and_check_component_ref(rhs, used, diagnostics, defined, globals);
        }
        Equation::For { indices, equations } => {
            // For loop indices are locally defined
            let mut local_defined = defined.clone();
            for index in indices {
                local_defined.insert(
                    index.ident.text.clone(),
                    DefinedSymbol {
                        line: index.ident.location.start_line,
                        col: index.ident.location.start_column,
                        name_len: index.ident.text.len() as u32,
                        is_parameter: false,
                        is_class: false,
                        has_default: true,
                        type_name: "Integer".to_string(), // loop indices are integers
                        shape: vec![],
                    },
                );
                collect_and_check_expression(
                    &index.range,
                    used,
                    diagnostics,
                    &local_defined,
                    globals,
                );
            }
            for sub_eq in equations {
                collect_equation_symbols(sub_eq, used, diagnostics, &local_defined, globals);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                collect_and_check_expression(&block.cond, used, diagnostics, defined, globals);
                for sub_eq in &block.eqs {
                    collect_equation_symbols(sub_eq, used, diagnostics, defined, globals);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_and_check_expression(&block.cond, used, diagnostics, defined, globals);
                for sub_eq in &block.eqs {
                    collect_equation_symbols(sub_eq, used, diagnostics, defined, globals);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    collect_equation_symbols(sub_eq, used, diagnostics, defined, globals);
                }
            }
        }
        Equation::FunctionCall { comp, args } => {
            collect_and_check_component_ref(comp, used, diagnostics, defined, globals);
            for arg in args {
                collect_and_check_expression(arg, used, diagnostics, defined, globals);
            }
        }
    }
}

/// Collect symbols used in a statement
fn collect_statement_symbols(
    stmt: &Statement,
    used: &mut HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
) {
    match stmt {
        Statement::Empty => {}
        Statement::Assignment { comp, value } => {
            collect_and_check_component_ref(comp, used, diagnostics, defined, globals);
            collect_and_check_expression(value, used, diagnostics, defined, globals);
        }
        Statement::FunctionCall { comp, args } => {
            collect_and_check_component_ref(comp, used, diagnostics, defined, globals);
            for arg in args {
                collect_and_check_expression(arg, used, diagnostics, defined, globals);
            }
        }
        Statement::For { indices, equations } => {
            let mut local_defined = defined.clone();
            for index in indices {
                local_defined.insert(
                    index.ident.text.clone(),
                    DefinedSymbol {
                        line: index.ident.location.start_line,
                        col: index.ident.location.start_column,
                        name_len: index.ident.text.len() as u32,
                        is_parameter: false,
                        is_class: false,
                        has_default: true,
                        type_name: "Integer".to_string(), // loop indices are integers
                        shape: vec![],
                    },
                );
                collect_and_check_expression(
                    &index.range,
                    used,
                    diagnostics,
                    &local_defined,
                    globals,
                );
            }
            for sub_stmt in equations {
                collect_statement_symbols(sub_stmt, used, diagnostics, &local_defined, globals);
            }
        }
        Statement::While(block) => {
            collect_and_check_expression(&block.cond, used, diagnostics, defined, globals);
            for sub_stmt in &block.stmts {
                collect_statement_symbols(sub_stmt, used, diagnostics, defined, globals);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_and_check_expression(&block.cond, used, diagnostics, defined, globals);
                for sub_stmt in &block.stmts {
                    collect_statement_symbols(sub_stmt, used, diagnostics, defined, globals);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    collect_statement_symbols(sub_stmt, used, diagnostics, defined, globals);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                collect_and_check_expression(&block.cond, used, diagnostics, defined, globals);
                for sub_stmt in &block.stmts {
                    collect_statement_symbols(sub_stmt, used, diagnostics, defined, globals);
                }
            }
        }
        Statement::Return { .. } | Statement::Break { .. } => {}
    }
}

/// Collect used symbols from an expression (for unused variable detection)
fn collect_used_symbols(expr: &Expression, used: &mut HashSet<String>) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            if let Some(first) = comp_ref.parts.first() {
                used.insert(first.ident.text.clone());
            }
        }
        Expression::Terminal { .. } => {}
        Expression::FunctionCall { comp, args } => {
            if let Some(first) = comp.parts.first() {
                used.insert(first.ident.text.clone());
            }
            for arg in args {
                collect_used_symbols(arg, used);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_used_symbols(lhs, used);
            collect_used_symbols(rhs, used);
        }
        Expression::Unary { rhs, .. } => {
            collect_used_symbols(rhs, used);
        }
        Expression::Array { elements } => {
            for elem in elements {
                collect_used_symbols(elem, used);
            }
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                collect_used_symbols(elem, used);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_used_symbols(cond, used);
                collect_used_symbols(then_expr, used);
            }
            collect_used_symbols(else_branch, used);
        }
        Expression::Range { start, step, end } => {
            collect_used_symbols(start, used);
            if let Some(s) = step {
                collect_used_symbols(s, used);
            }
            collect_used_symbols(end, used);
        }
    }
}

/// Collect and check expression for undefined references
fn collect_and_check_expression(
    expr: &Expression,
    used: &mut HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            collect_and_check_component_ref(comp_ref, used, diagnostics, defined, globals);
        }
        Expression::Terminal { .. } => {}
        Expression::FunctionCall { comp, args } => {
            // For function calls, the first part is the function name
            // Check if it's a known function or type
            if let Some(first) = comp.parts.first() {
                let name = &first.ident.text;
                // Functions are typically global builtins or user-defined
                // Mark as used if it's defined locally
                if defined.contains_key(name) {
                    used.insert(name.clone());
                }
                // Don't report error for function names - they might be external
            }
            for arg in args {
                collect_and_check_expression(arg, used, diagnostics, defined, globals);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_and_check_expression(lhs, used, diagnostics, defined, globals);
            collect_and_check_expression(rhs, used, diagnostics, defined, globals);
        }
        Expression::Unary { rhs, .. } => {
            collect_and_check_expression(rhs, used, diagnostics, defined, globals);
        }
        Expression::Array { elements } => {
            for elem in elements {
                collect_and_check_expression(elem, used, diagnostics, defined, globals);
            }
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                collect_and_check_expression(elem, used, diagnostics, defined, globals);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_and_check_expression(cond, used, diagnostics, defined, globals);
                collect_and_check_expression(then_expr, used, diagnostics, defined, globals);
            }
            collect_and_check_expression(else_branch, used, diagnostics, defined, globals);
        }
        Expression::Range { start, step, end } => {
            collect_and_check_expression(start, used, diagnostics, defined, globals);
            if let Some(s) = step {
                collect_and_check_expression(s, used, diagnostics, defined, globals);
            }
            collect_and_check_expression(end, used, diagnostics, defined, globals);
        }
    }
}

/// Check component reference for undefined symbols
fn collect_and_check_component_ref(
    comp_ref: &ComponentReference,
    used: &mut HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
) {
    if let Some(first) = comp_ref.parts.first() {
        let name = &first.ident.text;

        // Mark as used
        used.insert(name.clone());

        // Check if defined
        if !defined.contains_key(name) && !globals.contains(name) {
            diagnostics.push(create_diagnostic(
                first.ident.location.start_line,
                first.ident.location.start_column,
                format!("Undefined variable '{}'", name),
                DiagnosticSeverity::ERROR,
            ));
        }

        // Check subscript expressions
        if let Some(subs) = &first.subs {
            for sub in subs {
                if let crate::ir::ast::Subscript::Expression(expr) = sub {
                    collect_and_check_expression(expr, used, diagnostics, defined, globals);
                }
            }
        }
    }

    // Check remaining parts' subscripts
    for part in comp_ref.parts.iter().skip(1) {
        if let Some(subs) = &part.subs {
            for sub in subs {
                if let crate::ir::ast::Subscript::Expression(expr) = sub {
                    collect_and_check_expression(expr, used, diagnostics, defined, globals);
                }
            }
        }
    }
}

fn create_diagnostic(
    line: u32,
    col: u32,
    message: String,
    severity: DiagnosticSeverity,
) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: line.saturating_sub(1),
                character: col.saturating_sub(1),
            },
            end: Position {
                line: line.saturating_sub(1),
                character: col.saturating_sub(1) + 20,
            },
        },
        severity: Some(severity),
        source: Some("rumoca".to_string()),
        message,
        ..Default::default()
    }
}

/// Extract structured error information from ParolError
fn extract_structured_error(error: &ParolError, source: &str) -> (u32, u32, String) {
    if let ParolError::ParserError(parser_error) = error {
        if let Some((line, col, message)) = extract_from_parser_error(parser_error, source) {
            return (line, col, message);
        }
    }
    // Fallback
    (1, 1, "Syntax error".to_string())
}

/// Extract location and message from a ParserError
fn extract_from_parser_error(error: &ParserError, source: &str) -> Option<(u32, u32, String)> {
    match error {
        ParserError::SyntaxErrors { entries } => {
            if let Some(first) = entries.first() {
                let line = first.error_location.start_line;
                let col = first.error_location.start_column;
                let message = build_clean_message(first, source);
                return Some((line, col, message));
            }
        }
        ParserError::UnprocessedInput { last_token, .. } => {
            let line = last_token.start_line;
            let col = last_token.start_column;
            return Some((line, col, "Unexpected input after valid syntax".to_string()));
        }
        ParserError::PredictionError { .. } => {
            return Some((1, 1, "Unexpected token".to_string()));
        }
        _ => {}
    }
    None
}

/// Build a clean error message from a SyntaxError, using source to extract actual token text
fn build_clean_message(err: &parol_runtime::SyntaxError, source: &str) -> String {
    if !err.unexpected_tokens.is_empty() {
        let unexpected = &err.unexpected_tokens[0];
        // Extract actual token text from source using byte offsets
        let start = unexpected.token.start as usize;
        let end = unexpected.token.end as usize;
        let token_text = if start < source.len() && end <= source.len() && start < end {
            &source[start..end]
        } else {
            // Fallback to cleaned token_type if extraction fails
            return build_fallback_message(err);
        };

        let expected: Vec<String> = err
            .expected_tokens
            .iter()
            .take(5)
            .map(|s| clean_token_name(s))
            .collect();

        if expected.is_empty() {
            return format!("Unexpected '{}'", token_text);
        } else if expected.len() == 1 {
            return format!("Unexpected '{}', expected {}", token_text, expected[0]);
        } else {
            return format!(
                "Unexpected '{}', expected one of: {}",
                token_text,
                expected.join(", ")
            );
        }
    }
    "Syntax error".to_string()
}

/// Fallback message builder when source extraction fails
fn build_fallback_message(err: &parol_runtime::SyntaxError) -> String {
    if !err.unexpected_tokens.is_empty() {
        let token_name = clean_token_name(&err.unexpected_tokens[0].token_type);
        let expected: Vec<String> = err
            .expected_tokens
            .iter()
            .take(5)
            .map(|s| clean_token_name(s))
            .collect();

        if expected.is_empty() {
            return format!("Unexpected {}", token_name);
        } else {
            return format!(
                "Unexpected {}, expected one of: {}",
                token_name,
                expected.join(", ")
            );
        }
    }
    "Syntax error".to_string()
}

/// Clean up internal token names to be more user-friendly
fn clean_token_name(name: &str) -> String {
    match name {
        // Punctuation
        "Semicolon" => "';'".to_string(),
        "Comma" => "','".to_string(),
        "LParen" => "'('".to_string(),
        "RParen" => "')'".to_string(),
        "LBrace" => "'{'".to_string(),
        "RBrace" => "'}'".to_string(),
        "LBracket" => "'['".to_string(),
        "RBracket" => "']'".to_string(),
        "Assign" => "':='".to_string(),
        "Equals" => "'='".to_string(),
        "Colon" => "':'".to_string(),
        "Dot" => "'.'".to_string(),
        "Plus" => "'+'".to_string(),
        "Minus" => "'-'".to_string(),
        "Star" => "'*'".to_string(),
        "Slash" => "'/'".to_string(),
        "Caret" => "'^'".to_string(),
        "Less" => "'<'".to_string(),
        "Greater" => "'>'".to_string(),
        "LessEqual" => "'<='".to_string(),
        "GreaterEqual" => "'>='".to_string(),
        "NotEqual" => "'<>'".to_string(),

        // Regex-based identifier patterns (parol internal names)
        s if s.starts_with("LBracketUnderscore") && s.contains("AMinusZ") => {
            "identifier".to_string()
        }
        s if s.contains("AMinusZ") && s.contains("0Minus9") => "identifier".to_string(),

        // Number patterns
        s if s.contains("0Minus9") => "number".to_string(),

        // String patterns
        s if s.contains("QuotationMark") || s.contains("DoubleQuote") => "string".to_string(),

        // Clean up CamelCase keywords to lowercase
        s => s.to_lowercase(),
    }
}
