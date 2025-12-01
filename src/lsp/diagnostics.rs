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

use crate::ir::ast::{
    ClassDefinition, ComponentReference, Equation, Expression, OpBinary, Statement, TerminalType,
    Variability,
};
use crate::ir::constants::global_builtins;

/// Compute diagnostics for a document
pub fn compute_diagnostics(uri: &Uri, text: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    let path = uri.path().as_str();
    if path.ends_with(".mo") {
        use crate::modelica_grammar::ModelicaGrammar;
        use crate::modelica_parser::parse;

        let mut grammar = ModelicaGrammar::new();
        match parse(text, path, &mut grammar) {
            Ok(_) => {
                // Parsing succeeded, now try to compile with auto-detected model name
                if let Some(ref ast) = grammar.modelica {
                    // Get the first class name from the AST
                    if let Some(first_class_name) = ast.class_list.keys().next() {
                        // Try full compilation with the detected model name
                        match crate::Compiler::new()
                            .model(first_class_name)
                            .compile_str(text, path)
                        {
                            Ok(_) => {
                                // No compilation errors - run semantic analysis
                            }
                            Err(e) => {
                                let error_msg = format!("{}", e);
                                let (line, col) =
                                    extract_error_location(&error_msg).unwrap_or((1, 1));
                                diagnostics.push(create_diagnostic(
                                    line,
                                    col,
                                    error_msg,
                                    DiagnosticSeverity::ERROR,
                                ));
                            }
                        }
                    }

                    // Run semantic analysis for warnings
                    for class in ast.class_list.values() {
                        analyze_class(class, &mut diagnostics);
                    }
                }
            }
            Err(e) => {
                let error_debug = format!("{:?}", e);
                let error_msg = format!("{}", e);
                let (line, col) = extract_location_from_debug(&error_debug)
                    .or_else(|| extract_error_location(&error_msg))
                    .unwrap_or((1, 1));
                let message = extract_error_cause(&error_debug).unwrap_or(error_msg);
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

/// Extract location from ParolError debug output
fn extract_location_from_debug(debug_str: &str) -> Option<(u32, u32)> {
    if let Some(pos) = debug_str.find("error_location: Location {") {
        let after_location = &debug_str[pos..];

        let line_num = if let Some(line_pos) = after_location.find("start_line:") {
            let after_line = &after_location[line_pos + 11..];
            after_line
                .split(',')
                .next()
                .and_then(|s| s.trim().parse::<u32>().ok())
        } else {
            None
        };

        let col_num = if let Some(col_pos) = after_location.find("start_column:") {
            let after_col = &after_location[col_pos + 13..];
            after_col
                .split(',')
                .next()
                .and_then(|s| s.trim().parse::<u32>().ok())
        } else {
            None
        };

        match (line_num, col_num) {
            (Some(line), Some(col)) => Some((line, col)),
            _ => None,
        }
    } else {
        None
    }
}

/// Extract the error cause from ParolError debug output
fn extract_error_cause(debug_str: &str) -> Option<String> {
    if let Some(pos) = debug_str.find("cause: \"") {
        let after_cause = &debug_str[pos + 8..];
        let mut in_escape = false;
        let mut cause_end = 0;

        for (i, ch) in after_cause.chars().enumerate() {
            if in_escape {
                in_escape = false;
                continue;
            }
            if ch == '\\' {
                in_escape = true;
                continue;
            }
            if ch == '"' {
                cause_end = i;
                break;
            }
        }

        if cause_end > 0 {
            let cause = &after_cause[..cause_end];
            let first_line = cause.lines().next().unwrap_or(cause);
            return Some(first_line.to_string());
        }
    }
    None
}

/// Extract line and column from error message patterns
fn extract_error_location(error_msg: &str) -> Option<(u32, u32)> {
    // Try pattern like "filename.mo:14:9"
    if let Some(mo_pos) = error_msg.find(".mo:") {
        let after_mo = &error_msg[mo_pos + 4..];
        let line_end = after_mo
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(after_mo.len());
        if line_end > 0 {
            if let Ok(line) = after_mo[..line_end].parse::<u32>() {
                if after_mo.len() > line_end && after_mo.as_bytes()[line_end] == b':' {
                    let after_colon = &after_mo[line_end + 1..];
                    let col_end = after_colon
                        .find(|c: char| !c.is_ascii_digit())
                        .unwrap_or(after_colon.len());
                    if col_end > 0 {
                        if let Ok(col) = after_colon[..col_end].parse::<u32>() {
                            return Some((line, col));
                        }
                    }
                }
                return Some((line, 1));
            }
        }
    }

    // Try pattern like "[1:5]"
    if let Some(bracket_pos) = error_msg.find('[') {
        let after_bracket = &error_msg[bracket_pos + 1..];
        if let Some(colon_pos) = after_bracket.find(':') {
            let line_str = &after_bracket[..colon_pos];
            if let Ok(line) = line_str.trim().parse::<u32>() {
                let after_colon = &after_bracket[colon_pos + 1..];
                if let Some(end_pos) = after_colon.find(']') {
                    let col_str = &after_colon[..end_pos];
                    if let Ok(col) = col_str.trim().parse::<u32>() {
                        return Some((line, col));
                    }
                }
            }
        }
    }

    // Try pattern like "line X, column Y"
    if let Some(line_pos) = error_msg.find("line ") {
        let after_line = &error_msg[line_pos + 5..];
        let line_end = after_line
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(after_line.len());
        if let Ok(line) = after_line[..line_end].parse::<u32>() {
            if let Some(col_pos) = after_line.find("column ") {
                let after_col = &after_line[col_pos + 7..];
                let col_end = after_col
                    .find(|c: char| !c.is_ascii_digit())
                    .unwrap_or(after_col.len());
                if let Ok(col) = after_col[..col_end].parse::<u32>() {
                    return Some((line, col));
                }
            }
            return Some((line, 1));
        }
    }

    None
}
