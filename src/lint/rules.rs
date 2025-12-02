//! Individual lint rules for Modelica code.

use std::collections::HashSet;

use crate::ir::ast::{ClassDefinition, ClassType, Expression, TerminalType, Variability};

use super::{
    LintLevel, LintMessage, LintResult, collect_defined_symbols, collect_used_symbols,
    is_class_instance_type,
};

/// List of all available lint rules
pub const LINT_RULES: &[(&str, &str, LintLevel)] = &[
    (
        "naming-convention",
        "Check naming conventions (CamelCase for types, camelCase for variables)",
        LintLevel::Note,
    ),
    (
        "missing-documentation",
        "Warn about classes without documentation strings",
        LintLevel::Note,
    ),
    (
        "unused-variable",
        "Detect declared but unused variables",
        LintLevel::Warning,
    ),
    (
        "undefined-reference",
        "Detect references to undefined variables",
        LintLevel::Error,
    ),
    (
        "parameter-no-default",
        "Warn about parameters without default values",
        LintLevel::Help,
    ),
    (
        "empty-section",
        "Detect empty equation or algorithm sections",
        LintLevel::Note,
    ),
    (
        "magic-number",
        "Suggest using named constants instead of magic numbers",
        LintLevel::Help,
    ),
    (
        "complex-expression",
        "Warn about overly complex expressions",
        LintLevel::Note,
    ),
    (
        "inconsistent-units",
        "Check for potential unit inconsistencies",
        LintLevel::Warning,
    ),
    (
        "redundant-extends",
        "Detect redundant or circular extends",
        LintLevel::Warning,
    ),
];

/// Check naming conventions
pub fn lint_naming_conventions(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    let class_name = &class.name.text;
    let line = class.name.location.start_line;
    let col = class.name.location.start_column;

    // Class names should be CamelCase (start with uppercase)
    if !class_name
        .chars()
        .next()
        .map(|c| c.is_uppercase())
        .unwrap_or(false)
    {
        result.messages.push(
            LintMessage::new(
                "naming-convention",
                LintLevel::Note,
                format!(
                    "Class name '{}' should start with an uppercase letter (CamelCase)",
                    class_name
                ),
                file_path,
                line,
                col,
            )
            .with_suggestion(format!("Rename to '{}'", capitalize_first(class_name))),
        );
    }

    // Check component names (should be camelCase or snake_case for variables)
    for (comp_name, comp) in &class.components {
        let comp_line = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_line)
            .unwrap_or(1);
        let comp_col = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_column)
            .unwrap_or(1);

        // Variable/parameter names should not start with uppercase (unless it's a type instance)
        let is_type_instance = is_class_instance_type(&comp.type_name.to_string());
        if !is_type_instance
            && comp_name
                .chars()
                .next()
                .map(|c| c.is_uppercase())
                .unwrap_or(false)
        {
            result.messages.push(
                LintMessage::new(
                    "naming-convention",
                    LintLevel::Note,
                    format!(
                        "Variable '{}' should start with a lowercase letter",
                        comp_name
                    ),
                    file_path,
                    comp_line,
                    comp_col,
                )
                .with_suggestion(format!("Rename to '{}'", lowercase_first(comp_name))),
            );
        }

        // Single-letter names are discouraged except for common ones
        let allowed_single = [
            "x", "y", "z", "t", "u", "v", "w", "i", "j", "k", "n", "m", "p", "q", "r", "s",
        ];
        if comp_name.len() == 1 && !allowed_single.contains(&comp_name.as_str()) {
            result.messages.push(LintMessage::new(
                "naming-convention",
                LintLevel::Help,
                format!("Single-letter variable name '{}' may be unclear", comp_name),
                file_path,
                comp_line,
                comp_col,
            ));
        }
    }
}

/// Check for missing documentation
pub fn lint_missing_documentation(
    _class: &ClassDefinition,
    _file_path: &str,
    _result: &mut LintResult,
) {
    // ClassDefinition doesn't have a description field currently
    // This lint is a placeholder for future enhancement when class-level
    // documentation strings are captured by the parser
}

/// Check for unused variables
pub fn lint_unused_variables(
    class: &ClassDefinition,
    file_path: &str,
    _globals: &HashSet<String>,
    result: &mut LintResult,
) {
    let defined = collect_defined_symbols(class);
    let used = collect_used_symbols(class);

    for (name, sym) in &defined {
        // Skip if used, starts with underscore, is a parameter, is a class, or is a class instance
        if used.contains(name)
            || name.starts_with('_')
            || sym.is_parameter
            || sym.is_constant
            || sym.is_class
            || is_class_instance_type(&sym.type_name)
        {
            continue;
        }

        result.messages.push(
            LintMessage::new(
                "unused-variable",
                LintLevel::Warning,
                format!("Variable '{}' is declared but never used", name),
                file_path,
                sym.line,
                sym.col,
            )
            .with_suggestion(format!(
                "Remove the variable or prefix with underscore: _{}",
                name
            )),
        );
    }
}

/// Check for undefined references
pub fn lint_undefined_references(
    class: &ClassDefinition,
    file_path: &str,
    globals: &HashSet<String>,
    result: &mut LintResult,
) {
    let defined = collect_defined_symbols(class);

    // Check equations for undefined references
    for eq in &class.equations {
        check_equation_references(eq, file_path, &defined, globals, result);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        check_equation_references(eq, file_path, &defined, globals, result);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            check_statement_references(stmt, file_path, &defined, globals, result);
        }
    }

    // Check initial algorithms
    for algo in &class.initial_algorithms {
        for stmt in algo {
            check_statement_references(stmt, file_path, &defined, globals, result);
        }
    }

    // Check component start expressions
    for comp in class.components.values() {
        check_expression_references(&comp.start, file_path, &defined, globals, result);
    }
}

fn check_equation_references(
    eq: &crate::ir::ast::Equation,
    file_path: &str,
    defined: &std::collections::HashMap<String, super::DefinedSymbol>,
    globals: &HashSet<String>,
    result: &mut LintResult,
) {
    match eq {
        crate::ir::ast::Equation::Empty => {}
        crate::ir::ast::Equation::Simple { lhs, rhs } => {
            check_expression_references(lhs, file_path, defined, globals, result);
            check_expression_references(rhs, file_path, defined, globals, result);
        }
        crate::ir::ast::Equation::Connect { lhs, rhs } => {
            check_comp_ref_references(lhs, file_path, defined, globals, result);
            check_comp_ref_references(rhs, file_path, defined, globals, result);
        }
        crate::ir::ast::Equation::For { indices, equations } => {
            // Add loop indices as locally defined
            let mut local_defined = defined.clone();
            for index in indices {
                local_defined.insert(
                    index.ident.text.clone(),
                    super::DefinedSymbol {
                        line: index.ident.location.start_line,
                        col: index.ident.location.start_column,
                        is_parameter: false,
                        is_constant: false,
                        is_class: false,
                        has_default: true,
                        type_name: "Integer".to_string(),
                    },
                );
                check_expression_references(
                    &index.range,
                    file_path,
                    &local_defined,
                    globals,
                    result,
                );
            }
            for sub_eq in equations {
                check_equation_references(sub_eq, file_path, &local_defined, globals, result);
            }
        }
        crate::ir::ast::Equation::When(blocks) => {
            for block in blocks {
                check_expression_references(&block.cond, file_path, defined, globals, result);
                for sub_eq in &block.eqs {
                    check_equation_references(sub_eq, file_path, defined, globals, result);
                }
            }
        }
        crate::ir::ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expression_references(&block.cond, file_path, defined, globals, result);
                for sub_eq in &block.eqs {
                    check_equation_references(sub_eq, file_path, defined, globals, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_equation_references(sub_eq, file_path, defined, globals, result);
                }
            }
        }
        crate::ir::ast::Equation::FunctionCall { comp, args } => {
            // Don't check function name - it might be external
            // But check if it's a locally defined variable being called
            if let Some(first) = comp.parts.first() {
                if defined.contains_key(&first.ident.text) {
                    // It's a local variable, mark as used
                }
            }
            for arg in args {
                check_expression_references(arg, file_path, defined, globals, result);
            }
        }
    }
}

fn check_statement_references(
    stmt: &crate::ir::ast::Statement,
    file_path: &str,
    defined: &std::collections::HashMap<String, super::DefinedSymbol>,
    globals: &HashSet<String>,
    result: &mut LintResult,
) {
    match stmt {
        crate::ir::ast::Statement::Empty => {}
        crate::ir::ast::Statement::Assignment { comp, value } => {
            check_comp_ref_references(comp, file_path, defined, globals, result);
            check_expression_references(value, file_path, defined, globals, result);
        }
        crate::ir::ast::Statement::FunctionCall { comp, args } => {
            // Don't check function name
            if let Some(first) = comp.parts.first() {
                if defined.contains_key(&first.ident.text) {
                    // It's a local variable
                }
            }
            for arg in args {
                check_expression_references(arg, file_path, defined, globals, result);
            }
        }
        crate::ir::ast::Statement::For { indices, equations } => {
            let mut local_defined = defined.clone();
            for index in indices {
                local_defined.insert(
                    index.ident.text.clone(),
                    super::DefinedSymbol {
                        line: index.ident.location.start_line,
                        col: index.ident.location.start_column,
                        is_parameter: false,
                        is_constant: false,
                        is_class: false,
                        has_default: true,
                        type_name: "Integer".to_string(),
                    },
                );
                check_expression_references(
                    &index.range,
                    file_path,
                    &local_defined,
                    globals,
                    result,
                );
            }
            for sub_stmt in equations {
                check_statement_references(sub_stmt, file_path, &local_defined, globals, result);
            }
        }
        crate::ir::ast::Statement::While(block) => {
            check_expression_references(&block.cond, file_path, defined, globals, result);
            for sub_stmt in &block.stmts {
                check_statement_references(sub_stmt, file_path, defined, globals, result);
            }
        }
        crate::ir::ast::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expression_references(&block.cond, file_path, defined, globals, result);
                for sub_stmt in &block.stmts {
                    check_statement_references(sub_stmt, file_path, defined, globals, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    check_statement_references(sub_stmt, file_path, defined, globals, result);
                }
            }
        }
        crate::ir::ast::Statement::When(blocks) => {
            for block in blocks {
                check_expression_references(&block.cond, file_path, defined, globals, result);
                for sub_stmt in &block.stmts {
                    check_statement_references(sub_stmt, file_path, defined, globals, result);
                }
            }
        }
        crate::ir::ast::Statement::Return { .. } | crate::ir::ast::Statement::Break { .. } => {}
    }
}

fn check_expression_references(
    expr: &Expression,
    file_path: &str,
    defined: &std::collections::HashMap<String, super::DefinedSymbol>,
    globals: &HashSet<String>,
    result: &mut LintResult,
) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            check_comp_ref_references(comp_ref, file_path, defined, globals, result);
        }
        Expression::Terminal { .. } => {}
        Expression::FunctionCall { comp, args } => {
            // Function name might be external, don't report as undefined
            // But check subscripts if any
            for part in &comp.parts {
                if let Some(subs) = &part.subs {
                    for sub in subs {
                        if let crate::ir::ast::Subscript::Expression(sub_expr) = sub {
                            check_expression_references(
                                sub_expr, file_path, defined, globals, result,
                            );
                        }
                    }
                }
            }
            for arg in args {
                check_expression_references(arg, file_path, defined, globals, result);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_expression_references(lhs, file_path, defined, globals, result);
            check_expression_references(rhs, file_path, defined, globals, result);
        }
        Expression::Unary { rhs, .. } => {
            check_expression_references(rhs, file_path, defined, globals, result);
        }
        Expression::Array { elements } => {
            for elem in elements {
                check_expression_references(elem, file_path, defined, globals, result);
            }
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                check_expression_references(elem, file_path, defined, globals, result);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_expression_references(cond, file_path, defined, globals, result);
                check_expression_references(then_expr, file_path, defined, globals, result);
            }
            check_expression_references(else_branch, file_path, defined, globals, result);
        }
        Expression::Range { start, step, end } => {
            check_expression_references(start, file_path, defined, globals, result);
            if let Some(s) = step {
                check_expression_references(s, file_path, defined, globals, result);
            }
            check_expression_references(end, file_path, defined, globals, result);
        }
    }
}

fn check_comp_ref_references(
    comp_ref: &crate::ir::ast::ComponentReference,
    file_path: &str,
    defined: &std::collections::HashMap<String, super::DefinedSymbol>,
    globals: &HashSet<String>,
    result: &mut LintResult,
) {
    if let Some(first) = comp_ref.parts.first() {
        let name = &first.ident.text;

        // Check if defined locally or globally
        if !defined.contains_key(name) && !globals.contains(name) {
            result.messages.push(
                LintMessage::new(
                    "undefined-reference",
                    LintLevel::Error,
                    format!("Undefined variable '{}'", name),
                    file_path,
                    first.ident.location.start_line,
                    first.ident.location.start_column,
                )
                .with_suggestion("Check for typos or ensure the variable is declared"),
            );
        }

        // Check subscripts
        if let Some(subs) = &first.subs {
            for sub in subs {
                if let crate::ir::ast::Subscript::Expression(sub_expr) = sub {
                    check_expression_references(sub_expr, file_path, defined, globals, result);
                }
            }
        }
    }

    // Check remaining parts' subscripts
    for part in comp_ref.parts.iter().skip(1) {
        if let Some(subs) = &part.subs {
            for sub in subs {
                if let crate::ir::ast::Subscript::Expression(sub_expr) = sub {
                    check_expression_references(sub_expr, file_path, defined, globals, result);
                }
            }
        }
    }
}

/// Check for parameters without default values
pub fn lint_parameter_defaults(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    for (name, comp) in &class.components {
        if matches!(comp.variability, Variability::Parameter(_)) {
            let has_default = !matches!(comp.start, Expression::Empty);
            if !has_default {
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

                result.messages.push(
                    LintMessage::new(
                        "parameter-no-default",
                        LintLevel::Help,
                        format!("Parameter '{}' has no default value", name),
                        file_path,
                        line,
                        col,
                    )
                    .with_suggestion("Consider adding a default value for better usability"),
                );
            }
        }
    }
}

/// Check for empty sections
pub fn lint_empty_sections(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    let line = class.name.location.start_line;

    // Check for models/blocks without equations (might be intentional for partial classes)
    if matches!(class.class_type, ClassType::Model | ClassType::Block) {
        if class.equations.is_empty()
            && class.initial_equations.is_empty()
            && class.algorithms.is_empty()
            && class.initial_algorithms.is_empty()
            && class.extends.is_empty()  // Not inherited
            && !class.components.is_empty()
        // Has components but no equations
        {
            result.messages.push(LintMessage::new(
                "empty-section",
                LintLevel::Note,
                format!(
                    "{} '{}' has components but no equations or algorithms",
                    format_class_type(&class.class_type),
                    class.name.text
                ),
                file_path,
                line,
                1,
            ));
        }
    }
}

/// Check for magic numbers in equations
pub fn lint_magic_numbers(
    class: &ClassDefinition,
    file_path: &str,
    _source: &str,
    result: &mut LintResult,
) {
    // Common "acceptable" numbers that don't need to be constants
    let acceptable_numbers: HashSet<&str> = [
        "0",
        "1",
        "2",
        "-1",
        "0.0",
        "1.0",
        "2.0",
        "-1.0",
        "0.5",
        "10",
        "100",
        "3.14159",
        "3.141592653589793", // pi approximations
        "2.718281828",       // e approximations
    ]
    .iter()
    .cloned()
    .collect();

    for eq in &class.equations {
        check_magic_numbers_in_equation(eq, file_path, &acceptable_numbers, result);
    }
}

fn check_magic_numbers_in_equation(
    eq: &crate::ir::ast::Equation,
    file_path: &str,
    acceptable: &HashSet<&str>,
    result: &mut LintResult,
) {
    match eq {
        crate::ir::ast::Equation::Simple { lhs, rhs } => {
            check_magic_numbers_in_expr(lhs, file_path, acceptable, result);
            check_magic_numbers_in_expr(rhs, file_path, acceptable, result);
        }
        crate::ir::ast::Equation::For { equations, .. } => {
            for sub_eq in equations {
                check_magic_numbers_in_equation(sub_eq, file_path, acceptable, result);
            }
        }
        _ => {}
    }
}

fn check_magic_numbers_in_expr(
    expr: &Expression,
    file_path: &str,
    acceptable: &HashSet<&str>,
    result: &mut LintResult,
) {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token,
        } => {
            if !acceptable.contains(token.text.as_str()) {
                // Check if it looks like a "magic number" (specific constants)
                if let Ok(val) = token.text.parse::<f64>() {
                    // Skip very small or very large numbers (likely physical constants)
                    if val.abs() > 1e-6 && val.abs() < 1e6 && val.fract() != 0.0 {
                        result.messages.push(
                            LintMessage::new(
                                "magic-number",
                                LintLevel::Help,
                                format!(
                                    "Consider using a named constant instead of '{}'",
                                    token.text
                                ),
                                file_path,
                                token.location.start_line,
                                token.location.start_column,
                            )
                            .with_suggestion(
                                "Define as a parameter: parameter Real myConstant = ...",
                            ),
                        );
                    }
                }
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_magic_numbers_in_expr(lhs, file_path, acceptable, result);
            check_magic_numbers_in_expr(rhs, file_path, acceptable, result);
        }
        Expression::Unary { rhs, .. } => {
            check_magic_numbers_in_expr(rhs, file_path, acceptable, result);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                check_magic_numbers_in_expr(arg, file_path, acceptable, result);
            }
        }
        _ => {}
    }
}

/// Check for overly complex expressions
pub fn lint_complex_expressions(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    for eq in &class.equations {
        if let crate::ir::ast::Equation::Simple { lhs, rhs } = eq {
            let lhs_depth = expression_depth(lhs);
            let rhs_depth = expression_depth(rhs);

            if lhs_depth > 5 || rhs_depth > 5 {
                if let Some(loc) = lhs.get_location() {
                    result.messages.push(
                        LintMessage::new(
                            "complex-expression",
                            LintLevel::Note,
                            "Expression is deeply nested - consider breaking into intermediate variables",
                            file_path,
                            loc.start_line,
                            loc.start_column,
                        )
                        .with_suggestion("Extract sub-expressions into named variables for clarity"),
                    );
                }
            }
        }
    }
}

fn expression_depth(expr: &Expression) -> usize {
    match expr {
        Expression::Empty | Expression::Terminal { .. } | Expression::ComponentReference(_) => 1,
        Expression::Binary { lhs, rhs, .. } => 1 + expression_depth(lhs).max(expression_depth(rhs)),
        Expression::Unary { rhs, .. } => 1 + expression_depth(rhs),
        Expression::FunctionCall { args, .. } => {
            1 + args.iter().map(expression_depth).max().unwrap_or(0)
        }
        Expression::Array { elements } | Expression::Tuple { elements } => {
            1 + elements.iter().map(expression_depth).max().unwrap_or(0)
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            let branch_depth = branches
                .iter()
                .map(|(c, e)| expression_depth(c).max(expression_depth(e)))
                .max()
                .unwrap_or(0);
            1 + branch_depth.max(expression_depth(else_branch))
        }
        Expression::Range { start, step, end } => {
            let step_depth = step.as_ref().map(|s| expression_depth(s)).unwrap_or(0);
            1 + expression_depth(start)
                .max(step_depth)
                .max(expression_depth(end))
        }
    }
}

/// Check for unit consistency (simplified check)
pub fn lint_unit_consistency(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    // This is a simplified check - real unit analysis would require more infrastructure
    // For now, we just check if components have unit attributes

    let mut has_units = false;
    let mut missing_units = Vec::new();

    for (name, comp) in &class.components {
        // Skip non-Real types
        if comp.type_name.to_string() != "Real" {
            continue;
        }

        // Check if unit modifier is present (simplified - would need modifier parsing)
        // For now, just note that this lint exists
        let has_unit = false; // Placeholder - would check comp.modifiers

        if has_unit {
            has_units = true;
        } else {
            missing_units.push((
                name.clone(),
                comp.type_name
                    .name
                    .first()
                    .map(|t| t.location.start_line)
                    .unwrap_or(1),
            ));
        }
    }

    // If some variables have units but others don't, warn about inconsistency
    if has_units && !missing_units.is_empty() {
        for (name, line) in missing_units {
            result.messages.push(LintMessage::new(
                "inconsistent-units",
                LintLevel::Warning,
                format!(
                    "Variable '{}' has no unit specification while others do",
                    name
                ),
                file_path,
                line,
                1,
            ));
        }
    }
}

/// Check for redundant extends
pub fn lint_redundant_extends(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    let line = class.name.location.start_line;

    // Check for duplicate extends
    let mut seen_extends: HashSet<String> = HashSet::new();
    for ext in &class.extends {
        let ext_name = ext.comp.to_string();
        if seen_extends.contains(&ext_name) {
            result.messages.push(LintMessage::new(
                "redundant-extends",
                LintLevel::Warning,
                format!("Duplicate extends clause for '{}'", ext_name),
                file_path,
                line,
                1,
            ));
        }
        seen_extends.insert(ext_name);
    }

    // Check if extending self (would cause infinite recursion)
    for ext in &class.extends {
        if ext.comp.to_string() == class.name.text {
            result.messages.push(LintMessage::new(
                "redundant-extends",
                LintLevel::Error,
                format!("Class '{}' extends itself", class.name.text),
                file_path,
                line,
                1,
            ));
        }
    }
}

// Helper functions

fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().chain(chars).collect(),
    }
}

fn lowercase_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_lowercase().chain(chars).collect(),
    }
}

fn format_class_type(ct: &ClassType) -> &'static str {
    match ct {
        ClassType::Model => "Model",
        ClassType::Class => "Class",
        ClassType::Block => "Block",
        ClassType::Connector => "Connector",
        ClassType::Record => "Record",
        ClassType::Type => "Type",
        ClassType::Function => "Function",
        ClassType::Package => "Package",
        ClassType::Operator => "Operator",
    }
}
