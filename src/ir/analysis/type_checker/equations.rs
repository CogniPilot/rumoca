//! Basic equation and statement type checking.

use std::collections::HashMap;

use crate::ir::ast::{ClassDefinition, Equation, Expression, Statement};

use crate::ir::analysis::symbols::DefinedSymbol;
use crate::ir::analysis::type_inference::{
    ClassLookup, SymbolType, infer_expression_type_with_classes,
};

use super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check types in an equation
///
/// Returns a list of type errors found. The `defined` map should contain
/// all symbols defined in the current scope.
pub fn check_equation(eq: &Equation, defined: &HashMap<String, DefinedSymbol>) -> TypeCheckResult {
    check_equation_with_classes::<HashMap<String, ClassDefinition>>(eq, defined, None)
}

/// Check types in an equation with class lookup for member access resolution.
pub fn check_equation_with_classes<C: ClassLookup>(
    eq: &Equation,
    defined: &HashMap<String, DefinedSymbol>,
    classes: Option<&C>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();
    check_equation_impl(eq, defined, classes, &mut result);
    result
}

/// Check types in a list of equations
pub fn check_equations(
    equations: &[Equation],
    defined: &HashMap<String, DefinedSymbol>,
) -> TypeCheckResult {
    check_equations_with_classes::<HashMap<String, ClassDefinition>>(equations, defined, None)
}

/// Check types in a list of equations with class lookup.
pub fn check_equations_with_classes<C: ClassLookup>(
    equations: &[Equation],
    defined: &HashMap<String, DefinedSymbol>,
    classes: Option<&C>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();
    for eq in equations {
        check_equation_impl(eq, defined, classes, &mut result);
    }
    result
}

/// Check types in a statement
pub fn check_statement(
    stmt: &Statement,
    defined: &HashMap<String, DefinedSymbol>,
) -> TypeCheckResult {
    check_statement_with_classes::<HashMap<String, ClassDefinition>>(stmt, defined, None)
}

/// Check types in a statement with class lookup.
pub fn check_statement_with_classes<C: ClassLookup>(
    stmt: &Statement,
    defined: &HashMap<String, DefinedSymbol>,
    classes: Option<&C>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();
    check_statement_impl(stmt, defined, classes, &mut result);
    result
}

/// Check types in a list of statements
pub fn check_statements(
    statements: &[Statement],
    defined: &HashMap<String, DefinedSymbol>,
) -> TypeCheckResult {
    check_statements_with_classes::<HashMap<String, ClassDefinition>>(statements, defined, None)
}

/// Check types in a list of statements with class lookup.
pub fn check_statements_with_classes<C: ClassLookup>(
    statements: &[Statement],
    defined: &HashMap<String, DefinedSymbol>,
    classes: Option<&C>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();
    for stmt in statements {
        check_statement_impl(stmt, defined, classes, &mut result);
    }
    result
}

/// Internal implementation for checking an equation
fn check_equation_impl<C: ClassLookup>(
    eq: &Equation,
    defined: &HashMap<String, DefinedSymbol>,
    classes: Option<&C>,
    result: &mut TypeCheckResult,
) {
    match eq {
        Equation::Empty => {}
        Equation::Simple { lhs, rhs } => {
            check_expression_pair(lhs, rhs, defined, classes, result);
        }
        Equation::Connect { .. } => {
            // Connect equations have special typing rules not yet implemented
        }
        Equation::For { indices, equations } => {
            // For loop indices are locally defined
            let mut local_defined = defined.clone();
            for index in indices {
                local_defined.insert(
                    index.ident.text.clone(),
                    DefinedSymbol::loop_index(
                        &index.ident.text,
                        index.ident.location.start_line,
                        index.ident.location.start_column,
                    ),
                );
            }
            for sub_eq in equations {
                check_equation_impl(sub_eq, &local_defined, classes, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                // Check the condition is Boolean
                let cond_type = infer_expression_type_with_classes(&block.cond, defined, classes);
                if !matches!(
                    cond_type.base_type(),
                    SymbolType::Boolean | SymbolType::Unknown
                ) && let Some(loc) = block.cond.get_location()
                {
                    result.add_error(TypeError::new(
                        loc.clone(),
                        SymbolType::Boolean,
                        cond_type,
                        "When condition must be Boolean".to_string(),
                        TypeErrorSeverity::Error,
                    ));
                }
                for sub_eq in &block.eqs {
                    check_equation_impl(sub_eq, defined, classes, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                // Check the condition is Boolean
                let cond_type = infer_expression_type_with_classes(&block.cond, defined, classes);
                if !matches!(
                    cond_type.base_type(),
                    SymbolType::Boolean | SymbolType::Unknown
                ) && let Some(loc) = block.cond.get_location()
                {
                    result.add_error(TypeError::new(
                        loc.clone(),
                        SymbolType::Boolean,
                        cond_type,
                        "If condition must be Boolean".to_string(),
                        TypeErrorSeverity::Error,
                    ));
                }
                for sub_eq in &block.eqs {
                    check_equation_impl(sub_eq, defined, classes, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_equation_impl(sub_eq, defined, classes, result);
                }
            }
        }
        Equation::FunctionCall { .. } => {
            // Function call equations - argument type checking could be added
        }
    }
}

/// Internal implementation for checking a statement
fn check_statement_impl<C: ClassLookup>(
    stmt: &Statement,
    defined: &HashMap<String, DefinedSymbol>,
    classes: Option<&C>,
    result: &mut TypeCheckResult,
) {
    match stmt {
        Statement::Empty => {}
        Statement::Assignment { comp, value } => {
            // Infer the type of the target component
            if let Some(first) = comp.parts.first()
                && let Some(sym) = defined.get(&first.ident.text)
            {
                let target_type = sym.declared_type.clone();
                let value_type = infer_expression_type_with_classes(value, defined, classes);

                if !target_type.is_compatible_with(&value_type)
                    && let Some(loc) = value.get_location()
                {
                    result.add_error(TypeError::mismatch(loc.clone(), target_type, value_type));
                }
            }
        }
        Statement::FunctionCall { .. } => {
            // Function call statements - argument type checking could be added
        }
        Statement::For { indices, equations } => {
            let mut local_defined = defined.clone();
            for index in indices {
                local_defined.insert(
                    index.ident.text.clone(),
                    DefinedSymbol::loop_index(
                        &index.ident.text,
                        index.ident.location.start_line,
                        index.ident.location.start_column,
                    ),
                );
            }
            for sub_stmt in equations {
                check_statement_impl(sub_stmt, &local_defined, classes, result);
            }
        }
        Statement::While(block) => {
            // Check the condition is Boolean
            let cond_type = infer_expression_type_with_classes(&block.cond, defined, classes);
            if !matches!(
                cond_type.base_type(),
                SymbolType::Boolean | SymbolType::Unknown
            ) && let Some(loc) = block.cond.get_location()
            {
                result.add_error(TypeError::new(
                    loc.clone(),
                    SymbolType::Boolean,
                    cond_type,
                    "While condition must be Boolean".to_string(),
                    TypeErrorSeverity::Error,
                ));
            }
            for sub_stmt in &block.stmts {
                check_statement_impl(sub_stmt, defined, classes, result);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                let cond_type = infer_expression_type_with_classes(&block.cond, defined, classes);
                if !matches!(
                    cond_type.base_type(),
                    SymbolType::Boolean | SymbolType::Unknown
                ) && let Some(loc) = block.cond.get_location()
                {
                    result.add_error(TypeError::new(
                        loc.clone(),
                        SymbolType::Boolean,
                        cond_type,
                        "If condition must be Boolean".to_string(),
                        TypeErrorSeverity::Error,
                    ));
                }
                for sub_stmt in &block.stmts {
                    check_statement_impl(sub_stmt, defined, classes, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    check_statement_impl(sub_stmt, defined, classes, result);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                let cond_type = infer_expression_type_with_classes(&block.cond, defined, classes);
                if !matches!(
                    cond_type.base_type(),
                    SymbolType::Boolean | SymbolType::Unknown
                ) && let Some(loc) = block.cond.get_location()
                {
                    result.add_error(TypeError::new(
                        loc.clone(),
                        SymbolType::Boolean,
                        cond_type,
                        "When condition must be Boolean".to_string(),
                        TypeErrorSeverity::Error,
                    ));
                }
                for sub_stmt in &block.stmts {
                    check_statement_impl(sub_stmt, defined, classes, result);
                }
            }
        }
        Statement::Return { .. } | Statement::Break { .. } => {}
    }
}

/// Check that two expressions have compatible types (for equation LHS = RHS)
fn check_expression_pair<C: ClassLookup>(
    lhs: &Expression,
    rhs: &Expression,
    defined: &HashMap<String, DefinedSymbol>,
    classes: Option<&C>,
    result: &mut TypeCheckResult,
) {
    let lhs_type = infer_expression_type_with_classes(lhs, defined, classes);
    let rhs_type = infer_expression_type_with_classes(rhs, defined, classes);

    // Check for Boolean/numeric mixing (more severe)
    if (matches!(lhs_type.base_type(), SymbolType::Boolean) && rhs_type.is_numeric())
        || (lhs_type.is_numeric() && matches!(rhs_type.base_type(), SymbolType::Boolean))
    {
        if let Some(loc) = lhs.get_location() {
            // Move types since branches are mutually exclusive
            result.add_error(TypeError::boolean_numeric_mix(
                loc.clone(),
                lhs_type,
                rhs_type,
            ));
        }
    }
    // Check for general type mismatch (less severe)
    else if !lhs_type.is_compatible_with(&rhs_type)
        && let Some(loc) = lhs.get_location()
    {
        result.add_error(TypeError::mismatch(loc.clone(), lhs_type, rhs_type));
    }
}
