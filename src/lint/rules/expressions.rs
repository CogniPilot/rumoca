//! Expression analysis lint rules.
//!
//! Rules for checking magic numbers and complex expressions.

use std::collections::HashSet;

use crate::ir::ast::{ClassDefinition, Expression, TerminalType};
use crate::ir::visitor::{Visitable, Visitor};
use crate::lint::{LintLevel, LintMessage, LintResult};

// =============================================================================
// Magic Number Finder Visitor
// =============================================================================

/// Visitor that finds magic numbers in expressions.
struct MagicNumberFinder<'a> {
    acceptable: HashSet<&'static str>,
    file_path: &'a str,
    messages: Vec<LintMessage>,
}

impl<'a> MagicNumberFinder<'a> {
    fn new(file_path: &'a str) -> Self {
        // Common "acceptable" numbers that don't need to be constants
        let acceptable: HashSet<&'static str> = [
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

        Self {
            acceptable,
            file_path,
            messages: Vec::new(),
        }
    }

    fn into_messages(self) -> Vec<LintMessage> {
        self.messages
    }
}

impl Visitor for MagicNumberFinder<'_> {
    fn enter_expression(&mut self, node: &Expression) {
        let Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token,
        } = node
        else {
            return;
        };

        if self.acceptable.contains(token.text.as_str()) {
            return;
        }

        // Check if it looks like a "magic number" (specific constants)
        let Ok(val) = token.text.parse::<f64>() else {
            return;
        };

        // Skip very small or very large numbers (likely physical constants)
        if val.abs() > 1e-6 && val.abs() < 1e6 && val.fract() != 0.0 {
            self.messages.push(
                LintMessage::new(
                    "magic-number",
                    LintLevel::Help,
                    format!(
                        "Consider using a named constant instead of '{}'",
                        token.text
                    ),
                    self.file_path,
                    token.location.start_line,
                    token.location.start_column,
                )
                .with_suggestion("Define as a parameter: parameter Real myConstant = ..."),
            );
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
    let mut finder = MagicNumberFinder::new(file_path);
    class.accept(&mut finder);
    result.messages.extend(finder.into_messages());
}

/// Check for overly complex expressions
pub fn lint_complex_expressions(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    for eq in &class.equations {
        if let crate::ir::ast::Equation::Simple { lhs, rhs } = eq {
            let lhs_depth = expression_depth(lhs);
            let rhs_depth = expression_depth(rhs);

            if (lhs_depth > 5 || rhs_depth > 5)
                && let Some(loc) = lhs.get_location()
            {
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

fn expression_depth(expr: &Expression) -> usize {
    match expr {
        Expression::Empty | Expression::Terminal { .. } | Expression::ComponentReference(_) => 1,
        Expression::Binary { lhs, rhs, .. } => 1 + expression_depth(lhs).max(expression_depth(rhs)),
        Expression::Unary { rhs, .. } => 1 + expression_depth(rhs),
        Expression::FunctionCall { args, .. } => {
            1 + args.iter().map(expression_depth).max().unwrap_or(0)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
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
        Expression::Parenthesized { inner } => expression_depth(inner),
        Expression::ArrayComprehension { expr, indices } => {
            let index_depth = indices
                .iter()
                .map(|idx| expression_depth(&idx.range))
                .max()
                .unwrap_or(0);
            1 + expression_depth(expr).max(index_depth)
        }
    }
}
