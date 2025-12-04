//! Expression formatting for the format visitor.
//!
//! Provides formatting methods for expressions, component references, and arrays.

use crate::ir::ast::{
    ComponentRefPart, ComponentReference, Expression, OpBinary, Subscript, TerminalType,
};

use super::super::operators::{
    binary_op_is_right_assoc, binary_op_precedence, format_binary_op, format_unary_op,
};
use super::FormatVisitor;

impl FormatVisitor {
    pub fn format_expression(&self, expr: &Expression) -> String {
        self.format_expression_with_context(expr, None, false)
    }

    /// Format an expression with context about the parent operator for precedence handling.
    /// - `parent_op`: The parent binary operator, if any
    /// - `is_right_child`: Whether this expression is the right child of the parent operator
    pub fn format_expression_with_context(
        &self,
        expr: &Expression,
        parent_op: Option<&OpBinary>,
        is_right_child: bool,
    ) -> String {
        match expr {
            Expression::Empty => String::new(),
            Expression::Terminal {
                terminal_type,
                token,
            } => match terminal_type {
                TerminalType::String => format!("\"{}\"", token.text),
                _ => {
                    // Use original source text if available to preserve exact user input
                    if let Some(src) = &self.source {
                        let start = token.location.start as usize;
                        let end = token.location.end as usize;
                        if start < end && end <= src.len() {
                            return src[start..end].to_string();
                        }
                    }
                    token.text.clone()
                }
            },
            Expression::ComponentReference(comp_ref) => self.format_comp_ref(comp_ref),
            Expression::Binary { op, lhs, rhs } => {
                let my_prec = binary_op_precedence(op);

                // Format children with context
                let lhs_str = self.format_expression_with_context(lhs, Some(op), false);
                let rhs_str = self.format_expression_with_context(rhs, Some(op), true);
                let op_str = format_binary_op(op);
                let result = format!("{} {} {}", lhs_str, op_str, rhs_str);

                // Determine if we need parentheses based on parent operator
                if let Some(parent) = parent_op {
                    let parent_prec = binary_op_precedence(parent);
                    let needs_parens = if my_prec < parent_prec {
                        // Lower precedence always needs parens
                        true
                    } else if my_prec == parent_prec {
                        // Equal precedence: need parens for non-standard associativity
                        // Left-assoc ops need parens on right child, right-assoc on left child
                        if binary_op_is_right_assoc(parent) {
                            !is_right_child
                        } else {
                            is_right_child
                        }
                    } else {
                        false
                    };

                    if needs_parens {
                        format!("({})", result)
                    } else {
                        result
                    }
                } else {
                    result
                }
            }
            Expression::Unary { op, rhs } => {
                // Unary operators bind tightly, but need parens if parent is multiplication/division
                // and the unary is applied to a complex expression
                let rhs_str = self.format_expression_with_context(rhs, None, false);
                let op_str = format_unary_op(op);
                let result = format!("{}{}", op_str, rhs_str);

                // Unary expressions need parens when the parent is higher precedence than additive
                // e.g., -a * b should stay as (-a) * b, but we write it as -a * b
                // However, -(a + b) * c needs parens: (-(a + b)) * c
                if let Some(parent) = parent_op {
                    let parent_prec = binary_op_precedence(parent);
                    // Unary minus/plus have precedence between multiplicative and additive
                    // But when used with complex subexpressions, we need parens
                    if parent_prec >= 5 {
                        // multiplicative or higher
                        // Only need parens if the unary is applied to a binary expr
                        if matches!(**rhs, Expression::Binary { .. }) {
                            return format!("({})", result);
                        }
                    }
                }
                result
            }
            Expression::FunctionCall { comp, args } => {
                let args_str: Vec<String> =
                    args.iter().map(|a| self.format_expression(a)).collect();
                format!("{}({})", self.format_comp_ref(comp), args_str.join(", "))
            }
            Expression::Array { elements } => {
                let elem_str: Vec<String> =
                    elements.iter().map(|e| self.format_expression(e)).collect();
                format!("{{{}}}", elem_str.join(", "))
            }
            Expression::Tuple { elements } => {
                let elem_str: Vec<String> =
                    elements.iter().map(|e| self.format_expression(e)).collect();
                format!("({})", elem_str.join(", "))
            }
            Expression::Range { start, step, end } => {
                let start_str = self.format_expression(start);
                let end_str = self.format_expression(end);
                if let Some(step) = step {
                    format!("{}:{}:{}", start_str, self.format_expression(step), end_str)
                } else {
                    format!("{}:{}", start_str, end_str)
                }
            }
            Expression::If {
                branches,
                else_branch,
            } => {
                let mut result = String::new();
                for (i, (cond, then_expr)) in branches.iter().enumerate() {
                    if i == 0 {
                        result.push_str(&format!(
                            "if {} then {}",
                            self.format_expression(cond),
                            self.format_expression(then_expr)
                        ));
                    } else {
                        result.push_str(&format!(
                            " elseif {} then {}",
                            self.format_expression(cond),
                            self.format_expression(then_expr)
                        ));
                    }
                }
                result.push_str(&format!(" else {}", self.format_expression(else_branch)));
                result
            }
            Expression::Parenthesized { inner } => {
                format!("({})", self.format_expression(inner))
            }
            Expression::ArrayComprehension { expr, indices } => {
                let indices_str: Vec<String> = indices
                    .iter()
                    .map(|idx| {
                        format!(
                            "{} in {}",
                            idx.ident.text,
                            self.format_expression(&idx.range)
                        )
                    })
                    .collect();
                format!(
                    "{{ {} for {} }}",
                    self.format_expression(expr),
                    indices_str.join(", ")
                )
            }
        }
    }

    pub fn format_comp_ref(&self, comp_ref: &ComponentReference) -> String {
        let parts: Vec<String> = comp_ref
            .parts
            .iter()
            .map(|p| self.format_comp_ref_part(p))
            .collect();
        parts.join(".")
    }

    fn format_comp_ref_part(&self, part: &ComponentRefPart) -> String {
        let mut result = part.ident.text.clone();
        if let Some(subs) = &part.subs {
            let sub_str: Vec<String> = subs.iter().map(|s| self.format_subscript(s)).collect();
            result.push_str(&format!("[{}]", sub_str.join(", ")));
        }
        result
    }

    pub fn format_subscript(&self, sub: &Subscript) -> String {
        match sub {
            Subscript::Empty => String::new(),
            Subscript::Expression(expr) => self.format_expression(expr),
            Subscript::Range { .. } => ":".to_string(),
        }
    }

    /// Check if an array should be formatted across multiple lines
    pub fn should_format_array_multiline(&self, elements: &[Expression], level: usize) -> bool {
        // Always multiline if more than 2 elements
        if elements.len() > 2 {
            return true;
        }

        // Check if single-line would exceed max length
        let single_line = self.format_array_single_line(elements);
        let indent_len = level * self.options.indent_size;
        single_line.len() + indent_len > self.options.max_line_length
    }

    fn format_array_single_line(&self, elements: &[Expression]) -> String {
        let elem_str: Vec<String> = elements.iter().map(|e| self.format_expression(e)).collect();
        format!("{{{}}}", elem_str.join(", "))
    }

    pub fn format_array_multiline(&self, elements: &[Expression], level: usize) -> String {
        let inner_indent = self.indent_str.repeat(level + 1);
        let outer_indent = self.indent_str.repeat(level);
        let mut result = String::from("{\n");

        for (i, elem) in elements.iter().enumerate() {
            result.push_str(&inner_indent);
            result.push_str(&self.format_expression(elem));
            if i < elements.len() - 1 {
                result.push(',');
            }
            result.push('\n');
        }

        result.push_str(&outer_indent);
        result.push('}');
        result
    }
}
