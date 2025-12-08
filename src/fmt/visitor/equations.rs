//! Equation and statement formatting for the format visitor.
//!
//! Provides formatting methods for equations, statements, and for-indices.

use crate::ir::ast::{Equation, Expression, ForIndex, Statement};

use super::FormatVisitor;

impl FormatVisitor {
    pub fn format_equation(&self, eq: &Equation, level: usize) -> String {
        let indent = self.indent_str.repeat(level);

        match eq {
            Equation::Empty => String::new(),
            Equation::Simple { lhs, rhs } => {
                let lhs_str = self.format_expression(lhs);

                // Check if RHS is a multi-line array
                if let Expression::Array { elements } = rhs
                    && self.should_format_array_multiline(elements, level)
                {
                    return format!(
                        "{}{} = {};\n",
                        indent,
                        lhs_str,
                        self.format_array_multiline(elements, level)
                    );
                }

                let rhs_str = self.format_expression(rhs);
                format!("{}{} = {};\n", indent, lhs_str, rhs_str)
            }
            Equation::Connect { lhs, rhs } => {
                format!(
                    "{}connect({}, {});\n",
                    indent,
                    self.format_comp_ref(lhs),
                    self.format_comp_ref(rhs)
                )
            }
            Equation::For { indices, equations } => {
                let idx_str = self.format_for_indices(indices);
                let mut result = format!("{}for {} loop\n", indent, idx_str);
                for sub_eq in equations {
                    result.push_str(&self.format_equation(sub_eq, level + 1));
                }
                result.push_str(&format!("{}end for;\n", indent));
                result
            }
            Equation::When(blocks) => {
                let mut result = String::new();
                for (i, block) in blocks.iter().enumerate() {
                    if i == 0 {
                        result.push_str(&format!(
                            "{}when {} then\n",
                            indent,
                            self.format_expression(&block.cond)
                        ));
                    } else {
                        result.push_str(&format!(
                            "{}elsewhen {} then\n",
                            indent,
                            self.format_expression(&block.cond)
                        ));
                    }
                    for sub_eq in &block.eqs {
                        result.push_str(&self.format_equation(sub_eq, level + 1));
                    }
                }
                result.push_str(&format!("{}end when;\n", indent));
                result
            }
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                let mut result = String::new();
                for (i, block) in cond_blocks.iter().enumerate() {
                    if i == 0 {
                        result.push_str(&format!(
                            "{}if {} then\n",
                            indent,
                            self.format_expression(&block.cond)
                        ));
                    } else {
                        result.push_str(&format!(
                            "{}elseif {} then\n",
                            indent,
                            self.format_expression(&block.cond)
                        ));
                    }
                    for sub_eq in &block.eqs {
                        result.push_str(&self.format_equation(sub_eq, level + 1));
                    }
                }
                if let Some(else_eqs) = else_block {
                    result.push_str(&format!("{}else\n", indent));
                    for sub_eq in else_eqs {
                        result.push_str(&self.format_equation(sub_eq, level + 1));
                    }
                }
                result.push_str(&format!("{}end if;\n", indent));
                result
            }
            Equation::FunctionCall { comp, args } => {
                let args_str: Vec<String> =
                    args.iter().map(|a| self.format_expression(a)).collect();
                format!(
                    "{}{}({});\n",
                    indent,
                    self.format_comp_ref(comp),
                    args_str.join(", ")
                )
            }
        }
    }

    pub fn format_statement(&self, stmt: &Statement, level: usize) -> String {
        let indent = self.indent_str.repeat(level);

        match stmt {
            Statement::Empty => String::new(),
            Statement::Assignment { comp, value } => {
                format!(
                    "{}{} := {};\n",
                    indent,
                    self.format_comp_ref(comp),
                    self.format_expression(value)
                )
            }
            Statement::FunctionCall { comp, args } => {
                let args_str: Vec<String> =
                    args.iter().map(|a| self.format_expression(a)).collect();
                format!(
                    "{}{}({});\n",
                    indent,
                    self.format_comp_ref(comp),
                    args_str.join(", ")
                )
            }
            Statement::For { indices, equations } => {
                let idx_str = self.format_for_indices(indices);
                let mut result = format!("{}for {} loop\n", indent, idx_str);
                for sub_stmt in equations {
                    result.push_str(&self.format_statement(sub_stmt, level + 1));
                }
                result.push_str(&format!("{}end for;\n", indent));
                result
            }
            Statement::While(block) => {
                let mut result = format!(
                    "{}while {} loop\n",
                    indent,
                    self.format_expression(&block.cond)
                );
                for sub_stmt in &block.stmts {
                    result.push_str(&self.format_statement(sub_stmt, level + 1));
                }
                result.push_str(&format!("{}end while;\n", indent));
                result
            }
            Statement::If {
                cond_blocks,
                else_block,
            } => {
                let mut result = String::new();
                for (i, block) in cond_blocks.iter().enumerate() {
                    if i == 0 {
                        result.push_str(&format!(
                            "{}if {} then\n",
                            indent,
                            self.format_expression(&block.cond)
                        ));
                    } else {
                        result.push_str(&format!(
                            "{}elseif {} then\n",
                            indent,
                            self.format_expression(&block.cond)
                        ));
                    }
                    for sub_stmt in &block.stmts {
                        result.push_str(&self.format_statement(sub_stmt, level + 1));
                    }
                }
                if let Some(else_stmts) = else_block {
                    result.push_str(&format!("{}else\n", indent));
                    for sub_stmt in else_stmts {
                        result.push_str(&self.format_statement(sub_stmt, level + 1));
                    }
                }
                result.push_str(&format!("{}end if;\n", indent));
                result
            }
            Statement::When(blocks) => {
                let mut result = String::new();
                for (i, block) in blocks.iter().enumerate() {
                    if i == 0 {
                        result.push_str(&format!(
                            "{}when {} then\n",
                            indent,
                            self.format_expression(&block.cond)
                        ));
                    } else {
                        result.push_str(&format!(
                            "{}elsewhen {} then\n",
                            indent,
                            self.format_expression(&block.cond)
                        ));
                    }
                    for sub_stmt in &block.stmts {
                        result.push_str(&self.format_statement(sub_stmt, level + 1));
                    }
                }
                result.push_str(&format!("{}end when;\n", indent));
                result
            }
            Statement::Return { .. } => format!("{}return;\n", indent),
            Statement::Break { .. } => format!("{}break;\n", indent),
        }
    }

    pub fn format_for_indices(&self, indices: &[ForIndex]) -> String {
        let parts: Vec<String> = indices
            .iter()
            .map(|idx| {
                if matches!(idx.range, Expression::Empty) {
                    idx.ident.text.clone()
                } else {
                    format!(
                        "{} in {}",
                        idx.ident.text,
                        self.format_expression(&idx.range)
                    )
                }
            })
            .collect();
        parts.join(", ")
    }
}
