//! Modelica code formatter.
//!
//! Provides AST-based code formatting using a visitor pattern:
//! - Consistent indentation (2 or 4 spaces, or tabs)
//! - Proper spacing around operators
//! - Multi-line array formatting with proper indentation
//! - Normalized line endings
//!
//! ## Configuration
//!
//! The formatter can be configured via:
//! - A `.rumoca_fmt.toml` or `rumoca_fmt.toml` file in the project root
//! - Command line options (override file settings)
//!
//! Example config file:
//! ```toml
//! indent_size = 2
//! use_tabs = false
//! max_line_length = 100
//! ```

use crate::ir::ast::{
    Causality, ClassDefinition, ClassType, Component, ComponentRefPart, ComponentReference,
    Connection, Equation, Expression, ForIndex, Import, OpBinary, OpUnary, Statement,
    StoredDefinition, Subscript, TerminalType, Variability,
};
use crate::ir::visitor::Visitor;
use serde::{Deserialize, Serialize};

/// Formatting options for Modelica code
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct FormatOptions {
    /// Number of spaces per indentation level (ignored if use_tabs is true)
    pub indent_size: usize,
    /// Use tabs instead of spaces for indentation
    pub use_tabs: bool,
    /// Maximum line length before wrapping arrays
    pub max_line_length: usize,
    /// Preserve unformatted content (annotations, etc.) by copying from source
    /// When true, any content not explicitly handled by the formatter is preserved
    #[serde(default)]
    pub preserve_unformatted: bool,
    /// Number of blank lines to insert between top-level class definitions (models, functions, etc.)
    #[serde(default = "default_blank_lines_between_classes")]
    pub blank_lines_between_classes: usize,
}

fn default_blank_lines_between_classes() -> usize {
    1
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            indent_size: 2,
            use_tabs: false,
            max_line_length: 100,
            preserve_unformatted: true,
            blank_lines_between_classes: 1,
        }
    }
}

/// Config file names to search for (in priority order)
pub const CONFIG_FILE_NAMES: &[&str] = &[".rumoca_fmt.toml", "rumoca_fmt.toml"];

impl FormatOptions {
    /// Create options with specified indent size using spaces
    pub fn with_spaces(indent_size: usize) -> Self {
        Self {
            indent_size,
            use_tabs: false,
            max_line_length: 100,
            preserve_unformatted: true,
            blank_lines_between_classes: 1,
        }
    }

    /// Create options using tabs for indentation
    pub fn with_tabs() -> Self {
        Self {
            indent_size: 1,
            use_tabs: true,
            max_line_length: 100,
            preserve_unformatted: true,
            blank_lines_between_classes: 1,
        }
    }

    /// Load format options from a config file.
    ///
    /// Searches for config files in the following order:
    /// 1. `.rumoca_fmt.toml` in the given directory
    /// 2. `rumoca_fmt.toml` in the given directory
    /// 3. Same files in parent directories, up to the root
    ///
    /// Returns `None` if no config file is found.
    pub fn from_config_file(start_dir: &std::path::Path) -> Option<Self> {
        let mut current = start_dir.to_path_buf();
        if current.is_file() {
            current = current.parent()?.to_path_buf();
        }

        loop {
            for config_name in CONFIG_FILE_NAMES {
                let config_path = current.join(config_name);
                if config_path.exists() {
                    if let Ok(contents) = std::fs::read_to_string(&config_path) {
                        if let Ok(options) = toml::from_str::<FormatOptions>(&contents) {
                            return Some(options);
                        }
                    }
                }
            }

            // Move to parent directory
            if let Some(parent) = current.parent() {
                current = parent.to_path_buf();
            } else {
                break;
            }
        }

        None
    }

    /// Merge CLI options into this config, with CLI taking precedence.
    ///
    /// Only overrides fields that were explicitly set (non-default).
    pub fn merge_cli_options(
        &mut self,
        cli_indent_size: Option<usize>,
        cli_use_tabs: Option<bool>,
        cli_max_line_length: Option<usize>,
    ) {
        if let Some(indent_size) = cli_indent_size {
            self.indent_size = indent_size;
        }
        if let Some(use_tabs) = cli_use_tabs {
            self.use_tabs = use_tabs;
        }
        if let Some(max_line_length) = cli_max_line_length {
            self.max_line_length = max_line_length;
        }
    }

    /// Merge CLI options into this config, with CLI taking precedence (extended version).
    ///
    /// Only overrides fields that were explicitly set (non-default).
    pub fn merge_cli_options_ext(
        &mut self,
        cli_indent_size: Option<usize>,
        cli_use_tabs: Option<bool>,
        cli_max_line_length: Option<usize>,
        cli_blank_lines_between_classes: Option<usize>,
    ) {
        self.merge_cli_options(cli_indent_size, cli_use_tabs, cli_max_line_length);
        if let Some(blank_lines) = cli_blank_lines_between_classes {
            self.blank_lines_between_classes = blank_lines;
        }
    }
}

/// A comment with its location for reinsertion during formatting
#[derive(Debug, Clone)]
struct CommentInfo {
    text: String,
    line: u32,
}

/// AST-based formatter that implements the Visitor pattern
struct FormatVisitor {
    options: FormatOptions,
    indent_str: String,
    output: String,
    indent_level: usize,
    /// Comments to be inserted, sorted by line number
    comments: Vec<CommentInfo>,
    /// Index of next comment to potentially insert
    next_comment_idx: usize,
    /// Current output line number (1-based to match source)
    current_line: u32,
}

impl FormatVisitor {
    fn new(options: &FormatOptions) -> Self {
        let indent_str = if options.use_tabs {
            "\t".to_string()
        } else {
            " ".repeat(options.indent_size)
        };
        Self {
            options: options.clone(),
            indent_str,
            output: String::new(),
            indent_level: 0,
            comments: Vec::new(),
            next_comment_idx: 0,
            current_line: 1,
        }
    }

    fn with_comments(options: &FormatOptions, comments: Vec<CommentInfo>) -> Self {
        let indent_str = if options.use_tabs {
            "\t".to_string()
        } else {
            " ".repeat(options.indent_size)
        };
        Self {
            options: options.clone(),
            indent_str,
            output: String::new(),
            indent_level: 0,
            comments,
            next_comment_idx: 0,
            current_line: 1,
        }
    }

    /// Emit any comments that should appear before the given source line
    fn emit_comments_before_line(&mut self, target_line: u32) {
        while self.next_comment_idx < self.comments.len() {
            let comment = &self.comments[self.next_comment_idx];
            if comment.line < target_line {
                // Emit this comment (trim to remove any trailing newlines from the token)
                self.output.push_str(&self.indent());
                self.output.push_str(comment.text.trim_end());
                self.output.push('\n');
                self.current_line += 1;
                self.next_comment_idx += 1;
            } else {
                break;
            }
        }
    }

    /// Emit any remaining comments at the end of output
    fn emit_remaining_comments(&mut self) {
        while self.next_comment_idx < self.comments.len() {
            let comment = &self.comments[self.next_comment_idx];
            self.output.push_str(&self.indent());
            self.output.push_str(comment.text.trim_end());
            self.output.push('\n');
            self.next_comment_idx += 1;
        }
    }

    fn indent(&self) -> String {
        self.indent_str.repeat(self.indent_level)
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn writeln(&mut self, s: &str) {
        self.output.push_str(&self.indent());
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn format_import(&self, import: &Import) -> String {
        match import {
            Import::Qualified { path, .. } => format!("import {};", path),
            Import::Renamed { alias, path, .. } => {
                format!("import {} = {};", alias.text, path)
            }
            Import::Unqualified { path, .. } => format!("import {}.*;", path),
            Import::Selective { path, names, .. } => {
                let name_list: Vec<&str> = names.iter().map(|t| t.text.as_str()).collect();
                format!("import {}.{{{}}};", path, name_list.join(", "))
            }
        }
    }

    /// Check if a component has individual attributes that prevent grouping
    fn component_has_individual_attrs(&self, comp: &Component) -> bool {
        // Has modifications (e.g., R=10)
        if !comp.modifications.is_empty() {
            return true;
        }
        // Has description string
        if !comp.description.is_empty() {
            return true;
        }
        // Has annotation
        if !comp.annotation.is_empty() {
            return true;
        }
        // Has start value with explicit source location
        if let Expression::Terminal { token, .. } = &comp.start {
            if token.location.start_line > 0 {
                return true;
            }
        } else if !matches!(comp.start, Expression::Empty) {
            if comp.start.get_location().is_some_and(|l| l.start_line > 0) {
                return true;
            }
        }
        false
    }

    /// Format a group of components that can be combined on one line
    /// Returns the formatted string like "Real x, y, z;"
    fn format_component_group(&self, components: &[&Component]) -> String {
        if components.is_empty() {
            return String::new();
        }

        let first = components[0];
        let mut result = String::new();

        // Variability prefix (same for all in group)
        match &first.variability {
            Variability::Constant(_) => result.push_str("constant "),
            Variability::Parameter(_) => result.push_str("parameter "),
            Variability::Discrete(_) => result.push_str("discrete "),
            Variability::Empty => {}
        }

        // Causality prefix (same for all in group)
        match &first.causality {
            Causality::Input(_) => result.push_str("input "),
            Causality::Output(_) => result.push_str("output "),
            Causality::Empty => {}
        }

        // Connection prefix (same for all in group)
        match &first.connection {
            Connection::Flow(_) => result.push_str("flow "),
            Connection::Stream(_) => result.push_str("stream "),
            Connection::Empty => {}
        }

        // Type name (same for all in group)
        result.push_str(&first.type_name.to_string());
        result.push(' ');

        // Component names with their array dimensions
        let names: Vec<String> = components
            .iter()
            .map(|comp| {
                let mut name = comp.name.clone();
                if !comp.shape.is_empty() {
                    let dims: Vec<String> = comp.shape.iter().map(|d| d.to_string()).collect();
                    name.push_str(&format!("[{}]", dims.join(", ")));
                }
                name
            })
            .collect();
        result.push_str(&names.join(", "));
        result.push(';');
        result
    }

    fn format_component(&self, comp: &Component) -> String {
        let mut result = String::new();

        // Variability prefix
        match &comp.variability {
            Variability::Constant(_) => result.push_str("constant "),
            Variability::Parameter(_) => result.push_str("parameter "),
            Variability::Discrete(_) => result.push_str("discrete "),
            Variability::Empty => {}
        }

        // Causality prefix
        match &comp.causality {
            Causality::Input(_) => result.push_str("input "),
            Causality::Output(_) => result.push_str("output "),
            Causality::Empty => {}
        }

        // Connection prefix
        match &comp.connection {
            Connection::Flow(_) => result.push_str("flow "),
            Connection::Stream(_) => result.push_str("stream "),
            Connection::Empty => {}
        }

        // Type name
        result.push_str(&comp.type_name.to_string());
        result.push(' ');

        // Component name
        result.push_str(&comp.name);

        // Array dimensions
        if !comp.shape.is_empty() {
            let dims: Vec<String> = comp.shape.iter().map(|d| d.to_string()).collect();
            result.push_str(&format!("[{}]", dims.join(", ")));
        }

        // Modifications
        if !comp.modifications.is_empty() {
            let mods: Vec<String> = comp
                .modifications
                .iter()
                .map(|(k, v)| format!("{} = {}", k, self.format_expression(v)))
                .collect();
            result.push_str(&format!("({})", mods.join(", ")));
        }

        // Start value - only output if it has an explicit source location
        // (default values set by parser have empty locations)
        if let Expression::Terminal { token, .. } = &comp.start {
            if token.location.start_line > 0 {
                result.push_str(&format!(" = {}", self.format_expression(&comp.start)));
            }
        } else if !matches!(comp.start, Expression::Empty) {
            // Non-terminal expressions (like arrays) should always be output
            if comp.start.get_location().is_some_and(|l| l.start_line > 0) {
                result.push_str(&format!(" = {}", self.format_expression(&comp.start)));
            }
        }

        // Description string
        if !comp.description.is_empty() {
            let desc: Vec<String> = comp
                .description
                .iter()
                .map(|t| format!("\"{}\"", t.text))
                .collect();
            result.push_str(&format!(" {}", desc.join(" ")));
        }

        // Annotation
        if !comp.annotation.is_empty() {
            let args: Vec<String> = comp
                .annotation
                .iter()
                .map(|e| self.format_expression(e))
                .collect();
            result.push_str(&format!(" annotation({})", args.join(", ")));
        }

        result.push(';');
        result
    }

    fn format_equation(&self, eq: &Equation, level: usize) -> String {
        let indent = self.indent_str.repeat(level);

        match eq {
            Equation::Empty => String::new(),
            Equation::Simple { lhs, rhs } => {
                let lhs_str = self.format_expression(lhs);

                // Check if RHS is a multi-line array
                if let Expression::Array { elements } = rhs {
                    if self.should_format_array_multiline(elements, level) {
                        return format!(
                            "{}{} = {};\n",
                            indent,
                            lhs_str,
                            self.format_array_multiline(elements, level)
                        );
                    }
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

    fn format_statement(&self, stmt: &Statement, level: usize) -> String {
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

    fn format_for_indices(&self, indices: &[ForIndex]) -> String {
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

    fn format_expression(&self, expr: &Expression) -> String {
        self.format_expression_with_context(expr, None, false)
    }

    /// Format an expression with context about the parent operator for precedence handling.
    /// - `parent_op`: The parent binary operator, if any
    /// - `is_right_child`: Whether this expression is the right child of the parent operator
    fn format_expression_with_context(
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
                _ => token.text.clone(),
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
        }
    }

    fn format_comp_ref(&self, comp_ref: &ComponentReference) -> String {
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

    fn format_subscript(&self, sub: &Subscript) -> String {
        match sub {
            Subscript::Empty => String::new(),
            Subscript::Expression(expr) => self.format_expression(expr),
            Subscript::Range { .. } => ":".to_string(),
        }
    }

    /// Check if an array should be formatted across multiple lines
    fn should_format_array_multiline(&self, elements: &[Expression], level: usize) -> bool {
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

    fn format_array_multiline(&self, elements: &[Expression], level: usize) -> String {
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

/// Implement the Visitor trait for formatting
impl Visitor for FormatVisitor {
    fn enter_stored_definition(&mut self, node: &StoredDefinition) {
        // Within clause
        if let Some(within) = &node.within {
            self.writeln(&format!("within {};", within));
            self.write("\n");
        }
    }

    fn enter_class_definition(&mut self, node: &ClassDefinition) {
        // Class header
        let class_keyword = match node.class_type {
            ClassType::Model => "model",
            ClassType::Class => "class",
            ClassType::Block => "block",
            ClassType::Connector => "connector",
            ClassType::Record => "record",
            ClassType::Type => "type",
            ClassType::Package => "package",
            ClassType::Function => "function",
            ClassType::Operator => "operator",
        };

        let encapsulated = if node.encapsulated {
            "encapsulated "
        } else {
            ""
        };
        self.writeln(&format!(
            "{}{} {}",
            encapsulated, class_keyword, node.name.text
        ));
        self.indent_level += 1;

        // Extends
        for ext in &node.extends {
            self.writeln(&format!("extends {};", ext.comp));
        }

        // Imports
        for import in &node.imports {
            self.writeln(&self.format_import(import));
        }

        // Components (we handle these manually, not via visitor, to control ordering)
        for comp in node.components.values() {
            self.writeln(&self.format_component(comp));
        }

        // Nested classes - explicitly handle since we're not using accept() for format control
        for nested in node.classes.values() {
            self.enter_class_definition(nested);
            self.exit_class_definition(nested);
        }

        // Equations
        if !node.equations.is_empty() {
            self.indent_level -= 1;
            self.writeln("equation");
            self.indent_level += 1;
            for eq in &node.equations {
                let formatted = self.format_equation(eq, self.indent_level);
                self.write(&formatted);
            }
        }

        // Initial equations
        if !node.initial_equations.is_empty() {
            self.indent_level -= 1;
            self.writeln("initial equation");
            self.indent_level += 1;
            for eq in &node.initial_equations {
                let formatted = self.format_equation(eq, self.indent_level);
                self.write(&formatted);
            }
        }

        // Algorithms
        for algo in &node.algorithms {
            self.indent_level -= 1;
            self.writeln("algorithm");
            self.indent_level += 1;
            for stmt in algo {
                let formatted = self.format_statement(stmt, self.indent_level);
                self.write(&formatted);
            }
        }

        // Initial algorithms
        for algo in &node.initial_algorithms {
            self.indent_level -= 1;
            self.writeln("initial algorithm");
            self.indent_level += 1;
            for stmt in algo {
                let formatted = self.format_statement(stmt, self.indent_level);
                self.write(&formatted);
            }
        }
    }

    fn exit_class_definition(&mut self, node: &ClassDefinition) {
        self.indent_level -= 1;
        self.writeln(&format!("end {};", node.name.text));
    }
}

fn format_binary_op(op: &OpBinary) -> &'static str {
    match op {
        OpBinary::Empty => "",
        OpBinary::Add(_) => "+",
        OpBinary::Sub(_) => "-",
        OpBinary::Mul(_) => "*",
        OpBinary::Div(_) => "/",
        OpBinary::Exp(_) => "^",
        OpBinary::Eq(_) => "==",
        OpBinary::Neq(_) => "<>",
        OpBinary::Lt(_) => "<",
        OpBinary::Le(_) => "<=",
        OpBinary::Gt(_) => ">",
        OpBinary::Ge(_) => ">=",
        OpBinary::And(_) => "and",
        OpBinary::Or(_) => "or",
        OpBinary::AddElem(_) => ".+",
        OpBinary::SubElem(_) => ".-",
        OpBinary::MulElem(_) => ".*",
        OpBinary::DivElem(_) => "./",
    }
}

fn format_unary_op(op: &OpUnary) -> &'static str {
    match op {
        OpUnary::Empty => "",
        OpUnary::Minus(_) => "-",
        OpUnary::Plus(_) => "+",
        OpUnary::DotMinus(_) => ".-",
        OpUnary::DotPlus(_) => ".+",
        OpUnary::Not(_) => "not ",
    }
}

/// Get the precedence level for a binary operator.
/// Higher values bind tighter. Based on Modelica specification:
/// 1. or (lowest)
/// 2. and
/// 3. relational: <, <=, >, >=, ==, <>
/// 4. additive: +, -, .+, .-
/// 5. multiplicative: *, /, .*, ./
/// 6. exponentiation: ^ (highest for binary ops)
fn binary_op_precedence(op: &OpBinary) -> u8 {
    match op {
        OpBinary::Empty => 0,
        OpBinary::Or(_) => 1,
        OpBinary::And(_) => 2,
        OpBinary::Eq(_)
        | OpBinary::Neq(_)
        | OpBinary::Lt(_)
        | OpBinary::Le(_)
        | OpBinary::Gt(_)
        | OpBinary::Ge(_) => 3,
        OpBinary::Add(_) | OpBinary::Sub(_) | OpBinary::AddElem(_) | OpBinary::SubElem(_) => 4,
        OpBinary::Mul(_) | OpBinary::Div(_) | OpBinary::MulElem(_) | OpBinary::DivElem(_) => 5,
        OpBinary::Exp(_) => 6,
    }
}

/// Check if an operator is right-associative.
/// In Modelica, exponentiation (^) is right-associative.
fn binary_op_is_right_assoc(op: &OpBinary) -> bool {
    matches!(op, OpBinary::Exp(_))
}

/// Format Modelica code from an AST using the visitor pattern
pub fn format_ast(def: &StoredDefinition, options: &FormatOptions) -> String {
    let mut visitor = FormatVisitor::new(options);

    // Use the visitor pattern for traversal
    // However, since we need custom control over the output ordering,
    // we'll directly format class definitions
    visitor.enter_stored_definition(def);
    for class in def.class_list.values() {
        visitor.enter_class_definition(class);
        // Note: nested classes would be visited here, but we handle them in enter_class_definition
        visitor.exit_class_definition(class);
    }
    visitor.exit_stored_definition(def);

    visitor.output
}

/// Format Modelica code from source text
/// Parses the code, then formats from the AST while preserving comments
pub fn format_modelica(text: &str, options: &FormatOptions) -> String {
    use crate::modelica_grammar::ModelicaGrammar;
    use crate::modelica_parser::parse;

    let mut grammar = ModelicaGrammar::new();
    match parse(text, "<format>", &mut grammar) {
        Ok(_) => {
            if let Some(ast) = grammar.modelica {
                format_ast_with_comments(&ast, &grammar.comments, options)
            } else {
                text.to_string()
            }
        }
        Err(_) => {
            // Parse error - fall back to simple line-based formatting
            format_modelica_fallback(text, options)
        }
    }
}

/// Format AST with comments reinserted at their original locations
fn format_ast_with_comments(
    def: &StoredDefinition,
    comments: &[crate::modelica_grammar::ParsedComment],
    options: &FormatOptions,
) -> String {
    // Convert parsed comments to CommentInfo, sorted by line number
    let mut comment_infos: Vec<CommentInfo> = comments
        .iter()
        .map(|c| CommentInfo {
            text: c.text.clone(),
            line: c.line,
        })
        .collect();
    comment_infos.sort_by_key(|c| c.line);

    // Create visitor with comments
    let mut visitor = FormatVisitor::with_comments(options, comment_infos);

    // Format using the visitor
    visitor.enter_stored_definition(def);
    let class_count = def.class_list.len();
    for (i, class) in def.class_list.values().enumerate() {
        let is_last = i == class_count - 1;
        format_class_with_comments(&mut visitor, class, !is_last);
    }
    visitor.exit_stored_definition(def);

    // Emit any remaining comments at end
    visitor.emit_remaining_comments();

    visitor.output
}

/// Format a class definition with comment insertion
///
/// `add_trailing_blanks` - if true, adds blank lines after this class ends (for spacing between classes)
fn format_class_with_comments(
    visitor: &mut FormatVisitor,
    class: &ClassDefinition,
    add_trailing_blanks: bool,
) {
    // Get the class's starting line from its name token
    let class_line = class.name.location.start_line;

    // Emit any comments that should appear before this class
    visitor.emit_comments_before_line(class_line);

    // Format the class header
    let class_keyword = match class.class_type {
        ClassType::Model => "model",
        ClassType::Class => "class",
        ClassType::Block => "block",
        ClassType::Connector => "connector",
        ClassType::Record => "record",
        ClassType::Type => "type",
        ClassType::Package => "package",
        ClassType::Function => "function",
        ClassType::Operator => "operator",
    };

    let encapsulated = if class.encapsulated {
        "encapsulated "
    } else {
        ""
    };
    visitor.writeln(&format!(
        "{}{} {}",
        encapsulated, class_keyword, class.name.text
    ));
    visitor.indent_level += 1;

    // Extends
    for ext in &class.extends {
        let ext_line = ext.location.start_line;
        visitor.emit_comments_before_line(ext_line);
        visitor.writeln(&format!("extends {};", ext.comp));
    }

    // Imports
    for import in &class.imports {
        let import_line = import.location().start_line;
        visitor.emit_comments_before_line(import_line);
        visitor.writeln(&visitor.format_import(import));
    }

    // Components - group by source line to preserve combined declarations like "Real x, y, z;"
    let components: Vec<&Component> = class.components.values().collect();
    let mut i = 0;
    while i < components.len() {
        let comp = components[i];
        let comp_line = comp.location.start_line;
        visitor.emit_comments_before_line(comp_line);

        // Check if this component can be grouped with following ones
        // Components can be grouped if:
        // 1. They're on the same source line
        // 2. They have the same type, variability, causality, and connection
        // 3. None of them have individual attributes (descriptions, annotations, start values, modifications)
        if !visitor.component_has_individual_attrs(comp) {
            let mut group: Vec<&Component> = vec![comp];
            let mut j = i + 1;
            while j < components.len() {
                let next = components[j];
                if next.location.start_line == comp_line
                    && next.type_name == comp.type_name
                    && std::mem::discriminant(&next.variability)
                        == std::mem::discriminant(&comp.variability)
                    && std::mem::discriminant(&next.causality)
                        == std::mem::discriminant(&comp.causality)
                    && std::mem::discriminant(&next.connection)
                        == std::mem::discriminant(&comp.connection)
                    && !visitor.component_has_individual_attrs(next)
                {
                    group.push(next);
                    j += 1;
                } else {
                    break;
                }
            }

            if group.len() > 1 {
                // Output as a grouped declaration
                visitor.writeln(&visitor.format_component_group(&group));
                i = j;
                continue;
            }
        }

        // Output as individual declaration
        visitor.writeln(&visitor.format_component(comp));
        i += 1;
    }

    // Nested classes
    let nested_count = class.classes.len();
    for (i, nested) in class.classes.values().enumerate() {
        let is_last_nested = i == nested_count - 1;
        format_class_with_comments(visitor, nested, !is_last_nested);
    }

    // Equations
    if !class.equations.is_empty() {
        // Find first equation's line for comment insertion
        if let Some(first_eq) = class.equations.first() {
            if let Some(loc) = get_equation_location(first_eq) {
                // Emit comments before the equation keyword (approximate)
                visitor.emit_comments_before_line(loc.saturating_sub(1));
            }
        }
        visitor.indent_level -= 1;
        visitor.writeln("equation");
        visitor.indent_level += 1;
        for eq in &class.equations {
            if let Some(eq_line) = get_equation_location(eq) {
                visitor.emit_comments_before_line(eq_line);
            }
            let formatted = visitor.format_equation(eq, visitor.indent_level);
            visitor.write(&formatted);
        }
    }

    // Initial equations
    if !class.initial_equations.is_empty() {
        if let Some(first_eq) = class.initial_equations.first() {
            if let Some(loc) = get_equation_location(first_eq) {
                visitor.emit_comments_before_line(loc.saturating_sub(1));
            }
        }
        visitor.indent_level -= 1;
        visitor.writeln("initial equation");
        visitor.indent_level += 1;
        for eq in &class.initial_equations {
            if let Some(eq_line) = get_equation_location(eq) {
                visitor.emit_comments_before_line(eq_line);
            }
            let formatted = visitor.format_equation(eq, visitor.indent_level);
            visitor.write(&formatted);
        }
    }

    // Algorithms
    for algo in &class.algorithms {
        visitor.indent_level -= 1;
        visitor.writeln("algorithm");
        visitor.indent_level += 1;
        for stmt in algo {
            if let Some(stmt_line) = get_statement_location(stmt) {
                visitor.emit_comments_before_line(stmt_line);
            }
            let formatted = visitor.format_statement(stmt, visitor.indent_level);
            visitor.write(&formatted);
        }
    }

    // Initial algorithms
    for algo in &class.initial_algorithms {
        visitor.indent_level -= 1;
        visitor.writeln("initial algorithm");
        visitor.indent_level += 1;
        for stmt in algo {
            if let Some(stmt_line) = get_statement_location(stmt) {
                visitor.emit_comments_before_line(stmt_line);
            }
            let formatted = visitor.format_statement(stmt, visitor.indent_level);
            visitor.write(&formatted);
        }
    }

    // End class - emit any remaining comments for this class before end
    visitor.indent_level -= 1;
    visitor.writeln(&format!("end {};", class.name.text));

    // Add blank lines after this class if requested (for spacing between classes)
    if add_trailing_blanks {
        for _ in 0..visitor.options.blank_lines_between_classes {
            visitor.write("\n");
        }
    }
}

/// Get the source line number of an equation
fn get_equation_location(eq: &Equation) -> Option<u32> {
    match eq {
        Equation::Empty => None,
        Equation::Simple { lhs, .. } => lhs.get_location().map(|l| l.start_line),
        Equation::Connect { lhs, .. } => Some(lhs.parts.first()?.ident.location.start_line),
        Equation::For { indices, .. } => Some(indices.first()?.ident.location.start_line),
        Equation::When(blocks) => blocks
            .first()
            .and_then(|b| b.cond.get_location())
            .map(|l| l.start_line),
        Equation::If { cond_blocks, .. } => cond_blocks
            .first()
            .and_then(|b| b.cond.get_location())
            .map(|l| l.start_line),
        Equation::FunctionCall { comp, .. } => Some(comp.parts.first()?.ident.location.start_line),
    }
}

/// Get the source line number of a statement
fn get_statement_location(stmt: &Statement) -> Option<u32> {
    match stmt {
        Statement::Empty => None,
        Statement::Assignment { comp, .. } => Some(comp.parts.first()?.ident.location.start_line),
        Statement::FunctionCall { comp, .. } => Some(comp.parts.first()?.ident.location.start_line),
        Statement::For { indices, .. } => Some(indices.first()?.ident.location.start_line),
        Statement::While(block) => block.cond.get_location().map(|l| l.start_line),
        Statement::If { cond_blocks, .. } => cond_blocks
            .first()
            .and_then(|b| b.cond.get_location())
            .map(|l| l.start_line),
        Statement::When(blocks) => blocks
            .first()
            .and_then(|b| b.cond.get_location())
            .map(|l| l.start_line),
        Statement::Return { token } => Some(token.location.start_line),
        Statement::Break { token } => Some(token.location.start_line),
    }
}

/// Simple fallback formatter for when AST parsing fails
fn format_modelica_fallback(text: &str, options: &FormatOptions) -> String {
    let indent_str = if options.use_tabs {
        "\t".to_string()
    } else {
        " ".repeat(options.indent_size)
    };

    let mut result = String::new();
    let mut indent_level: i32 = 0;
    let mut prev_was_empty = false;

    for line in text.lines() {
        let trimmed = line.trim();

        // Skip multiple consecutive empty lines
        if trimmed.is_empty() {
            if !prev_was_empty {
                result.push('\n');
                prev_was_empty = true;
            }
            continue;
        }
        prev_was_empty = false;

        // Decrease indent before certain keywords
        if should_decrease_indent_before(trimmed) {
            indent_level = (indent_level - 1).max(0);
        }

        // Add indentation and line
        let indent = indent_str.repeat(indent_level as usize);
        result.push_str(&indent);
        result.push_str(trimmed);
        result.push('\n');

        // Increase indent after certain keywords
        if should_increase_indent_after(trimmed) {
            indent_level += 1;
        }
    }

    // Remove trailing newline if original didn't have one
    if !text.ends_with('\n') && result.ends_with('\n') {
        result.pop();
    }

    result
}

fn should_decrease_indent_before(line: &str) -> bool {
    let keywords = [
        "end ",
        "end;",
        "else",
        "elseif",
        "elsewhen",
        "protected",
        "public",
        "equation",
        "initial equation",
        "algorithm",
        "initial algorithm",
    ];
    keywords.iter().any(|k| line.starts_with(k))
}

fn should_increase_indent_after(line: &str) -> bool {
    // Class/model/function declarations
    if (line.starts_with("model ")
        || line.starts_with("class ")
        || line.starts_with("function ")
        || line.starts_with("record ")
        || line.starts_with("connector ")
        || line.starts_with("package ")
        || line.starts_with("block ")
        || line.starts_with("type ")
        || line.starts_with("operator "))
        && !line.contains("end ")
        && !line.ends_with(';')
    {
        return true;
    }

    if line.starts_with("partial ") && !line.contains("end ") && !line.ends_with(';') {
        return true;
    }

    let section_keywords = [
        "equation",
        "initial equation",
        "algorithm",
        "initial algorithm",
        "protected",
        "public",
    ];
    if section_keywords
        .iter()
        .any(|k| line == *k || line.starts_with(&format!("{} ", k)))
    {
        return true;
    }

    if (line.starts_with("if ")
        || line.starts_with("for ")
        || line.starts_with("while ")
        || line.starts_with("when "))
        && (line.ends_with("then") || line.ends_with("loop"))
    {
        return true;
    }

    if line == "else" || line.starts_with("elseif ") || line.starts_with("elsewhen ") {
        return true;
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_simple_model() {
        let input = "model Test\nReal x;\nend Test;";
        let result = format_modelica(input, &FormatOptions::default());
        assert!(result.contains("model Test\n"));
        assert!(result.contains("Real x"));
        assert!(result.contains("end Test;"));
        // Check indentation
        assert!(result.contains("  Real"));
    }

    #[test]
    fn test_format_with_tabs() {
        let input = "model Test\nReal x;\nend Test;";
        let result = format_modelica(input, &FormatOptions::with_tabs());
        assert!(result.contains("\tReal x"));
    }

    #[test]
    fn test_format_equation_with_operators() {
        let input = "model Test\nReal x;\nequation\nx=1+2*3;\nend Test;";
        let result = format_modelica(input, &FormatOptions::default());
        assert!(result.contains("x = 1 + 2 * 3;"));
    }

    #[test]
    fn test_format_multiline_array() {
        let input = r#"model Test
Real v[3];
equation
v = {1.0, 2.0, 3.0};
end Test;"#;
        let result = format_modelica(input, &FormatOptions::default());
        // Should format array across multiple lines (3 elements)
        assert!(result.contains("{\n"));
    }

    #[test]
    fn test_format_if_equation() {
        let input = r#"model Test
Real x;
equation
if x > 0 then
x = 1;
else
x = 0;
end if;
end Test;"#;
        let result = format_modelica(input, &FormatOptions::default());
        assert!(result.contains("if x > 0 then\n"));
        assert!(result.contains("else\n"));
        assert!(result.contains("end if;\n"));
    }

    #[test]
    fn test_format_preserves_component_annotations() {
        let input = r#"model Test
Real x annotation(Dialog(group="Test"));
end Test;"#;
        let result = format_modelica(input, &FormatOptions::default());
        // Should preserve the annotation
        assert!(
            result.contains("annotation("),
            "Result should contain annotation: {}",
            result
        );
        assert!(
            result.contains("Dialog"),
            "Result should contain Dialog: {}",
            result
        );
    }

    #[test]
    fn test_format_blank_lines_between_classes() {
        let input = r#"model A
Real x;
end A;
model B
Real y;
end B;
model C
Real z;
end C;"#;
        // Default should add 1 blank line between classes
        let result = format_modelica(input, &FormatOptions::default());
        assert!(
            result.contains("end A;\n\nmodel B"),
            "Should have blank line between A and B: {}",
            result
        );
        assert!(
            result.contains("end B;\n\nmodel C"),
            "Should have blank line between B and C: {}",
            result
        );

        // Test with 0 blank lines (no spacing)
        let mut options = FormatOptions::default();
        options.blank_lines_between_classes = 0;
        let result = format_modelica(input, &options);
        assert!(
            result.contains("end A;\nmodel B"),
            "Should have no blank line between A and B: {}",
            result
        );

        // Test with 2 blank lines
        options.blank_lines_between_classes = 2;
        let result = format_modelica(input, &options);
        assert!(
            result.contains("end A;\n\n\nmodel B"),
            "Should have 2 blank lines between A and B: {}",
            result
        );
    }

    #[test]
    fn test_format_preserves_grouped_declarations() {
        // Test that grouped variable declarations are preserved
        let input = r#"model Test
  Real x, y, z;
  parameter Real a, b;
  Motor m1, m2, m3;
end Test;"#;
        let result = format_modelica(input, &FormatOptions::default());
        assert!(
            result.contains("Real x, y, z;"),
            "Should preserve grouped Real declaration: {}",
            result
        );
        assert!(
            result.contains("parameter Real a, b;"),
            "Should preserve grouped parameter declaration: {}",
            result
        );
        assert!(
            result.contains("Motor m1, m2, m3;"),
            "Should preserve grouped Motor declaration: {}",
            result
        );
    }

    #[test]
    fn test_format_does_not_group_with_attributes() {
        // Test that declarations with individual attributes are NOT grouped
        let input = r#"model Test
  Real x "description";
  Real y;
end Test;"#;
        let result = format_modelica(input, &FormatOptions::default());
        // Should output them separately since x has a description
        assert!(
            result.contains("Real x \"description\";"),
            "Should preserve individual declaration with description: {}",
            result
        );
        assert!(
            result.contains("Real y;"),
            "Should have separate declaration: {}",
            result
        );
    }

    #[test]
    fn test_format_preserves_necessary_parentheses() {
        // Test that parentheses are preserved when they affect expression meaning
        let input = r#"model Test
  Real x, y, z;
equation
  // Lower precedence inside higher precedence needs parens
  x = (a + b) * c;
  // Subtraction with lower precedence terms
  y = -(a - b) * c;
  // Nested lower precedence
  z = (a + b) * (c + d);
end Test;"#;
        let result = format_modelica(input, &FormatOptions::default());

        // (a + b) * c - parens needed because + has lower precedence than *
        assert!(
            result.contains("(a + b) * c"),
            "Should preserve parens for (a + b) * c: {}",
            result
        );

        // -(a - b) * c - complex unary with parens
        assert!(
            result.contains("-(a - b) * c"),
            "Should preserve parens for -(a - b) * c: {}",
            result
        );

        // (a + b) * (c + d) - both sides need parens
        assert!(
            result.contains("(a + b) * (c + d)"),
            "Should preserve parens for (a + b) * (c + d): {}",
            result
        );
    }

    #[test]
    fn test_format_removes_redundant_parentheses() {
        // Test that redundant parentheses are removed (higher precedence inside lower)
        let input = r#"model Test
  Real x;
equation
  // Higher precedence inside lower - parens are redundant
  x = a + (b * c);
end Test;"#;
        let result = format_modelica(input, &FormatOptions::default());

        // a + (b * c) becomes a + b * c because * binds tighter than +
        assert!(
            result.contains("a + b * c"),
            "Should remove redundant parens from a + (b * c): {}",
            result
        );
    }
}
