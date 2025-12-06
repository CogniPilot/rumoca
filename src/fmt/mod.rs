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

mod class_formatter;
mod fallback;
mod operators;
mod options;
mod visitor;

pub use options::{CONFIG_FILE_NAMES, FormatOptions};

use crate::ir::ast::{Expression, StoredDefinition};
use class_formatter::format_class_with_comments;
use fallback::format_modelica_fallback;
use visitor::{CommentInfo, FormatVisitor};

/// Format Modelica code from an AST using the visitor pattern
pub fn format_ast(def: &StoredDefinition, options: &FormatOptions) -> String {
    use crate::ir::visitor::Visitor;

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
                format_ast_with_comments(&ast, &grammar.comments, text, options)
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

/// Format a single expression to a string
/// Useful for displaying expression values in hover info, etc.
pub fn format_expression(expr: &Expression) -> String {
    let visitor = FormatVisitor::new(&FormatOptions::default());
    visitor.format_expression(expr)
}

/// Format a single equation to a string
/// Useful for displaying equations in hover info, etc.
pub fn format_equation(eq: &crate::ir::ast::Equation) -> String {
    let visitor = FormatVisitor::new(&FormatOptions::default());
    visitor.format_equation(eq, 0)
}

/// Format AST with comments reinserted at their original locations
fn format_ast_with_comments(
    def: &StoredDefinition,
    comments: &[crate::modelica_grammar::ParsedComment],
    source: &str,
    options: &FormatOptions,
) -> String {
    use crate::ir::visitor::Visitor;

    // Convert parsed comments to CommentInfo, sorted by line number
    let mut comment_infos: Vec<CommentInfo> = comments
        .iter()
        .map(|c| CommentInfo {
            text: c.text.clone(),
            line: c.line,
        })
        .collect();
    comment_infos.sort_by_key(|c| c.line);

    // Create visitor with comments and source text for exact token preservation
    let mut visitor = FormatVisitor::with_comments_and_source(options, comment_infos, source);

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
        let options = FormatOptions {
            blank_lines_between_classes: 0,
            ..Default::default()
        };
        let result = format_modelica(input, &options);
        assert!(
            result.contains("end A;\nmodel B"),
            "Should have no blank line between A and B: {}",
            result
        );

        // Test with 2 blank lines
        let options = FormatOptions {
            blank_lines_between_classes: 2,
            ..Default::default()
        };
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
    fn test_format_preserves_source_parentheses() {
        // Test that source parentheses are preserved even if technically redundant
        let input = r#"model Test
  Real x;
equation
  // Higher precedence inside lower - parens preserved from source
  x = a + (b * c);
end Test;"#;
        let result = format_modelica(input, &FormatOptions::default());

        // Parentheses are now preserved from source for clarity
        assert!(
            result.contains("a + (b * c)"),
            "Should preserve source parens in a + (b * c): {}",
            result
        );
    }
}
