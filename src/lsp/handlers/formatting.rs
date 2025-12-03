//! Document Formatting handler for Modelica files.
//!
//! Provides LSP integration for code formatting.
//! The actual formatting logic is in the `fmt` module.

use std::collections::HashMap;

use lsp_types::{DocumentFormattingParams, Position, Range, TextEdit, Uri};

use crate::fmt::{FormatOptions, format_modelica};

/// Handle document formatting request
pub fn handle_formatting(
    documents: &HashMap<Uri, String>,
    params: DocumentFormattingParams,
) -> Option<Vec<TextEdit>> {
    let uri = &params.text_document.uri;
    let text = documents.get(uri)?;
    let lsp_options = &params.options;

    // Convert LSP options to our format options
    let options = FormatOptions {
        indent_size: lsp_options.tab_size as usize,
        use_tabs: !lsp_options.insert_spaces,
        max_line_length: 100,
        preserve_unformatted: true,
        blank_lines_between_classes: 1,
    };

    let formatted = format_modelica(text, &options);

    // If no changes, return empty vec
    if formatted == *text {
        return Some(vec![]);
    }

    // Return a single edit that replaces the entire document
    let lines: Vec<&str> = text.lines().collect();
    let last_line = lines.len().saturating_sub(1);
    let last_char = lines.last().map(|l| l.len()).unwrap_or(0);

    Some(vec![TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: last_line as u32,
                character: last_char as u32,
            },
        },
        new_text: formatted,
    }])
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::FormattingOptions;

    fn default_options() -> FormattingOptions {
        FormattingOptions {
            tab_size: 2,
            insert_spaces: true,
            ..Default::default()
        }
    }

    #[test]
    fn test_format_simple_model() {
        let input = "model Test\nReal x;\nend Test;";
        let expected = "model Test\n  Real x;\nend Test;\n";
        let options = FormatOptions {
            indent_size: default_options().tab_size as usize,
            use_tabs: !default_options().insert_spaces,
            max_line_length: 100,
            preserve_unformatted: true,
            blank_lines_between_classes: 1,
        };
        let result = format_modelica(input, &options);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_format_nested_package_model() {
        let input = r#"package test
model BouncingBall
Real h;
equation
der(h) = 1;
end BouncingBall;
end test;

model B
test.BouncingBall ball;
end B;"#;
        let expected = "package test\n  model BouncingBall\n    Real h;\n  equation\n    der(h) = 1;\n  end BouncingBall;\nend test;\n\nmodel B\n  test.BouncingBall ball;\nend B;\n";
        let options = FormatOptions::default();
        let result = format_modelica(input, &options);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_format_with_when_block() {
        let input = r#"model Test
Real x;
equation
der(x) = 1;
when x > 0 then
reinit(x, 0);
end when;
end Test;"#;
        let expected = "model Test\n  Real x;\nequation\n  der(x) = 1;\n  when x > 0 then\n    reinit(x, 0);\n  end when;\nend Test;\n";
        let options = FormatOptions::default();
        let result = format_modelica(input, &options);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_format_multiple_models_in_package() {
        // Test that multiple models inside a package maintain proper indentation
        let input = r#"package test
model BouncingBall
Real h;
equation
der(h) = 1;
end BouncingBall;
model Car
Real a;
equation
a = 5.0;
end Car;
end test;"#;
        let expected = "package test\n  model BouncingBall\n    Real h;\n  equation\n    der(h) = 1;\n  end BouncingBall;\n\n  model Car\n    Real a;\n  equation\n    a = 5.0;\n  end Car;\nend test;\n";
        let options = FormatOptions::default();
        let result = format_modelica(input, &options);
        assert_eq!(result, expected);
    }
}
