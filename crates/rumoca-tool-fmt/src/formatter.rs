//! Modelica formatter implementation.

use crate::format_errors::FormatError;
use crate::format_options::{FormatOptions, FormatProfile};

use rumoca_compile::parsing::validate_source_syntax;

/// Format Modelica source code.
pub fn format(source: &str, options: &FormatOptions) -> Result<String, FormatError> {
    format_with_source_name(source, options, "<format>")
}

/// Format Modelica source code with explicit source name.
pub fn format_with_source_name(
    source: &str,
    options: &FormatOptions,
    source_name: &str,
) -> Result<String, FormatError> {
    if let Err(e) = validate_source_syntax(source, source_name) {
        return Err(FormatError::SyntaxError(e.to_string()));
    }
    Ok(format_source(source, options))
}

/// Format source, returning original on syntax error.
pub fn format_or_original(source: &str, options: &FormatOptions) -> String {
    format(source, options).unwrap_or_else(|_| source.to_string())
}

/// Format source with explicit source name, returning original on syntax error.
pub fn format_or_original_with_source_name(
    source: &str,
    options: &FormatOptions,
    source_name: &str,
) -> String {
    format_with_source_name(source, options, source_name).unwrap_or_else(|_| source.to_string())
}

fn format_source(source: &str, options: &FormatOptions) -> String {
    let rules = style_rules(options.profile);
    let mut result = String::with_capacity(source.len());
    let indent_str = if options.use_tabs {
        "\t".to_string()
    } else {
        " ".repeat(options.indent_size)
    };

    let mut indent_level: usize = 0;
    let mut in_string = false;
    let mut prev_char = '\0';

    for line in source.lines() {
        let line_no_trailing = if options.trim_trailing_whitespace {
            line.trim_end_matches([' ', '\t'])
        } else {
            line
        };
        let trimmed = line_no_trailing.trim();

        if trimmed.is_empty() {
            result.push('\n');
            continue;
        }

        let lower = if in_string {
            String::new()
        } else {
            trimmed.to_ascii_lowercase()
        };

        if !in_string
            && rules.normalize_indentation
            && (is_end_keyword(&lower)
                || is_branch_keyword(&lower)
                || (rules.dedent_section_headers && is_section_header_keyword(&lower)))
        {
            indent_level = indent_level.saturating_sub(1);
        }

        if rules.normalize_indentation {
            for _ in 0..indent_level {
                result.push_str(&indent_str);
            }
        } else {
            let leading_len = line_no_trailing.len() - line_no_trailing.trim_start().len();
            result.push_str(&line_no_trailing[..leading_len]);
        }

        let line_content = if rules.normalize_indentation {
            trimmed
        } else {
            line_no_trailing.trim_start()
        };
        let (formatted_line, next_in_string, next_prev_char) =
            format_line(line_content, rules, in_string, prev_char);
        result.push_str(&formatted_line);
        result.push('\n');
        in_string = next_in_string;
        prev_char = next_prev_char;

        if !in_string
            && rules.normalize_indentation
            && (is_block_header_keyword(&lower)
                || is_branch_keyword(&lower)
                || is_section_header_keyword(&lower))
        {
            indent_level += 1;
        }
    }

    if !options.insert_final_newline && !source.ends_with('\n') {
        let _ = result.pop();
    }

    result
}

#[derive(Clone, Copy)]
struct StyleRules {
    normalize_indentation: bool,
    dedent_section_headers: bool,
    normalize_operator_spacing: bool,
    tighten_unary_sign_spacing: bool,
}

fn style_rules(profile: FormatProfile) -> StyleRules {
    match profile {
        // MSL profile: close to existing MSL whitespace while using full formatting path.
        FormatProfile::Msl => StyleRules {
            normalize_indentation: false,
            dedent_section_headers: false,
            normalize_operator_spacing: true,
            tighten_unary_sign_spacing: false,
        },
        // Canonical profile: fully normalize indentation and operator spacing.
        FormatProfile::Canonical => StyleRules {
            normalize_indentation: true,
            dedent_section_headers: true,
            normalize_operator_spacing: true,
            tighten_unary_sign_spacing: true,
        },
    }
}

fn needs_space_before_eq(prev_char: char, result: &str) -> bool {
    !matches!(prev_char, ':' | '=' | '<' | '>') && !result.ends_with(' ') && !result.is_empty()
}

fn needs_space_after_eq(next: Option<&char>) -> bool {
    matches!(next, Some(&c) if c != '=' && c != ' ')
}

fn needs_space_before_arith(prev_char: char, result: &str) -> bool {
    !matches!(prev_char, 'e' | 'E' | '(') && !result.ends_with(' ') && !result.is_empty()
}

fn needs_space_after_arith(next: Option<&char>) -> bool {
    matches!(next, Some(&c) if !matches!(c, ' ' | ')' | ';' | ','))
}

fn needs_space_after_comma(next: Option<&char>) -> bool {
    matches!(next, Some(&c) if c != ' ')
}

fn format_line(
    line: &str,
    rules: StyleRules,
    mut in_string: bool,
    mut prev_char: char,
) -> (String, bool, char) {
    let mut result = String::with_capacity(line.len());
    let mut chars = line.chars().peekable();
    let mut in_quoted_identifier = false;

    while let Some(c) = chars.next() {
        if !in_quoted_identifier && c == '"' && prev_char != '\\' {
            in_string = !in_string;
            result.push(c);
            prev_char = c;
            continue;
        }

        if !in_string && c == '\'' {
            in_quoted_identifier = !in_quoted_identifier;
            result.push(c);
            prev_char = c;
            continue;
        }

        if in_string || in_quoted_identifier {
            result.push(c);
            prev_char = c;
            continue;
        }

        if !rules.normalize_operator_spacing {
            result.push(c);
            prev_char = c;
            continue;
        }

        match c {
            '=' => {
                if needs_space_before_eq(prev_char, &result) {
                    result.push(' ');
                }
                result.push(c);
                if needs_space_after_eq(chars.peek()) {
                    result.push(' ');
                }
            }
            '+' | '-' | '*' | '/' => {
                let unary_sign = is_unary_sign(c, prev_char, prev_non_space_char(&result));
                if !unary_sign && needs_space_before_arith(prev_char, &result) {
                    result.push(' ');
                }
                result.push(c);
                if !unary_sign && needs_space_after_arith(chars.peek()) {
                    result.push(' ');
                } else if unary_sign && rules.tighten_unary_sign_spacing {
                    skip_horizontal_whitespace(&mut chars);
                }
            }
            ',' => {
                result.push(c);
                if needs_space_after_comma(chars.peek()) {
                    result.push(' ');
                }
            }
            _ => result.push(c),
        }

        prev_char = c;
    }

    (result, in_string, prev_char)
}

fn is_end_keyword(lower: &str) -> bool {
    lower.starts_with("end ") || lower == "end;"
}

fn is_branch_keyword(lower: &str) -> bool {
    lower.starts_with("else") || lower.starts_with("elseif")
}

fn is_section_header_keyword(lower: &str) -> bool {
    lower.starts_with("equation")
        || lower.starts_with("algorithm")
        || lower.starts_with("initial equation")
        || lower.starts_with("initial algorithm")
        || lower.starts_with("public")
        || lower.starts_with("protected")
}

fn is_block_header_keyword(lower: &str) -> bool {
    lower.starts_with("model ")
        || lower.starts_with("class ")
        || lower.starts_with("block ")
        || lower.starts_with("connector ")
        || lower.starts_with("record ")
        || lower.starts_with("type ")
        || lower.starts_with("package ")
        || lower.starts_with("function ")
        || lower.starts_with("if ")
        || lower.starts_with("for ")
        || lower.starts_with("while ")
        || lower.starts_with("when ")
}

fn prev_non_space_char(result: &str) -> Option<char> {
    result.chars().rev().find(|c| !c.is_whitespace())
}

fn is_unary_sign(current: char, prev_char: char, prev_non_space: Option<char>) -> bool {
    if !matches!(current, '+' | '-') {
        return false;
    }
    if matches!(prev_char, 'e' | 'E') {
        return true;
    }
    matches!(
        prev_non_space,
        None | Some('(' | '[' | '{' | ',' | '=' | ':' | '+' | '-' | '*' | '/' | '^' | '<' | '>')
    )
}

fn skip_horizontal_whitespace(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) {
    while matches!(chars.peek(), Some(' ' | '\t')) {
        let _ = chars.next();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_error_uses_explicit_source_name() {
        let source = "model M Real x end M;";
        let err = format_with_source_name(source, &FormatOptions::default(), "test_input.mo")
            .expect_err("expected syntax error");
        let rendered = err.to_string();
        assert!(rendered.contains("test_input.mo"));
    }

    #[test]
    fn test_format_ball_model_canonical_profile() {
        let source = r#"model Ball
  Real x(start = 10);
  Real v;
  equation
    der(x) = 2 * v;
    der(v) = - 9.8;
    when (x < 0) then
      reinit(v, - 0.8 * pre(v));
    end when;
  end Ball;
"#;
        let expected = r#"model Ball
  Real x(start = 10);
  Real v;
equation
  der(x) = 2 * v;
  der(v) = -9.8;
  when (x < 0) then
    reinit(v, -0.8 * pre(v));
  end when;
end Ball;
"#;
        let formatted = format(
            source,
            &FormatOptions {
                profile: FormatProfile::Canonical,
                ..FormatOptions::default()
            },
        )
        .expect("format");
        assert_eq!(formatted, expected);
    }

    #[test]
    fn test_format_ball_model_msl_profile_preserves_section_indent_and_unary_spacing() {
        let source = r#"model Ball
  Real x(start = 10);
  Real v;
  equation
    der(x) = 2 * v;
    der(v) = - 9.8;
    when (x < 0) then
      reinit(v, - 0.8 * pre(v));
    end when;
  end Ball;
"#;
        let formatted = format(source, &FormatOptions::default()).expect("format");
        assert_eq!(formatted, source);
    }

    #[test]
    fn test_multiline_string_is_preserved() {
        let source = r#"model C
  annotation(Documentation(info="<html>
<p>This function returns <em>re</em> and <em>im</em>.</p>
</html>"));
end C;
"#;
        let formatted = format(source, &FormatOptions::default()).expect("format");
        assert!(formatted.contains("info = \"<html>"));
        assert!(formatted.contains("<p>This function returns <em>re</em> and <em>im</em>.</p>"));
        assert!(formatted.contains("</html>"));
    }

    #[test]
    fn test_quoted_operator_identifier_is_preserved() {
        let source = r#"operator '-'
end '-';
"#;
        let formatted = format(source, &FormatOptions::default()).expect("format");
        assert_eq!(formatted, source);
    }
}
