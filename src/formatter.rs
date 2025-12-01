//! Modelica code formatter.
//!
//! Provides code formatting support:
//! - Consistent indentation (2 or 4 spaces, or tabs)
//! - Proper spacing around operators
//! - Normalized line endings
//! - Multiple consecutive empty lines collapsed to one

/// Formatting options for Modelica code
#[derive(Debug, Clone)]
pub struct FormatOptions {
    /// Number of spaces per indentation level (ignored if use_tabs is true)
    pub indent_size: usize,
    /// Use tabs instead of spaces for indentation
    pub use_tabs: bool,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            indent_size: 2,
            use_tabs: false,
        }
    }
}

impl FormatOptions {
    /// Create options with specified indent size using spaces
    pub fn with_spaces(indent_size: usize) -> Self {
        Self {
            indent_size,
            use_tabs: false,
        }
    }

    /// Create options using tabs for indentation
    pub fn with_tabs() -> Self {
        Self {
            indent_size: 1,
            use_tabs: true,
        }
    }
}

/// Format Modelica code according to style guidelines
pub fn format_modelica(text: &str, options: &FormatOptions) -> String {
    let indent_str = if options.use_tabs {
        "\t".to_string()
    } else {
        " ".repeat(options.indent_size)
    };

    let mut result = String::new();
    let mut indent_level: i32 = 0;
    let mut in_multiline_comment = false;
    let mut prev_was_empty = false;

    for line in text.lines() {
        let trimmed = line.trim();

        // Handle multi-line comments
        if in_multiline_comment {
            result.push_str(&format_comment_line(trimmed, indent_level, &indent_str));
            result.push('\n');
            if trimmed.contains("*/") {
                in_multiline_comment = false;
            }
            prev_was_empty = false;
            continue;
        }

        if trimmed.contains("/*") && !trimmed.contains("*/") {
            in_multiline_comment = true;
        }

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

        // Format the line
        let formatted_line = format_line(trimmed, indent_level, &indent_str);
        result.push_str(&formatted_line);
        result.push('\n');

        // Increase indent after certain keywords
        if should_increase_indent_after(trimmed) {
            indent_level += 1;
        }

        // Decrease indent after 'end' statements
        if should_decrease_indent_after(trimmed) {
            indent_level = (indent_level - 1).max(0);
        }
    }

    // Remove trailing newline if original didn't have one
    if !text.ends_with('\n') && result.ends_with('\n') {
        result.pop();
    }

    result
}

/// Check if we should decrease indent before this line
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

/// Check if we should increase indent after this line
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
    {
        return true;
    }

    // Partial classes
    if line.starts_with("partial ") && !line.contains("end ") {
        return true;
    }

    // Section keywords
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

    // Control structures (if not single-line)
    if (line.starts_with("if ")
        || line.starts_with("for ")
        || line.starts_with("while ")
        || line.starts_with("when "))
        && line.ends_with("then")
        || line.ends_with("loop")
    {
        return true;
    }

    // else/elseif/elsewhen increase indent for their block
    if line == "else" || line.starts_with("elseif ") || line.starts_with("elsewhen ") {
        return true;
    }

    false
}

/// Check if we should decrease indent after this line
fn should_decrease_indent_after(_line: &str) -> bool {
    // Currently no lines need to decrease indent after themselves.
    // The 'end' statements are handled by should_decrease_indent_before.
    // Section keywords (equation, algorithm) increase indent, and their
    // corresponding 'end ClassName;' decreases it via should_decrease_indent_before.
    false
}

/// Format a single line with proper indentation and spacing
fn format_line(line: &str, indent_level: i32, indent_str: &str) -> String {
    let indent = indent_str.repeat(indent_level as usize);

    // Don't format comment lines (except indentation)
    if line.starts_with("//") || line.starts_with("/*") {
        return format!("{}{}", indent, line);
    }

    let formatted = format_operators(line);
    format!("{}{}", indent, formatted)
}

/// Format a comment line (preserve content, fix indentation)
fn format_comment_line(line: &str, indent_level: i32, indent_str: &str) -> String {
    let indent = indent_str.repeat(indent_level as usize);
    format!("{}{}", indent, line)
}

/// Add proper spacing around operators
fn format_operators(line: &str) -> String {
    let mut result = String::new();
    let chars: Vec<char> = line.chars().collect();
    let len = chars.len();
    let mut i = 0;
    let mut in_string = false;

    while i < len {
        let c = chars[i];

        // Track string literals
        if c == '"' && (i == 0 || chars[i - 1] != '\\') {
            in_string = !in_string;
            result.push(c);
            i += 1;
            continue;
        }

        // Don't modify strings
        if in_string {
            result.push(c);
            i += 1;
            continue;
        }

        // Handle multi-character operators
        let next = chars.get(i + 1).copied();

        // := assignment
        if c == ':' && next == Some('=') {
            ensure_space_before(&mut result);
            result.push_str(":=");
            i += 2;
            ensure_space_after(&mut result, chars.get(i).copied());
            continue;
        }

        // == equality
        if c == '=' && next == Some('=') {
            ensure_space_before(&mut result);
            result.push_str("==");
            i += 2;
            ensure_space_after(&mut result, chars.get(i).copied());
            continue;
        }

        // <> not equal
        if c == '<' && next == Some('>') {
            ensure_space_before(&mut result);
            result.push_str("<>");
            i += 2;
            ensure_space_after(&mut result, chars.get(i).copied());
            continue;
        }

        // <= and >=
        if (c == '<' || c == '>') && next == Some('=') {
            ensure_space_before(&mut result);
            result.push(c);
            result.push('=');
            i += 2;
            ensure_space_after(&mut result, chars.get(i).copied());
            continue;
        }

        // .+ .- .* ./ .^
        if c == '.' && next.is_some_and(|n| "+-*/^".contains(n)) {
            ensure_space_before(&mut result);
            result.push('.');
            result.push(next.unwrap());
            i += 2;
            ensure_space_after(&mut result, chars.get(i).copied());
            continue;
        }

        // Single = (but not in annotations or modifications context)
        if c == '=' && !is_in_modification_context(&result) {
            // Check if it's part of :=, ==, <=, >=, <>
            let prev = result.chars().last();
            if prev != Some(':') && prev != Some('=') && prev != Some('<') && prev != Some('>') {
                ensure_space_before(&mut result);
                result.push('=');
                i += 1;
                ensure_space_after(&mut result, chars.get(i).copied());
                continue;
            }
        }

        // Binary operators: + - * / ^
        if "+-*/^".contains(c) {
            // Check if it's a sign (unary)
            let prev_nonspace = result.trim_end().chars().last();
            let is_unary =
                prev_nonspace.is_none() || prev_nonspace.is_some_and(|p| "=(<[,;:".contains(p));

            if is_unary {
                // Unary operator - don't add space before
                result.push(c);
            } else {
                ensure_space_before(&mut result);
                result.push(c);
                ensure_space_after(&mut result, chars.get(i + 1).copied());
            }
            i += 1;
            continue;
        }

        // Comparison operators < >
        if c == '<' || c == '>' {
            ensure_space_before(&mut result);
            result.push(c);
            i += 1;
            ensure_space_after(&mut result, chars.get(i).copied());
            continue;
        }

        // Comma: space after but not before
        if c == ',' {
            // Remove trailing space before comma
            while result.ends_with(' ') {
                result.pop();
            }
            result.push(',');
            result.push(' ');
            i += 1;
            // Skip any spaces after comma in input (we already added one)
            while i < len && chars[i] == ' ' {
                i += 1;
            }
            continue;
        }

        // Semicolon: no space before
        if c == ';' {
            while result.ends_with(' ') {
                result.pop();
            }
            result.push(';');
            i += 1;
            continue;
        }

        // Colon in ranges (1:10) - no spaces
        if c == ':' && next != Some('=') {
            result.push(c);
            i += 1;
            continue;
        }

        result.push(c);
        i += 1;
    }

    // Trim trailing whitespace
    result.trim_end().to_string()
}

/// Ensure there's a space before the current position
fn ensure_space_before(result: &mut String) {
    if !result.is_empty()
        && !result.ends_with(' ')
        && !result.ends_with('\t')
        && !result.ends_with('(')
        && !result.ends_with('[')
        && !result.ends_with('{')
    {
        result.push(' ');
    }
}

/// Ensure there's a space after (by not consuming if next char is space)
fn ensure_space_after(result: &mut String, next: Option<char>) {
    if let Some(c) = next {
        if c != ' '
            && c != '\t'
            && c != ')'
            && c != ']'
            && c != '}'
            && c != ','
            && c != ';'
            && c != '\n'
        {
            result.push(' ');
        }
    }
}

/// Check if we're inside a modification context (like annotation or component modification)
fn is_in_modification_context(text: &str) -> bool {
    // Simple heuristic: count parentheses
    let open_parens = text.chars().filter(|&c| c == '(').count();
    let close_parens = text.chars().filter(|&c| c == ')').count();
    open_parens > close_parens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_simple_model() {
        let input = "model Test\nReal x;\nend Test;";
        let expected = "model Test\n  Real x;\nend Test;";
        let result = format_modelica(input, &FormatOptions::default());
        assert_eq!(result, expected);
    }

    #[test]
    fn test_format_operators() {
        let input = "x=y+z*2";
        let expected = "x = y + z * 2";
        let result = format_operators(input);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_format_assignment() {
        let input = "x:=y+1";
        let expected = "x := y + 1";
        let result = format_operators(input);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_format_comparison() {
        let input = "if x>0 and y<=10 then";
        let expected = "if x > 0 and y <= 10 then";
        let result = format_operators(input);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_preserve_string_content() {
        let input = r#"s = "hello+world""#;
        let result = format_operators(input);
        assert!(result.contains(r#""hello+world""#));
    }

    #[test]
    fn test_unary_minus() {
        let input = "x = -1";
        let result = format_operators(input);
        assert_eq!(result, "x = -1");
    }

    #[test]
    fn test_comma_spacing() {
        let input = "f(a,b,c)";
        let expected = "f(a, b, c)";
        let result = format_operators(input);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_format_with_tabs() {
        let input = "model Test\nReal x;\nend Test;";
        let expected = "model Test\n\tReal x;\nend Test;";
        let result = format_modelica(input, &FormatOptions::with_tabs());
        assert_eq!(result, expected);
    }

    #[test]
    fn test_format_with_4_spaces() {
        let input = "model Test\nReal x;\nend Test;";
        let expected = "model Test\n    Real x;\nend Test;";
        let result = format_modelica(input, &FormatOptions::with_spaces(4));
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
        let expected = r#"package test
  model BouncingBall
    Real h;
  equation
    der(h) = 1;
  end BouncingBall;
end test;

model B
  test.BouncingBall ball;
end B;"#;
        let result = format_modelica(input, &FormatOptions::default());
        assert_eq!(result, expected);
    }

    #[test]
    fn test_format_multiple_models_in_package() {
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
        let expected = r#"package test
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
        let result = format_modelica(input, &FormatOptions::default());
        assert_eq!(result, expected);
    }
}
