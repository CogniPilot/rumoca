//! Fallback formatter for when AST parsing fails.

use super::FormatOptions;

/// Simple fallback formatter for when AST parsing fails
pub fn format_modelica_fallback(text: &str, options: &FormatOptions) -> String {
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
