//! Modifier completion handling.
//!
//! Provides completions for component modifiers inside parentheses.

use lsp_types::{CompletionItem, CompletionItemKind, InsertTextFormat};

use crate::ir::ast::{StoredDefinition, Variability};

use super::members::find_class_by_name;

/// Check if we're in a modifier context and return appropriate completions
///
/// Detects patterns like:
/// - `Real x(` - just opened paren (trigger: '(')
/// - `Real x(start=1,` - after comma (trigger: ',')
/// - `Real x(start=1, ` - after comma with space
/// - `Real x(st` - typing a modifier name
/// - `test.BouncingBall ball(` - class instance with member modifiers
pub fn get_modifier_completions(
    text_before: &str,
    ast: Option<&StoredDefinition>,
) -> Option<Vec<CompletionItem>> {
    // Find the last unmatched opening parenthesis
    let mut paren_depth = 0;
    let mut last_open_paren_pos = None;

    for (i, c) in text_before.char_indices() {
        match c {
            '(' => {
                paren_depth += 1;
                last_open_paren_pos = Some(i);
            }
            ')' => {
                paren_depth -= 1;
                if paren_depth <= 0 {
                    last_open_paren_pos = None;
                    paren_depth = 0;
                }
            }
            _ => {}
        }
    }

    // If we're not inside parentheses, no modifier completions
    let open_pos = last_open_paren_pos?;

    // Check if this looks like a modifier context (Type name( pattern)
    let before_paren = &text_before[..open_pos];
    let type_name = extract_type_from_modifier_context(before_paren)?;

    // Get what's after the opening paren
    let after_paren = &text_before[open_pos + 1..];

    // Determine what position we're at within the modifier list
    // Find the last comma to see what we're currently typing
    let last_comma_pos = after_paren.rfind(',');

    let current_part = match last_comma_pos {
        Some(pos) => &after_paren[pos + 1..],
        None => after_paren,
    };

    let current_trimmed = current_part.trim();

    // Show modifier completions if:
    // 1. Just after '(' - empty after paren
    // 2. Just after ',' - current part is empty or whitespace only
    // 3. Typing a modifier name - no '=' in current part yet
    // 4. After a complete modifier value - ends with a value (not '=')
    let should_show = after_paren.is_empty()                           // Just typed '('
        || current_trimmed.is_empty()                                   // Just typed ',' (with optional space)
        || !current_trimmed.contains('=')                               // Typing modifier name
        || (current_trimmed.contains('=') && {                          // After modifier value
            // Check we're not in the middle of typing the value
            let after_eq = current_trimmed.split('=').next_back().unwrap_or("").trim();
            !after_eq.is_empty() && text_before.ends_with(' ')
        });

    if should_show {
        let mut items = Vec::new();

        // Check if the type is a primitive type - if so, add standard modifiers
        if is_primitive_type(&type_name) {
            items.extend(get_modifier_items());
        } else if let Some(ast) = ast {
            // For class types, add member overrides from the class definition
            if let Some(type_class) = find_class_by_name(ast, &type_name) {
                items.extend(get_class_modifier_completions(type_class));
            }
            // Also add standard modifiers that apply to any component (like each, redeclare, final)
            items.extend(get_general_modifier_items());
        }

        if !items.is_empty() { Some(items) } else { None }
    } else {
        None
    }
}

/// Check if a type name is a primitive/built-in type
fn is_primitive_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "Real" | "Integer" | "Boolean" | "String" | "StateSelect" | "ExternalObject"
    )
}

/// Extract the type name from a modifier context
/// e.g., "Real x" -> "Real", "test.BouncingBall ball" -> "test.BouncingBall"
fn extract_type_from_modifier_context(before_paren: &str) -> Option<String> {
    let trimmed = before_paren.trim_end();

    // Must have at least a type and a name
    let parts: Vec<&str> = trimmed.split_whitespace().collect();
    if parts.len() < 2 {
        return None;
    }

    // Look for the type in the parts (skipping modifiers like parameter, constant, etc.)
    let modifiers = [
        "parameter",
        "constant",
        "input",
        "output",
        "flow",
        "stream",
        "discrete",
        "final",
        "replaceable",
        "redeclare",
        "inner",
        "outer",
    ];

    for (i, part) in parts.iter().enumerate() {
        // Skip known modifiers
        if modifiers.contains(part) {
            continue;
        }

        // Check if this looks like a type (followed by a variable name)
        if i + 1 < parts.len() {
            let next_part = parts[i + 1];
            // Type followed by variable name pattern
            // Handle array types like "Real[3]"
            let base_type = if let Some(bracket_pos) = part.find('[') {
                &part[..bracket_pos]
            } else {
                part
            };

            // Check if it looks like a type (starts with uppercase or is qualified like pkg.Type)
            let is_type = base_type.chars().next().is_some_and(|c| c.is_uppercase())
                || base_type.contains('.');

            // Check if next part looks like a variable name (starts with lowercase or underscore)
            let is_var_name = next_part
                .chars()
                .next()
                .is_some_and(|c| c.is_lowercase() || c == '_');

            if is_type && is_var_name {
                // Handle array types - return base type without dimensions
                return Some(base_type.to_string());
            }
        }
    }

    None
}

/// Get completion items for class member modifiers (for class instance modifications)
fn get_class_modifier_completions(
    type_class: &crate::ir::ast::ClassDefinition,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    for (member_name, member) in &type_class.components {
        // Create a snippet for the member modification
        let default_value = match member.type_name.to_string().as_str() {
            "Real" => "0.0",
            "Integer" => "0",
            "Boolean" => "false",
            "String" => "\"\"",
            _ => "...",
        };

        let snippet = format!("{} = ${{1:{}}}", member_name, default_value);

        let kind = match member.variability {
            Variability::Parameter(_) => CompletionItemKind::CONSTANT,
            Variability::Constant(_) => CompletionItemKind::CONSTANT,
            _ => CompletionItemKind::FIELD,
        };

        let mut detail = member.type_name.to_string();
        if !member.shape.is_empty() {
            detail += &format!(
                "[{}]",
                member
                    .shape
                    .iter()
                    .map(|d| d.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

        items.push(CompletionItem {
            label: member_name.clone(),
            kind: Some(kind),
            detail: Some(detail),
            documentation: if member.description.is_empty() {
                None
            } else {
                Some(lsp_types::Documentation::String(
                    member
                        .description
                        .iter()
                        .map(|t| t.text.trim_matches('"').to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                ))
            },
            insert_text: Some(snippet),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    items
}

/// Get general modifier items that apply to any component type
fn get_general_modifier_items() -> Vec<CompletionItem> {
    let modifiers = [
        ("each", "Apply modifier to each element", "each "),
        ("redeclare", "Redeclare a replaceable element", "redeclare "),
        ("final", "Prevent further modification", "final "),
    ];

    modifiers
        .into_iter()
        .map(|(label, detail, snippet)| CompletionItem {
            label: label.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
            insert_text: Some(snippet.to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        })
        .collect()
}

/// Get completion items for modifiers
fn get_modifier_items() -> Vec<CompletionItem> {
    let modifiers = [
        // Common modifiers for Real
        (
            "start",
            "Initial value for the variable",
            "start = ${1:0.0}",
        ),
        (
            "fixed",
            "Whether start value is fixed (default: false for states, true for parameters)",
            "fixed = ${1|true,false|}",
        ),
        ("min", "Minimum value constraint", "min = ${1:-1e10}"),
        ("max", "Maximum value constraint", "max = ${1:1e10}"),
        ("nominal", "Nominal value for scaling", "nominal = ${1:1.0}"),
        ("unit", "Physical unit (SI)", "unit = \"${1:}\""),
        (
            "displayUnit",
            "Display unit for GUI",
            "displayUnit = \"${1:}\"",
        ),
        (
            "stateSelect",
            "Hint for state selection",
            "stateSelect = StateSelect.${1|default,never,avoid,prefer,always|}",
        ),
        // For arrays
        ("each", "Apply modifier to each element", "each "),
        // For replaceable
        ("redeclare", "Redeclare a replaceable element", "redeclare "),
        ("final", "Prevent further modification", "final "),
    ];

    modifiers
        .into_iter()
        .map(|(label, detail, snippet)| CompletionItem {
            label: label.to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some(detail.to_string()),
            insert_text: Some(snippet.to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        })
        .collect()
}
