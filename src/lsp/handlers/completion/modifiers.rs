//! Modifier completion handling.
//!
//! Provides completions for component modifiers inside parentheses.
//!
//! This module uses canonical scope resolution functions from
//! `crate::ir::transform::scope_resolver` to avoid duplication.

use lsp_types::{CompletionItem, CompletionItemKind, InsertTextFormat};

use crate::ir::ast::{Import, StoredDefinition, Variability};
use crate::ir::transform::scope_resolver::{SymbolLookup, find_class_in_ast};

/// Check if we're in a modifier context and return appropriate completions
///
/// Detects patterns like:
/// - `Real x(` - just opened paren (trigger: '(')
/// - `Real x(start=1,` - after comma (trigger: ',')
/// - `Real x(start=1, ` - after comma with space
/// - `Real x(st` - typing a modifier name
/// - `test.BouncingBall ball(` - class instance with member modifiers
///
/// The `workspace` parameter enables lookup of types from libraries.
pub fn get_modifier_completions(
    text_before: &str,
    ast: Option<&StoredDefinition>,
    workspace: Option<&dyn SymbolLookup>,
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
    // Check for hierarchical member access (e.g., "I." in "pid1(I.")
    // This happens when the user is typing a dot-notation modification like "I.use_reset = true"
    if let Some(member_path) = current_trimmed.strip_suffix('.') {
        if let Some(items) =
            get_hierarchical_modifier_completions(&type_name, member_path, ast, workspace)
        {
            return Some(items);
        }
        // If hierarchical lookup failed, return empty to prevent fallback to general completions
        // This happens when we can't resolve the nested component's type
        #[cfg(target_arch = "wasm32")]
        web_sys::console::log_1(
            &format!(
                "[modifiers] hierarchical lookup failed for '{}.', returning empty",
                member_path
            )
            .into(),
        );
        return Some(Vec::new());
    }

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

        #[cfg(target_arch = "wasm32")]
        web_sys::console::log_1(
            &format!(
                "[modifiers] type_name='{}', has_ast={}, has_workspace={}",
                type_name,
                ast.is_some(),
                workspace.is_some()
            )
            .into(),
        );

        // Check if the type is a primitive type - if so, add standard modifiers
        if is_primitive_type(&type_name) {
            items.extend(get_modifier_items());
        } else {
            // For class types, add member overrides from the class definition
            // First try looking up in the local AST
            let mut found_class = false;
            if let Some(ast) = ast
                && let Some(type_class) = find_class_in_ast(ast, &type_name)
            {
                #[cfg(target_arch = "wasm32")]
                web_sys::console::log_1(
                    &format!("[modifiers] found '{}' in local AST", type_name).into(),
                );
                items.extend(get_class_modifier_completions(type_class));
                found_class = true;
            }

            // If not found locally, try looking up in the workspace (for library types)
            if !found_class && let Some(ws) = workspace {
                // First, try to resolve the type name via imports in the current AST
                let qualified_names = resolve_type_via_imports(&type_name, ast);

                #[cfg(target_arch = "wasm32")]
                web_sys::console::log_1(
                    &format!(
                        "[modifiers] resolved '{}' via imports: {:?}",
                        type_name, qualified_names
                    )
                    .into(),
                );

                for qname in &qualified_names {
                    #[cfg(target_arch = "wasm32")]
                    web_sys::console::log_1(
                        &format!("[modifiers] looking up '{}' in workspace", qname).into(),
                    );

                    if let Some(lib_ast) = ws.get_ast_for_symbol(qname) {
                        #[cfg(target_arch = "wasm32")]
                        web_sys::console::log_1(
                            &format!(
                                "[modifiers] found AST for '{}', class_list keys: {:?}",
                                qname,
                                lib_ast.class_list.keys().collect::<Vec<_>>()
                            )
                            .into(),
                        );

                        if let Some(type_class) = find_class_in_ast(lib_ast, qname) {
                            #[cfg(target_arch = "wasm32")]
                            web_sys::console::log_1(
                                &format!(
                                    "[modifiers] found class '{}' with {} components",
                                    qname,
                                    type_class.components.len()
                                )
                                .into(),
                            );
                            items.extend(get_class_modifier_completions(type_class));
                            found_class = true;
                            break;
                        } else {
                            #[cfg(target_arch = "wasm32")]
                            web_sys::console::log_1(
                                &format!(
                                    "[modifiers] find_class_in_ast returned None for '{}'",
                                    qname
                                )
                                .into(),
                            );
                        }
                    } else {
                        #[cfg(target_arch = "wasm32")]
                        web_sys::console::log_1(
                            &format!(
                                "[modifiers] get_ast_for_symbol returned None for '{}'",
                                qname
                            )
                            .into(),
                        );
                    }
                }

                // If still not found, try direct lookup (for fully qualified names in source)
                if !found_class
                    && let Some(lib_ast) = ws.get_ast_for_symbol(&type_name)
                    && let Some(type_class) = find_class_in_ast(lib_ast, &type_name)
                {
                    items.extend(get_class_modifier_completions(type_class));
                    found_class = true;
                }
            }

            // Also add standard modifiers that apply to any component (like each, redeclare, final)
            if found_class || ast.is_some() || workspace.is_some() {
                items.extend(get_general_modifier_items());
            }
        }

        #[cfg(target_arch = "wasm32")]
        web_sys::console::log_1(
            &format!(
                "[modifiers] returning {} items, found_class={}",
                items.len(),
                items
                    .iter()
                    .any(|i| i.kind == Some(CompletionItemKind::FIELD)
                        || i.kind == Some(CompletionItemKind::CONSTANT))
            )
            .into(),
        );

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

/// Resolve a type name to its fully qualified name(s) using imports from the AST
///
/// Given a short type name like "PID" and an AST containing imports like
/// `import Modelica.Blocks.Continuous.PID;`, returns a list of possible
/// fully qualified names (e.g., ["Modelica.Blocks.Continuous.PID"]).
fn resolve_type_via_imports(type_name: &str, ast: Option<&StoredDefinition>) -> Vec<String> {
    let mut qualified_names = Vec::new();

    let Some(ast) = ast else {
        return qualified_names;
    };

    // Search all classes in the AST for imports that match the type name
    for class in ast.class_list.values() {
        for import in &class.imports {
            match import {
                Import::Qualified { path, .. } => {
                    // import A.B.C; -> C maps to A.B.C
                    if let Some(last_part) = path.name.last()
                        && last_part.text == type_name
                    {
                        qualified_names.push(path.to_string());
                    }
                }
                Import::Renamed { alias, path, .. } => {
                    // import D = A.B.C; -> D maps to A.B.C
                    if alias.text == type_name {
                        qualified_names.push(path.to_string());
                    }
                }
                Import::Selective { path, names, .. } => {
                    // import A.B.{C, D}; -> C maps to A.B.C, D maps to A.B.D
                    for name_token in names {
                        if name_token.text == type_name {
                            qualified_names.push(format!("{}.{}", path, type_name));
                        }
                    }
                }
                Import::Unqualified { path, .. } => {
                    // import A.B.*; -> Any name could be from A.B
                    // We'll add the qualified path as a candidate to try
                    qualified_names.push(format!("{}.{}", path, type_name));
                }
            }
        }
    }

    qualified_names
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

/// Get completion items for hierarchical modifier access (e.g., "I." in "pid1(I.")
///
/// This handles dot-notation modifications like `pid1(I.use_reset = true)` where
/// we need to complete members of a nested component (`I`) rather than the parent class (`PID`).
fn get_hierarchical_modifier_completions(
    parent_type: &str,
    member_path: &str,
    ast: Option<&StoredDefinition>,
    workspace: Option<&dyn SymbolLookup>,
) -> Option<Vec<CompletionItem>> {
    #[cfg(target_arch = "wasm32")]
    web_sys::console::log_1(
        &format!(
            "[modifiers] hierarchical: parent='{}', path='{}'",
            parent_type, member_path
        )
        .into(),
    );

    // Split the member path for nested access (e.g., "I" or "I.limiter")
    let path_parts: Vec<&str> = member_path.split('.').collect();

    // Find the parent class - first resolve via imports to get full path
    let resolved_parent = resolve_type_via_imports(parent_type, ast);
    let parent_qualified = resolved_parent
        .first()
        .cloned()
        .unwrap_or_else(|| parent_type.to_string());

    #[cfg(target_arch = "wasm32")]
    web_sys::console::log_1(
        &format!(
            "[modifiers] hierarchical: resolved parent '{}' -> '{}'",
            parent_type, parent_qualified
        )
        .into(),
    );

    let parent_class = find_class_with_fallback(&parent_qualified, ast, workspace)?;

    // Get the package context for resolving relative type names (as owned String)
    let mut current_package: Option<String> = parent_qualified
        .rsplit_once('.')
        .map(|(pkg, _)| pkg.to_string());

    #[cfg(target_arch = "wasm32")]
    web_sys::console::log_1(
        &format!(
            "[modifiers] hierarchical: parent package = {:?}",
            current_package
        )
        .into(),
    );

    // Follow the path to find the target component's type
    let mut current_class = parent_class;

    for part in &path_parts {
        // Find the component with this name in current class
        let component = current_class.components.get(*part)?;

        // Get the type of this component
        let component_type = component.type_name.to_string();

        #[cfg(target_arch = "wasm32")]
        web_sys::console::log_1(
            &format!(
                "[modifiers] hierarchical: component '{}' has type '{}' (package: {:?})",
                part, component_type, current_package
            )
            .into(),
        );

        // Look up the component's type class, trying multiple resolution strategies
        current_class =
            find_class_in_context(&component_type, current_package.as_deref(), ast, workspace)?;

        // Update package context for nested lookups
        if component_type.contains('.') {
            current_package = component_type
                .rsplit_once('.')
                .map(|(pkg, _)| pkg.to_string());
        }
    }

    // Return completions for the target class's members
    let items = get_class_modifier_completions(current_class);

    #[cfg(target_arch = "wasm32")]
    web_sys::console::log_1(
        &format!(
            "[modifiers] hierarchical: returning {} items for nested type",
            items.len()
        )
        .into(),
    );

    if items.is_empty() { None } else { Some(items) }
}

/// Find a class, trying multiple resolution strategies including package context
fn find_class_in_context<'a>(
    type_name: &str,
    package_context: Option<&str>,
    ast: Option<&'a StoredDefinition>,
    workspace: Option<&'a dyn SymbolLookup>,
) -> Option<&'a crate::ir::ast::ClassDefinition> {
    #[cfg(target_arch = "wasm32")]
    web_sys::console::log_1(
        &format!(
            "[modifiers] find_class_in_context: type='{}', pkg={:?}",
            type_name, package_context
        )
        .into(),
    );

    // 1. Try the type name as-is (might already be fully qualified)
    if let Some(class) = find_class_with_fallback(type_name, ast, workspace) {
        return Some(class);
    }

    // 2. Try with "Modelica." prefix if type starts with known MSL packages
    // Types in libraries are often stored as "Blocks.Continuous.X" without "Modelica." prefix
    if type_name.starts_with("Blocks.")
        || type_name.starts_with("Mechanics.")
        || type_name.starts_with("Electrical.")
        || type_name.starts_with("Thermal.")
        || type_name.starts_with("Fluid.")
        || type_name.starts_with("Media.")
        || type_name.starts_with("Math.")
        || type_name.starts_with("Constants.")
        || type_name.starts_with("Icons.")
        || type_name.starts_with("Units.")
        || type_name.starts_with("Utilities.")
    {
        let with_modelica = format!("Modelica.{}", type_name);
        #[cfg(target_arch = "wasm32")]
        web_sys::console::log_1(
            &format!(
                "[modifiers] trying with Modelica prefix: '{}'",
                with_modelica
            )
            .into(),
        );
        if let Some(class) = find_class_with_fallback(&with_modelica, ast, workspace) {
            return Some(class);
        }
    }

    // 3. Try resolving in the package context (for relative type names like "LimIntegrator")
    if let Some(pkg) = package_context {
        let qualified = format!("{}.{}", pkg, type_name);
        #[cfg(target_arch = "wasm32")]
        web_sys::console::log_1(
            &format!("[modifiers] trying qualified name: '{}'", qualified).into(),
        );
        if let Some(class) = find_class_with_fallback(&qualified, ast, workspace) {
            return Some(class);
        }
    }

    // 4. For Modelica stdlib, some types might be in sub-packages
    // Try direct workspace lookup
    if let Some(ws) = workspace
        && let Some(lib_ast) = ws.get_ast_for_symbol(type_name)
        && let Some(class) = find_class_in_ast(lib_ast, type_name)
    {
        return Some(class);
    }

    None
}

/// Helper to find a class, trying local AST first, then workspace with import resolution
fn find_class_with_fallback<'a>(
    type_name: &str,
    ast: Option<&'a StoredDefinition>,
    workspace: Option<&'a dyn SymbolLookup>,
) -> Option<&'a crate::ir::ast::ClassDefinition> {
    // Try local AST first
    if let Some(ast) = ast
        && let Some(class) = find_class_in_ast(ast, type_name)
    {
        return Some(class);
    }

    // Try workspace with import resolution
    if let Some(ws) = workspace {
        // First resolve via imports
        let qualified_names = resolve_type_via_imports(type_name, ast);
        for qname in &qualified_names {
            if let Some(lib_ast) = ws.get_ast_for_symbol(qname)
                && let Some(class) = find_class_in_ast(lib_ast, qname)
            {
                return Some(class);
            }
        }

        // Try direct lookup
        if let Some(lib_ast) = ws.get_ast_for_symbol(type_name)
            && let Some(class) = find_class_in_ast(lib_ast, type_name)
        {
            return Some(class);
        }
    }

    None
}
