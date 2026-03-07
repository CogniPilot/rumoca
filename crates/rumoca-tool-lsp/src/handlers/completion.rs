//! Enhanced completion handler for Modelica files.

use lsp_types::{CompletionItem, CompletionItemKind, Position};
use rumoca_session::Session;
use rumoca_session::compile::core as rumoca_core;
use rumoca_session::parsing::ast;
use rumoca_session::parsing::ir_core as rumoca_ir_core;
use std::collections::HashSet;

use crate::helpers::{find_enclosing_class, get_text_before_cursor};

/// Handle completion request - returns keyword + scope-aware completions.
///
/// When a `session` is provided, also includes library class name completions
/// (e.g., "Modelica.Blocks.Continuous.PID") from the session's resolved tree.
pub fn handle_completion(
    source: &str,
    ast: Option<&ast::StoredDefinition>,
    session: Option<&Session>,
    line: u32,
    character: u32,
) -> Vec<CompletionItem> {
    let position = Position { line, character };
    let prefix = get_text_before_cursor(source, position)
        .unwrap_or_default()
        .trim()
        .to_string();

    // Get the partial word being typed (may include dots for qualified names)
    let partial: String = prefix
        .chars()
        .rev()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    let mut items = Vec::new();
    let mut library_class_names: Option<Vec<String>> = None;

    // Check for dot-completion (e.g., "Modelica.Blocks." or "pid.")
    if prefix.ends_with('.') || prefix.contains('.') {
        // Try local AST dot-completion first
        if let Some(dot_items) = dot_completion(ast, session, line, &prefix) {
            return dot_items;
        }
        // Try library dot-completion
        let class_names = library_class_names.get_or_insert_with(|| {
            session
                .map(|s| s.all_class_names_cached())
                .unwrap_or_default()
        });
        if !class_names.is_empty() {
            let lib_refs: Vec<&str> = class_names.iter().map(|s| s.as_str()).collect();
            let lib_items = library_dot_completion(&lib_refs, &prefix);
            if !lib_items.is_empty() {
                return lib_items;
            }
        }
    }

    // Check for modifier completion (inside parentheses)
    if is_in_modification_context(&prefix) {
        items.extend(modification_context_completions(
            ast, session, line, &prefix, &partial,
        ));
    }

    // Scope-aware local completions from AST
    if let Some(ast) = ast {
        items.extend(local_completions(ast, line, &partial));
    }

    // Library top-level class completions (e.g., "Model" -> "Modelica")
    if !partial.is_empty() {
        let class_names = library_class_names.get_or_insert_with(|| {
            session
                .map(|s| s.all_class_names_cached())
                .unwrap_or_default()
        });
        let lib_refs: Vec<&str> = class_names.iter().map(|s| s.as_str()).collect();
        items.extend(library_prefix_completions(&lib_refs, &partial));
    }

    // Built-in function completions
    items.extend(builtin_completions(&partial));

    // Keyword completions
    items.extend(keyword_completions(&partial));

    items
}

/// Dot-completion for library class names.
///
/// Given prefix "Modelica.Blocks." and library names like
/// "Modelica.Blocks.Continuous.PID", returns completion items for
/// the immediate children at that level (e.g., "Continuous", "Sources").
fn library_dot_completion(library_class_names: &[&str], prefix: &str) -> Vec<CompletionItem> {
    let (search_prefix, filter_partial) = extract_qualified_prefix(prefix);

    if search_prefix.is_empty() {
        return Vec::new();
    }

    let mut seen = std::collections::HashSet::new();
    let mut items = Vec::new();

    for name in library_class_names {
        let Some(rest) = name.strip_prefix(&search_prefix) else {
            continue;
        };
        let child = rest.split('.').next().unwrap_or(rest);
        if child.is_empty() {
            continue;
        }
        if !filter_partial.is_empty()
            && !child
                .to_lowercase()
                .starts_with(&filter_partial.to_lowercase())
        {
            continue;
        }
        if !seen.insert(child.to_string()) {
            continue;
        }
        let full_name = format!("{}{}", search_prefix, child);
        let has_children = library_class_names
            .iter()
            .any(|n| n.starts_with(&format!("{}.", full_name)));
        let kind = if has_children {
            CompletionItemKind::MODULE
        } else {
            CompletionItemKind::CLASS
        };
        items.push(CompletionItem {
            label: child.to_string(),
            kind: Some(kind),
            detail: Some(full_name),
            ..Default::default()
        });
    }

    items
}

/// Extract the qualified prefix for dot-completion.
///
/// Returns (search_prefix, partial_filter):
/// - "Modelica.Blocks." -> ("Modelica.Blocks.", "")
/// - "Modelica.Blocks.Con" -> ("Modelica.Blocks.", "Con")
/// - "Modelica." -> ("Modelica.", "")
fn extract_qualified_prefix(prefix: &str) -> (String, String) {
    // Find the qualified name being typed (walk back from end through dots and idents)
    let qualified: String = prefix
        .chars()
        .rev()
        .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '.')
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    if let Some(last_dot) = qualified.rfind('.') {
        let base = &qualified[..=last_dot]; // includes trailing dot
        let partial = &qualified[last_dot + 1..];
        (base.to_string(), partial.to_string())
    } else {
        (String::new(), qualified)
    }
}

/// Top-level library class prefix completion.
///
/// Given partial "Model" and library names, returns "Modelica" etc.
fn library_prefix_completions(library_class_names: &[&str], partial: &str) -> Vec<CompletionItem> {
    let mut seen = std::collections::HashSet::new();
    let mut items = Vec::new();
    let partial_lower = partial.to_lowercase();

    for name in library_class_names {
        // Get the top-level name (before first dot)
        let top_level = name.split('.').next().unwrap_or(name);
        if top_level.to_lowercase().starts_with(&partial_lower)
            && seen.insert(top_level.to_string())
        {
            items.push(CompletionItem {
                label: top_level.to_string(),
                kind: Some(CompletionItemKind::MODULE),
                detail: Some("Library package".to_string()),
                ..Default::default()
            });
        }
    }

    items
}

fn dot_completion(
    ast: Option<&ast::StoredDefinition>,
    session: Option<&Session>,
    line: u32,
    prefix: &str,
) -> Option<Vec<CompletionItem>> {
    let ast = ast?;
    let dot_pos = prefix.rfind('.')?;
    let member_partial = prefix[dot_pos + 1..].trim();
    // Extract the base name before the dot
    let base = prefix[..dot_pos].trim();
    let base_word: String = base
        .chars()
        .rev()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    if base_word.is_empty() {
        return None;
    }

    if let Some(class) = find_enclosing_class(ast, line)
        && let Some(comp) = class.components.get(&base_word)
    {
        let type_candidates = resolve_type_candidates(Some(ast), line, &comp.type_name.to_string());
        if let Some(items) =
            session_type_member_completions(session, &type_candidates, member_partial, false)
        {
            return Some(items);
        }
    }
    None
}

fn component_completion_kind(comp: &ast::Component) -> CompletionItemKind {
    match (&comp.variability, &comp.causality) {
        (rumoca_ir_core::Variability::Parameter(_), _)
        | (rumoca_ir_core::Variability::Constant(_), _) => CompletionItemKind::CONSTANT,
        (_, rumoca_ir_core::Causality::Input(_)) | (_, rumoca_ir_core::Causality::Output(_)) => {
            CompletionItemKind::PROPERTY
        }
        _ => CompletionItemKind::VARIABLE,
    }
}

fn is_in_modification_context(prefix: &str) -> bool {
    // Simple heuristic: more open parens than close parens
    let opens = prefix.matches('(').count();
    let closes = prefix.matches(')').count();
    opens > closes
}

fn modification_context_completions(
    ast: Option<&ast::StoredDefinition>,
    session: Option<&Session>,
    line: u32,
    prefix: &str,
    partial: &str,
) -> Vec<CompletionItem> {
    let Some(ctx) = modifier_context_from_prefix(prefix) else {
        return modifier_completions(partial);
    };
    if rumoca_core::is_builtin_type(&ctx.type_name) {
        return modifier_completions(partial);
    }

    let type_candidates = resolve_type_candidates(ast, line, &ctx.type_name);
    if let Some(items) = session_type_member_completions(session, &type_candidates, partial, true) {
        return items;
    }

    modifier_completions(partial)
}

fn session_type_member_completions(
    session: Option<&Session>,
    type_candidates: &[String],
    partial: &str,
    insert_assignment: bool,
) -> Option<Vec<CompletionItem>> {
    let session = session?;
    for type_name in type_candidates {
        let members = session.class_component_members_cached(type_name);
        if members.is_empty() {
            continue;
        }
        let items = members
            .into_iter()
            .filter(|(name, _)| partial.is_empty() || name.starts_with(partial))
            .map(|(name, member_type)| CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::PROPERTY),
                detail: Some(member_type),
                insert_text: Some(if insert_assignment {
                    format!("{name} = ")
                } else {
                    name.clone()
                }),
                ..Default::default()
            })
            .collect();
        return Some(items);
    }
    None
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ModifierContext {
    type_name: String,
}

fn modifier_context_from_prefix(prefix: &str) -> Option<ModifierContext> {
    let paren_pos = prefix.rfind('(')?;
    let left = prefix[..paren_pos].trim_end();
    let mut parts = left.split_whitespace();
    let instance_token = parts.next_back()?;
    let type_token = parts.next_back()?;
    if instance_token.is_empty() || type_token.is_empty() {
        return None;
    }
    Some(ModifierContext {
        type_name: type_token
            .trim_matches(|c: char| c == ',' || c == ';')
            .to_string(),
    })
}

fn resolve_type_candidates(
    ast: Option<&ast::StoredDefinition>,
    line: u32,
    raw_type_name: &str,
) -> Vec<String> {
    let mut seen = HashSet::<String>::new();
    let mut candidates = Vec::<String>::new();
    let mut push = |name: String| {
        if !name.is_empty() && seen.insert(name.clone()) {
            candidates.push(name);
        }
    };

    push(raw_type_name.to_string());

    let Some(ast) = ast else {
        return candidates;
    };
    let Some(class) = find_enclosing_class(ast, line) else {
        return candidates;
    };

    for import in &class.imports {
        match import {
            ast::Import::Qualified { path, .. } => {
                let full = path.to_string();
                if import_simple_name(&full) == raw_type_name {
                    push(full);
                }
            }
            ast::Import::Renamed { alias, path, .. } => {
                if alias.text.as_ref() == raw_type_name {
                    push(path.to_string());
                }
            }
            ast::Import::Selective { path, names, .. } => {
                if names.iter().any(|name| name.text.as_ref() == raw_type_name) {
                    push(format!("{}.{}", path, raw_type_name));
                }
            }
            ast::Import::Unqualified { path, .. } => {
                push(format!("{}.{}", path, raw_type_name));
            }
        }
    }

    candidates
}

fn import_simple_name(path: &str) -> &str {
    path.rsplit('.').next().unwrap_or(path)
}

fn modifier_completions(partial: &str) -> Vec<CompletionItem> {
    let modifiers = [
        ("start", "Initial value"),
        ("fixed", "Whether initial value is fixed"),
        ("min", "Minimum value"),
        ("max", "Maximum value"),
        ("nominal", "Nominal value for scaling"),
        ("unit", "Physical unit"),
        ("displayUnit", "Display unit"),
        ("quantity", "Physical quantity name"),
        ("stateSelect", "State selection hint"),
    ];

    modifiers
        .iter()
        .filter(|(label, _)| partial.is_empty() || label.starts_with(partial))
        .map(|(label, detail)| CompletionItem {
            label: label.to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some(detail.to_string()),
            insert_text: Some(format!("{} = ", label)),
            ..Default::default()
        })
        .collect()
}

fn local_completions(ast: &ast::StoredDefinition, line: u32, partial: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    if let Some(class) = find_enclosing_class(ast, line) {
        for (name, comp) in &class.components {
            if !partial.is_empty() && !name.starts_with(partial) {
                continue;
            }
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(component_completion_kind(comp)),
                detail: Some(comp.type_name.to_string()),
                ..Default::default()
            });
        }
        // Also suggest nested class names as types
        for (name, nested) in &class.classes {
            if !partial.is_empty() && !name.starts_with(partial) {
                continue;
            }
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some(format!("{:?}", nested.class_type)),
                ..Default::default()
            });
        }
    }
    items
}

fn builtin_completions(partial: &str) -> Vec<CompletionItem> {
    rumoca_core::BUILTIN_FUNCTIONS
        .iter()
        .filter(|name| {
            !partial.is_empty() && name.starts_with(partial) && !rumoca_core::is_builtin_type(name)
        })
        .map(|name| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("Built-in function".to_string()),
            ..Default::default()
        })
        .collect()
}

fn keyword_completions(partial: &str) -> Vec<CompletionItem> {
    let keywords = [
        ("model", "Define a model class"),
        ("package", "Define a package"),
        ("function", "Define a function"),
        ("block", "Define a block class"),
        ("connector", "Define a connector class"),
        ("record", "Define a record class"),
        ("type", "Define a type alias"),
        ("class", "Define a general class"),
        ("operator", "Define an operator class/operator record"),
        ("equation", "Equation section"),
        ("algorithm", "Algorithm section"),
        ("parameter", "Parameter declaration prefix"),
        ("constant", "Constant declaration prefix"),
        ("input", "Input causality prefix"),
        ("output", "Output causality prefix"),
        ("extends", "Inherit from base class"),
        ("import", "Import declarations"),
        ("if", "Conditional expression/equation"),
        ("for", "For-loop"),
        ("when", "Event handling"),
        ("while", "While loop (in algorithms)"),
        ("der", "Time derivative operator"),
        ("connect", "Connect two connectors"),
        ("Real", "Real number type"),
        ("Integer", "Integer number type"),
        ("Boolean", "Boolean type"),
        ("String", "String type"),
    ];

    keywords
        .iter()
        .filter(|(label, _)| partial.is_empty() || label.starts_with(partial))
        .map(|(label, detail)| CompletionItem {
            label: label.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
            insert_text: Some(label.to_string()),
            ..Default::default()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_session::parsing::parse_source_to_ast;

    fn parse_ast(source: &str) -> ast::StoredDefinition {
        parse_source_to_ast(source, "input.mo").expect("parse should succeed")
    }

    #[test]
    fn modifier_completion_suggests_imported_type_members() {
        let lib = r#"
package Modelica
  package Blocks
    package Continuous
      model PID
        parameter Real kp = 1.0;
        Real y;
        Real u;
      end PID;
    end Continuous;
  end Blocks;
end Modelica;
"#;
        let source = r#"
model Ball
  import Modelica.Blocks.Continuous.PID;
  Real x(start=0);
  PID pid();
end Ball;
"#;
        let mut session = Session::default();
        session.add_document("Lib.mo", lib).expect("lib parses");
        session
            .add_document("input.mo", source)
            .expect("source parses");
        let _ = session
            .all_class_names()
            .expect("resolution should succeed for completion");

        let ast = parse_ast(source);
        let line = 4;
        let character = "  PID pid(".len() as u32;
        let items = handle_completion(source, Some(&ast), Some(&session), line, character);
        assert!(
            items.iter().any(|i| i.label == "kp"),
            "expected PID member `kp` in completions: {:?}",
            items.iter().map(|i| i.label.clone()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn dot_completion_scopes_to_component_type_members() {
        let lib = r#"
package Modelica
  package Blocks
    package Continuous
      model PID
        parameter Real kp = 1.0;
        Real y;
        Real u;
      end PID;
    end Continuous;
  end Blocks;
end Modelica;
"#;
        let source = r#"
model Ball
  import Modelica.Blocks.Continuous.PID;
  Real x(start=0);
  PID pid();
equation
  pid.u = 1;
end Ball;
"#;
        let mut session = Session::default();
        session.add_document("Lib.mo", lib).expect("lib parses");
        session
            .add_document("input.mo", source)
            .expect("source parses");
        let _ = session
            .all_class_names()
            .expect("resolution should succeed for completion");

        let ast = parse_ast(source);
        let line = 6;
        let character = "  pid.".len() as u32;
        let items = handle_completion(source, Some(&ast), Some(&session), line, character);
        let labels = items.iter().map(|i| i.label.clone()).collect::<Vec<_>>();
        assert!(
            labels.iter().any(|label| label == "kp"),
            "expected PID member completions, got: {:?}",
            labels
        );
        assert!(
            !labels.iter().any(|label| label == "x"),
            "dot-completion on `pid.` should not include Ball-scoped names: {:?}",
            labels
        );
    }

    #[test]
    fn dot_completion_uses_session_for_local_model_component_members() {
        let source = r#"
model Plane
  Real x, y, theta;
equation
  der(x) = cos(theta);
  der(y) = sin(theta);
  der(theta) = 1;
end Plane;

model Sim
  Plane p1, p2;
equation
  p1.x = 1;
end Sim;
"#;
        let mut session = Session::default();
        session
            .add_document("input.mo", source)
            .expect("source parses");
        let _ = session
            .all_class_names()
            .expect("resolution should succeed for completion");

        let ast = parse_ast(source);
        let line = 12;
        let character = "  p1.".len() as u32;
        let items = handle_completion(source, Some(&ast), Some(&session), line, character);
        let labels = items.iter().map(|i| i.label.clone()).collect::<Vec<_>>();
        assert!(
            labels.iter().any(|label| label == "x"),
            "expected Plane member completions for `p1.`, got: {:?}",
            labels
        );
        assert!(
            labels.iter().any(|label| label == "theta"),
            "expected Plane member `theta` completion for `p1.`, got: {:?}",
            labels
        );
    }

    #[test]
    fn builtin_modifier_completion_remains_available_for_builtin_types() {
        let source = r#"
model M
  Real x(start = 0);
end M;
"#;
        let ast = parse_ast(source);
        let line = 2;
        let character = "  Real x(".len() as u32;
        let items = handle_completion(source, Some(&ast), None, line, character);
        assert!(
            items.iter().any(|i| i.label == "start"),
            "expected builtin modifier `start` completion"
        );
    }

    #[test]
    fn keyword_completion_includes_operator() {
        let items = keyword_completions("op");
        assert!(
            items.iter().any(|i| i.label == "operator"),
            "expected `operator` keyword completion"
        );
    }
}
