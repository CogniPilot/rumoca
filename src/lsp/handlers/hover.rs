//! Hover information handler for Modelica files.
//!
//! This module uses canonical scope resolution functions from
//! `crate::ir::transform::scope_resolver` to avoid duplication.

use std::collections::HashMap;

use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Position, Uri};

use crate::fmt::{format_equation, format_expression};
use crate::ir::ast::{
    Causality, ClassDefinition, ClassType, Component, Expression, Import, StoredDefinition,
    Variability,
};
use crate::ir::transform::constants::get_builtin_functions;
use crate::ir::transform::scope_resolver::{ResolvedSymbol, ScopeResolver, find_class_in_ast};

use crate::lsp::data::keywords::get_keyword_hover;
use crate::lsp::utils::{get_qualified_name_at_position, get_word_at_position, parse_document};
use crate::lsp::workspace::WorkspaceState;

/// Handle hover request
pub fn handle_hover(documents: &HashMap<Uri, String>, params: HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let word = get_word_at_position(text, position)?;

    // First check for hover info from the AST
    if let Some(ast) = parse_document(text, path)
        && let Some(hover_text) = get_ast_hover_info(&ast, &word, position)
    {
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: hover_text,
            }),
            range: None,
        });
    }

    // Check built-in functions
    for func in get_builtin_functions() {
        if func.name == word {
            let params_doc: String = func
                .parameters
                .iter()
                .map(|(name, doc)| format!("- `{}`: {}", name, doc))
                .collect::<Vec<_>>()
                .join("\n");

            let hover_text = format!(
                "```modelica\n{}\n```\n\n{}\n\n**Parameters:**\n{}",
                func.signature, func.documentation, params_doc
            );

            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_text,
                }),
                range: None,
            });
        }
    }

    // Provide hover info for known Modelica keywords and built-ins
    let hover_text = get_keyword_hover(&word)?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: hover_text,
        }),
        range: None,
    })
}

/// Handle hover request with workspace support for cross-file lookups
pub fn handle_hover_workspace(
    workspace: &mut WorkspaceState,
    params: HoverParams,
) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let text = workspace.get_document(uri)?;
    let path = uri.path().as_str();

    // Get both the simple word and the qualified name (dotted path)
    let word = get_word_at_position(text, position)?;
    let qualified_name = get_qualified_name_at_position(text, position);

    // Parse the document and use unified scope resolver with workspace lookup
    if let Some(ast) = parse_document(text, path) {
        // Pre-index: Check if word matches an import alias and ensure package is indexed
        // This is needed because resolve() returns None if the imported symbol isn't indexed yet
        for class in ast.class_list.values() {
            if let Some(import_path) = find_import_path_for_name(class, &word) {
                workspace.ensure_package_indexed(&import_path);
            }
        }

        // Resolve with indexed symbols
        let resolver = ScopeResolver::with_lookup(&ast, workspace as &WorkspaceState);
        let mut resolved = None;
        if let Some(ref qn) = qualified_name {
            resolved = resolver.resolve_qualified(qn, position.line + 1, position.character + 1);
        }
        if resolved.is_none() {
            resolved = resolver.resolve(&word, position.line + 1, position.character + 1);
        }

        if let Some(ref resolved) = resolved
            && let Some(hover_text) = format_resolved_symbol_unified(resolved, workspace, &ast)
        {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_text,
                }),
                range: None,
            });
        }
    }

    // Check built-in functions
    for func in get_builtin_functions() {
        if func.name == word {
            let params_doc: String = func
                .parameters
                .iter()
                .map(|(name, doc)| format!("- `{}`: {}", name, doc))
                .collect::<Vec<_>>()
                .join("\n");

            let hover_text = format!(
                "```modelica\n{}\n```\n\n{}\n\n**Parameters:**\n{}",
                func.signature, func.documentation, params_doc
            );

            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_text,
                }),
                range: None,
            });
        }
    }

    // Provide hover info for known Modelica keywords and built-ins
    let hover_text = get_keyword_hover(&word)?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: hover_text,
        }),
        range: None,
    })
}

/// Format a resolved symbol for hover display (unified scope resolver)
fn format_resolved_symbol_unified(
    resolved: &ResolvedSymbol,
    workspace: &WorkspaceState,
    ast: &StoredDefinition,
) -> Option<String> {
    match resolved {
        ResolvedSymbol::Component {
            component,
            defined_in,
            inherited_via,
        } => {
            // Look up the class definition for this component's type
            let type_name = component.type_name.to_string();
            // Try to find type class in workspace
            let type_class = workspace
                .get_parsed_ast_by_name(&type_name)
                .and_then(|ast| ast.class_list.values().next());

            let mut info = format_component_hover_with_class(component, type_class);

            // Add inheritance info if applicable
            if let Some(base_class_name) = inherited_via {
                info += &format!("\n\n*Inherited from `{}`*", base_class_name);
            } else {
                info += &format!("\n\n*Defined in `{}`*", defined_in.name.text);
            }

            Some(info)
        }
        ResolvedSymbol::Class(class_def) => {
            // Try to flatten the class to show inherited content
            let class_name = &class_def.name.text;
            if let Ok(flattened) = crate::ir::transform::flatten::flatten(ast, Some(class_name)) {
                Some(format_class_hover(&flattened, class_name))
            } else {
                // Fall back to unflattened view
                Some(format_class_hover(class_def, class_name))
            }
        }
        ResolvedSymbol::External(sym_info) => Some(format_symbol_info_hover(sym_info, workspace)),
    }
}

/// Format hover info for an external symbol (from workspace lookup via ExternalSymbol)
fn format_symbol_info_hover(
    sym_info: &crate::ir::transform::scope_resolver::ExternalSymbol,
    workspace: &WorkspaceState,
) -> String {
    // For qualified names like "Modelica.Blocks.Continuous.PID", we need to:
    // 1. Get the root library (e.g., "Modelica") from library_cache
    // 2. Navigate through the class hierarchy to find PID
    let parts: Vec<&str> = sym_info.qualified_name.split('.').collect();
    if !parts.is_empty() {
        let lib_name = parts[0];
        if let Some(lib_ast) = workspace.get_library(lib_name) {
            // Navigate the path: Modelica -> Blocks -> Continuous -> PID
            // The lib_ast has "Modelica" in class_list, so we start from the root
            if let Some(class) = navigate_class_path(&lib_ast, lib_name, &parts[1..]) {
                return format_class_hover(class, &sym_info.qualified_name);
            }
        }
    }

    // Try the old approach as fallback (for non-library symbols)
    if let Some(ast) = workspace.get_parsed_ast_by_name(&sym_info.qualified_name) {
        let simple_name = sym_info
            .qualified_name
            .rsplit('.')
            .next()
            .unwrap_or(&sym_info.qualified_name);
        if let Some(class) = ast.class_list.get(simple_name) {
            return format_class_hover(class, &sym_info.qualified_name);
        }
    }

    // Fall back to basic info from ExternalSymbol
    let kind = format!("{:?}", sym_info.kind);
    let mut info = format!("**{}** ({})\n\n", sym_info.qualified_name, kind);
    if let Some(detail) = &sym_info.detail {
        info += detail;
    }
    info
}

/// Navigate through nested classes to find a class at the given path
///
/// For "Modelica.Blocks.Continuous.PID":
/// - lib_name = "Modelica"
/// - path = ["Blocks", "Continuous", "PID"]
///
/// The ast.class_list contains "Modelica" as the root package.
/// We first get Modelica, then navigate Blocks -> Continuous -> PID.
fn navigate_class_path<'a>(
    ast: &'a StoredDefinition,
    lib_name: &str,
    path: &[&str],
) -> Option<&'a ClassDefinition> {
    // Get the root package class from class_list
    let mut current = ast.class_list.get(lib_name)?;

    // If path is empty, return the root package itself
    if path.is_empty() {
        return Some(current);
    }

    // Navigate through nested classes
    for &part in path {
        current = current.classes.get(part)?;
    }

    Some(current)
}

/// Get hover info from the AST for user-defined symbols
fn get_ast_hover_info(ast: &StoredDefinition, word: &str, position: Position) -> Option<String> {
    let resolver = ScopeResolver::new(ast);

    // Try to resolve the symbol at the cursor position
    if let Some(symbol) = resolver.resolve_0indexed(word, position.line, position.character) {
        match symbol {
            ResolvedSymbol::Component {
                component,
                defined_in,
                inherited_via,
            } => {
                // Look up the class definition for this component's type (using canonical function)
                let type_class = find_class_in_ast(ast, &component.type_name.to_string());
                let mut info = format_component_hover_with_class(component, type_class);

                // Add inheritance info if applicable
                if let Some(base_class_name) = inherited_via {
                    info += &format!("\n\n*Inherited from `{}`*", base_class_name);
                } else {
                    // Show the class where it's defined
                    info += &format!("\n\n*Defined in `{}`*", defined_in.name.text);
                }

                return Some(info);
            }
            ResolvedSymbol::Class(class_def) => {
                // Try to flatten the class to show inherited content
                let class_name = &class_def.name.text;
                if let Ok(flattened) = crate::ir::transform::flatten::flatten(ast, Some(class_name))
                {
                    return Some(format_class_hover(&flattened, class_name));
                }
                return Some(format_class_hover(class_def, word));
            }
            ResolvedSymbol::External(sym_info) => {
                // External symbol from workspace lookup - format basic info
                let kind = format!("{:?}", sym_info.kind);
                let mut info = format!("**{}** ({})\n\n", sym_info.qualified_name, kind);
                if let Some(detail) = &sym_info.detail {
                    info += detail;
                }
                return Some(info);
            }
        }
    }

    // Fall back: check if word is a class name anywhere
    if let Some(class_def) = ast.class_list.get(word) {
        // Try to flatten the class to show inherited content
        if let Ok(flattened) = crate::ir::transform::flatten::flatten(ast, Some(word)) {
            return Some(format_class_hover(&flattened, word));
        }
        return Some(format_class_hover(class_def, word));
    }

    // Check nested classes in all top-level classes
    for class in ast.class_list.values() {
        if let Some(nested) = class.classes.get(word) {
            // For nested classes, try to flatten with qualified name
            let qualified_name = format!("{}.{}", class.name.text, word);
            if let Ok(flattened) =
                crate::ir::transform::flatten::flatten(ast, Some(&qualified_name))
            {
                return Some(format_class_hover(&flattened, word));
            }
            return Some(format_class_hover(nested, word));
        }
    }

    None
}

/// Helper to format an expression for display
fn format_expr(expr: &Expression) -> String {
    format_expression(expr)
}

// Note: find_class_definition is replaced by find_class_in_ast from canonical module

/// Format hover info for a component with optional class definition
fn format_component_hover_with_class(
    comp: &Component,
    type_class: Option<&crate::ir::ast::ClassDefinition>,
) -> String {
    // Build the type signature line
    let mut type_sig = comp.type_name.to_string();

    if !comp.shape.is_empty() {
        type_sig += &format!(
            "[{}]",
            comp.shape
                .iter()
                .map(|d| d.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );
    }

    // Add variability/causality qualifiers
    let mut qualifiers = Vec::new();
    match &comp.variability {
        Variability::Parameter(_) => qualifiers.push("parameter"),
        Variability::Constant(_) => qualifiers.push("constant"),
        Variability::Discrete(_) => qualifiers.push("discrete"),
        _ => {}
    }
    match &comp.causality {
        Causality::Input(_) => qualifiers.push("input"),
        Causality::Output(_) => qualifiers.push("output"),
        _ => {}
    }

    let qualifier_str = if qualifiers.is_empty() {
        String::new()
    } else {
        format!("{} ", qualifiers.join(" "))
    };

    let mut info = format!(
        "```modelica\n{}{} {}\n```",
        qualifier_str, type_sig, comp.name
    );

    // Add description if present
    if !comp.description.is_empty() {
        let desc = comp
            .description
            .iter()
            .map(|t| t.text.trim_matches('"').to_string())
            .collect::<Vec<_>>()
            .join(" ");
        info += &format!("\n\n*{}*", desc);
    }

    // If we have the class definition, show its type and description
    if let Some(class_def) = type_class {
        let class_type_str = format!("{:?}", class_def.class_type).to_lowercase();
        info += &format!("\n\n**Type:** `{}`", class_type_str);

        // Show class description if present
        if !class_def.description.is_empty() {
            let class_desc = class_def
                .description
                .iter()
                .map(|t| t.text.trim_matches('"').to_string())
                .collect::<Vec<_>>()
                .join(" ");
            info += &format!(" - {}", class_desc);
        }

        // Show class attributes
        if !class_def.components.is_empty() {
            info += "\n\n**Class Attributes:**\n| Name | Type | Description |\n|------|------|-------------|\n";

            for (attr_name, attr_comp) in &class_def.components {
                let mut attr_type = attr_comp.type_name.to_string();

                // Add shape if present
                if !attr_comp.shape.is_empty() {
                    attr_type += &format!(
                        "[{}]",
                        attr_comp
                            .shape
                            .iter()
                            .map(|d| d.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }

                // Add qualifiers
                let mut attr_qualifiers = Vec::new();
                match &attr_comp.variability {
                    Variability::Parameter(_) => attr_qualifiers.push("parameter"),
                    Variability::Constant(_) => attr_qualifiers.push("constant"),
                    _ => {}
                }
                match &attr_comp.causality {
                    Causality::Input(_) => attr_qualifiers.push("input"),
                    Causality::Output(_) => attr_qualifiers.push("output"),
                    _ => {}
                }
                if !attr_qualifiers.is_empty() {
                    attr_type = format!("{} {}", attr_qualifiers.join(" "), attr_type);
                }

                // Get description
                let attr_desc = if !attr_comp.description.is_empty() {
                    attr_comp
                        .description
                        .iter()
                        .map(|t| t.text.trim_matches('"').to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                } else {
                    String::new()
                };

                info += &format!("| {} | `{}` | {} |\n", attr_name, attr_type, attr_desc);
            }
        }

        // Show class functions
        let functions: Vec<_> = class_def
            .classes
            .iter()
            .filter(|(_, c)| c.class_type == ClassType::Function)
            .collect();

        if !functions.is_empty() {
            info += "\n**Class Functions:**\n";
            for (func_name, func_def) in functions {
                let inputs: Vec<_> = func_def
                    .components
                    .iter()
                    .filter(|(_, c)| matches!(c.causality, Causality::Input(_)))
                    .map(|(n, c)| format!("{}: {}", n, c.type_name))
                    .collect();
                let outputs: Vec<_> = func_def
                    .components
                    .iter()
                    .filter(|(_, c)| matches!(c.causality, Causality::Output(_)))
                    .map(|(n, c)| format!("{}: {}", n, c.type_name))
                    .collect();

                let sig = if outputs.is_empty() {
                    format!("{}({})", func_name, inputs.join(", "))
                } else {
                    format!(
                        "{}({}) -> ({})",
                        func_name,
                        inputs.join(", "),
                        outputs.join(", ")
                    )
                };

                info += &format!("- `{}`\n", sig);
            }
        }

        // Show equations
        if !class_def.equations.is_empty() {
            info += "\n**Equations:**\n```modelica\n";
            for eq in &class_def.equations {
                info += &format_equation(eq);
            }
            info += "```\n";
        }

        // Show initial equations
        if !class_def.initial_equations.is_empty() {
            info += "\n**Initial Equations:**\n```modelica\n";
            for eq in &class_def.initial_equations {
                info += &format_equation(eq);
            }
            info += "```\n";
        }
    }

    // Build instance-specific attributes table (modifications)
    let mut attrs = Vec::new();

    // Shape (array dimensions)
    if !comp.shape.is_empty() {
        let shape_str = format!(
            "[{}]",
            comp.shape
                .iter()
                .map(|d| d.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );
        attrs.push(("shape", shape_str));
    }

    // Start value
    if comp.start != Expression::Empty {
        attrs.push(("start", format_expr(&comp.start)));
    }

    // Common modifications: unit, displayUnit, min, max, nominal, fixed, stateSelect
    let important_mods = [
        "unit",
        "displayUnit",
        "min",
        "max",
        "nominal",
        "fixed",
        "stateSelect",
    ];
    for mod_name in important_mods {
        if let Some(expr) = comp.modifications.get(mod_name) {
            attrs.push((mod_name, format_expr(expr)));
        }
    }

    // Add any other modifications not in the important list
    for (mod_name, expr) in &comp.modifications {
        if !important_mods.contains(&mod_name.as_str()) {
            attrs.push((mod_name.as_str(), format_expr(expr)));
        }
    }

    if !attrs.is_empty() {
        info += "\n\n**Instance Modifications:**\n| Attribute | Value |\n|-----------|-------|\n";
        for (name, value) in attrs {
            info += &format!("| {} | `{}` |\n", name, value);
        }
    }

    info
}

/// Extract Documentation info attribute from an annotation
fn extract_documentation_info(annotation: &[Expression]) -> Option<String> {
    for expr in annotation {
        if let Expression::FunctionCall { comp, args } = expr {
            // Check if this is a Documentation(...) call
            if comp.to_string() == "Documentation" {
                // Look for info="..." argument
                for arg in args {
                    if let Expression::Binary { op, lhs, rhs } = arg {
                        // Named arguments use Eq operator (name = value)
                        if matches!(op, crate::ir::ast::OpBinary::Eq(_))
                            && let Expression::ComponentReference(comp_ref) = lhs.as_ref()
                            && comp_ref.to_string() == "info"
                            && let Expression::Terminal { token, .. } = rhs.as_ref()
                        {
                            // Extract the HTML content from the string
                            let html = token.text.trim_matches('"');
                            return Some(html.to_string());
                        }
                    }
                }
            }
        }
    }
    None
}

/// Convert basic HTML to Markdown for hover display
fn html_to_markdown(html: &str) -> String {
    let mut result = html.to_string();

    // Remove <html> tags
    result = result.replace("<html>", "").replace("</html>", "");

    // Handle <pre> blocks first - convert to code blocks
    // We need to handle <blockquote><pre> and standalone <pre>
    let mut processed = String::new();
    let mut remaining = result.as_str();

    while let Some(pre_start) = remaining.find("<pre>") {
        // Add everything before <pre>
        processed.push_str(&remaining[..pre_start]);

        // Find the closing </pre>
        let after_pre_tag = &remaining[pre_start + 5..];
        if let Some(pre_end) = after_pre_tag.find("</pre>") {
            // Extract pre content and convert to code block
            // Only trim newlines, preserve internal whitespace for ASCII art
            let pre_content = &after_pre_tag[..pre_end];
            let pre_content = pre_content.trim_start_matches('\n').trim_end_matches('\n');
            processed.push_str("\n```\n");
            processed.push_str(pre_content);
            processed.push_str("\n```\n");
            remaining = &after_pre_tag[pre_end + 6..];
        } else {
            // No closing tag, just add the rest
            processed.push_str(remaining);
            remaining = "";
        }
    }
    processed.push_str(remaining);
    result = processed;

    // Remove blockquote tags (the pre content is already handled)
    result = result
        .replace("<blockquote>", "")
        .replace("</blockquote>", "");

    // Convert common HTML tags to Markdown
    result = result.replace("<p>", "\n\n").replace("</p>", "");
    result = result
        .replace("<br>", "\n")
        .replace("<br/>", "\n")
        .replace("<br />", "\n");
    result = result.replace("<b>", "**").replace("</b>", "**");
    result = result.replace("<strong>", "**").replace("</strong>", "**");
    result = result.replace("<i>", "*").replace("</i>", "*");
    result = result.replace("<em>", "*").replace("</em>", "*");
    result = result.replace("<code>", "`").replace("</code>", "`");

    // Convert headers
    result = result.replace("<h1>", "\n# ").replace("</h1>", "\n");
    result = result.replace("<h2>", "\n## ").replace("</h2>", "\n");
    result = result.replace("<h3>", "\n### ").replace("</h3>", "\n");
    result = result.replace("<h4>", "\n#### ").replace("</h4>", "\n");

    // Convert lists
    result = result.replace("<ul>", "\n").replace("</ul>", "\n");
    result = result.replace("<ol>", "\n").replace("</ol>", "\n");
    result = result.replace("<li>", "- ").replace("</li>", "\n");

    // Clean up extra whitespace
    result = result.trim().to_string();

    // Simple removal of remaining HTML tags (without regex)
    let mut clean = String::new();
    let mut in_tag = false;
    for ch in result.chars() {
        if ch == '<' {
            in_tag = true;
        } else if ch == '>' {
            in_tag = false;
        } else if !in_tag {
            clean.push(ch);
        }
    }

    // Clean up multiple consecutive newlines
    let mut final_result = String::new();
    let mut prev_newline_count = 0;
    for ch in clean.chars() {
        if ch == '\n' {
            prev_newline_count += 1;
            if prev_newline_count <= 2 {
                final_result.push(ch);
            }
        } else {
            prev_newline_count = 0;
            final_result.push(ch);
        }
    }

    final_result.trim().to_string()
}

/// Format hover info for a class definition
fn format_class_hover(class_def: &crate::ir::ast::ClassDefinition, name: &str) -> String {
    // Class type and name header
    let class_type_str = format!("{:?}", class_def.class_type).to_lowercase();
    let mut info = format!("```modelica\n{} {}\n```", class_type_str, name);

    // Add documentation string if present
    if !class_def.description.is_empty() {
        let desc = class_def
            .description
            .iter()
            .map(|t| t.text.trim_matches('"').to_string())
            .collect::<Vec<_>>()
            .join(" ");
        info += &format!("\n\n*{}*", desc);
    }

    // Add Documentation annotation info if present
    if let Some(doc_html) = extract_documentation_info(&class_def.annotation) {
        let doc_md = html_to_markdown(&doc_html);
        if !doc_md.is_empty() {
            info += &format!("\n\n---\n\n{}", doc_md);
        }
    }

    // For functions, show the signature
    if class_def.class_type == ClassType::Function {
        let mut inputs = Vec::new();
        let mut outputs = Vec::new();

        for (comp_name, comp) in &class_def.components {
            match &comp.causality {
                Causality::Input(_) => {
                    inputs.push(format!("{}: {}", comp_name, comp.type_name));
                }
                Causality::Output(_) => {
                    outputs.push(format!("{}: {}", comp_name, comp.type_name));
                }
                _ => {}
            }
        }

        info += &format!(
            "\n\n**Signature:**\n```modelica\n{}({}) -> ({})\n```",
            name,
            inputs.join(", "),
            outputs.join(", ")
        );
    }

    // List member functions (nested classes that are functions)
    let functions: Vec<_> = class_def
        .classes
        .iter()
        .filter(|(_, c)| c.class_type == ClassType::Function)
        .collect();

    if !functions.is_empty() {
        info += "\n\n**Functions:**\n";
        for (func_name, func_def) in functions {
            // Build function signature
            let inputs: Vec<_> = func_def
                .components
                .iter()
                .filter(|(_, c)| matches!(c.causality, Causality::Input(_)))
                .map(|(n, c)| format!("{}: {}", n, c.type_name))
                .collect();
            let outputs: Vec<_> = func_def
                .components
                .iter()
                .filter(|(_, c)| matches!(c.causality, Causality::Output(_)))
                .map(|(n, c)| format!("{}: {}", n, c.type_name))
                .collect();

            let sig = if outputs.is_empty() {
                format!("{}({})", func_name, inputs.join(", "))
            } else {
                format!(
                    "{}({}) -> ({})",
                    func_name,
                    inputs.join(", "),
                    outputs.join(", ")
                )
            };

            // Add description if present
            let desc = if !func_def.description.is_empty() {
                let d = func_def
                    .description
                    .iter()
                    .map(|t| t.text.trim_matches('"').to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                format!(" - {}", d)
            } else {
                String::new()
            };

            info += &format!("- `{}`{}\n", sig, desc);
        }
    }

    // List attributes/components (excluding function inputs/outputs which are already shown)
    if class_def.class_type != ClassType::Function && !class_def.components.is_empty() {
        info +=
            "\n\n**Attributes:**\n| Name | Type | Description |\n|------|------|-------------|\n";

        for (comp_name, comp) in &class_def.components {
            let mut type_str = comp.type_name.to_string();

            // Add shape if present
            if !comp.shape.is_empty() {
                type_str += &format!(
                    "[{}]",
                    comp.shape
                        .iter()
                        .map(|d| d.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }

            // Add qualifiers
            let mut qualifiers = Vec::new();
            match &comp.variability {
                Variability::Parameter(_) => qualifiers.push("parameter"),
                Variability::Constant(_) => qualifiers.push("constant"),
                _ => {}
            }
            match &comp.causality {
                Causality::Input(_) => qualifiers.push("input"),
                Causality::Output(_) => qualifiers.push("output"),
                _ => {}
            }
            if !qualifiers.is_empty() {
                type_str = format!("{} {}", qualifiers.join(" "), type_str);
            }

            // Get description
            let desc = if !comp.description.is_empty() {
                comp.description
                    .iter()
                    .map(|t| t.text.trim_matches('"').to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            } else {
                String::new()
            };

            info += &format!("| {} | `{}` | {} |\n", comp_name, type_str, desc);
        }
    }

    // List nested classes (non-functions)
    let nested_classes: Vec<_> = class_def
        .classes
        .iter()
        .filter(|(_, c)| c.class_type != ClassType::Function)
        .collect();

    if !nested_classes.is_empty() {
        info += "\n\n**Nested Types:**\n";
        for (nested_name, nested_def) in nested_classes {
            let nested_type = format!("{:?}", nested_def.class_type).to_lowercase();
            let desc = if !nested_def.description.is_empty() {
                let d = nested_def
                    .description
                    .iter()
                    .map(|t| t.text.trim_matches('"').to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                format!(" - {}", d)
            } else {
                String::new()
            };
            info += &format!("- `{} {}`{}\n", nested_type, nested_name, desc);
        }
    }

    // Show equations
    if !class_def.equations.is_empty() {
        info += "\n\n**Equations:**\n```modelica\n";
        for eq in &class_def.equations {
            info += &format_equation(eq);
        }
        info += "```";
    }

    // Show initial equations
    if !class_def.initial_equations.is_empty() {
        info += "\n\n**Initial Equations:**\n```modelica\n";
        for eq in &class_def.initial_equations {
            info += &format_equation(eq);
        }
        info += "```";
    }

    info
}

/// Find the import path for a name if it matches an import in the class
fn find_import_path_for_name(class: &ClassDefinition, name: &str) -> Option<String> {
    for import in &class.imports {
        match import {
            Import::Renamed { alias, path, .. } => {
                if alias.text == name {
                    return Some(path.to_string());
                }
            }
            Import::Qualified { path, .. } => {
                // For `import A.B.C;`, the alias is "C"
                if let Some(last) = path.name.last()
                    && last.text == name
                {
                    return Some(path.to_string());
                }
            }
            Import::Unqualified { path, .. } => {
                // For `import A.B.*;`, try path.name
                return Some(format!("{}.{}", path, name));
            }
            Import::Selective { path, names, .. } => {
                // For `import A.B.{C, D};`, check if name matches
                for n in names {
                    if n.text == name {
                        return Some(format!("{}.{}", path, name));
                    }
                }
            }
        }
    }

    // Check nested classes
    for nested in class.classes.values() {
        if let Some(path) = find_import_path_for_name(nested, name) {
            return Some(path);
        }
    }

    None
}
