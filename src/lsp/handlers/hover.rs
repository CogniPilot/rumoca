//! Hover information handler for Modelica files.

use std::collections::HashMap;

use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Position, Uri};

use crate::fmt::format_expression;
use crate::ir::ast::{Causality, ClassType, Component, Expression, StoredDefinition, Variability};
use crate::ir::transform::scope_resolver::{ResolvedSymbol, ScopeResolver};

use crate::lsp::data::builtin_functions::get_builtin_functions;
use crate::lsp::data::keywords::get_keyword_hover;
use crate::lsp::utils::{get_word_at_position, parse_document};

/// Handle hover request
pub fn handle_hover(documents: &HashMap<Uri, String>, params: HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let word = get_word_at_position(text, position)?;

    // First check for hover info from the AST
    if let Some(ast) = parse_document(text, path) {
        if let Some(hover_text) = get_ast_hover_info(&ast, &word, position) {
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
    let functions = get_builtin_functions();
    for func in &functions {
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
                // Look up the class definition for this component's type
                let type_class = find_class_definition(ast, &component.type_name.to_string());
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
                return Some(format_class_hover(class_def, word));
            }
        }
    }

    // Fall back: check if word is a class name anywhere
    if let Some(class_def) = ast.class_list.get(word) {
        return Some(format_class_hover(class_def, word));
    }

    // Check nested classes in all top-level classes
    for class in ast.class_list.values() {
        if let Some(nested) = class.classes.get(word) {
            return Some(format_class_hover(nested, word));
        }
    }

    None
}

/// Helper to format an expression for display
fn format_expr(expr: &Expression) -> String {
    format_expression(expr)
}

/// Find a class definition by name in the AST
fn find_class_definition<'a>(
    ast: &'a StoredDefinition,
    type_name: &str,
) -> Option<&'a crate::ir::ast::ClassDefinition> {
    // Handle qualified names like "SO2.SO2LieGroupElement"
    let parts: Vec<&str> = type_name.split('.').collect();

    if parts.len() == 1 {
        // Simple name - check top-level classes
        if let Some(class) = ast.class_list.get(type_name) {
            return Some(class);
        }
        // Check nested classes in all top-level classes
        for class in ast.class_list.values() {
            if let Some(nested) = class.classes.get(type_name) {
                return Some(nested);
            }
        }
    } else if parts.len() >= 2 {
        // Qualified name - navigate through the hierarchy
        let first = parts[0];
        if let Some(mut current) = ast.class_list.get(first) {
            for part in &parts[1..] {
                if let Some(nested) = current.classes.get(*part) {
                    current = nested;
                } else {
                    return None;
                }
            }
            return Some(current);
        }
    }

    None
}

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

    info
}
