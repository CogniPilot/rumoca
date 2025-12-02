//! Hover information handler for Modelica files.

use std::collections::HashMap;

use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Position, Uri};

use crate::ir::ast::{Causality, ClassType, Component, StoredDefinition, Variability};
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
                let mut info = format_component_hover(component);

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

/// Format hover info for a component
fn format_component_hover(comp: &Component) -> String {
    let mut info = format!("**{}**: {}", comp.name, comp.type_name);

    if !comp.shape.is_empty() {
        info += &format!(
            "[{}]",
            comp.shape
                .iter()
                .map(|d| d.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );
    }

    match &comp.variability {
        Variability::Parameter(_) => info += " (parameter)",
        Variability::Constant(_) => info += " (constant)",
        Variability::Discrete(_) => info += " (discrete)",
        _ => {}
    }

    match &comp.causality {
        Causality::Input(_) => info += " [input]",
        Causality::Output(_) => info += " [output]",
        _ => {}
    }

    if !comp.description.is_empty() {
        let desc = comp
            .description
            .iter()
            .map(|t| t.text.trim_matches('"').to_string())
            .collect::<Vec<_>>()
            .join(" ");
        info += &format!("\n\n{}", desc);
    }

    info
}

/// Format hover info for a class definition
fn format_class_hover(class_def: &crate::ir::ast::ClassDefinition, name: &str) -> String {
    let mut info = format!("**{:?}** {}", class_def.class_type, name);

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

        info += &format!("\n\n```modelica\nfunction {}({})", name, inputs.join(", "));
        if !outputs.is_empty() {
            info += &format!(" -> ({})", outputs.join(", "));
        }
        info += "\n```";
    }

    info
}
