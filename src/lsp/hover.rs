//! Hover information handler for Modelica files.

use std::collections::HashMap;

use lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Position, Uri};

use crate::ir::ast::{Causality, ClassType, StoredDefinition, Variability};

use super::builtin_functions::get_builtin_functions;
use super::utils::{get_word_at_position, parse_document};

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
    let hover_text = get_keyword_hover_info(&word)?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: hover_text,
        }),
        range: None,
    })
}

/// Get hover info from the AST for user-defined symbols
fn get_ast_hover_info(ast: &StoredDefinition, word: &str, _position: Position) -> Option<String> {
    for class in ast.class_list.values() {
        if let Some(comp) = class.components.get(word) {
            let mut info = format!("**{}**: {}", word, comp.type_name);

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

            return Some(info);
        }

        for (nested_name, nested_class) in &class.classes {
            if nested_name == word {
                let mut info = format!("**{:?}** {}", nested_class.class_type, nested_name);

                if nested_class.class_type == ClassType::Function {
                    let mut inputs = Vec::new();
                    let mut outputs = Vec::new();

                    for (comp_name, comp) in &nested_class.components {
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
                        "\n\n```modelica\nfunction {}({})",
                        nested_name,
                        inputs.join(", ")
                    );
                    if !outputs.is_empty() {
                        info += &format!(" -> ({})", outputs.join(", "));
                    }
                    info += "\n```";
                }

                return Some(info);
            }

            if let Some(comp) = nested_class.components.get(word) {
                return Some(format!("**{}**: {}", word, comp.type_name));
            }
        }
    }

    if let Some(class_def) = ast.class_list.get(word) {
        return Some(format!("**{:?}** {}", class_def.class_type, word));
    }

    None
}

/// Get hover info for Modelica keywords
fn get_keyword_hover_info(word: &str) -> Option<String> {
    let info = match word {
        "model" => {
            "**model**\n\nA model is a class that can contain equations and may be instantiated."
        }
        "class" => {
            "**class**\n\nA general class definition. Models, connectors, and other specialized classes inherit from class."
        }
        "connector" => {
            "**connector**\n\nA connector defines the interface for connections between components."
        }
        "package" => "**package**\n\nA package is a namespace for organizing classes and models.",
        "function" => {
            "**function**\n\nA function is a class that computes outputs from inputs using algorithms."
        }
        "record" => "**record**\n\nA record is a class used as a data structure without equations.",
        "block" => "**block**\n\nA block is a class with fixed causality for inputs and outputs.",
        "type" => "**type**\n\nDefines a type alias or derived type.",
        "parameter" => {
            "**parameter**\n\nA parameter is a variable that remains constant during simulation but can be changed between simulations."
        }
        "constant" => "**constant**\n\nA constant is a variable whose value cannot be changed.",
        "input" => "**input**\n\nDeclares an input connector variable.",
        "output" => "**output**\n\nDeclares an output connector variable.",
        "flow" => "**flow**\n\nDeclares a flow variable (summed to zero in connections).",
        "stream" => "**stream**\n\nDeclares a stream variable for bidirectional flow.",
        "Real" => {
            "**Real**\n\nFloating-point number type.\n\nAttributes: `unit`, `displayUnit`, `min`, `max`, `start`, `fixed`, `nominal`, `stateSelect`"
        }
        "Integer" => {
            "**Integer**\n\nInteger number type.\n\nAttributes: `min`, `max`, `start`, `fixed`"
        }
        "Boolean" => {
            "**Boolean**\n\nBoolean type with values `true` and `false`.\n\nAttributes: `start`, `fixed`"
        }
        "String" => "**String**\n\nString type for text.\n\nAttributes: `start`",
        "time" => "**time**\n\nBuilt-in variable representing simulation time.",
        "equation" => {
            "**equation**\n\nSection containing equations that define the mathematical relationships."
        }
        "algorithm" => "**algorithm**\n\nSection containing sequential assignment statements.",
        "when" => {
            "**when**\n\nEvent-triggered section. Equations inside are active only when condition becomes true."
        }
        "if" => "**if**\n\nConditional expression or statement.",
        "for" => "**for**\n\nLoop construct for iteration.",
        "extends" => "**extends**\n\nInheritance from a base class.",
        "import" => "**import**\n\nImports classes from other packages.",
        "within" => "**within**\n\nSpecifies the package this file belongs to.",
        "annotation" => "**annotation**\n\nMetadata for documentation, icons, experiments, etc.",
        _ => return None,
    };
    Some(info.to_string())
}
