//! Enhanced hover handler for Modelica files.

use lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};
use rumoca_ir_ast as ast;

use crate::helpers::{find_class_at_position, find_component_at_position, get_word_at_position};

/// Handle hover request - returns type/keyword/component info at position.
pub fn handle_hover(
    source: &str,
    ast: Option<&ast::StoredDefinition>,
    line: u32,
    character: u32,
) -> Option<Hover> {
    let position = Position { line, character };
    let word = get_word_at_position(source, position)?;

    // Try component hover first (most specific)
    if let Some(hover) = ast.and_then(|a| component_hover(a, &word)) {
        return Some(hover);
    }

    // Try class hover
    if let Some(hover) = ast.and_then(|a| class_hover(a, &word)) {
        return Some(hover);
    }

    // Try builtin function hover
    if let Some(hover) = builtin_hover(&word) {
        return Some(hover);
    }

    // Fall back to keyword hover
    let info = get_keyword_info(&word)?;
    Some(make_hover(&info))
}

fn make_hover(value: &str) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: value.to_string(),
        }),
        range: None,
    }
}

fn component_hover(ast: &ast::StoredDefinition, name: &str) -> Option<Hover> {
    let comp = find_component_at_position(ast, name)?;
    let info = format_component_info(comp);
    Some(make_hover(&info))
}

fn format_component_info(comp: &ast::Component) -> String {
    let mut parts = Vec::new();

    // ast::Variability prefix
    match &comp.variability {
        ast::Variability::Parameter(_) => parts.push("parameter".to_string()),
        ast::Variability::Constant(_) => parts.push("constant".to_string()),
        _ => {}
    }

    // Type name
    parts.push(comp.type_name.to_string());

    // ast::Component name
    parts.push(comp.name.clone());

    // Array shape
    if !comp.shape.is_empty() {
        let dims: Vec<String> = comp.shape.iter().map(|d| d.to_string()).collect();
        let last = parts.len() - 1;
        parts[last] = format!("{}[{}]", parts[last], dims.join(", "));
    }

    let mut result = format!("```modelica\n{}\n```", parts.join(" "));

    // Description string
    if let Some(desc) = comp.description.first() {
        result.push_str(&format!("\n\n{}", desc.text));
    }

    result
}

fn class_hover(ast: &ast::StoredDefinition, name: &str) -> Option<Hover> {
    let class = find_class_at_position(ast, name)?;
    let info = format_class_info(name, class);
    Some(make_hover(&info))
}

fn format_class_info(name: &str, class: &ast::ClassDef) -> String {
    let class_type = match class.class_type {
        ast::ClassType::Model => "model",
        ast::ClassType::Block => "block",
        ast::ClassType::Connector => "connector",
        ast::ClassType::Record => "record",
        ast::ClassType::Type => "type",
        ast::ClassType::Package => "package",
        ast::ClassType::Function => "function",
        ast::ClassType::Class => "class",
        ast::ClassType::Operator => "operator",
    };

    let mut result = format!("```modelica\n{} {}\n```", class_type, name);

    // Description
    if let Some(desc) = class.description.first() {
        result.push_str(&format!("\n\n{}", desc.text));
    }

    // Counts
    let n_comp = class.components.len();
    let n_eq = class.equations.len() + class.initial_equations.len();
    if n_comp > 0 || n_eq > 0 {
        result.push_str(&format!("\n\n{} components, {} equations", n_comp, n_eq));
    }

    result
}

fn builtin_hover(name: &str) -> Option<Hover> {
    if !rumoca_core::is_builtin_function(name) {
        return None;
    }
    let info = format!("**{}** — Built-in Modelica function", name);
    Some(make_hover(&info))
}

/// Get hover info for Modelica keywords.
fn get_keyword_info(word: &str) -> Option<String> {
    let info = match word {
        "model" => {
            "**model** — Define a Modelica model class\n\nA model can contain variables, parameters, equations, and algorithms."
        }
        "package" => {
            "**package** — Define a Modelica package\n\nA package organizes classes into a namespace."
        }
        "function" => {
            "**function** — Define a Modelica function\n\nFunctions compute outputs from inputs using algorithms."
        }
        "connector" => {
            "**connector** — Define a connector class\n\nConnectors define the interface for physical connections between components."
        }
        "record" => "**record** — Define a record class\n\nRecords group data without equations.",
        "block" => {
            "**block** — Define a block class\n\nBlocks are models with fixed input/output interfaces."
        }
        "type" => "**type** — Define a type alias\n\nCreates a new type based on an existing one.",
        "parameter" => {
            "**parameter** — Parameter variability\n\nParameters are constant during simulation but can be changed between runs."
        }
        "constant" => {
            "**constant** — Constant variability\n\nConstants never change and are set at compile time."
        }
        "input" => "**input** — Input causality\n\nInput variables are provided externally.",
        "output" => "**output** — Output causality\n\nOutput variables are computed by the model.",
        "extends" => {
            "**extends** — Inherit from a base class\n\nInherits all declarations and equations from the specified class."
        }
        "equation" => {
            "**equation** — Equation section\n\nDefines the mathematical relationships of the model."
        }
        "algorithm" => {
            "**algorithm** — Algorithm section\n\nDefines imperative computation sequences."
        }
        "der" => {
            "**der(x)** — Time derivative\n\nReturns the time derivative of a continuous state variable."
        }
        "connect" => {
            "**connect(a, b)** — Connect two connectors\n\nCreates physical connections between component interfaces."
        }
        "Real" => "**Real** — Real number type\n\nDouble-precision floating-point number.",
        "Integer" => "**Integer** — Integer type\n\nSigned integer number.",
        "Boolean" => "**Boolean** — Boolean type\n\nTrue or false value.",
        "String" => "**String** — String type\n\nText string value.",
        "time" => {
            "**time** — Simulation time\n\nBuilt-in variable representing the current simulation time."
        }
        _ => return None,
    };
    Some(info.to_string())
}
