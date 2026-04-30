//! Enhanced hover handler for Modelica files.

use lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};
use rumoca_compile::compile::core as rumoca_core;
use rumoca_compile::parsing::ast;
use rumoca_compile::parsing::ir_core as rumoca_ir_core;

use crate::helpers::{
    find_class_at_position, find_component_at_position, find_enclosing_class,
    get_qualified_class_name_at_position, get_word_at_position, imported_def_id,
    parsed_class_by_qualified_name, resolve_at_position,
};

/// Handle hover request - returns type/keyword/component info at position.
pub fn handle_hover(
    source: &str,
    ast: Option<&ast::StoredDefinition>,
    tree: Option<&ast::ClassTree>,
    line: u32,
    character: u32,
) -> Option<Hover> {
    let position = Position { line, character };
    let word = get_word_at_position(source, position)?;

    // Try component hover first (most specific)
    if let Some(hover) = ast.and_then(|a| component_hover(a, &word)) {
        return Some(hover);
    }

    if let Some(hover) = ast.and_then(|a| imported_class_hover(source, a, line, &word)) {
        return Some(hover);
    }

    if let Some(hover) = ast.and_then(|a| {
        qualified_class_hover(source, a, tree, position)
            .or_else(|| qualified_class_ast_hover(source, a, position))
    }) {
        return Some(hover);
    }

    // Try class hover
    if let Some(hover) = ast.and_then(|a| class_hover(a, &word)) {
        return Some(hover);
    }

    if let Some(hover) = ast.and_then(|a| tree.and_then(|t| resolved_class_hover(a, t, &word))) {
        return Some(hover);
    }

    builtin_or_keyword_hover(&word)
}

fn hover_for_qualified_class_name(
    ast: &ast::StoredDefinition,
    qualified_name: &str,
) -> Option<Hover> {
    let class = parsed_class_by_qualified_name(ast, qualified_name)?;
    let info = format_class_info(class.name.text.as_ref(), class);
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

pub(crate) fn builtin_or_keyword_hover(word: &str) -> Option<Hover> {
    if let Some(hover) = builtin_hover(word) {
        return Some(hover);
    }
    let info = get_keyword_info(word)?;
    Some(make_hover(&info))
}

fn component_hover(ast: &ast::StoredDefinition, name: &str) -> Option<Hover> {
    let comp = find_component_at_position(ast, name)?;
    let info = format_component_info(comp);
    Some(make_hover(&info))
}

fn format_component_info(comp: &ast::Component) -> String {
    let mut parts = Vec::new();

    // rumoca_ir_core::Variability prefix
    match &comp.variability {
        rumoca_ir_core::Variability::Parameter(_) => parts.push("parameter".to_string()),
        rumoca_ir_core::Variability::Constant(_) => parts.push("constant".to_string()),
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

fn imported_class_hover(
    source: &str,
    ast: &ast::StoredDefinition,
    line: u32,
    name: &str,
) -> Option<Hover> {
    if !is_import_line_for_name(source, line, name) {
        return None;
    }

    let class = find_enclosing_class(ast, line)?;
    let imported = class
        .imports
        .iter()
        .find_map(|import| imported_class_in_ast(ast, import, name))?;
    let info = format_class_info(imported.name.text.as_ref(), imported);
    Some(make_hover(&info))
}

fn qualified_class_ast_hover(
    source: &str,
    ast: &ast::StoredDefinition,
    position: Position,
) -> Option<Hover> {
    let qualified_name = get_qualified_class_name_at_position(source, position)?;
    hover_for_qualified_class_name(ast, &qualified_name)
}

fn qualified_class_hover(
    source: &str,
    ast: &ast::StoredDefinition,
    tree: Option<&ast::ClassTree>,
    position: Position,
) -> Option<Hover> {
    let qualified_name = get_qualified_class_name_at_position(source, position)?;
    let tree = tree?;
    let def_id = tree.get_def_id_by_name(&qualified_name)?;
    let class = tree
        .get_class_by_def_id(def_id)
        .or_else(|| parsed_class_by_qualified_name(ast, &qualified_name))?;
    let info = format_class_info(class.name.text.as_ref(), class);
    Some(make_hover(&info))
}

fn is_import_line_for_name(source: &str, line: u32, name: &str) -> bool {
    source
        .lines()
        .nth(line as usize)
        .map(str::trim_start)
        .is_some_and(|line_text| line_text.starts_with("import ") && line_text.contains(name))
}

fn imported_class_in_ast<'a>(
    ast: &'a ast::StoredDefinition,
    import: &ast::Import,
    name: &str,
) -> Option<&'a ast::ClassDef> {
    let qualified_name = match import {
        ast::Import::Qualified { path, .. } => {
            let last = path.name.last()?.text.as_ref();
            if last != name {
                return None;
            }
            path.to_string()
        }
        ast::Import::Renamed { alias, path, .. } => {
            if alias.text.as_ref() != name {
                return None;
            }
            path.to_string()
        }
        ast::Import::Unqualified { path, .. } => format!("{}.{}", path, name),
        ast::Import::Selective { path, names, .. } => {
            if !names.iter().any(|token| token.text.as_ref() == name) {
                return None;
            }
            format!("{}.{}", path, name)
        }
    };
    parsed_class_by_qualified_name(ast, &qualified_name)
}

fn resolved_class_hover(
    ast: &ast::StoredDefinition,
    tree: &ast::ClassTree,
    name: &str,
) -> Option<Hover> {
    let def_id =
        resolve_at_position(ast, tree, name).or_else(|| imported_class_def_id(ast, tree, name))?;
    let class = tree.get_class_by_def_id(def_id)?;
    let info = format_class_info(class.name.text.as_ref(), class);
    Some(make_hover(&info))
}

fn imported_class_def_id(
    ast: &ast::StoredDefinition,
    tree: &ast::ClassTree,
    name: &str,
) -> Option<rumoca_core::DefId> {
    for class in ast.classes.values() {
        if let Some(def_id) = imported_class_def_id_in_class(class, tree, name) {
            return Some(def_id);
        }
    }
    None
}

fn imported_class_def_id_in_class(
    class: &ast::ClassDef,
    tree: &ast::ClassTree,
    name: &str,
) -> Option<rumoca_core::DefId> {
    for import in &class.imports {
        if let Some(def_id) = imported_def_id(import, tree, name) {
            return Some(def_id);
        }
    }
    for nested in class.classes.values() {
        if let Some(def_id) = imported_class_def_id_in_class(nested, tree, name) {
            return Some(def_id);
        }
    }
    None
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

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::HoverContents;

    #[test]
    fn hover_resolves_imported_class_from_navigation_tree() {
        let source = r#"model Ball
  import Modelica.Blocks.Continuous.PID;
  PID pid;
end Ball;
"#;
        let broken = "model Broken\n  Real x\nend Broken;\n";
        let source_root = r#"package Modelica
  package Blocks
    package Continuous
      block PID
        Real u;
        Real y;
      equation
        y = u;
      end PID;
    end Continuous;
  end Blocks;
end Modelica;
"#;

        let mut session = rumoca_compile::Session::default();
        session.update_document("ball.mo", source);
        let parse_error = session.update_document("broken.mo", broken);
        assert!(parse_error.is_some(), "broken document should stay invalid");
        session.update_document("Modelica.mo", source_root);

        let ast = session
            .get_document("ball.mo")
            .and_then(|doc| doc.parsed().cloned())
            .expect("ball AST");
        let resolved = session
            .resolved_for_semantic_navigation("Ball")
            .expect("semantic navigation tree");
        let import_line = source.lines().nth(1).expect("import line");
        let char_pos = import_line.find("PID").expect("PID token") as u32 + 1;
        let hover = handle_hover(source, Some(&ast), Some(&resolved.0), 1, char_pos)
            .expect("hover should resolve imported class");

        let HoverContents::Markup(contents) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            contents.value.contains("block PID"),
            "expected imported class hover, got: {}",
            contents.value
        );
    }

    #[test]
    fn hover_resolves_imported_alias_from_ast() {
        let source = r#"package Lib
  block Target
    Real y;
  equation
    y = 1;
  end Target;
end Lib;

model M
  import Alias = Lib.Target;
  Alias a;
equation
  a.y = 1;
end M;
"#;
        let ast = rumoca_compile::parsing::parse_source_to_ast(source, "input.mo")
            .expect("parse should succeed");
        let import_line = source.lines().nth(9).expect("import line");
        let char_pos = import_line.find("Alias").expect("Alias token") as u32 + 1;
        let hover = handle_hover(source, Some(&ast), None, 9, char_pos)
            .expect("hover should resolve imported alias from AST");

        let HoverContents::Markup(contents) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            contents.value.contains("block Target"),
            "expected imported alias hover, got: {}",
            contents.value
        );
    }
}
