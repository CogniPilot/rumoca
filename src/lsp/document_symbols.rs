//! Document symbols handler for Modelica files (file outline).

use std::collections::HashMap;

use lsp_types::{
    DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, Position, Range, SymbolKind, Uri,
};

use crate::ir::ast::{Causality, ClassDefinition, ClassType, Variability};

use super::utils::parse_document;

/// Handle document symbols request - provides file outline
pub fn handle_document_symbols(
    documents: &HashMap<Uri, String>,
    params: DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let uri = &params.text_document.uri;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let ast = parse_document(text, path)?;
    let mut symbols = Vec::new();

    for (class_name, class_def) in &ast.class_list {
        if let Some(symbol) = build_class_symbol(class_name, class_def, text) {
            symbols.push(symbol);
        }
    }

    Some(DocumentSymbolResponse::Nested(symbols))
}

/// Build a DocumentSymbol for a class definition with its children
#[allow(deprecated)] // DocumentSymbol::deprecated is deprecated but still required
fn build_class_symbol(name: &str, class: &ClassDefinition, text: &str) -> Option<DocumentSymbol> {
    let kind = match class.class_type {
        ClassType::Model => SymbolKind::CLASS,
        ClassType::Block => SymbolKind::CLASS,
        ClassType::Connector => SymbolKind::INTERFACE,
        ClassType::Record => SymbolKind::STRUCT,
        ClassType::Type => SymbolKind::TYPE_PARAMETER,
        ClassType::Package => SymbolKind::NAMESPACE,
        ClassType::Function => SymbolKind::FUNCTION,
        ClassType::Class => SymbolKind::CLASS,
        _ => SymbolKind::CLASS,
    };

    let start_line = class.name.location.start_line.saturating_sub(1);
    let start_col = class.name.location.start_column.saturating_sub(1);

    // Find the end of the class by looking for "end ClassName"
    let end_pattern = format!("end {};", name);
    let end_pattern_semi = format!("end {}", name);
    let lines: Vec<&str> = text.lines().collect();
    let mut end_line = start_line;
    let mut end_col = 0u32;

    for (i, line) in lines.iter().enumerate().skip(start_line as usize) {
        if line.contains(&end_pattern) || line.trim().starts_with(&end_pattern_semi) {
            end_line = i as u32;
            end_col = line.len() as u32;
            break;
        }
    }

    let range = Range {
        start: Position {
            line: start_line,
            character: start_col,
        },
        end: Position {
            line: end_line,
            character: end_col,
        },
    };

    let selection_range = Range {
        start: Position {
            line: start_line,
            character: start_col,
        },
        end: Position {
            line: start_line,
            character: start_col + name.len() as u32,
        },
    };

    // Build children symbols
    let mut children = Vec::new();

    // Group components by category
    let mut parameters = Vec::new();
    let mut variables = Vec::new();
    let mut inputs = Vec::new();
    let mut outputs = Vec::new();

    for (comp_name, comp) in &class.components {
        let (comp_kind, category) = match (&comp.variability, &comp.causality) {
            (Variability::Parameter(_), _) => (SymbolKind::CONSTANT, &mut parameters),
            (Variability::Constant(_), _) => (SymbolKind::CONSTANT, &mut parameters),
            (_, Causality::Input(_)) => (SymbolKind::PROPERTY, &mut inputs),
            (_, Causality::Output(_)) => (SymbolKind::PROPERTY, &mut outputs),
            _ => (SymbolKind::VARIABLE, &mut variables),
        };

        let comp_line = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_line.saturating_sub(1))
            .unwrap_or(start_line);
        let comp_col = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_column.saturating_sub(1))
            .unwrap_or(0);

        let mut detail = comp.type_name.to_string();
        if !comp.shape.is_empty() {
            detail += &format!(
                "[{}]",
                comp.shape
                    .iter()
                    .map(|d| d.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

        category.push(DocumentSymbol {
            name: comp_name.clone(),
            detail: Some(detail),
            kind: comp_kind,
            tags: None,
            deprecated: None,
            range: Range {
                start: Position {
                    line: comp_line,
                    character: comp_col,
                },
                end: Position {
                    line: comp_line,
                    character: comp_col + comp_name.len() as u32 + 20,
                },
            },
            selection_range: Range {
                start: Position {
                    line: comp_line,
                    character: comp_col,
                },
                end: Position {
                    line: comp_line,
                    character: comp_col + comp_name.len() as u32,
                },
            },
            children: None,
        });
    }

    // Add grouped sections if they have content
    if !parameters.is_empty() {
        children.push(DocumentSymbol {
            name: "Parameters".to_string(),
            detail: Some(format!("{} items", parameters.len())),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range,
            selection_range,
            children: Some(parameters),
        });
    }

    if !inputs.is_empty() {
        children.push(DocumentSymbol {
            name: "Inputs".to_string(),
            detail: Some(format!("{} items", inputs.len())),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range,
            selection_range,
            children: Some(inputs),
        });
    }

    if !outputs.is_empty() {
        children.push(DocumentSymbol {
            name: "Outputs".to_string(),
            detail: Some(format!("{} items", outputs.len())),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range,
            selection_range,
            children: Some(outputs),
        });
    }

    if !variables.is_empty() {
        children.push(DocumentSymbol {
            name: "Variables".to_string(),
            detail: Some(format!("{} items", variables.len())),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range,
            selection_range,
            children: Some(variables),
        });
    }

    // Add nested classes (functions, records, etc.)
    for (nested_name, nested_class) in &class.classes {
        if let Some(nested_symbol) = build_class_symbol(nested_name, nested_class, text) {
            children.push(nested_symbol);
        }
    }

    // Count equations
    let equation_count = class.equations.len() + class.initial_equations.len();
    if equation_count > 0 {
        children.push(DocumentSymbol {
            name: "Equations".to_string(),
            detail: Some(format!("{} equations", equation_count)),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range,
            selection_range,
            children: None,
        });
    }

    // Count algorithms
    let algorithm_count = class.algorithms.len() + class.initial_algorithms.len();
    if algorithm_count > 0 {
        children.push(DocumentSymbol {
            name: "Algorithms".to_string(),
            detail: Some(format!("{} algorithm sections", algorithm_count)),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range,
            selection_range,
            children: None,
        });
    }

    let detail = format!("{:?}", class.class_type);

    Some(DocumentSymbol {
        name: name.to_string(),
        detail: Some(detail),
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: if children.is_empty() {
            None
        } else {
            Some(children)
        },
    })
}
