//! Document symbols handler for Modelica files (file outline).

use std::collections::HashMap;

use lsp_types::{
    DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, Position, Range, SymbolKind, Uri,
};

use crate::ir::ast::{Causality, ClassDefinition, ClassType, Variability};

use crate::lsp::utils::{location_to_range, parse_document, token_to_range};

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
        if let Some(symbol) = build_class_symbol(class_name, class_def) {
            symbols.push(symbol);
        }
    }

    Some(DocumentSymbolResponse::Nested(symbols))
}

/// Build a DocumentSymbol for a class definition with its children
#[allow(deprecated)] // DocumentSymbol::deprecated is deprecated but still required
fn build_class_symbol(name: &str, class: &ClassDefinition) -> Option<DocumentSymbol> {
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

    // Use the class location (spans from class keyword to end statement)
    let range = location_to_range(&class.location);

    // Selection range is the class name token
    let selection_range = token_to_range(&class.name);

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

        // Use proper location data from the component
        let comp_range = location_to_range(&comp.location);

        // Selection range is the component name token
        let comp_selection_range = token_to_range(&comp.name_token);

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
            range: comp_range,
            selection_range: comp_selection_range,
            children: None,
        });
    }

    // Helper to compute the bounding range of a list of symbols
    // Returns a valid range that is guaranteed to have start <= end
    fn compute_group_range(symbols: &[DocumentSymbol]) -> Range {
        let mut min_start = Position {
            line: u32::MAX,
            character: u32::MAX,
        };
        let mut max_end = Position {
            line: 0,
            character: 0,
        };
        for sym in symbols {
            if sym.range.start.line < min_start.line
                || (sym.range.start.line == min_start.line
                    && sym.range.start.character < min_start.character)
            {
                min_start = sym.range.start;
            }
            if sym.range.end.line > max_end.line
                || (sym.range.end.line == max_end.line
                    && sym.range.end.character > max_end.character)
            {
                max_end = sym.range.end;
            }
        }
        // Ensure the range is valid (end >= start)
        if min_start.line > max_end.line
            || (min_start.line == max_end.line && min_start.character > max_end.character)
        {
            // Fallback to a minimal valid range
            max_end = min_start;
        }
        Range {
            start: min_start,
            end: max_end,
        }
    }

    // Add grouped sections if they have content
    if !parameters.is_empty() {
        let group_range = compute_group_range(&parameters);
        children.push(DocumentSymbol {
            name: "Parameters".to_string(),
            detail: Some(format!("{} items", parameters.len())),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range: group_range,
            selection_range: group_range,
            children: Some(parameters),
        });
    }

    if !inputs.is_empty() {
        let group_range = compute_group_range(&inputs);
        children.push(DocumentSymbol {
            name: "Inputs".to_string(),
            detail: Some(format!("{} items", inputs.len())),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range: group_range,
            selection_range: group_range,
            children: Some(inputs),
        });
    }

    if !outputs.is_empty() {
        let group_range = compute_group_range(&outputs);
        children.push(DocumentSymbol {
            name: "Outputs".to_string(),
            detail: Some(format!("{} items", outputs.len())),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range: group_range,
            selection_range: group_range,
            children: Some(outputs),
        });
    }

    if !variables.is_empty() {
        let group_range = compute_group_range(&variables);
        children.push(DocumentSymbol {
            name: "Variables".to_string(),
            detail: Some(format!("{} items", variables.len())),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range: group_range,
            selection_range: group_range,
            children: Some(variables),
        });
    }

    // Add nested classes (functions, records, etc.)
    for (nested_name, nested_class) in &class.classes {
        if let Some(nested_symbol) = build_class_symbol(nested_name, nested_class) {
            children.push(nested_symbol);
        }
    }

    // Helper to compute range from equations
    // Returns a valid range that is guaranteed to have start <= end
    fn compute_equations_range(equations: &[crate::ir::ast::Equation]) -> Option<Range> {
        let mut min_line = u32::MAX;
        let mut max_line = 0u32;
        let mut min_col = u32::MAX;
        let mut max_col = 0u32;

        for eq in equations {
            if let Some(loc) = eq.get_location() {
                let line = loc.start_line.saturating_sub(1);
                let col = loc.start_column.saturating_sub(1);
                if line < min_line || (line == min_line && col < min_col) {
                    min_line = line;
                    min_col = col;
                }
                if line > max_line || (line == max_line && col + 20 > max_col) {
                    max_line = line;
                    max_col = col + 20; // Approximate end
                }
            }
        }

        if min_line == u32::MAX {
            None
        } else {
            // Ensure the range is valid (end >= start)
            if max_line < min_line || (max_line == min_line && max_col < min_col) {
                max_line = min_line;
                max_col = min_col;
            }
            Some(Range {
                start: Position {
                    line: min_line,
                    character: min_col,
                },
                end: Position {
                    line: max_line,
                    character: max_col,
                },
            })
        }
    }

    // Count equations
    let equation_count = class.equations.len() + class.initial_equations.len();
    if equation_count > 0 {
        // Combine all equations to find the range
        let all_equations: Vec<_> = class
            .equations
            .iter()
            .chain(class.initial_equations.iter())
            .cloned()
            .collect();
        let eq_range = compute_equations_range(&all_equations).unwrap_or(range);
        children.push(DocumentSymbol {
            name: "Equations".to_string(),
            detail: Some(format!("{} equations", equation_count)),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range: eq_range,
            selection_range: eq_range,
            children: None,
        });
    }

    // Count algorithms
    let algorithm_count = class.algorithms.len() + class.initial_algorithms.len();
    if algorithm_count > 0 {
        // For algorithms, use the parent range as fallback since we don't have location info readily available
        // The parent range is valid since selection_range == range for this symbol
        children.push(DocumentSymbol {
            name: "Algorithms".to_string(),
            detail: Some(format!("{} algorithm sections", algorithm_count)),
            kind: SymbolKind::NAMESPACE,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
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
