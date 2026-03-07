//! Document symbols handler for Modelica files (file outline).
//!
//! Ported from the main branch's `src/lsp/handlers/document_symbols.rs`.

use lsp_types::{DocumentSymbol, DocumentSymbolResponse, Position, Range, SymbolKind};
use rumoca_session::parsing::ast;
use rumoca_session::parsing::ir_core as rumoca_ir_core;

use crate::helpers::{location_to_range, token_to_range};

/// Handle document symbols request - provides file outline.
///
/// Takes a parsed AST from `rumoca-session` and returns document symbols.
pub fn handle_document_symbols(ast: &ast::StoredDefinition) -> Option<DocumentSymbolResponse> {
    let mut symbols = Vec::new();

    for (class_name, class_def) in &ast.classes {
        if let Some(symbol) = build_class_symbol(class_name, class_def) {
            symbols.push(symbol);
        }
    }

    Some(DocumentSymbolResponse::Nested(symbols))
}

/// Build a DocumentSymbol for a class definition with its children.
fn build_class_symbol(name: &str, class: &ast::ClassDef) -> Option<DocumentSymbol> {
    let kind = class_type_to_symbol_kind(&class.class_type);
    let range = location_to_range(&class.location);
    let selection_range = token_to_range(&class.name);

    let mut children = Vec::new();

    // Group components by category
    let mut parameters = Vec::new();
    let mut variables = Vec::new();
    let mut inputs = Vec::new();
    let mut outputs = Vec::new();

    for (comp_name, comp) in &class.components {
        let (comp_kind, category) = match (&comp.variability, &comp.causality) {
            (rumoca_ir_core::Variability::Parameter(_), _) => {
                (SymbolKind::CONSTANT, &mut parameters)
            }
            (rumoca_ir_core::Variability::Constant(_), _) => {
                (SymbolKind::CONSTANT, &mut parameters)
            }
            (_, rumoca_ir_core::Causality::Input(_)) => (SymbolKind::PROPERTY, &mut inputs),
            (_, rumoca_ir_core::Causality::Output(_)) => (SymbolKind::PROPERTY, &mut outputs),
            _ => (SymbolKind::VARIABLE, &mut variables),
        };

        let comp_range = location_to_range(&comp.location);
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

        category.push(new_document_symbol(
            comp_name.clone(),
            Some(detail),
            comp_kind,
            comp_range,
            comp_selection_range,
            None,
        ));
    }

    // Add grouped sections
    add_symbol_group(&mut children, "Parameters", &parameters);
    add_symbol_group(&mut children, "Inputs", &inputs);
    add_symbol_group(&mut children, "Outputs", &outputs);
    add_symbol_group(&mut children, "Variables", &variables);

    // Add nested classes
    for (nested_name, nested_class) in &class.classes {
        if let Some(nested_symbol) = build_class_symbol(nested_name, nested_class) {
            children.push(nested_symbol);
        }
    }

    // Add equation/algorithm counts
    add_section_counts(&mut children, class, range);

    Some(new_document_symbol(
        name.to_string(),
        Some(format!("{:?}", class.class_type)),
        kind,
        range,
        selection_range,
        if children.is_empty() {
            None
        } else {
            Some(children)
        },
    ))
}

fn class_type_to_symbol_kind(ct: &ast::ClassType) -> SymbolKind {
    match ct {
        ast::ClassType::Model | ast::ClassType::Block | ast::ClassType::Class => SymbolKind::CLASS,
        ast::ClassType::Connector => SymbolKind::INTERFACE,
        ast::ClassType::Record => SymbolKind::STRUCT,
        ast::ClassType::Type => SymbolKind::TYPE_PARAMETER,
        ast::ClassType::Package => SymbolKind::NAMESPACE,
        ast::ClassType::Function => SymbolKind::FUNCTION,
        ast::ClassType::Operator => SymbolKind::OPERATOR,
    }
}

fn add_symbol_group(children: &mut Vec<DocumentSymbol>, label: &str, items: &[DocumentSymbol]) {
    if items.is_empty() {
        return;
    }
    let group_range = compute_group_range(items);
    children.push(new_document_symbol(
        label.to_string(),
        Some(format!("{} items", items.len())),
        SymbolKind::NAMESPACE,
        group_range,
        group_range,
        Some(items.to_vec()),
    ));
}

fn add_section_counts(children: &mut Vec<DocumentSymbol>, class: &ast::ClassDef, range: Range) {
    let equation_count = class.equations.len() + class.initial_equations.len();
    if equation_count > 0 {
        let eq_range = compute_equations_range(class).unwrap_or(range);
        children.push(new_document_symbol(
            "Equations".to_string(),
            Some(format!("{} equations", equation_count)),
            SymbolKind::NAMESPACE,
            eq_range,
            eq_range,
            None,
        ));
    }

    let algorithm_count = class.algorithms.len() + class.initial_algorithms.len();
    if algorithm_count > 0 {
        children.push(new_document_symbol(
            "Algorithms".to_string(),
            Some(format!("{} algorithm sections", algorithm_count)),
            SymbolKind::NAMESPACE,
            range,
            range,
            None,
        ));
    }
}

#[expect(
    deprecated,
    reason = "lsp-types still requires deprecated field; remove once lsp-types drops it"
)]
fn new_document_symbol(
    name: String,
    detail: Option<String>,
    kind: SymbolKind,
    range: Range,
    selection_range: Range,
    children: Option<Vec<DocumentSymbol>>,
) -> DocumentSymbol {
    DocumentSymbol {
        name,
        detail,
        kind,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children,
    }
}

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
            || (sym.range.end.line == max_end.line && sym.range.end.character > max_end.character)
        {
            max_end = sym.range.end;
        }
    }
    if min_start.line > max_end.line
        || (min_start.line == max_end.line && min_start.character > max_end.character)
    {
        max_end = min_start;
    }
    Range {
        start: min_start,
        end: max_end,
    }
}

fn compute_equations_range(class: &ast::ClassDef) -> Option<Range> {
    let mut min_line = u32::MAX;
    let mut max_line = 0u32;
    let mut min_col = u32::MAX;
    let mut max_col = 0u32;

    for eq in class.equations.iter().chain(class.initial_equations.iter()) {
        if let Some(loc) = eq.get_location() {
            let line = loc.start_line.saturating_sub(1);
            let col = loc.start_column.saturating_sub(1);
            if line < min_line || (line == min_line && col < min_col) {
                min_line = line;
                min_col = col;
            }
            if line > max_line || (line == max_line && col + 20 > max_col) {
                max_line = line;
                max_col = col + 20;
            }
        }
    }

    if min_line == u32::MAX {
        None
    } else {
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
