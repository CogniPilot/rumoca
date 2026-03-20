use super::class_body::FileClassBodyIndex;
use super::declaration_index::{ItemKey, ItemKind};
use super::*;

#[derive(Debug, Clone, Default)]
pub(crate) struct FileOutline {
    symbols: Vec<DocumentSymbol>,
}

impl FileOutline {
    pub(crate) fn document_symbols(&self) -> &[DocumentSymbol] {
        &self.symbols
    }

    pub(crate) fn from_definition(
        file_id: FileId,
        definition: &ast::StoredDefinition,
        class_bodies: &FileClassBodyIndex,
    ) -> Self {
        let mut symbols = Vec::new();
        let within_prefix = definition
            .within
            .as_ref()
            .map(ToString::to_string)
            .filter(|path| !path.is_empty())
            .unwrap_or_default();
        for (name, class) in &definition.classes {
            if let Some(symbol) = collect_document_symbols_for_class(
                file_id,
                &within_prefix,
                name,
                class,
                class_bodies,
            ) {
                symbols.push(symbol);
            }
        }
        Self { symbols }
    }
}

fn collect_document_symbols_for_class(
    file_id: FileId,
    container_path: &str,
    name: &str,
    class: &ast::ClassDef,
    class_bodies: &FileClassBodyIndex,
) -> Option<DocumentSymbol> {
    let mut parameters = Vec::new();
    let mut variables = Vec::new();
    let mut inputs = Vec::new();
    let mut outputs = Vec::new();
    let mut nested_children = Vec::new();
    let item_key = ItemKey::new(file_id, ItemKind::Class, container_path, name);
    let qualified_name = item_key.qualified_name();
    let class_body = class_bodies.class_body(&item_key);

    for (comp_name, comp) in &class.components {
        let section = match (&comp.variability, &comp.causality) {
            (rumoca_ir_core::Variability::Parameter(_), _) => DocumentSymbolKind::ParametersSection,
            (rumoca_ir_core::Variability::Constant(_), _) => DocumentSymbolKind::ParametersSection,
            (_, rumoca_ir_core::Causality::Input(_)) => DocumentSymbolKind::InputsSection,
            (_, rumoca_ir_core::Causality::Output(_)) => DocumentSymbolKind::OutputsSection,
            _ => DocumentSymbolKind::VariablesSection,
        };

        let mut detail = comp.type_name.to_string();
        if !comp.shape.is_empty() {
            detail += &format!(
                "[{}]",
                comp.shape
                    .iter()
                    .map(|dim| dim.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

        let component = DocumentSymbol {
            name: comp_name.clone(),
            detail: Some(detail),
            kind: DocumentSymbolKind::Component,
            range: comp.location.clone(),
            selection_range: comp.name_token.location.clone(),
            children: Vec::new(),
        };
        match section {
            DocumentSymbolKind::ParametersSection => parameters.push(component),
            DocumentSymbolKind::InputsSection => inputs.push(component),
            DocumentSymbolKind::OutputsSection => outputs.push(component),
            DocumentSymbolKind::VariablesSection => variables.push(component),
            _ => {}
        }
    }

    add_document_symbol_group(&mut nested_children, "Parameters", &mut parameters);
    add_document_symbol_group(&mut nested_children, "Inputs", &mut inputs);
    add_document_symbol_group(&mut nested_children, "Outputs", &mut outputs);
    add_document_symbol_group(&mut nested_children, "Variables", &mut variables);

    for (nested_name, nested_class) in &class.classes {
        if let Some(nested_symbol) = collect_document_symbols_for_class(
            file_id,
            &qualified_name,
            nested_name,
            nested_class,
            class_bodies,
        ) {
            nested_children.push(nested_symbol);
        }
    }

    if let Some(section) = class_body.and_then(|body| body.equation_section()) {
        nested_children.push(DocumentSymbol {
            name: "Equations".to_string(),
            detail: Some(format!("{} equations", section.count())),
            kind: DocumentSymbolKind::EquationsSection,
            range: section
                .range()
                .cloned()
                .unwrap_or_else(|| class.location.clone()),
            selection_range: class.location.clone(),
            children: Vec::new(),
        });
    }

    if let Some(section) = class_body.and_then(|body| body.algorithm_section()) {
        nested_children.push(DocumentSymbol {
            name: "Algorithms".to_string(),
            detail: Some(format!("{} algorithm sections", section.count())),
            kind: DocumentSymbolKind::AlgorithmsSection,
            range: section
                .range()
                .cloned()
                .unwrap_or_else(|| class.location.clone()),
            selection_range: class.location.clone(),
            children: Vec::new(),
        });
    }

    Some(DocumentSymbol {
        name: name.to_string(),
        detail: Some(format!("{:?}", class.class_type)),
        kind: DocumentSymbolKind::Class(class.class_type.clone()),
        range: class.location.clone(),
        selection_range: class.name.location.clone(),
        children: nested_children,
    })
}

fn add_document_symbol_group(
    children: &mut Vec<DocumentSymbol>,
    name: &str,
    section_symbols: &mut Vec<DocumentSymbol>,
) {
    if section_symbols.is_empty() {
        return;
    }

    let range = document_symbol_group_range(section_symbols);
    let kind = match name {
        "Parameters" => DocumentSymbolKind::ParametersSection,
        "Inputs" => DocumentSymbolKind::InputsSection,
        "Outputs" => DocumentSymbolKind::OutputsSection,
        _ => DocumentSymbolKind::VariablesSection,
    };

    children.push(DocumentSymbol {
        name: name.to_string(),
        detail: Some(format!("{} items", section_symbols.len())),
        kind,
        range: range.clone(),
        selection_range: range,
        children: mem::take(section_symbols),
    });
}

fn document_symbol_group_range(symbols: &[DocumentSymbol]) -> ast::Location {
    let mut min_start = u32::MAX;
    let mut max_end = 0u32;
    let mut min_column = u32::MAX;
    let mut max_column = 0u32;

    for symbol in symbols {
        if symbol.range.start_line < min_start
            || (symbol.range.start_line == min_start && symbol.range.start_column < min_column)
        {
            min_start = symbol.range.start_line;
            min_column = symbol.range.start_column;
        }
        if symbol.range.end_line > max_end
            || (symbol.range.end_line == max_end && symbol.range.end_column > max_column)
        {
            max_end = symbol.range.end_line;
            max_column = symbol.range.end_column;
        }
    }

    if min_start == u32::MAX {
        return ast::Location {
            start_line: 1,
            start_column: 1,
            end_line: 1,
            end_column: 1,
            start: 0,
            end: 0,
            file_name: String::new(),
        };
    }

    ast::Location {
        start_line: min_start,
        start_column: min_column,
        end_line: max_end,
        end_column: max_column,
        start: 0,
        end: 0,
        file_name: symbols
            .first()
            .map(|symbol| symbol.range.file_name.clone())
            .unwrap_or_default(),
    }
}
