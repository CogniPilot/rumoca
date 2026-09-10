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
            (rumoca_core::Variability::Parameter(_), _) => DocumentSymbolKind::ParametersSection,
            (rumoca_core::Variability::Constant(_), _) => DocumentSymbolKind::ParametersSection,
            (_, rumoca_core::Causality::Input(_)) => DocumentSymbolKind::InputsSection,
            (_, rumoca_core::Causality::Output(_)) => DocumentSymbolKind::OutputsSection,
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

/// Byte span accumulated across a section's child symbols.
///
/// Grouped outline nodes ("Parameters", "Inputs", ...) are synthesized rather
/// than parsed, so they have no token of their own. Tracking the children's
/// byte offsets keeps `start`/`end` usable by the IDE layer, which converts
/// byte spans to UTF-16 LSP ranges (children whose span is `0..0` carry no
/// provenance and are skipped).
#[derive(Debug, Clone, Copy)]
struct GroupByteSpan {
    start: u32,
    end: u32,
}

impl GroupByteSpan {
    const fn empty() -> Self {
        Self {
            start: u32::MAX,
            end: 0,
        }
    }

    fn absorb(&mut self, location: &rumoca_core::Location) {
        if location.end <= location.start {
            return;
        }
        self.start = self.start.min(location.start);
        self.end = self.end.max(location.end);
    }

    /// `(start, end)` for a `Location`, or `(0, 0)` when nothing contributed.
    const fn resolved(self) -> (u32, u32) {
        if self.start == u32::MAX || self.end <= self.start {
            return (0, 0);
        }
        (self.start, self.end)
    }
}

fn document_symbol_group_range(symbols: &[DocumentSymbol]) -> rumoca_core::Location {
    let mut min_start = u32::MAX;
    let mut max_end = 0u32;
    let mut min_column = u32::MAX;
    let mut max_column = 0u32;
    let mut byte_span = GroupByteSpan::empty();

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
        byte_span.absorb(&symbol.range);
    }

    let (start, end) = byte_span.resolved();

    if min_start == u32::MAX {
        return rumoca_core::Location {
            start_line: 1,
            start_column: 1,
            end_line: 1,
            end_column: 1,
            start,
            end,
            ..rumoca_core::Location::default()
        };
    }

    // The group inherits its children's file identity; they all come from one
    // class body, so the first child is representative. With no children there
    // is no provenance to inherit, which is what `SourceId::DUMMY` records.
    let source = symbols
        .first()
        .map(|symbol| symbol.range.source)
        .unwrap_or(rumoca_core::SourceId::DUMMY);

    rumoca_core::Location {
        start_line: min_start,
        start_column: min_column,
        end_line: max_end,
        end_column: max_column,
        start,
        end,
        source,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn component_symbol(name: &str, start: u32, end: u32) -> DocumentSymbol {
        DocumentSymbol {
            name: name.to_string(),
            detail: None,
            kind: DocumentSymbolKind::Component,
            range: rumoca_core::Location {
                start_line: 2,
                start_column: 3,
                end_line: 2,
                end_column: 10,
                start,
                end,
                ..rumoca_core::Location::default()
            },
            selection_range: rumoca_core::Location::default(),
            children: Vec::new(),
        }
    }

    #[test]
    fn group_range_spans_child_byte_offsets() {
        // Grouped outline nodes ("Parameters", ...) are synthesized and own no
        // token, so their byte span has to come from their children; without it
        // the IDE layer cannot convert them to UTF-16 ranges from the span.
        let symbols = [component_symbol("k", 20, 30), component_symbol("j", 40, 55)];
        let range = document_symbol_group_range(&symbols);
        assert_eq!(range.start, 20);
        assert_eq!(range.end, 55);
    }

    #[test]
    fn group_range_ignores_children_without_provenance() {
        let mut without_span = component_symbol("synthetic", 0, 0);
        without_span.range.start_line = 1;
        let symbols = [without_span, component_symbol("k", 20, 30)];
        let range = document_symbol_group_range(&symbols);
        assert_eq!(range.start, 20, "a 0..0 child must not drag the span to 0");
        assert_eq!(range.end, 30);
    }

    #[test]
    fn group_range_without_any_provenance_stays_zero() {
        let symbols = [component_symbol("synthetic", 0, 0)];
        let range = document_symbol_group_range(&symbols);
        assert_eq!((range.start, range.end), (0, 0));
    }
}
