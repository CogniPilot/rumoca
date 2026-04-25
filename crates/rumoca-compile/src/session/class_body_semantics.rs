use super::class_body::{FileClassBodyIndex, ModifierClassTarget};
use super::declaration_index::{ItemKey, ItemKind};
use super::file_summary::FileSummary;
use indexmap::IndexMap;
use rumoca_ir_ast as ast;

#[derive(Debug, Clone, Default)]
pub(crate) struct FileClassBodySemantics {
    component_occurrences: IndexMap<ItemKey, Vec<ComponentOccurrence>>,
    modifier_class_targets: IndexMap<ItemKey, Vec<ModifierClassTarget>>,
    lookup_entries: Vec<ComponentLookupEntry>,
}

#[derive(Debug, Clone)]
struct ComponentOccurrence {
    location: ast::Location,
    is_declaration: bool,
}

#[derive(Debug, Clone)]
struct ComponentLookupEntry {
    item_key: ItemKey,
    location: ast::Location,
}

impl FileClassBodySemantics {
    pub(crate) fn from_parts(summary: &FileSummary, class_bodies: &FileClassBodyIndex) -> Self {
        let mut semantics = Self::default();
        for (item_key, class) in summary.iter() {
            collect_class_body_semantics(item_key, class, class_bodies, &mut semantics);
        }
        semantics
    }

    pub(crate) fn references_at(
        &self,
        line: u32,
        character: u32,
        include_declaration: bool,
    ) -> Option<Vec<ast::Location>> {
        let item_key = self.lookup_component_target(line, character)?;
        let locations = self
            .component_occurrences
            .get(item_key)?
            .iter()
            .filter(|occurrence| include_declaration || !occurrence.is_declaration)
            .map(|occurrence| occurrence.location.clone())
            .collect::<Vec<_>>();
        (!locations.is_empty()).then_some(locations)
    }

    pub(crate) fn rename_span_at(&self, line: u32, character: u32) -> Option<ast::Location> {
        self.lookup_entry(line, character)
            .map(|entry| entry.location.clone())
    }

    pub(crate) fn rename_locations_at(
        &self,
        line: u32,
        character: u32,
    ) -> Option<Vec<ast::Location>> {
        let item_key = self.lookup_component_target(line, character)?;
        let locations = self
            .component_occurrences
            .get(item_key)?
            .iter()
            .map(|occurrence| occurrence.location.clone())
            .collect::<Vec<_>>();
        (!locations.is_empty()).then_some(locations)
    }

    pub(crate) fn modifier_class_targets(
        &self,
        class_item_key: &ItemKey,
    ) -> &[ModifierClassTarget] {
        self.modifier_class_targets
            .get(class_item_key)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    pub(crate) fn component_target_at(&self, line: u32, character: u32) -> Option<&ItemKey> {
        self.lookup_component_target(line, character)
    }

    fn lookup_component_target(&self, line: u32, character: u32) -> Option<&ItemKey> {
        self.lookup_entry(line, character)
            .map(|entry| &entry.item_key)
    }

    fn lookup_entry(&self, line: u32, character: u32) -> Option<&ComponentLookupEntry> {
        self.lookup_entries
            .iter()
            .find(|entry| location_contains_position(&entry.location, line, character))
    }

    fn record_component_occurrence(
        &mut self,
        item_key: ItemKey,
        location: ast::Location,
        is_declaration: bool,
    ) {
        self.component_occurrences
            .entry(item_key.clone())
            .or_default()
            .push(ComponentOccurrence {
                location: location.clone(),
                is_declaration,
            });
        self.lookup_entries
            .push(ComponentLookupEntry { item_key, location });
    }
}

fn collect_class_body_semantics(
    class_item_key: &ItemKey,
    class: &super::file_summary::ClassSummary,
    class_bodies: &FileClassBodyIndex,
    semantics: &mut FileClassBodySemantics,
) {
    let local_components = class
        .components
        .iter()
        .map(|(component_name, component)| {
            let item_key = ItemKey::new(
                class_item_key.file_id(),
                ItemKind::Component,
                class_item_key.qualified_name(),
                component_name,
            );
            semantics.record_component_occurrence(
                item_key.clone(),
                component.name_location.clone(),
                true,
            );
            (component_name.clone(), item_key)
        })
        .collect::<IndexMap<_, _>>();

    let Some(class_body) = class_bodies.class_body(class_item_key) else {
        return;
    };
    for (component_name, item_key) in local_components {
        for location in class_body.component_occurrences(&component_name) {
            semantics.record_component_occurrence(item_key.clone(), location.clone(), false);
        }
    }

    if !class_body.modifier_class_targets().is_empty() {
        semantics.modifier_class_targets.insert(
            class_item_key.clone(),
            class_body.modifier_class_targets().to_vec(),
        );
    }
}

fn location_contains_position(location: &ast::Location, line: u32, character: u32) -> bool {
    let start_line = location.start_line.saturating_sub(1);
    let end_line = location.end_line.saturating_sub(1);
    if line < start_line || line > end_line {
        return false;
    }

    let start_character = if line == start_line {
        location.start_column.saturating_sub(1)
    } else {
        0
    };
    let end_character = if line == end_line {
        location.end_column
    } else {
        u32::MAX
    };

    character >= start_character && character <= end_character
}
