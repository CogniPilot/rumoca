use super::file_summary::FileSummary;
use super::{FileId, WorkspaceSymbol, WorkspaceSymbolKind};
use indexmap::IndexMap;
use rumoca_ir_ast as ast;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub(crate) enum ItemKind {
    Class,
    Component,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub(crate) struct ItemKey {
    file_id: FileId,
    kind: ItemKind,
    container_path: String,
    name: String,
}

impl ItemKey {
    pub(crate) fn new(
        file_id: FileId,
        kind: ItemKind,
        container_path: impl Into<String>,
        name: impl Into<String>,
    ) -> Self {
        Self {
            file_id,
            kind,
            container_path: container_path.into(),
            name: name.into(),
        }
    }

    pub(crate) fn qualified_name(&self) -> String {
        join_path(&self.container_path, &self.name)
    }

    pub(crate) fn file_id(&self) -> FileId {
        self.file_id
    }

    pub(crate) fn container_path(&self) -> &str {
        &self.container_path
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, Clone)]
pub(crate) struct DeclarationIndexEntry {
    pub(crate) symbol_kind: WorkspaceSymbolKind,
    pub(crate) container_name: Option<String>,
    pub(crate) location: ast::Location,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct DeclarationIndex {
    items: IndexMap<ItemKey, DeclarationIndexEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) enum PersistedWorkspaceSymbolKind {
    Class(ast::ClassType),
    Component,
}

impl PersistedWorkspaceSymbolKind {
    fn from_symbol_kind(kind: &WorkspaceSymbolKind) -> Self {
        match kind {
            WorkspaceSymbolKind::Class(class_type) => Self::Class(class_type.clone()),
            WorkspaceSymbolKind::Component => Self::Component,
        }
    }

    fn into_symbol_kind(self) -> WorkspaceSymbolKind {
        match self {
            Self::Class(class_type) => WorkspaceSymbolKind::Class(class_type),
            Self::Component => WorkspaceSymbolKind::Component,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct PersistedDeclarationIndexEntry {
    kind: ItemKind,
    container_path: String,
    name: String,
    symbol_kind: PersistedWorkspaceSymbolKind,
    container_name: Option<String>,
    location: ast::Location,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub(crate) struct PersistedDeclarationIndex {
    entries: Vec<PersistedDeclarationIndexEntry>,
}

impl DeclarationIndex {
    pub(crate) fn from_definition(file_id: FileId, definition: &ast::StoredDefinition) -> Self {
        let mut index = Self::default();
        let within_prefix = definition
            .within
            .as_ref()
            .map(ToString::to_string)
            .filter(|path| !path.is_empty())
            .unwrap_or_default();
        for (name, class) in &definition.classes {
            collect_class_declarations(file_id, &within_prefix, None, name, class, &mut index);
        }
        index
    }

    pub(crate) fn from_summary(summary: &FileSummary) -> Self {
        let mut index = Self::default();
        for (item_key, class) in summary.iter() {
            index.insert(
                item_key.clone(),
                WorkspaceSymbolKind::Class(class.class_type.clone()),
                container_name_for_item_key(item_key),
                class.name_location.clone(),
            );
            for (component_name, component) in &class.components {
                let component_key = ItemKey::new(
                    item_key.file_id,
                    ItemKind::Component,
                    item_key.qualified_name(),
                    component_name,
                );
                index.insert(
                    component_key,
                    WorkspaceSymbolKind::Component,
                    Some(item_key.name.clone()),
                    component.name_location.clone(),
                );
            }
        }
        index
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&ItemKey, &DeclarationIndexEntry)> {
        self.items.iter()
    }

    pub(crate) fn workspace_symbols(&self, uri: &str) -> Vec<WorkspaceSymbol> {
        self.iter()
            .map(|(key, entry)| WorkspaceSymbol {
                name: key.name.clone(),
                kind: entry.symbol_kind.clone(),
                container_name: entry.container_name.clone(),
                location: entry.location.clone(),
                uri: uri.to_string(),
            })
            .collect()
    }

    fn insert(
        &mut self,
        key: ItemKey,
        symbol_kind: WorkspaceSymbolKind,
        container_name: Option<String>,
        location: ast::Location,
    ) {
        self.items.insert(
            key,
            DeclarationIndexEntry {
                symbol_kind,
                container_name,
                location,
            },
        );
    }

    pub(crate) fn to_persisted(&self) -> PersistedDeclarationIndex {
        let entries = self
            .items
            .iter()
            .map(|(key, entry)| PersistedDeclarationIndexEntry {
                kind: key.kind,
                container_path: key.container_path.clone(),
                name: key.name.clone(),
                symbol_kind: PersistedWorkspaceSymbolKind::from_symbol_kind(&entry.symbol_kind),
                container_name: entry.container_name.clone(),
                location: entry.location.clone(),
            })
            .collect();
        PersistedDeclarationIndex { entries }
    }

    pub(crate) fn from_persisted(file_id: FileId, persisted: &PersistedDeclarationIndex) -> Self {
        let mut index = Self::default();
        for entry in &persisted.entries {
            index.insert(
                ItemKey::new(file_id, entry.kind, &entry.container_path, &entry.name),
                entry.symbol_kind.clone().into_symbol_kind(),
                entry.container_name.clone(),
                entry.location.clone(),
            );
        }
        index
    }
}

fn collect_class_declarations(
    file_id: FileId,
    container_path: &str,
    container_name: Option<&str>,
    name: &str,
    class: &ast::ClassDef,
    index: &mut DeclarationIndex,
) {
    let key = ItemKey::new(file_id, ItemKind::Class, container_path, name);
    let class_path = key.qualified_name();
    index.insert(
        key,
        WorkspaceSymbolKind::Class(class.class_type.clone()),
        container_name.map(ToString::to_string),
        class.location.clone(),
    );

    for (component_name, component) in &class.components {
        let component_key = ItemKey::new(file_id, ItemKind::Component, &class_path, component_name);
        index.insert(
            component_key,
            WorkspaceSymbolKind::Component,
            Some(name.to_string()),
            component.location.clone(),
        );
    }

    for (nested_name, nested_class) in &class.classes {
        collect_class_declarations(
            file_id,
            &class_path,
            Some(name),
            nested_name,
            nested_class,
            index,
        );
    }
}

fn join_path(prefix: &str, name: &str) -> String {
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{prefix}.{name}")
    }
}

fn container_name_for_item_key(item_key: &ItemKey) -> Option<String> {
    (!item_key.container_path.is_empty()).then(|| {
        item_key
            .container_path
            .rsplit('.')
            .next()
            .unwrap_or(item_key.container_path.as_str())
            .to_string()
    })
}
