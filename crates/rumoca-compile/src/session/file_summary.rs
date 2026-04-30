use super::FileId;
use super::declaration_index::{ItemKey, ItemKind};
use indexmap::IndexMap;
use rumoca_ir_ast as ast;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub(crate) struct FileSummary {
    pub(crate) within_path: Option<String>,
    pub(crate) class_keys_by_name: IndexMap<String, ItemKey>,
    pub(crate) classes: IndexMap<ItemKey, ClassSummary>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct PersistedFileSummaryEntry {
    qualified_name: String,
    class: ClassSummary,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct PersistedFileSummary {
    within_path: Option<String>,
    classes: Vec<PersistedFileSummaryEntry>,
}

impl FileSummary {
    pub(crate) fn from_definition(file_id: FileId, definition: &ast::StoredDefinition) -> Self {
        let mut summary = Self {
            within_path: definition
                .within
                .as_ref()
                .map(ToString::to_string)
                .filter(|path| !path.is_empty()),
            ..Self::default()
        };
        let container_path = summary.within_path.clone().unwrap_or_default();
        for (name, class) in &definition.classes {
            collect_class_summaries(file_id, &container_path, name, class, &mut summary);
        }
        summary
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&ItemKey, &ClassSummary)> {
        self.classes.iter()
    }

    #[cfg(test)]
    pub(crate) fn within_path(&self) -> Option<&str> {
        self.within_path.as_deref()
    }

    #[cfg(test)]
    pub(crate) fn class(&self, qualified_name: &str) -> Option<&ClassSummary> {
        let item_key = self.class_keys_by_name.get(qualified_name)?;
        self.classes.get(item_key)
    }

    #[cfg(test)]
    pub(crate) fn item_key_for_name(&self, qualified_name: &str) -> Option<&ItemKey> {
        self.class_keys_by_name.get(qualified_name)
    }

    pub(crate) fn to_persisted(&self) -> PersistedFileSummary {
        PersistedFileSummary {
            within_path: self.within_path.clone(),
            classes: self
                .classes
                .iter()
                .map(|(item_key, class)| PersistedFileSummaryEntry {
                    qualified_name: item_key.qualified_name(),
                    class: class.clone(),
                })
                .collect(),
        }
    }

    pub(crate) fn from_persisted(file_id: FileId, persisted: &PersistedFileSummary) -> Self {
        let mut summary = Self {
            within_path: persisted.within_path.clone(),
            ..Self::default()
        };
        for entry in &persisted.classes {
            let (container_path, name) = split_qualified_name(&entry.qualified_name);
            let item_key = ItemKey::new(file_id, ItemKind::Class, container_path, name);
            summary
                .class_keys_by_name
                .insert(entry.qualified_name.clone(), item_key.clone());
            summary.classes.insert(item_key, entry.class.clone());
        }
        summary
    }
}

pub(crate) fn summary_fingerprint(definition: &ast::StoredDefinition) -> super::Fingerprint {
    fingerprint_value(&FileSummary::from_definition(FileId::default(), definition))
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct ClassSummary {
    pub(crate) name_location: ast::Location,
    pub(crate) class_type: ast::ClassType,
    pub(crate) encapsulated: bool,
    pub(crate) partial: bool,
    pub(crate) expandable: bool,
    pub(crate) operator_record: bool,
    pub(crate) pure: bool,
    pub(crate) causality: ast::Causality,
    pub(crate) is_protected: bool,
    pub(crate) is_final: bool,
    pub(crate) is_replaceable: bool,
    pub(crate) constrainedby: Option<String>,
    pub(crate) array_subscripts: Vec<ast::Subscript>,
    pub(crate) imports: Vec<ImportSummary>,
    pub(crate) extends: Vec<ExtendSummary>,
    pub(crate) components: IndexMap<String, ComponentSummary>,
    pub(crate) nested_classes: Vec<String>,
    pub(crate) nested_class_headers: IndexMap<String, NestedClassSummary>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) enum ImportSummary {
    Qualified {
        path: String,
        global_scope: bool,
    },
    Renamed {
        alias: String,
        path: String,
        global_scope: bool,
    },
    Unqualified {
        path: String,
        global_scope: bool,
    },
    Selective {
        path: String,
        names: Vec<String>,
        global_scope: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct ExtendSummary {
    pub(crate) base_name: String,
    pub(crate) break_names: Vec<String>,
    pub(crate) is_protected: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct ComponentSummary {
    pub(crate) name_location: ast::Location,
    pub(crate) type_name: String,
    pub(crate) variability: ast::Variability,
    pub(crate) causality: ast::Causality,
    pub(crate) connection: ast::Connection,
    pub(crate) is_protected: bool,
    pub(crate) is_final: bool,
    pub(crate) is_replaceable: bool,
    pub(crate) constrainedby: Option<String>,
    pub(crate) shape: Vec<usize>,
    pub(crate) shape_expr: Vec<ast::Subscript>,
    pub(crate) condition: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct NestedClassSummary {
    pub(crate) class_type: ast::ClassType,
    pub(crate) partial: bool,
    pub(crate) is_replaceable: bool,
}

fn collect_class_summaries(
    file_id: FileId,
    container_path: &str,
    name: &str,
    class: &ast::ClassDef,
    summary: &mut FileSummary,
) {
    let item_key = ItemKey::new(file_id, ItemKind::Class, container_path, name);
    let qualified_name = item_key.qualified_name();
    summary
        .class_keys_by_name
        .insert(qualified_name.clone(), item_key.clone());
    summary
        .classes
        .insert(item_key, ClassSummary::from_class(class));

    for (nested_name, nested_class) in &class.classes {
        collect_class_summaries(file_id, &qualified_name, nested_name, nested_class, summary);
    }
}

impl ClassSummary {
    fn from_class(class: &ast::ClassDef) -> Self {
        Self {
            name_location: class.name.location.clone(),
            class_type: class.class_type.clone(),
            encapsulated: class.encapsulated,
            partial: class.partial,
            expandable: class.expandable,
            operator_record: class.operator_record,
            pure: class.pure,
            causality: class.causality.clone(),
            is_protected: class.is_protected,
            is_final: class.is_final,
            is_replaceable: class.is_replaceable,
            constrainedby: class.constrainedby.as_ref().map(ToString::to_string),
            array_subscripts: class.array_subscripts.clone(),
            imports: class
                .imports
                .iter()
                .map(ImportSummary::from_import)
                .collect(),
            extends: class
                .extends
                .iter()
                .map(ExtendSummary::from_extend)
                .collect(),
            components: class
                .components
                .iter()
                .map(|(name, component)| {
                    (name.clone(), ComponentSummary::from_component(component))
                })
                .collect(),
            nested_classes: class.classes.keys().cloned().collect(),
            nested_class_headers: class
                .classes
                .iter()
                .map(|(name, nested)| (name.clone(), NestedClassSummary::from_class(nested)))
                .collect(),
        }
    }
}

impl ImportSummary {
    fn from_import(import: &ast::Import) -> Self {
        match import {
            ast::Import::Qualified {
                path, global_scope, ..
            } => Self::Qualified {
                path: path.to_string(),
                global_scope: *global_scope,
            },
            ast::Import::Renamed {
                alias,
                path,
                global_scope,
                ..
            } => Self::Renamed {
                alias: alias.text.to_string(),
                path: path.to_string(),
                global_scope: *global_scope,
            },
            ast::Import::Unqualified {
                path, global_scope, ..
            } => Self::Unqualified {
                path: path.to_string(),
                global_scope: *global_scope,
            },
            ast::Import::Selective {
                path,
                names,
                global_scope,
                ..
            } => Self::Selective {
                path: path.to_string(),
                names: names.iter().map(|name| name.text.to_string()).collect(),
                global_scope: *global_scope,
            },
        }
    }
}

impl ExtendSummary {
    fn from_extend(extend: &ast::Extend) -> Self {
        Self {
            base_name: extend.base_name.to_string(),
            break_names: extend.break_names.clone(),
            is_protected: extend.is_protected,
        }
    }
}

impl ComponentSummary {
    fn from_component(component: &ast::Component) -> Self {
        Self {
            name_location: component.name_token.location.clone(),
            type_name: component.type_name.to_string(),
            variability: component.variability.clone(),
            causality: component.causality.clone(),
            connection: component.connection.clone(),
            is_protected: component.is_protected,
            is_final: component.is_final,
            is_replaceable: component.is_replaceable,
            constrainedby: component.constrainedby.as_ref().map(ToString::to_string),
            shape: component.shape.clone(),
            shape_expr: component.shape_expr.clone(),
            condition: component.condition.as_ref().map(ToString::to_string),
        }
    }
}

fn split_qualified_name(qualified_name: &str) -> (String, String) {
    match qualified_name.rsplit_once('.') {
        Some((container_path, name)) => (container_path.to_string(), name.to_string()),
        None => (String::new(), qualified_name.to_string()),
    }
}

impl NestedClassSummary {
    fn from_class(class: &ast::ClassDef) -> Self {
        Self {
            class_type: class.class_type.clone(),
            partial: class.partial,
            is_replaceable: class.is_replaceable,
        }
    }
}

fn fingerprint_value<T: Serialize>(value: &T) -> super::Fingerprint {
    let encoded =
        bincode::serialize(value).expect("query fingerprint serialization should succeed");
    *blake3::hash(&encoded).as_bytes()
}
