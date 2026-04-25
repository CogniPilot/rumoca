use super::declaration_index::ItemKey;
use super::file_summary::{FileSummary, ImportSummary};
use super::{ClassLocalCompletionItem, ClassLocalCompletionKind, LocalComponentInfo};
use indexmap::{IndexMap, IndexSet};
use rumoca_ir_ast as ast;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct ImportMap {
    explicit_bindings: Vec<(String, String)>,
    wildcard_paths: Vec<String>,
}

impl ImportMap {
    #[cfg(test)]
    pub(crate) fn explicit_bindings(&self) -> &[(String, String)] {
        &self.explicit_bindings
    }

    #[cfg(test)]
    pub(crate) fn wildcard_paths(&self) -> &[String] {
        &self.wildcard_paths
    }

    pub(crate) fn resolve_candidates(&self, raw_name: &str) -> Vec<String> {
        let mut seen = IndexSet::new();
        let mut candidates = Vec::new();

        for (local_name, qualified_name) in &self.explicit_bindings {
            if local_name == raw_name && seen.insert(qualified_name.clone()) {
                candidates.push(qualified_name.clone());
            }
        }
        for wildcard_path in &self.wildcard_paths {
            let qualified_name = format!("{wildcard_path}.{raw_name}");
            if seen.insert(qualified_name.clone()) {
                candidates.push(qualified_name);
            }
        }

        candidates
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct ComponentInterface {
    name_location: ast::Location,
    type_name: String,
    variability: ast::Variability,
    causality: ast::Causality,
    connection: ast::Connection,
    is_final: bool,
    is_replaceable: bool,
    constrainedby: Option<String>,
    shape: Vec<usize>,
}

impl ComponentInterface {
    pub(crate) fn type_name(&self) -> &str {
        &self.type_name
    }

    #[cfg(test)]
    pub(crate) fn is_replaceable(&self) -> bool {
        self.is_replaceable
    }

    #[cfg(test)]
    pub(crate) fn constrainedby(&self) -> Option<&str> {
        self.constrainedby.as_deref()
    }

    fn completion_kind(&self) -> ClassLocalCompletionKind {
        match (&self.variability, &self.causality) {
            (ast::Variability::Parameter(_), _) | (ast::Variability::Constant(_), _) => {
                ClassLocalCompletionKind::Constant
            }
            (_, ast::Causality::Input(_)) | (_, ast::Causality::Output(_)) => {
                ClassLocalCompletionKind::Property
            }
            _ => ClassLocalCompletionKind::Variable,
        }
    }

    fn hover_keyword(&self) -> Option<&'static str> {
        match self.variability {
            ast::Variability::Parameter(_) => Some("parameter"),
            ast::Variability::Constant(_) => Some("constant"),
            _ => None,
        }
    }

    fn local_component_info(&self, component_name: &str) -> LocalComponentInfo {
        LocalComponentInfo {
            name: component_name.to_string(),
            type_name: self.type_name.clone(),
            keyword_prefix: self.hover_keyword().map(str::to_string),
            shape: self.shape.clone(),
            declaration_location: self.name_location.clone(),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct NestedClassInterface {
    name: String,
    class_type: ast::ClassType,
    is_partial: bool,
    is_replaceable: bool,
}

impl NestedClassInterface {
    #[cfg(test)]
    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    #[cfg(test)]
    pub(crate) fn is_partial(&self) -> bool {
        self.is_partial
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct ExtendInterface {
    base_name: String,
    break_names: Vec<String>,
}

impl ExtendInterface {
    pub(crate) fn base_name(&self) -> &str {
        &self.base_name
    }

    pub(crate) fn break_names(&self) -> &[String] {
        &self.break_names
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct ClassInterface {
    class_type: ast::ClassType,
    import_map: ImportMap,
    components: IndexMap<String, ComponentInterface>,
    nested_classes: Vec<NestedClassInterface>,
    extends: Vec<ExtendInterface>,
    extends_bases: Vec<String>,
}

impl ClassInterface {
    pub(crate) fn class_type(&self) -> &ast::ClassType {
        &self.class_type
    }

    pub(crate) fn import_map(&self) -> &ImportMap {
        &self.import_map
    }

    pub(crate) fn component_type(&self, component_name: &str) -> Option<&str> {
        self.components
            .get(component_name)
            .map(ComponentInterface::type_name)
    }

    pub(crate) fn component_interfaces(
        &self,
    ) -> impl Iterator<Item = (&String, &ComponentInterface)> {
        self.components.iter()
    }

    pub(crate) fn component_interface(&self, component_name: &str) -> Option<&ComponentInterface> {
        self.components.get(component_name)
    }

    #[cfg(test)]
    pub(crate) fn nested_class_interfaces(&self) -> &[NestedClassInterface] {
        &self.nested_classes
    }

    #[cfg(test)]
    pub(crate) fn extends_bases(&self) -> &[String] {
        &self.extends_bases
    }

    pub(crate) fn extends(&self) -> &[ExtendInterface] {
        &self.extends
    }

    pub(crate) fn type_resolution_candidates(
        &self,
        enclosing_qualified_name: &str,
        raw_type_name: &str,
    ) -> Vec<String> {
        if raw_type_name.is_empty() {
            return Vec::new();
        }

        let mut seen = IndexSet::new();
        let mut candidates = Vec::new();
        let mut push = |candidate: String| {
            if !candidate.is_empty() && seen.insert(candidate.clone()) {
                candidates.push(candidate);
            }
        };

        if raw_type_name.contains('.') {
            push(raw_type_name.to_string());
            return candidates;
        }

        if self
            .nested_classes
            .iter()
            .any(|nested_class| nested_class.name == raw_type_name)
        {
            push(format!("{enclosing_qualified_name}.{raw_type_name}"));
        }

        for candidate in self.import_map.resolve_candidates(raw_type_name) {
            push(candidate);
        }

        push(raw_type_name.to_string());
        candidates
    }

    pub(crate) fn local_completion_items(&self) -> Vec<ClassLocalCompletionItem> {
        let mut items = Vec::new();

        for (name, component) in &self.components {
            items.push(ClassLocalCompletionItem {
                name: name.clone(),
                detail: component.type_name.clone(),
                kind: component.completion_kind(),
            });
        }

        for nested in &self.nested_classes {
            items.push(ClassLocalCompletionItem {
                name: nested.name.clone(),
                detail: format!("{:?}", nested.class_type),
                kind: ClassLocalCompletionKind::Class,
            });
        }

        items
    }

    pub(crate) fn local_component_info(&self, component_name: &str) -> Option<LocalComponentInfo> {
        self.component_interface(component_name)
            .map(|component| component.local_component_info(component_name))
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct FileClassInterfaceIndex {
    item_keys_by_name: IndexMap<String, ItemKey>,
    interfaces: IndexMap<ItemKey, ClassInterface>,
}

impl FileClassInterfaceIndex {
    pub(crate) fn from_summary(summary: &FileSummary) -> Self {
        let mut index = Self::default();
        for (item_key, class) in summary.iter() {
            let qualified_name = item_key.qualified_name();
            index
                .item_keys_by_name
                .entry(qualified_name)
                .or_insert_with(|| item_key.clone());
            index
                .interfaces
                .insert(item_key.clone(), ClassInterface::from_summary(class));
        }
        index
    }

    pub(crate) fn class_interface(&self, qualified_name: &str) -> Option<&ClassInterface> {
        let item_key = self.item_keys_by_name.get(qualified_name)?;
        self.interfaces.get(item_key)
    }

    pub(crate) fn item_key_for_name(&self, qualified_name: &str) -> Option<&ItemKey> {
        self.item_keys_by_name.get(qualified_name)
    }
}

impl ClassInterface {
    fn from_summary(class: &super::file_summary::ClassSummary) -> Self {
        Self {
            class_type: class.class_type.clone(),
            import_map: import_map_from_summary(class),
            components: class
                .components
                .iter()
                .map(|(name, component)| {
                    (
                        name.clone(),
                        ComponentInterface {
                            name_location: component.name_location.clone(),
                            type_name: component.type_name.clone(),
                            variability: component.variability.clone(),
                            causality: component.causality.clone(),
                            connection: component.connection.clone(),
                            is_final: component.is_final,
                            is_replaceable: component.is_replaceable,
                            constrainedby: component.constrainedby.clone(),
                            shape: component.shape.clone(),
                        },
                    )
                })
                .collect(),
            nested_classes: class
                .nested_classes
                .iter()
                .map(|name| {
                    let nested = class
                        .nested_class_headers
                        .get(name)
                        .expect("nested class header should be present");
                    NestedClassInterface {
                        name: name.clone(),
                        class_type: nested.class_type.clone(),
                        is_partial: nested.partial,
                        is_replaceable: nested.is_replaceable,
                    }
                })
                .collect(),
            extends: class
                .extends
                .iter()
                .map(|extend| ExtendInterface {
                    base_name: extend.base_name.clone(),
                    break_names: extend.break_names.clone(),
                })
                .collect(),
            extends_bases: class
                .extends
                .iter()
                .map(|extend| extend.base_name.clone())
                .collect(),
        }
    }
}

pub(crate) fn resolve_import_candidates(
    raw_type_name: &str,
    import_map: Option<&ImportMap>,
) -> Vec<String> {
    let mut seen = std::collections::HashSet::<String>::new();
    let mut candidates = Vec::new();
    let mut push = |name: String| {
        if !name.is_empty() && seen.insert(name.clone()) {
            candidates.push(name);
        }
    };

    push(raw_type_name.to_string());
    if raw_type_name.contains('.') {
        return candidates;
    }

    if let Some(import_map) = import_map {
        for candidate in import_map.resolve_candidates(raw_type_name) {
            push(candidate);
        }
    }

    candidates
}

fn import_map_from_summary(class: &super::file_summary::ClassSummary) -> ImportMap {
    let mut explicit_bindings = Vec::new();
    let mut wildcard_paths = Vec::new();

    for import in &class.imports {
        match import {
            ImportSummary::Qualified { path, .. } => {
                explicit_bindings.push((import_simple_name(path).to_string(), path.clone()));
            }
            ImportSummary::Renamed { alias, path, .. } => {
                explicit_bindings.push((alias.clone(), path.clone()));
            }
            ImportSummary::Selective { path, names, .. } => {
                for name in names {
                    explicit_bindings.push((name.clone(), format!("{path}.{name}")));
                }
            }
            ImportSummary::Unqualified { path, .. } => {
                wildcard_paths.push(path.clone());
            }
        }
    }

    ImportMap {
        explicit_bindings,
        wildcard_paths,
    }
}

fn import_simple_name(path: &str) -> &str {
    path.rsplit('.').next().unwrap_or(path)
}
