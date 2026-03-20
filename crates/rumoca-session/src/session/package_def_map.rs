use super::declaration_index::ItemKey;
use super::file_summary::FileSummary;
use indexmap::{IndexMap, IndexSet};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone)]
pub(crate) struct PackageDefEntry {
    pub(crate) item_key: ItemKey,
}

#[derive(Debug, Clone, Default)]
struct PackageDefNode {
    declared_class: Option<PackageDefEntry>,
    children: IndexSet<String>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct PackageDefMap {
    nodes: IndexMap<String, PackageDefNode>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct PersistedPackageDefEntry {
    uri: String,
    qualified_name: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct PersistedPackageDefMap {
    entries: Vec<PersistedPackageDefEntry>,
}

impl PackageDefMap {
    pub(crate) fn declared_class(&self, qualified_name: &str) -> Option<&PackageDefEntry> {
        self.nodes
            .get(qualified_name)
            .and_then(|node| node.declared_class.as_ref())
    }

    pub(crate) fn extend_from_package_def_map(&mut self, other: &PackageDefMap) {
        for (qualified_name, entry) in other.class_entries() {
            self.insert_declared_class(qualified_name.clone(), entry.clone());
        }
    }

    pub(crate) fn extend_from_summary(&mut self, summary: &FileSummary) {
        for (item_key, _) in summary.iter() {
            self.insert_declared_class(
                item_key.qualified_name(),
                PackageDefEntry {
                    item_key: item_key.clone(),
                },
            );
        }
    }

    pub(crate) fn from_persisted(
        persisted: &PersistedPackageDefMap,
        file_id_for_uri: impl Fn(&str) -> Option<super::FileId>,
    ) -> Self {
        let mut def_map = Self::default();
        for entry in &persisted.entries {
            let Some(file_id) = file_id_for_uri(&entry.uri) else {
                continue;
            };
            let (container_path, name) = split_qualified_name(&entry.qualified_name);
            def_map.insert_declared_class(
                entry.qualified_name.clone(),
                PackageDefEntry {
                    item_key: ItemKey::new(
                        file_id,
                        super::declaration_index::ItemKind::Class,
                        container_path,
                        name,
                    ),
                },
            );
        }
        def_map
    }

    pub(crate) fn class_entries(&self) -> impl Iterator<Item = (&String, &PackageDefEntry)> {
        self.nodes.iter().filter_map(|(qualified_name, node)| {
            node.declared_class
                .as_ref()
                .map(|entry| (qualified_name, entry))
        })
    }

    pub(crate) fn namespace_nodes(&self) -> impl Iterator<Item = (&String, &IndexSet<String>)> {
        self.nodes
            .iter()
            .filter(|(_, node)| !node.children.is_empty())
            .map(|(qualified_name, node)| (qualified_name, &node.children))
    }

    #[cfg(test)]
    pub(crate) fn children(&self, prefix: &str) -> Vec<String> {
        let mut children = self
            .nodes
            .get(&normalize_node_key(prefix))
            .map(|node| node.children.iter().cloned().collect::<Vec<_>>())
            .unwrap_or_default();
        children.sort_unstable();
        children
    }

    #[cfg(test)]
    pub(crate) fn member_item_keys(&self, prefix: &str) -> Vec<ItemKey> {
        let mut item_keys = self
            .children(prefix)
            .into_iter()
            .filter_map(|qualified_name| {
                self.nodes
                    .get(&qualified_name)
                    .and_then(|node| node.declared_class.as_ref())
                    .map(|entry| entry.item_key.clone())
            })
            .collect::<Vec<_>>();
        item_keys.sort_by_key(ItemKey::qualified_name);
        item_keys
    }

    fn insert_declared_class(&mut self, qualified_name: String, entry: PackageDefEntry) {
        self.nodes.entry(String::new()).or_default();
        let segments = qualified_name.split('.').collect::<Vec<_>>();
        for index in 0..segments.len() {
            let parent = segments[..index].join(".");
            let current = segments[..=index].join(".");
            self.nodes
                .entry(parent)
                .or_default()
                .children
                .insert(current.clone());
            self.nodes.entry(current).or_default();
        }
        self.nodes
            .entry(qualified_name)
            .or_default()
            .declared_class
            .get_or_insert(entry);
    }
}

impl PersistedPackageDefMap {
    pub(crate) fn from_file_summaries(
        file_summaries_by_uri: &IndexMap<String, FileSummary>,
    ) -> Self {
        Self {
            entries: file_summaries_by_uri
                .iter()
                .flat_map(|(uri, summary)| {
                    summary
                        .iter()
                        .map(move |(item_key, _)| PersistedPackageDefEntry {
                            uri: uri.clone(),
                            qualified_name: item_key.qualified_name(),
                        })
                })
                .collect(),
        }
    }
}

pub(crate) fn namespace_prefix_key(qualified_name: &str) -> String {
    if qualified_name.is_empty() {
        String::new()
    } else {
        format!("{qualified_name}.")
    }
}

#[cfg(test)]
fn normalize_node_key(prefix: &str) -> String {
    prefix.trim().trim_end_matches('.').to_string()
}

fn split_qualified_name(qualified_name: &str) -> (String, String) {
    match qualified_name.rsplit_once('.') {
        Some((container_path, name)) => (container_path.to_string(), name.to_string()),
        None => (String::new(), qualified_name.to_string()),
    }
}
