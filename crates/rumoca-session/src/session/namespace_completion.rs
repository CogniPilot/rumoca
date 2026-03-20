use super::package_def_map::{PackageDefMap, namespace_prefix_key};
use indexmap::{IndexMap, IndexSet};
#[cfg(test)]
use rumoca_ir_ast as ast;
use std::sync::OnceLock;

pub(crate) type NamespaceCompletionEntry = (String, String, bool);
type Fingerprint = [u8; 32];

#[derive(Debug, Default)]
pub(crate) struct NamespaceCompletionCache {
    class_names: Vec<String>,
    namespace_edges: IndexMap<String, IndexSet<String>>,
    children_by_prefix: IndexMap<String, Vec<NamespaceCompletionEntry>>,
    namespace_fingerprints: OnceLock<IndexMap<String, Fingerprint>>,
}

impl Clone for NamespaceCompletionCache {
    fn clone(&self) -> Self {
        let namespace_fingerprints = OnceLock::new();
        if let Some(fingerprints) = self.namespace_fingerprints.get() {
            let _ = namespace_fingerprints.set(fingerprints.clone());
        }
        Self {
            class_names: self.class_names.clone(),
            namespace_edges: self.namespace_edges.clone(),
            children_by_prefix: self.children_by_prefix.clone(),
            namespace_fingerprints,
        }
    }
}

impl NamespaceCompletionCache {
    #[cfg(test)]
    pub(crate) fn from_documents<'a>(
        definitions: impl Iterator<Item = &'a ast::StoredDefinition>,
    ) -> Self {
        let mut collector = NamespaceCacheCollector::default();

        for definition in definitions {
            collect_namespace_entries_from_definition(definition, &mut collector);
        }

        collector.finish()
    }

    pub(crate) fn class_names(&self) -> &[String] {
        &self.class_names
    }

    pub(crate) fn children(&self, prefix: &str) -> Vec<NamespaceCompletionEntry> {
        self.children_by_prefix
            .get(&normalize_namespace_prefix(prefix))
            .cloned()
            .unwrap_or_default()
    }

    pub(crate) fn fingerprint_hex(&self, prefix: &str) -> Option<String> {
        self.namespace_fingerprints()
            .get(&normalize_namespace_prefix(prefix))
            .map(fingerprint_to_hex)
    }

    fn namespace_fingerprints(&self) -> &IndexMap<String, Fingerprint> {
        self.namespace_fingerprints
            .get_or_init(|| build_namespace_fingerprints(&self.namespace_edges))
    }

    pub(crate) fn extend_from_package_def_map(&mut self, index: &PackageDefMap) {
        self.class_names.extend(
            index
                .class_entries()
                .map(|(qualified_name, _)| qualified_name.clone()),
        );
        for (prefix, edges) in index.namespace_nodes() {
            self.namespace_edges
                .entry(namespace_prefix_key(prefix))
                .or_default()
                .extend(edges.iter().cloned());
        }
    }

    pub(crate) fn finalize(mut self) -> Self {
        self.class_names.sort_unstable();
        self.class_names.dedup();
        self.children_by_prefix = build_children_by_prefix(&self.namespace_edges);
        self.namespace_fingerprints = OnceLock::new();
        self
    }
}

#[cfg(test)]
#[derive(Debug, Default)]
struct NamespaceCacheCollector {
    class_names: IndexSet<String>,
    namespace_edges: IndexMap<String, IndexSet<String>>,
}

#[cfg(test)]
impl NamespaceCacheCollector {
    fn record_class(&mut self, qualified_name: &str) {
        self.class_names.insert(qualified_name.to_string());
        add_namespace_edges(qualified_name, &mut self.namespace_edges);
    }

    fn finish(self) -> NamespaceCompletionCache {
        let mut class_names = self.class_names.into_iter().collect::<Vec<_>>();
        class_names.sort_unstable();
        let children_by_prefix = build_children_by_prefix(&self.namespace_edges);

        NamespaceCompletionCache {
            class_names,
            namespace_edges: self.namespace_edges,
            children_by_prefix,
            namespace_fingerprints: OnceLock::new(),
        }
    }
}

fn normalize_namespace_prefix(prefix: &str) -> String {
    let trimmed = prefix.trim();
    if trimmed.is_empty() {
        String::new()
    } else if trimmed.ends_with('.') {
        trimmed.to_string()
    } else {
        format!("{trimmed}.")
    }
}

#[cfg(test)]
fn collect_namespace_entries_from_definition(
    definition: &ast::StoredDefinition,
    collector: &mut NamespaceCacheCollector,
) {
    let within = definition
        .within
        .as_ref()
        .map(ToString::to_string)
        .unwrap_or_default();
    collect_namespace_entries_recursive(&definition.classes, &within, collector);
}

#[cfg(test)]
fn collect_namespace_entries_recursive(
    classes: &IndexMap<String, ast::ClassDef>,
    prefix: &str,
    collector: &mut NamespaceCacheCollector,
) {
    for (name, class) in classes {
        let qualified_name = join_qualified_name(prefix, name);
        collector.record_class(&qualified_name);
        if !class.classes.is_empty() {
            collect_namespace_entries_recursive(&class.classes, &qualified_name, collector);
        }
    }
}

#[cfg(test)]
fn join_qualified_name(prefix: &str, name: &str) -> String {
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{prefix}.{name}")
    }
}

#[cfg(test)]
fn add_namespace_edges(
    qualified_name: &str,
    namespace_edges: &mut IndexMap<String, IndexSet<String>>,
) {
    let segments: Vec<_> = qualified_name.split('.').collect();
    for index in 0..segments.len() {
        let prefix = namespace_prefix_for_segments(&segments[..index]);
        let full_name = segments[..=index].join(".");
        namespace_edges.entry(prefix).or_default().insert(full_name);
    }
}

#[cfg(test)]
fn namespace_prefix_for_segments(segments: &[&str]) -> String {
    if segments.is_empty() {
        String::new()
    } else {
        format!("{}.", segments.join("."))
    }
}

fn build_children_by_prefix(
    namespace_edges: &IndexMap<String, IndexSet<String>>,
) -> IndexMap<String, Vec<NamespaceCompletionEntry>> {
    let mut children_by_prefix = IndexMap::new();
    let mut prefixes = namespace_edges.keys().cloned().collect::<Vec<_>>();
    prefixes.sort_unstable();

    for prefix in prefixes {
        let mut children = namespace_edges
            .get(&prefix)
            .into_iter()
            .flat_map(|set| set.iter())
            .map(|full_name| {
                let child = full_name
                    .rsplit('.')
                    .next()
                    .unwrap_or(full_name)
                    .to_string();
                let child_prefix = format!("{full_name}.");
                let has_children = namespace_edges.contains_key(&child_prefix);
                (child, full_name.clone(), has_children)
            })
            .collect::<Vec<_>>();
        children.sort_by(|left, right| left.0.cmp(&right.0).then_with(|| left.1.cmp(&right.1)));
        children_by_prefix.insert(prefix, children);
    }

    children_by_prefix
}

fn build_namespace_fingerprints(
    namespace_edges: &IndexMap<String, IndexSet<String>>,
) -> IndexMap<String, Fingerprint> {
    let mut memo = IndexMap::new();
    let mut prefixes = namespace_edges.keys().cloned().collect::<Vec<_>>();
    prefixes.sort_unstable();
    for prefix in prefixes {
        namespace_fingerprint_recursive(&prefix, namespace_edges, &mut memo);
    }
    memo
}

fn namespace_fingerprint_recursive(
    prefix: &str,
    namespace_edges: &IndexMap<String, IndexSet<String>>,
    memo: &mut IndexMap<String, Fingerprint>,
) -> Fingerprint {
    if let Some(fingerprint) = memo.get(prefix) {
        return *fingerprint;
    }

    let own_name = prefix.strip_suffix('.').unwrap_or(prefix);
    let own_hash = synthetic_namespace_hash(own_name);
    let mut children = namespace_edges
        .get(prefix)
        .map(|set| set.iter().cloned().collect::<Vec<_>>())
        .unwrap_or_default();
    children.sort_unstable();

    let mut hasher = blake3::Hasher::new();
    hasher.update(b"rumoca-namespace-closure-v1");
    hasher.update(prefix.as_bytes());
    hasher.update(&own_hash);

    for child_full_name in children {
        hasher.update(child_full_name.as_bytes());
        let child_hash =
            child_namespace_or_class_fingerprint(&child_full_name, namespace_edges, memo);
        hasher.update(&child_hash);
    }

    let fingerprint = *hasher.finalize().as_bytes();
    memo.insert(prefix.to_string(), fingerprint);
    fingerprint
}

fn child_namespace_or_class_fingerprint(
    full_name: &str,
    namespace_edges: &IndexMap<String, IndexSet<String>>,
    memo: &mut IndexMap<String, Fingerprint>,
) -> Fingerprint {
    let child_prefix = format!("{full_name}.");
    if namespace_edges.contains_key(&child_prefix) {
        namespace_fingerprint_recursive(&child_prefix, namespace_edges, memo)
    } else {
        synthetic_namespace_hash(full_name)
    }
}

fn synthetic_namespace_hash(name: &str) -> Fingerprint {
    let mut hasher = blake3::Hasher::new();
    hasher.update(b"rumoca-namespace-synthetic-v1");
    hasher.update(name.as_bytes());
    *hasher.finalize().as_bytes()
}

fn fingerprint_to_hex(fingerprint: &Fingerprint) -> String {
    fingerprint
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect::<String>()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsing::parse_source_to_ast;

    #[test]
    fn namespace_cache_tracks_immediate_children_and_fingerprints() {
        let source = r#"
            package Lib
              package Electrical
                package Analog
                  model Resistor
                  end Resistor;
                end Analog;
              end Electrical;
            end Lib;
        "#;
        let definition = parse_source_to_ast(source, "Lib/package.mo").expect("parse");
        let cache = NamespaceCompletionCache::from_documents([&definition].into_iter());

        assert_eq!(
            cache.children("Lib."),
            vec![("Electrical".to_string(), "Lib.Electrical".to_string(), true)]
        );
        assert!(
            cache.fingerprint_hex("Lib.").is_some(),
            "namespace closure fingerprint should be available"
        );
    }
}
