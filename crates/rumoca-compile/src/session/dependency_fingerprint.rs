use indexmap::{IndexMap, IndexSet};
use rumoca_ir_ast as ast;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::PhaseResult;
use crate::traversal_adapter::collect_class_dependencies;

pub(crate) type Fingerprint = [u8; 32];

#[derive(Debug, Clone)]
pub(crate) struct CompileCacheEntry {
    pub(crate) fingerprint: Fingerprint,
    pub(crate) result: CachedCompileResult,
}

#[derive(Debug, Clone)]
pub(crate) enum CachedCompileResult {
    Full(PhaseResult),
    Success,
}

impl CachedCompileResult {
    pub(crate) fn from_phase_result(result: PhaseResult) -> Self {
        match result {
            PhaseResult::Success(_) => Self::Success,
            result => Self::Full(result),
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct DependencyFingerprintCache {
    class_hashes: IndexMap<String, Fingerprint>,
    class_deps: IndexMap<String, IndexSet<String>>,
    #[serde(skip)]
    model_fingerprints: IndexMap<String, Fingerprint>,
}

impl DependencyFingerprintCache {
    pub(crate) fn from_tree(tree: &ast::ClassTree) -> Self {
        let mut cache = Self::default();
        let mut file_bytes_cache: HashMap<String, Option<Vec<u8>>> = HashMap::new();

        for (qualified_name, &def_id) in &tree.name_map {
            let Some(class) = tree.get_class_by_def_id(def_id) else {
                continue;
            };

            cache.class_hashes.insert(
                qualified_name.clone(),
                class_source_fingerprint(tree, class, qualified_name, &mut file_bytes_cache),
            );
            cache.class_deps.insert(
                qualified_name.clone(),
                collect_class_dependencies(tree, class, qualified_name),
            );
        }

        cache
    }

    pub(crate) fn model_fingerprint(&mut self, model_name: &str) -> Fingerprint {
        let mut visiting = IndexSet::new();
        self.model_fingerprint_recursive(model_name, &mut visiting)
    }

    pub(crate) fn class_dependencies(&self) -> &IndexMap<String, IndexSet<String>> {
        &self.class_deps
    }

    pub(crate) fn merge_from(&mut self, other: &Self) {
        let mut changed_classes = IndexSet::new();

        for (qualified_name, hash) in &other.class_hashes {
            if self.class_hashes.get(qualified_name) != Some(hash) {
                changed_classes.insert(qualified_name.clone());
            }
            self.class_hashes.insert(qualified_name.clone(), *hash);
        }
        for (qualified_name, deps) in &other.class_deps {
            if self.class_deps.get(qualified_name) != Some(deps) {
                changed_classes.insert(qualified_name.clone());
            }
            self.class_deps.insert(qualified_name.clone(), deps.clone());
        }
        self.invalidate_model_fingerprints_for(changed_classes);
    }

    pub(crate) fn aggregate_fingerprint(&self) -> Fingerprint {
        let mut hasher = blake3::Hasher::new();
        hasher.update(b"rumoca-dependency-fingerprint-aggregate-v1");

        let mut class_hashes = self.class_hashes.iter().collect::<Vec<_>>();
        class_hashes.sort_by_key(|(qualified_name, _)| *qualified_name);
        for (qualified_name, fingerprint) in class_hashes {
            hasher.update(qualified_name.as_bytes());
            hasher.update(fingerprint);
        }

        let mut class_deps = self.class_deps.iter().collect::<Vec<_>>();
        class_deps.sort_by_key(|(qualified_name, _)| *qualified_name);
        for (qualified_name, deps) in class_deps {
            hasher.update(qualified_name.as_bytes());
            let mut sorted_deps = deps.iter().collect::<Vec<_>>();
            sorted_deps.sort_unstable();
            for dep in sorted_deps {
                hasher.update(dep.as_bytes());
            }
        }

        *hasher.finalize().as_bytes()
    }

    #[cfg(test)]
    pub(crate) fn replace_class_dependencies_for_test(
        &mut self,
        class_name: &str,
        deps: impl IntoIterator<Item = String>,
    ) {
        let mut changed_classes = IndexSet::new();
        changed_classes.insert(class_name.to_string());
        self.class_deps
            .insert(class_name.to_string(), deps.into_iter().collect());
        self.invalidate_model_fingerprints_for(changed_classes);
    }

    fn invalidate_model_fingerprints_for(&mut self, changed_classes: IndexSet<String>) {
        if changed_classes.is_empty() || self.model_fingerprints.is_empty() {
            return;
        }

        let affected = self.affected_classes(changed_classes);
        self.model_fingerprints
            .retain(|model_name, _| !affected.contains(model_name));
    }

    fn affected_classes(&self, changed_classes: IndexSet<String>) -> IndexSet<String> {
        let reverse_deps = self.reverse_dependencies();
        let mut affected = IndexSet::new();
        let mut pending = changed_classes.into_iter().collect::<Vec<_>>();

        while let Some(class_name) = pending.pop() {
            if !affected.insert(class_name.clone()) {
                continue;
            }
            if let Some(dependents) = reverse_deps.get(&class_name) {
                pending.extend(dependents.iter().cloned());
            }
        }

        affected
    }

    fn reverse_dependencies(&self) -> IndexMap<String, IndexSet<String>> {
        let mut reverse_deps: IndexMap<String, IndexSet<String>> = IndexMap::new();
        for (class_name, deps) in &self.class_deps {
            for dep in deps {
                reverse_deps
                    .entry(dep.clone())
                    .or_default()
                    .insert(class_name.clone());
            }
        }
        reverse_deps
    }

    fn model_fingerprint_recursive(
        &mut self,
        model_name: &str,
        visiting: &mut IndexSet<String>,
    ) -> Fingerprint {
        if let Some(fingerprint) = self.model_fingerprints.get(model_name) {
            return *fingerprint;
        }
        if !visiting.insert(model_name.to_string()) {
            let mut hasher = blake3::Hasher::new();
            hasher.update(b"rumoca-model-fingerprint-cycle-v1");
            hasher.update(model_name.as_bytes());
            return *hasher.finalize().as_bytes();
        }

        let own_hash = self
            .class_hashes
            .get(model_name)
            .copied()
            .unwrap_or_else(|| {
                let mut hasher = blake3::Hasher::new();
                hasher.update(b"rumoca-model-missing-v1");
                hasher.update(model_name.as_bytes());
                *hasher.finalize().as_bytes()
            });
        let mut deps = self
            .class_deps
            .get(model_name)
            .map(|set| set.iter().cloned().collect::<Vec<_>>())
            .unwrap_or_default();
        deps.sort_unstable();

        let mut hasher = blake3::Hasher::new();
        hasher.update(b"rumoca-model-fingerprint-v1");
        hasher.update(model_name.as_bytes());
        hasher.update(&own_hash);
        for dep in deps {
            let dep_hash = self.model_fingerprint_recursive(&dep, visiting);
            hasher.update(dep.as_bytes());
            hasher.update(&dep_hash);
        }
        let fingerprint = *hasher.finalize().as_bytes();
        visiting.shift_remove(model_name);
        self.model_fingerprints
            .insert(model_name.to_string(), fingerprint);
        fingerprint
    }
}

fn class_source_fingerprint(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    class_name: &str,
    file_bytes_cache: &mut HashMap<String, Option<Vec<u8>>>,
) -> Fingerprint {
    let location = &class.location;
    let start = location.start as usize;
    let end = location.end as usize;
    let mut hasher = blake3::Hasher::new();
    hasher.update(b"rumoca-class-source-v1");
    hasher.update(class_name.as_bytes());

    if let Some(source_id) = tree.source_map.get_id(&location.file_name)
        && let Some((_, content)) = tree.source_map.get_source(source_id)
        && !content.is_empty()
    {
        let bytes = content.as_bytes();
        if start < end && end <= bytes.len() {
            hasher.update(&bytes[start..end]);
            return *hasher.finalize().as_bytes();
        }
    }

    let file_bytes = file_bytes_cache
        .entry(location.file_name.clone())
        .or_insert_with(|| std::fs::read(&location.file_name).ok());
    if let Some(bytes) = file_bytes.as_deref()
        && start < end
        && end <= bytes.len()
    {
        hasher.update(&bytes[start..end]);
        return *hasher.finalize().as_bytes();
    }

    // Fallback for virtual or unavailable files.
    hasher.update(location.file_name.as_bytes());
    hasher.update(&location.start.to_le_bytes());
    hasher.update(&location.end.to_le_bytes());
    hasher.update(format!("{:?}", class.class_type).as_bytes());
    hasher.update(class.name.text.as_bytes());
    *hasher.finalize().as_bytes()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Session;

    fn test_fingerprint(byte: u8) -> Fingerprint {
        [byte; 32]
    }

    fn cache_with_classes(classes: &[(&str, u8, &[&str])]) -> DependencyFingerprintCache {
        let mut cache = DependencyFingerprintCache::default();
        for (class_name, hash_byte, deps) in classes {
            cache
                .class_hashes
                .insert((*class_name).to_string(), test_fingerprint(*hash_byte));
            cache.class_deps.insert(
                (*class_name).to_string(),
                deps.iter().map(|dep| (*dep).to_string()).collect(),
            );
        }
        cache
    }

    #[test]
    fn from_tree_collects_import_dependencies() {
        let source = r#"
            package P
              model Dep
                Real y;
              equation
                y = 1;
              end Dep;

              model Root
                import P.Dep;
                Real x;
              equation
                x = 1;
              end Root;
            end P;
        "#;

        let mut session = Session::default();
        session
            .add_document("test.mo", source)
            .expect("document should parse");
        session
            .build_resolved()
            .expect("resolved tree should be available");
        let tree = &session
            .ensure_resolved()
            .expect("resolved tree should be cached")
            .0;
        let cache = DependencyFingerprintCache::from_tree(tree);
        let deps = cache
            .class_dependencies()
            .get("P.Root")
            .cloned()
            .unwrap_or_default();

        assert!(
            deps.iter().any(|dep| dep == "P.Dep"),
            "import dependency should be included in class dependency graph"
        );
    }

    #[test]
    fn merge_from_keeps_unaffected_model_fingerprint_cache_entries() {
        let mut cache = cache_with_classes(&[
            ("P.Root", 1, &["P.Dep"]),
            ("P.Dep", 2, &[]),
            ("P.Sibling", 3, &[]),
        ]);

        let root_fingerprint = cache.model_fingerprint("P.Root");
        let sibling_fingerprint = cache.model_fingerprint("P.Sibling");

        let updated = cache_with_classes(&[("P.Sibling", 4, &[])]);
        cache.merge_from(&updated);

        assert_eq!(
            cache.model_fingerprints.get("P.Root"),
            Some(&root_fingerprint),
            "unaffected reachable closures should stay warm after an unrelated class changes"
        );
        assert_ne!(
            cache.model_fingerprint("P.Sibling"),
            sibling_fingerprint,
            "changed classes should be recomputed"
        );
    }

    #[test]
    fn merge_from_invalidates_reverse_dependency_closure() {
        let mut cache = cache_with_classes(&[
            ("P.Root", 1, &["P.Dep"]),
            ("P.Dep", 2, &[]),
            ("P.Sibling", 3, &[]),
        ]);

        cache.model_fingerprint("P.Root");
        let sibling_fingerprint = cache.model_fingerprint("P.Sibling");

        let updated = cache_with_classes(&[("P.Dep", 5, &[])]);
        cache.merge_from(&updated);

        assert!(
            !cache.model_fingerprints.contains_key("P.Root"),
            "dependent model fingerprints must be invalidated when a dependency changes"
        );
        assert_eq!(
            cache.model_fingerprints.get("P.Sibling"),
            Some(&sibling_fingerprint),
            "unrelated cached model fingerprints should remain warm"
        );
    }

    #[test]
    fn model_fingerprint_ignores_unreachable_classes() {
        let source_v1 = r#"
            package P
              model Dep
                Real y;
              equation
                y = 1;
              end Dep;

              model Root
                Dep d;
              equation
                d.y = 2;
              end Root;

              model Unused
                Real z;
              equation
                z = 3;
              end Unused;
            end P;
        "#;

        let source_v2 = r#"
            package P
              model Dep
                Real y;
              equation
                y = 1;
              end Dep;

              model Root
                Dep d;
              equation
                d.y = 2;
              end Root;

              model Unused
                Real z;
              equation
                z = 30;
              end Unused;
            end P;
        "#;

        let mut session_v1 = Session::default();
        session_v1
            .add_document("test.mo", source_v1)
            .expect("first document should parse");
        session_v1
            .build_resolved()
            .expect("first tree should resolve");
        let tree_v1 = &session_v1
            .ensure_resolved()
            .expect("first resolved tree should be cached")
            .0;
        let mut cache_v1 = DependencyFingerprintCache::from_tree(tree_v1);
        let fingerprint_v1 = cache_v1.model_fingerprint("P.Root");

        let mut session_v2 = Session::default();
        session_v2
            .add_document("test.mo", source_v2)
            .expect("second document should parse");
        session_v2
            .build_resolved()
            .expect("second tree should resolve");
        let tree_v2 = &session_v2
            .ensure_resolved()
            .expect("second resolved tree should be cached")
            .0;
        let mut cache_v2 = DependencyFingerprintCache::from_tree(tree_v2);
        let fingerprint_v2 = cache_v2.model_fingerprint("P.Root");

        assert_eq!(
            fingerprint_v1, fingerprint_v2,
            "reachable model fingerprint should not change when an unreachable class changes"
        );
    }

    #[test]
    fn from_tree_collects_external_function_argument_dependencies() {
        let source = r#"
            package P
              function Helper
                input Real u;
                output Real y;
              algorithm
                y := u;
              end Helper;

              function ExternalUser
                input Real u;
                output Real y;
              external "C" y = native_call(Helper(u));
              end ExternalUser;
            end P;
        "#;

        let mut session = Session::default();
        session
            .add_document("test.mo", source)
            .expect("document should parse");
        session
            .build_resolved()
            .expect("resolved tree should be available");
        let tree = &session
            .ensure_resolved()
            .expect("resolved tree should be cached")
            .0;
        let cache = DependencyFingerprintCache::from_tree(tree);
        let deps = cache
            .class_dependencies()
            .get("P.ExternalUser")
            .cloned()
            .unwrap_or_default();

        assert!(
            deps.iter().any(|dep| dep == "P.Helper"),
            "external declaration arguments should participate in dependency collection"
        );
    }
}
