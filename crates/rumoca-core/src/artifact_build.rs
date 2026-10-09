//! Immutable, target-neutral build inventory and borrowing slot requests.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

struct Inventory<S> {
    files: BTreeMap<PathBuf, Vec<u8>>,
    session: S,
    slots: BTreeMap<String, BTreeMap<String, String>>,
}

/// One owned inventory identity. Reconstructing the same bytes and session
/// creates a distinct identity; no public operation can transplant its brand.
pub struct PreparedBuildInventory<S>(Arc<Inventory<S>>);

/// Exact borrowed inventory and declared slot. Fields cannot be replaced.
pub struct PreparedBuildRequest<'inventory, S> {
    inventory: &'inventory PreparedBuildInventory<S>,
    slot: &'inventory str,
}

/// Origin binding only, not evidence that a tool ran or semantics were checked.
/// Checked tool results remain privately constructed in their adapter.
pub struct ArtifactBuildBinding<S> {
    inventory: Arc<Inventory<S>>,
    slot: String,
}

impl<S> PreparedBuildInventory<S> {
    pub fn construct(
        files: Vec<(PathBuf, Vec<u8>)>,
        session: S,
        slots: BTreeMap<String, BTreeMap<String, String>>,
    ) -> Result<Self, String> {
        let mut owned = BTreeMap::new();
        for (path, bytes) in files {
            validate_artifact_relative_path(&path)?;
            if owned.insert(path, bytes).is_some() {
                return Err("duplicate build inventory file".into());
            }
        }
        validate_artifact_file_paths(&owned.keys().cloned().collect())?;
        if slots.keys().any(|slot| slot.is_empty()) {
            return Err("empty build slot".into());
        }
        Ok(Self(Arc::new(Inventory {
            files: owned,
            session,
            slots,
        })))
    }

    pub fn files(&self) -> &BTreeMap<PathBuf, Vec<u8>> {
        &self.0.files
    }
    pub fn session(&self) -> &S {
        &self.0.session
    }

    pub fn request(&self, slot: &str) -> Result<PreparedBuildRequest<'_, S>, String> {
        let (slot, _) = self
            .0
            .slots
            .get_key_value(slot)
            .ok_or("missing declared build slot")?;
        Ok(PreparedBuildRequest {
            inventory: self,
            slot,
        })
    }

    pub fn accepts(&self, binding: &ArtifactBuildBinding<S>, slot: &str) -> bool {
        Arc::ptr_eq(&self.0, &binding.inventory)
            && binding.slot == slot
            && self.0.slots.contains_key(slot)
    }

    pub fn into_files(self) -> Result<BTreeMap<PathBuf, Vec<u8>>, String> {
        Arc::try_unwrap(self.0)
            .map(|inventory| inventory.files)
            .map_err(|_| "build inventory still has outstanding result bindings".into())
    }
}

impl<S> PreparedBuildRequest<'_, S> {
    pub fn files(&self) -> &BTreeMap<PathBuf, Vec<u8>> {
        self.inventory.files()
    }
    pub fn session(&self) -> &S {
        self.inventory.session()
    }
    pub fn parameters(&self) -> &BTreeMap<String, String> {
        &self.inventory.0.slots[self.slot]
    }
    pub fn binding(&self) -> ArtifactBuildBinding<S> {
        ArtifactBuildBinding {
            inventory: Arc::clone(&self.inventory.0),
            slot: self.slot.into(),
        }
    }
}

impl<S> ArtifactBuildBinding<S> {
    pub fn slot(&self) -> &str {
        &self.slot
    }
}

pub fn validate_artifact_relative_path(path: &Path) -> Result<(), String> {
    if path.as_os_str().is_empty()
        || path
            .components()
            .any(|part| !matches!(part, Component::Normal(_)))
    {
        return Err("artifact path must be a normalized relative child".into());
    }
    Ok(())
}

pub fn validate_artifact_file_paths(paths: &BTreeSet<PathBuf>) -> Result<(), String> {
    for path in paths {
        validate_artifact_relative_path(path)?;
        let mut ancestor = path.parent();
        while let Some(parent) = ancestor.filter(|parent| !parent.as_os_str().is_empty()) {
            if paths.contains(parent) {
                return Err(format!(
                    "artifact path '{}' is both a file and a directory",
                    parent.display()
                ));
            }
            ancestor = parent.parent();
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn inventory() -> PreparedBuildInventory<u32> {
        PreparedBuildInventory::construct(
            vec![("source".into(), vec![1])],
            7,
            BTreeMap::from([
                ("one".into(), BTreeMap::new()),
                ("two".into(), BTreeMap::new()),
            ]),
        )
        .unwrap()
    }

    #[test]
    fn identical_session_and_files_do_not_transfer_a_slot_binding() {
        let first = inventory();
        let second = inventory();
        let binding = first.request("one").unwrap().binding();
        assert_eq!(first.session(), second.session());
        assert_eq!(first.files(), second.files());
        assert!(first.accepts(&binding, "one"));
        assert!(!first.accepts(&binding, "two"));
        assert!(!second.accepts(&binding, "one"));
        assert!(first.request("missing").is_err());
    }

    #[test]
    fn duplicate_and_conflicting_inventory_files_are_refused() {
        assert!(
            PreparedBuildInventory::construct(
                vec![("a".into(), vec![]), ("a".into(), vec![])],
                (),
                BTreeMap::new()
            )
            .is_err()
        );
        assert!(
            PreparedBuildInventory::construct(
                vec![("a".into(), vec![]), ("a/b".into(), vec![])],
                (),
                BTreeMap::new()
            )
            .is_err()
        );
        assert!(
            PreparedBuildInventory::construct(vec![("../a".into(), vec![])], (), BTreeMap::new())
                .is_err()
        );
    }
}
