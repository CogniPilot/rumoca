use super::*;

#[derive(Debug, Clone)]
pub(crate) struct IndexedBinding {
    pub(super) slot: ScalarSlot,
    pub(super) indices: Vec<usize>,
}

#[derive(Default)]
pub(super) struct RecordComponentSources {
    pub(super) layout: Vec<(String, String, ScalarSlot)>,
    pub(super) direct: Vec<(String, String)>,
}

pub(super) type IndexedRecordFieldKeyCache =
    std::cell::RefCell<IndexMap<(String, String), Arc<IndexMap<Vec<usize>, String>>>>;

pub(crate) type IndexedBindingMap = Arc<IndexMap<ComponentReferenceKey, Vec<IndexedBinding>>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct LocalIndexedBinding {
    pub(super) reg: Reg,
    pub(super) indices: Vec<usize>,
}
