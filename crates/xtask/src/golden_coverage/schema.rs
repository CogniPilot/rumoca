#[cfg(test)]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct RustItemId {
    pub(super) package: String,
    pub(super) module: RustModuleId,
    pub(super) scope: RustItemScope,
    pub(super) kind: RustItemKind,
    pub(super) item_name: String,
}

#[cfg(test)]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum RustModuleId {
    CrateRoot,
    Path(Vec<String>),
}

#[cfg(test)]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum RustItemScope {
    Module,
    Impl {
        type_path: Vec<String>,
    },
    TraitImpl {
        trait_path: Vec<String>,
        type_path: Vec<String>,
    },
}

#[cfg(test)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum RustItemKind {
    Function,
    Method,
}
