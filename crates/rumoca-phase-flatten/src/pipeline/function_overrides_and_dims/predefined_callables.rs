//! Exact declaration identities of the predefined callables (MLS §3.7).
//!
//! Predefined operators such as `inStream`, `actualStream`, `der` or `sample`
//! are registered by Resolve as predefined scope members, not as classes, so
//! they never appear in the resolved class index. They are legal callables
//! with their own typed lowering path (stream-operator expansion, builtin-call
//! lowering), and they never participate in replaceable-function selection.
//!
//! The catalog is keyed on the exact `DefId` Resolve allocated for each
//! predefined name. A user declaration that merely reuses one of these
//! spellings owns a different `DefId` and therefore never matches, so this is
//! an exact-identity query rather than a name-based fallback.

use rumoca_core::{ComponentPath, DefId};
use rustc_hash::FxHashSet;

#[derive(Debug, Clone, Default)]
pub(super) struct PredefinedCallableIds {
    identities: FxHashSet<DefId>,
}

impl PredefinedCallableIds {
    pub(super) fn from_tree(tree: &rumoca_ir_ast::ClassTree) -> Self {
        let identities = rumoca_core::BUILTIN_FUNCTIONS
            .iter()
            .chain(rumoca_core::BUILTIN_TYPES.iter())
            .chain(rumoca_core::BUILTIN_VARIABLES.iter())
            .filter_map(|name| {
                tree.scope_tree
                    .predefined_member(&ComponentPath::from_flat_path(name))
            })
            .collect();
        Self { identities }
    }

    pub(super) fn contains(&self, def_id: DefId) -> bool {
        self.identities.contains(&def_id)
    }
}
