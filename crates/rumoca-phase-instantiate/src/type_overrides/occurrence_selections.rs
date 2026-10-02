//! Instance-exact class selections for the member tail of a deferred reference.
//!
//! MLS §7.3: the class of a component declared through a replaceable alias
//! (`Medium.BaseProperties medium`) is selected per occurrence, by the
//! redeclarations that reach that occurrence. A reference such as
//! `tank.medium.T` written outside `tank` therefore names a member whose
//! declaring class only the materialized occurrence `tank.medium` proves; the
//! class tree records no single class for the declaration.

use std::cell::OnceCell;

use crate::{InstantiateError, InstantiateResult};
use rumoca_core::{DefId, InstanceId};
use rumoca_ir_ast as ast;
use rustc_hash::FxHashMap;

/// Selected class of each component declaration in one instantiation scope.
///
/// Keys are the component declaration identities Resolve records on a
/// reference root; values are the classes instantiation actually selected for
/// those occurrences. With an occurrence source, a declaration path below a
/// root also resolves to the class its materialized occurrences selected.
#[derive(Default)]
pub(crate) struct SelectedComponentTypes<'o> {
    direct: FxHashMap<DefId, DefId>,
    occurrences: Option<ScopeOccurrences<'o>>,
}

struct ScopeOccurrences<'o> {
    scope: InstanceId,
    overlay: &'o ast::InstanceOverlay,
    index: OnceCell<OccurrenceIndex>,
}

/// Component occurrences grouped by owning class occurrence, and the class
/// occurrence each composite component materialized.
#[derive(Default)]
struct OccurrenceIndex {
    members: FxHashMap<InstanceId, Vec<MemberOccurrence>>,
    classes: FxHashMap<InstanceId, InstanceId>,
}

struct MemberOccurrence {
    declaration: DefId,
    selected: Option<DefId>,
    component: InstanceId,
}

impl<'o> SelectedComponentTypes<'o> {
    /// Selections of the components directly owned by `scope`, with nested
    /// member paths proved from the materialized occurrences of `overlay`.
    pub(crate) fn of_scope(overlay: &'o ast::InstanceOverlay, scope: InstanceId) -> Self {
        let direct = overlay
            .components
            .values()
            .filter(|component| component.owner_class_id == Some(scope))
            .filter_map(|component| {
                let declaration = component.component_ref.as_ref()?.target_def_id();
                Some((declaration, component.type_def_id?))
            })
            .collect();
        Self {
            direct,
            occurrences: Some(ScopeOccurrences {
                scope,
                overlay,
                index: OnceCell::new(),
            }),
        }
    }

    pub(crate) fn get(&self, declaration: &DefId) -> Option<DefId> {
        self.direct.get(declaration).copied()
    }

    pub(crate) fn insert(&mut self, declaration: DefId, selected: DefId) -> Option<DefId> {
        self.direct.insert(declaration, selected)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.direct.is_empty() && self.occurrences.is_none()
    }

    /// The class every occurrence of the declaration path selected.
    ///
    /// `path` starts at a component owned by the scope. `None` means the
    /// scope has no occurrence source or no occurrence of the path; a path
    /// whose occurrences selected different classes has no single declaring
    /// class for its member tail and is an error.
    pub(crate) fn selected_at_path(
        &self,
        path: &[DefId],
        span: rumoca_core::Span,
    ) -> InstantiateResult<Option<DefId>> {
        let Some(occurrences) = &self.occurrences else {
            return Ok(None);
        };
        let index = occurrences
            .index
            .get_or_init(|| OccurrenceIndex::new(occurrences.overlay));
        let mut owners = vec![occurrences.scope];
        let mut selected = None;
        for declaration in path {
            let matching = owners
                .iter()
                .filter_map(|owner| index.members.get(owner))
                .flatten()
                .filter(|member| member.declaration == *declaration)
                .collect::<Vec<_>>();
            if matching.is_empty() {
                return Ok(None);
            }
            selected = single_selection(&matching, span)?;
            owners = matching
                .iter()
                .filter_map(|member| index.classes.get(&member.component).copied())
                .collect();
        }
        Ok(selected)
    }
}

impl OccurrenceIndex {
    fn new(overlay: &ast::InstanceOverlay) -> Self {
        let mut index = Self::default();
        for component in overlay.components.values() {
            let (Some(owner), Some(reference)) =
                (component.owner_class_id, component.component_ref.as_ref())
            else {
                continue;
            };
            index
                .members
                .entry(owner)
                .or_default()
                .push(MemberOccurrence {
                    declaration: reference.target_def_id(),
                    selected: component.type_def_id,
                    component: component.instance_id,
                });
        }
        for class in overlay.classes.values() {
            if let Some(component) = class.owner_component_id {
                index.classes.insert(component, class.instance_id);
            }
        }
        index
    }
}

fn single_selection(
    occurrences: &[&MemberOccurrence],
    span: rumoca_core::Span,
) -> InstantiateResult<Option<DefId>> {
    let mut selected = occurrences.iter().map(|member| member.selected);
    let first = selected.next().flatten();
    if selected.any(|other| other != first) {
        return Err(Box::new(InstantiateError::redeclare_error(
            "<occurrence>",
            "occurrences of one component declaration selected different classes, so the \
             member tail has no single declaring class",
            span,
        )));
    }
    Ok(first)
}
