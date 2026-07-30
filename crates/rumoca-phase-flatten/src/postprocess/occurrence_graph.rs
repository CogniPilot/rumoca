//! Navigate the Flat occurrence graph by exact declaration identity.
//!
//! Instantiation records one occurrence per concrete component, connector,
//! record field and class body (`flat::Model::instance_relations`). Each
//! occurrence carries the `DefId` it was declared by and, for an array
//! element, that element's indices. A source-written reference such as
//! `plug.pin[k].v` is exactly a walk over that graph: start at the class
//! occurrence the equation belongs to, then select one member per path part.
//!
//! Resolving a reference this way is what lets the collapse pass name the flat
//! variable an occurrence materialized without synthesizing or parsing a
//! rendered path (SPEC_0036).

use rumoca_core::{DefId, InstanceId};
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;

/// Bound on how many nested base-class occurrences a member lookup searches.
///
/// Inherited members live under the base-class occurrence rather than the
/// deriving class occurrence, so a member lookup descends through `extends`
/// levels. The bound keeps a cyclic graph from looping.
const MAX_INHERITED_SCOPE_DEPTH: u8 = 32;

/// One occurrence in the Flat occurrence graph.
struct Occurrence {
    declaration: Option<DefId>,
    indices: Box<[i64]>,
    kind: flat::InstanceKind,
}

/// Position of a partially walked reference path.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum PathCursor {
    /// The path so far names exactly this occurrence.
    At(InstanceId),
    /// The path so far selected an array component whose element subscripts
    /// have not been supplied yet.
    ///
    /// Flatten spells `plug.pin[k]` as an `Index` node over a `plug.pin` base,
    /// so the array declaration is selected one step before its indices are
    /// known.
    PendingIndices {
        scope: InstanceId,
        declaration: DefId,
    },
}

/// Exact occurrence lookup for one flat model.
pub(crate) struct OccurrenceGraph {
    occurrences: FxHashMap<InstanceId, Occurrence>,
    members: FxHashMap<InstanceId, Vec<InstanceId>>,
}

impl OccurrenceGraph {
    pub(crate) fn build(flat: &flat::Model) -> Self {
        let mut occurrences =
            FxHashMap::with_capacity_and_hasher(flat.instance_relations.len(), Default::default());
        let mut members: FxHashMap<InstanceId, Vec<InstanceId>> = FxHashMap::default();
        for (instance_id, relation) in &flat.instance_relations {
            occurrences.insert(
                *instance_id,
                Occurrence {
                    declaration: relation.declaration,
                    indices: relation.indices.clone(),
                    kind: relation.kind,
                },
            );
            if let Some(owner) = relation.owner {
                members.entry(owner).or_default().push(*instance_id);
            }
        }
        Self {
            occurrences,
            members,
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.occurrences.is_empty()
    }

    pub(crate) fn kind(&self, instance_id: InstanceId) -> Option<flat::InstanceKind> {
        self.occurrences
            .get(&instance_id)
            .map(|occurrence| occurrence.kind)
    }

    /// The class occurrence whose members a path may select next.
    ///
    /// A component occurrence (`Aggregate`) owns exactly one class-body
    /// occurrence; a class-body occurrence is already the scope. A
    /// materialized leaf has no members.
    fn member_scope(&self, instance_id: InstanceId) -> Option<InstanceId> {
        match self.occurrences.get(&instance_id)?.kind {
            flat::InstanceKind::Class => Some(instance_id),
            flat::InstanceKind::Aggregate => {
                let mut bodies = self
                    .members
                    .get(&instance_id)?
                    .iter()
                    .copied()
                    .filter(|member| self.kind(*member) == Some(flat::InstanceKind::Class));
                let body = bodies.next()?;
                bodies.next().is_none().then_some(body)
            }
            flat::InstanceKind::Materialized => None,
        }
    }

    /// Select the member declared by `declaration` under `cursor`.
    ///
    /// `indices` is the element selection already supplied by the path; an
    /// empty slice means none was, which resolves to the scalar occurrence
    /// when the declaration has one and to a pending array selection when it
    /// does not.
    pub(crate) fn select_member(
        &self,
        cursor: PathCursor,
        declaration: DefId,
        indices: &[i64],
    ) -> Option<PathCursor> {
        // An array component still awaiting its indices has no members: the
        // element has to be chosen before anything inside it can be.
        let PathCursor::At(instance_id) = cursor else {
            return None;
        };
        let scope = self.member_scope(instance_id)?;
        self.select_in_scope(scope, declaration, indices, 0)
    }

    /// Supply element indices to a pending array selection.
    ///
    /// An empty `indices` leaves the cursor untouched so a path part without
    /// subscripts stays where it is.
    pub(crate) fn apply_indices(&self, cursor: PathCursor, indices: &[i64]) -> Option<PathCursor> {
        if indices.is_empty() {
            return Some(cursor);
        }
        let PathCursor::PendingIndices { scope, declaration } = cursor else {
            // Indexing an already-selected occurrence would mean subscripting
            // a materialized array variable, which names no separate
            // occurrence.
            return None;
        };
        self.select_in_scope(scope, declaration, indices, 0)
    }

    fn select_in_scope(
        &self,
        scope: InstanceId,
        declaration: DefId,
        indices: &[i64],
        depth: u8,
    ) -> Option<PathCursor> {
        if depth > MAX_INHERITED_SCOPE_DEPTH {
            return None;
        }
        let members = self.members.get(&scope)?;
        let declared: Vec<&Occurrence> = members
            .iter()
            .filter_map(|member| self.occurrences.get(member))
            .filter(|occurrence| occurrence.declaration == Some(declaration))
            .collect();
        if declared.is_empty() {
            // MLS §7.1: an inherited member occurs under the base-class body,
            // so a lookup that found nothing here continues into the classes
            // this scope extends.
            return members
                .iter()
                .copied()
                .filter(|member| self.kind(*member) == Some(flat::InstanceKind::Class))
                .find_map(|base| self.select_in_scope(base, declaration, indices, depth + 1));
        }

        let mut exact = members
            .iter()
            .copied()
            .filter(|member| {
                self.occurrences.get(member).is_some_and(|occurrence| {
                    occurrence.declaration == Some(declaration) && *occurrence.indices == *indices
                })
            })
            .fuse();
        match (exact.next(), exact.next()) {
            (Some(selected), None) => return Some(PathCursor::At(selected)),
            // Two occurrences of one declaration share element indices: the
            // path does not name a single occurrence.
            (Some(_), Some(_)) => return None,
            (None, _) => {}
        }

        // No occurrence carries these indices. With none supplied this is an
        // array declaration whose element the enclosing `Index` node selects;
        // with indices supplied the element is out of range for this model.
        (indices.is_empty()
            && declared
                .iter()
                .all(|occurrence| !occurrence.indices.is_empty()))
        .then_some(PathCursor::PendingIndices { scope, declaration })
    }
}
