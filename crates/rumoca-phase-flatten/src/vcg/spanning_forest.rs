//! The selected MLS §9.4 spanning forest, shared by `Connections.rooted` /
//! `Connections.isRoot` evaluation and by connection-equation emission.
//!
//! [`super::build_vcg`] selects the forest once (SPEC_0022 §3.12). Emission
//! then keeps the ordinary equalities of exactly the selected optional edges and
//! replaces exactly the broken ones by `equalityConstraint`, so the branch
//! orientation reported by `rooted` and the generated equations describe the
//! same tree.

use rustc_hash::{FxHashMap, FxHashSet};

use rumoca_ir_flat as flat;

use super::{VcgEdgeForest, normalize_edge_key, overconstrained_record_info};
use crate::FlattenError;

/// Optional edges of the virtual connection graph partitioned by the selected
/// spanning forest.
#[derive(Clone, Debug, Default)]
pub(crate) struct SelectedSpanningForest {
    /// Union-find over the selected optional edges alone. Required edges are
    /// excluded, so two records share a representative exactly when selected
    /// `connect` edges join them, which places them in one connection set whose
    /// ordinary equalities are retained.
    selected: VcgEdgeForest,
    /// Optional edges the forest breaks, in selection order.
    broken: Vec<(String, String)>,
}

impl SelectedSpanningForest {
    pub(super) fn new(
        required: &VcgEdgeForest,
        selected_edges: &[(String, String)],
        broken: Vec<(String, String)>,
    ) -> Self {
        let mut selected = VcgEdgeForest {
            index: required.index.clone(),
            parent: (0..required.parent.len()).collect(),
            has_definite_root: vec![false; required.parent.len()],
        };
        for (lhs, rhs) in selected_edges {
            if let (Some(lhs), Some(rhs)) = (
                selected.index.get(lhs).copied(),
                selected.index.get(rhs).copied(),
            ) {
                selected.union(lhs, rhs);
            }
        }
        Self { selected, broken }
    }

    fn representative(&mut self, record: &str) -> Option<usize> {
        let index = self.selected.index.get(record).copied()?;
        Some(self.selected.find(index))
    }

    /// Optional edges the selected forest breaks.
    #[cfg(test)]
    pub(crate) fn broken_edges(&self) -> &[(String, String)] {
        &self.broken
    }
}

/// One relation emitted for a potential connection set.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum EqualityLink {
    /// The ordinary equality `variables[lhs] = variables[rhs]` of the set.
    Equal { lhs: usize, rhs: usize },
    /// `equalityConstraint(lhs_record, rhs_record)` for one broken optional
    /// edge, emitted once per record pair across all fields of the record.
    Constraint {
        lhs_record: String,
        rhs_record: String,
        constraint_size: usize,
    },
}

/// Emission view of the selected spanning forest (MLS §9.4).
///
/// A potential connection set of overconstrained record fields is partitioned
/// into the groups joined by selected optional edges. Each group keeps an
/// equality chain; the groups are related only through the broken optional
/// edges that join them, each replaced by one `equalityConstraint` call, or
/// omitted when the constraint has zero width. A set without a broken edge
/// keeps its plain equality chain unchanged.
pub(crate) struct OverconstrainedEquationForest {
    tree: SelectedSpanningForest,
    emitted_constraints: FxHashSet<(String, String)>,
}

/// One member of a potential connection set. A member whose variable is not a
/// field of an overconstrained record in the graph has no record and a group of
/// its own that no broken edge touches, so it keeps its ordinary equality.
struct SetMember<'a> {
    record: Option<(&'a str, usize)>,
    group: usize,
}

impl OverconstrainedEquationForest {
    pub(crate) fn new(tree: SelectedSpanningForest) -> Self {
        Self {
            tree,
            emitted_constraints: FxHashSet::default(),
        }
    }

    #[cfg(test)]
    pub(crate) fn empty() -> Self {
        Self::new(SelectedSpanningForest::default())
    }

    /// The relations that realize one potential connection set.
    pub(crate) fn equality_links(
        &mut self,
        flat: &flat::Model,
        variables: &[rumoca_core::VarName],
    ) -> Result<Vec<EqualityLink>, FlattenError> {
        let chain = || {
            (1..variables.len())
                .map(|rhs| EqualityLink::Equal { lhs: rhs - 1, rhs })
                .collect()
        };
        let members = self.set_members(flat, variables);
        let records = members
            .iter()
            .filter_map(|member| member.record.map(|(record, _)| record))
            .collect::<FxHashSet<_>>();
        let broken = self
            .tree
            .broken
            .iter()
            .filter(|(lhs, rhs)| records.contains(lhs.as_str()) && records.contains(rhs.as_str()))
            .cloned()
            .collect::<Vec<_>>();
        if broken.is_empty() {
            return Ok(chain());
        }
        self.partitioned_links(&members, &broken)
    }

    /// Resolve every set member to its overconstrained record and
    /// selected-forest group. Members outside the graph get distinct groups
    /// above every forest representative.
    fn set_members<'a>(
        &mut self,
        flat: &'a flat::Model,
        variables: &[rumoca_core::VarName],
    ) -> Vec<SetMember<'a>> {
        let outside = self.tree.selected.parent.len();
        variables
            .iter()
            .enumerate()
            .map(|(index, variable)| {
                overconstrained_record_info(flat, variable)
                    .and_then(|(record, width)| {
                        let group = self.tree.representative(record)?;
                        Some(SetMember {
                            record: Some((record, width)),
                            group,
                        })
                    })
                    .unwrap_or(SetMember {
                        record: None,
                        group: outside + index,
                    })
            })
            .collect()
    }

    fn partitioned_links(
        &mut self,
        members: &[SetMember<'_>],
        broken: &[(String, String)],
    ) -> Result<Vec<EqualityLink>, FlattenError> {
        let mut links = Vec::new();
        let mut group_order = Vec::new();
        let mut first_member = FxHashMap::default();
        let mut last_member = FxHashMap::default();
        for (index, member) in members.iter().enumerate() {
            match last_member.insert(member.group, index) {
                Some(previous) => links.push(EqualityLink::Equal {
                    lhs: previous,
                    rhs: index,
                }),
                None => {
                    group_order.push(member.group);
                    first_member.insert(member.group, index);
                }
            }
        }
        let widths = members
            .iter()
            .filter_map(|member| member.record)
            .collect::<FxHashMap<_, _>>();
        let mut quotient = GroupQuotient::default();
        for (lhs, rhs) in broken {
            let (Some(lhs_group), Some(rhs_group)) =
                (self.tree.representative(lhs), self.tree.representative(rhs))
            else {
                continue;
            };
            if !quotient.join(lhs_group, rhs_group) {
                continue;
            }
            let (lhs_width, rhs_width) = (widths[lhs.as_str()], widths[rhs.as_str()]);
            if lhs_width != rhs_width {
                return Err(FlattenError::internal(format!(
                    "overconstrained record edge `{lhs}`--`{rhs}` has mismatched equalityConstraint widths {lhs_width} and {rhs_width}"
                )));
            }
            if lhs_width > 0
                && self
                    .emitted_constraints
                    .insert(normalize_edge_key(lhs, rhs))
            {
                links.push(EqualityLink::Constraint {
                    lhs_record: lhs.clone(),
                    rhs_record: rhs.clone(),
                    constraint_size: lhs_width,
                });
            }
        }
        // Groups that no broken edge of this set joins are related by an
        // ordinary equality, as every member of a connection set is equal.
        let Some((&anchor, rest)) = group_order.split_first() else {
            return Ok(links);
        };
        for &group in rest {
            if quotient.join(anchor, group) {
                links.push(EqualityLink::Equal {
                    lhs: first_member[&anchor],
                    rhs: first_member[&group],
                });
            }
        }
        Ok(links)
    }
}

/// Union-find over the selected-forest groups of one connection set.
#[derive(Default)]
struct GroupQuotient {
    parent: FxHashMap<usize, usize>,
}

impl GroupQuotient {
    fn find(&mut self, group: usize) -> usize {
        let parent = *self.parent.entry(group).or_insert(group);
        if parent == group {
            return group;
        }
        let root = self.find(parent);
        self.parent.insert(group, root);
        root
    }

    /// Join two groups; `false` when they were already joined.
    fn join(&mut self, lhs: usize, rhs: usize) -> bool {
        let (lhs, rhs) = (self.find(lhs), self.find(rhs));
        if lhs == rhs {
            return false;
        }
        self.parent.insert(lhs.max(rhs), lhs.min(rhs));
        true
    }
}

/// Build the emission forest the flatten pipeline would build for a graph.
#[cfg(test)]
pub(crate) fn test_equation_forest(
    definite_roots: &FxHashSet<String>,
    branches: &[(String, String)],
    optional_edges: &[(String, String)],
) -> OverconstrainedEquationForest {
    let data = tests::graph(definite_roots, branches);
    let required = super::RequiredEdgeForest::construct(&data, optional_edges)
        .expect("test required edges must satisfy the VCG construction contract");
    OverconstrainedEquationForest::new(
        super::build_vcg(&data, optional_edges, &required).spanning_forest,
    )
}

#[cfg(test)]
mod tests;
