//! BLT (Block Lower Triangular) block construction from SCCs.
//!
//! Regular equation families whose rows are provably self-contained are emitted
//! as one compact [`BltBlock::StructuredScalar`] and removed from the Tarjan
//! node set, so a whole-array ODE produces one block instead of one block (and
//! one cloned [`UnknownId`]) per array element.

use crate::incidence::{Incidence, StructuredMatchingFamily};
use crate::tarjan::tarjan_scc;
use crate::types::{BltBlock, EquationRef, StructuredScalarBlock, UnknownId};

/// Build BLT blocks from the incidence data, matching, and dependency graph.
///
/// Tarjan emits SCCs in reverse topological order of the condensation DAG.
/// Since dependency edges point from dependent → dependency, this output order
/// is already the correct BLT evaluation order (dependencies first).
pub(crate) fn build_blt_blocks<'dae>(
    incidence: &Incidence<'dae>,
    match_eq: &[Option<usize>],
    adj: &[Vec<usize>],
) -> Vec<BltBlock<'dae>> {
    let (structured, condensed) = split_self_contained_families(incidence, match_eq);
    let Some(condensed) = condensed else {
        return sccs_to_blocks(tarjan_scc(incidence.n_eq, adj), incidence, match_eq);
    };

    // Every condensed row was *checked* (not extrapolated) to reference exactly
    // one unknown that is its own match, so it has out-degree zero in `adj`: it
    // depends on nothing and is a valid first position in any topological
    // order. Rows that depend on anything outside their own match keep their
    // family out of `condensed` entirely, so no dependency can be scheduled
    // after a row that needs it.
    let (sub_adj, node_of_local) = condense_nodes(adj, &condensed.keep);
    let sccs = tarjan_scc(node_of_local.len(), &sub_adj);
    let mut blocks: Vec<BltBlock<'dae>> = structured
        .into_iter()
        .map(BltBlock::StructuredScalar)
        .collect();
    blocks.extend(sccs.into_iter().map(|scc| {
        let global: Vec<usize> = scc
            .iter()
            .filter_map(|&local| node_of_local.get(local).copied())
            .collect();
        scc_to_block(&global, incidence, match_eq)
    }));
    blocks
}

fn sccs_to_blocks<'dae>(
    sccs: Vec<Vec<usize>>,
    incidence: &Incidence<'dae>,
    match_eq: &[Option<usize>],
) -> Vec<BltBlock<'dae>> {
    sccs.into_iter()
        .map(|scc| scc_to_block(&scc, incidence, match_eq))
        .collect()
}

/// The rows kept in the Tarjan node set after compact families are removed.
struct CondensedNodes {
    keep: Vec<bool>,
}

/// Split the equation set into compact family blocks and the rows Tarjan still
/// has to sort.
///
/// Returns `(blocks, None)` when no family qualifies, so the common
/// (array-free) path allocates nothing extra.
fn split_self_contained_families(
    incidence: &Incidence<'_>,
    match_eq: &[Option<usize>],
) -> (Vec<StructuredScalarBlock>, Option<CondensedNodes>) {
    let mut blocks = Vec::new();
    let mut keep: Option<Vec<bool>> = None;
    for family in &incidence.structured_matching {
        let Some(range) = family.row_range() else {
            continue;
        };
        if !family_is_self_contained(family, incidence, match_eq) {
            continue;
        }
        let flags = keep.get_or_insert_with(|| vec![true; incidence.n_eq]);
        if !claim_rows(flags, range) {
            continue;
        }
        blocks.push(structured_scalar_block(family));
    }
    if blocks.is_empty() {
        return (blocks, None);
    }
    (blocks, keep.map(|keep| CondensedNodes { keep }))
}

/// Remove `range` from the Tarjan node set, refusing when any of its rows was
/// already claimed by an earlier family.
///
/// Family row ranges are disjoint by construction; a degraded descriptor that
/// overlapped another family would otherwise emit the same scalar row in two
/// compact blocks while removing it from Tarjan once.
fn claim_rows(flags: &mut [bool], range: std::ops::Range<usize>) -> bool {
    if range
        .clone()
        .any(|row| flags.get(row).copied() != Some(true))
    {
        return false;
    }
    for row in range {
        if let Some(flag) = flags.get_mut(row) {
            *flag = false;
        }
    }
    true
}

/// Whether every row of `family` is provably its own singleton SCC.
///
/// The property is *established per row*, never extrapolated from the base
/// cell: the descriptor's affine candidate is only a prediction, and a family
/// whose interiors were materialized (or whose interior bodies simply do not
/// translate) can carry rows the base cell says nothing about. For every scalar
/// row the family owns, both of these must hold:
/// 1. the row references exactly ONE unknown; and
/// 2. that unknown is the row's match.
///
/// Then the row's only column is matched to the row itself, so the row has
/// out-degree zero in the dependency graph: it is a singleton SCC *and* it
/// depends on nothing. Weakening (1) to "no intra-family edges" would NOT be
/// sound -- an out-edge to a row outside the family can still place the family
/// row in a multi-node SCC, and would also break the topological position the
/// compact block is emitted at.
///
/// The scan is `O(rows)` slice comparisons on the CSR store, which is the same
/// order as the Tarjan pass it replaces.
fn family_is_self_contained(
    family: &StructuredMatchingFamily,
    incidence: &Incidence<'_>,
    match_eq: &[Option<usize>],
) -> bool {
    let Some(range) = family.row_range() else {
        return false;
    };
    if range.end > incidence.n_eq || family.point_count == 0 || family.equations_per_point == 0 {
        return false;
    }
    for point in 0..family.point_count {
        for position in 0..family.equations_per_point {
            if !row_is_self_contained(family, incidence, match_eq, point, position) {
                return false;
            }
        }
    }
    true
}

/// Whether one scalar row of `family` references exactly its own matched
/// unknown and nothing else.
fn row_is_self_contained(
    family: &StructuredMatchingFamily,
    incidence: &Incidence,
    match_eq: &[Option<usize>],
    point: usize,
    position: usize,
) -> bool {
    let Some((row, unknown)) = family.candidate(point, position) else {
        return false;
    };
    incidence.eq_unknowns.row(row) == [unknown]
        && match_eq.get(row).copied().flatten() == Some(unknown)
}

fn structured_scalar_block(family: &StructuredMatchingFamily) -> StructuredScalarBlock {
    StructuredScalarBlock {
        span: family.span,
        first_equation_index: family.first_equation_index,
        equations_per_point: family.equations_per_point,
        point_count: family.point_count,
        extents: family.extents.clone(),
        cell_strides: family.cell_strides.clone(),
        base_unknowns: family.base_unknowns.clone(),
        unknown_steps: family.unknown_steps.clone(),
    }
}

/// Restrict `adj` to the nodes flagged in `keep`, renumbering them densely.
///
/// Edges into removed nodes are dropped, which is sound because every removed
/// node was verified to have out-degree zero: with no outgoing edge it cannot
/// lie on a cycle with a kept node, and being emitted ahead of every Tarjan
/// block already satisfies the only ordering constraint it takes part in.
fn condense_nodes(adj: &[Vec<usize>], keep: &[bool]) -> (Vec<Vec<usize>>, Vec<usize>) {
    let mut local_of_node = vec![usize::MAX; keep.len()];
    let mut node_of_local = Vec::new();
    for (node, &kept) in keep.iter().enumerate() {
        if kept {
            local_of_node[node] = node_of_local.len();
            node_of_local.push(node);
        }
    }
    let mut condensed: Vec<Vec<usize>> = vec![Vec::new(); node_of_local.len()];
    for (local, &node) in node_of_local.iter().enumerate() {
        let Some(neighbors) = adj.get(node) else {
            continue;
        };
        condensed[local].extend(
            neighbors
                .iter()
                .filter_map(|&target| local_of_node.get(target).copied())
                .filter(|&target| target != usize::MAX),
        );
    }
    (condensed, node_of_local)
}

/// Convert a single SCC into a BLT block.
fn scc_to_block<'dae>(
    scc: &[usize],
    incidence: &Incidence<'dae>,
    match_eq: &[Option<usize>],
) -> BltBlock<'dae> {
    if let [eq_idx] = scc {
        let eq_idx = *eq_idx;
        let eq_ref = incidence.equation_refs[eq_idx];
        let unknown = match match_eq[eq_idx] {
            Some(var_idx) => incidence.unknowns[var_idx],
            None => UnknownId::Unmatched { equation: eq_idx },
        };
        return BltBlock::Scalar {
            equation: eq_ref,
            unknown,
        };
    }
    let equations: Vec<EquationRef> = scc.iter().map(|&i| incidence.equation_refs[i]).collect();
    let unknowns: Vec<UnknownId<'dae>> = scc
        .iter()
        .filter_map(|&i| match_eq[i].map(|v| incidence.unknowns[v]))
        .collect();
    BltBlock::AlgebraicLoop {
        equations,
        unknowns,
    }
}
