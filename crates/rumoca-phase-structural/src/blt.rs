//! BLT (Block Lower Triangular) block construction from SCCs.
//!
//! Permuting a matched sparse system to block triangular form by taking the
//! strongly connected components of the matched digraph is the Dulmage-
//! Mendelsohn decomposition: A. L. Dulmage and N. S. Mendelsohn, "Coverings of
//! bipartite graphs", Canadian Journal of Mathematics 10:517-534, 1958,
//! doi:10.4153/CJM-1958-052-0. The algorithmic treatment, including why the
//! SCC order is the correct solve order, is I. S. Duff, A. M. Erisman and J. K.
//! Reid, "Direct Methods for Sparse Matrices", 2nd ed., Oxford University Press
//! 2017, chapter 6.
//!
//! Regular equation families whose rows are provably self-contained are emitted
//! as one compact [`BltBlock::StructuredScalar`] and removed from the Tarjan
//! node set, so a whole-array ODE produces one block instead of one block (and
//! one cloned [`UnknownId`]) per array element.

mod declaration_runs;

pub(crate) use declaration_runs::ErasedStructuredScalarBlock;
pub use declaration_runs::StructuredFamilyContext;

use crate::incidence::{Incidence, StructuredMatchingFamily};
use crate::tarjan::tarjan_scc;
use crate::types::{BltBlock, EquationRef, StructuralError, UnknownId};
use rumoca_core::Span;

#[derive(Debug, Clone)]
pub struct StructuredScalarBlock<'dae> {
    matching: std::sync::Arc<crate::incidence::StructuredMatchingFamily>,
    context: Option<StructuredFamilyContext<'dae>>,
}

impl<'dae> StructuredScalarBlock<'dae> {
    pub fn context(&self) -> Option<&StructuredFamilyContext<'dae>> {
        self.context.as_ref()
    }

    pub fn shares_matching_owner(&self, other: &Self) -> bool {
        std::sync::Arc::ptr_eq(&self.matching, &other.matching)
    }

    pub fn span(&self) -> Span {
        self.matching.span
    }

    pub fn first_equation_index(&self) -> usize {
        self.matching.first_equation_index
    }

    pub fn point_count(&self) -> usize {
        self.matching.point_count
    }

    pub fn equations_per_point(&self) -> usize {
        self.matching.equations_per_point
    }

    pub fn extents(&self) -> &[usize] {
        &self.matching.extents
    }

    pub fn unknown_steps(&self) -> &[Vec<i64>] {
        &self.matching.unknown_steps
    }

    pub fn row_range(&self) -> std::ops::Range<usize> {
        self.matching
            .row_range()
            .expect("issued matching row range is checked")
    }

    #[must_use]
    pub fn scalar_block_count(&self) -> usize {
        self.matching
            .row_count()
            .expect("issued matching row count is checked")
    }

    pub fn scalar_rows(
        &self,
    ) -> impl Iterator<Item = Result<(EquationRef, usize), StructuralError>> + '_ {
        (0..self.point_count()).flat_map(move |point| {
            (0..self.equations_per_point()).map(move |position| {
                self.scalar_row(point, position)
                    .ok_or_else(|| StructuralError::ContractViolation {
                        reason: format!(
                            "compact family at f_x[{}] cannot address point {point}, body {position}",
                            self.first_equation_index()
                        ),
                        span: self.span(),
                    })
            })
        })
    }

    pub fn scalar_row(&self, point: usize, position: usize) -> Option<(EquationRef, usize)> {
        self.matching
            .candidate(point, position)
            .map(|(equation, unknown)| (EquationRef(equation), unknown))
    }
}

/// Build BLT blocks from the incidence data, matching, and dependency graph.
///
/// Tarjan emits SCCs in reverse topological order of the condensation DAG.
/// Since dependency edges point from dependent → dependency, this output order
/// is already the correct BLT evaluation order (dependencies first).
pub(crate) fn build_blt_blocks<'dae>(
    view: Option<rumoca_ir_dae::DaeView<'dae>>,
    incidence: &Incidence<'dae>,
    match_eq: &[Option<usize>],
    adj: &[Vec<usize>],
) -> Vec<BltBlock<'dae>> {
    let (structured, condensed) = split_self_contained_families(view, incidence, match_eq);
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
fn split_self_contained_families<'dae>(
    view: Option<rumoca_ir_dae::DaeView<'dae>>,
    incidence: &Incidence<'dae>,
    match_eq: &[Option<usize>],
) -> (Vec<StructuredScalarBlock<'dae>>, Option<CondensedNodes>) {
    let mut blocks = Vec::new();
    let mut keep: Option<Vec<bool>> = None;
    for family in &incidence.structured_matching {
        let Some(range) = family.row_range() else {
            continue;
        };
        let Some(declarations) = family_is_self_contained(family, incidence, match_eq) else {
            continue;
        };
        let flags = keep.get_or_insert_with(|| vec![true; incidence.n_eq]);
        if !claim_rows(flags, range) {
            continue;
        }
        blocks.push(structured_scalar_block(
            view,
            family,
            incidence,
            declarations,
        ));
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
fn family_is_self_contained<'dae>(
    family: &StructuredMatchingFamily,
    incidence: &Incidence<'dae>,
    match_eq: &[Option<usize>],
) -> Option<Option<Box<[UnknownId<'dae>]>>> {
    let range = family.row_range()?;
    if range.end > incidence.n_eq || family.point_count == 0 || family.equations_per_point == 0 {
        return None;
    }
    let mut declarations = declaration_runs::base_declarations(family, incidence);
    for point in 0..family.point_count {
        for position in 0..family.equations_per_point {
            let unknown = row_is_self_contained(family, incidence, match_eq, point, position)?;
            if declarations.as_ref().is_some_and(|bases| {
                !declaration_runs::matches_declaration(family, incidence, bases, unknown, position)
            }) {
                declarations = None;
            }
        }
    }
    Some(declarations)
}

/// Whether one scalar row of `family` references exactly its own matched
/// unknown and nothing else.
fn row_is_self_contained(
    family: &StructuredMatchingFamily,
    incidence: &Incidence,
    match_eq: &[Option<usize>],
    point: usize,
    position: usize,
) -> Option<usize> {
    let (row, unknown) = family.candidate(point, position)?;
    (incidence.eq_unknowns.row(row) == [unknown]
        && match_eq.get(row).copied().flatten() == Some(unknown))
    .then_some(unknown)
}

fn structured_scalar_block<'dae>(
    view: Option<rumoca_ir_dae::DaeView<'dae>>,
    family: &StructuredMatchingFamily,
    incidence: &Incidence<'dae>,
    declarations: Option<Box<[UnknownId<'dae>]>>,
) -> StructuredScalarBlock<'dae> {
    StructuredScalarBlock {
        matching: std::sync::Arc::new(family.clone()),
        context: view.and_then(|view| {
            declaration_runs::checked_source(view, family, incidence, declarations)
        }),
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
    let tearing = tear_scc(scc, incidence, match_eq);
    BltBlock::AlgebraicLoop {
        equations,
        unknowns,
        tearing,
    }
}

/// Tear a coupled SCC into a reduced iteration set plus a causal
/// back-substitution, addressing the block's `equations`/`unknowns` by local
/// position. Returns `None` when every equation is matched (fully causal, not a
/// loop) or when tearing cannot reduce the dimension.
fn tear_scc(
    scc: &[usize],
    incidence: &Incidence<'_>,
    match_eq: &[Option<usize>],
) -> Option<crate::tearing::TearingResult> {
    // Local position of each SCC unknown, keyed by its global unknown index.
    // Only equations that are themselves matched contribute an unknown, so a
    // partially matched SCC yields fewer unknowns than equations and cannot be
    // torn into a square reduced system here.
    if scc.iter().any(|&eq| match_eq[eq].is_none()) {
        return None;
    }
    let mut var_local: std::collections::HashMap<usize, usize> =
        std::collections::HashMap::with_capacity(scc.len());
    for (local, &eq) in scc.iter().enumerate() {
        if let Some(var) = match_eq[eq] {
            var_local.insert(var, local);
        }
    }
    if var_local.len() != scc.len() {
        return None;
    }
    let eq_unknowns: Vec<std::collections::HashSet<usize>> = scc
        .iter()
        .map(|&eq| {
            incidence
                .eq_unknowns
                .row(eq)
                .iter()
                .filter_map(|global| var_local.get(global).copied())
                .collect()
        })
        .collect();
    crate::tearing::tear_algebraic_loop(scc.len(), &eq_unknowns)
}
