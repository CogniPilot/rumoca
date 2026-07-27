use std::collections::HashSet;

use super::*;
use crate::incidence::corners::tests::test_span;
use crate::incidence::rows::IncidenceRows;
use crate::types::StructuralError;

/// Expand a block, requiring every row to resolve.
fn expanded(block: &BltBlock, unknown_names: &[UnknownId]) -> Vec<(EquationRef, UnknownId)> {
    block
        .expand_scalar_blocks(unknown_names)
        .collect::<Result<Vec<_>, _>>()
        .expect("every row of a well-formed block resolves")
}

fn make_incidence(
    n_eq: usize,
    n_var: usize,
    eq_unknowns: Vec<HashSet<usize>>,
    eq_refs: Vec<EquationRef>,
    unknown_names: Vec<UnknownId>,
) -> Incidence {
    Incidence {
        n_eq,
        n_var,
        eq_unknowns: IncidenceRows::from_sets(eq_unknowns),
        unknown_names,
        unknown_spans: vec![None; n_var],
        equation_refs: eq_refs,
        structured_matching: Vec::new(),
    }
}

fn named(values: &[&str]) -> Vec<UnknownId> {
    values
        .iter()
        .map(|name| UnknownId::Variable(rumoca_core::VarName::from(*name)))
        .collect()
}

#[test]
fn test_blt_linear_chain() {
    // 3 equations in a linear chain: eq0→eq1→eq2 (no cycles)
    let eq_unknowns = vec![
        HashSet::from([0]),
        HashSet::from([0, 1]),
        HashSet::from([1, 2]),
    ];
    let eq_refs = vec![EquationRef(0), EquationRef(1), EquationRef(2)];
    let incidence = make_incidence(3, 3, eq_unknowns, eq_refs, named(&["a", "b", "c"]));

    let match_eq = vec![Some(0), Some(1), Some(2)];
    let adj = vec![vec![], vec![0], vec![1]];

    let blocks = build_blt_blocks(&incidence, &match_eq, &adj);
    assert_eq!(blocks.len(), 3, "3 scalar blocks");
    assert!(
        blocks.iter().all(|b| matches!(b, BltBlock::Scalar { .. })),
        "all blocks should be scalar"
    );
}

#[test]
fn test_blt_single_algebraic_loop() {
    let eq_unknowns = vec![HashSet::from([0, 1]), HashSet::from([0, 1])];
    let eq_refs = vec![EquationRef(0), EquationRef(1)];
    let incidence = make_incidence(2, 2, eq_unknowns, eq_refs, named(&["y", "z"]));

    let match_eq = vec![Some(0), Some(1)];
    let adj = vec![vec![1], vec![0]];

    let blocks = build_blt_blocks(&incidence, &match_eq, &adj);
    assert_eq!(blocks.len(), 1, "one algebraic loop block");
    assert!(
        matches!(
            &blocks[0],
            BltBlock::AlgebraicLoop { equations, unknowns }
                if equations.len() == 2 && unknowns.len() == 2
        ),
        "expected a 2x2 algebraic loop, got {:?}",
        blocks[0]
    );
}

#[test]
fn test_blt_mixed() {
    let eq_unknowns = vec![
        HashSet::from([0]),
        HashSet::from([0, 1, 2]),
        HashSet::from([1, 2]),
    ];
    let eq_refs = vec![EquationRef(0), EquationRef(1), EquationRef(2)];
    let incidence = make_incidence(3, 3, eq_unknowns, eq_refs, named(&["a", "b", "c"]));

    let match_eq = vec![Some(0), Some(1), Some(2)];
    let adj = vec![vec![], vec![0, 2], vec![1]];

    let blocks = build_blt_blocks(&incidence, &match_eq, &adj);
    assert_eq!(blocks.len(), 2, "one scalar + one loop");
    assert!(matches!(&blocks[0], BltBlock::Scalar { .. }));
    assert!(matches!(&blocks[1], BltBlock::AlgebraicLoop { .. }));
}

#[test]
fn unmatched_scalar_block_uses_typed_unmatched_unknown() {
    let eq_unknowns = vec![HashSet::new()];
    let incidence = make_incidence(1, 1, eq_unknowns, vec![EquationRef(0)], named(&["a"]));
    let blocks = build_blt_blocks(&incidence, &[None], &[Vec::new()]);
    let unknowns: Vec<UnknownId> = expanded(&blocks[0], &incidence.unknown_names)
        .into_iter()
        .map(|(_, unknown)| unknown)
        .collect();
    assert_eq!(unknowns, vec![UnknownId::Unmatched { equation: 0 }]);
    assert_eq!(unknowns[0].to_string(), "<unmatched f_x[0]>");
}

/// Incidence for `der(x[i]) = -x[i]` over an N-cell regular family: row `i`
/// references only unknown `i` and is matched to it.
fn whole_array_incidence(cells: usize) -> (Incidence, Vec<Option<usize>>, Vec<Vec<usize>>) {
    let eq_unknowns: Vec<HashSet<usize>> = (0..cells).map(|i| HashSet::from([i])).collect();
    let eq_refs: Vec<EquationRef> = (0..cells).map(EquationRef).collect();
    let unknown_names: Vec<UnknownId> = (0..cells)
        .map(|i| UnknownId::DerState(rumoca_core::VarName::from(format!("x[{i}]").as_str())))
        .collect();
    let mut incidence = make_incidence(cells, cells, eq_unknowns, eq_refs, unknown_names);
    incidence.structured_matching = vec![one_dim_family(cells)];
    let match_eq: Vec<Option<usize>> = (0..cells).map(Some).collect();
    let adj = vec![Vec::new(); cells];
    (incidence, match_eq, adj)
}

/// A 1-D family of `cells` rows starting at row 0, cell `p` solving unknown `p`.
fn one_dim_family(cells: usize) -> StructuredMatchingFamily {
    StructuredMatchingFamily {
        span: test_span(),
        first_equation_index: 0,
        equations_per_point: 1,
        point_count: cells,
        extents: vec![cells],
        cell_strides: vec![1],
        base_unknowns: vec![0],
        unknown_steps: vec![vec![1]],
    }
}

#[test]
fn regular_family_with_self_contained_rows_emits_one_structured_block() {
    const CELLS: usize = 4_096;
    let (incidence, match_eq, adj) = whole_array_incidence(CELLS);

    let blocks = build_blt_blocks(&incidence, &match_eq, &adj);

    assert_eq!(blocks.len(), 1, "the whole family collapses to one block");
    assert_eq!(blocks[0].scalar_block_count(), CELLS);
    let rows = expanded(&blocks[0], &incidence.unknown_names);
    let reference: Vec<(EquationRef, UnknownId)> = (0..CELLS)
        .map(|i| (EquationRef(i), incidence.unknown_names[i].clone()))
        .collect();
    assert_eq!(rows, reference);
}

/// Expanding a compact block against an unknown table that does not hold its
/// indices is a spanned contract violation, not an `Unmatched` stand-in: the
/// rows *are* matched, so naming them unmatched would invent semantics
/// (SPEC_0008 fail-fast).
#[test]
fn expanding_a_compact_block_against_a_short_name_table_errors() {
    const CELLS: usize = 4;
    let (incidence, match_eq, adj) = whole_array_incidence(CELLS);
    let blocks = build_blt_blocks(&incidence, &match_eq, &adj);

    let truncated = &incidence.unknown_names[..2];
    let rows: Result<Vec<_>, _> = blocks[0].expand_scalar_blocks(truncated).collect();
    let err = rows.expect_err("indices 2 and 3 are outside the truncated table");
    assert!(matches!(err, StructuralError::ContractViolation { .. }));
    assert_eq!(err.source_span(), Some(test_span()));
}

/// A descriptor whose per-position vectors are shorter than
/// `equations_per_point` cannot address its own rows; expansion reports rather
/// than skipping the rows it cannot build.
#[test]
fn scalar_rows_reports_a_row_it_cannot_address() {
    let block = StructuredScalarBlock {
        span: test_span(),
        first_equation_index: 0,
        equations_per_point: 2,
        point_count: 2,
        extents: vec![2],
        cell_strides: vec![1],
        // Only one position is described, so position 1 has no base unknown.
        base_unknowns: vec![0],
        unknown_steps: vec![vec![1]],
    };
    let rows: Vec<_> = block.scalar_rows().collect();
    assert_eq!(rows.len(), 4, "no row is silently dropped");
    assert!(rows[0].is_ok());
    let err = rows[1].as_ref().expect_err("position 1 is undescribed");
    assert!(matches!(err, StructuralError::ContractViolation { .. }));
}

#[test]
fn family_with_intra_family_edges_falls_back_to_scalar_blocks() {
    // A stencil row that also references its neighbour's matched unknown gives
    // the base row out-degree 1, so the family may not be emitted compactly.
    const CELLS: usize = 8;
    let (mut incidence, match_eq, mut adj) = whole_array_incidence(CELLS);
    let mut widened: Vec<HashSet<usize>> = (0..CELLS).map(|i| HashSet::from([i])).collect();
    widened[0].insert(1);
    incidence.eq_unknowns = IncidenceRows::from_sets(widened);
    adj[0].push(1);

    let blocks = build_blt_blocks(&incidence, &match_eq, &adj);

    assert_eq!(blocks.len(), CELLS, "every row stays its own scalar block");
    assert!(blocks.iter().all(|b| matches!(b, BltBlock::Scalar { .. })));
}

#[test]
fn family_whose_rows_are_not_matched_to_their_candidate_falls_back() {
    const CELLS: usize = 6;
    let (incidence, mut match_eq, adj) = whole_array_incidence(CELLS);
    match_eq[3] = None;

    let blocks = build_blt_blocks(&incidence, &match_eq, &adj);

    assert_eq!(blocks.len(), CELLS);
    assert!(blocks.iter().all(|b| matches!(b, BltBlock::Scalar { .. })));
}

#[test]
fn compact_family_rows_are_emitted_before_the_remaining_tarjan_blocks() {
    // Family rows 0..4 are self-contained; rows 4 and 5 form a loop that
    // depends on nothing else. The compact block must come first, and the loop
    // must still be recognized after condensation.
    const CELLS: usize = 4;
    let mut eq_unknowns: Vec<HashSet<usize>> = (0..CELLS).map(|i| HashSet::from([i])).collect();
    eq_unknowns.push(HashSet::from([4, 5]));
    eq_unknowns.push(HashSet::from([4, 5]));
    let eq_refs: Vec<EquationRef> = (0..CELLS + 2).map(EquationRef).collect();
    let unknown_names: Vec<UnknownId> = (0..CELLS + 2)
        .map(|i| UnknownId::Variable(rumoca_core::VarName::from(format!("v{i}").as_str())))
        .collect();
    let mut incidence = make_incidence(CELLS + 2, CELLS + 2, eq_unknowns, eq_refs, unknown_names);
    incidence.structured_matching = vec![one_dim_family(CELLS)];
    let match_eq: Vec<Option<usize>> = (0..CELLS + 2).map(Some).collect();
    let mut adj = vec![Vec::new(); CELLS + 2];
    adj[4] = vec![5];
    adj[5] = vec![4];

    let blocks = build_blt_blocks(&incidence, &match_eq, &adj);

    assert_eq!(blocks.len(), 2);
    assert!(matches!(&blocks[0], BltBlock::StructuredScalar(..)));
    let loop_rows: Vec<usize> = blocks
        .iter()
        .filter_map(|block| match block {
            BltBlock::AlgebraicLoop { equations, .. } => Some(equations),
            _ => None,
        })
        .flat_map(|equations| equations.iter().map(|equation| equation.0))
        .collect();
    let mut loop_rows = loop_rows;
    loop_rows.sort_unstable();
    assert_eq!(
        loop_rows,
        vec![4, 5],
        "the loop must survive node condensation"
    );
}

/// A family of `CELLS` rows (0..CELLS) plus one equation outside it, with an
/// extra column wired into family row `interior_row`.
///
/// Row `i` of the family is matched to unknown `i`; the outside equation
/// `CELLS` is matched to unknown `CELLS`. `outside_column` is the additional
/// unknown the interior row references, and `outside_row_columns` is what the
/// outside equation references.
fn family_with_interior_edge(
    cells: usize,
    interior_row: usize,
    outside_columns: &[usize],
) -> (Incidence, Vec<Option<usize>>, Vec<Vec<usize>>) {
    let n = cells + 1;
    let mut sets: Vec<HashSet<usize>> = (0..n).map(|i| HashSet::from([i])).collect();
    sets[interior_row].insert(cells);
    sets[cells].extend(outside_columns.iter().copied());
    let eq_refs: Vec<EquationRef> = (0..n).map(EquationRef).collect();
    let unknown_names: Vec<UnknownId> = (0..n)
        .map(|i| UnknownId::Variable(rumoca_core::VarName::from(format!("v{i}").as_str())))
        .collect();
    let mut incidence = make_incidence(n, n, sets.clone(), eq_refs, unknown_names);
    incidence.structured_matching = vec![one_dim_family(cells)];
    let match_eq: Vec<Option<usize>> = (0..n).map(Some).collect();
    let match_var = match_eq.clone();
    let adj = crate::incidence::build_dependency_graph(&incidence.eq_unknowns, &match_var, n);
    (incidence, match_eq, adj)
}

/// The out-degree-zero proof must be established for EVERY row that gets
/// condensed, never extrapolated from the point-0 base rows.
///
/// Here interior row 3 and the outside equation 4 reference each other's
/// matched unknowns, which is a genuine algebraic loop. The base row (row 0)
/// still references only its own unknown, so a check that only inspects point 0
/// would condense the whole family and dissolve the loop into independent
/// scalar blocks — a silently wrong decomposition the solver cannot detect.
#[test]
fn algebraic_loop_between_an_interior_family_row_and_an_outside_equation_survives() {
    const CELLS: usize = 4;
    let (incidence, match_eq, adj) = family_with_interior_edge(CELLS, 3, &[3]);

    let blocks = build_blt_blocks(&incidence, &match_eq, &adj);

    assert!(
        !blocks
            .iter()
            .any(|block| matches!(block, BltBlock::StructuredScalar(..))),
        "a family with an out-edge may not be condensed, got {blocks:?}"
    );
    let loops: Vec<Vec<usize>> = blocks
        .iter()
        .filter_map(|block| match block {
            BltBlock::AlgebraicLoop { equations, .. } => {
                let mut rows: Vec<usize> = equations.iter().map(|eq| eq.0).collect();
                rows.sort_unstable();
                Some(rows)
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        loops,
        vec![vec![3, 4]],
        "the interior row and the outside equation form one SCC"
    );
    assert_eq!(
        blocks.len(),
        CELLS,
        "three singleton rows plus the 2x2 loop block"
    );
}

/// BLT order is a topological order, so a family row that depends on an outside
/// equation must never be scheduled before it.
///
/// Row 2 of the family reads unknown 4, which equation 4 determines. Condensing
/// the family and emitting the compact block ahead of every Tarjan block would
/// evaluate row 2 before its input exists.
#[test]
fn family_row_that_depends_on_an_outside_equation_is_ordered_after_it() {
    const CELLS: usize = 4;
    // Equation 4 depends on nothing but its own unknown, so it is a source.
    let (incidence, match_eq, adj) = family_with_interior_edge(CELLS, 2, &[]);

    let blocks = build_blt_blocks(&incidence, &match_eq, &adj);

    let position_of = |row: usize| {
        blocks
            .iter()
            .position(
                |block| matches!(block, BltBlock::Scalar { equation, .. } if equation.0 == row),
            )
            .expect("every row keeps its own scalar block once the family is refused")
    };
    assert!(
        position_of(4) < position_of(2),
        "the dependency must be evaluated first, got {blocks:?}"
    );
}

/// Two descriptors claiming the same rows may not both be condensed: the rows
/// would be emitted twice while leaving the Tarjan node set once.
#[test]
fn overlapping_family_descriptors_do_not_double_emit_their_rows() {
    const CELLS: usize = 4;
    let (mut incidence, match_eq, adj) = whole_array_incidence(CELLS);
    incidence
        .structured_matching
        .push(one_dim_family(CELLS - 1));

    let blocks = build_blt_blocks(&incidence, &match_eq, &adj);

    let covered: usize = blocks.iter().map(BltBlock::scalar_block_count).sum();
    assert_eq!(covered, CELLS, "every row is covered exactly once");
    assert_eq!(
        blocks.len(),
        1,
        "the first descriptor wins and the overlapping one is refused"
    );
}
