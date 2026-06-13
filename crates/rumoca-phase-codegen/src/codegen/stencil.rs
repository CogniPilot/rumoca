//! Affine row-family detection over Solve-IR scalar programs.
//!
//! Discretized-PDE models (method of lines) lower to long runs of
//! structurally identical scalar rows whose `LoadY`/`LoadP` indices are
//! affine in the row number — a stencil. Recognizing those families lets
//! GPU backends emit one parametric kernel per family instead of one
//! switch case per row, which collapses both shader size and pipeline
//! compile time. This is format-equivalent compression of the scalar
//! fallback rows: every family expands back to exactly the original rows
//! (see `expand_row`), and rows that do not fit stay scalar.
//!
//! Promotion of these families to a first-class Solve-IR tensor node (and
//! range-preserving flattening so they exist without ever scalarizing) is
//! tracked in issue #238; this module is the recognition stepping stone.

use rumoca_ir_solve as solve;
use solve::LinearOp;

/// A run of `count` consecutive rows `start_row..start_row + count` whose
/// ops equal `base_ops` except that the load at op position `p` uses index
/// `base_index + r * stride` for row offset `r` (see `load_strides`).
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AffineRowFamily {
    pub(crate) start_row: usize,
    pub(crate) count: usize,
    pub(crate) base_ops: Vec<LinearOp>,
    /// (op position in `base_ops`, per-row index stride) for every
    /// `LoadY`/`LoadP` op. Stride 0 means the index is constant.
    pub(crate) load_strides: Vec<(usize, usize)>,
}

impl AffineRowFamily {
    /// Reconstruct the scalar row at offset `r` (0-based within the family).
    /// This is the family's correctness contract: tests verify every row of
    /// a detected family expands back to the original scalar row.
    #[cfg(test)]
    pub(crate) fn expand_row(&self, r: usize) -> Vec<LinearOp> {
        let mut ops = self.base_ops.clone();
        for &(position, stride) in &self.load_strides {
            match &mut ops[position] {
                LinearOp::LoadY { index, .. } | LinearOp::LoadP { index, .. } => {
                    *index += r * stride;
                }
                _ => unreachable!("load_strides only points at load ops"),
            }
        }
        ops
    }
}

/// Detection result: families plus the rows left scalar.
#[derive(Debug, Default)]
pub(crate) struct StencilPartition {
    pub(crate) families: Vec<AffineRowFamily>,
    pub(crate) residual_rows: Vec<usize>,
}

/// Minimum family size worth a dedicated kernel; smaller runs stay scalar.
const MIN_FAMILY_ROWS: usize = 8;

/// Partition rows into affine families and residual scalar rows.
pub(crate) fn partition_rows(rows: &[Vec<LinearOp>]) -> StencilPartition {
    let mut partition = StencilPartition::default();
    let mut row = 0;
    while row < rows.len() {
        let run = affine_run_length(rows, row);
        if run >= MIN_FAMILY_ROWS {
            let load_strides = load_strides_for_run(rows, row);
            partition.families.push(AffineRowFamily {
                start_row: row,
                count: run,
                base_ops: rows[row].clone(),
                load_strides,
            });
            row += run;
        } else {
            partition.residual_rows.push(row);
            row += 1;
        }
    }
    partition
}

/// Longest run starting at `start` where every row matches row `start`'s
/// skeleton and all load indices advance affinely (constant per-position
/// stride established between the first two rows).
fn affine_run_length(rows: &[Vec<LinearOp>], start: usize) -> usize {
    let base = &rows[start];
    let mut run = 1;
    let mut strides: Option<Vec<usize>> = None;
    while start + run < rows.len() {
        let candidate = &rows[start + run];
        let Some(deltas) = affine_deltas(base, candidate, run) else {
            break;
        };
        match &strides {
            None => strides = Some(deltas),
            Some(expected) if *expected == deltas => {}
            Some(_) => break,
        }
        run += 1;
    }
    if strides.is_some() { run } else { 1 }
}

/// If `candidate` equals `base` except for load indices that satisfy
/// `candidate.index = base.index + r * stride` with non-negative integer
/// strides, return the per-load-position stride list; else None.
fn affine_deltas(base: &[LinearOp], candidate: &[LinearOp], r: usize) -> Option<Vec<usize>> {
    if base.len() != candidate.len() {
        return None;
    }
    let mut strides = Vec::new();
    for (base_op, cand_op) in base.iter().zip(candidate) {
        match (base_op, cand_op) {
            (LinearOp::LoadY { dst: bd, index: bi }, LinearOp::LoadY { dst: cd, index: ci })
            | (LinearOp::LoadP { dst: bd, index: bi }, LinearOp::LoadP { dst: cd, index: ci }) => {
                if bd != cd || ci < bi {
                    return None;
                }
                let delta = ci - bi;
                if delta % r != 0 {
                    return None;
                }
                strides.push(delta / r);
            }
            (a, b) if a == b => {}
            _ => return None,
        }
    }
    Some(strides)
}

/// Per-load-position strides for an established run (base row vs row 1).
fn load_strides_for_run(rows: &[Vec<LinearOp>], start: usize) -> Vec<(usize, usize)> {
    let base = &rows[start];
    let next = &rows[start + 1];
    let mut result = Vec::new();
    for (position, (base_op, next_op)) in base.iter().zip(next).enumerate() {
        match (base_op, next_op) {
            (LinearOp::LoadY { index: bi, .. }, LinearOp::LoadY { index: ni, .. })
            | (LinearOp::LoadP { index: bi, .. }, LinearOp::LoadP { index: ni, .. }) => {
                result.push((position, ni - bi));
            }
            _ => {}
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use solve::{BinaryOp, UnaryOp};

    fn decay_row(y: usize, p: usize) -> Vec<LinearOp> {
        vec![
            LinearOp::LoadP { dst: 0, index: p },
            LinearOp::LoadY { dst: 1, index: y },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::Unary {
                dst: 3,
                op: UnaryOp::Neg,
                arg: 2,
            },
            LinearOp::StoreOutput { src: 3 },
        ]
    }

    #[test]
    fn detects_affine_family_and_expands_identically() {
        // 10 rows: y index strides by 1, p constant.
        let rows: Vec<_> = (0..10).map(|r| decay_row(r, 0)).collect();
        let partition = partition_rows(&rows);
        assert_eq!(partition.families.len(), 1);
        assert!(partition.residual_rows.is_empty());
        let family = &partition.families[0];
        assert_eq!(family.start_row, 0);
        assert_eq!(family.count, 10);
        assert_eq!(family.load_strides, vec![(0, 0), (1, 1)]);
        for (r, row) in rows.iter().enumerate() {
            assert_eq!(&family.expand_row(r), row, "row {r} must round-trip");
        }
    }

    #[test]
    fn short_runs_and_mismatched_rows_stay_scalar() {
        // 4 affine rows (below the threshold) + 1 structurally different row.
        let mut rows: Vec<_> = (0..4).map(|r| decay_row(r, 0)).collect();
        rows.push(vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]);
        let partition = partition_rows(&rows);
        assert!(partition.families.is_empty());
        assert_eq!(partition.residual_rows, vec![0, 1, 2, 3, 4]);
    }

    #[test]
    fn stride_changes_split_families() {
        // 8 rows stride 1, then 8 rows stride 2: two families.
        let mut rows: Vec<_> = (0..8).map(|r| decay_row(r, 0)).collect();
        rows.extend((0..8).map(|r| decay_row(100 + 2 * r, 0)));
        let partition = partition_rows(&rows);
        assert_eq!(partition.families.len(), 2);
        assert_eq!(partition.families[0].count, 8);
        assert_eq!(partition.families[1].count, 8);
        assert_eq!(partition.families[1].load_strides, vec![(0, 0), (1, 2)]);
    }

    #[test]
    fn constant_value_differences_break_the_skeleton() {
        let mut rows: Vec<_> = (0..8).map(|r| decay_row(r, 0)).collect();
        // A row whose constant differs must not join a family.
        rows.insert(
            4,
            vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        );
        let partition = partition_rows(&rows);
        // Split into two short runs (4 each) around the alien row.
        assert!(partition.families.is_empty());
        assert_eq!(partition.residual_rows.len(), 9);
    }
}
