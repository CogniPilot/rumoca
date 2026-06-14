use std::cmp::Reverse;
use std::collections::{HashMap, HashSet};

use rumoca_ir_solve::{ComputeBlock, ComputeNode, LinearOp, Reg, SolveVisitor, SparsityPattern};

/// Greedy column coloring for a Jacobian sparsity pattern.
///
/// `column_rows[c]` contains row indices where column `c` may be nonzero.
/// Returns color groups over compact column indices.
pub fn greedy_column_coloring(column_rows: &[Vec<usize>]) -> Vec<Vec<usize>> {
    if column_rows.is_empty() {
        return Vec::new();
    }

    let mut order: Vec<usize> = (0..column_rows.len()).collect();
    order.sort_by_key(|&col| Reverse(column_rows[col].len()));

    let mut colors: Vec<Vec<usize>> = Vec::new();
    let mut color_row_sets: Vec<HashSet<usize>> = Vec::new();

    for col in order {
        let rows = &column_rows[col];
        let mut placed = false;

        for (color_idx, row_set) in color_row_sets.iter_mut().enumerate() {
            if rows.iter().all(|row| !row_set.contains(row)) {
                colors[color_idx].push(col);
                row_set.extend(rows.iter().copied());
                placed = true;
                break;
            }
        }

        if !placed {
            let mut row_set = HashSet::with_capacity(rows.len());
            row_set.extend(rows.iter().copied());
            colors.push(vec![col]);
            color_row_sets.push(row_set);
        }
    }

    for color in &mut colors {
        color.sort_unstable();
    }
    colors.sort_by_key(|color| color.first().copied().unwrap_or(usize::MAX));
    colors
}

/// Forward def-use pass over a flat op list.
///
/// Returns a map `reg → sorted seed indices` representing the set of
/// `LoadSeed` indices that may transitively contribute to each register's value.
/// Registers that hold no seed dependency (constants, y-loads, p-loads) map to an
/// empty `Vec`.  Registers written by ops whose operands are not tracked
/// (table lookups, random generators) are treated conservatively as
/// "depends on all seeds up to `n_seeds`" — that is, they are omitted from the
/// map, so callers that see `None` should treat the register as opaque.
pub fn seeds_affecting_regs(ops: &[LinearOp]) -> HashMap<Reg, Vec<usize>> {
    let mut reg_seeds: HashMap<Reg, Vec<usize>> = HashMap::with_capacity(ops.len());

    let merge = |a: &[usize], b: &[usize]| -> Vec<usize> {
        let mut out = a.to_vec();
        out.extend_from_slice(b);
        out.sort_unstable();
        out.dedup();
        out
    };

    for &op in ops {
        match op {
            LinearOp::LoadSeed { dst, index } => {
                reg_seeds.insert(dst, vec![index]);
            }
            LinearOp::Const { dst, .. }
            | LinearOp::LoadTime { dst }
            | LinearOp::LoadY { dst, .. }
            | LinearOp::LoadP { dst, .. } => {
                reg_seeds.insert(dst, vec![]);
            }
            LinearOp::Move { dst, src } => {
                let seeds = reg_seeds.get(&src).cloned().unwrap_or_default();
                reg_seeds.insert(dst, seeds);
            }
            LinearOp::Unary { dst, arg, .. } => {
                let seeds = reg_seeds.get(&arg).cloned().unwrap_or_default();
                reg_seeds.insert(dst, seeds);
            }
            LinearOp::Binary { dst, lhs, rhs, .. } => {
                let a = reg_seeds.get(&lhs).cloned().unwrap_or_default();
                let b = reg_seeds.get(&rhs).cloned().unwrap_or_default();
                reg_seeds.insert(dst, merge(&a, &b));
            }
            LinearOp::Compare { dst, lhs, rhs, .. } => {
                let a = reg_seeds.get(&lhs).cloned().unwrap_or_default();
                let b = reg_seeds.get(&rhs).cloned().unwrap_or_default();
                reg_seeds.insert(dst, merge(&a, &b));
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let a = reg_seeds.get(&cond).cloned().unwrap_or_default();
                let b = reg_seeds.get(&if_true).cloned().unwrap_or_default();
                let c = reg_seeds.get(&if_false).cloned().unwrap_or_default();
                reg_seeds.insert(dst, merge(&merge(&a, &b), &c));
            }
            LinearOp::StoreOutput { .. } => {
                // Marks final row value; no register written.
            }
            LinearOp::LinearSolveComponent { dst, .. }
            | LinearOp::TableBounds { dst, .. }
            | LinearOp::TableLookup { dst, .. }
            | LinearOp::TableLookupSlope { dst, .. }
            | LinearOp::TableNextEvent { dst, .. }
            | LinearOp::RandomInitialState { dst, .. }
            | LinearOp::RandomResult { dst, .. }
            | LinearOp::RandomState { dst, .. }
            | LinearOp::ImpureRandomInit { dst, .. }
            | LinearOp::ImpureRandom { dst, .. }
            | LinearOp::ImpureRandomInteger { dst, .. }
            | LinearOp::ExternalCall { dst, .. } => {
                // Conservative: we don't track through these; leave dst absent from
                // the map so callers treat it as "unknown / all seeds".
                let _ = dst;
            }
        }
    }

    reg_seeds
}

fn sorted_unique_seeds_from_regs(reg_seeds: &HashMap<Reg, Vec<usize>>) -> Vec<usize> {
    let mut seeds: Vec<usize> = reg_seeds.values().flat_map(|s| s.iter().copied()).collect();
    seeds.sort_unstable();
    seeds.dedup();
    seeds
}

fn push_row_for_seeds(
    column_rows: &mut [Vec<usize>],
    seeds: impl IntoIterator<Item = usize>,
    out_row: usize,
) {
    for seed in seeds {
        if let Some(rows) = column_rows.get_mut(seed) {
            rows.push(out_row);
        }
    }
}

fn push_matmul_jac_sparsity(
    column_rows: &mut [Vec<usize>],
    rhs_ops: &[LinearOp],
    rhs_start: Reg,
    lhs_sparsity: &SparsityPattern,
    dims: (usize, usize, usize),
    row_offset: usize,
) {
    let (m, k, n) = dims;
    let rhs_reg_seeds = seeds_affecting_regs(rhs_ops);

    // Diagonal square A times a column vector (n == 1, m == k):
    // output[i] = A[i,i] * x[i], so only seeds affecting rhs[i] matter.
    let is_diagonal_matvec = matches!(lhs_sparsity, SparsityPattern::Diagonal) && n == 1 && m == k;
    let all_rhs_seeds = sorted_unique_seeds_from_regs(&rhs_reg_seeds);

    for slot in 0..m * n {
        let i = slot / n;
        let out_row = row_offset + slot;
        let seeds = if is_diagonal_matvec {
            rhs_reg_seeds
                .get(&(rhs_start + i as u32))
                .cloned()
                .unwrap_or_else(|| (0..column_rows.len()).collect())
        } else {
            all_rhs_seeds.clone()
        };
        push_row_for_seeds(column_rows, seeds, out_row);
    }
}

/// Structural sparsity of the Jacobian of a `ComputeBlock` JVP.
///
/// The JVP block computes `J · v` where `J[r][c] = d(residual[r])/d(y[c])`.
/// A JVP row `r` is nonzero in column `c` iff `LoadSeed { index: c }` appears
/// (transitively) in the ops for output row `r`.
///
/// Returns `column_rows[seed_index]` = sorted list of output row indices that may
/// depend on that seed.  Length of the outer vec is `n_seeds`.
pub fn compute_block_jac_sparsity(block: &ComputeBlock, n_seeds: usize) -> Vec<Vec<usize>> {
    let mut visitor = JacSparsityVisitor {
        column_rows: vec![Vec::new(); n_seeds],
        row_offset: 0,
    };
    let result = visitor.visit_compute_block(block);
    if let Err(error) = result {
        match error {}
    }

    for rows in &mut visitor.column_rows {
        rows.sort_unstable();
        rows.dedup();
    }
    visitor.column_rows
}

struct JacSparsityVisitor {
    column_rows: Vec<Vec<usize>>,
    row_offset: usize,
}

impl SolveVisitor for JacSparsityVisitor {
    type Error = std::convert::Infallible;

    fn visit_compute_node(
        &mut self,
        _node_index: usize,
        node: &ComputeNode,
    ) -> Result<(), Self::Error> {
        match node {
            ComputeNode::ScalarPrograms(rb) => {
                for (r, row_ops) in rb.programs.iter().enumerate() {
                    let out_row = self.row_offset + r;
                    let reg_seeds = seeds_affecting_regs(row_ops);
                    let row_seeds = sorted_unique_seeds_from_regs(&reg_seeds);
                    push_row_for_seeds(&mut self.column_rows, row_seeds, out_row);
                }
                self.row_offset += rb.programs.len();
            }

            ComputeNode::MatMul {
                rhs_ops,
                rhs_start,
                lhs_sparsity,
                m,
                k,
                n,
                ..
            } => {
                push_matmul_jac_sparsity(
                    &mut self.column_rows,
                    rhs_ops,
                    *rhs_start,
                    lhs_sparsity,
                    (*m, *k, *n),
                    self.row_offset,
                );
                self.row_offset += m * n;
            }

            ComputeNode::LinSolve { setup_ops, n, .. } => {
                // Conservative: any seed in setup_ops may affect any output row.
                let reg_seeds = seeds_affecting_regs(setup_ops);
                let all_seeds = sorted_unique_seeds_from_regs(&reg_seeds);
                for out_row in self.row_offset..self.row_offset + n {
                    push_row_for_seeds(&mut self.column_rows, all_seeds.iter().copied(), out_row);
                }
                self.row_offset += n;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_color_validity(column_rows: &[Vec<usize>], colors: &[Vec<usize>]) {
        let mut seen = vec![false; column_rows.len()];
        for color in colors {
            let mut rows = HashSet::new();
            for &col in color {
                assert!(!seen[col], "column {col} assigned more than once");
                seen[col] = true;
                assert_color_rows_disjoint(&mut rows, &column_rows[col]);
            }
        }
        assert!(seen.iter().all(|v| *v), "some columns were not colored");
    }

    fn assert_color_rows_disjoint(rows: &mut HashSet<usize>, column_rows: &[usize]) {
        for &row in column_rows {
            assert!(
                rows.insert(row),
                "row {row} appears in two columns of one color"
            );
        }
    }

    #[test]
    fn test_greedy_column_coloring_single_color_for_disjoint_columns() {
        let column_rows = vec![vec![0], vec![1], vec![2], vec![3]];
        let colors = greedy_column_coloring(&column_rows);
        assert_eq!(colors.len(), 1);
        assert_color_validity(&column_rows, &colors);
    }

    #[test]
    fn test_greedy_column_coloring_splits_conflicting_columns() {
        let column_rows = vec![vec![0, 1], vec![1, 2], vec![2, 3], vec![4]];
        let colors = greedy_column_coloring(&column_rows);
        assert!(colors.len() >= 2);
        assert_color_validity(&column_rows, &colors);
    }

    // Tests moved from rumoca-ir-solve::sparsity
    use rumoca_ir_solve::{
        BinaryOp, ComputeBlock, ComputeNode, LinearOp, ScalarProgramBlock, SparsityPattern,
    };

    fn make_scalar_block(rows: Vec<Vec<LinearOp>>) -> ComputeBlock {
        ComputeBlock {
            nodes: vec![ComputeNode::ScalarPrograms(ScalarProgramBlock::new(rows))],
        }
    }

    #[test]
    fn test_scalar_programs_exact_sparsity() {
        let block = make_scalar_block(vec![
            vec![
                LinearOp::LoadSeed { dst: 0, index: 0 },
                LinearOp::LoadY { dst: 1, index: 3 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
            ],
            vec![
                LinearOp::LoadSeed { dst: 0, index: 1 },
                LinearOp::Const { dst: 1, value: 2.0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
            ],
            vec![LinearOp::Const { dst: 0, value: 1.0 }],
        ]);

        let col_rows = compute_block_jac_sparsity(&block, 2);
        assert_eq!(col_rows[0], vec![0], "seed 0 affects row 0");
        assert_eq!(col_rows[1], vec![1], "seed 1 affects row 1");
    }

    #[test]
    fn test_diagonal_matvec_sparsity() {
        let block = ComputeBlock {
            nodes: vec![ComputeNode::MatMul {
                lhs_ops: vec![
                    LinearOp::Const { dst: 0, value: 3.0 },
                    LinearOp::Const { dst: 1, value: 0.0 },
                    LinearOp::Const { dst: 2, value: 0.0 },
                    LinearOp::Const { dst: 3, value: 5.0 },
                    LinearOp::Move { dst: 4, src: 0 },
                    LinearOp::Move { dst: 5, src: 1 },
                    LinearOp::Move { dst: 6, src: 2 },
                    LinearOp::Move { dst: 7, src: 3 },
                ],
                lhs_start: 4,
                rhs_ops: vec![
                    LinearOp::LoadSeed { dst: 8, index: 0 },
                    LinearOp::LoadSeed { dst: 9, index: 1 },
                    LinearOp::Move { dst: 10, src: 8 },
                    LinearOp::Move { dst: 11, src: 9 },
                ],
                rhs_start: 10,
                m: 2,
                k: 2,
                n: 1,
                lhs_sparsity: SparsityPattern::Diagonal,
                rhs_sparsity: SparsityPattern::Dense,
                metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        };

        let col_rows = compute_block_jac_sparsity(&block, 2);
        assert_eq!(
            col_rows[0],
            vec![0],
            "seed 0 (x[0]) only affects output row 0"
        );
        assert_eq!(
            col_rows[1],
            vec![1],
            "seed 1 (x[1]) only affects output row 1"
        );
    }

    #[test]
    fn test_dense_matmul_sparsity_conservative() {
        let block = ComputeBlock {
            nodes: vec![ComputeNode::MatMul {
                lhs_ops: vec![
                    LinearOp::LoadP { dst: 0, index: 0 },
                    LinearOp::LoadP { dst: 1, index: 1 },
                    LinearOp::LoadP { dst: 2, index: 2 },
                    LinearOp::LoadP { dst: 3, index: 3 },
                    LinearOp::Move { dst: 4, src: 0 },
                    LinearOp::Move { dst: 5, src: 1 },
                    LinearOp::Move { dst: 6, src: 2 },
                    LinearOp::Move { dst: 7, src: 3 },
                ],
                lhs_start: 4,
                rhs_ops: vec![
                    LinearOp::LoadSeed { dst: 8, index: 0 },
                    LinearOp::LoadSeed { dst: 9, index: 1 },
                    LinearOp::Move { dst: 10, src: 8 },
                    LinearOp::Move { dst: 11, src: 9 },
                ],
                rhs_start: 10,
                m: 2,
                k: 2,
                n: 1,
                lhs_sparsity: SparsityPattern::Dense,
                rhs_sparsity: SparsityPattern::Dense,
                metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        };

        let col_rows = compute_block_jac_sparsity(&block, 2);
        assert_eq!(
            col_rows[0],
            vec![0, 1],
            "dense: seed 0 affects both output rows"
        );
        assert_eq!(
            col_rows[1],
            vec![0, 1],
            "dense: seed 1 affects both output rows"
        );
    }
}
