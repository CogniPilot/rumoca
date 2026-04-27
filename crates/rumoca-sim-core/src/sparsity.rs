use std::cmp::Reverse;
use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;
use rumoca_phase_structural::build_solver_sparsity_triplets;

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

#[derive(Debug, Clone)]
pub struct SparsityValidation {
    pub structural_nnz: usize,
    pub runtime_nnz: usize,
    pub missing_count: usize,
    pub extra_count: usize,
    pub missing_samples: Vec<(usize, usize)>,
    pub extra_samples: Vec<(usize, usize)>,
}

impl SparsityValidation {
    pub fn has_mismatch(&self) -> bool {
        self.missing_count > 0 || self.extra_count > 0
    }
}

fn runtime_triplets(active_cols: &[usize], column_rows: &[Vec<usize>]) -> HashSet<(usize, usize)> {
    let mut triplets = HashSet::new();
    for (compact_col, rows) in column_rows.iter().enumerate() {
        let Some(&global_col) = active_cols.get(compact_col) else {
            continue;
        };
        for &row in rows {
            triplets.insert((row, global_col));
        }
    }
    triplets
}

fn sorted_sample(
    triplets: impl IntoIterator<Item = (usize, usize)>,
    limit: usize,
) -> Vec<(usize, usize)> {
    let mut items: Vec<(usize, usize)> = triplets.into_iter().collect();
    items.sort_unstable();
    items.truncate(limit);
    items
}

/// Compare structural sparsity from phase-structural against runtime NaN-detected
/// sparsity for the currently active Jacobian columns.
pub fn validate_solver_sparsity(
    dae: &dae::Dae,
    active_cols: &[usize],
    column_rows: &[Vec<usize>],
    sample_limit: usize,
) -> SparsityValidation {
    let active_col_set: HashSet<usize> = active_cols.iter().copied().collect();
    let structural: HashSet<(usize, usize)> = build_solver_sparsity_triplets(dae)
        .into_iter()
        .filter(|(_, col)| active_col_set.contains(col))
        .collect();
    let runtime = runtime_triplets(active_cols, column_rows);

    let missing: Vec<(usize, usize)> = structural.difference(&runtime).copied().collect();
    let extra: Vec<(usize, usize)> = runtime.difference(&structural).copied().collect();

    SparsityValidation {
        structural_nnz: structural.len(),
        runtime_nnz: runtime.len(),
        missing_count: missing.len(),
        extra_count: extra.len(),
        missing_samples: sorted_sample(missing, sample_limit),
        extra_samples: sorted_sample(extra, sample_limit),
    }
}

/// Build Jacobian column sparsity rows from structural triplets for active columns.
///
/// The return shape matches `greedy_column_coloring` input: each entry corresponds
/// to one compact column index in `active_cols`.
pub fn structural_column_sparsity(
    dae: &dae::Dae,
    active_cols: &[usize],
    n_rows: usize,
) -> Vec<Vec<usize>> {
    let mut compact_by_global = HashMap::with_capacity(active_cols.len());
    for (compact, &global_col) in active_cols.iter().enumerate() {
        compact_by_global.insert(global_col, compact);
    }

    let mut column_rows = vec![Vec::new(); active_cols.len()];
    for (row, global_col) in build_solver_sparsity_triplets(dae) {
        if row >= n_rows {
            continue;
        }
        let Some(&compact_col) = compact_by_global.get(&global_col) else {
            continue;
        };
        column_rows[compact_col].push(row);
    }

    for rows in &mut column_rows {
        rows.sort_unstable();
        rows.dedup();
    }
    column_rows
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;
    use rumoca_ir_dae as dae;
    type BuiltinFunction = dae::BuiltinFunction;
    type Literal = dae::Literal;
    type OpBinary = rumoca_ir_core::OpBinary;
    type VarName = dae::VarName;

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

    fn var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: VarName::new(name),
            subscripts: vec![],
        }
    }

    fn lit(v: f64) -> dae::Expression {
        dae::Expression::Literal(Literal::Real(v))
    }

    fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn add(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: OpBinary::Add(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn eq(rhs: dae::Expression) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs,
            span: Span::DUMMY,
            origin: String::new(),
            scalar_count: 1,
        }
    }

    fn sample_dae() -> dae::Dae {
        let mut dae = dae::Dae::new();
        dae.states
            .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
        dae.algebraics
            .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));
        dae.f_x.push(eq(sub(
            dae::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var("x")],
            },
            var("z"),
        )));
        dae.f_x.push(eq(sub(var("z"), add(var("x"), lit(1.0)))));
        dae
    }

    #[test]
    fn test_validate_solver_sparsity_detects_missing_and_extra_triplets() {
        let dae = sample_dae();
        // runtime rows per compact column; active global columns are [0, 1]
        // col 0 has row 1, col 1 has rows 0 and 1
        let runtime = vec![vec![1], vec![0, 1]];
        let report = validate_solver_sparsity(&dae, &[0, 1], &runtime, 8);
        assert!(!report.has_mismatch());

        let runtime_missing = vec![vec![1], vec![0]];
        let report = validate_solver_sparsity(&dae, &[0, 1], &runtime_missing, 8);
        assert!(report.has_mismatch());
        assert_eq!(report.missing_count, 1);
        assert!(report.missing_samples.contains(&(1, 1)));
    }

    #[test]
    fn test_structural_column_sparsity_maps_active_columns() {
        let dae = sample_dae();
        let rows = structural_column_sparsity(&dae, &[0, 1], 2);
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0], vec![1]);
        assert_eq!(rows[1], vec![0, 1]);
    }
}
