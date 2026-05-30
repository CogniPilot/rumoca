use std::collections::BTreeSet;

use rumoca_ir_solve::{ComputeBlock, LinearOp, Reg, ScalarProgramBlock};

use crate::to_scalar_program_block;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JacobianSparsity {
    pub rows: usize,
    pub cols: usize,
    pub nonzeros: Vec<(usize, usize)>,
}

pub fn jacobian_sparsity_from_jvp(
    block: &ComputeBlock,
    rows: usize,
    cols: usize,
) -> JacobianSparsity {
    let scalar = to_scalar_program_block(block);
    jacobian_sparsity_from_scalar_jvp(&scalar, rows, cols)
}

pub fn jacobian_sparsity_from_scalar_jvp(
    block: &ScalarProgramBlock,
    rows: usize,
    cols: usize,
) -> JacobianSparsity {
    let mut nonzeros = Vec::new();
    for (row_idx, program) in block.programs.iter().take(rows).enumerate() {
        for seed_idx in row_seed_dependencies(program) {
            if seed_idx < cols {
                nonzeros.push((row_idx, seed_idx));
            }
        }
    }
    JacobianSparsity {
        rows,
        cols,
        nonzeros,
    }
}

// SPEC_0021: Exception - exhaustive match over LinearOp variants keeps seed
// dependency propagation visibly complete when new Solve-IR ops are added.
#[allow(clippy::too_many_lines)]
pub fn row_seed_dependencies(program: &[LinearOp]) -> Vec<usize> {
    let mut deps: Vec<BTreeSet<usize>> = Vec::new();
    let mut output = BTreeSet::new();
    for op in program {
        match *op {
            LinearOp::Const { dst, .. }
            | LinearOp::LoadTime { dst }
            | LinearOp::LoadY { dst, .. }
            | LinearOp::LoadP { dst, .. } => {
                set_reg_deps(&mut deps, dst, BTreeSet::new());
            }
            LinearOp::LoadSeed { dst, index } => {
                set_reg_deps(&mut deps, dst, BTreeSet::from([index]));
            }
            LinearOp::Move { dst, src } => {
                let src_deps = reg_deps(&deps, src);
                set_reg_deps(&mut deps, dst, src_deps);
            }
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                ..
            } => {
                let mut op_deps = reg_range_deps(&deps, matrix_start, n.saturating_mul(n));
                op_deps.extend(reg_range_deps(&deps, rhs_start, n));
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::TableBounds { dst, table_id, .. } => {
                let op_deps = reg_deps(&deps, table_id);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::TableLookup {
                dst,
                table_id,
                column,
                input,
            } => {
                let op_deps = union_regs(&deps, [table_id, column, input]);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::TableLookupSlope {
                dst,
                table_id,
                column,
                input,
            } => {
                let op_deps = union_regs(&deps, [table_id, column, input]);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::TableNextEvent {
                dst,
                table_id,
                time,
            } => {
                let op_deps = union_regs(&deps, [table_id, time]);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::RandomInitialState {
                dst,
                local_seed,
                global_seed,
                ..
            } => {
                let op_deps = union_regs(&deps, [local_seed, global_seed]);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::RandomResult {
                dst,
                state_start,
                state_len,
                ..
            }
            | LinearOp::RandomState {
                dst,
                state_start,
                state_len,
                ..
            } => {
                let op_deps = reg_range_deps(&deps, state_start, state_len);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::ImpureRandomInit { dst, seed } => {
                let op_deps = reg_deps(&deps, seed);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::ImpureRandom { dst, id, .. } => {
                let op_deps = reg_deps(&deps, id);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::ImpureRandomInteger {
                dst,
                id,
                imin,
                imax,
                ..
            } => {
                let op_deps = union_regs(&deps, [id, imin, imax]);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::Unary { dst, arg, .. } => {
                let op_deps = reg_deps(&deps, arg);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
                let op_deps = union_regs(&deps, [lhs, rhs]);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let op_deps = union_regs(&deps, [cond, if_true, if_false]);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::StoreOutput { src } => {
                output = reg_deps(&deps, src);
            }
        }
    }
    output.into_iter().collect()
}

fn set_reg_deps(deps: &mut Vec<BTreeSet<usize>>, reg: Reg, value: BTreeSet<usize>) {
    let idx = reg as usize;
    if deps.len() <= idx {
        deps.resize_with(idx + 1, BTreeSet::new);
    }
    deps[idx] = value;
}

fn reg_deps(deps: &[BTreeSet<usize>], reg: Reg) -> BTreeSet<usize> {
    deps.get(reg as usize)
        .cloned()
        .expect("solve sparsity register must be defined before use")
}

fn reg_range_deps(deps: &[BTreeSet<usize>], start: Reg, len: usize) -> BTreeSet<usize> {
    (0..len)
        .flat_map(|offset| reg_deps(deps, start.saturating_add(offset as Reg)))
        .collect()
}

fn union_regs<const N: usize>(deps: &[BTreeSet<usize>], regs: [Reg; N]) -> BTreeSet<usize> {
    regs.into_iter()
        .flat_map(|reg| reg_deps(deps, reg))
        .collect()
}

#[cfg(test)]
mod tests {
    use rumoca_ir_solve::{BinaryOp, ComputeBlock, ScalarProgramBlock};

    use super::*;

    #[test]
    fn row_seed_dependencies_track_arithmetic_flow() {
        let row = vec![
            LinearOp::LoadSeed { dst: 0, index: 2 },
            LinearOp::Const { dst: 1, value: 4.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ];

        assert_eq!(row_seed_dependencies(&row), vec![2]);
    }

    #[test]
    fn jacobian_sparsity_skips_seed_independent_rows() {
        let block = ScalarProgramBlock::new(vec![
            vec![
                LinearOp::LoadSeed { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ]);
        let compute = ComputeBlock::from_scalar_program_block(block);

        let sparsity = jacobian_sparsity_from_jvp(&compute, 2, 3);

        assert_eq!(
            sparsity,
            JacobianSparsity {
                rows: 2,
                cols: 3,
                nonzeros: vec![(0, 1)],
            }
        );
    }
}
