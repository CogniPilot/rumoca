use std::collections::BTreeSet;

use rumoca_ir_solve::{ComputeBlock, LinearOp, Reg, ScalarProgramBlock};

use crate::{EvalSolveError, to_scalar_program_block};

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
) -> Result<JacobianSparsity, EvalSolveError> {
    let scalar = to_scalar_program_block(block)?;
    jacobian_sparsity_from_scalar_jvp(&scalar, rows, cols)
}

pub fn jacobian_sparsity_from_scalar_jvp(
    block: &ScalarProgramBlock,
    rows: usize,
    cols: usize,
) -> Result<JacobianSparsity, EvalSolveError> {
    let mut nonzeros = Vec::new();
    for (row_idx, program) in block.programs.iter().take(rows).enumerate() {
        for seed_idx in row_seed_dependencies(program)? {
            if seed_idx < cols {
                nonzeros.push((row_idx, seed_idx));
            }
        }
    }
    Ok(JacobianSparsity {
        rows,
        cols,
        nonzeros,
    })
}

// SPEC_0021: Exception - exhaustive match over LinearOp variants keeps seed
// dependency propagation visibly complete when new Solve-IR ops are added.
#[allow(clippy::too_many_lines)]
pub fn row_seed_dependencies(program: &[LinearOp]) -> Result<Vec<usize>, EvalSolveError> {
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
            // A runtime-indexed parameter load carries the seed dependencies of
            // its index register (which select decided the slot), matching the
            // select-chain form it replaces.
            LinearOp::LoadIndexedP { dst, index, .. } => {
                let src_deps = reg_deps(&deps, index)?;
                set_reg_deps(&mut deps, dst, src_deps);
            }
            // A runtime-indexed seed load may resolve to any column in its run,
            // plus whatever its index register depends on.
            LinearOp::LoadIndexedSeed {
                dst,
                base,
                count,
                index,
            } => {
                let mut op_deps = reg_deps(&deps, index)?;
                op_deps.extend(base..checked_indexed_seed_end(base, count)?);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::Move { dst, src } => {
                let src_deps = reg_deps(&deps, src)?;
                set_reg_deps(&mut deps, dst, src_deps);
            }
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                ..
            } => {
                let matrix_len = checked_product(n, n, "linear solve matrix")?;
                let mut op_deps = reg_range_deps(&deps, matrix_start, matrix_len)?;
                op_deps.extend(reg_range_deps(&deps, rhs_start, n)?);
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::TableBounds { dst, table_id, .. } => {
                let op_deps = reg_deps(&deps, table_id)?;
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::TableLookup {
                dst,
                table_id,
                column,
                input,
            } => {
                let op_deps = union_regs(&deps, [table_id, column, input])?;
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::TableLookupSlope {
                dst,
                table_id,
                column,
                input,
            } => {
                let op_deps = union_regs(&deps, [table_id, column, input])?;
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::TableNextEvent {
                dst,
                table_id,
                time,
            } => {
                let op_deps = union_regs(&deps, [table_id, time])?;
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::RandomInitialState {
                dst,
                local_seed,
                global_seed,
                ..
            } => {
                let op_deps = union_regs(&deps, [local_seed, global_seed])?;
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
                let op_deps = reg_range_deps(&deps, state_start, state_len)?;
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::ImpureRandomInit { dst, seed } => {
                let op_deps = reg_deps(&deps, seed)?;
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::ImpureRandom { dst, id, .. } => {
                let op_deps = reg_deps(&deps, id)?;
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::ImpureRandomInteger {
                dst,
                id,
                imin,
                imax,
                ..
            } => {
                let op_deps = union_regs(&deps, [id, imin, imax])?;
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::ExternalCall {
                dst,
                args,
                arg_count,
                ..
            } => {
                let mut op_deps = BTreeSet::new();
                for arg in args.iter().copied().take(arg_count) {
                    op_deps.extend(reg_deps(&deps, arg)?);
                }
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::Unary { dst, arg, .. } => {
                let op_deps = reg_deps(&deps, arg)?;
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
                let op_deps = union_regs(&deps, [lhs, rhs])?;
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let op_deps = union_regs(&deps, [cond, if_true, if_false])?;
                set_reg_deps(&mut deps, dst, op_deps);
            }
            LinearOp::StoreOutput { src } => {
                output = reg_deps(&deps, src)?;
            }
        }
    }
    Ok(output.into_iter().collect())
}

fn set_reg_deps(deps: &mut Vec<BTreeSet<usize>>, reg: Reg, value: BTreeSet<usize>) {
    let idx = reg as usize;
    if deps.len() <= idx {
        deps.resize_with(idx + 1, BTreeSet::new);
    }
    deps[idx] = value;
}

fn reg_deps(deps: &[BTreeSet<usize>], reg: Reg) -> Result<BTreeSet<usize>, EvalSolveError> {
    deps.get(reg as usize)
        .cloned()
        .ok_or(EvalSolveError::UninitializedRegister {
            register: reg,
            span: None,
        })
}

fn reg_range_deps(
    deps: &[BTreeSet<usize>],
    start: Reg,
    len: usize,
) -> Result<BTreeSet<usize>, EvalSolveError> {
    let mut result = BTreeSet::new();
    for offset in 0..len {
        let reg = checked_reg_offset(start, offset)?;
        result.extend(reg_deps(deps, reg)?);
    }
    Ok(result)
}

fn checked_product(
    lhs: usize,
    rhs: usize,
    operation: &'static str,
) -> Result<usize, EvalSolveError> {
    lhs.checked_mul(rhs)
        .ok_or_else(|| EvalSolveError::Scalarization {
            message: format!("{operation} shape product {lhs} * {rhs} overflows register range"),
            span: None,
        })
}

fn checked_indexed_seed_end(base: usize, count: usize) -> Result<usize, EvalSolveError> {
    let width = if count == 0 { 1 } else { count };
    base.checked_add(width)
        .ok_or_else(|| EvalSolveError::Scalarization {
            message: format!("indexed seed range base {base} plus count {count} overflows"),
            span: None,
        })
}

fn checked_reg_offset(start: Reg, offset: usize) -> Result<Reg, EvalSolveError> {
    let offset = u32::try_from(offset).map_err(|_| EvalSolveError::Scalarization {
        message: format!("register range offset {offset} overflows Solve-IR register id"),
        span: None,
    })?;
    start
        .checked_add(offset)
        .ok_or_else(|| EvalSolveError::Scalarization {
            message: format!("register range start {start} plus offset {offset} overflows"),
            span: None,
        })
}

fn union_regs<const N: usize>(
    deps: &[BTreeSet<usize>],
    regs: [Reg; N],
) -> Result<BTreeSet<usize>, EvalSolveError> {
    let mut result = BTreeSet::new();
    for reg in regs {
        result.extend(reg_deps(deps, reg)?);
    }
    Ok(result)
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

        assert_eq!(
            row_seed_dependencies(&row).expect("valid row dependencies should resolve"),
            vec![2]
        );
    }

    #[test]
    fn row_seed_dependencies_reports_undefined_source_register() {
        let row = vec![LinearOp::Move { dst: 1, src: 7 }];

        let err = row_seed_dependencies(&row)
            .expect_err("undefined source register should be a sparsity error");

        assert!(matches!(
            err,
            EvalSolveError::UninitializedRegister {
                register: 7,
                span: None,
            }
        ));
    }

    #[test]
    fn row_seed_dependencies_reports_linear_solve_matrix_shape_overflow() {
        let row = vec![LinearOp::LinearSolveComponent {
            dst: 0,
            matrix_start: 0,
            rhs_start: 0,
            n: usize::MAX,
            component: 0,
        }];

        let err = row_seed_dependencies(&row)
            .expect_err("overflowing linear solve matrix shape should fail dependency analysis");

        assert!(
            err.to_string()
                .contains("linear solve matrix shape product"),
            "error should explain matrix shape overflow: {err}"
        );
    }

    #[test]
    fn jacobian_sparsity_skips_seed_independent_rows() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("jvp_sparsity.mo"),
            1,
            2,
        );
        let block = ScalarProgramBlock::with_source_span(
            vec![
                vec![
                    LinearOp::LoadSeed { dst: 0, index: 1 },
                    LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    LinearOp::Const { dst: 0, value: 0.0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
            ],
            span,
        );
        let compute = ComputeBlock::from_scalar_program_block(block);

        let sparsity =
            jacobian_sparsity_from_jvp(&compute, 2, 3).expect("valid JVP should scalarize");

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
