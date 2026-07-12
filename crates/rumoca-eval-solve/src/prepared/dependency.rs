use std::collections::HashMap;

use rumoca_ir_solve::LinearOp;

use super::producer;

pub(super) fn reg_depends_on_y_index(row: &[LinearOp], reg: u32, target_y_index: usize) -> bool {
    // Register programs form a DAG: a register computed once can feed many
    // downstream ops, so memoize dependence on a fixed `y` index by register.
    let mut memo: HashMap<u32, bool> = HashMap::new();
    reg_depends_on_y_index_memo(row, reg, target_y_index, &mut memo)
}

fn reg_depends_on_y_index_memo(
    row: &[LinearOp],
    reg: u32,
    target_y_index: usize,
    memo: &mut HashMap<u32, bool>,
) -> bool {
    if let Some(&cached) = memo.get(&reg) {
        return cached;
    }
    // Guard against accidental cycles (register programs are acyclic in
    // practice): seed `false` before recursing so a back-edge terminates.
    memo.insert(reg, false);
    let result = producer(row, reg).is_some_and(|op| match *op {
        LinearOp::LoadY { index, .. } => index == target_y_index,
        LinearOp::Move { src, .. }
        | LinearOp::Unary { arg: src, .. }
        | LinearOp::LoadIndexedP { index: src, .. }
        | LinearOp::LoadIndexedSeed { index: src, .. } => {
            reg_depends_on_y_index_memo(row, src, target_y_index, memo)
        }
        LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
            reg_depends_on_y_index_memo(row, lhs, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, rhs, target_y_index, memo)
        }
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => {
            reg_depends_on_y_index_memo(row, cond, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, if_true, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, if_false, target_y_index, memo)
        }
        LinearOp::LinearSolveComponent {
            matrix_start,
            rhs_start,
            n,
            ..
        } => linear_solve_depends_on_y_index(row, matrix_start, rhs_start, n, target_y_index, memo),
        LinearOp::TableBounds { table_id, .. } => {
            reg_depends_on_y_index_memo(row, table_id, target_y_index, memo)
        }
        LinearOp::TableLookup {
            table_id,
            column,
            input,
            ..
        }
        | LinearOp::TableLookupSlope {
            table_id,
            column,
            input,
            ..
        } => {
            reg_depends_on_y_index_memo(row, table_id, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, column, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, input, target_y_index, memo)
        }
        LinearOp::TableNextEvent { table_id, time, .. } => {
            reg_depends_on_y_index_memo(row, table_id, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, time, target_y_index, memo)
        }
        LinearOp::RandomInitialState {
            local_seed,
            global_seed,
            ..
        } => {
            reg_depends_on_y_index_memo(row, local_seed, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, global_seed, target_y_index, memo)
        }
        LinearOp::RandomResult {
            state_start,
            state_len,
            ..
        }
        | LinearOp::RandomState {
            state_start,
            state_len,
            ..
        } => reg_range_depends_on_y_index(row, state_start, state_len, target_y_index, memo),
        LinearOp::ImpureRandomInit { seed, .. } => {
            reg_depends_on_y_index_memo(row, seed, target_y_index, memo)
        }
        LinearOp::ImpureRandom { id, .. } => {
            reg_depends_on_y_index_memo(row, id, target_y_index, memo)
        }
        LinearOp::ImpureRandomInteger { id, imin, imax, .. } => {
            reg_depends_on_y_index_memo(row, id, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, imin, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, imax, target_y_index, memo)
        }
        LinearOp::ExternalCall {
            args, arg_count, ..
        } => external_call_depends_on_y_index(row, &args, arg_count, target_y_index, memo),
        LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::LoadSeed { .. }
        | LinearOp::StoreOutput { .. } => false,
    });
    memo.insert(reg, result);
    result
}

fn linear_solve_depends_on_y_index(
    row: &[LinearOp],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
    target_y_index: usize,
    memo: &mut HashMap<u32, bool>,
) -> bool {
    let Some(matrix_len) = n.checked_mul(n) else {
        return true;
    };
    reg_range_depends_on_y_index(row, matrix_start, matrix_len, target_y_index, memo)
        || reg_range_depends_on_y_index(row, rhs_start, n, target_y_index, memo)
}

fn external_call_depends_on_y_index(
    row: &[LinearOp],
    args: &[u32; 8],
    arg_count: usize,
    target_y_index: usize,
    memo: &mut HashMap<u32, bool>,
) -> bool {
    let Some(effective_args) = args.get(..arg_count) else {
        return true;
    };
    effective_args
        .iter()
        .copied()
        .any(|arg| reg_depends_on_y_index_memo(row, arg, target_y_index, memo))
}

fn reg_range_depends_on_y_index(
    row: &[LinearOp],
    start: u32,
    len: usize,
    target_y_index: usize,
    memo: &mut HashMap<u32, bool>,
) -> bool {
    (0..len).any(|offset| {
        let Some(reg) = checked_reg_offset(start, offset) else {
            return true;
        };
        reg_depends_on_y_index_memo(row, reg, target_y_index, memo)
    })
}

fn checked_reg_offset(start: u32, offset: usize) -> Option<u32> {
    let offset = u32::try_from(offset).ok()?;
    start.checked_add(offset)
}

#[cfg(test)]
mod tests {
    use rumoca_ir_solve::{ExternalFunctionKind, LinearOp};

    use super::reg_depends_on_y_index;

    fn external_call(dst: u32, args: [u32; 8], arg_count: usize) -> LinearOp {
        LinearOp::ExternalCall {
            dst,
            function: ExternalFunctionKind::BuildingsEnergyPlusExchange,
            args,
            arg_count,
            output_index: 0,
        }
    }

    #[test]
    fn external_call_depends_on_target_through_effective_argument() {
        let row = [
            LinearOp::LoadY { dst: 1, index: 7 },
            LinearOp::Move { dst: 2, src: 1 },
            external_call(3, [2, 0, 0, 0, 0, 0, 0, 0], 1),
        ];

        assert!(reg_depends_on_y_index(&row, 3, 7));
    }

    #[test]
    fn external_call_ignores_other_y_and_unused_capacity() {
        let row = [
            LinearOp::Const { dst: 1, value: 1.0 },
            LinearOp::LoadY { dst: 2, index: 8 },
            LinearOp::LoadY { dst: 3, index: 7 },
            external_call(4, [1, 2, 3, 0, 0, 0, 0, 0], 2),
        ];

        assert!(!reg_depends_on_y_index(&row, 4, 7));
    }

    #[test]
    fn malformed_external_call_arg_count_is_conservatively_dependent() {
        let row = [external_call(1, [0; 8], 9)];

        assert!(reg_depends_on_y_index(&row, 1, 7));
    }
}
