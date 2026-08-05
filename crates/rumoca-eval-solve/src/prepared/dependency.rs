use rumoca_ir_solve::LinearOp;

pub(super) struct YDependencyAnalyzer<'a> {
    row: &'a [LinearOp],
    producers: Vec<Option<usize>>,
    memo_generation: Vec<u32>,
    memo_value: Vec<bool>,
    generation: u32,
    target_y_index: usize,
}

impl<'a> YDependencyAnalyzer<'a> {
    pub(super) fn new(row: &'a [LinearOp], target_y_index: usize) -> Self {
        // Register programs form a DAG: a register computed once can feed many
        // downstream ops, so memoize dependence on a fixed `y` index by register.
        let register_count = row
            .iter()
            .filter_map(LinearOp::dst_register)
            .max()
            .map_or(0, |register| register as usize + 1);
        let mut producers = vec![None; register_count];
        for (index, operation) in row.iter().enumerate() {
            if let Some(dst) = operation.dst_register() {
                producers[dst as usize] = Some(index);
            }
        }
        Self {
            row,
            producers,
            memo_generation: vec![0; register_count],
            memo_value: vec![false; register_count],
            generation: 1,
            target_y_index,
        }
    }

    pub(super) fn set_target(&mut self, target_y_index: usize) {
        self.target_y_index = target_y_index;
        self.generation = self.generation.wrapping_add(1);
        if self.generation == 0 {
            self.memo_generation.fill(0);
            self.generation = 1;
        }
    }

    pub(super) fn depends_on(&mut self, reg: u32) -> bool {
        reg_depends_on_y_index_memo(
            self.row,
            &self.producers,
            reg,
            self.target_y_index,
            self.generation,
            &mut self.memo_generation,
            &mut self.memo_value,
        )
    }
}

pub(super) fn reg_depends_on_y_index(row: &[LinearOp], reg: u32, target_y_index: usize) -> bool {
    YDependencyAnalyzer::new(row, target_y_index).depends_on(reg)
}

fn reg_depends_on_y_index_memo(
    row: &[LinearOp],
    producers: &[Option<usize>],
    reg: u32,
    target_y_index: usize,
    generation: u32,
    memo_generation: &mut [u32],
    memo_value: &mut [bool],
) -> bool {
    let reg_index = reg as usize;
    if memo_generation.get(reg_index).copied() == Some(generation) {
        return memo_value[reg_index];
    }
    // Guard against accidental cycles (register programs are acyclic in
    // practice): seed `false` before recursing so a back-edge terminates.
    let Some(memo_entry) = memo_generation.get_mut(reg_index) else {
        return false;
    };
    *memo_entry = generation;
    memo_value[reg_index] = false;
    let result = producers
        .get(reg as usize)
        .and_then(|producer| *producer)
        .and_then(|index| row.get(index))
        .is_some_and(|op| match *op {
            LinearOp::LoadY { index, .. } => index == target_y_index,
            LinearOp::Move { src, .. }
            | LinearOp::Unary { arg: src, .. }
            | LinearOp::LoadIndexedP { index: src, .. }
            | LinearOp::LoadIndexedSeed { index: src, .. } => reg_depends_on_y_index_memo(
                row,
                producers,
                src,
                target_y_index,
                generation,
                memo_generation,
                memo_value,
            ),
            LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
                reg_depends_on_y_index_memo(
                    row,
                    producers,
                    lhs,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                ) || reg_depends_on_y_index_memo(
                    row,
                    producers,
                    rhs,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                )
            }
            LinearOp::Select {
                cond,
                if_true,
                if_false,
                ..
            } => {
                reg_depends_on_y_index_memo(
                    row,
                    producers,
                    cond,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                ) || reg_depends_on_y_index_memo(
                    row,
                    producers,
                    if_true,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                ) || reg_depends_on_y_index_memo(
                    row,
                    producers,
                    if_false,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                )
            }
            LinearOp::LinearSolveComponent {
                matrix_start,
                rhs_start,
                n,
                ..
            } => {
                let Some(matrix_len) = n.checked_mul(n) else {
                    return true;
                };
                reg_range_depends_on_y_index(
                    row,
                    producers,
                    matrix_start,
                    matrix_len,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                ) || reg_range_depends_on_y_index(
                    row,
                    producers,
                    rhs_start,
                    n,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                )
            }
            LinearOp::TableBounds { table_id, .. } => reg_depends_on_y_index_memo(
                row,
                producers,
                table_id,
                target_y_index,
                generation,
                memo_generation,
                memo_value,
            ),
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
                reg_depends_on_y_index_memo(
                    row,
                    producers,
                    table_id,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                ) || reg_depends_on_y_index_memo(
                    row,
                    producers,
                    column,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                ) || reg_depends_on_y_index_memo(
                    row,
                    producers,
                    input,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                )
            }
            LinearOp::TableNextEvent { table_id, time, .. } => {
                reg_depends_on_y_index_memo(
                    row,
                    producers,
                    table_id,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                ) || reg_depends_on_y_index_memo(
                    row,
                    producers,
                    time,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                )
            }
            LinearOp::RandomInitialState {
                local_seed,
                global_seed,
                ..
            } => {
                reg_depends_on_y_index_memo(
                    row,
                    producers,
                    local_seed,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                ) || reg_depends_on_y_index_memo(
                    row,
                    producers,
                    global_seed,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                )
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
            } => reg_range_depends_on_y_index(
                row,
                producers,
                state_start,
                state_len,
                target_y_index,
                generation,
                memo_generation,
                memo_value,
            ),
            LinearOp::ImpureRandomInit { seed, .. } => reg_depends_on_y_index_memo(
                row,
                producers,
                seed,
                target_y_index,
                generation,
                memo_generation,
                memo_value,
            ),
            LinearOp::ImpureRandom { id, .. } => reg_depends_on_y_index_memo(
                row,
                producers,
                id,
                target_y_index,
                generation,
                memo_generation,
                memo_value,
            ),
            LinearOp::ImpureRandomInteger { id, imin, imax, .. } => {
                reg_depends_on_y_index_memo(
                    row,
                    producers,
                    id,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                ) || reg_depends_on_y_index_memo(
                    row,
                    producers,
                    imin,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                ) || reg_depends_on_y_index_memo(
                    row,
                    producers,
                    imax,
                    target_y_index,
                    generation,
                    memo_generation,
                    memo_value,
                )
            }
            LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::LoadSeed { .. }
            | LinearOp::StoreOutput { .. } => false,
        });
    memo_value[reg_index] = result;
    result
}

fn reg_range_depends_on_y_index(
    row: &[LinearOp],
    producers: &[Option<usize>],
    start: u32,
    len: usize,
    target_y_index: usize,
    generation: u32,
    memo_generation: &mut [u32],
    memo_value: &mut [bool],
) -> bool {
    (0..len).any(|offset| {
        let Some(reg) = checked_reg_offset(start, offset) else {
            return true;
        };
        reg_depends_on_y_index_memo(
            row,
            producers,
            reg,
            target_y_index,
            generation,
            memo_generation,
            memo_value,
        )
    })
}

fn checked_reg_offset(start: u32, offset: usize) -> Option<u32> {
    let offset = u32::try_from(offset).ok()?;
    start.checked_add(offset)
}
