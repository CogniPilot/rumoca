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
        let reg_index = reg as usize;
        if self.memo_generation.get(reg_index).copied() == Some(self.generation) {
            return self.memo_value[reg_index];
        }
        // Register programs are checked DAGs. Seed `false` before recursion so
        // malformed fixture cycles still terminate conservatively.
        let Some(memo_entry) = self.memo_generation.get_mut(reg_index) else {
            return false;
        };
        *memo_entry = self.generation;
        self.memo_value[reg_index] = false;
        let result = self
            .producer(reg)
            .is_some_and(|operation| self.operation_depends_on_target(operation));
        self.memo_value[reg_index] = result;
        result
    }

    fn producer(&self, register: u32) -> Option<LinearOp> {
        self.producers
            .get(register as usize)
            .and_then(|producer| *producer)
            .and_then(|index| self.row.get(index))
            .copied()
    }

    fn operation_depends_on_target(&mut self, operation: LinearOp) -> bool {
        match operation {
            LinearOp::LoadY { index, .. } => index == self.target_y_index,
            LinearOp::Move { src, .. }
            | LinearOp::Unary { arg: src, .. }
            | LinearOp::LoadIndexedP { index: src, .. }
            | LinearOp::LoadIndexedSeed { index: src, .. } => self.depends_on(src),
            LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
                self.any_register_depends([lhs, rhs])
            }
            LinearOp::Select {
                cond,
                if_true,
                if_false,
                ..
            } => self.any_register_depends([cond, if_true, if_false]),
            LinearOp::LinearSolveComponent {
                matrix_start,
                rhs_start,
                n,
                ..
            } => self.linear_solve_depends(matrix_start, rhs_start, n),
            LinearOp::TableBounds { table_id, .. } => self.depends_on(table_id),
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
            } => self.any_register_depends([table_id, column, input]),
            LinearOp::TableNextEvent { table_id, time, .. } => {
                self.any_register_depends([table_id, time])
            }
            LinearOp::RandomInitialState {
                local_seed,
                global_seed,
                ..
            } => self.any_register_depends([local_seed, global_seed]),
            LinearOp::RandomResult {
                state_start,
                state_len,
                ..
            }
            | LinearOp::RandomState {
                state_start,
                state_len,
                ..
            } => self.register_range_depends(state_start, state_len),
            LinearOp::ImpureRandomInit { seed, .. } => self.depends_on(seed),
            LinearOp::ImpureRandom { id, .. } => self.depends_on(id),
            LinearOp::ImpureRandomInteger { id, imin, imax, .. } => {
                self.any_register_depends([id, imin, imax])
            }
            LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::LoadSeed { .. }
            | LinearOp::StoreOutput { .. } => false,
        }
    }

    fn any_register_depends<const N: usize>(&mut self, registers: [u32; N]) -> bool {
        registers
            .into_iter()
            .any(|register| self.depends_on(register))
    }

    fn linear_solve_depends(&mut self, matrix_start: u32, rhs_start: u32, n: usize) -> bool {
        let Some(matrix_len) = n.checked_mul(n) else {
            return true;
        };
        self.register_range_depends(matrix_start, matrix_len)
            || self.register_range_depends(rhs_start, n)
    }

    fn register_range_depends(&mut self, start: u32, len: usize) -> bool {
        (0..len).any(|offset| {
            checked_reg_offset(start, offset).is_none_or(|register| self.depends_on(register))
        })
    }
}

pub(super) fn reg_depends_on_y_index(row: &[LinearOp], reg: u32, target_y_index: usize) -> bool {
    YDependencyAnalyzer::new(row, target_y_index).depends_on(reg)
}

fn checked_reg_offset(start: u32, offset: usize) -> Option<u32> {
    let offset = u32::try_from(offset).ok()?;
    start.checked_add(offset)
}
