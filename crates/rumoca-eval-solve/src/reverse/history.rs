//! Restore overwritten operands at the instruction that consumed them. The
//! common monotonically allocated register stream has no overwrite entries.
use rumoca_ir_solve::{LinearOp, Reg};

#[derive(Default, Clone)]
pub(super) struct History {
    high_water: usize,
    overwritten: Vec<(usize, usize, f64)>,
    pub(super) regions: Vec<(usize, super::region::Invocation)>,
    pub(super) demanded: Option<Vec<bool>>,
    stores: Vec<(usize, Reg, f64, bool)>,
}

impl History {
    pub(super) fn clear(&mut self) {
        self.high_water = 0;
        self.overwritten.clear();
        self.stores.clear();
        self.demanded = None;
        self.regions.clear();
    }

    pub(super) fn before_write(&mut self, instruction: usize, op: &LinearOp, regs: &[f64]) {
        if let Some(start) = op.dst_register() {
            let start = start as usize;
            let end = start + op.dst_register_count();
            for (slot, &value) in regs
                .iter()
                .enumerate()
                .take(end.min(self.high_water))
                .skip(start)
            {
                self.overwritten.push((instruction, slot, value));
            }
            self.high_water = self.high_water.max(end);
        }
    }

    pub(super) fn output(&mut self, instruction: usize, source: Reg, weight: f64, untaken: bool) {
        self.stores.push((instruction, source, weight, untaken));
    }

    pub(super) fn rewind(
        &mut self,
        instruction: usize,
        regs: &mut [f64],
        adj: &mut [f64],
        untaken: &mut [bool],
    ) {
        while let Some(&(i, slot, value)) = self.overwritten.last() {
            if i != instruction {
                break;
            }
            regs[slot] = value;
            self.overwritten.pop();
        }
        while let Some(&(i, source, weight, inactive)) = self.stores.last() {
            if i != instruction {
                break;
            }
            super::add_adj(adj, source, weight);
            if inactive && adj[source as usize] == 0. {
                untaken[source as usize] = true;
            }
            self.stores.pop();
        }
    }
}
