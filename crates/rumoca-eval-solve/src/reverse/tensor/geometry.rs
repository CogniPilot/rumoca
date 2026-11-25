//! Cross products and indexed replacements on a checked single-lane tensor.
use super::{Adjoints, LinearOp, Reg, add_adj, offset, reg, set, skips_untaken, take_adj};
use crate::tensor_update_register_value_offset;
use rumoca_ir_solve::TensorUpdateSubscript;

pub(super) fn reverse_cross(dst: Reg, lhs: Reg, rhs: Reg, regs: &[f64], state: &mut Adjoints<'_>) {
    for (i, (j, k)) in [(1, 2), (2, 0), (0, 1)].into_iter().enumerate() {
        let (lj, lk, rj, rk) = (lhs + j, lhs + k, rhs + j, rhs + k);
        let d = offset(dst, i);
        let g = take_adj(state.adj, d);
        if skips_untaken(state.untaken, state.adj, d, g, &[lj, rk, lk, rj]) {
            continue;
        }
        add_adj(state.adj, lj, g * reg(regs, rk));
        add_adj(state.adj, rk, g * reg(regs, lj));
        add_adj(state.adj, lk, -g * reg(regs, rj));
        add_adj(state.adj, rj, -g * reg(regs, lk));
    }
}

struct Update<'a> {
    dst: Reg,
    base: Reg,
    value: Reg,
    dimensions: &'a [u32],
    subscripts: &'a [TensorUpdateSubscript],
}

impl<'a> Update<'a> {
    fn of(op: &'a LinearOp) -> Self {
        let LinearOp::TensorUpdate {
            dst_start,
            base_start,
            value_start,
            dimensions,
            subscripts,
            ..
        } = op
        else {
            unreachable!("update rule receives a tensor update")
        };
        Self {
            dst: *dst_start,
            base: *base_start,
            value: *value_start,
            dimensions,
            subscripts,
        }
    }

    fn count(&self) -> usize {
        self.dimensions.iter().map(|&n| n as usize).product()
    }

    fn source(&self, i: usize, regs: &[f64]) -> Reg {
        let Ok(replacement) =
            tensor_update_register_value_offset(self.dimensions, self.subscripts, i, |r| {
                Ok::<_, std::convert::Infallible>(reg(regs, r))
            });
        match replacement {
            Some(j) => offset(self.value, j),
            None => offset(self.base, i),
        }
    }
}

pub(super) fn forward_update(op: &LinearOp, regs: &mut [f64]) {
    let update = Update::of(op);
    for i in 0..update.count() {
        let value = reg(regs, update.source(i, regs));
        set(regs, offset(update.dst, i), value);
    }
}

pub(super) fn reverse_update(op: &LinearOp, regs: &[f64], state: &mut Adjoints<'_>) {
    let update = Update::of(op);
    for i in 0..update.count() {
        let dst = offset(update.dst, i);
        let src = update.source(i, regs);
        let g = take_adj(state.adj, dst);
        if !skips_untaken(state.untaken, state.adj, dst, g, &[src]) {
            add_adj(state.adj, src, g);
        }
    }
}
