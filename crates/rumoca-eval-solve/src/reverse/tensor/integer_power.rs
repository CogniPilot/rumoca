//! Reverse rule of an elementwise literal power.
//!
//! The operation stands for the `TensorBinary` chain of its plan: the
//! quotient `1 / x` for a negative exponent, then one product per step. This
//! rule replays that chain's reverse sweep for each element, its steps last
//! first, with the same `Binary` partials, the same accumulation order into
//! every value, and the same unselected-arm marks, so the adjoint of the
//! source is the chain's bit for bit. The intermediate values are recomputed
//! from the source and live only for the element.

use super::{Adjoints, offset};
use crate::reverse::{add_adj, binary_partials, reg, take_adj};
use rumoca_ir_solve::{BinaryOp, IntegerPower, Reg};

/// Adjoint and unselected-arm mark of one value of the chain: the source
/// register for value 0 of a positive exponent, a local slot otherwise.
struct Values<'a, 'state> {
    state: &'a mut Adjoints<'state>,
    source: Reg,
    reciprocal: bool,
    adj: &'a mut [f64],
    untaken: &'a mut [bool],
}

impl Values<'_, '_> {
    fn is_source(&self, value: usize) -> bool {
        value == 0 && !self.reciprocal
    }

    fn adjoint(&self, value: usize) -> f64 {
        if self.is_source(value) {
            self.state.adj[self.source as usize]
        } else {
            self.adj[value]
        }
    }

    fn add(&mut self, value: usize, contribution: f64) {
        if self.is_source(value) {
            add_adj(self.state.adj, self.source, contribution);
        } else {
            self.adj[value] += contribution;
        }
    }

    fn mark_untaken(&mut self, value: usize) {
        if self.adjoint(value) == 0.0 {
            if self.is_source(value) {
                self.state.untaken[self.source as usize] = true;
            } else {
                self.untaken[value] = true;
            }
        }
    }

    /// `skips_untaken` for a value the chain keeps in a local register.
    fn skips_local(&mut self, value: usize, value_adj: f64, operands: &[usize]) -> bool {
        let skip = value_adj == 0.0 && std::mem::take(&mut self.untaken[value]);
        if skip {
            for &operand in operands {
                self.mark_untaken(operand);
            }
        }
        skip
    }
}

pub(super) fn reverse(
    (dst_start, src_start): (Reg, Reg),
    count: usize,
    power: IntegerPower,
    regs: &[f64],
    state: &mut Adjoints<'_>,
) {
    // Stack buffers bounded by the widest plan; nothing is allocated.
    const VALUES: usize = IntegerPower::MAX_PRODUCTS + 1;
    let mut pairs = [(0, 0); IntegerPower::MAX_PRODUCTS];
    let mut defined = 0;
    for (slot, pair) in pairs.iter_mut().zip(power.product_pairs()) {
        *slot = pair;
        defined += 1;
    }
    debug_assert_eq!(defined, power.product_count(), "the plan fits MAX_PRODUCTS");
    let products = &pairs[..defined];
    let (mut values, mut local_adj, mut local_untaken) =
        ([0.0; VALUES], [0.0; VALUES], [false; VALUES]);
    let values = &mut values[..=defined];
    for element in 0..count {
        // The destination never overlaps the source, so taking each element's
        // adjoint as it is reached is taking the whole range first.
        let dst = offset(dst_start, element);
        let g = take_adj(state.adj, dst);
        let source = offset(src_start, element);
        let x = reg(regs, source);
        values[0] = if power.reciprocal() {
            crate::eval_tensor_binary_primal(BinaryOp::Div, 1.0, x, 1)
        } else {
            x
        };
        for (index, &(lhs, rhs)) in products.iter().enumerate() {
            values[index + 1] =
                crate::eval_tensor_binary_primal(BinaryOp::Mul, values[lhs], values[rhs], 1);
        }
        local_adj[..=defined].fill(0.0);
        local_untaken[..=defined].fill(false);
        let mut chain = Values {
            state: &mut *state,
            source,
            reciprocal: power.reciprocal(),
            adj: &mut local_adj[..=defined],
            untaken: &mut local_untaken[..=defined],
        };
        for (index, &operands) in products.iter().enumerate().rev() {
            chain.reverse_product(index + 1, operands, values, (dst, g));
        }
        if power.reciprocal() {
            reverse_quotient(&mut chain, x);
        }
    }
}

impl Values<'_, '_> {
    /// The chain's `TensorBinary` product rule for the step defining `value`,
    /// the last value being the destination register with adjoint `g`.
    fn reverse_product(
        &mut self,
        value: usize,
        (lhs, rhs): (usize, usize),
        values: &[f64],
        (dst, g): (Reg, f64),
    ) {
        let value_adj = if value + 1 == values.len() {
            if skips_destination(self, dst, g, [lhs, rhs]) {
                return;
            }
            g
        } else {
            let value_adj = std::mem::take(&mut self.adj[value]);
            if self.skips_local(value, value_adj, &[lhs, rhs]) {
                return;
            }
            value_adj
        };
        let (dl, dr) = binary_partials(BinaryOp::Mul, values[lhs], values[rhs]);
        self.add(lhs, value_adj * dl);
        self.add(rhs, value_adj * dr);
    }
}

/// `skips_untaken` on the destination register, with the chain's operands.
fn skips_destination(chain: &mut Values<'_, '_>, dst: Reg, g: f64, operands: [usize; 2]) -> bool {
    let skip = g == 0.0 && std::mem::take(&mut chain.state.untaken[dst as usize]);
    if skip {
        for operand in operands {
            chain.mark_untaken(operand);
        }
    }
    skip
}

/// The reverse of `1 / x` into the source. The constant numerator's adjoint
/// reaches no input, so only the source receives a contribution.
fn reverse_quotient(chain: &mut Values<'_, '_>, x: f64) {
    let value_adj = std::mem::take(&mut chain.adj[0]);
    let skip = value_adj == 0.0 && std::mem::take(&mut chain.untaken[0]);
    let source = chain.source as usize;
    if skip {
        if chain.state.adj[source] == 0.0 {
            chain.state.untaken[source] = true;
        }
        return;
    }
    let (_, dr) = binary_partials(BinaryOp::Div, 1.0, x);
    add_adj(chain.state.adj, chain.source, value_adj * dr);
}
