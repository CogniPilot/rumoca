//! Lower single-lane tensor register ops to straight-line scalar ops.
//!
//! Exact-assignment and parameter-initialization programs replay prefixes of
//! canonical residual programs, which may contain aggregate tensor ops
//! (`TensorLoad`, `MatrixMultiply`, ...). The scalar ODE right-hand-side
//! templates render only scalar ops, so these programs are expanded here into
//! the equivalent scalar sequence. Every aggregate occupies a contiguous
//! register range, so expansion writes the same registers the aggregate would;
//! reductions use fresh registers above every register the program defines.
//!
//! A `PureCall` is inlined from its owner body when the caller supplies the
//! model's pure-call table (`pure_calls`).
//!
//! Only primal (`lanes == 1`) ops are lowered. Any other op that a scalar
//! template cannot render (AD lanes, concatenation, patches, tables, random
//! numbers) makes the whole program unlowerable, so the caller can refuse the
//! model at the capability gate instead of failing inside a template.

use rumoca_ir_solve::{BinaryOp, LinearOp, Reg, TensorInputKind};

mod pure_calls;

const EXPANSION_LIMIT: usize = 4_000_000;

/// One projection's cumulative emitted operations, scalar allocations and work.
struct ExpansionBudget {
    used: usize,
}

impl ExpansionBudget {
    fn check(&self, cost: usize) -> Option<()> {
        (self.used.checked_add(cost)? <= EXPANSION_LIMIT).then_some(())
    }

    fn reserve(&mut self, cost: usize) -> Option<()> {
        self.check(cost)?;
        self.used += cost;
        Some(())
    }
}

fn dot_cost(count: usize) -> Option<usize> {
    if count == 0 {
        Some(1)
    } else {
        count.checked_mul(2)?.checked_sub(1)
    }
}

/// Check the complete direct tensor expansion before allocating its output.
fn expansion_cost(op: &LinearOp) -> Option<usize> {
    match op {
        _ if is_scalar_op(op) => Some(1),
        LinearOp::TensorLoad {
            lanes: 1, count, ..
        }
        | LinearOp::TensorBinary {
            lanes: 1, count, ..
        }
        | LinearOp::TensorFill {
            lanes: 1, count, ..
        } => Some(*count),
        LinearOp::DotProduct { count, .. } => dot_cost(*count),
        LinearOp::MatrixMultiply {
            lanes: 1,
            rows,
            inner,
            columns,
            ..
        } => rows.checked_mul(*columns)?.checked_mul(dot_cost(*inner)?),
        LinearOp::TensorIdentity { lanes: 1, size, .. } => size.checked_mul(*size),
        LinearOp::TensorTranspose {
            lanes: 1,
            rows,
            columns,
            element_width,
            ..
        } => rows.checked_mul(*columns)?.checked_mul(*element_width),
        LinearOp::TensorCross { lanes: 1, .. } => Some(9),
        // Typed bodies reserve their scalar storage and domain work separately.
        LinearOp::PureCall { .. } => Some(0),
        _ => None,
    }
}

/// Ops every scalar ODE template renders directly.
fn is_scalar_op(op: &LinearOp) -> bool {
    matches!(
        op,
        LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadY { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::LoadIndexedP { .. }
            | LinearOp::Move { .. }
            | LinearOp::LinearSolveComponent { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
            | LinearOp::StoreOutput { .. }
            | LinearOp::StoreOutputRange { .. }
    )
}

struct Lowering<'a> {
    out: Vec<LinearOp>,
    budget: ExpansionBudget,
    next: Reg,
    pure_calls: Option<&'a rumoca_ir_solve::SolvePureCallTable>,
}

impl Lowering<'_> {
    fn fresh(&mut self) -> Option<Reg> {
        let reg = self.next;
        self.next = self.next.checked_add(1)?;
        Some(reg)
    }

    fn reg(base: Reg, offset: usize) -> Option<Reg> {
        base.checked_add(Reg::try_from(offset).ok()?)
    }

    /// `dst = sum_k lhs[k*ls] * rhs[k*rs]` over `count` terms.
    fn dot(
        &mut self,
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
        count: usize,
        ls: usize,
        rs: usize,
    ) -> Option<()> {
        if count == 0 {
            self.out.push(LinearOp::Const { dst, value: 0.0 });
            return Some(());
        }
        // Accumulate into fresh registers; the last write lands in `dst`.
        let mut acc: Option<Reg> = None;
        for k in 0..count {
            let last = k + 1 == count;
            let term = if last && acc.is_none() {
                dst
            } else {
                self.fresh()?
            };
            self.out.push(LinearOp::Binary {
                dst: term,
                op: BinaryOp::Mul,
                lhs: Self::reg(lhs, k.checked_mul(ls)?)?,
                rhs: Self::reg(rhs, k.checked_mul(rs)?)?,
            });
            acc = Some(self.accumulate(acc, term, last.then_some(dst))?);
        }
        Some(())
    }

    /// `previous + term` into `into` (or a fresh register); the first term is
    /// its own sum.
    fn accumulate(&mut self, previous: Option<Reg>, term: Reg, into: Option<Reg>) -> Option<Reg> {
        let Some(previous) = previous else {
            return Some(term);
        };
        let sum = match into {
            Some(dst) => dst,
            None => self.fresh()?,
        };
        self.out.push(LinearOp::Binary {
            dst: sum,
            op: BinaryOp::Add,
            lhs: previous,
            rhs: term,
        });
        Some(sum)
    }

    fn matrix_multiply(
        &mut self,
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
        shape: (usize, usize, usize),
    ) -> Option<()> {
        let (rows, inner, columns) = shape;
        for (i, j) in (0..rows).flat_map(|i| (0..columns).map(move |j| (i, j))) {
            let out = Self::reg(dst, i.checked_mul(columns)?.checked_add(j)?)?;
            let row = Self::reg(lhs, i.checked_mul(inner)?)?;
            self.dot(out, row, Self::reg(rhs, j)?, inner, 1, columns)?;
        }
        Some(())
    }

    fn identity(&mut self, dst: Reg, size: usize) -> Option<()> {
        for (i, j) in (0..size).flat_map(|i| (0..size).map(move |j| (i, j))) {
            self.out.push(LinearOp::Const {
                dst: Self::reg(dst, i.checked_mul(size)?.checked_add(j)?)?,
                value: if i == j { 1.0 } else { 0.0 },
            });
        }
        Some(())
    }

    /// Output rows x columns (x width) from a columns x rows source.
    fn transpose(&mut self, dst: Reg, src: Reg, shape: (usize, usize, usize)) -> Option<()> {
        let (rows, columns, width) = shape;
        let points = (0..rows)
            .flat_map(|i| (0..columns).flat_map(move |j| (0..width).map(move |e| (i, j, e))));
        for (i, j, e) in points {
            let out = i
                .checked_mul(columns)?
                .checked_add(j)?
                .checked_mul(width)?
                .checked_add(e)?;
            let from = j
                .checked_mul(rows)?
                .checked_add(i)?
                .checked_mul(width)?
                .checked_add(e)?;
            self.out.push(LinearOp::Move {
                dst: Self::reg(dst, out)?,
                src: Self::reg(src, from)?,
            });
        }
        Some(())
    }

    /// `c = a x b`: `c_i = a_j b_k - a_k b_j` for cyclic `(i, j, k)`.
    fn cross(&mut self, dst: Reg, lhs: Reg, rhs: Reg) -> Option<()> {
        for (i, j, k) in [(0, 1, 2), (1, 2, 0), (2, 0, 1)] {
            let ab = self.fresh()?;
            let ba = self.fresh()?;
            self.out.push(LinearOp::Binary {
                dst: ab,
                op: BinaryOp::Mul,
                lhs: Self::reg(lhs, j)?,
                rhs: Self::reg(rhs, k)?,
            });
            self.out.push(LinearOp::Binary {
                dst: ba,
                op: BinaryOp::Mul,
                lhs: Self::reg(lhs, k)?,
                rhs: Self::reg(rhs, j)?,
            });
            self.out.push(LinearOp::Binary {
                dst: Self::reg(dst, i)?,
                op: BinaryOp::Sub,
                lhs: ab,
                rhs: ba,
            });
        }
        Some(())
    }

    fn elementwise(&mut self, op: &LinearOp) -> Option<()> {
        match op {
            LinearOp::TensorLoad {
                dst_start,
                input,
                input_start,
                count,
                ..
            } => {
                for k in 0..*count {
                    let dst = Self::reg(*dst_start, k)?;
                    let index = input_start.checked_add(k)?;
                    self.out.push(match input {
                        TensorInputKind::Y => LinearOp::LoadY { dst, index },
                        TensorInputKind::P => LinearOp::LoadP { dst, index },
                    });
                }
            }
            LinearOp::TensorBinary {
                dst_start,
                op,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                ..
            } => {
                for k in 0..*count {
                    self.out.push(LinearOp::Binary {
                        dst: Self::reg(*dst_start, k)?,
                        op: *op,
                        lhs: Self::reg(*lhs_start, k.checked_mul(*lhs_stride)?)?,
                        rhs: Self::reg(*rhs_start, k.checked_mul(*rhs_stride)?)?,
                    });
                }
            }
            LinearOp::TensorFill {
                dst_start,
                value_start,
                count,
                ..
            } => {
                for k in 0..*count {
                    self.out.push(LinearOp::Move {
                        dst: Self::reg(*dst_start, k)?,
                        src: *value_start,
                    });
                }
            }
            _ => return None,
        }
        Some(())
    }

    fn lower(&mut self, op: &LinearOp) -> Option<()> {
        match op {
            _ if is_scalar_op(op) => self.out.push(op.clone()),
            LinearOp::TensorLoad { lanes: 1, .. }
            | LinearOp::TensorBinary { lanes: 1, .. }
            | LinearOp::TensorFill { lanes: 1, .. } => self.elementwise(op)?,
            LinearOp::DotProduct {
                dst,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
            } => self.dot(
                *dst,
                *lhs_start,
                *rhs_start,
                *count,
                *lhs_stride,
                *rhs_stride,
            )?,
            LinearOp::MatrixMultiply {
                dst_start,
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes: 1,
            } => self.matrix_multiply(
                *dst_start,
                *lhs_start,
                *rhs_start,
                (*rows, *inner, *columns),
            )?,
            LinearOp::TensorIdentity {
                dst_start,
                size,
                lanes: 1,
            } => self.identity(*dst_start, *size)?,
            LinearOp::TensorTranspose {
                dst_start,
                src_start,
                rows,
                columns,
                element_width,
                lanes: 1,
            } => self.transpose(*dst_start, *src_start, (*rows, *columns, *element_width))?,
            LinearOp::TensorCross {
                dst_start,
                lhs_start,
                rhs_start,
                lanes: 1,
            } => self.cross(*dst_start, *lhs_start, *rhs_start)?,
            LinearOp::PureCall {
                dst_start,
                input_starts,
                site,
            } => pure_calls::Inliner::new(
                self.pure_calls?,
                &mut self.out,
                &mut self.next,
                &mut self.budget,
            )
            .call_site(*dst_start, input_starts, site)?,
            _ => return None,
        }
        Some(())
    }
}

/// The scalar expansion of `program`, or `None` when it contains an op no
/// scalar template can render.
///
/// Each `PureCall` is inlined from `pure_calls`; without a table, a program
/// holding one is unlowerable. The checked typed arithmetic profile must be
/// representable by the binary64 scalar ABI; unsupported checked faults are
/// refused. Expansion is bounded before scalar allocations and tensor loops.
pub fn lower_scalar_ops(
    program: &[LinearOp],
    pure_calls: Option<&rumoca_ir_solve::SolvePureCallTable>,
) -> Option<Vec<LinearOp>> {
    let mut budget = ExpansionBudget { used: 0 };
    let cost = program.iter().try_fold(program.len(), |total, op| {
        total.checked_add(expansion_cost(op)?)
    })?;
    budget.reserve(cost)?;
    if program.iter().all(is_scalar_op) {
        return Some(program.to_vec());
    }
    let next = program.iter().try_fold(0, |highest, op| {
        let Some(start) = op.dst_register() else {
            return Some(highest);
        };
        let end = start.checked_add(Reg::try_from(op.dst_register_count()).ok()?)?;
        Some(highest.max(end))
    })?;
    let mut lowering = Lowering {
        out: Vec::with_capacity(program.len()),
        budget,
        next,
        pure_calls,
    };
    for op in program {
        lowering.lower(op)?;
    }
    Some(lowering.out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compact_tensor_expansion_is_refused_before_allocating_millions_of_ops() {
        let program = [LinearOp::TensorFill {
            dst_start: 1,
            value_start: 0,
            count: 4_000_001,
            lanes: 1,
        }];
        assert!(lower_scalar_ops(&program, None).is_none());
    }

    #[test]
    fn overflowing_matrix_expansion_is_refused_before_entering_its_domain() {
        let program = [LinearOp::MatrixMultiply {
            dst_start: 2,
            lhs_start: 0,
            rhs_start: 1,
            rows: usize::MAX,
            inner: 2,
            columns: 2,
            lanes: 1,
        }];
        assert!(lower_scalar_ops(&program, None).is_none());
    }

    #[test]
    fn matrix_multiply_lowers_to_row_dot_products() {
        // [1 2; 3 4] * [5; 6] = [17; 39]
        let program = vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::Const { dst: 2, value: 3.0 },
            LinearOp::Const { dst: 3, value: 4.0 },
            LinearOp::Const { dst: 4, value: 5.0 },
            LinearOp::Const { dst: 5, value: 6.0 },
            LinearOp::MatrixMultiply {
                dst_start: 6,
                lhs_start: 0,
                rhs_start: 4,
                rows: 2,
                inner: 2,
                columns: 1,
                lanes: 1,
            },
            LinearOp::StoreOutput { src: 6 },
            LinearOp::StoreOutput { src: 7 },
        ];
        let lowered = lower_scalar_ops(&program, None).expect("primal matmul lowers");
        assert!(lowered.iter().all(is_scalar_op));
        let mut regs = std::collections::HashMap::new();
        let mut outputs = Vec::new();
        for op in &lowered {
            match op {
                LinearOp::Const { dst, value } => {
                    regs.insert(*dst, *value);
                }
                LinearOp::Binary { dst, op, lhs, rhs } => {
                    let (a, b) = (regs[lhs], regs[rhs]);
                    let value = match op {
                        BinaryOp::Add => a + b,
                        BinaryOp::Mul => a * b,
                        BinaryOp::Sub => a - b,
                        _ => unreachable!(),
                    };
                    regs.insert(*dst, value);
                }
                LinearOp::StoreOutput { src } => outputs.push(regs[src]),
                other => panic!("unexpected op {other:?}"),
            }
        }
        assert_eq!(outputs, vec![17.0, 39.0]);
    }

    #[test]
    fn tangent_lanes_are_not_lowered() {
        let program = vec![LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: None,
            lanes: 2,
        }];
        assert!(lower_scalar_ops(&program, None).is_none());
    }
}
