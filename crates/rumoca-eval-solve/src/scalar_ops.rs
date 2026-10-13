//! Lower single-lane tensor register ops to straight-line scalar ops.
//!
//! Exact-assignment and parameter-initialization programs replay prefixes of
//! canonical residual programs, which may contain aggregate tensor ops
//! (`TensorLoad`, `MatrixMultiply`, ...). The scalar ODE right-hand-side
//! templates render only scalar ops, so these programs are expanded here into
//! the equivalent scalar sequence. Every aggregate occupies a contiguous
//! register range. Overlapping operands are preserved until all results have
//! been computed; temporary results and reductions use fresh registers above
//! every register the program defines.
//!
//! A `PureCall` is inlined from its owner body when the caller supplies the
//! model's pure-call table (`pure_calls`).
//!
//! Only primal (`lanes == 1`) ops are lowered. Any other op that a scalar
//! template cannot render (AD lanes, concatenation, patches, tables, random
//! numbers) makes the whole program unlowerable, so the caller can refuse the
//! model at the capability gate instead of failing inside a template.

#[cfg(test)]
mod budget_tests;
#[cfg(test)]
mod fold_tests;
mod pure_calls;
#[cfg(test)]
mod snapshot_tests;
mod snapshots;

use rumoca_ir_solve::{BinaryOp, LinearOp, Reg, TensorInputKind};

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
        // Scalar templates emit one assignment per output coordinate.
        LinearOp::StoreOutputRange { count, .. } => Some(*count),
        // Each emitted component passes the entire dense matrix and RHS.
        // Count those final scalar operands, not just the compact IR opcode.
        LinearOp::LinearSolveComponent { n, .. } => {
            n.checked_mul(*n)?.checked_add(*n)?.checked_add(1)
        }
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
        } => {
            if *rows == 0 || *columns == 0 {
                Some(0)
            } else {
                rows.checked_mul(*columns)?.checked_mul(dot_cost(*inner)?)
            }
        }
        LinearOp::TensorIdentity { lanes: 1, size, .. } => size.checked_mul(*size),
        LinearOp::TensorTranspose {
            lanes: 1,
            rows,
            columns,
            element_width,
            ..
        } => {
            if *rows == 0 || *columns == 0 || *element_width == 0 {
                Some(0)
            } else {
                rows.checked_mul(*columns)?.checked_mul(*element_width)
            }
        }
        LinearOp::TensorCross { lanes: 1, .. } => Some(9),
        LinearOp::TensorConcatenate {
            lanes: 1,
            dimensions,
            ..
        } => dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize)),
        LinearOp::FunctionFold { program, .. } | LinearOp::GuardedFunctionFold { program, .. } => {
            fold_cost(program)
        }
        // Typed bodies reserve their scalar storage and domain work separately.
        LinearOp::PureCall { .. } => Some(0),
        _ => None,
    }
}

/// The straight-line cost of unrolling one fold: its update body once per pass,
/// the pass inputs, and the commit of the carried tuple.
fn fold_cost(program: &rumoca_ir_solve::FunctionFoldProgram) -> Option<usize> {
    if program.continuation.is_some() {
        return None;
    }
    let passes = program.domain.scalar_count().ok()?;
    passes
        .checked_mul(program.update.len().checked_add(program.register_count)?)?
        .checked_add(program.carried_count.checked_mul(2)?)
}

/// The register one scalar op defines, for the ops a fold body unrolls into.
fn scalar_destination(op: &LinearOp) -> Option<Reg> {
    match op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::Move { dst, .. }
        | LinearOp::Unary { dst, .. }
        | LinearOp::Binary { dst, .. }
        | LinearOp::Compare { dst, .. }
        | LinearOp::Select { dst, .. } => Some(*dst),
        _ => None,
    }
}

/// The registers one scalar op reads.
fn scalar_reads(op: &LinearOp) -> Vec<Reg> {
    match op {
        LinearOp::Move { src, .. } => vec![*src],
        LinearOp::Unary { arg, .. } => vec![*arg],
        LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => vec![*lhs, *rhs],
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => vec![*cond, *if_true, *if_false],
        _ => Vec::new(),
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
        if rows == 0 || columns == 0 {
            return Some(());
        }
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
        if rows == 0 || columns == 0 || width == 0 {
            return Some(());
        }
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

    /// Unroll one counted fold over scalar registers. A guarded fold keeps the
    /// initial carried tuple when its activation is false.
    fn fold(
        &mut self,
        dst: Reg,
        (initial, capture): (Reg, Reg),
        activation: Option<Reg>,
        program: &rumoca_ir_solve::FunctionFoldProgram,
    ) -> Option<()> {
        if program.continuation.is_some() {
            return None;
        }
        let first = self.out.len();
        let mut current = (0..program.carried_count)
            .map(|index| Self::reg(initial, index))
            .collect::<Option<Vec<_>>>()?;
        for tuple in program.domain.index_tuple_iter().ok()? {
            let pass = FoldPass {
                base: self.next,
                tuple,
                capture,
            };
            self.next = Self::reg(pass.base, program.register_count)?;
            current = self.fold_pass(program, &pass, &current)?;
        }
        let body_end = self.out.len();
        for (index, value) in current.into_iter().enumerate() {
            let destination = Self::reg(dst, index)?;
            self.out.push(match activation {
                None => LinearOp::Move {
                    dst: destination,
                    src: value,
                },
                Some(cond) => LinearOp::Select {
                    dst: destination,
                    cond,
                    if_true: value,
                    if_false: Self::reg(initial, index)?,
                },
            });
        }
        self.drop_unread(first, body_end);
        Some(())
    }

    /// One pass of an unrolled fold: the update body over fresh registers, the
    /// stored next carried tuple returned.
    fn fold_pass(
        &mut self,
        program: &rumoca_ir_solve::FunctionFoldProgram,
        pass: &FoldPass,
        current: &[Reg],
    ) -> Option<Vec<Reg>> {
        let mut stored = Vec::with_capacity(program.carried_count);
        // Registers whose value this pass fixes: binder values and literals,
        // which resolve the runtime subscripts of local aggregates.
        let mut known = std::collections::HashMap::<Reg, f64>::new();
        for op in &program.update {
            if let LinearOp::StoreOutput { src } = op {
                stored.push(pass.local(*src)?);
                continue;
            }
            self.fold_pass_op(op, pass, current, &mut known)?;
        }
        (stored.len() == program.carried_count).then_some(stored)
    }

    fn fold_pass_op(
        &mut self,
        op: &LinearOp,
        pass: &FoldPass,
        current: &[Reg],
        known: &mut std::collections::HashMap<Reg, f64>,
    ) -> Option<()> {
        match op {
            LinearOp::TensorConcatenate {
                dst_start,
                sources,
                dimensions,
                axis,
                lanes: 1,
            } => return self.fold_concatenate(*dst_start, (sources, dimensions, *axis), pass),
            LinearOp::LoadIndexedRegister {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let offset = crate::tensor_index::tensor_register_offset(
                    dimensions,
                    indices,
                    None,
                    |register| {
                        pass.local(register)
                            .and_then(|register| known.get(&register).copied())
                            .ok_or_else(|| crate::invalid_row("fold subscript is not fixed"))
                    },
                )
                .ok()?;
                self.out.push(LinearOp::Move {
                    dst: pass.local(*dst)?,
                    src: pass.local(Self::reg(*base, offset.checked_mul(*stride)?)?)?,
                });
                return Some(());
            }
            _ => {}
        }
        let lowered = pass.scalar_op(op, current)?;
        if let Some((register, value)) = pass.fixed_value(op, known)? {
            known.insert(register, value);
        }
        self.out.push(lowered);
        Some(())
    }

    fn fold_concatenate(
        &mut self,
        dst_start: Reg,
        (sources, dimensions, axis): (&[rumoca_ir_solve::TensorConcatenateSource], &[u32], usize),
        pass: &FoldPass,
    ) -> Option<()> {
        let mut moves = Vec::new();
        crate::visit_tensor_concatenate(sources, dimensions, axis, 1, |source, offset| {
            moves.push((source, offset));
            Ok::<_, ()>(())
        })
        .ok()?;
        for (source, offset) in moves {
            self.out.push(LinearOp::Move {
                dst: pass.local(Self::reg(dst_start, offset)?)?,
                src: pass.local(source)?,
            });
        }
        Some(())
    }

    /// Drop the unrolled body ops whose result no later body op or commit reads.
    fn drop_unread(&mut self, first: usize, body_end: usize) {
        let mut live = std::collections::HashSet::new();
        for op in &self.out[body_end..] {
            live.extend(scalar_reads(op));
        }
        let mut keep = vec![true; body_end - first];
        for index in (first..body_end).rev() {
            let op = &self.out[index];
            match scalar_destination(op) {
                Some(destination) if live.contains(&destination) => {
                    live.extend(scalar_reads(op));
                }
                _ => keep[index - first] = false,
            }
        }
        let mut kept = Vec::with_capacity(self.out.len());
        for (index, op) in std::mem::take(&mut self.out).into_iter().enumerate() {
            let in_body = index >= first && index < body_end;
            if !in_body || keep[index - first] {
                kept.push(op);
            }
        }
        self.out = kept;
    }

    /// Row-major copy of each source block into its slice of the result; the
    /// caller stages the commit so overlapping operands stay readable.
    fn concatenate(
        &mut self,
        dst: Reg,
        sources: &[rumoca_ir_solve::TensorConcatenateSource],
        dimensions: &[u32],
        axis: usize,
    ) -> Option<()> {
        let mut moves = Vec::new();
        crate::visit_tensor_concatenate(sources, dimensions, axis, 1, |source, offset| {
            moves.push((source, offset));
            Ok::<_, ()>(())
        })
        .ok()?;
        for (source, offset) in moves {
            self.out.push(LinearOp::Move {
                dst: Self::reg(dst, offset)?,
                src: source,
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
            LinearOp::TensorConcatenate {
                dst_start,
                sources,
                dimensions,
                axis,
                lanes: 1,
            } => self.concatenate(*dst_start, sources, dimensions, *axis)?,
            LinearOp::FunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
            } => self.fold(*dst_start, (*initial_start, *capture_start), None, program)?,
            LinearOp::GuardedFunctionFold {
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            } => self.fold(
                *dst_start,
                (*initial_start, *capture_start),
                Some(*activation),
                program,
            )?,
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
        let expansion = expansion_cost(op)?;
        if expansion == 0 && !is_scalar_op(op) && !matches!(op, LinearOp::PureCall { .. }) {
            // An empty structured domain is distinct from a malformed direct
            // tensor op. Reuse its authoritative dimension admission before
            // an empty expansion could erase that construction refusal.
            rumoca_ir_solve::ScalarProgramRegisterFlow::derive(std::slice::from_ref(op)).ok()?;
        }
        total
            .checked_add(expansion)?
            .checked_add(snapshots::result_count(op)?.checked_mul(2)?)
    })?;
    budget.reserve(cost)?;
    if program.iter().all(is_scalar_op) {
        return Some(rumoca_ir_solve::prune_dead_constants(program.to_vec()));
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
        lowering.lower_with_snapshot(op)?;
    }
    Some(rumoca_ir_solve::prune_dead_constants(lowering.out))
}

/// The register frame and binder point of one unrolled fold pass.
struct FoldPass {
    base: Reg,
    tuple: Vec<i64>,
    capture: Reg,
}

impl FoldPass {
    fn local(&self, register: Reg) -> Option<Reg> {
        self.base.checked_add(register)
    }

    /// The register and value a pass-constant op fixes, if it is one.
    fn fixed_value(
        &self,
        op: &LinearOp,
        known: &std::collections::HashMap<Reg, f64>,
    ) -> Option<Option<(Reg, f64)>> {
        Some(match op {
            LinearOp::LoadFoldIndex { dst, dimension } => {
                Some((self.local(*dst)?, *self.tuple.get(*dimension)? as f64))
            }
            LinearOp::Const { dst, value } => Some((self.local(*dst)?, *value)),
            LinearOp::Move { dst, src } => match known.get(&self.local(*src)?) {
                Some(value) => Some((self.local(*dst)?, *value)),
                None => None,
            },
            _ => None,
        })
    }

    /// The scalar op for one update op over this pass's registers.
    fn scalar_op(&self, op: &LinearOp, current: &[Reg]) -> Option<LinearOp> {
        Some(match op {
            LinearOp::LoadFoldIndex { dst, dimension } => LinearOp::Const {
                dst: self.local(*dst)?,
                value: *self.tuple.get(*dimension)? as f64,
            },
            LinearOp::LoadFoldCapture { dst, index } => LinearOp::Move {
                dst: self.local(*dst)?,
                src: self.capture.checked_add(Reg::try_from(*index).ok()?)?,
            },
            LinearOp::LoadFoldCarried { dst, index } => LinearOp::Move {
                dst: self.local(*dst)?,
                src: *current.get(*index)?,
            },
            LinearOp::Const { dst, value } => LinearOp::Const {
                dst: self.local(*dst)?,
                value: *value,
            },
            LinearOp::LoadTime { dst } => LinearOp::LoadTime {
                dst: self.local(*dst)?,
            },
            LinearOp::LoadY { dst, index } => LinearOp::LoadY {
                dst: self.local(*dst)?,
                index: *index,
            },
            LinearOp::LoadP { dst, index } => LinearOp::LoadP {
                dst: self.local(*dst)?,
                index: *index,
            },
            LinearOp::Move { dst, src } => LinearOp::Move {
                dst: self.local(*dst)?,
                src: self.local(*src)?,
            },
            LinearOp::Unary { dst, op, arg } => LinearOp::Unary {
                dst: self.local(*dst)?,
                op: *op,
                arg: self.local(*arg)?,
            },
            LinearOp::Binary { dst, op, lhs, rhs } => LinearOp::Binary {
                dst: self.local(*dst)?,
                op: *op,
                lhs: self.local(*lhs)?,
                rhs: self.local(*rhs)?,
            },
            LinearOp::Compare { dst, op, lhs, rhs } => LinearOp::Compare {
                dst: self.local(*dst)?,
                op: *op,
                lhs: self.local(*lhs)?,
                rhs: self.local(*rhs)?,
            },
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => LinearOp::Select {
                dst: self.local(*dst)?,
                cond: self.local(*cond)?,
                if_true: self.local(*if_true)?,
                if_false: self.local(*if_false)?,
            },
            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn overlapping_cross_destination_preserves_snapshot_inputs() {
        let mut program = (0..6)
            .map(|dst| LinearOp::Const {
                dst,
                value: f64::from(dst + 1),
            })
            .collect::<Vec<_>>();
        program.push(LinearOp::TensorCross {
            dst_start: 0,
            lhs_start: 0,
            rhs_start: 3,
            lanes: 1,
        });
        program.extend((0..3).map(|src| LinearOp::StoreOutput { src }));
        let lowered = lower_scalar_ops(&program, None).expect("bounded primal cross projection");
        // {1,2,3} x {4,5,6} = {-3,6,-3}; both operands are read before
        // writing the overlapping destination aggregate.
        for (label, ops) in [
            ("canonical snapshot", program),
            ("scalar projection", lowered),
        ] {
            let span = rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("overlapping_cross.mo"),
                0,
                1,
            );
            let block = rumoca_ir_solve::ScalarProgramBlock::with_output_indices(
                vec![ops],
                vec![span],
                vec![0, 1, 2],
            )
            .expect("the checked constructor admits overlapping aggregate destinations");
            let mut actual = [f64::NAN; 3];
            crate::eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut actual)
                .expect("the admitted program must evaluate");
            assert_eq!(actual, [-3.0, 6.0, -3.0], "{label}");
        }
    }

    #[test]
    fn scalar_tensor_views_match_checked_reference_evaluation_and_independent_values() {
        let mut program = (0..12)
            .map(|dst| LinearOp::Const {
                dst,
                value: f64::from(dst + 1),
            })
            .collect::<Vec<_>>();
        program.extend([
            LinearOp::TensorTranspose {
                dst_start: 12,
                src_start: 0,
                rows: 3,
                columns: 2,
                element_width: 2,
                lanes: 1,
            },
            LinearOp::TensorCross {
                dst_start: 24,
                lhs_start: 0,
                rhs_start: 3,
                lanes: 1,
            },
            LinearOp::TensorIdentity {
                dst_start: 27,
                size: 3,
                lanes: 1,
            },
        ]);
        program.extend((12..36).map(|src| LinearOp::StoreOutput { src }));
        let lowered = lower_scalar_ops(&program, None).expect("bounded primal tensor views");
        let expected = [
            1.0, 2.0, 7.0, 8.0, 3.0, 4.0, 9.0, 10.0, 5.0, 6.0, 11.0, 12.0, -3.0, 6.0, -3.0, 1.0,
            0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0,
        ];
        for ops in [program, lowered] {
            let span = rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("scalar_tensor_views.mo"),
                0,
                1,
            );
            let block = rumoca_ir_solve::ScalarProgramBlock::with_output_indices(
                vec![ops],
                vec![span],
                (0..expected.len()).collect(),
            )
            .expect("checked tensor program outputs");
            let mut actual = [f64::NAN; 24];
            crate::eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut actual)
                .expect("reference evaluator accepts compact and scalar programs");
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn malformed_empty_direct_tensors_preserve_construction_refusal() {
        for (rows, columns) in [(usize::MAX, 0), (0, usize::MAX)] {
            let program = [
                LinearOp::MatrixMultiply {
                    dst_start: 0,
                    lhs_start: 0,
                    rhs_start: 0,
                    rows,
                    inner: usize::MAX,
                    columns,
                    lanes: 1,
                },
                LinearOp::TensorTranspose {
                    dst_start: 0,
                    src_start: 0,
                    rows,
                    columns,
                    element_width: usize::MAX,
                    lanes: 1,
                },
            ];
            for op in &program {
                assert!(
                    rumoca_ir_solve::ScalarProgramRegisterFlow::derive(std::slice::from_ref(op))
                        .is_err()
                );
                assert!(lower_scalar_ops(std::slice::from_ref(op), None).is_none());
            }
        }
        let program = [LinearOp::TensorTranspose {
            dst_start: 0,
            src_start: 0,
            rows: usize::MAX,
            columns: usize::MAX,
            element_width: 0,
            lanes: 1,
        }];
        assert!(rumoca_ir_solve::ScalarProgramRegisterFlow::derive(&program).is_err());
        assert!(lower_scalar_ops(&program, None).is_none());
    }

    #[test]
    fn typed_tensor_constructors_exclude_empty_and_overflowing_subshape_products() {
        use rumoca_ir_solve::{SolveScalarType, SolveValueType};
        for dimensions in [
            vec![0],
            vec![u32::MAX, u32::MAX, u32::MAX, 0],
            vec![0, u32::MAX, u32::MAX, u32::MAX],
        ] {
            assert!(SolveValueType::tensor(SolveScalarType::Boolean, dimensions).is_err());
        }
        let value_type = SolveValueType::tensor(SolveScalarType::Boolean, vec![2, 3, 4]).unwrap();
        assert_eq!(value_type.scalar_count(), 24);
        let largest = SolveValueType::tensor(SolveScalarType::Boolean, vec![u32::MAX]).unwrap();
        assert_eq!(largest.scalar_count(), u32::MAX);
    }

    #[test]
    fn scalar_template_output_ranges_are_admitted_by_expanded_output_count() {
        let program = [
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 4_000_001,
                stride: 0,
            },
        ];
        assert!(lower_scalar_ops(&program, None).is_none());
    }

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
