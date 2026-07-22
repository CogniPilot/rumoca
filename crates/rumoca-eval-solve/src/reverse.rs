//! Scalar reverse-mode AD (vector-Jacobian product) — Track A core.
//!
//! For a scalar Solve-IR program `f`, the reverse sweep computes `Jᵀλ` for an
//! output cotangent `λ` in a single pass, where `J = ∂f/∂(inputs)`. It records
//! each row's primal register values on a forward pass, then walks the ops
//! backward accumulating adjoints, reading the input cotangents at the
//! `LoadY` / `LoadP` / `LoadSeed` sites.
//!
//! Correctness gate (roadmap §9.1): the dot-product identity
//! `λᵀ(J v) = (Jᵀλ)ᵀ v` must hold against the forward JVP for random `v`, `λ`.
//!
//! Scope: scalar ops plus `LinearSolveComponent` (the linear-solve VJP — adjoint
//! `Aᵀμ = λ·e_c`). Table lookups, random generators, and runtime-indexed loads
//! are still deferred and raise an explicit error here rather than silently
//! returning a wrong gradient.

use rumoca_ir_solve::{BinaryOp, LinearOp, Reg, ScalarProgramBlock, UnaryOp};

use crate::{
    EvalSolveError, RowEvalContext, RowInputRequirements, eval_binary, eval_compare, eval_unary,
};

/// Input cotangents accumulated by a reverse sweep, one entry per scalar input
/// slot. Any slice may be empty if the caller does not need that input space
/// (e.g. a primal program has no `seed`; a forward-JVP program's `y`/`p` are the
/// fixed linearization point).
pub(crate) struct ReverseCotangents<'a> {
    pub y: &'a mut [f64],
    pub p: &'a mut [f64],
    pub seed: &'a mut [f64],
}

/// A scalar program plus the per-row register counts and cached input
/// requirements a reverse sweep needs from a
/// [`crate::prepared::PreparedScalarProgramBlock`].
pub(crate) struct ScalarVjpProgram<'a> {
    pub block: &'a ScalarProgramBlock,
    pub row_registers: &'a [usize],
    pub requirements: RowInputRequirements,
}

/// Primal evaluation point for the forward tape pass.
pub(crate) struct ReverseInputs<'a> {
    pub y: &'a [f64],
    pub p: &'a [f64],
    pub t: f64,
    pub context: RowEvalContext<'a>,
}

/// Reusable per-row register tape and adjoint buffers. Holding these across calls
/// keeps a hot reverse loop (e.g. an optimizer) allocation-free after warm-up —
/// `clear` + `resize` retains capacity. Mirrors how `SolveRuntime` reuses its
/// forward `StateDerivativeScratch`.
#[derive(Default, Clone)]
pub(crate) struct ReverseScratch {
    regs: Vec<f64>,
    adj: Vec<f64>,
}

/// Reverse-accumulate `Jᵀ · output_cotangents` of a scalar program block into
/// `cot`. Each row is an independent program; its contribution is summed into the
/// shared input cotangents.
pub(crate) fn reverse_scalar_block_vjp(
    program: &ScalarVjpProgram<'_>,
    inputs: &ReverseInputs<'_>,
    output_cotangents: &[f64],
    cot: &mut ReverseCotangents<'_>,
    scratch: &mut ReverseScratch,
) -> Result<(), EvalSolveError> {
    let block = program.block;
    // Fail loudly on inputs that cannot satisfy the program, rather than reading
    // a default for a missing slot: validate y/p/seed lengths (using the block's
    // cached requirements) and that the output cotangents cover every dense
    // output the block stores.
    crate::validate_input_requirements(
        program.requirements,
        inputs.y,
        inputs.p,
        inputs.context.seed,
    )?;
    let required_outputs = block.output_count();
    if output_cotangents.len() < required_outputs {
        return Err(EvalSolveError::OutputTooSmall {
            required: required_outputs,
            len: output_cotangents.len(),
            span: block.program_span(0),
        });
    }
    let mut output_ordinal = 0usize;
    for (row_idx, row) in block.programs.iter().enumerate() {
        let register_count = program.row_registers[row_idx];
        scratch.regs.clear();
        scratch.regs.resize(register_count, 0.0);
        scratch.adj.clear();
        scratch.adj.resize(register_count, 0.0);
        let span = block.program_span(row_idx);
        forward_row_tape(row, inputs, &mut scratch.regs)
            .map_err(|error| error.with_source_span(span))?;
        output_ordinal = seed_row_output_adjoints(
            row,
            block,
            output_ordinal,
            output_cotangents,
            &mut scratch.adj,
        );
        reverse_row_adjoints(row, &scratch.regs, &mut scratch.adj, cot)
            .map_err(|error| error.with_source_span(span))?;
    }
    Ok(())
}

/// Reverse one scalar-output row into its complete solver-`y` gradient.
///
/// A scalar residual row has one output, so reverse mode obtains every
/// `d(row)/d(y[i])` in one forward/reverse sweep. Returning `false` preserves a
/// precise fallback for rows containing operations whose reverse rule is not
/// implemented yet.
pub(crate) fn reverse_scalar_row_y_gradient(
    program: &ScalarVjpProgram<'_>,
    row_idx: usize,
    inputs: &ReverseInputs<'_>,
    y_gradient: &mut [f64],
    scratch: &mut ReverseScratch,
) -> Result<bool, EvalSolveError> {
    let Some(row) = program.block.programs.get(row_idx) else {
        return Ok(false);
    };
    let mut output_sources = row.iter().filter_map(|op| match op {
        LinearOp::StoreOutput { src } => Some(*src),
        _ => None,
    });
    let Some(output_source) = output_sources.next() else {
        return Ok(false);
    };
    if output_sources.next().is_some() || row.iter().any(|op| !reverse_row_op_supported(op)) {
        return Ok(false);
    }

    scratch.regs.clear();
    scratch.regs.resize(program.row_registers[row_idx], 0.0);
    scratch.adj.clear();
    scratch.adj.resize(program.row_registers[row_idx], 0.0);
    forward_row_tape(row, inputs, &mut scratch.regs)
        .map_err(|error| error.with_source_span(program.block.program_span(row_idx)))?;
    add_adj(&mut scratch.adj, output_source, 1.0);
    y_gradient.fill(0.0);
    reverse_row_adjoints(
        row,
        &scratch.regs,
        &mut scratch.adj,
        &mut ReverseCotangents {
            y: y_gradient,
            p: &mut [],
            seed: &mut [],
        },
    )
    .map_err(|error| error.with_source_span(program.block.program_span(row_idx)))?;
    Ok(true)
}

pub(crate) fn reverse_row_op_supported(op: &LinearOp) -> bool {
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
    )
}

/// Seed the row's `StoreOutput` register adjoints with the matching output
/// cotangents. `StoreOutput`s are visited in output-ordinal order — the same
/// order the forward sink stores them — so the running `ordinal` maps each to its
/// dense output slot via `output_indices`. Returns the advanced ordinal.
fn seed_row_output_adjoints(
    row: &[LinearOp],
    block: &ScalarProgramBlock,
    mut ordinal: usize,
    output_cotangents: &[f64],
    adj: &mut [f64],
) -> usize {
    for op in row {
        if let LinearOp::StoreOutput { src } = *op {
            // `ordinal` and the dense output slot are in bounds by block invariant
            // (one `output_indices` entry per `StoreOutput`, values < output_count)
            // and the caller's `output_cotangents` length is validated up front.
            let dense = block.output_indices[ordinal];
            add_adj(adj, src, output_cotangents[dense]);
            ordinal += 1;
        }
    }
    ordinal
}

/// Forward pass: evaluate `row` (primal semantics identical to the canonical
/// evaluator for the supported scalar ops) and record every register value.
fn forward_row_tape(
    row: &[LinearOp],
    inputs: &ReverseInputs<'_>,
    regs: &mut [f64],
) -> Result<(), EvalSolveError> {
    for op in row {
        match *op {
            LinearOp::Const { dst, value } => set(regs, dst, value),
            LinearOp::LoadTime { dst } => set(regs, dst, inputs.t),
            LinearOp::LoadY { dst, index } => set(regs, dst, load(inputs.y, "y", index)?),
            LinearOp::LoadP { dst, index } => set(regs, dst, load(inputs.p, "p", index)?),
            LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => {
                let slot = rumoca_ir_solve::resolve_indexed_slot(reg(regs, index), base, count);
                set(regs, dst, load(inputs.p, "p", slot)?);
            }
            LinearOp::LoadSeed { dst, index } => {
                let seed = inputs
                    .context
                    .seed
                    .ok_or_else(|| unsupported("LoadSeed evaluated without a seed vector"))?;
                set(regs, dst, load(seed, "seed", index)?);
            }
            LinearOp::Move { dst, src } => set(regs, dst, reg(regs, src)),
            LinearOp::Unary { dst, op, arg } => set(regs, dst, eval_unary(op, reg(regs, arg))),
            LinearOp::Binary { dst, op, lhs, rhs } => {
                set(regs, dst, eval_binary(op, reg(regs, lhs), reg(regs, rhs)));
            }
            LinearOp::Compare { dst, op, lhs, rhs } => {
                set(regs, dst, eval_compare(op, reg(regs, lhs), reg(regs, rhs)));
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let value = if reg(regs, cond) != 0.0 {
                    reg(regs, if_true)
                } else {
                    reg(regs, if_false)
                };
                set(regs, dst, value);
            }
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            } => {
                let solution = solve_linear_system(regs, matrix_start, rhs_start, n)?;
                let value = if component < n {
                    solution[component]
                } else {
                    0.0
                };
                set(regs, dst, value);
            }
            LinearOp::StoreOutput { .. } => {}
            ref other => return Err(unsupported_reverse_op(other)),
        }
    }
    Ok(())
}

/// Reverse pass: walk `row` backward, propagating each register's adjoint to its
/// operands and reading input cotangents at the load sites.
///
/// Each op that writes `dst` *consumes* its destination adjoint via [`take_adj`]
/// (read, then zero) before redistributing it. Zeroing makes the sweep correct
/// for register reuse and self-reference (`r = r * x`) — not just single
/// assignment — so it does not silently depend on the lowering staying SSA. For
/// today's SSA programs (the bump-allocated register stream) the zero is a no-op
/// observationally, since a written register is not touched again going backward.
fn reverse_row_adjoints(
    row: &[LinearOp],
    regs: &[f64],
    adj: &mut [f64],
    cot: &mut ReverseCotangents<'_>,
) -> Result<(), EvalSolveError> {
    for op in row.iter().rev() {
        match *op {
            // No destination register: nothing to consume.
            LinearOp::StoreOutput { .. } => {}
            // Destinations whose adjoint terminates: constants and time carry no
            // input dependence; comparisons are piecewise-constant (zero
            // derivative). Consume the adjoint without redistributing it.
            LinearOp::Const { dst, .. }
            | LinearOp::LoadTime { dst }
            | LinearOp::LoadIndexedP { dst, .. }
            | LinearOp::Compare { dst, .. } => {
                take_adj(adj, dst);
            }
            LinearOp::LoadY { dst, index } => accumulate(cot.y, index, take_adj(adj, dst)),
            LinearOp::LoadP { dst, index } => accumulate(cot.p, index, take_adj(adj, dst)),
            LinearOp::LoadSeed { dst, index } => accumulate(cot.seed, index, take_adj(adj, dst)),
            LinearOp::Move { dst, src } => {
                let dst_adj = take_adj(adj, dst);
                add_adj(adj, src, dst_adj);
            }
            LinearOp::Unary { dst, op, arg } => {
                let derivative = unary_derivative(op, reg(regs, arg));
                let dst_adj = take_adj(adj, dst);
                add_adj(adj, arg, dst_adj * derivative);
            }
            LinearOp::Binary { dst, op, lhs, rhs } => {
                let (dl, dr) = binary_partials(op, reg(regs, lhs), reg(regs, rhs));
                let dst_adj = take_adj(adj, dst);
                add_adj(adj, lhs, dst_adj * dl);
                add_adj(adj, rhs, dst_adj * dr);
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                // The adjoint flows to whichever branch the primal condition took.
                let dst_adj = take_adj(adj, dst);
                if reg(regs, cond) != 0.0 {
                    add_adj(adj, if_true, dst_adj);
                } else {
                    add_adj(adj, if_false, dst_adj);
                }
            }
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            } => {
                let dst_adj = take_adj(adj, dst);
                reverse_linear_solve_component(
                    regs,
                    adj,
                    LinearSolveAdjoint {
                        matrix_start,
                        rhs_start,
                        n,
                        component,
                        dst_adj,
                    },
                )?;
            }
            ref other => return Err(unsupported_reverse_op(other)),
        }
    }
    Ok(())
}

/// Inputs for the linear-solve VJP (kept in a struct to stay within the
/// argument-count budget).
struct LinearSolveAdjoint {
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    component: usize,
    /// Cotangent on the solved component `x[component]`.
    dst_adj: f64,
}

/// Solve `A x = b` (row-major `A` at `matrix_start`, `b` at `rhs_start`).
fn solve_linear_system(
    regs: &[f64],
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
) -> Result<Vec<f64>, EvalSolveError> {
    let mut x = vec![0.0_f64; n];
    crate::linear_solve::solve_all_unchecked(regs, matrix_start, rhs_start, n, &mut x)?;
    Ok(x)
}

/// Reverse a `LinearSolveComponent` (`x = A⁻¹ b`, output `x[component]`): with the
/// cotangent `dst_adj` on `x[component]`, solve the adjoint system `Aᵀ μ = dst_adj
/// · e_component` and scatter `∂x_c/∂b = (A⁻¹)ᵀ e_c = μ` onto the rhs registers and
/// `∂x_c/∂A_{ij} = −μ_i x_j` onto the matrix registers. (`A` row-major.)
fn reverse_linear_solve_component(
    regs: &[f64],
    adj: &mut [f64],
    op: LinearSolveAdjoint,
) -> Result<(), EvalSolveError> {
    let LinearSolveAdjoint {
        matrix_start,
        rhs_start,
        n,
        component,
        dst_adj,
    } = op;
    if n == 0 || component >= n || dst_adj == 0.0 {
        return Ok(());
    }
    let x = solve_linear_system(regs, matrix_start, rhs_start, n)?;
    // Aᵀ μ = dst_adj · e_component.
    let mut transposed = crate::linear_solve::AugmentedMatrix::zeroed(n)?;
    let matrix_base = matrix_start as usize;
    for i in 0..n {
        for j in 0..n {
            // (Aᵀ)[i][j] = A[j][i] = regs[matrix_start + j*n + i].
            transposed.set(i, j, regs[matrix_base + j * n + i]);
        }
        transposed.set(i, n, if i == component { dst_adj } else { 0.0 });
    }
    if crate::linear_solve::gaussian_eliminate(&mut transposed).is_none() {
        return Err(unsupported(
            "singular matrix in reverse-mode linear-solve VJP (Aᵀ not invertible)",
        ));
    }
    let rhs_base = rhs_start as usize;
    for i in 0..n {
        let mu_i = transposed.get(i, n);
        accumulate(adj, rhs_base + i, mu_i); // ∂x_c/∂b_i = μ_i
        for (j, &x_j) in x.iter().enumerate() {
            accumulate(adj, matrix_base + i * n + j, -mu_i * x_j); // ∂x_c/∂A_{ij} = −μ_i x_j
        }
    }
    Ok(())
}

/// Local derivative `d(op(x))/dx` at `x`. Piecewise-constant ops (Sign, Floor,
/// Ceil, Trunc, Not) have zero derivative.
fn unary_derivative(op: UnaryOp, x: f64) -> f64 {
    match op {
        UnaryOp::Neg => -1.0,
        UnaryOp::Abs => x.signum(),
        UnaryOp::Sqrt => guarded(0.5 / x.sqrt()),
        UnaryOp::Sin => x.cos(),
        UnaryOp::Cos => -x.sin(),
        UnaryOp::Tan => {
            let c = x.cos();
            guarded(1.0 / (c * c))
        }
        UnaryOp::Asin => guarded(1.0 / (1.0 - x * x).sqrt()),
        UnaryOp::Acos => guarded(-1.0 / (1.0 - x * x).sqrt()),
        UnaryOp::Atan => 1.0 / (1.0 + x * x),
        UnaryOp::Sinh => x.cosh(),
        UnaryOp::Cosh => x.sinh(),
        UnaryOp::Tanh => {
            let th = x.tanh();
            1.0 - th * th
        }
        UnaryOp::Exp => x.exp(),
        UnaryOp::Log => guarded(1.0 / x),
        UnaryOp::Log10 => guarded(1.0 / (x * std::f64::consts::LN_10)),
        UnaryOp::Sign | UnaryOp::Floor | UnaryOp::Ceil | UnaryOp::Trunc | UnaryOp::Not => 0.0,
    }
}

/// Local partials `(∂/∂lhs, ∂/∂rhs)` of `op(lhs, rhs)` at the primal operands.
fn binary_partials(op: BinaryOp, lhs: f64, rhs: f64) -> (f64, f64) {
    match op {
        BinaryOp::Add => (1.0, 1.0),
        BinaryOp::Sub => (1.0, -1.0),
        BinaryOp::Mul => (rhs, lhs),
        // Mirrors `guarded_division`: a zero denominator contributes no gradient.
        BinaryOp::Div => {
            if rhs == 0.0 {
                (0.0, 0.0)
            } else {
                (1.0 / rhs, -lhs / (rhs * rhs))
            }
        }
        // pow(l, r): ∂/∂l = r·l^(r-1); ∂/∂r = l^r·ln(l) (only for l > 0).
        BinaryOp::Pow => {
            let dl = guarded(rhs * lhs.powf(rhs - 1.0));
            let dr = if lhs > 0.0 {
                guarded(lhs.powf(rhs) * lhs.ln())
            } else {
                0.0
            };
            (dl, dr)
        }
        // atan2(l, r): ∂/∂l = r/(l²+r²); ∂/∂r = -l/(l²+r²).
        BinaryOp::Atan2 => {
            let denom = lhs * lhs + rhs * rhs;
            if denom == 0.0 {
                (0.0, 0.0)
            } else {
                (rhs / denom, -lhs / denom)
            }
        }
        // Min/Max are piecewise-linear: the gradient flows to the selected operand.
        BinaryOp::Min => {
            if lhs <= rhs {
                (1.0, 0.0)
            } else {
                (0.0, 1.0)
            }
        }
        BinaryOp::Max => {
            if lhs >= rhs {
                (1.0, 0.0)
            } else {
                (0.0, 1.0)
            }
        }
        // Boolean ops are non-differentiable.
        BinaryOp::And | BinaryOp::Or => (0.0, 0.0),
    }
}

/// Replace a non-finite local derivative (e.g. `1/0`, `sqrt'(0)`) with zero so a
/// boundary/degenerate primal point contributes no spurious gradient.
fn guarded(value: f64) -> f64 {
    if value.is_finite() { value } else { 0.0 }
}

// Register (regs/adj) reads and writes index statically-known slots that are
// in bounds by construction: each row's `register_count` is `required_registers`
// (max register index + 1), and the buffers are sized to it. This mirrors the
// canonical fast-path evaluator's direct indexing — there is no missing-data case
// to default away here.
fn reg(values: &[f64], r: Reg) -> f64 {
    values[r as usize]
}

fn set(regs: &mut [f64], r: Reg, value: f64) {
    regs[r as usize] = value;
}

fn add_adj(adj: &mut [f64], r: Reg, value: f64) {
    adj[r as usize] += value;
}

/// Consume a destination adjoint: return its value and reset the slot to zero.
/// Zeroing is what makes the reverse sweep independent of single-assignment — a
/// later (in forward order) write to the same register is a distinct value, so
/// its adjoint must not leak into the earlier definition we reverse next.
fn take_adj(adj: &mut [f64], r: Reg) -> f64 {
    let value = adj[r as usize];
    adj[r as usize] = 0.0;
    value
}

/// Accumulate into a *cotangent* slot, skipping indices outside the tracked
/// input space (e.g. a `LoadP` of a runtime-tail parameter that is not part of
/// the differentiated seed space). This is a deliberate scope filter, not a
/// missing-data default.
fn accumulate(values: &mut [f64], index: usize, value: f64) {
    if let Some(slot) = values.get_mut(index) {
        *slot += value;
    }
}

fn load(values: &[f64], name: &'static str, index: usize) -> Result<f64, EvalSolveError> {
    values
        .get(index)
        .copied()
        .ok_or_else(|| unsupported(&format!("{name}[{index}] out of range for reverse sweep")))
}

fn unsupported(reason: &str) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: reason.to_string(),
        span: None,
    }
}

fn unsupported_reverse_op(op: &LinearOp) -> EvalSolveError {
    unsupported(&format!(
        "reverse-mode AD does not yet support the `{}` op (Track B: tensor / table / random VJP)",
        op.kind_name()
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve::ScalarProgramBlock;

    /// Reverse over a program that *reuses and self-references* register 0:
    ///   r0 = y0; r1 = 3; r0 = r0 * r1; output = r0   (so f = 3·y0, df/dy0 = 3).
    /// A sweep that did not zero `adj[dst]` after consuming it would leak the
    /// post-multiply adjoint back into the `LoadY` and report 4 instead of 3. The
    /// lowering only emits SSA, so this non-SSA program can only arise by hand —
    /// the unit test is what guards the robustness of [`take_adj`].
    #[test]
    fn reverse_handles_register_reuse_and_self_reference() {
        let block = ScalarProgramBlock::with_output_indices(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 3.0 },
                LinearOp::Binary {
                    dst: 0,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 0 },
            ]],
            vec![rumoca_core::Span::DUMMY],
            vec![0],
        )
        .expect("valid scalar block");
        let row_registers: Vec<usize> = block
            .programs
            .iter()
            .map(|row| crate::required_registers(row).expect("register count"))
            .collect();
        let requirements =
            crate::scalar_program_block_input_requirements(&block).expect("requirements");

        let mut cot_y = [0.0_f64];
        let mut scratch = ReverseScratch::default();
        reverse_scalar_block_vjp(
            &ScalarVjpProgram {
                block: &block,
                row_registers: &row_registers,
                requirements,
            },
            &ReverseInputs {
                y: &[2.0],
                p: &[],
                t: 0.0,
                context: RowEvalContext {
                    seed: None,
                    external_tables: None,
                    runtime_state: None,
                },
            },
            &[1.0],
            &mut ReverseCotangents {
                y: &mut cot_y,
                p: &mut [],
                seed: &mut [],
            },
            &mut scratch,
        )
        .expect("reverse sweep");

        assert!(
            (cot_y[0] - 3.0).abs() < 1.0e-12,
            "df/dy0 should be 3 (register reuse handled), got {}",
            cot_y[0]
        );
    }

    /// Reverse VJP through a `LinearSolveComponent` (`x = A⁻¹ b`). The 2x2 system's
    /// `A`/`b` are loaded from solver-y, so `x[0]` is a function of `y`; the reverse
    /// `∂x0/∂y` must match a finite-difference of the forward solve.
    #[test]
    fn reverse_linear_solve_component_matches_finite_difference() {
        // regs: 0..4 = A row-major [[A00,A01],[A10,A11]], 4..6 = b, 6 = x[0].
        let row = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadY { dst: 1, index: 1 },
            LinearOp::LoadY { dst: 2, index: 2 },
            LinearOp::LoadY { dst: 3, index: 3 },
            LinearOp::LoadY { dst: 4, index: 4 },
            LinearOp::LoadY { dst: 5, index: 5 },
            LinearOp::LinearSolveComponent {
                dst: 6,
                matrix_start: 0,
                rhs_start: 4,
                n: 2,
                component: 0,
            },
            LinearOp::StoreOutput { src: 6 },
        ];
        let block = ScalarProgramBlock::with_output_indices(
            vec![row],
            vec![rumoca_core::Span::DUMMY],
            vec![0],
        )
        .expect("valid block");
        let row_registers: Vec<usize> = block
            .programs
            .iter()
            .map(|row| crate::required_registers(row).expect("registers"))
            .collect();
        let requirements =
            crate::scalar_program_block_input_requirements(&block).expect("requirements");
        let program = ScalarVjpProgram {
            block: &block,
            row_registers: &row_registers,
            requirements,
        };

        // A = [[2,1],[1,3]], b = [1,1]  ⇒  x = [0.4, 0.2].
        let y = [2.0_f64, 1.0, 1.0, 3.0, 1.0, 1.0];

        // Forward x[0] for a given y, by replaying the row's tape.
        let forward_x0 = |y: &[f64]| -> f64 {
            let mut regs = vec![0.0_f64; row_registers[0]];
            forward_row_tape(
                &block.programs[0],
                &ReverseInputs {
                    y,
                    p: &[],
                    t: 0.0,
                    context: RowEvalContext {
                        seed: None,
                        external_tables: None,
                        runtime_state: None,
                    },
                },
                &mut regs,
            )
            .expect("forward");
            regs[6]
        };
        assert!((forward_x0(&y) - 0.4).abs() < 1.0e-12, "x[0] should be 0.4");

        // Reverse ∂x0/∂y.
        let mut cot_y = [0.0_f64; 6];
        let mut scratch = ReverseScratch::default();
        reverse_scalar_block_vjp(
            &program,
            &ReverseInputs {
                y: &y,
                p: &[],
                t: 0.0,
                context: RowEvalContext {
                    seed: None,
                    external_tables: None,
                    runtime_state: None,
                },
            },
            &[1.0],
            &mut ReverseCotangents {
                y: &mut cot_y,
                p: &mut [],
                seed: &mut [],
            },
            &mut scratch,
        )
        .expect("reverse sweep");

        // Central finite differences of the forward solve.
        let h = 1.0e-6;
        for i in 0..6 {
            let mut yp = y;
            let mut ym = y;
            yp[i] += h;
            ym[i] -= h;
            let fd = (forward_x0(&yp) - forward_x0(&ym)) / (2.0 * h);
            assert!(
                (cot_y[i] - fd).abs() < 1.0e-6,
                "∂x0/∂y[{i}]: reverse={}, finite-diff={fd}",
                cot_y[i]
            );
        }
    }
}
