//! Scalar reverse-mode AD (vector-Jacobian product).
//!
//! For a scalar Solve-IR program `f`, the reverse sweep computes `Jᵀλ` for an
//! output cotangent `λ` in a single pass, where `J = ∂f/∂(inputs)`. It records
//! each row's primal register values on a forward pass, then walks the ops
//! backward accumulating adjoints, reading the input cotangents at the
//! `LoadY` / `LoadP` / `LoadSeed` sites.
//!
//! The dot-product identity
//! `λᵀ(J v) = (Jᵀλ)ᵀ v` must hold against the forward JVP for random `v`, `λ`.
//!
//! Scope: scalar ops plus `LinearSolveComponent` (the linear-solve VJP — adjoint
//! `Aᵀμ = λ·e_c`). Table lookups, random generators, and runtime-indexed loads
//! are still deferred and raise an explicit error here rather than silently
//! returning a wrong gradient.
//!
//! The reverse-sweep types are `pub` only so that
//! `rumoca_solver::runtime::solve_runtime` (the runtime state machine that owns
//! the scratch buffers) can drive them; they are not a general-purpose API.
//!
//! # Kink rules
//!
//! Four sites differentiate the same operations: these reverse rows, the
//! forward dual lowering (`rumoca_phase_solve::ad`, including the fused
//! dual-lane tensor division the evaluator, the native backend, and the C
//! kernel execute), the typed directional owner of a pure call
//! (`rumoca_ir_solve` `typed_program::program::directional`), and the
//! generated C kernel, which renders the forward and directional programs. The
//! projection solver assembles a block matrix from reverse rows and certifies
//! it with forward Jacobian-vector products, so the sites must agree wherever
//! an operation is not differentiable. This table is the one statement of
//! those rules; the other sites form the same local partials in emitted
//! operations and cite it.
//!
//! A partial the table qualifies "when finite" contributes zero wherever it is
//! not finite (a vertical tangent, a pole, a point outside the real domain, an
//! overflow, or an underflowing denominator), never a non-finite value: a NaN
//! or infinite tangent would poison the certification of every coordinate it
//! reaches. The remaining partials (`sin`, `cos`, `tan`, `atan`, `sinh`,
//! `cosh`, `tanh`, `exp`, and the product rule) are finite wherever their
//! operands are finite and the primal does not overflow; where they are not,
//! every site produces the same non-finite value. Each rule is a function of
//! the primal operands alone, never of which operands carry a tangent, so
//! forward products stay linear in the direction.
//!
//! | Operation | Local partials, and the rule where they do not exist |
//! |---|---|
//! | `abs(x)` | `+1` when `x >= 0`, including `-0.0`; `-1` otherwise, including NaN |
//! | `sign`, `floor`, `ceil`, `trunc`, `not`, `and`, `or`, comparisons | `0` everywhere, including at jumps; `integer`, `div`, `mod`, and `rem` lower through these |
//! | `min(l, r)` / `max(l, r)` | the operand the comparison `l <= r` / `l >= r` selects; ties select `l`, and a NaN operand selects `r` |
//! | `if` / `noEvent` / `smooth` | the branch the primal condition selects |
//! | `sqrt(x)` | `0.5 / sqrt(x)` when finite, else `0` (so `x <= 0` gives `0`) |
//! | `asin(x)` / `acos(x)` | `±1 / sqrt(1 - x²)` when finite, else `0` (so `|x| >= 1` gives `0`) |
//! | `log(x)` / `log10(x)` | `1 / x` / `1 / (x ln 10)` when finite, else `0` (so `x = 0` and a subnormal `x` give `0`) |
//! | `tan(x)` | `1 / cos²(x)`, finite for every finite `x`; NaN at a non-finite `x` |
//! | `tanh(x)` | `1 / cosh²(x)`, which reaches `0` when `cosh` overflows |
//! | `l / r` | `1 / r` and `-l / r²`, each when finite, else `0` (so `r = 0` gives `(0, 0)`, and a subnormal or huge `r` whose square underflows or overflows zeroes the partial that does) |
//! | `atan2(l, r)` | `r / (l² + r²)` and `-l / (l² + r²)`, each when finite, else `0` (so the origin gives `(0, 0)`) |
//! | `pow(l, r)` | `∂l = r·l^(r-1)` when finite, else `0` (so `l < 0` with a non-integer `r` gives `0`, and `l = 0` gives `1` at `r = 1` and `0` otherwise); `∂r = l^r·ln(l)` when `l > 0` and finite, else `0` |
//!
//! `rumoca_phase_solve`'s `kink_rule_tests` pin the reverse, forward, and typed
//! directional sites together at every row of this table, and the
//! `derivative_kinks` harness of `suite_template_runtime` pins the generated C
//! kernel's block Jacobians at the same kinks.

use rumoca_ir_solve::{BinaryOp, LinearOp, Reg, ScalarProgramBlock, UnaryOp};

use crate::{
    EvalSolveError, RowEvalContext, RowInputRequirements, eval_binary, eval_compare, eval_unary,
};

/// Input cotangents accumulated by a reverse sweep, one entry per scalar input
/// slot. Any slice may be empty if the caller does not need that input space
/// (e.g. a primal program has no `seed`; a forward-JVP program's `y`/`p` are the
/// fixed linearization point).
pub struct ReverseCotangents<'a> {
    pub y: &'a mut [f64],
    pub p: &'a mut [f64],
    pub seed: &'a mut [f64],
}

/// A scalar program plus the per-row register counts and cached input
/// requirements a reverse sweep needs from a
/// [`crate::prepared::PreparedScalarProgramBlock`].
pub struct ScalarVjpProgram<'a> {
    pub block: &'a ScalarProgramBlock,
    pub row_registers: &'a [usize],
    pub requirements: RowInputRequirements,
}

/// Primal evaluation point for the forward tape pass.
pub struct ReverseInputs<'a> {
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
pub struct ReverseScratch {
    regs: Vec<f64>,
    adj: Vec<f64>,
}

/// Reverse-accumulate `Jᵀ · output_cotangents` of a scalar program block into
/// `cot`. Each row is an independent program; its contribution is summed into the
/// shared input cotangents.
pub fn reverse_scalar_block_vjp(
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
    for (row_idx, row) in block.programs().iter().enumerate() {
        let register_count = program.row_registers[row_idx];
        scratch.prepare(register_count);
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
        debug_assert!(scratch.adj.iter().all(|value| *value == 0.0));
    }
    Ok(())
}

/// Reverse one scalar-output row into its complete solver-`y` gradient.
///
/// A scalar residual row has one output, so reverse mode obtains every
/// `d(row)/d(y[i])` in one forward/reverse sweep. Returning `false` preserves a
/// precise fallback for rows containing operations whose reverse rule is not
/// implemented yet.
pub fn reverse_scalar_row_y_gradient(
    program: &ScalarVjpProgram<'_>,
    row_idx: usize,
    inputs: &ReverseInputs<'_>,
    y_gradient: &mut [f64],
    scratch: &mut ReverseScratch,
) -> Result<bool, EvalSolveError> {
    let Some(row) = program.block.programs().get(row_idx) else {
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

    scratch.prepare(program.row_registers[row_idx]);
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
    debug_assert!(scratch.adj.iter().all(|value| *value == 0.0));
    Ok(true)
}

impl ReverseScratch {
    fn prepare(&mut self, register_count: usize) {
        self.regs.resize(register_count, 0.0);
        self.adj.resize(register_count, 0.0);
        debug_assert!(self.adj.iter().all(|value| *value == 0.0));
    }
}

pub fn reverse_row_op_supported(op: &LinearOp) -> bool {
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
            let dense = block.output_indices()[ordinal];
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
            | LinearOp::Compare { dst, .. } => {
                take_adj(adj, dst);
            }
            LinearOp::LoadY { dst, index } => accumulate(cot.y, index, take_adj(adj, dst)),
            LinearOp::LoadP { dst, index } => accumulate(cot.p, index, take_adj(adj, dst)),
            LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => {
                let slot = rumoca_ir_solve::resolve_indexed_slot(reg(regs, index), base, count);
                accumulate(cot.p, slot, take_adj(adj, dst));
            }
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
    crate::linear_solve::solve_all_unchecked(
        regs,
        matrix_start,
        rhs_start,
        n,
        crate::tensor_policy::LinearSolveKernel::Dense,
        None,
        &mut x,
    )?;
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
        // The forward rules select `du` when `x >= 0`, so the kink and a
        // negative zero take the right derivative; `f64::signum` would give
        // `-1` for `-0.0` and make reverse rows disagree with forward JVPs.
        UnaryOp::Abs => {
            if x >= 0.0 {
                1.0
            } else {
                -1.0
            }
        }
        UnaryOp::Sqrt => guarded(0.5 / x.sqrt()),
        UnaryOp::Sin => x.cos(),
        UnaryOp::Cos => -x.sin(),
        // Finite for every finite `x`; NaN at a non-finite `x`, as at the
        // forward sites, which apply it to aggregates without a guard.
        UnaryOp::Tan => {
            let c = x.cos();
            1.0 / (c * c)
        }
        UnaryOp::Asin => guarded(1.0 / (1.0 - x * x).sqrt()),
        UnaryOp::Acos => guarded(-1.0 / (1.0 - x * x).sqrt()),
        UnaryOp::Atan => 1.0 / (1.0 + x * x),
        UnaryOp::Sinh => x.cosh(),
        UnaryOp::Cosh => x.sinh(),
        // The forward rules divide by `cosh²`; `1 - tanh²` would round to
        // zero long before `cosh` overflows.
        UnaryOp::Tanh => {
            let c = x.cosh();
            1.0 / (c * c)
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
        // Each partial when finite: a zero denominator, a subnormal one whose
        // square underflows, and an overflowing `r²` all zero the partial
        // that does not exist rather than poisoning the product.
        BinaryOp::Div => (guarded(1.0 / rhs), guarded(-lhs / (rhs * rhs))),
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
        // atan2(l, r): ∂/∂l = r/(l²+r²); ∂/∂r = -l/(l²+r²), each when finite
        // (the origin, where both are 0/0, contributes nothing).
        BinaryOp::Atan2 => {
            let denom = lhs * lhs + rhs * rhs;
            (guarded(rhs / denom), guarded(-lhs / denom))
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

/// The forward tangent of `l / r` under the division kink rule, for the fused
/// dual-lane tensor division: each local partial when finite, else zero,
/// accumulated in the order the scalar forward lowering emits.
#[must_use]
pub fn division_tangent(lhs: f64, lhs_du: f64, rhs: f64, rhs_du: f64) -> f64 {
    let (dl, dr) = binary_partials(BinaryOp::Div, lhs, rhs);
    lhs_du * dl + rhs_du * dr
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

    fn fixture_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 0, 1)
    }

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
            vec![fixture_span()],
            vec![0],
        )
        .expect("valid scalar block");
        let row_registers: Vec<usize> = block
            .programs()
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
                context: RowEvalContext::default(),
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

    /// `f(y0) = abs(-y0)` at `y0 = 0`: the negation yields `-0.0`. The forward
    /// dual rule selects the tangent when the operand is `>= 0`, so the forward
    /// derivative is `-1`; the reverse row must agree, or a Jacobian assembled
    /// from reverse rows disagrees with the forward JVP that certifies it. An
    /// alias quotient that reads an eliminated member as `-representative`
    /// produces exactly this `-0.0` at an all-zero operating point.
    #[test]
    fn reverse_abs_at_negative_zero_matches_the_forward_rule() {
        let block = ScalarProgramBlock::with_output_indices(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::Unary {
                    dst: 1,
                    op: UnaryOp::Neg,
                    arg: 0,
                },
                LinearOp::Unary {
                    dst: 2,
                    op: UnaryOp::Abs,
                    arg: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            vec![fixture_span()],
            vec![0],
        )
        .expect("valid scalar block");
        let row_registers =
            [crate::required_registers(&block.programs()[0]).expect("register count")];
        let requirements =
            crate::scalar_program_block_input_requirements(&block).expect("requirements");
        let mut cot_y = [0.0];
        reverse_scalar_block_vjp(
            &ScalarVjpProgram {
                block: &block,
                row_registers: &row_registers,
                requirements,
            },
            &ReverseInputs {
                y: &[0.0],
                p: &[],
                t: 0.0,
                context: RowEvalContext::default(),
            },
            &[1.0],
            &mut ReverseCotangents {
                y: &mut cot_y,
                p: &mut [],
                seed: &mut [],
            },
            &mut ReverseScratch::default(),
        )
        .expect("reverse sweep");
        assert_eq!(
            cot_y,
            [-1.0],
            "reverse abs at -0.0 takes the forward branch"
        );
    }

    #[test]
    fn reverse_indexed_parameter_load_accumulates_selected_slot() {
        let block = ScalarProgramBlock::with_output_indices(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::LoadIndexedP {
                    dst: 1,
                    base: 1,
                    count: 3,
                    index: 0,
                },
                LinearOp::StoreOutput { src: 1 },
            ]],
            vec![fixture_span()],
            vec![0],
        )
        .expect("valid indexed-parameter block");
        let row_registers =
            [crate::required_registers(&block.programs()[0]).expect("register count")];
        let requirements =
            crate::scalar_program_block_input_requirements(&block).expect("requirements");
        let mut cot_y = [0.0];
        let mut cot_p = [0.0; 4];
        let mut scratch = ReverseScratch::default();

        reverse_scalar_block_vjp(
            &ScalarVjpProgram {
                block: &block,
                row_registers: &row_registers,
                requirements,
            },
            &ReverseInputs {
                y: &[1.0],
                p: &[10.0, 20.0, 30.0, 40.0],
                t: 0.0,
                context: RowEvalContext::default(),
            },
            &[2.5],
            &mut ReverseCotangents {
                y: &mut cot_y,
                p: &mut cot_p,
                seed: &mut [],
            },
            &mut scratch,
        )
        .expect("reverse sweep");

        assert_eq!(cot_y, [0.0]);
        assert_eq!(cot_p, [0.0, 0.0, 2.5, 0.0]);
        let parameter_direction = [1.0, -2.0, 3.0, -4.0];
        let forward_contraction = 2.5 * parameter_direction[2];
        let reverse_contraction: f64 = cot_p
            .iter()
            .zip(parameter_direction)
            .map(|(cotangent, direction)| cotangent * direction)
            .sum();
        assert_eq!(forward_contraction, reverse_contraction);
    }

    /// Reverse VJP through a `LinearSolveComponent` (`x = A⁻¹ b`). The 2x2 system's
    /// `A`/`b` are loaded from solver-y, so `x[0]` is a function of `y`; the reverse
    /// `∂x0/∂y` must match the analytic derivative of the solve: `∂x/∂b = A⁻¹` and
    /// `∂x/∂A_ij = -A⁻¹ e_i x_j`.
    #[test]
    fn reverse_linear_solve_component_matches_the_analytic_derivative() {
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
        let block =
            ScalarProgramBlock::with_output_indices(vec![row], vec![fixture_span()], vec![0])
                .expect("valid block");
        let row_registers: Vec<usize> = block
            .programs()
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
                &block.programs()[0],
                &ReverseInputs {
                    y,
                    p: &[],
                    t: 0.0,
                    context: RowEvalContext::default(),
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
                context: RowEvalContext::default(),
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

        // A⁻¹ = [[3, -1], [-1, 2]] / 5, so row 0 of A⁻¹ is [0.6, -0.2], and y is
        // ordered [A00, A01, A10, A11, b0, b1].
        let expected = [-0.24, -0.12, 0.08, 0.04, 0.6, -0.2];
        for (i, expected) in expected.iter().enumerate() {
            assert!(
                (cot_y[i] - expected).abs() < 1.0e-12,
                "∂x0/∂y[{i}]: reverse={}, analytic={expected}",
                cot_y[i]
            );
        }
    }
}
