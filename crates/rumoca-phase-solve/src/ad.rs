//! AD lowering from primal linear ops to forward-mode J·v ops.

use crate::lower::{LowerError, lower_initial_residual, lower_residual};
use rumoca_ir_solve::{
    BinaryOp, CompareOp, ComputeBlock, ComputeNode, LinearOp, Reg, ScalarProgramBlock, UnaryOp,
    VarLayout,
};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
struct DualReg {
    re: Reg,
    du: Reg,
}

pub fn lower_residual_ad(
    dae_model: &rumoca_ir_dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let primal_rows = lower_residual(dae_model, layout)?;
    lower_scalar_program_block_ad(&primal_rows)
}

pub fn lower_initial_residual_ad(
    dae_model: &rumoca_ir_dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let primal_rows = lower_initial_residual(dae_model, layout)?;
    lower_scalar_program_block_ad(&primal_rows)
}

pub fn lower_residual_full_ad(
    dae_model: &rumoca_ir_dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let primal_rows = lower_residual(dae_model, layout)?;
    lower_scalar_program_block_full_ad(&primal_rows, layout)
}

pub fn lower_initial_residual_full_ad(
    dae_model: &rumoca_ir_dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let primal_rows = lower_initial_residual(dae_model, layout)?;
    lower_scalar_program_block_full_ad(&primal_rows, layout)
}

/// Compute the forward-mode JVP of a `ComputeBlock`, preserving tensor structure.
///
/// For `ScalarPrograms` nodes: applies the existing scalar AD pass row-by-row.
/// For `MatMul` nodes: preserves tensor structure when one operand is constant
/// with respect to solver-y and substitutes `LoadSeed` into the variable
/// operand. Constant products lower to explicit zero scalar rows. If both
/// operands depend on solver-y, the product rule currently falls back to scalar
/// AD until Solve IR has a tensor add node.
/// For `LinSolve` nodes: emits another `LinSolve` using
/// `dx = A^{-1}(db - dA * x)` so coupled systems keep tensor solve structure.
pub fn lower_compute_block_jvp(block: &ComputeBlock) -> Result<ComputeBlock, LowerError> {
    let mut jvp_nodes = Vec::with_capacity(block.nodes.len());
    for node in &block.nodes {
        match node {
            ComputeNode::ScalarPrograms(rows) => {
                let jvp_rows = lower_scalar_program_block_ad(&rows.programs)?;
                jvp_nodes.push(ComputeNode::ScalarPrograms(
                    ScalarProgramBlock::with_program_spans(jvp_rows, rows.program_spans.clone()),
                ));
            }
            ComputeNode::MatMul {
                lhs_ops,
                lhs_start,
                rhs_ops,
                rhs_start,
                m,
                k,
                n,
                lhs_sparsity,
                rhs_sparsity,
                metadata,
                span,
            } => {
                let lhs_reads_y = ops_reference_y(lhs_ops);
                let rhs_reads_y = ops_reference_y(rhs_ops);
                match (lhs_reads_y, rhs_reads_y) {
                    (false, true) => {
                        let jvp_rhs_ops = substitute_load_y_with_seed(rhs_ops);
                        jvp_nodes.push(ComputeNode::MatMul {
                            lhs_ops: lhs_ops.clone(),
                            lhs_start: *lhs_start,
                            rhs_ops: jvp_rhs_ops,
                            rhs_start: *rhs_start,
                            m: *m,
                            k: *k,
                            n: *n,
                            lhs_sparsity: lhs_sparsity.clone(),
                            rhs_sparsity: rhs_sparsity.clone(),
                            metadata: metadata.clone(),
                            span: *span,
                        });
                    }
                    (true, false) => {
                        let jvp_lhs_ops = substitute_load_y_with_seed(lhs_ops);
                        jvp_nodes.push(ComputeNode::MatMul {
                            lhs_ops: jvp_lhs_ops,
                            lhs_start: *lhs_start,
                            rhs_ops: rhs_ops.clone(),
                            rhs_start: *rhs_start,
                            m: *m,
                            k: *k,
                            n: *n,
                            lhs_sparsity: lhs_sparsity.clone(),
                            rhs_sparsity: rhs_sparsity.clone(),
                            metadata: metadata.clone(),
                            span: *span,
                        });
                    }
                    (false, false) => {
                        jvp_nodes.push(ComputeNode::ScalarPrograms(zero_scalar_program_block(
                            *m * *n,
                            *span,
                        )));
                    }
                    (true, true) => {
                        let scalar = ComputeBlock {
                            nodes: vec![node.clone()],
                        };
                        let scalar_programs = rumoca_eval_solve::to_scalar_program_block(&scalar);
                        let jvp_rows = lower_scalar_program_block_ad(&scalar_programs.programs)?;
                        jvp_nodes.push(ComputeNode::ScalarPrograms(
                            ScalarProgramBlock::with_program_spans(
                                jvp_rows,
                                scalar_programs.program_spans,
                            ),
                        ));
                    }
                }
            }
            ComputeNode::LinSolve {
                setup_ops,
                matrix_start,
                rhs_start,
                n,
                metadata,
                span,
                ..
            } => jvp_nodes.push(lower_linsolve_jvp_node(
                setup_ops,
                *matrix_start,
                *rhs_start,
                *n,
                metadata.clone(),
                *span,
            )?),
        }
    }
    Ok(ComputeBlock { nodes: jvp_nodes })
}

/// Returns true if any op in the slice is `LoadY`.
fn ops_reference_y(ops: &[LinearOp]) -> bool {
    ops.iter().any(|op| matches!(op, LinearOp::LoadY { .. }))
}

/// Replace every `LoadY { dst, index }` with `LoadSeed { dst, index }`.
/// All other ops are passed through unchanged.
fn substitute_load_y_with_seed(ops: &[LinearOp]) -> Vec<LinearOp> {
    ops.iter()
        .map(|op| match *op {
            LinearOp::LoadY { dst, index } => LinearOp::LoadSeed { dst, index },
            other => other,
        })
        .collect()
}

fn zero_scalar_program_block(row_count: usize, span: rumoca_core::Span) -> ScalarProgramBlock {
    let rows = (0..row_count)
        .map(|_| {
            vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::StoreOutput { src: 0 },
            ]
        })
        .collect();
    ScalarProgramBlock::with_source_span(rows, span)
}

fn lower_linsolve_jvp_node(
    setup_ops: &[LinearOp],
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    metadata: rumoca_ir_solve::TensorNodeMetadata,
    span: rumoca_core::Span,
) -> Result<ComputeNode, LowerError> {
    if n == 0 {
        return Err(unsupported("invalid zero-sized LinSolve in JVP"));
    }

    let mut builder = AdBuilder::new(SeedMode::SolverYOnly);
    for op in setup_ops {
        builder.lower_op(*op)?;
    }

    let matrix = (0..n * n)
        .map(|idx| builder.lookup(matrix_start + idx as Reg))
        .collect::<Result<Vec<_>, _>>()?;
    let rhs = (0..n)
        .map(|idx| builder.lookup(rhs_start + idx as Reg))
        .collect::<Result<Vec<_>, _>>()?;

    let matrix_re = matrix.iter().map(|entry| entry.re).collect::<Vec<_>>();
    let rhs_re = rhs.iter().map(|entry| entry.re).collect::<Vec<_>>();
    let matrix_re_start = builder.pack_registers(&matrix_re);
    let rhs_re_start = builder.pack_registers(&rhs_re);

    let mut solution = Vec::with_capacity(n);
    for component in 0..n {
        let dst = builder.alloc_reg();
        builder.ops.push(LinearOp::LinearSolveComponent {
            dst,
            matrix_start: matrix_re_start,
            rhs_start: rhs_re_start,
            n,
            component,
        });
        solution.push(dst);
    }

    let mut tangent_rhs = Vec::with_capacity(n);
    for row in 0..n {
        let mut acc = rhs[row].du;
        for col in 0..n {
            let matrix_du = matrix[row * n + col].du;
            let product = builder.emit_binary(BinaryOp::Mul, matrix_du, solution[col]);
            acc = builder.emit_binary(BinaryOp::Sub, acc, product);
        }
        tangent_rhs.push(acc);
    }
    let tangent_rhs_start = builder.pack_registers(&tangent_rhs);
    let next_reg = builder.next_reg;

    Ok(ComputeNode::LinSolve {
        setup_ops: builder.ops,
        matrix_start: matrix_re_start,
        rhs_start: tangent_rhs_start,
        n,
        next_reg,
        metadata,
        span,
    })
}

pub fn lower_scalar_program_block_ad(
    primal_rows: &[Vec<LinearOp>],
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    primal_rows
        .iter()
        .map(|row| lower_row_ad(row, SeedMode::SolverYOnly))
        .collect()
}

pub fn lower_scalar_program_block_full_ad(
    primal_rows: &[Vec<LinearOp>],
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    primal_rows
        .iter()
        .map(|row| {
            lower_row_ad(
                row,
                SeedMode::SolverYAndP {
                    p_seed_offset: layout.y_scalars(),
                },
            )
        })
        .collect()
}

fn lower_row_ad(primal_ops: &[LinearOp], seed_mode: SeedMode) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = AdBuilder::new(seed_mode);
    for op in primal_ops {
        builder.lower_op(*op)?;
    }
    Ok(builder.ops)
}

#[derive(Clone, Copy, Default)]
enum SeedMode {
    #[default]
    SolverYOnly,
    SolverYAndP {
        p_seed_offset: usize,
    },
}

#[derive(Default)]
struct AdBuilder {
    ops: Vec<LinearOp>,
    next_reg: Reg,
    map: HashMap<Reg, DualReg>,
    cached_zero: Option<Reg>,
    cached_one: Option<Reg>,
    cached_ln10: Option<Reg>,
    cached_two: Option<Reg>,
    seed_mode: SeedMode,
}

impl AdBuilder {
    fn new(seed_mode: SeedMode) -> Self {
        Self {
            seed_mode,
            ..Self::default()
        }
    }

    fn lower_op(&mut self, op: LinearOp) -> Result<(), LowerError> {
        match op {
            LinearOp::Const { dst, value } => self.lower_const(dst, value),
            LinearOp::LoadTime { dst } => self.lower_load_time(dst),
            LinearOp::LoadY { dst, index } => self.lower_load_y(dst, index),
            LinearOp::LoadP { dst, index } => self.lower_load_p(dst, index),
            LinearOp::LoadSeed { .. } => Err(unsupported("unexpected LoadSeed in primal row")),
            LinearOp::Move { dst, src } => self.lower_move(dst, src),
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            } => self.lower_linear_solve_component(dst, matrix_start, rhs_start, n, component),
            LinearOp::TableBounds { dst, table_id, max } => {
                self.lower_table_bounds(dst, table_id, max)
            }
            LinearOp::TableLookup {
                dst,
                table_id,
                column,
                input,
            } => self.lower_table_lookup(dst, table_id, column, input),
            LinearOp::TableLookupSlope { .. } => {
                Err(unsupported("unexpected TableLookupSlope in primal row"))
            }
            LinearOp::TableNextEvent {
                dst,
                table_id,
                time,
            } => self.lower_table_next_event(dst, table_id, time),
            LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. } => {
                Err(unsupported("random solve-IR ops are discrete-only"))
            }
            LinearOp::Unary { dst, op, arg } => self.lower_unary(dst, op, arg),
            LinearOp::Binary { dst, op, lhs, rhs } => self.lower_binary(dst, op, lhs, rhs),
            LinearOp::Compare { dst, op, lhs, rhs } => self.lower_compare(dst, op, lhs, rhs),
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => self.lower_select(dst, cond, if_true, if_false),
            LinearOp::StoreOutput { src } => self.lower_store(src),
        }
    }

    fn lower_const(&mut self, dst: Reg, value: f64) -> Result<(), LowerError> {
        let re = self.emit_const(value);
        let du = self.zero_reg();
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_time(&mut self, dst: Reg) -> Result<(), LowerError> {
        let re = self.emit_load_time();
        let du = self.zero_reg();
        self.bind(dst, DualReg { re, du })
    }

    fn lower_move(&mut self, dst: Reg, src: Reg) -> Result<(), LowerError> {
        let value = self.lookup(src)?;
        self.bind(dst, value)
    }

    fn lower_linear_solve_component(
        &mut self,
        dst: Reg,
        matrix_start: Reg,
        rhs_start: Reg,
        n: usize,
        component: usize,
    ) -> Result<(), LowerError> {
        if n == 0 || component >= n {
            return Err(unsupported("invalid LinearSolveComponent shape in AD row"));
        }

        let matrix = (0..n * n)
            .map(|idx| self.lookup(matrix_start + idx as Reg))
            .collect::<Result<Vec<_>, _>>()?;
        let rhs = (0..n)
            .map(|idx| self.lookup(rhs_start + idx as Reg))
            .collect::<Result<Vec<_>, _>>()?;

        let matrix_re = matrix.iter().map(|entry| entry.re).collect::<Vec<_>>();
        let rhs_re = rhs.iter().map(|entry| entry.re).collect::<Vec<_>>();
        let matrix_start_re = self.pack_registers(&matrix_re);
        let rhs_start_re = self.pack_registers(&rhs_re);

        let mut solution_regs = Vec::with_capacity(n);
        for solution_component in 0..n {
            let solution = self.alloc_reg();
            self.ops.push(LinearOp::LinearSolveComponent {
                dst: solution,
                matrix_start: matrix_start_re,
                rhs_start: rhs_start_re,
                n,
                component: solution_component,
            });
            solution_regs.push(solution);
        }

        let mut tangent_rhs = Vec::with_capacity(n);
        for row in 0..n {
            let mut acc = rhs[row].du;
            for col in 0..n {
                let matrix_du = matrix[row * n + col].du;
                let product = self.emit_binary(BinaryOp::Mul, matrix_du, solution_regs[col]);
                acc = self.emit_binary(BinaryOp::Sub, acc, product);
            }
            tangent_rhs.push(acc);
        }

        let tangent_rhs_start = self.pack_registers(&tangent_rhs);
        let du = self.alloc_reg();
        self.ops.push(LinearOp::LinearSolveComponent {
            dst: du,
            matrix_start: matrix_start_re,
            rhs_start: tangent_rhs_start,
            n,
            component,
        });

        self.bind(
            dst,
            DualReg {
                re: solution_regs[component],
                du,
            },
        )
    }

    fn lower_load_y(&mut self, dst: Reg, index: usize) -> Result<(), LowerError> {
        let re = self.emit_load_y(index);
        let du = self.emit_load_seed(index);
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_p(&mut self, dst: Reg, index: usize) -> Result<(), LowerError> {
        let re = self.emit_load_p(index);
        let du = match self.seed_mode {
            SeedMode::SolverYOnly => self.zero_reg(),
            SeedMode::SolverYAndP { .. } => self.emit_load_seed(self.p_seed_index(index)),
        };
        self.bind(dst, DualReg { re, du })
    }

    fn lower_table_bounds(&mut self, dst: Reg, table_id: Reg, max: bool) -> Result<(), LowerError> {
        let table = self.lookup(table_id)?;
        let re = self.emit_table_bounds(table.re, max);
        let du = self.zero_reg();
        self.bind(dst, DualReg { re, du })
    }

    fn lower_table_lookup(
        &mut self,
        dst: Reg,
        table_id: Reg,
        column: Reg,
        input: Reg,
    ) -> Result<(), LowerError> {
        let table = self.lookup(table_id)?;
        let column = self.lookup(column)?;
        let input = self.lookup(input)?;
        let re = self.emit_table_lookup(table.re, column.re, input.re);
        let slope = self.emit_table_lookup_slope(table.re, column.re, input.re);
        let du = self.emit_binary(BinaryOp::Mul, slope, input.du);
        self.bind(dst, DualReg { re, du })
    }

    fn lower_table_next_event(
        &mut self,
        dst: Reg,
        table_id: Reg,
        time: Reg,
    ) -> Result<(), LowerError> {
        let table = self.lookup(table_id)?;
        let time = self.lookup(time)?;
        let re = self.emit_table_next_event(table.re, time.re);
        let du = self.zero_reg();
        self.bind(dst, DualReg { re, du })
    }

    fn lower_unary(&mut self, dst: Reg, op: UnaryOp, arg: Reg) -> Result<(), LowerError> {
        let x = self.lookup(arg)?;
        let out = self.unary_dual(op, x)?;
        self.bind(dst, out)
    }

    fn lower_binary(
        &mut self,
        dst: Reg,
        op: BinaryOp,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<(), LowerError> {
        let l = self.lookup(lhs)?;
        let r = self.lookup(rhs)?;
        let out = self.binary_dual(op, l, r)?;
        self.bind(dst, out)
    }

    fn lower_compare(
        &mut self,
        dst: Reg,
        op: CompareOp,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<(), LowerError> {
        let l = self.lookup(lhs)?;
        let r = self.lookup(rhs)?;
        let re = self.emit_compare(op, l.re, r.re);
        let du = self.zero_reg();
        self.bind(dst, DualReg { re, du })
    }

    fn lower_select(
        &mut self,
        dst: Reg,
        cond: Reg,
        if_true: Reg,
        if_false: Reg,
    ) -> Result<(), LowerError> {
        let c = self.lookup(cond)?;
        let t = self.lookup(if_true)?;
        let f = self.lookup(if_false)?;
        let re = self.emit_select(c.re, t.re, f.re);
        let du = self.emit_select(c.re, t.du, f.du);
        self.bind(dst, DualReg { re, du })
    }

    fn lower_store(&mut self, src: Reg) -> Result<(), LowerError> {
        let d = self.lookup(src)?;
        self.ops.push(LinearOp::StoreOutput { src: d.du });
        Ok(())
    }

    fn unary_dual(&mut self, op: UnaryOp, x: DualReg) -> Result<DualReg, LowerError> {
        let zero = self.zero_reg();
        let out = match op {
            UnaryOp::Neg => {
                let re = self.emit_unary(UnaryOp::Neg, x.re);
                let du = self.emit_unary(UnaryOp::Neg, x.du);
                DualReg { re, du }
            }
            UnaryOp::Not => {
                let re = self.emit_unary(UnaryOp::Not, x.re);
                DualReg { re, du: zero }
            }
            UnaryOp::Abs => {
                let re = self.emit_unary(UnaryOp::Abs, x.re);
                let neg_du = self.emit_unary(UnaryOp::Neg, x.du);
                let cond = self.emit_compare(CompareOp::Ge, x.re, zero);
                let du = self.emit_select(cond, x.du, neg_du);
                DualReg { re, du }
            }
            UnaryOp::Sign | UnaryOp::Floor | UnaryOp::Ceil | UnaryOp::Trunc => {
                let re = self.emit_unary(op, x.re);
                DualReg { re, du: zero }
            }
            UnaryOp::Sin => self.unary_mul_chain(UnaryOp::Sin, UnaryOp::Cos, x),
            UnaryOp::Cos => {
                let re = self.emit_unary(UnaryOp::Cos, x.re);
                let sinx = self.emit_unary(UnaryOp::Sin, x.re);
                let neg_sinx = self.emit_unary(UnaryOp::Neg, sinx);
                let du = self.emit_binary(BinaryOp::Mul, x.du, neg_sinx);
                DualReg { re, du }
            }
            UnaryOp::Tan => {
                let re = self.emit_unary(UnaryOp::Tan, x.re);
                let cosx = self.emit_unary(UnaryOp::Cos, x.re);
                let cos_sq = self.emit_binary(BinaryOp::Mul, cosx, cosx);
                let du = self.emit_binary(BinaryOp::Div, x.du, cos_sq);
                DualReg { re, du }
            }
            UnaryOp::Asin => self.lower_asin_or_acos(x, false),
            UnaryOp::Acos => self.lower_asin_or_acos(x, true),
            UnaryOp::Atan => {
                let re = self.emit_unary(UnaryOp::Atan, x.re);
                let one = self.one_reg();
                let x_sq = self.emit_binary(BinaryOp::Mul, x.re, x.re);
                let denom = self.emit_binary(BinaryOp::Add, one, x_sq);
                let du = self.emit_binary(BinaryOp::Div, x.du, denom);
                DualReg { re, du }
            }
            UnaryOp::Sinh => self.unary_mul_chain(UnaryOp::Sinh, UnaryOp::Cosh, x),
            UnaryOp::Cosh => self.unary_mul_chain(UnaryOp::Cosh, UnaryOp::Sinh, x),
            UnaryOp::Tanh => {
                let re = self.emit_unary(UnaryOp::Tanh, x.re);
                let cosh = self.emit_unary(UnaryOp::Cosh, x.re);
                let cosh_sq = self.emit_binary(BinaryOp::Mul, cosh, cosh);
                let du = self.emit_binary(BinaryOp::Div, x.du, cosh_sq);
                DualReg { re, du }
            }
            UnaryOp::Exp => {
                let re = self.emit_unary(UnaryOp::Exp, x.re);
                let du = self.emit_binary(BinaryOp::Mul, x.du, re);
                DualReg { re, du }
            }
            UnaryOp::Log => self.lower_log_like(x, false),
            UnaryOp::Log10 => self.lower_log_like(x, true),
            UnaryOp::Sqrt => self.lower_sqrt(x),
        };
        Ok(out)
    }

    fn unary_mul_chain(&mut self, re_op: UnaryOp, deriv_op: UnaryOp, x: DualReg) -> DualReg {
        let re = self.emit_unary(re_op, x.re);
        let deriv_term = self.emit_unary(deriv_op, x.re);
        let du = self.emit_binary(BinaryOp::Mul, x.du, deriv_term);
        DualReg { re, du }
    }

    fn lower_asin_or_acos(&mut self, x: DualReg, is_acos: bool) -> DualReg {
        let re = self.emit_unary(
            if is_acos {
                UnaryOp::Acos
            } else {
                UnaryOp::Asin
            },
            x.re,
        );
        let one = self.one_reg();
        let x_sq = self.emit_binary(BinaryOp::Mul, x.re, x.re);
        let denom_sq = self.emit_binary(BinaryOp::Sub, one, x_sq);
        let denom = self.emit_unary(UnaryOp::Sqrt, denom_sq);
        let safe = self.emit_binary(BinaryOp::Div, x.du, denom);
        let signed = if is_acos {
            self.emit_unary(UnaryOp::Neg, safe)
        } else {
            safe
        };
        let zero = self.zero_reg();
        let du_zero = self.emit_compare(CompareOp::Eq, x.du, zero);
        let du = self.emit_select(du_zero, zero, signed);
        DualReg { re, du }
    }

    fn lower_log_like(&mut self, x: DualReg, is_log10: bool) -> DualReg {
        let op = if is_log10 {
            UnaryOp::Log10
        } else {
            UnaryOp::Log
        };
        let re = self.emit_unary(op, x.re);
        let zero = self.zero_reg();
        let nonzero = self.emit_compare(CompareOp::Ne, x.re, zero);
        let denom = if is_log10 {
            let ln10 = self.ln10_reg();
            self.emit_binary(BinaryOp::Mul, x.re, ln10)
        } else {
            x.re
        };
        let safe = self.emit_binary(BinaryOp::Div, x.du, denom);
        let du = self.emit_select(nonzero, safe, zero);
        DualReg { re, du }
    }

    fn lower_sqrt(&mut self, x: DualReg) -> DualReg {
        let re = self.emit_unary(UnaryOp::Sqrt, x.re);
        let zero = self.zero_reg();
        let nonzero = self.emit_compare(CompareOp::Ne, x.re, zero);
        let two = self.two_reg();
        let denom = self.emit_binary(BinaryOp::Mul, two, re);
        let safe = self.emit_binary(BinaryOp::Div, x.du, denom);
        let du = self.emit_select(nonzero, safe, zero);
        DualReg { re, du }
    }

    fn binary_dual(
        &mut self,
        op: BinaryOp,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, LowerError> {
        let out = match op {
            BinaryOp::Add => self.binary_add(lhs, rhs),
            BinaryOp::Sub => self.binary_sub(lhs, rhs),
            BinaryOp::Mul => self.binary_mul(lhs, rhs),
            BinaryOp::Div => self.binary_div(lhs, rhs),
            BinaryOp::Pow => self.binary_pow(lhs, rhs),
            BinaryOp::And | BinaryOp::Or => self.binary_bool(op, lhs, rhs),
            BinaryOp::Atan2 => self.binary_atan2(lhs, rhs),
            BinaryOp::Min => self.binary_minmax(lhs, rhs, false),
            BinaryOp::Max => self.binary_minmax(lhs, rhs, true),
        };
        Ok(out)
    }

    fn binary_add(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(BinaryOp::Add, lhs.re, rhs.re);
        let du = self.emit_binary(BinaryOp::Add, lhs.du, rhs.du);
        DualReg { re, du }
    }

    fn binary_sub(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(BinaryOp::Sub, lhs.re, rhs.re);
        let du = self.emit_binary(BinaryOp::Sub, lhs.du, rhs.du);
        DualReg { re, du }
    }

    fn binary_mul(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.re);
        let term1 = self.emit_binary(BinaryOp::Mul, lhs.du, rhs.re);
        let term2 = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.du);
        let du = self.emit_binary(BinaryOp::Add, term1, term2);
        DualReg { re, du }
    }

    fn binary_div(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let zero = self.zero_reg();
        let denom_zero = self.emit_compare(CompareOp::Eq, rhs.re, zero);
        let numer_zero = self.emit_compare(CompareOp::Eq, lhs.re, zero);

        let safe_re = self.emit_binary(BinaryOp::Div, lhs.re, rhs.re);
        let denom_zero_re = self.emit_select(numer_zero, zero, safe_re);
        let re = self.emit_select(denom_zero, denom_zero_re, safe_re);

        let term1 = self.emit_binary(BinaryOp::Mul, lhs.du, rhs.re);
        let term2 = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.du);
        let numer_du = self.emit_binary(BinaryOp::Sub, term1, term2);
        let rhs_sq = self.emit_binary(BinaryOp::Mul, rhs.re, rhs.re);
        let safe_du = self.emit_binary(BinaryOp::Div, numer_du, rhs_sq);
        let du = self.emit_select(denom_zero, zero, safe_du);

        DualReg { re, du }
    }

    fn binary_pow(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(BinaryOp::Pow, lhs.re, rhs.re);
        let du = self.lower_pow_du(lhs, rhs, re);
        DualReg { re, du }
    }

    fn lower_pow_du(&mut self, lhs: DualReg, rhs: DualReg, re: Reg) -> Reg {
        let zero = self.zero_reg();
        let one = self.one_reg();
        let rhs_du_zero = self.emit_compare(CompareOp::Eq, rhs.du, zero);

        let lhs_re_zero = self.emit_compare(CompareOp::Eq, lhs.re, zero);
        let rhs_re_one = self.emit_compare(CompareOp::Eq, rhs.re, one);
        let rhs_minus_one = self.emit_binary(BinaryOp::Sub, rhs.re, one);
        let x_pow_n_minus_1 = self.emit_binary(BinaryOp::Pow, lhs.re, rhs_minus_one);
        let n_times = self.emit_binary(BinaryOp::Mul, rhs.re, x_pow_n_minus_1);
        let const_exp_safe = self.emit_binary(BinaryOp::Mul, n_times, lhs.du);
        let lhs_zero_branch = self.emit_select(rhs_re_one, lhs.du, zero);
        let const_exp_du = self.emit_select(lhs_re_zero, lhs_zero_branch, const_exp_safe);

        let lhs_positive = self.emit_compare(CompareOp::Gt, lhs.re, zero);
        let ln_x = self.emit_unary(UnaryOp::Log, lhs.re);
        let term1 = self.emit_binary(BinaryOp::Mul, rhs.du, ln_x);
        let xprime_over_x = self.emit_binary(BinaryOp::Div, lhs.du, lhs.re);
        let term2 = self.emit_binary(BinaryOp::Mul, rhs.re, xprime_over_x);
        let sum = self.emit_binary(BinaryOp::Add, term1, term2);
        let var_exp_safe = self.emit_binary(BinaryOp::Mul, re, sum);
        let var_exp_du = self.emit_select(lhs_positive, var_exp_safe, zero);

        self.emit_select(rhs_du_zero, const_exp_du, var_exp_du)
    }

    fn binary_bool(&mut self, op: BinaryOp, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(op, lhs.re, rhs.re);
        let du = self.zero_reg();
        DualReg { re, du }
    }

    fn binary_atan2(&mut self, lhs: DualReg, rhs: DualReg) -> DualReg {
        let re = self.emit_binary(BinaryOp::Atan2, lhs.re, rhs.re);
        let term1 = self.emit_binary(BinaryOp::Mul, lhs.du, rhs.re);
        let term2 = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.du);
        let numer = self.emit_binary(BinaryOp::Sub, term1, term2);
        let lhs_sq = self.emit_binary(BinaryOp::Mul, lhs.re, lhs.re);
        let rhs_sq = self.emit_binary(BinaryOp::Mul, rhs.re, rhs.re);
        let denom = self.emit_binary(BinaryOp::Add, lhs_sq, rhs_sq);
        let du = self.emit_binary(BinaryOp::Div, numer, denom);
        DualReg { re, du }
    }

    fn binary_minmax(&mut self, lhs: DualReg, rhs: DualReg, is_max: bool) -> DualReg {
        let cmp = if is_max { CompareOp::Ge } else { CompareOp::Le };
        let cond = self.emit_compare(cmp, lhs.re, rhs.re);
        let re = self.emit_select(cond, lhs.re, rhs.re);
        let du = self.emit_select(cond, lhs.du, rhs.du);
        DualReg { re, du }
    }

    fn bind(&mut self, src: Reg, dual: DualReg) -> Result<(), LowerError> {
        if self.map.insert(src, dual).is_some() {
            return Err(unsupported("duplicate destination register in primal row"));
        }
        Ok(())
    }

    fn lookup(&self, reg: Reg) -> Result<DualReg, LowerError> {
        self.map
            .get(&reg)
            .copied()
            .ok_or_else(|| unsupported("missing source register in primal row"))
    }

    fn alloc_reg(&mut self) -> Reg {
        let reg = self.next_reg;
        self.next_reg = self.next_reg.saturating_add(1);
        reg
    }

    fn emit_const(&mut self, value: f64) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Const { dst, value });
        dst
    }

    fn pack_registers(&mut self, regs: &[Reg]) -> Reg {
        let start = self.next_reg;
        for &src in regs {
            let dst = self.alloc_reg();
            self.ops.push(LinearOp::Move { dst, src });
        }
        start
    }

    fn emit_load_time(&mut self) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::LoadTime { dst });
        dst
    }

    fn emit_load_y(&mut self, index: usize) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::LoadY { dst, index });
        dst
    }

    fn emit_load_p(&mut self, index: usize) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::LoadP { dst, index });
        dst
    }

    fn emit_load_seed(&mut self, index: usize) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::LoadSeed { dst, index });
        dst
    }

    fn p_seed_index(&self, index: usize) -> usize {
        match self.seed_mode {
            SeedMode::SolverYOnly => index,
            SeedMode::SolverYAndP { p_seed_offset } => p_seed_offset + index,
        }
    }

    fn emit_table_bounds(&mut self, table_id: Reg, max: bool) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableBounds { dst, table_id, max });
        dst
    }

    fn emit_table_lookup(&mut self, table_id: Reg, column: Reg, input: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        });
        dst
    }

    fn emit_table_lookup_slope(&mut self, table_id: Reg, column: Reg, input: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        });
        dst
    }

    fn emit_table_next_event(&mut self, table_id: Reg, time: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        });
        dst
    }

    fn emit_unary(&mut self, op: UnaryOp, arg: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Unary { dst, op, arg });
        dst
    }

    fn emit_binary(&mut self, op: BinaryOp, lhs: Reg, rhs: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Binary { dst, op, lhs, rhs });
        dst
    }

    fn emit_compare(&mut self, op: CompareOp, lhs: Reg, rhs: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Compare { dst, op, lhs, rhs });
        dst
    }

    fn emit_select(&mut self, cond: Reg, if_true: Reg, if_false: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        });
        dst
    }

    fn zero_reg(&mut self) -> Reg {
        if let Some(reg) = self.cached_zero {
            return reg;
        }
        let reg = self.emit_const(0.0);
        self.cached_zero = Some(reg);
        reg
    }

    fn one_reg(&mut self) -> Reg {
        if let Some(reg) = self.cached_one {
            return reg;
        }
        let reg = self.emit_const(1.0);
        self.cached_one = Some(reg);
        reg
    }

    fn ln10_reg(&mut self) -> Reg {
        if let Some(reg) = self.cached_ln10 {
            return reg;
        }
        let reg = self.emit_const(std::f64::consts::LN_10);
        self.cached_ln10 = Some(reg);
        reg
    }

    fn two_reg(&mut self) -> Reg {
        if let Some(reg) = self.cached_two {
            return reg;
        }
        let reg = self.emit_const(2.0);
        self.cached_two = Some(reg);
        reg
    }
}

fn unsupported(reason: &str) -> LowerError {
    LowerError::Unsupported {
        reason: reason.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::layout::build_var_layout;
    use rumoca_core::OpBinary;
    use rumoca_ir_dae as dae;

    fn scalar_var(name: &str) -> dae::Variable {
        dae::Variable::new(rumoca_core::VarName::new(name))
    }

    fn decay_dae() -> dae::Dae {
        let mut dae = dae::Dae::new();
        dae.variables.states.insert("x".into(), scalar_var("x"));
        dae.variables.parameters.insert("k".into(), scalar_var("k"));
        dae.continuous.equations.push(dae::Equation {
            lhs: None,
            rhs: rumoca_core::Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: "k".into(),
                    subscripts: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::VarRef {
                    name: "x".into(),
                    subscripts: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            span: Default::default(),
            origin: "test".into(),
            scalar_count: 1,
        });
        dae
    }

    #[test]
    fn full_ad_seeds_parameter_slots_after_solver_y_slots() {
        let dae = decay_dae();
        let layout = build_var_layout(&dae);
        let rows = lower_residual_full_ad(&dae, &layout).expect("lower full AD");

        assert!(
            rows[0]
                .iter()
                .any(|op| matches!(op, LinearOp::LoadSeed { index: 1, .. })),
            "parameter k should use seed index after the single solver-y state: {:?}",
            rows[0]
        );
    }

    #[test]
    fn solver_ad_keeps_parameters_unseeded_for_diffsol_jacobian() {
        let dae = decay_dae();
        let layout = build_var_layout(&dae);
        let rows = lower_residual_ad(&dae, &layout).expect("lower solver AD");

        assert!(
            !rows[0]
                .iter()
                .any(|op| matches!(op, LinearOp::LoadSeed { index: 1, .. })),
            "solver Jacobian should remain with respect to solver-y slots only: {:?}",
            rows[0]
        );
    }

    #[test]
    fn compute_block_jvp_scalar_programs_applies_standard_ad() {
        // A ComputeBlock with only ScalarPrograms should produce the same result as
        // the existing scalar AD pass.
        use rumoca_ir_solve::ScalarProgramBlock;
        let row = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ];
        let block = ComputeBlock {
            nodes: vec![ComputeNode::ScalarPrograms(ScalarProgramBlock::new(vec![
                row,
            ]))],
        };
        let jvp = lower_compute_block_jvp(&block).expect("JVP of scalar row");
        assert_eq!(jvp.nodes.len(), 1);
        let jvp_rows = rumoca_eval_solve::to_scalar_program_block(&jvp);
        assert_eq!(jvp_rows.programs.len(), 1, "one JVP row for one primal row");
        assert!(
            jvp_rows.programs[0]
                .iter()
                .any(|op| matches!(op, LinearOp::LoadSeed { index: 0, .. })),
            "JVP row should load seed[0] for LoadY[0]: {:?}",
            jvp_rows.programs[0]
        );
    }

    #[test]
    fn compute_block_jvp_matmul_constant_lhs_emits_matmul_jvp() {
        // For MatMul(A_param, x_state) where A has only LoadP ops,
        // the JVP node should also be MatMul with rhs_ops using LoadSeed.
        let lhs_ops = vec![
            LinearOp::LoadP { dst: 0, index: 0 }, // A[0,0]
            LinearOp::LoadP { dst: 1, index: 1 }, // A[0,1]
            LinearOp::Move { dst: 2, src: 0 },
            LinearOp::Move { dst: 3, src: 1 },
        ];
        let rhs_ops = vec![
            LinearOp::LoadY { dst: 4, index: 0 }, // x[0]
            LinearOp::LoadY { dst: 5, index: 1 }, // x[1]
            LinearOp::Move { dst: 6, src: 4 },
            LinearOp::Move { dst: 7, src: 5 },
        ];
        let block = ComputeBlock {
            nodes: vec![ComputeNode::MatMul {
                lhs_ops,
                lhs_start: 2,
                rhs_ops,
                rhs_start: 6,
                m: 1,
                k: 2,
                n: 1,
                lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
                rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
                metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        };

        let jvp = lower_compute_block_jvp(&block).expect("JVP of MatMul with P-lhs");

        // The JVP node should also be a MatMul (not ScalarPrograms).
        assert_eq!(jvp.nodes.len(), 1, "one JVP node for one primal node");
        assert!(
            matches!(
                jvp.nodes[0],
                ComputeNode::MatMul {
                    m: 1,
                    k: 2,
                    n: 1,
                    ..
                }
            ),
            "JVP of MatMul(P-lhs, Y-rhs) should be MatMul, got {:?}",
            jvp.nodes[0]
        );

        // The JVP rhs_ops should use LoadSeed instead of LoadY.
        if let ComputeNode::MatMul { rhs_ops, .. } = &jvp.nodes[0] {
            assert!(
                rhs_ops
                    .iter()
                    .any(|op| matches!(op, LinearOp::LoadSeed { .. })),
                "JVP rhs_ops should contain LoadSeed: {:?}",
                rhs_ops
            );
            assert!(
                !rhs_ops
                    .iter()
                    .any(|op| matches!(op, LinearOp::LoadY { .. })),
                "JVP rhs_ops must not contain LoadY: {:?}",
                rhs_ops
            );
        }
    }

    #[test]
    fn compute_block_jvp_matmul_variable_lhs_emits_matmul_jvp() {
        // For MatMul(A_state, x_param), the JVP should also be MatMul with
        // lhs_ops using LoadSeed.
        let lhs_ops = vec![
            LinearOp::LoadY { dst: 0, index: 0 }, // A depends on states
            LinearOp::Move { dst: 1, src: 0 },
        ];
        let rhs_ops = vec![
            LinearOp::LoadP { dst: 2, index: 0 },
            LinearOp::Move { dst: 3, src: 2 },
        ];
        let block = ComputeBlock {
            nodes: vec![ComputeNode::MatMul {
                lhs_ops,
                lhs_start: 1,
                rhs_ops,
                rhs_start: 3,
                m: 1,
                k: 1,
                n: 1,
                lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
                rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
                metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        };

        let jvp = lower_compute_block_jvp(&block).expect("JVP of MatMul with Y-lhs");

        assert!(
            matches!(
                jvp.nodes[0],
                ComputeNode::MatMul {
                    m: 1,
                    k: 1,
                    n: 1,
                    ..
                }
            ),
            "JVP of MatMul(Y-lhs, P-rhs) should remain MatMul: {:?}",
            jvp.nodes[0]
        );
        if let ComputeNode::MatMul {
            lhs_ops, rhs_ops, ..
        } = &jvp.nodes[0]
        {
            assert!(
                lhs_ops
                    .iter()
                    .any(|op| matches!(op, LinearOp::LoadSeed { .. })),
                "JVP lhs_ops should contain LoadSeed: {:?}",
                lhs_ops
            );
            assert!(
                !rhs_ops
                    .iter()
                    .any(|op| matches!(op, LinearOp::LoadSeed { .. })),
                "constant rhs_ops should not contain LoadSeed: {:?}",
                rhs_ops
            );
        }
    }

    #[test]
    fn compute_block_jvp_matmul_constant_operands_emit_zero_rows() {
        let block = ComputeBlock {
            nodes: vec![ComputeNode::MatMul {
                lhs_ops: vec![
                    LinearOp::LoadP { dst: 0, index: 0 },
                    LinearOp::Move { dst: 1, src: 0 },
                ],
                lhs_start: 1,
                rhs_ops: vec![
                    LinearOp::LoadP { dst: 2, index: 1 },
                    LinearOp::Move { dst: 3, src: 2 },
                ],
                rhs_start: 3,
                m: 1,
                k: 1,
                n: 1,
                lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
                rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
                metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        };

        let jvp = lower_compute_block_jvp(&block).expect("constant MatMul JVP should lower");
        let ComputeNode::ScalarPrograms(rows) = &jvp.nodes[0] else {
            panic!("constant MatMul JVP should lower to zero scalar rows");
        };
        assert_eq!(rows.programs.len(), 1);
        assert_eq!(
            rows.programs[0],
            vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::StoreOutput { src: 0 },
            ]
        );
    }

    #[test]
    fn compute_block_jvp_linsolve_preserves_tensor_node_and_matches_scalar_fallback() {
        let block = ComputeBlock {
            nodes: vec![ComputeNode::LinSolve {
                setup_ops: vec![
                    LinearOp::LoadY { dst: 0, index: 0 },
                    LinearOp::Const { dst: 1, value: 0.0 },
                    LinearOp::Const { dst: 2, value: 0.0 },
                    LinearOp::Const { dst: 3, value: 2.0 },
                    LinearOp::LoadP { dst: 4, index: 0 },
                    LinearOp::LoadY { dst: 5, index: 1 },
                ],
                matrix_start: 0,
                rhs_start: 4,
                n: 2,
                next_reg: 6,
                metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        };

        let tensor_jvp = lower_compute_block_jvp(&block).expect("tensor LinSolve JVP should lower");
        assert!(
            matches!(tensor_jvp.nodes[0], ComputeNode::LinSolve { n: 2, .. }),
            "LinSolve JVP should remain a tensor LinSolve node: {:?}",
            tensor_jvp.nodes[0]
        );

        let scalar_programs = rumoca_eval_solve::to_scalar_program_block(&block);
        let scalar_jvp = ComputeBlock::from_scalar_program_block(ScalarProgramBlock::new(
            lower_scalar_program_block_ad(&scalar_programs.programs)
                .expect("scalar LinSolve JVP should lower"),
        ));

        let y = [3.0, 8.0];
        let p = [6.0];
        let seed = [0.5, 1.0];
        let mut tensor_out = vec![0.0; tensor_jvp.len()];
        let mut scalar_out = vec![0.0; scalar_jvp.len()];
        let context = rumoca_eval_solve::RowEvalContext {
            seed: Some(&seed),
            ..Default::default()
        };

        rumoca_eval_solve::PreparedComputeBlock::new(&tensor_jvp)
            .eval_with_context(&y, &p, 0.0, context, &mut tensor_out)
            .expect("tensor JVP should evaluate");
        rumoca_eval_solve::PreparedComputeBlock::new(&scalar_jvp)
            .eval_with_context(&y, &p, 0.0, context, &mut scalar_out)
            .expect("scalar fallback JVP should evaluate");

        assert_eq!(tensor_out.len(), scalar_out.len());
        for (tensor, scalar) in tensor_out.iter().zip(scalar_out.iter()) {
            assert!(
                (tensor - scalar).abs() <= 1.0e-12,
                "tensor LinSolve JVP {tensor} should match scalar fallback {scalar}"
            );
        }
        assert!((tensor_out[0] + 1.0 / 3.0).abs() <= 1.0e-12);
        assert!((tensor_out[1] - 0.5).abs() <= 1.0e-12);
    }
}
