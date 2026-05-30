use std::convert::Infallible;

use rumoca_ir_solve::{
    BinaryOp, ComputeBlock, ComputeNode, LinearOp, Reg, ScalarProgramBlock, SolveVisitor,
};

/// Expand tensor `ComputeBlock` nodes to flat scalar programs.
///
/// This is the scalar fallback for evaluators and backends that cannot consume
/// tensor-level Solve-IR nodes directly. It lives with Solve-IR evaluation, not
/// in the IR data crate or DAE-to-Solve lowering phase.
pub fn to_scalar_program_block(block: &ComputeBlock) -> ScalarProgramBlock {
    let mut collector = ScalarProgramCollector::default();
    let result: Result<(), Infallible> = collector.visit_compute_block(block);
    match result {
        Ok(()) => ScalarProgramBlock::with_program_spans(collector.rows, collector.program_spans),
        Err(error) => match error {},
    }
}

#[derive(Default)]
struct ScalarProgramCollector {
    rows: Vec<Vec<LinearOp>>,
    program_spans: Vec<rumoca_core::Span>,
}

impl SolveVisitor for ScalarProgramCollector {
    type Error = Infallible;

    fn visit_compute_node(
        &mut self,
        _node_index: usize,
        node: &ComputeNode,
    ) -> Result<(), Self::Error> {
        match node {
            ComputeNode::ScalarPrograms(block) => {
                self.visit_scalar_program_block(block)?;
            }
            ComputeNode::MatMul {
                lhs_ops,
                lhs_start,
                rhs_ops,
                rhs_start,
                m,
                k,
                n,
                span,
                ..
            } => {
                let scalar_programs =
                    scalarize_matmul(lhs_ops, *lhs_start, rhs_ops, *rhs_start, *m, *k, *n);
                self.program_spans
                    .extend(std::iter::repeat_n(*span, scalar_programs.len()));
                self.rows.extend(scalar_programs);
            }
            ComputeNode::LinSolve {
                setup_ops,
                matrix_start,
                rhs_start,
                n,
                next_reg,
                span,
                ..
            } => {
                let scalar_programs =
                    scalarize_linsolve(setup_ops, *matrix_start, *rhs_start, *n, *next_reg);
                self.program_spans
                    .extend(std::iter::repeat_n(*span, scalar_programs.len()));
                self.rows.extend(scalar_programs);
            }
        }
        Ok(())
    }

    fn visit_scalar_program(
        &mut self,
        _program_index: usize,
        span: Option<rumoca_core::Span>,
        ops: &[LinearOp],
    ) -> Result<(), Self::Error> {
        self.rows.push(ops.to_vec());
        self.program_spans
            .push(span.unwrap_or(rumoca_core::Span::DUMMY));
        Ok(())
    }
}

fn scalarize_linsolve(
    setup_ops: &[LinearOp],
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    next_reg: Reg,
) -> Vec<Vec<LinearOp>> {
    (0..n)
        .map(|component| {
            let mut ops = setup_ops.to_vec();
            let dst = next_reg;
            ops.push(LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            });
            ops.push(LinearOp::StoreOutput { src: dst });
            ops
        })
        .collect()
}

fn scalarize_matmul(
    lhs_ops: &[LinearOp],
    lhs_start: Reg,
    rhs_ops: &[LinearOp],
    rhs_start: Reg,
    m: usize,
    k: usize,
    n: usize,
) -> Vec<Vec<LinearOp>> {
    (0..m)
        .flat_map(|row| {
            (0..n).map(move |col| {
                let mut ops = lhs_ops.to_vec();
                ops.extend_from_slice(rhs_ops);
                if k == 0 {
                    let dst = next_free_reg(&ops);
                    ops.push(LinearOp::Const { dst, value: 0.0 });
                    ops.push(LinearOp::StoreOutput { src: dst });
                    return ops;
                }

                let mut next = next_free_reg(&ops);
                let first_lhs = lhs_start + (row * k) as Reg;
                let first_rhs = rhs_start + col as Reg;
                let mut acc = next;
                next += 1;
                ops.push(LinearOp::Binary {
                    dst: acc,
                    op: BinaryOp::Mul,
                    lhs: first_lhs,
                    rhs: first_rhs,
                });

                for ki in 1..k {
                    let lhs = lhs_start + (row * k + ki) as Reg;
                    let rhs = rhs_start + (ki * n + col) as Reg;
                    let product = next;
                    next += 1;
                    ops.push(LinearOp::Binary {
                        dst: product,
                        op: BinaryOp::Mul,
                        lhs,
                        rhs,
                    });
                    let sum = next;
                    next += 1;
                    ops.push(LinearOp::Binary {
                        dst: sum,
                        op: BinaryOp::Add,
                        lhs: acc,
                        rhs: product,
                    });
                    acc = sum;
                }

                ops.push(LinearOp::StoreOutput { src: acc });
                ops
            })
        })
        .collect()
}

fn next_free_reg(ops: &[LinearOp]) -> Reg {
    ops.iter().map(max_reg_in_op).max().unwrap_or(0) + 1
}

fn max_reg_in_op(op: &LinearOp) -> Reg {
    match *op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadSeed { dst, .. }
        | LinearOp::Move { dst, .. }
        | LinearOp::Unary { dst, .. }
        | LinearOp::Binary { dst, .. }
        | LinearOp::Compare { dst, .. }
        | LinearOp::Select { dst, .. }
        | LinearOp::LinearSolveComponent { dst, .. }
        | LinearOp::TableBounds { dst, .. }
        | LinearOp::TableLookup { dst, .. }
        | LinearOp::TableLookupSlope { dst, .. }
        | LinearOp::TableNextEvent { dst, .. }
        | LinearOp::RandomInitialState { dst, .. }
        | LinearOp::RandomResult { dst, .. }
        | LinearOp::RandomState { dst, .. }
        | LinearOp::ImpureRandomInit { dst, .. }
        | LinearOp::ImpureRandom { dst, .. }
        | LinearOp::ImpureRandomInteger { dst, .. } => dst,
        LinearOp::StoreOutput { src } => src,
    }
}
