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
            ComputeNode::AffineStencil {
                count,
                base_ops,
                load_strides,
                const_strides,
                span,
                ..
            } => {
                let scalar_programs =
                    scalarize_affine_stencil(base_ops, load_strides, const_strides, *count);
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

pub(crate) fn scalarize_affine_stencil(
    base_ops: &[LinearOp],
    load_strides: &[rumoca_ir_solve::AffineStencilLoadStride],
    const_strides: &[rumoca_ir_solve::AffineStencilConstStride],
    count: usize,
) -> Vec<Vec<LinearOp>> {
    (0..count)
        .map(|row_offset| {
            let mut ops = base_ops.to_vec();
            for stride in load_strides {
                apply_affine_load_stride(&mut ops, stride, row_offset);
            }
            for stride in const_strides {
                apply_affine_const_stride(&mut ops, stride, row_offset);
            }
            ops
        })
        .collect()
}

fn apply_affine_load_stride(
    ops: &mut [LinearOp],
    stride: &rumoca_ir_solve::AffineStencilLoadStride,
    row_offset: usize,
) {
    match &mut ops[stride.op_position] {
        LinearOp::LoadY { index, .. } | LinearOp::LoadP { index, .. } => {
            *index += row_offset * stride.stride;
        }
        _ => unreachable!("AffineStencil load_strides only point at load ops"),
    }
}

fn apply_affine_const_stride(
    ops: &mut [LinearOp],
    stride: &rumoca_ir_solve::AffineStencilConstStride,
    row_offset: usize,
) {
    match &mut ops[stride.op_position] {
        LinearOp::Const { value, .. } => {
            *value += row_offset as f64 * stride.stride;
        }
        _ => unreachable!("AffineStencil const_strides only point at const ops"),
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

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve::{
        AffineStencilDomain, AffineStencilIteration, AffineStencilLoadStride, UnaryOp,
    };

    fn test_stencil_domain(count: usize) -> AffineStencilDomain {
        AffineStencilDomain {
            index_names: vec!["i".to_string()],
            iterations: (0..count)
                .map(|idx| AffineStencilIteration {
                    index_values: vec![idx as i64 + 1],
                })
                .collect(),
        }
    }

    #[test]
    fn affine_stencil_expands_to_exact_scalar_rows() {
        let block = ComputeBlock {
            nodes: vec![ComputeNode::AffineStencil {
                count: 3,
                domain: test_stencil_domain(3),
                base_ops: vec![
                    LinearOp::LoadP { dst: 0, index: 2 },
                    LinearOp::LoadY { dst: 1, index: 10 },
                    LinearOp::Unary {
                        dst: 2,
                        op: UnaryOp::Neg,
                        arg: 1,
                    },
                    LinearOp::StoreOutput { src: 2 },
                ],
                load_strides: vec![
                    AffineStencilLoadStride {
                        op_position: 0,
                        stride: 0,
                    },
                    AffineStencilLoadStride {
                        op_position: 1,
                        stride: 2,
                    },
                ],
                const_strides: Vec::new(),
                metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        };

        let rows = to_scalar_program_block(&block);
        assert_eq!(rows.programs.len(), 3);
        assert!(matches!(
            rows.programs[0][1],
            LinearOp::LoadY { dst: 1, index: 10 }
        ));
        assert!(matches!(
            rows.programs[1][1],
            LinearOp::LoadY { dst: 1, index: 12 }
        ));
        assert!(matches!(
            rows.programs[2][1],
            LinearOp::LoadY { dst: 1, index: 14 }
        ));
        assert!(matches!(
            rows.programs[2][0],
            LinearOp::LoadP { dst: 0, index: 2 }
        ));
    }
}
