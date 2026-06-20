//! Read-only traversal helpers for Solve IR.
//!
//! These visitors centralize Solve-IR traversal without encoding evaluation,
//! validation, or backend policy in the data crate.

use crate::{
    ComputeBlock, ComputeNode, ContinuousSolveArtifacts, ContinuousSolveSystem,
    DiscreteSolveSystem, InitializationSolveSystem, LinearOp, ScalarProgramBlock, SolveArtifacts,
    SolveClockPartition, SolveEventPartition, SolveModel, SolveProblem,
};
use rumoca_core::Span;

/// Identifies the op slice currently being visited.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LinearOpSliceKind {
    /// One scalar register program in a `ScalarProgramBlock`.
    ScalarProgram {
        program_index: usize,
        span: Option<Span>,
    },
    /// The left operand setup stream for `ComputeNode::MatMul`.
    MatMulLhs { node_index: usize, span: Span },
    /// The right operand setup stream for `ComputeNode::MatMul`.
    MatMulRhs { node_index: usize, span: Span },
    /// The matrix/rhs setup stream for `ComputeNode::LinSolve`.
    LinSolveSetup { node_index: usize, span: Span },
    /// The base row for `ComputeNode::Map`.
    MapBase { node_index: usize, span: Span },
    /// The base row for `ComputeNode::AffineStencil`.
    AffineStencilBase { node_index: usize, span: Span },
}

pub enum VisitScope<'a> {
    Model(&'a SolveModel),
    Problem(&'a SolveProblem),
    Artifacts(&'a SolveArtifacts),
    ContinuousSystem(&'a ContinuousSolveSystem),
    InitializationSystem(&'a InitializationSolveSystem),
    DiscreteSystem(&'a DiscreteSolveSystem),
    EventPartition(&'a SolveEventPartition),
    ClockPartition(&'a SolveClockPartition),
    ContinuousArtifacts(&'a ContinuousSolveArtifacts),
    ComputeBlock(&'a ComputeBlock),
    ComputeNode {
        index: usize,
        node: &'a ComputeNode,
    },
    ScalarProgramBlock(&'a ScalarProgramBlock),
    LinearOpSlice {
        kind: LinearOpSliceKind,
        ops: &'a [LinearOp],
    },
}

/// Read-only Solve-IR visitor.
///
/// Implementors override the hooks they care about and call the default walker
/// when traversal should continue through children. The associated error type
/// lets phase and backend crates return their native structured errors.
pub trait SolveVisitor {
    type Error;

    fn enter_scope(&mut self, _scope: VisitScope<'_>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn exit_scope(&mut self, _scope: VisitScope<'_>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_solve_model(&mut self, model: &SolveModel) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::Model(model))?;
        let result = walk_solve_model(self, model);
        let exit_result = self.exit_scope(VisitScope::Model(model));
        result?;
        exit_result
    }

    fn visit_solve_problem(&mut self, problem: &SolveProblem) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::Problem(problem))?;
        let result = walk_solve_problem(self, problem);
        let exit_result = self.exit_scope(VisitScope::Problem(problem));
        result?;
        exit_result
    }

    fn visit_solve_artifacts(&mut self, artifacts: &SolveArtifacts) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::Artifacts(artifacts))?;
        let result = walk_solve_artifacts(self, artifacts);
        let exit_result = self.exit_scope(VisitScope::Artifacts(artifacts));
        result?;
        exit_result
    }

    fn visit_continuous_system(
        &mut self,
        system: &ContinuousSolveSystem,
    ) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::ContinuousSystem(system))?;
        let result = walk_continuous_system(self, system);
        let exit_result = self.exit_scope(VisitScope::ContinuousSystem(system));
        result?;
        exit_result
    }

    fn visit_initialization_system(
        &mut self,
        system: &InitializationSolveSystem,
    ) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::InitializationSystem(system))?;
        let result = walk_initialization_system(self, system);
        let exit_result = self.exit_scope(VisitScope::InitializationSystem(system));
        result?;
        exit_result
    }

    fn visit_discrete_system(&mut self, system: &DiscreteSolveSystem) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::DiscreteSystem(system))?;
        let result = walk_discrete_system(self, system);
        let exit_result = self.exit_scope(VisitScope::DiscreteSystem(system));
        result?;
        exit_result
    }

    fn visit_event_partition(
        &mut self,
        partition: &SolveEventPartition,
    ) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::EventPartition(partition))?;
        let result = walk_event_partition(self, partition);
        let exit_result = self.exit_scope(VisitScope::EventPartition(partition));
        result?;
        exit_result
    }

    fn visit_clock_partition(
        &mut self,
        partition: &SolveClockPartition,
    ) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::ClockPartition(partition))?;
        let result = walk_clock_partition(self, partition);
        let exit_result = self.exit_scope(VisitScope::ClockPartition(partition));
        result?;
        exit_result
    }

    fn visit_continuous_artifacts(
        &mut self,
        artifacts: &ContinuousSolveArtifacts,
    ) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::ContinuousArtifacts(artifacts))?;
        let result = walk_continuous_artifacts(self, artifacts);
        let exit_result = self.exit_scope(VisitScope::ContinuousArtifacts(artifacts));
        result?;
        exit_result
    }

    fn visit_compute_block(&mut self, block: &ComputeBlock) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::ComputeBlock(block))?;
        let result = walk_compute_block(self, block);
        let exit_result = self.exit_scope(VisitScope::ComputeBlock(block));
        result?;
        exit_result
    }

    fn visit_compute_node(
        &mut self,
        node_index: usize,
        node: &ComputeNode,
    ) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::ComputeNode {
            index: node_index,
            node,
        })?;
        let result = walk_compute_node(self, node_index, node);
        let exit_result = self.exit_scope(VisitScope::ComputeNode {
            index: node_index,
            node,
        });
        result?;
        exit_result
    }

    fn visit_scalar_program_block(
        &mut self,
        block: &ScalarProgramBlock,
    ) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::ScalarProgramBlock(block))?;
        let result = walk_scalar_program_block(self, block);
        let exit_result = self.exit_scope(VisitScope::ScalarProgramBlock(block));
        result?;
        exit_result
    }

    fn visit_scalar_program(
        &mut self,
        program_index: usize,
        span: Option<Span>,
        ops: &[LinearOp],
    ) -> Result<(), Self::Error> {
        self.visit_linear_op_slice(
            LinearOpSliceKind::ScalarProgram {
                program_index,
                span,
            },
            ops,
        )
    }

    fn visit_linear_op_slice(
        &mut self,
        kind: LinearOpSliceKind,
        ops: &[LinearOp],
    ) -> Result<(), Self::Error> {
        self.enter_scope(VisitScope::LinearOpSlice { kind, ops })?;
        let result = walk_linear_op_slice(self, kind, ops);
        let exit_result = self.exit_scope(VisitScope::LinearOpSlice { kind, ops });
        result?;
        exit_result
    }

    fn visit_linear_op(
        &mut self,
        _kind: LinearOpSliceKind,
        _op_index: usize,
        _op: &LinearOp,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub fn walk_solve_model<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    model: &SolveModel,
) -> Result<(), V::Error> {
    visitor.visit_solve_problem(&model.problem)?;
    visitor.visit_solve_artifacts(&model.artifacts)?;
    visitor.visit_scalar_program_block(&model.visible_value_rows)
}

pub fn walk_solve_problem<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    problem: &SolveProblem,
) -> Result<(), V::Error> {
    visitor.visit_continuous_system(&problem.continuous)?;
    visitor.visit_initialization_system(&problem.initialization)?;
    visitor.visit_discrete_system(&problem.discrete)?;
    visitor.visit_event_partition(&problem.events)?;
    visitor.visit_clock_partition(&problem.clocks)
}

pub fn walk_solve_artifacts<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    artifacts: &SolveArtifacts,
) -> Result<(), V::Error> {
    visitor.visit_continuous_artifacts(&artifacts.continuous)
}

pub fn walk_continuous_system<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    system: &ContinuousSolveSystem,
) -> Result<(), V::Error> {
    visitor.visit_compute_block(&system.implicit_rhs)?;
    visitor.visit_compute_block(&system.residual)?;
    visitor.visit_compute_block(&system.derivative_rhs)
}

pub fn walk_initialization_system<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    system: &InitializationSolveSystem,
) -> Result<(), V::Error> {
    visitor.visit_scalar_program_block(&system.residual)?;
    visitor.visit_scalar_program_block(&system.update_rhs)
}

pub fn walk_discrete_system<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    system: &DiscreteSolveSystem,
) -> Result<(), V::Error> {
    visitor.visit_scalar_program_block(&system.runtime_assignment_rhs)?;
    visitor.visit_scalar_program_block(&system.rhs)
}

pub fn walk_event_partition<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    partition: &SolveEventPartition,
) -> Result<(), V::Error> {
    visitor.visit_scalar_program_block(&partition.root_conditions)?;
    visitor.visit_scalar_program_block(&partition.dynamic_time_event_rhs)
}

pub fn walk_clock_partition<V: SolveVisitor + ?Sized>(
    _visitor: &mut V,
    _partition: &SolveClockPartition,
) -> Result<(), V::Error> {
    Ok(())
}

pub fn walk_continuous_artifacts<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    artifacts: &ContinuousSolveArtifacts,
) -> Result<(), V::Error> {
    visitor.visit_compute_block(&artifacts.implicit_jacobian_v)?;
    visitor.visit_scalar_program_block(&artifacts.full_jacobian_v)
}

pub fn walk_compute_block<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    block: &ComputeBlock,
) -> Result<(), V::Error> {
    for (node_index, node) in block.nodes.iter().enumerate() {
        visitor.visit_compute_node(node_index, node)?;
    }
    Ok(())
}

pub fn walk_compute_node<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    node_index: usize,
    node: &ComputeNode,
) -> Result<(), V::Error> {
    match node {
        ComputeNode::ScalarPrograms(block) => visitor.visit_scalar_program_block(block),
        ComputeNode::MatMul {
            lhs_ops,
            rhs_ops,
            span,
            ..
        } => {
            visitor.visit_linear_op_slice(
                LinearOpSliceKind::MatMulLhs {
                    node_index,
                    span: *span,
                },
                lhs_ops,
            )?;
            visitor.visit_linear_op_slice(
                LinearOpSliceKind::MatMulRhs {
                    node_index,
                    span: *span,
                },
                rhs_ops,
            )
        }
        ComputeNode::LinSolve {
            setup_ops, span, ..
        } => visitor.visit_linear_op_slice(
            LinearOpSliceKind::LinSolveSetup {
                node_index,
                span: *span,
            },
            setup_ops,
        ),
        ComputeNode::Map { base_ops, span, .. } => visitor.visit_linear_op_slice(
            LinearOpSliceKind::MapBase {
                node_index,
                span: *span,
            },
            base_ops,
        ),
        ComputeNode::AffineStencil { base_ops, span, .. } => visitor.visit_linear_op_slice(
            LinearOpSliceKind::AffineStencilBase {
                node_index,
                span: *span,
            },
            base_ops,
        ),
    }
}

pub fn walk_scalar_program_block<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    block: &ScalarProgramBlock,
) -> Result<(), V::Error> {
    for (program_index, program) in block.programs.iter().enumerate() {
        visitor.visit_scalar_program(program_index, block.program_span(program_index), program)?;
    }
    Ok(())
}

pub fn walk_linear_op_slice<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    kind: LinearOpSliceKind,
    ops: &[LinearOp],
) -> Result<(), V::Error> {
    for (op_index, op) in ops.iter().enumerate() {
        visitor.visit_linear_op(kind, op_index, op)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{BinaryOp, Reg};
    use std::convert::Infallible;

    #[derive(Default)]
    struct CountingVisitor {
        nodes: usize,
        rows: usize,
        ops: usize,
        kinds: Vec<LinearOpSliceKind>,
    }

    impl SolveVisitor for CountingVisitor {
        type Error = Infallible;

        fn visit_compute_node(
            &mut self,
            node_index: usize,
            node: &ComputeNode,
        ) -> Result<(), Self::Error> {
            self.nodes += 1;
            walk_compute_node(self, node_index, node)
        }

        fn visit_scalar_program(
            &mut self,
            program_index: usize,
            span: Option<Span>,
            ops: &[LinearOp],
        ) -> Result<(), Self::Error> {
            self.rows += 1;
            self.visit_linear_op_slice(
                LinearOpSliceKind::ScalarProgram {
                    program_index,
                    span,
                },
                ops,
            )
        }

        fn visit_linear_op(
            &mut self,
            kind: LinearOpSliceKind,
            _op_index: usize,
            _op: &LinearOp,
        ) -> Result<(), Self::Error> {
            self.ops += 1;
            self.kinds.push(kind);
            Ok(())
        }
    }

    fn store_row(src: Reg) -> Vec<LinearOp> {
        vec![LinearOp::StoreOutput { src }]
    }

    fn matmul_node(span: Span) -> ComputeNode {
        ComputeNode::MatMul {
            lhs_ops: vec![LinearOp::LoadY { dst: 0, index: 0 }],
            lhs_start: 0,
            rhs_ops: vec![LinearOp::LoadP { dst: 1, index: 0 }],
            rhs_start: 1,
            m: 1,
            k: 1,
            n: 1,
            lhs_sparsity: crate::SparsityPattern::Dense,
            rhs_sparsity: crate::SparsityPattern::Dense,
            metadata: crate::TensorNodeMetadata::default(),
            span,
        }
    }

    fn linsolve_node(span: Span) -> ComputeNode {
        ComputeNode::LinSolve {
            setup_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::Const { dst: 1, value: 2.0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
            ],
            matrix_start: 0,
            rhs_start: 1,
            n: 1,
            next_reg: 3,
            metadata: crate::TensorNodeMetadata::default(),
            span,
        }
    }

    fn single_binder_domain() -> crate::StructuredIndexDomain {
        crate::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        }
    }

    fn single_stride_output_map() -> crate::TensorOutputMap {
        crate::TensorOutputMap {
            start: 0,
            strides: vec![crate::AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        }
    }

    fn single_load_stride() -> Vec<crate::AffineStencilLoadStride> {
        vec![crate::AffineStencilLoadStride {
            op_position: 0,
            terms: vec![crate::AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        }]
    }

    fn map_node(span: Span) -> ComputeNode {
        ComputeNode::Map {
            domain: single_binder_domain(),
            output_map: single_stride_output_map(),
            base_ops: vec![LinearOp::LoadY { dst: 0, index: 0 }],
            load_strides: single_load_stride(),
            const_strides: Vec::new(),
            metadata: crate::TensorNodeMetadata::default(),
            span,
        }
    }

    fn affine_stencil_node(span: Span) -> ComputeNode {
        ComputeNode::AffineStencil {
            domain: single_binder_domain(),
            output_map: single_stride_output_map(),
            base_ops: vec![LinearOp::LoadP { dst: 0, index: 0 }],
            load_strides: single_load_stride(),
            const_strides: Vec::new(),
            metadata: crate::TensorNodeMetadata::default(),
            span,
        }
    }

    #[test]
    fn compute_block_visitor_walks_scalar_and_tensor_op_slices() {
        let span = Span::DUMMY;
        let block = ComputeBlock {
            nodes: vec![
                ComputeNode::ScalarPrograms(ScalarProgramBlock::with_source_span(
                    vec![store_row(0)],
                    span,
                )),
                matmul_node(span),
                linsolve_node(span),
                map_node(span),
                affine_stencil_node(span),
            ],
        };

        let mut visitor = CountingVisitor::default();
        visitor.visit_compute_block(&block).unwrap();

        assert_eq!(visitor.nodes, 5);
        assert_eq!(visitor.rows, 1);
        assert_eq!(visitor.ops, 8);
        assert!(visitor.kinds.contains(&LinearOpSliceKind::MatMulLhs {
            node_index: 1,
            span
        }));
        assert!(visitor.kinds.contains(&LinearOpSliceKind::MatMulRhs {
            node_index: 1,
            span
        }));
        assert!(visitor.kinds.contains(&LinearOpSliceKind::LinSolveSetup {
            node_index: 2,
            span
        }));
        assert!(visitor.kinds.contains(&LinearOpSliceKind::MapBase {
            node_index: 3,
            span
        }));
        assert!(
            visitor
                .kinds
                .contains(&LinearOpSliceKind::AffineStencilBase {
                    node_index: 4,
                    span
                })
        );
    }
}
