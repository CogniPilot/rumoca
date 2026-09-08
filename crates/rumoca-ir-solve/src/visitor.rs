//! Read-only traversal helpers for Solve IR.
//!
//! These visitors centralize Solve-IR traversal without encoding evaluation,
//! validation, or backend policy in the data crate.

use crate::{
    ComputeBlock, ComputeNode, ContinuousSolveArtifacts, ContinuousSolveSystem,
    DiscreteSolveSystem, EventTransactionProgram, InitializationSolveArtifacts,
    InitializationSolveSystem, LinearOp, ScalarProgramBlock, SolveArtifacts, SolveClockPartition,
    SolveEventAction, SolveEventMessage, SolveEventMessagePart, SolveEventPartition, SolveModel,
    SolveProblem, SolveStringConversionFormat,
};
use rumoca_core::Span;

/// Closed inventory of every `ComputeBlock` owner in Solve IR.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ComputeBlockOwner {
    ContinuousImplicitRhs,
    ContinuousResidual,
    ContinuousManifoldResidual,
    ContinuousDerivativeRhs,
    InitializationResidual,
    DiscreteStructuredRhs,
    ContinuousImplicitJacobianV,
    ContinuousManifoldJacobianV,
    InitializationResidualJacobianV,
}

impl ComputeBlockOwner {
    #[must_use]
    pub const fn context(self) -> &'static str {
        match self {
            Self::ContinuousImplicitRhs => "continuous.implicit_rhs",
            Self::ContinuousResidual => "continuous.residual",
            Self::ContinuousManifoldResidual => "continuous.manifold_residual",
            Self::ContinuousDerivativeRhs => "continuous.derivative_rhs",
            Self::InitializationResidual => "initialization.residual",
            Self::DiscreteStructuredRhs => "discrete.structured_rhs",
            Self::ContinuousImplicitJacobianV => "artifacts.continuous.implicit_jacobian_v",
            Self::ContinuousManifoldJacobianV => "artifacts.continuous.manifold_jacobian_v",
            Self::InitializationResidualJacobianV => "artifacts.initialization.residual_jacobian_v",
        }
    }
}

/// Closed inventory of every `ScalarProgramBlock` owner in Solve IR.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScalarProgramBlockOwner {
    ComputeBlock(ComputeBlockOwner),
    InitializationUpdateRhs,
    DiscreteRuntimeAssignmentRhs,
    DiscretePostCommitAssignmentRhs,
    DiscreteRhs,
    DiscreteClockPartitionIntermediates,
    EventRootConditions,
    EventDynamicTimeEventRhs,
    EventActionConditions,
    EventDelaySourceRhs,
    EventDelayTimeRhs,
    EventDelayMaxRhs,
    ContinuousExactRefreshAssignmentFinalProgram,
    ContinuousFullJacobianV,
    ContinuousImplicitJacobianVScalar,
    VisibleValueRows,
}

impl ScalarProgramBlockOwner {
    #[must_use]
    pub const fn context(self) -> &'static str {
        match self {
            Self::ComputeBlock(owner) => owner.context(),
            Self::InitializationUpdateRhs => "initialization.update_rhs",
            Self::DiscreteRuntimeAssignmentRhs => "discrete.runtime_assignment_rhs",
            Self::DiscretePostCommitAssignmentRhs => "discrete.post_commit_assignment_rhs",
            Self::DiscreteRhs => "discrete.rhs",
            Self::DiscreteClockPartitionIntermediates => "discrete.clock_partition_intermediates",
            Self::EventRootConditions => "events.root_conditions",
            Self::EventDynamicTimeEventRhs => "events.dynamic_time_event_rhs",
            Self::EventActionConditions => "events.action_conditions",
            Self::EventDelaySourceRhs => "events.delays.source_rhs",
            Self::EventDelayTimeRhs => "events.delays.delay_time_rhs",
            Self::EventDelayMaxRhs => "events.delays.delay_max_rhs",
            Self::ContinuousExactRefreshAssignmentFinalProgram => {
                "continuous.refresh.exact_assignment.final_program"
            }
            Self::ContinuousFullJacobianV => "artifacts.continuous.full_jacobian_v",
            Self::ContinuousImplicitJacobianVScalar => {
                "artifacts.continuous.implicit_jacobian_v_scalar"
            }
            Self::VisibleValueRows => "visible_value_rows",
        }
    }
}

/// Exact message-program owner within the event inventory.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EventMessageOwner {
    Action {
        action_index: usize,
    },
    TransactionAssertion {
        transaction_index: usize,
        assertion_index: usize,
    },
}

/// Identifies the op slice currently being visited and its exact Solve owner.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LinearOpSliceOwner {
    /// A scalar program while its local child certificate is being issued,
    /// before it is joined to a model-level owner.
    ScalarProgramConstruction {
        program_index: usize,
        span: Option<Span>,
    },
    /// One scalar register program in a `ScalarProgramBlock`.
    ScalarProgram {
        owner: ScalarProgramBlockOwner,
        program_index: usize,
        span: Option<Span>,
    },
    /// One compact correlated guarded-assignment program.
    GuardedAssignmentProgram { program_index: usize, span: Span },
    /// The value expression of one event message conversion.
    EventMessageValue {
        owner: EventMessageOwner,
        part_index: usize,
        span: Span,
    },
    /// One optional formatting expression of an event message conversion.
    EventMessageFormat {
        owner: EventMessageOwner,
        part_index: usize,
        field: EventMessageFormatField,
        span: Span,
    },
    /// The left operand setup stream for `ComputeNode::MatMul`.
    MatMulLhs {
        owner: ComputeBlockOwner,
        node_index: usize,
        span: Span,
    },
    /// The right operand setup stream for `ComputeNode::MatMul`.
    MatMulRhs {
        owner: ComputeBlockOwner,
        node_index: usize,
        span: Span,
    },
    /// The matrix/rhs setup stream for `ComputeNode::LinSolve`.
    LinSolveSetup {
        owner: ComputeBlockOwner,
        node_index: usize,
        span: Span,
    },
    /// The base row for `ComputeNode::Map`.
    MapBase {
        owner: ComputeBlockOwner,
        node_index: usize,
        span: Span,
    },
    /// The base row for `ComputeNode::AffineStencil`.
    AffineStencilBase {
        owner: ComputeBlockOwner,
        node_index: usize,
        span: Span,
    },
}

/// Closed formatting-expression inventory for one conversion part.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EventMessageFormatField {
    MinimumLength,
    LeftJustified,
    SignificantDigits,
}

/// Read-only Solve-IR visitor.
///
/// Implementors override the hooks they care about and call the default walker
/// when traversal should continue through children. The associated error type
/// lets phase and backend crates return their native structured errors.
///
/// Every hook is per-node: there is no generic enter/exit pair over an
/// enumerated scope. A visitor that needs to know it is inside a compute block
/// overrides `visit_compute_block` and calls `walk_compute_block` itself, which
/// is the same information without the walk paying to build a scope value at
/// every node whether or not anyone reads it.
pub trait SolveVisitor {
    type Error;

    fn visit_solve_model(&mut self, model: &SolveModel) -> Result<(), Self::Error> {
        walk_solve_model(self, model)
    }

    fn visit_solve_problem(&mut self, problem: &SolveProblem) -> Result<(), Self::Error> {
        walk_solve_problem(self, problem)
    }

    fn visit_solve_artifacts(&mut self, artifacts: &SolveArtifacts) -> Result<(), Self::Error> {
        walk_solve_artifacts(self, artifacts)
    }

    fn visit_continuous_system(
        &mut self,
        system: &ContinuousSolveSystem,
    ) -> Result<(), Self::Error> {
        walk_continuous_system(self, system)
    }

    fn visit_initialization_system(
        &mut self,
        system: &InitializationSolveSystem,
    ) -> Result<(), Self::Error> {
        walk_initialization_system(self, system)
    }

    fn visit_discrete_system(&mut self, system: &DiscreteSolveSystem) -> Result<(), Self::Error> {
        walk_discrete_system(self, system)
    }

    /// An event-transaction program is a leaf of this traversal: its ops are
    /// reached through the transaction's own accessors, not through the walk.
    fn visit_event_transaction_program(
        &mut self,
        _index: usize,
        _program: &EventTransactionProgram,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_event_partition(
        &mut self,
        partition: &SolveEventPartition,
    ) -> Result<(), Self::Error> {
        walk_event_partition(self, partition)
    }

    fn visit_clock_partition(
        &mut self,
        partition: &SolveClockPartition,
    ) -> Result<(), Self::Error> {
        walk_clock_partition(self, partition)
    }

    fn visit_continuous_artifacts(
        &mut self,
        artifacts: &ContinuousSolveArtifacts,
    ) -> Result<(), Self::Error> {
        walk_continuous_artifacts(self, artifacts)
    }

    fn visit_initialization_artifacts(
        &mut self,
        artifacts: &InitializationSolveArtifacts,
    ) -> Result<(), Self::Error> {
        walk_initialization_artifacts(self, artifacts)
    }

    fn visit_compute_block(
        &mut self,
        owner: ComputeBlockOwner,
        block: &ComputeBlock,
    ) -> Result<(), Self::Error> {
        walk_compute_block(self, owner, block)
    }

    fn visit_compute_node(
        &mut self,
        owner: ComputeBlockOwner,
        node_index: usize,
        node: &ComputeNode,
    ) -> Result<(), Self::Error> {
        walk_compute_node(self, owner, node_index, node)
    }

    fn visit_scalar_program_block(
        &mut self,
        owner: ScalarProgramBlockOwner,
        block: &ScalarProgramBlock,
    ) -> Result<(), Self::Error> {
        walk_scalar_program_block(self, owner, block)
    }

    fn visit_scalar_program(
        &mut self,
        owner: ScalarProgramBlockOwner,
        program_index: usize,
        span: Option<Span>,
        ops: &[LinearOp],
    ) -> Result<(), Self::Error> {
        self.visit_linear_op_slice(
            LinearOpSliceOwner::ScalarProgram {
                owner,
                program_index,
                span,
            },
            ops,
        )
    }

    fn visit_linear_op_slice(
        &mut self,
        kind: LinearOpSliceOwner,
        ops: &[LinearOp],
    ) -> Result<(), Self::Error> {
        walk_linear_op_slice(self, kind, ops)
    }

    fn visit_linear_op(
        &mut self,
        kind: LinearOpSliceOwner,
        op_index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        walk_linear_op(self, kind, op_index, op)
    }
}

pub fn walk_solve_model<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    model: &SolveModel,
) -> Result<(), V::Error> {
    visitor.visit_solve_problem(model.problem())?;
    visitor.visit_solve_artifacts(model.artifacts())?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::VisibleValueRows,
        model.visible_value_rows(),
    )
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
    visitor.visit_continuous_artifacts(artifacts.continuous())?;
    visitor.visit_initialization_artifacts(artifacts.initialization())
}

pub fn walk_continuous_system<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    system: &ContinuousSolveSystem,
) -> Result<(), V::Error> {
    visitor.visit_compute_block(
        ComputeBlockOwner::ContinuousImplicitRhs,
        &system.implicit_rhs,
    )?;
    visitor.visit_compute_block(ComputeBlockOwner::ContinuousResidual, &system.residual)?;
    visitor.visit_compute_block(
        ComputeBlockOwner::ContinuousManifoldResidual,
        &system.manifold_residual,
    )?;
    visitor.visit_compute_block(
        ComputeBlockOwner::ContinuousDerivativeRhs,
        &system.derivative_rhs,
    )?;
    for program in system.refresh_owners().exact_assignment_programs() {
        visitor.visit_scalar_program_block(
            ScalarProgramBlockOwner::ContinuousExactRefreshAssignmentFinalProgram,
            program.final_program(),
        )?;
    }
    Ok(())
}

pub fn walk_initialization_system<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    system: &InitializationSolveSystem,
) -> Result<(), V::Error> {
    visitor.visit_compute_block(ComputeBlockOwner::InitializationResidual, &system.residual)?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::InitializationUpdateRhs,
        &system.update_rhs,
    )
}

pub fn walk_discrete_system<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    system: &DiscreteSolveSystem,
) -> Result<(), V::Error> {
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::DiscreteRuntimeAssignmentRhs,
        &system.runtime_assignment_rhs,
    )?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::DiscretePostCommitAssignmentRhs,
        &system.post_commit_assignment_rhs,
    )?;
    visitor.visit_scalar_program_block(ScalarProgramBlockOwner::DiscreteRhs, &system.rhs)?;
    for (program_index, program) in system.guarded_assignments.iter().enumerate() {
        visitor.visit_linear_op_slice(
            LinearOpSliceOwner::GuardedAssignmentProgram {
                program_index,
                span: program.span(),
            },
            program.program(),
        )?;
    }
    for (program_index, program) in system.event_transactions.iter().enumerate() {
        visitor.visit_event_transaction_program(program_index, program)?;
        for (assertion_index, action) in program.assertions().iter().enumerate() {
            walk_event_action_message(
                visitor,
                EventMessageOwner::TransactionAssertion {
                    transaction_index: program_index,
                    assertion_index,
                },
                action,
            )?;
        }
    }
    visitor.visit_compute_block(
        ComputeBlockOwner::DiscreteStructuredRhs,
        &system.structured_rhs,
    )?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::DiscreteClockPartitionIntermediates,
        &system.clock_partition_intermediates,
    )
}

pub fn walk_event_partition<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    partition: &SolveEventPartition,
) -> Result<(), V::Error> {
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::EventRootConditions,
        &partition.root_conditions,
    )?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::EventDynamicTimeEventRhs,
        &partition.dynamic_time_event_rhs,
    )?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::EventActionConditions,
        &partition.action_conditions,
    )?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::EventDelaySourceRhs,
        &partition.delays.source_rhs,
    )?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::EventDelayTimeRhs,
        &partition.delays.delay_time_rhs,
    )?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::EventDelayMaxRhs,
        &partition.delays.delay_max_rhs,
    )?;
    for (action_index, action) in partition.actions.iter().enumerate() {
        walk_event_action_message(visitor, EventMessageOwner::Action { action_index }, action)?;
    }
    Ok(())
}

pub fn walk_clock_partition<V: SolveVisitor + ?Sized>(
    _visitor: &mut V,
    _partition: &SolveClockPartition,
) -> Result<(), V::Error> {
    Ok(())
}

fn walk_event_action_message<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    owner: EventMessageOwner,
    action: &SolveEventAction,
) -> Result<(), V::Error> {
    walk_event_message(visitor, owner, action.span, &action.message)
}

fn walk_event_message<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    owner: EventMessageOwner,
    span: Span,
    message: &SolveEventMessage,
) -> Result<(), V::Error> {
    for (part_index, part) in message.parts.iter().enumerate() {
        let SolveEventMessagePart::Conversion { value, format, .. } = part else {
            continue;
        };
        visitor.visit_linear_op_slice(
            LinearOpSliceOwner::EventMessageValue {
                owner,
                part_index,
                span,
            },
            value,
        )?;
        let SolveStringConversionFormat::Options {
            minimum_length,
            left_justified,
            significant_digits,
        } = format;
        for (field, program) in [
            (EventMessageFormatField::MinimumLength, minimum_length),
            (EventMessageFormatField::LeftJustified, left_justified),
            (
                EventMessageFormatField::SignificantDigits,
                significant_digits,
            ),
        ] {
            if let Some(program) = program {
                visitor.visit_linear_op_slice(
                    LinearOpSliceOwner::EventMessageFormat {
                        owner,
                        part_index,
                        field,
                        span,
                    },
                    program,
                )?;
            }
        }
    }
    Ok(())
}

const fn scalar_program_owner_for_compute_block(
    owner: ComputeBlockOwner,
) -> ScalarProgramBlockOwner {
    ScalarProgramBlockOwner::ComputeBlock(owner)
}

pub fn walk_continuous_artifacts<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    artifacts: &ContinuousSolveArtifacts,
) -> Result<(), V::Error> {
    visitor.visit_compute_block(
        ComputeBlockOwner::ContinuousImplicitJacobianV,
        &artifacts.implicit_jacobian_v,
    )?;
    visitor.visit_compute_block(
        ComputeBlockOwner::ContinuousManifoldJacobianV,
        &artifacts.manifold_jacobian_v,
    )?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::ContinuousFullJacobianV,
        &artifacts.full_jacobian_v,
    )?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::ContinuousImplicitJacobianVScalar,
        &artifacts.implicit_jacobian_v_scalar,
    )
}

pub fn walk_initialization_artifacts<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    artifacts: &InitializationSolveArtifacts,
) -> Result<(), V::Error> {
    visitor.visit_compute_block(
        ComputeBlockOwner::InitializationResidualJacobianV,
        &artifacts.residual_jacobian_v,
    )
}

pub fn walk_compute_block<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    owner: ComputeBlockOwner,
    block: &ComputeBlock,
) -> Result<(), V::Error> {
    for (node_index, node) in block.nodes.iter().enumerate() {
        visitor.visit_compute_node(owner, node_index, node)?;
    }
    Ok(())
}

pub fn walk_compute_node<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    owner: ComputeBlockOwner,
    node_index: usize,
    node: &ComputeNode,
) -> Result<(), V::Error> {
    match node {
        ComputeNode::ScalarPrograms(block) => {
            visitor.visit_scalar_program_block(scalar_program_owner_for_compute_block(owner), block)
        }
        ComputeNode::MatMul {
            lhs_ops,
            rhs_ops,
            span,
            ..
        } => {
            visitor.visit_linear_op_slice(
                LinearOpSliceOwner::MatMulLhs {
                    owner,
                    node_index,
                    span: *span,
                },
                lhs_ops,
            )?;
            visitor.visit_linear_op_slice(
                LinearOpSliceOwner::MatMulRhs {
                    owner,
                    node_index,
                    span: *span,
                },
                rhs_ops,
            )
        }
        ComputeNode::LinSolve {
            setup_ops, span, ..
        } => visitor.visit_linear_op_slice(
            LinearOpSliceOwner::LinSolveSetup {
                owner,
                node_index,
                span: *span,
            },
            setup_ops,
        ),
        ComputeNode::Map { base_ops, span, .. } => visitor.visit_linear_op_slice(
            LinearOpSliceOwner::MapBase {
                owner,
                node_index,
                span: *span,
            },
            base_ops,
        ),
        ComputeNode::AffineStencil { base_ops, span, .. } => visitor.visit_linear_op_slice(
            LinearOpSliceOwner::AffineStencilBase {
                owner,
                node_index,
                span: *span,
            },
            base_ops,
        ),
    }
}

pub fn walk_scalar_program_block<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    owner: ScalarProgramBlockOwner,
    block: &ScalarProgramBlock,
) -> Result<(), V::Error> {
    for (program_index, program) in block.programs().iter().enumerate() {
        visitor.visit_scalar_program(
            owner,
            program_index,
            block.program_span(program_index),
            program,
        )?;
    }
    Ok(())
}

pub fn walk_linear_op_slice<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    kind: LinearOpSliceOwner,
    ops: &[LinearOp],
) -> Result<(), V::Error> {
    for (op_index, op) in ops.iter().enumerate() {
        visitor.visit_linear_op(kind, op_index, op)?;
    }
    Ok(())
}

/// Walk the typed regions owned by one linear operation.
///
/// The enclosing slice identity remains authoritative for nested regions: the
/// regions are part of that same checked program and inherit its source span.
pub fn walk_linear_op<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    kind: LinearOpSliceOwner,
    _op_index: usize,
    op: &LinearOp,
) -> Result<(), V::Error> {
    match op {
        LinearOp::FunctionFold { program, .. }
        | LinearOp::GuardedFunctionFold { program, .. }
        | LinearOp::StoreOutputFunctionFold { program, .. } => {
            visitor.visit_linear_op_slice(kind, program.update())
        }
        LinearOp::FunctionConditional { program, .. } => {
            for arm in program.arms() {
                visitor.visit_linear_op_slice(kind, arm.condition())?;
                visitor.visit_linear_op_slice(kind, arm.result())?;
            }
            visitor.visit_linear_op_slice(kind, program.fallback())
        }
        _ => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{BinaryOp, Reg};
    use rumoca_core::SourceId;
    use std::convert::Infallible;

    #[derive(Default)]
    struct CountingVisitor {
        nodes: usize,
        rows: usize,
        ops: usize,
        kinds: Vec<LinearOpSliceOwner>,
    }

    impl SolveVisitor for CountingVisitor {
        type Error = Infallible;

        fn visit_compute_node(
            &mut self,
            owner: ComputeBlockOwner,
            node_index: usize,
            node: &ComputeNode,
        ) -> Result<(), Self::Error> {
            self.nodes += 1;
            walk_compute_node(self, owner, node_index, node)
        }

        fn visit_scalar_program(
            &mut self,
            owner: ScalarProgramBlockOwner,
            program_index: usize,
            span: Option<Span>,
            ops: &[LinearOp],
        ) -> Result<(), Self::Error> {
            self.rows += 1;
            self.visit_linear_op_slice(
                LinearOpSliceOwner::ScalarProgram {
                    owner,
                    program_index,
                    span,
                },
                ops,
            )
        }

        fn visit_linear_op(
            &mut self,
            kind: LinearOpSliceOwner,
            op_index: usize,
            op: &LinearOp,
        ) -> Result<(), Self::Error> {
            self.ops += 1;
            self.kinds.push(kind);
            walk_linear_op(self, kind, op_index, op)
        }
    }

    fn store_row(src: Reg) -> Vec<LinearOp> {
        vec![
            LinearOp::Const {
                dst: src,
                value: 0.0,
            },
            LinearOp::StoreOutput { src },
        ]
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
            lhs_pattern: crate::fixture_pattern(1, 1, false),
            rhs_pattern: crate::fixture_pattern(1, 1, false),
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
            matrix_pattern: crate::fixture_pattern(1, 1, false),
            metadata: crate::TensorNodeMetadata::default(),
            span,
        }
    }

    fn single_binder_domain() -> crate::StructuredIndexDomain {
        crate::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
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
        let span = Span::from_offsets(SourceId::from_source_name(file!()), 0, 1);
        let block = ComputeBlock {
            nodes: vec![
                ComputeNode::ScalarPrograms(
                    ScalarProgramBlock::with_source_span(
                        vec![store_row(0)],
                        span.require_provenance("Solve visitor fixture")
                            .expect("fixture span is source-backed"),
                    )
                    .expect("visitor scalar fixture is computable"),
                ),
                matmul_node(span),
                linsolve_node(span),
                map_node(span),
                affine_stencil_node(span),
            ],
        };

        let mut visitor = CountingVisitor::default();
        let owner = ComputeBlockOwner::ContinuousImplicitRhs;
        visitor.visit_compute_block(owner, &block).unwrap();

        assert_eq!(visitor.nodes, 5);
        assert_eq!(visitor.rows, 1);
        assert_eq!(visitor.ops, 9);
        assert!(visitor.kinds.contains(&LinearOpSliceOwner::MatMulLhs {
            owner,
            node_index: 1,
            span
        }));
        assert!(visitor.kinds.contains(&LinearOpSliceOwner::MatMulRhs {
            owner,
            node_index: 1,
            span
        }));
        assert!(visitor.kinds.contains(&LinearOpSliceOwner::LinSolveSetup {
            owner,
            node_index: 2,
            span
        }));
        assert!(visitor.kinds.contains(&LinearOpSliceOwner::MapBase {
            owner,
            node_index: 3,
            span
        }));
        assert!(
            visitor
                .kinds
                .contains(&LinearOpSliceOwner::AffineStencilBase {
                    owner,
                    node_index: 4,
                    span
                })
        );
    }
}
