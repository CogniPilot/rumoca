//! Layout addressability of every program-bearing Solve owner.
//!
//! The canonical Solve visitor is the sole owner inventory. Primal programs
//! own no seed ABI; each derived artifact receives the exact seed domain that
//! its issuing AD mode defines.

use rumoca_core::Span;

use crate::{
    ComputeBlockOwner, ComputeNode, LinearOp, LinearOpSliceOwner, ScalarProgramBlockOwner,
    SolveModel, SolveProblem, SolveProblemShapeContractError, SolveVisitor, TensorInputKind,
    VarLayout,
};

/// Closed seed policy for every Solve program owner.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SeedDomain {
    Forbidden,
    SolverY,
    SolverYAndParameters,
}

impl SeedDomain {
    fn extent(self, layout: &VarLayout) -> Result<usize, SolveProblemShapeContractError> {
        match self {
            Self::Forbidden => Ok(0),
            Self::SolverY => Ok(layout.y_scalars()),
            Self::SolverYAndParameters => layout.y_scalars().checked_add(layout.p_scalars()).ok_or(
                SolveProblemShapeContractError::VariableIndexOutOfBounds {
                    context: "Solve seed domain",
                    storage: "seed",
                    index: usize::MAX,
                    extent: usize::MAX,
                    span: None,
                },
            ),
        }
    }
}

/// Validate every primal program owner exactly once at `SolveProblem` sealing.
pub(crate) fn validate_solve_problem_variable_bounds(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    VariableBoundsVisitor::new(problem.layout()).visit_solve_problem(problem)
}

/// Validate every derived-artifact and visible-row owner exactly once at the
/// `SolveModel` join. The already sealed primal problem is not revisited.
pub(crate) fn validate_solve_model_program_bounds(
    model: &SolveModel,
) -> Result<(), SolveProblemShapeContractError> {
    let mut visitor = VariableBoundsVisitor::new(model.problem().layout());
    visitor.visit_solve_artifacts(model.artifacts())?;
    visitor.visit_scalar_program_block(
        ScalarProgramBlockOwner::VisibleValueRows,
        model.visible_value_rows(),
    )
}

struct VariableBoundsVisitor<'layout> {
    layout: &'layout VarLayout,
    context: &'static str,
    seed_domain: SeedDomain,
}

impl<'layout> VariableBoundsVisitor<'layout> {
    const fn new(layout: &'layout VarLayout) -> Self {
        Self {
            layout,
            context: "Solve program",
            seed_domain: SeedDomain::Forbidden,
        }
    }

    fn with_owner<T>(
        &mut self,
        context: &'static str,
        seed_domain: SeedDomain,
        visit: impl FnOnce(&mut Self) -> Result<T, SolveProblemShapeContractError>,
    ) -> Result<T, SolveProblemShapeContractError> {
        let previous_context = std::mem::replace(&mut self.context, context);
        let previous_domain = std::mem::replace(&mut self.seed_domain, seed_domain);
        let result = visit(self);
        self.context = previous_context;
        self.seed_domain = previous_domain;
        result
    }

    fn check_run(
        &self,
        storage: &'static str,
        base: usize,
        count: usize,
        extent: usize,
        owner: LinearOpSliceOwner,
    ) -> Result<(), SolveProblemShapeContractError> {
        if count == 0 {
            return Ok(());
        }
        let last = base
            .checked_add(count)
            .and_then(|end| end.checked_sub(1))
            .ok_or(SolveProblemShapeContractError::VariableIndexOutOfBounds {
                context: self.context,
                storage,
                index: base,
                extent,
                span: slice_span(owner),
            })?;
        if last < extent {
            return Ok(());
        }
        Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
            context: self.context,
            storage,
            index: last,
            extent,
            span: slice_span(owner),
        })
    }

    fn check_seed_run(
        &self,
        base: usize,
        count: usize,
        owner: LinearOpSliceOwner,
    ) -> Result<(), SolveProblemShapeContractError> {
        if self.seed_domain == SeedDomain::Forbidden {
            return Err(SolveProblemShapeContractError::SeedAccessForbidden {
                context: self.context,
                span: slice_span(owner),
            });
        }
        self.check_run(
            "seed",
            base,
            count,
            self.seed_domain.extent(self.layout)?,
            owner,
        )
    }
}

impl SolveVisitor for VariableBoundsVisitor<'_> {
    type Error = SolveProblemShapeContractError;

    fn visit_compute_block(
        &mut self,
        owner: ComputeBlockOwner,
        block: &crate::ComputeBlock,
    ) -> Result<(), Self::Error> {
        let seed_domain = match owner {
            ComputeBlockOwner::ContinuousImplicitJacobianV
            | ComputeBlockOwner::ContinuousManifoldJacobianV => SeedDomain::SolverY,
            ComputeBlockOwner::InitializationResidualJacobianV => SeedDomain::SolverYAndParameters,
            ComputeBlockOwner::ContinuousImplicitRhs
            | ComputeBlockOwner::ContinuousResidual
            | ComputeBlockOwner::ContinuousManifoldResidual
            | ComputeBlockOwner::ContinuousDerivativeRhs
            | ComputeBlockOwner::InitializationResidual
            | ComputeBlockOwner::DiscreteStructuredRhs => SeedDomain::Forbidden,
        };
        self.with_owner(owner.context(), seed_domain, |visitor| {
            crate::visitor::walk_compute_block(visitor, owner, block)
        })
    }

    fn visit_scalar_program_block(
        &mut self,
        owner: ScalarProgramBlockOwner,
        block: &crate::ScalarProgramBlock,
    ) -> Result<(), Self::Error> {
        let seed_domain = match owner {
            ScalarProgramBlockOwner::ComputeBlock(compute_owner) => match compute_owner {
                ComputeBlockOwner::ContinuousImplicitJacobianV
                | ComputeBlockOwner::ContinuousManifoldJacobianV => SeedDomain::SolverY,
                ComputeBlockOwner::InitializationResidualJacobianV => {
                    SeedDomain::SolverYAndParameters
                }
                _ => SeedDomain::Forbidden,
            },
            ScalarProgramBlockOwner::ContinuousFullJacobianV
            | ScalarProgramBlockOwner::ContinuousImplicitJacobianVScalar => {
                SeedDomain::SolverYAndParameters
            }
            _ => SeedDomain::Forbidden,
        };
        self.with_owner(owner.context(), seed_domain, |visitor| {
            crate::visitor::walk_scalar_program_block(visitor, owner, block)
        })
    }

    fn visit_compute_node(
        &mut self,
        owner: ComputeBlockOwner,
        node_index: usize,
        node: &ComputeNode,
    ) -> Result<(), Self::Error> {
        let Some(ranges) =
            crate::tensor::affine_load_index_ranges_for_node(node, owner.context(), node_index)?
        else {
            return crate::visitor::walk_compute_node(self, owner, node_index, node);
        };
        let (ops, slice_owner) = match node {
            ComputeNode::Map { base_ops, span, .. } => (
                base_ops.as_slice(),
                LinearOpSliceOwner::MapBase {
                    owner,
                    node_index,
                    span: *span,
                },
            ),
            ComputeNode::AffineStencil { base_ops, span, .. } => (
                base_ops.as_slice(),
                LinearOpSliceOwner::AffineStencilBase {
                    owner,
                    node_index,
                    span: *span,
                },
            ),
            _ => unreachable!("affine range query only returns affine nodes"),
        };
        for (op_index, op) in ops.iter().enumerate() {
            if ranges.empty_domain {
                self.visit_linear_op(slice_owner, op_index, op)?;
                continue;
            }
            let Some((_, maximum)) = ranges.by_op[op_index] else {
                self.visit_linear_op(slice_owner, op_index, op)?;
                continue;
            };
            match *op {
                LinearOp::LoadY { .. } => {
                    self.check_run("Y", maximum, 1, self.layout.y_scalars(), slice_owner)?;
                }
                LinearOp::LoadP { .. } => {
                    self.check_run("P", maximum, 1, self.layout.p_scalars(), slice_owner)?;
                }
                LinearOp::LoadSeed { .. } => self.check_seed_run(maximum, 1, slice_owner)?,
                _ => {}
            }
            crate::visitor::walk_linear_op(self, slice_owner, op_index, op)?;
        }
        Ok(())
    }

    fn visit_linear_op(
        &mut self,
        owner: LinearOpSliceOwner,
        op_index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        match *op {
            LinearOp::LoadY { index, .. } => {
                self.check_run("Y", index, 1, self.layout.y_scalars(), owner)?
            }
            LinearOp::LoadP { index, .. } => {
                self.check_run("P", index, 1, self.layout.p_scalars(), owner)?
            }
            LinearOp::LoadSeed { index, .. } => self.check_seed_run(index, 1, owner)?,
            LinearOp::TensorLoad {
                input,
                input_start,
                count,
                seed_start,
                ..
            } => {
                let (storage, extent) = match input {
                    TensorInputKind::Y => ("Y", self.layout.y_scalars()),
                    TensorInputKind::P => ("P", self.layout.p_scalars()),
                };
                self.check_run(storage, input_start, count, extent, owner)?;
                if let Some(seed_start) = seed_start {
                    self.check_seed_run(seed_start, count, owner)?;
                }
            }
            _ => {}
        }
        crate::visitor::walk_linear_op(self, owner, op_index, op)
    }
}

fn slice_span(owner: LinearOpSliceOwner) -> Option<Span> {
    let span = match owner {
        LinearOpSliceOwner::ScalarProgram { span, .. }
        | LinearOpSliceOwner::ScalarProgramConstruction { span, .. } => return span,
        LinearOpSliceOwner::GuardedAssignmentProgram { span, .. }
        | LinearOpSliceOwner::EventMessageValue { span, .. }
        | LinearOpSliceOwner::EventMessageFormat { span, .. }
        | LinearOpSliceOwner::MatMulLhs { span, .. }
        | LinearOpSliceOwner::MatMulRhs { span, .. }
        | LinearOpSliceOwner::LinSolveSetup { span, .. }
        | LinearOpSliceOwner::MapBase { span, .. }
        | LinearOpSliceOwner::AffineStencilBase { span, .. } => span,
    };
    (!span.is_dummy()).then_some(span)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        AffineStencilIndexStrideTerm, AffineStencilLoadStride, ComputeBlock,
        FunctionConditionalProgram, ScalarProgramBlock, StructuredIndexDomain, TensorNodeMetadata,
        TensorOutputMap,
    };
    use indexmap::IndexMap;
    use rumoca_core::{SourceId, StructuredIndexBinder, StructuredIndexBinderId};

    fn layout(y: usize, p: usize) -> VarLayout {
        VarLayout::from_parts(IndexMap::new(), y, p)
    }

    fn span() -> Span {
        Span::from_offsets(SourceId::from_source_name("variable_bounds_tests.mo"), 0, 1)
    }

    fn seed_row(operation: LinearOp) -> ScalarProgramBlock {
        ScalarProgramBlock::with_source_span(
            vec![vec![operation, LinearOp::StoreOutput { src: 0 }]],
            span()
                .require_provenance("variable-bounds fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("seed fixture has checked local register flow")
    }

    fn empty_affine_blocks(operation: LinearOp) -> [(&'static str, ComputeBlock); 2] {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 0,
                step: 1,
            }],
        };
        let output_map = TensorOutputMap::dense_contiguous(0, &domain)
            .expect("empty fixture domain has checked dense output strides");
        let map = ComputeBlock {
            nodes: vec![ComputeNode::Map {
                domain: domain.clone(),
                output_map: output_map.clone(),
                base_ops: vec![operation.clone()],
                load_strides: Vec::new(),
                const_strides: Vec::new(),
                metadata: TensorNodeMetadata::default(),
                span: span(),
            }],
        };
        let stencil = ComputeBlock {
            nodes: vec![ComputeNode::AffineStencil {
                domain,
                output_map,
                base_ops: vec![operation],
                load_strides: Vec::new(),
                const_strides: Vec::new(),
                metadata: TensorNodeMetadata::default(),
                span: span(),
            }],
        };
        [("Map", map), ("AffineStencil", stencil)]
    }

    fn assert_empty_affine_variable_refusal(
        layout: &VarLayout,
        owner: ComputeBlockOwner,
        operation: LinearOp,
        storage: &'static str,
        index: usize,
        extent: usize,
    ) {
        for (node_kind, block) in empty_affine_blocks(operation.clone()) {
            assert!(
                matches!(
                    VariableBoundsVisitor::new(layout).visit_compute_block(owner, &block),
                    Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
                        storage: actual_storage,
                        index: actual_index,
                        extent: actual_extent,
                        ..
                    }) if actual_storage == storage
                        && actual_index == index
                        && actual_extent == extent
                ),
                "empty {node_kind} did not refuse {storage}[{index}] against extent {extent}"
            );
        }
    }

    fn assert_empty_affine_seed_forbidden(
        layout: &VarLayout,
        owner: ComputeBlockOwner,
        operation: LinearOp,
    ) {
        for (node_kind, block) in empty_affine_blocks(operation.clone()) {
            assert!(
                matches!(
                    VariableBoundsVisitor::new(layout).visit_compute_block(owner, &block),
                    Err(SolveProblemShapeContractError::SeedAccessForbidden { .. })
                ),
                "empty {node_kind} admitted a seed read in a primal owner"
            );
        }
    }

    #[test]
    fn scalar_artifact_parameter_seed_is_admitted_but_the_next_column_is_refused() {
        let layout = layout(2, 3);
        let owner = ScalarProgramBlockOwner::ContinuousImplicitJacobianVScalar;
        let accepted = seed_row(LinearOp::LoadSeed { dst: 0, index: 4 });
        VariableBoundsVisitor::new(&layout)
            .visit_scalar_program_block(owner, &accepted)
            .expect("the scalar implicit JVP seed domain is exactly Y followed by P");

        let refused = seed_row(LinearOp::LoadSeed { dst: 0, index: 5 });
        assert!(matches!(
            VariableBoundsVisitor::new(&layout).visit_scalar_program_block(owner, &refused),
            Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
                storage: "seed",
                index: 5,
                extent: 5,
                ..
            })
        ));
    }

    #[test]
    fn every_scalar_owner_has_one_closed_seed_policy() {
        let layout = layout(1, 1);
        let row = seed_row(LinearOp::LoadSeed { dst: 0, index: 0 });
        for (owner, seed_allowed) in [
            (
                ScalarProgramBlockOwner::ComputeBlock(
                    ComputeBlockOwner::ContinuousImplicitJacobianV,
                ),
                true,
            ),
            (
                ScalarProgramBlockOwner::ComputeBlock(
                    ComputeBlockOwner::ContinuousManifoldJacobianV,
                ),
                true,
            ),
            (
                ScalarProgramBlockOwner::ComputeBlock(
                    ComputeBlockOwner::InitializationResidualJacobianV,
                ),
                true,
            ),
            (
                ScalarProgramBlockOwner::ComputeBlock(ComputeBlockOwner::ContinuousImplicitRhs),
                false,
            ),
            (
                ScalarProgramBlockOwner::ComputeBlock(ComputeBlockOwner::ContinuousResidual),
                false,
            ),
            (
                ScalarProgramBlockOwner::ComputeBlock(
                    ComputeBlockOwner::ContinuousManifoldResidual,
                ),
                false,
            ),
            (
                ScalarProgramBlockOwner::ComputeBlock(ComputeBlockOwner::ContinuousDerivativeRhs),
                false,
            ),
            (
                ScalarProgramBlockOwner::ComputeBlock(ComputeBlockOwner::InitializationResidual),
                false,
            ),
            (
                ScalarProgramBlockOwner::ComputeBlock(ComputeBlockOwner::DiscreteStructuredRhs),
                false,
            ),
            (ScalarProgramBlockOwner::InitializationUpdateRhs, false),
            (ScalarProgramBlockOwner::DiscreteRuntimeAssignmentRhs, false),
            (
                ScalarProgramBlockOwner::DiscretePostCommitAssignmentRhs,
                false,
            ),
            (ScalarProgramBlockOwner::DiscreteRhs, false),
            (
                ScalarProgramBlockOwner::DiscreteClockPartitionIntermediates,
                false,
            ),
            (ScalarProgramBlockOwner::EventRootConditions, false),
            (ScalarProgramBlockOwner::EventDynamicTimeEventRhs, false),
            (ScalarProgramBlockOwner::EventActionConditions, false),
            (ScalarProgramBlockOwner::EventDelaySourceRhs, false),
            (ScalarProgramBlockOwner::EventDelayTimeRhs, false),
            (ScalarProgramBlockOwner::EventDelayMaxRhs, false),
            (
                ScalarProgramBlockOwner::ContinuousExactRefreshAssignmentFinalProgram,
                false,
            ),
            (ScalarProgramBlockOwner::ContinuousFullJacobianV, true),
            (
                ScalarProgramBlockOwner::ContinuousImplicitJacobianVScalar,
                true,
            ),
            (ScalarProgramBlockOwner::VisibleValueRows, false),
        ] {
            let result =
                VariableBoundsVisitor::new(&layout).visit_scalar_program_block(owner, &row);
            assert_eq!(
                result.is_ok(),
                seed_allowed,
                "seed policy drifted for {owner:?}"
            );
            if !seed_allowed {
                assert!(matches!(
                    result,
                    Err(SolveProblemShapeContractError::SeedAccessForbidden { .. })
                ));
            }
        }
    }

    #[test]
    fn exact_refresh_final_program_cannot_smuggle_a_seed_load() {
        let program = seed_row(LinearOp::LoadSeed { dst: 0, index: 0 });
        let error = VariableBoundsVisitor::new(&layout(1, 1))
            .visit_scalar_program_block(
                ScalarProgramBlockOwner::ContinuousExactRefreshAssignmentFinalProgram,
                &program,
            )
            .expect_err("a primal exact-refresh program has no derivative seed ABI");
        assert!(matches!(
            error,
            SolveProblemShapeContractError::SeedAccessForbidden {
                context: "continuous.refresh.exact_assignment.final_program",
                ..
            }
        ));
    }

    #[test]
    fn tensor_and_nested_primal_seed_accesses_are_typed_refusals() {
        let layout = layout(1, 0);
        let tensor = seed_row(LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 1,
            seed_start: Some(0),
            lanes: 2,
        });
        assert!(matches!(
            VariableBoundsVisitor::new(&layout)
                .visit_scalar_program_block(ScalarProgramBlockOwner::VisibleValueRows, &tensor),
            Err(SolveProblemShapeContractError::SeedAccessForbidden { .. })
        ));

        let conditional = FunctionConditionalProgram::checked(
            0,
            [1],
            [(
                vec![
                    LinearOp::LoadSeed { dst: 0, index: 0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    LinearOp::Const { dst: 0, value: 0.0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
            )],
            vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        )
        .expect("nested conditional fixture is checked");
        let nested = [LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(conditional),
        }];
        assert!(matches!(
            VariableBoundsVisitor::new(&layout).visit_linear_op_slice(
                LinearOpSliceOwner::ScalarProgram {
                    owner: ScalarProgramBlockOwner::VisibleValueRows,
                    program_index: 0,
                    span: Some(span()),
                },
                &nested,
            ),
            Err(SolveProblemShapeContractError::SeedAccessForbidden { .. })
        ));
    }

    #[test]
    fn affine_seed_maximum_and_empty_primal_domain_cannot_bypass_policy() {
        let nonempty_domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        };
        let affine = |domain| ComputeBlock {
            nodes: vec![ComputeNode::Map {
                output_map: TensorOutputMap::dense_contiguous(0, &domain)
                    .expect("fixture domain has checked dense output strides"),
                domain,
                base_ops: vec![LinearOp::LoadSeed { dst: 0, index: 0 }],
                load_strides: vec![AffineStencilLoadStride {
                    op_position: 0,
                    terms: vec![AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: 1,
                    }],
                }],
                const_strides: Vec::new(),
                metadata: TensorNodeMetadata::default(),
                span: span(),
            }],
        };
        let solver_y_owner = ComputeBlockOwner::ContinuousImplicitJacobianV;
        assert!(matches!(
            VariableBoundsVisitor::new(&layout(1, 0))
                .visit_compute_block(solver_y_owner, &affine(nonempty_domain)),
            Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
                storage: "seed",
                index: 1,
                extent: 1,
                ..
            })
        ));

        let empty_domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 0,
                step: 1,
            }],
        };
        let primal_owner = ComputeBlockOwner::ContinuousResidual;
        assert!(matches!(
            VariableBoundsVisitor::new(&layout(1, 0))
                .visit_compute_block(primal_owner, &affine(empty_domain)),
            Err(SolveProblemShapeContractError::SeedAccessForbidden { .. })
        ));
    }

    #[test]
    fn empty_affine_scalar_loads_validate_their_direct_indices() {
        let layout = layout(2, 2);
        let primal = ComputeBlockOwner::ContinuousResidual;
        for (operation, storage, index) in [
            (LinearOp::LoadY { dst: 0, index: 2 }, "Y", 2),
            (LinearOp::LoadY { dst: 0, index: 3 }, "Y", 3),
            (LinearOp::LoadP { dst: 0, index: 2 }, "P", 2),
            (LinearOp::LoadP { dst: 0, index: 3 }, "P", 3),
        ] {
            assert_empty_affine_variable_refusal(&layout, primal, operation, storage, index, 2);
        }

        let derivative = ComputeBlockOwner::ContinuousImplicitJacobianV;
        for index in [2, 3] {
            assert_empty_affine_variable_refusal(
                &layout,
                derivative,
                LinearOp::LoadSeed { dst: 0, index },
                "seed",
                index,
                2,
            );
        }
        assert_empty_affine_seed_forbidden(
            &layout,
            primal,
            LinearOp::LoadSeed { dst: 0, index: 0 },
        );
    }

    #[test]
    fn empty_affine_tensor_loads_validate_input_and_seed_runs() {
        let layout = layout(2, 2);
        let primal = ComputeBlockOwner::ContinuousResidual;
        for (input, storage) in [(TensorInputKind::Y, "Y"), (TensorInputKind::P, "P")] {
            for (count, index) in [(1, 2), (2, 3)] {
                assert_empty_affine_variable_refusal(
                    &layout,
                    primal,
                    LinearOp::TensorLoad {
                        dst_start: 0,
                        input,
                        input_start: 2,
                        count,
                        seed_start: None,
                        lanes: 1,
                    },
                    storage,
                    index,
                    2,
                );
            }
        }

        let derivative = ComputeBlockOwner::ContinuousImplicitJacobianV;
        for (count, index) in [(1, 2), (2, 3)] {
            assert_empty_affine_variable_refusal(
                &layout,
                derivative,
                LinearOp::TensorLoad {
                    dst_start: 0,
                    input: TensorInputKind::Y,
                    input_start: 0,
                    count,
                    seed_start: Some(2),
                    lanes: 2,
                },
                "seed",
                index,
                2,
            );
        }
        assert_empty_affine_seed_forbidden(
            &layout,
            primal,
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: 1,
                seed_start: Some(0),
                lanes: 2,
            },
        );
    }
}
