use super::*;
use crate::{
    ImplicitRowTargets, ProjectionBlocks, ProjectionStage, RefreshScalarProgramSource,
    ScalarProgramBlock, SolveLayout, SolveVariableDeclaration, SolveVariableStorageRole,
    SolveVariableStorageRun, SolveVariableValueKind, TensorInputKind,
};

fn fixture(count: usize) -> (ComputeBlock, ProjectionBlocks, SolveLayout, VarLayout) {
    let n = u32::try_from(count).unwrap();
    let mut operations = Vec::new();
    for ordinal in 0..2u32 {
        let base = 3 * n * ordinal;
        operations.extend([
            LinearOp::TensorLoad {
                dst_start: base,
                input: TensorInputKind::Y,
                input_start: count * ordinal as usize,
                count,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorLoad {
                dst_start: base + n,
                input: TensorInputKind::P,
                input_start: count * ordinal as usize,
                count,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorBinary {
                dst_start: base + 2 * n,
                op: BinaryOp::Sub,
                lhs_start: base,
                rhs_start: base + n,
                count,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            },
        ]);
    }
    // Two declarations share the same actual prefix. Source store order is
    // deliberately different from declaration/Y storage order.
    operations.extend([
        LinearOp::StoreOutputRange {
            start: 5 * n,
            count,
            stride: 1,
        },
        LinearOp::StoreOutputRange {
            start: 2 * n,
            count,
            stride: 1,
        },
    ]);
    let source = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_program_spans(
            vec![operations],
            vec![Span::from_offsets(
                SourceId::from_source_name("FusedDeclarations.mo"),
                0,
                1,
            )],
        )
        .unwrap(),
    );
    let storage = |index| SolveVariableStorageRun {
        base: scalar_slot_y(index),
        scalar_count: count,
        role: SolveVariableStorageRole::Algebraic,
        value_kind: SolveVariableValueKind::Real,
    };
    let declarations = SolveLayout {
        variable_storage_runs: vec![storage(0), storage(count)],
        variable_declarations: vec![
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::Algebraic,
                SolveVariableValueKind::Real,
            );
            2
        ],
        algebraic_scalar_count: 2 * count,
        ..Default::default()
    };
    let selector = RefreshScalarProgramSource::checked(0, 0).unwrap();
    let stages = ProjectionBlocks::from_stages(vec![
        ProjectionStage::SingletonDeclarationRun {
            source: selector,
            first_row: 0,
            declaration: 1,
            storage: storage(count),
        },
        ProjectionStage::SingletonDeclarationRun {
            source: selector,
            first_row: count,
            declaration: 0,
            storage: storage(0),
        },
    ])
    .unwrap();
    (
        source,
        stages,
        declarations,
        VarLayout::from_parts(Default::default(), 2 * count, 2 * count),
    )
}

#[test]
fn fused_declarations_keep_correlated_stores_and_execute_one_native_prefix() {
    for count in [7, 131_072] {
        let (source, stages, declarations, layout) = fixture(count);
        assert_eq!(stages.len(), 2 * count);
        assert_eq!(stages.stages().len(), 2);
        assert!(serde_json::to_vec(&stages).unwrap().len() < 1_000);
        for stage in stages.stages() {
            stage
                .checked_native_run(&source, &declarations, &layout)
                .unwrap();
        }
        let targets = ImplicitRowTargets::from_projection(&stages);
        let families = derive_with(
            &source,
            TargetInventory::Projection(&targets),
            &layout,
            Vec::new(),
        )
        .unwrap();
        assert_eq!(families.len(), 1);
        let stage = &families[0].stage;
        assert_eq!(stage.target_range(), Some(0..2 * count));
        let [ComputeNode::ScalarPrograms(kernel)] = stage.value_kernel().nodes.as_slice() else {
            panic!("scalar source family was changed")
        };
        assert_eq!(kernel.row_count(), 1);
        // The isolated stores read the P loads; the target loads and the
        // residual differences nothing reads are not part of the stage.
        assert_eq!(kernel.programs()[0].len(), 4);
        assert_eq!(kernel.program_output_span(0, 2).unwrap().start(), count);
        assert_eq!(kernel.program_output_span(0, 3).unwrap().start(), 0);
        assert_eq!(kernel.stored_output_count(), 2 * count);
        assert_eq!(targets.target_at(0), Some(scalar_slot_y(count)));
        assert_eq!(targets.target_at(count), Some(scalar_slot_y(0)));
    }
}

#[test]
fn equal_row_and_target_sets_cannot_exchange_fused_store_pairing() {
    let (source, stages, declarations, layout) = fixture(7);
    let mut inputs = stages.into_stage_inputs();
    let ProjectionStage::SingletonDeclarationRun {
        declaration,
        storage,
        ..
    } = &mut inputs[0]
    else {
        unreachable!()
    };
    *declaration = 0;
    *storage = declarations.variable_storage_runs[0];
    assert!(
        inputs[0]
            .checked_native_run(&source, &declarations, &layout)
            .is_err()
    );
}

#[test]
fn compact_refresh_recipe_owns_complete_source_and_refuses_scalar_dispatch() {
    let count = 7;
    let (source, stages, declarations, layout) = fixture(count);
    let mut problem = crate::SolveProblem {
        layout,
        solve_layout: declarations,
        ..crate::SolveProblem::default()
    };
    problem.continuous.implicit_rhs = source;
    problem.continuous.implicit_row_targets = ImplicitRowTargets::from_projection(&stages);
    problem.continuous.algebraic_projection_plan.blocks = stages;
    let owners = crate::ContinuousRefreshOwners::try_compact_for_problem(&problem)
        .unwrap()
        .expect("complete paired source is admitted");
    assert!(owners.is_issued());
    assert_complete_value_projection(
        &owners,
        &problem.continuous.algebraic_projection_plan.blocks,
    );
    assert!(owners.algebraic().rows.is_empty());
    assert!(owners.algebraic().simultaneous_block_indices.is_empty());
    assert_eq!(
        owners.algebraic().simultaneous_plan.blocks.stages().len(),
        2
    );
    assert!(
        owners
            .staged_refresh_steps(owners.algebraic(), crate::RefreshStageSchedule::Uncertified)
            .is_err()
    );
    let schedule = owners.native_assignment_schedule().unwrap();
    assert_eq!(schedule.stages().len(), 1);
    let encoded = serde_json::to_vec(&owners).unwrap();
    let decoded: crate::ContinuousRefreshOwners = serde_json::from_slice(&encoded).unwrap();
    assert!(!decoded.is_issued(), "wire cannot admit a native receipt");
    assert_complete_value_projection(&decoded, &decoded.algebraic().simultaneous_plan.blocks);
    let mut incomplete: serde_json::Value = serde_json::from_slice(&encoded).unwrap();
    incomplete["algebraic"]["value_projection_plan"] =
        serde_json::to_value(crate::AlgebraicProjectionPlan::default()).unwrap();
    assert!(serde_json::from_value::<crate::ContinuousRefreshOwners>(incomplete).is_err());
    problem.continuous.refresh_owners = decoded;
    crate::issue_native_assignment_schedule(&mut problem);
    assert_complete_value_projection(
        &problem.continuous.refresh_owners,
        &problem.continuous.algebraic_projection_plan.blocks,
    );
    let structures = crate::ContinuousStructuralArtifacts::checked_assignment_for_problem(&problem)
        .unwrap()
        .expect("checked stateless source owns assignment artifacts");
    assert_eq!(
        structures.require_scalar_derivatives(),
        Err(crate::ScalarArtifactRefusal::CheckedAssignmentOnly)
    );
    let assignment = structures.checked_assignment().unwrap();
    assignment.check_source(&problem).unwrap();
    let source_wire = serde_json::to_vec(&problem.continuous.implicit_rhs).unwrap();
    let mut replay = problem.clone();
    replay.continuous.implicit_rhs = serde_json::from_slice(&source_wire).unwrap();
    assert_eq!(
        assignment.check_source(&replay),
        Err(crate::ScalarArtifactRefusal::ForeignAssignmentOwner)
    );
    crate::issue_native_assignment_schedule(&mut replay);
    let reconstructed =
        crate::ContinuousStructuralArtifacts::checked_assignment_for_problem(&replay)
            .unwrap()
            .unwrap();
    reconstructed
        .checked_assignment()
        .unwrap()
        .check_source(&replay)
        .unwrap();
    let projection_wire =
        serde_json::to_vec(&problem.continuous.algebraic_projection_plan.blocks).unwrap();
    replay = problem.clone();
    replay.continuous.algebraic_projection_plan.blocks =
        serde_json::from_slice(&projection_wire).unwrap();
    assert_eq!(
        assignment.check_source(&replay),
        Err(crate::ScalarArtifactRefusal::ForeignAssignmentOwner)
    );
    assert!(problem.continuous.refresh_owners.is_issued());
    problem
        .continuous
        .refresh_owners
        .validate_compact_root(&problem)
        .unwrap();
    assert!(
        problem
            .continuous
            .implicit_row_targets
            .shares_projection_owner(
                problem
                    .continuous
                    .refresh_owners
                    .compact_projection()
                    .unwrap()
            )
    );
}

fn assert_complete_value_projection(
    owners: &crate::ContinuousRefreshOwners,
    source: &ProjectionBlocks,
) {
    assert!(!source.is_empty());
    for plan in [
        owners.algebraic(),
        owners.derivative(),
        owners.root(),
        owners.event(),
    ] {
        assert!(plan.rows.is_empty());
        assert!(plan.simultaneous_block_indices.is_empty());
        assert!(plan.simultaneous_plan.blocks.shares_stage_owner(source));
        assert!(plan.value_projection_plan.blocks.shares_stage_owner(source));
    }
}

#[test]
fn an_empty_problem_does_not_issue_a_compact_refresh_recipe() {
    assert!(
        crate::ContinuousRefreshOwners::try_compact_for_problem(&crate::SolveProblem::default())
            .unwrap()
            .is_none()
    );
}

#[test]
fn compact_refresh_refuses_a_fused_prefix_that_reads_another_owned_target() {
    let count = 7;
    let (source, stages, declarations, layout) = fixture(count);
    let [ComputeNode::ScalarPrograms(program)] = source.nodes.as_slice() else {
        unreachable!()
    };
    let mut operations = program.programs()[0].to_vec();
    let LinearOp::TensorLoad {
        input, input_start, ..
    } = &mut operations[1]
    else {
        unreachable!()
    };
    *input = TensorInputKind::Y;
    *input_start = count;
    let source = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_program_spans(
            vec![operations],
            vec![program.program_span(0).unwrap()],
        )
        .unwrap(),
    );
    for stage in stages.stages() {
        stage
            .checked_native_run(&source, &declarations, &layout)
            .unwrap();
    }
    let mut problem = crate::SolveProblem {
        layout,
        solve_layout: declarations,
        ..crate::SolveProblem::default()
    };
    problem.continuous.implicit_rhs = source;
    problem.continuous.implicit_row_targets = ImplicitRowTargets::from_projection(&stages);
    problem.continuous.algebraic_projection_plan.blocks = stages;
    let owners = crate::ContinuousRefreshOwners::try_compact_for_problem(&problem)
        .unwrap()
        .expect("native refusal preserves the canonical source owner");
    assert!(owners.is_issued());
    assert!(owners.native_assignment_refusal().is_some());
    assert!(owners.native_assignment_schedule().is_none());
    assert!(
        owners
            .staged_refresh_steps(owners.algebraic(), crate::RefreshStageSchedule::Uncertified)
            .is_err()
    );
}

#[test]
fn compact_source_owner_preserves_a_real_nonlinear_coupled_block() {
    let count = 7;
    let (source, stages, mut declarations, _) = fixture(count);
    let [ComputeNode::ScalarPrograms(fused)] = source.nodes.as_slice() else {
        unreachable!()
    };
    let x = 2 * count;
    let z = x + 1;
    let operations = vec![
        LinearOp::LoadY { dst: 0, index: x },
        LinearOp::LoadY { dst: 1, index: z },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 3 },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 5,
            op: BinaryOp::Add,
            lhs: 4,
            rhs: 0,
        },
        LinearOp::StoreOutput { src: 5 },
    ];
    let span = fused.program_span(0).unwrap();
    let source = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_program_spans(
            vec![fused.programs()[0].to_vec(), operations],
            vec![span, span],
        )
        .unwrap(),
    );
    let coupled = crate::AlgebraicProjectionBlock {
        rows: vec![x, z],
        y_indices: vec![x, z],
        ..Default::default()
    };
    let mut stages = stages.into_stage_inputs();
    stages.push(ProjectionStage::Explicit(coupled.clone()));
    let stages = ProjectionBlocks::from_stages(stages).unwrap();
    declarations.algebraic_scalar_count = z + 1;
    let mut problem = crate::SolveProblem {
        layout: VarLayout::from_parts(Default::default(), z + 1, 2 * count),
        solve_layout: declarations,
        ..crate::SolveProblem::default()
    };
    problem.continuous.implicit_rhs = source;
    problem.continuous.implicit_row_targets = ImplicitRowTargets::from_projection(&stages);
    problem.continuous.algebraic_projection_plan.blocks = stages;
    let owners = crate::ContinuousRefreshOwners::try_compact_for_problem(&problem)
        .unwrap()
        .expect("valid coupled source remains computable");
    assert!(owners.native_assignment_refusal().is_some());
    assert_complete_value_projection(
        &owners,
        &problem.continuous.algebraic_projection_plan.blocks,
    );
    problem.continuous.refresh_owners = owners.clone();
    assert!(
        crate::ContinuousStructuralArtifacts::checked_assignment_for_problem(&problem)
            .unwrap()
            .is_none()
    );
    assert_eq!(
        owners
            .algebraic()
            .simultaneous_plan
            .blocks
            .explicit_blocks()
            .collect::<Vec<_>>(),
        vec![&coupled]
    );
}
