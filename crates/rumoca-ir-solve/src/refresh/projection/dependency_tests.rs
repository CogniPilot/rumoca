use super::*;
use crate::{
    AlgebraicProjectionBlock, BinaryOp, BlockTearing, CausalStep, ComputeBlock, LinearOp,
    ScalarProgramBlock, SolveArithmeticProfile, SolveConversionOperator, SolveIntegerDomain,
    SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallTable, SolveRealFormat,
    SolveScalarType, SolveValue, SolveValueType, TensorInputKind,
};

fn at() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("bounds.mo"), 0, 1)
}

fn fallible_call() -> SolvePureCallTable {
    let arithmetic =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let array = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![1]).unwrap();
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![array, real.clone()],
            vec![SolvePureCallOutput::result(real); 3],
            at(),
            |builder, inputs, outputs| {
                let array = builder.load(inputs[0], at())?;
                let index = builder.load(inputs[1], at())?;
                let index = builder.convert(
                    SolveConversionOperator::RealToIntegerTowardZero,
                    index,
                    at(),
                )?;
                let value = builder.project_element_dynamic(array, &[index], at())?;
                builder.store(outputs[0], value, at())?;
                for (output, value) in [(outputs[1], 7.0), (outputs[2], 8.0)] {
                    let value = builder.constant(SolveValue::real(arithmetic, value), at())?;
                    builder.store(output, value, at())?;
                }
                Ok(())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn call_program(table: &SolvePureCallTable) -> Vec<LinearOp> {
    vec![
        LinearOp::Const {
            dst: 0,
            value: 42.0,
        },
        LinearOp::LoadY { dst: 1, index: 3 },
        LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: table.owners()[0].call_site(),
        },
        LinearOp::TensorLoad {
            dst_start: 5,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::LoadP { dst: 8, index: 0 },
        LinearOp::TensorBinary {
            dst_start: 9,
            op: BinaryOp::Mul,
            lhs_start: 5,
            rhs_start: 8,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 0,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 12,
            op: BinaryOp::Sub,
            lhs_start: 9,
            rhs_start: 2,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 12,
            count: 3,
            stride: 1,
        },
    ]
}

#[test]
fn tensor_lane_pruning_preserves_call_failure_scheduling_dependency() {
    let table = fallible_call();
    let program = call_program(&table);
    let dependency = super::super::dependency::ScalarProgramYDependency::new(&program);
    assert!(
        !dependency.depends_on(13, 3),
        "selected value is independent of the index"
    );
    let shape =
        super::super::assignment_shape::canonical_assignment_shape_for_output(&program, 1, 1)
            .unwrap();
    assert!(matches!(
        shape,
        crate::TargetAssignmentShape::TensorAffine { .. }
    ));
    assert!(
        dependency.assignment_depends_on(&shape, 3),
        "whole-call execution still needs the index"
    );
    let source = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_source_span(
            vec![
                program,
                vec![
                    LinearOp::LoadY { dst: 0, index: 3 },
                    LinearOp::Const { dst: 1, value: 1.0 },
                    LinearOp::Binary {
                        dst: 2,
                        op: BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    LinearOp::StoreOutput { src: 2 },
                ],
            ],
            at().require_provenance("bounds order").unwrap(),
        )
        .unwrap(),
    );
    let mut plan = AlgebraicProjectionPlan {
        blocks: vec![AlgebraicProjectionBlock {
            rows: vec![0, 1, 2, 3],
            y_indices: vec![0, 1, 2, 3],
            alternate_charts: vec![],
            guarded_tearing: Some(BlockTearing {
                tear_y_indices: vec![0, 2],
                residual_rows: vec![0, 2],
                causal_steps: vec![
                    CausalStep { row: 1, y_index: 1 },
                    CausalStep { row: 3, y_index: 3 },
                ],
            }),
            tearing: Some(BlockTearing {
                tear_y_indices: vec![0, 2, 3],
                residual_rows: vec![0, 2, 3],
                causal_steps: vec![CausalStep { row: 1, y_index: 1 }],
            }),
        }],
    };
    assert!(plan.blocks[0].has_valid_tearing_partitions());
    assert!(
        validate_tearing_sources(&plan, &source).is_err(),
        "the index must be recovered before the whole call executes"
    );
    plan.blocks[0]
        .guarded_tearing
        .as_mut()
        .unwrap()
        .causal_steps
        .reverse();
    validate_tearing_sources(&plan, &source).unwrap();
}
