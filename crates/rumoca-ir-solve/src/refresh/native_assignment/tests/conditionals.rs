//! Scoped captures, complete fault prefixes and exact dependency ordering.

use super::*;
use crate::{FunctionConditionalProgram, ScalarProgramBlock};
use std::sync::Arc;

fn conditional(body: Vec<LinearOp>) -> LinearOp {
    LinearOp::FunctionConditional {
        dst_start: 3,
        capture_start: 1,
        program: Arc::new(
            FunctionConditionalProgram::checked(
                2,
                vec![1],
                [(
                    vec![
                        LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                        LinearOp::StoreOutput { src: 0 },
                    ],
                    body,
                )],
                vec![
                    LinearOp::Const { dst: 0, value: 7. },
                    LinearOp::StoreOutput { src: 0 },
                ],
            )
            .unwrap(),
        ),
    }
}

fn body() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ]
}

fn operations(body: Vec<LinearOp>, capture: LinearOp) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        capture,
        conditional(body),
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ]
}

fn source(programs: Vec<Vec<LinearOp>>) -> ComputeBlock {
    let spans =
        vec![Span::from_offsets(SourceId::from_source_name("Scoped.mo"), 4, 13); programs.len()];
    ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_program_spans(programs, spans).unwrap(),
    )
}

#[test]
fn conditional_captures_preserve_original_prefix_and_scoped_namespace() {
    let operations = operations(body(), LinearOp::LoadP { dst: 2, index: 1 });
    let block = source(vec![operations.clone()]);
    let layout = VarLayout::from_parts(Default::default(), 1, 2);
    let schedule = derive(&block, &[Some(scalar_slot_y(0))], &layout).unwrap();
    let ComputeNode::ScalarPrograms(value) = &schedule.stages()[0].value_kernel().nodes[0] else {
        panic!("scalar")
    };
    assert!(operations_match(
        &operations[..4],
        &value.programs()[0][..4]
    ));
    assert_eq!(
        value.programs()[0].last(),
        Some(&LinearOp::StoreOutput { src: 3 })
    );
}

#[test]
fn all_conditional_regions_retain_external_y_dependencies_and_order() {
    let mut dependent = body();
    dependent.insert(0, LinearOp::LoadY { dst: 9, index: 1 });
    let block = source(vec![
        operations(dependent, LinearOp::LoadP { dst: 2, index: 1 }),
        vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::LoadP { dst: 1, index: 1 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    ]);
    let layout = VarLayout::from_parts(Default::default(), 2, 2);
    let schedule = derive(
        &block,
        &[Some(scalar_slot_y(0)), Some(scalar_slot_y(1))],
        &layout,
    )
    .unwrap();
    assert_eq!(
        schedule.stages()[0].source_projection,
        SourceProjection::Scalar {
            program: 1,
            output: 1,
            stores: vec![LinearOp::StoreOutput { src: 2 }],
        }
    );
}

#[test]
fn unused_own_target_capture_and_region_reads_cannot_hide_coupling() {
    let layout = VarLayout::from_parts(Default::default(), 1, 2);
    let captures = operations(body(), LinearOp::LoadY { dst: 2, index: 0 });
    assert_eq!(
        derive(&source(vec![captures]), &[Some(scalar_slot_y(0))], &layout)
            .unwrap_err()
            .0,
        "native conditional captures depend on its own assignment target"
    );
    let mut own = body();
    own.insert(0, LinearOp::LoadY { dst: 9, index: 0 });
    let block = source(vec![operations(own, LinearOp::LoadP { dst: 2, index: 1 })]);
    assert_eq!(
        derive(&block, &[Some(scalar_slot_y(0))], &layout)
            .unwrap_err()
            .0,
        "native conditional region reads its own assignment target"
    );
}

#[test]
fn inactive_region_bounds_and_range_publication_refuse_before_admission() {
    let layout = VarLayout::from_parts(Default::default(), 1, 2);
    let mut invalid = body();
    invalid.insert(0, LinearOp::LoadP { dst: 9, index: 2 });
    let block = source(vec![operations(
        invalid,
        LinearOp::LoadP { dst: 2, index: 1 },
    )]);
    assert_eq!(
        derive(&block, &[Some(scalar_slot_y(0))], &layout)
            .unwrap_err()
            .0,
        "native tensor load exceeds its owned variable layout"
    );
    let block = source(vec![operations(
        vec![
            LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 1 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 1,
                stride: 1,
            },
        ],
        LinearOp::LoadP { dst: 2, index: 1 },
    )]);
    assert_eq!(
        derive(&block, &[Some(scalar_slot_y(0))], &layout)
            .unwrap_err()
            .0,
        "native tensor program contains unsupported or effectful operations"
    );
}
