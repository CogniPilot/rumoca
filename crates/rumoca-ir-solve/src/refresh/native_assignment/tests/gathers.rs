use super::*;
use crate::{ScalarProgramBlock, TensorIndex};

fn source(selector: LinearOp) -> ComputeBlock {
    ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_program_spans(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                selector,
                LinearOp::Const { dst: 2, value: 9. },
                LinearOp::LoadIndexedRegister {
                    dst: 3,
                    base: 2,
                    stride: 1,
                    dimensions: vec![1].into_boxed_slice(),
                    indices: vec![TensorIndex::Runtime(1)].into_boxed_slice(),
                },
                LinearOp::Binary {
                    dst: 4,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 3,
                },
                LinearOp::StoreOutput { src: 4 },
            ]],
            vec![Span::from_offsets(
                SourceId::from_source_name("Gather.mo"),
                1,
                9,
            )],
        )
        .unwrap(),
    )
}

#[test]
fn canonical_gather_admission_preserves_address_dependence_and_program_span() {
    let layout = VarLayout::from_parts(Default::default(), 1, 1);
    let independent = source(LinearOp::LoadP { dst: 1, index: 0 });
    let schedule = derive(&independent, &[Some(scalar_slot_y(0))], &layout).unwrap();
    let ComputeNode::ScalarPrograms(block) = &schedule.stages()[0].value_kernel().nodes[0] else {
        panic!("scalar")
    };
    assert_eq!(
        block.program_spans(),
        &[Span::from_offsets(
            SourceId::from_source_name("Gather.mo"),
            1,
            9
        )]
    );
    assert!(
        block.programs()[0]
            .iter()
            .any(|op| matches!(op, LinearOp::LoadIndexedRegister { .. }))
    );
    let dependent = source(LinearOp::LoadY { dst: 1, index: 0 });
    assert!(derive(&dependent, &[Some(scalar_slot_y(0))], &layout).is_err());
}
