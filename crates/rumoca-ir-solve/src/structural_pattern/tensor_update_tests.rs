use super::*;
use crate::{TensorIndex, TensorUpdateSubscript};

fn update_pattern(
    dimensions: &[u32],
    subscripts: Vec<TensorUpdateSubscript>,
    value_count: usize,
    lanes: usize,
) -> StructuralPattern {
    let count = dimensions.iter().map(|&n| n as usize).product::<usize>() * lanes;
    let columns = count + value_count * lanes + 1;
    let mut program = (0..columns)
        .map(|index| LinearOp::LoadSeed {
            dst: index as Reg,
            index,
        })
        .collect::<Vec<_>>();
    program.push(LinearOp::TensorUpdate {
        dst_start: columns as Reg,
        base_start: 0,
        value_start: count as Reg,
        dimensions: dimensions.into(),
        subscripts: subscripts.into_boxed_slice(),
        lanes,
    });
    program.push(LinearOp::StoreOutputRange {
        start: columns as Reg,
        count,
        stride: 1,
    });
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("tensor_update_pattern.mo"),
        0,
        1,
    );
    let block = ScalarProgramBlock::with_program_spans(vec![program], vec![span]).unwrap();
    StructuralPattern::derive_from_scalar_jvp(&block, count, columns, span).unwrap()
}

fn fixed(index: u32) -> TensorUpdateSubscript {
    TensorUpdateSubscript::Index(TensorIndex::Constant(index))
}

fn assert_rows(pattern: &StructuralPattern, expected: &[&[usize]]) {
    for (row, &columns) in expected.iter().enumerate() {
        let mut actual = Vec::new();
        pattern.visit_row_columns(row, |column| actual.push(column));
        assert_eq!(actual, columns, "output {row}");
    }
}

#[test]
fn constant_tensor_update_replaces_the_overwritten_dependency() {
    // The reduced RollingWheel owner: replace the third residual of a vector.
    let pattern = update_pattern(&[3], vec![fixed(2)], 1, 1);
    assert_rows(&pattern, &[&[0], &[1], &[3]]);
}

#[test]
fn constant_tensor_update_keeps_matrix_coordinates_and_ad_lanes_separate() {
    let row = update_pattern(&[2, 3], vec![fixed(1), TensorUpdateSubscript::Whole], 3, 2);
    assert_rows(
        &row,
        &[
            &[0],
            &[1],
            &[2],
            &[3],
            &[4],
            &[5],
            &[12],
            &[13],
            &[14],
            &[15],
            &[16],
            &[17],
        ],
    );
    let column = update_pattern(&[2, 3], vec![TensorUpdateSubscript::Whole, fixed(1)], 2, 1);
    assert_rows(&column, &[&[0], &[6], &[2], &[3], &[7], &[5]]);
}

#[test]
fn constant_tensor_update_preserves_the_order_of_multiple_whole_axes() {
    let pattern = update_pattern(
        &[2, 3, 2],
        vec![
            TensorUpdateSubscript::Whole,
            fixed(1),
            TensorUpdateSubscript::Whole,
        ],
        4,
        1,
    );
    assert_rows(
        &pattern,
        &[
            &[0],
            &[1],
            &[12],
            &[13],
            &[4],
            &[5],
            &[6],
            &[7],
            &[14],
            &[15],
            &[10],
            &[11],
        ],
    );
}

#[test]
fn dynamic_tensor_updates_retain_base_patch_and_selector_dependencies() {
    let runtime = update_pattern(
        &[3],
        vec![TensorUpdateSubscript::Index(TensorIndex::Runtime(4))],
        1,
        1,
    );
    assert_rows(&runtime, &[&[0, 3, 4], &[1, 3, 4], &[2, 3, 4]]);
    let slice = update_pattern(
        &[3],
        vec![TensorUpdateSubscript::Slice {
            start: 4,
            dimensions: Box::new([1]),
        }],
        1,
        1,
    );
    assert_rows(&slice, &[&[0, 3, 4], &[1, 3, 4], &[2, 3, 4]]);
}
