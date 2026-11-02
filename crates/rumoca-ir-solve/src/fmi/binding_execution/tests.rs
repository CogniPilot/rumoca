use super::*;
use crate::LinearOp;

fn p(index: usize) -> ScalarSlot {
    ScalarSlot::P {
        index,
        byte_offset: index * 8,
    }
}

fn y(index: usize) -> ScalarSlot {
    ScalarSlot::Y {
        index,
        byte_offset: index * 8,
    }
}

fn block(counts: &[usize], outputs: Vec<usize>, compact: bool) -> ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi-binding-execution.mo"),
        0,
        1,
    );
    let programs = counts
        .iter()
        .map(|&count| {
            let mut ops = (0..count)
                .map(|dst| LinearOp::Const {
                    dst: dst.try_into().unwrap(),
                    value: dst as f64 + 1.0,
                })
                .collect::<Vec<_>>();
            if compact {
                ops.push(LinearOp::StoreOutputRange {
                    start: 0,
                    count,
                    stride: 1,
                });
            } else {
                ops.extend((0..count).map(|src| LinearOp::StoreOutput {
                    src: src.try_into().unwrap(),
                }));
            }
            ops
        })
        .collect();
    ScalarProgramBlock::with_output_indices(programs, vec![span; counts.len()], outputs).unwrap()
}

#[test]
fn mixed_scalar_program_keeps_whole_owner_and_separates_commits() {
    let source = block(&[2], vec![0, 1], false);
    let execution = BindingExecution::checked(&source, &[p(1), y(0)], &[0]).unwrap();
    assert!(execution.source().shares_program_owner(&source));
    assert_eq!(execution.source().programs(), source.programs());
    let program = &execution.programs()[0];
    assert_eq!(program.program_index(), 0);
    assert_eq!(program.parameter_rows(), &[0]);
    assert_eq!(program.state_start_rows, [1]);
    assert!(!program.initialization_only);
}

#[test]
fn mixed_compact_program_uses_issued_output_placement() {
    let source = block(&[2], vec![1, 0], true);
    let execution = BindingExecution::checked(&source, &[p(1), y(0)], &[0]).unwrap();
    assert!(execution.source().shares_program_owner(&source));
    assert_eq!(execution.programs()[0].parameter_rows(), &[0]);
    assert_eq!(execution.programs()[0].state_start_rows, [1]);
    assert!(!execution.programs()[0].initialization_only);
}

#[test]
fn compact_state_only_program_has_no_post_initialization_commits() {
    let source = block(&[2], vec![1, 0], true);
    let execution = BindingExecution::checked(&source, &[y(0), y(1)], &[0]).unwrap();
    assert!(execution.programs()[0].initialization_only);
    assert!(execution.programs()[0].parameter_rows().is_empty());
    assert_eq!(execution.programs()[0].state_start_rows, [1, 0]);
}

#[test]
fn execution_retains_admitted_program_order_without_rewriting_operations() {
    let source = block(&[1, 2], vec![2, 0, 1], false);
    let execution = BindingExecution::checked(&source, &[p(1), y(0), p(2)], &[1, 0]).unwrap();
    assert!(execution.source().shares_program_owner(&source));
    assert_eq!(execution.programs()[0].program_index(), 1);
    assert_eq!(execution.programs()[0].parameter_rows(), &[0]);
    assert_eq!(execution.programs()[1].program_index(), 0);
    assert_eq!(execution.programs()[1].parameter_rows(), &[2]);
}

#[test]
fn missing_duplicate_or_foreign_program_is_refused() {
    let source = block(&[1, 1], vec![0, 1], false);
    for order in [vec![0], vec![0, 0], vec![0, 2], vec![0, 1, 1]] {
        assert!(BindingExecution::checked(&source, &[p(1), y(0)], &order).is_err());
    }
}

#[test]
fn output_target_count_and_non_storage_targets_are_refused() {
    let source = block(&[2], vec![0, 1], false);
    assert!(BindingExecution::checked(&source, &[p(1)], &[0]).is_err());
    for bad in [ScalarSlot::Time, ScalarSlot::Constant(2.0)] {
        assert!(BindingExecution::checked(&source, &[p(1), bad], &[0]).is_err());
    }
}

#[test]
fn out_of_inventory_output_row_is_refused() {
    let source = block(&[2], vec![0, 2], false);
    assert!(BindingExecution::checked(&source, &[p(1), y(0)], &[0]).is_err());
}

#[test]
fn empty_initialization_retains_its_exact_empty_owner() {
    let source = ScalarProgramBlock::default();
    let execution = BindingExecution::checked(&source, &[], &[]).unwrap();
    assert!(execution.source().shares_program_owner(&source));
    assert!(execution.programs().is_empty());
}

#[test]
fn program_without_an_owned_result_is_refused() {
    let source = block(&[0], vec![], false);
    assert!(BindingExecution::checked(&source, &[], &[0]).is_err());
}
