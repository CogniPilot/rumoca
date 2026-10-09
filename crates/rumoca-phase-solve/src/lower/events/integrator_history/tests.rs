use super::*;

fn tensor(input: solve::TensorInputKind, start: usize, count: usize) -> solve::LinearOp {
    solve::LinearOp::TensorLoad {
        dst_start: 0,
        input,
        input_start: start,
        count,
        seed_start: None,
        lanes: 1,
    }
}

#[test]
fn supported_scalar_and_overlapping_tensor_reads_keep_the_exact_union() {
    let mut dependencies = BTreeSet::from([HistoryDependencySlot::P(99)]);
    let ops = [
        solve::LinearOp::LoadY { dst: 0, index: 3 },
        tensor(solve::TensorInputKind::Y, 2, 3),
        tensor(solve::TensorInputKind::P, 7, 2),
        solve::LinearOp::LoadIndexedP {
            dst: 0,
            base: 8,
            count: 2,
            index: 0,
        },
    ];
    assert_eq!(
        collect_linear_op_dependencies(&ops, &mut dependencies),
        Some(())
    );
    assert_eq!(
        dependencies,
        BTreeSet::from([
            HistoryDependencySlot::Y(2),
            HistoryDependencySlot::Y(3),
            HistoryDependencySlot::Y(4),
            HistoryDependencySlot::P(7),
            HistoryDependencySlot::P(8),
            HistoryDependencySlot::P(9),
            HistoryDependencySlot::P(99),
        ])
    );
}

#[test]
fn unsupported_operation_does_not_publish_a_large_tensor_prefix() {
    let original = BTreeSet::from([HistoryDependencySlot::P(99)]);
    let mut dependencies = original.clone();
    let ops = [
        tensor(solve::TensorInputKind::Y, 0, 16384),
        solve::LinearOp::LoadSeed { dst: 0, index: 0 },
    ];
    assert_eq!(
        collect_linear_op_dependencies(&ops, &mut dependencies),
        None
    );
    assert_eq!(dependencies, original);
}

#[test]
fn nested_refusal_does_not_publish_supported_earlier_regions() {
    let original = BTreeSet::from([HistoryDependencySlot::P(99)]);
    let mut dependencies = original.clone();
    let program = solve::FunctionConditionalProgram {
        owner: None,
        capture_count: 0,
        target_widths: vec![1].into_boxed_slice(),
        result_count: 1,
        arms: vec![solve::FunctionConditionalArmProgram {
            condition_register_count: 1,
            result_register_count: 64,
            condition: vec![solve::LinearOp::LoadP { dst: 0, index: 2 }],
            result: vec![tensor(solve::TensorInputKind::Y, 4, 64)],
        }]
        .into_boxed_slice(),
        fallback_register_count: 1,
        fallback: vec![solve::LinearOp::LoadSeed { dst: 0, index: 0 }],
    };
    assert_eq!(
        collect_linear_op_dependencies(
            &[solve::LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: std::sync::Arc::new(program),
            }],
            &mut dependencies
        ),
        None
    );
    assert_eq!(dependencies, original);
}

#[test]
fn overflowing_range_refuses_without_publishing_prior_reads() {
    let original = BTreeSet::from([HistoryDependencySlot::P(99)]);
    let mut dependencies = original.clone();
    let ops = [
        solve::LinearOp::LoadY { dst: 0, index: 3 },
        tensor(solve::TensorInputKind::P, usize::MAX, 2),
    ];
    assert_eq!(
        collect_linear_op_dependencies(&ops, &mut dependencies),
        None
    );
    assert_eq!(dependencies, original);
    assert_eq!(
        collect_linear_op_dependencies(
            &[tensor(solve::TensorInputKind::P, usize::MAX, 0)],
            &mut dependencies
        ),
        Some(())
    );
    assert_eq!(dependencies, original);
}
