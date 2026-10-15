use super::*;
use rumoca_ir_solve::{BinaryOp, TensorInputKind};

fn source(count: usize, offset: f64, label: &str) -> ScalarProgramBlock {
    let count_reg = u32::try_from(count).unwrap();
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(label), 3, 19);
    let load = |dst_start, input| LinearOp::TensorLoad {
        dst_start,
        input,
        input_start: 0,
        count,
        seed_start: None,
        lanes: 1,
    };
    ScalarProgramBlock::with_program_spans(
        vec![vec![
            load(0, TensorInputKind::Y),
            load(count_reg, TensorInputKind::P),
            LinearOp::TensorBinary {
                dst_start: 2 * count_reg,
                op: BinaryOp::Mul,
                lhs_start: 0,
                rhs_start: count_reg,
                count,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            },
            LinearOp::Const {
                dst: 3 * count_reg,
                value: offset,
            },
            LinearOp::TensorBinary {
                dst_start: 3 * count_reg + 1,
                op: BinaryOp::Sub,
                lhs_start: 2 * count_reg,
                rhs_start: 3 * count_reg,
                count,
                lhs_stride: 1,
                rhs_stride: 0,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 3 * count_reg + 1,
                count,
                stride: 1,
            },
        ]],
        vec![span],
    )
    .unwrap()
}

fn evaluate(plan: &PreparedTargetValuePlan, y: &[f64], p: &[f64]) -> Vec<f64> {
    let code = super::super::PreparedEvaluationBlock::new(plan.program().clone()).unwrap();
    let mut values = vec![19.0; plan.program().output_count()];
    code.eval_with_context(y, p, 0.0, RowEvalContext::default(), &mut values)
        .unwrap();
    values
}

#[test]
fn target_family_shares_one_program_across_many_exact_selections() {
    fn assert_send<T: Send>() {}
    assert_send::<PreparedScalarProgramBlock>();
    assert_send::<PreparedTargetValuePlan>();
    for count in [4, 8, 16] {
        let prepared = PreparedScalarProgramBlock::new(source(count, 4.0, "family.mo")).unwrap();
        let first = prepared
            .portable_target_value_plan(0, 0, 0)
            .unwrap()
            .unwrap();
        let mut offsets = std::collections::BTreeSet::new();
        for selected in 0..count {
            let plan = prepared
                .portable_target_value_plan(0, selected, selected)
                .unwrap()
                .unwrap();
            assert!(first.program().shares_program_owner(plan.program()));
            assert!(offsets.insert(plan.private_result_offset()));
            assert_eq!((plan.output(), plan.target()), (selected, selected));
        }
        assert!(
            prepared
                .portable_target_value_plan(0, count, count - 1)
                .unwrap()
                .is_none()
        );
        let last = prepared
            .portable_target_value_plan(0, count - 1, count - 1)
            .unwrap()
            .unwrap();
        assert!(first.program().shares_program_owner(last.program()));
        assert_eq!(
            &first.program().programs()[0][..first.canonical_prefix_len()],
            &prepared.block().programs()[0][..first.canonical_prefix_len()]
        );
        drop(prepared);
        let p = (0..count)
            .map(|index| index as f64 + 2.0)
            .collect::<Vec<_>>();
        let y = vec![1e20; count];
        let values = evaluate(&last, &y, &p);
        assert_eq!(values[first.private_result_offset()], 2.0);
        assert_eq!(values[last.private_result_offset()], 4.0 / p[count - 1]);
        for plan in [&first, &last] {
            let &(_, target, coefficient) = plan
                .coefficient_outputs()
                .iter()
                .find(|&&(value, _, _)| value == plan.private_result_offset())
                .unwrap();
            assert_eq!(target, plan.target());
            assert_eq!(values[coefficient], p[target]);
        }
        assert_eq!(y, vec![1e20; count]);
    }
}

#[test]
fn target_family_replacement_keeps_its_source_and_provenance() {
    let original = PreparedScalarProgramBlock::new(source(2, 4.0, "original.mo")).unwrap();
    let first = original
        .portable_target_value_plan(0, 0, 0)
        .unwrap()
        .unwrap();
    let changed = PreparedScalarProgramBlock::with_replaced_programs(
        &original,
        source(2, 8.0, "replacement.mo"),
        &[0],
    )
    .unwrap();
    let replacement = changed
        .portable_target_value_plan(0, 0, 0)
        .unwrap()
        .unwrap();
    assert!(!first.program().shares_program_owner(replacement.program()));
    assert_ne!(
        first.program().program_spans(),
        replacement.program().program_spans()
    );
    assert_eq!(
        evaluate(&first, &[20.0, 30.0], &[2.0, 4.0])[first.private_result_offset()],
        2.0
    );
    assert_eq!(
        evaluate(&replacement, &[20.0, 30.0], &[2.0, 4.0])[replacement.private_result_offset()],
        4.0
    );
    let cloned = original.clone();
    let clone_plan = cloned.portable_target_value_plan(0, 0, 0).unwrap().unwrap();
    assert_eq!(
        evaluate(&clone_plan, &[20.0, 30.0], &[2.0, 4.0])[clone_plan.private_result_offset()],
        2.0
    );
    // Parameter changes remain runtime inputs, never entries in the construction memo.
    assert_eq!(
        evaluate(&first, &[20.0, 30.0], &[4.0, 8.0])[first.private_result_offset()],
        1.0
    );
}
