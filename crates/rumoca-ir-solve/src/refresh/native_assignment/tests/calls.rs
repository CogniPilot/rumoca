//! Complete-domain call dependencies retain even unused fault-relevant inputs.
use super::*;
use crate::{
    SolveArithmeticProfile, SolveCompareOperator, SolveIntegerDomain, SolvePureCallIdentity,
    SolvePureCallOutput, SolvePureCallSite, SolvePureCallTable, SolveRealFormat, SolveScalarType,
    SolveValue, SolveValueType,
};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("CompactCalls.mo"), 4, 13)
}

fn site() -> SolvePureCallSite {
    let p = SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let real = SolveValueType::scalar(SolveScalarType::real(p));
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let mut table = SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![real.clone(); 2],
            vec![
                SolvePureCallOutput::result(real),
                SolvePureCallOutput::result(boolean),
            ],
            span(),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span())?;
                let zero = b.constant(SolveValue::real(p, 0.), span())?;
                let equal = b.compare(SolveCompareOperator::Equal, value, zero, span())?;
                b.store(outputs[0], value, span())?;
                b.store(outputs[1], equal, span())
            },
        )
        .unwrap();
    table.call_site(owner).unwrap()
}

fn call_family(mut node: ComputeNode, site: &SolvePureCallSite) -> ComputeNode {
    let ComputeNode::Map { base_ops, .. } = &mut node else {
        panic!("map")
    };
    base_ops.truncate(2);
    base_ops.extend([
        LinearOp::Const { dst: 2, value: 0. },
        LinearOp::PureCall {
            dst_start: 3,
            input_starts: vec![1, 2].into_boxed_slice(),
            site: site.clone(),
        },
        LinearOp::Select {
            dst: 5,
            cond: 4,
            if_true: 2,
            if_false: 3,
        },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 5,
        },
        LinearOp::StoreOutput { src: 6 },
    ]);
    node
}

fn calls(count: usize) -> (ComputeBlock, Vec<Option<ScalarSlot>>, VarLayout) {
    let (mut source, targets, layout) = fixture(count);
    let site = site();
    source.nodes = source
        .nodes
        .into_iter()
        .map(|node| call_family(node, &site))
        .collect();
    (source, targets, layout)
}

fn operations(source: &mut ComputeBlock) -> &mut Vec<LinearOp> {
    let ComputeNode::Map { base_ops, .. } = &mut source.nodes[1] else {
        panic!("map")
    };
    base_ops
}

#[test]
fn compact_calls_retain_original_domain_order_and_complete_prefix() {
    for count in [3, 14_400] {
        let (source, targets, layout) = calls(count);
        let schedule = derive(&source, &targets, &layout).unwrap();
        assert_eq!(
            schedule
                .stages()
                .iter()
                .map(|s| continuous_node(s))
                .collect::<Vec<_>>(),
            [1, 0]
        );
        for stage in schedule.stages() {
            let ComputeNode::Map {
                base_ops: original,
                domain,
                ..
            } = &source.nodes[continuous_node(stage)]
            else {
                panic!("source")
            };
            let ComputeNode::Map {
                base_ops: value,
                domain: retained,
                ..
            } = &stage.value_kernel().nodes[0]
            else {
                panic!("value")
            };
            assert_eq!(domain, retained);
            assert!(operations_match(&original[..6], &value[..6]));
            assert_eq!(value.last(), Some(&LinearOp::StoreOutput { src: 5 }));
            assert_eq!(stage.target_count(), count);
        }
    }
}

#[test]
fn compact_call_refuses_own_target_even_when_formal_is_unused() {
    let (mut source, targets, layout) = calls(3);
    let LinearOp::PureCall { input_starts, .. } = &mut operations(&mut source)[3] else {
        panic!("call")
    };
    input_starts[1] = 0;
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native call inputs depend on its own assignment target"
    );
}

#[test]
fn compact_call_refuses_later_domain_coupling_and_bounds() {
    let (mut source, targets, layout) = calls(3);
    operations(&mut source)[1] = LinearOp::LoadY { dst: 1, index: 2 };
    assert!(
        derive(&source, &targets, &layout)
            .unwrap_err()
            .0
            .contains("coupled targets")
    );
    let (mut source, targets, layout) = calls(3);
    operations(&mut source)[1] = LinearOp::LoadP { dst: 1, index: 1 };
    assert!(
        derive(&source, &targets, &layout)
            .unwrap_err()
            .0
            .contains("layout")
    );
}

#[test]
fn compact_calls_refuse_undefined_overlapping_and_incomplete_argument_ranges() {
    for inputs in [vec![1, 99], vec![1]] {
        let (mut source, targets, layout) = calls(3);
        let LinearOp::PureCall { input_starts, .. } = &mut operations(&mut source)[3] else {
            panic!("call")
        };
        *input_starts = inputs.into_boxed_slice();
        let reason = derive(&source, &targets, &layout).unwrap_err().0;
        assert!(
            matches!(
                reason,
                "native family has no exact target isolator"
                    | "native family call prefix has invalid register flow"
            ),
            "{reason}"
        );
        // Public certification may refuse the malformed value projection first.
        // Exercise the call boundary itself to prove its register-flow guard.
        let ops = operations(&mut source);
        let reason = range::checked_family_call_inputs(
            &ops[..6],
            &[Some(true), Some(false), None, None, None, None],
        )
        .unwrap_err()
        .0;
        assert_eq!(
            reason,
            "native family call prefix has invalid register flow"
        );
    }
    let (mut source, targets, layout) = calls(3);
    operations(&mut source)[2] = LinearOp::Const { dst: 1, value: 0. };
    assert!(derive(&source, &targets, &layout).is_err());
}

#[test]
fn compact_call_register_versions_refuse_overlap_after_valid_flow() {
    let (mut source, _, _) = calls(3);
    let ops = operations(&mut source);
    ops.insert(3, LinearOp::Const { dst: 1, value: 0. });
    crate::ScalarProgramRegisterFlow::derive(ops).unwrap();
    let reason = range::checked_family_call_inputs(
        &ops[..7],
        &[Some(true), Some(false), None, None, None, None, None],
    )
    .unwrap_err()
    .0;
    assert_eq!(
        reason,
        "native call prefix has overlapping destination versions"
    );
}

#[test]
fn compact_calls_refuse_malformed_and_overflowing_domains() {
    for (upper, step) in [(3, 0), (i64::MAX, 1)] {
        let (mut source, targets, layout) = calls(3);
        let ComputeNode::Map { domain, .. } = &mut source.nodes[1] else {
            panic!("map")
        };
        domain.binders[0].upper = upper;
        domain.binders[0].step = step;
        assert!(derive(&source, &targets, &layout).is_err());
    }
    let (mut source, targets, layout) = calls(3);
    let ComputeNode::Map { domain, .. } = &mut source.nodes[1] else {
        panic!("map")
    };
    domain.binders[0].upper = i64::MAX;
    domain.binders.push(StructuredIndexBinder {
        id: 1,
        display_name: "j".into(),
        lower: 1,
        upper: i64::MAX,
        step: 1,
    });
    assert!(
        domain.scalar_count().is_err(),
        "actual domain product overflows"
    );
    assert!(derive(&source, &targets, &layout).is_err());
}

#[test]
fn compact_call_input_dependence_follows_moves_without_erasing_original_prefix() {
    for owned in [false, true] {
        let (mut source, targets, layout) = calls(3);
        let ops = operations(&mut source);
        ops[2] = LinearOp::Move {
            dst: 2,
            src: if owned { 0 } else { 1 },
        };
        ops[4] = LinearOp::Select {
            dst: 5,
            cond: 4,
            if_true: 3,
            if_false: 3,
        };
        let result = derive(&source, &targets, &layout);
        if owned {
            assert_eq!(
                result.unwrap_err().0,
                "native call inputs depend on its own assignment target"
            );
        } else {
            assert!(result.is_ok());
        }
    }
}

fn unused_tensor_site() -> SolvePureCallSite {
    let p = SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let real = SolveValueType::scalar(SolveScalarType::real(p));
    let tensor = SolveValueType::tensor(SolveScalarType::real(p), vec![2]).unwrap();
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let mut table = SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(2).unwrap()),
            vec![tensor, real.clone()],
            vec![
                SolvePureCallOutput::result(real),
                SolvePureCallOutput::result(boolean),
            ],
            span(),
            |b, _, outputs| {
                let zero = b.constant(SolveValue::real(p, 0.), span())?;
                let equal = b.compare(SolveCompareOperator::Equal, zero, zero, span())?;
                b.store(outputs[0], zero, span())?;
                b.store(outputs[1], equal, span())
            },
        )
        .unwrap();
    table.call_site(owner).unwrap()
}

#[test]
fn compact_call_checks_entire_unused_tensor_argument_range() {
    let (mut source, targets, layout) = calls(3);
    let LinearOp::PureCall {
        input_starts, site, ..
    } = &mut operations(&mut source)[3]
    else {
        panic!("call")
    };
    *site = unused_tensor_site();
    *input_starts = vec![1, 2].into_boxed_slice();
    derive(&source, &targets, &layout).unwrap();
    let LinearOp::PureCall { input_starts, .. } = &mut operations(&mut source)[3] else {
        panic!("call")
    };
    input_starts[0] = 0;
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native call inputs depend on its own assignment target"
    );
}
