use super::*;

/// A ranged store's Real sibling still reads its Real view, even when another
/// output binds a view of the same Integer input to an exact lane.
#[test]
fn range_binding_keeps_unbound_real_views_checked() {
    let lanes = [NativeInputLane {
        p_index: 0,
        lane: NativeOutputLane::Integer,
        lane_offset: 0,
        checked: false,
    }];
    for (integer_outputs, expected) in [(1, true), (2, false)] {
        let mut checked = vec![false];
        let mut flow = StageFlow {
            views: &BTreeMap::from([(0, 0)]),
            lanes: &lanes,
            sinks: (0..integer_outputs)
                .map(|ordinal| (ordinal, NativeIntegerSource::Input { lane_offset: 0 }))
                .collect(),
            stored_output: 0,
            checked: &mut checked,
            loads: vec![0],
            bindings: Default::default(),
        };
        flow.program(&[
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ])
        .unwrap();
        assert_eq!(checked[0], expected);
    }
}
