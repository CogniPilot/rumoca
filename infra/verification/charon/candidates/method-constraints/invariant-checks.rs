use closure_region_facts::{ClosureRegionFacts, RegionPart, RegionSlot};

pub fn check(root: &str, facts: &ClosureRegionFacts) {
    let captures = slots(facts, RegionPart::Captures);
    let outputs = slots(facts, RegionPart::Signature);
    let parents = slots(facts, RegionPart::ParentArguments);
    assert_eq!(parents.len(), 2, "two independent declaration parameters");
    assert!(!has_edge(facts, parents[0], parents[1]));
    assert!(!has_edge(facts, parents[1], parents[0]));

    if root.ends_with("::both_ids") {
        // Fresh PhantomData does not make either output brand depend on the
        // stored slice capture. Do not invent a required relation here.
        assert_eq!(outputs.len(), 2);
        assert_eq!(captures.len(), 1);
        for &source in facts.slots() {
            for &target in facts.slots() {
                assert_eq!(has_edge(facts, source, target), source == target);
            }
        }
        return;
    }

    assert_eq!(captures.len(), 2, "the row call captures the whole table");
    assert_eq!(outputs.len(), 3);
    let source_for_output = if root.ends_with("::swapped") {
        [0, 1, 0]
    } else {
        assert!(root.ends_with("::ordered"));
        [0, 0, 1]
    };
    let invariant_data_input = !root.starts_with("CovariantTable::");
    // Independent partition of the entire source-level signature, not just
    // the capture/output submatrix. Class 3 is captured data; class 4 is its
    // shortened output in the covariant-input control.
    let class = |slot: RegionSlot| match slot.part() {
        RegionPart::ParentArguments => slot.occurrence(),
        RegionPart::Captures => 2 + slot.occurrence(),
        RegionPart::Signature => match source_for_output[slot.occurrence()] {
            0 => 2,
            _ if invariant_data_input => 3,
            _ => 4,
        },
    };
    for &source in facts.slots() {
        for &target in facts.slots() {
            let wanted = class(source) == class(target)
                || (!invariant_data_input && class(source) == 3 && class(target) == 4);
            assert_eq!(
                has_edge(facts, source, target),
                wanted,
                "{root}: {source:?} -> {target:?}"
            );
        }
    }
    println!("  CHECKED_INVARIANT_VS_COVARIANT_REQUIREMENTS {root}");
}

fn slots(facts: &ClosureRegionFacts, part: RegionPart) -> Vec<RegionSlot> {
    facts
        .slots()
        .iter()
        .copied()
        .filter(|slot| slot.part() == part)
        .collect()
}

fn has_edge(facts: &ClosureRegionFacts, source: RegionSlot, target: RegionSlot) -> bool {
    facts
        .required_outlives()
        .iter()
        .any(|edge| edge.longer() == source && edge.shorter() == target)
}
