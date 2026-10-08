//! Completed region returns may share storage; root publication stays private.
use super::FramePlan;
use rumoca_ir_solve as solve;

pub(super) enum OutputPolicy {
    Returned,
    Carried(usize),
}

impl FramePlan {
    pub(super) fn borrow_completed_outputs(
        &mut self,
        program: &solve::TypedProgram,
        policy: OutputPolicy,
    ) {
        let inputs = interface(program, solve::SolveStorageClass::Input);
        for (ordinal, slot) in interface(program, solve::SolveStorageClass::Output)
            .into_iter()
            .enumerate()
        {
            let Some(source) = completed_return(program, slot) else {
                continue;
            };
            let range = self.registers[source.index()];
            if range.bytes != self.slots[slot.index()].bytes {
                continue;
            }
            // Cross-ordinal returns retain independent snapshots: committing
            // a tuple swap must not overwrite the next old carried source.
            if let OutputPolicy::Carried(count) = policy
                && (ordinal >= count || range != self.slots[inputs[ordinal].index()])
            {
                continue;
            }
            self.slots[slot.index()] = range;
            self.slot_reusable[slot.index()] = self.register_reusable[source.index()];
        }
    }

    pub(super) fn borrow_conditional_results(
        &mut self,
        destinations: &[solve::SolveRegisterId],
        branches: &[Self],
        base: u32,
    ) {
        // Region slots retain their issued order, including output ordinal.
        let first = branches[0].returned_outputs();
        let second = branches[1].returned_outputs();
        for (ordinal, destination) in destinations.iter().enumerate() {
            let bytes = self.registers[destination.index()].bytes;
            let arms = [&first, &second];
            let Some((range, reusable)) = conditional_result(ordinal, arms, base, bytes) else {
                continue;
            };
            self.registers[destination.index()] = range;
            // Borrowing immutable storage never creates mutation permission.
            self.register_reusable[destination.index()] = reusable;
        }
    }

    fn returned_outputs(&self) -> Vec<(super::CellRange, bool)> {
        self.output_slots
            .iter()
            .map(|slot| (self.slots[*slot], self.slot_reusable[*slot]))
            .collect()
    }
}

fn completed_return(
    program: &solve::TypedProgram,
    output: solve::SolveSlotId,
) -> Option<solve::SolveRegisterId> {
    let mut result = None;
    let mut tail = false;
    for operation in program.operations() {
        match operation.operation() {
            solve::SolveOperation::Store { slot, source } if *slot == output => {
                // Multiple stores or an earlier load require a real snapshot.
                if result.is_some() {
                    return None;
                }
                result = Some(*source);
                tail = true;
            }
            solve::SolveOperation::Load { slot, .. } if *slot == output => return None,
            solve::SolveOperation::Store { slot, .. }
                if program.slots()[slot.index()].storage() == solve::SolveStorageClass::Output => {}
            _ if tail => return None,
            _ => {}
        }
    }
    // After this store only output stores remain. Each borrowed output store
    // becomes an exact same-range no-op; retained output slots are disjoint
    // fresh allocations. Thus no later operation, including a nested region,
    // can mutate the returned source or fault after its publication.
    result
}

/// The parent range output `ordinal` of a conditional takes, and whether it may
/// be rewritten, from what its two arms return.
///
/// Arm-private ranges are placed at the same base and die with the operation,
/// so only a range the parent already owns (below `base`) may become a register
/// of the parent. Both arms returning the same parent range is an immutable
/// borrow. One arm passing the parent range through while the other builds its
/// value elsewhere puts the result in the parent range, so only the building
/// arm copies; that rewrites the range, so the passing arm's slot must carry the
/// capture's last-read proof (SOLVE-C71) and no other output of either arm may
/// return the range, since it would observe the rewrite.
fn conditional_result(
    ordinal: usize,
    arms: [&Vec<(super::CellRange, bool)>; 2],
    base: u32,
    bytes: u32,
) -> Option<(super::CellRange, bool)> {
    let owned =
        |(range, _): (super::CellRange, bool)| range.bytes == bytes && parent_owned(range, base);
    let (left, right) = (arms[0][ordinal], arms[1][ordinal]);
    if left.0 == right.0 {
        return owned(left).then_some((left.0, left.1 && right.1));
    }
    let passed = passed_through(left, right, owned)?;
    let returned_elsewhere = arms.iter().any(|outputs| {
        outputs
            .iter()
            .enumerate()
            .any(|(other, (range, _))| other != ordinal && *range == passed.0)
    });
    (passed.1 && !returned_elsewhere).then_some(passed)
}

/// The parent-owned range exactly one arm returns, with the permission to
/// rewrite it.
fn passed_through(
    left: (super::CellRange, bool),
    right: (super::CellRange, bool),
    owned: impl Fn((super::CellRange, bool)) -> bool,
) -> Option<(super::CellRange, bool)> {
    match (owned(left), owned(right)) {
        (true, false) => Some(left),
        (false, true) => Some(right),
        _ => None,
    }
}

fn interface(
    program: &solve::TypedProgram,
    storage: solve::SolveStorageClass,
) -> Vec<solve::SolveSlotId> {
    program
        .slots()
        .iter()
        .filter(|slot| slot.storage() == storage)
        .map(|slot| slot.id())
        .collect()
}

/// A range a parent body keeps alive: an input span, or scratch below the
/// base where the operation's private storage starts.
fn parent_owned(range: super::CellRange, base: u32) -> bool {
    range.base != 2
        || range
            .offset
            .checked_add(range.bytes)
            .is_some_and(|end| end <= base)
}
