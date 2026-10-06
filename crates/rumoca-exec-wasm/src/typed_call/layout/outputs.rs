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
    ) {
        // Region slots retain their issued order, including output ordinal.
        let first = branches[0].returned_outputs();
        let second = branches[1].returned_outputs();
        for ((destination, (left, left_reusable)), (right, right_reusable)) in
            destinations.iter().zip(first).zip(second)
        {
            if left != right || left.bytes != self.registers[destination.index()].bytes {
                continue;
            }
            self.registers[destination.index()] = left;
            // Borrowing immutable storage never creates mutation permission.
            self.register_reusable[destination.index()] = left_reusable && right_reusable;
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
