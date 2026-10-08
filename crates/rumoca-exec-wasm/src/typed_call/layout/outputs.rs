//! Completed region returns may share storage; root publication stays private.
use super::{FramePlan, TypedCallCompileError, advance};
use rumoca_ir_solve as solve;

#[derive(Clone, Copy)]
pub(super) enum OutputPolicy {
    Returned,
    Carried(usize),
}

impl FramePlan {
    /// Place the output slot an operation accesses first (SOLVE-C78).
    pub(super) fn place_accessed_output(
        &mut self,
        program: &solve::TypedProgram,
        operation: &solve::SolveOperation,
    ) -> Result<(), TypedCallCompileError> {
        match operation {
            solve::SolveOperation::Store { slot, source } => {
                self.place_output(program, slot.index(), Some(*source))
            }
            solve::SolveOperation::Load { slot, .. } => {
                self.place_output(program, slot.index(), None)
            }
            _ => Ok(()),
        }
    }

    /// Give a region's output slot its storage, once, where the body first
    /// accesses it.
    ///
    /// A slot that is the completed return of one register, with its source in
    /// full and nothing reading it first, is that register's range: the store
    /// is an exact same-range no-op and nothing is allocated. Any other
    /// output is a fresh range above the registers placed so far, so the
    /// storage of every later operation stays disjoint from it. Placing the
    /// slot before it is known to be borrowed would leave its span allocated
    /// but unused in every region level that returns an aggregate.
    pub(super) fn place_output(
        &mut self,
        program: &solve::TypedProgram,
        slot: usize,
        store: Option<solve::SolveRegisterId>,
    ) -> Result<(), TypedCallCompileError> {
        if self.placed[slot] {
            return Ok(());
        }
        self.placed[slot] = true;
        let id = program.slots()[slot].id();
        let bytes = self.slots[slot].bytes;
        if let Some(source) = store.filter(|source| completed_return(program, id) == Some(*source))
        {
            let range = self.registers[source.index()];
            if range.bytes == bytes && self.returns_in_place(program, id, range) {
                self.slots[slot] = range;
                self.slot_reusable[slot] = self.register_reusable[source.index()];
                return Ok(());
            }
        }
        advance(&mut self.slot_bytes, bytes)?;
        self.slots[slot] = self.keep(bytes)?;
        Ok(())
    }

    /// Whether returning `range` for output `slot` is permitted by the policy.
    ///
    /// Cross-ordinal returns of a carried tuple retain independent snapshots:
    /// committing a tuple swap must not overwrite the next old carried source.
    fn returns_in_place(
        &self,
        program: &solve::TypedProgram,
        slot: solve::SolveSlotId,
        range: super::CellRange,
    ) -> bool {
        let OutputPolicy::Carried(count) = self.output_policy else {
            return true;
        };
        let ordinal = interface(program, solve::SolveStorageClass::Output)
            .iter()
            .position(|output| *output == slot);
        let inputs = interface(program, solve::SolveStorageClass::Input);
        ordinal.is_some_and(|ordinal| {
            ordinal < count && inputs.get(ordinal).is_some_and(|input| range == self.slots[input.index()])
        })
    }

    /// Place the result registers of a conditional whose arms are planned at
    /// `base`, the end of the parent's live storage (SOLVE-C78).
    ///
    /// A result both arms return as one parent-owned range, or that one arm
    /// passes through, is that range. Every other result is a fresh register
    /// at `base`, and the arms, which only know their private storage starts
    /// at `base`, move up past those registers. Allocating the results before
    /// the arms are planned would leave a dead span for every borrowed result.
    pub(super) fn place_conditional_results(
        &mut self,
        program: &solve::TypedProgram,
        destinations: &[solve::SolveRegisterId],
        arms: &mut [Self],
        base: u32,
    ) -> Result<(), TypedCallCompileError> {
        // Region slots retain their issued order, including output ordinal.
        let returned = [arms[0].returned_outputs(), arms[1].returned_outputs()];
        let mut results = Vec::with_capacity(destinations.len());
        let mut fresh = 0;
        for (ordinal, destination) in destinations.iter().enumerate() {
            let count = super::bytes(&program.register_types()[destination.index()])?;
            let result = conditional_result(ordinal, [&returned[0], &returned[1]], base, count);
            if result.is_none() {
                advance(&mut fresh, count)?;
            }
            results.push((count, result));
        }
        for arm in arms {
            arm.relocate(base, fresh)?;
        }
        for (destination, (count, result)) in destinations.iter().zip(results) {
            let (range, reusable) = match result {
                // Borrowing immutable storage never creates mutation permission.
                Some(borrowed) => borrowed,
                None => (self.fresh_register(count)?, true),
            };
            self.push_register(*destination, range)?;
            self.register_reusable.push(reusable);
        }
        Ok(())
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
