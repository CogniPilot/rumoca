//! Output demand: which results of a body its consumer reads (SOLVE-C95,
//! SOLVE-C102).
//!
//! A consumer, a call site or the enclosing conditional of a region, reads some
//! of the results it receives. The body built for that consumer builds only
//! those: a store to an unread output and every computation that feeds nothing
//! else are not planned or emitted. Operations that can fail or have an effect
//! always run, so demand never changes which faults a body raises.
//!
//! Demand is decided once, from the consumer's own typed body, when the
//! program is built. Nothing is tested at run time.
use crate as solve;
use std::collections::BTreeMap;

/// Which outputs of a body, in output order, a consumer reads. An output past
/// the end of the list is read, so the empty list is the demand of a consumer
/// that reads everything.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct SolveOutputDemand(Box<[bool]>);

impl SolveOutputDemand {
    /// Every output is read.
    pub fn all() -> Self {
        Self::default()
    }

    /// The demand whose output `k` is read when `reads[k]` is.
    pub fn of(reads: impl IntoIterator<Item = bool>) -> Self {
        let mut reads = reads.into_iter().collect::<Vec<_>>();
        while reads.last() == Some(&true) {
            reads.pop();
        }
        Self(reads.into_boxed_slice())
    }

    /// The demand of a consumer that reads exactly the `needed` registers among
    /// the `destinations` it receives the outputs in.
    pub fn of_destinations(destinations: &[solve::SolveRegisterId], needed: &[bool]) -> Self {
        Self::of(
            destinations
                .iter()
                .map(|destination| needed[destination.index()]),
        )
    }

    pub fn wants(&self, output: usize) -> bool {
        self.0.get(output).copied().unwrap_or(true)
    }

    pub fn is_all(&self) -> bool {
        self.0.is_empty()
    }
}

/// The operations of one body that run when only a demand is built, and the
/// registers they read.
#[derive(Debug, Clone, Default)]
pub struct SolveLiveness {
    /// Operations that are neither planned nor emitted; empty when none is.
    dead: Vec<bool>,
    /// Registers some running operation reads.
    needed: Vec<bool>,
    /// Output slots whose stores are dead; empty when none is.
    dropped: Vec<bool>,
    /// Slots some running operation loads; empty when every slot is.
    loaded: Vec<bool>,
    /// The captures of each running conditional that neither arm loads, by
    /// operation.
    unused_captures: BTreeMap<usize, Box<[bool]>>,
}

impl SolveLiveness {
    pub fn of(program: &solve::TypedProgram, demand: &SolveOutputDemand) -> Self {
        let registers = program.register_types().len();
        let mut needed = vec![false; registers];
        if demand.is_all() {
            for operation in program.operations() {
                operation
                    .operation()
                    .visit_input_registers(|register| needed[register.index()] = true);
            }
            return Self {
                dead: Vec::new(),
                needed,
                ..Self::default()
            };
        }
        let dropped = dropped_slots(program, demand);
        let mut dead = vec![false; program.operations().len()];
        let mut loaded = vec![false; program.slots().len()];
        let mut unused_captures = BTreeMap::new();
        // Producers precede their readers, so one backward sweep reaches all.
        for (index, spanned) in program.operations().iter().enumerate().rev() {
            let operation = spanned.operation();
            let runs = match operation {
                solve::SolveOperation::Store { slot, .. } => !dropped[slot.index()],
                _ if program.is_prunable(index) => {
                    let mut read = false;
                    operation.visit_output_registers(|register| read |= needed[register.index()]);
                    read
                }
                _ => true,
            };
            if !runs {
                dead[index] = true;
                continue;
            }
            if let solve::SolveOperation::Load { slot, .. } = operation {
                loaded[slot.index()] = true;
            }
            let unused = unused_conditional_captures(operation, &needed).unwrap_or_default();
            visit_demanded_reads(operation, &unused, |register| {
                needed[register.index()] = true
            });
            if !unused.is_empty() {
                unused_captures.insert(index, unused);
            }
        }
        Self {
            dead,
            needed,
            dropped,
            loaded,
            unused_captures,
        }
    }

    pub fn is_dead(&self, operation: usize) -> bool {
        self.dead.get(operation).copied().unwrap_or(false)
    }

    /// Whether the body leaves output `slot` unbuilt.
    pub fn drops(&self, slot: usize) -> bool {
        self.dropped.get(slot).copied().unwrap_or(false)
    }

    /// Whether a running operation loads `slot`.
    pub fn loads(&self, slot: usize) -> bool {
        self.loaded.get(slot).copied().unwrap_or(true)
    }

    /// Whether a running operation loads the `ordinal`-th input slot of `program`.
    fn loads_input(&self, program: &solve::TypedProgram, ordinal: usize) -> bool {
        program
            .slots()
            .iter()
            .enumerate()
            .filter(|(_, slot)| slot.storage() == solve::SolveStorageClass::Input)
            .nth(ordinal)
            .is_none_or(|(slot, _)| self.loads(slot))
    }

    /// The captures of the conditional at `operation` that neither arm loads,
    /// in capture order; empty when every capture is loaded.
    pub fn unused_captures(&self, operation: usize) -> &[bool] {
        self.unused_captures
            .get(&operation)
            .map_or(&[], |unused| unused)
    }

    pub fn needs(&self, register: solve::SolveRegisterId) -> bool {
        self.needed.get(register.index()).copied().unwrap_or(false)
    }

    pub fn needed(&self) -> &[bool] {
        &self.needed
    }

    pub fn dead(&self) -> &[bool] {
        &self.dead
    }
}

/// The registers an operation reads, leaving out the captures of a conditional
/// that `unused` marks (in capture order, after the condition).
pub fn visit_demanded_reads(
    operation: &solve::SolveOperation,
    unused: &[bool],
    mut visit: impl FnMut(solve::SolveRegisterId),
) {
    let mut position = 0usize;
    operation.visit_input_registers(|register| {
        let capture = position.checked_sub(1).and_then(|k| unused.get(k));
        if !capture.copied().unwrap_or(false) {
            visit(register);
        }
        position += 1;
    });
}

/// The captures of a running conditional that neither of its arms loads, when
/// the arms are built for the results `needed` reads. The arms are planned for
/// the same demand, so a capture they do not load is never copied or borrowed.
fn unused_conditional_captures(
    operation: &solve::SolveOperation,
    needed: &[bool],
) -> Option<Box<[bool]>> {
    let solve::SolveOperation::Conditional {
        captures,
        destinations,
        if_true,
        if_false,
        ..
    } = operation
    else {
        return None;
    };
    let demand = SolveOutputDemand::of_destinations(destinations, needed);
    if demand.is_all() {
        return None;
    }
    let arms = [if_true, if_false].map(|arm| (arm.body(), SolveLiveness::of(arm.body(), &demand)));
    let unused = (0..captures.len())
        .map(|capture| {
            !arms
                .iter()
                .any(|(body, arm)| arm.loads_input(body, capture))
        })
        .collect::<Box<[bool]>>();
    unused.contains(&true).then_some(unused)
}

/// The slots of the outputs `demand` leaves unread. An output the body loads
/// is read by the body itself, so its stores stay.
fn dropped_slots(program: &solve::TypedProgram, demand: &SolveOutputDemand) -> Vec<bool> {
    let mut dropped = vec![false; program.slots().len()];
    let outputs = program
        .slots()
        .iter()
        .enumerate()
        .filter(|(_, slot)| slot.storage() == solve::SolveStorageClass::Output);
    for (ordinal, (slot, _)) in outputs.enumerate() {
        dropped[slot] = !demand.wants(ordinal);
    }
    for operation in program.operations() {
        if let solve::SolveOperation::Load { slot, .. } = operation.operation() {
            dropped[slot.index()] = false;
        }
    }
    dropped
}

/// Every call a body runs with the outputs of it the body reads, in operation
/// order, descending into the regions that run. A region of a conditional
/// builds only the results its conditional reads; every other region builds all
/// of its outputs.
pub fn visit_demanded_calls(
    program: &solve::TypedProgram,
    liveness: &SolveLiveness,
    visit: &mut impl FnMut(solve::SolvePureCallOwnerId, SolveOutputDemand),
) {
    use solve::SolveOperation as O;
    for (index, spanned) in program.operations().iter().enumerate() {
        if liveness.is_dead(index) {
            continue;
        }
        match spanned.operation() {
            O::Call {
                owner,
                destinations,
                ..
            } => visit(
                *owner,
                SolveOutputDemand::of_destinations(destinations, liveness.needed()),
            ),
            O::Conditional {
                destinations,
                if_true,
                if_false,
                ..
            } => {
                let demand = SolveOutputDemand::of_destinations(destinations, liveness.needed());
                visit_region(if_true.body(), &demand, visit);
                visit_region(if_false.body(), &demand, visit);
            }
            O::AssignedConditional { program } => {
                visit_region(program.if_true().body(), &SolveOutputDemand::all(), visit);
                visit_region(program.if_false().body(), &SolveOutputDemand::all(), visit);
            }
            O::Map { body, .. } => visit_region(body.body(), &SolveOutputDemand::all(), visit),
            O::Fold {
                transition,
                continuation,
                ..
            } => {
                visit_region(transition.body(), &SolveOutputDemand::all(), visit);
                if let Some(predicate) = continuation {
                    visit_region(predicate.body(), &SolveOutputDemand::all(), visit);
                }
            }
            O::CheckAssertion {
                message: solve::SolveAssertionMessage::Captures { program },
                ..
            } => visit_region(program.body(), &SolveOutputDemand::all(), visit),
            _ => {}
        }
    }
}

fn visit_region(
    body: &solve::TypedProgram,
    demand: &SolveOutputDemand,
    visit: &mut impl FnMut(solve::SolvePureCallOwnerId, SolveOutputDemand),
) {
    visit_demanded_calls(body, &SolveLiveness::of(body, demand), visit);
}

impl SolveLiveness {
    /// The demand of every call operation of `program`: the outputs of the
    /// callee that `program` itself reads, with `None` for any other
    /// operation. A plan built for the call site serves exactly this set.
    pub(super) fn call_site_demands(
        program: &solve::TypedProgram,
    ) -> Box<[Option<SolveOutputDemand>]> {
        let liveness = Self::of(program, &SolveOutputDemand::all());
        program
            .operations()
            .iter()
            .map(|spanned| match spanned.operation() {
                solve::SolveOperation::Call { destinations, .. } => Some(
                    SolveOutputDemand::of_destinations(destinations, liveness.needed()),
                ),
                _ => None,
            })
            .collect()
    }
}
