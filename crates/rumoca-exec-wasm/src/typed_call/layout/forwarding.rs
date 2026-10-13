//! A call result a conditional returns whole is built in the conditional's
//! output buffer (SOLVE-C95).
//!
//! When an output of the body is the result of a conditional, the conditional
//! reserves the output's buffer and an arm that builds its value copies it
//! there. An arm that returns the by-reference result of a call nothing else
//! reads needs no copy if the call builds that result in the buffer itself: the
//! result then names the buffer, or an argument the call returns unchanged,
//! and both outlive the body. The result may reach the arm through nested
//! conditionals, each of which returns it whole.
use super::{CellRange, FramePlan, outputs, visit_reads};
use rumoca_ir_solve as solve;

/// A call result built in the buffer of an output: its register range and the
/// arguments the call may return unchanged instead.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Forwarded {
    pub range: CellRange,
    pub names: Vec<CellRange>,
}

impl FramePlan {
    /// The call results this body may build in an output buffer: the result a
    /// conditional with a hinted result returns whole from an arm, when it is
    /// read by that conditional alone and returned by no other output of its arms.
    /// The call that defines one decides at its own planning whether the
    /// values it may return instead outlive the body.
    pub(super) fn find_forwardable_calls(&mut self, program: &solve::TypedProgram) {
        let mut reads = vec![0u32; program.register_types().len()];
        for (index, operation) in program.operations().iter().enumerate() {
            if !self.liveness.is_dead(index) {
                visit_reads(
                    operation.operation(),
                    self.liveness.unused_captures(index),
                    |register| reads[register.index()] += 1,
                );
            }
        }
        for (index, operation) in program.operations().iter().enumerate() {
            if !self.liveness.is_dead(index) {
                self.offer_conditional_results(program, operation.operation(), &reads);
            }
        }
    }

    /// The call results the arms of `operation`, a conditional, return whole as
    /// its hinted results.
    fn offer_conditional_results(
        &mut self,
        program: &solve::TypedProgram,
        operation: &solve::SolveOperation,
        reads: &[u32],
    ) {
        let solve::SolveOperation::Conditional {
            captures,
            destinations,
            if_true,
            if_false,
            ..
        } = operation
        else {
            return;
        };
        let arms = [if_true.body(), if_false.body()];
        for (output, destination) in destinations.iter().enumerate() {
            let Some(hint) = self.register_hints.get(&destination.index()).copied() else {
                continue;
            };
            let candidates = arms
                .iter()
                .flat_map(|arm| whole_returns(arm, output).unwrap_or_default())
                .filter(|input| captured_once(captures, *input))
                .filter(|input| {
                    arms.iter()
                        .all(|arm| !returned_elsewhere(arm, output, *input))
                })
                .collect::<Vec<_>>();
            for input in candidates {
                self.offer_candidate(program, captures.get(input), reads, hint);
            }
        }
    }

    /// Note `capture` as a call result to build in the buffer of `hint`'s
    /// output when it is read once, by the conditional that captures it, and
    /// no other result is already noted for that output.
    fn offer_candidate(
        &mut self,
        program: &solve::TypedProgram,
        capture: Option<&solve::SolveRegisterId>,
        reads: &[u32],
        hint: (usize, super::OutputHint),
    ) {
        let Some(capture) = capture.copied() else {
            return;
        };
        let taken = self
            .hint_candidates
            .values()
            .any(|(ordinal, _)| *ordinal == hint.0);
        if reads[capture.index()] == 1 && !taken && self.defined_by_call(program, capture) {
            self.hint_candidates.insert(capture.index(), hint);
        }
    }

    fn defined_by_call(
        &self,
        program: &solve::TypedProgram,
        register: solve::SolveRegisterId,
    ) -> bool {
        program
            .operations()
            .iter()
            .enumerate()
            .any(|(index, operation)| match operation.operation() {
                solve::SolveOperation::Call { destinations, .. } => {
                    !self.liveness.is_dead(index) && destinations.contains(&register)
                }
                _ => false,
            })
    }

    /// Build the by-reference results of a call that are candidates in the
    /// output buffer they are returned in, when everything else the call may
    /// return outlives this body.
    pub(super) fn activate_forwarded_results(
        &mut self,
        callee: &Self,
        arguments: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
    ) {
        for (ordinal, destination) in destinations.iter().enumerate() {
            let Some((output, hint)) = self.hint_candidates.get(&destination.index()).copied()
            else {
                continue;
            };
            let slot = callee.output_slots[ordinal];
            if !callee.ref_outputs.contains_key(&slot) || !self.liveness.needs(*destination) {
                continue;
            }
            let named = self.aliased_arguments(callee, slot, arguments);
            if named
                .iter()
                .all(|source| outputs::parent_owned(*source, hint.floor))
            {
                self.register_hints
                    .entry(destination.index())
                    .or_insert((output, hint));
                self.forwarded_names.insert(destination.index(), named);
            }
        }
    }

    /// The call results built in the buffer of output `ordinal`: this body's
    /// own and the ones its parent built in the buffer of the result this body
    /// returns as that output.
    pub(super) fn forwarded(&self, ordinal: usize) -> Vec<Forwarded> {
        let own = self
            .forwarded_names
            .iter()
            .filter(|(register, _)| {
                self.register_hints
                    .get(register)
                    .is_some_and(|(output, _)| *output == ordinal)
            })
            .filter_map(|(register, names)| {
                Some(Forwarded {
                    range: *self.registers.get(*register)?,
                    names: names.clone(),
                })
            });
        let inherited = self
            .inherited_forwards
            .iter()
            .filter(|(output, _)| *output == ordinal)
            .map(|(_, forwarded)| forwarded.clone());
        own.chain(inherited).collect()
    }

    /// What a value named by a forwarded call result may be: the arguments the
    /// call returns unchanged. Any other source names itself.
    pub(super) fn expand_forwarded(&self, sources: &[CellRange]) -> Vec<CellRange> {
        let own = self
            .forwarded_names
            .iter()
            .filter_map(|(register, names)| Some((*self.registers.get(*register)?, names)));
        let inherited = self
            .inherited_forwards
            .iter()
            .map(|(_, forwarded)| (forwarded.range, &forwarded.names));
        let known = own.chain(inherited).collect::<Vec<_>>();
        sources
            .iter()
            .flat_map(|source| {
                known
                    .iter()
                    .find(|(range, _)| range == source)
                    .map_or_else(|| vec![*source], |(_, names)| (*names).clone())
            })
            .collect()
    }
}

/// Whether the capture for `input` is listed once among `captures`.
fn captured_once(captures: &[solve::SolveRegisterId], input: usize) -> bool {
    captures
        .get(input)
        .is_some_and(|capture| captures.iter().filter(|other| *other == capture).count() == 1)
}

/// The inputs of `body` that its output `output` may return whole, or `None`
/// when the output is not the single completed return of one register and so
/// may name anything.
fn whole_returns(body: &solve::TypedProgram, output: usize) -> Option<Vec<usize>> {
    let slot = *outputs::interface(body, solve::SolveStorageClass::Output).get(output)?;
    let returned = outputs::completed_return(body, slot)?;
    Some(returned_whole(body, returned))
}

/// The inputs register `returned` may be whole: the input it loads, or the
/// ones the arms of the conditional that defines it may return from their
/// captures.
fn returned_whole(body: &solve::TypedProgram, returned: solve::SolveRegisterId) -> Vec<usize> {
    let defined = body.operations().iter().find_map(|operation| {
        let mut hit = false;
        operation
            .operation()
            .visit_output_registers(|register| hit |= register == returned);
        hit.then(|| operation.operation())
    });
    match defined {
        Some(solve::SolveOperation::Load { slot, .. }) => {
            outputs::interface(body, solve::SolveStorageClass::Input)
                .iter()
                .position(|input| input == slot)
                .into_iter()
                .collect()
        }
        Some(solve::SolveOperation::Conditional {
            captures,
            destinations,
            if_true,
            if_false,
            ..
        }) => {
            let Some(output) = destinations
                .iter()
                .position(|destination| *destination == returned)
            else {
                return Vec::new();
            };
            let mut inputs = Vec::new();
            for arm in [if_true, if_false] {
                let taken = match whole_returns(arm.body(), output) {
                    Some(arm_inputs) => arm_inputs
                        .into_iter()
                        .filter_map(|input| captures.get(input).copied())
                        .collect::<Vec<_>>(),
                    // An output of unknown source may name any capture.
                    None => captures.to_vec(),
                };
                for capture in taken {
                    inputs.extend(returned_whole(body, capture));
                }
            }
            inputs
        }
        _ => Vec::new(),
    }
}

/// Whether an output of `arm` other than `target` may return `input` whole, or
/// may name anything: that output would then name the storage a call result is
/// built in, which the output `target` overwrites.
fn returned_elsewhere(arm: &solve::TypedProgram, target: usize, input: usize) -> bool {
    let count = outputs::interface(arm, solve::SolveStorageClass::Output).len();
    (0..count)
        .filter(|output| *output != target)
        .any(|output| whole_returns(arm, output).is_none_or(|inputs| inputs.contains(&input)))
}
