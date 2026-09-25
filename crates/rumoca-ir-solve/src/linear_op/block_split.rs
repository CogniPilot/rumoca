//! Block residual split (SPEC_0043 §6a): one residual program divided, for
//! one projection block's unknowns, into an invariant part evaluated once per
//! block projection call and a dependent part evaluated per pass over the
//! invariant part's live-out registers.

use std::collections::BTreeSet;

use super::*;

/// A checked split of one residual program for one block's unknown set.
#[derive(Clone, Debug, PartialEq)]
pub struct BlockResidualSplit {
    invariant: Box<[LinearOp]>,
    live_out: Box<[Reg]>,
    dependent: Box<[LinearOp]>,
    register_count: usize,
}

/// Why a split is not the one its program and unknown set construct.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlockResidualSplitError {
    /// The program's register flow is invalid.
    Program,
    /// The invariant or dependent part differs from the re-derived one.
    Mismatch,
    /// The dependent part writes a live-out register, clobbering its preset.
    ClobberedPreset { register: Reg },
    /// A part reads a register it neither writes nor receives.
    UndefinedRegister { register: Reg },
}

impl BlockResidualSplit {
    /// The split of `program` for the block unknowns `unknowns`, or `None`
    /// when no operation is invariant or the program's register flow is
    /// invalid.
    #[must_use]
    pub fn derive(program: &[LinearOp], unknowns: &[usize]) -> Option<Self> {
        let register_count = ScalarProgramRegisterFlow::derive(program)
            .ok()?
            .register_count();
        let unknowns: BTreeSet<usize> = unknowns.iter().copied().collect();
        let hoisted = invariant_positions(program, &unknowns, register_count)?;
        if !hoisted.iter().any(|&hoisted| hoisted) {
            return None;
        }
        let (invariant, dependent): (Vec<_>, Vec<_>) = program
            .iter()
            .zip(&hoisted)
            .partition(|(_, hoisted)| **hoisted);
        let invariant: Box<[LinearOp]> = invariant.into_iter().map(|(op, _)| op.clone()).collect();
        let dependent: Box<[LinearOp]> = dependent.into_iter().map(|(op, _)| op.clone()).collect();
        let live_out = live_out_registers(&dependent, &invariant, register_count)?;
        Some(Self {
            invariant,
            live_out: live_out.into_boxed_slice(),
            dependent,
            register_count,
        })
    }

    /// Re-derive the split of `program` for `unknowns` and reject any
    /// difference, a dependent write to a live-out register, and a read
    /// neither part defines.
    pub fn check(
        &self,
        program: &[LinearOp],
        unknowns: &[usize],
    ) -> Result<(), BlockResidualSplitError> {
        self.check_flow()?;
        let derived = Self::derive(program, unknowns).ok_or(BlockResidualSplitError::Mismatch)?;
        if derived != *self {
            return Err(BlockResidualSplitError::Mismatch);
        }
        Ok(())
    }

    /// The preset and definition discipline of the two parts on their own.
    fn check_flow(&self) -> Result<(), BlockResidualSplitError> {
        let mut defined = vec![false; self.register_count];
        validate_part(&self.invariant, &mut defined)?;
        let mut preset = vec![false; self.register_count];
        for &register in self.live_out.iter() {
            if !defined.get(register as usize).copied().unwrap_or(false) {
                return Err(BlockResidualSplitError::UndefinedRegister { register });
            }
            preset[register as usize] = true;
        }
        if let Some(register) = self
            .dependent
            .iter()
            .filter_map(|op| {
                let start = op.dst_register()? as usize;
                (start..start + op.dst_register_count()).find(|&r| preset.get(r) == Some(&true))
            })
            .next()
        {
            return Err(BlockResidualSplitError::ClobberedPreset {
                register: register as Reg,
            });
        }
        validate_part(&self.dependent, &mut preset)
    }

    /// The invariant operations in program order.
    #[must_use]
    pub fn invariant(&self) -> &[LinearOp] {
        &self.invariant
    }

    /// Registers of the invariant part the dependent part reads, ascending.
    #[must_use]
    pub fn live_out(&self) -> &[Reg] {
        &self.live_out
    }

    /// The remaining operations in program order, output stores included.
    #[must_use]
    pub fn dependent(&self) -> &[LinearOp] {
        &self.dependent
    }

    /// Register file size of both parts (that of the program).
    #[must_use]
    pub const fn register_count(&self) -> usize {
        self.register_count
    }

    /// The invariant part as a standalone program storing its live-out
    /// registers, in [`Self::live_out`] order, as its outputs.
    #[must_use]
    pub fn invariant_program(&self) -> Vec<LinearOp> {
        let mut program = self.invariant.to_vec();
        program.extend(
            self.live_out
                .iter()
                .map(|&src| LinearOp::StoreOutput { src }),
        );
        program
    }

    /// The dependent part as a standalone program receiving the live-out
    /// values from its seed vector (live-out `i` at seed index `offset + i`,
    /// so the programs of one block share one vector), storing the original
    /// program's outputs.
    #[must_use]
    pub fn dependent_program(&self, offset: usize) -> Vec<LinearOp> {
        let mut program: Vec<LinearOp> = self
            .live_out
            .iter()
            .enumerate()
            .map(|(index, &dst)| LinearOp::LoadSeed {
                dst,
                index: offset + index,
            })
            .collect();
        program.extend(self.dependent.iter().cloned());
        program
    }
}

/// Validate `part` in order over the registers `defined` already holds,
/// marking each write.
fn validate_part(part: &[LinearOp], defined: &mut [bool]) -> Result<(), BlockResidualSplitError> {
    let mut validation = ScalarProgramValidationCache::default();
    for (index, op) in part.iter().enumerate() {
        match validate_op_sources(op, index, defined, None, None, &mut validation) {
            Ok(_) => {}
            Err(ScalarProgramRegisterError::UndefinedRegister { register, .. }) => {
                return Err(BlockResidualSplitError::UndefinedRegister { register });
            }
            Err(_) => return Err(BlockResidualSplitError::Program),
        }
        if let Some(start) = op.dst_register() {
            let end = start as usize + op.dst_register_count();
            defined
                .get_mut(start as usize..end)
                .ok_or(BlockResidualSplitError::Program)?
                .fill(true);
        }
    }
    Ok(())
}

/// Invariant positions: outside the block-unknown cone (every register
/// version read was written by an invariant operation, and no block unknown
/// is read directly or in a nested body), every written register written once
/// in the program, no non-repeatable effect, and not an output store.
fn invariant_positions(
    program: &[LinearOp],
    unknowns: &BTreeSet<usize>,
    register_count: usize,
) -> Option<Vec<bool>> {
    let mut writes = vec![0_usize; register_count];
    for op in program {
        if let Some(start) = op.dst_register() {
            for count in writes.get_mut(start as usize..start as usize + op.dst_register_count())? {
                *count += 1;
            }
        }
    }
    let mut hoisted_version = vec![false; register_count];
    let mut validation = ScalarProgramValidationCache::default();
    let mut positions = Vec::with_capacity(program.len());
    for (index, op) in program.iter().enumerate() {
        let destination = op
            .dst_register()
            .map(|start| start as usize..start as usize + op.dst_register_count());
        let invariant = destination
            .clone()
            .is_some_and(|range| writes[range].iter().all(|&count| count == 1))
            && repeatable(op)
            && !reads_unknown(op, unknowns)
            && validate_op_sources(op, index, &hoisted_version, None, None, &mut validation)
                .is_ok();
        if let Some(range) = destination {
            hoisted_version[range].fill(invariant);
        }
        positions.push(invariant);
    }
    Some(positions)
}

/// Registers the dependent part reads before writing, which the invariant
/// part must supply.
fn live_out_registers(
    dependent: &[LinearOp],
    invariant: &[LinearOp],
    register_count: usize,
) -> Option<Vec<Reg>> {
    let mut supplied = vec![false; register_count];
    for op in invariant {
        if let Some(start) = op.dst_register() {
            supplied
                .get_mut(start as usize..start as usize + op.dst_register_count())?
                .fill(true);
        }
    }
    let mut defined = vec![false; register_count];
    let mut live_out = BTreeSet::new();
    let mut validation = ScalarProgramValidationCache::default();
    for (index, op) in dependent.iter().enumerate() {
        loop {
            match validate_op_sources(op, index, &defined, None, None, &mut validation) {
                Ok(_) => break,
                Err(ScalarProgramRegisterError::UndefinedRegister { register, .. })
                    if supplied.get(register as usize) == Some(&true)
                        && live_out.insert(register) =>
                {
                    defined[register as usize] = true;
                }
                Err(_) => return None,
            }
        }
        if let Some(start) = op.dst_register() {
            defined
                .get_mut(start as usize..start as usize + op.dst_register_count())?
                .fill(true);
        }
    }
    Some(live_out.into_iter().collect())
}

/// Whether evaluating `op` twice at the same operands gives the same result
/// and effects; output stores are the dependent part's by definition.
fn repeatable(op: &LinearOp) -> bool {
    !matches!(
        op,
        LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. }
            | LinearOp::LoadSeed { .. }
            | LinearOp::LoadIndexedSeed { .. }
            | LinearOp::StoreOutput { .. }
            | LinearOp::StoreOutputRange { .. }
            | LinearOp::StoreOutputFunctionFold { .. }
            | LinearOp::StoreOutputFoldTensorUpdate { .. }
    )
}

impl LinearOp {
    /// Whether this operation reads one of `indices` of solver Y directly
    /// (`LoadY`, a Y tensor range) or inside a fold or conditional body.
    #[must_use]
    pub fn reads_y_index_in(&self, indices: &BTreeSet<usize>) -> bool {
        reads_unknown(self, indices)
    }
}

/// Whether `op` reads a block unknown directly or inside a nested body.
fn reads_unknown(op: &LinearOp, unknowns: &BTreeSet<usize>) -> bool {
    match op {
        LinearOp::LoadY { index, .. } => unknowns.contains(index),
        LinearOp::TensorLoad {
            input: TensorInputKind::Y,
            input_start,
            count,
            ..
        } => unknowns
            .range(*input_start..input_start.saturating_add(*count))
            .next()
            .is_some(),
        LinearOp::FunctionFold { program, .. }
        | LinearOp::GuardedFunctionFold { program, .. }
        | LinearOp::StoreOutputFunctionFold { program, .. } => {
            program.update.iter().any(|op| reads_unknown(op, unknowns))
        }
        LinearOp::FunctionConditional { program, .. } => program
            .arms
            .iter()
            .flat_map(|arm| arm.condition.iter().chain(arm.result.iter()))
            .chain(program.fallback.iter())
            .any(|op| reads_unknown(op, unknowns)),
        _ => false,
    }
}

#[cfg(test)]
mod tests;
