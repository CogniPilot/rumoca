//! Register accounting of one scalar program.
//!
//! The ledger owns three things that belong together: the exact facts known
//! about individual registers (a literal value, an exact Integer value, the
//! operand a register negates), the owner every allocated register range is
//! charged to, and the budget that bounds the register file before an
//! allocation is attempted.
//!
//! Facts are stored sparsely. A tensor, a typed call result, or a packed
//! record field is allocated as one register range and carries no per-register
//! metadata, so its footprint is its register count and nothing else.

use std::fmt::Write as _;

use rumoca_core::Span;
use rumoca_ir_solve as solve;
use rustc_hash::FxHashMap;

use crate::LowerError;

/// Run-time bytes one register occupies: one primal and one tangent `f64`.
pub(super) const REGISTER_BYTES: u64 = 16;

/// Bytes one program may hold across its register file, its operation list and
/// its register facts. Each is charged as it grows, so a program unrolled into
/// scalars is refused before the memory of its operations and facts is spent,
/// not after the register file alone reaches the limit.
pub(super) const PROGRAM_BYTE_BUDGET: u64 = 256 << 20;

/// Bytes of one operation in the operation list.
const OPERATION_BYTES: u64 = std::mem::size_of::<solve::LinearOp>() as u64;

/// Bytes of one register fact entry.
const FACT_BYTES: u64 = std::mem::size_of::<(solve::Reg, RegisterFact)>() as u64;

/// A program that allocates at least this many registers reports its owners to
/// the `rumoca_phase_solve::registers` trace target.
#[cfg(feature = "tracing")]
const REPORT_THRESHOLD: u64 = 1 << 16;

/// A single range of at least this many registers is reported when allocated.
#[cfg(feature = "tracing")]
const LARGE_RANGE: u64 = 1 << 14;

/// Owners named in a budget refusal.
const REFUSAL_OWNERS: usize = 5;

/// One register range to admit: its width, the register file size after it, and
/// the operations issued so far.
pub(super) struct Allocation {
    pub(super) count: u64,
    pub(super) end: u64,
    pub(super) operations: usize,
}

/// What a register range is charged to: the pure function being lowered (or the
/// model row that owns the program) and the lowering site that asked for it.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(super) struct OwnerKey {
    /// The pure function being lowered, or none for the model row itself.
    pub(super) function: Option<u32>,
    /// The lowering site that asked for the range.
    pub(super) site: &'static std::panic::Location<'static>,
}

#[derive(Clone, Copy, Default)]
struct RegisterFact {
    integer: Option<i64>,
    real: Option<f64>,
    negated: Option<solve::Reg>,
}

impl RegisterFact {
    const fn is_empty(&self) -> bool {
        self.integer.is_none() && self.real.is_none() && self.negated.is_none()
    }
}

struct OwnerUse {
    key: OwnerKey,
    name: String,
    registers: u64,
}

pub(super) struct RegisterLedger {
    budget: u64,
    facts: FxHashMap<solve::Reg, RegisterFact>,
    owners: Vec<OwnerUse>,
    owner_slots: FxHashMap<OwnerKey, usize>,
    last_owner: Option<(OwnerKey, usize)>,
    registers: u64,
    peak_facts: usize,
    peak_ops: usize,
}

impl RegisterLedger {
    pub(super) fn new() -> Self {
        Self::with_byte_budget(PROGRAM_BYTE_BUDGET)
    }

    /// A ledger whose register file alone may hold `registers`.
    #[cfg(test)]
    pub(super) fn with_budget(registers: u64) -> Self {
        Self::with_byte_budget(registers * REGISTER_BYTES)
    }

    fn with_byte_budget(budget: u64) -> Self {
        Self {
            budget,
            facts: FxHashMap::default(),
            owners: Vec::new(),
            owner_slots: FxHashMap::default(),
            last_owner: None,
            registers: 0,
            peak_facts: 0,
            peak_ops: 0,
        }
    }

    /// Admit one register range for `owner`, or refuse with the owners that
    /// already hold the file. Nothing is recorded on refusal.
    pub(super) fn admit(
        &mut self,
        owner: OwnerKey,
        name: impl FnOnce() -> String,
        allocation: Allocation,
        span: Span,
    ) -> Result<(), LowerError> {
        let Allocation {
            count,
            end: total,
            operations: ops,
        } = allocation;
        let facts = self.facts.len() as u64;
        let needed = total
            .saturating_mul(REGISTER_BYTES)
            .saturating_add((ops as u64).saturating_mul(OPERATION_BYTES))
            .saturating_add(facts.saturating_mul(FACT_BYTES));
        if needed > self.budget {
            let culprit = self.owner_name(owner).unwrap_or_else(name);
            return Err(LowerError::budget_exceeded(
                format!(
                    "register budget exceeded in {culprit}: the program needs {total} registers \
                     ({REGISTER_BYTES} bytes each), {ops} operations ({OPERATION_BYTES} bytes each) \
                     and {facts} register facts ({FACT_BYTES} bytes each), {} bytes, and may hold {} bytes; \
                     owners by registers: {}",
                    needed,
                    self.budget,
                    self.owner_table(REFUSAL_OWNERS),
                ),
                span,
            ));
        }
        let slot = match self.last_owner {
            Some((key, slot)) if key == owner => slot,
            _ => self.owner_slot(owner, name),
        };
        self.last_owner = Some((owner, slot));
        self.owners[slot].registers += count;
        #[cfg(feature = "tracing")]
        if count >= LARGE_RANGE {
            tracing::debug!(
                target: "rumoca_phase_solve::registers",
                owner = %self.owners[slot].name,
                registers = count,
                bytes = count.saturating_mul(REGISTER_BYTES),
                "large register range"
            );
        }
        self.registers = self.registers.max(total);
        self.peak_ops = self.peak_ops.max(ops);
        Ok(())
    }

    #[cfg(test)]
    pub(super) fn fact_count(&self) -> usize {
        self.facts.len()
    }

    fn owner_slot(&mut self, owner: OwnerKey, name: impl FnOnce() -> String) -> usize {
        if let Some(&slot) = self.owner_slots.get(&owner) {
            return slot;
        }
        let slot = self.owners.len();
        self.owners.push(OwnerUse {
            key: owner,
            name: name(),
            registers: 0,
        });
        self.owner_slots.insert(owner, slot);
        slot
    }

    fn owner_name(&self, owner: OwnerKey) -> Option<String> {
        let slot = *self.owner_slots.get(&owner)?;
        Some(self.owners[slot].name.clone())
    }

    /// The `limit` owners holding the most registers, largest first.
    pub(super) fn owner_table(&self, limit: usize) -> String {
        let mut order: Vec<&OwnerUse> = self.owners.iter().collect();
        order.sort_by(|left, right| {
            right
                .registers
                .cmp(&left.registers)
                .then_with(|| left.key.cmp(&right.key))
        });
        let mut table = String::new();
        for (position, owner) in order.into_iter().take(limit).enumerate() {
            if position > 0 {
                table.push_str("; ");
            }
            let _ = write!(
                table,
                "{} -> {} registers, {} bytes",
                owner.name,
                owner.registers,
                owner.registers.saturating_mul(REGISTER_BYTES),
            );
        }
        table
    }

    pub(super) fn integer(&self, register: solve::Reg) -> Option<i64> {
        self.facts.get(&register)?.integer
    }

    pub(super) fn real(&self, register: solve::Reg) -> Option<f64> {
        self.facts.get(&register)?.real
    }

    pub(super) fn negated(&self, register: solve::Reg) -> Option<solve::Reg> {
        self.facts.get(&register)?.negated
    }

    pub(super) fn set_integer(&mut self, register: solve::Reg, value: Option<i64>) {
        self.update(register, |fact| fact.integer = value);
    }

    pub(super) fn set_real(&mut self, register: solve::Reg, value: f64) {
        self.update(register, |fact| fact.real = Some(value));
    }

    pub(super) fn set_negated(&mut self, register: solve::Reg, operand: solve::Reg) {
        self.update(register, |fact| fact.negated = Some(operand));
    }

    fn update(&mut self, register: solve::Reg, apply: impl FnOnce(&mut RegisterFact)) {
        let fact = self.facts.entry(register).or_default();
        apply(fact);
        if fact.is_empty() {
            self.facts.remove(&register);
        }
        self.peak_facts = self.peak_facts.max(self.facts.len());
    }
}

#[cfg(feature = "tracing")]
impl Drop for RegisterLedger {
    fn drop(&mut self) {
        if self.registers < REPORT_THRESHOLD {
            return;
        }
        tracing::debug!(
            target: "rumoca_phase_solve::registers",
            registers = self.registers,
            register_file_bytes = self.registers.saturating_mul(REGISTER_BYTES),
            fact_registers = self.peak_facts,
            fact_bytes = self.peak_facts * std::mem::size_of::<(solve::Reg, RegisterFact)>(),
            ops_high_water = self.peak_ops,
            ops_high_water_bytes = self.peak_ops * std::mem::size_of::<solve::LinearOp>(),
            owners = %self.owner_table(16),
            "scalar program register allocation"
        );
    }
}
