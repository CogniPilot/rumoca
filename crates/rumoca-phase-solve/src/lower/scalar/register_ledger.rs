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

/// Bytes of run-time register file one program may need (16 bytes per register).
/// This protects the memory a backend allocates to execute the program, which
/// is what the 4 GiB WASM linear memory must also hold the model storage beside.
pub(super) const REGISTER_FILE_BYTE_BUDGET: u64 = 256 << 20;

/// Bytes of lowering metadata (operation list and register facts) one program
/// may hold. This protects the compiler's own memory while it builds the
/// program, charged as each operation and fact is issued so a program unrolled
/// into scalars is refused before that memory is spent.
pub(super) const METADATA_BYTE_BUDGET: u64 = 256 << 20;

/// The same metadata cost authority for scalar construction and optional fusion.
pub(in crate::lower) struct MetadataBudget(u64);

impl Default for MetadataBudget {
    fn default() -> Self {
        Self(METADATA_BYTE_BUDGET)
    }
}

impl MetadataBudget {
    fn bytes(operations: usize, facts: usize) -> u64 {
        (operations as u64)
            .saturating_mul(OPERATION_BYTES)
            .saturating_add((facts as u64).saturating_mul(FACT_BYTES))
    }

    pub(in crate::lower) fn permits_operations(&self, count: usize) -> bool {
        Self::bytes(count, 0) <= self.0
    }

    #[cfg(test)]
    pub(in crate::lower) fn with_bytes(bytes: u64) -> Self {
        Self(bytes)
    }
}

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
    file_budget: u64,
    metadata_budget: MetadataBudget,
    facts: FxHashMap<solve::Reg, RegisterFact>,
    owners: Vec<OwnerUse>,
    owner_slots: FxHashMap<OwnerKey, usize>,
    last_owner: Option<(OwnerKey, usize)>,
    last_span: Option<Span>,
    registers: u64,
    operations: usize,
    peak_facts: usize,
    peak_ops: usize,
}

impl RegisterLedger {
    pub(super) fn new() -> Self {
        Self::with_byte_budgets(REGISTER_FILE_BYTE_BUDGET, METADATA_BYTE_BUDGET)
    }

    /// A ledger whose register file alone may hold `registers`.
    #[cfg(test)]
    pub(super) fn with_budget(registers: u64) -> Self {
        Self::with_byte_budgets(registers * REGISTER_BYTES, registers * REGISTER_BYTES)
    }

    pub(super) fn with_byte_budgets(file_budget: u64, metadata_budget: u64) -> Self {
        Self {
            file_budget,
            metadata_budget: MetadataBudget(metadata_budget),
            facts: FxHashMap::default(),
            owners: Vec::new(),
            owner_slots: FxHashMap::default(),
            last_owner: None,
            last_span: None,
            registers: 0,
            operations: 0,
            peak_facts: 0,
            peak_ops: 0,
        }
    }

    /// Admit one register range for `owner`, or refuse with the owners that
    /// already hold the file. Nothing is recorded on refusal.
    pub(super) fn admit(
        &mut self,
        owner: OwnerKey,
        mut name: impl FnMut() -> String,
        allocation: Allocation,
        span: Span,
    ) -> Result<(), LowerError> {
        let Allocation {
            count,
            end: total,
            operations: ops,
        } = allocation;
        self.check_usage(
            total,
            ops,
            self.facts.len(),
            || self.owner_name(owner).unwrap_or_else(&mut name),
            span,
        )?;
        let slot = match self.last_owner {
            Some((key, slot)) if key == owner => slot,
            _ => self.owner_slot(owner, name),
        };
        self.last_owner = Some((owner, slot));
        self.last_span = Some(span);
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

    fn check_usage(
        &self,
        total: u64,
        ops: usize,
        facts: usize,
        culprit: impl FnOnce() -> String,
        span: Span,
    ) -> Result<(), LowerError> {
        let file = total.saturating_mul(REGISTER_BYTES);
        let metadata = MetadataBudget::bytes(ops, facts);
        if file > self.file_budget || metadata > self.metadata_budget.0 {
            let culprit = culprit();
            return Err(LowerError::budget_exceeded(
                format!(
                    "register budget exceeded in {culprit}: the program needs {total} registers \
                     ({REGISTER_BYTES} bytes each), {ops} operations ({OPERATION_BYTES} bytes each) \
                     and {facts} register facts ({FACT_BYTES} bytes each): {file} bytes of register file \
                     against {} and {metadata} bytes of metadata against {}; \
                     owners by registers: {}",
                    self.file_budget,
                    self.metadata_budget.0,
                    self.owner_table(REFUSAL_OWNERS),
                ),
                span,
            ));
        }
        Ok(())
    }

    fn admit_metadata(&self, operations: usize, facts: usize) -> Result<(), LowerError> {
        let Some((_, slot)) = self.last_owner else {
            // A no-register program cannot carry register facts or operations.
            assert_eq!((operations, facts), (0, 0));
            return Ok(());
        };
        self.check_usage(
            self.registers,
            operations,
            facts,
            || self.owners[slot].name.clone(),
            self.last_span.expect("allocated owner has a span"),
        )
    }

    /// Charge the prospective operation before growing the compiler's buffer.
    pub(super) fn admit_operation(&mut self, count: usize) -> Result<(), LowerError> {
        self.admit_metadata(count, self.facts.len())?;
        self.operations = count;
        self.peak_ops = self.peak_ops.max(count);
        Ok(())
    }

    /// Finalization consumes the same tracked construction counts, not an IR scan.
    pub(super) fn finish(&self, operations: usize) -> Result<(), LowerError> {
        assert_eq!(
            operations, self.operations,
            "operations bypassed checked emission"
        );
        self.admit_metadata(operations, self.facts.len())
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

    pub(super) fn set_integer(
        &mut self,
        register: solve::Reg,
        value: Option<i64>,
    ) -> Result<(), LowerError> {
        self.update(register, |fact| fact.integer = value)
    }

    pub(super) fn set_real(&mut self, register: solve::Reg, value: f64) -> Result<(), LowerError> {
        self.update(register, |fact| fact.real = Some(value))
    }

    pub(super) fn set_negated(
        &mut self,
        register: solve::Reg,
        operand: solve::Reg,
    ) -> Result<(), LowerError> {
        self.update(register, |fact| fact.negated = Some(operand))
    }

    fn update(
        &mut self,
        register: solve::Reg,
        apply: impl FnOnce(&mut RegisterFact),
    ) -> Result<(), LowerError> {
        let previous = self.facts.get(&register).copied();
        let mut fact = previous.unwrap_or_default();
        apply(&mut fact);
        let count =
            self.facts.len() - usize::from(previous.is_some()) + usize::from(!fact.is_empty());
        self.admit_metadata(self.operations, count)?;
        if fact.is_empty() {
            self.facts.remove(&register);
        } else {
            self.facts.insert(register, fact);
        }
        self.peak_facts = self.peak_facts.max(self.facts.len());
        Ok(())
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
