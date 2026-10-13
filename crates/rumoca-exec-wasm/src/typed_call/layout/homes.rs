//! Scalar registers that live in wasm locals instead of scratch memory.
//!
//! A register is a single-cell value with one defining operation. When every
//! read of it is made by an operation that reads scalar operands through
//! [`Emitter::push_scalar`](super::super::emit::Emitter), the value never needs
//! an address, so the plan gives it a wasm local (or, for a constant, folds it
//! into each read). Any other read - a copy into a callee frame, a fold
//! capture, an aggregate element - keeps the register in scratch memory, so a
//! register is promoted only by construction and never falls back at run time.
use super::{FramePlan, TypedCallCompileError};
use rumoca_ir_solve as solve;

/// Where one register's value lives while its body runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::typed_call) enum Home {
    /// Scratch memory at the register's [`CellRange`](super::CellRange).
    Memory,
    /// The `ordinal`-th promoted local of this plan, `f64` when `real`.
    Local { ordinal: u32, real: bool },
    /// A constant folded into every read.
    Immediate { bits: i64, real: bool },
    /// Never read, and its operation cannot fault: no code defines it.
    Unused,
}

/// Closed bounds `(lowest, highest)` of an Integer value.
pub(in crate::typed_call) type Interval = (i64, i64);

/// The bounds a fold, map or comprehension binder stays within.
pub(super) fn binder_intervals(
    domain: &rumoca_core::StructuredIndexDomain,
) -> Vec<Option<Interval>> {
    domain
        .binders
        .iter()
        .map(|binder| {
            Some((
                binder.lower.min(binder.upper),
                binder.lower.max(binder.upper),
            ))
        })
        .collect()
}

/// How many wasm locals of each kind a plan (or a plan and its regions) needs.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(in crate::typed_call) struct LocalCounts {
    pub integers: u32,
    pub reals: u32,
}

impl LocalCounts {
    pub(super) fn plus(self, other: Self) -> Result<Self, TypedCallCompileError> {
        Ok(Self {
            integers: self
                .integers
                .checked_add(other.integers)
                .ok_or(TypedCallCompileError::SizeLimit)?,
            reals: self
                .reals
                .checked_add(other.reals)
                .ok_or(TypedCallCompileError::SizeLimit)?,
        })
    }

    pub(super) fn largest(self, other: Self) -> Self {
        Self {
            integers: self.integers.max(other.integers),
            reals: self.reals.max(other.reals),
        }
    }
}

/// How the operations of one body read its registers.
pub(super) struct Reads {
    /// Registers some operation reads other than as a scalar value.
    pub escapes: Vec<bool>,
    /// Registers some operation reads at all.
    pub read: Vec<bool>,
}

/// Which registers `program` reads, and which of those reads need an address.
pub(super) fn register_reads(program: &solve::TypedProgram, element_result: bool) -> Reads {
    use solve::SolveOperation as O;
    let mut escapes = vec![false; program.register_types().len()];
    let mut read = vec![false; escapes.len()];
    for operation in program.operations() {
        operation
            .operation()
            .visit_input_registers(|register| read[register.index()] = true);
    }
    let single = |register: solve::SolveRegisterId| is_single(program, register);
    for operation in program.operations() {
        match operation.operation() {
            O::Unary { destination, .. }
            | O::Binary { destination, .. }
            | O::Compare { destination, .. }
            | O::Convert { destination, .. }
            | O::Select { destination, .. }
                if single(*destination) => {}
            O::ProjectElementDynamic { aggregate, .. } | O::UpdateElement { aggregate, .. } => {
                escapes[aggregate.index()] = true;
            }
            O::Store { source, .. } if single(*source) => {}
            // The fill value is read as a scalar.
            O::Fill { .. } => {}
            O::Conditional {
                captures,
                destinations,
                ..
            } => {
                let _ = destinations;
                for capture in captures {
                    escapes[capture.index()] = true;
                }
            }
            other => other.visit_input_registers(|register| escapes[register.index()] = true),
        }
    }
    // A register returned whole through an output slot is that slot's storage.
    for slot in program.slots() {
        if slot.storage() == solve::SolveStorageClass::Output
            && !(element_result && slot.value_type().scalar_count() == 1)
            && let Some(source) = super::outputs::completed_return(program, slot.id())
        {
            escapes[source.index()] = true;
        }
    }
    Reads { escapes, read }
}

fn is_single(program: &solve::TypedProgram, register: solve::SolveRegisterId) -> bool {
    program.register_types()[register.index()].scalar_count() == 1
}

impl FramePlan {
    /// The home of `destination` when `operation` defines it, if it may be
    /// promoted: a single-cell value made by a scalar operation and read only
    /// as a scalar.
    pub(super) fn promoted_home(
        &mut self,
        program: &solve::TypedProgram,
        operation: &solve::SolveOperation,
        destination: solve::SolveRegisterId,
        interval: Option<Interval>,
    ) -> Option<super::Home> {
        use solve::SolveOperation as O;
        if !self.promotes || self.escapes[destination.index()] || !is_single(program, destination) {
            return None;
        }
        // An Integer the bounds pin to one value is that constant wherever it
        // is read.
        if let Some((low, high)) = interval
            && low == high
        {
            return Some(super::Home::Immediate {
                bits: low,
                real: false,
            });
        }
        let proven = interval.is_some();
        if !self.read[destination.index()] && cannot_fault(operation, proven) {
            return Some(super::Home::Unused);
        }
        let real = matches!(
            program.register_types()[destination.index()].element_type(),
            solve::SolveScalarType::Real { .. }
        );
        match operation {
            O::Constant { value, .. } => {
                let bits = match value.kind() {
                    solve::SolveValueKind::Real64(bits) => bits as i64,
                    solve::SolveValueKind::Integer(value) => value,
                    solve::SolveValueKind::Boolean(value) => i64::from(value),
                    solve::SolveValueKind::Real32(_) => return None,
                };
                Some(super::Home::Immediate { bits, real })
            }
            O::Unary { .. }
            | O::Binary { .. }
            | O::Compare { .. }
            | O::Convert { .. }
            | O::Select { .. }
            | O::ProjectElementDynamic { .. }
            | O::Load { .. } => {
                let counter = if real {
                    &mut self.own_locals.reals
                } else {
                    &mut self.own_locals.integers
                };
                let ordinal = *counter;
                *counter += 1;
                Some(super::Home::Local { ordinal, real })
            }
            _ => None,
        }
    }
}

impl FramePlan {
    pub(super) fn register_intervals(
        &self,
        registers: &[solve::SolveRegisterId],
    ) -> Vec<Option<Interval>> {
        registers
            .iter()
            .map(|register| self.intervals[register.index()])
            .collect()
    }

    /// The bounds of the Integer value `operation` defines, when every operand
    /// is bounded and the result cannot leave its domain or the `i64` range.
    pub(super) fn interval_of(
        &self,
        program: &solve::TypedProgram,
        operation: &solve::SolveOperation,
    ) -> Option<Interval> {
        self.interval_with(program, operation, |register| {
            self.intervals[register.index()]
        })
    }

    /// The bounds of the Integer value `operation` defines, given the bounds
    /// `operand` knows of its operands.
    pub(in crate::typed_call) fn interval_with(
        &self,
        program: &solve::TypedProgram,
        operation: &solve::SolveOperation,
        known: impl Fn(solve::SolveRegisterId) -> Option<Interval>,
    ) -> Option<Interval> {
        use solve::SolveBinaryOperator as B;
        use solve::SolveOperation as O;
        let operand = |register: &solve::SolveRegisterId| known(*register);
        let result = match operation {
            O::Constant { value, .. } => match value.kind() {
                solve::SolveValueKind::Integer(value) => (value, value),
                _ => return None,
            },
            O::Load { slot, .. } => self.slot_intervals.get(slot.index()).copied().flatten()?,
            O::Binary {
                operator, lhs, rhs, ..
            } => {
                let ((low_l, high_l), (low_r, high_r)) = (operand(lhs)?, operand(rhs)?);
                match operator {
                    B::Add => (low_l.checked_add(low_r)?, high_l.checked_add(high_r)?),
                    B::Subtract => (low_l.checked_sub(high_r)?, high_l.checked_sub(low_r)?),
                    B::Multiply => {
                        let corners = [
                            low_l.checked_mul(low_r)?,
                            low_l.checked_mul(high_r)?,
                            high_l.checked_mul(low_r)?,
                            high_l.checked_mul(high_r)?,
                        ];
                        (*corners.iter().min()?, *corners.iter().max()?)
                    }
                    // A divisor above zero: truncating division is monotone in each
                    // operand and cannot overflow.
                    B::IntegerQuotient if low_r >= 1 => {
                        let corners = [
                            low_l / low_r,
                            low_l / high_r,
                            high_l / low_r,
                            high_l / high_r,
                        ];
                        (*corners.iter().min()?, *corners.iter().max()?)
                    }
                    B::Min => (low_l.min(low_r), high_l.min(high_r)),
                    B::Max => (low_l.max(low_r), high_l.max(high_r)),
                    _ => return None,
                }
            }
            O::Select {
                if_true, if_false, ..
            } => {
                let ((low_t, high_t), (low_f, high_f)) = (operand(if_true)?, operand(if_false)?);
                (low_t.min(low_f), high_t.max(high_f))
            }
            _ => return None,
        };
        let destination = {
            let mut found = None;
            operation.visit_output_registers(|register| found = Some(register));
            found?
        };
        let solve::SolveScalarType::Integer(domain) =
            program.register_types()[destination.index()].element_type()
        else {
            return None;
        };
        (result.0 >= domain.minimum() && result.1 <= domain.maximum()).then_some(result)
    }
}

/// Whether `operation` has no effect besides defining its register, so that
/// an unread result needs no code. `proven` is set when the result's Integer
/// bounds exclude every overflow.
fn cannot_fault(operation: &solve::SolveOperation, proven: bool) -> bool {
    use solve::SolveOperation as O;
    match operation {
        O::Load { .. } | O::Constant { .. } | O::Compare { .. } | O::Select { .. } => true,
        O::Convert { operator, .. } => *operator == solve::SolveConversionOperator::IntegerToReal,
        O::Unary { .. } | O::Binary { .. } => proven,
        _ => false,
    }
}
