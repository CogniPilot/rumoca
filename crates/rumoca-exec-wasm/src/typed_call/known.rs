//! Integer and Boolean values the emitter knows while it emits straight-line
//! code: constants, the binder of an unrolled pass, scalars a region captures,
//! and the operations over them. A known value is a constant in every read, an
//! operation over known operands defines its register as a constant, and an
//! element index that is known addresses its cell statically. A value is known
//! only when the operation that computes it cannot fault, so every fault the
//! checked code would raise stays in the code of an operation that is not folded.
use super::emit::Emitter;
use super::layout::Interval;
use rumoca_ir_solve as solve;
use std::rc::Rc;

/// The cells of an Integer or Boolean aggregate, row-major, all known.
pub(super) type Cells = Rc<[i64]>;

/// What is known of the registers or input slots of a body.
#[derive(Clone, Default)]
pub(super) struct Facts {
    pub scalars: Vec<Option<i64>>,
    pub cells: Vec<Option<Cells>>,
}

impl Emitter<'_> {
    /// The value of an Integer or Boolean register that is the same on every
    /// execution of the code emitted for it.
    pub(super) fn known_integer(&self, register: solve::SolveRegisterId) -> Option<i64> {
        if self.is_real(register) {
            return None;
        }
        self.known[register.index()].or(match self.home(register) {
            super::layout::Home::Immediate { bits, real: false } => Some(bits),
            _ => None,
        })
    }

    /// What is known of the input slots of a region whose inputs are the
    /// `leading` carried values, the `captures`, then the `trailing` binders.
    pub(super) fn known_inputs(
        &self,
        leading: usize,
        captures: &[solve::SolveRegisterId],
        trailing: usize,
    ) -> Facts {
        let mut facts = Facts {
            scalars: vec![None; leading],
            cells: vec![None; leading],
        };
        facts
            .scalars
            .extend(captures.iter().map(|r| self.known_integer(*r)));
        facts
            .cells
            .extend(captures.iter().map(|r| self.known_cells[r.index()].clone()));
        facts.scalars.extend(std::iter::repeat_n(None, trailing));
        facts.cells.extend(std::iter::repeat_n(None, trailing));
        facts
    }

    /// Define `register` as the constant `value` and remember it.
    pub(super) fn define_known(&mut self, register: solve::SolveRegisterId, value: i64) {
        self.known[register.index()] = Some(value);
        if !matches!(
            self.home(register),
            super::layout::Home::Immediate { .. } | super::layout::Home::Unused
        ) {
            self.begin_result(register);
            self.push_constant(value, false);
            self.end_result(register);
        }
    }

    /// The value `operation` computes when every operand is known and the
    /// checked code would not fault, else `None`.
    pub(super) fn fold_known(&self, operation: &solve::SolveOperation) -> Option<i64> {
        use solve::SolveOperation as O;
        match operation {
            O::Unary {
                destination,
                operator,
                operand,
            } => self.fold_unary(*destination, *operator, self.known_integer(*operand)?),
            O::Binary {
                destination,
                operator,
                lhs,
                rhs,
            } => self.fold_binary(
                *destination,
                *operator,
                (self.known_integer(*lhs)?, self.known_integer(*rhs)?),
            ),
            O::Compare {
                operator, lhs, rhs, ..
            } if !self.is_real(*lhs) => Some(i64::from(solve::fold_integer_compare(
                *operator,
                self.known_integer(*lhs)?,
                self.known_integer(*rhs)?,
            ))),
            O::Select {
                destination,
                condition,
                if_true,
                if_false,
            } if self.is_scalar(*destination) && !self.is_real(*destination) => {
                let chosen = if self.known_integer(*condition)? != 0 {
                    if_true
                } else {
                    if_false
                };
                self.known_integer(*chosen)
            }
            _ => None,
        }
    }

    fn is_scalar(&self, register: solve::SolveRegisterId) -> bool {
        self.program.register_types()[register.index()].scalar_count() == 1
    }

    fn fold_unary(
        &self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveUnaryOperator,
        value: i64,
    ) -> Option<i64> {
        self.is_scalar(destination)
            .then(|| solve::fold_scalar_unary(self.scalar(destination), operator, value))?
    }

    fn fold_binary(
        &self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveBinaryOperator,
        operands: (i64, i64),
    ) -> Option<i64> {
        self.is_scalar(destination)
            .then(|| solve::fold_scalar_binary(self.scalar(destination), operator, operands))?
    }
}

impl Emitter<'_> {
    /// The cell an element of `aggregate` selects when every index is known
    /// and within its extent.
    pub(super) fn static_element(
        &self,
        aggregate: solve::SolveRegisterId,
        indices: &[solve::SolveRegisterId],
    ) -> Option<u32> {
        let shape = self.program.register_types()[aggregate.index()].dimensions();
        let mut cell: u32 = 0;
        for (index, extent) in indices.iter().zip(shape) {
            let value = u32::try_from(self.known_integer(*index)?.checked_sub(1)?).ok()?;
            if value >= *extent {
                return None;
            }
            cell = cell.checked_mul(*extent)?.checked_add(value)?;
        }
        Some(cell)
    }
}

impl Emitter<'_> {
    /// The arm of a conditional that runs when its condition is known: 0 for
    /// the true arm, 1 for the false one.
    pub(super) fn known_arm(&self, condition: solve::SolveRegisterId) -> Option<usize> {
        self.known_integer(condition)
            .map(|value| usize::from(value == 0))
    }
}

impl Emitter<'_> {
    /// The bounds the Integer `register` stays within.
    pub(super) fn interval(&self, register: solve::SolveRegisterId) -> Option<Interval> {
        match self.known_integer(register) {
            Some(value) => Some((value, value)),
            None => self.intervals[register.index()],
        }
    }

    /// Bound the Integer `operation` defines from what is known of its
    /// operands, when the plan did not.
    pub(super) fn refine_interval(&mut self, operation: &solve::SolveOperation) {
        let mut destination = None;
        operation.visit_output_registers(|register| destination = Some(register));
        let Some(destination) = destination else {
            return;
        };
        if self.intervals[destination.index()].is_some() || !self.is_scalar(destination) {
            return;
        }
        let bounds = self
            .plan
            .interval_with(self.program, operation, |register| self.interval(register));
        self.intervals[destination.index()] = bounds;
    }
}

/// Aggregates with more cells than this are not tracked.
const MAX_KNOWN_CELLS: usize = 4096;

impl Emitter<'_> {
    /// The input facts of slot `slot`: its ordinal among the input slots
    /// selects the known value the parent captured into it.
    fn input_facts(&self, slot: usize) -> (Option<i64>, Option<Cells>) {
        let ordinal = self
            .program
            .slots()
            .iter()
            .filter(|input| input.storage() == solve::SolveStorageClass::Input)
            .position(|input| input.id().index() == slot);
        match ordinal {
            Some(ordinal) => (
                self.slot_facts.scalars.get(ordinal).copied().flatten(),
                self.slot_facts.cells.get(ordinal).cloned().flatten(),
            ),
            None => (None, None),
        }
    }

    /// Record what a `Load` of `slot` is known to hold; returns the scalar
    /// value when the load folds to a constant.
    pub(super) fn known_load(
        &mut self,
        destination: solve::SolveRegisterId,
        slot: usize,
    ) -> Option<i64> {
        let (scalar, cells) = self.input_facts(slot);
        self.known_cells[destination.index()] = cells;
        scalar.filter(|_| !self.is_real(destination))
    }

    /// The cells of `register` when all are known: an aggregate with known
    /// cells, or a scalar with a known value.
    fn cells_of(&self, register: solve::SolveRegisterId) -> Option<Vec<i64>> {
        if let Some(cells) = &self.known_cells[register.index()] {
            return Some(cells.to_vec());
        }
        if self.is_scalar(register) {
            return self.known_integer(register).map(|value| vec![value]);
        }
        None
    }

    /// Track the cells an aggregate operation builds from known cells.
    pub(super) fn record_cells(&mut self, operation: &solve::SolveOperation) {
        use solve::SolveOperation as O;
        let (destination, cells) = match operation {
            O::Fill { destination, value } => {
                let count =
                    self.program.register_types()[destination.index()].scalar_count() as usize;
                let value = self
                    .known_integer(*value)
                    .filter(|_| count <= MAX_KNOWN_CELLS);
                (*destination, value.map(|value| vec![value; count]))
            }
            O::ConstructAggregate {
                destination,
                elements,
            } => (
                *destination,
                elements
                    .iter()
                    .map(|element| self.cells_of(*element))
                    .collect::<Option<Vec<_>>>()
                    .map(|parts| parts.concat()),
            ),
            O::Concatenate {
                destination,
                axis,
                operands,
            } => (
                *destination,
                self.concatenated(*destination, *axis, operands),
            ),
            _ => return,
        };
        if self.is_real(destination) {
            return;
        }
        self.known_cells[destination.index()] = cells
            .filter(|cells| cells.len() <= MAX_KNOWN_CELLS)
            .map(Rc::from);
    }

    /// The cells `Concatenate` builds, laid out as the emitted copies do.
    fn concatenated(
        &self,
        destination: solve::SolveRegisterId,
        axis: u32,
        operands: &[solve::SolveRegisterId],
    ) -> Option<Vec<i64>> {
        let types = self.program.register_types();
        let dimensions = types[destination.index()].dimensions();
        let axis = axis as usize;
        let inner = dimensions.get(axis + 1..)?.iter().product::<u32>() as usize;
        let outer = dimensions.get(..axis)?.iter().product::<u32>() as usize;
        let total = types[destination.index()].scalar_count() as usize;
        let row = total / outer.max(1);
        let mut cells = vec![0; total];
        let mut prefix = 0;
        for operand in operands {
            let extent = types[operand.index()]
                .dimensions()
                .get(axis)
                .copied()
                .unwrap_or(1);
            let width = extent as usize * inner;
            let source = self.cells_of(*operand)?;
            if source.len() != outer * width {
                return None;
            }
            for line in 0..outer {
                let from = &source[line * width..(line + 1) * width];
                cells
                    .get_mut(line * row + prefix..line * row + prefix + width)?
                    .copy_from_slice(from);
            }
            prefix += width;
        }
        (prefix == row).then_some(cells)
    }

    /// Fold a projection of a known aggregate at known indices to a
    /// constant; returns whether it did.
    pub(super) fn fold_projection(&mut self, operation: &solve::SolveOperation) -> bool {
        use solve::SolveOperation as O;
        let (destination, aggregate, cell) = match operation {
            O::ProjectElementDynamic {
                destination,
                aggregate,
                indices,
            } => (
                *destination,
                *aggregate,
                self.static_element(*aggregate, indices),
            ),
            O::ProjectElement {
                destination,
                aggregate,
                indices,
            } => {
                let shape = self.program.register_types()[aggregate.index()].dimensions();
                let cell = indices
                    .iter()
                    .zip(shape)
                    .fold(0, |cell, (index, extent)| cell * extent + index);
                (*destination, *aggregate, Some(cell))
            }
            _ => return false,
        };
        let value = cell.and_then(|cell| {
            self.known_cells[aggregate.index()]
                .as_ref()
                .and_then(|cells| cells.get(cell as usize).copied())
        });
        match value {
            Some(value) if !self.is_real(destination) => {
                self.define_known(destination, value);
                true
            }
            _ => false,
        }
    }
}
