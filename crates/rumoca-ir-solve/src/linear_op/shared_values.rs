//! Shared-value segments of an ordered assignment-program sequence
//! (SPEC_0043 §6a).
//!
//! The programs of one exact refresh schedule or causal chain run in order,
//! each storing its outputs into solver slots later programs may read.
//! Consecutive fusible programs are concatenated into one segment: each
//! program's registers are offset into one register file, a value an earlier
//! operation of the segment already computed is read from its register
//! instead of recomputed, and a load of a slot the segment already stored
//! reads the stored register. The checker evaluates the original programs and
//! the segments over one hash-consed term table and requires identical output
//! and final slot terms.

mod registers;
mod symbolic;
#[cfg(test)]
mod tests;

use std::collections::{HashMap, HashSet};

use registers::{Role, read_registers, renamable, visit_registers};
use symbolic::{SymbolicSlots, Term, Terms, ValueKey, ValueShape, value_shape};

use super::*;

/// Most registers one segment's register file may hold. A program that would
/// exceed it starts the next segment, and every value that segment recomputes
/// because the earlier one is closed is recorded.
pub const SHARED_VALUE_REGISTER_CAP: usize = 8192;

/// One program of an ordered assignment sequence: its operations and the
/// solver slot each output stores into, in output order.
#[derive(Clone, Copy, Debug)]
pub struct AssignmentProgram<'a> {
    pub ops: &'a [LinearOp],
    pub targets: &'a [usize],
}

/// One segment: a program storing into `targets` in output order.
#[derive(Clone, Debug, PartialEq)]
pub struct SharedValueSegment {
    ops: Vec<LinearOp>,
    targets: Vec<usize>,
    first_program: usize,
}

impl SharedValueSegment {
    #[must_use]
    pub fn ops(&self) -> &[LinearOp] {
        &self.ops
    }

    #[must_use]
    pub fn targets(&self) -> &[usize] {
        &self.targets
    }

    /// The sequence position of the first program the segment holds.
    #[must_use]
    pub const fn first_program(&self) -> usize {
        self.first_program
    }
}

/// A value a segment recomputes because an earlier segment holding it was
/// closed at [`SHARED_VALUE_REGISTER_CAP`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CappedValue {
    pub segment: usize,
    pub operation: &'static str,
}

/// The shared-value segments of one ordered assignment sequence.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct SharedValueSegments {
    segments: Vec<SharedValueSegment>,
    shared: usize,
    capped: Vec<CappedValue>,
}

/// Why segments are not an equivalent form of their programs.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SharedValueError {
    /// A program or segment cannot be evaluated symbolically.
    Unevaluable,
    /// The segments store a different number of outputs.
    OutputCount,
    /// Output `index`, in sequence order, is a different value.
    Output { index: usize },
    /// A solver slot ends with a different value.
    FinalSlot { slot: usize },
}

impl SharedValueSegments {
    /// The segments of `programs`, in order.
    #[must_use]
    pub fn derive(programs: &[AssignmentProgram<'_>]) -> Self {
        let mut builder = Builder::default();
        for (index, program) in programs.iter().enumerate() {
            match FusibleProgram::classify(program.ops) {
                Some(fusible) => builder.append(index, program.targets, &fusible),
                None => builder.push_opaque(index, program),
            }
        }
        builder.finish()
    }

    /// Evaluate `programs` and the segments symbolically and require every
    /// output, in order, and every final slot to be the identical term.
    pub fn check(&self, programs: &[AssignmentProgram<'_>]) -> Result<(), SharedValueError> {
        let mut terms = Terms::default();
        let mut original = SymbolicSlots::default();
        let mut expected = Vec::new();
        for program in programs {
            let outputs = original
                .run(&mut terms, program.ops, program.targets)
                .ok_or(SharedValueError::Unevaluable)?;
            expected.extend(outputs);
        }
        let mut shared = SymbolicSlots::default();
        let mut found = Vec::new();
        for segment in &self.segments {
            let outputs = shared
                .run(&mut terms, &segment.ops, &segment.targets)
                .ok_or(SharedValueError::Unevaluable)?;
            found.extend(outputs);
        }
        if expected.len() != found.len() {
            return Err(SharedValueError::OutputCount);
        }
        if let Some(index) = expected.iter().zip(&found).position(|(a, b)| a != b) {
            return Err(SharedValueError::Output { index });
        }
        let (before, after) = (original.final_slots(), shared.final_slots());
        if let Some(&slot) = before
            .keys()
            .find(|slot| before.get(slot) != after.get(slot))
        {
            return Err(SharedValueError::FinalSlot { slot });
        }
        if let Some(&slot) = after.keys().find(|slot| !before.contains_key(slot)) {
            return Err(SharedValueError::FinalSlot { slot });
        }
        Ok(())
    }

    #[must_use]
    pub fn segments(&self) -> &[SharedValueSegment] {
        &self.segments
    }

    /// Operations the segments no longer compute.
    #[must_use]
    pub const fn shared_operations(&self) -> usize {
        self.shared
    }

    /// Values recomputed because a segment closed at the register cap.
    #[must_use]
    pub fn capped(&self) -> &[CappedValue] {
        &self.capped
    }
}

/// A program a segment may absorb, classified once: every operation is a
/// load, a copy, an output store, or a renamable value, every register is
/// written once, and the program's register flow validates.
struct FusibleProgram<'a> {
    steps: Vec<Step<'a>>,
    registers: usize,
}

/// One classified operation of a fusible program.
enum Step<'a> {
    LoadSlot {
        dst: Reg,
        index: usize,
    },
    LoadSlots {
        op: &'a LinearOp,
        dst: Reg,
        first: usize,
        count: usize,
    },
    Copy {
        dst: Reg,
        src: Reg,
    },
    Store(Vec<Reg>),
    Value {
        op: &'a LinearOp,
        shape: ValueShape,
        dst: Reg,
        count: usize,
    },
}

impl<'a> FusibleProgram<'a> {
    fn classify(ops: &'a [LinearOp]) -> Option<Self> {
        let registers = ScalarProgramRegisterFlow::derive(ops)
            .ok()?
            .register_count();
        let mut written = HashSet::new();
        let mut steps = Vec::with_capacity(ops.len());
        for op in ops {
            let start = op.dst_register().map_or(0, |start| start as usize);
            let count = op.dst_register().map_or(0, |_| op.dst_register_count());
            if !(start..start + count).all(|register| written.insert(register)) {
                return None;
            }
            steps.push(Step::classify(op)?);
        }
        Some(Self { steps, registers })
    }
}

impl<'a> Step<'a> {
    fn classify(op: &'a LinearOp) -> Option<Self> {
        Some(match *op {
            LinearOp::LoadY { dst, index } => Self::LoadSlot { dst, index },
            LinearOp::Move { dst, src } => Self::Copy { dst, src },
            LinearOp::StoreOutput { src } => Self::Store(vec![src]),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => Self::Store(
                (0..count)
                    .map(|ordinal| start + (ordinal * stride) as Reg)
                    .collect(),
            ),
            _ => match y_tensor_load(op) {
                Some((dst, first, count)) => Self::LoadSlots {
                    op,
                    dst,
                    first,
                    count,
                },
                None if renamable(op) => Self::Value {
                    op,
                    shape: value_shape(op)?,
                    dst: op.dst_register()?,
                    count: op.dst_register_count(),
                },
                None => return None,
            },
        })
    }
}

/// The destination, first slot, and count of a primal solver-Y tensor load.
fn y_tensor_load(op: &LinearOp) -> Option<(Reg, usize, usize)> {
    match *op {
        LinearOp::TensorLoad {
            dst_start,
            input: TensorInputKind::Y,
            input_start,
            count,
            seed_start: None,
            lanes: 1,
        } => Some((dst_start, input_start, count)),
        _ => None,
    }
}

/// The segment under construction and the sequence's committed state.
#[derive(Default)]
struct Builder {
    terms: Terms,
    slots: SymbolicSlots,
    done: Vec<SharedValueSegment>,
    shared: usize,
    capped: Vec<CappedValue>,
    /// Values of segments closed at the cap.
    closed: HashSet<ValueKey>,
    segment: Segment,
}

/// One open segment: its operations, targets, and value table.
#[derive(Default)]
struct Segment {
    ops: Vec<LinearOp>,
    targets: Vec<usize>,
    first_program: usize,
    registers: usize,
    /// Value key to the registers holding its outputs.
    values: HashMap<ValueKey, Vec<Reg>>,
    /// Term of every register the segment wrote.
    terms: HashMap<Reg, Term>,
    /// Slots stored so far and the register holding each.
    stored: HashMap<usize, Reg>,
}

impl Builder {
    /// Append a fusible program to the open segment, first closing it when
    /// the program's registers would exceed the cap. An open segment holds at
    /// most the cap, so the program's offset is a register.
    fn append(&mut self, index: usize, targets: &[usize], program: &FusibleProgram<'_>) {
        let fits = self.segment.registers + program.registers <= SHARED_VALUE_REGISTER_CAP;
        let base = match Reg::try_from(self.segment.registers) {
            Ok(base) if fits || self.segment.ops.is_empty() => base,
            _ => {
                self.close(true);
                0
            }
        };
        if self.segment.ops.is_empty() {
            self.segment.first_program = index;
        }
        self.segment.registers += program.registers;
        let mut program_builder = ProgramBuilder {
            builder: self,
            base,
            map: HashMap::new(),
            output: 0,
            targets,
        };
        for step in &program.steps {
            program_builder.step(step);
        }
        self.segment.targets.extend_from_slice(targets);
    }

    fn push_opaque(&mut self, index: usize, program: &AssignmentProgram<'_>) {
        self.close(false);
        let _ = self
            .slots
            .run(&mut self.terms, program.ops, program.targets);
        self.done.push(SharedValueSegment {
            ops: program.ops.to_vec(),
            targets: program.targets.to_vec(),
            first_program: index,
        });
    }

    /// Close the open segment: drop dead operations and commit its stores.
    fn close(&mut self, at_cap: bool) {
        let segment = std::mem::take(&mut self.segment);
        if segment.ops.is_empty() {
            return;
        }
        let stored = segment
            .targets
            .iter()
            .map(|target| segment.terms[&segment.stored[target]])
            .collect::<Vec<_>>();
        self.slots.commit(&segment.targets, &stored);
        if at_cap {
            self.closed.extend(segment.values.into_keys());
        }
        self.done.push(SharedValueSegment {
            ops: without_dead_operations(segment.ops),
            targets: segment.targets,
            first_program: segment.first_program,
        });
    }

    fn finish(mut self) -> SharedValueSegments {
        self.close(false);
        SharedValueSegments {
            segments: self.done,
            shared: self.shared,
            capped: self.capped,
        }
    }
}

/// One program being appended to the open segment.
struct ProgramBuilder<'a, 'p> {
    builder: &'a mut Builder,
    base: Reg,
    /// Program register to the segment register holding its value.
    map: HashMap<Reg, Reg>,
    output: usize,
    targets: &'p [usize],
}

impl ProgramBuilder<'_, '_> {
    fn step(&mut self, step: &Step<'_>) {
        match step {
            &Step::LoadSlot { dst, index } => self.load_slot(dst, index),
            &Step::LoadSlots {
                op,
                dst,
                first,
                count,
            } => self.load_slots(op, dst, first, count),
            // A copy names the value it copies; a range reading it gathers
            // the value into place.
            &Step::Copy { dst, src } => {
                let held = self.map[&src];
                self.map.insert(dst, held);
            }
            Step::Store(registers) => {
                for register in registers {
                    self.store(self.map[register]);
                }
            }
            &Step::Value {
                op,
                ref shape,
                dst,
                count,
            } => self.value(op, shape, dst, count),
        }
    }

    fn segment(&mut self) -> &mut Segment {
        &mut self.builder.segment
    }

    fn store(&mut self, register: Reg) {
        let target = self.targets[self.output];
        self.output += 1;
        let segment = self.segment();
        segment.ops.push(LinearOp::StoreOutput { src: register });
        segment.stored.insert(target, register);
    }

    /// Emit `op` with its destination at this program's offset, recording the
    /// term of every register it writes.
    fn emit(&mut self, op: LinearOp, key: &ValueKey, dst: Reg, count: usize) {
        let first = self.base + dst;
        let registers = (0..count)
            .map(|offset| first + offset as Reg)
            .collect::<Vec<_>>();
        for (offset, &register) in registers.iter().enumerate() {
            let term = self.builder.terms.value(key, offset);
            self.segment().terms.insert(register, term);
            self.map.insert(dst + offset as Reg, register);
        }
        self.segment().ops.push(op);
        self.segment().values.insert(key.clone(), registers);
    }

    /// Read `dst` from `registers` computed earlier: nothing is emitted.
    fn reuse(&mut self, dst: Reg, registers: &[Reg]) {
        for (offset, &register) in registers.iter().enumerate() {
            self.map.insert(dst + offset as Reg, register);
        }
        self.builder.shared += 1;
    }

    fn term(&self, register: Reg) -> Term {
        self.builder.segment.terms[&self.map[&register]]
    }

    fn load_slot(&mut self, dst: Reg, index: usize) {
        if let Some(&register) = self.builder.segment.stored.get(&index) {
            self.map.insert(dst, register);
            return;
        }
        let term = self.builder.slots.slot(&mut self.builder.terms, index);
        let key = self.builder.terms.key("Slot", vec![(0, term)]);
        if let Some(registers) = self.builder.segment.values.get(&key).cloned() {
            return self.reuse(dst, &registers);
        }
        let op = LinearOp::LoadY {
            dst: self.base + dst,
            index,
        };
        let register = self.base + dst;
        self.segment().terms.insert(register, term);
        self.map.insert(dst, register);
        self.segment().ops.push(op);
        self.segment().values.insert(key, vec![register]);
    }

    fn load_slots(&mut self, op: &LinearOp, dst: Reg, first: usize, count: usize) {
        let terms = (0..count)
            .map(
                |offset| match self.builder.segment.stored.get(&(first + offset)) {
                    Some(&register) => self.builder.segment.terms[&register],
                    None => self
                        .builder
                        .slots
                        .slot(&mut self.builder.terms, first + offset),
                },
            )
            .collect::<Vec<_>>();
        let key = self
            .builder
            .terms
            .key("Slots", terms.iter().copied().enumerate().collect());
        if let Some(registers) = self.builder.segment.values.get(&key).cloned() {
            return self.reuse(dst, &registers);
        }
        let mut load = op.clone();
        if let LinearOp::TensorLoad { dst_start, .. } = &mut load {
            *dst_start = self.base + dst;
        }
        self.segment().ops.push(load);
        let mut registers = Vec::with_capacity(count);
        for (offset, &term) in terms.iter().enumerate() {
            let register = self.base + dst + offset as Reg;
            if let Some(&stored) = self.builder.segment.stored.get(&(first + offset)) {
                self.segment().ops.push(LinearOp::Move {
                    dst: register,
                    src: stored,
                });
            }
            self.segment().terms.insert(register, term);
            self.map.insert(dst + offset as Reg, register);
            registers.push(register);
        }
        self.segment().values.insert(key, registers);
    }

    fn value(&mut self, op: &LinearOp, shape: &ValueShape, dst: Reg, count: usize) {
        let operands = shape
            .operands
            .iter()
            .map(|&(field, register)| (field, self.term(register)))
            .collect();
        let key = self.builder.terms.key(&shape.shape, operands);
        // A call may reach an external body, so it runs at every occurrence.
        let shareable = !matches!(op, LinearOp::PureCall { .. });
        if shareable && let Some(registers) = self.builder.segment.values.get(&key).cloned() {
            return self.reuse(dst, &registers);
        }
        if self.builder.closed.contains(&key) {
            let segment = self.builder.done.len();
            self.builder.capped.push(CappedValue {
                segment,
                operation: op.kind_name(),
            });
        }
        let renamed = self.renamed(op, shape);
        self.emit(renamed, &key, dst, count);
    }

    /// `op` in the segment's register file: scalar operands read the register
    /// holding their value; a range operand is first gathered at this
    /// program's offset, so every range keeps its layout.
    fn renamed(&mut self, op: &LinearOp, shape: &ValueShape) -> LinearOp {
        if shape.ranged {
            for &register in &shape.reads {
                self.gather(register);
            }
        }
        let (base, map, ranged) = (self.base, &self.map, shape.ranged);
        let mut renamed = op.clone();
        visit_registers(&mut renamed, &mut |role, register| {
            *register = match role {
                Role::Scalar if !ranged => map[&*register],
                _ => base + *register,
            };
        });
        renamed
    }

    /// Place the value of `register` at this program's offset.
    fn gather(&mut self, register: Reg) {
        let (own, held) = (self.base + register, self.map[&register]);
        if own == held {
            return;
        }
        let term = self.builder.segment.terms[&held];
        self.segment().ops.push(LinearOp::Move {
            dst: own,
            src: held,
        });
        self.segment().terms.insert(own, term);
        self.map.insert(register, own);
    }
}

/// `ops` without every operation whose written registers no later operation
/// reads; output stores and operations writing nothing are kept.
fn without_dead_operations(ops: Vec<LinearOp>) -> Vec<LinearOp> {
    let mut live: HashSet<Reg> = HashSet::new();
    let mut keep = vec![false; ops.len()];
    for (index, op) in ops.iter().enumerate().rev() {
        let written = match op.dst_register() {
            Some(start) => (0..op.dst_register_count())
                .map(|offset| start + offset as Reg)
                .collect::<Vec<_>>(),
            None => Vec::new(),
        };
        let needed = written.is_empty() || written.iter().any(|register| live.contains(register));
        for register in &written {
            live.remove(register);
        }
        if !needed {
            continue;
        }
        keep[index] = true;
        live.extend(read_registers(op).unwrap_or_default());
    }
    ops.into_iter()
        .zip(keep)
        .filter_map(|(op, keep)| keep.then_some(op))
        .collect()
}
