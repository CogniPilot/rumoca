//! Independent passes of every loop of one checked typed program (SPEC_0040
//! SOLVE-C104).
//!
//! A loop has independent passes when its result does not depend on the order
//! in which its passes run. Each pass of a Map reads its captures and its
//! binders and writes its own element, so every Map qualifies. A Fold carries
//! a tuple from pass to pass, so it qualifies only when every carried value
//! passes through each pass unchanged except for arrays whose elements each
//! pass overwrites at an index that is a function of the pass's own binders
//! (the index tuple determines the binder tuple, so no two passes write the
//! same element), and no pass reads a carried element other than the one it
//! writes. The analysis walks the transition once, tracking for each register
//! whether it is a carried value, a carried array with the pass's element
//! written, an Integer whose form over the binders is known, or anything else.
//! A read or write it cannot place disqualifies the loop; nothing is assumed.
//!
//! The exactness of the operations of one pass comes from the fault flow, which
//! analyses the body under the intervals of its binders.

use super::fault_flow::FaultFlow;
use super::{
    IndependentPasses, SolveIndexForm, SolveOperation, SolveProgramRegion, SolveRegisterId,
    SolveScalarType, SolveSlotId, SolveSpannedOperation, SolveStorageClass, SolveValueType,
    TypedProgram, invariant_guard,
};
use crate::SolveValueKind;
use rumoca_core::StructuredIndexDomain;

/// The independent-passes property of each operation of one program.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(super) struct PassFlow {
    passes: Box<[Option<IndependentPasses>]>,
}

impl PassFlow {
    pub(super) fn of(operations: &[SolveSpannedOperation], faults: &FaultFlow) -> Self {
        let passes = operations
            .iter()
            .enumerate()
            .map(|(index, operation)| {
                let exactness = faults.pass_lanes(index)?;
                independent(operation.operation()).then(|| IndependentPasses::new(exactness))
            })
            .collect();
        Self { passes }
    }

    pub(super) fn passes(&self, operation: usize) -> Option<IndependentPasses> {
        self.passes.get(operation).copied().flatten()
    }
}

/// Whether the passes of loop `operation` are independent.
fn independent(operation: &SolveOperation) -> bool {
    match operation {
        SolveOperation::Map { .. } => true,
        SolveOperation::Fold {
            domain,
            initial,
            captures,
            state_captures,
            transition,
            continuation,
            ..
        } => {
            state_captures.is_empty()
                && (continuation.is_none() || invariant_guard(operation).is_some())
                && fold_is_independent(domain, (initial.len(), captures.len()), transition)
        }
        _ => false,
    }
}

/// What a register holds, as far as the independence of passes is concerned.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Value {
    /// An Integer scalar with a known form over the binders.
    Index(SolveIndexForm),
    /// The carried value of the given ordinal, as the pass received it.
    Carried(usize),
    /// The carried array of the given ordinal with the pass's own element
    /// overwritten.
    Written(usize),
    /// Any value that is not a carried array or an Integer form.
    Plain,
}

type Key = Box<[SolveIndexForm]>;

/// The element accesses of carried arrays seen while walking one transition.
#[derive(Default)]
struct Accesses {
    writes: Vec<(usize, Key)>,
    reads: Vec<(usize, Key)>,
    /// Carried values used other than by element access or update.
    whole_reads: Vec<usize>,
}

/// The binding of one region body being walked: the value of each input and
/// the slot roles.
struct Frame<'a> {
    program: &'a TypedProgram,
    inputs: &'a [Value],
}

fn fold_is_independent(
    domain: &StructuredIndexDomain,
    (carried, captures): (usize, usize),
    transition: &SolveProgramRegion,
) -> bool {
    let mut inputs = Vec::new();
    inputs.extend((0..carried).map(Value::Carried));
    inputs.extend(
        transition.inputs()[carried..carried + captures]
            .iter()
            .map(|value_type| {
                if is_integer_scalar(value_type) {
                    Value::Index(SolveIndexForm::Invariant(None))
                } else {
                    Value::Plain
                }
            }),
    );
    inputs.extend(
        (0..domain.binders.len()).map(|ordinal| Value::Index(SolveIndexForm::binder(ordinal))),
    );
    let mut accesses = Accesses::default();
    let Some(outputs) = accesses.run(&Frame {
        program: transition.body(),
        inputs: &inputs,
    }) else {
        return false;
    };
    outputs.len() == carried
        && outputs.iter().enumerate().all(|(ordinal, output)| {
            matches!(output, Value::Carried(k) | Value::Written(k) if *k == ordinal)
        })
        && (0..carried).all(|ordinal| accesses.disjoint(ordinal, domain.binders.len()))
}

fn is_integer_scalar(value_type: &SolveValueType) -> bool {
    value_type.dimensions().is_empty()
        && matches!(value_type.element_type(), SolveScalarType::Integer(_))
}

impl Accesses {
    /// Whether the passes of a loop over `binders` binders write carried array
    /// `ordinal` at pairwise distinct elements and read no element another
    /// pass writes.
    fn disjoint(&self, ordinal: usize, binders: usize) -> bool {
        let mut writes = self
            .writes
            .iter()
            .filter(|(carried, _)| *carried == ordinal)
            .map(|(_, key)| key);
        let Some(key) = writes.next() else {
            return true;
        };
        writes.all(|other| other == key)
            && !self.whole_reads.contains(&ordinal)
            && self
                .reads
                .iter()
                .filter(|(carried, _)| *carried == ordinal)
                .all(|(_, read)| read == key)
            && determines_binders(key, binders)
    }

    /// Walk `frame`'s program, returning the value stored to each of its
    /// output slots, or `None` when an operation uses a carried value in a way
    /// that cannot be placed.
    fn run(&mut self, frame: &Frame) -> Option<Vec<Value>> {
        let program = frame.program;
        let mut values = vec![Value::Plain; program.register_types().len()];
        let mut outputs = vec![Value::Plain; count_slots(program, SolveStorageClass::Output)];
        for operation in program.operations() {
            self.step(frame, operation.operation(), (&mut values, &mut outputs))?;
        }
        Some(outputs)
    }

    fn step(
        &mut self,
        frame: &Frame,
        operation: &SolveOperation,
        (values, outputs): (&mut [Value], &mut [Value]),
    ) -> Option<()> {
        use SolveOperation as O;
        match operation {
            O::Load { destination, slot } => {
                values[destination.index()] = match role(frame.program, *slot) {
                    Some((SolveStorageClass::Input, ordinal)) => frame.inputs[ordinal],
                    _ => plain_result(frame.program, *destination, false),
                };
            }
            O::Store { slot, source } => {
                let (SolveStorageClass::Output, ordinal) = role(frame.program, *slot)? else {
                    return None;
                };
                outputs[ordinal] = values[source.index()];
            }
            O::Select {
                destination,
                condition,
                if_true,
                if_false,
            } => {
                self.consume(&[*condition], values)?;
                values[destination.index()] =
                    merge(values[if_true.index()], values[if_false.index()])?;
            }
            O::Conditional {
                condition,
                captures,
                destinations,
                if_true,
                if_false,
            } => {
                self.consume(&[*condition], values)?;
                let entered = captures
                    .iter()
                    .map(|capture| values[capture.index()])
                    .collect::<Vec<_>>();
                let arm = |this: &mut Self, region: &SolveProgramRegion| {
                    this.run(&Frame {
                        program: region.body(),
                        inputs: &entered,
                    })
                };
                let (taken, other) = (arm(self, if_true)?, arm(self, if_false)?);
                for ((destination, first), second) in destinations.iter().zip(taken).zip(other) {
                    values[destination.index()] = merge(first, second)?;
                }
            }
            O::UpdateElement {
                destination,
                aggregate,
                value,
                indices,
            } => {
                self.consume(&[*value], values)?;
                values[destination.index()] = match values[aggregate.index()] {
                    Value::Carried(ordinal) | Value::Written(ordinal) => {
                        self.writes.push((ordinal, key_of(indices, values)));
                        Value::Written(ordinal)
                    }
                    _ => Value::Plain,
                };
            }
            O::ProjectElementDynamic {
                destination,
                aggregate,
                indices,
            } => {
                self.read(values[aggregate.index()], key_of(indices, values));
                values[destination.index()] = plain_result(frame.program, *destination, false);
            }
            O::SelectElement {
                destination,
                aggregate,
                indices,
                out_of_range,
            } => {
                self.consume(&[*out_of_range], values)?;
                self.read(values[aggregate.index()], key_of(indices, values));
                values[destination.index()] = plain_result(frame.program, *destination, false);
            }
            O::ProjectElement {
                destination,
                aggregate,
                indices,
            } => {
                let key = indices
                    .iter()
                    .map(|index| SolveIndexForm::Invariant(Some(i64::from(*index) + 1)))
                    .collect();
                self.read(values[aggregate.index()], key);
                values[destination.index()] = plain_result(frame.program, *destination, false);
            }
            O::AssignedConditional { .. } | O::AssignedState { .. } => return None,
            _ => self.generic(frame.program, operation, values)?,
        }
        Some(())
    }

    /// An operation this analysis does not place: it may use a carried value
    /// that no pass writes, and nothing else; its results are not carried.
    fn generic(
        &mut self,
        program: &TypedProgram,
        operation: &SolveOperation,
        values: &mut [Value],
    ) -> Option<()> {
        let mut operands = Vec::new();
        operation.visit_input_registers(|register| operands.push(register));
        self.consume(&operands, values)?;
        if let Some((destination, form)) = integer_form(program, operation, &|register| {
            form_of(values[register.index()])
        }) {
            values[destination.index()] = Value::Index(form);
            return Some(());
        }
        let invariant = operands.iter().all(|register| {
            matches!(
                values[register.index()],
                Value::Index(SolveIndexForm::Invariant(_))
            )
        });
        operation.visit_output_registers(|register| {
            values[register.index()] = plain_result(program, register, invariant);
        });
        Some(())
    }

    /// Use `registers` as whole values: a carried value used this way must be
    /// one no pass writes, which `disjoint` checks; a written one cannot be.
    fn consume(&mut self, registers: &[SolveRegisterId], values: &[Value]) -> Option<()> {
        for register in registers {
            match values[register.index()] {
                Value::Carried(ordinal) => self.whole_reads.push(ordinal),
                Value::Written(_) => return None,
                Value::Index(_) | Value::Plain => {}
            }
        }
        Some(())
    }

    fn read(&mut self, aggregate: Value, key: Key) {
        if let Value::Carried(ordinal) | Value::Written(ordinal) = aggregate {
            self.reads.push((ordinal, key));
        }
    }
}

/// The value of an Integer result that no rule places: invariant when every
/// operand was, else unknown; any other result is plain.
fn plain_result(program: &TypedProgram, register: SolveRegisterId, invariant: bool) -> Value {
    let value_type = &program.register_types()[register.index()];
    if is_integer_scalar(value_type) {
        Value::Index(if invariant {
            SolveIndexForm::Invariant(None)
        } else {
            SolveIndexForm::Varying
        })
    } else {
        Value::Plain
    }
}

fn form_of(value: Value) -> SolveIndexForm {
    match value {
        Value::Index(form) => form,
        _ => SolveIndexForm::Varying,
    }
}

fn key_of(indices: &[SolveRegisterId], values: &[Value]) -> Key {
    indices
        .iter()
        .map(|index| form_of(values[index.index()]))
        .collect()
}

/// The value that is `first` on some paths and `second` on the others.
fn merge(first: Value, second: Value) -> Option<Value> {
    Some(match (first, second) {
        (Value::Index(a), Value::Index(b)) => Value::Index(SolveIndexForm::merge(a, b)),
        (Value::Plain, Value::Plain) => Value::Plain,
        (Value::Carried(a) | Value::Written(a), Value::Carried(b) | Value::Written(b))
            if a == b =>
        {
            if first == second {
                first
            } else {
                Value::Written(a)
            }
        }
        _ => return None,
    })
}

/// Whether an index tuple of forms determines the binder tuple: every binder
/// of the loop appears, each in one index, and every other index is a constant.
fn determines_binders(key: &[SolveIndexForm], binders: usize) -> bool {
    let mut seen = vec![false; binders];
    for form in key {
        match form {
            SolveIndexForm::Invariant(Some(_)) => {}
            SolveIndexForm::Binder { ordinal, .. } => {
                if std::mem::replace(&mut seen[*ordinal], true) {
                    return false;
                }
            }
            _ => return false,
        }
    }
    seen.iter().all(|seen| *seen)
}

/// The storage class and ordinal among its class of slot `slot`, for the
/// Input and Output slots a region interface declares.
fn role(program: &TypedProgram, slot: SolveSlotId) -> Option<(SolveStorageClass, usize)> {
    let storage = program.slots().get(slot.index())?.storage();
    matches!(
        storage,
        SolveStorageClass::Input | SolveStorageClass::Output
    )
    .then(|| {
        let ordinal = program.slots()[..slot.index()]
            .iter()
            .filter(|earlier| earlier.storage() == storage)
            .count();
        (storage, ordinal)
    })
}

fn count_slots(program: &TypedProgram, storage: SolveStorageClass) -> usize {
    program
        .slots()
        .iter()
        .filter(|slot| slot.storage() == storage)
        .count()
}

/// The form of the Integer scalar `operation` computes, when it is a constant,
/// a negation or one of the arithmetic operators over Integer scalars.
fn integer_form(
    program: &TypedProgram,
    operation: &SolveOperation,
    form: &impl Fn(SolveRegisterId) -> SolveIndexForm,
) -> Option<(SolveRegisterId, SolveIndexForm)> {
    use super::SolveUnaryOperator as U;
    let integer =
        |register: SolveRegisterId| is_integer_scalar(&program.register_types()[register.index()]);
    match operation {
        SolveOperation::Constant { destination, value } => {
            let SolveValueKind::Integer(value) = value.kind() else {
                return None;
            };
            Some((*destination, SolveIndexForm::Invariant(Some(value))))
        }
        SolveOperation::Unary {
            destination,
            operator: U::Negate,
            operand,
        } if integer(*destination) => Some((*destination, form(*operand).negate())),
        SolveOperation::Binary {
            destination,
            operator,
            lhs,
            rhs,
        } if integer(*destination) => Some((
            *destination,
            SolveIndexForm::binary(*operator, form(*lhs), form(*rhs)),
        )),
        _ => None,
    }
}

/// The form of every register of `program` over the passes of a loop, given
/// the forms of its input slots in order. Registers no rule places are
/// `Varying` unless every operand was invariant.
#[must_use]
pub fn index_forms(program: &TypedProgram, inputs: &[SolveIndexForm]) -> Box<[SolveIndexForm]> {
    let mut forms = vec![SolveIndexForm::Varying; program.register_types().len()];
    for operation in program.operations() {
        let operation = operation.operation();
        if let SolveOperation::Load { destination, slot } = operation {
            if let Some((SolveStorageClass::Input, ordinal)) = role(program, *slot) {
                forms[destination.index()] = inputs[ordinal];
            }
            continue;
        }
        if let Some((destination, form)) =
            integer_form(program, operation, &|register| forms[register.index()])
        {
            forms[destination.index()] = form;
            continue;
        }
        let mut invariant = true;
        operation.visit_input_registers(|register| {
            invariant &= matches!(forms[register.index()], SolveIndexForm::Invariant(_));
        });
        if invariant {
            operation.visit_output_registers(|register| {
                forms[register.index()] = SolveIndexForm::Invariant(None);
            });
        }
    }
    forms.into_boxed_slice()
}
