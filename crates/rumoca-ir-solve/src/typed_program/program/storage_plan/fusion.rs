//! Elementwise fusion facts of one program body.
//!
//! An elementwise `Real` operation whose result one later elementwise
//! operation of the same extent reads once, or a reduction reads in order,
//! need not be stored: the reader evaluates it per element inside its own
//! loop. Every element still sees the same operators on the same operands in
//! the same order, so the result is bit-identical, and these operators cannot
//! fault, so no failure is reordered. Deferring an operation moves its reads
//! of its operands to the operation that finally evaluates the chain, so
//! those operands' last reads are extended there.
//!
//! These rules were applied by the in-process compiled target on its own;
//! the plan now issues them. A target that ignores them stores every
//! register, as before. They are ported unchanged from that target's
//! `emit/typed_program/fusion.rs` (sha256 `e4ee1332…`), which an
//! architecture test pins so a change there re-reviews this port.

use serde::Serialize;

use super::super::{
    SolveBinaryOperator, SolveConversionOperator, SolveOperation, SolveProgramRegion,
    SolveRegisterId, SolveSpannedOperation, SolveUnaryOperator, SolveValueType,
};
use crate::SolveRealFormat;
use crate::typed_program::types::SolveScalarType;

/// Matrix-product work (rows × inner × columns) above which a binary64
/// product runs in the host kernel instead of being expanded statically.
/// It mirrors the in-process target's `MAX_STATIC_MATRIX_WORK`; the
/// architecture guard keeps the two equal until a target declares it.
pub const SOLVE_STATIC_MATRIX_WORK: u64 = 64;

/// A register that is not stored: operation `definition` is evaluated per
/// element inside the loop of operation `point`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SolveFusedRegister {
    pub definition: usize,
    pub point: usize,
}

/// The fused registers of one body and the last reads that result.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveFusionPlan {
    fused: Box<[Option<SolveFusedRegister>]>,
    last_reads: Box<[Option<usize>]>,
}

impl SolveFusionPlan {
    pub(super) fn of(
        register_types: &[SolveValueType],
        operations: &[SolveSpannedOperation],
        last_reads: &[Option<usize>],
    ) -> Self {
        let flow = Flow::of(register_types.len(), operations);
        let shapes = operations
            .iter()
            .map(|issued| elementwise(register_types, operations, &issued.operation))
            .collect::<Vec<_>>();
        let mut consumer = single_use_consumers(register_types, operations, &shapes, &flow);
        let mut materialized = materialization(&shapes, &consumer);
        recompute_cheap_producers(
            operations,
            &shapes,
            &flow,
            (&mut consumer, &mut materialized),
        );
        // Recomputation moves producers into later loops, and every producer
        // fused into a moved one moves with it. Only the final consumer edges
        // say where each value is read, so materialization is rebuilt from
        // them before any operand's life is extended.
        let materialized = materialization(&shapes, &consumer);
        let mut last_reads = last_reads.to_vec();
        last_reads.resize(register_types.len(), None);
        let mut fused = vec![None; register_types.len()];
        fuse_product_transposes(
            register_types,
            operations,
            &flow,
            &mut last_reads,
            &mut fused,
        );
        for (index, shape) in shapes.iter().enumerate() {
            let Some(shape) = shape else {
                continue;
            };
            if consumer[shape.destination.index()].is_none() {
                continue;
            }
            extend_operands(
                &operations[index].operation,
                materialized[index],
                &mut last_reads,
            );
            fused[shape.destination.index()] = Some(SolveFusedRegister {
                definition: index,
                point: materialized[index],
            });
        }
        Self {
            fused: fused.into_boxed_slice(),
            last_reads: last_reads.into_boxed_slice(),
        }
    }

    /// The fused placement of each register, `None` for a stored one.
    #[must_use]
    pub fn registers(&self) -> &[Option<SolveFusedRegister>] {
        &self.fused
    }

    /// The last read of each register once fused operands live until the
    /// operation that evaluates them.
    #[must_use]
    pub fn last_reads(&self) -> &[Option<usize>] {
        &self.last_reads
    }
}

/// The per-element operands of a fusible operation and its result.
struct Elementwise {
    destination: SolveRegisterId,
    operands: Vec<SolveRegisterId>,
}

fn scalar_count(register_types: &[SolveValueType], register: SolveRegisterId) -> Option<u32> {
    register_types
        .get(register.index())
        .map(SolveValueType::scalar_count)
}

fn is_real(register_types: &[SolveValueType], register: SolveRegisterId) -> bool {
    register_types
        .get(register.index())
        .is_some_and(|value_type| matches!(value_type.element_type(), SolveScalarType::Real { .. }))
}

fn elementwise(
    register_types: &[SolveValueType],
    operations: &[SolveSpannedOperation],
    operation: &SolveOperation,
) -> Option<Elementwise> {
    use SolveOperation as Op;
    let (destination, operands, real_operands) = match operation {
        Op::Unary {
            destination,
            operand,
            ..
        } => (*destination, vec![*operand], vec![*operand]),
        Op::Binary {
            destination,
            lhs,
            rhs,
            ..
        } => (*destination, vec![*lhs, *rhs], vec![*destination]),
        Op::Compare {
            destination,
            lhs,
            rhs,
            ..
        } => (*destination, vec![*lhs, *rhs], vec![*lhs, *rhs]),
        Op::Select {
            destination,
            if_true,
            if_false,
            ..
        } => (*destination, vec![*if_true, *if_false], vec![*destination]),
        Op::Convert {
            destination,
            operator: SolveConversionOperator::IntegerToReal,
            operand,
        } => (*destination, vec![*operand], vec![*destination]),
        // `sign` yields -1, 0 or 1 (0 for NaN), whose conversion cannot fault.
        Op::Convert {
            destination,
            operator:
                SolveConversionOperator::RealToIntegerTowardZero
                | SolveConversionOperator::RealToIntegerTowardNegativeInfinity,
            operand,
        } if sign_definition(operations, *operand) => {
            (*destination, vec![*operand], vec![*operand])
        }
        Op::Scale {
            destination,
            aggregate,
            ..
        }
        | Op::BroadcastBinary {
            destination,
            aggregate,
            ..
        } => (*destination, vec![*aggregate], vec![*destination]),
        Op::Map {
            destination,
            captures,
            body,
            ..
        } if broadcast_capture(captures, body).is_some() => {
            (*destination, vec![], vec![*destination])
        }
        _ => return None,
    };
    let count = scalar_count(register_types, destination)?;
    // A single element gains nothing from fusion.
    if count < 2 {
        return None;
    }
    if !real_operands
        .iter()
        .all(|register| is_real(register_types, *register))
    {
        return None;
    }
    for operand in &operands {
        if scalar_count(register_types, *operand)? != count {
            return None;
        }
    }
    Some(Elementwise {
        destination,
        operands,
    })
}

/// Who reads and defines each register.
struct Flow {
    reads: Vec<u32>,
    definitions: Vec<Option<usize>>,
    readers: Vec<Vec<usize>>,
}

impl Flow {
    fn of(registers: usize, operations: &[SolveSpannedOperation]) -> Self {
        let mut flow = Self {
            reads: vec![0; registers],
            definitions: vec![None; registers],
            readers: vec![Vec::new(); registers],
        };
        for (index, issued) in operations.iter().enumerate() {
            issued
                .operation
                .visit_input_registers(|register| flow.read(register, index));
            issued
                .operation
                .visit_output_registers(|register| flow.define(register, index));
        }
        flow
    }

    fn read(&mut self, register: SolveRegisterId, operation: usize) {
        if let Some(reads) = self.reads.get_mut(register.index()) {
            *reads += 1;
            self.readers[register.index()].push(operation);
        }
    }

    fn define(&mut self, register: SolveRegisterId, operation: usize) {
        if let Some(definition) = self.definitions.get_mut(register.index()) {
            *definition = Some(operation);
        }
    }
}

/// For each register, the elementwise operation (or reduction) that is its
/// only reader and reads it at its own extent.
fn single_use_consumers(
    register_types: &[SolveValueType],
    operations: &[SolveSpannedOperation],
    shapes: &[Option<Elementwise>],
    flow: &Flow,
) -> Vec<Option<usize>> {
    let mut consumer = vec![None::<usize>; flow.reads.len()];
    for (index, shape) in shapes.iter().enumerate() {
        // A reduction reads its operand element by element, in order.
        let (operands, count) = match (shape, &operations[index].operation) {
            (Some(shape), _) => (
                shape.operands.clone(),
                scalar_count(register_types, shape.destination),
            ),
            (None, SolveOperation::Reduce { operand, .. }) => {
                (vec![*operand], scalar_count(register_types, *operand))
            }
            _ => continue,
        };
        for operand in operands {
            let produced = flow.definitions[operand.index()].is_some_and(|definition| {
                shapes[definition]
                    .as_ref()
                    .is_some_and(|producer| producer.destination == operand)
            });
            if produced
                && flow.reads[operand.index()] == 1
                && scalar_count(register_types, operand) == count
            {
                consumer[operand.index()] = Some(index);
            }
        }
    }
    consumer
}

/// Where each operation's value is finally evaluated: its own index, or that
/// of the operation it is fused into, transitively. Every consumer edge
/// points to a later operation, so one reverse pass sees each consumer's
/// final point before its producers.
fn materialization(shapes: &[Option<Elementwise>], consumer: &[Option<usize>]) -> Vec<usize> {
    let mut materialized = (0..shapes.len()).collect::<Vec<_>>();
    for index in (0..shapes.len()).rev() {
        if let Some(shape) = &shapes[index]
            && let Some(reader) = consumer[shape.destination.index()]
        {
            materialized[index] = materialized[reader];
        }
    }
    materialized
}

/// A cheap producer read several times, every read inside the one loop that
/// evaluates them all, is recomputed per read instead of stored.
fn recompute_cheap_producers(
    operations: &[SolveSpannedOperation],
    shapes: &[Option<Elementwise>],
    flow: &Flow,
    (consumer, materialized): (&mut [Option<usize>], &mut [usize]),
) {
    for (index, shape) in shapes.iter().enumerate() {
        let Some(shape) = shape else {
            continue;
        };
        let cheap = matches!(
            operations[index].operation,
            SolveOperation::Binary {
                operator: SolveBinaryOperator::Add
                    | SolveBinaryOperator::Subtract
                    | SolveBinaryOperator::Multiply,
                ..
            } | SolveOperation::Scale { .. }
        );
        let destination = shape.destination.index();
        if !cheap || flow.reads[destination] < 2 || consumer[destination].is_some() {
            continue;
        }
        let readers = &flow.readers[destination];
        let point = materialized[readers[0]];
        // Every reader is elementwise, later, and either `point` itself or
        // fused into it.
        let inside = |reader: &usize| {
            shapes[*reader].as_ref().is_some_and(|shape| {
                materialized[*reader] == point
                    && *reader > index
                    && (*reader == point || consumer[shape.destination.index()].is_some())
            })
        };
        if readers.iter().all(inside) {
            consumer[destination] = Some(point);
            materialized[index] = point;
        }
    }
}

/// A transpose read once, by a binary64 product the host kernel forms, is
/// read in place by that product instead of stored.
fn fuse_product_transposes(
    register_types: &[SolveValueType],
    operations: &[SolveSpannedOperation],
    flow: &Flow,
    last_reads: &mut [Option<usize>],
    fused: &mut [Option<SolveFusedRegister>],
) {
    for (product, issued) in operations.iter().enumerate() {
        let SolveOperation::MatrixMultiply {
            lhs,
            rhs,
            destination,
        } = &issued.operation
        else {
            continue;
        };
        if !host_product(register_types, (*lhs, *rhs, *destination)) {
            continue;
        }
        for operand in [lhs, rhs] {
            let Some(definition) = flow.definitions[operand.index()] else {
                continue;
            };
            let transpose = &operations[definition].operation;
            let SolveOperation::Transpose {
                operand: source, ..
            } = transpose
            else {
                continue;
            };
            let plain = register_types
                .get(source.index())
                .is_some_and(|source| source.dimensions().len() == 2 && is_binary64(source));
            if flow.reads[operand.index()] != 1 || !plain {
                continue;
            }
            // The product reads the transpose's source in its place.
            extend_operands(transpose, product, last_reads);
            fused[operand.index()] = Some(SolveFusedRegister {
                definition,
                point: product,
            });
        }
    }
}

/// Whether a product of these registers runs in the binary64 host kernel.
fn host_product(
    register_types: &[SolveValueType],
    (lhs, rhs, destination): (SolveRegisterId, SolveRegisterId, SolveRegisterId),
) -> bool {
    let (Some(lhs_type), Some(rhs_type)) = (
        register_types.get(lhs.index()),
        register_types.get(rhs.index()),
    ) else {
        return false;
    };
    let (lhs_dims, rhs_dims) = (lhs_type.dimensions(), rhs_type.dimensions());
    if lhs_dims.len() != 2 || rhs_dims.len() != 2 {
        return false;
    }
    let work = u64::from(lhs_dims[0]) * u64::from(lhs_dims[1]) * u64::from(rhs_dims[1]);
    work > SOLVE_STATIC_MATRIX_WORK
        && [lhs, rhs, destination].iter().all(|register| {
            register_types
                .get(register.index())
                .is_some_and(is_binary64)
        })
}

/// Extend the last read of every operand of `operation` to `point`.
fn extend_operands(operation: &SolveOperation, point: usize, last_reads: &mut [Option<usize>]) {
    operation.visit_input_registers(|operand| {
        if let Some(last) = last_reads.get_mut(operand.index()) {
            *last = Some(last.map_or(point, |last| last.max(point)));
        }
    });
}

/// The capture a map repeats, when its body only stores one capture loaded
/// unchanged: element `n` of the map is then element `n mod width` of it.
fn broadcast_capture(
    captures: &[SolveRegisterId],
    body: &SolveProgramRegion,
) -> Option<SolveRegisterId> {
    let outputs = body.outputs();
    if outputs.len() != 1 {
        return None;
    }
    let program = body.body();
    let output_slot = body.inputs().len();
    let mut loaded = Vec::new();
    let mut stored = None;
    for issued in program.operations() {
        match &issued.operation {
            SolveOperation::Load { destination, slot } if slot.index() < output_slot => {
                loaded.push((*destination, slot.index()));
            }
            SolveOperation::Store { slot, source }
                if slot.index() == output_slot && stored.is_none() =>
            {
                stored = Some(*source);
            }
            _ => return None,
        }
    }
    let (_, slot) = loaded
        .into_iter()
        .find(|(register, _)| Some(*register) == stored)?;
    let capture = *captures.get(slot)?;
    (program.slots().get(slot)?.value_type() == &outputs[0]).then_some(capture)
}

/// Whether `register` is defined by a `sign` of a Real.
fn sign_definition(operations: &[SolveSpannedOperation], register: SolveRegisterId) -> bool {
    operations.iter().any(|issued| {
        matches!(
            issued.operation,
            SolveOperation::Unary {
                destination,
                operator: SolveUnaryOperator::Sign,
                ..
            } if destination == register
        )
    })
}

fn is_binary64(value_type: &SolveValueType) -> bool {
    matches!(
        value_type.element_type(),
        SolveScalarType::Real {
            format: SolveRealFormat::Binary64,
            ..
        }
    )
}
