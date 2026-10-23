//! Elementwise fusion of `Real` tensor operations.
//!
//! Each elementwise operation lowers to its own loop over a stored result, so
//! an expression such as Adam's `m / c1 ./ (sqrt(v / c2) .+ eps)` runs one
//! pass and one temporary per operator. A result read only once, by another
//! elementwise operation of the same extent, is instead never stored: the
//! reading operation evaluates it per element inside its own loop. Every
//! element still sees the same operators on the same operands in the same
//! order, so the result is bit-identical, and `Real` operators cannot fault,
//! so no failure is reordered.
//!
//! Deferring an operation moves its reads of its operands to the operation
//! that finally materializes the chain; [`plan`] extends those operands'
//! lifetimes accordingly before any storage is assigned.

use rumoca_ir_solve as solve;

use super::CompileError;

/// The per-element operands of a fusible operation and its result.
struct Elementwise {
    destination: solve::SolveRegisterId,
    operands: Vec<solve::SolveRegisterId>,
}

fn real(program: &solve::TypedProgram, register: solve::SolveRegisterId) -> Option<u32> {
    let value_type = program.register_types().get(register.index())?;
    matches!(
        value_type.element_type(),
        solve::SolveScalarType::Real { .. }
    )
    .then(|| value_type.scalar_count())
}

fn elementwise(
    program: &solve::TypedProgram,
    operation: &solve::SolveOperation,
) -> Option<Elementwise> {
    use solve::SolveOperation as Op;
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
            operator: solve::SolveConversionOperator::IntegerToReal,
            operand,
        } => (*destination, vec![*operand], vec![*destination]),
        // `sign` yields -1, 0 or 1 (0 for NaN), whose conversion cannot fault.
        Op::Convert {
            destination,
            operator:
                solve::SolveConversionOperator::RealToIntegerTowardZero
                | solve::SolveConversionOperator::RealToIntegerTowardNegativeInfinity,
            operand,
        } if sign_definition(program, *operand) => (*destination, vec![*operand], vec![*operand]),
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
    let count = program
        .register_types()
        .get(destination.index())?
        .scalar_count();
    // A single element gains nothing from fusion.
    if count < 2 {
        return None;
    }
    for register in &real_operands {
        real(program, *register)?;
    }
    for operand in &operands {
        if program
            .register_types()
            .get(operand.index())?
            .scalar_count()
            != count
        {
            return None;
        }
    }
    Some(Elementwise {
        destination,
        operands,
    })
}

/// The operation each fused register defers, by register; `last_uses` is
/// extended so every operand of a fused operation lives until the operation
/// that materializes it.
pub(super) fn plan(
    program: &solve::TypedProgram,
    last_uses: &mut [Option<usize>],
) -> Result<Vec<Option<solve::SolveOperation>>, CompileError> {
    let flow = Flow::of(program);
    let shapes = program
        .operations()
        .iter()
        .map(|operation| elementwise(program, operation.operation()))
        .collect::<Vec<_>>();
    let mut consumer = single_use_consumers(program, &shapes, &flow);
    let mut materialized = materialization(&shapes, &consumer);
    recompute_cheap_producers(program, &shapes, &flow, (&mut consumer, &mut materialized));
    // Recomputation moves producers into later loops, and every producer fused
    // into a moved one moves with it. Only the final consumer edges say where
    // each value is read, so materialization is rebuilt from them before any
    // operand's life is extended. Each edge points forward, so one reverse
    // pass reaches the fixpoint.
    let materialized = materialization(&shapes, &consumer);
    let mut fused = vec![None; flow.reads.len()];
    fuse_product_transposes(program, &flow, last_uses, &mut fused)?;
    for (index, shape) in shapes.iter().enumerate() {
        let Some(shape) = shape else {
            continue;
        };
        if consumer[shape.destination.index()].is_none() {
            continue;
        }
        let operation = program.operations()[index].operation();
        extend_operands(operation, materialized[index], last_uses)?;
        fused[shape.destination.index()] = Some(operation.clone());
    }
    Ok(fused)
}

/// Who reads and defines each register.
struct Flow {
    reads: Vec<u32>,
    definitions: Vec<Option<usize>>,
    readers: Vec<Vec<usize>>,
}

impl Flow {
    fn of(program: &solve::TypedProgram) -> Self {
        let register_count = program.register_types().len();
        let mut flow = Self {
            reads: vec![0; register_count],
            definitions: vec![None; register_count],
            readers: vec![Vec::new(); register_count],
        };
        for (index, operation) in program.operations().iter().enumerate() {
            operation.operation().visit_input_registers(|register| {
                flow.reads[register.index()] += 1;
                flow.readers[register.index()].push(index);
            });
            operation.operation().visit_output_registers(|register| {
                flow.definitions[register.index()] = Some(index);
            });
        }
        flow
    }
}

/// For each register, the elementwise operation (or reduction) that is its
/// only reader and reads it at its own extent.
fn single_use_consumers(
    program: &solve::TypedProgram,
    shapes: &[Option<Elementwise>],
    flow: &Flow,
) -> Vec<Option<usize>> {
    let types = program.register_types();
    let mut consumer = vec![None::<usize>; flow.reads.len()];
    for (index, shape) in shapes.iter().enumerate() {
        // A reduction reads its operand element by element, in order.
        let (operands, count) = match (shape, program.operations()[index].operation()) {
            (Some(shape), _) => (
                shape.operands.clone(),
                types[shape.destination.index()].scalar_count(),
            ),
            (None, solve::SolveOperation::Reduce { operand, .. }) => {
                (vec![*operand], types[operand.index()].scalar_count())
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
                && types[operand.index()].scalar_count() == count
            {
                consumer[operand.index()] = Some(index);
            }
        }
    }
    consumer
}

/// Where each operation's value is finally stored: its own index, or that of
/// the operation it is fused into, transitively. Every consumer edge points to
/// a later operation, so visiting operations last to first sees each
/// consumer's final point before its producers.
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
/// materializes them all, is recomputed per read instead of stored.
fn recompute_cheap_producers(
    program: &solve::TypedProgram,
    shapes: &[Option<Elementwise>],
    flow: &Flow,
    (consumer, materialized): (&mut [Option<usize>], &mut [usize]),
) {
    for (index, shape) in shapes.iter().enumerate() {
        let Some(shape) = shape else {
            continue;
        };
        let cheap = matches!(
            program.operations()[index].operation(),
            solve::SolveOperation::Binary {
                operator: solve::SolveBinaryOperator::Add
                    | solve::SolveBinaryOperator::Subtract
                    | solve::SolveBinaryOperator::Multiply,
                ..
            } | solve::SolveOperation::Scale { .. }
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
/// read in place by that kernel instead of stored.
fn fuse_product_transposes(
    program: &solve::TypedProgram,
    flow: &Flow,
    last_uses: &mut [Option<usize>],
    fused: &mut [Option<solve::SolveOperation>],
) -> Result<(), CompileError> {
    let operations = program.operations();
    let types = program.register_types();
    for (product, operation) in operations.iter().enumerate() {
        let solve::SolveOperation::MatrixMultiply {
            lhs,
            rhs,
            destination,
        } = operation.operation()
        else {
            continue;
        };
        if !host_product(types, (*lhs, *rhs, *destination)) {
            continue;
        }
        for operand in [lhs, rhs] {
            let Some(definition) = flow.definitions[operand.index()] else {
                continue;
            };
            let transpose = operations[definition].operation();
            let solve::SolveOperation::Transpose {
                operand: source, ..
            } = transpose
            else {
                continue;
            };
            let plain = types[source.index()].dimensions().len() == 2
                && is_binary64(&types[source.index()]);
            if flow.reads[operand.index()] != 1 || !plain {
                continue;
            }
            // The product reads the transpose's source in its place.
            extend_operands(transpose, product, last_uses)?;
            fused[operand.index()] = Some(transpose.clone());
        }
    }
    Ok(())
}

/// Whether a product of these registers runs in the binary64 host kernel.
fn host_product(
    types: &[solve::SolveValueType],
    (lhs, rhs, destination): (
        solve::SolveRegisterId,
        solve::SolveRegisterId,
        solve::SolveRegisterId,
    ),
) -> bool {
    let (lhs_dims, rhs_dims) = (
        types[lhs.index()].dimensions(),
        types[rhs.index()].dimensions(),
    );
    if lhs_dims.len() != 2 || rhs_dims.len() != 2 {
        return false;
    }
    let work = u64::from(lhs_dims[0]) * u64::from(lhs_dims[1]) * u64::from(rhs_dims[1]);
    work > crate::emit::MAX_STATIC_MATRIX_WORK as u64
        && [lhs, rhs, destination]
            .iter()
            .all(|register| is_binary64(&types[register.index()]))
}

/// Extend the life of every operand of `operation` to `point`.
fn extend_operands(
    operation: &solve::SolveOperation,
    point: usize,
    last_uses: &mut [Option<usize>],
) -> Result<(), CompileError> {
    let mut operands = Vec::new();
    operation.visit_input_registers(|register| operands.push(register));
    for operand in operands {
        let last = last_uses
            .get_mut(operand.index())
            .ok_or_else(|| CompileError::Backend("fused operand is missing".into()))?;
        *last = Some(last.map_or(point, |last| last.max(point)));
    }
    Ok(())
}

/// The capture a map repeats, when its body only stores one capture loaded
/// unchanged: element `n` of the map is then element `n mod width` of it.
pub(super) fn broadcast_capture(
    captures: &[solve::SolveRegisterId],
    body: &solve::SolveProgramRegion,
) -> Option<solve::SolveRegisterId> {
    let outputs = body.outputs();
    if outputs.len() != 1 {
        return None;
    }
    let program = body.body();
    let output_slot = body.inputs().len();
    let mut loaded = Vec::new();
    let mut stored = None;
    for operation in program.operations() {
        match operation.operation() {
            solve::SolveOperation::Load { destination, slot } if slot.index() < output_slot => {
                loaded.push((*destination, slot.index()));
            }
            solve::SolveOperation::Store { slot, source }
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
fn sign_definition(program: &solve::TypedProgram, register: solve::SolveRegisterId) -> bool {
    program.operations().iter().any(|operation| {
        matches!(
            operation.operation(),
            solve::SolveOperation::Unary {
                destination,
                operator: solve::SolveUnaryOperator::Sign,
                ..
            } if *destination == register
        )
    })
}

fn is_binary64(value_type: &solve::SolveValueType) -> bool {
    matches!(
        value_type.element_type(),
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary64,
            ..
        }
    )
}

/// The registers `operation` reads once fused operands are expanded into the
/// operands of the operations they defer.
pub(super) fn leaf_operands(
    fused: &[Option<solve::SolveOperation>],
    operation: &solve::SolveOperation,
) -> Vec<solve::SolveRegisterId> {
    let mut pending = Vec::new();
    operation.visit_input_registers(|register| pending.push(register));
    let mut leaves = Vec::new();
    while let Some(register) = pending.pop() {
        match fused.get(register.index()).and_then(Option::as_ref) {
            Some(deferred) => deferred.visit_input_registers(|operand| pending.push(operand)),
            None => leaves.push(register),
        }
    }
    leaves
}
