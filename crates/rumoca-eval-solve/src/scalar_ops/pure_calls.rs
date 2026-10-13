//! Inline compiler-issued pure calls as straight-line scalar ops.
//!
//! Solve lowering may issue one `PureCall` for a function call or for a
//! compacted algebraic family (SPEC_0043 §6c), whose body is a checked
//! [`solve::TypedProgram`]. The scalar ODE right-hand-side templates render
//! only scalar ops, so a call is expanded here into the equivalent scalar
//! sequence: every typed register becomes its row-major elements, maps and
//! folds are unrolled over their compact domains, and nested calls are inlined.
//!
//! An element is either a runtime register or a value known at expansion
//! time. Loop binders are known, so integer index arithmetic over them folds
//! and a run-time subscript resolves to one static element. Only index-like
//! arithmetic (integer and Boolean operations, comparisons, rounding) folds;
//! every other operation is emitted for the target runtime, so a constant
//! transcendental expression evaluates where it always did.
//!
//! The semantics mirror the reference evaluator (`rumoca-eval-solve`
//! `typed_program`). Anything that cannot be expanded faithfully — a subscript
//! that stays a runtime value, a runtime `while` predicate, a linear solve, a
//! native body, an assertion output, or an expansion past the op budget —
//! makes the whole call unexpandable, so the caller refuses the model at the
//! capability gate instead of emitting a different function.

mod admission;
mod conditional;
mod number;
mod shape;

use super::ExpansionBudget;
use crate::typed_program::element_matches_type;
use admission::program_work;
use number::{binders, compare_op, constant, identity_element, unary_op};
use rumoca_core::Span;
use rumoca_ir_solve as solve;
use shape::{concatenate, coordinates, row_major_offset, transpose};
use solve::{
    BinaryOp, CompareOp, LinearOp, Reg, SolveBinaryOperator, SolveCompareOperator,
    SolveConversionOperator, SolveOperation, SolveProgramRegion, SolvePureCallOutputKind,
    SolvePureCallOwnerId, SolveReductionOperator, SolveRegisterId, SolveTensorViewAxis,
    SolveUnaryOperator, SolveValueKind, TypedProgram, UnaryOp,
};
use std::sync::Arc;

/// Nested call depth before a (recursive) call is refused.
const CALL_DEPTH: usize = 64;

#[derive(Clone, Copy)]
enum Elem {
    Reg(Reg),
    Const(SolveValueKind),
}

#[derive(Clone)]
struct Val {
    scalar: solve::SolveScalarType,
    dims: Arc<Vec<u32>>,
    elems: Arc<Vec<Elem>>,
}

impl Val {
    fn scalar(scalar: solve::SolveScalarType, elem: Elem) -> Self {
        Self {
            scalar,
            dims: Arc::new(Vec::new()),
            elems: Arc::new(vec![elem]),
        }
    }

    fn one(&self) -> Option<Elem> {
        match self.elems.as_slice() {
            [elem] => Some(*elem),
            _ => None,
        }
    }
    fn matches(&self, value_type: &solve::SolveValueType) -> bool {
        self.scalar == value_type.element_type()
            && self.dims.as_slice() == value_type.dimensions()
            && self.elems.len() == value_type.scalar_count() as usize
    }
}

pub(super) struct Inliner<'a> {
    table: &'a solve::SolvePureCallTable,
    out: &'a mut Vec<LinearOp>,
    next: &'a mut Reg,
    depth: usize,
    budget: &'a mut ExpansionBudget,
    provenance: Option<Span>,
}

impl<'a> Inliner<'a> {
    pub(super) fn new(
        table: &'a solve::SolvePureCallTable,
        out: &'a mut Vec<LinearOp>,
        next: &'a mut Reg,
        budget: &'a mut ExpansionBudget,
    ) -> Self {
        Self {
            table,
            out,
            next,
            depth: 0,
            budget,
            provenance: None,
        }
    }

    /// Expand `PureCall { dst_start, input_starts, site }`: read each input's
    /// registers, run the owner body, and write every result scalar to
    /// consecutive registers from `dst_start`.
    pub(super) fn call_site(
        &mut self,
        dst_start: Reg,
        input_starts: &[Reg],
        site: &solve::SolvePureCallSite,
    ) -> Option<()> {
        if !self.table.matches_site(site)
            || self.table.arithmetic().real_format() != solve::SolveRealFormat::Binary64
        {
            return None;
        }
        let owner = self.table.owner(site.owner())?;
        if input_starts.len() != owner.inputs().len() {
            return None;
        }
        // Call-scoped assertions are observed through the owner, so an owner that
        // holds one stays a typed call in the row: inlining its body would drop
        // the observation the generated products report from.
        if owner
            .outputs()
            .iter()
            .any(|output| output.assertion_level().is_some())
        {
            return None;
        }
        self.budget
            .reserve(owner.inputs().iter().try_fold(0usize, |total, value| {
                total
                    .checked_add(value.scalar_count() as usize)?
                    .checked_add(value.dimensions().len())?
                    .checked_add(1)
            })?)?;
        let arguments = owner
            .inputs()
            .iter()
            .zip(input_starts)
            .map(|(value_type, start)| self.input_value(value_type, *start))
            .collect::<Option<Vec<_>>>()?;
        let results = self.owner(site.owner(), arguments)?;
        let mut dst = dst_start;
        for elem in results.iter().flat_map(|value| value.elems.iter()) {
            let src = self.materialize(*elem)?;
            self.push(LinearOp::Move { dst, src })?;
            dst = dst.checked_add(1)?;
        }
        Some(())
    }

    fn input_value(&mut self, value_type: &solve::SolveValueType, start: Reg) -> Option<Val> {
        let elems = (0..value_type.scalar_count())
            .map(|offset| {
                let register = start.checked_add(offset)?;
                if matches!(
                    value_type.element_type(),
                    solve::SolveScalarType::Integer(_)
                ) {
                    self.literal_input(register, value_type)
                } else {
                    Some(Elem::Reg(register))
                }
            })
            .collect::<Option<Vec<_>>>()?;
        Some(Val {
            scalar: value_type.element_type(),
            dims: Arc::new(value_type.dimensions().to_vec()),
            elems: Arc::new(elems),
        })
    }

    /// A literal capture retains the checked scalar ABI's exact Integer value.
    /// Follow only moves through the already emitted prefix. The first other
    /// write refuses, so a later load cannot inherit an earlier literal value.
    fn literal_input(
        &mut self,
        mut register: Reg,
        value_type: &solve::SolveValueType,
    ) -> Option<Elem> {
        for operation in self.out.iter().rev() {
            self.budget.reserve(1)?;
            let Some(start) = operation.dst_register() else {
                continue;
            };
            let end = start.checked_add(Reg::try_from(operation.dst_register_count()).ok()?)?;
            if !(start..end).contains(&register) {
                continue;
            }
            match operation {
                LinearOp::Const { value, .. } => {
                    let kind = crate::typed_kind_from_scalar(*value, value_type).ok()?;
                    return Some(constant(kind));
                }
                LinearOp::Move { src, .. } => register = *src,
                _ => return None,
            }
        }
        None
    }

    fn owner(&mut self, id: SolvePureCallOwnerId, arguments: Vec<Val>) -> Option<Vec<Val>> {
        let owner = self.table.owner(id)?;
        if owner
            .outputs()
            .iter()
            .any(|output| output.kind() != SolvePureCallOutputKind::Result)
            || self.depth >= CALL_DEPTH
        {
            return None;
        }
        self.depth += 1;
        let results = self.program(owner.body(), arguments, owner.outputs().len());
        self.depth -= 1;
        results
    }

    fn region(&mut self, region: &SolveProgramRegion, arguments: Vec<Val>) -> Option<Vec<Val>> {
        if arguments.len() != region.inputs().len() {
            return None;
        }
        self.program(region.body(), arguments, region.outputs().len())
    }

    /// Run `program` with its leading slots bound to `arguments`; its outputs
    /// are the `output_count` slots that follow.
    fn program(
        &mut self,
        program: &TypedProgram,
        arguments: Vec<Val>,
        output_count: usize,
    ) -> Option<Vec<Val>> {
        if program.arithmetic().real_format() != solve::SolveRealFormat::Binary64 {
            return None;
        }
        self.budget.reserve(program_work(program)?)?;
        let input_count = arguments.len();
        let mut frame = Frame {
            program,
            slots: vec![None; program.slots().len()],
            registers: vec![None; program.register_types().len()],
        };
        for ((slot, declaration), value) in
            frame.slots.iter_mut().zip(program.slots()).zip(arguments)
        {
            if !value.matches(declaration.value_type()) {
                return None;
            }
            *slot = Some(value);
        }
        for operation in program.operations() {
            let previous = self.provenance.replace(operation.provenance());
            let result = self.operation(&mut frame, operation.operation());
            self.provenance = previous;
            result?;
        }
        (input_count..input_count.checked_add(output_count)?)
            .map(|slot| frame.slots.get_mut(slot)?.take())
            .collect()
    }
}

struct Frame<'p> {
    program: &'p TypedProgram,
    slots: Vec<Option<Val>>,
    registers: Vec<Option<Val>>,
}

impl Frame<'_> {
    fn read(&self, register: SolveRegisterId) -> Option<&Val> {
        self.registers.get(register.index())?.as_ref()
    }

    fn reads(&self, registers: &[SolveRegisterId]) -> Option<Vec<Val>> {
        registers
            .iter()
            .map(|register| self.read(*register).cloned())
            .collect()
    }

    fn dims(&self, register: SolveRegisterId) -> Option<Vec<u32>> {
        Some(
            self.program
                .register_types()
                .get(register.index())?
                .dimensions()
                .to_vec(),
        )
    }

    fn write(&mut self, register: SolveRegisterId, elems: Vec<Elem>) -> Option<()> {
        let dims = self.dims(register)?;
        let count = dims
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))?;
        if count != elems.len() {
            return None;
        }
        let scalar = self
            .program
            .register_types()
            .get(register.index())?
            .element_type();
        if elems.iter().any(|elem| match elem {
            Elem::Const(value) => !element_matches_type(*value, scalar),
            Elem::Reg(_) => matches!(scalar, solve::SolveScalarType::Integer(_)),
        }) {
            return None;
        }
        *self.registers.get_mut(register.index())? = Some(Val {
            scalar,
            dims: Arc::new(dims),
            elems: Arc::new(elems),
        });
        Some(())
    }

    fn write_val(&mut self, register: SolveRegisterId, value: Val) -> Option<()> {
        if !value.matches(self.program.register_types().get(register.index())?) {
            return None;
        }
        *self.registers.get_mut(register.index())? = Some(value);
        Some(())
    }
}

impl Inliner<'_> {
    fn push(&mut self, op: LinearOp) -> Option<()> {
        self.budget.reserve(1)?;
        self.out.push(op);
        Some(())
    }

    fn fresh(&mut self) -> Option<Reg> {
        let reg = *self.next;
        *self.next = reg.checked_add(1)?;
        Some(reg)
    }

    fn sum(&mut self, terms: impl IntoIterator<Item = Elem>) -> Option<Elem> {
        let mut terms = terms.into_iter();
        let mut total = terms.next()?;
        for term in terms {
            total = self.binary(BinaryOp::Add, total, term)?;
        }
        Some(total)
    }
}

impl Inliner<'_> {
    /// A one-based Modelica subscript known at expansion time, as a zero-based
    /// coordinate.
    fn static_index(frame: &Frame<'_>, register: SolveRegisterId) -> Option<i64> {
        match frame.read(register)?.one()? {
            Elem::Const(SolveValueKind::Integer(value)) => value.checked_sub(1),
            _ => None,
        }
    }

    /// A scalar select cannot preserve a conditional bounds fault. Refuse
    /// every potentially active invalid access rather than inventing a value.
    fn element(&self, value: &Val, coordinate: &[i64]) -> Option<Elem> {
        let coordinate = coordinate
            .iter()
            .map(|index| u32::try_from(*index).ok())
            .collect::<Option<Vec<_>>>();
        match coordinate.and_then(|coordinate| row_major_offset(&value.dims, &coordinate)) {
            Some(offset) => value.elems.get(offset).copied(),
            None => None,
        }
    }

    /// Resolved view axes: `Some(index)` fixes an axis, `None` keeps it.
    fn view_axes(
        frame: &Frame<'_>,
        axes: &[SolveTensorViewAxis],
    ) -> Option<Vec<(Option<i64>, u32)>> {
        axes.iter()
            .map(|axis| match *axis {
                SolveTensorViewAxis::Index(register) => {
                    Some((Some(Self::static_index(frame, register)?), 0))
                }
                SolveTensorViewAxis::Span { origin, .. } => Some((None, origin)),
            })
            .collect()
    }

    fn view_coordinate(axes: &[(Option<i64>, u32)], retained: &[u32]) -> Option<Vec<i64>> {
        let mut retained = retained.iter();
        let coordinate = axes
            .iter()
            .map(|(index, origin)| match index {
                Some(index) => Some(*index),
                None => Some(i64::from(*origin) + i64::from(*retained.next()?)),
            })
            .collect::<Option<Vec<_>>>()?;
        retained.next().is_none().then_some(coordinate)
    }
}

impl Inliner<'_> {
    // Exhaustive dispatch over the closed typed operation vocabulary, in the
    // order the reference evaluator lists it.
    // SPEC_0021: Exception - one arm per typed operation stays contiguous so the vocabulary is auditable.
    #[allow(clippy::too_many_lines)]
    fn operation(&mut self, frame: &mut Frame<'_>, operation: &SolveOperation) -> Option<()> {
        match operation {
            SolveOperation::Constant { destination, value } => {
                frame.write(*destination, vec![constant(value.kind())])
            }
            SolveOperation::Load { destination, slot } => {
                let value = frame.slots.get(slot.index())?.clone()?;
                frame.write_val(*destination, value)
            }
            SolveOperation::Store { slot, source } => {
                let value = frame.read(*source)?.clone();
                *frame.slots.get_mut(slot.index())? = Some(value);
                Some(())
            }
            SolveOperation::Unary {
                destination,
                operator,
                operand,
            } => {
                let operand = frame.read(*operand)?.clone();
                let elems = operand
                    .elems
                    .iter()
                    .map(|elem| self.unary(unary_op(*operator), *elem))
                    .collect::<Option<Vec<_>>>()?;
                frame.write(*destination, elems)
            }
            SolveOperation::Binary {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                let (lhs, rhs) = (frame.read(*lhs)?.clone(), frame.read(*rhs)?.clone());
                let elems = self.zip_binary(*operator, &lhs, &rhs)?;
                frame.write(*destination, elems)
            }
            SolveOperation::Compare {
                destination,
                operator,
                lhs,
                rhs,
            } => {
                let (lhs, rhs) = (frame.read(*lhs)?.clone(), frame.read(*rhs)?.clone());
                if lhs.elems.len() != rhs.elems.len() {
                    return None;
                }
                let elems = lhs
                    .elems
                    .iter()
                    .zip(rhs.elems.iter())
                    .map(|(lhs, rhs)| self.compare(compare_op(*operator), *lhs, *rhs))
                    .collect::<Option<Vec<_>>>()?;
                frame.write(*destination, elems)
            }
            SolveOperation::Convert {
                destination,
                operator,
                operand,
            } => {
                let operand = frame.read(*operand)?.clone();
                let elems = operand
                    .elems
                    .iter()
                    .map(|elem| {
                        self.convert(
                            *operator,
                            *elem,
                            frame
                                .program
                                .register_types()
                                .get(destination.index())?
                                .element_type(),
                        )
                    })
                    .collect::<Option<Vec<_>>>()?;
                frame.write(*destination, elems)
            }
            SolveOperation::Select {
                destination,
                condition,
                if_true,
                if_false,
            } => {
                let condition = frame.read(*condition)?.one()?;
                let (if_true, if_false) = (
                    frame.read(*if_true)?.clone(),
                    frame.read(*if_false)?.clone(),
                );
                if if_true.elems.len() != if_false.elems.len() {
                    return None;
                }
                let elems = if_true
                    .elems
                    .iter()
                    .zip(if_false.elems.iter())
                    .map(|(if_true, if_false)| self.select(condition, *if_true, *if_false))
                    .collect::<Option<Vec<_>>>()?;
                frame.write(*destination, elems)
            }
            SolveOperation::Conditional {
                condition,
                captures,
                destinations,
                if_true,
                if_false,
            } => self.conditional(
                frame,
                *condition,
                captures,
                destinations,
                (if_true, if_false),
            ),
            SolveOperation::Map {
                domain,
                captures,
                destination,
                body,
            } => {
                let captures = frame.reads(captures)?;
                let mut elems = Vec::new();
                self.check_domain(domain, body.body())?;
                for tuple in domain.index_tuple_iter().ok()? {
                    self.budget
                        .reserve(captures.len().checked_add(tuple.len().checked_mul(3)?)?)?;
                    let mut arguments = captures.clone();
                    arguments.extend(binders(&tuple, self.table.arithmetic())?);
                    let [output] = <[Val; 1]>::try_from(self.region(body, arguments)?).ok()?;
                    elems.extend(output.elems.iter().copied());
                }
                frame.write(*destination, elems)
            }
            SolveOperation::Fold {
                domain,
                initial,
                captures,
                state_captures,
                destinations,
                transition,
                continuation,
            } => {
                if !state_captures.is_empty() {
                    return None;
                }
                let carried = self.fold(
                    domain,
                    frame.reads(initial)?,
                    &frame.reads(captures)?,
                    transition,
                    continuation.as_deref(),
                )?;
                if carried.len() != destinations.len() {
                    return None;
                }
                for (destination, value) in destinations.iter().zip(carried) {
                    frame.write_val(*destination, value)?;
                }
                Some(())
            }
            SolveOperation::Scale {
                destination,
                aggregate,
                scalar,
            } => {
                let aggregate = frame.read(*aggregate)?.clone();
                let scalar = frame.read(*scalar)?.one()?;
                let elems = aggregate
                    .elems
                    .iter()
                    .map(|elem| self.binary(BinaryOp::Mul, *elem, scalar))
                    .collect::<Option<Vec<_>>>()?;
                frame.write(*destination, elems)
            }
            SolveOperation::BroadcastBinary {
                destination,
                operator,
                aggregate,
                scalar,
                scalar_on_lhs,
            } => {
                let aggregate = frame.read(*aggregate)?.clone();
                let scalar = frame.read(*scalar)?.one()?;
                let elems = aggregate
                    .elems
                    .iter()
                    .map(|elem| match scalar_on_lhs {
                        true => self.typed_binary(*operator, scalar, *elem),
                        false => self.typed_binary(*operator, *elem, scalar),
                    })
                    .collect::<Option<Vec<_>>>()?;
                frame.write(*destination, elems)
            }
            SolveOperation::Transpose {
                destination,
                operand,
            } => {
                let elems = transpose(frame.read(*operand)?)?;
                frame.write(*destination, elems)
            }
            SolveOperation::MatrixMultiply {
                destination,
                lhs,
                rhs,
            } => {
                let (lhs, rhs) = (frame.read(*lhs)?.clone(), frame.read(*rhs)?.clone());
                let elems = self.matrix_multiply(&lhs, &rhs)?;
                frame.write(*destination, elems)
            }
            SolveOperation::Cross {
                destination,
                lhs,
                rhs,
            } => {
                let (lhs, rhs) = (frame.read(*lhs)?.clone(), frame.read(*rhs)?.clone());
                let [a0, a1, a2] = <[Elem; 3]>::try_from(lhs.elems.as_slice()).ok()?;
                let [b0, b1, b2] = <[Elem; 3]>::try_from(rhs.elems.as_slice()).ok()?;
                let mut component = |p: Elem, q: Elem, r: Elem, s: Elem| {
                    let positive = self.binary(BinaryOp::Mul, p, q)?;
                    let negative = self.binary(BinaryOp::Mul, r, s)?;
                    self.binary(BinaryOp::Sub, positive, negative)
                };
                let elems = vec![
                    component(a1, b2, a2, b1)?,
                    component(a2, b0, a0, b2)?,
                    component(a0, b1, a1, b0)?,
                ];
                frame.write(*destination, elems)
            }
            SolveOperation::Reduce {
                destination,
                operator,
                operand,
            } => {
                let operand = frame.read(*operand)?.clone();
                let op = match operator {
                    SolveReductionOperator::Sum => BinaryOp::Add,
                    SolveReductionOperator::Product => BinaryOp::Mul,
                    SolveReductionOperator::Minimum => BinaryOp::Min,
                    SolveReductionOperator::Maximum => BinaryOp::Max,
                    SolveReductionOperator::All => BinaryOp::And,
                };
                let mut elems = operand.elems.iter().copied();
                let mut value = elems.next()?;
                for elem in elems {
                    value = self.binary(op, value, elem)?;
                }
                frame.write(*destination, vec![value])
            }
            SolveOperation::Identity { destination } => {
                let dims = frame.dims(*destination)?;
                let [rows, columns] = dims.as_slice() else {
                    return None;
                };
                let scalar = frame.program.register_types()[destination.index()].element_type();
                let elems = (0..*rows)
                    .flat_map(|row| {
                        (0..*columns).map(move |column| identity_element(scalar, row == column))
                    })
                    .collect();
                frame.write(*destination, elems)
            }
            SolveOperation::Diagonal {
                destination,
                operand,
            } => {
                let operand = frame.read(*operand)?.clone();
                let extent = operand.elems.len();
                let mut elems = vec![
                    identity_element(
                        frame.program.register_types()[destination.index()].element_type(),
                        false
                    );
                    extent.checked_mul(extent)?
                ];
                for (ordinal, elem) in operand.elems.iter().copied().enumerate() {
                    elems[ordinal * extent + ordinal] = elem;
                }
                frame.write(*destination, elems)
            }
            SolveOperation::Concatenate {
                destination,
                axis,
                operands,
            } => {
                let dims = frame.dims(*destination)?;
                let elems = concatenate(&dims, *axis as usize, &frame.reads(operands)?)?;
                frame.write(*destination, elems)
            }
            SolveOperation::Fill { destination, value } => {
                let value = frame.read(*value)?.one()?;
                let dims = frame.dims(*destination)?;
                let count = dims
                    .iter()
                    .map(|extent| *extent as usize)
                    .product::<usize>();
                frame.write(*destination, vec![value; count])
            }
            SolveOperation::ConstructAggregate {
                destination,
                elements,
            } => {
                let mut elems = Vec::new();
                for value in frame.reads(elements)? {
                    elems.extend(value.elems.iter().copied());
                }
                frame.write(*destination, elems)
            }
            SolveOperation::ProjectElement {
                destination,
                aggregate,
                indices,
            } => {
                let aggregate = frame.read(*aggregate)?;
                let coordinate = indices
                    .iter()
                    .map(|index| i64::from(*index))
                    .collect::<Vec<_>>();
                let elem = self.element(aggregate, &coordinate)?;
                frame.write(*destination, vec![elem])
            }
            SolveOperation::ProjectElementDynamic {
                destination,
                aggregate,
                indices,
            } => {
                let coordinate = indices
                    .iter()
                    .map(|index| Self::static_index(frame, *index))
                    .collect::<Option<Vec<_>>>()?;
                let elem = self.element(frame.read(*aggregate)?, &coordinate)?;
                frame.write(*destination, vec![elem])
            }
            SolveOperation::ProjectSlice {
                destination,
                aggregate,
                origin,
            } => {
                let aggregate = frame.read(*aggregate)?.clone();
                let dims = frame.dims(*destination)?;
                let elems = coordinates(&dims)
                    .map(|coordinate| {
                        let source = coordinate
                            .iter()
                            .zip(origin.iter())
                            .map(|(index, origin)| i64::from(*index) + i64::from(*origin))
                            .collect::<Vec<_>>();
                        self.element(&aggregate, &source)
                    })
                    .collect::<Option<Vec<_>>>()?;
                frame.write(*destination, elems)
            }
            SolveOperation::ProjectView {
                destination,
                aggregate,
                axes,
            } => {
                let axes = Self::view_axes(frame, axes)?;
                let aggregate = frame.read(*aggregate)?.clone();
                let dims = frame.dims(*destination)?;
                let elems = coordinates(&dims)
                    .map(|coordinate| {
                        let source = Self::view_coordinate(&axes, &coordinate)?;
                        self.element(&aggregate, &source)
                    })
                    .collect::<Option<Vec<_>>>()?;
                frame.write(*destination, elems)
            }
            SolveOperation::SelectElement {
                destination,
                aggregate,
                indices,
                out_of_range,
            } => {
                let coordinate = indices
                    .iter()
                    .map(|index| Self::static_index(frame, *index))
                    .collect::<Option<Vec<_>>>()?;
                let aggregate = frame.read(*aggregate)?;
                let elem = coordinate
                    .iter()
                    .map(|index| u32::try_from(*index).ok())
                    .collect::<Option<Vec<_>>>()
                    .and_then(|coordinate| row_major_offset(&aggregate.dims, &coordinate))
                    .and_then(|offset| aggregate.elems.get(offset).copied());
                let elem = match elem {
                    Some(elem) => elem,
                    None => frame.read(*out_of_range)?.one()?,
                };
                frame.write(*destination, vec![elem])
            }
            SolveOperation::UpdateElement {
                destination,
                aggregate,
                value,
                indices,
            } => {
                let coordinate = indices
                    .iter()
                    .map(|index| Self::static_index(frame, *index))
                    .collect::<Option<Vec<_>>>()?;
                let value = frame.read(*value)?.one()?;
                let mut aggregate = frame.read(*aggregate)?.clone();
                self.update(&mut aggregate, &coordinate, value)?;
                frame.write_val(*destination, aggregate)
            }
            SolveOperation::UpdateSlice {
                destination,
                aggregate,
                value,
                origin,
            } => {
                let update = frame.read(*value)?.clone();
                let mut aggregate = frame.read(*aggregate)?.clone();
                for (coordinate, elem) in coordinates(&update.dims).zip(update.elems.iter()) {
                    let target = coordinate
                        .iter()
                        .zip(origin.iter())
                        .map(|(index, origin)| i64::from(*index) + i64::from(*origin))
                        .collect::<Vec<_>>();
                    self.update(&mut aggregate, &target, *elem)?;
                }
                frame.write_val(*destination, aggregate)
            }
            SolveOperation::UpdateView {
                destination,
                aggregate,
                value,
                axes,
            } => {
                let axes = Self::view_axes(frame, axes)?;
                let update = frame.read(*value)?.clone();
                let mut aggregate = frame.read(*aggregate)?.clone();
                for (coordinate, elem) in coordinates(&update.dims).zip(update.elems.iter()) {
                    let target = Self::view_coordinate(&axes, &coordinate)?;
                    self.update(&mut aggregate, &target, *elem)?;
                }
                frame.write_val(*destination, aggregate)
            }
            SolveOperation::Call {
                owner,
                arguments,
                destinations,
                assertion_forwarding,
            } => {
                if !assertion_forwarding.is_empty() {
                    return None;
                }
                let arguments = frame.reads(arguments)?;
                let results = self.owner(*owner, arguments)?;
                if results.len() != destinations.len() {
                    return None;
                }
                for (destination, value) in destinations.iter().zip(results) {
                    frame.write_val(*destination, value)?;
                }
                Some(())
            }
            // Assigned-state and assertion-forwarding programs keep their ordered
            // state in the typed evaluator, never in scalar expansion.
            SolveOperation::AssignedConditional { .. }
            | SolveOperation::AssignedState { .. }
            | SolveOperation::CheckAssertion { .. }
            | SolveOperation::LinearSolve { .. }
            | SolveOperation::Native { .. } => None,
        }
    }

    /// One compact fold, unrolled. A bounded `while` predicate must be known
    /// at expansion time on every pass.
    fn fold(
        &mut self,
        domain: &rumoca_core::StructuredIndexDomain,
        mut carried: Vec<Val>,
        captures: &[Val],
        transition: &SolveProgramRegion,
        continuation: Option<&SolveProgramRegion>,
    ) -> Option<Vec<Val>> {
        self.check_domain(domain, transition.body())?;
        for tuple in domain.index_tuple_iter().ok()? {
            let point = binders(&tuple, self.table.arithmetic())?;
            if let Some(continuation) = continuation
                && !self.continues(continuation, &carried, captures, &point)?
            {
                break;
            }
            self.budget.reserve(
                carried
                    .len()
                    .checked_add(captures.len())?
                    .checked_add(tuple.len().checked_mul(3)?)?,
            )?;
            let mut arguments = carried;
            arguments.extend(captures.iter().cloned());
            arguments.extend(point);
            carried = self.region(transition, arguments)?;
        }
        Some(carried)
    }

    fn continues(
        &mut self,
        continuation: &SolveProgramRegion,
        carried: &[Val],
        captures: &[Val],
        point: &[Val],
    ) -> Option<bool> {
        self.budget.reserve(
            carried
                .len()
                .checked_add(captures.len())?
                .checked_add(point.len())?,
        )?;
        let mut arguments = carried.to_vec();
        arguments.extend(captures.iter().cloned());
        arguments.extend(point.iter().cloned());
        let [predicate] = <[Val; 1]>::try_from(self.region(continuation, arguments)?).ok()?;
        match predicate.one()? {
            Elem::Const(SolveValueKind::Boolean(value)) => Some(value),
            Elem::Const(_) => None,
            Elem::Reg(_) => None,
        }
    }

    fn update(&self, aggregate: &mut Val, coordinate: &[i64], value: Elem) -> Option<()> {
        let offset = coordinate
            .iter()
            .map(|index| u32::try_from(*index).ok())
            .collect::<Option<Vec<_>>>()
            .and_then(|coordinate| row_major_offset(&aggregate.dims, &coordinate));
        match offset {
            Some(offset) => {
                *Arc::make_mut(&mut aggregate.elems).get_mut(offset)? = value;
                Some(())
            }
            None => None,
        }
    }

    /// MLS vector/matrix products: `[n]·[n]`, `[r,n]·[n]`, `[n]·[n,c]`,
    /// `[r,n]·[n,c]`, each element a left-to-right sum of products.
    fn matrix_multiply(&mut self, lhs: &Val, rhs: &Val) -> Option<Vec<Elem>> {
        let (rows, inner, columns) = match (lhs.dims.as_slice(), rhs.dims.as_slice()) {
            ([n], [m]) if n == m => (1, *n as usize, 1),
            ([r, n], [m]) if n == m => (*r as usize, *n as usize, 1),
            ([n], [m, c]) if n == m => (1, *n as usize, *c as usize),
            ([r, n], [m, c]) if n == m => (*r as usize, *n as usize, *c as usize),
            _ => return None,
        };
        self.budget.reserve(
            rows.checked_mul(columns)?
                .checked_mul(inner.checked_mul(2)?)?,
        )?;
        (0..rows)
            .flat_map(|row| (0..columns).map(move |column| (row, column)))
            .map(|(row, column)| {
                let lhs_terms = (0..inner).map(|shared| match lhs.dims.len() {
                    1 => shared,
                    _ => row * inner + shared,
                });
                let rhs_terms = (0..inner).map(|shared| match rhs.dims.len() {
                    1 => shared,
                    _ => shared * columns + column,
                });
                self.dot(lhs, rhs, lhs_terms.zip(rhs_terms))
            })
            .collect()
    }

    /// `sum_k lhs[i_k] * rhs[j_k]`, left to right.
    fn dot(
        &mut self,
        lhs: &Val,
        rhs: &Val,
        offsets: impl Iterator<Item = (usize, usize)>,
    ) -> Option<Elem> {
        let terms = offsets
            .map(|(i, j)| self.binary(BinaryOp::Mul, *lhs.elems.get(i)?, *rhs.elems.get(j)?))
            .collect::<Option<Vec<_>>>()?;
        self.sum(terms)
    }

    /// Keep selected regions lazy in the final scalar view.
    fn conditional(
        &mut self,
        frame: &mut Frame<'_>,
        condition: SolveRegisterId,
        captures: &[SolveRegisterId],
        destinations: &[SolveRegisterId],
        (if_true, if_false): (&SolveProgramRegion, &SolveProgramRegion),
    ) -> Option<()> {
        let condition = frame.read(condition)?.one()?;
        let captures = frame.reads(captures)?;
        let outputs = match condition {
            Elem::Const(SolveValueKind::Boolean(value)) => {
                self.region(if value { if_true } else { if_false }, captures)?
            }
            Elem::Const(_) => return None,
            Elem::Reg(condition) => {
                self.selected_regions(condition, captures, if_true, if_false)?
            }
        };
        if outputs.len() != destinations.len() {
            return None;
        }
        for (destination, value) in destinations.iter().zip(outputs) {
            frame.write_val(*destination, value)?;
        }
        Some(())
    }
}
