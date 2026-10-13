//! Fault classification and Integer intervals of one checked typed program
//! (SPEC_0040 SOLVE-C102).
//!
//! Each operation is classified once, when the program is constructed, from
//! its operator, its register types and the Integer intervals of its operands.
//! An interval starts from constants and from the checked finite bounds of the
//! binders of an enclosing loop, and every Integer operation extends it only
//! when the result stays inside the Integer domain of its destination; an
//! operation whose result is so bounded cannot fault. Every executor and every
//! pruning pass reads the stored classes, so "pure and cannot fault" has one
//! definition.

use super::{
    LaneExactness, SolveBinaryOperator, SolveOperation, SolveProgramRegion, SolveRegisterId,
    SolveScalarType, SolveSlot, SolveSpannedOperation, SolveStorageClass, SolveTensorViewAxis,
    SolveUnaryOperator, SolveValueType,
};
use crate::SolveValueKind;
use crate::fault::{FaultClass, FaultKind};
use crate::typed_program::integer_interval::SolveIntegerInterval;
use rumoca_core::StructuredIndexBinder;

type Interval = Option<SolveIntegerInterval>;

/// The fault class of every operation of a program, in operation order.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(super) struct FaultFlow {
    classes: Box<[FaultClass]>,
    lanes: Box<[LaneExactness]>,
    /// For a loop operation: the exactness of the operations of one pass.
    pass_lanes: Box<[Option<LaneExactness>]>,
}

impl FaultFlow {
    /// Classify `operations`, whose `n`-th Input slot holds a value in
    /// `inputs[n]` (unbounded when absent).
    pub(super) fn of(
        operations: &[SolveSpannedOperation],
        register_types: &[SolveValueType],
        slots: &[SolveSlot],
        inputs: &[Interval],
    ) -> Self {
        let mut flow = Analysis {
            register_types,
            slots,
            intervals: vec![None; register_types.len()],
            slot_intervals: slot_intervals(slots, inputs),
        };
        let facts = operations
            .iter()
            .map(|operation| flow.classify(operation.operation()))
            .collect::<Vec<_>>();
        Self {
            classes: facts.iter().map(|facts| facts.class).collect(),
            lanes: facts.iter().map(|facts| facts.lanes).collect(),
            pass_lanes: facts.iter().map(|facts| facts.pass_lanes).collect(),
        }
    }

    pub(super) fn classes(&self) -> &[FaultClass] {
        &self.classes
    }

    /// The exactness of the operations of one pass of loop operation
    /// `operation`, analysed under the intervals of its binders and captures.
    pub(super) fn pass_lanes(&self, operation: usize) -> Option<LaneExactness> {
        self.pass_lanes.get(operation).copied().flatten()
    }

    /// The exactness of the whole program as a region.
    pub(super) fn region_lanes(&self) -> LaneExactness {
        self.lanes
            .iter()
            .fold(LaneExactness::Exact, |lanes, own| lanes.join(*own))
    }

    /// The class of the whole program as a region: its stores to its own
    /// Output slots are not effects.
    pub(super) fn region_class(
        &self,
        operations: &[SolveSpannedOperation],
        slots: &[SolveSlot],
    ) -> FaultClass {
        operations.iter().zip(&self.classes).fold(
            FaultClass::CannotFault,
            |class, (operation, own)| match operation.operation() {
                SolveOperation::Store { slot, .. }
                    if slots[slot.index()].storage() == SolveStorageClass::Output =>
                {
                    class
                }
                _ => class.join(*own),
            },
        )
    }
}

fn slot_intervals(slots: &[SolveSlot], inputs: &[Interval]) -> Vec<Interval> {
    let mut next = inputs.iter().copied();
    slots
        .iter()
        .map(|slot| {
            (slot.storage() == SolveStorageClass::Input)
                .then(|| next.next().flatten())
                .flatten()
        })
        .collect()
}

/// What one operation contributes to the analysis of its program.
struct Facts {
    class: FaultClass,
    lanes: LaneExactness,
    pass_lanes: Option<LaneExactness>,
}

impl Facts {
    const fn of(class: FaultClass, lanes: LaneExactness) -> Self {
        Self {
            class,
            lanes,
            pass_lanes: None,
        }
    }
}

/// The class and the lane exactness of a region body.
struct RegionFacts {
    class: FaultClass,
    lanes: LaneExactness,
}

struct Analysis<'a> {
    register_types: &'a [SolveValueType],
    slots: &'a [SolveSlot],
    intervals: Vec<Interval>,
    slot_intervals: Vec<Interval>,
}

impl Analysis<'_> {
    fn scalar(&self, register: SolveRegisterId) -> SolveScalarType {
        self.register_types[register.index()].element_type()
    }

    fn is_integer(&self, register: SolveRegisterId) -> bool {
        matches!(self.scalar(register), SolveScalarType::Integer(_))
    }

    fn is_scalar(&self, register: SolveRegisterId) -> bool {
        self.register_types[register.index()].scalar_count() == 1
    }

    /// Whether the operation touches an Integer register, so that its checked
    /// arithmetic can fail.
    fn touches_integer(&self, operation: &SolveOperation) -> bool {
        let mut integer = false;
        operation.visit_input_registers(|register| integer |= self.is_integer(register));
        operation.visit_output_registers(|register| integer |= self.is_integer(register));
        integer
    }

    fn classify(&mut self, operation: &SolveOperation) -> Facts {
        use SolveOperation as O;
        match operation {
            O::Map {
                domain,
                captures,
                body,
                ..
            } => {
                let region = self.region(0, captures, &domain.binders, body);
                Facts {
                    class: region.class,
                    lanes: LaneExactness::PassOrdered,
                    pass_lanes: Some(region.lanes),
                }
            }
            O::Fold {
                domain,
                initial,
                captures,
                transition,
                ..
            } => {
                let region = self.region(initial.len(), captures, &domain.binders, transition);
                Facts {
                    class: FaultClass::HasEffect,
                    lanes: LaneExactness::PassOrdered,
                    pass_lanes: Some(region.lanes),
                }
            }
            O::Conditional {
                captures,
                if_true,
                if_false,
                ..
            } => {
                let (taken, other) = (
                    self.region(0, captures, &[], if_true),
                    self.region(0, captures, &[], if_false),
                );
                Facts::of(taken.class.join(other.class), taken.lanes.join(other.lanes))
            }
            _ => {
                let class = self.leaf_class(operation);
                Facts::of(class, self.leaf_lanes(operation, class))
            }
        }
    }

    /// The class of an operation that has no region of its own to analyse.
    fn leaf_class(&mut self, operation: &SolveOperation) -> FaultClass {
        use SolveOperation as O;
        match operation {
            O::Constant { destination, value } => {
                if let (SolveValueKind::Integer(value), SolveScalarType::Integer(domain)) =
                    (value.kind(), self.scalar(*destination))
                {
                    self.intervals[destination.index()] =
                        SolveIntegerInterval::point(domain, value);
                }
                FaultClass::CannotFault
            }
            O::Load { destination, slot } => {
                self.intervals[destination.index()] = self.slot_intervals[slot.index()];
                FaultClass::CannotFault
            }
            O::Unary {
                destination,
                operator,
                operand,
            } => self.integer_scalar(operation, *destination, |this, domain| {
                this.intervals[operand.index()]?.unary(*operator, domain)
            }),
            O::Binary {
                destination,
                operator,
                lhs,
                rhs,
            } => self.integer_scalar(operation, *destination, |this, domain| {
                this.intervals[lhs.index()]?.binary(*operator, this.intervals[rhs.index()]?, domain)
            }),
            O::Compare { .. }
            | O::Select { .. }
            | O::Fill { .. }
            | O::ConstructAggregate { .. }
            | O::Concatenate { .. }
            | O::Transpose { .. }
            | O::Identity { .. }
            | O::Diagonal { .. }
            | O::ProjectElement { .. }
            | O::ProjectSlice { .. }
            | O::UpdateSlice { .. }
            | O::SelectElement { .. } => FaultClass::CannotFault,
            O::Convert { operator, .. } => {
                if *operator == super::SolveConversionOperator::IntegerToReal {
                    FaultClass::CannotFault
                } else {
                    FaultClass::MayFault(FaultKind::IntegerConversion)
                }
            }
            O::Scale { .. }
            | O::BroadcastBinary { .. }
            | O::MatrixMultiply { .. }
            | O::Cross { .. }
            | O::Reduce { .. } => self.numeric(operation),
            O::ProjectView { axes, .. } | O::UpdateView { axes, .. } => {
                if axes
                    .iter()
                    .all(|axis| matches!(axis, SolveTensorViewAxis::Span { .. }))
                {
                    FaultClass::CannotFault
                } else {
                    FaultClass::MayFault(FaultKind::IndexBounds)
                }
            }
            O::ProjectElementDynamic {
                aggregate, indices, ..
            }
            | O::UpdateElement {
                aggregate, indices, ..
            } => self.subscript(*aggregate, indices),
            O::LinearSolve { .. } => FaultClass::MayFault(FaultKind::SingularSystem),
            _ => FaultClass::HasEffect,
        }
    }

    /// A computed subscript cannot fault exactly when the interval of each
    /// index lies inside the extent it indexes (one-based coordinates).
    fn subscript(&self, aggregate: SolveRegisterId, indices: &[SolveRegisterId]) -> FaultClass {
        let extents = self.register_types[aggregate.index()].dimensions();
        let inside = extents.len() == indices.len()
            && indices.iter().zip(extents).all(|(index, extent)| {
                self.intervals[index.index()]
                    .is_some_and(|interval| interval.within(1, i64::from(*extent)))
            });
        if inside {
            FaultClass::CannotFault
        } else {
            FaultClass::MayFault(FaultKind::IndexBounds)
        }
    }

    /// Whether the lane form of `operation` computes, for each lane, what the
    /// operation computes for that lane's operands alone (SOLVE-C104).
    fn leaf_lanes(&self, operation: &SolveOperation, class: FaultClass) -> LaneExactness {
        use SolveOperation as O;
        let cannot_fault = class == FaultClass::CannotFault;
        let exact = match operation {
            O::Constant { .. } | O::Load { .. } => true,
            O::Store { slot, .. } => {
                self.slots[slot.index()].storage() == SolveStorageClass::Output
            }
            O::Compare { lhs, .. } => self.is_scalar(*lhs),
            O::Select { destination, .. } => self.is_scalar(*destination),
            O::Convert { operator, .. } => {
                *operator == super::SolveConversionOperator::IntegerToReal
            }
            O::Unary {
                destination,
                operator,
                operand,
            } => {
                cannot_fault
                    && self.is_scalar(*destination)
                    && lane_unary(*operator, self.scalar(*operand))
            }
            O::Binary {
                destination,
                operator,
                lhs,
                ..
            } => {
                cannot_fault
                    && self.is_scalar(*destination)
                    && lane_binary(*operator, self.scalar(*lhs))
            }
            O::ProjectElement { .. }
            | O::ProjectElementDynamic { .. }
            | O::SelectElement { .. }
            | O::UpdateElement { .. } => cannot_fault,
            _ => false,
        };
        if exact {
            LaneExactness::Exact
        } else {
            LaneExactness::PassOrdered
        }
    }

    /// A tensor operation is pure and total unless it computes with Integers.
    fn numeric(&self, operation: &SolveOperation) -> FaultClass {
        if self.touches_integer(operation) {
            FaultClass::MayFault(FaultKind::IntegerArithmetic)
        } else {
            FaultClass::CannotFault
        }
    }

    /// Classify a unary or binary operation. An Integer scalar result whose
    /// interval `bound` proves inside the domain cannot fault and records the
    /// interval; any other Integer operation can fault.
    fn integer_scalar(
        &mut self,
        operation: &SolveOperation,
        destination: SolveRegisterId,
        bound: impl FnOnce(&Self, crate::SolveIntegerDomain) -> Interval,
    ) -> FaultClass {
        if !self.touches_integer(operation) {
            return FaultClass::CannotFault;
        }
        if let SolveScalarType::Integer(domain) = self.scalar(destination)
            && self.is_scalar(destination)
            && let Some(interval) = bound(self, domain)
        {
            self.intervals[destination.index()] = Some(interval);
            return FaultClass::CannotFault;
        }
        FaultClass::MayFault(FaultKind::IntegerArithmetic)
    }

    /// The facts of a region body entered with `leading` unbounded inputs
    /// (the carried tuple of a fold), then `captures`, then `binders`.
    fn region(
        &self,
        leading: usize,
        captures: &[SolveRegisterId],
        binders: &[StructuredIndexBinder],
        region: &SolveProgramRegion,
    ) -> RegionFacts {
        let body = region.body();
        let inputs = region.inputs();
        let mut intervals = vec![None; leading];
        intervals.extend(
            captures
                .iter()
                .map(|register| self.intervals[register.index()]),
        );
        for (binder, input) in binders
            .iter()
            .zip(inputs.iter().skip(leading + captures.len()))
        {
            intervals.push(match input.element_type() {
                SolveScalarType::Integer(integer) => {
                    SolveIntegerInterval::span(integer, binder.lower, binder.upper)
                }
                _ => None,
            });
        }
        let flow = FaultFlow::of(
            body.operations(),
            body.register_types(),
            body.slots(),
            &intervals,
        );
        RegionFacts {
            class: flow.region_class(body.operations(), body.slots()),
            lanes: flow.region_lanes(),
        }
    }
}

/// Whether the lane form of the unary `operator` over `scalar` is the
/// operation applied to each lane (SOLVE-C104).
fn lane_unary(operator: SolveUnaryOperator, scalar: SolveScalarType) -> bool {
    use SolveUnaryOperator as U;
    match scalar {
        SolveScalarType::Real { .. } => matches!(operator, U::Negate | U::Abs | U::Sqrt),
        SolveScalarType::Integer(_) => matches!(operator, U::Negate | U::Abs),
        SolveScalarType::Boolean => operator == U::Not,
    }
}

/// Whether the lane form of the binary `operator` over `scalar` is the
/// operation applied to each lane (SOLVE-C104).
fn lane_binary(operator: SolveBinaryOperator, scalar: SolveScalarType) -> bool {
    use SolveBinaryOperator as B;
    match scalar {
        SolveScalarType::Real { .. } => matches!(
            operator,
            B::Add | B::Subtract | B::Multiply | B::Divide | B::Min | B::Max
        ),
        SolveScalarType::Integer(_) => {
            matches!(
                operator,
                B::Add | B::Subtract | B::Multiply | B::Min | B::Max
            )
        }
        SolveScalarType::Boolean => matches!(operator, B::And | B::Or),
    }
}
