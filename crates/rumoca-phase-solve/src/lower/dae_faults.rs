//! The one fault classification of a DAE expression (SPEC_0040 SOLVE-C61,
//! SOLVE-C102, DAE-C22).
//!
//! Whether an expression may be evaluated unconditionally, because evaluating
//! it can neither fail nor act, is decided here and nowhere else. The answer
//! is a [`solve::FaultClass`], the vocabulary of the typed program, and every
//! Integer bound comes from [`solve::SolveIntegerInterval`], so a DAE
//! expression and the typed operations it lowers to are classified by the same
//! rules; `typed_functions/tests/dae_faults.rs` lowers every operator class and checks that
//! the two classifications agree.
//!
//! A conditional whose arms are all `CannotFault` lowers to selections that
//! evaluate every arm; any other arm is lowered in its own region, entered
//! only when selected.

use std::collections::HashMap;

use rumoca_core::StructuredIndexBinder;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use solve::{FaultClass, FaultKind};

/// The class of every expression classified so far, memoized over the
/// expression graph.
pub(super) type FaultMemo<'dae> = HashMap<dae::ExprId<'dae>, FaultClass>;

/// Domain points of the scalar row being lowered: a binder with a point
/// contributes that exact value, any other binder its declared `for` range.
pub(super) type DomainPoints<'dae> = [(dae::DomainId<'dae>, Vec<i64>)];

/// How a coordinate read is computed.
pub(super) enum CoordinateRead<'dae> {
    /// A plain read of a stored value.
    Free,
    /// A read computed by the expression of a same-tick definition.
    Defined(dae::ExprId<'dae>),
    /// A read whose computation is not proven acyclic.
    Unproven,
}

/// The owner of the coordinate reads of the lowering being classified.
pub(super) trait CoordinateReads<'dae> {
    fn read(&self, coordinate: dae::CoordinateView<'dae>) -> CoordinateRead<'dae>;
}

/// Every coordinate is a plain read.
struct StoredCoordinates;

impl<'dae> CoordinateReads<'dae> for StoredCoordinates {
    fn read(&self, _: dae::CoordinateView<'dae>) -> CoordinateRead<'dae> {
        CoordinateRead::Free
    }
}

/// Classifies expressions of one DAE under the Integer domain of the lowering's
/// arithmetic profile.
#[derive(Clone, Copy)]
pub(super) struct DaeFaults<'a, 'dae> {
    view: dae::DaeView<'dae>,
    points: &'a DomainPoints<'dae>,
    coordinates: &'a dyn CoordinateReads<'dae>,
    differentiable: bool,
    scalar_signum: bool,
}

impl<'a, 'dae> DaeFaults<'a, 'dae> {
    pub(super) fn new(view: dae::DaeView<'dae>, points: &'a DomainPoints<'dae>) -> Self {
        Self {
            view,
            points,
            coordinates: &StoredCoordinates,
            differentiable: false,
            scalar_signum: false,
        }
    }

    /// Classify coordinate reads through `coordinates`.
    pub(super) fn with_coordinates(self, coordinates: &'a dyn CoordinateReads<'dae>) -> Self {
        Self {
            coordinates,
            ..self
        }
    }

    /// Also treat a builtin off its domain (`sqrt`, `log`, `asin`, `acos`) as
    /// faulting: its value is a NaN, but a consumer differentiating through an
    /// eager selection would take a non-finite partial from the unselected arm,
    /// so such an arm is evaluated only when selected.
    pub(super) fn differentiable(self) -> Self {
        Self {
            differentiable: true,
            ..self
        }
    }

    /// Classify for the scalar model path, which evaluates `sign` of a Real as
    /// the Real-valued signum: its value is -1, 0, 1 or NaN and no Integer
    /// conversion is performed, so it cannot fault.
    pub(super) fn scalar_signum(self) -> Self {
        Self {
            scalar_signum: true,
            ..self
        }
    }

    /// Whether evaluating `root` can neither fail nor act.
    pub(super) fn is_total(self, memo: &mut FaultMemo<'dae>, root: dae::ExprId<'dae>) -> bool {
        self.class(memo, root).is_prunable()
    }

    /// The class of evaluating `root`: the join of its own operation with
    /// every operand, computed iteratively since the graph may be deep.
    pub(super) fn class(self, memo: &mut FaultMemo<'dae>, root: dae::ExprId<'dae>) -> FaultClass {
        let mut stack = vec![(root, false)];
        while let Some((expression, expanded)) = stack.pop() {
            if memo.contains_key(&expression) {
                continue;
            }
            let Some(node) = self.view.expression(expression) else {
                memo.insert(expression, FaultClass::HasEffect);
                continue;
            };
            let (own, operands) = self.own_class(expression, node);
            if own == FaultClass::HasEffect {
                memo.insert(expression, own);
            } else if expanded {
                let class = join_operands(memo, own, &operands);
                memo.insert(expression, class);
            } else {
                stack.push((expression, true));
                stack.extend(unvisited(memo, operands));
            }
        }
        memo.get(&root).copied().unwrap_or(FaultClass::HasEffect)
    }

    /// The class of the node's own operation, and the operands whose classes
    /// join into it.
    fn own_class(
        self,
        id: dae::ExprId<'dae>,
        node: dae::ExpressionView<'dae>,
    ) -> (FaultClass, Vec<dae::ExprId<'dae>>) {
        use dae::ExpressionOperation as O;
        let value_type = node.value_type();
        let integer = value_type.scalar_type() == dae::ScalarType::Integer;
        let bounded = || self.integer_interval(id).is_some();
        let integer_arithmetic = || {
            if integer && !bounded() {
                FaultClass::MayFault(FaultKind::IntegerArithmetic)
            } else {
                FaultClass::CannotFault
            }
        };
        match node.operation() {
            O::Literal(dae::DaeLiteral::String(_)) => (FaultClass::HasEffect, Vec::new()),
            O::Coordinate(coordinate) => match self.coordinates.read(coordinate) {
                CoordinateRead::Free => (FaultClass::CannotFault, Vec::new()),
                CoordinateRead::Defined(definition) => (FaultClass::CannotFault, vec![definition]),
                CoordinateRead::Unproven => (FaultClass::HasEffect, Vec::new()),
            },
            O::Literal(_)
            | O::FunctionValue { .. }
            | O::FunctionFoldParameter { .. }
            | O::FunctionConditionalOutput { .. } => (FaultClass::CannotFault, Vec::new()),
            O::Unary { operator, operand } => {
                let own = if operator == dae::UnaryOperator::Negate {
                    integer_arithmetic()
                } else {
                    FaultClass::CannotFault
                };
                (own, vec![operand])
            }
            O::Binary { lhs, rhs, .. } => (integer_arithmetic(), vec![lhs, rhs]),
            O::Field { base, .. } => (FaultClass::CannotFault, vec![base]),
            O::Conditional(operands) | O::Array(operands) | O::Record(operands) => {
                (FaultClass::CannotFault, operands.iter().collect())
            }
            O::Builtin { builtin, arguments } => (
                self.builtin_class(builtin, integer, id, self.real_first_argument(arguments)),
                arguments.iter().collect(),
            ),
            O::Index { base, subscripts } => (self.index_class(base, subscripts), vec![base]),
            O::ArrayUpdate { base, value, .. } => (
                FaultClass::MayFault(FaultKind::IndexBounds),
                vec![base, value],
            ),
            _ => (FaultClass::HasEffect, Vec::new()),
        }
    }

    /// A pure builtin computes with IEEE Reals unless its result is an Integer:
    /// then it is checked Integer arithmetic (bounded by an interval) or a
    /// conversion from Real.
    fn builtin_class(
        self,
        builtin: dae::PureBuiltin,
        integer: bool,
        id: dae::ExprId<'dae>,
        real_first_argument: bool,
    ) -> FaultClass {
        use dae::PureBuiltin as B;
        let arithmetic = FaultClass::MayFault(FaultKind::IntegerArithmetic);
        match builtin {
            B::Integer => FaultClass::MayFault(FaultKind::IntegerConversion),
            B::LinearSolve => FaultClass::MayFault(FaultKind::SingularSystem),
            B::Sqrt | B::Log | B::Log10 | B::Asin | B::Acos if self.differentiable => {
                FaultClass::MayFault(FaultKind::RealDomain)
            }
            B::Div | B::Mod | B::Rem if integer => arithmetic,
            B::Sign if real_first_argument && self.scalar_signum => FaultClass::CannotFault,
            B::Abs | B::Sign | B::Min | B::Max
                if integer && self.integer_interval(id).is_none() =>
            {
                arithmetic
            }
            B::Sum | B::Product | B::Cross | B::OuterProduct | B::Skew if integer => arithmetic,
            _ => FaultClass::CannotFault,
        }
    }

    /// Whether the first argument of a builtin is not an Integer.
    fn real_first_argument(self, arguments: dae::ExpressionOperands<'dae>) -> bool {
        arguments
            .get(0)
            .and_then(|argument| self.view.expression(argument))
            .is_some_and(|argument| argument.value_type().scalar_type() != dae::ScalarType::Integer)
    }

    /// An indexed read cannot fail when every subscript is a whole axis or an
    /// Integer whose interval lies inside the extent it indexes.
    fn index_class(
        self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
    ) -> FaultClass {
        let Some(base) = self.view.expression(base) else {
            return FaultClass::HasEffect;
        };
        let extents = base.value_type().dimensions();
        let in_range = subscripts.len() <= extents.len()
            && subscripts
                .iter()
                .zip(extents)
                .all(|(subscript, extent)| match subscript {
                    dae::SubscriptView::Whole { .. } => true,
                    dae::SubscriptView::Index { expression, .. } => self
                        .integer_interval(expression)
                        .is_some_and(|interval| interval.within(1, i64::from(*extent))),
                    dae::SubscriptView::Slice { .. } => false,
                });
        if in_range {
            FaultClass::CannotFault
        } else {
            FaultClass::MayFault(FaultKind::IndexBounds)
        }
    }

    /// The interval an Integer expression provably stays within, from literals
    /// and binders alone, bounded by the Integer domain of the arithmetic
    /// profile. The operations are the interval operations of the Solve IR.
    pub(super) fn integer_interval(
        self,
        expression: dae::ExprId<'dae>,
    ) -> Option<solve::SolveIntegerInterval> {
        let domain = super::typed_functions::arithmetic_profile().integer_domain();
        let node = self.view.expression(expression)?;
        if !node.value_type().dimensions().is_empty() {
            return None;
        }
        match node.operation() {
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => {
                solve::SolveIntegerInterval::point(domain, *value)
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                let (low, high) = self.binder_bounds(binder)?;
                solve::SolveIntegerInterval::span(domain, low, high)
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let operand = self.integer_interval(operand)?;
                match operator {
                    dae::UnaryOperator::Plus => Some(operand),
                    dae::UnaryOperator::Negate => operand.negate(domain),
                    dae::UnaryOperator::Not => None,
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                let operator = match operator {
                    dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => {
                        solve::SolveBinaryOperator::Add
                    }
                    dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
                        solve::SolveBinaryOperator::Subtract
                    }
                    dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
                        solve::SolveBinaryOperator::Multiply
                    }
                    _ => return None,
                };
                self.integer_interval(lhs)?
                    .binary(operator, self.integer_interval(rhs)?, domain)
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                self.builtin_interval(builtin, arguments, domain)
            }
            _ => None,
        }
    }

    fn builtin_interval(
        self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        domain: solve::SolveIntegerDomain,
    ) -> Option<solve::SolveIntegerInterval> {
        let first = self.integer_interval(arguments.get(0)?)?;
        let operator = match builtin {
            dae::PureBuiltin::Abs => return first.unary(solve::SolveUnaryOperator::Abs, domain),
            dae::PureBuiltin::Sign => {
                return first.unary(solve::SolveUnaryOperator::Sign, domain);
            }
            dae::PureBuiltin::Min => solve::SolveBinaryOperator::Min,
            dae::PureBuiltin::Max => solve::SolveBinaryOperator::Max,
            _ => return None,
        };
        if arguments.len() != 2 {
            return None;
        }
        first.binary(operator, self.integer_interval(arguments.get(1)?)?, domain)
    }

    fn binder_bounds(self, binder: dae::DomainBinderId<'dae>) -> Option<(i64, i64)> {
        let ordinal = binder.ordinal() as usize;
        if let Some((_, point)) = self
            .points
            .iter()
            .rev()
            .find(|(domain, _)| *domain == binder.domain())
        {
            let value = *point.get(ordinal)?;
            return Some((value, value));
        }
        let StructuredIndexBinder { lower, upper, .. } = self
            .view
            .domain(binder.domain())?
            .structured()
            .binders
            .get(ordinal)?;
        Some((*lower, *upper))
    }
}

/// The class of a node whose own operation is `own` joined with the classes
/// of its already classified operands.
fn join_operands<'dae>(
    memo: &FaultMemo<'dae>,
    own: FaultClass,
    operands: &[dae::ExprId<'dae>],
) -> FaultClass {
    operands.iter().fold(own, |class, operand| {
        class.join(memo.get(operand).copied().unwrap_or(FaultClass::HasEffect))
    })
}

/// The operands not yet classified, queued for classification.
fn unvisited<'dae>(
    memo: &FaultMemo<'dae>,
    operands: Vec<dae::ExprId<'dae>>,
) -> Vec<(dae::ExprId<'dae>, bool)> {
    operands
        .into_iter()
        .filter(|operand| !memo.contains_key(operand))
        .map(|operand| (operand, false))
        .collect()
}
