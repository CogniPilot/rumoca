//! Compact views of the existing invariant-zero proof, never per-lane bitmaps.

use std::sync::Arc;

use super::*;

/// A construction-issued zero proof with the original tensor's shape.
/// Only the incidence proof owner constructs one; consumers inspect its view.
#[derive(Clone)]
pub struct ZeroCoefficientPattern(Arc<Pattern>);

// Equality names the immutable construction-issued proof owner. Transient
// memoization cannot turn equal geometry from another derivation into proof.
// Traversal order, not these keys, determines emitted program order.
impl PartialEq for ZeroCoefficientPattern {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for ZeroCoefficientPattern {}

impl std::hash::Hash for ZeroCoefficientPattern {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(Arc::as_ptr(&self.0), state);
    }
}

struct Pattern {
    dimensions: Box<[u32]>,
    operation: Operation,
    any: bool,
    all: bool,
}

enum Operation {
    Uniform(bool),
    Array(Box<[ZeroCoefficientPattern]>),
    Concatenate(u32, Box<[ZeroCoefficientPattern]>),
    Transpose(ZeroCoefficientPattern),
    Index(ZeroCoefficientPattern, Box<[Option<i64>]>),
    Union(ZeroCoefficientPattern, ZeroCoefficientPattern),
}

/// Borrowed structure of a checked proof; Boolean constants denote proof,
/// rather than a run-time comparison of a numeric coefficient with zero.
pub enum ZeroCoefficientPatternView<'a> {
    Uniform(bool),
    Array(&'a [ZeroCoefficientPattern]),
    Concatenate(u32, &'a [ZeroCoefficientPattern]),
    Transpose(&'a ZeroCoefficientPattern),
    Index(&'a ZeroCoefficientPattern, &'a [Option<i64>]),
    Union(&'a ZeroCoefficientPattern, &'a ZeroCoefficientPattern),
}

impl ZeroCoefficientPattern {
    pub fn dimensions(&self) -> &[u32] {
        &self.0.dimensions
    }

    /// Whether any source region may carry a zero proof. A selection can
    /// conservatively retain this fact even when it selects no such region.
    pub fn may_prove_zero(&self) -> bool {
        self.0.any
    }

    pub fn proves_all_zero(&self) -> bool {
        self.0.all
    }

    pub fn view(&self) -> ZeroCoefficientPatternView<'_> {
        match &self.0.operation {
            Operation::Uniform(value) => ZeroCoefficientPatternView::Uniform(*value),
            Operation::Array(elements) => ZeroCoefficientPatternView::Array(elements),
            Operation::Concatenate(axis, elements) => {
                ZeroCoefficientPatternView::Concatenate(*axis, elements)
            }
            Operation::Transpose(base) => ZeroCoefficientPatternView::Transpose(base),
            Operation::Index(base, indices) => ZeroCoefficientPatternView::Index(base, indices),
            Operation::Union(lhs, rhs) => ZeroCoefficientPatternView::Union(lhs, rhs),
        }
    }

    fn uniform(dimensions: &[u32], zero: bool) -> Self {
        Self::new(dimensions, Operation::Uniform(zero), zero, zero)
    }

    fn new(dimensions: &[u32], operation: Operation, any: bool, all: bool) -> Self {
        Self(Arc::new(Pattern {
            dimensions: dimensions.into(),
            operation,
            any,
            all,
        }))
    }

    fn in_shape(self, dimensions: &[u32]) -> Self {
        if self.dimensions() == dimensions {
            return self;
        }
        // A scalar uniform declaration binding broadcasts; never silently
        // reshape a nonuniform proof with a different coordinate meaning.
        match self.view() {
            ZeroCoefficientPatternView::Uniform(zero) => Self::uniform(dimensions, zero),
            _ => Self::uniform(dimensions, false),
        }
    }

    fn aggregate(dimensions: &[u32], axis: Option<u32>, elements: Vec<Self>) -> Self {
        let any = elements.iter().any(Self::may_prove_zero);
        let all = elements.iter().all(Self::proves_all_zero);
        if !any || all {
            return Self::uniform(dimensions, all);
        }
        let operation = match axis {
            Some(axis) => Operation::Concatenate(axis, elements.into()),
            None => Operation::Array(elements.into()),
        };
        Self::new(dimensions, operation, any, all)
    }

    fn union(self, rhs: Self) -> Self {
        if self.proves_all_zero() || !rhs.may_prove_zero() {
            return self;
        }
        if rhs.proves_all_zero() || !self.may_prove_zero() {
            return rhs;
        }
        let dimensions = self.dimensions().to_vec();
        Self::new(&dimensions, Operation::Union(self, rhs), true, false)
    }
}

impl<'dae> ZeroCoefficients<'dae> {
    /// A compact omission proof for `coefficient * factor`. Unsupported
    /// coefficient regions remain false; domain/effectful factors refuse the
    /// proof entirely, exactly as the indexed incidence query does.
    pub fn omission_pattern(
        &mut self,
        view: dae::DaeView<'dae>,
        factor: dae::ExprId<'dae>,
        coefficient: dae::ExprId<'dae>,
    ) -> Option<ZeroCoefficientPattern> {
        let node = view.expression(factor)?;
        (node.value_type().scalar_type() == dae::ScalarType::Real
            && self.removable_factor(view, factor))
        .then(|| self.pattern(view, coefficient, Reach::Coefficient, &mut Vec::new()))
    }

    fn pattern(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        reach: Reach,
        active: &mut Vec<dae::ExprId<'dae>>,
    ) -> ZeroCoefficientPattern {
        if let Some(value) = self.patterns.get(&(expression, reach)) {
            return value.clone();
        }
        let dimensions = view
            .expression(expression)
            .unwrap()
            .value_type()
            .dimensions();
        if active.contains(&expression) {
            return ZeroCoefficientPattern::uniform(dimensions, false);
        }
        let result = if dimensions.is_empty() {
            // Scalar leaves use the existing proof, including OwnerValue's
            // exact rules; there is no second numeric-zero interpretation.
            ZeroCoefficientPattern::uniform(
                dimensions,
                self.prove(view, expression, 0, reach, &mut Vec::new()),
            )
        } else {
            active.push(expression);
            let result = self.pattern_operation(view, expression, reach, active);
            active.pop();
            result
        };
        let result = result.in_shape(dimensions);
        self.patterns.insert((expression, reach), result.clone());
        result
    }

    fn pattern_operation(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        reach: Reach,
        active: &mut Vec<dae::ExprId<'dae>>,
    ) -> ZeroCoefficientPattern {
        let node = view.expression(expression).unwrap();
        let dimensions = node.value_type().dimensions();
        match node.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let variable = view.variable(parameter.into()).unwrap();
                if !variable.is_tunable()
                    && variable.fixed_uniform() != Some(false)
                    && let Some(binding) = variable.binding()
                {
                    return self.pattern(view, binding, Reach::Coefficient, active);
                }
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) => {
                let variable = dae::VariableId::from(variable).index() as usize;
                let literal = self
                    .literal_binding(view, variable)
                    .map(|value| self.pattern(view, value, reach, active))
                    .unwrap_or_else(|| ZeroCoefficientPattern::uniform(dimensions, false));
                let owner = self
                    .owner_binding(view, variable)
                    .map(|value| self.pattern(view, value, Reach::OwnerValue, active))
                    .unwrap_or_else(|| ZeroCoefficientPattern::uniform(dimensions, false));
                return literal.union(owner);
            }
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => return self.pattern(view, operand, reach, active),
            dae::ExpressionOperation::Builtin {
                builtin: builtin @ (dae::PureBuiltin::Smooth | dae::PureBuiltin::NoEvent),
                arguments,
            } => {
                if let Some(value) = arguments.get(usize::from(builtin == dae::PureBuiltin::Smooth))
                {
                    return self.pattern(view, value, reach, active);
                }
            }
            dae::ExpressionOperation::Array(elements) => {
                let children = elements
                    .iter()
                    .map(|value| self.pattern(view, value, reach, active))
                    .collect();
                return ZeroCoefficientPattern::aggregate(dimensions, None, children);
            }
            dae::ExpressionOperation::Builtin {
                builtin: builtin @ (dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2),
                arguments,
            } => {
                let children = arguments
                    .iter()
                    .map(|value| self.pattern(view, value, reach, active))
                    .collect();
                return ZeroCoefficientPattern::aggregate(
                    dimensions,
                    Some(u32::from(builtin == dae::PureBuiltin::PromotedCat2)),
                    children,
                );
            }
            dae::ExpressionOperation::Builtin {
                builtin: dae::PureBuiltin::Transpose,
                arguments,
            } => {
                if let Some(matrix) = arguments.get(0)
                    && view
                        .expression(matrix)
                        .unwrap()
                        .value_type()
                        .dimensions()
                        .len()
                        == 2
                {
                    let base = self.pattern(view, matrix, reach, active);
                    return ZeroCoefficientPattern::new(
                        dimensions,
                        Operation::Transpose(base.clone()),
                        base.0.any,
                        base.0.all,
                    );
                }
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                if let Some(indices) = literal_pattern_indices(view, base, subscripts) {
                    let base = self.pattern(view, base, reach, active);
                    return ZeroCoefficientPattern::new(
                        dimensions,
                        Operation::Index(base.clone(), indices.into()),
                        base.0.any,
                        base.0.all,
                    );
                }
            }
            _ => {}
        }
        ZeroCoefficientPattern::uniform(dimensions, false)
    }
}

pub(super) fn literal_pattern_indices<'dae>(
    view: dae::DaeView<'dae>,
    base: dae::ExprId<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
) -> Option<Vec<Option<i64>>> {
    let dimensions = view.expression(base)?.value_type().dimensions();
    subscripts
        .iter()
        .enumerate()
        .map(|(axis, subscript)| match subscript {
            dae::SubscriptView::Index { expression, .. } => {
                let index = literal_integer(view, expression)?;
                (index > 0 && index <= i64::from(*dimensions.get(axis)?)).then_some(Some(index))
            }
            dae::SubscriptView::Whole { .. } => Some(None),
            dae::SubscriptView::Slice { .. } => None,
        })
        .collect()
}
