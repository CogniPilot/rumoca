//! Invariant-zero incidence proofs over the unchanged tensor expression owner.

use super::*;

/// Memoized proofs that one factor of a product is exactly zero.
///
/// Structural incidence omits a Real factor multiplied by such a factor, and
/// Solve lowering omits the same product term, so both derive the same
/// dependency structure from one proof.
#[derive(Default)]
pub struct ZeroCoefficients<'dae> {
    values: HashMap<(dae::ExprId<'dae>, usize), bool>,
    /// The literal each declaration's own continuous owner binds it to, by
    /// declaration ordinal, computed on first use.
    bindings: Option<Vec<Option<dae::ExprId<'dae>>>>,
}

impl<'dae> ZeroCoefficients<'dae> {
    /// Whether the term `(lhs_index, rhs_index)` of `lhs * rhs` is an exactly
    /// zero product of a removable Real factor, which incidence omits.
    pub fn omits_term(
        &mut self,
        view: dae::DaeView<'dae>,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        lhs_index: usize,
        rhs_index: usize,
    ) -> bool {
        self.omits_coordinate(view, rhs, lhs, lhs_index)
            || self.omits_coordinate(view, lhs, rhs, rhs_index)
    }

    /// Whether `factor` may be omitted from a product whose other factor,
    /// `coefficient`, is proven zero at `scalar`.
    pub(super) fn omits_coordinate(
        &mut self,
        view: dae::DaeView<'dae>,
        factor: dae::ExprId<'dae>,
        coefficient: dae::ExprId<'dae>,
        scalar: usize,
    ) -> bool {
        let node = view.expression(factor).unwrap();
        node.value_type().scalar_type() == dae::ScalarType::Real
            && removable_factor(view, factor)
            && self.prove(view, coefficient, scalar, &mut Vec::new())
    }

    fn prove(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        active: &mut Vec<dae::ExprId<'dae>>,
    ) -> bool {
        if let Some(&zero) = self.values.get(&(expression, scalar)) {
            return zero;
        }
        if active.contains(&expression) {
            return false;
        }
        active.push(expression);
        let zero = self.prove_operation(view, expression, scalar, active);
        active.pop();
        self.values.insert((expression, scalar), zero);
        zero
    }

    fn prove_operation(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        active: &mut Vec<dae::ExprId<'dae>>,
    ) -> bool {
        let node = view.expression(expression).unwrap();
        match node.operation() {
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Integer(0) | dae::DaeLiteral::Real(0.0),
            ) => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let variable = view.variable(parameter.into()).unwrap();
                // A parameter's `fixed` is uniform (flatten refuses non-uniform
                // parameter arrays, EF033), so this reduction is exact.
                !variable.is_tunable()
                    && variable.fixed_uniform() != Some(false)
                    && variable
                        .binding()
                        .is_some_and(|binding| self.prove(view, binding, scalar, active))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) => self
                .literal_binding(view, dae::VariableId::from(variable).index() as usize)
                .is_some_and(|value| self.prove(view, value, scalar, active)),
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => self.prove(view, operand, scalar, active),
            dae::ExpressionOperation::Array(elements) => {
                let (element, scalar) = scalar_selection::array_scalar(view, elements, scalar);
                self.prove(view, element, scalar, active)
            }
            dae::ExpressionOperation::Index { base, subscripts } => literal_index_scalar(
                view,
                base,
                subscripts,
                node.value_type().dimensions(),
                scalar,
            )
            .is_some_and(|scalar| self.prove(view, base, scalar, active)),
            dae::ExpressionOperation::Builtin {
                builtin: dae::PureBuiltin::Transpose,
                arguments,
            } => {
                let Some(matrix) = arguments.get(0) else {
                    return false;
                };
                transposed_scalar(view, matrix, scalar)
                    .is_some_and(|scalar| self.prove(view, matrix, scalar, active))
            }
            dae::ExpressionOperation::Builtin {
                builtin: builtin @ (dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2),
                arguments,
            } => {
                let (element, scalar) = scalar_selection::concatenation_scalar(
                    view,
                    arguments,
                    usize::from(builtin == dae::PureBuiltin::PromotedCat2),
                    node.value_type().dimensions(),
                    scalar,
                );
                self.prove(view, element, scalar, active)
            }
            _ => false,
        }
    }

    /// The literal a declaration's own continuous owner binds it to.
    fn literal_binding(
        &mut self,
        view: dae::DaeView<'dae>,
        variable: usize,
    ) -> Option<dae::ExprId<'dae>> {
        let bindings = self.bindings.get_or_insert_with(|| {
            literal_bindings::literal_bindings(view)
                .into_iter()
                .map(|binding| binding.map(|binding| binding.value))
                .collect()
        });
        bindings.get(variable).copied().flatten()
    }
}

/// A factor whose omission removes no evaluation effect: literals, coordinate
/// reads, and their sums, differences, products, arrays, transposes, and
/// literal-subscript selections. Division, calls, and every other operation
/// may fail or depend on a domain, so such a factor is never omitted.
fn removable_factor<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> bool {
    let Some(node) = view.expression(expression) else {
        return false;
    };
    match node.operation() {
        dae::ExpressionOperation::Literal(
            dae::DaeLiteral::Real(_) | dae::DaeLiteral::Integer(_),
        )
        | dae::ExpressionOperation::Coordinate(_) => true,
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => removable_factor(view, operand),
        dae::ExpressionOperation::Binary {
            operator:
                dae::BinaryOperator::Add | dae::BinaryOperator::Subtract | dae::BinaryOperator::Multiply,
            lhs,
            rhs,
        } => removable_factor(view, lhs) && removable_factor(view, rhs),
        dae::ExpressionOperation::Array(elements)
        | dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Transpose,
            arguments: elements,
        } => elements
            .iter()
            .all(|element| removable_factor(view, element)),
        dae::ExpressionOperation::Index { base, subscripts } => {
            subscripts.iter().all(|subscript| match subscript {
                dae::SubscriptView::Index { expression, .. } => {
                    literal_integer(view, expression).is_some()
                }
                dae::SubscriptView::Whole { .. } => true,
                dae::SubscriptView::Slice { .. } => false,
            }) && removable_factor(view, base)
        }
        _ => false,
    }
}

fn literal_integer<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> Option<i64> {
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => Some(*value),
        _ => None,
    }
}

/// The base scalar a literal-subscript selection reads for result `scalar`.
fn literal_index_scalar<'dae>(
    view: dae::DaeView<'dae>,
    base: dae::ExprId<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
    result_dimensions: &[u32],
    scalar: usize,
) -> Option<usize> {
    let base_dimensions = view.expression(base)?.value_type().dimensions();
    let result = row_major_coordinates(result_dimensions, scalar)?;
    let mut result_axis = 0;
    let mut coordinates = Vec::with_capacity(base_dimensions.len());
    for (axis, &extent) in base_dimensions.iter().enumerate() {
        match subscripts.get(axis) {
            Some(dae::SubscriptView::Index { expression, .. }) => {
                let index = literal_integer(view, expression)?;
                let coordinate = u32::try_from(index.checked_sub(1)?).ok()?;
                (coordinate < extent).then_some(())?;
                coordinates.push(coordinate);
            }
            Some(dae::SubscriptView::Whole { .. }) | None => {
                coordinates.push(*result.get(result_axis)?);
                result_axis += 1;
            }
            Some(dae::SubscriptView::Slice { .. }) => return None,
        }
    }
    flatten_coordinates(base_dimensions, &coordinates)
}

/// The scalar of a matrix whose transpose is read at result `scalar`.
fn transposed_scalar<'dae>(
    view: dae::DaeView<'dae>,
    matrix: dae::ExprId<'dae>,
    scalar: usize,
) -> Option<usize> {
    let &[rows, columns] = view.expression(matrix)?.value_type().dimensions() else {
        return None;
    };
    let (rows, columns) = (rows as usize, columns as usize);
    // The result is `columns x rows`; its (i, j) entry is the matrix's (j, i).
    let (i, j) = (scalar / rows, scalar % rows);
    (i < columns).then_some(j * columns + i)
}
