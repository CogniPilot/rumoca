//! Declarations their own continuous owner binds to a literal.
//!
//! A continuous residual owner, or a single-body row-major structured family,
//! whose residual is `v - L` or `L - v` (under unary signs) for a whole
//! continuous algebraic declaration `v` and a literal-structured `L` of the same
//! shape states `v = L` at every instant. Consumers read `v` as `L`; the owner
//! itself stays the definition of `v`.

use rumoca_ir_dae as dae;

/// One literal binding: the declaration read of the binding owner and the
/// literal it equals.
#[derive(Clone, Copy, Debug)]
pub struct LiteralBinding<'dae> {
    pub access: dae::ExprId<'dae>,
    pub value: dae::ExprId<'dae>,
}

/// The unique literal binding of each declaration, by declaration ordinal. A
/// declaration two owners bind is left unbound.
pub fn literal_bindings(view: dae::DaeView<'_>) -> Vec<Option<LiteralBinding<'_>>> {
    let mut bindings = vec![None; view.variable_count()];
    let mut repeated = vec![false; view.variable_count()];
    for owner in view.continuous_owners() {
        let root = match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => equation.residual(),
            dae::ContinuousOwnerView::Structured { family, .. } => {
                let mut bodies = family.bodies().iter();
                match (family.scalar_view(), bodies.next(), bodies.next()) {
                    (
                        rumoca_core::ComprehensionScalarView::RowMajorProjection,
                        Some(body),
                        None,
                    ) => body,
                    _ => continue,
                }
            }
        };
        let Some((variable, binding)) = binding_of(view, root) else {
            continue;
        };
        if bindings[variable].is_some() {
            repeated[variable] = true;
        }
        bindings[variable] = Some(binding);
    }
    for (binding, repeated) in bindings.iter_mut().zip(repeated) {
        if repeated {
            *binding = None;
        }
    }
    bindings
}

fn binding_of<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
) -> Option<(usize, LiteralBinding<'dae>)> {
    let mut residual = root;
    while let dae::ExpressionOperation::Unary {
        operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
        operand,
    } = view.expression(residual)?.operation()
    {
        residual = operand;
    }
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = view.expression(residual)?.operation()
    else {
        return None;
    };
    whole_declaration(view, lhs, rhs).or_else(|| whole_declaration(view, rhs, lhs))
}

fn whole_declaration<'dae>(
    view: dae::DaeView<'dae>,
    access: dae::ExprId<'dae>,
    value: dae::ExprId<'dae>,
) -> Option<(usize, LiteralBinding<'dae>)> {
    let node = view.expression(access)?;
    let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) =
        node.operation()
    else {
        return None;
    };
    let literal = view.expression(value)?;
    (node.function_scope().is_none()
        && node.binder_domain().is_none()
        && node.value_type().scalar_type() == dae::ScalarType::Real
        && literal.value_type().dimensions() == node.value_type().dimensions()
        && is_literal_structure(view, value))
    .then(|| {
        (
            dae::VariableId::from(variable).index() as usize,
            LiteralBinding { access, value },
        )
    })
}

/// A numeric literal, or arrays and unary signs of literal structure.
pub(crate) fn is_literal_structure<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    let Some(node) = view.expression(expression) else {
        return false;
    };
    match node.operation() {
        dae::ExpressionOperation::Literal(
            dae::DaeLiteral::Real(_) | dae::DaeLiteral::Integer(_),
        ) => true,
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => is_literal_structure(view, operand),
        dae::ExpressionOperation::Array(elements) => elements
            .iter()
            .all(|element| is_literal_structure(view, element)),
        _ => false,
    }
}
