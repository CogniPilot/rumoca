//! Composed tensor contractions: `A*X*A'` read as one node, not as a nest.
//!
//! A quadratic form is a contraction whose left operand is itself a
//! contraction. Its value `(A*X)[row][contracted]` is reused by every result
//! column, so the composed node materializes it once as an explicit rank-1
//! tensor over the contracted axis and contracts that tensor with the right
//! operand. Both facts are read off the checked DAE at the node: the operand's
//! own operation says it is a contraction, its operand shapes say what the
//! intermediate's extent is, and this node's own outer index says the
//! intermediate is replicated across something.
//!
//! Nothing here inspects a lowered statement. The intermediate is an IR value
//! with its own declaration and its own liveness, decided before either
//! operand is projected, which is what retires the loop fission that used to
//! recover the same shape by splitting an already-emitted body.

use super::*;

/// One composed contraction: the operands of the left contraction, and the
/// contraction shape projecting it at this node's contracted index takes.
pub(super) struct ComposedContraction<'dae> {
    pub(super) lhs: dae::ExprId<'dae>,
    pub(super) rhs: dae::ExprId<'dae>,
    pub(super) contraction: TensorContraction,
    pub(super) scalar_type: gast::ScalarType,
    pub(super) span: Span,
}

/// Where a materialized contraction accumulates.
pub(super) enum ContractionAccumulator {
    /// A scalar temporary the contraction declares for itself.
    Fresh,
    /// One element of the intermediate tensor a composing contraction
    /// declared, which the composing node reads back at the same index.
    Element(Box<IntermediateElement>),
}

/// The element of an intermediate tensor one contracted value owns, with the
/// shape of the tensor it belongs to.
pub(super) struct IntermediateElement {
    /// The intermediate tensor.
    pub(super) name: gast::Name,
    /// Elements in it: one per value of the composing node's contracted index.
    pub(super) count: u32,
    /// That index, which this contraction never reads.
    pub(super) row: gast::Name,
    /// The element this contraction accumulates into.
    pub(super) reference: gast::Reference,
}

/// Attest that this contraction's loop is a whole row of a matrix product, so
/// the target may accumulate the intermediate run at once instead of walking a
/// column per element.
///
/// Three facts, all owned by this node: the accumulator is the intermediate
/// tensor it was handed, which is a temporary the composing node declared and
/// therefore aliases neither operand; the coefficient is the left operand
/// projected without the row index, so no value of that index can change it,
/// and it is refused unless it is pure arithmetic, because the row form
/// evaluates it once per contracted value rather than once per element; and
/// the right operand is a reference whose last subscript is the row index, so
/// dropping it names one run per contracted value.
pub(super) fn attest_row_contraction(
    element: &IntermediateElement,
    iterator: &gast::Name,
    extent: u32,
    scale: &gast::Expression,
    source: &gast::Expression,
) -> Option<gast::RowContraction> {
    if !is_hoistable_coefficient(scale) || mentions(scale, &element.row) {
        return None;
    }
    let gast::Expression::Ref(gast::Reference::Local(part)) = source else {
        return None;
    };
    let (last, rest) = part.subscripts.split_last()?;
    if !is_reference_to(last, &element.row)
        || rest.iter().any(|index| mentions(index, &element.row))
    {
        return None;
    }
    Some(gast::RowContraction::new(
        element.name.clone(),
        element.count,
        iterator.clone(),
        extent,
    ))
}

/// Whether an expression may be evaluated once per contracted value instead of
/// once per element: literals, reads and arithmetic only. A call or a
/// comparison would have its evaluation count changed, and a conditional holds
/// a legalized twin that a rewrite of one arm would leave stale.
fn is_hoistable_coefficient(value: &gast::Expression) -> bool {
    match value {
        gast::Expression::Bool(_) | gast::Expression::Integer(_) | gast::Expression::Real(_) => {
            true
        }
        gast::Expression::Ref(_) | gast::Expression::Neg(_) => true,
        gast::Expression::Paren(inner) => is_hoistable_coefficient(inner),
        gast::Expression::Binary { op, lhs, rhs } => {
            matches!(
                op,
                gast::BinaryOp::Add
                    | gast::BinaryOp::Sub
                    | gast::BinaryOp::Mul
                    | gast::BinaryOp::Div
                    | gast::BinaryOp::Pow
            ) && is_hoistable_coefficient(lhs)
                && is_hoistable_coefficient(rhs)
        }
        gast::Expression::Size { .. }
        | gast::Expression::Call(_)
        | gast::Expression::Not(_)
        | gast::Expression::If(_)
        | gast::Expression::Array(_) => false,
    }
}

fn is_reference_to(value: &gast::Expression, name: &gast::Name) -> bool {
    matches!(
        value,
        gast::Expression::Ref(gast::Reference::Local(part))
            if part.name == *name && part.subscripts.is_empty()
    )
}

fn mentions(value: &gast::Expression, name: &gast::Name) -> bool {
    match value {
        gast::Expression::Bool(_) | gast::Expression::Integer(_) | gast::Expression::Real(_) => {
            false
        }
        gast::Expression::Ref(reference) | gast::Expression::Neg(reference) => {
            mentions_in_reference(reference, name)
        }
        gast::Expression::Size { array, dimension } => {
            mentions_in_reference(array, name) || mentions(dimension, name)
        }
        gast::Expression::Call(call) => call.arguments.iter().any(|value| mentions(value, name)),
        gast::Expression::Paren(inner) | gast::Expression::Not(inner) => mentions(inner, name),
        gast::Expression::If(selection) => {
            selection
                .branches
                .iter()
                .any(|(condition, value)| mentions(condition, name) || mentions(value, name))
                || mentions(&selection.else_value, name)
        }
        gast::Expression::Array(values) => values.iter().any(|value| mentions(value, name)),
        gast::Expression::Binary { lhs, rhs, .. } => mentions(lhs, name) || mentions(rhs, name),
    }
}

fn mentions_in_reference(reference: &gast::Reference, name: &gast::Name) -> bool {
    let parts = match reference {
        gast::Reference::Local(part) => std::slice::from_ref(part),
        gast::Reference::State(parts) => parts.as_slice(),
    };
    parts
        .iter()
        .any(|part| part.name == *name || part.subscripts.iter().any(|index| mentions(index, name)))
}

/// Read `lhs` as the left contraction of a composed contraction, or `None`
/// when this node is not one.
///
/// Four conditions, all answered by the checked DAE and this node's own shape:
///
/// * the node replicates over an outer index on the right, so an intermediate
///   value is reused rather than consumed once;
/// * the left operand is a matrix, so projecting it names a row of a matrix
///   rather than an element of a vector;
/// * the left operand's own operation is `*`; and
/// * its operand shapes make that `*` a contraction at this node's own
///   contracted index.
pub(super) fn composed_left_contraction<'dae>(
    view: dae::DaeView<'dae>,
    lhs: dae::ExprId<'dae>,
    contraction: &TensorContraction,
    contracted: &gast::Expression,
) -> Option<ComposedContraction<'dae>> {
    if contraction.rhs_outer.is_empty() || !contraction.lhs_matrix {
        return None;
    }
    let node = view.expression(lhs)?;
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Multiply,
        lhs: inner_lhs,
        rhs: inner_rhs,
    } = node.operation()
    else {
        return None;
    };
    let indices = [contraction.lhs_outer.first()?.clone(), contracted.clone()];
    let inner = tensor_contraction(
        view.expression(inner_lhs)?.value_type().dimensions(),
        view.expression(inner_rhs)?.value_type().dimensions(),
        &indices,
    )?;
    let span = node.provenance().span();
    let scalar_type = scalar_type(node.value_type().scalar_type(), "<contraction>", span).ok()?;
    Some(ComposedContraction {
        lhs: inner_lhs,
        rhs: inner_rhs,
        contraction: inner,
        scalar_type,
        span,
    })
}

/// The element of an intermediate tensor one contracted value owns.
pub(super) fn intermediate_element(
    name: &gast::Name,
    contracted: &gast::Expression,
) -> gast::Reference {
    let mut part = gast::RefPart::plain(name.clone());
    part.subscripts.push(contracted.clone());
    gast::Reference::Local(part)
}
