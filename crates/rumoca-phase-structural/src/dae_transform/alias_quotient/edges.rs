//! Alias-edge recognition over continuous owners (SPEC_0040 STRUCT-T02).
//!
//! A scalar residual owner is an edge when its additive normal form is two
//! whole coordinates with coefficients ±1. A structured family is an edge when
//! its one body has that form over two coordinates indexed by exactly the
//! family's own binders, in the same order, and the family domain covers every
//! element: the family then states the same relation for the whole arrays.

use rumoca_ir_dae as dae;

use super::MemberFacts;

/// One side of an alias edge: the declaration and the source term reading it.
#[derive(Clone, Copy, Debug)]
pub(super) struct AliasTerm {
    pub(super) variable: u32,
    pub(super) expression: u32,
}

/// One alias edge: `first = ±second` stated by one continuous owner body.
#[derive(Clone, Copy, Debug)]
pub(super) struct AliasEdge {
    pub(super) owner: usize,
    pub(super) body: Option<usize>,
    pub(super) residual: u32,
    pub(super) first: AliasTerm,
    pub(super) second: AliasTerm,
    pub(super) negated: bool,
}

pub(super) fn alias_edges(view: dae::DaeView<'_>, members: &[MemberFacts]) -> Vec<AliasEdge> {
    view.continuous_owners()
        .enumerate()
        .filter_map(|(owner, continuous)| match continuous {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                alias_edge(view, owner, None, equation.residual(), members)
            }
            dae::ContinuousOwnerView::Structured { family, .. } => {
                let mut bodies = family.bodies().iter();
                let body = bodies.next()?;
                if bodies.next().is_some() || !covering_domain(view, family) {
                    return None;
                }
                alias_edge(view, owner, Some(family), body, members)
            }
        })
        .collect()
}

/// The family domain is a root domain, so its binders enumerate distinct
/// index tuples of one array exactly once when they index it in full.
fn covering_domain<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
) -> bool {
    view.domain(family.domain())
        .is_some_and(|domain| domain.parent().is_none())
}

/// Recognize `±first ± second = 0` over two eligible coordinate accesses.
fn alias_edge<'dae>(
    view: dae::DaeView<'dae>,
    owner: usize,
    family: Option<dae::StructuredFamilyView<'dae>>,
    residual: dae::ExprId<'dae>,
    members: &[MemberFacts],
) -> Option<AliasEdge> {
    let mut terms = Vec::new();
    if !additive_terms(view, residual, false, 0, &mut terms) {
        return None;
    }
    let [(first, first_negated), (second, second_negated)] = terms.as_slice() else {
        return None;
    };
    let first_access = coordinate_access(view, *first, family, members)?;
    let second_access = coordinate_access(view, *second, family, members)?;
    (first_access.variable != second_access.variable
        && first_access.value_type == second_access.value_type
        && first_access.binders == second_access.binders)
        .then_some(AliasEdge {
            owner,
            body: family.map(|_| 0),
            residual: residual.index(),
            first: AliasTerm {
                variable: first_access.variable,
                expression: first.index(),
            },
            second: AliasTerm {
                variable: second_access.variable,
                expression: second.index(),
            },
            // `a + b = 0` and `-a - b = 0` state a negation; mixed signs a copy.
            negated: first_negated == second_negated,
        })
}

const MAX_TERM_DEPTH: usize = 8;

/// Flatten unary signs and additive operators into signed terms, dropping
/// literal zeros. Returns `false` once more than two terms or a deep tree make
/// the residual something other than an alias.
fn additive_terms<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    negated: bool,
    depth: usize,
    terms: &mut Vec<(dae::ExprId<'dae>, bool)>,
) -> bool {
    if depth > MAX_TERM_DEPTH || terms.len() > 2 {
        return false;
    }
    let Some(node) = view.expression(expression) else {
        return false;
    };
    match node.operation() {
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus,
            operand,
        } => additive_terms(view, operand, negated, depth + 1, terms),
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Negate,
            operand,
        } => additive_terms(view, operand, !negated, depth + 1, terms),
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Add,
            lhs,
            rhs,
        } => {
            additive_terms(view, lhs, negated, depth + 1, terms)
                && additive_terms(view, rhs, negated, depth + 1, terms)
        }
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Subtract,
            lhs,
            rhs,
        } => {
            additive_terms(view, lhs, negated, depth + 1, terms)
                && additive_terms(view, rhs, !negated, depth + 1, terms)
        }
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) if *value == 0.0 => true,
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(0)) => true,
        _ => {
            terms.push((expression, negated));
            terms.len() <= 2
        }
    }
}

struct CoordinateAccess<'dae> {
    variable: u32,
    /// Structural value type (scalar kind and shape); declared quantity and
    /// unit types may differ across an equation, which already equates them.
    value_type: &'dae dae::ValueType,
    binders: Vec<u32>,
}

/// A whole eligible coordinate read by a scalar residual or a row-major
/// projected tensor equation, or an eligible array read element-wise by
/// exactly the binders of a binder-substitution family.
fn coordinate_access<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    family: Option<dae::StructuredFamilyView<'dae>>,
    members: &[MemberFacts],
) -> Option<CoordinateAccess<'dae>> {
    let node = view.expression(expression)?;
    let (base, binders) = match (family, node.operation()) {
        (None, dae::ExpressionOperation::Coordinate(_)) => (expression, Vec::new()),
        (Some(family), dae::ExpressionOperation::Coordinate(_))
            if family.scalar_view() == rumoca_core::ComprehensionScalarView::RowMajorProjection =>
        {
            (expression, Vec::new())
        }
        (Some(family), dae::ExpressionOperation::Index { base, subscripts })
            if family.scalar_view() == rumoca_core::ComprehensionScalarView::BinderSubstitution =>
        {
            let binders = subscripts
                .iter()
                .map(|subscript| family_binder(view, family, subscript))
                .collect::<Option<Vec<_>>>()?;
            (base, binders)
        }
        _ => return None,
    };
    let variable = match view.expression(base)?.operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) => id.index(),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(id)) => id.index(),
        _ => return None,
    };
    if !members
        .get(variable as usize)
        .is_some_and(|facts| facts.eligible)
    {
        return None;
    }
    let declaration = view.variable(view.variable_id(variable as usize)?)?;
    if let Some(family) = family.filter(|_| !binders.is_empty()) {
        let domain = view.domain(family.domain())?;
        let mut distinct = binders.clone();
        distinct.sort_unstable();
        distinct.dedup();
        if binders.len() != declaration.value_type().dimensions().len()
            || distinct.len() != binders.len()
            || domain.scalar_count() as usize != declaration.scalar_count()
        {
            return None;
        }
    }
    Some(CoordinateAccess {
        variable,
        value_type: declaration.value_type(),
        binders,
    })
}

/// The ordinal of a subscript that is a binder of the family's own domain.
fn family_binder<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
    subscript: dae::SubscriptView<'dae>,
) -> Option<u32> {
    let dae::SubscriptView::Index { expression, .. } = subscript else {
        return None;
    };
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder))
            if binder.domain() == family.domain() =>
        {
            Some(binder.ordinal())
        }
        _ => None,
    }
}
