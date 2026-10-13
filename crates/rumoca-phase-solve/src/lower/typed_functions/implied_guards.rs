//! Guards a fold's early-exit predicate already proves.
//!
//! A fold whose body does nothing while a selection is false carries the
//! selection's exit tests as its continuation: a pass runs only when every one
//! of them holds at its start (MLS §11.2.2). A conditional statement of that
//! body whose guard consists of tests the continuation names reads the same
//! values, because definitions are immutable and a carried target is read
//! through its entry parameter, so it is taken on every pass that runs. Its
//! statements are lowered in the enclosing scope instead of a branch region.

use rumoca_ir_dae as dae;

/// Deepest chain of Boolean definitions followed to reach a test.
const DEPTH: u32 = 32;

/// The tests whose conjunction is `root`: operands of `and`, both parts of
/// `if c then v else false`, and the right-hand side of a Boolean definition.
pub(super) fn conjuncts<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
) -> Vec<dae::ExprId<'dae>> {
    let mut tests = Vec::new();
    collect(view, root, &mut tests, 0);
    tests
}

fn collect<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    tests: &mut Vec<dae::ExprId<'dae>>,
    depth: u32,
) {
    let Some(node) = view.expression(expression) else {
        return;
    };
    if depth >= DEPTH {
        tests.push(expression);
        return;
    }
    match node.operation() {
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::And,
            lhs,
            rhs,
        } => {
            collect(view, lhs, tests, depth + 1);
            collect(view, rhs, tests, depth + 1);
        }
        dae::ExpressionOperation::Conditional(operands) => {
            let parts = operands.iter().collect::<Vec<_>>();
            match parts.as_slice() {
                [condition, value, otherwise] if is_false(view, *otherwise) => {
                    collect(view, *condition, tests, depth + 1);
                    collect(view, *value, tests, depth + 1);
                }
                _ => tests.push(expression),
            }
        }
        dae::ExpressionOperation::FunctionValue { definition, .. }
            if node.value_type().scalar_type() == dae::ScalarType::Boolean =>
        {
            collect(view, definition.rhs(), tests, depth + 1);
        }
        _ => tests.push(expression),
    }
}

fn is_false<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> bool {
    matches!(
        view.expression(expression).map(|node| node.operation()),
        Some(dae::ExpressionOperation::Literal(dae::DaeLiteral::Boolean(
            false
        )))
    )
}

/// Whether every test of `guard` is one of `premises`, each a test the
/// continuation names.
pub(super) fn implied<'dae>(
    view: dae::DaeView<'dae>,
    premises: &[dae::ExprId<'dae>],
    guard: dae::ExprId<'dae>,
) -> bool {
    let tests = conjuncts(view, guard);
    !tests.is_empty()
        && tests.iter().all(|test| {
            premises
                .iter()
                .any(|premise| same_value(view, *premise, *test, 0))
        })
}

/// Whether two expressions are the same computation over the same values.
fn same_value<'dae>(
    view: dae::DaeView<'dae>,
    first: dae::ExprId<'dae>,
    second: dae::ExprId<'dae>,
    depth: u32,
) -> bool {
    if first == second {
        return true;
    }
    if depth >= DEPTH {
        return false;
    }
    let (Some(a), Some(b)) = (view.expression(first), view.expression(second)) else {
        return false;
    };
    use dae::ExpressionOperation as O;
    let same = |x, y| same_value(view, x, y, depth + 1);
    match (a.operation(), b.operation()) {
        (O::Literal(x), O::Literal(y)) => x == y,
        (O::Coordinate(x), O::Coordinate(y)) => x == y,
        (
            O::Unary {
                operator: x,
                operand: p,
            },
            O::Unary {
                operator: y,
                operand: q,
            },
        ) => x == y && same(p, q),
        (
            O::Binary {
                operator: x,
                lhs: p,
                rhs: r,
            },
            O::Binary {
                operator: y,
                lhs: q,
                rhs: s,
            },
        ) => x == y && same(p, q) && same(r, s),
        (
            O::Builtin {
                builtin: x,
                arguments: p,
            },
            O::Builtin {
                builtin: y,
                arguments: q,
            },
        ) => {
            let (p, q) = (p.iter().collect::<Vec<_>>(), q.iter().collect::<Vec<_>>());
            x == y && p.len() == q.len() && p.iter().zip(&q).all(|(x, y)| same(*x, *y))
        }
        (
            O::FunctionFoldParameter {
                fold: x,
                carried: p,
                ..
            },
            O::FunctionFoldParameter {
                fold: y,
                carried: q,
                ..
            },
        ) => x == y && p == q,
        (O::FunctionValue { definition: x, .. }, O::FunctionValue { definition: y, .. }) => {
            x.id() == y.id() || same(x.rhs(), y.rhs())
        }
        (O::FunctionValue { definition, .. }, _) => same(definition.rhs(), second),
        (_, O::FunctionValue { definition, .. }) => same(first, definition.rhs()),
        _ => false,
    }
}
