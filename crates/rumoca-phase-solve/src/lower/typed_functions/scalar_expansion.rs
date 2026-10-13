//! Which function bodies the scalar row programs can expand in place.
//!
//! Scalar expansion substitutes a call's body into the calling row. It
//! preserves nothing the checked typed owner must keep: assigned-state
//! reads execute only through the owner, and scalar row
//! storage is binary64, so a body that carries an assigned-state read or an
//! Integer binary64 cannot hold, directly or through any callee, is not
//! expandable. A call-scoped assertion does not block expansion: the owner
//! registered for the root call carries every nested assertion and checks it,
//! so the expansion only produces values. A source-ordered conditional is
//! expandable when it is pure: its branches only assign values or hold pure
//! conditionals themselves, and every output is a non-record value, so the
//! scalar path lowers it as one checked conditional program.

use std::collections::HashSet;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::assertions::statement_roots;
use super::recursive_groups::function_calls;
use crate::lower::integer_is_exact_binary64;

/// Whether `function` and every function it transitively calls is free of
/// constructs that only the typed owner executes exactly.
pub(crate) fn is_scalar_expandable<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionId<'dae>,
) -> Result<bool, solve::SolveProgramConstructionError> {
    every_body(view, function, |body| {
        !carries_owner_only_construct(view, body.statements(), false)
            && !holds_inexact_integer(view, body)
            && !selects_partial_derivative_arm(view, body)
    })
}

/// Whether `function` or any function it transitively calls holds a call-scoped
/// assertion or a loop: constructs whose ordinary call the generated-C
/// refresh keeps behind the typed owner instead of expanding them into the
/// calling row.
pub(crate) fn holds_assertion_or_loop<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionId<'dae>,
) -> Result<bool, solve::SolveProgramConstructionError> {
    Ok(!every_body(view, function, |body| {
        !statements_hold_assertion_or_loop(body.statements())
    })?)
}

fn statements_hold_assertion_or_loop(statements: dae::FunctionStatements<'_>) -> bool {
    statements.into_iter().any(|statement| match statement {
        dae::FunctionStatementView::Assertion { .. } | dae::FunctionStatementView::For { .. } => {
            true
        }
        dae::FunctionStatementView::Conditional { conditional } => conditional
            .branches()
            .chain(std::iter::once(conditional.fallback()))
            .any(|branch| statements_hold_assertion_or_loop(branch.statements())),
        _ => false,
    })
}

/// Whether this body alone reads only Integer values scalar row storage holds
/// exactly and converts no Real to an Integer, whose out-of-range fault only
/// the checked typed owner raises. A registered callee interface carries the
/// transitive fact, composed from each callee's own.
pub(super) fn body_is_scalar_storable<'dae>(
    view: dae::DaeView<'dae>,
    body: dae::FunctionView<'dae>,
) -> bool {
    body.is_external()
        || (!holds_inexact_integer(view, body) && !converts_real_to_integer(view, body))
}

/// Whether `accepts` holds for the body of `function` and of every function
/// it transitively calls; external bodies carry no statements to inspect.
fn every_body<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionId<'dae>,
    accepts: impl Fn(dae::FunctionView<'dae>) -> bool,
) -> Result<bool, solve::SolveProgramConstructionError> {
    let mut visited = HashSet::new();
    let mut pending = vec![function];
    while let Some(next) = pending.pop() {
        if !visited.insert(next) {
            continue;
        }
        let body = view
            .function(next)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        if !body.is_external() && !accepts(body) {
            return Ok(false);
        }
        pending.extend(
            function_calls(view, next)?
                .into_iter()
                .map(|(_, callee)| callee),
        );
    }
    Ok(true)
}

/// Whether the statements hold a construct only the typed owner executes.
/// `in_loop` is whether they are a loop body, where no conditional expands.
fn carries_owner_only_construct<'dae>(
    view: dae::DaeView<'dae>,
    statements: dae::FunctionStatements<'dae>,
    in_loop: bool,
) -> bool {
    statements.into_iter().any(|statement| match statement {
        dae::FunctionStatementView::Assignment { .. }
        | dae::FunctionStatementView::AssignmentGroup { .. }
        | dae::FunctionStatementView::Assertion { .. } => false,
        dae::FunctionStatementView::RequireAssigned { .. } => true,
        dae::FunctionStatementView::Conditional { conditional } => {
            in_loop || !is_pure_conditional(view, conditional)
        }
        dae::FunctionStatementView::For { statements, .. } => {
            carries_owner_only_construct(view, statements, true)
        }
    })
}

fn is_pure_conditional<'dae>(
    view: dae::DaeView<'dae>,
    conditional: dae::FunctionConditionalBodyView<'dae>,
) -> bool {
    conditional.outputs().all(|output| {
        view.expression(output.rhs())
            .is_some_and(|node| !node.value_type().is_record())
    }) && conditional
        .branches()
        .chain(std::iter::once(conditional.fallback()))
        .all(|branch| {
            !carries_owner_only_construct(view, branch.statements(), false)
                && !holds_partial_derivative_builtin(view, branch.statements())
        })
}

/// Whether the body selects between arms and reads a domain-limited builtin, so an
/// eager scalar expansion could differentiate an unselected arm off its domain.
fn selects_partial_derivative_arm<'dae>(
    view: dae::DaeView<'dae>,
    body: dae::FunctionView<'dae>,
) -> bool {
    fn has_conditional_statement(statements: dae::FunctionStatements<'_>) -> bool {
        statements.into_iter().any(|statement| match statement {
            dae::FunctionStatementView::Conditional { .. } => true,
            dae::FunctionStatementView::For { statements, .. } => {
                has_conditional_statement(statements)
            }
            _ => false,
        })
    }
    let mut selects = has_conditional_statement(body.statements());
    let mut partial = false;
    for root in body_roots(body) {
        dae::for_each_expression(view, root, |_, node| match node.operation() {
            dae::ExpressionOperation::Conditional(_)
            | dae::ExpressionOperation::FunctionConditionalOutput { .. } => selects = true,
            dae::ExpressionOperation::Builtin {
                builtin:
                    dae::PureBuiltin::Sqrt
                    | dae::PureBuiltin::Log
                    | dae::PureBuiltin::Log10
                    | dae::PureBuiltin::Asin
                    | dae::PureBuiltin::Acos,
                ..
            } => partial = true,
            _ => {}
        });
    }
    selects && partial
}

/// Whether the branch reads a domain-limited builtin (`sqrt`, `log`, `asin`, ...):
/// its value is total, but an eager scalar expansion would make a reverse-mode
/// consumer differentiate the unselected arm off its domain. The typed owner
/// keeps the arm a lazily selected region.
fn holds_partial_derivative_builtin<'dae>(
    view: dae::DaeView<'dae>,
    statements: dae::FunctionStatements<'dae>,
) -> bool {
    let mut found = false;
    for root in statement_roots(statements, &|_, _| false) {
        dae::for_each_expression(view, root, |_, node| {
            found |= matches!(
                node.operation(),
                dae::ExpressionOperation::Builtin {
                    builtin: dae::PureBuiltin::Sqrt
                        | dae::PureBuiltin::Log
                        | dae::PureBuiltin::Log10
                        | dae::PureBuiltin::Asin
                        | dae::PureBuiltin::Acos,
                    ..
                }
            );
        });
    }
    found
}

/// Whether the body converts a Real to an Integer with `integer`, a conversion
/// whose target domain the scalar row programs do not check.
fn converts_real_to_integer<'dae>(view: dae::DaeView<'dae>, body: dae::FunctionView<'dae>) -> bool {
    let mut converts = false;
    for root in body_roots(body) {
        dae::for_each_expression(view, root, |_, node| {
            converts |= matches!(
                node.operation(),
                dae::ExpressionOperation::Builtin {
                    builtin: dae::PureBuiltin::Integer,
                    ..
                }
            );
        });
    }
    converts
}

/// Whether the body reads an Integer literal binary64 cannot hold exactly.
fn holds_inexact_integer<'dae>(view: dae::DaeView<'dae>, body: dae::FunctionView<'dae>) -> bool {
    expressions_hold_inexact_integer(view, body_roots(body))
}

/// Every expression a body result or statement reads.
fn body_roots<'dae>(body: dae::FunctionView<'dae>) -> Vec<dae::ExprId<'dae>> {
    let mut roots = body.result_values().rhs_iter().collect::<Vec<_>>();
    roots.extend(statement_roots(body.statements(), &|_, _| false));
    roots
}

/// Whether any of the expressions reads an Integer literal binary64 cannot
/// hold exactly: scalar row storage would refuse it.
pub(crate) fn expressions_hold_inexact_integer<'dae>(
    view: dae::DaeView<'dae>,
    roots: impl IntoIterator<Item = dae::ExprId<'dae>>,
) -> bool {
    let mut inexact = false;
    for root in roots {
        dae::for_each_expression(view, root, |_, node| {
            if let dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) =
                node.operation()
            {
                inexact |= !integer_is_exact_binary64(*value);
            }
        });
    }
    inexact
}
