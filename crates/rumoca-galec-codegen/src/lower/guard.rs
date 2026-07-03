//! Recognition and unwrapping of the guarded discrete-update form.
//!
//! The canonical DAE has no stored when/algorithm structure: every `f_z`/
//! `f_m` update right-hand side arrives as the guarded if-expression
//!
//! ```text
//! if (c[i] and not __pre__.c[i]) then <body>
//! else (if Initial() then <target> else __pre__.<target>)
//! ```
//!
//! (or with the plain `<target>` / `__pre__.<target>` hold forms), where
//! `c[i] and not __pre__.c[i]` is the lowered when-edge of condition `i`.
//! `DoStep` runs exactly once per clock tick, so for rows guarded by the
//! **sample-tick** condition the edge guard dissolves — the row lowers to a
//! flat assignment of `<body>` — and the hold branch (the no-tick case) is
//! dropped after being checked to really be the keep-previous form. Nothing
//! is dropped silently: rows guarded by any other condition, or not in this
//! recognizable shape, fail with stable `unsupported-feature` diagnostics
//! (GAL-007) rather than emitting the guarded form.

use rumoca_core::{BuiltinFunction, Expression, OpBinary, OpUnary, Subscript, pre_slot_name};
use rumoca_ir_dae::Equation;

use crate::diagnostic::GalecTargetError;
use crate::lower::conditions::{ConditionTable, condition_ref_index, pre_condition_ref_index};

/// Unwrap one guarded `f_z`/`f_m` equation to its sample-tick body.
pub(crate) fn unwrap_guarded_update<'a>(
    equation: &'a Equation,
    target: &str,
    table: &ConditionTable<'_>,
    sample_index: Option<usize>,
) -> Result<&'a Expression, GalecTargetError> {
    let reject = |detail: String| GalecTargetError::UnsupportedFeature {
        feature: "when-update-form".to_owned(),
        detail,
        span: (!equation.span.is_dummy()).then_some(equation.span),
    };
    let Expression::If {
        branches,
        else_branch,
        ..
    } = &equation.rhs
    else {
        return Err(reject(format!(
            "discrete update of `{target}` is not in the guarded when-edge form \
             (origin: {})",
            equation.origin
        )));
    };
    let [(guard, body)] = branches.as_slice() else {
        return Err(reject(format!(
            "discrete update of `{target}` has {} guard branches, expected exactly one",
            branches.len()
        )));
    };
    let Some(condition_base) = &table.base_name else {
        return Err(reject(format!(
            "discrete update of `{target}` is guarded but the model has no \
             condition equations"
        )));
    };
    let Some(edge_index) = when_edge_index(guard, condition_base) else {
        return Err(reject(format!(
            "discrete update of `{target}` is not guarded by a recognizable \
             `c[i] and not __pre__.c[i]` when-edge"
        )));
    };
    if Some(edge_index) != sample_index {
        return Err(GalecTargetError::UnsupportedFeature {
            feature: "non-clock-when".to_owned(),
            detail: format!(
                "discrete update of `{target}` fires on condition c[{edge_index}], \
                 not on the block clock's sample tick (when-clauses on non-clock \
                 conditions)"
            ),
            span: (!equation.span.is_dummy()).then_some(equation.span),
        });
    }
    if !is_hold_form(else_branch, target) {
        return Err(reject(format!(
            "the no-tick branch of `{target}` is not the keep-previous form \
             and cannot be dropped"
        )));
    }
    Ok(body)
}

/// `c[i] and not __pre__.c[i]` → `i`.
fn when_edge_index(guard: &Expression, condition_base: &str) -> Option<usize> {
    let Expression::Binary {
        op: OpBinary::And,
        lhs,
        rhs,
        ..
    } = guard
    else {
        return None;
    };
    let current = condition_ref_index(lhs, condition_base)?;
    let Expression::Unary {
        op: OpUnary::Not,
        rhs: negated,
        ..
    } = rhs.as_ref()
    else {
        return None;
    };
    let previous = pre_condition_ref_index(negated, condition_base)?;
    (current == previous).then_some(current)
}

/// The keep-previous hold forms the compiler emits for the inactive branch:
/// `<target>`, `__pre__.<target>`, or
/// `if Initial() then <target> else __pre__.<target>`.
fn is_hold_form(expr: &Expression, target: &str) -> bool {
    if is_ref_to(expr, target) || is_pre_ref_to(expr, target) {
        return true;
    }
    let Expression::If {
        branches,
        else_branch,
        ..
    } = expr
    else {
        return false;
    };
    let [(condition, value)] = branches.as_slice() else {
        return false;
    };
    is_initial_call(condition) && is_ref_to(value, target) && is_pre_ref_to(else_branch, target)
}

fn is_initial_call(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall {
            function: BuiltinFunction::Initial,
            args,
            ..
        } if args.is_empty()
    )
}

fn is_pre_ref_to(expr: &Expression, target: &str) -> bool {
    is_ref_to(expr, pre_slot_name(target).as_str())
}

/// True when `expr` is a plain reference rendering exactly as `wanted`
/// (structured literal subscripts render as `[i]`/`[i,j]` suffixes).
fn is_ref_to(expr: &Expression, wanted: &str) -> bool {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return false;
    };
    let mut rendered = name.as_str().to_owned();
    if !subscripts.is_empty() {
        let mut indices = Vec::with_capacity(subscripts.len());
        for subscript in subscripts {
            let Subscript::Index { value, .. } = subscript else {
                return false;
            };
            indices.push(value.to_string());
        }
        rendered = format!("{rendered}[{}]", indices.join(","));
    }
    rendered == wanted
}
