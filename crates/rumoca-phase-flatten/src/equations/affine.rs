//! Affine analysis of `for`-loop subscripts (family-native lowering).
//!
//! A regular elementwise `for` family (a finite-volume stencil, an elementwise
//! ODE) accesses arrays at indices that are *affine* in the loop binders, e.g.
//! `u[i + 1, j]` or `q[i, j - 1]`. To keep such families compact instead of
//! unrolling them into one scalar equation per index tuple, flattening needs to
//! recognize those affine accesses and recover their per-binder strides.
//!
//! This module is the pure, side-effect-free core of that recognition: it turns
//! an AST index expression into an [`AffineForm`] -- `constant + Σ coeff_b · b`
//! over the binder list -- or returns `None` when the expression is not affine
//! in the binders (e.g. `i * j`, `i ^ 2`, or a runtime-valued reference). A
//! `None` simply means "not a regular family"; the caller then falls back to
//! full materialization, so this analysis is conservative by construction.

use std::sync::Arc;

use rumoca_core::{AffineForm, ArrayAccess, OpBinary, OpUnary, RegularForFamily};
use rumoca_ir_ast as ast;

/// Recognize `expr` as an affine form over `binders`, using `resolve_const` to
/// fold non-binder references (parameters/constants such as `NX`) to integers.
///
/// Returns `None` when the expression is not provably affine in the binders --
/// the conservative "not a regular family" signal.
pub(crate) fn affine_form_in_binders<F>(
    expr: &ast::Expression,
    binders: &[&str],
    resolve_const: &F,
) -> Option<AffineForm>
where
    F: Fn(&ast::ComponentReference) -> Option<i64>,
{
    let n = binders.len();
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token
            .text
            .parse::<i64>()
            .ok()
            .map(|v| AffineForm::constant(v, n)),

        ast::Expression::Parenthesized { inner, .. } => {
            affine_form_in_binders(inner, binders, resolve_const)
        }

        ast::Expression::ComponentReference(cr) => {
            if let Some(index) = binder_position(cr, binders) {
                Some(AffineForm::unit_binder(index, n))
            } else {
                // Not a binder: only acceptable if it folds to an integer
                // constant (a parameter like `NX`, an integer constant, ...).
                resolve_const(cr).map(|v| AffineForm::constant(v, n))
            }
        }

        ast::Expression::Unary { op, rhs, .. } => {
            let inner = affine_form_in_binders(rhs, binders, resolve_const)?;
            match op {
                OpUnary::Minus => Some(inner.neg()),
                OpUnary::Plus => Some(inner),
                _ => None,
            }
        }

        ast::Expression::Binary { op, lhs, rhs, .. } => {
            affine_binary(op, lhs, rhs, binders, resolve_const)
        }

        _ => None,
    }
}

fn affine_binary<F>(
    op: &OpBinary,
    lhs: &Arc<ast::Expression>,
    rhs: &Arc<ast::Expression>,
    binders: &[&str],
    resolve_const: &F,
) -> Option<AffineForm>
where
    F: Fn(&ast::ComponentReference) -> Option<i64>,
{
    let left = affine_form_in_binders(lhs, binders, resolve_const)?;
    let right = affine_form_in_binders(rhs, binders, resolve_const)?;
    match op {
        OpBinary::Add => Some(left.add(&right)),
        OpBinary::Sub => Some(left.add(&right.neg())),
        OpBinary::Mul => {
            // Affine × affine is affine only when at least one side is a
            // binder-free constant (`2*i` or `i*2`); `i*j` is rejected.
            if right.is_binder_free() {
                Some(left.scale(right.constant))
            } else if left.is_binder_free() {
                Some(right.scale(left.constant))
            } else {
                None
            }
        }
        OpBinary::Div => {
            // Only constant/constant division stays integer-affine here; a
            // binder-dependent numerator would not generally be integral.
            if left.is_binder_free() && right.is_binder_free() && right.constant != 0 {
                Some(AffineForm::constant(
                    left.constant / right.constant,
                    binders.len(),
                ))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Classify a `for` equation as a regular elementwise family.
///
/// Descends a chain of singly-nested `for` loops to gather all binders, then
/// requires every body equation to be a simple residual whose array subscripts
/// are all affine in those binders. Returns `None` (not regular -> caller
/// materializes) for non-simple bodies, non-`Expression` subscripts (`:`, ranges),
/// multi-segment subscripted refs, or any non-affine subscript.
pub(crate) fn classify_regular_for_family<F>(
    indices: &[ast::ForIndex],
    body: &[ast::Equation],
    resolve_const: &F,
) -> Option<RegularForFamily>
where
    F: Fn(&ast::ComponentReference) -> Option<i64>,
{
    let mut binders: Vec<String> = indices
        .iter()
        .map(|idx| idx.ident.text.to_string())
        .collect();
    let mut innermost: &[ast::Equation] = body;
    // Collapse a chain of singly-nested for-loops (e.g. `for i loop for j loop`).
    while let [
        ast::Equation::For {
            indices: inner_indices,
            equations: inner_body,
        },
    ] = innermost
    {
        binders.extend(inner_indices.iter().map(|idx| idx.ident.text.to_string()));
        innermost = inner_body.as_slice();
    }
    if binders.is_empty() {
        return None;
    }

    let binder_names: Vec<&str> = binders.iter().map(String::as_str).collect();
    let mut accesses = Vec::new();
    for equation in innermost {
        let ast::Equation::Simple { lhs, rhs } = equation else {
            return None;
        };
        collect_affine_accesses(lhs, &binder_names, resolve_const, &mut accesses)?;
        collect_affine_accesses(rhs, &binder_names, resolve_const, &mut accesses)?;
    }
    Some(RegularForFamily { binders, accesses })
}

/// Append every subscripted access in `expr` to `out`, returning `None` if any
/// access is not affine in `binders` (which rejects the whole family).
fn collect_affine_accesses<F>(
    expr: &ast::Expression,
    binders: &[&str],
    resolve_const: &F,
    out: &mut Vec<ArrayAccess>,
) -> Option<()>
where
    F: Fn(&ast::ComponentReference) -> Option<i64>,
{
    for cr in rumoca_ir_ast::collect_component_refs(expr) {
        if let Some(access) = subscripted_access(&cr, binders, resolve_const)? {
            out.push(access);
        }
    }
    Some(())
}

/// Outer `Option` is the affine-regularity verdict (`None` rejects the family);
/// inner `Option` distinguishes "a handled affine access" from "not an array
/// access at all" (a scalar/param/binder reference, which is simply skipped).
fn subscripted_access<F>(
    cr: &ast::ComponentReference,
    binders: &[&str],
    resolve_const: &F,
) -> Option<Option<ArrayAccess>>
where
    F: Fn(&ast::ComponentReference) -> Option<i64>,
{
    let mut subscripted = cr
        .parts
        .iter()
        .filter(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()));
    let Some(part) = subscripted.next() else {
        return Some(None); // no subscripts: not an array access, skip
    };
    if subscripted.next().is_some() {
        return None; // more than one subscripted segment: not handled -> reject
    }
    let subs = part.subs.as_ref().expect("subscripted part has subs");
    let mut dims = Vec::with_capacity(subs.len());
    for sub in subs {
        let ast::Subscript::Expression(index) = sub else {
            return None; // `:` / range subscript -> not a regular elementwise access
        };
        dims.push(affine_form_in_binders(index, binders, resolve_const)?);
    }
    let var = cr
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    Some(Some(ArrayAccess {
        var,
        subscripts: dims,
    }))
}

/// If `cr` is a bare reference to one of `binders` (single segment, no
/// subscripts), return its position in the binder list.
fn binder_position(cr: &ast::ComponentReference, binders: &[&str]) -> Option<usize> {
    let [part] = cr.parts.as_slice() else {
        return None;
    };
    if part.subs.as_ref().is_some_and(|subs| !subs.is_empty()) {
        return None;
    }
    binders.iter().position(|name| *name == &*part.ident.text)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn int(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: value.to_string().into(),
                ..Default::default()
            },
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn name_ref(name: &str) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: name.into(),
                    ..Default::default()
                },
                subs: None,
            }],
            span: rumoca_core::Span::DUMMY,
            def_id: None,
        })
    }

    fn binary(op: OpBinary, lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
        ast::Expression::Binary {
            op,
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn no_consts(_: &ast::ComponentReference) -> Option<i64> {
        None
    }

    fn form(expr: &ast::Expression, binders: &[&str]) -> Option<AffineForm> {
        affine_form_in_binders(expr, binders, &no_consts)
    }

    #[test]
    fn bare_binder_has_unit_coefficient() {
        assert_eq!(
            form(&name_ref("i"), &["i", "j"]),
            Some(AffineForm {
                constant: 0,
                coeffs: vec![1, 0]
            })
        );
        assert_eq!(
            form(&name_ref("j"), &["i", "j"]),
            Some(AffineForm {
                constant: 0,
                coeffs: vec![0, 1]
            })
        );
    }

    #[test]
    fn integer_literal_is_a_constant() {
        assert_eq!(
            form(&int(5), &["i"]),
            Some(AffineForm {
                constant: 5,
                coeffs: vec![0]
            })
        );
    }

    #[test]
    fn stencil_offsets_recover_strides() {
        // i + 1
        assert_eq!(
            form(&binary(OpBinary::Add, name_ref("i"), int(1)), &["i", "j"]),
            Some(AffineForm {
                constant: 1,
                coeffs: vec![1, 0]
            })
        );
        // j - 1
        assert_eq!(
            form(&binary(OpBinary::Sub, name_ref("j"), int(1)), &["i", "j"]),
            Some(AffineForm {
                constant: -1,
                coeffs: vec![0, 1]
            })
        );
    }

    #[test]
    fn scaled_and_combined_binders_stay_affine() {
        // 2 * i + 3
        let two_i = binary(OpBinary::Mul, int(2), name_ref("i"));
        assert_eq!(
            form(&binary(OpBinary::Add, two_i, int(3)), &["i"]),
            Some(AffineForm {
                constant: 3,
                coeffs: vec![2]
            })
        );
        // i + j
        assert_eq!(
            form(
                &binary(OpBinary::Add, name_ref("i"), name_ref("j")),
                &["i", "j"]
            ),
            Some(AffineForm {
                constant: 0,
                coeffs: vec![1, 1]
            })
        );
    }

    #[test]
    fn nonaffine_products_are_rejected() {
        // i * j is not affine in (i, j)
        assert_eq!(
            form(
                &binary(OpBinary::Mul, name_ref("i"), name_ref("j")),
                &["i", "j"]
            ),
            None
        );
    }

    #[test]
    fn unknown_reference_without_resolver_is_rejected() {
        // `NX` is not a binder and no resolver folds it -> not provably affine.
        assert_eq!(form(&name_ref("NX"), &["i", "j"]), None);
    }

    fn comp_ref(name: &str, subs: Option<Vec<ast::Subscript>>) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: name.into(),
                    ..Default::default()
                },
                subs,
            }],
            span: rumoca_core::Span::DUMMY,
            def_id: None,
        }
    }

    fn arr(name: &str, subs: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression::ComponentReference(comp_ref(
            name,
            Some(subs.into_iter().map(ast::Subscript::Expression).collect()),
        ))
    }

    fn call(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression::FunctionCall {
            comp: comp_ref(name, None),
            args,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn simple(lhs: ast::Expression, rhs: ast::Expression) -> ast::Equation {
        ast::Equation::Simple { lhs, rhs }
    }

    fn for_index(name: &str) -> ast::ForIndex {
        ast::ForIndex {
            ident: rumoca_core::Token {
                text: name.into(),
                ..Default::default()
            },
            range: int(1),
        }
    }

    fn for_eq(binder: &str, body: Vec<ast::Equation>) -> ast::Equation {
        ast::Equation::For {
            indices: vec![for_index(binder)],
            equations: body,
        }
    }

    #[test]
    fn nested_stencil_loop_classifies_regular_with_strides() {
        // for i loop for j loop der(u[i,j]) = u[i+1,j] - u[i-1,j]; end for; end for;
        let body = simple(
            call("der", vec![arr("u", vec![name_ref("i"), name_ref("j")])]),
            binary(
                OpBinary::Sub,
                arr(
                    "u",
                    vec![binary(OpBinary::Add, name_ref("i"), int(1)), name_ref("j")],
                ),
                arr(
                    "u",
                    vec![binary(OpBinary::Sub, name_ref("i"), int(1)), name_ref("j")],
                ),
            ),
        );
        let outer = for_eq("i", vec![for_eq("j", vec![body])]);
        let ast::Equation::For { indices, equations } = &outer else {
            unreachable!()
        };
        let family =
            classify_regular_for_family(indices, equations, &no_consts).expect("regular family");
        assert_eq!(family.binders, vec!["i".to_string(), "j".to_string()]);
        // u[i,j] (output), u[i+1,j], u[i-1,j]
        assert_eq!(
            family.accesses,
            vec![
                ArrayAccess {
                    var: "u".into(),
                    subscripts: vec![
                        AffineForm {
                            constant: 0,
                            coeffs: vec![1, 0]
                        },
                        AffineForm {
                            constant: 0,
                            coeffs: vec![0, 1]
                        },
                    ],
                },
                ArrayAccess {
                    var: "u".into(),
                    subscripts: vec![
                        AffineForm {
                            constant: 1,
                            coeffs: vec![1, 0]
                        },
                        AffineForm {
                            constant: 0,
                            coeffs: vec![0, 1]
                        },
                    ],
                },
                ArrayAccess {
                    var: "u".into(),
                    subscripts: vec![
                        AffineForm {
                            constant: -1,
                            coeffs: vec![1, 0]
                        },
                        AffineForm {
                            constant: 0,
                            coeffs: vec![0, 1]
                        },
                    ],
                },
            ]
        );
    }

    #[test]
    fn nonaffine_access_rejects_the_family() {
        // for i loop for j loop x[i,j] = y[i*j]; -> i*j is not affine
        let body = simple(
            arr("x", vec![name_ref("i"), name_ref("j")]),
            arr(
                "y",
                vec![binary(OpBinary::Mul, name_ref("i"), name_ref("j"))],
            ),
        );
        let outer = for_eq("i", vec![for_eq("j", vec![body])]);
        let ast::Equation::For { indices, equations } = &outer else {
            unreachable!()
        };
        assert_eq!(
            classify_regular_for_family(indices, equations, &no_consts),
            None
        );
    }

    #[test]
    fn nonlinear_bare_binder_dependency_still_classifies_regular() {
        // for i loop x[i] = sin(i); end for; is a regular structured family:
        // the output and array accesses are affine even though solve lowering
        // may later decline corner-based reassembly for the nonlinear value.
        let body = simple(
            arr("x", vec![name_ref("i")]),
            call("sin", vec![name_ref("i")]),
        );
        let outer = for_eq("i", vec![body]);
        let ast::Equation::For { indices, equations } = &outer else {
            unreachable!()
        };
        let family =
            classify_regular_for_family(indices, equations, &no_consts).expect("regular family");
        assert_eq!(family.binders, vec!["i".to_string()]);
        assert_eq!(family.accesses.len(), 1);
    }

    #[test]
    fn affine_bare_binder_dependency_still_classifies_regular() {
        // for i loop x[i] = i * scale + offset; end for; remains affine in the
        // binder. `scale` and `offset` are binder-free symbols, so corner rows can
        // still characterize the per-cell scalar change.
        let body = simple(
            arr("x", vec![name_ref("i")]),
            binary(
                OpBinary::Add,
                binary(OpBinary::Mul, name_ref("i"), name_ref("scale")),
                name_ref("offset"),
            ),
        );
        let outer = for_eq("i", vec![body]);
        let ast::Equation::For { indices, equations } = &outer else {
            unreachable!()
        };
        let family =
            classify_regular_for_family(indices, equations, &no_consts).expect("regular family");
        assert_eq!(family.binders, vec!["i".to_string()]);
    }

    #[test]
    fn resolver_folds_parameters_to_constants() {
        // `NX - 1` with NX=30 -> constant 29 (e.g. a boundary family's fixed index).
        let resolve = |cr: &ast::ComponentReference| -> Option<i64> {
            (cr.parts.len() == 1 && &*cr.parts[0].ident.text == "NX").then_some(30)
        };
        assert_eq!(
            affine_form_in_binders(
                &binary(OpBinary::Sub, name_ref("NX"), int(1)),
                &["j"],
                &resolve
            ),
            Some(AffineForm {
                constant: 29,
                coeffs: vec![0]
            })
        );
    }
}
