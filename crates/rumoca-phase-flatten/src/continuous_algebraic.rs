//! Prove which structured equation families are continuous algebraic
//! residual families whose captured template is their only body
//! (SPEC_0043 §6c).
//!
//! The proof is occurrence-scoped and declaration-identified, like the
//! parameter-variability proof: a family qualifies only when every target it
//! defines is a continuous-time `Real` declaration of the class occurrence
//! that owns the equation and no `der` equation of that occurrence reads it,
//! and its template reads only operations with no event, memory, clock, or
//! call semantics. Failing to prove the property keeps the family
//! materialized, which costs only compile time.

use rumoca_core::{BuiltinFunction, Expression, InstanceId, OpBinary, Subscript, Variability};
use rumoca_ir_ast as ast;
use rustc_hash::FxHashSet;

use crate::param_variability::{derivative_declarations, family_targets};

/// Checked evidence that an exact declaration occurrence is a continuous-time
/// `Real` an algebraic family may define.
#[derive(Default)]
pub(crate) struct ContinuousAlgebraicTargets {
    proven: FxHashSet<(InstanceId, rumoca_core::DefId)>,
}

impl ContinuousAlgebraicTargets {
    /// Construct the only evidence accepted by algebraic family cheapening.
    pub(crate) fn prove(overlay: &ast::InstanceOverlay, types: &ast::TypeTable) -> Self {
        let mut proven = FxHashSet::default();
        for class_data in overlay.classes.values() {
            let owner = class_data.instance_id;
            let derivatives = derivative_declarations(&class_data.equations);
            proven.extend(
                overlay
                    .components
                    .values()
                    .filter(|component| {
                        component.owner_class_id == Some(owner)
                            && continuous_real(overlay, types, component)
                    })
                    .filter_map(|component| {
                        component
                            .component_ref
                            .as_ref()
                            .map(rumoca_core::ComponentReference::target_def_id)
                    })
                    .filter(|declaration| !derivatives.contains(declaration))
                    .map(|declaration| (owner, declaration)),
            );
        }
        Self { proven }
    }

    /// Whether the `for` family over `indices` with `equations`, whose
    /// captured template is `template`, is a continuous algebraic family of
    /// class occurrence `owner`.
    pub(crate) fn admits(
        &self,
        owner: InstanceId,
        indices: &[ast::ForIndex],
        equations: &[ast::Equation],
        template: &rumoca_core::ComprehensionTemplate,
    ) -> bool {
        family_targets(indices, equations).is_some_and(|targets| {
            !targets.is_empty()
                && targets
                    .iter()
                    .all(|declaration| self.proven.contains(&(owner, *declaration)))
        }) && fixed_nested_domains(&binder_names(indices), equations)
            && template.body.iter().all(pure_continuous)
    }
}

fn binder_names(indices: &[ast::ForIndex]) -> FxHashSet<String> {
    indices
        .iter()
        .map(|index| index.ident.text.to_string())
        .collect()
}

/// Whether every nested `for` range is independent of the enclosing binders,
/// so each domain point issues the same rows (MLS 3.7 §8.3.2.1 evaluates a
/// nested range per enclosing iteration; a dependent one makes the family
/// non-uniform, and its rows stay materialized).
fn fixed_nested_domains(binders: &FxHashSet<String>, equations: &[ast::Equation]) -> bool {
    equations.iter().all(|equation| match equation {
        ast::Equation::Simple { .. } => true,
        ast::Equation::For { indices, equations } => {
            indices
                .iter()
                .all(|index| !mentions_binder(&index.range, binders))
                && {
                    let mut nested = binders.clone();
                    nested.extend(binder_names(indices));
                    fixed_nested_domains(&nested, equations)
                }
        }
        _ => false,
    })
}

fn mentions_binder(expression: &ast::Expression, binders: &FxHashSet<String>) -> bool {
    struct Mentions<'a>(&'a FxHashSet<String>);
    impl ast::Visitor for Mentions<'_> {
        fn visit_component_reference_ctx(
            &mut self,
            reference: &ast::ComponentReference,
            _context: ast::ComponentReferenceContext,
        ) -> std::ops::ControlFlow<()> {
            if reference
                .parts
                .first()
                .is_some_and(|root| self.0.contains(root.ident.text.as_ref()))
            {
                return std::ops::ControlFlow::Break(());
            }
            ast::walk_component_reference_default(self, reference)
        }
    }
    ast::Visitor::visit_expression(&mut Mentions(binders), expression).is_break()
}

/// A continuous-time `Real` declaration that is neither a flow nor a stream
/// variable (MLS 3.7 §4.5, §9.2).
fn continuous_real(
    overlay: &ast::InstanceOverlay,
    types: &ast::TypeTable,
    component: &ast::InstanceData,
) -> bool {
    matches!(
        component.variability,
        Variability::Empty | Variability::Continuous(_)
    ) && !component.flow
        && !component.stream
        && overlay
            .effective_types
            .get(&component.type_id)
            .is_some_and(|effective| effective.canonical_type() == types.real())
}

/// Whether a template residual reads only arithmetic over references,
/// literals, arrays, ranges, comprehensions, and smooth elementary functions.
/// Relations, `if`, user function calls, records, and every operator with
/// event, memory, clock, or derivative semantics fail the proof.
fn pure_continuous(expression: &Expression) -> bool {
    match expression {
        Expression::Binary { op, lhs, rhs, .. } => {
            arithmetic(op) && pure_continuous(lhs) && pure_continuous(rhs)
        }
        Expression::Unary { rhs, .. } => pure_continuous(rhs),
        Expression::VarRef { subscripts, .. } => subscripts.iter().all(pure_subscript),
        Expression::Literal { .. } => true,
        Expression::BuiltinCall { function, args, .. } => {
            smooth_builtin(*function) && args.iter().all(pure_continuous)
        }
        Expression::Array { elements, .. } => elements.iter().all(pure_continuous),
        Expression::Range {
            start, step, end, ..
        } => {
            pure_continuous(start)
                && step.as_deref().is_none_or(pure_continuous)
                && pure_continuous(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            filter.is_none()
                && pure_continuous(expr)
                && indices.iter().all(|index| pure_continuous(&index.range))
        }
        Expression::Index {
            base, subscripts, ..
        } => pure_continuous(base) && subscripts.iter().all(pure_subscript),
        Expression::FunctionCall { .. }
        | Expression::StringConversion { .. }
        | Expression::If { .. }
        | Expression::Tuple { .. }
        | Expression::FieldAccess { .. }
        | Expression::Empty { .. } => false,
    }
}

fn pure_subscript(subscript: &Subscript) -> bool {
    match subscript {
        Subscript::Index { .. } | Subscript::Colon { .. } => true,
        Subscript::Expr { expr, .. } => pure_continuous(expr),
    }
}

fn arithmetic(op: &OpBinary) -> bool {
    matches!(
        op,
        OpBinary::Add
            | OpBinary::Sub
            | OpBinary::Mul
            | OpBinary::Div
            | OpBinary::Exp
            | OpBinary::AddElem
            | OpBinary::SubElem
            | OpBinary::MulElem
            | OpBinary::DivElem
            | OpBinary::ExpElem
    )
}

/// Elementary functions without event, memory, or clock semantics
/// (MLS 3.7 §3.7.1.1 lists `abs`, `sign`, `min`, `max`, and the rounding
/// functions with event semantics, so they are excluded).
fn smooth_builtin(function: BuiltinFunction) -> bool {
    matches!(
        function,
        BuiltinFunction::Sqrt
            | BuiltinFunction::Sin
            | BuiltinFunction::Cos
            | BuiltinFunction::Tan
            | BuiltinFunction::Asin
            | BuiltinFunction::Acos
            | BuiltinFunction::Atan
            | BuiltinFunction::Atan2
            | BuiltinFunction::Sinh
            | BuiltinFunction::Cosh
            | BuiltinFunction::Tanh
            | BuiltinFunction::Exp
            | BuiltinFunction::Log
            | BuiltinFunction::Log10
            | BuiltinFunction::Sum
            | BuiltinFunction::Product
            | BuiltinFunction::Size
            | BuiltinFunction::Transpose
    )
}
