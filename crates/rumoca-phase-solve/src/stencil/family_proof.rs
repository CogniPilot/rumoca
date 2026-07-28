//! Proof that a structured family's whole domain can use its compact producer
//! metadata, validated by CORNER cells (base plus one unit neighbor per binder).
//!
//! Compact lowering reads `O(ndim)` validation cells and lifts producer access
//! metadata into one tensor node covering every point of the domain. That is
//! sound only when the family genuinely varies affinely in its binders. Two
//! independent facts are required
//! and neither implies the other:
//!
//! * `family.regular` -- Flat IR proved every cell shares ONE affine body, so
//!   the interior cells cannot carry a different expression (`if i == 3 then
//!   100 else 1` is not regular, and corner-extrapolating it silently loses the
//!   interior branch).
//! * binder confinement -- every occurrence of a domain binder inside the
//!   template body sits inside a SUBSCRIPT. A binder used as a value (`der(x[i])
//!   = i*i`) makes the body itself vary non-affinely with the index, so the
//!   per-cell constants the corners expose cannot be extrapolated.
//!
//! `residual_compute_block::compact_direct_family_is_proven` applies the same
//! standard to the residual corner path; this module is the shared statement of
//! it for the stencil-side callers.

use rumoca_core::ExpressionVisitor;
use rumoca_ir_dae as dae;

/// Whether `family` may be lowered from its corner cells alone.
///
/// Returns `false` for any family the compiler has not PROVEN uniform over the
/// full domain, including every family lowered before comprehension templates
/// existed (`template: None`). Callers must fall back to the row-based path,
/// which reads every row, rather than treating a `false` here as a hard error.
pub(crate) fn compact_corner_lowering_is_proven(family: &dae::StructuredEquationFamily) -> bool {
    if family.regular.is_none() {
        return false;
    }
    let Some(template) = family.template.as_ref() else {
        return false;
    };
    match template.scalar_view {
        rumoca_core::ComprehensionScalarView::BinderSubstitution => template
            .body
            .iter()
            .all(|body| binders_are_confined_to_subscripts(body, family)),
        // Row-major projection indices are synthesized from the already
        // validated domain rather than inferred from source subscripts, so no
        // binder reaches the body as a value.
        rumoca_core::ComprehensionScalarView::RowMajorProjection => true,
    }
}

fn binders_are_confined_to_subscripts(
    expression: &rumoca_core::Expression,
    family: &dae::StructuredEquationFamily,
) -> bool {
    let mut proof = BinderSubscriptUseProof {
        binders: &family.domain.binders,
        inside_subscript: false,
        valid: true,
    };
    proof.visit_expression(expression);
    proof.valid
}

struct BinderSubscriptUseProof<'a> {
    binders: &'a [rumoca_core::StructuredIndexBinder],
    inside_subscript: bool,
    valid: bool,
}

impl ExpressionVisitor for BinderSubscriptUseProof<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if !self.inside_subscript
            && self
                .binders
                .iter()
                .any(|binder| binder.display_name == name.as_str())
        {
            self.valid = false;
        }
        self.walk_var_ref(name, subscripts);
    }

    fn visit_subscript(&mut self, subscript: &rumoca_core::Subscript) {
        let was_inside = self.inside_subscript;
        self.inside_subscript = true;
        self.walk_subscript(subscript);
        self.inside_subscript = was_inside;
    }
}
