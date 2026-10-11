//! Preserve a checked source family instead of its packed scalar view.
//!
//! MLS §8.3.3/§10.6.1 and Appendix B, SPEC_0007 DAE-C14/SOLVE-C20, and
//! SPEC_0032 §§1–4 retain the source domain/body and exact target coverage.

use super::*;

impl AggregateDiscreteConnections {
    /// Recognize a whole, row-major source family from the existing group proof.
    /// Other profiles keep their existing checked packed owner.
    pub(in crate::construction) fn source_family_assignment<'flat>(
        &self,
        flat: &'flat flat::Model,
        family: &'flat flat::StructuredEquationFamily,
        template: &'flat rumoca_core::ComprehensionTemplate,
        roles: &HashMap<VarName, PlannedRole>,
    ) -> Option<DiscreteValueAssignmentPlan<'flat>> {
        if !family.interiors_materialized()
            || family.equations_per_point != 1
            || template.scalar_view != rumoca_core::ComprehensionScalarView::BinderSubstitution
        {
            return None;
        }
        let [body] = template.body.as_slice() else {
            return None;
        };
        let Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } = body
        else {
            return None;
        };
        let Expression::VarRef {
            name, subscripts, ..
        } = lhs.as_ref()
        else {
            return None;
        };
        let variable = flat.variables.get(name.var_name())?;
        let owner = self.owners.get(&family.first_equation_index)?;
        if name.instance_id() != Some(variable.instance_id)
            || name.var_name() != &owner.target
            || !matches!(roles.get(name.var_name()), Some(PlannedRole::DiscreteValue))
            || owner.ordered_rows.as_ref() != family.materialized_rows().as_ref()
            || owner.ordered_rows.is_none()
            || family.domain.scalar_count().ok()? != owner.scalar_count
        {
            return None;
        }
        let binders = target_axis_binders(family, &variable.dims, subscripts)?;
        if !scope_is_preserved(rhs, variable.instance_id, name.var_name(), &binders) {
            return None;
        }
        Some(DiscreteValueAssignmentPlan {
            target: name.var_name(),
            value: Cow::Borrowed(rhs),
            generated: false,
            scalar_count: None,
            ordered_scalar_self_dependencies: false,
        })
    }
}

fn target_axis_binders<'a>(
    family: &flat::StructuredEquationFamily,
    dimensions: &[i64],
    subscripts: &'a [Subscript],
) -> Option<Vec<&'a rumoca_core::Reference>> {
    if dimensions.len() != family.domain.binders.len() || subscripts.len() != dimensions.len() {
        return None;
    }
    let mut references = Vec::with_capacity(subscripts.len());
    for (axis, ((binder, &extent), subscript)) in family
        .domain
        .binders
        .iter()
        .zip(dimensions)
        .zip(subscripts)
        .enumerate()
    {
        // SPEC_0001/SPEC_0041: the source domain, not an arbitrary nonzero
        // reference id or matching spelling, supplies declaration authority.
        let rumoca_core::IndexBinderProvenance::Source(source_id) = binder.provenance else {
            return None;
        };
        if binder.id != axis || binder.lower != 1 || binder.step != 1 || binder.upper != extent {
            return None;
        }
        let Subscript::Expr { expr, .. } = subscript else {
            return None;
        };
        let Expression::VarRef {
            name, subscripts, ..
        } = expr.as_ref()
        else {
            return None;
        };
        if !subscripts.is_empty()
            || name.as_str() != binder.display_name
            || name.is_generated()
            || name.target_def_id() != Some(source_id)
            || references
                .iter()
                .any(|previous: &&rumoca_core::Reference| same_binder(previous, name))
        {
            return None;
        }
        references.push(name);
    }
    Some(references)
}

fn same_binder(left: &rumoca_core::Reference, right: &rumoca_core::Reference) -> bool {
    match (left.target_def_id(), right.target_def_id()) {
        (Some(left), Some(right)) => left == right,
        (None, None) => {
            left.is_generated() && right.is_generated() && left.var_name() == right.var_name()
        }
        _ => false,
    }
}

fn scope_is_preserved(
    body: &Expression,
    target: rumoca_core::InstanceId,
    target_name: &VarName,
    binders: &[&rumoca_core::Reference],
) -> bool {
    struct Scope<'a> {
        target: rumoca_core::InstanceId,
        target_name: &'a VarName,
        binders: &'a [&'a rumoca_core::Reference],
        preserved: bool,
    }
    impl rumoca_core::ExpressionVisitor for Scope<'_> {
        fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
            // A scalar body cannot borrow the packed aggregate's backward-read proof.
            self.preserved &=
                name.instance_id() != Some(self.target) && name.var_name() != self.target_name;
            self.preserved &= self
                .binders
                .iter()
                .all(|binder| name.var_name() != binder.var_name() || same_binder(binder, name));
            self.walk_var_ref(name, subscripts);
        }
    }
    let mut scope = Scope {
        target,
        target_name,
        binders,
        preserved: true,
    };
    rumoca_core::ExpressionVisitor::visit_expression(&mut scope, body);
    scope.preserved
}
