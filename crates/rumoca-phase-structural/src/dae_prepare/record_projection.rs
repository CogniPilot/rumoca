//! Record field projection for structural analysis.
//!
//! Flattened MSL keeps `Complex` ports as record-valued references and selects
//! their components with a field access on a composite expression, for example
//! the magnetic-circuit reluctance row `(port_p.V_m - port_n.V_m).im`. Every
//! analysis in this phase is keyed by scalar variable names, so it needs that
//! selection rewritten onto the scalar components the DAE actually declares.

use super::*;
use rumoca_core::Span;

fn make_binary(op: OpBinary, lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn make_unary(op: OpUnary, rhs: Expression, span: Span) -> Expression {
    Expression::Unary {
        op,
        rhs: Box::new(rhs),
        span,
    }
}

/// Rewrite a record field selection as the scalar component expression the DAE
/// already carries.
///
/// MLS 4.7 defines a record component selection as selecting that component of
/// the record value, and MLS 10.6 makes record addition, subtraction and
/// negation elementwise, so selection distributes over them. It does *not*
/// distribute over record multiplication or division — `Complex` overloads
/// those with the complex product — so only the linear forms are projected.
///
/// The derivative map is keyed by scalar variable names, so a record-valued
/// subexpression has no entry in it: the magnetic-circuit reluctance row writes
/// `(port_p.V_m - port_n.V_m).im`, whose scalar components `port_p.V_m.im` and
/// `port_n.V_m.im` are ordinary DAE variables while `port_p.V_m` is not a
/// variable at all. Projection is refused unless every leaf resolves to a
/// declared scalar variable, so this never invents a name for a record the DAE
/// did not scalarize.
pub(super) fn project_record_field(
    dae: &Dae,
    base: &Expression,
    field: &str,
    span: Span,
) -> Option<Expression> {
    match base {
        Expression::Binary {
            op: op @ (OpBinary::Add | OpBinary::Sub | OpBinary::AddElem | OpBinary::SubElem),
            lhs,
            rhs,
            span: base_span,
        } => Some(make_binary(
            op.clone(),
            project_record_field(dae, lhs, field, *base_span)?,
            project_record_field(dae, rhs, field, *base_span)?,
            *base_span,
        )),
        Expression::Unary {
            op: op @ (OpUnary::Minus | OpUnary::Plus | OpUnary::DotMinus | OpUnary::DotPlus),
            rhs,
            span: base_span,
        } => Some(make_unary(
            op.clone(),
            project_record_field(dae, rhs, field, *base_span)?,
            *base_span,
        )),
        Expression::VarRef { .. } | Expression::FieldAccess { .. } => {
            let component = VarName::new(format!("{}.{field}", record_field_owner_name(base)?));
            variable_is_scalar(dae, &component)
                .then(|| super::dummy_state_metadata::var_expr(&component, span))
        }
        _ => None,
    }
}

/// True when `name` is a declared scalar variable of `dae`.
pub(super) fn variable_is_scalar(dae: &Dae, name: &VarName) -> bool {
    let variables = &dae.variables;
    variables
        .states
        .get(name)
        .or_else(|| variables.algebraics.get(name))
        .or_else(|| variables.outputs.get(name))
        .or_else(|| variables.inputs.get(name))
        .or_else(|| variables.parameters.get(name))
        .or_else(|| variables.constants.get(name))
        .is_some_and(|variable| variable.dims.is_empty())
}

/// Every scalar component name that a record field selection inside `expr`
/// reads.
///
/// The derivative-map closure walks plain variable references, which stop at
/// the record-valued base of a field selection. Feeding it these projected
/// names is what lets a constraint row like `(port_p.V_m - port_n.V_m).im = ...`
/// resolve the derivatives of its scalar components.
pub(super) fn record_field_projection_names(dae: &Dae, expr: &Expression) -> IndexSet<VarName> {
    struct Collector<'a> {
        dae: &'a Dae,
        names: IndexSet<VarName>,
    }
    impl ExpressionVisitor for Collector<'_> {
        fn visit_expression(&mut self, expr: &Expression) {
            if let Expression::FieldAccess { base, field, span } = expr
                && !matches!(base.as_ref(), Expression::FunctionCall { .. })
                && let Some(projected) = project_record_field(self.dae, base, field, *span)
            {
                self.names.extend(collect_rhs_var_refs(&projected));
            }
            self.walk_expression(expr);
        }
    }
    let mut collector = Collector {
        dae,
        names: IndexSet::new(),
    };
    collector.visit_expression(expr);
    collector.names
}

/// Flat name of the record a field selection reads, when the selection chain
/// bottoms out in an unsubscripted reference.
fn record_field_owner_name(expr: &Expression) -> Option<String> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.as_str().to_string()),
        Expression::FieldAccess { base, field, .. } => {
            Some(format!("{}.{field}", record_field_owner_name(base)?))
        }
        _ => None,
    }
}
