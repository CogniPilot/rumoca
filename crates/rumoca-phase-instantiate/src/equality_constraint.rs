use super::*;

pub(super) fn construct_equality_constraint_exposure(
    tree: &ast::ClassTree,
    record: &ast::ClassDef,
    overlay: &mut ast::InstanceOverlay,
    record_instance: rumoca_core::InstanceId,
    ctx: &InstantiateContext,
    error_span: Span,
) -> InstantiateResult<()> {
    if record.class_type != rumoca_core::ClassType::Record {
        return Ok(());
    }
    let declarations = ctx.equality_constraint_declarations();
    if record.def_id.is_none() {
        return Err(invalid_equality_constraint_exposure(
            ctx,
            "the effective equalityConstraint record has no resolved identity",
            error_span,
        ));
    }
    overlay
        .construct_and_register_equality_constraint_occurrence(declarations, tree, record_instance)
        .map(|_| ())
        .map_err(|reason| invalid_equality_constraint_exposure(ctx, reason.to_string(), error_span))
}

pub(super) fn invalid_equality_constraint_exposure(
    ctx: &InstantiateContext,
    reason: impl Into<String>,
    span: Span,
) -> Box<InstantiateError> {
    Box::new(InstantiateError::invalid_equality_constraint_exposure(
        ctx.current_path().to_flat_string(),
        reason.into(),
        span,
    ))
}
