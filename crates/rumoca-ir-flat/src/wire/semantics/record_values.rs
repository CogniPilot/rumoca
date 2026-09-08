use super::*;

pub(super) fn record_declaration_identity(
    model: &Model,
    declaration: DefId,
) -> Result<WireRecordValueIdentity, FlatWireError> {
    let nominal = model
        .type_ids_by_def_id
        .get(&declaration)
        .copied()
        .ok_or_else(|| expression_shape_error("a record declaration has no exact nominal type"))?;
    let canonical = model
        .type_roots
        .get(&nominal)
        .copied()
        .ok_or_else(|| expression_shape_error("a record declaration has no canonical root"))?;
    if model.type_roots.get(&canonical) != Some(&canonical) {
        return Err(expression_shape_error(
            "a record declaration's canonical root is not self-issued",
        ));
    }
    if model.record_types.contains_key(&declaration) {
        Ok(WireRecordValueIdentity {
            declaration,
            nominal,
            canonical,
        })
    } else {
        Err(expression_shape_error(
            "a record value declaration has no exact retained layout",
        ))
    }
}

pub(super) fn record_field_value_identity(
    model: &Model,
    field: &RecordField,
) -> Result<Option<WireRecordValueIdentity>, FlatWireError> {
    let nominal = model
        .type_ids_by_def_id
        .get(&field.type_def_id)
        .copied()
        .ok_or_else(|| expression_shape_error("a record field type has no declaration identity"))?;
    let canonical = model
        .type_roots
        .get(&nominal)
        .copied()
        .ok_or_else(|| expression_shape_error("a record field type has no canonical root"))?;
    if nominal != field.effective_type.nominal_type()
        || canonical != field.effective_type.canonical_type()
        || model.type_roots.get(&canonical) != Some(&canonical)
    {
        return Err(expression_shape_error(
            "a record field's declared and effective type identities conflict",
        ));
    }
    if model.record_types.contains_key(&field.type_def_id) {
        let identity = record_declaration_identity(model, field.type_def_id)?;
        if identity.nominal != nominal || identity.canonical != canonical {
            return Err(expression_shape_error(
                "a nested record field conflicts with its exact retained declaration",
            ));
        }
        Ok(Some(identity))
    } else {
        Ok(None)
    }
}

pub(super) fn record_value_identity(
    model: &Model,
    record: &RecordInstance,
) -> Result<WireRecordValueIdentity, FlatWireError> {
    let identity = record_declaration_identity(model, record.type_def_id)?;
    let effective = model
        .effective_types
        .get(&record.effective_type_id)
        .ok_or_else(|| expression_shape_error("a record value lacks exact effective identity"))?;
    if effective.nominal_type() == identity.nominal
        && effective.canonical_type() == identity.canonical
    {
        Ok(identity)
    } else {
        Err(expression_shape_error(
            "a record value's nominal or canonical identity contradicts its declaration",
        ))
    }
}

pub(super) fn function_param_record_identity(
    model: &Model,
    param: &FunctionParam,
) -> Result<Option<WireRecordValueIdentity>, FlatWireError> {
    if param.type_class != Some(rumoca_core::ClassType::Record) {
        return Ok(None);
    }
    let declaration = param.type_def_id.ok_or_else(|| {
        expression_shape_error("a function record value lacks exact declaration identity")
    })?;
    let identity = record_declaration_identity(model, declaration)?;
    if identity.nominal == param.effective_type.nominal_type()
        && identity.canonical == param.effective_type.canonical_type()
    {
        Ok(Some(identity))
    } else {
        Err(expression_shape_error(
            "a function record value contradicts its exact effective identity",
        ))
    }
}

pub(super) fn same_record_argument_identity(
    actual: &WireExpressionShape,
    formal: &FunctionParam,
) -> bool {
    match (
        actual.record,
        formal.type_class.as_ref(),
        formal.type_def_id,
    ) {
        (None, Some(rumoca_core::ClassType::Record), _) | (Some(_), _, None) => false,
        (None, _, _) => true,
        (Some(actual), Some(rumoca_core::ClassType::Record), Some(declaration)) => {
            actual.declaration == declaration
                && actual.nominal == formal.effective_type.nominal_type()
                && actual.canonical == formal.effective_type.canonical_type()
        }
        (Some(_), _, Some(_)) => false,
    }
}
