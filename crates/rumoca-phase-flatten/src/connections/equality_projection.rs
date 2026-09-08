use super::*;

/// Whole-record argument retaining the Flat-issued reference and occurrence.
fn record_instance_expr(
    rendered: &str,
    instance: &flat::RecordInstance,
    span: ProvenanceSpan,
) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            rendered,
            instance.component_ref.clone(),
        )
        .with_instance_id(instance.instance_id),
        subscripts: Vec::new(),
        span: span.span(),
    }
}

struct CheckedRecordEdge<'a> {
    lhs: &'a flat::RecordInstance,
    rhs: &'a flat::RecordInstance,
    exposure: ast::EqualityConstraintOccurrenceExposure,
    effective_type: &'a rumoca_core::EffectiveType,
}

struct CheckedEqualityFunction<'a> {
    function: &'a rumoca_core::Function,
    instance: rumoca_core::FunctionInstanceId,
    constraint_size: usize,
}

fn checked_record_instance<'a>(
    flat: &'a flat::Model,
    rendered: &str,
    span: Span,
) -> Result<&'a flat::RecordInstance, FlattenError> {
    flat.record_instances
        .get(&rumoca_core::VarName::new(rendered))
        .ok_or_else(|| {
            FlattenError::invalid_connection_evidence(
                format!("overconstrained record `{rendered}` is absent from Flat record metadata"),
                span,
            )
        })
}

fn checked_occurrence_exposure(
    catalog: &ast::FinalizedOverconstrainedCatalog<'_>,
    instance: &flat::RecordInstance,
    rendered: &str,
    span: Span,
) -> Result<ast::EqualityConstraintOccurrenceExposure, FlattenError> {
    catalog
        .exposure(instance.instance_id)
        .copied()
        .ok_or_else(|| {
            FlattenError::invalid_connection_evidence(
                format!(
                    "overconstrained record `{rendered}` lacks its checked occurrence exposure"
                ),
                span,
            )
        })
}

fn exposures_match_instances(
    lhs_instance: &flat::RecordInstance,
    rhs_instance: &flat::RecordInstance,
    lhs: ast::EqualityConstraintOccurrenceExposure,
    rhs: ast::EqualityConstraintOccurrenceExposure,
) -> bool {
    let lhs_identity = lhs.effective_record_identity();
    let rhs_identity = rhs.effective_record_identity();
    lhs.specialization_key().record_instance() == lhs_instance.instance_id
        && rhs.specialization_key().record_instance() == rhs_instance.instance_id
        && lhs_identity.record_type_def_id() == lhs_instance.type_def_id
        && rhs_identity.record_type_def_id() == rhs_instance.type_def_id
        && lhs_identity.effective_type_id() == lhs_instance.effective_type_id
        && rhs_identity.effective_type_id() == rhs_instance.effective_type_id
        && lhs_identity.effective_type_id() == rhs_identity.effective_type_id()
        && lhs.slot_def_id() == rhs.slot_def_id()
        && lhs.selected_function_def_id() == rhs.selected_function_def_id()
        && lhs.input_def_ids() == rhs.input_def_ids()
        && lhs.input_type_def_ids() == rhs.input_type_def_ids()
        && lhs.output_def_id() == rhs.output_def_id()
        && lhs.output_type_def_id() == rhs.output_type_def_id()
        && lhs.cardinality() == rhs.cardinality()
}

fn checked_effective_type<'a>(
    flat: &'a flat::Model,
    instance: &flat::RecordInstance,
    rendered: &str,
) -> Result<&'a rumoca_core::EffectiveType, FlattenError> {
    flat.effective_types
        .get(&instance.effective_type_id)
        .ok_or_else(|| {
            FlattenError::invalid_connection_evidence(
                format!(
                    "overconstrained record `{rendered}` lacks its exact Flat effective-type descriptor"
                ),
                instance.source_span,
            )
        })
}

fn checked_record_edge<'a>(
    flat: &'a flat::Model,
    catalog: &ast::FinalizedOverconstrainedCatalog<'_>,
    lhs_record: &str,
    rhs_record: &str,
    span: Span,
) -> Result<CheckedRecordEdge<'a>, FlattenError> {
    let lhs = checked_record_instance(flat, lhs_record, span)?;
    let rhs = checked_record_instance(flat, rhs_record, span)?;
    if lhs.type_def_id != rhs.type_def_id {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "overconstrained record edge `{lhs_record}`--`{rhs_record}` has incompatible record types"
            ),
            span,
        ));
    }
    let lhs_exposure = checked_occurrence_exposure(catalog, lhs, lhs_record, span)?;
    let rhs_exposure = checked_occurrence_exposure(catalog, rhs, rhs_record, span)?;
    if !exposures_match_instances(lhs, rhs, lhs_exposure, rhs_exposure) {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "overconstrained record edge `{lhs_record}`--`{rhs_record}` has contradictory exact occurrence exposures"
            ),
            span,
        ));
    }
    let lhs_effective_type = checked_effective_type(flat, lhs, lhs_record)?;
    let rhs_effective_type = checked_effective_type(flat, rhs, rhs_record)?;
    if !lhs.dims.is_empty() || !rhs.dims.is_empty() || lhs_effective_type != rhs_effective_type {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "overconstrained record edge `{lhs_record}`--`{rhs_record}` contradicts its checked non-array effective type"
            ),
            span,
        ));
    }
    Ok(CheckedRecordEdge {
        lhs,
        rhs,
        exposure: lhs_exposure,
        effective_type: lhs_effective_type,
    })
}

fn validate_function_inputs(
    function: &rumoca_core::Function,
    edge: &CheckedRecordEdge<'_>,
    span: Span,
) -> Result<(), FlattenError> {
    let function_name = function.name.as_str();
    let [lhs_input, rhs_input] = function.inputs.as_slice() else {
        return Err(FlattenError::invalid_connection_evidence(
            format!("equalityConstraint function `{function_name}` must have exactly two inputs"),
            span,
        ));
    };
    let input_def_ids = edge.exposure.input_def_ids();
    let input_type_def_ids = edge.exposure.input_type_def_ids();
    if [lhs_input.def_id, rhs_input.def_id] != input_def_ids.map(Some)
        || [lhs_input.type_def_id, rhs_input.type_def_id] != input_type_def_ids.map(Some)
    {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "equalityConstraint function `{function_name}` input declarations contradict the exact occurrence exposure"
            ),
            span,
        ));
    }
    if [lhs_input, rhs_input].into_iter().any(|input| {
        !input.dimensions().is_empty()
            || input.type_def_id != Some(edge.lhs.type_def_id)
            || input.type_class != Some(rumoca_core::ClassType::Record)
            || &input.effective_type != edge.effective_type
    }) {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "equalityConstraint function `{function_name}` inputs do not preserve the exact non-array effective record type"
            ),
            span,
        ));
    }
    Ok(())
}

fn checked_output_size(
    flat: &flat::Model,
    function: &rumoca_core::Function,
    exposure: ast::EqualityConstraintOccurrenceExposure,
    span: Span,
) -> Result<usize, FlattenError> {
    let function_name = function.name.as_str();
    let [output] = function.outputs.as_slice() else {
        return Err(FlattenError::invalid_connection_evidence(
            format!("equalityConstraint function `{function_name}` must have exactly one output"),
            span,
        ));
    };
    if output.def_id != Some(exposure.output_def_id())
        || output.type_def_id != Some(exposure.output_type_def_id())
        || output.effective_type.canonical_type() != flat.predefined_types.real
        || flat
            .type_ids_by_def_id
            .get(&exposure.output_type_def_id())
            .copied()
            != Some(flat.predefined_types.real)
    {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "equalityConstraint function `{function_name}` output contradicts the exact checked Real declaration exposure"
            ),
            span,
        ));
    }
    let [output_extent] = output.dimensions() else {
        return Err(FlattenError::invalid_connection_evidence(
            format!("equalityConstraint function `{function_name}` output is not rank-one Real[n]"),
            span,
        ));
    };
    let output_extent = usize::try_from(*output_extent).map_err(|_| {
        FlattenError::invalid_connection_evidence(
            format!("equalityConstraint function `{function_name}` has an invalid output extent"),
            span,
        )
    })?;
    let constraint_size = exposure.cardinality().scalar_count();
    if output_extent != constraint_size {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "equalityConstraint function `{function_name}` output extent contradicts its exact occurrence exposure"
            ),
            span,
        ));
    }
    Ok(constraint_size)
}

fn checked_equality_function<'a>(
    flat: &'a flat::Model,
    edge: &CheckedRecordEdge<'_>,
    span: Span,
) -> Result<CheckedEqualityFunction<'a>, FlattenError> {
    let selected_declaration = edge.exposure.selected_function_def_id();
    let selected_functions = flat
        .functions
        .values()
        .filter(|function| function.def_id == Some(selected_declaration))
        .collect::<Vec<_>>();
    let [function] = selected_functions.as_slice() else {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "selected equalityConstraint declaration {selected_declaration:?} requires exactly one collected Flat specialization, found {}; Flat cannot yet bind multiple effective specializations without a construction-issued specialization key",
                selected_functions.len()
            ),
            span,
        ));
    };
    let instance = function.instance_id.ok_or_else(|| {
        FlattenError::invalid_connection_evidence(
            "the selected equalityConstraint specialization lacks exact callable identity",
            span,
        )
    })?;
    validate_function_inputs(function, edge, span)?;
    let constraint_size = checked_output_size(flat, function, edge.exposure, span)?;
    Ok(CheckedEqualityFunction {
        function,
        instance,
        constraint_size,
    })
}

fn equality_constraint_residual(
    lhs_record: &str,
    rhs_record: &str,
    edge: &CheckedRecordEdge<'_>,
    selected: &CheckedEqualityFunction<'_>,
    provenance: ProvenanceSpan,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from_var_name(selected.function.name.clone())
            .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                instance_id: selected.instance,
                base_part_count: 0,
                transitively_non_replaceable: selected.function.transitively_non_replaceable,
            }),
        args: vec![
            record_instance_expr(lhs_record, edge.lhs, provenance),
            record_instance_expr(rhs_record, edge.rhs, provenance),
        ],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: provenance.span(),
    }
}

pub(super) fn plan_equality_constraint_equation(
    flat: &flat::Model,
    overconstrained: &ast::FinalizedOverconstrainedCatalog<'_>,
    lhs_record: &str,
    rhs_record: &str,
    span: Span,
    equation_index: usize,
) -> Result<Option<PlannedConnectionEquation>, FlattenError> {
    let provenance = require_connection_provenance(span, "overconstrained equalityConstraint")?;
    let edge = checked_record_edge(flat, overconstrained, lhs_record, rhs_record, span)?;
    let selected = checked_equality_function(flat, &edge, span)?;
    if selected.constraint_size == 0 {
        return Ok(None);
    }
    let origin = flat::EquationOrigin::EqualityConstraint {
        lhs_record: edge.lhs.instance_id,
        rhs_record: edge.rhs.instance_id,
        function: selected.instance,
    };
    let residual =
        equality_constraint_residual(lhs_record, rhs_record, &edge, &selected, provenance);
    let structured_extent = i64::try_from(selected.constraint_size).map_err(|_| {
        FlattenError::invalid_connection_evidence(
            "equalityConstraint output exceeds structured-domain index range",
            span,
        )
    })?;
    plan_connection_equation(
        equation_index,
        flat::Equation::new_array(residual, span, origin, selected.constraint_size),
        Some(&[structured_extent]),
    )
    .map(Some)
}
