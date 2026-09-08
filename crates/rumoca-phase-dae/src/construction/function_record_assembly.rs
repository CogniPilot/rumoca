use super::*;
use crate::construction::analysis::FunctionRecordScalarSource;
use rumoca_core::ExpressionRewriter;

pub(super) fn lower_function_record_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    source: &[rumoca_core::Statement],
    plan: &FunctionRecordAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let (target, record, generated) =
        lower_function_record_value(construction, symbols, body, source, plan)?;
    construction.functions(|functions| functions.assign(body, target, record, generated))
}

pub(super) fn lower_function_loop_record_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    source: &[rumoca_core::Statement],
    plan: &FunctionRecordAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let (target, record, generated) =
        lower_function_record_value(construction, symbols, loop_body.body(), source, plan)?;
    construction.functions(|functions| functions.assign_loop(loop_body, target, record, generated))
}

pub(super) fn lower_function_record_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &dae::FunctionBody<'dae>,
    source: &[rumoca_core::Statement],
    plan: &FunctionRecordAssemblyPlan,
) -> Result<
    (
        dae::FunctionValueId<'dae>,
        dae::ExprId<'dae>,
        dae::DaeProvenance,
    ),
    dae::DaeConstructionError,
> {
    let owner_span = function_record_owner_span(source)?;
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionAggregateLowering, owner_span)?;
    let mut values = Vec::with_capacity(source.len());
    let mut available = HashSet::new();
    let mut staged_values = FunctionRecordStagingValues::default();
    let mut staged_available = FunctionRecordStagingAvailability::default();
    for (statement_offset, statement) in source.iter().enumerate() {
        let rumoca_core::Statement::Assignment { value, .. } = statement else {
            return Err(dae::DaeConstructionError::InvalidExpressionForm { span: owner_span });
        };
        let mut staged_reads = StagedRecordReadRewriter {
            target: &plan.target,
            target_def_id: plan.target_def_id,
            available: &available,
            error: None,
        };
        let value = staged_reads.rewrite_expression(value);
        if let Some(error) = staged_reads.error {
            return Err(error);
        }
        values.push(lower_expression_scoped(
            construction,
            LoweringSymbols {
                coordinates: symbols.coordinates,
                record_staging: Some(FunctionRecordStagingScope::from_inventories(
                    &staged_values,
                    &staged_available,
                )),
                functions: symbols.functions,
                shapes: symbols.shapes,
                function_body: Some(body),
                values: None,
                owner_clock: None,
            },
            &HashMap::new(),
            &value,
            None,
        )?);
        for field in &plan.fields {
            if record_field_completion_offset(field) != Some(statement_offset) {
                continue;
            }
            let field_value = lower_record_field_value(construction, &values, field, generated)?;
            available.insert(field.def_id);
            let identity = FunctionRecordFieldIdentity {
                target: plan.target_def_id,
                field: field.def_id,
            };
            staged_values.insert_expression(identity, field_value, owner_span)?;
            staged_available.insert(identity);
        }
    }
    finish_function_record_value(
        construction,
        symbols,
        plan,
        &values,
        &staged_values,
        generated,
    )
}

fn finish_function_record_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    plan: &FunctionRecordAssemblyPlan,
    values: &[dae::ExprId<'dae>],
    staged_values: &FunctionRecordStagingValues<'dae>,
    generated: dae::DaeProvenance,
) -> Result<
    (
        dae::FunctionValueId<'dae>,
        dae::ExprId<'dae>,
        dae::DaeProvenance,
    ),
    dae::DaeConstructionError,
> {
    let fields = plan
        .fields
        .iter()
        .map(|field| {
            let identity = FunctionRecordFieldIdentity {
                target: plan.target_def_id,
                field: field.def_id,
            };
            match staged_values.get(identity) {
                Some(FunctionRecordStagedValue::Expression(value)) => Ok(value),
                Some(FunctionRecordStagedValue::Local(_)) => {
                    Err(dae::DaeConstructionError::InvalidExpressionForm {
                        span: generated.span(),
                    })
                }
                None => lower_record_field_value(construction, values, field, generated),
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    let target = function_value_coordinate(symbols.coordinates, &plan.target, generated.span())?;
    let value_type = construction.functions(|functions| functions.value_type(target, generated))?;
    construction.types(|types| {
        types.expect_record_layout(
            value_type,
            plan.fields.iter().map(|field| field.name.clone()),
            generated,
        )
    })?;
    let record = construction
        .expressions(|expressions| expressions.at(generated).record(value_type, fields))?;
    Ok((target, record, generated))
}

fn record_field_completion_offset(field: &FunctionRecordFieldAssembly) -> Option<usize> {
    field.aggregate_statement.or_else(|| {
        field
            .scalars
            .iter()
            .map(|source| source.statement_offset)
            .max()
    })
}

pub(super) fn lower_function_record_field_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    source: &[rumoca_core::Statement],
    plan: &FunctionRecordFieldAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let owner_span = function_record_owner_span(source)?;
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionAggregateLowering, owner_span)?;
    let available = plan
        .available_fields
        .iter()
        .map(|field| field.def_id)
        .collect::<HashSet<_>>();
    let values = source
        .iter()
        .map(|statement| {
            let rumoca_core::Statement::Assignment { value, .. } = statement else {
                return Err(dae::DaeConstructionError::InvalidExpressionForm { span: owner_span });
            };
            let mut staged_reads = StagedRecordReadRewriter {
                target: &plan.target,
                target_def_id: plan.target_def_id,
                available: &available,
                error: None,
            };
            let value = staged_reads.rewrite_expression(value);
            if let Some(error) = staged_reads.error {
                return Err(error);
            }
            lower_expression_scoped(
                construction,
                LoweringSymbols {
                    coordinates: symbols.coordinates,
                    record_staging: Some(symbols.record_staging_scope()),
                    functions: symbols.functions,
                    shapes: symbols.shapes,
                    function_body: Some(body),
                    values: None,
                    owner_clock: None,
                },
                &HashMap::new(),
                &value,
                None,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    let field_value = lower_record_field_value(construction, &values, &plan.field, generated)?;
    let staged = symbols
        .record_staging
        .get(FunctionRecordFieldIdentity {
            target: plan.target_def_id,
            field: plan.field.def_id,
        })
        .and_then(|value| match value {
            FunctionRecordStagedValue::Local(local) => Some(local),
            FunctionRecordStagedValue::Expression(_) => None,
        })
        .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span: owner_span })?;
    construction.functions(|functions| functions.assign(body, staged, field_value, generated))?;
    let Some(field_names) = &plan.finalize_fields else {
        return Ok(());
    };
    finalize_staged_record(construction, symbols, body, plan, field_names, generated)
}

fn finalize_staged_record<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    plan: &FunctionRecordFieldAssemblyPlan,
    field_names: &[ResolvedFunctionRecordField],
    generated: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let target = function_value_coordinate(symbols.coordinates, &plan.target, generated.span())?;
    let fields = field_names
        .iter()
        .map(|field| {
            let identity = FunctionRecordFieldIdentity {
                target: plan.target_def_id,
                field: field.def_id,
            };
            // The field completed by this statement has just been assigned to
            // its typed staging local, even though the source-point capability
            // advances only after the statement. Other fields may use staging
            // only while their exact reaching-definition capability survives.
            let staged = if identity.field == plan.field.def_id {
                symbols.record_staging.get(identity)
            } else {
                symbols.record_staging_scope().get(identity)
            };
            match staged {
                Some(FunctionRecordStagedValue::Local(local)) => {
                    construction.functions(|functions| functions.read(body, local, generated))
                }
                Some(FunctionRecordStagedValue::Expression(value)) => Ok(value),
                None => {
                    // A whole-record assignment invalidates older field
                    // staging. Preserve that newer reaching definition by
                    // projecting the exact constructor field from the current
                    // record value instead of reviving stale storage.
                    let record = construction
                        .functions(|functions| functions.read(body, target, generated))?;
                    let ordinal = construction.expressions(|expressions| {
                        expressions.record_field_ordinal(record, &field.name, generated)
                    })?;
                    let Some(ordinal) = ordinal else {
                        return Err(dae::DaeConstructionError::InvalidVariableRole {
                            name: field.name.clone(),
                            span: generated.span(),
                        });
                    };
                    construction
                        .expressions(|expressions| expressions.at(generated).field(record, ordinal))
                }
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    let value_type = construction.functions(|functions| functions.value_type(target, generated))?;
    construction.types(|types| {
        types.expect_record_layout(
            value_type,
            field_names.iter().map(|field| field.name.clone()),
            generated,
        )
    })?;
    let record = construction
        .expressions(|expressions| expressions.at(generated).record(value_type, fields))?;
    construction.functions(|functions| functions.assign(body, target, record, generated))
}

struct StagedRecordReadRewriter<'scope> {
    target: &'scope VarName,
    target_def_id: rumoca_core::DefId,
    available: &'scope HashSet<rumoca_core::DefId>,
    error: Option<dae::DaeConstructionError>,
}

impl ExpressionRewriter for StagedRecordReadRewriter<'_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        let rewritten = self.walk_expression(expression);
        let Expression::FieldAccess {
            base,
            field,
            field_def_id,
            span,
        } = &rewritten
        else {
            return rewritten;
        };
        let Expression::VarRef {
            name, subscripts, ..
        } = base.as_ref()
        else {
            return rewritten;
        };
        if name.var_name() != self.target
            || name.target_def_id() != Some(self.target_def_id)
            || !subscripts.is_empty()
            || !self.available.contains(field_def_id)
        {
            return rewritten;
        }
        let provenance = match span.require_provenance("staged record field read") {
            Ok(provenance) => provenance,
            Err(_) => {
                self.error = Some(dae::DaeConstructionError::MissingProvenance {
                    origin: dae::DaeProvenanceOrigin::Source,
                    attempted_span: Some(*span),
                });
                return rewritten;
            }
        };
        let name = match name.with_appended_field(field, *field_def_id, provenance) {
            Ok(name) => name,
            Err(_) => {
                self.error = Some(dae::DaeConstructionError::InvalidExpressionForm { span: *span });
                return rewritten;
            }
        };
        Expression::VarRef {
            name,
            subscripts: Vec::new(),
            span: *span,
        }
    }
}

fn lower_record_field_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    values: &[dae::ExprId<'dae>],
    field: &FunctionRecordFieldAssembly,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if let Some(statement_offset) = field.aggregate_statement {
        return values.get(statement_offset).copied().ok_or(
            dae::DaeConstructionError::InvalidExpressionForm {
                span: provenance.span(),
            },
        );
    }
    let scalars = field
        .scalars
        .iter()
        .map(|source| lower_record_scalar_source(construction, values, source, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    let dimensions = field
        .dimensions
        .iter()
        .map(|extent| *extent as usize)
        .collect::<Vec<_>>();
    if !scalars.is_empty() {
        return pack_row_major_body(construction, &scalars, &dimensions, provenance);
    }
    let scalar_type =
        field
            .scalar_type
            .ok_or(dae::DaeConstructionError::InvalidExpressionForm {
                span: provenance.span(),
            })?;
    let value_type = construction.types(|types| {
        types.derived(
            dae::ValueType::array(scalar_type, field.dimensions.clone()),
            provenance,
        )
    })?;
    construction.expressions(|expressions| expressions.at(provenance).empty_array(value_type))
}

fn lower_record_scalar_source<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    values: &[dae::ExprId<'dae>],
    source: &FunctionRecordScalarSource,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let value = values.get(source.statement_offset).copied().ok_or(
        dae::DaeConstructionError::InvalidExpressionForm {
            span: provenance.span(),
        },
    )?;
    project_record_field_scalar(construction, value, &source.value_coordinates, provenance)
}

fn function_record_owner_span(
    source: &[rumoca_core::Statement],
) -> Result<rumoca_core::Span, dae::DaeConstructionError> {
    source
        .first()
        .and_then(rumoca_core::Statement::source_span)
        .ok_or(dae::DaeConstructionError::MissingProvenance {
            origin: dae::DaeProvenanceOrigin::Generated(
                dae::DaeGeneration::FunctionAggregateLowering,
            ),
            attempted_span: None,
        })
}

fn project_record_field_scalar<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    value: dae::ExprId<'dae>,
    coordinates: &[u32],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if coordinates.is_empty() {
        return Ok(value);
    }
    construction.expressions(|expressions| {
        let subscripts = coordinates
            .iter()
            .map(|coordinate| {
                let expression = expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Integer(i64::from(*coordinate) + 1))?;
                Ok(dae::Subscript::Index {
                    expression,
                    provenance,
                })
            })
            .collect::<Result<Vec<_>, dae::DaeConstructionError>>()?;
        expressions.at(provenance).index(value, subscripts)
    })
}
