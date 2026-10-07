use super::*;
use crate::construction::analysis::FunctionRecordScalarSource;
use rumoca_core::{ExpressionRewriter, Reference};

pub(super) fn lower_function_record_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    source: &[rumoca_core::Statement],
    plan: &FunctionRecordAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let (target, record, generated) =
        lower_function_record_value(construction, symbols, body, &HashMap::new(), source, plan)?;
    construction.functions(|functions| functions.assign(body, target, record, generated))
}

pub(super) fn lower_function_loop_record_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    source: &[rumoca_core::Statement],
    plan: &FunctionRecordAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let (target, record, generated) = lower_function_record_value(
        construction,
        symbols,
        loop_body.body(),
        binders,
        source,
        plan,
    )?;
    construction.functions(|functions| functions.assign_loop(loop_body, target, record, generated))
}

/// The record `plan` assembles from `source`, with every member expression
/// lowered in the scope of the enclosing loop `binders` (empty at the
/// function's top level).
pub(super) fn lower_function_record_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &dae::FunctionBody<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
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
    let owner_span = source[0]
        .source_span()
        .expect("analysis requires record-assembly provenance");
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionAggregateLowering, owner_span)?;
    let target = function_value_coordinate(symbols.coordinates, &plan.target);
    let value_type = construction.functions(|functions| functions.value_type(target, generated))?;
    let mut values = Vec::with_capacity(source.len());
    let mut available = HashSet::new();
    let mut staged_values = HashMap::new();
    let mut completed_fields = HashMap::new();
    for (statement_offset, statement) in source.iter().enumerate() {
        let rumoca_core::Statement::Assignment { value, .. } = statement else {
            unreachable!("record assembly certificate contains assignments")
        };
        let mut staged_reads = StagedRecordReadRewriter {
            target: &plan.target,
            available: &available,
        };
        let value = staged_reads.rewrite_expression(value);
        values.push(lower_expression_scoped(
            construction,
            LoweringSymbols {
                coordinates: symbols.coordinates,
                functions: symbols.functions,
                shapes: symbols.shapes,
                function_body: Some(body),
                values: Some(&staged_values),
                owner_clock: None,
            },
            binders,
            &value,
            None,
        )?);
        for field in &plan.fields {
            if record_field_completion_offset(field) != Some(statement_offset) {
                continue;
            }
            let field_value =
                lower_record_field_value(construction, &values, field, value_type, &[], generated)?;
            available.insert(field.name.clone());
            staged_values.insert(
                function_record_field_name(&plan.target, &field.name),
                field_value,
            );
            completed_fields.insert(field.name.clone(), field_value);
        }
    }
    let fields = plan
        .fields
        .iter()
        .map(|field| {
            completed_fields.get(&field.name).copied().map_or_else(
                || {
                    lower_record_field_value(
                        construction,
                        &values,
                        field,
                        value_type,
                        &[],
                        generated,
                    )
                },
                Ok,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
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

/// The group offset of the last statement contributing to `field`.
fn record_field_completion_offset(field: &FunctionRecordFieldAssembly) -> Option<usize> {
    match &field.source {
        FunctionRecordFieldSource::Aggregate { statement }
        | FunctionRecordFieldSource::Whole { statement, .. } => Some(*statement),
        FunctionRecordFieldSource::Tensor { scalars, .. } => {
            scalars.iter().map(|source| source.statement_offset).max()
        }
        FunctionRecordFieldSource::Record { fields, .. } => fields
            .iter()
            .filter_map(record_field_completion_offset)
            .max(),
    }
}

pub(super) fn lower_function_record_field_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    source: &[rumoca_core::Statement],
    plan: &FunctionRecordFieldAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let owner_span = source[0]
        .source_span()
        .expect("analysis requires staged record provenance");
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionAggregateLowering, owner_span)?;
    let available = plan
        .available_fields
        .iter()
        .cloned()
        .collect::<HashSet<_>>();
    let values = source
        .iter()
        .map(|statement| {
            let rumoca_core::Statement::Assignment { value, .. } = statement else {
                unreachable!("record field certificate contains assignments")
            };
            let mut staged_reads = StagedRecordReadRewriter {
                target: &plan.target,
                available: &available,
            };
            let value = staged_reads.rewrite_expression(value);
            lower_function_expression(
                construction,
                symbols.coordinates,
                symbols.functions,
                symbols.shapes,
                body,
                &value,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    let target = function_value_coordinate(symbols.coordinates, &plan.target);
    let value_type = construction.functions(|functions| functions.value_type(target, generated))?;
    let field_value = lower_record_field_value(
        construction,
        &values,
        &plan.field,
        value_type,
        &[],
        generated,
    )?;
    let staged_name = function_record_field_name(&plan.target, &plan.field.name);
    let staged = function_value_coordinate(symbols.coordinates, &staged_name);
    construction.functions(|functions| functions.assign(body, staged, field_value, generated))?;
    let Some(field_names) = &plan.finalize_fields else {
        return Ok(());
    };
    let fields = field_names
        .iter()
        .map(|field| {
            let staged_name = function_record_field_name(&plan.target, field);
            let staged = function_value_coordinate(symbols.coordinates, &staged_name);
            construction.functions(|functions| functions.read(body, staged, generated))
        })
        .collect::<Result<Vec<_>, _>>()?;
    construction.types(|types| {
        types.expect_record_layout(value_type, field_names.iter().cloned(), generated)
    })?;
    let record = construction
        .expressions(|expressions| expressions.at(generated).record(value_type, fields))?;
    construction.functions(|functions| functions.assign(body, target, record, generated))
}

struct StagedRecordReadRewriter<'scope> {
    target: &'scope VarName,
    available: &'scope HashSet<VarName>,
}

impl ExpressionRewriter for StagedRecordReadRewriter<'_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        let rewritten = self.walk_expression(expression);
        let Expression::FieldAccess {
            base, field, span, ..
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
        let field = VarName::new(field);
        if name.var_name() != self.target
            || !subscripts.is_empty()
            || !self.available.contains(&field)
        {
            return rewritten;
        }
        Expression::VarRef {
            name: Reference::from_var_name(function_record_field_name(self.target, &field)),
            subscripts: Vec::new(),
            span: *span,
        }
    }
}

/// Lower one assembled field of a record whose type is `record_type`.
///
/// `element` holds the binder coordinates of the enclosing array-of-records
/// element, if any: every group value below such a field is a column whose
/// leading axes are the element axes, so it is read at `element`.
fn lower_record_field_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    values: &[dae::ExprId<'dae>],
    field: &FunctionRecordFieldAssembly,
    record_type: dae::ValueTypeId<'dae>,
    element: &[dae::ExprId<'dae>],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let column = match &field.source {
        FunctionRecordFieldSource::Aggregate { statement } => values[*statement],
        FunctionRecordFieldSource::Whole {
            statement,
            value_field,
        } => project_value_field(
            construction,
            values[*statement],
            value_field.as_ref(),
            provenance,
        )?,
        FunctionRecordFieldSource::Tensor {
            scalar_type,
            dimensions,
            scalars,
        } => lower_record_tensor_field(
            construction,
            values,
            *scalar_type,
            dimensions,
            scalars,
            provenance,
        )?,
        FunctionRecordFieldSource::Record { fields, extents } => {
            return lower_nested_record_field(
                construction,
                values,
                NestedRecordField {
                    name: &field.name,
                    fields,
                    extents,
                    record_type,
                    element,
                },
                provenance,
            );
        }
    };
    select_element(construction, column, element, provenance)
}

struct NestedRecordField<'scope, 'dae> {
    name: &'scope VarName,
    fields: &'scope [FunctionRecordFieldAssembly],
    extents: &'scope [u32],
    record_type: dae::ValueTypeId<'dae>,
    element: &'scope [dae::ExprId<'dae>],
}

/// A record-typed field built from its own fields; a field with extents is
/// an array of records, built as the comprehension over its element domain
/// of the element record whose fields read the columns at that element.
fn lower_nested_record_field<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    values: &[dae::ExprId<'dae>],
    nested: NestedRecordField<'_, 'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let field_type = construction
        .types(|types| types.record_field(nested.record_type, nested.name, provenance))?;
    let element_type = if nested.extents.is_empty() {
        field_type
    } else {
        construction.types(|types| types.record_element(field_type, provenance))?
    };
    construction.types(|types| {
        types.expect_record_layout(
            element_type,
            nested.fields.iter().map(|field| field.name.clone()),
            provenance,
        )
    })?;
    let domain = (!nested.extents.is_empty())
        .then(|| {
            let binders = nested
                .extents
                .iter()
                .enumerate()
                .map(|(ordinal, extent)| StructuredIndexBinder {
                    id: ordinal,
                    display_name: format!("{}{ordinal}", nested.name),
                    lower: 1,
                    upper: i64::from(*extent),
                    step: 1,
                })
                .collect::<Vec<_>>();
            construction.domains(|domains| {
                domains.structured(StructuredIndexDomain { binders }, provenance)
            })
        })
        .transpose()?;
    let mut element = nested.element.to_vec();
    if let Some(domain) = domain {
        for ordinal in 0..nested.extents.len() {
            let binder =
                construction.domains(|domains| domains.binder(domain, ordinal, provenance))?;
            element.push(
                construction
                    .expressions(|expressions| expressions.at(provenance).binder(binder))?,
            );
        }
    }
    let fields = nested
        .fields
        .iter()
        .map(|field| {
            lower_record_field_value(
                construction,
                values,
                field,
                element_type,
                &element,
                provenance,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    let record = construction
        .expressions(|expressions| expressions.at(provenance).record(element_type, fields))?;
    match domain {
        Some(domain) => construction
            .expressions(|expressions| expressions.at(provenance).comprehension(domain, record)),
        None => Ok(record),
    }
}

/// The value of `value` at the array-of-records element `element`: its
/// leading axes are the element axes, and the remaining axes stay whole.
fn select_element<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    value: dae::ExprId<'dae>,
    element: &[dae::ExprId<'dae>],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if element.is_empty() {
        return Ok(value);
    }
    let rank = construction
        .expressions(|expressions| expressions.value_type(value, provenance))?
        .dimensions()
        .len();
    construction.expressions(|expressions| {
        let subscripts = element
            .iter()
            .map(|expression| dae::Subscript::Index {
                expression: *expression,
                provenance,
            })
            .chain((element.len()..rank).map(|_| dae::Subscript::Whole { provenance }))
            .collect::<Vec<_>>();
        expressions.at(provenance).index(value, subscripts)
    })
}

/// `value.field` for a decomposed record value, or `value` itself.
fn project_value_field<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    value: dae::ExprId<'dae>,
    field: Option<&VarName>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let Some(field) = field else {
        return Ok(value);
    };
    let ordinal = construction
        .expressions(|expressions| expressions.record_field_ordinal(value, field, provenance))?
        .ok_or_else(|| dae::DaeConstructionError::InvalidVariableRole {
            name: field.clone(),
            span: provenance.span(),
        })?;
    construction.expressions(|expressions| expressions.at(provenance).field(value, ordinal))
}

fn lower_record_tensor_field<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    values: &[dae::ExprId<'dae>],
    scalar_type: dae::ScalarType,
    dimensions: &[u32],
    scalars: &[FunctionRecordScalarSource],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let scalars = scalars
        .iter()
        .map(|source| lower_record_scalar_source(construction, values, source, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    if !scalars.is_empty() {
        let extents = dimensions
            .iter()
            .map(|extent| *extent as usize)
            .collect::<Vec<_>>();
        return pack_row_major_body(construction, &scalars, &extents, provenance);
    }
    let value_type = construction.types(|types| {
        types.derived(
            dae::ValueType::array(scalar_type, dimensions.to_vec()),
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
    let value = project_value_field(
        construction,
        values[source.statement_offset],
        source.value_field.as_ref(),
        provenance,
    )?;
    project_record_field_scalar(construction, value, &source.value_coordinates, provenance)
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
