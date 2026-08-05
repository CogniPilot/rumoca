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
    let owner_span = source[0]
        .source_span()
        .expect("analysis requires record-assembly provenance");
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionAggregateLowering, owner_span)?;
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
            &HashMap::new(),
            &value,
            None,
        )?);
        for field in &plan.fields {
            if record_field_completion_offset(field) != Some(statement_offset) {
                continue;
            }
            let field_value = lower_record_field_value(construction, &values, field, generated)?;
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
                || lower_record_field_value(construction, &values, field, generated),
                Ok,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    let target = function_value_coordinate(symbols.coordinates, &plan.target);
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
    let field_value = lower_record_field_value(construction, &values, &plan.field, generated)?;
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
    let target = function_value_coordinate(symbols.coordinates, &plan.target);
    let value_type = construction.functions(|functions| functions.value_type(target, generated))?;
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

fn lower_record_field_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    values: &[dae::ExprId<'dae>],
    field: &FunctionRecordFieldAssembly,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if let Some(statement_offset) = field.aggregate_statement {
        return Ok(values[statement_offset]);
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
    let scalar_type = field
        .scalar_type
        .expect("analysis gives every tensor record field a scalar type");
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
    let mut value = values[source.statement_offset];
    if let Some(field) = &source.value_field {
        let ordinal = construction
            .expressions(|expressions| expressions.record_field_ordinal(value, field, provenance))?
            .ok_or_else(|| dae::DaeConstructionError::InvalidVariableRole {
                name: field.clone(),
                span: provenance.span(),
            })?;
        value = construction
            .expressions(|expressions| expressions.at(provenance).field(value, ordinal))?;
    }
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
