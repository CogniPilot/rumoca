use super::*;

pub(super) fn lower_function_record_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    source: &[rumoca_core::Statement],
    plan: &FunctionRecordAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let owner_span = source[0]
        .source_span()
        .expect("analysis requires record-assembly provenance");
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionAggregateLowering, owner_span)?;
    let values = source
        .iter()
        .map(|statement| {
            let rumoca_core::Statement::Assignment { value, .. } = statement else {
                unreachable!("record assembly certificate contains assignments")
            };
            lower_function_expression(
                construction,
                symbols.coordinates,
                symbols.functions,
                symbols.shapes,
                body,
                value,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut fields = Vec::with_capacity(plan.fields.len());
    for field in &plan.fields {
        let scalars = field
            .scalars
            .iter()
            .map(|source| {
                project_record_field_scalar(
                    construction,
                    values[source.statement_offset],
                    &source.value_coordinates,
                    generated,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        fields.push(pack_row_major_body(
            construction,
            &scalars,
            &field
                .dimensions
                .iter()
                .map(|extent| *extent as usize)
                .collect::<Vec<_>>(),
            generated,
        )?);
    }
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
    construction.functions(|functions| functions.assign(body, target, record, generated))
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
