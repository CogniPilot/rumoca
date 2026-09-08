use super::analysis::function_statement_products::FunctionArraySuffixProduct;
use super::*;

pub(super) fn lower_function_array_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    source: &[rumoca_core::Statement],
    plan: &FunctionArrayAssemblyPlan,
) -> Result<(), dae::DaeConstructionError> {
    let owner_span = source
        .first()
        .and_then(rumoca_core::Statement::source_span)
        .ok_or(dae::DaeConstructionError::MissingProvenance {
            origin: dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::FunctionLoopLowering),
            attempted_span: None,
        })?;
    let direct_source = source
        .get(..plan.direct_count)
        .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span: owner_span })?;
    let mut elements = direct_source
        .iter()
        .map(|statement| {
            let rumoca_core::Statement::Assignment { value, .. } = statement else {
                return Err(dae::DaeConstructionError::InvalidExpressionForm {
                    span: statement.source_span().unwrap_or(owner_span),
                });
            };
            let value = lower_function_expression(
                construction,
                symbols.coordinates,
                Some(symbols.record_staging_scope()),
                symbols.functions,
                symbols.shapes,
                body,
                value,
            )?;
            Ok(value)
        })
        .collect::<Result<Vec<_>, dae::DaeConstructionError>>()?;
    if let Some(suffix) = &plan.suffix {
        let loop_elements = lower_array_assembly_loop(construction, symbols, body, suffix)?;
        elements.extend(loop_elements);
    }
    if elements.len() != plan.extent {
        return Err(dae::DaeConstructionError::InvalidExpressionForm { span: owner_span });
    }

    let owner =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionLoopLowering, owner_span)?;
    let target = *symbols
        .function_values
        .get(&plan.target_def_id)
        .ok_or(dae::DaeConstructionError::InvalidExpressionForm { span: owner_span })?;
    construction.functions(|functions| functions.assign_array(body, target, elements, owner))
}

fn lower_array_assembly_loop<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    suffix: &FunctionArraySuffixProduct,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let domain_owner = dae::DaeProvenance::source(suffix.span)?;
    let domain_id =
        construction.domains(|domains| domains.structured(suffix.domain.clone(), domain_owner))?;
    let binders = lower_function_binders(
        construction,
        domain_id,
        &[&suffix.index],
        &[suffix.binder_span],
    )?;
    let mut loop_shapes = symbols.shapes.clone();
    for binder in binders.keys() {
        // A loop binder is a scalar whose value varies over the iteration, so
        // it shadows any enclosing coordinate's proven value (MLS §11.2.2).
        loop_shapes.insert(binder.clone(), Vec::new());
    }
    let element = lower_function_expression_scoped(
        construction,
        FunctionExpressionValues {
            coordinates: symbols.coordinates,
            record_staging: Some(symbols.record_staging_scope()),
        },
        symbols.functions,
        &loop_shapes,
        body,
        &binders,
        &suffix.value,
    )?;
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionLoopLowering, suffix.span)?;
    let comprehension = construction
        .expressions(|expressions| expressions.at(generated).comprehension(domain_id, element))?;
    let count = suffix
        .domain
        .validated()
        .map_err(|source| dae::DaeConstructionError::InvalidDomain {
            source,
            span: suffix.span,
        })?
        .scalar_count();
    (0..count)
        .map(|point| {
            let value = construction.expressions(|expressions| {
                let index = expressions
                    .at(generated)
                    .literal(dae::DaeLiteral::Integer((point + 1) as i64))?;
                expressions.at(generated).index(
                    comprehension,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: generated,
                    }],
                )
            })?;
            Ok(value)
        })
        .collect()
}
